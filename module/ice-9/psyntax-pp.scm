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
    (let ((#{ribcage?\ 410}# (if #f #f))
          (#{wrap-subst\ 396}# (if #f #f))
          (#{wrap-marks\ 394}# (if #f #f))
          (#{make-wrap\ 392}# (if #f #f))
          (#{binding-value\ 372}# (if #f #f))
          (#{binding-type\ 370}# (if #f #f))
          (#{set-syntax-object-module!\ 363}# (if #f #f))
          (#{set-syntax-object-wrap!\ 361}# (if #f #f))
          (#{set-syntax-object-expression!\ 359}#
            (if #f #f))
          (#{fx<\ 294}# (if #f #f))
          (#{fx=\ 292}# (if #f #f))
          (#{fx-\ 290}# (if #f #f))
          (#{fx+\ 288}# (if #f #f))
          (#{make-primitive-ref\ 249}# (if #f #f)))
      (letrec*
        ((#{lambda-var-list\ 502}#
           (lambda (#{vars\ 717}#)
             (letrec*
               ((#{lvl\ 723}#
                  (lambda (#{vars\ 724}# #{ls\ 725}# #{w\ 726}#)
                    (if (pair? #{vars\ 724}#)
                      (#{lvl\ 723}#
                        (cdr #{vars\ 724}#)
                        (cons (#{wrap\ 456}# (car #{vars\ 724}#) #{w\ 726}# #f)
                              #{ls\ 725}#)
                        #{w\ 726}#)
                      (if (#{id?\ 387}# #{vars\ 724}#)
                        (cons (#{wrap\ 456}# #{vars\ 724}# #{w\ 726}# #f)
                              #{ls\ 725}#)
                        (if (null? #{vars\ 724}#)
                          #{ls\ 725}#
                          (if (#{syntax-object?\ 351}# #{vars\ 724}#)
                            (#{lvl\ 723}#
                              (#{syntax-object-expression\ 353}# #{vars\ 724}#)
                              #{ls\ 725}#
                              (#{join-wraps\ 438}#
                                #{w\ 726}#
                                (#{syntax-object-wrap\ 355}# #{vars\ 724}#)))
                            (cons #{vars\ 724}# #{ls\ 725}#))))))))
               (begin
                 (#{lvl\ 723}#
                   #{vars\ 717}#
                   '()
                   '(()))))))
         (#{gen-var\ 500}#
           (lambda (#{id\ 737}#)
             (begin
               (let ((#{id\ 740}#
                       (if (#{syntax-object?\ 351}# #{id\ 737}#)
                         (#{syntax-object-expression\ 353}# #{id\ 737}#)
                         #{id\ 737}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 740}#) " "))))))
         (#{strip\ 498}#
           (lambda (#{x\ 742}# #{w\ 743}#)
             (if (memq 'top
                       (#{wrap-marks\ 394}# #{w\ 743}#))
               #{x\ 742}#
               (letrec*
                 ((#{f\ 749}# (lambda (#{x\ 750}#)
                                (if (#{syntax-object?\ 351}# #{x\ 750}#)
                                  (#{strip\ 498}#
                                    (#{syntax-object-expression\ 353}#
                                      #{x\ 750}#)
                                    (#{syntax-object-wrap\ 355}# #{x\ 750}#))
                                  (if (pair? #{x\ 750}#)
                                    (begin
                                      (let ((#{a\ 757}# (#{f\ 749}# (car #{x\ 750}#)))
                                            (#{d\ 758}# (#{f\ 749}# (cdr #{x\ 750}#))))
                                        (if (if (eq? #{a\ 757}#
                                                     (car #{x\ 750}#))
                                              (eq? #{d\ 758}# (cdr #{x\ 750}#))
                                              #f)
                                          #{x\ 750}#
                                          (cons #{a\ 757}# #{d\ 758}#))))
                                    (if (vector? #{x\ 750}#)
                                      (begin
                                        (let ((#{old\ 764}#
                                                (vector->list #{x\ 750}#)))
                                          (begin
                                            (let ((#{new\ 766}#
                                                    (map #{f\ 749}#
                                                         #{old\ 764}#)))
                                              (if (#{and-map*\ 35}#
                                                    eq?
                                                    #{old\ 764}#
                                                    #{new\ 766}#)
                                                #{x\ 750}#
                                                (list->vector
                                                  #{new\ 766}#))))))
                                      #{x\ 750}#))))))
                 (begin (#{f\ 749}# #{x\ 742}#))))))
         (#{chi-lambda-case\ 496}#
           (lambda (#{e\ 768}#
                    #{r\ 769}#
                    #{w\ 770}#
                    #{s\ 771}#
                    #{mod\ 772}#
                    #{get-formals\ 773}#
                    #{clauses\ 774}#)
             (letrec*
               ((#{expand-body\ 789}#
                  (lambda (#{req\ 790}#
                           #{opt\ 791}#
                           #{rest\ 792}#
                           #{kw\ 793}#
                           #{body\ 794}#
                           #{vars\ 795}#
                           #{r*\ 796}#
                           #{w*\ 797}#
                           #{inits\ 798}#
                           #{meta\ 799}#)
                    (let ((#{tmp\ 810}# #{body\ 794}#))
                      (let ((#{tmp\ 811}#
                              ($sc-dispatch
                                #{tmp\ 810}#
                                '(any any . each-any))))
                        (if (if #{tmp\ 811}#
                              (@apply
                                (lambda (#{docstring\ 815}#
                                         #{e1\ 816}#
                                         #{e2\ 817}#)
                                  (string? (syntax->datum #{docstring\ 815}#)))
                                #{tmp\ 811}#)
                              #f)
                          (@apply
                            (lambda (#{docstring\ 821}#
                                     #{e1\ 822}#
                                     #{e2\ 823}#)
                              (#{expand-body\ 789}#
                                #{req\ 790}#
                                #{opt\ 791}#
                                #{rest\ 792}#
                                #{kw\ 793}#
                                (cons #{e1\ 822}# #{e2\ 823}#)
                                #{vars\ 795}#
                                #{r*\ 796}#
                                #{w*\ 797}#
                                #{inits\ 798}#
                                (append
                                  #{meta\ 799}#
                                  (list (cons 'documentation
                                              (syntax->datum
                                                #{docstring\ 821}#))))))
                            #{tmp\ 811}#)
                          (let ((#{tmp\ 826}#
                                  ($sc-dispatch
                                    #{tmp\ 810}#
                                    '(#(vector #(each (any . any)))
                                      any
                                      .
                                      each-any))))
                            (if #{tmp\ 826}#
                              (@apply
                                (lambda (#{k\ 831}#
                                         #{v\ 832}#
                                         #{e1\ 833}#
                                         #{e2\ 834}#)
                                  (#{expand-body\ 789}#
                                    #{req\ 790}#
                                    #{opt\ 791}#
                                    #{rest\ 792}#
                                    #{kw\ 793}#
                                    (cons #{e1\ 833}# #{e2\ 834}#)
                                    #{vars\ 795}#
                                    #{r*\ 796}#
                                    #{w*\ 797}#
                                    #{inits\ 798}#
                                    (append
                                      #{meta\ 799}#
                                      (syntax->datum
                                        (map cons #{k\ 831}# #{v\ 832}#)))))
                                #{tmp\ 826}#)
                              (let ((#{tmp\ 838}#
                                      ($sc-dispatch
                                        #{tmp\ 810}#
                                        '(any . each-any))))
                                (if #{tmp\ 838}#
                                  (@apply
                                    (lambda (#{e1\ 841}# #{e2\ 842}#)
                                      (values
                                        #{meta\ 799}#
                                        #{req\ 790}#
                                        #{opt\ 791}#
                                        #{rest\ 792}#
                                        #{kw\ 793}#
                                        #{inits\ 798}#
                                        #{vars\ 795}#
                                        (#{chi-body\ 480}#
                                          (cons #{e1\ 841}# #{e2\ 842}#)
                                          (#{source-wrap\ 458}#
                                            #{e\ 768}#
                                            #{w\ 770}#
                                            #{s\ 771}#
                                            #{mod\ 772}#)
                                          #{r*\ 796}#
                                          #{w*\ 797}#
                                          #{mod\ 772}#)))
                                    #{tmp\ 838}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 810}#))))))))))
                (#{expand-kw\ 787}#
                  (lambda (#{req\ 844}#
                           #{opt\ 845}#
                           #{rest\ 846}#
                           #{kw\ 847}#
                           #{body\ 848}#
                           #{vars\ 849}#
                           #{r*\ 850}#
                           #{w*\ 851}#
                           #{aok\ 852}#
                           #{out\ 853}#
                           #{inits\ 854}#)
                    (if (pair? #{kw\ 847}#)
                      (let ((#{tmp\ 868}# (car #{kw\ 847}#)))
                        (let ((#{tmp\ 869}#
                                ($sc-dispatch
                                  #{tmp\ 868}#
                                  '(any any any))))
                          (if #{tmp\ 869}#
                            (@apply
                              (lambda (#{k\ 873}# #{id\ 874}# #{i\ 875}#)
                                (begin
                                  (let ((#{v\ 878}# (#{gen-var\ 500}#
                                                      #{id\ 874}#)))
                                    (begin
                                      (let ((#{l\ 880}# (#{gen-labels\ 405}#
                                                          (list #{v\ 878}#))))
                                        (begin
                                          (let ((#{r**\ 882}#
                                                  (#{extend-var-env\ 377}#
                                                    #{l\ 880}#
                                                    (list #{v\ 878}#)
                                                    #{r*\ 850}#)))
                                            (begin
                                              (let ((#{w**\ 884}#
                                                      (#{make-binding-wrap\ 434}#
                                                        (list #{id\ 874}#)
                                                        #{l\ 880}#
                                                        #{w*\ 851}#)))
                                                (#{expand-kw\ 787}#
                                                  #{req\ 844}#
                                                  #{opt\ 845}#
                                                  #{rest\ 846}#
                                                  (cdr #{kw\ 847}#)
                                                  #{body\ 848}#
                                                  (cons #{v\ 878}#
                                                        #{vars\ 849}#)
                                                  #{r**\ 882}#
                                                  #{w**\ 884}#
                                                  #{aok\ 852}#
                                                  (cons (list (syntax->datum
                                                                #{k\ 873}#)
                                                              (syntax->datum
                                                                #{id\ 874}#)
                                                              #{v\ 878}#)
                                                        #{out\ 853}#)
                                                  (cons (#{chi\ 472}#
                                                          #{i\ 875}#
                                                          #{r*\ 850}#
                                                          #{w*\ 851}#
                                                          #{mod\ 772}#)
                                                        #{inits\ 854}#)))))))))))
                              #{tmp\ 869}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 868}#))))
                      (#{expand-body\ 789}#
                        #{req\ 844}#
                        #{opt\ 845}#
                        #{rest\ 846}#
                        (if (begin
                              (let ((#{t\ 888}# #{aok\ 852}#))
                                (if #{t\ 888}#
                                  #{t\ 888}#
                                  (pair? #{out\ 853}#))))
                          (cons #{aok\ 852}# (reverse #{out\ 853}#))
                          #f)
                        #{body\ 848}#
                        (reverse #{vars\ 849}#)
                        #{r*\ 850}#
                        #{w*\ 851}#
                        (reverse #{inits\ 854}#)
                        '()))))
                (#{expand-opt\ 785}#
                  (lambda (#{req\ 890}#
                           #{opt\ 891}#
                           #{rest\ 892}#
                           #{kw\ 893}#
                           #{body\ 894}#
                           #{vars\ 895}#
                           #{r*\ 896}#
                           #{w*\ 897}#
                           #{out\ 898}#
                           #{inits\ 899}#)
                    (if (pair? #{opt\ 891}#)
                      (let ((#{tmp\ 912}# (car #{opt\ 891}#)))
                        (let ((#{tmp\ 913}#
                                ($sc-dispatch #{tmp\ 912}# (quote (any any)))))
                          (if #{tmp\ 913}#
                            (@apply
                              (lambda (#{id\ 916}# #{i\ 917}#)
                                (begin
                                  (let ((#{v\ 920}# (#{gen-var\ 500}#
                                                      #{id\ 916}#)))
                                    (begin
                                      (let ((#{l\ 922}# (#{gen-labels\ 405}#
                                                          (list #{v\ 920}#))))
                                        (begin
                                          (let ((#{r**\ 924}#
                                                  (#{extend-var-env\ 377}#
                                                    #{l\ 922}#
                                                    (list #{v\ 920}#)
                                                    #{r*\ 896}#)))
                                            (begin
                                              (let ((#{w**\ 926}#
                                                      (#{make-binding-wrap\ 434}#
                                                        (list #{id\ 916}#)
                                                        #{l\ 922}#
                                                        #{w*\ 897}#)))
                                                (#{expand-opt\ 785}#
                                                  #{req\ 890}#
                                                  (cdr #{opt\ 891}#)
                                                  #{rest\ 892}#
                                                  #{kw\ 893}#
                                                  #{body\ 894}#
                                                  (cons #{v\ 920}#
                                                        #{vars\ 895}#)
                                                  #{r**\ 924}#
                                                  #{w**\ 926}#
                                                  (cons (syntax->datum
                                                          #{id\ 916}#)
                                                        #{out\ 898}#)
                                                  (cons (#{chi\ 472}#
                                                          #{i\ 917}#
                                                          #{r*\ 896}#
                                                          #{w*\ 897}#
                                                          #{mod\ 772}#)
                                                        #{inits\ 899}#)))))))))))
                              #{tmp\ 913}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 912}#))))
                      (if #{rest\ 892}#
                        (begin
                          (let ((#{v\ 931}# (#{gen-var\ 500}# #{rest\ 892}#)))
                            (begin
                              (let ((#{l\ 933}# (#{gen-labels\ 405}#
                                                  (list #{v\ 931}#))))
                                (begin
                                  (let ((#{r*\ 935}#
                                          (#{extend-var-env\ 377}#
                                            #{l\ 933}#
                                            (list #{v\ 931}#)
                                            #{r*\ 896}#)))
                                    (begin
                                      (let ((#{w*\ 937}#
                                              (#{make-binding-wrap\ 434}#
                                                (list #{rest\ 892}#)
                                                #{l\ 933}#
                                                #{w*\ 897}#)))
                                        (#{expand-kw\ 787}#
                                          #{req\ 890}#
                                          (if (pair? #{out\ 898}#)
                                            (reverse #{out\ 898}#)
                                            #f)
                                          (syntax->datum #{rest\ 892}#)
                                          (if (pair? #{kw\ 893}#)
                                            (cdr #{kw\ 893}#)
                                            #{kw\ 893}#)
                                          #{body\ 894}#
                                          (cons #{v\ 931}# #{vars\ 895}#)
                                          #{r*\ 935}#
                                          #{w*\ 937}#
                                          (if (pair? #{kw\ 893}#)
                                            (car #{kw\ 893}#)
                                            #f)
                                          '()
                                          #{inits\ 899}#)))))))))
                        (#{expand-kw\ 787}#
                          #{req\ 890}#
                          (if (pair? #{out\ 898}#)
                            (reverse #{out\ 898}#)
                            #f)
                          #f
                          (if (pair? #{kw\ 893}#)
                            (cdr #{kw\ 893}#)
                            #{kw\ 893}#)
                          #{body\ 894}#
                          #{vars\ 895}#
                          #{r*\ 896}#
                          #{w*\ 897}#
                          (if (pair? #{kw\ 893}#) (car #{kw\ 893}#) #f)
                          '()
                          #{inits\ 899}#)))))
                (#{expand-req\ 783}#
                  (lambda (#{req\ 939}#
                           #{opt\ 940}#
                           #{rest\ 941}#
                           #{kw\ 942}#
                           #{body\ 943}#)
                    (begin
                      (let ((#{vars\ 951}#
                              (map #{gen-var\ 500}# #{req\ 939}#))
                            (#{labels\ 952}#
                              (#{gen-labels\ 405}# #{req\ 939}#)))
                        (begin
                          (let ((#{r*\ 955}#
                                  (#{extend-var-env\ 377}#
                                    #{labels\ 952}#
                                    #{vars\ 951}#
                                    #{r\ 769}#))
                                (#{w*\ 956}#
                                  (#{make-binding-wrap\ 434}#
                                    #{req\ 939}#
                                    #{labels\ 952}#
                                    #{w\ 770}#)))
                            (#{expand-opt\ 785}#
                              (map syntax->datum #{req\ 939}#)
                              #{opt\ 940}#
                              #{rest\ 941}#
                              #{kw\ 942}#
                              #{body\ 943}#
                              (reverse #{vars\ 951}#)
                              #{r*\ 955}#
                              #{w*\ 956}#
                              '()
                              '()))))))))
               (begin
                 (let ((#{tmp\ 957}# #{clauses\ 774}#))
                   (let ((#{tmp\ 958}#
                           ($sc-dispatch #{tmp\ 957}# (quote ()))))
                     (if #{tmp\ 958}#
                       (@apply
                         (lambda () (values (quote ()) #f))
                         #{tmp\ 958}#)
                       (let ((#{tmp\ 959}#
                               ($sc-dispatch
                                 #{tmp\ 957}#
                                 '((any any . each-any)
                                   .
                                   #(each (any any . each-any))))))
                         (if #{tmp\ 959}#
                           (@apply
                             (lambda (#{args\ 966}#
                                      #{e1\ 967}#
                                      #{e2\ 968}#
                                      #{args*\ 969}#
                                      #{e1*\ 970}#
                                      #{e2*\ 971}#)
                               (call-with-values
                                 (lambda ()
                                   (#{get-formals\ 773}# #{args\ 966}#))
                                 (lambda (#{req\ 972}#
                                          #{opt\ 973}#
                                          #{rest\ 974}#
                                          #{kw\ 975}#)
                                   (call-with-values
                                     (lambda ()
                                       (#{expand-req\ 783}#
                                         #{req\ 972}#
                                         #{opt\ 973}#
                                         #{rest\ 974}#
                                         #{kw\ 975}#
                                         (cons #{e1\ 967}# #{e2\ 968}#)))
                                     (lambda (#{meta\ 981}#
                                              #{req\ 982}#
                                              #{opt\ 983}#
                                              #{rest\ 984}#
                                              #{kw\ 985}#
                                              #{inits\ 986}#
                                              #{vars\ 987}#
                                              #{body\ 988}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{chi-lambda-case\ 496}#
                                             #{e\ 768}#
                                             #{r\ 769}#
                                             #{w\ 770}#
                                             #{s\ 771}#
                                             #{mod\ 772}#
                                             #{get-formals\ 773}#
                                             (map (lambda (#{tmp\ 999}#
                                                           #{tmp\ 998}#
                                                           #{tmp\ 997}#)
                                                    (cons #{tmp\ 997}#
                                                          (cons #{tmp\ 998}#
                                                                #{tmp\ 999}#)))
                                                  #{e2*\ 971}#
                                                  #{e1*\ 970}#
                                                  #{args*\ 969}#)))
                                         (lambda (#{meta*\ 1001}#
                                                  #{else*\ 1002}#)
                                           (values
                                             (append
                                               #{meta\ 981}#
                                               #{meta*\ 1001}#)
                                             (#{build-lambda-case\ 333}#
                                               #{s\ 771}#
                                               #{req\ 982}#
                                               #{opt\ 983}#
                                               #{rest\ 984}#
                                               #{kw\ 985}#
                                               #{inits\ 986}#
                                               #{vars\ 987}#
                                               #{body\ 988}#
                                               #{else*\ 1002}#)))))))))
                             #{tmp\ 959}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp\ 957}#))))))))))
         (#{lambda*-formals\ 494}#
           (lambda (#{orig-args\ 1005}#)
             (letrec*
               ((#{check\ 1016}#
                  (lambda (#{req\ 1017}#
                           #{opt\ 1018}#
                           #{rest\ 1019}#
                           #{kw\ 1020}#)
                    (if (#{distinct-bound-ids?\ 452}#
                          (append
                            #{req\ 1017}#
                            (map car #{opt\ 1018}#)
                            (if #{rest\ 1019}#
                              (list #{rest\ 1019}#)
                              '())
                            (if (pair? #{kw\ 1020}#)
                              (map cadr (cdr #{kw\ 1020}#))
                              '())))
                      (values
                        #{req\ 1017}#
                        #{opt\ 1018}#
                        #{rest\ 1019}#
                        #{kw\ 1020}#)
                      (syntax-violation
                        'lambda*
                        "duplicate identifier in argument list"
                        #{orig-args\ 1005}#))))
                (#{rest\ 1014}#
                  (lambda (#{args\ 1028}#
                           #{req\ 1029}#
                           #{opt\ 1030}#
                           #{kw\ 1031}#)
                    (let ((#{tmp\ 1036}# #{args\ 1028}#))
                      (let ((#{tmp\ 1037}# (list #{tmp\ 1036}#)))
                        (if (if #{tmp\ 1037}#
                              (@apply
                                (lambda (#{r\ 1039}#)
                                  (#{id?\ 387}# #{r\ 1039}#))
                                #{tmp\ 1037}#)
                              #f)
                          (@apply
                            (lambda (#{r\ 1041}#)
                              (#{check\ 1016}#
                                #{req\ 1029}#
                                #{opt\ 1030}#
                                #{r\ 1041}#
                                #{kw\ 1031}#))
                            #{tmp\ 1037}#)
                          (let ((#{else\ 1043}# #{tmp\ 1036}#))
                            (syntax-violation
                              'lambda*
                              "invalid rest argument"
                              #{orig-args\ 1005}#
                              #{args\ 1028}#)))))))
                (#{key\ 1012}#
                  (lambda (#{args\ 1044}#
                           #{req\ 1045}#
                           #{opt\ 1046}#
                           #{rkey\ 1047}#)
                    (let ((#{tmp\ 1052}# #{args\ 1044}#))
                      (let ((#{tmp\ 1053}#
                              ($sc-dispatch #{tmp\ 1052}# (quote ()))))
                        (if #{tmp\ 1053}#
                          (@apply
                            (lambda ()
                              (#{check\ 1016}#
                                #{req\ 1045}#
                                #{opt\ 1046}#
                                #f
                                (cons #f (reverse #{rkey\ 1047}#))))
                            #{tmp\ 1053}#)
                          (let ((#{tmp\ 1054}#
                                  ($sc-dispatch
                                    #{tmp\ 1052}#
                                    '(any . any))))
                            (if (if #{tmp\ 1054}#
                                  (@apply
                                    (lambda (#{a\ 1057}# #{b\ 1058}#)
                                      (#{id?\ 387}# #{a\ 1057}#))
                                    #{tmp\ 1054}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 1061}# #{b\ 1062}#)
                                  (let ((#{tmp\ 1064}#
                                          (symbol->keyword
                                            (syntax->datum #{a\ 1061}#))))
                                    (let ((#{k\ 1066}# #{tmp\ 1064}#))
                                      (#{key\ 1012}#
                                        #{b\ 1062}#
                                        #{req\ 1045}#
                                        #{opt\ 1046}#
                                        (cons (cons #{k\ 1066}#
                                                    (cons #{a\ 1061}#
                                                          '(#(syntax-object
                                                              #f
                                                              ((top)
                                                               #(ribcage
                                                                 #(k)
                                                                 #((top))
                                                                 #("i1065"))
                                                               #(ribcage
                                                                 #(a b)
                                                                 #((top) (top))
                                                                 #("i1059"
                                                                   "i1060"))
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
                                                                 #("i1048"
                                                                   "i1049"
                                                                   "i1050"
                                                                   "i1051"))
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
                                                                 ("i1015"
                                                                  "i1013"
                                                                  "i1011"
                                                                  "i1009"
                                                                  "i1007"))
                                                               #(ribcage
                                                                 #(orig-args)
                                                                 #((top))
                                                                 #("i1006"))
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
                                              #{rkey\ 1047}#)))))
                                #{tmp\ 1054}#)
                              (let ((#{tmp\ 1067}#
                                      ($sc-dispatch
                                        #{tmp\ 1052}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 1067}#
                                      (@apply
                                        (lambda (#{a\ 1071}#
                                                 #{init\ 1072}#
                                                 #{b\ 1073}#)
                                          (#{id?\ 387}# #{a\ 1071}#))
                                        #{tmp\ 1067}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 1077}#
                                             #{init\ 1078}#
                                             #{b\ 1079}#)
                                      (let ((#{tmp\ 1081}#
                                              (symbol->keyword
                                                (syntax->datum #{a\ 1077}#))))
                                        (let ((#{k\ 1083}# #{tmp\ 1081}#))
                                          (#{key\ 1012}#
                                            #{b\ 1079}#
                                            #{req\ 1045}#
                                            #{opt\ 1046}#
                                            (cons (list #{k\ 1083}#
                                                        #{a\ 1077}#
                                                        #{init\ 1078}#)
                                                  #{rkey\ 1047}#)))))
                                    #{tmp\ 1067}#)
                                  (let ((#{tmp\ 1084}#
                                          ($sc-dispatch
                                            #{tmp\ 1052}#
                                            '((any any any) . any))))
                                    (if (if #{tmp\ 1084}#
                                          (@apply
                                            (lambda (#{a\ 1089}#
                                                     #{init\ 1090}#
                                                     #{k\ 1091}#
                                                     #{b\ 1092}#)
                                              (if (#{id?\ 387}# #{a\ 1089}#)
                                                (keyword?
                                                  (syntax->datum #{k\ 1091}#))
                                                #f))
                                            #{tmp\ 1084}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 1099}#
                                                 #{init\ 1100}#
                                                 #{k\ 1101}#
                                                 #{b\ 1102}#)
                                          (#{key\ 1012}#
                                            #{b\ 1102}#
                                            #{req\ 1045}#
                                            #{opt\ 1046}#
                                            (cons (list #{k\ 1101}#
                                                        #{a\ 1099}#
                                                        #{init\ 1100}#)
                                                  #{rkey\ 1047}#)))
                                        #{tmp\ 1084}#)
                                      (let ((#{tmp\ 1103}#
                                              ($sc-dispatch
                                                #{tmp\ 1052}#
                                                '(any))))
                                        (if (if #{tmp\ 1103}#
                                              (@apply
                                                (lambda (#{aok\ 1105}#)
                                                  (eq? (syntax->datum
                                                         #{aok\ 1105}#)
                                                       #:allow-other-keys))
                                                #{tmp\ 1103}#)
                                              #f)
                                          (@apply
                                            (lambda (#{aok\ 1107}#)
                                              (#{check\ 1016}#
                                                #{req\ 1045}#
                                                #{opt\ 1046}#
                                                #f
                                                (cons #t
                                                      (reverse
                                                        #{rkey\ 1047}#))))
                                            #{tmp\ 1103}#)
                                          (let ((#{tmp\ 1108}#
                                                  ($sc-dispatch
                                                    #{tmp\ 1052}#
                                                    '(any any any))))
                                            (if (if #{tmp\ 1108}#
                                                  (@apply
                                                    (lambda (#{aok\ 1112}#
                                                             #{a\ 1113}#
                                                             #{b\ 1114}#)
                                                      (if (eq? (syntax->datum
                                                                 #{aok\ 1112}#)
                                                               #:allow-other-keys)
                                                        (eq? (syntax->datum
                                                               #{a\ 1113}#)
                                                             #:rest)
                                                        #f))
                                                    #{tmp\ 1108}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{aok\ 1120}#
                                                         #{a\ 1121}#
                                                         #{b\ 1122}#)
                                                  (#{rest\ 1014}#
                                                    #{b\ 1122}#
                                                    #{req\ 1045}#
                                                    #{opt\ 1046}#
                                                    (cons #t
                                                          (reverse
                                                            #{rkey\ 1047}#))))
                                                #{tmp\ 1108}#)
                                              (let ((#{tmp\ 1123}#
                                                      ($sc-dispatch
                                                        #{tmp\ 1052}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 1123}#
                                                      (@apply
                                                        (lambda (#{aok\ 1126}#
                                                                 #{r\ 1127}#)
                                                          (if (eq? (syntax->datum
                                                                     #{aok\ 1126}#)
                                                                   #:allow-other-keys)
                                                            (#{id?\ 387}#
                                                              #{r\ 1127}#)
                                                            #f))
                                                        #{tmp\ 1123}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{aok\ 1132}#
                                                             #{r\ 1133}#)
                                                      (#{rest\ 1014}#
                                                        #{r\ 1133}#
                                                        #{req\ 1045}#
                                                        #{opt\ 1046}#
                                                        (cons #t
                                                              (reverse
                                                                #{rkey\ 1047}#))))
                                                    #{tmp\ 1123}#)
                                                  (let ((#{tmp\ 1134}#
                                                          ($sc-dispatch
                                                            #{tmp\ 1052}#
                                                            '(any any))))
                                                    (if (if #{tmp\ 1134}#
                                                          (@apply
                                                            (lambda (#{a\ 1137}#
                                                                     #{b\ 1138}#)
                                                              (eq? (syntax->datum
                                                                     #{a\ 1137}#)
                                                                   #:rest))
                                                            #{tmp\ 1134}#)
                                                          #f)
                                                      (@apply
                                                        (lambda (#{a\ 1141}#
                                                                 #{b\ 1142}#)
                                                          (#{rest\ 1014}#
                                                            #{b\ 1142}#
                                                            #{req\ 1045}#
                                                            #{opt\ 1046}#
                                                            (cons #f
                                                                  (reverse
                                                                    #{rkey\ 1047}#))))
                                                        #{tmp\ 1134}#)
                                                      (let ((#{tmp\ 1143}#
                                                              (list #{tmp\ 1052}#)))
                                                        (if (if #{tmp\ 1143}#
                                                              (@apply
                                                                (lambda (#{r\ 1145}#)
                                                                  (#{id?\ 387}#
                                                                    #{r\ 1145}#))
                                                                #{tmp\ 1143}#)
                                                              #f)
                                                          (@apply
                                                            (lambda (#{r\ 1147}#)
                                                              (#{rest\ 1014}#
                                                                #{r\ 1147}#
                                                                #{req\ 1045}#
                                                                #{opt\ 1046}#
                                                                (cons #f
                                                                      (reverse
                                                                        #{rkey\ 1047}#))))
                                                            #{tmp\ 1143}#)
                                                          (let ((#{else\ 1149}#
                                                                  #{tmp\ 1052}#))
                                                            (syntax-violation
                                                              'lambda*
                                                              "invalid keyword argument list"
                                                              #{orig-args\ 1005}#
                                                              #{args\ 1044}#)))))))))))))))))))))))
                (#{opt\ 1010}#
                  (lambda (#{args\ 1150}# #{req\ 1151}# #{ropt\ 1152}#)
                    (let ((#{tmp\ 1156}# #{args\ 1150}#))
                      (let ((#{tmp\ 1157}#
                              ($sc-dispatch #{tmp\ 1156}# (quote ()))))
                        (if #{tmp\ 1157}#
                          (@apply
                            (lambda ()
                              (#{check\ 1016}#
                                #{req\ 1151}#
                                (reverse #{ropt\ 1152}#)
                                #f
                                '()))
                            #{tmp\ 1157}#)
                          (let ((#{tmp\ 1158}#
                                  ($sc-dispatch
                                    #{tmp\ 1156}#
                                    '(any . any))))
                            (if (if #{tmp\ 1158}#
                                  (@apply
                                    (lambda (#{a\ 1161}# #{b\ 1162}#)
                                      (#{id?\ 387}# #{a\ 1161}#))
                                    #{tmp\ 1158}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 1165}# #{b\ 1166}#)
                                  (#{opt\ 1010}#
                                    #{b\ 1166}#
                                    #{req\ 1151}#
                                    (cons (cons #{a\ 1165}#
                                                '(#(syntax-object
                                                    #f
                                                    ((top)
                                                     #(ribcage
                                                       #(a b)
                                                       #((top) (top))
                                                       #("i1163" "i1164"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(args req ropt)
                                                       #((top) (top) (top))
                                                       #("i1153"
                                                         "i1154"
                                                         "i1155"))
                                                     #(ribcage
                                                       (check rest key opt req)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i1015"
                                                        "i1013"
                                                        "i1011"
                                                        "i1009"
                                                        "i1007"))
                                                     #(ribcage
                                                       #(orig-args)
                                                       #((top))
                                                       #("i1006"))
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
                                          #{ropt\ 1152}#)))
                                #{tmp\ 1158}#)
                              (let ((#{tmp\ 1167}#
                                      ($sc-dispatch
                                        #{tmp\ 1156}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 1167}#
                                      (@apply
                                        (lambda (#{a\ 1171}#
                                                 #{init\ 1172}#
                                                 #{b\ 1173}#)
                                          (#{id?\ 387}# #{a\ 1171}#))
                                        #{tmp\ 1167}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 1177}#
                                             #{init\ 1178}#
                                             #{b\ 1179}#)
                                      (#{opt\ 1010}#
                                        #{b\ 1179}#
                                        #{req\ 1151}#
                                        (cons (list #{a\ 1177}# #{init\ 1178}#)
                                              #{ropt\ 1152}#)))
                                    #{tmp\ 1167}#)
                                  (let ((#{tmp\ 1180}#
                                          ($sc-dispatch
                                            #{tmp\ 1156}#
                                            '(any . any))))
                                    (if (if #{tmp\ 1180}#
                                          (@apply
                                            (lambda (#{a\ 1183}# #{b\ 1184}#)
                                              (eq? (syntax->datum #{a\ 1183}#)
                                                   #:key))
                                            #{tmp\ 1180}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 1187}# #{b\ 1188}#)
                                          (#{key\ 1012}#
                                            #{b\ 1188}#
                                            #{req\ 1151}#
                                            (reverse #{ropt\ 1152}#)
                                            '()))
                                        #{tmp\ 1180}#)
                                      (let ((#{tmp\ 1189}#
                                              ($sc-dispatch
                                                #{tmp\ 1156}#
                                                '(any any))))
                                        (if (if #{tmp\ 1189}#
                                              (@apply
                                                (lambda (#{a\ 1192}#
                                                         #{b\ 1193}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 1192}#)
                                                       #:rest))
                                                #{tmp\ 1189}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 1196}# #{b\ 1197}#)
                                              (#{rest\ 1014}#
                                                #{b\ 1197}#
                                                #{req\ 1151}#
                                                (reverse #{ropt\ 1152}#)
                                                '()))
                                            #{tmp\ 1189}#)
                                          (let ((#{tmp\ 1198}#
                                                  (list #{tmp\ 1156}#)))
                                            (if (if #{tmp\ 1198}#
                                                  (@apply
                                                    (lambda (#{r\ 1200}#)
                                                      (#{id?\ 387}#
                                                        #{r\ 1200}#))
                                                    #{tmp\ 1198}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 1202}#)
                                                  (#{rest\ 1014}#
                                                    #{r\ 1202}#
                                                    #{req\ 1151}#
                                                    (reverse #{ropt\ 1152}#)
                                                    '()))
                                                #{tmp\ 1198}#)
                                              (let ((#{else\ 1204}#
                                                      #{tmp\ 1156}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid optional argument list"
                                                  #{orig-args\ 1005}#
                                                  #{args\ 1150}#)))))))))))))))))
                (#{req\ 1008}#
                  (lambda (#{args\ 1205}# #{rreq\ 1206}#)
                    (let ((#{tmp\ 1209}# #{args\ 1205}#))
                      (let ((#{tmp\ 1210}#
                              ($sc-dispatch #{tmp\ 1209}# (quote ()))))
                        (if #{tmp\ 1210}#
                          (@apply
                            (lambda ()
                              (#{check\ 1016}#
                                (reverse #{rreq\ 1206}#)
                                '()
                                #f
                                '()))
                            #{tmp\ 1210}#)
                          (let ((#{tmp\ 1211}#
                                  ($sc-dispatch
                                    #{tmp\ 1209}#
                                    '(any . any))))
                            (if (if #{tmp\ 1211}#
                                  (@apply
                                    (lambda (#{a\ 1214}# #{b\ 1215}#)
                                      (#{id?\ 387}# #{a\ 1214}#))
                                    #{tmp\ 1211}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 1218}# #{b\ 1219}#)
                                  (#{req\ 1008}#
                                    #{b\ 1219}#
                                    (cons #{a\ 1218}# #{rreq\ 1206}#)))
                                #{tmp\ 1211}#)
                              (let ((#{tmp\ 1220}#
                                      ($sc-dispatch
                                        #{tmp\ 1209}#
                                        '(any . any))))
                                (if (if #{tmp\ 1220}#
                                      (@apply
                                        (lambda (#{a\ 1223}# #{b\ 1224}#)
                                          (eq? (syntax->datum #{a\ 1223}#)
                                               #:optional))
                                        #{tmp\ 1220}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 1227}# #{b\ 1228}#)
                                      (#{opt\ 1010}#
                                        #{b\ 1228}#
                                        (reverse #{rreq\ 1206}#)
                                        '()))
                                    #{tmp\ 1220}#)
                                  (let ((#{tmp\ 1229}#
                                          ($sc-dispatch
                                            #{tmp\ 1209}#
                                            '(any . any))))
                                    (if (if #{tmp\ 1229}#
                                          (@apply
                                            (lambda (#{a\ 1232}# #{b\ 1233}#)
                                              (eq? (syntax->datum #{a\ 1232}#)
                                                   #:key))
                                            #{tmp\ 1229}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 1236}# #{b\ 1237}#)
                                          (#{key\ 1012}#
                                            #{b\ 1237}#
                                            (reverse #{rreq\ 1206}#)
                                            '()
                                            '()))
                                        #{tmp\ 1229}#)
                                      (let ((#{tmp\ 1238}#
                                              ($sc-dispatch
                                                #{tmp\ 1209}#
                                                '(any any))))
                                        (if (if #{tmp\ 1238}#
                                              (@apply
                                                (lambda (#{a\ 1241}#
                                                         #{b\ 1242}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 1241}#)
                                                       #:rest))
                                                #{tmp\ 1238}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 1245}# #{b\ 1246}#)
                                              (#{rest\ 1014}#
                                                #{b\ 1246}#
                                                (reverse #{rreq\ 1206}#)
                                                '()
                                                '()))
                                            #{tmp\ 1238}#)
                                          (let ((#{tmp\ 1247}#
                                                  (list #{tmp\ 1209}#)))
                                            (if (if #{tmp\ 1247}#
                                                  (@apply
                                                    (lambda (#{r\ 1249}#)
                                                      (#{id?\ 387}#
                                                        #{r\ 1249}#))
                                                    #{tmp\ 1247}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 1251}#)
                                                  (#{rest\ 1014}#
                                                    #{r\ 1251}#
                                                    (reverse #{rreq\ 1206}#)
                                                    '()
                                                    '()))
                                                #{tmp\ 1247}#)
                                              (let ((#{else\ 1253}#
                                                      #{tmp\ 1209}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid argument list"
                                                  #{orig-args\ 1005}#
                                                  #{args\ 1205}#))))))))))))))))))
               (begin
                 (#{req\ 1008}# #{orig-args\ 1005}# (quote ()))))))
         (#{chi-simple-lambda\ 492}#
           (lambda (#{e\ 1254}#
                    #{r\ 1255}#
                    #{w\ 1256}#
                    #{s\ 1257}#
                    #{mod\ 1258}#
                    #{req\ 1259}#
                    #{rest\ 1260}#
                    #{meta\ 1261}#
                    #{body\ 1262}#)
             (begin
               (let ((#{ids\ 1274}#
                       (if #{rest\ 1260}#
                         (append #{req\ 1259}# (list #{rest\ 1260}#))
                         #{req\ 1259}#)))
                 (begin
                   (let ((#{vars\ 1276}#
                           (map #{gen-var\ 500}# #{ids\ 1274}#)))
                     (begin
                       (let ((#{labels\ 1278}#
                               (#{gen-labels\ 405}# #{ids\ 1274}#)))
                         (#{build-simple-lambda\ 329}#
                           #{s\ 1257}#
                           (map syntax->datum #{req\ 1259}#)
                           (if #{rest\ 1260}#
                             (syntax->datum #{rest\ 1260}#)
                             #f)
                           #{vars\ 1276}#
                           #{meta\ 1261}#
                           (#{chi-body\ 480}#
                             #{body\ 1262}#
                             (#{source-wrap\ 458}#
                               #{e\ 1254}#
                               #{w\ 1256}#
                               #{s\ 1257}#
                               #{mod\ 1258}#)
                             (#{extend-var-env\ 377}#
                               #{labels\ 1278}#
                               #{vars\ 1276}#
                               #{r\ 1255}#)
                             (#{make-binding-wrap\ 434}#
                               #{ids\ 1274}#
                               #{labels\ 1278}#
                               #{w\ 1256}#)
                             #{mod\ 1258}#))))))))))
         (#{lambda-formals\ 490}#
           (lambda (#{orig-args\ 1281}#)
             (letrec*
               ((#{check\ 1286}#
                  (lambda (#{req\ 1287}# #{rest\ 1288}#)
                    (if (#{distinct-bound-ids?\ 452}#
                          (if #{rest\ 1288}#
                            (cons #{rest\ 1288}# #{req\ 1287}#)
                            #{req\ 1287}#))
                      (values #{req\ 1287}# #f #{rest\ 1288}# #f)
                      (syntax-violation
                        'lambda
                        "duplicate identifier in argument list"
                        #{orig-args\ 1281}#))))
                (#{req\ 1284}#
                  (lambda (#{args\ 1294}# #{rreq\ 1295}#)
                    (let ((#{tmp\ 1298}# #{args\ 1294}#))
                      (let ((#{tmp\ 1299}#
                              ($sc-dispatch #{tmp\ 1298}# (quote ()))))
                        (if #{tmp\ 1299}#
                          (@apply
                            (lambda ()
                              (#{check\ 1286}# (reverse #{rreq\ 1295}#) #f))
                            #{tmp\ 1299}#)
                          (let ((#{tmp\ 1300}#
                                  ($sc-dispatch
                                    #{tmp\ 1298}#
                                    '(any . any))))
                            (if (if #{tmp\ 1300}#
                                  (@apply
                                    (lambda (#{a\ 1303}# #{b\ 1304}#)
                                      (#{id?\ 387}# #{a\ 1303}#))
                                    #{tmp\ 1300}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 1307}# #{b\ 1308}#)
                                  (#{req\ 1284}#
                                    #{b\ 1308}#
                                    (cons #{a\ 1307}# #{rreq\ 1295}#)))
                                #{tmp\ 1300}#)
                              (let ((#{tmp\ 1309}# (list #{tmp\ 1298}#)))
                                (if (if #{tmp\ 1309}#
                                      (@apply
                                        (lambda (#{r\ 1311}#)
                                          (#{id?\ 387}# #{r\ 1311}#))
                                        #{tmp\ 1309}#)
                                      #f)
                                  (@apply
                                    (lambda (#{r\ 1313}#)
                                      (#{check\ 1286}#
                                        (reverse #{rreq\ 1295}#)
                                        #{r\ 1313}#))
                                    #{tmp\ 1309}#)
                                  (let ((#{else\ 1315}# #{tmp\ 1298}#))
                                    (syntax-violation
                                      'lambda
                                      "invalid argument list"
                                      #{orig-args\ 1281}#
                                      #{args\ 1294}#))))))))))))
               (begin
                 (#{req\ 1284}# #{orig-args\ 1281}# (quote ()))))))
         (#{ellipsis?\ 488}#
           (lambda (#{x\ 1316}#)
             (if (#{nonsymbol-id?\ 385}# #{x\ 1316}#)
               (#{free-id=?\ 446}#
                 #{x\ 1316}#
                 '#(syntax-object
                    ...
                    ((top)
                     #(ribcage () () ())
                     #(ribcage () () ())
                     #(ribcage #(x) #((top)) #("i1317"))
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
         (#{chi-void\ 486}#
           (lambda () (#{build-void\ 307}# #f)))
         (#{eval-local-transformer\ 484}#
           (lambda (#{expanded\ 1321}# #{mod\ 1322}#)
             (begin
               (let ((#{p\ 1326}#
                       (#{local-eval-hook\ 298}#
                         #{expanded\ 1321}#
                         #{mod\ 1322}#)))
                 (if (procedure? #{p\ 1326}#)
                   #{p\ 1326}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 1326}#))))))
         (#{chi-local-syntax\ 482}#
           (lambda (#{rec?\ 1327}#
                    #{e\ 1328}#
                    #{r\ 1329}#
                    #{w\ 1330}#
                    #{s\ 1331}#
                    #{mod\ 1332}#
                    #{k\ 1333}#)
             (let ((#{tmp\ 1341}# #{e\ 1328}#))
               (let ((#{tmp\ 1342}#
                       ($sc-dispatch
                         #{tmp\ 1341}#
                         '(any #(each (any any)) any . each-any))))
                 (if #{tmp\ 1342}#
                   (@apply
                     (lambda (#{_\ 1348}#
                              #{id\ 1349}#
                              #{val\ 1350}#
                              #{e1\ 1351}#
                              #{e2\ 1352}#)
                       (begin
                         (let ((#{ids\ 1354}# #{id\ 1349}#))
                           (if (not (#{valid-bound-ids?\ 450}# #{ids\ 1354}#))
                             (syntax-violation
                               #f
                               "duplicate bound keyword"
                               #{e\ 1328}#)
                             (begin
                               (let ((#{labels\ 1357}#
                                       (#{gen-labels\ 405}# #{ids\ 1354}#)))
                                 (begin
                                   (let ((#{new-w\ 1359}#
                                           (#{make-binding-wrap\ 434}#
                                             #{ids\ 1354}#
                                             #{labels\ 1357}#
                                             #{w\ 1330}#)))
                                     (#{k\ 1333}#
                                       (cons #{e1\ 1351}# #{e2\ 1352}#)
                                       (#{extend-env\ 375}#
                                         #{labels\ 1357}#
                                         (begin
                                           (let ((#{w\ 1363}#
                                                   (if #{rec?\ 1327}#
                                                     #{new-w\ 1359}#
                                                     #{w\ 1330}#))
                                                 (#{trans-r\ 1364}#
                                                   (#{macros-only-env\ 379}#
                                                     #{r\ 1329}#)))
                                             (map (lambda (#{x\ 1365}#)
                                                    (cons 'macro
                                                          (#{eval-local-transformer\ 484}#
                                                            (#{chi\ 472}#
                                                              #{x\ 1365}#
                                                              #{trans-r\ 1364}#
                                                              #{w\ 1363}#
                                                              #{mod\ 1332}#)
                                                            #{mod\ 1332}#)))
                                                  #{val\ 1350}#)))
                                         #{r\ 1329}#)
                                       #{new-w\ 1359}#
                                       #{s\ 1331}#
                                       #{mod\ 1332}#)))))))))
                     #{tmp\ 1342}#)
                   (let ((#{_\ 1370}# #{tmp\ 1341}#))
                     (syntax-violation
                       #f
                       "bad local syntax definition"
                       (#{source-wrap\ 458}#
                         #{e\ 1328}#
                         #{w\ 1330}#
                         #{s\ 1331}#
                         #{mod\ 1332}#))))))))
         (#{chi-body\ 480}#
           (lambda (#{body\ 1371}#
                    #{outer-form\ 1372}#
                    #{r\ 1373}#
                    #{w\ 1374}#
                    #{mod\ 1375}#)
             (begin
               (let ((#{r\ 1383}#
                       (cons '("placeholder" placeholder)
                             #{r\ 1373}#)))
                 (begin
                   (let ((#{ribcage\ 1385}#
                           (#{make-ribcage\ 408}#
                             '()
                             '()
                             '())))
                     (begin
                       (let ((#{w\ 1388}#
                               (#{make-wrap\ 392}#
                                 (#{wrap-marks\ 394}# #{w\ 1374}#)
                                 (cons #{ribcage\ 1385}#
                                       (#{wrap-subst\ 396}# #{w\ 1374}#)))))
                         (letrec*
                           ((#{parse\ 1397}#
                              (lambda (#{body\ 1398}#
                                       #{ids\ 1399}#
                                       #{labels\ 1400}#
                                       #{var-ids\ 1401}#
                                       #{vars\ 1402}#
                                       #{vals\ 1403}#
                                       #{bindings\ 1404}#)
                                (if (null? #{body\ 1398}#)
                                  (syntax-violation
                                    #f
                                    "no expressions in body"
                                    #{outer-form\ 1372}#)
                                  (begin
                                    (let ((#{e\ 1409}#
                                            (cdr (car #{body\ 1398}#)))
                                          (#{er\ 1410}#
                                            (car (car #{body\ 1398}#))))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 468}#
                                            #{e\ 1409}#
                                            #{er\ 1410}#
                                            '(())
                                            (#{source-annotation\ 366}#
                                              #{er\ 1410}#)
                                            #{ribcage\ 1385}#
                                            #{mod\ 1375}#
                                            #f))
                                        (lambda (#{type\ 1412}#
                                                 #{value\ 1413}#
                                                 #{e\ 1414}#
                                                 #{w\ 1415}#
                                                 #{s\ 1416}#
                                                 #{mod\ 1417}#)
                                          (if (eqv? #{type\ 1412}#
                                                    'define-form)
                                            (begin
                                              (let ((#{id\ 1427}#
                                                      (#{wrap\ 456}#
                                                        #{value\ 1413}#
                                                        #{w\ 1415}#
                                                        #{mod\ 1417}#))
                                                    (#{label\ 1428}#
                                                      (#{gen-label\ 403}#)))
                                                (begin
                                                  (let ((#{var\ 1430}#
                                                          (#{gen-var\ 500}#
                                                            #{id\ 1427}#)))
                                                    (begin
                                                      (#{extend-ribcage!\ 432}#
                                                        #{ribcage\ 1385}#
                                                        #{id\ 1427}#
                                                        #{label\ 1428}#)
                                                      (#{parse\ 1397}#
                                                        (cdr #{body\ 1398}#)
                                                        (cons #{id\ 1427}#
                                                              #{ids\ 1399}#)
                                                        (cons #{label\ 1428}#
                                                              #{labels\ 1400}#)
                                                        (cons #{id\ 1427}#
                                                              #{var-ids\ 1401}#)
                                                        (cons #{var\ 1430}#
                                                              #{vars\ 1402}#)
                                                        (cons (cons #{er\ 1410}#
                                                                    (#{wrap\ 456}#
                                                                      #{e\ 1414}#
                                                                      #{w\ 1415}#
                                                                      #{mod\ 1417}#))
                                                              #{vals\ 1403}#)
                                                        (cons (cons 'lexical
                                                                    #{var\ 1430}#)
                                                              #{bindings\ 1404}#)))))))
                                            (if (eqv? #{type\ 1412}#
                                                      'define-syntax-form)
                                              (begin
                                                (let ((#{id\ 1435}#
                                                        (#{wrap\ 456}#
                                                          #{value\ 1413}#
                                                          #{w\ 1415}#
                                                          #{mod\ 1417}#))
                                                      (#{label\ 1436}#
                                                        (#{gen-label\ 403}#)))
                                                  (begin
                                                    (#{extend-ribcage!\ 432}#
                                                      #{ribcage\ 1385}#
                                                      #{id\ 1435}#
                                                      #{label\ 1436}#)
                                                    (#{parse\ 1397}#
                                                      (cdr #{body\ 1398}#)
                                                      (cons #{id\ 1435}#
                                                            #{ids\ 1399}#)
                                                      (cons #{label\ 1436}#
                                                            #{labels\ 1400}#)
                                                      #{var-ids\ 1401}#
                                                      #{vars\ 1402}#
                                                      #{vals\ 1403}#
                                                      (cons (cons 'macro
                                                                  (cons #{er\ 1410}#
                                                                        (#{wrap\ 456}#
                                                                          #{e\ 1414}#
                                                                          #{w\ 1415}#
                                                                          #{mod\ 1417}#)))
                                                            #{bindings\ 1404}#)))))
                                              (if (eqv? #{type\ 1412}#
                                                        'begin-form)
                                                (let ((#{tmp\ 1439}#
                                                        #{e\ 1414}#))
                                                  (let ((#{tmp\ 1440}#
                                                          ($sc-dispatch
                                                            #{tmp\ 1439}#
                                                            '(any .
                                                                  each-any))))
                                                    (if #{tmp\ 1440}#
                                                      (@apply
                                                        (lambda (#{_\ 1443}#
                                                                 #{e1\ 1444}#)
                                                          (#{parse\ 1397}#
                                                            (letrec*
                                                              ((#{f\ 1447}#
                                                                 (lambda (#{forms\ 1448}#)
                                                                   (if (null? #{forms\ 1448}#)
                                                                     (cdr #{body\ 1398}#)
                                                                     (cons (cons #{er\ 1410}#
                                                                                 (#{wrap\ 456}#
                                                                                   (car #{forms\ 1448}#)
                                                                                   #{w\ 1415}#
                                                                                   #{mod\ 1417}#))
                                                                           (#{f\ 1447}#
                                                                             (cdr #{forms\ 1448}#)))))))
                                                              (begin
                                                                (#{f\ 1447}#
                                                                  #{e1\ 1444}#)))
                                                            #{ids\ 1399}#
                                                            #{labels\ 1400}#
                                                            #{var-ids\ 1401}#
                                                            #{vars\ 1402}#
                                                            #{vals\ 1403}#
                                                            #{bindings\ 1404}#))
                                                        #{tmp\ 1440}#)
                                                      (syntax-violation
                                                        #f
                                                        "source expression failed to match any pattern"
                                                        #{tmp\ 1439}#))))
                                                (if (eqv? #{type\ 1412}#
                                                          'local-syntax-form)
                                                  (#{chi-local-syntax\ 482}#
                                                    #{value\ 1413}#
                                                    #{e\ 1414}#
                                                    #{er\ 1410}#
                                                    #{w\ 1415}#
                                                    #{s\ 1416}#
                                                    #{mod\ 1417}#
                                                    (lambda (#{forms\ 1451}#
                                                             #{er\ 1452}#
                                                             #{w\ 1453}#
                                                             #{s\ 1454}#
                                                             #{mod\ 1455}#)
                                                      (#{parse\ 1397}#
                                                        (letrec*
                                                          ((#{f\ 1463}#
                                                             (lambda (#{forms\ 1464}#)
                                                               (if (null? #{forms\ 1464}#)
                                                                 (cdr #{body\ 1398}#)
                                                                 (cons (cons #{er\ 1452}#
                                                                             (#{wrap\ 456}#
                                                                               (car #{forms\ 1464}#)
                                                                               #{w\ 1453}#
                                                                               #{mod\ 1455}#))
                                                                       (#{f\ 1463}#
                                                                         (cdr #{forms\ 1464}#)))))))
                                                          (begin
                                                            (#{f\ 1463}#
                                                              #{forms\ 1451}#)))
                                                        #{ids\ 1399}#
                                                        #{labels\ 1400}#
                                                        #{var-ids\ 1401}#
                                                        #{vars\ 1402}#
                                                        #{vals\ 1403}#
                                                        #{bindings\ 1404}#)))
                                                  (if (null? #{ids\ 1399}#)
                                                    (#{build-sequence\ 339}#
                                                      #f
                                                      (map (lambda (#{x\ 1467}#)
                                                             (#{chi\ 472}#
                                                               (cdr #{x\ 1467}#)
                                                               (car #{x\ 1467}#)
                                                               '(())
                                                               #{mod\ 1417}#))
                                                           (cons (cons #{er\ 1410}#
                                                                       (#{source-wrap\ 458}#
                                                                         #{e\ 1414}#
                                                                         #{w\ 1415}#
                                                                         #{s\ 1416}#
                                                                         #{mod\ 1417}#))
                                                                 (cdr #{body\ 1398}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 450}#
                                                                 #{ids\ 1399}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 1372}#))
                                                      (letrec*
                                                        ((#{loop\ 1474}#
                                                           (lambda (#{bs\ 1475}#
                                                                    #{er-cache\ 1476}#
                                                                    #{r-cache\ 1477}#)
                                                             (if (not (null? #{bs\ 1475}#))
                                                               (begin
                                                                 (let ((#{b\ 1480}#
                                                                         (car #{bs\ 1475}#)))
                                                                   (if (eq? (car #{b\ 1480}#)
                                                                            'macro)
                                                                     (begin
                                                                       (let ((#{er\ 1483}#
                                                                               (car (cdr #{b\ 1480}#))))
                                                                         (begin
                                                                           (let ((#{r-cache\ 1485}#
                                                                                   (if (eq? #{er\ 1483}#
                                                                                            #{er-cache\ 1476}#)
                                                                                     #{r-cache\ 1477}#
                                                                                     (#{macros-only-env\ 379}#
                                                                                       #{er\ 1483}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 1480}#
                                                                                 (#{eval-local-transformer\ 484}#
                                                                                   (#{chi\ 472}#
                                                                                     (cdr (cdr #{b\ 1480}#))
                                                                                     #{r-cache\ 1485}#
                                                                                     '(())
                                                                                     #{mod\ 1417}#)
                                                                                   #{mod\ 1417}#))
                                                                               (#{loop\ 1474}#
                                                                                 (cdr #{bs\ 1475}#)
                                                                                 #{er\ 1483}#
                                                                                 #{r-cache\ 1485}#))))))
                                                                     (#{loop\ 1474}#
                                                                       (cdr #{bs\ 1475}#)
                                                                       #{er-cache\ 1476}#
                                                                       #{r-cache\ 1477}#))))))))
                                                        (begin
                                                          (#{loop\ 1474}#
                                                            #{bindings\ 1404}#
                                                            #f
                                                            #f)))
                                                      (set-cdr!
                                                        #{r\ 1383}#
                                                        (#{extend-env\ 375}#
                                                          #{labels\ 1400}#
                                                          #{bindings\ 1404}#
                                                          (cdr #{r\ 1383}#)))
                                                      (#{build-letrec\ 345}#
                                                        #f
                                                        #t
                                                        (map syntax->datum
                                                             #{var-ids\ 1401}#)
                                                        #{vars\ 1402}#
                                                        (map (lambda (#{x\ 1488}#)
                                                               (#{chi\ 472}#
                                                                 (cdr #{x\ 1488}#)
                                                                 (car #{x\ 1488}#)
                                                                 '(())
                                                                 #{mod\ 1417}#))
                                                             #{vals\ 1403}#)
                                                        (#{build-sequence\ 339}#
                                                          #f
                                                          (map (lambda (#{x\ 1492}#)
                                                                 (#{chi\ 472}#
                                                                   (cdr #{x\ 1492}#)
                                                                   (car #{x\ 1492}#)
                                                                   '(())
                                                                   #{mod\ 1417}#))
                                                               (cons (cons #{er\ 1410}#
                                                                           (#{source-wrap\ 458}#
                                                                             #{e\ 1414}#
                                                                             #{w\ 1415}#
                                                                             #{s\ 1416}#
                                                                             #{mod\ 1417}#))
                                                                     (cdr #{body\ 1398}#)))))))))))))))))))
                           (begin
                             (#{parse\ 1397}#
                               (map (lambda (#{x\ 1405}#)
                                      (cons #{r\ 1383}#
                                            (#{wrap\ 456}#
                                              #{x\ 1405}#
                                              #{w\ 1388}#
                                              #{mod\ 1375}#)))
                                    #{body\ 1371}#)
                               '()
                               '()
                               '()
                               '()
                               '()
                               '())))))))))))
         (#{chi-macro\ 478}#
           (lambda (#{p\ 1495}#
                    #{e\ 1496}#
                    #{r\ 1497}#
                    #{w\ 1498}#
                    #{s\ 1499}#
                    #{rib\ 1500}#
                    #{mod\ 1501}#)
             (letrec*
               ((#{rebuild-macro-output\ 1510}#
                  (lambda (#{x\ 1511}# #{m\ 1512}#)
                    (if (pair? #{x\ 1511}#)
                      (#{decorate-source\ 305}#
                        (cons (#{rebuild-macro-output\ 1510}#
                                (car #{x\ 1511}#)
                                #{m\ 1512}#)
                              (#{rebuild-macro-output\ 1510}#
                                (cdr #{x\ 1511}#)
                                #{m\ 1512}#))
                        #{s\ 1499}#)
                      (if (#{syntax-object?\ 351}# #{x\ 1511}#)
                        (begin
                          (let ((#{w\ 1520}#
                                  (#{syntax-object-wrap\ 355}# #{x\ 1511}#)))
                            (begin
                              (let ((#{ms\ 1523}#
                                      (#{wrap-marks\ 394}# #{w\ 1520}#))
                                    (#{s\ 1524}#
                                      (#{wrap-subst\ 396}# #{w\ 1520}#)))
                                (if (if (pair? #{ms\ 1523}#)
                                      (eq? (car #{ms\ 1523}#) #f)
                                      #f)
                                  (#{make-syntax-object\ 349}#
                                    (#{syntax-object-expression\ 353}#
                                      #{x\ 1511}#)
                                    (#{make-wrap\ 392}#
                                      (cdr #{ms\ 1523}#)
                                      (if #{rib\ 1500}#
                                        (cons #{rib\ 1500}# (cdr #{s\ 1524}#))
                                        (cdr #{s\ 1524}#)))
                                    (#{syntax-object-module\ 357}#
                                      #{x\ 1511}#))
                                  (#{make-syntax-object\ 349}#
                                    (#{decorate-source\ 305}#
                                      (#{syntax-object-expression\ 353}#
                                        #{x\ 1511}#)
                                      #{s\ 1524}#)
                                    (#{make-wrap\ 392}#
                                      (cons #{m\ 1512}# #{ms\ 1523}#)
                                      (if #{rib\ 1500}#
                                        (cons #{rib\ 1500}#
                                              (cons (quote shift) #{s\ 1524}#))
                                        (cons (quote shift) #{s\ 1524}#)))
                                    (#{syntax-object-module\ 357}#
                                      #{x\ 1511}#)))))))
                        (if (vector? #{x\ 1511}#)
                          (begin
                            (let ((#{n\ 1532}# (vector-length #{x\ 1511}#)))
                              (begin
                                (let ((#{v\ 1534}#
                                        (#{decorate-source\ 305}#
                                          (make-vector #{n\ 1532}#)
                                          #{x\ 1511}#)))
                                  (letrec*
                                    ((#{loop\ 1537}#
                                       (lambda (#{i\ 1538}#)
                                         (if (#{fx=\ 292}#
                                               #{i\ 1538}#
                                               #{n\ 1532}#)
                                           (begin (if #f #f) #{v\ 1534}#)
                                           (begin
                                             (vector-set!
                                               #{v\ 1534}#
                                               #{i\ 1538}#
                                               (#{rebuild-macro-output\ 1510}#
                                                 (vector-ref
                                                   #{x\ 1511}#
                                                   #{i\ 1538}#)
                                                 #{m\ 1512}#))
                                             (#{loop\ 1537}#
                                               (#{fx+\ 288}#
                                                 #{i\ 1538}#
                                                 1)))))))
                                    (begin (#{loop\ 1537}# 0)))))))
                          (if (symbol? #{x\ 1511}#)
                            (syntax-violation
                              #f
                              "encountered raw symbol in macro output"
                              (#{source-wrap\ 458}#
                                #{e\ 1496}#
                                #{w\ 1498}#
                                (#{wrap-subst\ 396}# #{w\ 1498}#)
                                #{mod\ 1501}#)
                              #{x\ 1511}#)
                            (#{decorate-source\ 305}#
                              #{x\ 1511}#
                              #{s\ 1499}#))))))))
               (begin
                 (#{rebuild-macro-output\ 1510}#
                   (#{p\ 1495}#
                     (#{source-wrap\ 458}#
                       #{e\ 1496}#
                       (#{anti-mark\ 428}# #{w\ 1498}#)
                       #{s\ 1499}#
                       #{mod\ 1501}#))
                   (gensym "m"))))))
         (#{chi-application\ 476}#
           (lambda (#{x\ 1545}#
                    #{e\ 1546}#
                    #{r\ 1547}#
                    #{w\ 1548}#
                    #{s\ 1549}#
                    #{mod\ 1550}#)
             (let ((#{tmp\ 1557}# #{e\ 1546}#))
               (let ((#{tmp\ 1558}#
                       ($sc-dispatch
                         #{tmp\ 1557}#
                         '(any . each-any))))
                 (if #{tmp\ 1558}#
                   (@apply
                     (lambda (#{e0\ 1561}# #{e1\ 1562}#)
                       (#{build-application\ 309}#
                         #{s\ 1549}#
                         #{x\ 1545}#
                         (map (lambda (#{e\ 1563}#)
                                (#{chi\ 472}#
                                  #{e\ 1563}#
                                  #{r\ 1547}#
                                  #{w\ 1548}#
                                  #{mod\ 1550}#))
                              #{e1\ 1562}#)))
                     #{tmp\ 1558}#)
                   (syntax-violation
                     #f
                     "source expression failed to match any pattern"
                     #{tmp\ 1557}#))))))
         (#{chi-expr\ 474}#
           (lambda (#{type\ 1566}#
                    #{value\ 1567}#
                    #{e\ 1568}#
                    #{r\ 1569}#
                    #{w\ 1570}#
                    #{s\ 1571}#
                    #{mod\ 1572}#)
             (if (eqv? #{type\ 1566}# (quote lexical))
               (#{build-lexical-reference\ 315}#
                 'value
                 #{s\ 1571}#
                 #{e\ 1568}#
                 #{value\ 1567}#)
               (if (if (eqv? #{type\ 1566}# (quote core))
                     #t
                     (eqv? #{type\ 1566}# (quote core-form)))
                 (#{value\ 1567}#
                   #{e\ 1568}#
                   #{r\ 1569}#
                   #{w\ 1570}#
                   #{s\ 1571}#
                   #{mod\ 1572}#)
                 (if (eqv? #{type\ 1566}# (quote module-ref))
                   (call-with-values
                     (lambda ()
                       (#{value\ 1567}#
                         #{e\ 1568}#
                         #{r\ 1569}#
                         #{w\ 1570}#))
                     (lambda (#{e\ 1583}#
                              #{r\ 1584}#
                              #{w\ 1585}#
                              #{s\ 1586}#
                              #{mod\ 1587}#)
                       (#{chi\ 472}#
                         #{e\ 1583}#
                         #{r\ 1584}#
                         #{w\ 1585}#
                         #{mod\ 1587}#)))
                   (if (eqv? #{type\ 1566}# (quote lexical-call))
                     (#{chi-application\ 476}#
                       (begin
                         (let ((#{id\ 1595}# (car #{e\ 1568}#)))
                           (#{build-lexical-reference\ 315}#
                             'fun
                             (#{source-annotation\ 366}# #{id\ 1595}#)
                             (if (#{syntax-object?\ 351}# #{id\ 1595}#)
                               (syntax->datum #{id\ 1595}#)
                               #{id\ 1595}#)
                             #{value\ 1567}#)))
                       #{e\ 1568}#
                       #{r\ 1569}#
                       #{w\ 1570}#
                       #{s\ 1571}#
                       #{mod\ 1572}#)
                     (if (eqv? #{type\ 1566}# (quote global-call))
                       (#{chi-application\ 476}#
                         (#{build-global-reference\ 321}#
                           (#{source-annotation\ 366}# (car #{e\ 1568}#))
                           (if (#{syntax-object?\ 351}# #{value\ 1567}#)
                             (#{syntax-object-expression\ 353}#
                               #{value\ 1567}#)
                             #{value\ 1567}#)
                           (if (#{syntax-object?\ 351}# #{value\ 1567}#)
                             (#{syntax-object-module\ 357}# #{value\ 1567}#)
                             #{mod\ 1572}#))
                         #{e\ 1568}#
                         #{r\ 1569}#
                         #{w\ 1570}#
                         #{s\ 1571}#
                         #{mod\ 1572}#)
                       (if (eqv? #{type\ 1566}# (quote constant))
                         (#{build-data\ 337}#
                           #{s\ 1571}#
                           (#{strip\ 498}#
                             (#{source-wrap\ 458}#
                               #{e\ 1568}#
                               #{w\ 1570}#
                               #{s\ 1571}#
                               #{mod\ 1572}#)
                             '(())))
                         (if (eqv? #{type\ 1566}# (quote global))
                           (#{build-global-reference\ 321}#
                             #{s\ 1571}#
                             #{value\ 1567}#
                             #{mod\ 1572}#)
                           (if (eqv? #{type\ 1566}# (quote call))
                             (#{chi-application\ 476}#
                               (#{chi\ 472}#
                                 (car #{e\ 1568}#)
                                 #{r\ 1569}#
                                 #{w\ 1570}#
                                 #{mod\ 1572}#)
                               #{e\ 1568}#
                               #{r\ 1569}#
                               #{w\ 1570}#
                               #{s\ 1571}#
                               #{mod\ 1572}#)
                             (if (eqv? #{type\ 1566}# (quote begin-form))
                               (let ((#{tmp\ 1602}# #{e\ 1568}#))
                                 (let ((#{tmp\ 1603}#
                                         ($sc-dispatch
                                           #{tmp\ 1602}#
                                           '(any any . each-any))))
                                   (if #{tmp\ 1603}#
                                     (@apply
                                       (lambda (#{_\ 1607}#
                                                #{e1\ 1608}#
                                                #{e2\ 1609}#)
                                         (#{chi-sequence\ 460}#
                                           (cons #{e1\ 1608}# #{e2\ 1609}#)
                                           #{r\ 1569}#
                                           #{w\ 1570}#
                                           #{s\ 1571}#
                                           #{mod\ 1572}#))
                                       #{tmp\ 1603}#)
                                     (syntax-violation
                                       #f
                                       "source expression failed to match any pattern"
                                       #{tmp\ 1602}#))))
                               (if (eqv? #{type\ 1566}#
                                         'local-syntax-form)
                                 (#{chi-local-syntax\ 482}#
                                   #{value\ 1567}#
                                   #{e\ 1568}#
                                   #{r\ 1569}#
                                   #{w\ 1570}#
                                   #{s\ 1571}#
                                   #{mod\ 1572}#
                                   #{chi-sequence\ 460}#)
                                 (if (eqv? #{type\ 1566}#
                                           'eval-when-form)
                                   (let ((#{tmp\ 1613}# #{e\ 1568}#))
                                     (let ((#{tmp\ 1614}#
                                             ($sc-dispatch
                                               #{tmp\ 1613}#
                                               '(any each-any
                                                     any
                                                     .
                                                     each-any))))
                                       (if #{tmp\ 1614}#
                                         (@apply
                                           (lambda (#{_\ 1619}#
                                                    #{x\ 1620}#
                                                    #{e1\ 1621}#
                                                    #{e2\ 1622}#)
                                             (begin
                                               (let ((#{when-list\ 1624}#
                                                       (#{chi-when-list\ 466}#
                                                         #{e\ 1568}#
                                                         #{x\ 1620}#
                                                         #{w\ 1570}#)))
                                                 (if (memq 'eval
                                                           #{when-list\ 1624}#)
                                                   (#{chi-sequence\ 460}#
                                                     (cons #{e1\ 1621}#
                                                           #{e2\ 1622}#)
                                                     #{r\ 1569}#
                                                     #{w\ 1570}#
                                                     #{s\ 1571}#
                                                     #{mod\ 1572}#)
                                                   (#{chi-void\ 486}#)))))
                                           #{tmp\ 1614}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 1613}#))))
                                   (if (if (eqv? #{type\ 1566}#
                                                 'define-form)
                                         #t
                                         (eqv? #{type\ 1566}#
                                               'define-syntax-form))
                                     (syntax-violation
                                       #f
                                       "definition in expression context"
                                       #{e\ 1568}#
                                       (#{wrap\ 456}#
                                         #{value\ 1567}#
                                         #{w\ 1570}#
                                         #{mod\ 1572}#))
                                     (if (eqv? #{type\ 1566}# (quote syntax))
                                       (syntax-violation
                                         #f
                                         "reference to pattern variable outside syntax form"
                                         (#{source-wrap\ 458}#
                                           #{e\ 1568}#
                                           #{w\ 1570}#
                                           #{s\ 1571}#
                                           #{mod\ 1572}#))
                                       (if (eqv? #{type\ 1566}#
                                                 'displaced-lexical)
                                         (syntax-violation
                                           #f
                                           "reference to identifier outside its scope"
                                           (#{source-wrap\ 458}#
                                             #{e\ 1568}#
                                             #{w\ 1570}#
                                             #{s\ 1571}#
                                             #{mod\ 1572}#))
                                         (syntax-violation
                                           #f
                                           "unexpected syntax"
                                           (#{source-wrap\ 458}#
                                             #{e\ 1568}#
                                             #{w\ 1570}#
                                             #{s\ 1571}#
                                             #{mod\ 1572}#))))))))))))))))))
         (#{chi\ 472}#
           (lambda (#{e\ 1631}#
                    #{r\ 1632}#
                    #{w\ 1633}#
                    #{mod\ 1634}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 468}#
                   #{e\ 1631}#
                   #{r\ 1632}#
                   #{w\ 1633}#
                   (#{source-annotation\ 366}# #{e\ 1631}#)
                   #f
                   #{mod\ 1634}#
                   #f))
               (lambda (#{type\ 1639}#
                        #{value\ 1640}#
                        #{e\ 1641}#
                        #{w\ 1642}#
                        #{s\ 1643}#
                        #{mod\ 1644}#)
                 (#{chi-expr\ 474}#
                   #{type\ 1639}#
                   #{value\ 1640}#
                   #{e\ 1641}#
                   #{r\ 1632}#
                   #{w\ 1642}#
                   #{s\ 1643}#
                   #{mod\ 1644}#)))))
         (#{chi-top\ 470}#
           (lambda (#{e\ 1651}#
                    #{r\ 1652}#
                    #{w\ 1653}#
                    #{m\ 1654}#
                    #{esew\ 1655}#
                    #{mod\ 1656}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 468}#
                   #{e\ 1651}#
                   #{r\ 1652}#
                   #{w\ 1653}#
                   (#{source-annotation\ 366}# #{e\ 1651}#)
                   #f
                   #{mod\ 1656}#
                   #f))
               (lambda (#{type\ 1677}#
                        #{value\ 1678}#
                        #{e\ 1679}#
                        #{w\ 1680}#
                        #{s\ 1681}#
                        #{mod\ 1682}#)
                 (if (eqv? #{type\ 1677}# (quote begin-form))
                   (let ((#{tmp\ 1690}# #{e\ 1679}#))
                     (let ((#{tmp\ 1691}#
                             ($sc-dispatch #{tmp\ 1690}# (quote (any)))))
                       (if #{tmp\ 1691}#
                         (@apply
                           (lambda (#{_\ 1693}#) (#{chi-void\ 486}#))
                           #{tmp\ 1691}#)
                         (let ((#{tmp\ 1694}#
                                 ($sc-dispatch
                                   #{tmp\ 1690}#
                                   '(any any . each-any))))
                           (if #{tmp\ 1694}#
                             (@apply
                               (lambda (#{_\ 1698}# #{e1\ 1699}# #{e2\ 1700}#)
                                 (#{chi-top-sequence\ 462}#
                                   (cons #{e1\ 1699}# #{e2\ 1700}#)
                                   #{r\ 1652}#
                                   #{w\ 1680}#
                                   #{s\ 1681}#
                                   #{m\ 1654}#
                                   #{esew\ 1655}#
                                   #{mod\ 1682}#))
                               #{tmp\ 1694}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 1690}#))))))
                   (if (eqv? #{type\ 1677}# (quote local-syntax-form))
                     (#{chi-local-syntax\ 482}#
                       #{value\ 1678}#
                       #{e\ 1679}#
                       #{r\ 1652}#
                       #{w\ 1680}#
                       #{s\ 1681}#
                       #{mod\ 1682}#
                       (lambda (#{body\ 1703}#
                                #{r\ 1704}#
                                #{w\ 1705}#
                                #{s\ 1706}#
                                #{mod\ 1707}#)
                         (#{chi-top-sequence\ 462}#
                           #{body\ 1703}#
                           #{r\ 1704}#
                           #{w\ 1705}#
                           #{s\ 1706}#
                           #{m\ 1654}#
                           #{esew\ 1655}#
                           #{mod\ 1707}#)))
                     (if (eqv? #{type\ 1677}# (quote eval-when-form))
                       (let ((#{tmp\ 1714}# #{e\ 1679}#))
                         (let ((#{tmp\ 1715}#
                                 ($sc-dispatch
                                   #{tmp\ 1714}#
                                   '(any each-any any . each-any))))
                           (if #{tmp\ 1715}#
                             (@apply
                               (lambda (#{_\ 1720}#
                                        #{x\ 1721}#
                                        #{e1\ 1722}#
                                        #{e2\ 1723}#)
                                 (begin
                                   (let ((#{when-list\ 1726}#
                                           (#{chi-when-list\ 466}#
                                             #{e\ 1679}#
                                             #{x\ 1721}#
                                             #{w\ 1680}#))
                                         (#{body\ 1727}#
                                           (cons #{e1\ 1722}# #{e2\ 1723}#)))
                                     (if (eq? #{m\ 1654}# (quote e))
                                       (if (memq 'eval
                                                 #{when-list\ 1726}#)
                                         (#{chi-top-sequence\ 462}#
                                           #{body\ 1727}#
                                           #{r\ 1652}#
                                           #{w\ 1680}#
                                           #{s\ 1681}#
                                           (if (memq 'expand
                                                     #{when-list\ 1726}#)
                                             'c&e
                                             'e)
                                           '(eval)
                                           #{mod\ 1682}#)
                                         (begin
                                           (if (memq 'expand
                                                     #{when-list\ 1726}#)
                                             (#{top-level-eval-hook\ 296}#
                                               (#{chi-top-sequence\ 462}#
                                                 #{body\ 1727}#
                                                 #{r\ 1652}#
                                                 #{w\ 1680}#
                                                 #{s\ 1681}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 1682}#)
                                               #{mod\ 1682}#))
                                           (#{chi-void\ 486}#)))
                                       (if (memq 'load
                                                 #{when-list\ 1726}#)
                                         (if (begin
                                               (let ((#{t\ 1736}#
                                                       (memq 'compile
                                                             #{when-list\ 1726}#)))
                                                 (if #{t\ 1736}#
                                                   #{t\ 1736}#
                                                   (begin
                                                     (let ((#{t\ 1739}#
                                                             (memq 'expand
                                                                   #{when-list\ 1726}#)))
                                                       (if #{t\ 1739}#
                                                         #{t\ 1739}#
                                                         (if (eq? #{m\ 1654}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 1726}#)
                                                           #f)))))))
                                           (#{chi-top-sequence\ 462}#
                                             #{body\ 1727}#
                                             #{r\ 1652}#
                                             #{w\ 1680}#
                                             #{s\ 1681}#
                                             'c&e
                                             '(compile load)
                                             #{mod\ 1682}#)
                                           (if (if (eq? #{m\ 1654}# (quote c))
                                                 #t
                                                 (eq? #{m\ 1654}# (quote c&e)))
                                             (#{chi-top-sequence\ 462}#
                                               #{body\ 1727}#
                                               #{r\ 1652}#
                                               #{w\ 1680}#
                                               #{s\ 1681}#
                                               'c
                                               '(load)
                                               #{mod\ 1682}#)
                                             (#{chi-void\ 486}#)))
                                         (if (begin
                                               (let ((#{t\ 1747}#
                                                       (memq 'compile
                                                             #{when-list\ 1726}#)))
                                                 (if #{t\ 1747}#
                                                   #{t\ 1747}#
                                                   (begin
                                                     (let ((#{t\ 1750}#
                                                             (memq 'expand
                                                                   #{when-list\ 1726}#)))
                                                       (if #{t\ 1750}#
                                                         #{t\ 1750}#
                                                         (if (eq? #{m\ 1654}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 1726}#)
                                                           #f)))))))
                                           (begin
                                             (#{top-level-eval-hook\ 296}#
                                               (#{chi-top-sequence\ 462}#
                                                 #{body\ 1727}#
                                                 #{r\ 1652}#
                                                 #{w\ 1680}#
                                                 #{s\ 1681}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 1682}#)
                                               #{mod\ 1682}#)
                                             (#{chi-void\ 486}#))
                                           (#{chi-void\ 486}#)))))))
                               #{tmp\ 1715}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 1714}#))))
                       (if (eqv? #{type\ 1677}# (quote define-syntax-form))
                         (begin
                           (let ((#{n\ 1758}#
                                   (#{id-var-name\ 444}#
                                     #{value\ 1678}#
                                     #{w\ 1680}#))
                                 (#{r\ 1759}#
                                   (#{macros-only-env\ 379}# #{r\ 1652}#)))
                             (if (eqv? #{m\ 1654}# (quote c))
                               (if (memq (quote compile) #{esew\ 1655}#)
                                 (begin
                                   (let ((#{e\ 1762}#
                                           (#{chi-install-global\ 464}#
                                             #{n\ 1758}#
                                             (#{chi\ 472}#
                                               #{e\ 1679}#
                                               #{r\ 1759}#
                                               #{w\ 1680}#
                                               #{mod\ 1682}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 296}#
                                         #{e\ 1762}#
                                         #{mod\ 1682}#)
                                       (if (memq (quote load) #{esew\ 1655}#)
                                         #{e\ 1762}#
                                         (#{chi-void\ 486}#)))))
                                 (if (memq (quote load) #{esew\ 1655}#)
                                   (#{chi-install-global\ 464}#
                                     #{n\ 1758}#
                                     (#{chi\ 472}#
                                       #{e\ 1679}#
                                       #{r\ 1759}#
                                       #{w\ 1680}#
                                       #{mod\ 1682}#))
                                   (#{chi-void\ 486}#)))
                               (if (eqv? #{m\ 1654}# (quote c&e))
                                 (begin
                                   (let ((#{e\ 1765}#
                                           (#{chi-install-global\ 464}#
                                             #{n\ 1758}#
                                             (#{chi\ 472}#
                                               #{e\ 1679}#
                                               #{r\ 1759}#
                                               #{w\ 1680}#
                                               #{mod\ 1682}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 296}#
                                         #{e\ 1765}#
                                         #{mod\ 1682}#)
                                       #{e\ 1765}#)))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 1655}#)
                                     (#{top-level-eval-hook\ 296}#
                                       (#{chi-install-global\ 464}#
                                         #{n\ 1758}#
                                         (#{chi\ 472}#
                                           #{e\ 1679}#
                                           #{r\ 1759}#
                                           #{w\ 1680}#
                                           #{mod\ 1682}#))
                                       #{mod\ 1682}#))
                                   (#{chi-void\ 486}#))))))
                         (if (eqv? #{type\ 1677}# (quote define-form))
                           (begin
                             (let ((#{n\ 1770}#
                                     (#{id-var-name\ 444}#
                                       #{value\ 1678}#
                                       #{w\ 1680}#)))
                               (begin
                                 (let ((#{type\ 1772}#
                                         (#{binding-type\ 370}#
                                           (#{lookup\ 381}#
                                             #{n\ 1770}#
                                             #{r\ 1652}#
                                             #{mod\ 1682}#))))
                                   (if (if (eqv? #{type\ 1772}# (quote global))
                                         #t
                                         (if (eqv? #{type\ 1772}# (quote core))
                                           #t
                                           (if (eqv? #{type\ 1772}#
                                                     'macro)
                                             #t
                                             (eqv? #{type\ 1772}#
                                                   'module-ref))))
                                     (begin
                                       (if (if (if (eq? #{m\ 1654}# (quote c))
                                                 #t
                                                 (eq? #{m\ 1654}# (quote c&e)))
                                             (if (not (module-local-variable
                                                        (current-module)
                                                        #{n\ 1770}#))
                                               (current-module)
                                               #f)
                                             #f)
                                         (begin
                                           (let ((#{old\ 1778}#
                                                   (module-variable
                                                     (current-module)
                                                     #{n\ 1770}#)))
                                             (module-define!
                                               (current-module)
                                               #{n\ 1770}#
                                               (if (variable? #{old\ 1778}#)
                                                 (variable-ref #{old\ 1778}#)
                                                 #f)))))
                                       (begin
                                         (let ((#{x\ 1781}#
                                                 (#{build-global-definition\ 327}#
                                                   #{s\ 1681}#
                                                   #{n\ 1770}#
                                                   (#{chi\ 472}#
                                                     #{e\ 1679}#
                                                     #{r\ 1652}#
                                                     #{w\ 1680}#
                                                     #{mod\ 1682}#))))
                                           (begin
                                             (if (eq? #{m\ 1654}# (quote c&e))
                                               (#{top-level-eval-hook\ 296}#
                                                 #{x\ 1781}#
                                                 #{mod\ 1682}#))
                                             #{x\ 1781}#))))
                                     (if (eqv? #{type\ 1772}#
                                               'displaced-lexical)
                                       (syntax-violation
                                         #f
                                         "identifier out of context"
                                         #{e\ 1679}#
                                         (#{wrap\ 456}#
                                           #{value\ 1678}#
                                           #{w\ 1680}#
                                           #{mod\ 1682}#))
                                       (syntax-violation
                                         #f
                                         "cannot define keyword at top level"
                                         #{e\ 1679}#
                                         (#{wrap\ 456}#
                                           #{value\ 1678}#
                                           #{w\ 1680}#
                                           #{mod\ 1682}#))))))))
                           (begin
                             (let ((#{x\ 1787}#
                                     (#{chi-expr\ 474}#
                                       #{type\ 1677}#
                                       #{value\ 1678}#
                                       #{e\ 1679}#
                                       #{r\ 1652}#
                                       #{w\ 1680}#
                                       #{s\ 1681}#
                                       #{mod\ 1682}#)))
                               (begin
                                 (if (eq? #{m\ 1654}# (quote c&e))
                                   (#{top-level-eval-hook\ 296}#
                                     #{x\ 1787}#
                                     #{mod\ 1682}#))
                                 #{x\ 1787}#))))))))))))
         (#{syntax-type\ 468}#
           (lambda (#{e\ 1788}#
                    #{r\ 1789}#
                    #{w\ 1790}#
                    #{s\ 1791}#
                    #{rib\ 1792}#
                    #{mod\ 1793}#
                    #{for-car?\ 1794}#)
             (if (symbol? #{e\ 1788}#)
               (begin
                 (let ((#{n\ 1806}#
                         (#{id-var-name\ 444}# #{e\ 1788}# #{w\ 1790}#)))
                   (begin
                     (let ((#{b\ 1808}#
                             (#{lookup\ 381}#
                               #{n\ 1806}#
                               #{r\ 1789}#
                               #{mod\ 1793}#)))
                       (begin
                         (let ((#{type\ 1810}#
                                 (#{binding-type\ 370}# #{b\ 1808}#)))
                           (if (eqv? #{type\ 1810}# (quote lexical))
                             (values
                               #{type\ 1810}#
                               (#{binding-value\ 372}# #{b\ 1808}#)
                               #{e\ 1788}#
                               #{w\ 1790}#
                               #{s\ 1791}#
                               #{mod\ 1793}#)
                             (if (eqv? #{type\ 1810}# (quote global))
                               (values
                                 #{type\ 1810}#
                                 #{n\ 1806}#
                                 #{e\ 1788}#
                                 #{w\ 1790}#
                                 #{s\ 1791}#
                                 #{mod\ 1793}#)
                               (if (eqv? #{type\ 1810}# (quote macro))
                                 (if #{for-car?\ 1794}#
                                   (values
                                     #{type\ 1810}#
                                     (#{binding-value\ 372}# #{b\ 1808}#)
                                     #{e\ 1788}#
                                     #{w\ 1790}#
                                     #{s\ 1791}#
                                     #{mod\ 1793}#)
                                   (#{syntax-type\ 468}#
                                     (#{chi-macro\ 478}#
                                       (#{binding-value\ 372}# #{b\ 1808}#)
                                       #{e\ 1788}#
                                       #{r\ 1789}#
                                       #{w\ 1790}#
                                       #{s\ 1791}#
                                       #{rib\ 1792}#
                                       #{mod\ 1793}#)
                                     #{r\ 1789}#
                                     '(())
                                     #{s\ 1791}#
                                     #{rib\ 1792}#
                                     #{mod\ 1793}#
                                     #f))
                                 (values
                                   #{type\ 1810}#
                                   (#{binding-value\ 372}# #{b\ 1808}#)
                                   #{e\ 1788}#
                                   #{w\ 1790}#
                                   #{s\ 1791}#
                                   #{mod\ 1793}#))))))))))
               (if (pair? #{e\ 1788}#)
                 (begin
                   (let ((#{first\ 1819}# (car #{e\ 1788}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 468}#
                           #{first\ 1819}#
                           #{r\ 1789}#
                           #{w\ 1790}#
                           #{s\ 1791}#
                           #{rib\ 1792}#
                           #{mod\ 1793}#
                           #t))
                       (lambda (#{ftype\ 1820}#
                                #{fval\ 1821}#
                                #{fe\ 1822}#
                                #{fw\ 1823}#
                                #{fs\ 1824}#
                                #{fmod\ 1825}#)
                         (if (eqv? #{ftype\ 1820}# (quote lexical))
                           (values
                             'lexical-call
                             #{fval\ 1821}#
                             #{e\ 1788}#
                             #{w\ 1790}#
                             #{s\ 1791}#
                             #{mod\ 1793}#)
                           (if (eqv? #{ftype\ 1820}# (quote global))
                             (values
                               'global-call
                               (#{make-syntax-object\ 349}#
                                 #{fval\ 1821}#
                                 #{w\ 1790}#
                                 #{fmod\ 1825}#)
                               #{e\ 1788}#
                               #{w\ 1790}#
                               #{s\ 1791}#
                               #{mod\ 1793}#)
                             (if (eqv? #{ftype\ 1820}# (quote macro))
                               (#{syntax-type\ 468}#
                                 (#{chi-macro\ 478}#
                                   #{fval\ 1821}#
                                   #{e\ 1788}#
                                   #{r\ 1789}#
                                   #{w\ 1790}#
                                   #{s\ 1791}#
                                   #{rib\ 1792}#
                                   #{mod\ 1793}#)
                                 #{r\ 1789}#
                                 '(())
                                 #{s\ 1791}#
                                 #{rib\ 1792}#
                                 #{mod\ 1793}#
                                 #{for-car?\ 1794}#)
                               (if (eqv? #{ftype\ 1820}# (quote module-ref))
                                 (call-with-values
                                   (lambda ()
                                     (#{fval\ 1821}#
                                       #{e\ 1788}#
                                       #{r\ 1789}#
                                       #{w\ 1790}#))
                                   (lambda (#{e\ 1837}#
                                            #{r\ 1838}#
                                            #{w\ 1839}#
                                            #{s\ 1840}#
                                            #{mod\ 1841}#)
                                     (#{syntax-type\ 468}#
                                       #{e\ 1837}#
                                       #{r\ 1838}#
                                       #{w\ 1839}#
                                       #{s\ 1840}#
                                       #{rib\ 1792}#
                                       #{mod\ 1841}#
                                       #{for-car?\ 1794}#)))
                                 (if (eqv? #{ftype\ 1820}# (quote core))
                                   (values
                                     'core-form
                                     #{fval\ 1821}#
                                     #{e\ 1788}#
                                     #{w\ 1790}#
                                     #{s\ 1791}#
                                     #{mod\ 1793}#)
                                   (if (eqv? #{ftype\ 1820}#
                                             'local-syntax)
                                     (values
                                       'local-syntax-form
                                       #{fval\ 1821}#
                                       #{e\ 1788}#
                                       #{w\ 1790}#
                                       #{s\ 1791}#
                                       #{mod\ 1793}#)
                                     (if (eqv? #{ftype\ 1820}# (quote begin))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 1788}#
                                         #{w\ 1790}#
                                         #{s\ 1791}#
                                         #{mod\ 1793}#)
                                       (if (eqv? #{ftype\ 1820}#
                                                 'eval-when)
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 1788}#
                                           #{w\ 1790}#
                                           #{s\ 1791}#
                                           #{mod\ 1793}#)
                                         (if (eqv? #{ftype\ 1820}#
                                                   'define)
                                           (let ((#{tmp\ 1852}# #{e\ 1788}#))
                                             (let ((#{tmp\ 1853}#
                                                     ($sc-dispatch
                                                       #{tmp\ 1852}#
                                                       '(any any any))))
                                               (if (if #{tmp\ 1853}#
                                                     (@apply
                                                       (lambda (#{_\ 1857}#
                                                                #{name\ 1858}#
                                                                #{val\ 1859}#)
                                                         (#{id?\ 387}#
                                                           #{name\ 1858}#))
                                                       #{tmp\ 1853}#)
                                                     #f)
                                                 (@apply
                                                   (lambda (#{_\ 1863}#
                                                            #{name\ 1864}#
                                                            #{val\ 1865}#)
                                                     (values
                                                       'define-form
                                                       #{name\ 1864}#
                                                       #{val\ 1865}#
                                                       #{w\ 1790}#
                                                       #{s\ 1791}#
                                                       #{mod\ 1793}#))
                                                   #{tmp\ 1853}#)
                                                 (let ((#{tmp\ 1866}#
                                                         ($sc-dispatch
                                                           #{tmp\ 1852}#
                                                           '(any (any . any)
                                                                 any
                                                                 .
                                                                 each-any))))
                                                   (if (if #{tmp\ 1866}#
                                                         (@apply
                                                           (lambda (#{_\ 1872}#
                                                                    #{name\ 1873}#
                                                                    #{args\ 1874}#
                                                                    #{e1\ 1875}#
                                                                    #{e2\ 1876}#)
                                                             (if (#{id?\ 387}#
                                                                   #{name\ 1873}#)
                                                               (#{valid-bound-ids?\ 450}#
                                                                 (#{lambda-var-list\ 502}#
                                                                   #{args\ 1874}#))
                                                               #f))
                                                           #{tmp\ 1866}#)
                                                         #f)
                                                     (@apply
                                                       (lambda (#{_\ 1884}#
                                                                #{name\ 1885}#
                                                                #{args\ 1886}#
                                                                #{e1\ 1887}#
                                                                #{e2\ 1888}#)
                                                         (values
                                                           'define-form
                                                           (#{wrap\ 456}#
                                                             #{name\ 1885}#
                                                             #{w\ 1790}#
                                                             #{mod\ 1793}#)
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
                                                                         #("i1879"
                                                                           "i1880"
                                                                           "i1881"
                                                                           "i1882"
                                                                           "i1883"))
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
                                                                         #("i1826"
                                                                           "i1827"
                                                                           "i1828"
                                                                           "i1829"
                                                                           "i1830"
                                                                           "i1831"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(first)
                                                                         #((top))
                                                                         #("i1818"))
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
                                                                         #("i1795"
                                                                           "i1796"
                                                                           "i1797"
                                                                           "i1798"
                                                                           "i1799"
                                                                           "i1800"
                                                                           "i1801"))
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
                                                                     (cons #{args\ 1886}#
                                                                           (cons #{e1\ 1887}#
                                                                                 #{e2\ 1888}#))
                                                                     #{w\ 1790}#
                                                                     #{mod\ 1793}#))
                                                             #{s\ 1791}#)
                                                           '(())
                                                           #{s\ 1791}#
                                                           #{mod\ 1793}#))
                                                       #{tmp\ 1866}#)
                                                     (let ((#{tmp\ 1891}#
                                                             ($sc-dispatch
                                                               #{tmp\ 1852}#
                                                               '(any any))))
                                                       (if (if #{tmp\ 1891}#
                                                             (@apply
                                                               (lambda (#{_\ 1894}#
                                                                        #{name\ 1895}#)
                                                                 (#{id?\ 387}#
                                                                   #{name\ 1895}#))
                                                               #{tmp\ 1891}#)
                                                             #f)
                                                         (@apply
                                                           (lambda (#{_\ 1898}#
                                                                    #{name\ 1899}#)
                                                             (values
                                                               'define-form
                                                               (#{wrap\ 456}#
                                                                 #{name\ 1899}#
                                                                 #{w\ 1790}#
                                                                 #{mod\ 1793}#)
                                                               '(#(syntax-object
                                                                   if
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i1896"
                                                                        "i1897"))
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
                                                                      #("i1826"
                                                                        "i1827"
                                                                        "i1828"
                                                                        "i1829"
                                                                        "i1830"
                                                                        "i1831"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1818"))
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
                                                                      #("i1795"
                                                                        "i1796"
                                                                        "i1797"
                                                                        "i1798"
                                                                        "i1799"
                                                                        "i1800"
                                                                        "i1801"))
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
                                                                      #("i1896"
                                                                        "i1897"))
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
                                                                      #("i1826"
                                                                        "i1827"
                                                                        "i1828"
                                                                        "i1829"
                                                                        "i1830"
                                                                        "i1831"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1818"))
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
                                                                      #("i1795"
                                                                        "i1796"
                                                                        "i1797"
                                                                        "i1798"
                                                                        "i1799"
                                                                        "i1800"
                                                                        "i1801"))
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
                                                                      #("i1896"
                                                                        "i1897"))
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
                                                                      #("i1826"
                                                                        "i1827"
                                                                        "i1828"
                                                                        "i1829"
                                                                        "i1830"
                                                                        "i1831"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1818"))
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
                                                                      #("i1795"
                                                                        "i1796"
                                                                        "i1797"
                                                                        "i1798"
                                                                        "i1799"
                                                                        "i1800"
                                                                        "i1801"))
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
                                                               #{s\ 1791}#
                                                               #{mod\ 1793}#))
                                                           #{tmp\ 1891}#)
                                                         (syntax-violation
                                                           #f
                                                           "source expression failed to match any pattern"
                                                           #{tmp\ 1852}#))))))))
                                           (if (eqv? #{ftype\ 1820}#
                                                     'define-syntax)
                                             (let ((#{tmp\ 1902}# #{e\ 1788}#))
                                               (let ((#{tmp\ 1903}#
                                                       ($sc-dispatch
                                                         #{tmp\ 1902}#
                                                         '(any any any))))
                                                 (if (if #{tmp\ 1903}#
                                                       (@apply
                                                         (lambda (#{_\ 1907}#
                                                                  #{name\ 1908}#
                                                                  #{val\ 1909}#)
                                                           (#{id?\ 387}#
                                                             #{name\ 1908}#))
                                                         #{tmp\ 1903}#)
                                                       #f)
                                                   (@apply
                                                     (lambda (#{_\ 1913}#
                                                              #{name\ 1914}#
                                                              #{val\ 1915}#)
                                                       (values
                                                         'define-syntax-form
                                                         #{name\ 1914}#
                                                         #{val\ 1915}#
                                                         #{w\ 1790}#
                                                         #{s\ 1791}#
                                                         #{mod\ 1793}#))
                                                     #{tmp\ 1903}#)
                                                   (syntax-violation
                                                     #f
                                                     "source expression failed to match any pattern"
                                                     #{tmp\ 1902}#))))
                                             (values
                                               'call
                                               #f
                                               #{e\ 1788}#
                                               #{w\ 1790}#
                                               #{s\ 1791}#
                                               #{mod\ 1793}#)))))))))))))))
                 (if (#{syntax-object?\ 351}# #{e\ 1788}#)
                   (#{syntax-type\ 468}#
                     (#{syntax-object-expression\ 353}# #{e\ 1788}#)
                     #{r\ 1789}#
                     (#{join-wraps\ 438}#
                       #{w\ 1790}#
                       (#{syntax-object-wrap\ 355}# #{e\ 1788}#))
                     (begin
                       (let ((#{t\ 1921}#
                               (#{source-annotation\ 366}# #{e\ 1788}#)))
                         (if #{t\ 1921}# #{t\ 1921}# #{s\ 1791}#)))
                     #{rib\ 1792}#
                     (begin
                       (let ((#{t\ 1925}#
                               (#{syntax-object-module\ 357}# #{e\ 1788}#)))
                         (if #{t\ 1925}# #{t\ 1925}# #{mod\ 1793}#)))
                     #{for-car?\ 1794}#)
                   (if (self-evaluating? #{e\ 1788}#)
                     (values
                       'constant
                       #f
                       #{e\ 1788}#
                       #{w\ 1790}#
                       #{s\ 1791}#
                       #{mod\ 1793}#)
                     (values
                       'other
                       #f
                       #{e\ 1788}#
                       #{w\ 1790}#
                       #{s\ 1791}#
                       #{mod\ 1793}#)))))))
         (#{chi-when-list\ 466}#
           (lambda (#{e\ 1930}# #{when-list\ 1931}# #{w\ 1932}#)
             (letrec*
               ((#{f\ 1939}#
                  (lambda (#{when-list\ 1940}# #{situations\ 1941}#)
                    (if (null? #{when-list\ 1940}#)
                      #{situations\ 1941}#
                      (#{f\ 1939}#
                        (cdr #{when-list\ 1940}#)
                        (cons (begin
                                (let ((#{x\ 1943}# (car #{when-list\ 1940}#)))
                                  (if (#{free-id=?\ 446}#
                                        #{x\ 1943}#
                                        '#(syntax-object
                                           compile
                                           ((top)
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i1942"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(f when-list situations)
                                              #((top) (top) (top))
                                              #("i1936" "i1937" "i1938"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(e when-list w)
                                              #((top) (top) (top))
                                              #("i1933" "i1934" "i1935"))
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
                                          #{x\ 1943}#
                                          '#(syntax-object
                                             load
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i1942"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(f when-list situations)
                                                #((top) (top) (top))
                                                #("i1936" "i1937" "i1938"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(e when-list w)
                                                #((top) (top) (top))
                                                #("i1933" "i1934" "i1935"))
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
                                            #{x\ 1943}#
                                            '#(syntax-object
                                               eval
                                               ((top)
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i1942"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(f when-list situations)
                                                  #((top) (top) (top))
                                                  #("i1936" "i1937" "i1938"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(e when-list w)
                                                  #((top) (top) (top))
                                                  #("i1933" "i1934" "i1935"))
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
                                              #{x\ 1943}#
                                              '#(syntax-object
                                                 expand
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i1942"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(f when-list situations)
                                                    #((top) (top) (top))
                                                    #("i1936" "i1937" "i1938"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e when-list w)
                                                    #((top) (top) (top))
                                                    #("i1933" "i1934" "i1935"))
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
                                            #{e\ 1930}#
                                            (#{wrap\ 456}#
                                              #{x\ 1943}#
                                              #{w\ 1932}#
                                              #f))))))))
                              #{situations\ 1941}#))))))
               (begin
                 (#{f\ 1939}# #{when-list\ 1931}# (quote ()))))))
         (#{chi-install-global\ 464}#
           (lambda (#{name\ 1953}# #{e\ 1954}#)
             (#{build-global-definition\ 327}#
               #f
               #{name\ 1953}#
               (#{build-application\ 309}#
                 #f
                 (#{build-primref\ 335}#
                   #f
                   'make-syntax-transformer)
                 (list (#{build-data\ 337}# #f #{name\ 1953}#)
                       (#{build-data\ 337}# #f (quote macro))
                       #{e\ 1954}#)))))
         (#{chi-top-sequence\ 462}#
           (lambda (#{body\ 1962}#
                    #{r\ 1963}#
                    #{w\ 1964}#
                    #{s\ 1965}#
                    #{m\ 1966}#
                    #{esew\ 1967}#
                    #{mod\ 1968}#)
             (#{build-sequence\ 339}#
               #{s\ 1965}#
               (letrec*
                 ((#{dobody\ 1984}#
                    (lambda (#{body\ 1985}#
                             #{r\ 1986}#
                             #{w\ 1987}#
                             #{m\ 1988}#
                             #{esew\ 1989}#
                             #{mod\ 1990}#
                             #{out\ 1991}#)
                      (if (null? #{body\ 1985}#)
                        (reverse #{out\ 1991}#)
                        (#{dobody\ 1984}#
                          (cdr #{body\ 1985}#)
                          #{r\ 1986}#
                          #{w\ 1987}#
                          #{m\ 1988}#
                          #{esew\ 1989}#
                          #{mod\ 1990}#
                          (cons (#{chi-top\ 470}#
                                  (car #{body\ 1985}#)
                                  #{r\ 1986}#
                                  #{w\ 1987}#
                                  #{m\ 1988}#
                                  #{esew\ 1989}#
                                  #{mod\ 1990}#)
                                #{out\ 1991}#))))))
                 (begin
                   (#{dobody\ 1984}#
                     #{body\ 1962}#
                     #{r\ 1963}#
                     #{w\ 1964}#
                     #{m\ 1966}#
                     #{esew\ 1967}#
                     #{mod\ 1968}#
                     '()))))))
         (#{chi-sequence\ 460}#
           (lambda (#{body\ 1992}#
                    #{r\ 1993}#
                    #{w\ 1994}#
                    #{s\ 1995}#
                    #{mod\ 1996}#)
             (#{build-sequence\ 339}#
               #{s\ 1995}#
               (letrec*
                 ((#{dobody\ 2007}#
                    (lambda (#{body\ 2008}#
                             #{r\ 2009}#
                             #{w\ 2010}#
                             #{mod\ 2011}#)
                      (if (null? #{body\ 2008}#)
                        '()
                        (begin
                          (let ((#{first\ 2013}#
                                  (#{chi\ 472}#
                                    (car #{body\ 2008}#)
                                    #{r\ 2009}#
                                    #{w\ 2010}#
                                    #{mod\ 2011}#)))
                            (cons #{first\ 2013}#
                                  (#{dobody\ 2007}#
                                    (cdr #{body\ 2008}#)
                                    #{r\ 2009}#
                                    #{w\ 2010}#
                                    #{mod\ 2011}#))))))))
                 (begin
                   (#{dobody\ 2007}#
                     #{body\ 1992}#
                     #{r\ 1993}#
                     #{w\ 1994}#
                     #{mod\ 1996}#))))))
         (#{source-wrap\ 458}#
           (lambda (#{x\ 2014}#
                    #{w\ 2015}#
                    #{s\ 2016}#
                    #{defmod\ 2017}#)
             (#{wrap\ 456}#
               (#{decorate-source\ 305}#
                 #{x\ 2014}#
                 #{s\ 2016}#)
               #{w\ 2015}#
               #{defmod\ 2017}#)))
         (#{wrap\ 456}#
           (lambda (#{x\ 2022}# #{w\ 2023}# #{defmod\ 2024}#)
             (if (if (null? (#{wrap-marks\ 394}# #{w\ 2023}#))
                   (null? (#{wrap-subst\ 396}# #{w\ 2023}#))
                   #f)
               #{x\ 2022}#
               (if (#{syntax-object?\ 351}# #{x\ 2022}#)
                 (#{make-syntax-object\ 349}#
                   (#{syntax-object-expression\ 353}# #{x\ 2022}#)
                   (#{join-wraps\ 438}#
                     #{w\ 2023}#
                     (#{syntax-object-wrap\ 355}# #{x\ 2022}#))
                   (#{syntax-object-module\ 357}# #{x\ 2022}#))
                 (if (null? #{x\ 2022}#)
                   #{x\ 2022}#
                   (#{make-syntax-object\ 349}#
                     #{x\ 2022}#
                     #{w\ 2023}#
                     #{defmod\ 2024}#))))))
         (#{bound-id-member?\ 454}#
           (lambda (#{x\ 2037}# #{list\ 2038}#)
             (if (not (null? #{list\ 2038}#))
               (begin
                 (let ((#{t\ 2045}#
                         (#{bound-id=?\ 448}#
                           #{x\ 2037}#
                           (car #{list\ 2038}#))))
                   (if #{t\ 2045}#
                     #{t\ 2045}#
                     (#{bound-id-member?\ 454}#
                       #{x\ 2037}#
                       (cdr #{list\ 2038}#)))))
               #f)))
         (#{distinct-bound-ids?\ 452}#
           (lambda (#{ids\ 2047}#)
             (letrec*
               ((#{distinct?\ 2051}#
                  (lambda (#{ids\ 2052}#)
                    (begin
                      (let ((#{t\ 2055}# (null? #{ids\ 2052}#)))
                        (if #{t\ 2055}#
                          #{t\ 2055}#
                          (if (not (#{bound-id-member?\ 454}#
                                     (car #{ids\ 2052}#)
                                     (cdr #{ids\ 2052}#)))
                            (#{distinct?\ 2051}# (cdr #{ids\ 2052}#))
                            #f)))))))
               (begin (#{distinct?\ 2051}# #{ids\ 2047}#)))))
         (#{valid-bound-ids?\ 450}#
           (lambda (#{ids\ 2059}#)
             (if (letrec*
                   ((#{all-ids?\ 2064}#
                      (lambda (#{ids\ 2065}#)
                        (begin
                          (let ((#{t\ 2068}# (null? #{ids\ 2065}#)))
                            (if #{t\ 2068}#
                              #{t\ 2068}#
                              (if (#{id?\ 387}# (car #{ids\ 2065}#))
                                (#{all-ids?\ 2064}# (cdr #{ids\ 2065}#))
                                #f)))))))
                   (begin (#{all-ids?\ 2064}# #{ids\ 2059}#)))
               (#{distinct-bound-ids?\ 452}# #{ids\ 2059}#)
               #f)))
         (#{bound-id=?\ 448}#
           (lambda (#{i\ 2073}# #{j\ 2074}#)
             (if (if (#{syntax-object?\ 351}# #{i\ 2073}#)
                   (#{syntax-object?\ 351}# #{j\ 2074}#)
                   #f)
               (if (eq? (#{syntax-object-expression\ 353}# #{i\ 2073}#)
                        (#{syntax-object-expression\ 353}# #{j\ 2074}#))
                 (#{same-marks?\ 442}#
                   (#{wrap-marks\ 394}#
                     (#{syntax-object-wrap\ 355}# #{i\ 2073}#))
                   (#{wrap-marks\ 394}#
                     (#{syntax-object-wrap\ 355}# #{j\ 2074}#)))
                 #f)
               (eq? #{i\ 2073}# #{j\ 2074}#))))
         (#{free-id=?\ 446}#
           (lambda (#{i\ 2081}# #{j\ 2082}#)
             (if (eq? (begin
                        (let ((#{x\ 2088}# #{i\ 2081}#))
                          (if (#{syntax-object?\ 351}# #{x\ 2088}#)
                            (#{syntax-object-expression\ 353}# #{x\ 2088}#)
                            #{x\ 2088}#)))
                      (begin
                        (let ((#{x\ 2091}# #{j\ 2082}#))
                          (if (#{syntax-object?\ 351}# #{x\ 2091}#)
                            (#{syntax-object-expression\ 353}# #{x\ 2091}#)
                            #{x\ 2091}#))))
               (eq? (#{id-var-name\ 444}# #{i\ 2081}# (quote (())))
                    (#{id-var-name\ 444}# #{j\ 2082}# (quote (()))))
               #f)))
         (#{id-var-name\ 444}#
           (lambda (#{id\ 2095}# #{w\ 2096}#)
             (letrec*
               ((#{search-vector-rib\ 2105}#
                  (lambda (#{sym\ 2117}#
                           #{subst\ 2118}#
                           #{marks\ 2119}#
                           #{symnames\ 2120}#
                           #{ribcage\ 2121}#)
                    (begin
                      (let ((#{n\ 2128}# (vector-length #{symnames\ 2120}#)))
                        (letrec*
                          ((#{f\ 2131}#
                             (lambda (#{i\ 2132}#)
                               (if (#{fx=\ 292}# #{i\ 2132}# #{n\ 2128}#)
                                 (#{search\ 2101}#
                                   #{sym\ 2117}#
                                   (cdr #{subst\ 2118}#)
                                   #{marks\ 2119}#)
                                 (if (if (eq? (vector-ref
                                                #{symnames\ 2120}#
                                                #{i\ 2132}#)
                                              #{sym\ 2117}#)
                                       (#{same-marks?\ 442}#
                                         #{marks\ 2119}#
                                         (vector-ref
                                           (#{ribcage-marks\ 414}#
                                             #{ribcage\ 2121}#)
                                           #{i\ 2132}#))
                                       #f)
                                   (values
                                     (vector-ref
                                       (#{ribcage-labels\ 416}#
                                         #{ribcage\ 2121}#)
                                       #{i\ 2132}#)
                                     #{marks\ 2119}#)
                                   (#{f\ 2131}#
                                     (#{fx+\ 288}# #{i\ 2132}# 1)))))))
                          (begin (#{f\ 2131}# 0)))))))
                (#{search-list-rib\ 2103}#
                  (lambda (#{sym\ 2140}#
                           #{subst\ 2141}#
                           #{marks\ 2142}#
                           #{symnames\ 2143}#
                           #{ribcage\ 2144}#)
                    (letrec*
                      ((#{f\ 2153}#
                         (lambda (#{symnames\ 2154}# #{i\ 2155}#)
                           (if (null? #{symnames\ 2154}#)
                             (#{search\ 2101}#
                               #{sym\ 2140}#
                               (cdr #{subst\ 2141}#)
                               #{marks\ 2142}#)
                             (if (if (eq? (car #{symnames\ 2154}#)
                                          #{sym\ 2140}#)
                                   (#{same-marks?\ 442}#
                                     #{marks\ 2142}#
                                     (list-ref
                                       (#{ribcage-marks\ 414}#
                                         #{ribcage\ 2144}#)
                                       #{i\ 2155}#))
                                   #f)
                               (values
                                 (list-ref
                                   (#{ribcage-labels\ 416}# #{ribcage\ 2144}#)
                                   #{i\ 2155}#)
                                 #{marks\ 2142}#)
                               (#{f\ 2153}#
                                 (cdr #{symnames\ 2154}#)
                                 (#{fx+\ 288}# #{i\ 2155}# 1)))))))
                      (begin (#{f\ 2153}# #{symnames\ 2143}# 0)))))
                (#{search\ 2101}#
                  (lambda (#{sym\ 2163}# #{subst\ 2164}# #{marks\ 2165}#)
                    (if (null? #{subst\ 2164}#)
                      (values #f #{marks\ 2165}#)
                      (begin
                        (let ((#{fst\ 2170}# (car #{subst\ 2164}#)))
                          (if (eq? #{fst\ 2170}# (quote shift))
                            (#{search\ 2101}#
                              #{sym\ 2163}#
                              (cdr #{subst\ 2164}#)
                              (cdr #{marks\ 2165}#))
                            (begin
                              (let ((#{symnames\ 2172}#
                                      (#{ribcage-symnames\ 412}#
                                        #{fst\ 2170}#)))
                                (if (vector? #{symnames\ 2172}#)
                                  (#{search-vector-rib\ 2105}#
                                    #{sym\ 2163}#
                                    #{subst\ 2164}#
                                    #{marks\ 2165}#
                                    #{symnames\ 2172}#
                                    #{fst\ 2170}#)
                                  (#{search-list-rib\ 2103}#
                                    #{sym\ 2163}#
                                    #{subst\ 2164}#
                                    #{marks\ 2165}#
                                    #{symnames\ 2172}#
                                    #{fst\ 2170}#)))))))))))
               (begin
                 (if (symbol? #{id\ 2095}#)
                   (begin
                     (let ((#{t\ 2175}#
                             (call-with-values
                               (lambda ()
                                 (#{search\ 2101}#
                                   #{id\ 2095}#
                                   (#{wrap-subst\ 396}# #{w\ 2096}#)
                                   (#{wrap-marks\ 394}# #{w\ 2096}#)))
                               (lambda (#{x\ 2177}# . #{ignore\ 2178}#)
                                 #{x\ 2177}#))))
                       (if #{t\ 2175}# #{t\ 2175}# #{id\ 2095}#)))
                   (if (#{syntax-object?\ 351}# #{id\ 2095}#)
                     (begin
                       (let ((#{id\ 2186}#
                               (#{syntax-object-expression\ 353}#
                                 #{id\ 2095}#))
                             (#{w1\ 2187}#
                               (#{syntax-object-wrap\ 355}# #{id\ 2095}#)))
                         (begin
                           (let ((#{marks\ 2189}#
                                   (#{join-marks\ 440}#
                                     (#{wrap-marks\ 394}# #{w\ 2096}#)
                                     (#{wrap-marks\ 394}# #{w1\ 2187}#))))
                             (call-with-values
                               (lambda ()
                                 (#{search\ 2101}#
                                   #{id\ 2186}#
                                   (#{wrap-subst\ 396}# #{w\ 2096}#)
                                   #{marks\ 2189}#))
                               (lambda (#{new-id\ 2190}# #{marks\ 2191}#)
                                 (begin
                                   (let ((#{t\ 2196}# #{new-id\ 2190}#))
                                     (if #{t\ 2196}#
                                       #{t\ 2196}#
                                       (begin
                                         (let ((#{t\ 2199}#
                                                 (call-with-values
                                                   (lambda ()
                                                     (#{search\ 2101}#
                                                       #{id\ 2186}#
                                                       (#{wrap-subst\ 396}#
                                                         #{w1\ 2187}#)
                                                       #{marks\ 2191}#))
                                                   (lambda (#{x\ 2201}#
                                                            .
                                                            #{ignore\ 2202}#)
                                                     #{x\ 2201}#))))
                                           (if #{t\ 2199}#
                                             #{t\ 2199}#
                                             #{id\ 2186}#))))))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 2095}#)))))))
         (#{same-marks?\ 442}#
           (lambda (#{x\ 2207}# #{y\ 2208}#)
             (begin
               (let ((#{t\ 2213}# (eq? #{x\ 2207}# #{y\ 2208}#)))
                 (if #{t\ 2213}#
                   #{t\ 2213}#
                   (if (not (null? #{x\ 2207}#))
                     (if (not (null? #{y\ 2208}#))
                       (if (eq? (car #{x\ 2207}#) (car #{y\ 2208}#))
                         (#{same-marks?\ 442}#
                           (cdr #{x\ 2207}#)
                           (cdr #{y\ 2208}#))
                         #f)
                       #f)
                     #f))))))
         (#{join-marks\ 440}#
           (lambda (#{m1\ 2219}# #{m2\ 2220}#)
             (#{smart-append\ 436}# #{m1\ 2219}# #{m2\ 2220}#)))
         (#{join-wraps\ 438}#
           (lambda (#{w1\ 2223}# #{w2\ 2224}#)
             (begin
               (let ((#{m1\ 2229}# (#{wrap-marks\ 394}# #{w1\ 2223}#))
                     (#{s1\ 2230}# (#{wrap-subst\ 396}# #{w1\ 2223}#)))
                 (if (null? #{m1\ 2229}#)
                   (if (null? #{s1\ 2230}#)
                     #{w2\ 2224}#
                     (#{make-wrap\ 392}#
                       (#{wrap-marks\ 394}# #{w2\ 2224}#)
                       (#{smart-append\ 436}#
                         #{s1\ 2230}#
                         (#{wrap-subst\ 396}# #{w2\ 2224}#))))
                   (#{make-wrap\ 392}#
                     (#{smart-append\ 436}#
                       #{m1\ 2229}#
                       (#{wrap-marks\ 394}# #{w2\ 2224}#))
                     (#{smart-append\ 436}#
                       #{s1\ 2230}#
                       (#{wrap-subst\ 396}# #{w2\ 2224}#))))))))
         (#{smart-append\ 436}#
           (lambda (#{m1\ 2231}# #{m2\ 2232}#)
             (if (null? #{m2\ 2232}#)
               #{m1\ 2231}#
               (append #{m1\ 2231}# #{m2\ 2232}#))))
         (#{make-binding-wrap\ 434}#
           (lambda (#{ids\ 2235}# #{labels\ 2236}# #{w\ 2237}#)
             (if (null? #{ids\ 2235}#)
               #{w\ 2237}#
               (#{make-wrap\ 392}#
                 (#{wrap-marks\ 394}# #{w\ 2237}#)
                 (cons (begin
                         (let ((#{labelvec\ 2242}#
                                 (list->vector #{labels\ 2236}#)))
                           (begin
                             (let ((#{n\ 2244}#
                                     (vector-length #{labelvec\ 2242}#)))
                               (begin
                                 (let ((#{symnamevec\ 2247}#
                                         (make-vector #{n\ 2244}#))
                                       (#{marksvec\ 2248}#
                                         (make-vector #{n\ 2244}#)))
                                   (begin
                                     (letrec*
                                       ((#{f\ 2252}#
                                          (lambda (#{ids\ 2253}# #{i\ 2254}#)
                                            (if (not (null? #{ids\ 2253}#))
                                              (call-with-values
                                                (lambda ()
                                                  (#{id-sym-name&marks\ 390}#
                                                    (car #{ids\ 2253}#)
                                                    #{w\ 2237}#))
                                                (lambda (#{symname\ 2255}#
                                                         #{marks\ 2256}#)
                                                  (begin
                                                    (vector-set!
                                                      #{symnamevec\ 2247}#
                                                      #{i\ 2254}#
                                                      #{symname\ 2255}#)
                                                    (vector-set!
                                                      #{marksvec\ 2248}#
                                                      #{i\ 2254}#
                                                      #{marks\ 2256}#)
                                                    (#{f\ 2252}#
                                                      (cdr #{ids\ 2253}#)
                                                      (#{fx+\ 288}#
                                                        #{i\ 2254}#
                                                        1)))))))))
                                       (begin (#{f\ 2252}# #{ids\ 2235}# 0)))
                                     (#{make-ribcage\ 408}#
                                       #{symnamevec\ 2247}#
                                       #{marksvec\ 2248}#
                                       #{labelvec\ 2242}#))))))))
                       (#{wrap-subst\ 396}# #{w\ 2237}#))))))
         (#{extend-ribcage!\ 432}#
           (lambda (#{ribcage\ 2259}# #{id\ 2260}# #{label\ 2261}#)
             (begin
               (#{set-ribcage-symnames!\ 418}#
                 #{ribcage\ 2259}#
                 (cons (#{syntax-object-expression\ 353}# #{id\ 2260}#)
                       (#{ribcage-symnames\ 412}# #{ribcage\ 2259}#)))
               (#{set-ribcage-marks!\ 420}#
                 #{ribcage\ 2259}#
                 (cons (#{wrap-marks\ 394}#
                         (#{syntax-object-wrap\ 355}# #{id\ 2260}#))
                       (#{ribcage-marks\ 414}# #{ribcage\ 2259}#)))
               (#{set-ribcage-labels!\ 422}#
                 #{ribcage\ 2259}#
                 (cons #{label\ 2261}#
                       (#{ribcage-labels\ 416}# #{ribcage\ 2259}#))))))
         (#{anti-mark\ 428}#
           (lambda (#{w\ 2265}#)
             (#{make-wrap\ 392}#
               (cons #f (#{wrap-marks\ 394}# #{w\ 2265}#))
               (cons 'shift
                     (#{wrap-subst\ 396}# #{w\ 2265}#)))))
         (#{set-ribcage-labels!\ 422}#
           (lambda (#{x\ 2268}# #{update\ 2269}#)
             (vector-set! #{x\ 2268}# 3 #{update\ 2269}#)))
         (#{set-ribcage-marks!\ 420}#
           (lambda (#{x\ 2272}# #{update\ 2273}#)
             (vector-set! #{x\ 2272}# 2 #{update\ 2273}#)))
         (#{set-ribcage-symnames!\ 418}#
           (lambda (#{x\ 2276}# #{update\ 2277}#)
             (vector-set! #{x\ 2276}# 1 #{update\ 2277}#)))
         (#{ribcage-labels\ 416}#
           (lambda (#{x\ 2280}#) (vector-ref #{x\ 2280}# 3)))
         (#{ribcage-marks\ 414}#
           (lambda (#{x\ 2282}#) (vector-ref #{x\ 2282}# 2)))
         (#{ribcage-symnames\ 412}#
           (lambda (#{x\ 2284}#) (vector-ref #{x\ 2284}# 1)))
         (#{make-ribcage\ 408}#
           (lambda (#{symnames\ 2291}#
                    #{marks\ 2292}#
                    #{labels\ 2293}#)
             (vector
               'ribcage
               #{symnames\ 2291}#
               #{marks\ 2292}#
               #{labels\ 2293}#)))
         (#{gen-labels\ 405}#
           (lambda (#{ls\ 2297}#)
             (if (null? #{ls\ 2297}#)
               '()
               (cons (#{gen-label\ 403}#)
                     (#{gen-labels\ 405}# (cdr #{ls\ 2297}#))))))
         (#{gen-label\ 403}#
           (lambda () (symbol->string (gensym "i"))))
         (#{id-sym-name&marks\ 390}#
           (lambda (#{x\ 2299}# #{w\ 2300}#)
             (if (#{syntax-object?\ 351}# #{x\ 2299}#)
               (values
                 (#{syntax-object-expression\ 353}# #{x\ 2299}#)
                 (#{join-marks\ 440}#
                   (#{wrap-marks\ 394}# #{w\ 2300}#)
                   (#{wrap-marks\ 394}#
                     (#{syntax-object-wrap\ 355}# #{x\ 2299}#))))
               (values
                 #{x\ 2299}#
                 (#{wrap-marks\ 394}# #{w\ 2300}#)))))
         (#{id?\ 387}#
           (lambda (#{x\ 2303}#)
             (if (symbol? #{x\ 2303}#)
               #t
               (if (#{syntax-object?\ 351}# #{x\ 2303}#)
                 (symbol?
                   (#{syntax-object-expression\ 353}# #{x\ 2303}#))
                 #f))))
         (#{nonsymbol-id?\ 385}#
           (lambda (#{x\ 2310}#)
             (if (#{syntax-object?\ 351}# #{x\ 2310}#)
               (symbol?
                 (#{syntax-object-expression\ 353}# #{x\ 2310}#))
               #f)))
         (#{global-extend\ 383}#
           (lambda (#{type\ 2314}# #{sym\ 2315}# #{val\ 2316}#)
             (#{put-global-definition-hook\ 301}#
               #{sym\ 2315}#
               #{type\ 2314}#
               #{val\ 2316}#)))
         (#{lookup\ 381}#
           (lambda (#{x\ 2320}# #{r\ 2321}# #{mod\ 2322}#)
             (begin
               (let ((#{t\ 2328}# (assq #{x\ 2320}# #{r\ 2321}#)))
                 (if #{t\ 2328}#
                   (cdr #{t\ 2328}#)
                   (if (symbol? #{x\ 2320}#)
                     (begin
                       (let ((#{t\ 2334}#
                               (#{get-global-definition-hook\ 303}#
                                 #{x\ 2320}#
                                 #{mod\ 2322}#)))
                         (if #{t\ 2334}# #{t\ 2334}# (quote (global)))))
                     '(displaced-lexical)))))))
         (#{macros-only-env\ 379}#
           (lambda (#{r\ 2339}#)
             (if (null? #{r\ 2339}#)
               '()
               (begin
                 (let ((#{a\ 2342}# (car #{r\ 2339}#)))
                   (if (eq? (car (cdr #{a\ 2342}#)) (quote macro))
                     (cons #{a\ 2342}#
                           (#{macros-only-env\ 379}# (cdr #{r\ 2339}#)))
                     (#{macros-only-env\ 379}# (cdr #{r\ 2339}#))))))))
         (#{extend-var-env\ 377}#
           (lambda (#{labels\ 2343}# #{vars\ 2344}# #{r\ 2345}#)
             (if (null? #{labels\ 2343}#)
               #{r\ 2345}#
               (#{extend-var-env\ 377}#
                 (cdr #{labels\ 2343}#)
                 (cdr #{vars\ 2344}#)
                 (cons (cons (car #{labels\ 2343}#)
                             (cons (quote lexical) (car #{vars\ 2344}#)))
                       #{r\ 2345}#)))))
         (#{extend-env\ 375}#
           (lambda (#{labels\ 2350}# #{bindings\ 2351}# #{r\ 2352}#)
             (if (null? #{labels\ 2350}#)
               #{r\ 2352}#
               (#{extend-env\ 375}#
                 (cdr #{labels\ 2350}#)
                 (cdr #{bindings\ 2351}#)
                 (cons (cons (car #{labels\ 2350}#)
                             (car #{bindings\ 2351}#))
                       #{r\ 2352}#)))))
         (#{source-annotation\ 366}#
           (lambda (#{x\ 2356}#)
             (if (#{syntax-object?\ 351}# #{x\ 2356}#)
               (#{source-annotation\ 366}#
                 (#{syntax-object-expression\ 353}# #{x\ 2356}#))
               (if (pair? #{x\ 2356}#)
                 (begin
                   (let ((#{props\ 2363}# (source-properties #{x\ 2356}#)))
                     (if (pair? #{props\ 2363}#) #{props\ 2363}# #f)))
                 #f))))
         (#{syntax-object-module\ 357}#
           (lambda (#{x\ 2377}#) (vector-ref #{x\ 2377}# 3)))
         (#{syntax-object-wrap\ 355}#
           (lambda (#{x\ 2379}#) (vector-ref #{x\ 2379}# 2)))
         (#{syntax-object-expression\ 353}#
           (lambda (#{x\ 2381}#) (vector-ref #{x\ 2381}# 1)))
         (#{syntax-object?\ 351}#
           (lambda (#{x\ 2383}#)
             (if (vector? #{x\ 2383}#)
               (if (= (vector-length #{x\ 2383}#) 4)
                 (eq? (vector-ref #{x\ 2383}# 0)
                      'syntax-object)
                 #f)
               #f)))
         (#{make-syntax-object\ 349}#
           (lambda (#{expression\ 2388}#
                    #{wrap\ 2389}#
                    #{module\ 2390}#)
             (vector
               'syntax-object
               #{expression\ 2388}#
               #{wrap\ 2389}#
               #{module\ 2390}#)))
         (#{build-letrec\ 345}#
           (lambda (#{src\ 2394}#
                    #{in-order?\ 2395}#
                    #{ids\ 2396}#
                    #{vars\ 2397}#
                    #{val-exps\ 2398}#
                    #{body-exp\ 2399}#)
             (if (null? #{vars\ 2397}#)
               #{body-exp\ 2399}#
               (begin
                 (for-each
                   #{maybe-name-value!\ 325}#
                   #{ids\ 2396}#
                   #{val-exps\ 2398}#)
                 (#{make-letrec\ 277}#
                   #{src\ 2394}#
                   #{in-order?\ 2395}#
                   #{ids\ 2396}#
                   #{vars\ 2397}#
                   #{val-exps\ 2398}#
                   #{body-exp\ 2399}#)))))
         (#{build-named-let\ 343}#
           (lambda (#{src\ 2406}#
                    #{ids\ 2407}#
                    #{vars\ 2408}#
                    #{val-exps\ 2409}#
                    #{body-exp\ 2410}#)
             (begin
               (let ((#{f\ 2420}# (car #{vars\ 2408}#))
                     (#{f-name\ 2421}# (car #{ids\ 2407}#))
                     (#{vars\ 2422}# (cdr #{vars\ 2408}#))
                     (#{ids\ 2423}# (cdr #{ids\ 2407}#)))
                 (begin
                   (let ((#{proc\ 2425}#
                           (#{build-simple-lambda\ 329}#
                             #{src\ 2406}#
                             #{ids\ 2423}#
                             #f
                             #{vars\ 2422}#
                             '()
                             #{body-exp\ 2410}#)))
                     (begin
                       (#{maybe-name-value!\ 325}#
                         #{f-name\ 2421}#
                         #{proc\ 2425}#)
                       (for-each
                         #{maybe-name-value!\ 325}#
                         #{ids\ 2423}#
                         #{val-exps\ 2409}#)
                       (#{make-letrec\ 277}#
                         #{src\ 2406}#
                         #f
                         (list #{f-name\ 2421}#)
                         (list #{f\ 2420}#)
                         (list #{proc\ 2425}#)
                         (#{build-application\ 309}#
                           #{src\ 2406}#
                           (#{build-lexical-reference\ 315}#
                             'fun
                             #{src\ 2406}#
                             #{f-name\ 2421}#
                             #{f\ 2420}#)
                           #{val-exps\ 2409}#)))))))))
         (#{build-let\ 341}#
           (lambda (#{src\ 2426}#
                    #{ids\ 2427}#
                    #{vars\ 2428}#
                    #{val-exps\ 2429}#
                    #{body-exp\ 2430}#)
             (begin
               (for-each
                 #{maybe-name-value!\ 325}#
                 #{ids\ 2427}#
                 #{val-exps\ 2429}#)
               (if (null? #{vars\ 2428}#)
                 #{body-exp\ 2430}#
                 (#{make-let\ 275}#
                   #{src\ 2426}#
                   #{ids\ 2427}#
                   #{vars\ 2428}#
                   #{val-exps\ 2429}#
                   #{body-exp\ 2430}#)))))
         (#{build-sequence\ 339}#
           (lambda (#{src\ 2436}# #{exps\ 2437}#)
             (if (null? (cdr #{exps\ 2437}#))
               (car #{exps\ 2437}#)
               (#{make-sequence\ 269}#
                 #{src\ 2436}#
                 #{exps\ 2437}#))))
         (#{build-data\ 337}#
           (lambda (#{src\ 2440}# #{exp\ 2441}#)
             (#{make-const\ 247}# #{src\ 2440}# #{exp\ 2441}#)))
         (#{build-primref\ 335}#
           (lambda (#{src\ 2444}# #{name\ 2445}#)
             (if (equal?
                   (module-name (current-module))
                   '(guile))
               (#{make-toplevel-ref\ 259}#
                 #{src\ 2444}#
                 #{name\ 2445}#)
               (#{make-module-ref\ 255}#
                 #{src\ 2444}#
                 '(guile)
                 #{name\ 2445}#
                 #f))))
         (#{build-lambda-case\ 333}#
           (lambda (#{src\ 2448}#
                    #{req\ 2449}#
                    #{opt\ 2450}#
                    #{rest\ 2451}#
                    #{kw\ 2452}#
                    #{inits\ 2453}#
                    #{vars\ 2454}#
                    #{body\ 2455}#
                    #{else-case\ 2456}#)
             (#{make-lambda-case\ 273}#
               #{src\ 2448}#
               #{req\ 2449}#
               #{opt\ 2450}#
               #{rest\ 2451}#
               #{kw\ 2452}#
               #{inits\ 2453}#
               #{vars\ 2454}#
               #{body\ 2455}#
               #{else-case\ 2456}#)))
         (#{build-case-lambda\ 331}#
           (lambda (#{src\ 2466}# #{meta\ 2467}# #{body\ 2468}#)
             (#{make-lambda\ 271}#
               #{src\ 2466}#
               #{meta\ 2467}#
               #{body\ 2468}#)))
         (#{build-simple-lambda\ 329}#
           (lambda (#{src\ 2472}#
                    #{req\ 2473}#
                    #{rest\ 2474}#
                    #{vars\ 2475}#
                    #{meta\ 2476}#
                    #{exp\ 2477}#)
             (#{make-lambda\ 271}#
               #{src\ 2472}#
               #{meta\ 2476}#
               (#{make-lambda-case\ 273}#
                 #{src\ 2472}#
                 #{req\ 2473}#
                 #f
                 #{rest\ 2474}#
                 #f
                 '()
                 #{vars\ 2475}#
                 #{exp\ 2477}#
                 #f))))
         (#{build-global-definition\ 327}#
           (lambda (#{source\ 2484}# #{var\ 2485}# #{exp\ 2486}#)
             (begin
               (#{maybe-name-value!\ 325}#
                 #{var\ 2485}#
                 #{exp\ 2486}#)
               (#{make-toplevel-define\ 263}#
                 #{source\ 2484}#
                 #{var\ 2485}#
                 #{exp\ 2486}#))))
         (#{maybe-name-value!\ 325}#
           (lambda (#{name\ 2490}# #{val\ 2491}#)
             (if (#{lambda?\ 282}# #{val\ 2491}#)
               (begin
                 (let ((#{meta\ 2495}#
                         (#{lambda-meta\ 284}# #{val\ 2491}#)))
                   (if (not (assq (quote name) #{meta\ 2495}#))
                     (#{set-lambda-meta!\ 286}#
                       #{val\ 2491}#
                       (cons (cons (quote name) #{name\ 2490}#)
                             #{meta\ 2495}#))))))))
         (#{build-global-assignment\ 323}#
           (lambda (#{source\ 2496}#
                    #{var\ 2497}#
                    #{exp\ 2498}#
                    #{mod\ 2499}#)
             (#{analyze-variable\ 319}#
               #{mod\ 2499}#
               #{var\ 2497}#
               (lambda (#{mod\ 2504}# #{var\ 2505}# #{public?\ 2506}#)
                 (#{make-module-set\ 257}#
                   #{source\ 2496}#
                   #{mod\ 2504}#
                   #{var\ 2505}#
                   #{public?\ 2506}#
                   #{exp\ 2498}#))
               (lambda (#{var\ 2510}#)
                 (#{make-toplevel-set\ 261}#
                   #{source\ 2496}#
                   #{var\ 2510}#
                   #{exp\ 2498}#)))))
         (#{build-global-reference\ 321}#
           (lambda (#{source\ 2512}# #{var\ 2513}# #{mod\ 2514}#)
             (#{analyze-variable\ 319}#
               #{mod\ 2514}#
               #{var\ 2513}#
               (lambda (#{mod\ 2518}# #{var\ 2519}# #{public?\ 2520}#)
                 (#{make-module-ref\ 255}#
                   #{source\ 2512}#
                   #{mod\ 2518}#
                   #{var\ 2519}#
                   #{public?\ 2520}#))
               (lambda (#{var\ 2524}#)
                 (#{make-toplevel-ref\ 259}#
                   #{source\ 2512}#
                   #{var\ 2524}#)))))
         (#{analyze-variable\ 319}#
           (lambda (#{mod\ 2526}#
                    #{var\ 2527}#
                    #{modref-cont\ 2528}#
                    #{bare-cont\ 2529}#)
             (if (not #{mod\ 2526}#)
               (#{bare-cont\ 2529}# #{var\ 2527}#)
               (begin
                 (let ((#{kind\ 2536}# (car #{mod\ 2526}#))
                       (#{mod\ 2537}# (cdr #{mod\ 2526}#)))
                   (if (eqv? #{kind\ 2536}# (quote public))
                     (#{modref-cont\ 2528}#
                       #{mod\ 2537}#
                       #{var\ 2527}#
                       #t)
                     (if (eqv? #{kind\ 2536}# (quote private))
                       (if (not (equal?
                                  #{mod\ 2537}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 2528}#
                           #{mod\ 2537}#
                           #{var\ 2527}#
                           #f)
                         (#{bare-cont\ 2529}# #{var\ 2527}#))
                       (if (eqv? #{kind\ 2536}# (quote bare))
                         (#{bare-cont\ 2529}# #{var\ 2527}#)
                         (if (eqv? #{kind\ 2536}# (quote hygiene))
                           (if (if (not (equal?
                                          #{mod\ 2537}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 2537}#)
                                   #{var\ 2527}#)
                                 #f)
                             (#{modref-cont\ 2528}#
                               #{mod\ 2537}#
                               #{var\ 2527}#
                               #f)
                             (#{bare-cont\ 2529}# #{var\ 2527}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 2527}#
                             #{mod\ 2537}#))))))))))
         (#{build-lexical-assignment\ 317}#
           (lambda (#{source\ 2545}#
                    #{name\ 2546}#
                    #{var\ 2547}#
                    #{exp\ 2548}#)
             (#{make-lexical-set\ 253}#
               #{source\ 2545}#
               #{name\ 2546}#
               #{var\ 2547}#
               #{exp\ 2548}#)))
         (#{build-lexical-reference\ 315}#
           (lambda (#{type\ 2553}#
                    #{source\ 2554}#
                    #{name\ 2555}#
                    #{var\ 2556}#)
             (#{make-lexical-ref\ 251}#
               #{source\ 2554}#
               #{name\ 2555}#
               #{var\ 2556}#)))
         (#{build-dynlet\ 313}#
           (lambda (#{source\ 2561}#
                    #{fluids\ 2562}#
                    #{vals\ 2563}#
                    #{body\ 2564}#)
             (#{make-dynlet\ 279}#
               #{source\ 2561}#
               #{fluids\ 2562}#
               #{vals\ 2563}#
               #{body\ 2564}#)))
         (#{build-conditional\ 311}#
           (lambda (#{source\ 2569}#
                    #{test-exp\ 2570}#
                    #{then-exp\ 2571}#
                    #{else-exp\ 2572}#)
             (#{make-conditional\ 265}#
               #{source\ 2569}#
               #{test-exp\ 2570}#
               #{then-exp\ 2571}#
               #{else-exp\ 2572}#)))
         (#{build-application\ 309}#
           (lambda (#{source\ 2577}#
                    #{fun-exp\ 2578}#
                    #{arg-exps\ 2579}#)
             (#{make-application\ 267}#
               #{source\ 2577}#
               #{fun-exp\ 2578}#
               #{arg-exps\ 2579}#)))
         (#{build-void\ 307}#
           (lambda (#{source\ 2583}#)
             (#{make-void\ 245}# #{source\ 2583}#)))
         (#{decorate-source\ 305}#
           (lambda (#{e\ 2585}# #{s\ 2586}#)
             (begin
               (if (if (pair? #{e\ 2585}#) #{s\ 2586}# #f)
                 (set-source-properties! #{e\ 2585}# #{s\ 2586}#))
               #{e\ 2585}#)))
         (#{get-global-definition-hook\ 303}#
           (lambda (#{symbol\ 2591}# #{module\ 2592}#)
             (begin
               (if (if (not #{module\ 2592}#) (current-module) #f)
                 (warn "module system is booted, we should have a module"
                       #{symbol\ 2591}#))
               (begin
                 (let ((#{v\ 2598}#
                         (module-variable
                           (if #{module\ 2592}#
                             (resolve-module (cdr #{module\ 2592}#))
                             (current-module))
                           #{symbol\ 2591}#)))
                   (if #{v\ 2598}#
                     (if (variable-bound? #{v\ 2598}#)
                       (begin
                         (let ((#{val\ 2603}# (variable-ref #{v\ 2598}#)))
                           (if (macro? #{val\ 2603}#)
                             (if (macro-type #{val\ 2603}#)
                               (cons (macro-type #{val\ 2603}#)
                                     (macro-binding #{val\ 2603}#))
                               #f)
                             #f)))
                       #f)
                     #f))))))
         (#{put-global-definition-hook\ 301}#
           (lambda (#{symbol\ 2607}# #{type\ 2608}# #{val\ 2609}#)
             (module-define!
               (current-module)
               #{symbol\ 2607}#
               (make-syntax-transformer
                 #{symbol\ 2607}#
                 #{type\ 2608}#
                 #{val\ 2609}#))))
         (#{local-eval-hook\ 298}#
           (lambda (#{x\ 2613}# #{mod\ 2614}#)
             (primitive-eval #{x\ 2613}#)))
         (#{top-level-eval-hook\ 296}#
           (lambda (#{x\ 2617}# #{mod\ 2618}#)
             (primitive-eval #{x\ 2617}#)))
         (#{set-lambda-meta!\ 286}#
           (lambda (#{x\ 2621}# #{v\ 2622}#)
             (struct-set! #{x\ 2621}# 1 #{v\ 2622}#)))
         (#{lambda-meta\ 284}#
           (lambda (#{x\ 2625}#) (struct-ref #{x\ 2625}# 1)))
         (#{lambda?\ 282}#
           (lambda (#{x\ 2627}#)
             (if (struct? #{x\ 2627}#)
               (eq? (struct-vtable #{x\ 2627}#)
                    (vector-ref %expanded-vtables 13))
               #f)))
         (#{make-dynlet\ 279}#
           (lambda (#{src\ 2631}#
                    #{fluids\ 2632}#
                    #{vals\ 2633}#
                    #{body\ 2634}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 17)
               #{src\ 2631}#
               #{fluids\ 2632}#
               #{vals\ 2633}#
               #{body\ 2634}#)))
         (#{make-letrec\ 277}#
           (lambda (#{src\ 2639}#
                    #{in-order?\ 2640}#
                    #{names\ 2641}#
                    #{gensyms\ 2642}#
                    #{vals\ 2643}#
                    #{body\ 2644}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 16)
               #{src\ 2639}#
               #{in-order?\ 2640}#
               #{names\ 2641}#
               #{gensyms\ 2642}#
               #{vals\ 2643}#
               #{body\ 2644}#)))
         (#{make-let\ 275}#
           (lambda (#{src\ 2651}#
                    #{names\ 2652}#
                    #{gensyms\ 2653}#
                    #{vals\ 2654}#
                    #{body\ 2655}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 15)
               #{src\ 2651}#
               #{names\ 2652}#
               #{gensyms\ 2653}#
               #{vals\ 2654}#
               #{body\ 2655}#)))
         (#{make-lambda-case\ 273}#
           (lambda (#{src\ 2661}#
                    #{req\ 2662}#
                    #{opt\ 2663}#
                    #{rest\ 2664}#
                    #{kw\ 2665}#
                    #{inits\ 2666}#
                    #{gensyms\ 2667}#
                    #{body\ 2668}#
                    #{alternate\ 2669}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 14)
               #{src\ 2661}#
               #{req\ 2662}#
               #{opt\ 2663}#
               #{rest\ 2664}#
               #{kw\ 2665}#
               #{inits\ 2666}#
               #{gensyms\ 2667}#
               #{body\ 2668}#
               #{alternate\ 2669}#)))
         (#{make-lambda\ 271}#
           (lambda (#{src\ 2679}# #{meta\ 2680}# #{body\ 2681}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 13)
               #{src\ 2679}#
               #{meta\ 2680}#
               #{body\ 2681}#)))
         (#{make-sequence\ 269}#
           (lambda (#{src\ 2685}# #{exps\ 2686}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 12)
               #{src\ 2685}#
               #{exps\ 2686}#)))
         (#{make-application\ 267}#
           (lambda (#{src\ 2689}# #{proc\ 2690}# #{args\ 2691}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 11)
               #{src\ 2689}#
               #{proc\ 2690}#
               #{args\ 2691}#)))
         (#{make-conditional\ 265}#
           (lambda (#{src\ 2695}#
                    #{test\ 2696}#
                    #{consequent\ 2697}#
                    #{alternate\ 2698}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 10)
               #{src\ 2695}#
               #{test\ 2696}#
               #{consequent\ 2697}#
               #{alternate\ 2698}#)))
         (#{make-toplevel-define\ 263}#
           (lambda (#{src\ 2703}# #{name\ 2704}# #{exp\ 2705}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 9)
               #{src\ 2703}#
               #{name\ 2704}#
               #{exp\ 2705}#)))
         (#{make-toplevel-set\ 261}#
           (lambda (#{src\ 2709}# #{name\ 2710}# #{exp\ 2711}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 8)
               #{src\ 2709}#
               #{name\ 2710}#
               #{exp\ 2711}#)))
         (#{make-toplevel-ref\ 259}#
           (lambda (#{src\ 2715}# #{name\ 2716}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 7)
               #{src\ 2715}#
               #{name\ 2716}#)))
         (#{make-module-set\ 257}#
           (lambda (#{src\ 2719}#
                    #{mod\ 2720}#
                    #{name\ 2721}#
                    #{public?\ 2722}#
                    #{exp\ 2723}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 6)
               #{src\ 2719}#
               #{mod\ 2720}#
               #{name\ 2721}#
               #{public?\ 2722}#
               #{exp\ 2723}#)))
         (#{make-module-ref\ 255}#
           (lambda (#{src\ 2729}#
                    #{mod\ 2730}#
                    #{name\ 2731}#
                    #{public?\ 2732}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 5)
               #{src\ 2729}#
               #{mod\ 2730}#
               #{name\ 2731}#
               #{public?\ 2732}#)))
         (#{make-lexical-set\ 253}#
           (lambda (#{src\ 2737}#
                    #{name\ 2738}#
                    #{gensym\ 2739}#
                    #{exp\ 2740}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 4)
               #{src\ 2737}#
               #{name\ 2738}#
               #{gensym\ 2739}#
               #{exp\ 2740}#)))
         (#{make-lexical-ref\ 251}#
           (lambda (#{src\ 2745}# #{name\ 2746}# #{gensym\ 2747}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 3)
               #{src\ 2745}#
               #{name\ 2746}#
               #{gensym\ 2747}#)))
         (#{make-const\ 247}#
           (lambda (#{src\ 2755}# #{exp\ 2756}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 1)
               #{src\ 2755}#
               #{exp\ 2756}#)))
         (#{make-void\ 245}#
           (lambda (#{src\ 2759}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 0)
               #{src\ 2759}#))))
        (begin
          (set! #{ribcage?\ 410}#
            (lambda (#{x\ 2286}#)
              (if (vector? #{x\ 2286}#)
                (if (= (vector-length #{x\ 2286}#) 4)
                  (eq? (vector-ref #{x\ 2286}# 0) (quote ribcage))
                  #f)
                #f)))
          (set! #{wrap-subst\ 396}# cdr)
          (set! #{wrap-marks\ 394}# car)
          (set! #{make-wrap\ 392}# cons)
          (set! #{binding-value\ 372}# cdr)
          (set! #{binding-type\ 370}# car)
          (set! #{set-syntax-object-module!\ 363}#
            (lambda (#{x\ 2365}# #{update\ 2366}#)
              (vector-set! #{x\ 2365}# 3 #{update\ 2366}#)))
          (set! #{set-syntax-object-wrap!\ 361}#
            (lambda (#{x\ 2369}# #{update\ 2370}#)
              (vector-set! #{x\ 2369}# 2 #{update\ 2370}#)))
          (set! #{set-syntax-object-expression!\ 359}#
            (lambda (#{x\ 2373}# #{update\ 2374}#)
              (vector-set! #{x\ 2373}# 1 #{update\ 2374}#)))
          (set! #{fx<\ 294}# <)
          (set! #{fx=\ 292}# =)
          (set! #{fx-\ 290}# -)
          (set! #{fx+\ 288}# +)
          (set! #{make-primitive-ref\ 249}#
            (lambda (#{src\ 2751}# #{name\ 2752}#)
              (make-struct/no-tail
                (vector-ref %expanded-vtables 2)
                #{src\ 2751}#
                #{name\ 2752}#)))
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
                ((#{regen\ 2850}#
                   (lambda (#{x\ 2851}#)
                     (begin
                       (let ((#{atom-key\ 2855}# (car #{x\ 2851}#)))
                         (if (eqv? #{atom-key\ 2855}# (quote ref))
                           (#{build-lexical-reference\ 315}#
                             'value
                             #f
                             (car (cdr #{x\ 2851}#))
                             (car (cdr #{x\ 2851}#)))
                           (if (eqv? #{atom-key\ 2855}# (quote primitive))
                             (#{build-primref\ 335}#
                               #f
                               (car (cdr #{x\ 2851}#)))
                             (if (eqv? #{atom-key\ 2855}# (quote quote))
                               (#{build-data\ 337}# #f (car (cdr #{x\ 2851}#)))
                               (if (eqv? #{atom-key\ 2855}# (quote lambda))
                                 (if (list? (car (cdr #{x\ 2851}#)))
                                   (#{build-simple-lambda\ 329}#
                                     #f
                                     (car (cdr #{x\ 2851}#))
                                     #f
                                     (car (cdr #{x\ 2851}#))
                                     '()
                                     (#{regen\ 2850}#
                                       (car (cdr (cdr #{x\ 2851}#)))))
                                   (error "how did we get here" #{x\ 2851}#))
                                 (#{build-application\ 309}#
                                   #f
                                   (#{build-primref\ 335}#
                                     #f
                                     (car #{x\ 2851}#))
                                   (map #{regen\ 2850}#
                                        (cdr #{x\ 2851}#)))))))))))
                 (#{gen-vector\ 2848}#
                   (lambda (#{x\ 2867}#)
                     (if (eq? (car #{x\ 2867}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 2867}#))
                       (if (eq? (car #{x\ 2867}#) (quote quote))
                         (list 'quote
                               (list->vector (car (cdr #{x\ 2867}#))))
                         (list (quote list->vector) #{x\ 2867}#)))))
                 (#{gen-append\ 2846}#
                   (lambda (#{x\ 2877}# #{y\ 2878}#)
                     (if (equal? #{y\ 2878}# (quote (quote ())))
                       #{x\ 2877}#
                       (list (quote append) #{x\ 2877}# #{y\ 2878}#))))
                 (#{gen-cons\ 2844}#
                   (lambda (#{x\ 2882}# #{y\ 2883}#)
                     (begin
                       (let ((#{atom-key\ 2888}# (car #{y\ 2883}#)))
                         (if (eqv? #{atom-key\ 2888}# (quote quote))
                           (if (eq? (car #{x\ 2882}#) (quote quote))
                             (list 'quote
                                   (cons (car (cdr #{x\ 2882}#))
                                         (car (cdr #{y\ 2883}#))))
                             (if (eq? (car (cdr #{y\ 2883}#)) (quote ()))
                               (list (quote list) #{x\ 2882}#)
                               (list (quote cons) #{x\ 2882}# #{y\ 2883}#)))
                           (if (eqv? #{atom-key\ 2888}# (quote list))
                             (cons 'list
                                   (cons #{x\ 2882}# (cdr #{y\ 2883}#)))
                             (list (quote cons) #{x\ 2882}# #{y\ 2883}#)))))))
                 (#{gen-map\ 2842}#
                   (lambda (#{e\ 2897}# #{map-env\ 2898}#)
                     (begin
                       (let ((#{formals\ 2903}# (map cdr #{map-env\ 2898}#))
                             (#{actuals\ 2904}#
                               (map (lambda (#{x\ 2905}#)
                                      (list (quote ref) (car #{x\ 2905}#)))
                                    #{map-env\ 2898}#)))
                         (if (eq? (car #{e\ 2897}#) (quote ref))
                           (car #{actuals\ 2904}#)
                           (if (and-map
                                 (lambda (#{x\ 2912}#)
                                   (if (eq? (car #{x\ 2912}#) (quote ref))
                                     (memq (car (cdr #{x\ 2912}#))
                                           #{formals\ 2903}#)
                                     #f))
                                 (cdr #{e\ 2897}#))
                             (cons 'map
                                   (cons (list 'primitive
                                               (car #{e\ 2897}#))
                                         (map (begin
                                                (let ((#{r\ 2918}#
                                                        (map cons
                                                             #{formals\ 2903}#
                                                             #{actuals\ 2904}#)))
                                                  (lambda (#{x\ 2919}#)
                                                    (cdr (assq (car (cdr #{x\ 2919}#))
                                                               #{r\ 2918}#)))))
                                              (cdr #{e\ 2897}#))))
                             (cons 'map
                                   (cons (list 'lambda
                                               #{formals\ 2903}#
                                               #{e\ 2897}#)
                                         #{actuals\ 2904}#))))))))
                 (#{gen-mappend\ 2840}#
                   (lambda (#{e\ 2923}# #{map-env\ 2924}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 2842}# #{e\ 2923}# #{map-env\ 2924}#))))
                 (#{gen-ref\ 2838}#
                   (lambda (#{src\ 2928}#
                            #{var\ 2929}#
                            #{level\ 2930}#
                            #{maps\ 2931}#)
                     (if (#{fx=\ 292}# #{level\ 2930}# 0)
                       (values #{var\ 2929}# #{maps\ 2931}#)
                       (if (null? #{maps\ 2931}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 2928}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 2838}#
                               #{src\ 2928}#
                               #{var\ 2929}#
                               (#{fx-\ 290}# #{level\ 2930}# 1)
                               (cdr #{maps\ 2931}#)))
                           (lambda (#{outer-var\ 2936}# #{outer-maps\ 2937}#)
                             (begin
                               (let ((#{b\ 2941}#
                                       (assq #{outer-var\ 2936}#
                                             (car #{maps\ 2931}#))))
                                 (if #{b\ 2941}#
                                   (values (cdr #{b\ 2941}#) #{maps\ 2931}#)
                                   (begin
                                     (let ((#{inner-var\ 2943}#
                                             (#{gen-var\ 500}# (quote tmp))))
                                       (values
                                         #{inner-var\ 2943}#
                                         (cons (cons (cons #{outer-var\ 2936}#
                                                           #{inner-var\ 2943}#)
                                                     (car #{maps\ 2931}#))
                                               #{outer-maps\ 2937}#)))))))))))))
                 (#{gen-syntax\ 2836}#
                   (lambda (#{src\ 2944}#
                            #{e\ 2945}#
                            #{r\ 2946}#
                            #{maps\ 2947}#
                            #{ellipsis?\ 2948}#
                            #{mod\ 2949}#)
                     (if (#{id?\ 387}# #{e\ 2945}#)
                       (begin
                         (let ((#{label\ 2957}#
                                 (#{id-var-name\ 444}#
                                   #{e\ 2945}#
                                   '(()))))
                           (begin
                             (let ((#{b\ 2960}#
                                     (#{lookup\ 381}#
                                       #{label\ 2957}#
                                       #{r\ 2946}#
                                       #{mod\ 2949}#)))
                               (if (eq? (#{binding-type\ 370}# #{b\ 2960}#)
                                        'syntax)
                                 (call-with-values
                                   (lambda ()
                                     (begin
                                       (let ((#{var.lev\ 2962}#
                                               (#{binding-value\ 372}#
                                                 #{b\ 2960}#)))
                                         (#{gen-ref\ 2838}#
                                           #{src\ 2944}#
                                           (car #{var.lev\ 2962}#)
                                           (cdr #{var.lev\ 2962}#)
                                           #{maps\ 2947}#))))
                                   (lambda (#{var\ 2963}# #{maps\ 2964}#)
                                     (values
                                       (list (quote ref) #{var\ 2963}#)
                                       #{maps\ 2964}#)))
                                 (if (#{ellipsis?\ 2948}# #{e\ 2945}#)
                                   (syntax-violation
                                     'syntax
                                     "misplaced ellipsis"
                                     #{src\ 2944}#)
                                   (values
                                     (list (quote quote) #{e\ 2945}#)
                                     #{maps\ 2947}#)))))))
                       (let ((#{tmp\ 2969}# #{e\ 2945}#))
                         (let ((#{tmp\ 2970}#
                                 ($sc-dispatch
                                   #{tmp\ 2969}#
                                   '(any any))))
                           (if (if #{tmp\ 2970}#
                                 (@apply
                                   (lambda (#{dots\ 2973}# #{e\ 2974}#)
                                     (#{ellipsis?\ 2948}# #{dots\ 2973}#))
                                   #{tmp\ 2970}#)
                                 #f)
                             (@apply
                               (lambda (#{dots\ 2977}# #{e\ 2978}#)
                                 (#{gen-syntax\ 2836}#
                                   #{src\ 2944}#
                                   #{e\ 2978}#
                                   #{r\ 2946}#
                                   #{maps\ 2947}#
                                   (lambda (#{x\ 2979}#) #f)
                                   #{mod\ 2949}#))
                               #{tmp\ 2970}#)
                             (let ((#{tmp\ 2981}#
                                     ($sc-dispatch
                                       #{tmp\ 2969}#
                                       '(any any . any))))
                               (if (if #{tmp\ 2981}#
                                     (@apply
                                       (lambda (#{x\ 2985}#
                                                #{dots\ 2986}#
                                                #{y\ 2987}#)
                                         (#{ellipsis?\ 2948}# #{dots\ 2986}#))
                                       #{tmp\ 2981}#)
                                     #f)
                                 (@apply
                                   (lambda (#{x\ 2991}#
                                            #{dots\ 2992}#
                                            #{y\ 2993}#)
                                     (letrec*
                                       ((#{f\ 2997}#
                                          (lambda (#{y\ 2998}# #{k\ 2999}#)
                                            (let ((#{tmp\ 3006}# #{y\ 2998}#))
                                              (let ((#{tmp\ 3007}#
                                                      ($sc-dispatch
                                                        #{tmp\ 3006}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 3007}#
                                                      (@apply
                                                        (lambda (#{dots\ 3010}#
                                                                 #{y\ 3011}#)
                                                          (#{ellipsis?\ 2948}#
                                                            #{dots\ 3010}#))
                                                        #{tmp\ 3007}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{dots\ 3014}#
                                                             #{y\ 3015}#)
                                                      (#{f\ 2997}#
                                                        #{y\ 3015}#
                                                        (lambda (#{maps\ 3016}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{k\ 2999}#
                                                                (cons '()
                                                                      #{maps\ 3016}#)))
                                                            (lambda (#{x\ 3018}#
                                                                     #{maps\ 3019}#)
                                                              (if (null? (car #{maps\ 3019}#))
                                                                (syntax-violation
                                                                  'syntax
                                                                  "extra ellipsis"
                                                                  #{src\ 2944}#)
                                                                (values
                                                                  (#{gen-mappend\ 2840}#
                                                                    #{x\ 3018}#
                                                                    (car #{maps\ 3019}#))
                                                                  (cdr #{maps\ 3019}#))))))))
                                                    #{tmp\ 3007}#)
                                                  (let ((#{_\ 3023}#
                                                          #{tmp\ 3006}#))
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{gen-syntax\ 2836}#
                                                          #{src\ 2944}#
                                                          #{y\ 2998}#
                                                          #{r\ 2946}#
                                                          #{maps\ 2947}#
                                                          #{ellipsis?\ 2948}#
                                                          #{mod\ 2949}#))
                                                      (lambda (#{y\ 3024}#
                                                               #{maps\ 3025}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{k\ 2999}#
                                                              #{maps\ 3025}#))
                                                          (lambda (#{x\ 3028}#
                                                                   #{maps\ 3029}#)
                                                            (values
                                                              (#{gen-append\ 2846}#
                                                                #{x\ 3028}#
                                                                #{y\ 3024}#)
                                                              #{maps\ 3029}#))))))))))))
                                       (begin
                                         (#{f\ 2997}#
                                           #{y\ 2993}#
                                           (lambda (#{maps\ 3000}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2836}#
                                                   #{src\ 2944}#
                                                   #{x\ 2991}#
                                                   #{r\ 2946}#
                                                   (cons '()
                                                         #{maps\ 3000}#)
                                                   #{ellipsis?\ 2948}#
                                                   #{mod\ 2949}#))
                                               (lambda (#{x\ 3002}#
                                                        #{maps\ 3003}#)
                                                 (if (null? (car #{maps\ 3003}#))
                                                   (syntax-violation
                                                     'syntax
                                                     "extra ellipsis"
                                                     #{src\ 2944}#)
                                                   (values
                                                     (#{gen-map\ 2842}#
                                                       #{x\ 3002}#
                                                       (car #{maps\ 3003}#))
                                                     (cdr #{maps\ 3003}#))))))))))
                                   #{tmp\ 2981}#)
                                 (let ((#{tmp\ 3032}#
                                         ($sc-dispatch
                                           #{tmp\ 2969}#
                                           '(any . any))))
                                   (if #{tmp\ 3032}#
                                     (@apply
                                       (lambda (#{x\ 3035}# #{y\ 3036}#)
                                         (call-with-values
                                           (lambda ()
                                             (#{gen-syntax\ 2836}#
                                               #{src\ 2944}#
                                               #{x\ 3035}#
                                               #{r\ 2946}#
                                               #{maps\ 2947}#
                                               #{ellipsis?\ 2948}#
                                               #{mod\ 2949}#))
                                           (lambda (#{x\ 3037}# #{maps\ 3038}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2836}#
                                                   #{src\ 2944}#
                                                   #{y\ 3036}#
                                                   #{r\ 2946}#
                                                   #{maps\ 3038}#
                                                   #{ellipsis?\ 2948}#
                                                   #{mod\ 2949}#))
                                               (lambda (#{y\ 3041}#
                                                        #{maps\ 3042}#)
                                                 (values
                                                   (#{gen-cons\ 2844}#
                                                     #{x\ 3037}#
                                                     #{y\ 3041}#)
                                                   #{maps\ 3042}#))))))
                                       #{tmp\ 3032}#)
                                     (let ((#{tmp\ 3045}#
                                             ($sc-dispatch
                                               #{tmp\ 2969}#
                                               '#(vector (any . each-any)))))
                                       (if #{tmp\ 3045}#
                                         (@apply
                                           (lambda (#{e1\ 3048}# #{e2\ 3049}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2836}#
                                                   #{src\ 2944}#
                                                   (cons #{e1\ 3048}#
                                                         #{e2\ 3049}#)
                                                   #{r\ 2946}#
                                                   #{maps\ 2947}#
                                                   #{ellipsis?\ 2948}#
                                                   #{mod\ 2949}#))
                                               (lambda (#{e\ 3051}#
                                                        #{maps\ 3052}#)
                                                 (values
                                                   (#{gen-vector\ 2848}#
                                                     #{e\ 3051}#)
                                                   #{maps\ 3052}#))))
                                           #{tmp\ 3045}#)
                                         (let ((#{_\ 3056}# #{tmp\ 2969}#))
                                           (values
                                             (list (quote quote) #{e\ 2945}#)
                                             #{maps\ 2947}#)))))))))))))))
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
                ((#{gen-syntax-case\ 3671}#
                   (lambda (#{x\ 3672}#
                            #{keys\ 3673}#
                            #{clauses\ 3674}#
                            #{r\ 3675}#
                            #{mod\ 3676}#)
                     (if (null? #{clauses\ 3674}#)
                       (#{build-application\ 309}#
                         #f
                         (#{build-primref\ 335}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 337}# #f #f)
                               (#{build-data\ 337}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 3672}#))
                       (let ((#{tmp\ 3686}# (car #{clauses\ 3674}#)))
                         (let ((#{tmp\ 3687}#
                                 ($sc-dispatch
                                   #{tmp\ 3686}#
                                   '(any any))))
                           (if #{tmp\ 3687}#
                             (@apply
                               (lambda (#{pat\ 3690}# #{exp\ 3691}#)
                                 (if (if (#{id?\ 387}# #{pat\ 3690}#)
                                       (and-map
                                         (lambda (#{x\ 3694}#)
                                           (not (#{free-id=?\ 446}#
                                                  #{pat\ 3690}#
                                                  #{x\ 3694}#)))
                                         (cons '#(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(pat exp)
                                                     #((top) (top))
                                                     #("i3688" "i3689"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x keys clauses r mod)
                                                     #((top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top))
                                                     #("i3677"
                                                       "i3678"
                                                       "i3679"
                                                       "i3680"
                                                       "i3681"))
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
                                               #{keys\ 3673}#))
                                       #f)
                                   (begin
                                     (let ((#{labels\ 3698}#
                                             (list (#{gen-label\ 403}#)))
                                           (#{var\ 3699}#
                                             (#{gen-var\ 500}# #{pat\ 3690}#)))
                                       (#{build-application\ 309}#
                                         #f
                                         (#{build-simple-lambda\ 329}#
                                           #f
                                           (list (syntax->datum #{pat\ 3690}#))
                                           #f
                                           (list #{var\ 3699}#)
                                           '()
                                           (#{chi\ 472}#
                                             #{exp\ 3691}#
                                             (#{extend-env\ 375}#
                                               #{labels\ 3698}#
                                               (list (cons 'syntax
                                                           (cons #{var\ 3699}#
                                                                 0)))
                                               #{r\ 3675}#)
                                             (#{make-binding-wrap\ 434}#
                                               (list #{pat\ 3690}#)
                                               #{labels\ 3698}#
                                               '(()))
                                             #{mod\ 3676}#))
                                         (list #{x\ 3672}#))))
                                   (#{gen-clause\ 3669}#
                                     #{x\ 3672}#
                                     #{keys\ 3673}#
                                     (cdr #{clauses\ 3674}#)
                                     #{r\ 3675}#
                                     #{pat\ 3690}#
                                     #t
                                     #{exp\ 3691}#
                                     #{mod\ 3676}#)))
                               #{tmp\ 3687}#)
                             (let ((#{tmp\ 3705}#
                                     ($sc-dispatch
                                       #{tmp\ 3686}#
                                       '(any any any))))
                               (if #{tmp\ 3705}#
                                 (@apply
                                   (lambda (#{pat\ 3709}#
                                            #{fender\ 3710}#
                                            #{exp\ 3711}#)
                                     (#{gen-clause\ 3669}#
                                       #{x\ 3672}#
                                       #{keys\ 3673}#
                                       (cdr #{clauses\ 3674}#)
                                       #{r\ 3675}#
                                       #{pat\ 3709}#
                                       #{fender\ 3710}#
                                       #{exp\ 3711}#
                                       #{mod\ 3676}#))
                                   #{tmp\ 3705}#)
                                 (let ((#{_\ 3713}# #{tmp\ 3686}#))
                                   (syntax-violation
                                     'syntax-case
                                     "invalid clause"
                                     (car #{clauses\ 3674}#)))))))))))
                 (#{gen-clause\ 3669}#
                   (lambda (#{x\ 3714}#
                            #{keys\ 3715}#
                            #{clauses\ 3716}#
                            #{r\ 3717}#
                            #{pat\ 3718}#
                            #{fender\ 3719}#
                            #{exp\ 3720}#
                            #{mod\ 3721}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 3665}#
                           #{pat\ 3718}#
                           #{keys\ 3715}#))
                       (lambda (#{p\ 3730}# #{pvars\ 3731}#)
                         (if (not (#{distinct-bound-ids?\ 452}#
                                    (map car #{pvars\ 3731}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 3718}#)
                           (if (not (and-map
                                      (lambda (#{x\ 3738}#)
                                        (not (#{ellipsis?\ 488}#
                                               (car #{x\ 3738}#))))
                                      #{pvars\ 3731}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 3718}#)
                             (begin
                               (let ((#{y\ 3742}#
                                       (#{gen-var\ 500}# (quote tmp))))
                                 (#{build-application\ 309}#
                                   #f
                                   (#{build-simple-lambda\ 329}#
                                     #f
                                     (list (quote tmp))
                                     #f
                                     (list #{y\ 3742}#)
                                     '()
                                     (begin
                                       (let ((#{y\ 3746}#
                                               (#{build-lexical-reference\ 315}#
                                                 'value
                                                 #f
                                                 'tmp
                                                 #{y\ 3742}#)))
                                         (#{build-conditional\ 311}#
                                           #f
                                           (let ((#{tmp\ 3749}#
                                                   #{fender\ 3719}#))
                                             (let ((#{tmp\ 3750}#
                                                     ($sc-dispatch
                                                       #{tmp\ 3749}#
                                                       '#(atom #t))))
                                               (if #{tmp\ 3750}#
                                                 (@apply
                                                   (lambda () #{y\ 3746}#)
                                                   #{tmp\ 3750}#)
                                                 (let ((#{_\ 3752}#
                                                         #{tmp\ 3749}#))
                                                   (#{build-conditional\ 311}#
                                                     #f
                                                     #{y\ 3746}#
                                                     (#{build-dispatch-call\ 3667}#
                                                       #{pvars\ 3731}#
                                                       #{fender\ 3719}#
                                                       #{y\ 3746}#
                                                       #{r\ 3717}#
                                                       #{mod\ 3721}#)
                                                     (#{build-data\ 337}#
                                                       #f
                                                       #f))))))
                                           (#{build-dispatch-call\ 3667}#
                                             #{pvars\ 3731}#
                                             #{exp\ 3720}#
                                             #{y\ 3746}#
                                             #{r\ 3717}#
                                             #{mod\ 3721}#)
                                           (#{gen-syntax-case\ 3671}#
                                             #{x\ 3714}#
                                             #{keys\ 3715}#
                                             #{clauses\ 3716}#
                                             #{r\ 3717}#
                                             #{mod\ 3721}#)))))
                                   (list (if (eq? #{p\ 3730}# (quote any))
                                           (#{build-application\ 309}#
                                             #f
                                             (#{build-primref\ 335}#
                                               #f
                                               'list)
                                             (list #{x\ 3714}#))
                                           (#{build-application\ 309}#
                                             #f
                                             (#{build-primref\ 335}#
                                               #f
                                               '$sc-dispatch)
                                             (list #{x\ 3714}#
                                                   (#{build-data\ 337}#
                                                     #f
                                                     #{p\ 3730}#))))))))))))))
                 (#{build-dispatch-call\ 3667}#
                   (lambda (#{pvars\ 3760}#
                            #{exp\ 3761}#
                            #{y\ 3762}#
                            #{r\ 3763}#
                            #{mod\ 3764}#)
                     (begin
                       (map cdr #{pvars\ 3760}#)
                       (let ((#{ids\ 3772}# (map car #{pvars\ 3760}#)))
                         (begin
                           (let ((#{labels\ 3776}#
                                   (#{gen-labels\ 405}# #{ids\ 3772}#))
                                 (#{new-vars\ 3777}#
                                   (map #{gen-var\ 500}# #{ids\ 3772}#)))
                             (#{build-application\ 309}#
                               #f
                               (#{build-primref\ 335}# #f (quote apply))
                               (list (#{build-simple-lambda\ 329}#
                                       #f
                                       (map syntax->datum #{ids\ 3772}#)
                                       #f
                                       #{new-vars\ 3777}#
                                       '()
                                       (#{chi\ 472}#
                                         #{exp\ 3761}#
                                         (#{extend-env\ 375}#
                                           #{labels\ 3776}#
                                           (map (lambda (#{var\ 3781}#
                                                         #{level\ 3782}#)
                                                  (cons 'syntax
                                                        (cons #{var\ 3781}#
                                                              #{level\ 3782}#)))
                                                #{new-vars\ 3777}#
                                                (map cdr #{pvars\ 3760}#))
                                           #{r\ 3763}#)
                                         (#{make-binding-wrap\ 434}#
                                           #{ids\ 3772}#
                                           #{labels\ 3776}#
                                           '(()))
                                         #{mod\ 3764}#))
                                     #{y\ 3762}#))))))))
                 (#{convert-pattern\ 3665}#
                   (lambda (#{pattern\ 3788}# #{keys\ 3789}#)
                     (letrec*
                       ((#{cvt\ 3795}#
                          (lambda (#{p\ 3796}# #{n\ 3797}# #{ids\ 3798}#)
                            (if (#{id?\ 387}# #{p\ 3796}#)
                              (if (#{bound-id-member?\ 454}#
                                    #{p\ 3796}#
                                    #{keys\ 3789}#)
                                (values
                                  (vector (quote free-id) #{p\ 3796}#)
                                  #{ids\ 3798}#)
                                (values
                                  'any
                                  (cons (cons #{p\ 3796}# #{n\ 3797}#)
                                        #{ids\ 3798}#)))
                              (let ((#{tmp\ 3802}# #{p\ 3796}#))
                                (let ((#{tmp\ 3803}#
                                        ($sc-dispatch
                                          #{tmp\ 3802}#
                                          '(any any))))
                                  (if (if #{tmp\ 3803}#
                                        (@apply
                                          (lambda (#{x\ 3806}# #{dots\ 3807}#)
                                            (#{ellipsis?\ 488}#
                                              #{dots\ 3807}#))
                                          #{tmp\ 3803}#)
                                        #f)
                                    (@apply
                                      (lambda (#{x\ 3810}# #{dots\ 3811}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt\ 3795}#
                                              #{x\ 3810}#
                                              (#{fx+\ 288}# #{n\ 3797}# 1)
                                              #{ids\ 3798}#))
                                          (lambda (#{p\ 3812}# #{ids\ 3813}#)
                                            (values
                                              (if (eq? #{p\ 3812}# (quote any))
                                                'each-any
                                                (vector
                                                  'each
                                                  #{p\ 3812}#))
                                              #{ids\ 3813}#))))
                                      #{tmp\ 3803}#)
                                    (let ((#{tmp\ 3816}#
                                            ($sc-dispatch
                                              #{tmp\ 3802}#
                                              '(any any . each-any))))
                                      (if (if #{tmp\ 3816}#
                                            (@apply
                                              (lambda (#{x\ 3820}#
                                                       #{dots\ 3821}#
                                                       #{ys\ 3822}#)
                                                (#{ellipsis?\ 488}#
                                                  #{dots\ 3821}#))
                                              #{tmp\ 3816}#)
                                            #f)
                                        (@apply
                                          (lambda (#{x\ 3826}#
                                                   #{dots\ 3827}#
                                                   #{ys\ 3828}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{cvt*\ 3793}#
                                                  #{ys\ 3828}#
                                                  #{n\ 3797}#
                                                  #{ids\ 3798}#))
                                              (lambda (#{ys\ 3830}#
                                                       #{ids\ 3831}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 3795}#
                                                      #{x\ 3826}#
                                                      (1+ #{n\ 3797}#)
                                                      #{ids\ 3831}#))
                                                  (lambda (#{x\ 3834}#
                                                           #{ids\ 3835}#)
                                                    (values
                                                      (list->vector
                                                        (cons 'each+
                                                              (cons #{x\ 3834}#
                                                                    (cons (reverse
                                                                            #{ys\ 3830}#)
                                                                          '(())))))
                                                      #{ids\ 3835}#))))))
                                          #{tmp\ 3816}#)
                                        (let ((#{tmp\ 3839}#
                                                ($sc-dispatch
                                                  #{tmp\ 3802}#
                                                  '(any . any))))
                                          (if #{tmp\ 3839}#
                                            (@apply
                                              (lambda (#{x\ 3842}# #{y\ 3843}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 3795}#
                                                      #{y\ 3843}#
                                                      #{n\ 3797}#
                                                      #{ids\ 3798}#))
                                                  (lambda (#{y\ 3844}#
                                                           #{ids\ 3845}#)
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{cvt\ 3795}#
                                                          #{x\ 3842}#
                                                          #{n\ 3797}#
                                                          #{ids\ 3845}#))
                                                      (lambda (#{x\ 3848}#
                                                               #{ids\ 3849}#)
                                                        (values
                                                          (cons #{x\ 3848}#
                                                                #{y\ 3844}#)
                                                          #{ids\ 3849}#))))))
                                              #{tmp\ 3839}#)
                                            (let ((#{tmp\ 3852}#
                                                    ($sc-dispatch
                                                      #{tmp\ 3802}#
                                                      '())))
                                              (if #{tmp\ 3852}#
                                                (@apply
                                                  (lambda ()
                                                    (values
                                                      '()
                                                      #{ids\ 3798}#))
                                                  #{tmp\ 3852}#)
                                                (let ((#{tmp\ 3853}#
                                                        ($sc-dispatch
                                                          #{tmp\ 3802}#
                                                          '#(vector
                                                             each-any))))
                                                  (if #{tmp\ 3853}#
                                                    (@apply
                                                      (lambda (#{x\ 3855}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{cvt\ 3795}#
                                                              #{x\ 3855}#
                                                              #{n\ 3797}#
                                                              #{ids\ 3798}#))
                                                          (lambda (#{p\ 3857}#
                                                                   #{ids\ 3858}#)
                                                            (values
                                                              (vector
                                                                'vector
                                                                #{p\ 3857}#)
                                                              #{ids\ 3858}#))))
                                                      #{tmp\ 3853}#)
                                                    (let ((#{x\ 3862}#
                                                            #{tmp\ 3802}#))
                                                      (values
                                                        (vector
                                                          'atom
                                                          (#{strip\ 498}#
                                                            #{p\ 3796}#
                                                            '(())))
                                                        #{ids\ 3798}#))))))))))))))))
                        (#{cvt*\ 3793}#
                          (lambda (#{p*\ 3864}# #{n\ 3865}# #{ids\ 3866}#)
                            (if (null? #{p*\ 3864}#)
                              (values (quote ()) #{ids\ 3866}#)
                              (call-with-values
                                (lambda ()
                                  (#{cvt*\ 3793}#
                                    (cdr #{p*\ 3864}#)
                                    #{n\ 3865}#
                                    #{ids\ 3866}#))
                                (lambda (#{y\ 3870}# #{ids\ 3871}#)
                                  (call-with-values
                                    (lambda ()
                                      (#{cvt\ 3795}#
                                        (car #{p*\ 3864}#)
                                        #{n\ 3865}#
                                        #{ids\ 3871}#))
                                    (lambda (#{x\ 3874}# #{ids\ 3875}#)
                                      (values
                                        (cons #{x\ 3874}# #{y\ 3870}#)
                                        #{ids\ 3875}#)))))))))
                       (begin
                         (#{cvt\ 3795}# #{pattern\ 3788}# 0 (quote ())))))))
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
              ((#{match\ 4001}#
                 (lambda (#{e\ 4002}#
                          #{p\ 4003}#
                          #{w\ 4004}#
                          #{r\ 4005}#
                          #{mod\ 4006}#)
                   (if (not #{r\ 4005}#)
                     #f
                     (if (eq? #{p\ 4003}# (quote any))
                       (cons (#{wrap\ 456}#
                               #{e\ 4002}#
                               #{w\ 4004}#
                               #{mod\ 4006}#)
                             #{r\ 4005}#)
                       (if (#{syntax-object?\ 351}# #{e\ 4002}#)
                         (#{match*\ 3999}#
                           (#{syntax-object-expression\ 353}# #{e\ 4002}#)
                           #{p\ 4003}#
                           (#{join-wraps\ 438}#
                             #{w\ 4004}#
                             (#{syntax-object-wrap\ 355}# #{e\ 4002}#))
                           #{r\ 4005}#
                           (#{syntax-object-module\ 357}# #{e\ 4002}#))
                         (#{match*\ 3999}#
                           #{e\ 4002}#
                           #{p\ 4003}#
                           #{w\ 4004}#
                           #{r\ 4005}#
                           #{mod\ 4006}#))))))
               (#{match*\ 3999}#
                 (lambda (#{e\ 4019}#
                          #{p\ 4020}#
                          #{w\ 4021}#
                          #{r\ 4022}#
                          #{mod\ 4023}#)
                   (if (null? #{p\ 4020}#)
                     (if (null? #{e\ 4019}#) #{r\ 4022}# #f)
                     (if (pair? #{p\ 4020}#)
                       (if (pair? #{e\ 4019}#)
                         (#{match\ 4001}#
                           (car #{e\ 4019}#)
                           (car #{p\ 4020}#)
                           #{w\ 4021}#
                           (#{match\ 4001}#
                             (cdr #{e\ 4019}#)
                             (cdr #{p\ 4020}#)
                             #{w\ 4021}#
                             #{r\ 4022}#
                             #{mod\ 4023}#)
                           #{mod\ 4023}#)
                         #f)
                       (if (eq? #{p\ 4020}# (quote each-any))
                         (begin
                           (let ((#{l\ 4040}#
                                   (#{match-each-any\ 3993}#
                                     #{e\ 4019}#
                                     #{w\ 4021}#
                                     #{mod\ 4023}#)))
                             (if #{l\ 4040}#
                               (cons #{l\ 4040}# #{r\ 4022}#)
                               #f)))
                         (begin
                           (let ((#{atom-key\ 4046}#
                                   (vector-ref #{p\ 4020}# 0)))
                             (if (eqv? #{atom-key\ 4046}# (quote each))
                               (if (null? #{e\ 4019}#)
                                 (#{match-empty\ 3995}#
                                   (vector-ref #{p\ 4020}# 1)
                                   #{r\ 4022}#)
                                 (begin
                                   (let ((#{l\ 4049}#
                                           (#{match-each\ 3989}#
                                             #{e\ 4019}#
                                             (vector-ref #{p\ 4020}# 1)
                                             #{w\ 4021}#
                                             #{mod\ 4023}#)))
                                     (if #{l\ 4049}#
                                       (letrec*
                                         ((#{collect\ 4054}#
                                            (lambda (#{l\ 4055}#)
                                              (if (null? (car #{l\ 4055}#))
                                                #{r\ 4022}#
                                                (cons (map car #{l\ 4055}#)
                                                      (#{collect\ 4054}#
                                                        (map cdr
                                                             #{l\ 4055}#)))))))
                                         (begin
                                           (#{collect\ 4054}# #{l\ 4049}#)))
                                       #f))))
                               (if (eqv? #{atom-key\ 4046}# (quote each+))
                                 (call-with-values
                                   (lambda ()
                                     (#{match-each+\ 3991}#
                                       #{e\ 4019}#
                                       (vector-ref #{p\ 4020}# 1)
                                       (vector-ref #{p\ 4020}# 2)
                                       (vector-ref #{p\ 4020}# 3)
                                       #{w\ 4021}#
                                       #{r\ 4022}#
                                       #{mod\ 4023}#))
                                   (lambda (#{xr*\ 4057}#
                                            #{y-pat\ 4058}#
                                            #{r\ 4059}#)
                                     (if #{r\ 4059}#
                                       (if (null? #{y-pat\ 4058}#)
                                         (if (null? #{xr*\ 4057}#)
                                           (#{match-empty\ 3995}#
                                             (vector-ref #{p\ 4020}# 1)
                                             #{r\ 4059}#)
                                           (#{combine\ 3997}#
                                             #{xr*\ 4057}#
                                             #{r\ 4059}#))
                                         #f)
                                       #f)))
                                 (if (eqv? #{atom-key\ 4046}# (quote free-id))
                                   (if (#{id?\ 387}# #{e\ 4019}#)
                                     (if (#{free-id=?\ 446}#
                                           (#{wrap\ 456}#
                                             #{e\ 4019}#
                                             #{w\ 4021}#
                                             #{mod\ 4023}#)
                                           (vector-ref #{p\ 4020}# 1))
                                       #{r\ 4022}#
                                       #f)
                                     #f)
                                   (if (eqv? #{atom-key\ 4046}# (quote atom))
                                     (if (equal?
                                           (vector-ref #{p\ 4020}# 1)
                                           (#{strip\ 498}#
                                             #{e\ 4019}#
                                             #{w\ 4021}#))
                                       #{r\ 4022}#
                                       #f)
                                     (if (eqv? #{atom-key\ 4046}#
                                               'vector)
                                       (if (vector? #{e\ 4019}#)
                                         (#{match\ 4001}#
                                           (vector->list #{e\ 4019}#)
                                           (vector-ref #{p\ 4020}# 1)
                                           #{w\ 4021}#
                                           #{r\ 4022}#
                                           #{mod\ 4023}#)
                                         #f)))))))))))))
               (#{combine\ 3997}#
                 (lambda (#{r*\ 4076}# #{r\ 4077}#)
                   (if (null? (car #{r*\ 4076}#))
                     #{r\ 4077}#
                     (cons (map car #{r*\ 4076}#)
                           (#{combine\ 3997}#
                             (map cdr #{r*\ 4076}#)
                             #{r\ 4077}#)))))
               (#{match-empty\ 3995}#
                 (lambda (#{p\ 4080}# #{r\ 4081}#)
                   (if (null? #{p\ 4080}#)
                     #{r\ 4081}#
                     (if (eq? #{p\ 4080}# (quote any))
                       (cons (quote ()) #{r\ 4081}#)
                       (if (pair? #{p\ 4080}#)
                         (#{match-empty\ 3995}#
                           (car #{p\ 4080}#)
                           (#{match-empty\ 3995}#
                             (cdr #{p\ 4080}#)
                             #{r\ 4081}#))
                         (if (eq? #{p\ 4080}# (quote each-any))
                           (cons (quote ()) #{r\ 4081}#)
                           (begin
                             (let ((#{atom-key\ 4095}#
                                     (vector-ref #{p\ 4080}# 0)))
                               (if (eqv? #{atom-key\ 4095}# (quote each))
                                 (#{match-empty\ 3995}#
                                   (vector-ref #{p\ 4080}# 1)
                                   #{r\ 4081}#)
                                 (if (eqv? #{atom-key\ 4095}# (quote each+))
                                   (#{match-empty\ 3995}#
                                     (vector-ref #{p\ 4080}# 1)
                                     (#{match-empty\ 3995}#
                                       (reverse (vector-ref #{p\ 4080}# 2))
                                       (#{match-empty\ 3995}#
                                         (vector-ref #{p\ 4080}# 3)
                                         #{r\ 4081}#)))
                                   (if (if (eqv? #{atom-key\ 4095}#
                                                 'free-id)
                                         #t
                                         (eqv? #{atom-key\ 4095}#
                                               'atom))
                                     #{r\ 4081}#
                                     (if (eqv? #{atom-key\ 4095}#
                                               'vector)
                                       (#{match-empty\ 3995}#
                                         (vector-ref #{p\ 4080}# 1)
                                         #{r\ 4081}#)))))))))))))
               (#{match-each-any\ 3993}#
                 (lambda (#{e\ 4100}# #{w\ 4101}# #{mod\ 4102}#)
                   (if (pair? #{e\ 4100}#)
                     (begin
                       (let ((#{l\ 4109}#
                               (#{match-each-any\ 3993}#
                                 (cdr #{e\ 4100}#)
                                 #{w\ 4101}#
                                 #{mod\ 4102}#)))
                         (if #{l\ 4109}#
                           (cons (#{wrap\ 456}#
                                   (car #{e\ 4100}#)
                                   #{w\ 4101}#
                                   #{mod\ 4102}#)
                                 #{l\ 4109}#)
                           #f)))
                     (if (null? #{e\ 4100}#)
                       '()
                       (if (#{syntax-object?\ 351}# #{e\ 4100}#)
                         (#{match-each-any\ 3993}#
                           (#{syntax-object-expression\ 353}# #{e\ 4100}#)
                           (#{join-wraps\ 438}#
                             #{w\ 4101}#
                             (#{syntax-object-wrap\ 355}# #{e\ 4100}#))
                           #{mod\ 4102}#)
                         #f)))))
               (#{match-each+\ 3991}#
                 (lambda (#{e\ 4117}#
                          #{x-pat\ 4118}#
                          #{y-pat\ 4119}#
                          #{z-pat\ 4120}#
                          #{w\ 4121}#
                          #{r\ 4122}#
                          #{mod\ 4123}#)
                   (letrec*
                     ((#{f\ 4134}#
                        (lambda (#{e\ 4135}# #{w\ 4136}#)
                          (if (pair? #{e\ 4135}#)
                            (call-with-values
                              (lambda ()
                                (#{f\ 4134}# (cdr #{e\ 4135}#) #{w\ 4136}#))
                              (lambda (#{xr*\ 4139}#
                                       #{y-pat\ 4140}#
                                       #{r\ 4141}#)
                                (if #{r\ 4141}#
                                  (if (null? #{y-pat\ 4140}#)
                                    (begin
                                      (let ((#{xr\ 4146}#
                                              (#{match\ 4001}#
                                                (car #{e\ 4135}#)
                                                #{x-pat\ 4118}#
                                                #{w\ 4136}#
                                                '()
                                                #{mod\ 4123}#)))
                                        (if #{xr\ 4146}#
                                          (values
                                            (cons #{xr\ 4146}# #{xr*\ 4139}#)
                                            #{y-pat\ 4140}#
                                            #{r\ 4141}#)
                                          (values #f #f #f))))
                                    (values
                                      '()
                                      (cdr #{y-pat\ 4140}#)
                                      (#{match\ 4001}#
                                        (car #{e\ 4135}#)
                                        (car #{y-pat\ 4140}#)
                                        #{w\ 4136}#
                                        #{r\ 4141}#
                                        #{mod\ 4123}#)))
                                  (values #f #f #f))))
                            (if (#{syntax-object?\ 351}# #{e\ 4135}#)
                              (#{f\ 4134}#
                                (#{syntax-object-expression\ 353}# #{e\ 4135}#)
                                (#{join-wraps\ 438}# #{w\ 4136}# #{e\ 4135}#))
                              (values
                                '()
                                #{y-pat\ 4119}#
                                (#{match\ 4001}#
                                  #{e\ 4135}#
                                  #{z-pat\ 4120}#
                                  #{w\ 4136}#
                                  #{r\ 4122}#
                                  #{mod\ 4123}#)))))))
                     (begin (#{f\ 4134}# #{e\ 4117}# #{w\ 4121}#)))))
               (#{match-each\ 3989}#
                 (lambda (#{e\ 4150}#
                          #{p\ 4151}#
                          #{w\ 4152}#
                          #{mod\ 4153}#)
                   (if (pair? #{e\ 4150}#)
                     (begin
                       (let ((#{first\ 4161}#
                               (#{match\ 4001}#
                                 (car #{e\ 4150}#)
                                 #{p\ 4151}#
                                 #{w\ 4152}#
                                 '()
                                 #{mod\ 4153}#)))
                         (if #{first\ 4161}#
                           (begin
                             (let ((#{rest\ 4165}#
                                     (#{match-each\ 3989}#
                                       (cdr #{e\ 4150}#)
                                       #{p\ 4151}#
                                       #{w\ 4152}#
                                       #{mod\ 4153}#)))
                               (if #{rest\ 4165}#
                                 (cons #{first\ 4161}# #{rest\ 4165}#)
                                 #f)))
                           #f)))
                     (if (null? #{e\ 4150}#)
                       '()
                       (if (#{syntax-object?\ 351}# #{e\ 4150}#)
                         (#{match-each\ 3989}#
                           (#{syntax-object-expression\ 353}# #{e\ 4150}#)
                           #{p\ 4151}#
                           (#{join-wraps\ 438}#
                             #{w\ 4152}#
                             (#{syntax-object-wrap\ 355}# #{e\ 4150}#))
                           (#{syntax-object-module\ 357}# #{e\ 4150}#))
                         #f))))))
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

