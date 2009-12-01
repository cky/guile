(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((#{and-map*\ 3695}#
           (lambda (#{f\ 3733}# #{first\ 3734}# . #{rest\ 3735}#)
             (let ((#{t\ 3736}# (null? #{first\ 3734}#)))
               (if #{t\ 3736}#
                 #{t\ 3736}#
                 (if (null? #{rest\ 3735}#)
                   (letrec ((#{andmap\ 3737}#
                              (lambda (#{first\ 3738}#)
                                (let ((#{x\ 3739}# (car #{first\ 3738}#))
                                      (#{first\ 3740}# (cdr #{first\ 3738}#)))
                                  (if (null? #{first\ 3740}#)
                                    (#{f\ 3733}# #{x\ 3739}#)
                                    (if (#{f\ 3733}# #{x\ 3739}#)
                                      (#{andmap\ 3737}# #{first\ 3740}#)
                                      #f))))))
                     (#{andmap\ 3737}# #{first\ 3734}#))
                   (letrec ((#{andmap\ 3741}#
                              (lambda (#{first\ 3742}# #{rest\ 3743}#)
                                (let ((#{x\ 3744}# (car #{first\ 3742}#))
                                      (#{xr\ 3745}# (map car #{rest\ 3743}#))
                                      (#{first\ 3746}# (cdr #{first\ 3742}#))
                                      (#{rest\ 3747}#
                                        (map cdr #{rest\ 3743}#)))
                                  (if (null? #{first\ 3746}#)
                                    (apply #{f\ 3733}#
                                           (cons #{x\ 3744}# #{xr\ 3745}#))
                                    (if (apply #{f\ 3733}#
                                               (cons #{x\ 3744}# #{xr\ 3745}#))
                                      (#{andmap\ 3741}#
                                        #{first\ 3746}#
                                        #{rest\ 3747}#)
                                      #f))))))
                     (#{andmap\ 3741}# #{first\ 3734}# #{rest\ 3735}#))))))))
  (letrec ((#{lambda-var-list\ 3846}#
             (lambda (#{vars\ 3970}#)
               (letrec ((#{lvl\ 3971}#
                          (lambda (#{vars\ 3972}# #{ls\ 3973}# #{w\ 3974}#)
                            (if (pair? #{vars\ 3972}#)
                              (#{lvl\ 3971}#
                                (cdr #{vars\ 3972}#)
                                (cons (#{wrap\ 3823}#
                                        (car #{vars\ 3972}#)
                                        #{w\ 3974}#
                                        #f)
                                      #{ls\ 3973}#)
                                #{w\ 3974}#)
                              (if (#{id?\ 3795}# #{vars\ 3972}#)
                                (cons (#{wrap\ 3823}#
                                        #{vars\ 3972}#
                                        #{w\ 3974}#
                                        #f)
                                      #{ls\ 3973}#)
                                (if (null? #{vars\ 3972}#)
                                  #{ls\ 3973}#
                                  (if (#{syntax-object?\ 3779}# #{vars\ 3972}#)
                                    (#{lvl\ 3971}#
                                      (#{syntax-object-expression\ 3780}#
                                        #{vars\ 3972}#)
                                      #{ls\ 3973}#
                                      (#{join-wraps\ 3814}#
                                        #{w\ 3974}#
                                        (#{syntax-object-wrap\ 3781}#
                                          #{vars\ 3972}#)))
                                    (cons #{vars\ 3972}# #{ls\ 3973}#))))))))
                 (#{lvl\ 3971}#
                   #{vars\ 3970}#
                   '()
                   '(())))))
           (#{gen-var\ 3845}#
             (lambda (#{id\ 3975}#)
               (let ((#{id\ 3976}#
                       (if (#{syntax-object?\ 3779}# #{id\ 3975}#)
                         (#{syntax-object-expression\ 3780}# #{id\ 3975}#)
                         #{id\ 3975}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 3976}#) " ")))))
           (#{strip\ 3844}#
             (lambda (#{x\ 3977}# #{w\ 3978}#)
               (if (memq 'top
                         (#{wrap-marks\ 3798}# #{w\ 3978}#))
                 #{x\ 3977}#
                 (letrec ((#{f\ 3979}#
                            (lambda (#{x\ 3980}#)
                              (if (#{syntax-object?\ 3779}# #{x\ 3980}#)
                                (#{strip\ 3844}#
                                  (#{syntax-object-expression\ 3780}#
                                    #{x\ 3980}#)
                                  (#{syntax-object-wrap\ 3781}# #{x\ 3980}#))
                                (if (pair? #{x\ 3980}#)
                                  (let ((#{a\ 3981}#
                                          (#{f\ 3979}# (car #{x\ 3980}#)))
                                        (#{d\ 3982}#
                                          (#{f\ 3979}# (cdr #{x\ 3980}#))))
                                    (if (if (eq? #{a\ 3981}# (car #{x\ 3980}#))
                                          (eq? #{d\ 3982}# (cdr #{x\ 3980}#))
                                          #f)
                                      #{x\ 3980}#
                                      (cons #{a\ 3981}# #{d\ 3982}#)))
                                  (if (vector? #{x\ 3980}#)
                                    (let ((#{old\ 3983}#
                                            (vector->list #{x\ 3980}#)))
                                      (let ((#{new\ 3984}#
                                              (map #{f\ 3979}# #{old\ 3983}#)))
                                        (if (#{and-map*\ 3695}#
                                              eq?
                                              #{old\ 3983}#
                                              #{new\ 3984}#)
                                          #{x\ 3980}#
                                          (list->vector #{new\ 3984}#))))
                                    #{x\ 3980}#))))))
                   (#{f\ 3979}# #{x\ 3977}#)))))
           (#{chi-lambda-case\ 3843}#
             (lambda (#{e\ 3985}#
                      #{r\ 3986}#
                      #{w\ 3987}#
                      #{s\ 3988}#
                      #{mod\ 3989}#
                      #{get-formals\ 3990}#
                      #{clauses\ 3991}#)
               (letrec ((#{expand-body\ 3995}#
                          (lambda (#{req\ 3996}#
                                   #{opt\ 3997}#
                                   #{rest\ 3998}#
                                   #{kw\ 3999}#
                                   #{body\ 4000}#
                                   #{vars\ 4001}#
                                   #{r*\ 4002}#
                                   #{w*\ 4003}#
                                   #{inits\ 4004}#)
                            ((lambda (#{tmp\ 4005}#)
                               ((lambda (#{tmp\ 4006}#)
                                  (if (if #{tmp\ 4006}#
                                        (apply (lambda (#{docstring\ 4007}#
                                                        #{e1\ 4008}#
                                                        #{e2\ 4009}#)
                                                 (string?
                                                   (syntax->datum
                                                     #{docstring\ 4007}#)))
                                               #{tmp\ 4006}#)
                                        #f)
                                    (apply (lambda (#{docstring\ 4010}#
                                                    #{e1\ 4011}#
                                                    #{e2\ 4012}#)
                                             (values
                                               (syntax->datum
                                                 #{docstring\ 4010}#)
                                               #{req\ 3996}#
                                               #{opt\ 3997}#
                                               #{rest\ 3998}#
                                               #{kw\ 3999}#
                                               #{inits\ 4004}#
                                               #{vars\ 4001}#
                                               (#{chi-body\ 3835}#
                                                 (cons #{e1\ 4011}#
                                                       #{e2\ 4012}#)
                                                 (#{source-wrap\ 3824}#
                                                   #{e\ 3985}#
                                                   #{w\ 3987}#
                                                   #{s\ 3988}#
                                                   #{mod\ 3989}#)
                                                 #{r*\ 4002}#
                                                 #{w*\ 4003}#
                                                 #{mod\ 3989}#)))
                                           #{tmp\ 4006}#)
                                    ((lambda (#{tmp\ 4014}#)
                                       (if #{tmp\ 4014}#
                                         (apply (lambda (#{e1\ 4015}#
                                                         #{e2\ 4016}#)
                                                  (values
                                                    #f
                                                    #{req\ 3996}#
                                                    #{opt\ 3997}#
                                                    #{rest\ 3998}#
                                                    #{kw\ 3999}#
                                                    #{inits\ 4004}#
                                                    #{vars\ 4001}#
                                                    (#{chi-body\ 3835}#
                                                      (cons #{e1\ 4015}#
                                                            #{e2\ 4016}#)
                                                      (#{source-wrap\ 3824}#
                                                        #{e\ 3985}#
                                                        #{w\ 3987}#
                                                        #{s\ 3988}#
                                                        #{mod\ 3989}#)
                                                      #{r*\ 4002}#
                                                      #{w*\ 4003}#
                                                      #{mod\ 3989}#)))
                                                #{tmp\ 4014}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 4005}#)))
                                     ($sc-dispatch
                                       #{tmp\ 4005}#
                                       '(any . each-any)))))
                                ($sc-dispatch
                                  #{tmp\ 4005}#
                                  '(any any . each-any))))
                             #{body\ 4000}#)))
                        (#{expand-kw\ 3994}#
                          (lambda (#{req\ 4018}#
                                   #{opt\ 4019}#
                                   #{rest\ 4020}#
                                   #{kw\ 4021}#
                                   #{body\ 4022}#
                                   #{vars\ 4023}#
                                   #{r*\ 4024}#
                                   #{w*\ 4025}#
                                   #{aok\ 4026}#
                                   #{out\ 4027}#
                                   #{inits\ 4028}#)
                            (if (pair? #{kw\ 4021}#)
                              ((lambda (#{tmp\ 4029}#)
                                 ((lambda (#{tmp\ 4030}#)
                                    (if #{tmp\ 4030}#
                                      (apply (lambda (#{k\ 4031}#
                                                      #{id\ 4032}#
                                                      #{i\ 4033}#)
                                               (let ((#{v\ 4034}#
                                                       (#{gen-var\ 3845}#
                                                         #{id\ 4032}#)))
                                                 (let ((#{l\ 4035}#
                                                         (#{gen-labels\ 3801}#
                                                           (list #{v\ 4034}#))))
                                                   (let ((#{r**\ 4036}#
                                                           (#{extend-var-env\ 3790}#
                                                             #{l\ 4035}#
                                                             (list #{v\ 4034}#)
                                                             #{r*\ 4024}#)))
                                                     (let ((#{w**\ 4037}#
                                                             (#{make-binding-wrap\ 3812}#
                                                               (list #{id\ 4032}#)
                                                               #{l\ 4035}#
                                                               #{w*\ 4025}#)))
                                                       (#{expand-kw\ 3994}#
                                                         #{req\ 4018}#
                                                         #{opt\ 4019}#
                                                         #{rest\ 4020}#
                                                         (cdr #{kw\ 4021}#)
                                                         #{body\ 4022}#
                                                         (cons #{v\ 4034}#
                                                               #{vars\ 4023}#)
                                                         #{r**\ 4036}#
                                                         #{w**\ 4037}#
                                                         #{aok\ 4026}#
                                                         (cons (list (syntax->datum
                                                                       #{k\ 4031}#)
                                                                     (syntax->datum
                                                                       #{id\ 4032}#)
                                                                     #{v\ 4034}#)
                                                               #{out\ 4027}#)
                                                         (cons (#{chi\ 3831}#
                                                                 #{i\ 4033}#
                                                                 #{r*\ 4024}#
                                                                 #{w*\ 4025}#
                                                                 #{mod\ 3989}#)
                                                               #{inits\ 4028}#)))))))
                                             #{tmp\ 4030}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 4029}#)))
                                  ($sc-dispatch
                                    #{tmp\ 4029}#
                                    '(any any any))))
                               (car #{kw\ 4021}#))
                              (#{expand-body\ 3995}#
                                #{req\ 4018}#
                                #{opt\ 4019}#
                                #{rest\ 4020}#
                                (if (let ((#{t\ 4038}# #{aok\ 4026}#))
                                      (if #{t\ 4038}#
                                        #{t\ 4038}#
                                        (pair? #{out\ 4027}#)))
                                  (cons #{aok\ 4026}# (reverse #{out\ 4027}#))
                                  #f)
                                #{body\ 4022}#
                                (reverse #{vars\ 4023}#)
                                #{r*\ 4024}#
                                #{w*\ 4025}#
                                (reverse #{inits\ 4028}#)))))
                        (#{expand-opt\ 3993}#
                          (lambda (#{req\ 4039}#
                                   #{opt\ 4040}#
                                   #{rest\ 4041}#
                                   #{kw\ 4042}#
                                   #{body\ 4043}#
                                   #{vars\ 4044}#
                                   #{r*\ 4045}#
                                   #{w*\ 4046}#
                                   #{out\ 4047}#
                                   #{inits\ 4048}#)
                            (if (pair? #{opt\ 4040}#)
                              ((lambda (#{tmp\ 4049}#)
                                 ((lambda (#{tmp\ 4050}#)
                                    (if #{tmp\ 4050}#
                                      (apply (lambda (#{id\ 4051}# #{i\ 4052}#)
                                               (let ((#{v\ 4053}#
                                                       (#{gen-var\ 3845}#
                                                         #{id\ 4051}#)))
                                                 (let ((#{l\ 4054}#
                                                         (#{gen-labels\ 3801}#
                                                           (list #{v\ 4053}#))))
                                                   (let ((#{r**\ 4055}#
                                                           (#{extend-var-env\ 3790}#
                                                             #{l\ 4054}#
                                                             (list #{v\ 4053}#)
                                                             #{r*\ 4045}#)))
                                                     (let ((#{w**\ 4056}#
                                                             (#{make-binding-wrap\ 3812}#
                                                               (list #{id\ 4051}#)
                                                               #{l\ 4054}#
                                                               #{w*\ 4046}#)))
                                                       (#{expand-opt\ 3993}#
                                                         #{req\ 4039}#
                                                         (cdr #{opt\ 4040}#)
                                                         #{rest\ 4041}#
                                                         #{kw\ 4042}#
                                                         #{body\ 4043}#
                                                         (cons #{v\ 4053}#
                                                               #{vars\ 4044}#)
                                                         #{r**\ 4055}#
                                                         #{w**\ 4056}#
                                                         (cons (syntax->datum
                                                                 #{id\ 4051}#)
                                                               #{out\ 4047}#)
                                                         (cons (#{chi\ 3831}#
                                                                 #{i\ 4052}#
                                                                 #{r*\ 4045}#
                                                                 #{w*\ 4046}#
                                                                 #{mod\ 3989}#)
                                                               #{inits\ 4048}#)))))))
                                             #{tmp\ 4050}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 4049}#)))
                                  ($sc-dispatch
                                    #{tmp\ 4049}#
                                    '(any any))))
                               (car #{opt\ 4040}#))
                              (if #{rest\ 4041}#
                                (let ((#{v\ 4057}#
                                        (#{gen-var\ 3845}# #{rest\ 4041}#)))
                                  (let ((#{l\ 4058}#
                                          (#{gen-labels\ 3801}#
                                            (list #{v\ 4057}#))))
                                    (let ((#{r*\ 4059}#
                                            (#{extend-var-env\ 3790}#
                                              #{l\ 4058}#
                                              (list #{v\ 4057}#)
                                              #{r*\ 4045}#)))
                                      (let ((#{w*\ 4060}#
                                              (#{make-binding-wrap\ 3812}#
                                                (list #{rest\ 4041}#)
                                                #{l\ 4058}#
                                                #{w*\ 4046}#)))
                                        (#{expand-kw\ 3994}#
                                          #{req\ 4039}#
                                          (if (pair? #{out\ 4047}#)
                                            (reverse #{out\ 4047}#)
                                            #f)
                                          (syntax->datum #{rest\ 4041}#)
                                          (if (pair? #{kw\ 4042}#)
                                            (cdr #{kw\ 4042}#)
                                            #{kw\ 4042}#)
                                          #{body\ 4043}#
                                          (cons #{v\ 4057}# #{vars\ 4044}#)
                                          #{r*\ 4059}#
                                          #{w*\ 4060}#
                                          (if (pair? #{kw\ 4042}#)
                                            (car #{kw\ 4042}#)
                                            #f)
                                          '()
                                          #{inits\ 4048}#)))))
                                (#{expand-kw\ 3994}#
                                  #{req\ 4039}#
                                  (if (pair? #{out\ 4047}#)
                                    (reverse #{out\ 4047}#)
                                    #f)
                                  #f
                                  (if (pair? #{kw\ 4042}#)
                                    (cdr #{kw\ 4042}#)
                                    #{kw\ 4042}#)
                                  #{body\ 4043}#
                                  #{vars\ 4044}#
                                  #{r*\ 4045}#
                                  #{w*\ 4046}#
                                  (if (pair? #{kw\ 4042}#)
                                    (car #{kw\ 4042}#)
                                    #f)
                                  '()
                                  #{inits\ 4048}#)))))
                        (#{expand-req\ 3992}#
                          (lambda (#{req\ 4061}#
                                   #{opt\ 4062}#
                                   #{rest\ 4063}#
                                   #{kw\ 4064}#
                                   #{body\ 4065}#)
                            (let ((#{vars\ 4066}#
                                    (map #{gen-var\ 3845}# #{req\ 4061}#))
                                  (#{labels\ 4067}#
                                    (#{gen-labels\ 3801}# #{req\ 4061}#)))
                              (let ((#{r*\ 4068}#
                                      (#{extend-var-env\ 3790}#
                                        #{labels\ 4067}#
                                        #{vars\ 4066}#
                                        #{r\ 3986}#))
                                    (#{w*\ 4069}#
                                      (#{make-binding-wrap\ 3812}#
                                        #{req\ 4061}#
                                        #{labels\ 4067}#
                                        #{w\ 3987}#)))
                                (#{expand-opt\ 3993}#
                                  (map syntax->datum #{req\ 4061}#)
                                  #{opt\ 4062}#
                                  #{rest\ 4063}#
                                  #{kw\ 4064}#
                                  #{body\ 4065}#
                                  (reverse #{vars\ 4066}#)
                                  #{r*\ 4068}#
                                  #{w*\ 4069}#
                                  '()
                                  '()))))))
                 ((lambda (#{tmp\ 4070}#)
                    ((lambda (#{tmp\ 4071}#)
                       (if #{tmp\ 4071}#
                         (apply (lambda () (values #f #f)) #{tmp\ 4071}#)
                         ((lambda (#{tmp\ 4072}#)
                            (if #{tmp\ 4072}#
                              (apply (lambda (#{args\ 4073}#
                                              #{e1\ 4074}#
                                              #{e2\ 4075}#
                                              #{args*\ 4076}#
                                              #{e1*\ 4077}#
                                              #{e2*\ 4078}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{get-formals\ 3990}#
                                             #{args\ 4073}#))
                                         (lambda (#{req\ 4079}#
                                                  #{opt\ 4080}#
                                                  #{rest\ 4081}#
                                                  #{kw\ 4082}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{expand-req\ 3992}#
                                                 #{req\ 4079}#
                                                 #{opt\ 4080}#
                                                 #{rest\ 4081}#
                                                 #{kw\ 4082}#
                                                 (cons #{e1\ 4074}#
                                                       #{e2\ 4075}#)))
                                             (lambda (#{docstring\ 4084}#
                                                      #{req\ 4085}#
                                                      #{opt\ 4086}#
                                                      #{rest\ 4087}#
                                                      #{kw\ 4088}#
                                                      #{inits\ 4089}#
                                                      #{vars\ 4090}#
                                                      #{body\ 4091}#)
                                               (call-with-values
                                                 (lambda ()
                                                   (#{chi-lambda-case\ 3843}#
                                                     #{e\ 3985}#
                                                     #{r\ 3986}#
                                                     #{w\ 3987}#
                                                     #{s\ 3988}#
                                                     #{mod\ 3989}#
                                                     #{get-formals\ 3990}#
                                                     (map (lambda (#{tmp\ 4094}#
                                                                   #{tmp\ 4093}#
                                                                   #{tmp\ 4092}#)
                                                            (cons #{tmp\ 4092}#
                                                                  (cons #{tmp\ 4093}#
                                                                        #{tmp\ 4094}#)))
                                                          #{e2*\ 4078}#
                                                          #{e1*\ 4077}#
                                                          #{args*\ 4076}#)))
                                                 (lambda (#{docstring*\ 4096}#
                                                          #{else*\ 4097}#)
                                                   (values
                                                     (let ((#{t\ 4098}#
                                                             #{docstring\ 4084}#))
                                                       (if #{t\ 4098}#
                                                         #{t\ 4098}#
                                                         #{docstring*\ 4096}#))
                                                     (#{build-lambda-case\ 3771}#
                                                       #{s\ 3988}#
                                                       #{req\ 4085}#
                                                       #{opt\ 4086}#
                                                       #{rest\ 4087}#
                                                       #{kw\ 4088}#
                                                       #{inits\ 4089}#
                                                       #{vars\ 4090}#
                                                       #{body\ 4091}#
                                                       #{else*\ 4097}#)))))))))
                                     #{tmp\ 4072}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 4070}#)))
                          ($sc-dispatch
                            #{tmp\ 4070}#
                            '((any any . each-any)
                              .
                              #(each (any any . each-any)))))))
                     ($sc-dispatch #{tmp\ 4070}# (quote ()))))
                  #{clauses\ 3991}#))))
           (#{lambda*-formals\ 3842}#
             (lambda (#{orig-args\ 4099}#)
               (letrec ((#{check\ 4104}#
                          (lambda (#{req\ 4105}#
                                   #{opt\ 4106}#
                                   #{rest\ 4107}#
                                   #{kw\ 4108}#)
                            (if (#{distinct-bound-ids?\ 3821}#
                                  (append
                                    #{req\ 4105}#
                                    (map car #{opt\ 4106}#)
                                    (if #{rest\ 4107}#
                                      (list #{rest\ 4107}#)
                                      '())
                                    (if (pair? #{kw\ 4108}#)
                                      (map cadr (cdr #{kw\ 4108}#))
                                      '())))
                              (values
                                #{req\ 4105}#
                                #{opt\ 4106}#
                                #{rest\ 4107}#
                                #{kw\ 4108}#)
                              (syntax-violation
                                'lambda*
                                "duplicate identifier in argument list"
                                #{orig-args\ 4099}#))))
                        (#{rest\ 4103}#
                          (lambda (#{args\ 4109}#
                                   #{req\ 4110}#
                                   #{opt\ 4111}#
                                   #{kw\ 4112}#)
                            ((lambda (#{tmp\ 4113}#)
                               ((lambda (#{tmp\ 4114}#)
                                  (if (if #{tmp\ 4114}#
                                        (apply (lambda (#{r\ 4115}#)
                                                 (#{id?\ 3795}# #{r\ 4115}#))
                                               #{tmp\ 4114}#)
                                        #f)
                                    (apply (lambda (#{r\ 4116}#)
                                             (#{check\ 4104}#
                                               #{req\ 4110}#
                                               #{opt\ 4111}#
                                               #{r\ 4116}#
                                               #{kw\ 4112}#))
                                           #{tmp\ 4114}#)
                                    ((lambda (#{else\ 4117}#)
                                       (syntax-violation
                                         'lambda*
                                         "invalid rest argument"
                                         #{orig-args\ 4099}#
                                         #{args\ 4109}#))
                                     #{tmp\ 4113}#)))
                                (list #{tmp\ 4113}#)))
                             #{args\ 4109}#)))
                        (#{key\ 4102}#
                          (lambda (#{args\ 4118}#
                                   #{req\ 4119}#
                                   #{opt\ 4120}#
                                   #{rkey\ 4121}#)
                            ((lambda (#{tmp\ 4122}#)
                               ((lambda (#{tmp\ 4123}#)
                                  (if #{tmp\ 4123}#
                                    (apply (lambda ()
                                             (#{check\ 4104}#
                                               #{req\ 4119}#
                                               #{opt\ 4120}#
                                               #f
                                               (cons #f
                                                     (reverse
                                                       #{rkey\ 4121}#))))
                                           #{tmp\ 4123}#)
                                    ((lambda (#{tmp\ 4124}#)
                                       (if (if #{tmp\ 4124}#
                                             (apply (lambda (#{a\ 4125}#
                                                             #{b\ 4126}#)
                                                      (#{id?\ 3795}#
                                                        #{a\ 4125}#))
                                                    #{tmp\ 4124}#)
                                             #f)
                                         (apply (lambda (#{a\ 4127}#
                                                         #{b\ 4128}#)
                                                  ((lambda (#{tmp\ 4129}#)
                                                     ((lambda (#{k\ 4130}#)
                                                        (#{key\ 4102}#
                                                          #{b\ 4128}#
                                                          #{req\ 4119}#
                                                          #{opt\ 4120}#
                                                          (cons (cons #{k\ 4130}#
                                                                      (cons #{a\ 4127}#
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
                                                                #{rkey\ 4121}#)))
                                                      #{tmp\ 4129}#))
                                                   (symbol->keyword
                                                     (syntax->datum
                                                       #{a\ 4127}#))))
                                                #{tmp\ 4124}#)
                                         ((lambda (#{tmp\ 4131}#)
                                            (if (if #{tmp\ 4131}#
                                                  (apply (lambda (#{a\ 4132}#
                                                                  #{init\ 4133}#
                                                                  #{b\ 4134}#)
                                                           (#{id?\ 3795}#
                                                             #{a\ 4132}#))
                                                         #{tmp\ 4131}#)
                                                  #f)
                                              (apply (lambda (#{a\ 4135}#
                                                              #{init\ 4136}#
                                                              #{b\ 4137}#)
                                                       ((lambda (#{tmp\ 4138}#)
                                                          ((lambda (#{k\ 4139}#)
                                                             (#{key\ 4102}#
                                                               #{b\ 4137}#
                                                               #{req\ 4119}#
                                                               #{opt\ 4120}#
                                                               (cons (list #{k\ 4139}#
                                                                           #{a\ 4135}#
                                                                           #{init\ 4136}#)
                                                                     #{rkey\ 4121}#)))
                                                           #{tmp\ 4138}#))
                                                        (symbol->keyword
                                                          (syntax->datum
                                                            #{a\ 4135}#))))
                                                     #{tmp\ 4131}#)
                                              ((lambda (#{tmp\ 4140}#)
                                                 (if (if #{tmp\ 4140}#
                                                       (apply (lambda (#{a\ 4141}#
                                                                       #{init\ 4142}#
                                                                       #{k\ 4143}#
                                                                       #{b\ 4144}#)
                                                                (if (#{id?\ 3795}#
                                                                      #{a\ 4141}#)
                                                                  (keyword?
                                                                    (syntax->datum
                                                                      #{k\ 4143}#))
                                                                  #f))
                                                              #{tmp\ 4140}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 4145}#
                                                                   #{init\ 4146}#
                                                                   #{k\ 4147}#
                                                                   #{b\ 4148}#)
                                                            (#{key\ 4102}#
                                                              #{b\ 4148}#
                                                              #{req\ 4119}#
                                                              #{opt\ 4120}#
                                                              (cons (list #{k\ 4147}#
                                                                          #{a\ 4145}#
                                                                          #{init\ 4146}#)
                                                                    #{rkey\ 4121}#)))
                                                          #{tmp\ 4140}#)
                                                   ((lambda (#{tmp\ 4149}#)
                                                      (if (if #{tmp\ 4149}#
                                                            (apply (lambda (#{aok\ 4150}#)
                                                                     (eq? (syntax->datum
                                                                            #{aok\ 4150}#)
                                                                          #:allow-other-keys))
                                                                   #{tmp\ 4149}#)
                                                            #f)
                                                        (apply (lambda (#{aok\ 4151}#)
                                                                 (#{check\ 4104}#
                                                                   #{req\ 4119}#
                                                                   #{opt\ 4120}#
                                                                   #f
                                                                   (cons #t
                                                                         (reverse
                                                                           #{rkey\ 4121}#))))
                                                               #{tmp\ 4149}#)
                                                        ((lambda (#{tmp\ 4152}#)
                                                           (if (if #{tmp\ 4152}#
                                                                 (apply (lambda (#{aok\ 4153}#
                                                                                 #{a\ 4154}#
                                                                                 #{b\ 4155}#)
                                                                          (if (eq? (syntax->datum
                                                                                     #{aok\ 4153}#)
                                                                                   #:allow-other-keys)
                                                                            (eq? (syntax->datum
                                                                                   #{a\ 4154}#)
                                                                                 #:rest)
                                                                            #f))
                                                                        #{tmp\ 4152}#)
                                                                 #f)
                                                             (apply (lambda (#{aok\ 4156}#
                                                                             #{a\ 4157}#
                                                                             #{b\ 4158}#)
                                                                      (#{rest\ 4103}#
                                                                        #{b\ 4158}#
                                                                        #{req\ 4119}#
                                                                        #{opt\ 4120}#
                                                                        (cons #t
                                                                              (reverse
                                                                                #{rkey\ 4121}#))))
                                                                    #{tmp\ 4152}#)
                                                             ((lambda (#{tmp\ 4159}#)
                                                                (if (if #{tmp\ 4159}#
                                                                      (apply (lambda (#{aok\ 4160}#
                                                                                      #{r\ 4161}#)
                                                                               (if (eq? (syntax->datum
                                                                                          #{aok\ 4160}#)
                                                                                        #:allow-other-keys)
                                                                                 (#{id?\ 3795}#
                                                                                   #{r\ 4161}#)
                                                                                 #f))
                                                                             #{tmp\ 4159}#)
                                                                      #f)
                                                                  (apply (lambda (#{aok\ 4162}#
                                                                                  #{r\ 4163}#)
                                                                           (#{rest\ 4103}#
                                                                             #{r\ 4163}#
                                                                             #{req\ 4119}#
                                                                             #{opt\ 4120}#
                                                                             (cons #t
                                                                                   (reverse
                                                                                     #{rkey\ 4121}#))))
                                                                         #{tmp\ 4159}#)
                                                                  ((lambda (#{tmp\ 4164}#)
                                                                     (if (if #{tmp\ 4164}#
                                                                           (apply (lambda (#{a\ 4165}#
                                                                                           #{b\ 4166}#)
                                                                                    (eq? (syntax->datum
                                                                                           #{a\ 4165}#)
                                                                                         #:rest))
                                                                                  #{tmp\ 4164}#)
                                                                           #f)
                                                                       (apply (lambda (#{a\ 4167}#
                                                                                       #{b\ 4168}#)
                                                                                (#{rest\ 4103}#
                                                                                  #{b\ 4168}#
                                                                                  #{req\ 4119}#
                                                                                  #{opt\ 4120}#
                                                                                  (cons #f
                                                                                        (reverse
                                                                                          #{rkey\ 4121}#))))
                                                                              #{tmp\ 4164}#)
                                                                       ((lambda (#{tmp\ 4169}#)
                                                                          (if (if #{tmp\ 4169}#
                                                                                (apply (lambda (#{r\ 4170}#)
                                                                                         (#{id?\ 3795}#
                                                                                           #{r\ 4170}#))
                                                                                       #{tmp\ 4169}#)
                                                                                #f)
                                                                            (apply (lambda (#{r\ 4171}#)
                                                                                     (#{rest\ 4103}#
                                                                                       #{r\ 4171}#
                                                                                       #{req\ 4119}#
                                                                                       #{opt\ 4120}#
                                                                                       (cons #f
                                                                                             (reverse
                                                                                               #{rkey\ 4121}#))))
                                                                                   #{tmp\ 4169}#)
                                                                            ((lambda (#{else\ 4172}#)
                                                                               (syntax-violation
                                                                                 'lambda*
                                                                                 "invalid keyword argument list"
                                                                                 #{orig-args\ 4099}#
                                                                                 #{args\ 4118}#))
                                                                             #{tmp\ 4122}#)))
                                                                        (list #{tmp\ 4122}#))))
                                                                   ($sc-dispatch
                                                                     #{tmp\ 4122}#
                                                                     '(any any)))))
                                                              ($sc-dispatch
                                                                #{tmp\ 4122}#
                                                                '(any .
                                                                      any)))))
                                                         ($sc-dispatch
                                                           #{tmp\ 4122}#
                                                           '(any any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 4122}#
                                                      '(any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 4122}#
                                                 '((any any any) . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 4122}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 4122}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 4122}# (quote ()))))
                             #{args\ 4118}#)))
                        (#{opt\ 4101}#
                          (lambda (#{args\ 4173}# #{req\ 4174}# #{ropt\ 4175}#)
                            ((lambda (#{tmp\ 4176}#)
                               ((lambda (#{tmp\ 4177}#)
                                  (if #{tmp\ 4177}#
                                    (apply (lambda ()
                                             (#{check\ 4104}#
                                               #{req\ 4174}#
                                               (reverse #{ropt\ 4175}#)
                                               #f
                                               '()))
                                           #{tmp\ 4177}#)
                                    ((lambda (#{tmp\ 4178}#)
                                       (if (if #{tmp\ 4178}#
                                             (apply (lambda (#{a\ 4179}#
                                                             #{b\ 4180}#)
                                                      (#{id?\ 3795}#
                                                        #{a\ 4179}#))
                                                    #{tmp\ 4178}#)
                                             #f)
                                         (apply (lambda (#{a\ 4181}#
                                                         #{b\ 4182}#)
                                                  (#{opt\ 4101}#
                                                    #{b\ 4182}#
                                                    #{req\ 4174}#
                                                    (cons (cons #{a\ 4181}#
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
                                                          #{ropt\ 4175}#)))
                                                #{tmp\ 4178}#)
                                         ((lambda (#{tmp\ 4183}#)
                                            (if (if #{tmp\ 4183}#
                                                  (apply (lambda (#{a\ 4184}#
                                                                  #{init\ 4185}#
                                                                  #{b\ 4186}#)
                                                           (#{id?\ 3795}#
                                                             #{a\ 4184}#))
                                                         #{tmp\ 4183}#)
                                                  #f)
                                              (apply (lambda (#{a\ 4187}#
                                                              #{init\ 4188}#
                                                              #{b\ 4189}#)
                                                       (#{opt\ 4101}#
                                                         #{b\ 4189}#
                                                         #{req\ 4174}#
                                                         (cons (list #{a\ 4187}#
                                                                     #{init\ 4188}#)
                                                               #{ropt\ 4175}#)))
                                                     #{tmp\ 4183}#)
                                              ((lambda (#{tmp\ 4190}#)
                                                 (if (if #{tmp\ 4190}#
                                                       (apply (lambda (#{a\ 4191}#
                                                                       #{b\ 4192}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 4191}#)
                                                                     #:key))
                                                              #{tmp\ 4190}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 4193}#
                                                                   #{b\ 4194}#)
                                                            (#{key\ 4102}#
                                                              #{b\ 4194}#
                                                              #{req\ 4174}#
                                                              (reverse
                                                                #{ropt\ 4175}#)
                                                              '()))
                                                          #{tmp\ 4190}#)
                                                   ((lambda (#{tmp\ 4195}#)
                                                      (if (if #{tmp\ 4195}#
                                                            (apply (lambda (#{a\ 4196}#
                                                                            #{b\ 4197}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 4196}#)
                                                                          #:rest))
                                                                   #{tmp\ 4195}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 4198}#
                                                                        #{b\ 4199}#)
                                                                 (#{rest\ 4103}#
                                                                   #{b\ 4199}#
                                                                   #{req\ 4174}#
                                                                   (reverse
                                                                     #{ropt\ 4175}#)
                                                                   '()))
                                                               #{tmp\ 4195}#)
                                                        ((lambda (#{tmp\ 4200}#)
                                                           (if (if #{tmp\ 4200}#
                                                                 (apply (lambda (#{r\ 4201}#)
                                                                          (#{id?\ 3795}#
                                                                            #{r\ 4201}#))
                                                                        #{tmp\ 4200}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 4202}#)
                                                                      (#{rest\ 4103}#
                                                                        #{r\ 4202}#
                                                                        #{req\ 4174}#
                                                                        (reverse
                                                                          #{ropt\ 4175}#)
                                                                        '()))
                                                                    #{tmp\ 4200}#)
                                                             ((lambda (#{else\ 4203}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid optional argument list"
                                                                  #{orig-args\ 4099}#
                                                                  #{args\ 4173}#))
                                                              #{tmp\ 4176}#)))
                                                         (list #{tmp\ 4176}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 4176}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 4176}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 4176}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 4176}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 4176}# (quote ()))))
                             #{args\ 4173}#)))
                        (#{req\ 4100}#
                          (lambda (#{args\ 4204}# #{rreq\ 4205}#)
                            ((lambda (#{tmp\ 4206}#)
                               ((lambda (#{tmp\ 4207}#)
                                  (if #{tmp\ 4207}#
                                    (apply (lambda ()
                                             (#{check\ 4104}#
                                               (reverse #{rreq\ 4205}#)
                                               '()
                                               #f
                                               '()))
                                           #{tmp\ 4207}#)
                                    ((lambda (#{tmp\ 4208}#)
                                       (if (if #{tmp\ 4208}#
                                             (apply (lambda (#{a\ 4209}#
                                                             #{b\ 4210}#)
                                                      (#{id?\ 3795}#
                                                        #{a\ 4209}#))
                                                    #{tmp\ 4208}#)
                                             #f)
                                         (apply (lambda (#{a\ 4211}#
                                                         #{b\ 4212}#)
                                                  (#{req\ 4100}#
                                                    #{b\ 4212}#
                                                    (cons #{a\ 4211}#
                                                          #{rreq\ 4205}#)))
                                                #{tmp\ 4208}#)
                                         ((lambda (#{tmp\ 4213}#)
                                            (if (if #{tmp\ 4213}#
                                                  (apply (lambda (#{a\ 4214}#
                                                                  #{b\ 4215}#)
                                                           (eq? (syntax->datum
                                                                  #{a\ 4214}#)
                                                                #:optional))
                                                         #{tmp\ 4213}#)
                                                  #f)
                                              (apply (lambda (#{a\ 4216}#
                                                              #{b\ 4217}#)
                                                       (#{opt\ 4101}#
                                                         #{b\ 4217}#
                                                         (reverse
                                                           #{rreq\ 4205}#)
                                                         '()))
                                                     #{tmp\ 4213}#)
                                              ((lambda (#{tmp\ 4218}#)
                                                 (if (if #{tmp\ 4218}#
                                                       (apply (lambda (#{a\ 4219}#
                                                                       #{b\ 4220}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 4219}#)
                                                                     #:key))
                                                              #{tmp\ 4218}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 4221}#
                                                                   #{b\ 4222}#)
                                                            (#{key\ 4102}#
                                                              #{b\ 4222}#
                                                              (reverse
                                                                #{rreq\ 4205}#)
                                                              '()
                                                              '()))
                                                          #{tmp\ 4218}#)
                                                   ((lambda (#{tmp\ 4223}#)
                                                      (if (if #{tmp\ 4223}#
                                                            (apply (lambda (#{a\ 4224}#
                                                                            #{b\ 4225}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 4224}#)
                                                                          #:rest))
                                                                   #{tmp\ 4223}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 4226}#
                                                                        #{b\ 4227}#)
                                                                 (#{rest\ 4103}#
                                                                   #{b\ 4227}#
                                                                   (reverse
                                                                     #{rreq\ 4205}#)
                                                                   '()
                                                                   '()))
                                                               #{tmp\ 4223}#)
                                                        ((lambda (#{tmp\ 4228}#)
                                                           (if (if #{tmp\ 4228}#
                                                                 (apply (lambda (#{r\ 4229}#)
                                                                          (#{id?\ 3795}#
                                                                            #{r\ 4229}#))
                                                                        #{tmp\ 4228}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 4230}#)
                                                                      (#{rest\ 4103}#
                                                                        #{r\ 4230}#
                                                                        (reverse
                                                                          #{rreq\ 4205}#)
                                                                        '()
                                                                        '()))
                                                                    #{tmp\ 4228}#)
                                                             ((lambda (#{else\ 4231}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid argument list"
                                                                  #{orig-args\ 4099}#
                                                                  #{args\ 4204}#))
                                                              #{tmp\ 4206}#)))
                                                         (list #{tmp\ 4206}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 4206}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 4206}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 4206}#
                                            '(any . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 4206}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 4206}# (quote ()))))
                             #{args\ 4204}#))))
                 (#{req\ 4100}# #{orig-args\ 4099}# (quote ())))))
           (#{chi-simple-lambda\ 3841}#
             (lambda (#{e\ 4232}#
                      #{r\ 4233}#
                      #{w\ 4234}#
                      #{s\ 4235}#
                      #{mod\ 4236}#
                      #{req\ 4237}#
                      #{rest\ 4238}#
                      #{docstring\ 4239}#
                      #{body\ 4240}#)
               (let ((#{ids\ 4241}#
                       (if #{rest\ 4238}#
                         (append #{req\ 4237}# (list #{rest\ 4238}#))
                         #{req\ 4237}#)))
                 (let ((#{vars\ 4242}#
                         (map #{gen-var\ 3845}# #{ids\ 4241}#)))
                   (let ((#{labels\ 4243}#
                           (#{gen-labels\ 3801}# #{ids\ 4241}#)))
                     (#{build-simple-lambda\ 3769}#
                       #{s\ 4235}#
                       (map syntax->datum #{req\ 4237}#)
                       (if #{rest\ 4238}#
                         (syntax->datum #{rest\ 4238}#)
                         #f)
                       #{vars\ 4242}#
                       #{docstring\ 4239}#
                       (#{chi-body\ 3835}#
                         #{body\ 4240}#
                         (#{source-wrap\ 3824}#
                           #{e\ 4232}#
                           #{w\ 4234}#
                           #{s\ 4235}#
                           #{mod\ 4236}#)
                         (#{extend-var-env\ 3790}#
                           #{labels\ 4243}#
                           #{vars\ 4242}#
                           #{r\ 4233}#)
                         (#{make-binding-wrap\ 3812}#
                           #{ids\ 4241}#
                           #{labels\ 4243}#
                           #{w\ 4234}#)
                         #{mod\ 4236}#)))))))
           (#{lambda-formals\ 3840}#
             (lambda (#{orig-args\ 4244}#)
               (letrec ((#{check\ 4246}#
                          (lambda (#{req\ 4247}# #{rest\ 4248}#)
                            (if (#{distinct-bound-ids?\ 3821}#
                                  (if #{rest\ 4248}#
                                    (cons #{rest\ 4248}# #{req\ 4247}#)
                                    #{req\ 4247}#))
                              (values #{req\ 4247}# #f #{rest\ 4248}# #f)
                              (syntax-violation
                                'lambda
                                "duplicate identifier in argument list"
                                #{orig-args\ 4244}#))))
                        (#{req\ 4245}#
                          (lambda (#{args\ 4249}# #{rreq\ 4250}#)
                            ((lambda (#{tmp\ 4251}#)
                               ((lambda (#{tmp\ 4252}#)
                                  (if #{tmp\ 4252}#
                                    (apply (lambda ()
                                             (#{check\ 4246}#
                                               (reverse #{rreq\ 4250}#)
                                               #f))
                                           #{tmp\ 4252}#)
                                    ((lambda (#{tmp\ 4253}#)
                                       (if (if #{tmp\ 4253}#
                                             (apply (lambda (#{a\ 4254}#
                                                             #{b\ 4255}#)
                                                      (#{id?\ 3795}#
                                                        #{a\ 4254}#))
                                                    #{tmp\ 4253}#)
                                             #f)
                                         (apply (lambda (#{a\ 4256}#
                                                         #{b\ 4257}#)
                                                  (#{req\ 4245}#
                                                    #{b\ 4257}#
                                                    (cons #{a\ 4256}#
                                                          #{rreq\ 4250}#)))
                                                #{tmp\ 4253}#)
                                         ((lambda (#{tmp\ 4258}#)
                                            (if (if #{tmp\ 4258}#
                                                  (apply (lambda (#{r\ 4259}#)
                                                           (#{id?\ 3795}#
                                                             #{r\ 4259}#))
                                                         #{tmp\ 4258}#)
                                                  #f)
                                              (apply (lambda (#{r\ 4260}#)
                                                       (#{check\ 4246}#
                                                         (reverse
                                                           #{rreq\ 4250}#)
                                                         #{r\ 4260}#))
                                                     #{tmp\ 4258}#)
                                              ((lambda (#{else\ 4261}#)
                                                 (syntax-violation
                                                   'lambda
                                                   "invalid argument list"
                                                   #{orig-args\ 4244}#
                                                   #{args\ 4249}#))
                                               #{tmp\ 4251}#)))
                                          (list #{tmp\ 4251}#))))
                                     ($sc-dispatch
                                       #{tmp\ 4251}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 4251}# (quote ()))))
                             #{args\ 4249}#))))
                 (#{req\ 4245}# #{orig-args\ 4244}# (quote ())))))
           (#{ellipsis?\ 3839}#
             (lambda (#{x\ 4262}#)
               (if (#{nonsymbol-id?\ 3794}# #{x\ 4262}#)
                 (#{free-id=?\ 3818}#
                   #{x\ 4262}#
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
           (#{chi-void\ 3838}#
             (lambda () (#{build-void\ 3759}# #f)))
           (#{eval-local-transformer\ 3837}#
             (lambda (#{expanded\ 4263}# #{mod\ 4264}#)
               (let ((#{p\ 4265}#
                       (#{local-eval-hook\ 3755}#
                         #{expanded\ 4263}#
                         #{mod\ 4264}#)))
                 (if (procedure? #{p\ 4265}#)
                   (cons #{p\ 4265}# (module-name (current-module)))
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 4265}#)))))
           (#{chi-local-syntax\ 3836}#
             (lambda (#{rec?\ 4266}#
                      #{e\ 4267}#
                      #{r\ 4268}#
                      #{w\ 4269}#
                      #{s\ 4270}#
                      #{mod\ 4271}#
                      #{k\ 4272}#)
               ((lambda (#{tmp\ 4273}#)
                  ((lambda (#{tmp\ 4274}#)
                     (if #{tmp\ 4274}#
                       (apply (lambda (#{_\ 4275}#
                                       #{id\ 4276}#
                                       #{val\ 4277}#
                                       #{e1\ 4278}#
                                       #{e2\ 4279}#)
                                (let ((#{ids\ 4280}# #{id\ 4276}#))
                                  (if (not (#{valid-bound-ids?\ 3820}#
                                             #{ids\ 4280}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 4267}#)
                                    (let ((#{labels\ 4282}#
                                            (#{gen-labels\ 3801}#
                                              #{ids\ 4280}#)))
                                      (let ((#{new-w\ 4283}#
                                              (#{make-binding-wrap\ 3812}#
                                                #{ids\ 4280}#
                                                #{labels\ 4282}#
                                                #{w\ 4269}#)))
                                        (#{k\ 4272}#
                                          (cons #{e1\ 4278}# #{e2\ 4279}#)
                                          (#{extend-env\ 3789}#
                                            #{labels\ 4282}#
                                            (let ((#{w\ 4285}#
                                                    (if #{rec?\ 4266}#
                                                      #{new-w\ 4283}#
                                                      #{w\ 4269}#))
                                                  (#{trans-r\ 4286}#
                                                    (#{macros-only-env\ 3791}#
                                                      #{r\ 4268}#)))
                                              (map (lambda (#{x\ 4287}#)
                                                     (cons 'macro
                                                           (#{eval-local-transformer\ 3837}#
                                                             (#{chi\ 3831}#
                                                               #{x\ 4287}#
                                                               #{trans-r\ 4286}#
                                                               #{w\ 4285}#
                                                               #{mod\ 4271}#)
                                                             #{mod\ 4271}#)))
                                                   #{val\ 4277}#))
                                            #{r\ 4268}#)
                                          #{new-w\ 4283}#
                                          #{s\ 4270}#
                                          #{mod\ 4271}#))))))
                              #{tmp\ 4274}#)
                       ((lambda (#{_\ 4289}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 3824}#
                              #{e\ 4267}#
                              #{w\ 4269}#
                              #{s\ 4270}#
                              #{mod\ 4271}#)))
                        #{tmp\ 4273}#)))
                   ($sc-dispatch
                     #{tmp\ 4273}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 4267}#)))
           (#{chi-body\ 3835}#
             (lambda (#{body\ 4290}#
                      #{outer-form\ 4291}#
                      #{r\ 4292}#
                      #{w\ 4293}#
                      #{mod\ 4294}#)
               (let ((#{r\ 4295}#
                       (cons '("placeholder" placeholder)
                             #{r\ 4292}#)))
                 (let ((#{ribcage\ 4296}#
                         (#{make-ribcage\ 3802}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 4297}#
                           (#{make-wrap\ 3797}#
                             (#{wrap-marks\ 3798}# #{w\ 4293}#)
                             (cons #{ribcage\ 4296}#
                                   (#{wrap-subst\ 3799}# #{w\ 4293}#)))))
                     (letrec ((#{parse\ 4298}#
                                (lambda (#{body\ 4299}#
                                         #{ids\ 4300}#
                                         #{labels\ 4301}#
                                         #{var-ids\ 4302}#
                                         #{vars\ 4303}#
                                         #{vals\ 4304}#
                                         #{bindings\ 4305}#)
                                  (if (null? #{body\ 4299}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 4291}#)
                                    (let ((#{e\ 4307}# (cdar #{body\ 4299}#))
                                          (#{er\ 4308}# (caar #{body\ 4299}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 3829}#
                                            #{e\ 4307}#
                                            #{er\ 4308}#
                                            '(())
                                            (#{source-annotation\ 3786}#
                                              #{er\ 4308}#)
                                            #{ribcage\ 4296}#
                                            #{mod\ 4294}#
                                            #f))
                                        (lambda (#{type\ 4309}#
                                                 #{value\ 4310}#
                                                 #{e\ 4311}#
                                                 #{w\ 4312}#
                                                 #{s\ 4313}#
                                                 #{mod\ 4314}#)
                                          (if (memv #{type\ 4309}#
                                                    '(define-form))
                                            (let ((#{id\ 4315}#
                                                    (#{wrap\ 3823}#
                                                      #{value\ 4310}#
                                                      #{w\ 4312}#
                                                      #{mod\ 4314}#))
                                                  (#{label\ 4316}#
                                                    (#{gen-label\ 3800}#)))
                                              (let ((#{var\ 4317}#
                                                      (#{gen-var\ 3845}#
                                                        #{id\ 4315}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 3811}#
                                                    #{ribcage\ 4296}#
                                                    #{id\ 4315}#
                                                    #{label\ 4316}#)
                                                  (#{parse\ 4298}#
                                                    (cdr #{body\ 4299}#)
                                                    (cons #{id\ 4315}#
                                                          #{ids\ 4300}#)
                                                    (cons #{label\ 4316}#
                                                          #{labels\ 4301}#)
                                                    (cons #{id\ 4315}#
                                                          #{var-ids\ 4302}#)
                                                    (cons #{var\ 4317}#
                                                          #{vars\ 4303}#)
                                                    (cons (cons #{er\ 4308}#
                                                                (#{wrap\ 3823}#
                                                                  #{e\ 4311}#
                                                                  #{w\ 4312}#
                                                                  #{mod\ 4314}#))
                                                          #{vals\ 4304}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 4317}#)
                                                          #{bindings\ 4305}#)))))
                                            (if (memv #{type\ 4309}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 4318}#
                                                      (#{wrap\ 3823}#
                                                        #{value\ 4310}#
                                                        #{w\ 4312}#
                                                        #{mod\ 4314}#))
                                                    (#{label\ 4319}#
                                                      (#{gen-label\ 3800}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 3811}#
                                                    #{ribcage\ 4296}#
                                                    #{id\ 4318}#
                                                    #{label\ 4319}#)
                                                  (#{parse\ 4298}#
                                                    (cdr #{body\ 4299}#)
                                                    (cons #{id\ 4318}#
                                                          #{ids\ 4300}#)
                                                    (cons #{label\ 4319}#
                                                          #{labels\ 4301}#)
                                                    #{var-ids\ 4302}#
                                                    #{vars\ 4303}#
                                                    #{vals\ 4304}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 4308}#
                                                                      (#{wrap\ 3823}#
                                                                        #{e\ 4311}#
                                                                        #{w\ 4312}#
                                                                        #{mod\ 4314}#)))
                                                          #{bindings\ 4305}#))))
                                              (if (memv #{type\ 4309}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 4320}#)
                                                   ((lambda (#{tmp\ 4321}#)
                                                      (if #{tmp\ 4321}#
                                                        (apply (lambda (#{_\ 4322}#
                                                                        #{e1\ 4323}#)
                                                                 (#{parse\ 4298}#
                                                                   (letrec ((#{f\ 4324}#
                                                                              (lambda (#{forms\ 4325}#)
                                                                                (if (null? #{forms\ 4325}#)
                                                                                  (cdr #{body\ 4299}#)
                                                                                  (cons (cons #{er\ 4308}#
                                                                                              (#{wrap\ 3823}#
                                                                                                (car #{forms\ 4325}#)
                                                                                                #{w\ 4312}#
                                                                                                #{mod\ 4314}#))
                                                                                        (#{f\ 4324}#
                                                                                          (cdr #{forms\ 4325}#)))))))
                                                                     (#{f\ 4324}#
                                                                       #{e1\ 4323}#))
                                                                   #{ids\ 4300}#
                                                                   #{labels\ 4301}#
                                                                   #{var-ids\ 4302}#
                                                                   #{vars\ 4303}#
                                                                   #{vals\ 4304}#
                                                                   #{bindings\ 4305}#))
                                                               #{tmp\ 4321}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 4320}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 4320}#
                                                      '(any . each-any))))
                                                 #{e\ 4311}#)
                                                (if (memv #{type\ 4309}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 3836}#
                                                    #{value\ 4310}#
                                                    #{e\ 4311}#
                                                    #{er\ 4308}#
                                                    #{w\ 4312}#
                                                    #{s\ 4313}#
                                                    #{mod\ 4314}#
                                                    (lambda (#{forms\ 4327}#
                                                             #{er\ 4328}#
                                                             #{w\ 4329}#
                                                             #{s\ 4330}#
                                                             #{mod\ 4331}#)
                                                      (#{parse\ 4298}#
                                                        (letrec ((#{f\ 4332}#
                                                                   (lambda (#{forms\ 4333}#)
                                                                     (if (null? #{forms\ 4333}#)
                                                                       (cdr #{body\ 4299}#)
                                                                       (cons (cons #{er\ 4328}#
                                                                                   (#{wrap\ 3823}#
                                                                                     (car #{forms\ 4333}#)
                                                                                     #{w\ 4329}#
                                                                                     #{mod\ 4331}#))
                                                                             (#{f\ 4332}#
                                                                               (cdr #{forms\ 4333}#)))))))
                                                          (#{f\ 4332}#
                                                            #{forms\ 4327}#))
                                                        #{ids\ 4300}#
                                                        #{labels\ 4301}#
                                                        #{var-ids\ 4302}#
                                                        #{vars\ 4303}#
                                                        #{vals\ 4304}#
                                                        #{bindings\ 4305}#)))
                                                  (if (null? #{ids\ 4300}#)
                                                    (#{build-sequence\ 3774}#
                                                      #f
                                                      (map (lambda (#{x\ 4334}#)
                                                             (#{chi\ 3831}#
                                                               (cdr #{x\ 4334}#)
                                                               (car #{x\ 4334}#)
                                                               '(())
                                                               #{mod\ 4314}#))
                                                           (cons (cons #{er\ 4308}#
                                                                       (#{source-wrap\ 3824}#
                                                                         #{e\ 4311}#
                                                                         #{w\ 4312}#
                                                                         #{s\ 4313}#
                                                                         #{mod\ 4314}#))
                                                                 (cdr #{body\ 4299}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 3820}#
                                                                 #{ids\ 4300}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 4291}#))
                                                      (letrec ((#{loop\ 4335}#
                                                                 (lambda (#{bs\ 4336}#
                                                                          #{er-cache\ 4337}#
                                                                          #{r-cache\ 4338}#)
                                                                   (if (not (null? #{bs\ 4336}#))
                                                                     (let ((#{b\ 4339}#
                                                                             (car #{bs\ 4336}#)))
                                                                       (if (eq? (car #{b\ 4339}#)
                                                                                'macro)
                                                                         (let ((#{er\ 4340}#
                                                                                 (cadr #{b\ 4339}#)))
                                                                           (let ((#{r-cache\ 4341}#
                                                                                   (if (eq? #{er\ 4340}#
                                                                                            #{er-cache\ 4337}#)
                                                                                     #{r-cache\ 4338}#
                                                                                     (#{macros-only-env\ 3791}#
                                                                                       #{er\ 4340}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 4339}#
                                                                                 (#{eval-local-transformer\ 3837}#
                                                                                   (#{chi\ 3831}#
                                                                                     (cddr #{b\ 4339}#)
                                                                                     #{r-cache\ 4341}#
                                                                                     '(())
                                                                                     #{mod\ 4314}#)
                                                                                   #{mod\ 4314}#))
                                                                               (#{loop\ 4335}#
                                                                                 (cdr #{bs\ 4336}#)
                                                                                 #{er\ 4340}#
                                                                                 #{r-cache\ 4341}#))))
                                                                         (#{loop\ 4335}#
                                                                           (cdr #{bs\ 4336}#)
                                                                           #{er-cache\ 4337}#
                                                                           #{r-cache\ 4338}#)))))))
                                                        (#{loop\ 4335}#
                                                          #{bindings\ 4305}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 4295}#
                                                        (#{extend-env\ 3789}#
                                                          #{labels\ 4301}#
                                                          #{bindings\ 4305}#
                                                          (cdr #{r\ 4295}#)))
                                                      (#{build-letrec\ 3777}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 4302}#)
                                                        #{vars\ 4303}#
                                                        (map (lambda (#{x\ 4342}#)
                                                               (#{chi\ 3831}#
                                                                 (cdr #{x\ 4342}#)
                                                                 (car #{x\ 4342}#)
                                                                 '(())
                                                                 #{mod\ 4314}#))
                                                             #{vals\ 4304}#)
                                                        (#{build-sequence\ 3774}#
                                                          #f
                                                          (map (lambda (#{x\ 4343}#)
                                                                 (#{chi\ 3831}#
                                                                   (cdr #{x\ 4343}#)
                                                                   (car #{x\ 4343}#)
                                                                   '(())
                                                                   #{mod\ 4314}#))
                                                               (cons (cons #{er\ 4308}#
                                                                           (#{source-wrap\ 3824}#
                                                                             #{e\ 4311}#
                                                                             #{w\ 4312}#
                                                                             #{s\ 4313}#
                                                                             #{mod\ 4314}#))
                                                                     (cdr #{body\ 4299}#))))))))))))))))))
                       (#{parse\ 4298}#
                         (map (lambda (#{x\ 4306}#)
                                (cons #{r\ 4295}#
                                      (#{wrap\ 3823}#
                                        #{x\ 4306}#
                                        #{w\ 4297}#
                                        #{mod\ 4294}#)))
                              #{body\ 4290}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 3834}#
             (lambda (#{p\ 4344}#
                      #{e\ 4345}#
                      #{r\ 4346}#
                      #{w\ 4347}#
                      #{rib\ 4348}#
                      #{mod\ 4349}#)
               (letrec ((#{rebuild-macro-output\ 4350}#
                          (lambda (#{x\ 4351}# #{m\ 4352}#)
                            (if (pair? #{x\ 4351}#)
                              (cons (#{rebuild-macro-output\ 4350}#
                                      (car #{x\ 4351}#)
                                      #{m\ 4352}#)
                                    (#{rebuild-macro-output\ 4350}#
                                      (cdr #{x\ 4351}#)
                                      #{m\ 4352}#))
                              (if (#{syntax-object?\ 3779}# #{x\ 4351}#)
                                (let ((#{w\ 4353}#
                                        (#{syntax-object-wrap\ 3781}#
                                          #{x\ 4351}#)))
                                  (let ((#{ms\ 4354}#
                                          (#{wrap-marks\ 3798}# #{w\ 4353}#))
                                        (#{s\ 4355}#
                                          (#{wrap-subst\ 3799}# #{w\ 4353}#)))
                                    (if (if (pair? #{ms\ 4354}#)
                                          (eq? (car #{ms\ 4354}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 3778}#
                                        (#{syntax-object-expression\ 3780}#
                                          #{x\ 4351}#)
                                        (#{make-wrap\ 3797}#
                                          (cdr #{ms\ 4354}#)
                                          (if #{rib\ 4348}#
                                            (cons #{rib\ 4348}#
                                                  (cdr #{s\ 4355}#))
                                            (cdr #{s\ 4355}#)))
                                        (#{syntax-object-module\ 3782}#
                                          #{x\ 4351}#))
                                      (#{make-syntax-object\ 3778}#
                                        (#{syntax-object-expression\ 3780}#
                                          #{x\ 4351}#)
                                        (#{make-wrap\ 3797}#
                                          (cons #{m\ 4352}# #{ms\ 4354}#)
                                          (if #{rib\ 4348}#
                                            (cons #{rib\ 4348}#
                                                  (cons 'shift
                                                        #{s\ 4355}#))
                                            (cons (quote shift) #{s\ 4355}#)))
                                        (cons 'hygiene
                                              (cdr #{p\ 4344}#))))))
                                (if (vector? #{x\ 4351}#)
                                  (let ((#{n\ 4356}#
                                          (vector-length #{x\ 4351}#)))
                                    (let ((#{v\ 4357}#
                                            (make-vector #{n\ 4356}#)))
                                      (letrec ((#{loop\ 4358}#
                                                 (lambda (#{i\ 4359}#)
                                                   (if (#{fx=\ 3752}#
                                                         #{i\ 4359}#
                                                         #{n\ 4356}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 4357}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 4357}#
                                                         #{i\ 4359}#
                                                         (#{rebuild-macro-output\ 4350}#
                                                           (vector-ref
                                                             #{x\ 4351}#
                                                             #{i\ 4359}#)
                                                           #{m\ 4352}#))
                                                       (#{loop\ 4358}#
                                                         (#{fx+\ 3750}#
                                                           #{i\ 4359}#
                                                           1)))))))
                                        (#{loop\ 4358}# 0))))
                                  (if (symbol? #{x\ 4351}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 3824}#
                                        #{e\ 4345}#
                                        #{w\ 4347}#
                                        (#{wrap-subst\ 3799}# #{w\ 4347}#)
                                        #{mod\ 4349}#)
                                      #{x\ 4351}#)
                                    #{x\ 4351}#)))))))
                 (#{rebuild-macro-output\ 4350}#
                   ((car #{p\ 4344}#)
                    (#{wrap\ 3823}#
                      #{e\ 4345}#
                      (#{anti-mark\ 3810}# #{w\ 4347}#)
                      #{mod\ 4349}#))
                   (string #\m)))))
           (#{chi-application\ 3833}#
             (lambda (#{x\ 4360}#
                      #{e\ 4361}#
                      #{r\ 4362}#
                      #{w\ 4363}#
                      #{s\ 4364}#
                      #{mod\ 4365}#)
               ((lambda (#{tmp\ 4366}#)
                  ((lambda (#{tmp\ 4367}#)
                     (if #{tmp\ 4367}#
                       (apply (lambda (#{e0\ 4368}# #{e1\ 4369}#)
                                (#{build-application\ 3760}#
                                  #{s\ 4364}#
                                  #{x\ 4360}#
                                  (map (lambda (#{e\ 4370}#)
                                         (#{chi\ 3831}#
                                           #{e\ 4370}#
                                           #{r\ 4362}#
                                           #{w\ 4363}#
                                           #{mod\ 4365}#))
                                       #{e1\ 4369}#)))
                              #{tmp\ 4367}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 4366}#)))
                   ($sc-dispatch
                     #{tmp\ 4366}#
                     '(any . each-any))))
                #{e\ 4361}#)))
           (#{chi-expr\ 3832}#
             (lambda (#{type\ 4372}#
                      #{value\ 4373}#
                      #{e\ 4374}#
                      #{r\ 4375}#
                      #{w\ 4376}#
                      #{s\ 4377}#
                      #{mod\ 4378}#)
               (if (memv #{type\ 4372}# (quote (lexical)))
                 (#{build-lexical-reference\ 3762}#
                   'value
                   #{s\ 4377}#
                   #{e\ 4374}#
                   #{value\ 4373}#)
                 (if (memv #{type\ 4372}# (quote (core core-form)))
                   (#{value\ 4373}#
                     #{e\ 4374}#
                     #{r\ 4375}#
                     #{w\ 4376}#
                     #{s\ 4377}#
                     #{mod\ 4378}#)
                   (if (memv #{type\ 4372}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 4373}# #{e\ 4374}#))
                       (lambda (#{id\ 4379}# #{mod\ 4380}#)
                         (#{build-global-reference\ 3765}#
                           #{s\ 4377}#
                           #{id\ 4379}#
                           #{mod\ 4380}#)))
                     (if (memv #{type\ 4372}# (quote (lexical-call)))
                       (#{chi-application\ 3833}#
                         (#{build-lexical-reference\ 3762}#
                           'fun
                           (#{source-annotation\ 3786}# (car #{e\ 4374}#))
                           (car #{e\ 4374}#)
                           #{value\ 4373}#)
                         #{e\ 4374}#
                         #{r\ 4375}#
                         #{w\ 4376}#
                         #{s\ 4377}#
                         #{mod\ 4378}#)
                       (if (memv #{type\ 4372}# (quote (global-call)))
                         (#{chi-application\ 3833}#
                           (#{build-global-reference\ 3765}#
                             (#{source-annotation\ 3786}# (car #{e\ 4374}#))
                             (if (#{syntax-object?\ 3779}# #{value\ 4373}#)
                               (#{syntax-object-expression\ 3780}#
                                 #{value\ 4373}#)
                               #{value\ 4373}#)
                             (if (#{syntax-object?\ 3779}# #{value\ 4373}#)
                               (#{syntax-object-module\ 3782}# #{value\ 4373}#)
                               #{mod\ 4378}#))
                           #{e\ 4374}#
                           #{r\ 4375}#
                           #{w\ 4376}#
                           #{s\ 4377}#
                           #{mod\ 4378}#)
                         (if (memv #{type\ 4372}# (quote (constant)))
                           (#{build-data\ 3773}#
                             #{s\ 4377}#
                             (#{strip\ 3844}#
                               (#{source-wrap\ 3824}#
                                 #{e\ 4374}#
                                 #{w\ 4376}#
                                 #{s\ 4377}#
                                 #{mod\ 4378}#)
                               '(())))
                           (if (memv #{type\ 4372}# (quote (global)))
                             (#{build-global-reference\ 3765}#
                               #{s\ 4377}#
                               #{value\ 4373}#
                               #{mod\ 4378}#)
                             (if (memv #{type\ 4372}# (quote (call)))
                               (#{chi-application\ 3833}#
                                 (#{chi\ 3831}#
                                   (car #{e\ 4374}#)
                                   #{r\ 4375}#
                                   #{w\ 4376}#
                                   #{mod\ 4378}#)
                                 #{e\ 4374}#
                                 #{r\ 4375}#
                                 #{w\ 4376}#
                                 #{s\ 4377}#
                                 #{mod\ 4378}#)
                               (if (memv #{type\ 4372}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 4381}#)
                                    ((lambda (#{tmp\ 4382}#)
                                       (if #{tmp\ 4382}#
                                         (apply (lambda (#{_\ 4383}#
                                                         #{e1\ 4384}#
                                                         #{e2\ 4385}#)
                                                  (#{chi-sequence\ 3825}#
                                                    (cons #{e1\ 4384}#
                                                          #{e2\ 4385}#)
                                                    #{r\ 4375}#
                                                    #{w\ 4376}#
                                                    #{s\ 4377}#
                                                    #{mod\ 4378}#))
                                                #{tmp\ 4382}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 4381}#)))
                                     ($sc-dispatch
                                       #{tmp\ 4381}#
                                       '(any any . each-any))))
                                  #{e\ 4374}#)
                                 (if (memv #{type\ 4372}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 3836}#
                                     #{value\ 4373}#
                                     #{e\ 4374}#
                                     #{r\ 4375}#
                                     #{w\ 4376}#
                                     #{s\ 4377}#
                                     #{mod\ 4378}#
                                     #{chi-sequence\ 3825}#)
                                   (if (memv #{type\ 4372}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 4387}#)
                                        ((lambda (#{tmp\ 4388}#)
                                           (if #{tmp\ 4388}#
                                             (apply (lambda (#{_\ 4389}#
                                                             #{x\ 4390}#
                                                             #{e1\ 4391}#
                                                             #{e2\ 4392}#)
                                                      (let ((#{when-list\ 4393}#
                                                              (#{chi-when-list\ 3828}#
                                                                #{e\ 4374}#
                                                                #{x\ 4390}#
                                                                #{w\ 4376}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 4393}#)
                                                          (#{chi-sequence\ 3825}#
                                                            (cons #{e1\ 4391}#
                                                                  #{e2\ 4392}#)
                                                            #{r\ 4375}#
                                                            #{w\ 4376}#
                                                            #{s\ 4377}#
                                                            #{mod\ 4378}#)
                                                          (#{chi-void\ 3838}#))))
                                                    #{tmp\ 4388}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 4387}#)))
                                         ($sc-dispatch
                                           #{tmp\ 4387}#
                                           '(any each-any any . each-any))))
                                      #{e\ 4374}#)
                                     (if (memv #{type\ 4372}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 4374}#
                                         (#{wrap\ 3823}#
                                           #{value\ 4373}#
                                           #{w\ 4376}#
                                           #{mod\ 4378}#))
                                       (if (memv #{type\ 4372}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 3824}#
                                             #{e\ 4374}#
                                             #{w\ 4376}#
                                             #{s\ 4377}#
                                             #{mod\ 4378}#))
                                         (if (memv #{type\ 4372}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 3824}#
                                               #{e\ 4374}#
                                               #{w\ 4376}#
                                               #{s\ 4377}#
                                               #{mod\ 4378}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 3824}#
                                               #{e\ 4374}#
                                               #{w\ 4376}#
                                               #{s\ 4377}#
                                               #{mod\ 4378}#))))))))))))))))))
           (#{chi\ 3831}#
             (lambda (#{e\ 4396}#
                      #{r\ 4397}#
                      #{w\ 4398}#
                      #{mod\ 4399}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 3829}#
                     #{e\ 4396}#
                     #{r\ 4397}#
                     #{w\ 4398}#
                     (#{source-annotation\ 3786}# #{e\ 4396}#)
                     #f
                     #{mod\ 4399}#
                     #f))
                 (lambda (#{type\ 4400}#
                          #{value\ 4401}#
                          #{e\ 4402}#
                          #{w\ 4403}#
                          #{s\ 4404}#
                          #{mod\ 4405}#)
                   (#{chi-expr\ 3832}#
                     #{type\ 4400}#
                     #{value\ 4401}#
                     #{e\ 4402}#
                     #{r\ 4397}#
                     #{w\ 4403}#
                     #{s\ 4404}#
                     #{mod\ 4405}#)))))
           (#{chi-top\ 3830}#
             (lambda (#{e\ 4406}#
                      #{r\ 4407}#
                      #{w\ 4408}#
                      #{m\ 4409}#
                      #{esew\ 4410}#
                      #{mod\ 4411}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 3829}#
                     #{e\ 4406}#
                     #{r\ 4407}#
                     #{w\ 4408}#
                     (#{source-annotation\ 3786}# #{e\ 4406}#)
                     #f
                     #{mod\ 4411}#
                     #f))
                 (lambda (#{type\ 4419}#
                          #{value\ 4420}#
                          #{e\ 4421}#
                          #{w\ 4422}#
                          #{s\ 4423}#
                          #{mod\ 4424}#)
                   (if (memv #{type\ 4419}# (quote (begin-form)))
                     ((lambda (#{tmp\ 4425}#)
                        ((lambda (#{tmp\ 4426}#)
                           (if #{tmp\ 4426}#
                             (apply (lambda (#{_\ 4427}#) (#{chi-void\ 3838}#))
                                    #{tmp\ 4426}#)
                             ((lambda (#{tmp\ 4428}#)
                                (if #{tmp\ 4428}#
                                  (apply (lambda (#{_\ 4429}#
                                                  #{e1\ 4430}#
                                                  #{e2\ 4431}#)
                                           (#{chi-top-sequence\ 3826}#
                                             (cons #{e1\ 4430}# #{e2\ 4431}#)
                                             #{r\ 4407}#
                                             #{w\ 4422}#
                                             #{s\ 4423}#
                                             #{m\ 4409}#
                                             #{esew\ 4410}#
                                             #{mod\ 4424}#))
                                         #{tmp\ 4428}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 4425}#)))
                              ($sc-dispatch
                                #{tmp\ 4425}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 4425}# (quote (any)))))
                      #{e\ 4421}#)
                     (if (memv #{type\ 4419}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 3836}#
                         #{value\ 4420}#
                         #{e\ 4421}#
                         #{r\ 4407}#
                         #{w\ 4422}#
                         #{s\ 4423}#
                         #{mod\ 4424}#
                         (lambda (#{body\ 4433}#
                                  #{r\ 4434}#
                                  #{w\ 4435}#
                                  #{s\ 4436}#
                                  #{mod\ 4437}#)
                           (#{chi-top-sequence\ 3826}#
                             #{body\ 4433}#
                             #{r\ 4434}#
                             #{w\ 4435}#
                             #{s\ 4436}#
                             #{m\ 4409}#
                             #{esew\ 4410}#
                             #{mod\ 4437}#)))
                       (if (memv #{type\ 4419}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 4438}#)
                            ((lambda (#{tmp\ 4439}#)
                               (if #{tmp\ 4439}#
                                 (apply (lambda (#{_\ 4440}#
                                                 #{x\ 4441}#
                                                 #{e1\ 4442}#
                                                 #{e2\ 4443}#)
                                          (let ((#{when-list\ 4444}#
                                                  (#{chi-when-list\ 3828}#
                                                    #{e\ 4421}#
                                                    #{x\ 4441}#
                                                    #{w\ 4422}#))
                                                (#{body\ 4445}#
                                                  (cons #{e1\ 4442}#
                                                        #{e2\ 4443}#)))
                                            (if (eq? #{m\ 4409}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 4444}#)
                                                (#{chi-top-sequence\ 3826}#
                                                  #{body\ 4445}#
                                                  #{r\ 4407}#
                                                  #{w\ 4422}#
                                                  #{s\ 4423}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 4424}#)
                                                (#{chi-void\ 3838}#))
                                              (if (memq 'load
                                                        #{when-list\ 4444}#)
                                                (if (let ((#{t\ 4448}#
                                                            (memq 'compile
                                                                  #{when-list\ 4444}#)))
                                                      (if #{t\ 4448}#
                                                        #{t\ 4448}#
                                                        (if (eq? #{m\ 4409}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 4444}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 3826}#
                                                    #{body\ 4445}#
                                                    #{r\ 4407}#
                                                    #{w\ 4422}#
                                                    #{s\ 4423}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 4424}#)
                                                  (if (memq #{m\ 4409}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 3826}#
                                                      #{body\ 4445}#
                                                      #{r\ 4407}#
                                                      #{w\ 4422}#
                                                      #{s\ 4423}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 4424}#)
                                                    (#{chi-void\ 3838}#)))
                                                (if (let ((#{t\ 4449}#
                                                            (memq 'compile
                                                                  #{when-list\ 4444}#)))
                                                      (if #{t\ 4449}#
                                                        #{t\ 4449}#
                                                        (if (eq? #{m\ 4409}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 4444}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 3754}#
                                                      (#{chi-top-sequence\ 3826}#
                                                        #{body\ 4445}#
                                                        #{r\ 4407}#
                                                        #{w\ 4422}#
                                                        #{s\ 4423}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 4424}#)
                                                      #{mod\ 4424}#)
                                                    (#{chi-void\ 3838}#))
                                                  (#{chi-void\ 3838}#))))))
                                        #{tmp\ 4439}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 4438}#)))
                             ($sc-dispatch
                               #{tmp\ 4438}#
                               '(any each-any any . each-any))))
                          #{e\ 4421}#)
                         (if (memv #{type\ 4419}#
                                   '(define-syntax-form))
                           (let ((#{n\ 4450}#
                                   (#{id-var-name\ 3817}#
                                     #{value\ 4420}#
                                     #{w\ 4422}#))
                                 (#{r\ 4451}#
                                   (#{macros-only-env\ 3791}# #{r\ 4407}#)))
                             (if (memv #{m\ 4409}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 4410}#)
                                 (let ((#{e\ 4452}#
                                         (#{chi-install-global\ 3827}#
                                           #{n\ 4450}#
                                           (#{chi\ 3831}#
                                             #{e\ 4421}#
                                             #{r\ 4451}#
                                             #{w\ 4422}#
                                             #{mod\ 4424}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 3754}#
                                       #{e\ 4452}#
                                       #{mod\ 4424}#)
                                     (if (memq (quote load) #{esew\ 4410}#)
                                       #{e\ 4452}#
                                       (#{chi-void\ 3838}#))))
                                 (if (memq (quote load) #{esew\ 4410}#)
                                   (#{chi-install-global\ 3827}#
                                     #{n\ 4450}#
                                     (#{chi\ 3831}#
                                       #{e\ 4421}#
                                       #{r\ 4451}#
                                       #{w\ 4422}#
                                       #{mod\ 4424}#))
                                   (#{chi-void\ 3838}#)))
                               (if (memv #{m\ 4409}# (quote (c&e)))
                                 (let ((#{e\ 4453}#
                                         (#{chi-install-global\ 3827}#
                                           #{n\ 4450}#
                                           (#{chi\ 3831}#
                                             #{e\ 4421}#
                                             #{r\ 4451}#
                                             #{w\ 4422}#
                                             #{mod\ 4424}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 3754}#
                                       #{e\ 4453}#
                                       #{mod\ 4424}#)
                                     #{e\ 4453}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 4410}#)
                                     (#{top-level-eval-hook\ 3754}#
                                       (#{chi-install-global\ 3827}#
                                         #{n\ 4450}#
                                         (#{chi\ 3831}#
                                           #{e\ 4421}#
                                           #{r\ 4451}#
                                           #{w\ 4422}#
                                           #{mod\ 4424}#))
                                       #{mod\ 4424}#))
                                   (#{chi-void\ 3838}#)))))
                           (if (memv #{type\ 4419}# (quote (define-form)))
                             (let ((#{n\ 4454}#
                                     (#{id-var-name\ 3817}#
                                       #{value\ 4420}#
                                       #{w\ 4422}#)))
                               (let ((#{type\ 4455}#
                                       (#{binding-type\ 3787}#
                                         (#{lookup\ 3792}#
                                           #{n\ 4454}#
                                           #{r\ 4407}#
                                           #{mod\ 4424}#))))
                                 (if (memv #{type\ 4455}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 4454}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 4456}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 4454}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 4454}#
                                           (if (variable? #{old\ 4456}#)
                                             (variable-ref #{old\ 4456}#)
                                             #f))))
                                     (let ((#{x\ 4457}#
                                             (#{build-global-definition\ 3768}#
                                               #{s\ 4423}#
                                               #{n\ 4454}#
                                               (#{chi\ 3831}#
                                                 #{e\ 4421}#
                                                 #{r\ 4407}#
                                                 #{w\ 4422}#
                                                 #{mod\ 4424}#))))
                                       (begin
                                         (if (eq? #{m\ 4409}# (quote c&e))
                                           (#{top-level-eval-hook\ 3754}#
                                             #{x\ 4457}#
                                             #{mod\ 4424}#))
                                         #{x\ 4457}#)))
                                   (if (memv #{type\ 4455}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 4421}#
                                       (#{wrap\ 3823}#
                                         #{value\ 4420}#
                                         #{w\ 4422}#
                                         #{mod\ 4424}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 4421}#
                                       (#{wrap\ 3823}#
                                         #{value\ 4420}#
                                         #{w\ 4422}#
                                         #{mod\ 4424}#))))))
                             (let ((#{x\ 4458}#
                                     (#{chi-expr\ 3832}#
                                       #{type\ 4419}#
                                       #{value\ 4420}#
                                       #{e\ 4421}#
                                       #{r\ 4407}#
                                       #{w\ 4422}#
                                       #{s\ 4423}#
                                       #{mod\ 4424}#)))
                               (begin
                                 (if (eq? #{m\ 4409}# (quote c&e))
                                   (#{top-level-eval-hook\ 3754}#
                                     #{x\ 4458}#
                                     #{mod\ 4424}#))
                                 #{x\ 4458}#)))))))))))
           (#{syntax-type\ 3829}#
             (lambda (#{e\ 4459}#
                      #{r\ 4460}#
                      #{w\ 4461}#
                      #{s\ 4462}#
                      #{rib\ 4463}#
                      #{mod\ 4464}#
                      #{for-car?\ 4465}#)
               (if (symbol? #{e\ 4459}#)
                 (let ((#{n\ 4466}#
                         (#{id-var-name\ 3817}# #{e\ 4459}# #{w\ 4461}#)))
                   (let ((#{b\ 4467}#
                           (#{lookup\ 3792}#
                             #{n\ 4466}#
                             #{r\ 4460}#
                             #{mod\ 4464}#)))
                     (let ((#{type\ 4468}#
                             (#{binding-type\ 3787}# #{b\ 4467}#)))
                       (if (memv #{type\ 4468}# (quote (lexical)))
                         (values
                           #{type\ 4468}#
                           (#{binding-value\ 3788}# #{b\ 4467}#)
                           #{e\ 4459}#
                           #{w\ 4461}#
                           #{s\ 4462}#
                           #{mod\ 4464}#)
                         (if (memv #{type\ 4468}# (quote (global)))
                           (values
                             #{type\ 4468}#
                             #{n\ 4466}#
                             #{e\ 4459}#
                             #{w\ 4461}#
                             #{s\ 4462}#
                             #{mod\ 4464}#)
                           (if (memv #{type\ 4468}# (quote (macro)))
                             (if #{for-car?\ 4465}#
                               (values
                                 #{type\ 4468}#
                                 (#{binding-value\ 3788}# #{b\ 4467}#)
                                 #{e\ 4459}#
                                 #{w\ 4461}#
                                 #{s\ 4462}#
                                 #{mod\ 4464}#)
                               (#{syntax-type\ 3829}#
                                 (#{chi-macro\ 3834}#
                                   (#{binding-value\ 3788}# #{b\ 4467}#)
                                   #{e\ 4459}#
                                   #{r\ 4460}#
                                   #{w\ 4461}#
                                   #{rib\ 4463}#
                                   #{mod\ 4464}#)
                                 #{r\ 4460}#
                                 '(())
                                 #{s\ 4462}#
                                 #{rib\ 4463}#
                                 #{mod\ 4464}#
                                 #f))
                             (values
                               #{type\ 4468}#
                               (#{binding-value\ 3788}# #{b\ 4467}#)
                               #{e\ 4459}#
                               #{w\ 4461}#
                               #{s\ 4462}#
                               #{mod\ 4464}#)))))))
                 (if (pair? #{e\ 4459}#)
                   (let ((#{first\ 4469}# (car #{e\ 4459}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 3829}#
                           #{first\ 4469}#
                           #{r\ 4460}#
                           #{w\ 4461}#
                           #{s\ 4462}#
                           #{rib\ 4463}#
                           #{mod\ 4464}#
                           #t))
                       (lambda (#{ftype\ 4470}#
                                #{fval\ 4471}#
                                #{fe\ 4472}#
                                #{fw\ 4473}#
                                #{fs\ 4474}#
                                #{fmod\ 4475}#)
                         (if (memv #{ftype\ 4470}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 4471}#
                             #{e\ 4459}#
                             #{w\ 4461}#
                             #{s\ 4462}#
                             #{mod\ 4464}#)
                           (if (memv #{ftype\ 4470}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 3778}#
                                 #{fval\ 4471}#
                                 #{w\ 4461}#
                                 #{fmod\ 4475}#)
                               #{e\ 4459}#
                               #{w\ 4461}#
                               #{s\ 4462}#
                               #{mod\ 4464}#)
                             (if (memv #{ftype\ 4470}# (quote (macro)))
                               (#{syntax-type\ 3829}#
                                 (#{chi-macro\ 3834}#
                                   #{fval\ 4471}#
                                   #{e\ 4459}#
                                   #{r\ 4460}#
                                   #{w\ 4461}#
                                   #{rib\ 4463}#
                                   #{mod\ 4464}#)
                                 #{r\ 4460}#
                                 '(())
                                 #{s\ 4462}#
                                 #{rib\ 4463}#
                                 #{mod\ 4464}#
                                 #{for-car?\ 4465}#)
                               (if (memv #{ftype\ 4470}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 4471}# #{e\ 4459}#))
                                   (lambda (#{sym\ 4476}# #{mod\ 4477}#)
                                     (#{syntax-type\ 3829}#
                                       #{sym\ 4476}#
                                       #{r\ 4460}#
                                       #{w\ 4461}#
                                       #{s\ 4462}#
                                       #{rib\ 4463}#
                                       #{mod\ 4477}#
                                       #{for-car?\ 4465}#)))
                                 (if (memv #{ftype\ 4470}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 4471}#
                                     #{e\ 4459}#
                                     #{w\ 4461}#
                                     #{s\ 4462}#
                                     #{mod\ 4464}#)
                                   (if (memv #{ftype\ 4470}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 4471}#
                                       #{e\ 4459}#
                                       #{w\ 4461}#
                                       #{s\ 4462}#
                                       #{mod\ 4464}#)
                                     (if (memv #{ftype\ 4470}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 4459}#
                                         #{w\ 4461}#
                                         #{s\ 4462}#
                                         #{mod\ 4464}#)
                                       (if (memv #{ftype\ 4470}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 4459}#
                                           #{w\ 4461}#
                                           #{s\ 4462}#
                                           #{mod\ 4464}#)
                                         (if (memv #{ftype\ 4470}#
                                                   '(define))
                                           ((lambda (#{tmp\ 4478}#)
                                              ((lambda (#{tmp\ 4479}#)
                                                 (if (if #{tmp\ 4479}#
                                                       (apply (lambda (#{_\ 4480}#
                                                                       #{name\ 4481}#
                                                                       #{val\ 4482}#)
                                                                (#{id?\ 3795}#
                                                                  #{name\ 4481}#))
                                                              #{tmp\ 4479}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 4483}#
                                                                   #{name\ 4484}#
                                                                   #{val\ 4485}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 4484}#
                                                              #{val\ 4485}#
                                                              #{w\ 4461}#
                                                              #{s\ 4462}#
                                                              #{mod\ 4464}#))
                                                          #{tmp\ 4479}#)
                                                   ((lambda (#{tmp\ 4486}#)
                                                      (if (if #{tmp\ 4486}#
                                                            (apply (lambda (#{_\ 4487}#
                                                                            #{name\ 4488}#
                                                                            #{args\ 4489}#
                                                                            #{e1\ 4490}#
                                                                            #{e2\ 4491}#)
                                                                     (if (#{id?\ 3795}#
                                                                           #{name\ 4488}#)
                                                                       (#{valid-bound-ids?\ 3820}#
                                                                         (#{lambda-var-list\ 3846}#
                                                                           #{args\ 4489}#))
                                                                       #f))
                                                                   #{tmp\ 4486}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 4492}#
                                                                        #{name\ 4493}#
                                                                        #{args\ 4494}#
                                                                        #{e1\ 4495}#
                                                                        #{e2\ 4496}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 3823}#
                                                                     #{name\ 4493}#
                                                                     #{w\ 4461}#
                                                                     #{mod\ 4464}#)
                                                                   (#{decorate-source\ 3758}#
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
                                                                           (#{wrap\ 3823}#
                                                                             (cons #{args\ 4494}#
                                                                                   (cons #{e1\ 4495}#
                                                                                         #{e2\ 4496}#))
                                                                             #{w\ 4461}#
                                                                             #{mod\ 4464}#))
                                                                     #{s\ 4462}#)
                                                                   '(())
                                                                   #{s\ 4462}#
                                                                   #{mod\ 4464}#))
                                                               #{tmp\ 4486}#)
                                                        ((lambda (#{tmp\ 4498}#)
                                                           (if (if #{tmp\ 4498}#
                                                                 (apply (lambda (#{_\ 4499}#
                                                                                 #{name\ 4500}#)
                                                                          (#{id?\ 3795}#
                                                                            #{name\ 4500}#))
                                                                        #{tmp\ 4498}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 4501}#
                                                                             #{name\ 4502}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 3823}#
                                                                          #{name\ 4502}#
                                                                          #{w\ 4461}#
                                                                          #{mod\ 4464}#)
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
                                                                        #{s\ 4462}#
                                                                        #{mod\ 4464}#))
                                                                    #{tmp\ 4498}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 4478}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 4478}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 4478}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 4478}#
                                                 '(any any any))))
                                            #{e\ 4459}#)
                                           (if (memv #{ftype\ 4470}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 4503}#)
                                                ((lambda (#{tmp\ 4504}#)
                                                   (if (if #{tmp\ 4504}#
                                                         (apply (lambda (#{_\ 4505}#
                                                                         #{name\ 4506}#
                                                                         #{val\ 4507}#)
                                                                  (#{id?\ 3795}#
                                                                    #{name\ 4506}#))
                                                                #{tmp\ 4504}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 4508}#
                                                                     #{name\ 4509}#
                                                                     #{val\ 4510}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 4509}#
                                                                #{val\ 4510}#
                                                                #{w\ 4461}#
                                                                #{s\ 4462}#
                                                                #{mod\ 4464}#))
                                                            #{tmp\ 4504}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 4503}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 4503}#
                                                   '(any any any))))
                                              #{e\ 4459}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 4459}#
                                               #{w\ 4461}#
                                               #{s\ 4462}#
                                               #{mod\ 4464}#))))))))))))))
                   (if (#{syntax-object?\ 3779}# #{e\ 4459}#)
                     (#{syntax-type\ 3829}#
                       (#{syntax-object-expression\ 3780}# #{e\ 4459}#)
                       #{r\ 4460}#
                       (#{join-wraps\ 3814}#
                         #{w\ 4461}#
                         (#{syntax-object-wrap\ 3781}# #{e\ 4459}#))
                       #{s\ 4462}#
                       #{rib\ 4463}#
                       (let ((#{t\ 4511}#
                               (#{syntax-object-module\ 3782}# #{e\ 4459}#)))
                         (if #{t\ 4511}# #{t\ 4511}# #{mod\ 4464}#))
                       #{for-car?\ 4465}#)
                     (if (self-evaluating? #{e\ 4459}#)
                       (values
                         'constant
                         #f
                         #{e\ 4459}#
                         #{w\ 4461}#
                         #{s\ 4462}#
                         #{mod\ 4464}#)
                       (values
                         'other
                         #f
                         #{e\ 4459}#
                         #{w\ 4461}#
                         #{s\ 4462}#
                         #{mod\ 4464}#)))))))
           (#{chi-when-list\ 3828}#
             (lambda (#{e\ 4512}# #{when-list\ 4513}# #{w\ 4514}#)
               (letrec ((#{f\ 4515}#
                          (lambda (#{when-list\ 4516}# #{situations\ 4517}#)
                            (if (null? #{when-list\ 4516}#)
                              #{situations\ 4517}#
                              (#{f\ 4515}#
                                (cdr #{when-list\ 4516}#)
                                (cons (let ((#{x\ 4518}#
                                              (car #{when-list\ 4516}#)))
                                        (if (#{free-id=?\ 3818}#
                                              #{x\ 4518}#
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
                                          (if (#{free-id=?\ 3818}#
                                                #{x\ 4518}#
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
                                            (if (#{free-id=?\ 3818}#
                                                  #{x\ 4518}#
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
                                                #{e\ 4512}#
                                                (#{wrap\ 3823}#
                                                  #{x\ 4518}#
                                                  #{w\ 4514}#
                                                  #f))))))
                                      #{situations\ 4517}#))))))
                 (#{f\ 4515}# #{when-list\ 4513}# (quote ())))))
           (#{chi-install-global\ 3827}#
             (lambda (#{name\ 4519}# #{e\ 4520}#)
               (#{build-global-definition\ 3768}#
                 #f
                 #{name\ 4519}#
                 (if (let ((#{v\ 4521}#
                             (module-variable
                               (current-module)
                               #{name\ 4519}#)))
                       (if #{v\ 4521}#
                         (if (variable-bound? #{v\ 4521}#)
                           (if (macro? (variable-ref #{v\ 4521}#))
                             (not (eq? (macro-type (variable-ref #{v\ 4521}#))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (#{build-application\ 3760}#
                     #f
                     (#{build-primref\ 3772}#
                       #f
                       'make-extended-syncase-macro)
                     (list (#{build-application\ 3760}#
                             #f
                             (#{build-primref\ 3772}# #f (quote module-ref))
                             (list (#{build-application\ 3760}#
                                     #f
                                     (#{build-primref\ 3772}#
                                       #f
                                       'current-module)
                                     '())
                                   (#{build-data\ 3773}# #f #{name\ 4519}#)))
                           (#{build-data\ 3773}# #f (quote macro))
                           (#{build-application\ 3760}#
                             #f
                             (#{build-primref\ 3772}# #f (quote cons))
                             (list #{e\ 4520}#
                                   (#{build-application\ 3760}#
                                     #f
                                     (#{build-primref\ 3772}#
                                       #f
                                       'module-name)
                                     (list (#{build-application\ 3760}#
                                             #f
                                             (#{build-primref\ 3772}#
                                               #f
                                               'current-module)
                                             '())))))))
                   (#{build-application\ 3760}#
                     #f
                     (#{build-primref\ 3772}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 3773}# #f (quote macro))
                           (#{build-application\ 3760}#
                             #f
                             (#{build-primref\ 3772}# #f (quote cons))
                             (list #{e\ 4520}#
                                   (#{build-application\ 3760}#
                                     #f
                                     (#{build-primref\ 3772}#
                                       #f
                                       'module-name)
                                     (list (#{build-application\ 3760}#
                                             #f
                                             (#{build-primref\ 3772}#
                                               #f
                                               'current-module)
                                             '())))))))))))
           (#{chi-top-sequence\ 3826}#
             (lambda (#{body\ 4522}#
                      #{r\ 4523}#
                      #{w\ 4524}#
                      #{s\ 4525}#
                      #{m\ 4526}#
                      #{esew\ 4527}#
                      #{mod\ 4528}#)
               (#{build-sequence\ 3774}#
                 #{s\ 4525}#
                 (letrec ((#{dobody\ 4529}#
                            (lambda (#{body\ 4530}#
                                     #{r\ 4531}#
                                     #{w\ 4532}#
                                     #{m\ 4533}#
                                     #{esew\ 4534}#
                                     #{mod\ 4535}#)
                              (if (null? #{body\ 4530}#)
                                '()
                                (let ((#{first\ 4536}#
                                        (#{chi-top\ 3830}#
                                          (car #{body\ 4530}#)
                                          #{r\ 4531}#
                                          #{w\ 4532}#
                                          #{m\ 4533}#
                                          #{esew\ 4534}#
                                          #{mod\ 4535}#)))
                                  (cons #{first\ 4536}#
                                        (#{dobody\ 4529}#
                                          (cdr #{body\ 4530}#)
                                          #{r\ 4531}#
                                          #{w\ 4532}#
                                          #{m\ 4533}#
                                          #{esew\ 4534}#
                                          #{mod\ 4535}#)))))))
                   (#{dobody\ 4529}#
                     #{body\ 4522}#
                     #{r\ 4523}#
                     #{w\ 4524}#
                     #{m\ 4526}#
                     #{esew\ 4527}#
                     #{mod\ 4528}#)))))
           (#{chi-sequence\ 3825}#
             (lambda (#{body\ 4537}#
                      #{r\ 4538}#
                      #{w\ 4539}#
                      #{s\ 4540}#
                      #{mod\ 4541}#)
               (#{build-sequence\ 3774}#
                 #{s\ 4540}#
                 (letrec ((#{dobody\ 4542}#
                            (lambda (#{body\ 4543}#
                                     #{r\ 4544}#
                                     #{w\ 4545}#
                                     #{mod\ 4546}#)
                              (if (null? #{body\ 4543}#)
                                '()
                                (let ((#{first\ 4547}#
                                        (#{chi\ 3831}#
                                          (car #{body\ 4543}#)
                                          #{r\ 4544}#
                                          #{w\ 4545}#
                                          #{mod\ 4546}#)))
                                  (cons #{first\ 4547}#
                                        (#{dobody\ 4542}#
                                          (cdr #{body\ 4543}#)
                                          #{r\ 4544}#
                                          #{w\ 4545}#
                                          #{mod\ 4546}#)))))))
                   (#{dobody\ 4542}#
                     #{body\ 4537}#
                     #{r\ 4538}#
                     #{w\ 4539}#
                     #{mod\ 4541}#)))))
           (#{source-wrap\ 3824}#
             (lambda (#{x\ 4548}#
                      #{w\ 4549}#
                      #{s\ 4550}#
                      #{defmod\ 4551}#)
               (#{wrap\ 3823}#
                 (#{decorate-source\ 3758}#
                   #{x\ 4548}#
                   #{s\ 4550}#)
                 #{w\ 4549}#
                 #{defmod\ 4551}#)))
           (#{wrap\ 3823}#
             (lambda (#{x\ 4552}# #{w\ 4553}# #{defmod\ 4554}#)
               (if (if (null? (#{wrap-marks\ 3798}# #{w\ 4553}#))
                     (null? (#{wrap-subst\ 3799}# #{w\ 4553}#))
                     #f)
                 #{x\ 4552}#
                 (if (#{syntax-object?\ 3779}# #{x\ 4552}#)
                   (#{make-syntax-object\ 3778}#
                     (#{syntax-object-expression\ 3780}# #{x\ 4552}#)
                     (#{join-wraps\ 3814}#
                       #{w\ 4553}#
                       (#{syntax-object-wrap\ 3781}# #{x\ 4552}#))
                     (#{syntax-object-module\ 3782}# #{x\ 4552}#))
                   (if (null? #{x\ 4552}#)
                     #{x\ 4552}#
                     (#{make-syntax-object\ 3778}#
                       #{x\ 4552}#
                       #{w\ 4553}#
                       #{defmod\ 4554}#))))))
           (#{bound-id-member?\ 3822}#
             (lambda (#{x\ 4555}# #{list\ 4556}#)
               (if (not (null? #{list\ 4556}#))
                 (let ((#{t\ 4557}#
                         (#{bound-id=?\ 3819}#
                           #{x\ 4555}#
                           (car #{list\ 4556}#))))
                   (if #{t\ 4557}#
                     #{t\ 4557}#
                     (#{bound-id-member?\ 3822}#
                       #{x\ 4555}#
                       (cdr #{list\ 4556}#))))
                 #f)))
           (#{distinct-bound-ids?\ 3821}#
             (lambda (#{ids\ 4558}#)
               (letrec ((#{distinct?\ 4559}#
                          (lambda (#{ids\ 4560}#)
                            (let ((#{t\ 4561}# (null? #{ids\ 4560}#)))
                              (if #{t\ 4561}#
                                #{t\ 4561}#
                                (if (not (#{bound-id-member?\ 3822}#
                                           (car #{ids\ 4560}#)
                                           (cdr #{ids\ 4560}#)))
                                  (#{distinct?\ 4559}# (cdr #{ids\ 4560}#))
                                  #f))))))
                 (#{distinct?\ 4559}# #{ids\ 4558}#))))
           (#{valid-bound-ids?\ 3820}#
             (lambda (#{ids\ 4562}#)
               (if (letrec ((#{all-ids?\ 4563}#
                              (lambda (#{ids\ 4564}#)
                                (let ((#{t\ 4565}# (null? #{ids\ 4564}#)))
                                  (if #{t\ 4565}#
                                    #{t\ 4565}#
                                    (if (#{id?\ 3795}# (car #{ids\ 4564}#))
                                      (#{all-ids?\ 4563}# (cdr #{ids\ 4564}#))
                                      #f))))))
                     (#{all-ids?\ 4563}# #{ids\ 4562}#))
                 (#{distinct-bound-ids?\ 3821}# #{ids\ 4562}#)
                 #f)))
           (#{bound-id=?\ 3819}#
             (lambda (#{i\ 4566}# #{j\ 4567}#)
               (if (if (#{syntax-object?\ 3779}# #{i\ 4566}#)
                     (#{syntax-object?\ 3779}# #{j\ 4567}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 3780}# #{i\ 4566}#)
                          (#{syntax-object-expression\ 3780}# #{j\ 4567}#))
                   (#{same-marks?\ 3816}#
                     (#{wrap-marks\ 3798}#
                       (#{syntax-object-wrap\ 3781}# #{i\ 4566}#))
                     (#{wrap-marks\ 3798}#
                       (#{syntax-object-wrap\ 3781}# #{j\ 4567}#)))
                   #f)
                 (eq? #{i\ 4566}# #{j\ 4567}#))))
           (#{free-id=?\ 3818}#
             (lambda (#{i\ 4568}# #{j\ 4569}#)
               (if (eq? (let ((#{x\ 4570}# #{i\ 4568}#))
                          (if (#{syntax-object?\ 3779}# #{x\ 4570}#)
                            (#{syntax-object-expression\ 3780}# #{x\ 4570}#)
                            #{x\ 4570}#))
                        (let ((#{x\ 4571}# #{j\ 4569}#))
                          (if (#{syntax-object?\ 3779}# #{x\ 4571}#)
                            (#{syntax-object-expression\ 3780}# #{x\ 4571}#)
                            #{x\ 4571}#)))
                 (eq? (#{id-var-name\ 3817}# #{i\ 4568}# (quote (())))
                      (#{id-var-name\ 3817}# #{j\ 4569}# (quote (()))))
                 #f)))
           (#{id-var-name\ 3817}#
             (lambda (#{id\ 4572}# #{w\ 4573}#)
               (letrec ((#{search-vector-rib\ 4576}#
                          (lambda (#{sym\ 4582}#
                                   #{subst\ 4583}#
                                   #{marks\ 4584}#
                                   #{symnames\ 4585}#
                                   #{ribcage\ 4586}#)
                            (let ((#{n\ 4587}#
                                    (vector-length #{symnames\ 4585}#)))
                              (letrec ((#{f\ 4588}#
                                         (lambda (#{i\ 4589}#)
                                           (if (#{fx=\ 3752}#
                                                 #{i\ 4589}#
                                                 #{n\ 4587}#)
                                             (#{search\ 4574}#
                                               #{sym\ 4582}#
                                               (cdr #{subst\ 4583}#)
                                               #{marks\ 4584}#)
                                             (if (if (eq? (vector-ref
                                                            #{symnames\ 4585}#
                                                            #{i\ 4589}#)
                                                          #{sym\ 4582}#)
                                                   (#{same-marks?\ 3816}#
                                                     #{marks\ 4584}#
                                                     (vector-ref
                                                       (#{ribcage-marks\ 3805}#
                                                         #{ribcage\ 4586}#)
                                                       #{i\ 4589}#))
                                                   #f)
                                               (values
                                                 (vector-ref
                                                   (#{ribcage-labels\ 3806}#
                                                     #{ribcage\ 4586}#)
                                                   #{i\ 4589}#)
                                                 #{marks\ 4584}#)
                                               (#{f\ 4588}#
                                                 (#{fx+\ 3750}#
                                                   #{i\ 4589}#
                                                   1)))))))
                                (#{f\ 4588}# 0)))))
                        (#{search-list-rib\ 4575}#
                          (lambda (#{sym\ 4590}#
                                   #{subst\ 4591}#
                                   #{marks\ 4592}#
                                   #{symnames\ 4593}#
                                   #{ribcage\ 4594}#)
                            (letrec ((#{f\ 4595}#
                                       (lambda (#{symnames\ 4596}# #{i\ 4597}#)
                                         (if (null? #{symnames\ 4596}#)
                                           (#{search\ 4574}#
                                             #{sym\ 4590}#
                                             (cdr #{subst\ 4591}#)
                                             #{marks\ 4592}#)
                                           (if (if (eq? (car #{symnames\ 4596}#)
                                                        #{sym\ 4590}#)
                                                 (#{same-marks?\ 3816}#
                                                   #{marks\ 4592}#
                                                   (list-ref
                                                     (#{ribcage-marks\ 3805}#
                                                       #{ribcage\ 4594}#)
                                                     #{i\ 4597}#))
                                                 #f)
                                             (values
                                               (list-ref
                                                 (#{ribcage-labels\ 3806}#
                                                   #{ribcage\ 4594}#)
                                                 #{i\ 4597}#)
                                               #{marks\ 4592}#)
                                             (#{f\ 4595}#
                                               (cdr #{symnames\ 4596}#)
                                               (#{fx+\ 3750}#
                                                 #{i\ 4597}#
                                                 1)))))))
                              (#{f\ 4595}# #{symnames\ 4593}# 0))))
                        (#{search\ 4574}#
                          (lambda (#{sym\ 4598}#
                                   #{subst\ 4599}#
                                   #{marks\ 4600}#)
                            (if (null? #{subst\ 4599}#)
                              (values #f #{marks\ 4600}#)
                              (let ((#{fst\ 4601}# (car #{subst\ 4599}#)))
                                (if (eq? #{fst\ 4601}# (quote shift))
                                  (#{search\ 4574}#
                                    #{sym\ 4598}#
                                    (cdr #{subst\ 4599}#)
                                    (cdr #{marks\ 4600}#))
                                  (let ((#{symnames\ 4602}#
                                          (#{ribcage-symnames\ 3804}#
                                            #{fst\ 4601}#)))
                                    (if (vector? #{symnames\ 4602}#)
                                      (#{search-vector-rib\ 4576}#
                                        #{sym\ 4598}#
                                        #{subst\ 4599}#
                                        #{marks\ 4600}#
                                        #{symnames\ 4602}#
                                        #{fst\ 4601}#)
                                      (#{search-list-rib\ 4575}#
                                        #{sym\ 4598}#
                                        #{subst\ 4599}#
                                        #{marks\ 4600}#
                                        #{symnames\ 4602}#
                                        #{fst\ 4601}#)))))))))
                 (if (symbol? #{id\ 4572}#)
                   (let ((#{t\ 4603}#
                           (call-with-values
                             (lambda ()
                               (#{search\ 4574}#
                                 #{id\ 4572}#
                                 (#{wrap-subst\ 3799}# #{w\ 4573}#)
                                 (#{wrap-marks\ 3798}# #{w\ 4573}#)))
                             (lambda (#{x\ 4604}# . #{ignore\ 4605}#)
                               #{x\ 4604}#))))
                     (if #{t\ 4603}# #{t\ 4603}# #{id\ 4572}#))
                   (if (#{syntax-object?\ 3779}# #{id\ 4572}#)
                     (let ((#{id\ 4606}#
                             (#{syntax-object-expression\ 3780}# #{id\ 4572}#))
                           (#{w1\ 4607}#
                             (#{syntax-object-wrap\ 3781}# #{id\ 4572}#)))
                       (let ((#{marks\ 4608}#
                               (#{join-marks\ 3815}#
                                 (#{wrap-marks\ 3798}# #{w\ 4573}#)
                                 (#{wrap-marks\ 3798}# #{w1\ 4607}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 4574}#
                               #{id\ 4606}#
                               (#{wrap-subst\ 3799}# #{w\ 4573}#)
                               #{marks\ 4608}#))
                           (lambda (#{new-id\ 4609}# #{marks\ 4610}#)
                             (let ((#{t\ 4611}# #{new-id\ 4609}#))
                               (if #{t\ 4611}#
                                 #{t\ 4611}#
                                 (let ((#{t\ 4612}#
                                         (call-with-values
                                           (lambda ()
                                             (#{search\ 4574}#
                                               #{id\ 4606}#
                                               (#{wrap-subst\ 3799}#
                                                 #{w1\ 4607}#)
                                               #{marks\ 4610}#))
                                           (lambda (#{x\ 4613}#
                                                    .
                                                    #{ignore\ 4614}#)
                                             #{x\ 4613}#))))
                                   (if #{t\ 4612}#
                                     #{t\ 4612}#
                                     #{id\ 4606}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 4572}#))))))
           (#{same-marks?\ 3816}#
             (lambda (#{x\ 4615}# #{y\ 4616}#)
               (let ((#{t\ 4617}# (eq? #{x\ 4615}# #{y\ 4616}#)))
                 (if #{t\ 4617}#
                   #{t\ 4617}#
                   (if (not (null? #{x\ 4615}#))
                     (if (not (null? #{y\ 4616}#))
                       (if (eq? (car #{x\ 4615}#) (car #{y\ 4616}#))
                         (#{same-marks?\ 3816}#
                           (cdr #{x\ 4615}#)
                           (cdr #{y\ 4616}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 3815}#
             (lambda (#{m1\ 4618}# #{m2\ 4619}#)
               (#{smart-append\ 3813}#
                 #{m1\ 4618}#
                 #{m2\ 4619}#)))
           (#{join-wraps\ 3814}#
             (lambda (#{w1\ 4620}# #{w2\ 4621}#)
               (let ((#{m1\ 4622}#
                       (#{wrap-marks\ 3798}# #{w1\ 4620}#))
                     (#{s1\ 4623}#
                       (#{wrap-subst\ 3799}# #{w1\ 4620}#)))
                 (if (null? #{m1\ 4622}#)
                   (if (null? #{s1\ 4623}#)
                     #{w2\ 4621}#
                     (#{make-wrap\ 3797}#
                       (#{wrap-marks\ 3798}# #{w2\ 4621}#)
                       (#{smart-append\ 3813}#
                         #{s1\ 4623}#
                         (#{wrap-subst\ 3799}# #{w2\ 4621}#))))
                   (#{make-wrap\ 3797}#
                     (#{smart-append\ 3813}#
                       #{m1\ 4622}#
                       (#{wrap-marks\ 3798}# #{w2\ 4621}#))
                     (#{smart-append\ 3813}#
                       #{s1\ 4623}#
                       (#{wrap-subst\ 3799}# #{w2\ 4621}#)))))))
           (#{smart-append\ 3813}#
             (lambda (#{m1\ 4624}# #{m2\ 4625}#)
               (if (null? #{m2\ 4625}#)
                 #{m1\ 4624}#
                 (append #{m1\ 4624}# #{m2\ 4625}#))))
           (#{make-binding-wrap\ 3812}#
             (lambda (#{ids\ 4626}# #{labels\ 4627}# #{w\ 4628}#)
               (if (null? #{ids\ 4626}#)
                 #{w\ 4628}#
                 (#{make-wrap\ 3797}#
                   (#{wrap-marks\ 3798}# #{w\ 4628}#)
                   (cons (let ((#{labelvec\ 4629}#
                                 (list->vector #{labels\ 4627}#)))
                           (let ((#{n\ 4630}#
                                   (vector-length #{labelvec\ 4629}#)))
                             (let ((#{symnamevec\ 4631}#
                                     (make-vector #{n\ 4630}#))
                                   (#{marksvec\ 4632}#
                                     (make-vector #{n\ 4630}#)))
                               (begin
                                 (letrec ((#{f\ 4633}#
                                            (lambda (#{ids\ 4634}# #{i\ 4635}#)
                                              (if (not (null? #{ids\ 4634}#))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{id-sym-name&marks\ 3796}#
                                                      (car #{ids\ 4634}#)
                                                      #{w\ 4628}#))
                                                  (lambda (#{symname\ 4636}#
                                                           #{marks\ 4637}#)
                                                    (begin
                                                      (vector-set!
                                                        #{symnamevec\ 4631}#
                                                        #{i\ 4635}#
                                                        #{symname\ 4636}#)
                                                      (vector-set!
                                                        #{marksvec\ 4632}#
                                                        #{i\ 4635}#
                                                        #{marks\ 4637}#)
                                                      (#{f\ 4633}#
                                                        (cdr #{ids\ 4634}#)
                                                        (#{fx+\ 3750}#
                                                          #{i\ 4635}#
                                                          1)))))))))
                                   (#{f\ 4633}# #{ids\ 4626}# 0))
                                 (#{make-ribcage\ 3802}#
                                   #{symnamevec\ 4631}#
                                   #{marksvec\ 4632}#
                                   #{labelvec\ 4629}#)))))
                         (#{wrap-subst\ 3799}# #{w\ 4628}#))))))
           (#{extend-ribcage!\ 3811}#
             (lambda (#{ribcage\ 4638}# #{id\ 4639}# #{label\ 4640}#)
               (begin
                 (#{set-ribcage-symnames!\ 3807}#
                   #{ribcage\ 4638}#
                   (cons (#{syntax-object-expression\ 3780}# #{id\ 4639}#)
                         (#{ribcage-symnames\ 3804}# #{ribcage\ 4638}#)))
                 (#{set-ribcage-marks!\ 3808}#
                   #{ribcage\ 4638}#
                   (cons (#{wrap-marks\ 3798}#
                           (#{syntax-object-wrap\ 3781}# #{id\ 4639}#))
                         (#{ribcage-marks\ 3805}# #{ribcage\ 4638}#)))
                 (#{set-ribcage-labels!\ 3809}#
                   #{ribcage\ 4638}#
                   (cons #{label\ 4640}#
                         (#{ribcage-labels\ 3806}# #{ribcage\ 4638}#))))))
           (#{anti-mark\ 3810}#
             (lambda (#{w\ 4641}#)
               (#{make-wrap\ 3797}#
                 (cons #f (#{wrap-marks\ 3798}# #{w\ 4641}#))
                 (cons 'shift
                       (#{wrap-subst\ 3799}# #{w\ 4641}#)))))
           (#{set-ribcage-labels!\ 3809}#
             (lambda (#{x\ 4642}# #{update\ 4643}#)
               (vector-set! #{x\ 4642}# 3 #{update\ 4643}#)))
           (#{set-ribcage-marks!\ 3808}#
             (lambda (#{x\ 4644}# #{update\ 4645}#)
               (vector-set! #{x\ 4644}# 2 #{update\ 4645}#)))
           (#{set-ribcage-symnames!\ 3807}#
             (lambda (#{x\ 4646}# #{update\ 4647}#)
               (vector-set! #{x\ 4646}# 1 #{update\ 4647}#)))
           (#{ribcage-labels\ 3806}#
             (lambda (#{x\ 4648}#) (vector-ref #{x\ 4648}# 3)))
           (#{ribcage-marks\ 3805}#
             (lambda (#{x\ 4649}#) (vector-ref #{x\ 4649}# 2)))
           (#{ribcage-symnames\ 3804}#
             (lambda (#{x\ 4650}#) (vector-ref #{x\ 4650}# 1)))
           (#{ribcage?\ 3803}#
             (lambda (#{x\ 4651}#)
               (if (vector? #{x\ 4651}#)
                 (if (= (vector-length #{x\ 4651}#) 4)
                   (eq? (vector-ref #{x\ 4651}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 3802}#
             (lambda (#{symnames\ 4652}#
                      #{marks\ 4653}#
                      #{labels\ 4654}#)
               (vector
                 'ribcage
                 #{symnames\ 4652}#
                 #{marks\ 4653}#
                 #{labels\ 4654}#)))
           (#{gen-labels\ 3801}#
             (lambda (#{ls\ 4655}#)
               (if (null? #{ls\ 4655}#)
                 '()
                 (cons (#{gen-label\ 3800}#)
                       (#{gen-labels\ 3801}# (cdr #{ls\ 4655}#))))))
           (#{gen-label\ 3800}# (lambda () (string #\i)))
           (#{wrap-subst\ 3799}# cdr)
           (#{wrap-marks\ 3798}# car)
           (#{make-wrap\ 3797}# cons)
           (#{id-sym-name&marks\ 3796}#
             (lambda (#{x\ 4656}# #{w\ 4657}#)
               (if (#{syntax-object?\ 3779}# #{x\ 4656}#)
                 (values
                   (#{syntax-object-expression\ 3780}# #{x\ 4656}#)
                   (#{join-marks\ 3815}#
                     (#{wrap-marks\ 3798}# #{w\ 4657}#)
                     (#{wrap-marks\ 3798}#
                       (#{syntax-object-wrap\ 3781}# #{x\ 4656}#))))
                 (values
                   #{x\ 4656}#
                   (#{wrap-marks\ 3798}# #{w\ 4657}#)))))
           (#{id?\ 3795}#
             (lambda (#{x\ 4658}#)
               (if (symbol? #{x\ 4658}#)
                 #t
                 (if (#{syntax-object?\ 3779}# #{x\ 4658}#)
                   (symbol?
                     (#{syntax-object-expression\ 3780}# #{x\ 4658}#))
                   #f))))
           (#{nonsymbol-id?\ 3794}#
             (lambda (#{x\ 4659}#)
               (if (#{syntax-object?\ 3779}# #{x\ 4659}#)
                 (symbol?
                   (#{syntax-object-expression\ 3780}# #{x\ 4659}#))
                 #f)))
           (#{global-extend\ 3793}#
             (lambda (#{type\ 4660}# #{sym\ 4661}# #{val\ 4662}#)
               (#{put-global-definition-hook\ 3756}#
                 #{sym\ 4661}#
                 #{type\ 4660}#
                 #{val\ 4662}#)))
           (#{lookup\ 3792}#
             (lambda (#{x\ 4663}# #{r\ 4664}# #{mod\ 4665}#)
               (let ((#{t\ 4666}# (assq #{x\ 4663}# #{r\ 4664}#)))
                 (if #{t\ 4666}#
                   (cdr #{t\ 4666}#)
                   (if (symbol? #{x\ 4663}#)
                     (let ((#{t\ 4667}#
                             (#{get-global-definition-hook\ 3757}#
                               #{x\ 4663}#
                               #{mod\ 4665}#)))
                       (if #{t\ 4667}# #{t\ 4667}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 3791}#
             (lambda (#{r\ 4668}#)
               (if (null? #{r\ 4668}#)
                 '()
                 (let ((#{a\ 4669}# (car #{r\ 4668}#)))
                   (if (eq? (cadr #{a\ 4669}#) (quote macro))
                     (cons #{a\ 4669}#
                           (#{macros-only-env\ 3791}# (cdr #{r\ 4668}#)))
                     (#{macros-only-env\ 3791}# (cdr #{r\ 4668}#)))))))
           (#{extend-var-env\ 3790}#
             (lambda (#{labels\ 4670}# #{vars\ 4671}# #{r\ 4672}#)
               (if (null? #{labels\ 4670}#)
                 #{r\ 4672}#
                 (#{extend-var-env\ 3790}#
                   (cdr #{labels\ 4670}#)
                   (cdr #{vars\ 4671}#)
                   (cons (cons (car #{labels\ 4670}#)
                               (cons (quote lexical) (car #{vars\ 4671}#)))
                         #{r\ 4672}#)))))
           (#{extend-env\ 3789}#
             (lambda (#{labels\ 4673}# #{bindings\ 4674}# #{r\ 4675}#)
               (if (null? #{labels\ 4673}#)
                 #{r\ 4675}#
                 (#{extend-env\ 3789}#
                   (cdr #{labels\ 4673}#)
                   (cdr #{bindings\ 4674}#)
                   (cons (cons (car #{labels\ 4673}#)
                               (car #{bindings\ 4674}#))
                         #{r\ 4675}#)))))
           (#{binding-value\ 3788}# cdr)
           (#{binding-type\ 3787}# car)
           (#{source-annotation\ 3786}#
             (lambda (#{x\ 4676}#)
               (if (#{syntax-object?\ 3779}# #{x\ 4676}#)
                 (#{source-annotation\ 3786}#
                   (#{syntax-object-expression\ 3780}# #{x\ 4676}#))
                 (if (pair? #{x\ 4676}#)
                   (let ((#{props\ 4677}# (source-properties #{x\ 4676}#)))
                     (if (pair? #{props\ 4677}#) #{props\ 4677}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 3785}#
             (lambda (#{x\ 4678}# #{update\ 4679}#)
               (vector-set! #{x\ 4678}# 3 #{update\ 4679}#)))
           (#{set-syntax-object-wrap!\ 3784}#
             (lambda (#{x\ 4680}# #{update\ 4681}#)
               (vector-set! #{x\ 4680}# 2 #{update\ 4681}#)))
           (#{set-syntax-object-expression!\ 3783}#
             (lambda (#{x\ 4682}# #{update\ 4683}#)
               (vector-set! #{x\ 4682}# 1 #{update\ 4683}#)))
           (#{syntax-object-module\ 3782}#
             (lambda (#{x\ 4684}#) (vector-ref #{x\ 4684}# 3)))
           (#{syntax-object-wrap\ 3781}#
             (lambda (#{x\ 4685}#) (vector-ref #{x\ 4685}# 2)))
           (#{syntax-object-expression\ 3780}#
             (lambda (#{x\ 4686}#) (vector-ref #{x\ 4686}# 1)))
           (#{syntax-object?\ 3779}#
             (lambda (#{x\ 4687}#)
               (if (vector? #{x\ 4687}#)
                 (if (= (vector-length #{x\ 4687}#) 4)
                   (eq? (vector-ref #{x\ 4687}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 3778}#
             (lambda (#{expression\ 4688}#
                      #{wrap\ 4689}#
                      #{module\ 4690}#)
               (vector
                 'syntax-object
                 #{expression\ 4688}#
                 #{wrap\ 4689}#
                 #{module\ 4690}#)))
           (#{build-letrec\ 3777}#
             (lambda (#{src\ 4691}#
                      #{ids\ 4692}#
                      #{vars\ 4693}#
                      #{val-exps\ 4694}#
                      #{body-exp\ 4695}#)
               (if (null? #{vars\ 4693}#)
                 #{body-exp\ 4695}#
                 (let ((#{atom-key\ 4696}# (fluid-ref #{*mode*\ 3749}#)))
                   (if (memv #{atom-key\ 4696}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 3767}#
                         #{ids\ 4692}#
                         #{val-exps\ 4694}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 4691}#
                        #{ids\ 4692}#
                        #{vars\ 4693}#
                        #{val-exps\ 4694}#
                        #{body-exp\ 4695}#))
                     (#{decorate-source\ 3758}#
                       (list 'letrec
                             (map list #{vars\ 4693}# #{val-exps\ 4694}#)
                             #{body-exp\ 4695}#)
                       #{src\ 4691}#))))))
           (#{build-named-let\ 3776}#
             (lambda (#{src\ 4697}#
                      #{ids\ 4698}#
                      #{vars\ 4699}#
                      #{val-exps\ 4700}#
                      #{body-exp\ 4701}#)
               (let ((#{f\ 4702}# (car #{vars\ 4699}#))
                     (#{f-name\ 4703}# (car #{ids\ 4698}#))
                     (#{vars\ 4704}# (cdr #{vars\ 4699}#))
                     (#{ids\ 4705}# (cdr #{ids\ 4698}#)))
                 (let ((#{atom-key\ 4706}# (fluid-ref #{*mode*\ 3749}#)))
                   (if (memv #{atom-key\ 4706}# (quote (c)))
                     (let ((#{proc\ 4707}#
                             (#{build-simple-lambda\ 3769}#
                               #{src\ 4697}#
                               #{ids\ 4705}#
                               #f
                               #{vars\ 4704}#
                               #f
                               #{body-exp\ 4701}#)))
                       (begin
                         (#{maybe-name-value!\ 3767}#
                           #{f-name\ 4703}#
                           #{proc\ 4707}#)
                         (for-each
                           #{maybe-name-value!\ 3767}#
                           #{ids\ 4705}#
                           #{val-exps\ 4700}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 4697}#
                          (list #{f-name\ 4703}#)
                          (list #{f\ 4702}#)
                          (list #{proc\ 4707}#)
                          (#{build-application\ 3760}#
                            #{src\ 4697}#
                            (#{build-lexical-reference\ 3762}#
                              'fun
                              #{src\ 4697}#
                              #{f-name\ 4703}#
                              #{f\ 4702}#)
                            #{val-exps\ 4700}#))))
                     (#{decorate-source\ 3758}#
                       (list 'letrec
                             (list (list #{f\ 4702}#
                                         (list 'lambda
                                               #{vars\ 4704}#
                                               #{body-exp\ 4701}#)))
                             (cons #{f\ 4702}# #{val-exps\ 4700}#))
                       #{src\ 4697}#))))))
           (#{build-let\ 3775}#
             (lambda (#{src\ 4708}#
                      #{ids\ 4709}#
                      #{vars\ 4710}#
                      #{val-exps\ 4711}#
                      #{body-exp\ 4712}#)
               (if (null? #{vars\ 4710}#)
                 #{body-exp\ 4712}#
                 (let ((#{atom-key\ 4713}# (fluid-ref #{*mode*\ 3749}#)))
                   (if (memv #{atom-key\ 4713}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 3767}#
                         #{ids\ 4709}#
                         #{val-exps\ 4711}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 4708}#
                        #{ids\ 4709}#
                        #{vars\ 4710}#
                        #{val-exps\ 4711}#
                        #{body-exp\ 4712}#))
                     (#{decorate-source\ 3758}#
                       (list 'let
                             (map list #{vars\ 4710}# #{val-exps\ 4711}#)
                             #{body-exp\ 4712}#)
                       #{src\ 4708}#))))))
           (#{build-sequence\ 3774}#
             (lambda (#{src\ 4714}# #{exps\ 4715}#)
               (if (null? (cdr #{exps\ 4715}#))
                 (car #{exps\ 4715}#)
                 (let ((#{atom-key\ 4716}# (fluid-ref #{*mode*\ 3749}#)))
                   (if (memv #{atom-key\ 4716}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 4714}#
                      #{exps\ 4715}#)
                     (#{decorate-source\ 3758}#
                       (cons (quote begin) #{exps\ 4715}#)
                       #{src\ 4714}#))))))
           (#{build-data\ 3773}#
             (lambda (#{src\ 4717}# #{exp\ 4718}#)
               (let ((#{atom-key\ 4719}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4719}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 4717}#
                    #{exp\ 4718}#)
                   (#{decorate-source\ 3758}#
                     (if (if (self-evaluating? #{exp\ 4718}#)
                           (not (vector? #{exp\ 4718}#))
                           #f)
                       #{exp\ 4718}#
                       (list (quote quote) #{exp\ 4718}#))
                     #{src\ 4717}#)))))
           (#{build-primref\ 3772}#
             (lambda (#{src\ 4720}# #{name\ 4721}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 4722}# (fluid-ref #{*mode*\ 3749}#)))
                   (if (memv #{atom-key\ 4722}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 4720}#
                      #{name\ 4721}#)
                     (#{decorate-source\ 3758}#
                       #{name\ 4721}#
                       #{src\ 4720}#)))
                 (let ((#{atom-key\ 4723}# (fluid-ref #{*mode*\ 3749}#)))
                   (if (memv #{atom-key\ 4723}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 4720}#
                      '(guile)
                      #{name\ 4721}#
                      #f)
                     (#{decorate-source\ 3758}#
                       (list (quote @@) (quote (guile)) #{name\ 4721}#)
                       #{src\ 4720}#))))))
           (#{build-lambda-case\ 3771}#
             (lambda (#{src\ 4724}#
                      #{req\ 4725}#
                      #{opt\ 4726}#
                      #{rest\ 4727}#
                      #{kw\ 4728}#
                      #{inits\ 4729}#
                      #{vars\ 4730}#
                      #{body\ 4731}#
                      #{else-case\ 4732}#)
               (let ((#{atom-key\ 4733}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4733}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 4724}#
                    #{req\ 4725}#
                    #{opt\ 4726}#
                    #{rest\ 4727}#
                    #{kw\ 4728}#
                    #{inits\ 4729}#
                    #{vars\ 4730}#
                    #{body\ 4731}#
                    #{else-case\ 4732}#)
                   (let ((#{nreq\ 4734}# (length #{req\ 4725}#)))
                     (let ((#{nopt\ 4735}#
                             (if #{opt\ 4726}# (length #{opt\ 4726}#) 0)))
                       (let ((#{rest-idx\ 4736}#
                               (if #{rest\ 4727}#
                                 (+ #{nreq\ 4734}# #{nopt\ 4735}#)
                                 #f)))
                         (let ((#{allow-other-keys?\ 4737}#
                                 (if #{kw\ 4728}# (car #{kw\ 4728}#) #f)))
                           (let ((#{kw-indices\ 4738}#
                                   (map (lambda (#{x\ 4739}#)
                                          (cons (car #{x\ 4739}#)
                                                (list-index
                                                  #{vars\ 4730}#
                                                  (caddr #{x\ 4739}#))))
                                        (if #{kw\ 4728}#
                                          (cdr #{kw\ 4728}#)
                                          '()))))
                             (let ((#{nargs\ 4740}#
                                     (apply max
                                            (+ #{nreq\ 4734}#
                                               #{nopt\ 4735}#
                                               (if #{rest\ 4727}# 1 0))
                                            (map 1+
                                                 (map cdr
                                                      #{kw-indices\ 4738}#)))))
                               (begin
                                 (let ((#{t\ 4741}#
                                         (= #{nargs\ 4740}#
                                            (length #{vars\ 4730}#)
                                            (+ #{nreq\ 4734}#
                                               (length #{inits\ 4729}#)
                                               (if #{rest\ 4727}# 1 0)))))
                                   (if #{t\ 4741}#
                                     #{t\ 4741}#
                                     (error "something went wrong"
                                            #{req\ 4725}#
                                            #{opt\ 4726}#
                                            #{rest\ 4727}#
                                            #{kw\ 4728}#
                                            #{inits\ 4729}#
                                            #{vars\ 4730}#
                                            #{nreq\ 4734}#
                                            #{nopt\ 4735}#
                                            #{kw-indices\ 4738}#
                                            #{nargs\ 4740}#)))
                                 (#{decorate-source\ 3758}#
                                   (cons (list (cons '(@@ (ice-9 optargs)
                                                          parse-lambda-case)
                                                     (cons (list 'quote
                                                                 (list #{nreq\ 4734}#
                                                                       #{nopt\ 4735}#
                                                                       #{rest-idx\ 4736}#
                                                                       #{nargs\ 4740}#
                                                                       #{allow-other-keys?\ 4737}#
                                                                       #{kw-indices\ 4738}#))
                                                           (cons (cons 'list
                                                                       (map (lambda (#{i\ 4742}#)
                                                                              (list 'lambda
                                                                                    #{vars\ 4730}#
                                                                                    #{i\ 4742}#))
                                                                            #{inits\ 4729}#))
                                                                 '(%%args))))
                                               '=>
                                               (list 'lambda
                                                     '(%%%args . _)
                                                     (cons 'apply
                                                           (cons (list 'lambda
                                                                       #{vars\ 4730}#
                                                                       #{body\ 4731}#)
                                                                 '(%%%args)))))
                                         (let ((#{t\ 4743}#
                                                 #{else-case\ 4732}#))
                                           (if #{t\ 4743}#
                                             #{t\ 4743}#
                                             '((%%args
                                                 (error "wrong number of arguments"
                                                        %%args))))))
                                   #{src\ 4724}#))))))))))))
           (#{build-case-lambda\ 3770}#
             (lambda (#{src\ 4744}#
                      #{docstring\ 4745}#
                      #{body\ 4746}#)
               (let ((#{atom-key\ 4747}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4747}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 4744}#
                    (if #{docstring\ 4745}#
                      (list (cons (quote documentation) #{docstring\ 4745}#))
                      '())
                    #{body\ 4746}#)
                   (#{decorate-source\ 3758}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 4745}#
                                     (list #{docstring\ 4745}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 4746}#)))))
                     #{src\ 4744}#)))))
           (#{build-simple-lambda\ 3769}#
             (lambda (#{src\ 4748}#
                      #{req\ 4749}#
                      #{rest\ 4750}#
                      #{vars\ 4751}#
                      #{docstring\ 4752}#
                      #{exp\ 4753}#)
               (let ((#{atom-key\ 4754}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4754}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 4748}#
                    (if #{docstring\ 4752}#
                      (list (cons (quote documentation) #{docstring\ 4752}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 4748}#
                     #{req\ 4749}#
                     #f
                     #{rest\ 4750}#
                     #f
                     '()
                     #{vars\ 4751}#
                     #{exp\ 4753}#
                     #f))
                   (#{decorate-source\ 3758}#
                     (cons 'lambda
                           (cons (if #{rest\ 4750}#
                                   (apply cons* #{vars\ 4751}#)
                                   #{vars\ 4751}#)
                                 (append
                                   (if #{docstring\ 4752}#
                                     (list #{docstring\ 4752}#)
                                     '())
                                   (list #{exp\ 4753}#))))
                     #{src\ 4748}#)))))
           (#{build-global-definition\ 3768}#
             (lambda (#{source\ 4755}# #{var\ 4756}# #{exp\ 4757}#)
               (let ((#{atom-key\ 4758}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4758}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 3767}#
                       #{var\ 4756}#
                       #{exp\ 4757}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 4755}#
                      #{var\ 4756}#
                      #{exp\ 4757}#))
                   (#{decorate-source\ 3758}#
                     (list (quote define) #{var\ 4756}# #{exp\ 4757}#)
                     #{source\ 4755}#)))))
           (#{maybe-name-value!\ 3767}#
             (lambda (#{name\ 4759}# #{val\ 4760}#)
               (if ((@ (language tree-il) lambda?) #{val\ 4760}#)
                 (let ((#{meta\ 4761}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 4760}#)))
                   (if (not (assq (quote name) #{meta\ 4761}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 4760}#
                      (acons 'name
                             #{name\ 4759}#
                             #{meta\ 4761}#)))))))
           (#{build-global-assignment\ 3766}#
             (lambda (#{source\ 4762}#
                      #{var\ 4763}#
                      #{exp\ 4764}#
                      #{mod\ 4765}#)
               (#{analyze-variable\ 3764}#
                 #{mod\ 4765}#
                 #{var\ 4763}#
                 (lambda (#{mod\ 4766}# #{var\ 4767}# #{public?\ 4768}#)
                   (let ((#{atom-key\ 4769}# (fluid-ref #{*mode*\ 3749}#)))
                     (if (memv #{atom-key\ 4769}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 4762}#
                        #{mod\ 4766}#
                        #{var\ 4767}#
                        #{public?\ 4768}#
                        #{exp\ 4764}#)
                       (#{decorate-source\ 3758}#
                         (list 'set!
                               (list (if #{public?\ 4768}#
                                       '@
                                       '@@)
                                     #{mod\ 4766}#
                                     #{var\ 4767}#)
                               #{exp\ 4764}#)
                         #{source\ 4762}#))))
                 (lambda (#{var\ 4770}#)
                   (let ((#{atom-key\ 4771}# (fluid-ref #{*mode*\ 3749}#)))
                     (if (memv #{atom-key\ 4771}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 4762}#
                        #{var\ 4770}#
                        #{exp\ 4764}#)
                       (#{decorate-source\ 3758}#
                         (list (quote set!) #{var\ 4770}# #{exp\ 4764}#)
                         #{source\ 4762}#)))))))
           (#{build-global-reference\ 3765}#
             (lambda (#{source\ 4772}# #{var\ 4773}# #{mod\ 4774}#)
               (#{analyze-variable\ 3764}#
                 #{mod\ 4774}#
                 #{var\ 4773}#
                 (lambda (#{mod\ 4775}# #{var\ 4776}# #{public?\ 4777}#)
                   (let ((#{atom-key\ 4778}# (fluid-ref #{*mode*\ 3749}#)))
                     (if (memv #{atom-key\ 4778}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 4772}#
                        #{mod\ 4775}#
                        #{var\ 4776}#
                        #{public?\ 4777}#)
                       (#{decorate-source\ 3758}#
                         (list (if #{public?\ 4777}# (quote @) (quote @@))
                               #{mod\ 4775}#
                               #{var\ 4776}#)
                         #{source\ 4772}#))))
                 (lambda (#{var\ 4779}#)
                   (let ((#{atom-key\ 4780}# (fluid-ref #{*mode*\ 3749}#)))
                     (if (memv #{atom-key\ 4780}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 4772}#
                        #{var\ 4779}#)
                       (#{decorate-source\ 3758}#
                         #{var\ 4779}#
                         #{source\ 4772}#)))))))
           (#{analyze-variable\ 3764}#
             (lambda (#{mod\ 4781}#
                      #{var\ 4782}#
                      #{modref-cont\ 4783}#
                      #{bare-cont\ 4784}#)
               (if (not #{mod\ 4781}#)
                 (#{bare-cont\ 4784}# #{var\ 4782}#)
                 (let ((#{kind\ 4785}# (car #{mod\ 4781}#))
                       (#{mod\ 4786}# (cdr #{mod\ 4781}#)))
                   (if (memv #{kind\ 4785}# (quote (public)))
                     (#{modref-cont\ 4783}#
                       #{mod\ 4786}#
                       #{var\ 4782}#
                       #t)
                     (if (memv #{kind\ 4785}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 4786}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 4783}#
                           #{mod\ 4786}#
                           #{var\ 4782}#
                           #f)
                         (#{bare-cont\ 4784}# #{var\ 4782}#))
                       (if (memv #{kind\ 4785}# (quote (bare)))
                         (#{bare-cont\ 4784}# #{var\ 4782}#)
                         (if (memv #{kind\ 4785}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 4786}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 4786}#)
                                   #{var\ 4782}#)
                                 #f)
                             (#{modref-cont\ 4783}#
                               #{mod\ 4786}#
                               #{var\ 4782}#
                               #f)
                             (#{bare-cont\ 4784}# #{var\ 4782}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 4782}#
                             #{mod\ 4786}#)))))))))
           (#{build-lexical-assignment\ 3763}#
             (lambda (#{source\ 4787}#
                      #{name\ 4788}#
                      #{var\ 4789}#
                      #{exp\ 4790}#)
               (let ((#{atom-key\ 4791}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4791}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 4787}#
                    #{name\ 4788}#
                    #{var\ 4789}#
                    #{exp\ 4790}#)
                   (#{decorate-source\ 3758}#
                     (list (quote set!) #{var\ 4789}# #{exp\ 4790}#)
                     #{source\ 4787}#)))))
           (#{build-lexical-reference\ 3762}#
             (lambda (#{type\ 4792}#
                      #{source\ 4793}#
                      #{name\ 4794}#
                      #{var\ 4795}#)
               (let ((#{atom-key\ 4796}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4796}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 4793}#
                    #{name\ 4794}#
                    #{var\ 4795}#)
                   (#{decorate-source\ 3758}#
                     #{var\ 4795}#
                     #{source\ 4793}#)))))
           (#{build-conditional\ 3761}#
             (lambda (#{source\ 4797}#
                      #{test-exp\ 4798}#
                      #{then-exp\ 4799}#
                      #{else-exp\ 4800}#)
               (let ((#{atom-key\ 4801}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4801}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 4797}#
                    #{test-exp\ 4798}#
                    #{then-exp\ 4799}#
                    #{else-exp\ 4800}#)
                   (#{decorate-source\ 3758}#
                     (if (equal? #{else-exp\ 4800}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 4798}#
                             #{then-exp\ 4799}#)
                       (list 'if
                             #{test-exp\ 4798}#
                             #{then-exp\ 4799}#
                             #{else-exp\ 4800}#))
                     #{source\ 4797}#)))))
           (#{build-application\ 3760}#
             (lambda (#{source\ 4802}#
                      #{fun-exp\ 4803}#
                      #{arg-exps\ 4804}#)
               (let ((#{atom-key\ 4805}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4805}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 4802}#
                    #{fun-exp\ 4803}#
                    #{arg-exps\ 4804}#)
                   (#{decorate-source\ 3758}#
                     (cons #{fun-exp\ 4803}# #{arg-exps\ 4804}#)
                     #{source\ 4802}#)))))
           (#{build-void\ 3759}#
             (lambda (#{source\ 4806}#)
               (let ((#{atom-key\ 4807}# (fluid-ref #{*mode*\ 3749}#)))
                 (if (memv #{atom-key\ 4807}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 4806}#)
                   (#{decorate-source\ 3758}#
                     '(if #f #f)
                     #{source\ 4806}#)))))
           (#{decorate-source\ 3758}#
             (lambda (#{e\ 4808}# #{s\ 4809}#)
               (begin
                 (if (if (pair? #{e\ 4808}#) #{s\ 4809}# #f)
                   (set-source-properties! #{e\ 4808}# #{s\ 4809}#))
                 #{e\ 4808}#)))
           (#{get-global-definition-hook\ 3757}#
             (lambda (#{symbol\ 4810}# #{module\ 4811}#)
               (begin
                 (if (if (not #{module\ 4811}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 4810}#))
                 (let ((#{v\ 4812}#
                         (module-variable
                           (if #{module\ 4811}#
                             (resolve-module (cdr #{module\ 4811}#))
                             (current-module))
                           #{symbol\ 4810}#)))
                   (if #{v\ 4812}#
                     (if (variable-bound? #{v\ 4812}#)
                       (let ((#{val\ 4813}# (variable-ref #{v\ 4812}#)))
                         (if (macro? #{val\ 4813}#)
                           (if (syncase-macro-type #{val\ 4813}#)
                             (cons (syncase-macro-type #{val\ 4813}#)
                                   (syncase-macro-binding #{val\ 4813}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 3756}#
             (lambda (#{symbol\ 4814}# #{type\ 4815}# #{val\ 4816}#)
               (let ((#{existing\ 4817}#
                       (let ((#{v\ 4818}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 4814}#)))
                         (if #{v\ 4818}#
                           (if (variable-bound? #{v\ 4818}#)
                             (let ((#{val\ 4819}# (variable-ref #{v\ 4818}#)))
                               (if (macro? #{val\ 4819}#)
                                 (if (not (syncase-macro-type #{val\ 4819}#))
                                   #{val\ 4819}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 4814}#
                   (if #{existing\ 4817}#
                     (make-extended-syncase-macro
                       #{existing\ 4817}#
                       #{type\ 4815}#
                       #{val\ 4816}#)
                     (make-syncase-macro #{type\ 4815}# #{val\ 4816}#))))))
           (#{local-eval-hook\ 3755}#
             (lambda (#{x\ 4820}# #{mod\ 4821}#)
               (primitive-eval
                 (list #{noexpand\ 3748}#
                       (let ((#{atom-key\ 4822}# (fluid-ref #{*mode*\ 3749}#)))
                         (if (memv #{atom-key\ 4822}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 4820}#)
                           #{x\ 4820}#))))))
           (#{top-level-eval-hook\ 3754}#
             (lambda (#{x\ 4823}# #{mod\ 4824}#)
               (primitive-eval
                 (list #{noexpand\ 3748}#
                       (let ((#{atom-key\ 4825}# (fluid-ref #{*mode*\ 3749}#)))
                         (if (memv #{atom-key\ 4825}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 4823}#)
                           #{x\ 4823}#))))))
           (#{fx<\ 3753}# <)
           (#{fx=\ 3752}# =)
           (#{fx-\ 3751}# -)
           (#{fx+\ 3750}# +)
           (#{*mode*\ 3749}# (make-fluid))
           (#{noexpand\ 3748}# "noexpand"))
    (begin
      (#{global-extend\ 3793}#
        'local-syntax
        'letrec-syntax
        #t)
      (#{global-extend\ 3793}#
        'local-syntax
        'let-syntax
        #f)
      (#{global-extend\ 3793}#
        'core
        'fluid-let-syntax
        (lambda (#{e\ 4826}#
                 #{r\ 4827}#
                 #{w\ 4828}#
                 #{s\ 4829}#
                 #{mod\ 4830}#)
          ((lambda (#{tmp\ 4831}#)
             ((lambda (#{tmp\ 4832}#)
                (if (if #{tmp\ 4832}#
                      (apply (lambda (#{_\ 4833}#
                                      #{var\ 4834}#
                                      #{val\ 4835}#
                                      #{e1\ 4836}#
                                      #{e2\ 4837}#)
                               (#{valid-bound-ids?\ 3820}# #{var\ 4834}#))
                             #{tmp\ 4832}#)
                      #f)
                  (apply (lambda (#{_\ 4839}#
                                  #{var\ 4840}#
                                  #{val\ 4841}#
                                  #{e1\ 4842}#
                                  #{e2\ 4843}#)
                           (let ((#{names\ 4844}#
                                   (map (lambda (#{x\ 4845}#)
                                          (#{id-var-name\ 3817}#
                                            #{x\ 4845}#
                                            #{w\ 4828}#))
                                        #{var\ 4840}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 4847}# #{n\ 4848}#)
                                   (let ((#{atom-key\ 4849}#
                                           (#{binding-type\ 3787}#
                                             (#{lookup\ 3792}#
                                               #{n\ 4848}#
                                               #{r\ 4827}#
                                               #{mod\ 4830}#))))
                                     (if (memv #{atom-key\ 4849}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 4826}#
                                         (#{source-wrap\ 3824}#
                                           #{id\ 4847}#
                                           #{w\ 4828}#
                                           #{s\ 4829}#
                                           #{mod\ 4830}#)))))
                                 #{var\ 4840}#
                                 #{names\ 4844}#)
                               (#{chi-body\ 3835}#
                                 (cons #{e1\ 4842}# #{e2\ 4843}#)
                                 (#{source-wrap\ 3824}#
                                   #{e\ 4826}#
                                   #{w\ 4828}#
                                   #{s\ 4829}#
                                   #{mod\ 4830}#)
                                 (#{extend-env\ 3789}#
                                   #{names\ 4844}#
                                   (let ((#{trans-r\ 4852}#
                                           (#{macros-only-env\ 3791}#
                                             #{r\ 4827}#)))
                                     (map (lambda (#{x\ 4853}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 3837}#
                                                    (#{chi\ 3831}#
                                                      #{x\ 4853}#
                                                      #{trans-r\ 4852}#
                                                      #{w\ 4828}#
                                                      #{mod\ 4830}#)
                                                    #{mod\ 4830}#)))
                                          #{val\ 4841}#))
                                   #{r\ 4827}#)
                                 #{w\ 4828}#
                                 #{mod\ 4830}#))))
                         #{tmp\ 4832}#)
                  ((lambda (#{_\ 4855}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 3824}#
                         #{e\ 4826}#
                         #{w\ 4828}#
                         #{s\ 4829}#
                         #{mod\ 4830}#)))
                   #{tmp\ 4831}#)))
              ($sc-dispatch
                #{tmp\ 4831}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 4826}#)))
      (#{global-extend\ 3793}#
        'core
        'quote
        (lambda (#{e\ 4856}#
                 #{r\ 4857}#
                 #{w\ 4858}#
                 #{s\ 4859}#
                 #{mod\ 4860}#)
          ((lambda (#{tmp\ 4861}#)
             ((lambda (#{tmp\ 4862}#)
                (if #{tmp\ 4862}#
                  (apply (lambda (#{_\ 4863}# #{e\ 4864}#)
                           (#{build-data\ 3773}#
                             #{s\ 4859}#
                             (#{strip\ 3844}# #{e\ 4864}# #{w\ 4858}#)))
                         #{tmp\ 4862}#)
                  ((lambda (#{_\ 4865}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 3824}#
                         #{e\ 4856}#
                         #{w\ 4858}#
                         #{s\ 4859}#
                         #{mod\ 4860}#)))
                   #{tmp\ 4861}#)))
              ($sc-dispatch #{tmp\ 4861}# (quote (any any)))))
           #{e\ 4856}#)))
      (#{global-extend\ 3793}#
        'core
        'syntax
        (letrec ((#{regen\ 4873}#
                   (lambda (#{x\ 4874}#)
                     (let ((#{atom-key\ 4875}# (car #{x\ 4874}#)))
                       (if (memv #{atom-key\ 4875}# (quote (ref)))
                         (#{build-lexical-reference\ 3762}#
                           'value
                           #f
                           (cadr #{x\ 4874}#)
                           (cadr #{x\ 4874}#))
                         (if (memv #{atom-key\ 4875}# (quote (primitive)))
                           (#{build-primref\ 3772}# #f (cadr #{x\ 4874}#))
                           (if (memv #{atom-key\ 4875}# (quote (quote)))
                             (#{build-data\ 3773}# #f (cadr #{x\ 4874}#))
                             (if (memv #{atom-key\ 4875}# (quote (lambda)))
                               (if (list? (cadr #{x\ 4874}#))
                                 (#{build-simple-lambda\ 3769}#
                                   #f
                                   (cadr #{x\ 4874}#)
                                   #f
                                   (cadr #{x\ 4874}#)
                                   #f
                                   (#{regen\ 4873}# (caddr #{x\ 4874}#)))
                                 (error "how did we get here" #{x\ 4874}#))
                               (#{build-application\ 3760}#
                                 #f
                                 (#{build-primref\ 3772}# #f (car #{x\ 4874}#))
                                 (map #{regen\ 4873}#
                                      (cdr #{x\ 4874}#))))))))))
                 (#{gen-vector\ 4872}#
                   (lambda (#{x\ 4876}#)
                     (if (eq? (car #{x\ 4876}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 4876}#))
                       (if (eq? (car #{x\ 4876}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 4876}#)))
                         (list (quote list->vector) #{x\ 4876}#)))))
                 (#{gen-append\ 4871}#
                   (lambda (#{x\ 4877}# #{y\ 4878}#)
                     (if (equal? #{y\ 4878}# (quote (quote ())))
                       #{x\ 4877}#
                       (list (quote append) #{x\ 4877}# #{y\ 4878}#))))
                 (#{gen-cons\ 4870}#
                   (lambda (#{x\ 4879}# #{y\ 4880}#)
                     (let ((#{atom-key\ 4881}# (car #{y\ 4880}#)))
                       (if (memv #{atom-key\ 4881}# (quote (quote)))
                         (if (eq? (car #{x\ 4879}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 4879}#) (cadr #{y\ 4880}#)))
                           (if (eq? (cadr #{y\ 4880}#) (quote ()))
                             (list (quote list) #{x\ 4879}#)
                             (list (quote cons) #{x\ 4879}# #{y\ 4880}#)))
                         (if (memv #{atom-key\ 4881}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 4879}# (cdr #{y\ 4880}#)))
                           (list (quote cons) #{x\ 4879}# #{y\ 4880}#))))))
                 (#{gen-map\ 4869}#
                   (lambda (#{e\ 4882}# #{map-env\ 4883}#)
                     (let ((#{formals\ 4884}# (map cdr #{map-env\ 4883}#))
                           (#{actuals\ 4885}#
                             (map (lambda (#{x\ 4886}#)
                                    (list (quote ref) (car #{x\ 4886}#)))
                                  #{map-env\ 4883}#)))
                       (if (eq? (car #{e\ 4882}#) (quote ref))
                         (car #{actuals\ 4885}#)
                         (if (and-map
                               (lambda (#{x\ 4887}#)
                                 (if (eq? (car #{x\ 4887}#) (quote ref))
                                   (memq (cadr #{x\ 4887}#) #{formals\ 4884}#)
                                   #f))
                               (cdr #{e\ 4882}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 4882}#))
                                       (map (let ((#{r\ 4888}#
                                                    (map cons
                                                         #{formals\ 4884}#
                                                         #{actuals\ 4885}#)))
                                              (lambda (#{x\ 4889}#)
                                                (cdr (assq (cadr #{x\ 4889}#)
                                                           #{r\ 4888}#))))
                                            (cdr #{e\ 4882}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 4884}#
                                             #{e\ 4882}#)
                                       #{actuals\ 4885}#)))))))
                 (#{gen-mappend\ 4868}#
                   (lambda (#{e\ 4890}# #{map-env\ 4891}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 4869}# #{e\ 4890}# #{map-env\ 4891}#))))
                 (#{gen-ref\ 4867}#
                   (lambda (#{src\ 4892}#
                            #{var\ 4893}#
                            #{level\ 4894}#
                            #{maps\ 4895}#)
                     (if (#{fx=\ 3752}# #{level\ 4894}# 0)
                       (values #{var\ 4893}# #{maps\ 4895}#)
                       (if (null? #{maps\ 4895}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 4892}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 4867}#
                               #{src\ 4892}#
                               #{var\ 4893}#
                               (#{fx-\ 3751}# #{level\ 4894}# 1)
                               (cdr #{maps\ 4895}#)))
                           (lambda (#{outer-var\ 4896}# #{outer-maps\ 4897}#)
                             (let ((#{b\ 4898}#
                                     (assq #{outer-var\ 4896}#
                                           (car #{maps\ 4895}#))))
                               (if #{b\ 4898}#
                                 (values (cdr #{b\ 4898}#) #{maps\ 4895}#)
                                 (let ((#{inner-var\ 4899}#
                                         (#{gen-var\ 3845}# (quote tmp))))
                                   (values
                                     #{inner-var\ 4899}#
                                     (cons (cons (cons #{outer-var\ 4896}#
                                                       #{inner-var\ 4899}#)
                                                 (car #{maps\ 4895}#))
                                           #{outer-maps\ 4897}#)))))))))))
                 (#{gen-syntax\ 4866}#
                   (lambda (#{src\ 4900}#
                            #{e\ 4901}#
                            #{r\ 4902}#
                            #{maps\ 4903}#
                            #{ellipsis?\ 4904}#
                            #{mod\ 4905}#)
                     (if (#{id?\ 3795}# #{e\ 4901}#)
                       (let ((#{label\ 4906}#
                               (#{id-var-name\ 3817}#
                                 #{e\ 4901}#
                                 '(()))))
                         (let ((#{b\ 4907}#
                                 (#{lookup\ 3792}#
                                   #{label\ 4906}#
                                   #{r\ 4902}#
                                   #{mod\ 4905}#)))
                           (if (eq? (#{binding-type\ 3787}# #{b\ 4907}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 4908}#
                                         (#{binding-value\ 3788}#
                                           #{b\ 4907}#)))
                                   (#{gen-ref\ 4867}#
                                     #{src\ 4900}#
                                     (car #{var.lev\ 4908}#)
                                     (cdr #{var.lev\ 4908}#)
                                     #{maps\ 4903}#)))
                               (lambda (#{var\ 4909}# #{maps\ 4910}#)
                                 (values
                                   (list (quote ref) #{var\ 4909}#)
                                   #{maps\ 4910}#)))
                             (if (#{ellipsis?\ 4904}# #{e\ 4901}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 4900}#)
                               (values
                                 (list (quote quote) #{e\ 4901}#)
                                 #{maps\ 4903}#)))))
                       ((lambda (#{tmp\ 4911}#)
                          ((lambda (#{tmp\ 4912}#)
                             (if (if #{tmp\ 4912}#
                                   (apply (lambda (#{dots\ 4913}# #{e\ 4914}#)
                                            (#{ellipsis?\ 4904}#
                                              #{dots\ 4913}#))
                                          #{tmp\ 4912}#)
                                   #f)
                               (apply (lambda (#{dots\ 4915}# #{e\ 4916}#)
                                        (#{gen-syntax\ 4866}#
                                          #{src\ 4900}#
                                          #{e\ 4916}#
                                          #{r\ 4902}#
                                          #{maps\ 4903}#
                                          (lambda (#{x\ 4917}#) #f)
                                          #{mod\ 4905}#))
                                      #{tmp\ 4912}#)
                               ((lambda (#{tmp\ 4918}#)
                                  (if (if #{tmp\ 4918}#
                                        (apply (lambda (#{x\ 4919}#
                                                        #{dots\ 4920}#
                                                        #{y\ 4921}#)
                                                 (#{ellipsis?\ 4904}#
                                                   #{dots\ 4920}#))
                                               #{tmp\ 4918}#)
                                        #f)
                                    (apply (lambda (#{x\ 4922}#
                                                    #{dots\ 4923}#
                                                    #{y\ 4924}#)
                                             (letrec ((#{f\ 4925}#
                                                        (lambda (#{y\ 4926}#
                                                                 #{k\ 4927}#)
                                                          ((lambda (#{tmp\ 4931}#)
                                                             ((lambda (#{tmp\ 4932}#)
                                                                (if (if #{tmp\ 4932}#
                                                                      (apply (lambda (#{dots\ 4933}#
                                                                                      #{y\ 4934}#)
                                                                               (#{ellipsis?\ 4904}#
                                                                                 #{dots\ 4933}#))
                                                                             #{tmp\ 4932}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 4935}#
                                                                                  #{y\ 4936}#)
                                                                           (#{f\ 4925}#
                                                                             #{y\ 4936}#
                                                                             (lambda (#{maps\ 4937}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 4927}#
                                                                                     (cons '()
                                                                                           #{maps\ 4937}#)))
                                                                                 (lambda (#{x\ 4938}#
                                                                                          #{maps\ 4939}#)
                                                                                   (if (null? (car #{maps\ 4939}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 4900}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 4868}#
                                                                                         #{x\ 4938}#
                                                                                         (car #{maps\ 4939}#))
                                                                                       (cdr #{maps\ 4939}#))))))))
                                                                         #{tmp\ 4932}#)
                                                                  ((lambda (#{_\ 4940}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 4866}#
                                                                           #{src\ 4900}#
                                                                           #{y\ 4926}#
                                                                           #{r\ 4902}#
                                                                           #{maps\ 4903}#
                                                                           #{ellipsis?\ 4904}#
                                                                           #{mod\ 4905}#))
                                                                       (lambda (#{y\ 4941}#
                                                                                #{maps\ 4942}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 4927}#
                                                                               #{maps\ 4942}#))
                                                                           (lambda (#{x\ 4943}#
                                                                                    #{maps\ 4944}#)
                                                                             (values
                                                                               (#{gen-append\ 4871}#
                                                                                 #{x\ 4943}#
                                                                                 #{y\ 4941}#)
                                                                               #{maps\ 4944}#))))))
                                                                   #{tmp\ 4931}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 4931}#
                                                                '(any . any))))
                                                           #{y\ 4926}#))))
                                               (#{f\ 4925}#
                                                 #{y\ 4924}#
                                                 (lambda (#{maps\ 4928}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 4866}#
                                                         #{src\ 4900}#
                                                         #{x\ 4922}#
                                                         #{r\ 4902}#
                                                         (cons '()
                                                               #{maps\ 4928}#)
                                                         #{ellipsis?\ 4904}#
                                                         #{mod\ 4905}#))
                                                     (lambda (#{x\ 4929}#
                                                              #{maps\ 4930}#)
                                                       (if (null? (car #{maps\ 4930}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 4900}#)
                                                         (values
                                                           (#{gen-map\ 4869}#
                                                             #{x\ 4929}#
                                                             (car #{maps\ 4930}#))
                                                           (cdr #{maps\ 4930}#)))))))))
                                           #{tmp\ 4918}#)
                                    ((lambda (#{tmp\ 4945}#)
                                       (if #{tmp\ 4945}#
                                         (apply (lambda (#{x\ 4946}#
                                                         #{y\ 4947}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 4866}#
                                                        #{src\ 4900}#
                                                        #{x\ 4946}#
                                                        #{r\ 4902}#
                                                        #{maps\ 4903}#
                                                        #{ellipsis?\ 4904}#
                                                        #{mod\ 4905}#))
                                                    (lambda (#{x\ 4948}#
                                                             #{maps\ 4949}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 4866}#
                                                            #{src\ 4900}#
                                                            #{y\ 4947}#
                                                            #{r\ 4902}#
                                                            #{maps\ 4949}#
                                                            #{ellipsis?\ 4904}#
                                                            #{mod\ 4905}#))
                                                        (lambda (#{y\ 4950}#
                                                                 #{maps\ 4951}#)
                                                          (values
                                                            (#{gen-cons\ 4870}#
                                                              #{x\ 4948}#
                                                              #{y\ 4950}#)
                                                            #{maps\ 4951}#))))))
                                                #{tmp\ 4945}#)
                                         ((lambda (#{tmp\ 4952}#)
                                            (if #{tmp\ 4952}#
                                              (apply (lambda (#{e1\ 4953}#
                                                              #{e2\ 4954}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 4866}#
                                                             #{src\ 4900}#
                                                             (cons #{e1\ 4953}#
                                                                   #{e2\ 4954}#)
                                                             #{r\ 4902}#
                                                             #{maps\ 4903}#
                                                             #{ellipsis?\ 4904}#
                                                             #{mod\ 4905}#))
                                                         (lambda (#{e\ 4956}#
                                                                  #{maps\ 4957}#)
                                                           (values
                                                             (#{gen-vector\ 4872}#
                                                               #{e\ 4956}#)
                                                             #{maps\ 4957}#))))
                                                     #{tmp\ 4952}#)
                                              ((lambda (#{_\ 4958}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 4901}#)
                                                   #{maps\ 4903}#))
                                               #{tmp\ 4911}#)))
                                          ($sc-dispatch
                                            #{tmp\ 4911}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 4911}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 4911}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 4911}# (quote (any any)))))
                        #{e\ 4901}#)))))
          (lambda (#{e\ 4959}#
                   #{r\ 4960}#
                   #{w\ 4961}#
                   #{s\ 4962}#
                   #{mod\ 4963}#)
            (let ((#{e\ 4964}#
                    (#{source-wrap\ 3824}#
                      #{e\ 4959}#
                      #{w\ 4961}#
                      #{s\ 4962}#
                      #{mod\ 4963}#)))
              ((lambda (#{tmp\ 4965}#)
                 ((lambda (#{tmp\ 4966}#)
                    (if #{tmp\ 4966}#
                      (apply (lambda (#{_\ 4967}# #{x\ 4968}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 4866}#
                                     #{e\ 4964}#
                                     #{x\ 4968}#
                                     #{r\ 4960}#
                                     '()
                                     #{ellipsis?\ 3839}#
                                     #{mod\ 4963}#))
                                 (lambda (#{e\ 4969}# #{maps\ 4970}#)
                                   (#{regen\ 4873}# #{e\ 4969}#))))
                             #{tmp\ 4966}#)
                      ((lambda (#{_\ 4971}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 4964}#))
                       #{tmp\ 4965}#)))
                  ($sc-dispatch #{tmp\ 4965}# (quote (any any)))))
               #{e\ 4964}#)))))
      (#{global-extend\ 3793}#
        'core
        'lambda
        (lambda (#{e\ 4972}#
                 #{r\ 4973}#
                 #{w\ 4974}#
                 #{s\ 4975}#
                 #{mod\ 4976}#)
          ((lambda (#{tmp\ 4977}#)
             ((lambda (#{tmp\ 4978}#)
                (if (if #{tmp\ 4978}#
                      (apply (lambda (#{_\ 4979}#
                                      #{args\ 4980}#
                                      #{docstring\ 4981}#
                                      #{e1\ 4982}#
                                      #{e2\ 4983}#)
                               (string? (syntax->datum #{docstring\ 4981}#)))
                             #{tmp\ 4978}#)
                      #f)
                  (apply (lambda (#{_\ 4984}#
                                  #{args\ 4985}#
                                  #{docstring\ 4986}#
                                  #{e1\ 4987}#
                                  #{e2\ 4988}#)
                           (call-with-values
                             (lambda ()
                               (#{lambda-formals\ 3840}# #{args\ 4985}#))
                             (lambda (#{req\ 4989}#
                                      #{opt\ 4990}#
                                      #{rest\ 4991}#
                                      #{kw\ 4992}#)
                               (#{chi-simple-lambda\ 3841}#
                                 #{e\ 4972}#
                                 #{r\ 4973}#
                                 #{w\ 4974}#
                                 #{s\ 4975}#
                                 #{mod\ 4976}#
                                 #{req\ 4989}#
                                 #{rest\ 4991}#
                                 (syntax->datum #{docstring\ 4986}#)
                                 (cons #{e1\ 4987}# #{e2\ 4988}#)))))
                         #{tmp\ 4978}#)
                  ((lambda (#{tmp\ 4994}#)
                     (if #{tmp\ 4994}#
                       (apply (lambda (#{_\ 4995}#
                                       #{args\ 4996}#
                                       #{e1\ 4997}#
                                       #{e2\ 4998}#)
                                (call-with-values
                                  (lambda ()
                                    (#{lambda-formals\ 3840}# #{args\ 4996}#))
                                  (lambda (#{req\ 4999}#
                                           #{opt\ 5000}#
                                           #{rest\ 5001}#
                                           #{kw\ 5002}#)
                                    (#{chi-simple-lambda\ 3841}#
                                      #{e\ 4972}#
                                      #{r\ 4973}#
                                      #{w\ 4974}#
                                      #{s\ 4975}#
                                      #{mod\ 4976}#
                                      #{req\ 4999}#
                                      #{rest\ 5001}#
                                      #f
                                      (cons #{e1\ 4997}# #{e2\ 4998}#)))))
                              #{tmp\ 4994}#)
                       ((lambda (#{_\ 5004}#)
                          (syntax-violation
                            'lambda
                            "bad lambda"
                            #{e\ 4972}#))
                        #{tmp\ 4977}#)))
                   ($sc-dispatch
                     #{tmp\ 4977}#
                     '(any any any . each-any)))))
              ($sc-dispatch
                #{tmp\ 4977}#
                '(any any any any . each-any))))
           #{e\ 4972}#)))
      (#{global-extend\ 3793}#
        'core
        'lambda*
        (lambda (#{e\ 5005}#
                 #{r\ 5006}#
                 #{w\ 5007}#
                 #{s\ 5008}#
                 #{mod\ 5009}#)
          ((lambda (#{tmp\ 5010}#)
             ((lambda (#{tmp\ 5011}#)
                (if #{tmp\ 5011}#
                  (apply (lambda (#{_\ 5012}#
                                  #{args\ 5013}#
                                  #{e1\ 5014}#
                                  #{e2\ 5015}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 3843}#
                                 #{e\ 5005}#
                                 #{r\ 5006}#
                                 #{w\ 5007}#
                                 #{s\ 5008}#
                                 #{mod\ 5009}#
                                 #{lambda*-formals\ 3842}#
                                 (list (cons #{args\ 5013}#
                                             (cons #{e1\ 5014}#
                                                   #{e2\ 5015}#)))))
                             (lambda (#{docstring\ 5017}# #{lcase\ 5018}#)
                               (#{build-case-lambda\ 3770}#
                                 #{s\ 5008}#
                                 #{docstring\ 5017}#
                                 #{lcase\ 5018}#))))
                         #{tmp\ 5011}#)
                  ((lambda (#{_\ 5019}#)
                     (syntax-violation
                       'lambda
                       "bad lambda*"
                       #{e\ 5005}#))
                   #{tmp\ 5010}#)))
              ($sc-dispatch
                #{tmp\ 5010}#
                '(any any any . each-any))))
           #{e\ 5005}#)))
      (#{global-extend\ 3793}#
        'core
        'case-lambda
        (lambda (#{e\ 5020}#
                 #{r\ 5021}#
                 #{w\ 5022}#
                 #{s\ 5023}#
                 #{mod\ 5024}#)
          ((lambda (#{tmp\ 5025}#)
             ((lambda (#{tmp\ 5026}#)
                (if #{tmp\ 5026}#
                  (apply (lambda (#{_\ 5027}#
                                  #{args\ 5028}#
                                  #{e1\ 5029}#
                                  #{e2\ 5030}#
                                  #{args*\ 5031}#
                                  #{e1*\ 5032}#
                                  #{e2*\ 5033}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 3843}#
                                 #{e\ 5020}#
                                 #{r\ 5021}#
                                 #{w\ 5022}#
                                 #{s\ 5023}#
                                 #{mod\ 5024}#
                                 #{lambda-formals\ 3840}#
                                 (cons (cons #{args\ 5028}#
                                             (cons #{e1\ 5029}# #{e2\ 5030}#))
                                       (map (lambda (#{tmp\ 5037}#
                                                     #{tmp\ 5036}#
                                                     #{tmp\ 5035}#)
                                              (cons #{tmp\ 5035}#
                                                    (cons #{tmp\ 5036}#
                                                          #{tmp\ 5037}#)))
                                            #{e2*\ 5033}#
                                            #{e1*\ 5032}#
                                            #{args*\ 5031}#))))
                             (lambda (#{docstring\ 5039}# #{lcase\ 5040}#)
                               (#{build-case-lambda\ 3770}#
                                 #{s\ 5023}#
                                 #{docstring\ 5039}#
                                 #{lcase\ 5040}#))))
                         #{tmp\ 5026}#)
                  ((lambda (#{_\ 5041}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda"
                       #{e\ 5020}#))
                   #{tmp\ 5025}#)))
              ($sc-dispatch
                #{tmp\ 5025}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 5020}#)))
      (#{global-extend\ 3793}#
        'core
        'case-lambda*
        (lambda (#{e\ 5042}#
                 #{r\ 5043}#
                 #{w\ 5044}#
                 #{s\ 5045}#
                 #{mod\ 5046}#)
          ((lambda (#{tmp\ 5047}#)
             ((lambda (#{tmp\ 5048}#)
                (if #{tmp\ 5048}#
                  (apply (lambda (#{_\ 5049}#
                                  #{args\ 5050}#
                                  #{e1\ 5051}#
                                  #{e2\ 5052}#
                                  #{args*\ 5053}#
                                  #{e1*\ 5054}#
                                  #{e2*\ 5055}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 3843}#
                                 #{e\ 5042}#
                                 #{r\ 5043}#
                                 #{w\ 5044}#
                                 #{s\ 5045}#
                                 #{mod\ 5046}#
                                 #{lambda*-formals\ 3842}#
                                 (cons (cons #{args\ 5050}#
                                             (cons #{e1\ 5051}# #{e2\ 5052}#))
                                       (map (lambda (#{tmp\ 5059}#
                                                     #{tmp\ 5058}#
                                                     #{tmp\ 5057}#)
                                              (cons #{tmp\ 5057}#
                                                    (cons #{tmp\ 5058}#
                                                          #{tmp\ 5059}#)))
                                            #{e2*\ 5055}#
                                            #{e1*\ 5054}#
                                            #{args*\ 5053}#))))
                             (lambda (#{docstring\ 5061}# #{lcase\ 5062}#)
                               (#{build-case-lambda\ 3770}#
                                 #{s\ 5045}#
                                 #{docstring\ 5061}#
                                 #{lcase\ 5062}#))))
                         #{tmp\ 5048}#)
                  ((lambda (#{_\ 5063}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda*"
                       #{e\ 5042}#))
                   #{tmp\ 5047}#)))
              ($sc-dispatch
                #{tmp\ 5047}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 5042}#)))
      (#{global-extend\ 3793}#
        'core
        'let
        (letrec ((#{chi-let\ 5064}#
                   (lambda (#{e\ 5065}#
                            #{r\ 5066}#
                            #{w\ 5067}#
                            #{s\ 5068}#
                            #{mod\ 5069}#
                            #{constructor\ 5070}#
                            #{ids\ 5071}#
                            #{vals\ 5072}#
                            #{exps\ 5073}#)
                     (if (not (#{valid-bound-ids?\ 3820}# #{ids\ 5071}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 5065}#)
                       (let ((#{labels\ 5074}#
                               (#{gen-labels\ 3801}# #{ids\ 5071}#))
                             (#{new-vars\ 5075}#
                               (map #{gen-var\ 3845}# #{ids\ 5071}#)))
                         (let ((#{nw\ 5076}#
                                 (#{make-binding-wrap\ 3812}#
                                   #{ids\ 5071}#
                                   #{labels\ 5074}#
                                   #{w\ 5067}#))
                               (#{nr\ 5077}#
                                 (#{extend-var-env\ 3790}#
                                   #{labels\ 5074}#
                                   #{new-vars\ 5075}#
                                   #{r\ 5066}#)))
                           (#{constructor\ 5070}#
                             #{s\ 5068}#
                             (map syntax->datum #{ids\ 5071}#)
                             #{new-vars\ 5075}#
                             (map (lambda (#{x\ 5078}#)
                                    (#{chi\ 3831}#
                                      #{x\ 5078}#
                                      #{r\ 5066}#
                                      #{w\ 5067}#
                                      #{mod\ 5069}#))
                                  #{vals\ 5072}#)
                             (#{chi-body\ 3835}#
                               #{exps\ 5073}#
                               (#{source-wrap\ 3824}#
                                 #{e\ 5065}#
                                 #{nw\ 5076}#
                                 #{s\ 5068}#
                                 #{mod\ 5069}#)
                               #{nr\ 5077}#
                               #{nw\ 5076}#
                               #{mod\ 5069}#))))))))
          (lambda (#{e\ 5079}#
                   #{r\ 5080}#
                   #{w\ 5081}#
                   #{s\ 5082}#
                   #{mod\ 5083}#)
            ((lambda (#{tmp\ 5084}#)
               ((lambda (#{tmp\ 5085}#)
                  (if (if #{tmp\ 5085}#
                        (apply (lambda (#{_\ 5086}#
                                        #{id\ 5087}#
                                        #{val\ 5088}#
                                        #{e1\ 5089}#
                                        #{e2\ 5090}#)
                                 (and-map #{id?\ 3795}# #{id\ 5087}#))
                               #{tmp\ 5085}#)
                        #f)
                    (apply (lambda (#{_\ 5092}#
                                    #{id\ 5093}#
                                    #{val\ 5094}#
                                    #{e1\ 5095}#
                                    #{e2\ 5096}#)
                             (#{chi-let\ 5064}#
                               #{e\ 5079}#
                               #{r\ 5080}#
                               #{w\ 5081}#
                               #{s\ 5082}#
                               #{mod\ 5083}#
                               #{build-let\ 3775}#
                               #{id\ 5093}#
                               #{val\ 5094}#
                               (cons #{e1\ 5095}# #{e2\ 5096}#)))
                           #{tmp\ 5085}#)
                    ((lambda (#{tmp\ 5100}#)
                       (if (if #{tmp\ 5100}#
                             (apply (lambda (#{_\ 5101}#
                                             #{f\ 5102}#
                                             #{id\ 5103}#
                                             #{val\ 5104}#
                                             #{e1\ 5105}#
                                             #{e2\ 5106}#)
                                      (if (#{id?\ 3795}# #{f\ 5102}#)
                                        (and-map #{id?\ 3795}# #{id\ 5103}#)
                                        #f))
                                    #{tmp\ 5100}#)
                             #f)
                         (apply (lambda (#{_\ 5108}#
                                         #{f\ 5109}#
                                         #{id\ 5110}#
                                         #{val\ 5111}#
                                         #{e1\ 5112}#
                                         #{e2\ 5113}#)
                                  (#{chi-let\ 5064}#
                                    #{e\ 5079}#
                                    #{r\ 5080}#
                                    #{w\ 5081}#
                                    #{s\ 5082}#
                                    #{mod\ 5083}#
                                    #{build-named-let\ 3776}#
                                    (cons #{f\ 5109}# #{id\ 5110}#)
                                    #{val\ 5111}#
                                    (cons #{e1\ 5112}# #{e2\ 5113}#)))
                                #{tmp\ 5100}#)
                         ((lambda (#{_\ 5117}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 3824}#
                                #{e\ 5079}#
                                #{w\ 5081}#
                                #{s\ 5082}#
                                #{mod\ 5083}#)))
                          #{tmp\ 5084}#)))
                     ($sc-dispatch
                       #{tmp\ 5084}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 5084}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 5079}#))))
      (#{global-extend\ 3793}#
        'core
        'letrec
        (lambda (#{e\ 5118}#
                 #{r\ 5119}#
                 #{w\ 5120}#
                 #{s\ 5121}#
                 #{mod\ 5122}#)
          ((lambda (#{tmp\ 5123}#)
             ((lambda (#{tmp\ 5124}#)
                (if (if #{tmp\ 5124}#
                      (apply (lambda (#{_\ 5125}#
                                      #{id\ 5126}#
                                      #{val\ 5127}#
                                      #{e1\ 5128}#
                                      #{e2\ 5129}#)
                               (and-map #{id?\ 3795}# #{id\ 5126}#))
                             #{tmp\ 5124}#)
                      #f)
                  (apply (lambda (#{_\ 5131}#
                                  #{id\ 5132}#
                                  #{val\ 5133}#
                                  #{e1\ 5134}#
                                  #{e2\ 5135}#)
                           (let ((#{ids\ 5136}# #{id\ 5132}#))
                             (if (not (#{valid-bound-ids?\ 3820}#
                                        #{ids\ 5136}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 5118}#)
                               (let ((#{labels\ 5138}#
                                       (#{gen-labels\ 3801}# #{ids\ 5136}#))
                                     (#{new-vars\ 5139}#
                                       (map #{gen-var\ 3845}# #{ids\ 5136}#)))
                                 (let ((#{w\ 5140}#
                                         (#{make-binding-wrap\ 3812}#
                                           #{ids\ 5136}#
                                           #{labels\ 5138}#
                                           #{w\ 5120}#))
                                       (#{r\ 5141}#
                                         (#{extend-var-env\ 3790}#
                                           #{labels\ 5138}#
                                           #{new-vars\ 5139}#
                                           #{r\ 5119}#)))
                                   (#{build-letrec\ 3777}#
                                     #{s\ 5121}#
                                     (map syntax->datum #{ids\ 5136}#)
                                     #{new-vars\ 5139}#
                                     (map (lambda (#{x\ 5142}#)
                                            (#{chi\ 3831}#
                                              #{x\ 5142}#
                                              #{r\ 5141}#
                                              #{w\ 5140}#
                                              #{mod\ 5122}#))
                                          #{val\ 5133}#)
                                     (#{chi-body\ 3835}#
                                       (cons #{e1\ 5134}# #{e2\ 5135}#)
                                       (#{source-wrap\ 3824}#
                                         #{e\ 5118}#
                                         #{w\ 5140}#
                                         #{s\ 5121}#
                                         #{mod\ 5122}#)
                                       #{r\ 5141}#
                                       #{w\ 5140}#
                                       #{mod\ 5122}#)))))))
                         #{tmp\ 5124}#)
                  ((lambda (#{_\ 5145}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 3824}#
                         #{e\ 5118}#
                         #{w\ 5120}#
                         #{s\ 5121}#
                         #{mod\ 5122}#)))
                   #{tmp\ 5123}#)))
              ($sc-dispatch
                #{tmp\ 5123}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 5118}#)))
      (#{global-extend\ 3793}#
        'core
        'set!
        (lambda (#{e\ 5146}#
                 #{r\ 5147}#
                 #{w\ 5148}#
                 #{s\ 5149}#
                 #{mod\ 5150}#)
          ((lambda (#{tmp\ 5151}#)
             ((lambda (#{tmp\ 5152}#)
                (if (if #{tmp\ 5152}#
                      (apply (lambda (#{_\ 5153}# #{id\ 5154}# #{val\ 5155}#)
                               (#{id?\ 3795}# #{id\ 5154}#))
                             #{tmp\ 5152}#)
                      #f)
                  (apply (lambda (#{_\ 5156}# #{id\ 5157}# #{val\ 5158}#)
                           (let ((#{val\ 5159}#
                                   (#{chi\ 3831}#
                                     #{val\ 5158}#
                                     #{r\ 5147}#
                                     #{w\ 5148}#
                                     #{mod\ 5150}#))
                                 (#{n\ 5160}#
                                   (#{id-var-name\ 3817}#
                                     #{id\ 5157}#
                                     #{w\ 5148}#)))
                             (let ((#{b\ 5161}#
                                     (#{lookup\ 3792}#
                                       #{n\ 5160}#
                                       #{r\ 5147}#
                                       #{mod\ 5150}#)))
                               (let ((#{atom-key\ 5162}#
                                       (#{binding-type\ 3787}# #{b\ 5161}#)))
                                 (if (memv #{atom-key\ 5162}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 3763}#
                                     #{s\ 5149}#
                                     (syntax->datum #{id\ 5157}#)
                                     (#{binding-value\ 3788}# #{b\ 5161}#)
                                     #{val\ 5159}#)
                                   (if (memv #{atom-key\ 5162}#
                                             '(global))
                                     (#{build-global-assignment\ 3766}#
                                       #{s\ 5149}#
                                       #{n\ 5160}#
                                       #{val\ 5159}#
                                       #{mod\ 5150}#)
                                     (if (memv #{atom-key\ 5162}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 3823}#
                                           #{id\ 5157}#
                                           #{w\ 5148}#
                                           #{mod\ 5150}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 3824}#
                                           #{e\ 5146}#
                                           #{w\ 5148}#
                                           #{s\ 5149}#
                                           #{mod\ 5150}#)))))))))
                         #{tmp\ 5152}#)
                  ((lambda (#{tmp\ 5163}#)
                     (if #{tmp\ 5163}#
                       (apply (lambda (#{_\ 5164}#
                                       #{head\ 5165}#
                                       #{tail\ 5166}#
                                       #{val\ 5167}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 3829}#
                                      #{head\ 5165}#
                                      #{r\ 5147}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 5150}#
                                      #t))
                                  (lambda (#{type\ 5168}#
                                           #{value\ 5169}#
                                           #{ee\ 5170}#
                                           #{ww\ 5171}#
                                           #{ss\ 5172}#
                                           #{modmod\ 5173}#)
                                    (if (memv #{type\ 5168}#
                                              '(module-ref))
                                      (let ((#{val\ 5174}#
                                              (#{chi\ 3831}#
                                                #{val\ 5167}#
                                                #{r\ 5147}#
                                                #{w\ 5148}#
                                                #{mod\ 5150}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 5169}#
                                              (cons #{head\ 5165}#
                                                    #{tail\ 5166}#)))
                                          (lambda (#{id\ 5176}# #{mod\ 5177}#)
                                            (#{build-global-assignment\ 3766}#
                                              #{s\ 5149}#
                                              #{id\ 5176}#
                                              #{val\ 5174}#
                                              #{mod\ 5177}#))))
                                      (#{build-application\ 3760}#
                                        #{s\ 5149}#
                                        (#{chi\ 3831}#
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
                                                #{head\ 5165}#)
                                          #{r\ 5147}#
                                          #{w\ 5148}#
                                          #{mod\ 5150}#)
                                        (map (lambda (#{e\ 5178}#)
                                               (#{chi\ 3831}#
                                                 #{e\ 5178}#
                                                 #{r\ 5147}#
                                                 #{w\ 5148}#
                                                 #{mod\ 5150}#))
                                             (append
                                               #{tail\ 5166}#
                                               (list #{val\ 5167}#))))))))
                              #{tmp\ 5163}#)
                       ((lambda (#{_\ 5180}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 3824}#
                              #{e\ 5146}#
                              #{w\ 5148}#
                              #{s\ 5149}#
                              #{mod\ 5150}#)))
                        #{tmp\ 5151}#)))
                   ($sc-dispatch
                     #{tmp\ 5151}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 5151}#
                '(any any any))))
           #{e\ 5146}#)))
      (#{global-extend\ 3793}#
        'module-ref
        '@
        (lambda (#{e\ 5181}#)
          ((lambda (#{tmp\ 5182}#)
             ((lambda (#{tmp\ 5183}#)
                (if (if #{tmp\ 5183}#
                      (apply (lambda (#{_\ 5184}# #{mod\ 5185}# #{id\ 5186}#)
                               (if (and-map #{id?\ 3795}# #{mod\ 5185}#)
                                 (#{id?\ 3795}# #{id\ 5186}#)
                                 #f))
                             #{tmp\ 5183}#)
                      #f)
                  (apply (lambda (#{_\ 5188}# #{mod\ 5189}# #{id\ 5190}#)
                           (values
                             (syntax->datum #{id\ 5190}#)
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
                                     #{mod\ 5189}#))))
                         #{tmp\ 5183}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 5182}#)))
              ($sc-dispatch
                #{tmp\ 5182}#
                '(any each-any any))))
           #{e\ 5181}#)))
      (#{global-extend\ 3793}#
        'module-ref
        '@@
        (lambda (#{e\ 5192}#)
          ((lambda (#{tmp\ 5193}#)
             ((lambda (#{tmp\ 5194}#)
                (if (if #{tmp\ 5194}#
                      (apply (lambda (#{_\ 5195}# #{mod\ 5196}# #{id\ 5197}#)
                               (if (and-map #{id?\ 3795}# #{mod\ 5196}#)
                                 (#{id?\ 3795}# #{id\ 5197}#)
                                 #f))
                             #{tmp\ 5194}#)
                      #f)
                  (apply (lambda (#{_\ 5199}# #{mod\ 5200}# #{id\ 5201}#)
                           (values
                             (syntax->datum #{id\ 5201}#)
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
                                     #{mod\ 5200}#))))
                         #{tmp\ 5194}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 5193}#)))
              ($sc-dispatch
                #{tmp\ 5193}#
                '(any each-any any))))
           #{e\ 5192}#)))
      (#{global-extend\ 3793}#
        'core
        'if
        (lambda (#{e\ 5203}#
                 #{r\ 5204}#
                 #{w\ 5205}#
                 #{s\ 5206}#
                 #{mod\ 5207}#)
          ((lambda (#{tmp\ 5208}#)
             ((lambda (#{tmp\ 5209}#)
                (if #{tmp\ 5209}#
                  (apply (lambda (#{_\ 5210}# #{test\ 5211}# #{then\ 5212}#)
                           (#{build-conditional\ 3761}#
                             #{s\ 5206}#
                             (#{chi\ 3831}#
                               #{test\ 5211}#
                               #{r\ 5204}#
                               #{w\ 5205}#
                               #{mod\ 5207}#)
                             (#{chi\ 3831}#
                               #{then\ 5212}#
                               #{r\ 5204}#
                               #{w\ 5205}#
                               #{mod\ 5207}#)
                             (#{build-void\ 3759}# #f)))
                         #{tmp\ 5209}#)
                  ((lambda (#{tmp\ 5213}#)
                     (if #{tmp\ 5213}#
                       (apply (lambda (#{_\ 5214}#
                                       #{test\ 5215}#
                                       #{then\ 5216}#
                                       #{else\ 5217}#)
                                (#{build-conditional\ 3761}#
                                  #{s\ 5206}#
                                  (#{chi\ 3831}#
                                    #{test\ 5215}#
                                    #{r\ 5204}#
                                    #{w\ 5205}#
                                    #{mod\ 5207}#)
                                  (#{chi\ 3831}#
                                    #{then\ 5216}#
                                    #{r\ 5204}#
                                    #{w\ 5205}#
                                    #{mod\ 5207}#)
                                  (#{chi\ 3831}#
                                    #{else\ 5217}#
                                    #{r\ 5204}#
                                    #{w\ 5205}#
                                    #{mod\ 5207}#)))
                              #{tmp\ 5213}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 5208}#)))
                   ($sc-dispatch
                     #{tmp\ 5208}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 5208}#
                '(any any any))))
           #{e\ 5203}#)))
      (#{global-extend\ 3793}#
        'begin
        'begin
        '())
      (#{global-extend\ 3793}#
        'define
        'define
        '())
      (#{global-extend\ 3793}#
        'define-syntax
        'define-syntax
        '())
      (#{global-extend\ 3793}#
        'eval-when
        'eval-when
        '())
      (#{global-extend\ 3793}#
        'core
        'syntax-case
        (letrec ((#{gen-syntax-case\ 5221}#
                   (lambda (#{x\ 5222}#
                            #{keys\ 5223}#
                            #{clauses\ 5224}#
                            #{r\ 5225}#
                            #{mod\ 5226}#)
                     (if (null? #{clauses\ 5224}#)
                       (#{build-application\ 3760}#
                         #f
                         (#{build-primref\ 3772}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 3773}# #f #f)
                               (#{build-data\ 3773}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 5222}#))
                       ((lambda (#{tmp\ 5227}#)
                          ((lambda (#{tmp\ 5228}#)
                             (if #{tmp\ 5228}#
                               (apply (lambda (#{pat\ 5229}# #{exp\ 5230}#)
                                        (if (if (#{id?\ 3795}# #{pat\ 5229}#)
                                              (and-map
                                                (lambda (#{x\ 5231}#)
                                                  (not (#{free-id=?\ 3818}#
                                                         #{pat\ 5229}#
                                                         #{x\ 5231}#)))
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
                                                      #{keys\ 5223}#))
                                              #f)
                                          (let ((#{labels\ 5232}#
                                                  (list (#{gen-label\ 3800}#)))
                                                (#{var\ 5233}#
                                                  (#{gen-var\ 3845}#
                                                    #{pat\ 5229}#)))
                                            (#{build-application\ 3760}#
                                              #f
                                              (#{build-simple-lambda\ 3769}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 5229}#))
                                                #f
                                                (list #{var\ 5233}#)
                                                #f
                                                (#{chi\ 3831}#
                                                  #{exp\ 5230}#
                                                  (#{extend-env\ 3789}#
                                                    #{labels\ 5232}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 5233}#
                                                                      0)))
                                                    #{r\ 5225}#)
                                                  (#{make-binding-wrap\ 3812}#
                                                    (list #{pat\ 5229}#)
                                                    #{labels\ 5232}#
                                                    '(()))
                                                  #{mod\ 5226}#))
                                              (list #{x\ 5222}#)))
                                          (#{gen-clause\ 5220}#
                                            #{x\ 5222}#
                                            #{keys\ 5223}#
                                            (cdr #{clauses\ 5224}#)
                                            #{r\ 5225}#
                                            #{pat\ 5229}#
                                            #t
                                            #{exp\ 5230}#
                                            #{mod\ 5226}#)))
                                      #{tmp\ 5228}#)
                               ((lambda (#{tmp\ 5234}#)
                                  (if #{tmp\ 5234}#
                                    (apply (lambda (#{pat\ 5235}#
                                                    #{fender\ 5236}#
                                                    #{exp\ 5237}#)
                                             (#{gen-clause\ 5220}#
                                               #{x\ 5222}#
                                               #{keys\ 5223}#
                                               (cdr #{clauses\ 5224}#)
                                               #{r\ 5225}#
                                               #{pat\ 5235}#
                                               #{fender\ 5236}#
                                               #{exp\ 5237}#
                                               #{mod\ 5226}#))
                                           #{tmp\ 5234}#)
                                    ((lambda (#{_\ 5238}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 5224}#)))
                                     #{tmp\ 5227}#)))
                                ($sc-dispatch
                                  #{tmp\ 5227}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 5227}# (quote (any any)))))
                        (car #{clauses\ 5224}#)))))
                 (#{gen-clause\ 5220}#
                   (lambda (#{x\ 5239}#
                            #{keys\ 5240}#
                            #{clauses\ 5241}#
                            #{r\ 5242}#
                            #{pat\ 5243}#
                            #{fender\ 5244}#
                            #{exp\ 5245}#
                            #{mod\ 5246}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 5218}#
                           #{pat\ 5243}#
                           #{keys\ 5240}#))
                       (lambda (#{p\ 5247}# #{pvars\ 5248}#)
                         (if (not (#{distinct-bound-ids?\ 3821}#
                                    (map car #{pvars\ 5248}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 5243}#)
                           (if (not (and-map
                                      (lambda (#{x\ 5249}#)
                                        (not (#{ellipsis?\ 3839}#
                                               (car #{x\ 5249}#))))
                                      #{pvars\ 5248}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 5243}#)
                             (let ((#{y\ 5250}#
                                     (#{gen-var\ 3845}# (quote tmp))))
                               (#{build-application\ 3760}#
                                 #f
                                 (#{build-simple-lambda\ 3769}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 5250}#)
                                   #f
                                   (let ((#{y\ 5251}#
                                           (#{build-lexical-reference\ 3762}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 5250}#)))
                                     (#{build-conditional\ 3761}#
                                       #f
                                       ((lambda (#{tmp\ 5252}#)
                                          ((lambda (#{tmp\ 5253}#)
                                             (if #{tmp\ 5253}#
                                               (apply (lambda () #{y\ 5251}#)
                                                      #{tmp\ 5253}#)
                                               ((lambda (#{_\ 5254}#)
                                                  (#{build-conditional\ 3761}#
                                                    #f
                                                    #{y\ 5251}#
                                                    (#{build-dispatch-call\ 5219}#
                                                      #{pvars\ 5248}#
                                                      #{fender\ 5244}#
                                                      #{y\ 5251}#
                                                      #{r\ 5242}#
                                                      #{mod\ 5246}#)
                                                    (#{build-data\ 3773}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 5252}#)))
                                           ($sc-dispatch
                                             #{tmp\ 5252}#
                                             '#(atom #t))))
                                        #{fender\ 5244}#)
                                       (#{build-dispatch-call\ 5219}#
                                         #{pvars\ 5248}#
                                         #{exp\ 5245}#
                                         #{y\ 5251}#
                                         #{r\ 5242}#
                                         #{mod\ 5246}#)
                                       (#{gen-syntax-case\ 5221}#
                                         #{x\ 5239}#
                                         #{keys\ 5240}#
                                         #{clauses\ 5241}#
                                         #{r\ 5242}#
                                         #{mod\ 5246}#))))
                                 (list (if (eq? #{p\ 5247}# (quote any))
                                         (#{build-application\ 3760}#
                                           #f
                                           (#{build-primref\ 3772}#
                                             #f
                                             'list)
                                           (list #{x\ 5239}#))
                                         (#{build-application\ 3760}#
                                           #f
                                           (#{build-primref\ 3772}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 5239}#
                                                 (#{build-data\ 3773}#
                                                   #f
                                                   #{p\ 5247}#)))))))))))))
                 (#{build-dispatch-call\ 5219}#
                   (lambda (#{pvars\ 5255}#
                            #{exp\ 5256}#
                            #{y\ 5257}#
                            #{r\ 5258}#
                            #{mod\ 5259}#)
                     (let ((#{ids\ 5260}# (map car #{pvars\ 5255}#))
                           (#{levels\ 5261}# (map cdr #{pvars\ 5255}#)))
                       (let ((#{labels\ 5262}#
                               (#{gen-labels\ 3801}# #{ids\ 5260}#))
                             (#{new-vars\ 5263}#
                               (map #{gen-var\ 3845}# #{ids\ 5260}#)))
                         (#{build-application\ 3760}#
                           #f
                           (#{build-primref\ 3772}# #f (quote apply))
                           (list (#{build-simple-lambda\ 3769}#
                                   #f
                                   (map syntax->datum #{ids\ 5260}#)
                                   #f
                                   #{new-vars\ 5263}#
                                   #f
                                   (#{chi\ 3831}#
                                     #{exp\ 5256}#
                                     (#{extend-env\ 3789}#
                                       #{labels\ 5262}#
                                       (map (lambda (#{var\ 5264}#
                                                     #{level\ 5265}#)
                                              (cons 'syntax
                                                    (cons #{var\ 5264}#
                                                          #{level\ 5265}#)))
                                            #{new-vars\ 5263}#
                                            (map cdr #{pvars\ 5255}#))
                                       #{r\ 5258}#)
                                     (#{make-binding-wrap\ 3812}#
                                       #{ids\ 5260}#
                                       #{labels\ 5262}#
                                       '(()))
                                     #{mod\ 5259}#))
                                 #{y\ 5257}#))))))
                 (#{convert-pattern\ 5218}#
                   (lambda (#{pattern\ 5266}# #{keys\ 5267}#)
                     (letrec ((#{cvt\ 5268}#
                                (lambda (#{p\ 5269}# #{n\ 5270}# #{ids\ 5271}#)
                                  (if (#{id?\ 3795}# #{p\ 5269}#)
                                    (if (#{bound-id-member?\ 3822}#
                                          #{p\ 5269}#
                                          #{keys\ 5267}#)
                                      (values
                                        (vector (quote free-id) #{p\ 5269}#)
                                        #{ids\ 5271}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 5269}# #{n\ 5270}#)
                                              #{ids\ 5271}#)))
                                    ((lambda (#{tmp\ 5272}#)
                                       ((lambda (#{tmp\ 5273}#)
                                          (if (if #{tmp\ 5273}#
                                                (apply (lambda (#{x\ 5274}#
                                                                #{dots\ 5275}#)
                                                         (#{ellipsis?\ 3839}#
                                                           #{dots\ 5275}#))
                                                       #{tmp\ 5273}#)
                                                #f)
                                            (apply (lambda (#{x\ 5276}#
                                                            #{dots\ 5277}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 5268}#
                                                           #{x\ 5276}#
                                                           (#{fx+\ 3750}#
                                                             #{n\ 5270}#
                                                             1)
                                                           #{ids\ 5271}#))
                                                       (lambda (#{p\ 5278}#
                                                                #{ids\ 5279}#)
                                                         (values
                                                           (if (eq? #{p\ 5278}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 5278}#))
                                                           #{ids\ 5279}#))))
                                                   #{tmp\ 5273}#)
                                            ((lambda (#{tmp\ 5280}#)
                                               (if #{tmp\ 5280}#
                                                 (apply (lambda (#{x\ 5281}#
                                                                 #{y\ 5282}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 5268}#
                                                                #{y\ 5282}#
                                                                #{n\ 5270}#
                                                                #{ids\ 5271}#))
                                                            (lambda (#{y\ 5283}#
                                                                     #{ids\ 5284}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 5268}#
                                                                    #{x\ 5281}#
                                                                    #{n\ 5270}#
                                                                    #{ids\ 5284}#))
                                                                (lambda (#{x\ 5285}#
                                                                         #{ids\ 5286}#)
                                                                  (values
                                                                    (cons #{x\ 5285}#
                                                                          #{y\ 5283}#)
                                                                    #{ids\ 5286}#))))))
                                                        #{tmp\ 5280}#)
                                                 ((lambda (#{tmp\ 5287}#)
                                                    (if #{tmp\ 5287}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 5271}#))
                                                             #{tmp\ 5287}#)
                                                      ((lambda (#{tmp\ 5288}#)
                                                         (if #{tmp\ 5288}#
                                                           (apply (lambda (#{x\ 5289}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 5268}#
                                                                          #{x\ 5289}#
                                                                          #{n\ 5270}#
                                                                          #{ids\ 5271}#))
                                                                      (lambda (#{p\ 5291}#
                                                                               #{ids\ 5292}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 5291}#)
                                                                          #{ids\ 5292}#))))
                                                                  #{tmp\ 5288}#)
                                                           ((lambda (#{x\ 5293}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 3844}#
                                                                    #{p\ 5269}#
                                                                    '(())))
                                                                #{ids\ 5271}#))
                                                            #{tmp\ 5272}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 5272}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 5272}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 5272}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 5272}#
                                          '(any any))))
                                     #{p\ 5269}#)))))
                       (#{cvt\ 5268}# #{pattern\ 5266}# 0 (quote ()))))))
          (lambda (#{e\ 5294}#
                   #{r\ 5295}#
                   #{w\ 5296}#
                   #{s\ 5297}#
                   #{mod\ 5298}#)
            (let ((#{e\ 5299}#
                    (#{source-wrap\ 3824}#
                      #{e\ 5294}#
                      #{w\ 5296}#
                      #{s\ 5297}#
                      #{mod\ 5298}#)))
              ((lambda (#{tmp\ 5300}#)
                 ((lambda (#{tmp\ 5301}#)
                    (if #{tmp\ 5301}#
                      (apply (lambda (#{_\ 5302}#
                                      #{val\ 5303}#
                                      #{key\ 5304}#
                                      #{m\ 5305}#)
                               (if (and-map
                                     (lambda (#{x\ 5306}#)
                                       (if (#{id?\ 3795}# #{x\ 5306}#)
                                         (not (#{ellipsis?\ 3839}#
                                                #{x\ 5306}#))
                                         #f))
                                     #{key\ 5304}#)
                                 (let ((#{x\ 5308}#
                                         (#{gen-var\ 3845}# (quote tmp))))
                                   (#{build-application\ 3760}#
                                     #{s\ 5297}#
                                     (#{build-simple-lambda\ 3769}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 5308}#)
                                       #f
                                       (#{gen-syntax-case\ 5221}#
                                         (#{build-lexical-reference\ 3762}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 5308}#)
                                         #{key\ 5304}#
                                         #{m\ 5305}#
                                         #{r\ 5295}#
                                         #{mod\ 5298}#))
                                     (list (#{chi\ 3831}#
                                             #{val\ 5303}#
                                             #{r\ 5295}#
                                             '(())
                                             #{mod\ 5298}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 5299}#)))
                             #{tmp\ 5301}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 5300}#)))
                  ($sc-dispatch
                    #{tmp\ 5300}#
                    '(any any each-any . each-any))))
               #{e\ 5299}#)))))
      (set! sc-expand
        (lambda (#{x\ 5311}# . #{rest\ 5312}#)
          (if (if (pair? #{x\ 5311}#)
                (equal? (car #{x\ 5311}#) #{noexpand\ 3748}#)
                #f)
            (cadr #{x\ 5311}#)
            (let ((#{m\ 5313}#
                    (if (null? #{rest\ 5312}#)
                      'e
                      (car #{rest\ 5312}#)))
                  (#{esew\ 5314}#
                    (if (let ((#{t\ 5315}# (null? #{rest\ 5312}#)))
                          (if #{t\ 5315}#
                            #{t\ 5315}#
                            (null? (cdr #{rest\ 5312}#))))
                      '(eval)
                      (cadr #{rest\ 5312}#))))
              (with-fluid*
                #{*mode*\ 3749}#
                #{m\ 5313}#
                (lambda ()
                  (#{chi-top\ 3830}#
                    #{x\ 5311}#
                    '()
                    '((top))
                    #{m\ 5313}#
                    #{esew\ 5314}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 5316}#)
          (#{nonsymbol-id?\ 3794}# #{x\ 5316}#)))
      (set! datum->syntax
        (lambda (#{id\ 5317}# #{datum\ 5318}#)
          (#{make-syntax-object\ 3778}#
            #{datum\ 5318}#
            (#{syntax-object-wrap\ 3781}# #{id\ 5317}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 5319}#)
          (#{strip\ 3844}# #{x\ 5319}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 5320}#)
          (begin
            (let ((#{x\ 5321}# #{ls\ 5320}#))
              (if (not (list? #{x\ 5321}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 5321}#)))
            (map (lambda (#{x\ 5322}#)
                   (#{wrap\ 3823}# (gensym) (quote ((top))) #f))
                 #{ls\ 5320}#))))
      (set! free-identifier=?
        (lambda (#{x\ 5323}# #{y\ 5324}#)
          (begin
            (let ((#{x\ 5325}# #{x\ 5323}#))
              (if (not (#{nonsymbol-id?\ 3794}# #{x\ 5325}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 5325}#)))
            (let ((#{x\ 5326}# #{y\ 5324}#))
              (if (not (#{nonsymbol-id?\ 3794}# #{x\ 5326}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 5326}#)))
            (#{free-id=?\ 3818}# #{x\ 5323}# #{y\ 5324}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 5327}# #{y\ 5328}#)
          (begin
            (let ((#{x\ 5329}# #{x\ 5327}#))
              (if (not (#{nonsymbol-id?\ 3794}# #{x\ 5329}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 5329}#)))
            (let ((#{x\ 5330}# #{y\ 5328}#))
              (if (not (#{nonsymbol-id?\ 3794}# #{x\ 5330}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 5330}#)))
            (#{bound-id=?\ 3819}# #{x\ 5327}# #{y\ 5328}#))))
      (set! syntax-violation
        (lambda (#{who\ 5331}#
                 #{message\ 5332}#
                 #{form\ 5333}#
                 .
                 #{subform\ 5334}#)
          (begin
            (let ((#{x\ 5335}# #{who\ 5331}#))
              (if (not ((lambda (#{x\ 5336}#)
                          (let ((#{t\ 5337}# (not #{x\ 5336}#)))
                            (if #{t\ 5337}#
                              #{t\ 5337}#
                              (let ((#{t\ 5338}# (string? #{x\ 5336}#)))
                                (if #{t\ 5338}#
                                  #{t\ 5338}#
                                  (symbol? #{x\ 5336}#))))))
                        #{x\ 5335}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 5335}#)))
            (let ((#{x\ 5339}# #{message\ 5332}#))
              (if (not (string? #{x\ 5339}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 5339}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 5331}# "~a: " "")
                "~a "
                (if (null? #{subform\ 5334}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 5340}#
                      (cons #{message\ 5332}#
                            (map (lambda (#{x\ 5341}#)
                                   (#{strip\ 3844}# #{x\ 5341}# (quote (()))))
                                 (append
                                   #{subform\ 5334}#
                                   (list #{form\ 5333}#))))))
                (if #{who\ 5331}#
                  (cons #{who\ 5331}# #{tail\ 5340}#)
                  #{tail\ 5340}#))
              #f))))
      (letrec ((#{match\ 5346}#
                 (lambda (#{e\ 5347}#
                          #{p\ 5348}#
                          #{w\ 5349}#
                          #{r\ 5350}#
                          #{mod\ 5351}#)
                   (if (not #{r\ 5350}#)
                     #f
                     (if (eq? #{p\ 5348}# (quote any))
                       (cons (#{wrap\ 3823}#
                               #{e\ 5347}#
                               #{w\ 5349}#
                               #{mod\ 5351}#)
                             #{r\ 5350}#)
                       (if (#{syntax-object?\ 3779}# #{e\ 5347}#)
                         (#{match*\ 5345}#
                           (#{syntax-object-expression\ 3780}# #{e\ 5347}#)
                           #{p\ 5348}#
                           (#{join-wraps\ 3814}#
                             #{w\ 5349}#
                             (#{syntax-object-wrap\ 3781}# #{e\ 5347}#))
                           #{r\ 5350}#
                           (#{syntax-object-module\ 3782}# #{e\ 5347}#))
                         (#{match*\ 5345}#
                           #{e\ 5347}#
                           #{p\ 5348}#
                           #{w\ 5349}#
                           #{r\ 5350}#
                           #{mod\ 5351}#))))))
               (#{match*\ 5345}#
                 (lambda (#{e\ 5352}#
                          #{p\ 5353}#
                          #{w\ 5354}#
                          #{r\ 5355}#
                          #{mod\ 5356}#)
                   (if (null? #{p\ 5353}#)
                     (if (null? #{e\ 5352}#) #{r\ 5355}# #f)
                     (if (pair? #{p\ 5353}#)
                       (if (pair? #{e\ 5352}#)
                         (#{match\ 5346}#
                           (car #{e\ 5352}#)
                           (car #{p\ 5353}#)
                           #{w\ 5354}#
                           (#{match\ 5346}#
                             (cdr #{e\ 5352}#)
                             (cdr #{p\ 5353}#)
                             #{w\ 5354}#
                             #{r\ 5355}#
                             #{mod\ 5356}#)
                           #{mod\ 5356}#)
                         #f)
                       (if (eq? #{p\ 5353}# (quote each-any))
                         (let ((#{l\ 5357}#
                                 (#{match-each-any\ 5343}#
                                   #{e\ 5352}#
                                   #{w\ 5354}#
                                   #{mod\ 5356}#)))
                           (if #{l\ 5357}#
                             (cons #{l\ 5357}# #{r\ 5355}#)
                             #f))
                         (let ((#{atom-key\ 5358}# (vector-ref #{p\ 5353}# 0)))
                           (if (memv #{atom-key\ 5358}# (quote (each)))
                             (if (null? #{e\ 5352}#)
                               (#{match-empty\ 5344}#
                                 (vector-ref #{p\ 5353}# 1)
                                 #{r\ 5355}#)
                               (let ((#{l\ 5359}#
                                       (#{match-each\ 5342}#
                                         #{e\ 5352}#
                                         (vector-ref #{p\ 5353}# 1)
                                         #{w\ 5354}#
                                         #{mod\ 5356}#)))
                                 (if #{l\ 5359}#
                                   (letrec ((#{collect\ 5360}#
                                              (lambda (#{l\ 5361}#)
                                                (if (null? (car #{l\ 5361}#))
                                                  #{r\ 5355}#
                                                  (cons (map car #{l\ 5361}#)
                                                        (#{collect\ 5360}#
                                                          (map cdr
                                                               #{l\ 5361}#)))))))
                                     (#{collect\ 5360}# #{l\ 5359}#))
                                   #f)))
                             (if (memv #{atom-key\ 5358}# (quote (free-id)))
                               (if (#{id?\ 3795}# #{e\ 5352}#)
                                 (if (#{free-id=?\ 3818}#
                                       (#{wrap\ 3823}#
                                         #{e\ 5352}#
                                         #{w\ 5354}#
                                         #{mod\ 5356}#)
                                       (vector-ref #{p\ 5353}# 1))
                                   #{r\ 5355}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 5358}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 5353}# 1)
                                       (#{strip\ 3844}#
                                         #{e\ 5352}#
                                         #{w\ 5354}#))
                                   #{r\ 5355}#
                                   #f)
                                 (if (memv #{atom-key\ 5358}# (quote (vector)))
                                   (if (vector? #{e\ 5352}#)
                                     (#{match\ 5346}#
                                       (vector->list #{e\ 5352}#)
                                       (vector-ref #{p\ 5353}# 1)
                                       #{w\ 5354}#
                                       #{r\ 5355}#
                                       #{mod\ 5356}#)
                                     #f)))))))))))
               (#{match-empty\ 5344}#
                 (lambda (#{p\ 5362}# #{r\ 5363}#)
                   (if (null? #{p\ 5362}#)
                     #{r\ 5363}#
                     (if (eq? #{p\ 5362}# (quote any))
                       (cons (quote ()) #{r\ 5363}#)
                       (if (pair? #{p\ 5362}#)
                         (#{match-empty\ 5344}#
                           (car #{p\ 5362}#)
                           (#{match-empty\ 5344}#
                             (cdr #{p\ 5362}#)
                             #{r\ 5363}#))
                         (if (eq? #{p\ 5362}# (quote each-any))
                           (cons (quote ()) #{r\ 5363}#)
                           (let ((#{atom-key\ 5364}#
                                   (vector-ref #{p\ 5362}# 0)))
                             (if (memv #{atom-key\ 5364}# (quote (each)))
                               (#{match-empty\ 5344}#
                                 (vector-ref #{p\ 5362}# 1)
                                 #{r\ 5363}#)
                               (if (memv #{atom-key\ 5364}#
                                         '(free-id atom))
                                 #{r\ 5363}#
                                 (if (memv #{atom-key\ 5364}# (quote (vector)))
                                   (#{match-empty\ 5344}#
                                     (vector-ref #{p\ 5362}# 1)
                                     #{r\ 5363}#)))))))))))
               (#{match-each-any\ 5343}#
                 (lambda (#{e\ 5365}# #{w\ 5366}# #{mod\ 5367}#)
                   (if (pair? #{e\ 5365}#)
                     (let ((#{l\ 5368}#
                             (#{match-each-any\ 5343}#
                               (cdr #{e\ 5365}#)
                               #{w\ 5366}#
                               #{mod\ 5367}#)))
                       (if #{l\ 5368}#
                         (cons (#{wrap\ 3823}#
                                 (car #{e\ 5365}#)
                                 #{w\ 5366}#
                                 #{mod\ 5367}#)
                               #{l\ 5368}#)
                         #f))
                     (if (null? #{e\ 5365}#)
                       '()
                       (if (#{syntax-object?\ 3779}# #{e\ 5365}#)
                         (#{match-each-any\ 5343}#
                           (#{syntax-object-expression\ 3780}# #{e\ 5365}#)
                           (#{join-wraps\ 3814}#
                             #{w\ 5366}#
                             (#{syntax-object-wrap\ 3781}# #{e\ 5365}#))
                           #{mod\ 5367}#)
                         #f)))))
               (#{match-each\ 5342}#
                 (lambda (#{e\ 5369}#
                          #{p\ 5370}#
                          #{w\ 5371}#
                          #{mod\ 5372}#)
                   (if (pair? #{e\ 5369}#)
                     (let ((#{first\ 5373}#
                             (#{match\ 5346}#
                               (car #{e\ 5369}#)
                               #{p\ 5370}#
                               #{w\ 5371}#
                               '()
                               #{mod\ 5372}#)))
                       (if #{first\ 5373}#
                         (let ((#{rest\ 5374}#
                                 (#{match-each\ 5342}#
                                   (cdr #{e\ 5369}#)
                                   #{p\ 5370}#
                                   #{w\ 5371}#
                                   #{mod\ 5372}#)))
                           (if #{rest\ 5374}#
                             (cons #{first\ 5373}# #{rest\ 5374}#)
                             #f))
                         #f))
                     (if (null? #{e\ 5369}#)
                       '()
                       (if (#{syntax-object?\ 3779}# #{e\ 5369}#)
                         (#{match-each\ 5342}#
                           (#{syntax-object-expression\ 3780}# #{e\ 5369}#)
                           #{p\ 5370}#
                           (#{join-wraps\ 3814}#
                             #{w\ 5371}#
                             (#{syntax-object-wrap\ 3781}# #{e\ 5369}#))
                           (#{syntax-object-module\ 3782}# #{e\ 5369}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 5375}# #{p\ 5376}#)
            (if (eq? #{p\ 5376}# (quote any))
              (list #{e\ 5375}#)
              (if (#{syntax-object?\ 3779}# #{e\ 5375}#)
                (#{match*\ 5345}#
                  (#{syntax-object-expression\ 3780}# #{e\ 5375}#)
                  #{p\ 5376}#
                  (#{syntax-object-wrap\ 3781}# #{e\ 5375}#)
                  '()
                  (#{syntax-object-module\ 3782}# #{e\ 5375}#))
                (#{match*\ 5345}#
                  #{e\ 5375}#
                  #{p\ 5376}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5377}#)
            ((lambda (#{tmp\ 5378}#)
               ((lambda (#{tmp\ 5379}#)
                  (if #{tmp\ 5379}#
                    (apply (lambda (#{_\ 5380}# #{e1\ 5381}# #{e2\ 5382}#)
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
                                   (cons #{e1\ 5381}# #{e2\ 5382}#)))
                           #{tmp\ 5379}#)
                    ((lambda (#{tmp\ 5384}#)
                       (if #{tmp\ 5384}#
                         (apply (lambda (#{_\ 5385}#
                                         #{out\ 5386}#
                                         #{in\ 5387}#
                                         #{e1\ 5388}#
                                         #{e2\ 5389}#)
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
                                        #{in\ 5387}#
                                        '()
                                        (list #{out\ 5386}#
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
                                                    (cons #{e1\ 5388}#
                                                          #{e2\ 5389}#)))))
                                #{tmp\ 5384}#)
                         ((lambda (#{tmp\ 5391}#)
                            (if #{tmp\ 5391}#
                              (apply (lambda (#{_\ 5392}#
                                              #{out\ 5393}#
                                              #{in\ 5394}#
                                              #{e1\ 5395}#
                                              #{e2\ 5396}#)
                                       (list '#(syntax-object
                                                syntax-case
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
                                                   #{in\ 5394}#)
                                             '()
                                             (list #{out\ 5393}#
                                                   (cons '#(syntax-object
                                                            begin
                                                            ((top)
                                                             #(ribcage
                                                               #(_
                                                                 out
                                                                 in
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
                                                            (hygiene guile))
                                                         (cons #{e1\ 5395}#
                                                               #{e2\ 5396}#)))))
                                     #{tmp\ 5391}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 5378}#)))
                          ($sc-dispatch
                            #{tmp\ 5378}#
                            '(any #(each (any any)) any . each-any)))))
                     ($sc-dispatch
                       #{tmp\ 5378}#
                       '(any ((any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 5378}#
                  '(any () any . each-any))))
             #{x\ 5377}#))
          (module-name (current-module)))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5400}#)
            ((lambda (#{tmp\ 5401}#)
               ((lambda (#{tmp\ 5402}#)
                  (if #{tmp\ 5402}#
                    (apply (lambda (#{_\ 5403}#
                                    #{k\ 5404}#
                                    #{keyword\ 5405}#
                                    #{pattern\ 5406}#
                                    #{template\ 5407}#)
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
                                                     #("i" "i" "i" "i" "i"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i")))
                                                  (hygiene guile))
                                               (cons #{k\ 5404}#
                                                     (map (lambda (#{tmp\ 5410}#
                                                                   #{tmp\ 5409}#)
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
                                                                        #{tmp\ 5409}#)
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
                                                                        #{tmp\ 5410}#)))
                                                          #{template\ 5407}#
                                                          #{pattern\ 5406}#))))))
                           #{tmp\ 5402}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5401}#)))
                ($sc-dispatch
                  #{tmp\ 5401}#
                  '(any each-any . #(each ((any . any) any))))))
             #{x\ 5400}#))
          (module-name (current-module)))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (cons (lambda (#{x\ 5411}#)
            ((lambda (#{tmp\ 5412}#)
               ((lambda (#{tmp\ 5413}#)
                  (if (if #{tmp\ 5413}#
                        (apply (lambda (#{let*\ 5414}#
                                        #{x\ 5415}#
                                        #{v\ 5416}#
                                        #{e1\ 5417}#
                                        #{e2\ 5418}#)
                                 (and-map identifier? #{x\ 5415}#))
                               #{tmp\ 5413}#)
                        #f)
                    (apply (lambda (#{let*\ 5420}#
                                    #{x\ 5421}#
                                    #{v\ 5422}#
                                    #{e1\ 5423}#
                                    #{e2\ 5424}#)
                             (letrec ((#{f\ 5425}#
                                        (lambda (#{bindings\ 5426}#)
                                          (if (null? #{bindings\ 5426}#)
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
                                                        (cons #{e1\ 5423}#
                                                              #{e2\ 5424}#)))
                                            ((lambda (#{tmp\ 5430}#)
                                               ((lambda (#{tmp\ 5431}#)
                                                  (if #{tmp\ 5431}#
                                                    (apply (lambda (#{body\ 5432}#
                                                                    #{binding\ 5433}#)
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
                                                                   (list #{binding\ 5433}#)
                                                                   #{body\ 5432}#))
                                                           #{tmp\ 5431}#)
                                                    (syntax-violation
                                                      #f
                                                      "source expression failed to match any pattern"
                                                      #{tmp\ 5430}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 5430}#
                                                  '(any any))))
                                             (list (#{f\ 5425}#
                                                     (cdr #{bindings\ 5426}#))
                                                   (car #{bindings\ 5426}#)))))))
                               (#{f\ 5425}#
                                 (map list #{x\ 5421}# #{v\ 5422}#))))
                           #{tmp\ 5413}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5412}#)))
                ($sc-dispatch
                  #{tmp\ 5412}#
                  '(any #(each (any any)) any . each-any))))
             #{x\ 5411}#))
          (module-name (current-module)))))

(define do
  (make-syncase-macro
    'macro
    (cons (lambda (#{orig-x\ 5434}#)
            ((lambda (#{tmp\ 5435}#)
               ((lambda (#{tmp\ 5436}#)
                  (if #{tmp\ 5436}#
                    (apply (lambda (#{_\ 5437}#
                                    #{var\ 5438}#
                                    #{init\ 5439}#
                                    #{step\ 5440}#
                                    #{e0\ 5441}#
                                    #{e1\ 5442}#
                                    #{c\ 5443}#)
                             ((lambda (#{tmp\ 5444}#)
                                ((lambda (#{tmp\ 5445}#)
                                   (if #{tmp\ 5445}#
                                     (apply (lambda (#{step\ 5446}#)
                                              ((lambda (#{tmp\ 5447}#)
                                                 ((lambda (#{tmp\ 5448}#)
                                                    (if #{tmp\ 5448}#
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
                                                                          #{var\ 5438}#
                                                                          #{init\ 5439}#)
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
                                                                                 #{e0\ 5441}#)
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
                                                                                   #{c\ 5443}#
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
                                                                                               #{step\ 5446}#)))))))
                                                             #{tmp\ 5448}#)
                                                      ((lambda (#{tmp\ 5453}#)
                                                         (if #{tmp\ 5453}#
                                                           (apply (lambda (#{e1\ 5454}#
                                                                           #{e2\ 5455}#)
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
                                                                               #{var\ 5438}#
                                                                               #{init\ 5439}#)
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
                                                                                #{e0\ 5441}#
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
                                                                                      (cons #{e1\ 5454}#
                                                                                            #{e2\ 5455}#))
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
                                                                                        #{c\ 5443}#
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
                                                                                                    #{step\ 5446}#)))))))
                                                                  #{tmp\ 5453}#)
                                                           (syntax-violation
                                                             #f
                                                             "source expression failed to match any pattern"
                                                             #{tmp\ 5447}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 5447}#
                                                         '(any . each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 5447}#
                                                    '())))
                                               #{e1\ 5442}#))
                                            #{tmp\ 5445}#)
                                     (syntax-violation
                                       #f
                                       "source expression failed to match any pattern"
                                       #{tmp\ 5444}#)))
                                 ($sc-dispatch
                                   #{tmp\ 5444}#
                                   'each-any)))
                              (map (lambda (#{v\ 5462}# #{s\ 5463}#)
                                     ((lambda (#{tmp\ 5464}#)
                                        ((lambda (#{tmp\ 5465}#)
                                           (if #{tmp\ 5465}#
                                             (apply (lambda () #{v\ 5462}#)
                                                    #{tmp\ 5465}#)
                                             ((lambda (#{tmp\ 5466}#)
                                                (if #{tmp\ 5466}#
                                                  (apply (lambda (#{e\ 5467}#)
                                                           #{e\ 5467}#)
                                                         #{tmp\ 5466}#)
                                                  ((lambda (#{_\ 5468}#)
                                                     (syntax-violation
                                                       'do
                                                       "bad step expression"
                                                       #{orig-x\ 5434}#
                                                       #{s\ 5463}#))
                                                   #{tmp\ 5464}#)))
                                              ($sc-dispatch
                                                #{tmp\ 5464}#
                                                '(any)))))
                                         ($sc-dispatch
                                           #{tmp\ 5464}#
                                           '())))
                                      #{s\ 5463}#))
                                   #{var\ 5438}#
                                   #{step\ 5440}#)))
                           #{tmp\ 5436}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5435}#)))
                ($sc-dispatch
                  #{tmp\ 5435}#
                  '(any #(each (any any . any))
                        (any . each-any)
                        .
                        each-any))))
             #{orig-x\ 5434}#))
          (module-name (current-module)))))

(define quasiquote
  (make-syncase-macro
    'macro
    (cons (letrec ((#{quasicons\ 5471}#
                     (lambda (#{x\ 5475}# #{y\ 5476}#)
                       ((lambda (#{tmp\ 5477}#)
                          ((lambda (#{tmp\ 5478}#)
                             (if #{tmp\ 5478}#
                               (apply (lambda (#{x\ 5479}# #{y\ 5480}#)
                                        ((lambda (#{tmp\ 5481}#)
                                           ((lambda (#{tmp\ 5482}#)
                                              (if #{tmp\ 5482}#
                                                (apply (lambda (#{dy\ 5483}#)
                                                         ((lambda (#{tmp\ 5484}#)
                                                            ((lambda (#{tmp\ 5485}#)
                                                               (if #{tmp\ 5485}#
                                                                 (apply (lambda (#{dx\ 5486}#)
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
                                                                                (cons #{dx\ 5486}#
                                                                                      #{dy\ 5483}#)))
                                                                        #{tmp\ 5485}#)
                                                                 ((lambda (#{_\ 5487}#)
                                                                    (if (null? #{dy\ 5483}#)
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
                                                                            #{x\ 5479}#)
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
                                                                            #{x\ 5479}#
                                                                            #{y\ 5480}#)))
                                                                  #{tmp\ 5484}#)))
                                                             ($sc-dispatch
                                                               #{tmp\ 5484}#
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
                                                                        #(x y)
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
                                                                       guile)))
                                                                 any))))
                                                          #{x\ 5479}#))
                                                       #{tmp\ 5482}#)
                                                ((lambda (#{tmp\ 5488}#)
                                                   (if #{tmp\ 5488}#
                                                     (apply (lambda (#{stuff\ 5489}#)
                                                              (cons '#(syntax-object
                                                                       list
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(stuff)
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
                                                                    (cons #{x\ 5479}#
                                                                          #{stuff\ 5489}#)))
                                                            #{tmp\ 5488}#)
                                                     ((lambda (#{else\ 5490}#)
                                                        (list '#(syntax-object
                                                                 cons
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(else)
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
                                                              #{x\ 5479}#
                                                              #{y\ 5480}#))
                                                      #{tmp\ 5481}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 5481}#
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
                                                            #("i"
                                                              "i"
                                                              "i"
                                                              "i")))
                                                         (hygiene guile)))
                                                     .
                                                     any)))))
                                            ($sc-dispatch
                                              #{tmp\ 5481}#
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
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i" "i" "i" "i")))
                                                    (hygiene guile)))
                                                any))))
                                         #{y\ 5480}#))
                                      #{tmp\ 5478}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 5477}#)))
                           ($sc-dispatch #{tmp\ 5477}# (quote (any any)))))
                        (list #{x\ 5475}# #{y\ 5476}#))))
                   (#{quasiappend\ 5472}#
                     (lambda (#{x\ 5491}# #{y\ 5492}#)
                       ((lambda (#{tmp\ 5493}#)
                          ((lambda (#{tmp\ 5494}#)
                             (if #{tmp\ 5494}#
                               (apply (lambda (#{x\ 5495}# #{y\ 5496}#)
                                        ((lambda (#{tmp\ 5497}#)
                                           ((lambda (#{tmp\ 5498}#)
                                              (if #{tmp\ 5498}#
                                                (apply (lambda () #{x\ 5495}#)
                                                       #{tmp\ 5498}#)
                                                ((lambda (#{_\ 5499}#)
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
                                                            (hygiene guile))
                                                         #{x\ 5495}#
                                                         #{y\ 5496}#))
                                                 #{tmp\ 5497}#)))
                                            ($sc-dispatch
                                              #{tmp\ 5497}#
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
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i" "i" "i" "i")))
                                                    (hygiene guile)))
                                                ()))))
                                         #{y\ 5496}#))
                                      #{tmp\ 5494}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 5493}#)))
                           ($sc-dispatch #{tmp\ 5493}# (quote (any any)))))
                        (list #{x\ 5491}# #{y\ 5492}#))))
                   (#{quasivector\ 5473}#
                     (lambda (#{x\ 5500}#)
                       ((lambda (#{tmp\ 5501}#)
                          ((lambda (#{x\ 5502}#)
                             ((lambda (#{tmp\ 5503}#)
                                ((lambda (#{tmp\ 5504}#)
                                   (if #{tmp\ 5504}#
                                     (apply (lambda (#{x\ 5505}#)
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
                                                          #((top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                          #("i" "i" "i" "i")))
                                                       (hygiene guile))
                                                    (list->vector
                                                      #{x\ 5505}#)))
                                            #{tmp\ 5504}#)
                                     ((lambda (#{tmp\ 5507}#)
                                        (if #{tmp\ 5507}#
                                          (apply (lambda (#{x\ 5508}#)
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
                                                         #{x\ 5508}#))
                                                 #{tmp\ 5507}#)
                                          ((lambda (#{_\ 5510}#)
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
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i" "i" "i" "i")))
                                                      (hygiene guile))
                                                   #{x\ 5502}#))
                                           #{tmp\ 5503}#)))
                                      ($sc-dispatch
                                        #{tmp\ 5503}#
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
                                   #{tmp\ 5503}#
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
                              #{x\ 5502}#))
                           #{tmp\ 5501}#))
                        #{x\ 5500}#)))
                   (#{quasi\ 5474}#
                     (lambda (#{p\ 5511}# #{lev\ 5512}#)
                       ((lambda (#{tmp\ 5513}#)
                          ((lambda (#{tmp\ 5514}#)
                             (if #{tmp\ 5514}#
                               (apply (lambda (#{p\ 5515}#)
                                        (if (= #{lev\ 5512}# 0)
                                          #{p\ 5515}#
                                          (#{quasicons\ 5471}#
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
                                                (hygiene guile))
                                              #(syntax-object
                                                unquote
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
                                                (hygiene guile)))
                                            (#{quasi\ 5474}#
                                              (list #{p\ 5515}#)
                                              (- #{lev\ 5512}# 1)))))
                                      #{tmp\ 5514}#)
                               ((lambda (#{tmp\ 5516}#)
                                  (if (if #{tmp\ 5516}#
                                        (apply (lambda (#{args\ 5517}#)
                                                 (= #{lev\ 5512}# 0))
                                               #{tmp\ 5516}#)
                                        #f)
                                    (apply (lambda (#{args\ 5518}#)
                                             (syntax-violation
                                               'unquote
                                               "unquote takes exactly one argument"
                                               #{p\ 5511}#
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
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i" "i" "i" "i")))
                                                        (hygiene guile))
                                                     #{args\ 5518}#)))
                                           #{tmp\ 5516}#)
                                    ((lambda (#{tmp\ 5519}#)
                                       (if #{tmp\ 5519}#
                                         (apply (lambda (#{p\ 5520}#
                                                         #{q\ 5521}#)
                                                  (if (= #{lev\ 5512}# 0)
                                                    (#{quasiappend\ 5472}#
                                                      #{p\ 5520}#
                                                      (#{quasi\ 5474}#
                                                        #{q\ 5521}#
                                                        #{lev\ 5512}#))
                                                    (#{quasicons\ 5471}#
                                                      (#{quasicons\ 5471}#
                                                        '(#(syntax-object
                                                            quote
                                                            ((top)
                                                             #(ribcage
                                                               #(p q)
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
                                                          #(syntax-object
                                                            unquote-splicing
                                                            ((top)
                                                             #(ribcage
                                                               #(p q)
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
                                                            (hygiene guile)))
                                                        (#{quasi\ 5474}#
                                                          (list #{p\ 5520}#)
                                                          (- #{lev\ 5512}# 1)))
                                                      (#{quasi\ 5474}#
                                                        #{q\ 5521}#
                                                        #{lev\ 5512}#))))
                                                #{tmp\ 5519}#)
                                         ((lambda (#{tmp\ 5522}#)
                                            (if (if #{tmp\ 5522}#
                                                  (apply (lambda (#{args\ 5523}#
                                                                  #{q\ 5524}#)
                                                           (= #{lev\ 5512}# 0))
                                                         #{tmp\ 5522}#)
                                                  #f)
                                              (apply (lambda (#{args\ 5525}#
                                                              #{q\ 5526}#)
                                                       (syntax-violation
                                                         'unquote-splicing
                                                         "unquote-splicing takes exactly one argument"
                                                         #{p\ 5511}#
                                                         (cons '#(syntax-object
                                                                  unquote-splicing
                                                                  ((top)
                                                                   #(ribcage
                                                                     #(args q)
                                                                     #((top)
                                                                       (top))
                                                                     #("i"
                                                                       "i"))
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
                                                               #{args\ 5525}#)))
                                                     #{tmp\ 5522}#)
                                              ((lambda (#{tmp\ 5527}#)
                                                 (if #{tmp\ 5527}#
                                                   (apply (lambda (#{p\ 5528}#)
                                                            (#{quasicons\ 5471}#
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
                                                                    guile)))
                                                              (#{quasi\ 5474}#
                                                                (list #{p\ 5528}#)
                                                                (+ #{lev\ 5512}#
                                                                   1))))
                                                          #{tmp\ 5527}#)
                                                   ((lambda (#{tmp\ 5529}#)
                                                      (if #{tmp\ 5529}#
                                                        (apply (lambda (#{p\ 5530}#
                                                                        #{q\ 5531}#)
                                                                 (#{quasicons\ 5471}#
                                                                   (#{quasi\ 5474}#
                                                                     #{p\ 5530}#
                                                                     #{lev\ 5512}#)
                                                                   (#{quasi\ 5474}#
                                                                     #{q\ 5531}#
                                                                     #{lev\ 5512}#)))
                                                               #{tmp\ 5529}#)
                                                        ((lambda (#{tmp\ 5532}#)
                                                           (if #{tmp\ 5532}#
                                                             (apply (lambda (#{x\ 5533}#)
                                                                      (#{quasivector\ 5473}#
                                                                        (#{quasi\ 5474}#
                                                                          #{x\ 5533}#
                                                                          #{lev\ 5512}#)))
                                                                    #{tmp\ 5532}#)
                                                             ((lambda (#{p\ 5535}#)
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
                                                                            #(p
                                                                              lev)
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
                                                                      #{p\ 5535}#))
                                                              #{tmp\ 5513}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 5513}#
                                                           '#(vector
                                                              each-any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 5513}#
                                                      '(any . any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 5513}#
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
                                                          #((top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                          #("i" "i" "i" "i")))
                                                       (hygiene guile)))
                                                   any)))))
                                          ($sc-dispatch
                                            #{tmp\ 5513}#
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
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i" "i" "i" "i")))
                                                   (hygiene guile)))
                                               .
                                               any)
                                              .
                                              any)))))
                                     ($sc-dispatch
                                       #{tmp\ 5513}#
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
                                  #{tmp\ 5513}#
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
                                           #(quasicons
                                             quasiappend
                                             quasivector
                                             quasi)
                                           #((top) (top) (top) (top))
                                           #("i" "i" "i" "i")))
                                        (hygiene guile)))
                                    .
                                    any)))))
                           ($sc-dispatch
                             #{tmp\ 5513}#
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
                                      #(quasicons
                                        quasiappend
                                        quasivector
                                        quasi)
                                      #((top) (top) (top) (top))
                                      #("i" "i" "i" "i")))
                                   (hygiene guile)))
                               any))))
                        #{p\ 5511}#))))
            (lambda (#{x\ 5536}#)
              ((lambda (#{tmp\ 5537}#)
                 ((lambda (#{tmp\ 5538}#)
                    (if #{tmp\ 5538}#
                      (apply (lambda (#{_\ 5539}# #{e\ 5540}#)
                               (#{quasi\ 5474}# #{e\ 5540}# 0))
                             #{tmp\ 5538}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 5537}#)))
                  ($sc-dispatch #{tmp\ 5537}# (quote (any any)))))
               #{x\ 5536}#)))
          (module-name (current-module)))))

(define include
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5541}#)
            (letrec ((#{read-file\ 5542}#
                       (lambda (#{fn\ 5543}# #{k\ 5544}#)
                         (let ((#{p\ 5545}# (open-input-file #{fn\ 5543}#)))
                           (letrec ((#{f\ 5546}#
                                      (lambda (#{x\ 5547}#)
                                        (if (eof-object? #{x\ 5547}#)
                                          (begin
                                            (close-input-port #{p\ 5545}#)
                                            '())
                                          (cons (datum->syntax
                                                  #{k\ 5544}#
                                                  #{x\ 5547}#)
                                                (#{f\ 5546}#
                                                  (read #{p\ 5545}#)))))))
                             (#{f\ 5546}# (read #{p\ 5545}#)))))))
              ((lambda (#{tmp\ 5548}#)
                 ((lambda (#{tmp\ 5549}#)
                    (if #{tmp\ 5549}#
                      (apply (lambda (#{k\ 5550}# #{filename\ 5551}#)
                               (let ((#{fn\ 5552}#
                                       (syntax->datum #{filename\ 5551}#)))
                                 ((lambda (#{tmp\ 5553}#)
                                    ((lambda (#{tmp\ 5554}#)
                                       (if #{tmp\ 5554}#
                                         (apply (lambda (#{exp\ 5555}#)
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
                                                        #{exp\ 5555}#))
                                                #{tmp\ 5554}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 5553}#)))
                                     ($sc-dispatch
                                       #{tmp\ 5553}#
                                       'each-any)))
                                  (#{read-file\ 5542}#
                                    #{fn\ 5552}#
                                    #{k\ 5550}#))))
                             #{tmp\ 5549}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 5548}#)))
                  ($sc-dispatch #{tmp\ 5548}# (quote (any any)))))
               #{x\ 5541}#)))
          (module-name (current-module)))))

(define include-from-path
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5557}#)
            ((lambda (#{tmp\ 5558}#)
               ((lambda (#{tmp\ 5559}#)
                  (if #{tmp\ 5559}#
                    (apply (lambda (#{k\ 5560}# #{filename\ 5561}#)
                             (let ((#{fn\ 5562}#
                                     (syntax->datum #{filename\ 5561}#)))
                               ((lambda (#{tmp\ 5563}#)
                                  ((lambda (#{fn\ 5564}#)
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
                                           #{fn\ 5564}#))
                                   #{tmp\ 5563}#))
                                (let ((#{t\ 5565}#
                                        (%search-load-path #{fn\ 5562}#)))
                                  (if #{t\ 5565}#
                                    #{t\ 5565}#
                                    (syntax-violation
                                      'include-from-path
                                      "file not found in path"
                                      #{x\ 5557}#
                                      #{filename\ 5561}#))))))
                           #{tmp\ 5559}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5558}#)))
                ($sc-dispatch #{tmp\ 5558}# (quote (any any)))))
             #{x\ 5557}#))
          (module-name (current-module)))))

(define unquote
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5566}#)
            ((lambda (#{tmp\ 5567}#)
               ((lambda (#{tmp\ 5568}#)
                  (if #{tmp\ 5568}#
                    (apply (lambda (#{_\ 5569}# #{e\ 5570}#)
                             (syntax-violation
                               'unquote
                               "expression not valid outside of quasiquote"
                               #{x\ 5566}#))
                           #{tmp\ 5568}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5567}#)))
                ($sc-dispatch #{tmp\ 5567}# (quote (any any)))))
             #{x\ 5566}#))
          (module-name (current-module)))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5571}#)
            ((lambda (#{tmp\ 5572}#)
               ((lambda (#{tmp\ 5573}#)
                  (if #{tmp\ 5573}#
                    (apply (lambda (#{_\ 5574}# #{e\ 5575}#)
                             (syntax-violation
                               'unquote-splicing
                               "expression not valid outside of quasiquote"
                               #{x\ 5571}#))
                           #{tmp\ 5573}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5572}#)))
                ($sc-dispatch #{tmp\ 5572}# (quote (any any)))))
             #{x\ 5571}#))
          (module-name (current-module)))))

(define case
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5576}#)
            ((lambda (#{tmp\ 5577}#)
               ((lambda (#{tmp\ 5578}#)
                  (if #{tmp\ 5578}#
                    (apply (lambda (#{_\ 5579}#
                                    #{e\ 5580}#
                                    #{m1\ 5581}#
                                    #{m2\ 5582}#)
                             ((lambda (#{tmp\ 5583}#)
                                ((lambda (#{body\ 5584}#)
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
                                                        (hygiene guile))
                                                     #{e\ 5580}#))
                                         #{body\ 5584}#))
                                 #{tmp\ 5583}#))
                              (letrec ((#{f\ 5585}#
                                         (lambda (#{clause\ 5586}#
                                                  #{clauses\ 5587}#)
                                           (if (null? #{clauses\ 5587}#)
                                             ((lambda (#{tmp\ 5589}#)
                                                ((lambda (#{tmp\ 5590}#)
                                                   (if #{tmp\ 5590}#
                                                     (apply (lambda (#{e1\ 5591}#
                                                                     #{e2\ 5592}#)
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
                                                                    (cons #{e1\ 5591}#
                                                                          #{e2\ 5592}#)))
                                                            #{tmp\ 5590}#)
                                                     ((lambda (#{tmp\ 5594}#)
                                                        (if #{tmp\ 5594}#
                                                          (apply (lambda (#{k\ 5595}#
                                                                          #{e1\ 5596}#
                                                                          #{e2\ 5597}#)
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
                                                                                     #{k\ 5595}#))
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
                                                                               (cons #{e1\ 5596}#
                                                                                     #{e2\ 5597}#))))
                                                                 #{tmp\ 5594}#)
                                                          ((lambda (#{_\ 5600}#)
                                                             (syntax-violation
                                                               'case
                                                               "bad clause"
                                                               #{x\ 5576}#
                                                               #{clause\ 5586}#))
                                                           #{tmp\ 5589}#)))
                                                      ($sc-dispatch
                                                        #{tmp\ 5589}#
                                                        '(each-any
                                                           any
                                                           .
                                                           each-any)))))
                                                 ($sc-dispatch
                                                   #{tmp\ 5589}#
                                                   '(#(free-id
                                                       #(syntax-object
                                                         else
                                                         ((top)
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
                                                         (hygiene guile)))
                                                     any
                                                     .
                                                     each-any))))
                                              #{clause\ 5586}#)
                                             ((lambda (#{tmp\ 5601}#)
                                                ((lambda (#{rest\ 5602}#)
                                                   ((lambda (#{tmp\ 5603}#)
                                                      ((lambda (#{tmp\ 5604}#)
                                                         (if #{tmp\ 5604}#
                                                           (apply (lambda (#{k\ 5605}#
                                                                           #{e1\ 5606}#
                                                                           #{e2\ 5607}#)
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
                                                                                      #{k\ 5605}#))
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
                                                                                (cons #{e1\ 5606}#
                                                                                      #{e2\ 5607}#))
                                                                          #{rest\ 5602}#))
                                                                  #{tmp\ 5604}#)
                                                           ((lambda (#{_\ 5610}#)
                                                              (syntax-violation
                                                                'case
                                                                "bad clause"
                                                                #{x\ 5576}#
                                                                #{clause\ 5586}#))
                                                            #{tmp\ 5603}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 5603}#
                                                         '(each-any
                                                            any
                                                            .
                                                            each-any))))
                                                    #{clause\ 5586}#))
                                                 #{tmp\ 5601}#))
                                              (#{f\ 5585}#
                                                (car #{clauses\ 5587}#)
                                                (cdr #{clauses\ 5587}#)))))))
                                (#{f\ 5585}# #{m1\ 5581}# #{m2\ 5582}#))))
                           #{tmp\ 5578}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5577}#)))
                ($sc-dispatch
                  #{tmp\ 5577}#
                  '(any any any . each-any))))
             #{x\ 5576}#))
          (module-name (current-module)))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5611}#)
            ((lambda (#{tmp\ 5612}#)
               ((lambda (#{tmp\ 5613}#)
                  (if #{tmp\ 5613}#
                    (apply (lambda (#{_\ 5614}# #{e\ 5615}#)
                             (list '#(syntax-object
                                      lambda
                                      ((top)
                                       #(ribcage
                                         #(_ e)
                                         #((top) (top))
                                         #("i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i")))
                                      (hygiene guile))
                                   '(#(syntax-object
                                       x
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i" "i"))
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
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i")))
                                                  (hygiene guile))
                                               '(#(syntax-object
                                                   identifier?
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
                                                 (#(syntax-object
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
                                                  #(syntax-object
                                                    id
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
                                                     #{e\ 5615}#))
                                         (list (cons #{_\ 5614}#
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
                                                     (cons #{e\ 5615}#
                                                           '(#(syntax-object
                                                               x
                                                               ((top)
                                                                #(ribcage
                                                                  #(_ e)
                                                                  #((top)
                                                                    (top))
                                                                  #("i" "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
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
                                                                  #((top)
                                                                    (top))
                                                                  #("i" "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(x)
                                                                  #((top))
                                                                  #("i")))
                                                               (hygiene
                                                                 guile)))))))))
                           #{tmp\ 5613}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5612}#)))
                ($sc-dispatch #{tmp\ 5612}# (quote (any any)))))
             #{x\ 5611}#))
          (module-name (current-module)))))

(define define*
  (make-syncase-macro
    'macro
    (cons (lambda (#{x\ 5616}#)
            ((lambda (#{tmp\ 5617}#)
               ((lambda (#{tmp\ 5618}#)
                  (if #{tmp\ 5618}#
                    (apply (lambda (#{dummy\ 5619}#
                                    #{id\ 5620}#
                                    #{args\ 5621}#
                                    #{b0\ 5622}#
                                    #{b1\ 5623}#)
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
                                   #{id\ 5620}#
                                   (cons '#(syntax-object
                                            lambda*
                                            ((top)
                                             #(ribcage
                                               #(dummy id args b0 b1)
                                               #(("m" top)
                                                 (top)
                                                 (top)
                                                 (top)
                                                 (top))
                                               #("i" "i" "i" "i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #(("m" top))
                                               #("i")))
                                            (hygiene guile))
                                         (cons #{args\ 5621}#
                                               (cons #{b0\ 5622}#
                                                     #{b1\ 5623}#)))))
                           #{tmp\ 5618}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 5617}#)))
                ($sc-dispatch
                  #{tmp\ 5617}#
                  '(any (any . any) any . each-any))))
             #{x\ 5616}#))
          (module-name (current-module)))))

