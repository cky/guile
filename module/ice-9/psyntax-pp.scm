(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((#{and-map*\ 261}#
           (lambda (#{f\ 299}# #{first\ 300}# . #{rest\ 301}#)
             (let ((#{t\ 302}# (null? #{first\ 300}#)))
               (if #{t\ 302}#
                 #{t\ 302}#
                 (if (null? #{rest\ 301}#)
                   (letrec ((#{andmap\ 303}#
                              (lambda (#{first\ 304}#)
                                (let ((#{x\ 305}# (car #{first\ 304}#))
                                      (#{first\ 306}# (cdr #{first\ 304}#)))
                                  (if (null? #{first\ 306}#)
                                    (#{f\ 299}# #{x\ 305}#)
                                    (if (#{f\ 299}# #{x\ 305}#)
                                      (#{andmap\ 303}# #{first\ 306}#)
                                      #f))))))
                     (#{andmap\ 303}# #{first\ 300}#))
                   (letrec ((#{andmap\ 307}#
                              (lambda (#{first\ 308}# #{rest\ 309}#)
                                (let ((#{x\ 310}# (car #{first\ 308}#))
                                      (#{xr\ 311}# (map car #{rest\ 309}#))
                                      (#{first\ 312}# (cdr #{first\ 308}#))
                                      (#{rest\ 313}# (map cdr #{rest\ 309}#)))
                                  (if (null? #{first\ 312}#)
                                    (apply #{f\ 299}#
                                           (cons #{x\ 310}# #{xr\ 311}#))
                                    (if (apply #{f\ 299}#
                                               (cons #{x\ 310}# #{xr\ 311}#))
                                      (#{andmap\ 307}#
                                        #{first\ 312}#
                                        #{rest\ 313}#)
                                      #f))))))
                     (#{andmap\ 307}# #{first\ 300}# #{rest\ 301}#))))))))
  (letrec ((#{lambda-var-list\ 412}#
             (lambda (#{vars\ 536}#)
               (letrec ((#{lvl\ 537}#
                          (lambda (#{vars\ 538}# #{ls\ 539}# #{w\ 540}#)
                            (if (pair? #{vars\ 538}#)
                              (#{lvl\ 537}#
                                (cdr #{vars\ 538}#)
                                (cons (#{wrap\ 389}#
                                        (car #{vars\ 538}#)
                                        #{w\ 540}#
                                        #f)
                                      #{ls\ 539}#)
                                #{w\ 540}#)
                              (if (#{id?\ 361}# #{vars\ 538}#)
                                (cons (#{wrap\ 389}#
                                        #{vars\ 538}#
                                        #{w\ 540}#
                                        #f)
                                      #{ls\ 539}#)
                                (if (null? #{vars\ 538}#)
                                  #{ls\ 539}#
                                  (if (#{syntax-object?\ 345}# #{vars\ 538}#)
                                    (#{lvl\ 537}#
                                      (#{syntax-object-expression\ 346}#
                                        #{vars\ 538}#)
                                      #{ls\ 539}#
                                      (#{join-wraps\ 380}#
                                        #{w\ 540}#
                                        (#{syntax-object-wrap\ 347}#
                                          #{vars\ 538}#)))
                                    (cons #{vars\ 538}# #{ls\ 539}#))))))))
                 (#{lvl\ 537}#
                   #{vars\ 536}#
                   '()
                   '(())))))
           (#{gen-var\ 411}#
             (lambda (#{id\ 541}#)
               (let ((#{id\ 542}#
                       (if (#{syntax-object?\ 345}# #{id\ 541}#)
                         (#{syntax-object-expression\ 346}# #{id\ 541}#)
                         #{id\ 541}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 542}#) " ")))))
           (#{strip\ 410}#
             (lambda (#{x\ 543}# #{w\ 544}#)
               (if (memq 'top
                         (#{wrap-marks\ 364}# #{w\ 544}#))
                 #{x\ 543}#
                 (letrec ((#{f\ 545}# (lambda (#{x\ 546}#)
                                        (if (#{syntax-object?\ 345}#
                                              #{x\ 546}#)
                                          (#{strip\ 410}#
                                            (#{syntax-object-expression\ 346}#
                                              #{x\ 546}#)
                                            (#{syntax-object-wrap\ 347}#
                                              #{x\ 546}#))
                                          (if (pair? #{x\ 546}#)
                                            (let ((#{a\ 547}# (#{f\ 545}# (car #{x\ 546}#)))
                                                  (#{d\ 548}# (#{f\ 545}# (cdr #{x\ 546}#))))
                                              (if (if (eq? #{a\ 547}#
                                                           (car #{x\ 546}#))
                                                    (eq? #{d\ 548}#
                                                         (cdr #{x\ 546}#))
                                                    #f)
                                                #{x\ 546}#
                                                (cons #{a\ 547}# #{d\ 548}#)))
                                            (if (vector? #{x\ 546}#)
                                              (let ((#{old\ 549}#
                                                      (vector->list
                                                        #{x\ 546}#)))
                                                (let ((#{new\ 550}#
                                                        (map #{f\ 545}#
                                                             #{old\ 549}#)))
                                                  (if (#{and-map*\ 261}#
                                                        eq?
                                                        #{old\ 549}#
                                                        #{new\ 550}#)
                                                    #{x\ 546}#
                                                    (list->vector
                                                      #{new\ 550}#))))
                                              #{x\ 546}#))))))
                   (#{f\ 545}# #{x\ 543}#)))))
           (#{chi-lambda-case\ 409}#
             (lambda (#{e\ 551}#
                      #{r\ 552}#
                      #{w\ 553}#
                      #{s\ 554}#
                      #{mod\ 555}#
                      #{get-formals\ 556}#
                      #{clauses\ 557}#)
               (letrec ((#{expand-body\ 561}#
                          (lambda (#{req\ 562}#
                                   #{opt\ 563}#
                                   #{rest\ 564}#
                                   #{kw\ 565}#
                                   #{body\ 566}#
                                   #{vars\ 567}#
                                   #{r*\ 568}#
                                   #{w*\ 569}#
                                   #{inits\ 570}#)
                            ((lambda (#{tmp\ 571}#)
                               ((lambda (#{tmp\ 572}#)
                                  (if (if #{tmp\ 572}#
                                        (apply (lambda (#{docstring\ 573}#
                                                        #{e1\ 574}#
                                                        #{e2\ 575}#)
                                                 (string?
                                                   (syntax->datum
                                                     #{docstring\ 573}#)))
                                               #{tmp\ 572}#)
                                        #f)
                                    (apply (lambda (#{docstring\ 576}#
                                                    #{e1\ 577}#
                                                    #{e2\ 578}#)
                                             (values
                                               (syntax->datum
                                                 #{docstring\ 576}#)
                                               #{req\ 562}#
                                               #{opt\ 563}#
                                               #{rest\ 564}#
                                               #{kw\ 565}#
                                               #{inits\ 570}#
                                               #{vars\ 567}#
                                               #f
                                               (#{chi-body\ 401}#
                                                 (cons #{e1\ 577}# #{e2\ 578}#)
                                                 (#{source-wrap\ 390}#
                                                   #{e\ 551}#
                                                   #{w\ 553}#
                                                   #{s\ 554}#
                                                   #{mod\ 555}#)
                                                 #{r*\ 568}#
                                                 #{w*\ 569}#
                                                 #{mod\ 555}#)))
                                           #{tmp\ 572}#)
                                    ((lambda (#{tmp\ 580}#)
                                       (if #{tmp\ 580}#
                                         (apply (lambda (#{e1\ 581}#
                                                         #{e2\ 582}#)
                                                  (values
                                                    #f
                                                    #{req\ 562}#
                                                    #{opt\ 563}#
                                                    #{rest\ 564}#
                                                    #{kw\ 565}#
                                                    #{inits\ 570}#
                                                    #{vars\ 567}#
                                                    #f
                                                    (#{chi-body\ 401}#
                                                      (cons #{e1\ 581}#
                                                            #{e2\ 582}#)
                                                      (#{source-wrap\ 390}#
                                                        #{e\ 551}#
                                                        #{w\ 553}#
                                                        #{s\ 554}#
                                                        #{mod\ 555}#)
                                                      #{r*\ 568}#
                                                      #{w*\ 569}#
                                                      #{mod\ 555}#)))
                                                #{tmp\ 580}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 571}#)))
                                     ($sc-dispatch
                                       #{tmp\ 571}#
                                       '(any . each-any)))))
                                ($sc-dispatch
                                  #{tmp\ 571}#
                                  '(any any . each-any))))
                             #{body\ 566}#)))
                        (#{expand-kw\ 560}#
                          (lambda (#{req\ 584}#
                                   #{opt\ 585}#
                                   #{rest\ 586}#
                                   #{kw\ 587}#
                                   #{body\ 588}#
                                   #{vars\ 589}#
                                   #{r*\ 590}#
                                   #{w*\ 591}#
                                   #{aok\ 592}#
                                   #{out\ 593}#
                                   #{inits\ 594}#)
                            (if (pair? #{kw\ 587}#)
                              ((lambda (#{tmp\ 595}#)
                                 ((lambda (#{tmp\ 596}#)
                                    (if #{tmp\ 596}#
                                      (apply (lambda (#{k\ 597}#
                                                      #{id\ 598}#
                                                      #{i\ 599}#)
                                               (let ((#{v\ 600}# (#{gen-var\ 411}#
                                                                   #{id\ 598}#)))
                                                 (let ((#{l\ 601}# (#{gen-labels\ 367}#
                                                                     (list #{v\ 600}#))))
                                                   (let ((#{r**\ 602}#
                                                           (#{extend-var-env\ 356}#
                                                             #{l\ 601}#
                                                             (list #{v\ 600}#)
                                                             #{r*\ 590}#)))
                                                     (let ((#{w**\ 603}#
                                                             (#{make-binding-wrap\ 378}#
                                                               (list #{id\ 598}#)
                                                               #{l\ 601}#
                                                               #{w*\ 591}#)))
                                                       (#{expand-kw\ 560}#
                                                         #{req\ 584}#
                                                         #{opt\ 585}#
                                                         #{rest\ 586}#
                                                         (cdr #{kw\ 587}#)
                                                         #{body\ 588}#
                                                         (cons #{v\ 600}#
                                                               #{vars\ 589}#)
                                                         #{r**\ 602}#
                                                         #{w**\ 603}#
                                                         #{aok\ 592}#
                                                         (cons (list (syntax->datum
                                                                       #{k\ 597}#)
                                                                     (syntax->datum
                                                                       #{id\ 598}#)
                                                                     #{v\ 600}#)
                                                               #{out\ 593}#)
                                                         (cons (#{chi\ 397}#
                                                                 #{i\ 599}#
                                                                 #{r*\ 590}#
                                                                 #{w*\ 591}#
                                                                 #{mod\ 555}#)
                                                               #{inits\ 594}#)))))))
                                             #{tmp\ 596}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 595}#)))
                                  ($sc-dispatch
                                    #{tmp\ 595}#
                                    '(any any any))))
                               (car #{kw\ 587}#))
                              (#{expand-body\ 561}#
                                #{req\ 584}#
                                #{opt\ 585}#
                                #{rest\ 586}#
                                (if (let ((#{t\ 604}# #{aok\ 592}#))
                                      (if #{t\ 604}#
                                        #{t\ 604}#
                                        (pair? #{out\ 593}#)))
                                  (cons #{aok\ 592}# (reverse #{out\ 593}#))
                                  #f)
                                #{body\ 588}#
                                (reverse #{vars\ 589}#)
                                #{r*\ 590}#
                                #{w*\ 591}#
                                (reverse #{inits\ 594}#)))))
                        (#{expand-opt\ 559}#
                          (lambda (#{req\ 605}#
                                   #{opt\ 606}#
                                   #{rest\ 607}#
                                   #{kw\ 608}#
                                   #{body\ 609}#
                                   #{vars\ 610}#
                                   #{r*\ 611}#
                                   #{w*\ 612}#
                                   #{out\ 613}#
                                   #{inits\ 614}#)
                            (if (pair? #{opt\ 606}#)
                              ((lambda (#{tmp\ 615}#)
                                 ((lambda (#{tmp\ 616}#)
                                    (if #{tmp\ 616}#
                                      (apply (lambda (#{id\ 617}# #{i\ 618}#)
                                               (let ((#{v\ 619}# (#{gen-var\ 411}#
                                                                   #{id\ 617}#)))
                                                 (let ((#{l\ 620}# (#{gen-labels\ 367}#
                                                                     (list #{v\ 619}#))))
                                                   (let ((#{r**\ 621}#
                                                           (#{extend-var-env\ 356}#
                                                             #{l\ 620}#
                                                             (list #{v\ 619}#)
                                                             #{r*\ 611}#)))
                                                     (let ((#{w**\ 622}#
                                                             (#{make-binding-wrap\ 378}#
                                                               (list #{id\ 617}#)
                                                               #{l\ 620}#
                                                               #{w*\ 612}#)))
                                                       (#{expand-opt\ 559}#
                                                         #{req\ 605}#
                                                         (cdr #{opt\ 606}#)
                                                         #{rest\ 607}#
                                                         #{kw\ 608}#
                                                         #{body\ 609}#
                                                         (cons #{v\ 619}#
                                                               #{vars\ 610}#)
                                                         #{r**\ 621}#
                                                         #{w**\ 622}#
                                                         (cons (syntax->datum
                                                                 #{id\ 617}#)
                                                               #{out\ 613}#)
                                                         (cons (#{chi\ 397}#
                                                                 #{i\ 618}#
                                                                 #{r*\ 611}#
                                                                 #{w*\ 612}#
                                                                 #{mod\ 555}#)
                                                               #{inits\ 614}#)))))))
                                             #{tmp\ 616}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 615}#)))
                                  ($sc-dispatch
                                    #{tmp\ 615}#
                                    '(any any))))
                               (car #{opt\ 606}#))
                              (if #{rest\ 607}#
                                (let ((#{v\ 623}# (#{gen-var\ 411}#
                                                    #{rest\ 607}#)))
                                  (let ((#{l\ 624}# (#{gen-labels\ 367}#
                                                      (list #{v\ 623}#))))
                                    (let ((#{r*\ 625}#
                                            (#{extend-var-env\ 356}#
                                              #{l\ 624}#
                                              (list #{v\ 623}#)
                                              #{r*\ 611}#)))
                                      (let ((#{w*\ 626}#
                                              (#{make-binding-wrap\ 378}#
                                                (list #{rest\ 607}#)
                                                #{l\ 624}#
                                                #{w*\ 612}#)))
                                        (#{expand-kw\ 560}#
                                          #{req\ 605}#
                                          (if (pair? #{out\ 613}#)
                                            (reverse #{out\ 613}#)
                                            #f)
                                          (syntax->datum #{rest\ 607}#)
                                          (if (pair? #{kw\ 608}#)
                                            (cdr #{kw\ 608}#)
                                            #{kw\ 608}#)
                                          #{body\ 609}#
                                          (cons #{v\ 623}# #{vars\ 610}#)
                                          #{r*\ 625}#
                                          #{w*\ 626}#
                                          (if (pair? #{kw\ 608}#)
                                            (car #{kw\ 608}#)
                                            #f)
                                          '()
                                          #{inits\ 614}#)))))
                                (#{expand-kw\ 560}#
                                  #{req\ 605}#
                                  (if (pair? #{out\ 613}#)
                                    (reverse #{out\ 613}#)
                                    #f)
                                  #f
                                  (if (pair? #{kw\ 608}#)
                                    (cdr #{kw\ 608}#)
                                    #{kw\ 608}#)
                                  #{body\ 609}#
                                  #{vars\ 610}#
                                  #{r*\ 611}#
                                  #{w*\ 612}#
                                  (if (pair? #{kw\ 608}#) (car #{kw\ 608}#) #f)
                                  '()
                                  #{inits\ 614}#)))))
                        (#{expand-req\ 558}#
                          (lambda (#{req\ 627}#
                                   #{opt\ 628}#
                                   #{rest\ 629}#
                                   #{kw\ 630}#
                                   #{body\ 631}#)
                            (let ((#{vars\ 632}#
                                    (map #{gen-var\ 411}# #{req\ 627}#))
                                  (#{labels\ 633}#
                                    (#{gen-labels\ 367}# #{req\ 627}#)))
                              (let ((#{r*\ 634}#
                                      (#{extend-var-env\ 356}#
                                        #{labels\ 633}#
                                        #{vars\ 632}#
                                        #{r\ 552}#))
                                    (#{w*\ 635}#
                                      (#{make-binding-wrap\ 378}#
                                        #{req\ 627}#
                                        #{labels\ 633}#
                                        #{w\ 553}#)))
                                (#{expand-opt\ 559}#
                                  (map syntax->datum #{req\ 627}#)
                                  #{opt\ 628}#
                                  #{rest\ 629}#
                                  #{kw\ 630}#
                                  #{body\ 631}#
                                  (reverse #{vars\ 632}#)
                                  #{r*\ 634}#
                                  #{w*\ 635}#
                                  '()
                                  '()))))))
                 ((lambda (#{tmp\ 636}#)
                    ((lambda (#{tmp\ 637}#)
                       (if #{tmp\ 637}#
                         (apply (lambda () (values #f #f)) #{tmp\ 637}#)
                         ((lambda (#{tmp\ 638}#)
                            (if #{tmp\ 638}#
                              (apply (lambda (#{args\ 639}#
                                              #{e1\ 640}#
                                              #{e2\ 641}#
                                              #{args*\ 642}#
                                              #{e1*\ 643}#
                                              #{e2*\ 644}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{get-formals\ 556}#
                                             #{args\ 639}#))
                                         (lambda (#{req\ 645}#
                                                  #{opt\ 646}#
                                                  #{rest\ 647}#
                                                  #{kw\ 648}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{expand-req\ 558}#
                                                 #{req\ 645}#
                                                 #{opt\ 646}#
                                                 #{rest\ 647}#
                                                 #{kw\ 648}#
                                                 (cons #{e1\ 640}#
                                                       #{e2\ 641}#)))
                                             (lambda (#{docstring\ 650}#
                                                      #{req\ 651}#
                                                      #{opt\ 652}#
                                                      #{rest\ 653}#
                                                      #{kw\ 654}#
                                                      #{inits\ 655}#
                                                      #{vars\ 656}#
                                                      #{pred\ 657}#
                                                      #{body\ 658}#)
                                               (call-with-values
                                                 (lambda ()
                                                   (#{chi-lambda-case\ 409}#
                                                     #{e\ 551}#
                                                     #{r\ 552}#
                                                     #{w\ 553}#
                                                     #{s\ 554}#
                                                     #{mod\ 555}#
                                                     #{get-formals\ 556}#
                                                     (map (lambda (#{tmp\ 661}#
                                                                   #{tmp\ 660}#
                                                                   #{tmp\ 659}#)
                                                            (cons #{tmp\ 659}#
                                                                  (cons #{tmp\ 660}#
                                                                        #{tmp\ 661}#)))
                                                          #{e2*\ 644}#
                                                          #{e1*\ 643}#
                                                          #{args*\ 642}#)))
                                                 (lambda (#{docstring*\ 663}#
                                                          #{else*\ 664}#)
                                                   (values
                                                     (let ((#{t\ 665}# #{docstring\ 650}#))
                                                       (if #{t\ 665}#
                                                         #{t\ 665}#
                                                         #{docstring*\ 663}#))
                                                     (#{build-lambda-case\ 337}#
                                                       #{s\ 554}#
                                                       #{req\ 651}#
                                                       #{opt\ 652}#
                                                       #{rest\ 653}#
                                                       #{kw\ 654}#
                                                       #{inits\ 655}#
                                                       #{vars\ 656}#
                                                       #{pred\ 657}#
                                                       #{body\ 658}#
                                                       #{else*\ 664}#)))))))))
                                     #{tmp\ 638}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 636}#)))
                          ($sc-dispatch
                            #{tmp\ 636}#
                            '((any any . each-any)
                              .
                              #(each (any any . each-any)))))))
                     ($sc-dispatch #{tmp\ 636}# (quote ()))))
                  #{clauses\ 557}#))))
           (#{lambda*-formals\ 408}#
             (lambda (#{orig-args\ 666}#)
               (letrec ((#{check\ 671}#
                          (lambda (#{req\ 672}#
                                   #{opt\ 673}#
                                   #{rest\ 674}#
                                   #{kw\ 675}#)
                            (if (#{distinct-bound-ids?\ 387}#
                                  (append
                                    #{req\ 672}#
                                    (map car #{opt\ 673}#)
                                    (if #{rest\ 674}#
                                      (list #{rest\ 674}#)
                                      '())
                                    (if (pair? #{kw\ 675}#)
                                      (map cadr (cdr #{kw\ 675}#))
                                      '())))
                              (values
                                #{req\ 672}#
                                #{opt\ 673}#
                                #{rest\ 674}#
                                #{kw\ 675}#)
                              (syntax-violation
                                'lambda*
                                "duplicate identifier in argument list"
                                #{orig-args\ 666}#))))
                        (#{rest\ 670}#
                          (lambda (#{args\ 676}#
                                   #{req\ 677}#
                                   #{opt\ 678}#
                                   #{kw\ 679}#)
                            ((lambda (#{tmp\ 680}#)
                               ((lambda (#{tmp\ 681}#)
                                  (if (if #{tmp\ 681}#
                                        (apply (lambda (#{r\ 682}#)
                                                 (#{id?\ 361}# #{r\ 682}#))
                                               #{tmp\ 681}#)
                                        #f)
                                    (apply (lambda (#{r\ 683}#)
                                             (#{check\ 671}#
                                               #{req\ 677}#
                                               #{opt\ 678}#
                                               #{r\ 683}#
                                               #{kw\ 679}#))
                                           #{tmp\ 681}#)
                                    ((lambda (#{else\ 684}#)
                                       (syntax-violation
                                         'lambda*
                                         "invalid rest argument"
                                         #{orig-args\ 666}#
                                         #{args\ 676}#))
                                     #{tmp\ 680}#)))
                                (list #{tmp\ 680}#)))
                             #{args\ 676}#)))
                        (#{key\ 669}#
                          (lambda (#{args\ 685}#
                                   #{req\ 686}#
                                   #{opt\ 687}#
                                   #{rkey\ 688}#)
                            ((lambda (#{tmp\ 689}#)
                               ((lambda (#{tmp\ 690}#)
                                  (if #{tmp\ 690}#
                                    (apply (lambda ()
                                             (#{check\ 671}#
                                               #{req\ 686}#
                                               #{opt\ 687}#
                                               #f
                                               (cons #f
                                                     (reverse #{rkey\ 688}#))))
                                           #{tmp\ 690}#)
                                    ((lambda (#{tmp\ 691}#)
                                       (if (if #{tmp\ 691}#
                                             (apply (lambda (#{a\ 692}#
                                                             #{b\ 693}#)
                                                      (#{id?\ 361}#
                                                        #{a\ 692}#))
                                                    #{tmp\ 691}#)
                                             #f)
                                         (apply (lambda (#{a\ 694}# #{b\ 695}#)
                                                  ((lambda (#{tmp\ 696}#)
                                                     ((lambda (#{k\ 697}#)
                                                        (#{key\ 669}#
                                                          #{b\ 695}#
                                                          #{req\ 686}#
                                                          #{opt\ 687}#
                                                          (cons (cons #{k\ 697}#
                                                                      (cons #{a\ 694}#
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
                                                                #{rkey\ 688}#)))
                                                      #{tmp\ 696}#))
                                                   (symbol->keyword
                                                     (syntax->datum
                                                       #{a\ 694}#))))
                                                #{tmp\ 691}#)
                                         ((lambda (#{tmp\ 698}#)
                                            (if (if #{tmp\ 698}#
                                                  (apply (lambda (#{a\ 699}#
                                                                  #{init\ 700}#
                                                                  #{b\ 701}#)
                                                           (#{id?\ 361}#
                                                             #{a\ 699}#))
                                                         #{tmp\ 698}#)
                                                  #f)
                                              (apply (lambda (#{a\ 702}#
                                                              #{init\ 703}#
                                                              #{b\ 704}#)
                                                       ((lambda (#{tmp\ 705}#)
                                                          ((lambda (#{k\ 706}#)
                                                             (#{key\ 669}#
                                                               #{b\ 704}#
                                                               #{req\ 686}#
                                                               #{opt\ 687}#
                                                               (cons (list #{k\ 706}#
                                                                           #{a\ 702}#
                                                                           #{init\ 703}#)
                                                                     #{rkey\ 688}#)))
                                                           #{tmp\ 705}#))
                                                        (symbol->keyword
                                                          (syntax->datum
                                                            #{a\ 702}#))))
                                                     #{tmp\ 698}#)
                                              ((lambda (#{tmp\ 707}#)
                                                 (if (if #{tmp\ 707}#
                                                       (apply (lambda (#{a\ 708}#
                                                                       #{init\ 709}#
                                                                       #{k\ 710}#
                                                                       #{b\ 711}#)
                                                                (if (#{id?\ 361}#
                                                                      #{a\ 708}#)
                                                                  (keyword?
                                                                    (syntax->datum
                                                                      #{k\ 710}#))
                                                                  #f))
                                                              #{tmp\ 707}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 712}#
                                                                   #{init\ 713}#
                                                                   #{k\ 714}#
                                                                   #{b\ 715}#)
                                                            (#{key\ 669}#
                                                              #{b\ 715}#
                                                              #{req\ 686}#
                                                              #{opt\ 687}#
                                                              (cons (list #{k\ 714}#
                                                                          #{a\ 712}#
                                                                          #{init\ 713}#)
                                                                    #{rkey\ 688}#)))
                                                          #{tmp\ 707}#)
                                                   ((lambda (#{tmp\ 716}#)
                                                      (if (if #{tmp\ 716}#
                                                            (apply (lambda (#{aok\ 717}#)
                                                                     (eq? (syntax->datum
                                                                            #{aok\ 717}#)
                                                                          #:allow-other-keys))
                                                                   #{tmp\ 716}#)
                                                            #f)
                                                        (apply (lambda (#{aok\ 718}#)
                                                                 (#{check\ 671}#
                                                                   #{req\ 686}#
                                                                   #{opt\ 687}#
                                                                   #f
                                                                   (cons #t
                                                                         (reverse
                                                                           #{rkey\ 688}#))))
                                                               #{tmp\ 716}#)
                                                        ((lambda (#{tmp\ 719}#)
                                                           (if (if #{tmp\ 719}#
                                                                 (apply (lambda (#{aok\ 720}#
                                                                                 #{a\ 721}#
                                                                                 #{b\ 722}#)
                                                                          (if (eq? (syntax->datum
                                                                                     #{aok\ 720}#)
                                                                                   #:allow-other-keys)
                                                                            (eq? (syntax->datum
                                                                                   #{a\ 721}#)
                                                                                 #:rest)
                                                                            #f))
                                                                        #{tmp\ 719}#)
                                                                 #f)
                                                             (apply (lambda (#{aok\ 723}#
                                                                             #{a\ 724}#
                                                                             #{b\ 725}#)
                                                                      (#{rest\ 670}#
                                                                        #{b\ 725}#
                                                                        #{req\ 686}#
                                                                        #{opt\ 687}#
                                                                        (cons #t
                                                                              (reverse
                                                                                #{rkey\ 688}#))))
                                                                    #{tmp\ 719}#)
                                                             ((lambda (#{tmp\ 726}#)
                                                                (if (if #{tmp\ 726}#
                                                                      (apply (lambda (#{aok\ 727}#
                                                                                      #{r\ 728}#)
                                                                               (if (eq? (syntax->datum
                                                                                          #{aok\ 727}#)
                                                                                        #:allow-other-keys)
                                                                                 (#{id?\ 361}#
                                                                                   #{r\ 728}#)
                                                                                 #f))
                                                                             #{tmp\ 726}#)
                                                                      #f)
                                                                  (apply (lambda (#{aok\ 729}#
                                                                                  #{r\ 730}#)
                                                                           (#{rest\ 670}#
                                                                             #{r\ 730}#
                                                                             #{req\ 686}#
                                                                             #{opt\ 687}#
                                                                             (cons #t
                                                                                   (reverse
                                                                                     #{rkey\ 688}#))))
                                                                         #{tmp\ 726}#)
                                                                  ((lambda (#{tmp\ 731}#)
                                                                     (if (if #{tmp\ 731}#
                                                                           (apply (lambda (#{a\ 732}#
                                                                                           #{b\ 733}#)
                                                                                    (eq? (syntax->datum
                                                                                           #{a\ 732}#)
                                                                                         #:rest))
                                                                                  #{tmp\ 731}#)
                                                                           #f)
                                                                       (apply (lambda (#{a\ 734}#
                                                                                       #{b\ 735}#)
                                                                                (#{rest\ 670}#
                                                                                  #{b\ 735}#
                                                                                  #{req\ 686}#
                                                                                  #{opt\ 687}#
                                                                                  (cons #f
                                                                                        (reverse
                                                                                          #{rkey\ 688}#))))
                                                                              #{tmp\ 731}#)
                                                                       ((lambda (#{tmp\ 736}#)
                                                                          (if (if #{tmp\ 736}#
                                                                                (apply (lambda (#{r\ 737}#)
                                                                                         (#{id?\ 361}#
                                                                                           #{r\ 737}#))
                                                                                       #{tmp\ 736}#)
                                                                                #f)
                                                                            (apply (lambda (#{r\ 738}#)
                                                                                     (#{rest\ 670}#
                                                                                       #{r\ 738}#
                                                                                       #{req\ 686}#
                                                                                       #{opt\ 687}#
                                                                                       (cons #f
                                                                                             (reverse
                                                                                               #{rkey\ 688}#))))
                                                                                   #{tmp\ 736}#)
                                                                            ((lambda (#{else\ 739}#)
                                                                               (syntax-violation
                                                                                 'lambda*
                                                                                 "invalid keyword argument list"
                                                                                 #{orig-args\ 666}#
                                                                                 #{args\ 685}#))
                                                                             #{tmp\ 689}#)))
                                                                        (list #{tmp\ 689}#))))
                                                                   ($sc-dispatch
                                                                     #{tmp\ 689}#
                                                                     '(any any)))))
                                                              ($sc-dispatch
                                                                #{tmp\ 689}#
                                                                '(any .
                                                                      any)))))
                                                         ($sc-dispatch
                                                           #{tmp\ 689}#
                                                           '(any any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 689}#
                                                      '(any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 689}#
                                                 '((any any any) . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 689}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 689}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 689}# (quote ()))))
                             #{args\ 685}#)))
                        (#{opt\ 668}#
                          (lambda (#{args\ 740}# #{req\ 741}# #{ropt\ 742}#)
                            ((lambda (#{tmp\ 743}#)
                               ((lambda (#{tmp\ 744}#)
                                  (if #{tmp\ 744}#
                                    (apply (lambda ()
                                             (#{check\ 671}#
                                               #{req\ 741}#
                                               (reverse #{ropt\ 742}#)
                                               #f
                                               '()))
                                           #{tmp\ 744}#)
                                    ((lambda (#{tmp\ 745}#)
                                       (if (if #{tmp\ 745}#
                                             (apply (lambda (#{a\ 746}#
                                                             #{b\ 747}#)
                                                      (#{id?\ 361}#
                                                        #{a\ 746}#))
                                                    #{tmp\ 745}#)
                                             #f)
                                         (apply (lambda (#{a\ 748}# #{b\ 749}#)
                                                  (#{opt\ 668}#
                                                    #{b\ 749}#
                                                    #{req\ 741}#
                                                    (cons (cons #{a\ 748}#
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
                                                          #{ropt\ 742}#)))
                                                #{tmp\ 745}#)
                                         ((lambda (#{tmp\ 750}#)
                                            (if (if #{tmp\ 750}#
                                                  (apply (lambda (#{a\ 751}#
                                                                  #{init\ 752}#
                                                                  #{b\ 753}#)
                                                           (#{id?\ 361}#
                                                             #{a\ 751}#))
                                                         #{tmp\ 750}#)
                                                  #f)
                                              (apply (lambda (#{a\ 754}#
                                                              #{init\ 755}#
                                                              #{b\ 756}#)
                                                       (#{opt\ 668}#
                                                         #{b\ 756}#
                                                         #{req\ 741}#
                                                         (cons (list #{a\ 754}#
                                                                     #{init\ 755}#)
                                                               #{ropt\ 742}#)))
                                                     #{tmp\ 750}#)
                                              ((lambda (#{tmp\ 757}#)
                                                 (if (if #{tmp\ 757}#
                                                       (apply (lambda (#{a\ 758}#
                                                                       #{b\ 759}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 758}#)
                                                                     #:key))
                                                              #{tmp\ 757}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 760}#
                                                                   #{b\ 761}#)
                                                            (#{key\ 669}#
                                                              #{b\ 761}#
                                                              #{req\ 741}#
                                                              (reverse
                                                                #{ropt\ 742}#)
                                                              '()))
                                                          #{tmp\ 757}#)
                                                   ((lambda (#{tmp\ 762}#)
                                                      (if (if #{tmp\ 762}#
                                                            (apply (lambda (#{a\ 763}#
                                                                            #{b\ 764}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 763}#)
                                                                          #:rest))
                                                                   #{tmp\ 762}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 765}#
                                                                        #{b\ 766}#)
                                                                 (#{rest\ 670}#
                                                                   #{b\ 766}#
                                                                   #{req\ 741}#
                                                                   (reverse
                                                                     #{ropt\ 742}#)
                                                                   '()))
                                                               #{tmp\ 762}#)
                                                        ((lambda (#{tmp\ 767}#)
                                                           (if (if #{tmp\ 767}#
                                                                 (apply (lambda (#{r\ 768}#)
                                                                          (#{id?\ 361}#
                                                                            #{r\ 768}#))
                                                                        #{tmp\ 767}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 769}#)
                                                                      (#{rest\ 670}#
                                                                        #{r\ 769}#
                                                                        #{req\ 741}#
                                                                        (reverse
                                                                          #{ropt\ 742}#)
                                                                        '()))
                                                                    #{tmp\ 767}#)
                                                             ((lambda (#{else\ 770}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid optional argument list"
                                                                  #{orig-args\ 666}#
                                                                  #{args\ 740}#))
                                                              #{tmp\ 743}#)))
                                                         (list #{tmp\ 743}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 743}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 743}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 743}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 743}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 743}# (quote ()))))
                             #{args\ 740}#)))
                        (#{req\ 667}#
                          (lambda (#{args\ 771}# #{rreq\ 772}#)
                            ((lambda (#{tmp\ 773}#)
                               ((lambda (#{tmp\ 774}#)
                                  (if #{tmp\ 774}#
                                    (apply (lambda ()
                                             (#{check\ 671}#
                                               (reverse #{rreq\ 772}#)
                                               '()
                                               #f
                                               '()))
                                           #{tmp\ 774}#)
                                    ((lambda (#{tmp\ 775}#)
                                       (if (if #{tmp\ 775}#
                                             (apply (lambda (#{a\ 776}#
                                                             #{b\ 777}#)
                                                      (#{id?\ 361}#
                                                        #{a\ 776}#))
                                                    #{tmp\ 775}#)
                                             #f)
                                         (apply (lambda (#{a\ 778}# #{b\ 779}#)
                                                  (#{req\ 667}#
                                                    #{b\ 779}#
                                                    (cons #{a\ 778}#
                                                          #{rreq\ 772}#)))
                                                #{tmp\ 775}#)
                                         ((lambda (#{tmp\ 780}#)
                                            (if (if #{tmp\ 780}#
                                                  (apply (lambda (#{a\ 781}#
                                                                  #{b\ 782}#)
                                                           (eq? (syntax->datum
                                                                  #{a\ 781}#)
                                                                #:optional))
                                                         #{tmp\ 780}#)
                                                  #f)
                                              (apply (lambda (#{a\ 783}#
                                                              #{b\ 784}#)
                                                       (#{opt\ 668}#
                                                         #{b\ 784}#
                                                         (reverse
                                                           #{rreq\ 772}#)
                                                         '()))
                                                     #{tmp\ 780}#)
                                              ((lambda (#{tmp\ 785}#)
                                                 (if (if #{tmp\ 785}#
                                                       (apply (lambda (#{a\ 786}#
                                                                       #{b\ 787}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 786}#)
                                                                     #:key))
                                                              #{tmp\ 785}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 788}#
                                                                   #{b\ 789}#)
                                                            (#{key\ 669}#
                                                              #{b\ 789}#
                                                              (reverse
                                                                #{rreq\ 772}#)
                                                              '()
                                                              '()))
                                                          #{tmp\ 785}#)
                                                   ((lambda (#{tmp\ 790}#)
                                                      (if (if #{tmp\ 790}#
                                                            (apply (lambda (#{a\ 791}#
                                                                            #{b\ 792}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 791}#)
                                                                          #:rest))
                                                                   #{tmp\ 790}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 793}#
                                                                        #{b\ 794}#)
                                                                 (#{rest\ 670}#
                                                                   #{b\ 794}#
                                                                   (reverse
                                                                     #{rreq\ 772}#)
                                                                   '()
                                                                   '()))
                                                               #{tmp\ 790}#)
                                                        ((lambda (#{tmp\ 795}#)
                                                           (if (if #{tmp\ 795}#
                                                                 (apply (lambda (#{r\ 796}#)
                                                                          (#{id?\ 361}#
                                                                            #{r\ 796}#))
                                                                        #{tmp\ 795}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 797}#)
                                                                      (#{rest\ 670}#
                                                                        #{r\ 797}#
                                                                        (reverse
                                                                          #{rreq\ 772}#)
                                                                        '()
                                                                        '()))
                                                                    #{tmp\ 795}#)
                                                             ((lambda (#{else\ 798}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid argument list"
                                                                  #{orig-args\ 666}#
                                                                  #{args\ 771}#))
                                                              #{tmp\ 773}#)))
                                                         (list #{tmp\ 773}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 773}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 773}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 773}#
                                            '(any . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 773}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 773}# (quote ()))))
                             #{args\ 771}#))))
                 (#{req\ 667}# #{orig-args\ 666}# (quote ())))))
           (#{chi-simple-lambda\ 407}#
             (lambda (#{e\ 799}#
                      #{r\ 800}#
                      #{w\ 801}#
                      #{s\ 802}#
                      #{mod\ 803}#
                      #{req\ 804}#
                      #{rest\ 805}#
                      #{docstring\ 806}#
                      #{body\ 807}#)
               (let ((#{ids\ 808}#
                       (if #{rest\ 805}#
                         (append #{req\ 804}# (list #{rest\ 805}#))
                         #{req\ 804}#)))
                 (let ((#{vars\ 809}#
                         (map #{gen-var\ 411}# #{ids\ 808}#)))
                   (let ((#{labels\ 810}#
                           (#{gen-labels\ 367}# #{ids\ 808}#)))
                     (#{build-simple-lambda\ 335}#
                       #{s\ 802}#
                       (map syntax->datum #{req\ 804}#)
                       (if #{rest\ 805}#
                         (syntax->datum #{rest\ 805}#)
                         #f)
                       #{vars\ 809}#
                       #{docstring\ 806}#
                       (#{chi-body\ 401}#
                         #{body\ 807}#
                         (#{source-wrap\ 390}#
                           #{e\ 799}#
                           #{w\ 801}#
                           #{s\ 802}#
                           #{mod\ 803}#)
                         (#{extend-var-env\ 356}#
                           #{labels\ 810}#
                           #{vars\ 809}#
                           #{r\ 800}#)
                         (#{make-binding-wrap\ 378}#
                           #{ids\ 808}#
                           #{labels\ 810}#
                           #{w\ 801}#)
                         #{mod\ 803}#)))))))
           (#{lambda-formals\ 406}#
             (lambda (#{orig-args\ 811}#)
               (letrec ((#{check\ 813}#
                          (lambda (#{req\ 814}# #{rest\ 815}#)
                            (if (#{distinct-bound-ids?\ 387}#
                                  (if #{rest\ 815}#
                                    (cons #{rest\ 815}# #{req\ 814}#)
                                    #{req\ 814}#))
                              (values #{req\ 814}# #f #{rest\ 815}# #f)
                              (syntax-violation
                                'lambda
                                "duplicate identifier in argument list"
                                #{orig-args\ 811}#))))
                        (#{req\ 812}#
                          (lambda (#{args\ 816}# #{rreq\ 817}#)
                            ((lambda (#{tmp\ 818}#)
                               ((lambda (#{tmp\ 819}#)
                                  (if #{tmp\ 819}#
                                    (apply (lambda ()
                                             (#{check\ 813}#
                                               (reverse #{rreq\ 817}#)
                                               #f))
                                           #{tmp\ 819}#)
                                    ((lambda (#{tmp\ 820}#)
                                       (if (if #{tmp\ 820}#
                                             (apply (lambda (#{a\ 821}#
                                                             #{b\ 822}#)
                                                      (#{id?\ 361}#
                                                        #{a\ 821}#))
                                                    #{tmp\ 820}#)
                                             #f)
                                         (apply (lambda (#{a\ 823}# #{b\ 824}#)
                                                  (#{req\ 812}#
                                                    #{b\ 824}#
                                                    (cons #{a\ 823}#
                                                          #{rreq\ 817}#)))
                                                #{tmp\ 820}#)
                                         ((lambda (#{tmp\ 825}#)
                                            (if (if #{tmp\ 825}#
                                                  (apply (lambda (#{r\ 826}#)
                                                           (#{id?\ 361}#
                                                             #{r\ 826}#))
                                                         #{tmp\ 825}#)
                                                  #f)
                                              (apply (lambda (#{r\ 827}#)
                                                       (#{check\ 813}#
                                                         (reverse
                                                           #{rreq\ 817}#)
                                                         #{r\ 827}#))
                                                     #{tmp\ 825}#)
                                              ((lambda (#{else\ 828}#)
                                                 (syntax-violation
                                                   'lambda
                                                   "invalid argument list"
                                                   #{orig-args\ 811}#
                                                   #{args\ 816}#))
                                               #{tmp\ 818}#)))
                                          (list #{tmp\ 818}#))))
                                     ($sc-dispatch
                                       #{tmp\ 818}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 818}# (quote ()))))
                             #{args\ 816}#))))
                 (#{req\ 812}# #{orig-args\ 811}# (quote ())))))
           (#{ellipsis?\ 405}#
             (lambda (#{x\ 829}#)
               (if (#{nonsymbol-id?\ 360}# #{x\ 829}#)
                 (#{free-id=?\ 384}#
                   #{x\ 829}#
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
           (#{chi-void\ 404}#
             (lambda () (#{build-void\ 325}# #f)))
           (#{eval-local-transformer\ 403}#
             (lambda (#{expanded\ 830}# #{mod\ 831}#)
               (let ((#{p\ 832}# (#{local-eval-hook\ 321}#
                                   #{expanded\ 830}#
                                   #{mod\ 831}#)))
                 (if (procedure? #{p\ 832}#)
                   #{p\ 832}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 832}#)))))
           (#{chi-local-syntax\ 402}#
             (lambda (#{rec?\ 833}#
                      #{e\ 834}#
                      #{r\ 835}#
                      #{w\ 836}#
                      #{s\ 837}#
                      #{mod\ 838}#
                      #{k\ 839}#)
               ((lambda (#{tmp\ 840}#)
                  ((lambda (#{tmp\ 841}#)
                     (if #{tmp\ 841}#
                       (apply (lambda (#{_\ 842}#
                                       #{id\ 843}#
                                       #{val\ 844}#
                                       #{e1\ 845}#
                                       #{e2\ 846}#)
                                (let ((#{ids\ 847}# #{id\ 843}#))
                                  (if (not (#{valid-bound-ids?\ 386}#
                                             #{ids\ 847}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 834}#)
                                    (let ((#{labels\ 849}#
                                            (#{gen-labels\ 367}#
                                              #{ids\ 847}#)))
                                      (let ((#{new-w\ 850}#
                                              (#{make-binding-wrap\ 378}#
                                                #{ids\ 847}#
                                                #{labels\ 849}#
                                                #{w\ 836}#)))
                                        (#{k\ 839}# (cons #{e1\ 845}#
                                                          #{e2\ 846}#)
                                                    (#{extend-env\ 355}#
                                                      #{labels\ 849}#
                                                      (let ((#{w\ 852}# (if #{rec?\ 833}#
                                                                          #{new-w\ 850}#
                                                                          #{w\ 836}#))
                                                            (#{trans-r\ 853}#
                                                              (#{macros-only-env\ 357}#
                                                                #{r\ 835}#)))
                                                        (map (lambda (#{x\ 854}#)
                                                               (cons 'macro
                                                                     (#{eval-local-transformer\ 403}#
                                                                       (#{chi\ 397}#
                                                                         #{x\ 854}#
                                                                         #{trans-r\ 853}#
                                                                         #{w\ 852}#
                                                                         #{mod\ 838}#)
                                                                       #{mod\ 838}#)))
                                                             #{val\ 844}#))
                                                      #{r\ 835}#)
                                                    #{new-w\ 850}#
                                                    #{s\ 837}#
                                                    #{mod\ 838}#))))))
                              #{tmp\ 841}#)
                       ((lambda (#{_\ 856}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 390}#
                              #{e\ 834}#
                              #{w\ 836}#
                              #{s\ 837}#
                              #{mod\ 838}#)))
                        #{tmp\ 840}#)))
                   ($sc-dispatch
                     #{tmp\ 840}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 834}#)))
           (#{chi-body\ 401}#
             (lambda (#{body\ 857}#
                      #{outer-form\ 858}#
                      #{r\ 859}#
                      #{w\ 860}#
                      #{mod\ 861}#)
               (let ((#{r\ 862}# (cons '("placeholder" placeholder)
                                       #{r\ 859}#)))
                 (let ((#{ribcage\ 863}#
                         (#{make-ribcage\ 368}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 864}# (#{make-wrap\ 363}#
                                       (#{wrap-marks\ 364}# #{w\ 860}#)
                                       (cons #{ribcage\ 863}#
                                             (#{wrap-subst\ 365}#
                                               #{w\ 860}#)))))
                     (letrec ((#{parse\ 865}#
                                (lambda (#{body\ 866}#
                                         #{ids\ 867}#
                                         #{labels\ 868}#
                                         #{var-ids\ 869}#
                                         #{vars\ 870}#
                                         #{vals\ 871}#
                                         #{bindings\ 872}#)
                                  (if (null? #{body\ 866}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 858}#)
                                    (let ((#{e\ 874}# (cdar #{body\ 866}#))
                                          (#{er\ 875}# (caar #{body\ 866}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 395}#
                                            #{e\ 874}#
                                            #{er\ 875}#
                                            '(())
                                            (#{source-annotation\ 352}#
                                              #{er\ 875}#)
                                            #{ribcage\ 863}#
                                            #{mod\ 861}#
                                            #f))
                                        (lambda (#{type\ 876}#
                                                 #{value\ 877}#
                                                 #{e\ 878}#
                                                 #{w\ 879}#
                                                 #{s\ 880}#
                                                 #{mod\ 881}#)
                                          (if (memv #{type\ 876}#
                                                    '(define-form))
                                            (let ((#{id\ 882}#
                                                    (#{wrap\ 389}#
                                                      #{value\ 877}#
                                                      #{w\ 879}#
                                                      #{mod\ 881}#))
                                                  (#{label\ 883}#
                                                    (#{gen-label\ 366}#)))
                                              (let ((#{var\ 884}#
                                                      (#{gen-var\ 411}#
                                                        #{id\ 882}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 377}#
                                                    #{ribcage\ 863}#
                                                    #{id\ 882}#
                                                    #{label\ 883}#)
                                                  (#{parse\ 865}#
                                                    (cdr #{body\ 866}#)
                                                    (cons #{id\ 882}#
                                                          #{ids\ 867}#)
                                                    (cons #{label\ 883}#
                                                          #{labels\ 868}#)
                                                    (cons #{id\ 882}#
                                                          #{var-ids\ 869}#)
                                                    (cons #{var\ 884}#
                                                          #{vars\ 870}#)
                                                    (cons (cons #{er\ 875}#
                                                                (#{wrap\ 389}#
                                                                  #{e\ 878}#
                                                                  #{w\ 879}#
                                                                  #{mod\ 881}#))
                                                          #{vals\ 871}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 884}#)
                                                          #{bindings\ 872}#)))))
                                            (if (memv #{type\ 876}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 885}#
                                                      (#{wrap\ 389}#
                                                        #{value\ 877}#
                                                        #{w\ 879}#
                                                        #{mod\ 881}#))
                                                    (#{label\ 886}#
                                                      (#{gen-label\ 366}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 377}#
                                                    #{ribcage\ 863}#
                                                    #{id\ 885}#
                                                    #{label\ 886}#)
                                                  (#{parse\ 865}#
                                                    (cdr #{body\ 866}#)
                                                    (cons #{id\ 885}#
                                                          #{ids\ 867}#)
                                                    (cons #{label\ 886}#
                                                          #{labels\ 868}#)
                                                    #{var-ids\ 869}#
                                                    #{vars\ 870}#
                                                    #{vals\ 871}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 875}#
                                                                      (#{wrap\ 389}#
                                                                        #{e\ 878}#
                                                                        #{w\ 879}#
                                                                        #{mod\ 881}#)))
                                                          #{bindings\ 872}#))))
                                              (if (memv #{type\ 876}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 887}#)
                                                   ((lambda (#{tmp\ 888}#)
                                                      (if #{tmp\ 888}#
                                                        (apply (lambda (#{_\ 889}#
                                                                        #{e1\ 890}#)
                                                                 (#{parse\ 865}#
                                                                   (letrec ((#{f\ 891}# (lambda (#{forms\ 892}#)
                                                                                          (if (null? #{forms\ 892}#)
                                                                                            (cdr #{body\ 866}#)
                                                                                            (cons (cons #{er\ 875}#
                                                                                                        (#{wrap\ 389}#
                                                                                                          (car #{forms\ 892}#)
                                                                                                          #{w\ 879}#
                                                                                                          #{mod\ 881}#))
                                                                                                  (#{f\ 891}# (cdr #{forms\ 892}#)))))))
                                                                     (#{f\ 891}# #{e1\ 890}#))
                                                                   #{ids\ 867}#
                                                                   #{labels\ 868}#
                                                                   #{var-ids\ 869}#
                                                                   #{vars\ 870}#
                                                                   #{vals\ 871}#
                                                                   #{bindings\ 872}#))
                                                               #{tmp\ 888}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 887}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 887}#
                                                      '(any . each-any))))
                                                 #{e\ 878}#)
                                                (if (memv #{type\ 876}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 402}#
                                                    #{value\ 877}#
                                                    #{e\ 878}#
                                                    #{er\ 875}#
                                                    #{w\ 879}#
                                                    #{s\ 880}#
                                                    #{mod\ 881}#
                                                    (lambda (#{forms\ 894}#
                                                             #{er\ 895}#
                                                             #{w\ 896}#
                                                             #{s\ 897}#
                                                             #{mod\ 898}#)
                                                      (#{parse\ 865}#
                                                        (letrec ((#{f\ 899}# (lambda (#{forms\ 900}#)
                                                                               (if (null? #{forms\ 900}#)
                                                                                 (cdr #{body\ 866}#)
                                                                                 (cons (cons #{er\ 895}#
                                                                                             (#{wrap\ 389}#
                                                                                               (car #{forms\ 900}#)
                                                                                               #{w\ 896}#
                                                                                               #{mod\ 898}#))
                                                                                       (#{f\ 899}# (cdr #{forms\ 900}#)))))))
                                                          (#{f\ 899}# #{forms\ 894}#))
                                                        #{ids\ 867}#
                                                        #{labels\ 868}#
                                                        #{var-ids\ 869}#
                                                        #{vars\ 870}#
                                                        #{vals\ 871}#
                                                        #{bindings\ 872}#)))
                                                  (if (null? #{ids\ 867}#)
                                                    (#{build-sequence\ 340}#
                                                      #f
                                                      (map (lambda (#{x\ 901}#)
                                                             (#{chi\ 397}#
                                                               (cdr #{x\ 901}#)
                                                               (car #{x\ 901}#)
                                                               '(())
                                                               #{mod\ 881}#))
                                                           (cons (cons #{er\ 875}#
                                                                       (#{source-wrap\ 390}#
                                                                         #{e\ 878}#
                                                                         #{w\ 879}#
                                                                         #{s\ 880}#
                                                                         #{mod\ 881}#))
                                                                 (cdr #{body\ 866}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 386}#
                                                                 #{ids\ 867}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 858}#))
                                                      (letrec ((#{loop\ 902}#
                                                                 (lambda (#{bs\ 903}#
                                                                          #{er-cache\ 904}#
                                                                          #{r-cache\ 905}#)
                                                                   (if (not (null? #{bs\ 903}#))
                                                                     (let ((#{b\ 906}# (car #{bs\ 903}#)))
                                                                       (if (eq? (car #{b\ 906}#)
                                                                                'macro)
                                                                         (let ((#{er\ 907}#
                                                                                 (cadr #{b\ 906}#)))
                                                                           (let ((#{r-cache\ 908}#
                                                                                   (if (eq? #{er\ 907}#
                                                                                            #{er-cache\ 904}#)
                                                                                     #{r-cache\ 905}#
                                                                                     (#{macros-only-env\ 357}#
                                                                                       #{er\ 907}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 906}#
                                                                                 (#{eval-local-transformer\ 403}#
                                                                                   (#{chi\ 397}#
                                                                                     (cddr #{b\ 906}#)
                                                                                     #{r-cache\ 908}#
                                                                                     '(())
                                                                                     #{mod\ 881}#)
                                                                                   #{mod\ 881}#))
                                                                               (#{loop\ 902}#
                                                                                 (cdr #{bs\ 903}#)
                                                                                 #{er\ 907}#
                                                                                 #{r-cache\ 908}#))))
                                                                         (#{loop\ 902}#
                                                                           (cdr #{bs\ 903}#)
                                                                           #{er-cache\ 904}#
                                                                           #{r-cache\ 905}#)))))))
                                                        (#{loop\ 902}#
                                                          #{bindings\ 872}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 862}#
                                                        (#{extend-env\ 355}#
                                                          #{labels\ 868}#
                                                          #{bindings\ 872}#
                                                          (cdr #{r\ 862}#)))
                                                      (#{build-letrec\ 343}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 869}#)
                                                        #{vars\ 870}#
                                                        (map (lambda (#{x\ 909}#)
                                                               (#{chi\ 397}#
                                                                 (cdr #{x\ 909}#)
                                                                 (car #{x\ 909}#)
                                                                 '(())
                                                                 #{mod\ 881}#))
                                                             #{vals\ 871}#)
                                                        (#{build-sequence\ 340}#
                                                          #f
                                                          (map (lambda (#{x\ 910}#)
                                                                 (#{chi\ 397}#
                                                                   (cdr #{x\ 910}#)
                                                                   (car #{x\ 910}#)
                                                                   '(())
                                                                   #{mod\ 881}#))
                                                               (cons (cons #{er\ 875}#
                                                                           (#{source-wrap\ 390}#
                                                                             #{e\ 878}#
                                                                             #{w\ 879}#
                                                                             #{s\ 880}#
                                                                             #{mod\ 881}#))
                                                                     (cdr #{body\ 866}#))))))))))))))))))
                       (#{parse\ 865}#
                         (map (lambda (#{x\ 873}#)
                                (cons #{r\ 862}#
                                      (#{wrap\ 389}#
                                        #{x\ 873}#
                                        #{w\ 864}#
                                        #{mod\ 861}#)))
                              #{body\ 857}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 400}#
             (lambda (#{p\ 911}#
                      #{e\ 912}#
                      #{r\ 913}#
                      #{w\ 914}#
                      #{rib\ 915}#
                      #{mod\ 916}#)
               (letrec ((#{rebuild-macro-output\ 917}#
                          (lambda (#{x\ 918}# #{m\ 919}#)
                            (if (pair? #{x\ 918}#)
                              (cons (#{rebuild-macro-output\ 917}#
                                      (car #{x\ 918}#)
                                      #{m\ 919}#)
                                    (#{rebuild-macro-output\ 917}#
                                      (cdr #{x\ 918}#)
                                      #{m\ 919}#))
                              (if (#{syntax-object?\ 345}# #{x\ 918}#)
                                (let ((#{w\ 920}# (#{syntax-object-wrap\ 347}#
                                                    #{x\ 918}#)))
                                  (let ((#{ms\ 921}#
                                          (#{wrap-marks\ 364}# #{w\ 920}#))
                                        (#{s\ 922}# (#{wrap-subst\ 365}#
                                                      #{w\ 920}#)))
                                    (if (if (pair? #{ms\ 921}#)
                                          (eq? (car #{ms\ 921}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 344}#
                                        (#{syntax-object-expression\ 346}#
                                          #{x\ 918}#)
                                        (#{make-wrap\ 363}#
                                          (cdr #{ms\ 921}#)
                                          (if #{rib\ 915}#
                                            (cons #{rib\ 915}#
                                                  (cdr #{s\ 922}#))
                                            (cdr #{s\ 922}#)))
                                        (#{syntax-object-module\ 348}#
                                          #{x\ 918}#))
                                      (#{make-syntax-object\ 344}#
                                        (#{syntax-object-expression\ 346}#
                                          #{x\ 918}#)
                                        (#{make-wrap\ 363}#
                                          (cons #{m\ 919}# #{ms\ 921}#)
                                          (if #{rib\ 915}#
                                            (cons #{rib\ 915}#
                                                  (cons 'shift
                                                        #{s\ 922}#))
                                            (cons (quote shift) #{s\ 922}#)))
                                        (let ((#{pmod\ 923}#
                                                (procedure-module #{p\ 911}#)))
                                          (if #{pmod\ 923}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 923}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 918}#)
                                  (let ((#{n\ 924}# (vector-length
                                                      #{x\ 918}#)))
                                    (let ((#{v\ 925}# (make-vector
                                                        #{n\ 924}#)))
                                      (letrec ((#{loop\ 926}#
                                                 (lambda (#{i\ 927}#)
                                                   (if (#{fx=\ 318}#
                                                         #{i\ 927}#
                                                         #{n\ 924}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 925}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 925}#
                                                         #{i\ 927}#
                                                         (#{rebuild-macro-output\ 917}#
                                                           (vector-ref
                                                             #{x\ 918}#
                                                             #{i\ 927}#)
                                                           #{m\ 919}#))
                                                       (#{loop\ 926}#
                                                         (#{fx+\ 316}#
                                                           #{i\ 927}#
                                                           1)))))))
                                        (#{loop\ 926}# 0))))
                                  (if (symbol? #{x\ 918}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 390}#
                                        #{e\ 912}#
                                        #{w\ 914}#
                                        (#{wrap-subst\ 365}# #{w\ 914}#)
                                        #{mod\ 916}#)
                                      #{x\ 918}#)
                                    #{x\ 918}#)))))))
                 (#{rebuild-macro-output\ 917}#
                   (#{p\ 911}# (#{wrap\ 389}#
                                 #{e\ 912}#
                                 (#{anti-mark\ 376}# #{w\ 914}#)
                                 #{mod\ 916}#))
                   (string #\m)))))
           (#{chi-application\ 399}#
             (lambda (#{x\ 928}#
                      #{e\ 929}#
                      #{r\ 930}#
                      #{w\ 931}#
                      #{s\ 932}#
                      #{mod\ 933}#)
               ((lambda (#{tmp\ 934}#)
                  ((lambda (#{tmp\ 935}#)
                     (if #{tmp\ 935}#
                       (apply (lambda (#{e0\ 936}# #{e1\ 937}#)
                                (#{build-application\ 326}#
                                  #{s\ 932}#
                                  #{x\ 928}#
                                  (map (lambda (#{e\ 938}#)
                                         (#{chi\ 397}#
                                           #{e\ 938}#
                                           #{r\ 930}#
                                           #{w\ 931}#
                                           #{mod\ 933}#))
                                       #{e1\ 937}#)))
                              #{tmp\ 935}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 934}#)))
                   ($sc-dispatch
                     #{tmp\ 934}#
                     '(any . each-any))))
                #{e\ 929}#)))
           (#{chi-expr\ 398}#
             (lambda (#{type\ 940}#
                      #{value\ 941}#
                      #{e\ 942}#
                      #{r\ 943}#
                      #{w\ 944}#
                      #{s\ 945}#
                      #{mod\ 946}#)
               (if (memv #{type\ 940}# (quote (lexical)))
                 (#{build-lexical-reference\ 328}#
                   'value
                   #{s\ 945}#
                   #{e\ 942}#
                   #{value\ 941}#)
                 (if (memv #{type\ 940}# (quote (core core-form)))
                   (#{value\ 941}#
                     #{e\ 942}#
                     #{r\ 943}#
                     #{w\ 944}#
                     #{s\ 945}#
                     #{mod\ 946}#)
                   (if (memv #{type\ 940}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 941}# #{e\ 942}#))
                       (lambda (#{id\ 947}# #{mod\ 948}#)
                         (#{build-global-reference\ 331}#
                           #{s\ 945}#
                           #{id\ 947}#
                           #{mod\ 948}#)))
                     (if (memv #{type\ 940}# (quote (lexical-call)))
                       (#{chi-application\ 399}#
                         (#{build-lexical-reference\ 328}#
                           'fun
                           (#{source-annotation\ 352}# (car #{e\ 942}#))
                           (car #{e\ 942}#)
                           #{value\ 941}#)
                         #{e\ 942}#
                         #{r\ 943}#
                         #{w\ 944}#
                         #{s\ 945}#
                         #{mod\ 946}#)
                       (if (memv #{type\ 940}# (quote (global-call)))
                         (#{chi-application\ 399}#
                           (#{build-global-reference\ 331}#
                             (#{source-annotation\ 352}# (car #{e\ 942}#))
                             (if (#{syntax-object?\ 345}# #{value\ 941}#)
                               (#{syntax-object-expression\ 346}#
                                 #{value\ 941}#)
                               #{value\ 941}#)
                             (if (#{syntax-object?\ 345}# #{value\ 941}#)
                               (#{syntax-object-module\ 348}# #{value\ 941}#)
                               #{mod\ 946}#))
                           #{e\ 942}#
                           #{r\ 943}#
                           #{w\ 944}#
                           #{s\ 945}#
                           #{mod\ 946}#)
                         (if (memv #{type\ 940}# (quote (constant)))
                           (#{build-data\ 339}#
                             #{s\ 945}#
                             (#{strip\ 410}#
                               (#{source-wrap\ 390}#
                                 #{e\ 942}#
                                 #{w\ 944}#
                                 #{s\ 945}#
                                 #{mod\ 946}#)
                               '(())))
                           (if (memv #{type\ 940}# (quote (global)))
                             (#{build-global-reference\ 331}#
                               #{s\ 945}#
                               #{value\ 941}#
                               #{mod\ 946}#)
                             (if (memv #{type\ 940}# (quote (call)))
                               (#{chi-application\ 399}#
                                 (#{chi\ 397}#
                                   (car #{e\ 942}#)
                                   #{r\ 943}#
                                   #{w\ 944}#
                                   #{mod\ 946}#)
                                 #{e\ 942}#
                                 #{r\ 943}#
                                 #{w\ 944}#
                                 #{s\ 945}#
                                 #{mod\ 946}#)
                               (if (memv #{type\ 940}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 949}#)
                                    ((lambda (#{tmp\ 950}#)
                                       (if #{tmp\ 950}#
                                         (apply (lambda (#{_\ 951}#
                                                         #{e1\ 952}#
                                                         #{e2\ 953}#)
                                                  (#{chi-sequence\ 391}#
                                                    (cons #{e1\ 952}#
                                                          #{e2\ 953}#)
                                                    #{r\ 943}#
                                                    #{w\ 944}#
                                                    #{s\ 945}#
                                                    #{mod\ 946}#))
                                                #{tmp\ 950}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 949}#)))
                                     ($sc-dispatch
                                       #{tmp\ 949}#
                                       '(any any . each-any))))
                                  #{e\ 942}#)
                                 (if (memv #{type\ 940}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 402}#
                                     #{value\ 941}#
                                     #{e\ 942}#
                                     #{r\ 943}#
                                     #{w\ 944}#
                                     #{s\ 945}#
                                     #{mod\ 946}#
                                     #{chi-sequence\ 391}#)
                                   (if (memv #{type\ 940}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 955}#)
                                        ((lambda (#{tmp\ 956}#)
                                           (if #{tmp\ 956}#
                                             (apply (lambda (#{_\ 957}#
                                                             #{x\ 958}#
                                                             #{e1\ 959}#
                                                             #{e2\ 960}#)
                                                      (let ((#{when-list\ 961}#
                                                              (#{chi-when-list\ 394}#
                                                                #{e\ 942}#
                                                                #{x\ 958}#
                                                                #{w\ 944}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 961}#)
                                                          (#{chi-sequence\ 391}#
                                                            (cons #{e1\ 959}#
                                                                  #{e2\ 960}#)
                                                            #{r\ 943}#
                                                            #{w\ 944}#
                                                            #{s\ 945}#
                                                            #{mod\ 946}#)
                                                          (#{chi-void\ 404}#))))
                                                    #{tmp\ 956}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 955}#)))
                                         ($sc-dispatch
                                           #{tmp\ 955}#
                                           '(any each-any any . each-any))))
                                      #{e\ 942}#)
                                     (if (memv #{type\ 940}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 942}#
                                         (#{wrap\ 389}#
                                           #{value\ 941}#
                                           #{w\ 944}#
                                           #{mod\ 946}#))
                                       (if (memv #{type\ 940}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 390}#
                                             #{e\ 942}#
                                             #{w\ 944}#
                                             #{s\ 945}#
                                             #{mod\ 946}#))
                                         (if (memv #{type\ 940}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 390}#
                                               #{e\ 942}#
                                               #{w\ 944}#
                                               #{s\ 945}#
                                               #{mod\ 946}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 390}#
                                               #{e\ 942}#
                                               #{w\ 944}#
                                               #{s\ 945}#
                                               #{mod\ 946}#))))))))))))))))))
           (#{chi\ 397}#
             (lambda (#{e\ 964}# #{r\ 965}# #{w\ 966}# #{mod\ 967}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 395}#
                     #{e\ 964}#
                     #{r\ 965}#
                     #{w\ 966}#
                     (#{source-annotation\ 352}# #{e\ 964}#)
                     #f
                     #{mod\ 967}#
                     #f))
                 (lambda (#{type\ 968}#
                          #{value\ 969}#
                          #{e\ 970}#
                          #{w\ 971}#
                          #{s\ 972}#
                          #{mod\ 973}#)
                   (#{chi-expr\ 398}#
                     #{type\ 968}#
                     #{value\ 969}#
                     #{e\ 970}#
                     #{r\ 965}#
                     #{w\ 971}#
                     #{s\ 972}#
                     #{mod\ 973}#)))))
           (#{chi-top\ 396}#
             (lambda (#{e\ 974}#
                      #{r\ 975}#
                      #{w\ 976}#
                      #{m\ 977}#
                      #{esew\ 978}#
                      #{mod\ 979}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 395}#
                     #{e\ 974}#
                     #{r\ 975}#
                     #{w\ 976}#
                     (#{source-annotation\ 352}# #{e\ 974}#)
                     #f
                     #{mod\ 979}#
                     #f))
                 (lambda (#{type\ 987}#
                          #{value\ 988}#
                          #{e\ 989}#
                          #{w\ 990}#
                          #{s\ 991}#
                          #{mod\ 992}#)
                   (if (memv #{type\ 987}# (quote (begin-form)))
                     ((lambda (#{tmp\ 993}#)
                        ((lambda (#{tmp\ 994}#)
                           (if #{tmp\ 994}#
                             (apply (lambda (#{_\ 995}#) (#{chi-void\ 404}#))
                                    #{tmp\ 994}#)
                             ((lambda (#{tmp\ 996}#)
                                (if #{tmp\ 996}#
                                  (apply (lambda (#{_\ 997}#
                                                  #{e1\ 998}#
                                                  #{e2\ 999}#)
                                           (#{chi-top-sequence\ 392}#
                                             (cons #{e1\ 998}# #{e2\ 999}#)
                                             #{r\ 975}#
                                             #{w\ 990}#
                                             #{s\ 991}#
                                             #{m\ 977}#
                                             #{esew\ 978}#
                                             #{mod\ 992}#))
                                         #{tmp\ 996}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 993}#)))
                              ($sc-dispatch
                                #{tmp\ 993}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 993}# (quote (any)))))
                      #{e\ 989}#)
                     (if (memv #{type\ 987}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 402}#
                         #{value\ 988}#
                         #{e\ 989}#
                         #{r\ 975}#
                         #{w\ 990}#
                         #{s\ 991}#
                         #{mod\ 992}#
                         (lambda (#{body\ 1001}#
                                  #{r\ 1002}#
                                  #{w\ 1003}#
                                  #{s\ 1004}#
                                  #{mod\ 1005}#)
                           (#{chi-top-sequence\ 392}#
                             #{body\ 1001}#
                             #{r\ 1002}#
                             #{w\ 1003}#
                             #{s\ 1004}#
                             #{m\ 977}#
                             #{esew\ 978}#
                             #{mod\ 1005}#)))
                       (if (memv #{type\ 987}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 1006}#)
                            ((lambda (#{tmp\ 1007}#)
                               (if #{tmp\ 1007}#
                                 (apply (lambda (#{_\ 1008}#
                                                 #{x\ 1009}#
                                                 #{e1\ 1010}#
                                                 #{e2\ 1011}#)
                                          (let ((#{when-list\ 1012}#
                                                  (#{chi-when-list\ 394}#
                                                    #{e\ 989}#
                                                    #{x\ 1009}#
                                                    #{w\ 990}#))
                                                (#{body\ 1013}#
                                                  (cons #{e1\ 1010}#
                                                        #{e2\ 1011}#)))
                                            (if (eq? #{m\ 977}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 1012}#)
                                                (#{chi-top-sequence\ 392}#
                                                  #{body\ 1013}#
                                                  #{r\ 975}#
                                                  #{w\ 990}#
                                                  #{s\ 991}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 992}#)
                                                (#{chi-void\ 404}#))
                                              (if (memq 'load
                                                        #{when-list\ 1012}#)
                                                (if (let ((#{t\ 1016}#
                                                            (memq 'compile
                                                                  #{when-list\ 1012}#)))
                                                      (if #{t\ 1016}#
                                                        #{t\ 1016}#
                                                        (if (eq? #{m\ 977}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 1012}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 392}#
                                                    #{body\ 1013}#
                                                    #{r\ 975}#
                                                    #{w\ 990}#
                                                    #{s\ 991}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 992}#)
                                                  (if (memq #{m\ 977}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 392}#
                                                      #{body\ 1013}#
                                                      #{r\ 975}#
                                                      #{w\ 990}#
                                                      #{s\ 991}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 992}#)
                                                    (#{chi-void\ 404}#)))
                                                (if (let ((#{t\ 1017}#
                                                            (memq 'compile
                                                                  #{when-list\ 1012}#)))
                                                      (if #{t\ 1017}#
                                                        #{t\ 1017}#
                                                        (if (eq? #{m\ 977}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 1012}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 320}#
                                                      (#{chi-top-sequence\ 392}#
                                                        #{body\ 1013}#
                                                        #{r\ 975}#
                                                        #{w\ 990}#
                                                        #{s\ 991}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 992}#)
                                                      #{mod\ 992}#)
                                                    (#{chi-void\ 404}#))
                                                  (#{chi-void\ 404}#))))))
                                        #{tmp\ 1007}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 1006}#)))
                             ($sc-dispatch
                               #{tmp\ 1006}#
                               '(any each-any any . each-any))))
                          #{e\ 989}#)
                         (if (memv #{type\ 987}# (quote (define-syntax-form)))
                           (let ((#{n\ 1018}#
                                   (#{id-var-name\ 383}#
                                     #{value\ 988}#
                                     #{w\ 990}#))
                                 (#{r\ 1019}#
                                   (#{macros-only-env\ 357}# #{r\ 975}#)))
                             (if (memv #{m\ 977}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 978}#)
                                 (let ((#{e\ 1020}#
                                         (#{chi-install-global\ 393}#
                                           #{n\ 1018}#
                                           (#{chi\ 397}#
                                             #{e\ 989}#
                                             #{r\ 1019}#
                                             #{w\ 990}#
                                             #{mod\ 992}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 320}#
                                       #{e\ 1020}#
                                       #{mod\ 992}#)
                                     (if (memq (quote load) #{esew\ 978}#)
                                       #{e\ 1020}#
                                       (#{chi-void\ 404}#))))
                                 (if (memq (quote load) #{esew\ 978}#)
                                   (#{chi-install-global\ 393}#
                                     #{n\ 1018}#
                                     (#{chi\ 397}#
                                       #{e\ 989}#
                                       #{r\ 1019}#
                                       #{w\ 990}#
                                       #{mod\ 992}#))
                                   (#{chi-void\ 404}#)))
                               (if (memv #{m\ 977}# (quote (c&e)))
                                 (let ((#{e\ 1021}#
                                         (#{chi-install-global\ 393}#
                                           #{n\ 1018}#
                                           (#{chi\ 397}#
                                             #{e\ 989}#
                                             #{r\ 1019}#
                                             #{w\ 990}#
                                             #{mod\ 992}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 320}#
                                       #{e\ 1021}#
                                       #{mod\ 992}#)
                                     #{e\ 1021}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 978}#)
                                     (#{top-level-eval-hook\ 320}#
                                       (#{chi-install-global\ 393}#
                                         #{n\ 1018}#
                                         (#{chi\ 397}#
                                           #{e\ 989}#
                                           #{r\ 1019}#
                                           #{w\ 990}#
                                           #{mod\ 992}#))
                                       #{mod\ 992}#))
                                   (#{chi-void\ 404}#)))))
                           (if (memv #{type\ 987}# (quote (define-form)))
                             (let ((#{n\ 1022}#
                                     (#{id-var-name\ 383}#
                                       #{value\ 988}#
                                       #{w\ 990}#)))
                               (let ((#{type\ 1023}#
                                       (#{binding-type\ 353}#
                                         (#{lookup\ 358}#
                                           #{n\ 1022}#
                                           #{r\ 975}#
                                           #{mod\ 992}#))))
                                 (if (memv #{type\ 1023}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 1022}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 1024}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 1022}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 1022}#
                                           (if (variable? #{old\ 1024}#)
                                             (variable-ref #{old\ 1024}#)
                                             #f))))
                                     (let ((#{x\ 1025}#
                                             (#{build-global-definition\ 334}#
                                               #{s\ 991}#
                                               #{n\ 1022}#
                                               (#{chi\ 397}#
                                                 #{e\ 989}#
                                                 #{r\ 975}#
                                                 #{w\ 990}#
                                                 #{mod\ 992}#))))
                                       (begin
                                         (if (eq? #{m\ 977}# (quote c&e))
                                           (#{top-level-eval-hook\ 320}#
                                             #{x\ 1025}#
                                             #{mod\ 992}#))
                                         #{x\ 1025}#)))
                                   (if (memv #{type\ 1023}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 989}#
                                       (#{wrap\ 389}#
                                         #{value\ 988}#
                                         #{w\ 990}#
                                         #{mod\ 992}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 989}#
                                       (#{wrap\ 389}#
                                         #{value\ 988}#
                                         #{w\ 990}#
                                         #{mod\ 992}#))))))
                             (let ((#{x\ 1026}#
                                     (#{chi-expr\ 398}#
                                       #{type\ 987}#
                                       #{value\ 988}#
                                       #{e\ 989}#
                                       #{r\ 975}#
                                       #{w\ 990}#
                                       #{s\ 991}#
                                       #{mod\ 992}#)))
                               (begin
                                 (if (eq? #{m\ 977}# (quote c&e))
                                   (#{top-level-eval-hook\ 320}#
                                     #{x\ 1026}#
                                     #{mod\ 992}#))
                                 #{x\ 1026}#)))))))))))
           (#{syntax-type\ 395}#
             (lambda (#{e\ 1027}#
                      #{r\ 1028}#
                      #{w\ 1029}#
                      #{s\ 1030}#
                      #{rib\ 1031}#
                      #{mod\ 1032}#
                      #{for-car?\ 1033}#)
               (if (symbol? #{e\ 1027}#)
                 (let ((#{n\ 1034}#
                         (#{id-var-name\ 383}# #{e\ 1027}# #{w\ 1029}#)))
                   (let ((#{b\ 1035}#
                           (#{lookup\ 358}#
                             #{n\ 1034}#
                             #{r\ 1028}#
                             #{mod\ 1032}#)))
                     (let ((#{type\ 1036}#
                             (#{binding-type\ 353}# #{b\ 1035}#)))
                       (if (memv #{type\ 1036}# (quote (lexical)))
                         (values
                           #{type\ 1036}#
                           (#{binding-value\ 354}# #{b\ 1035}#)
                           #{e\ 1027}#
                           #{w\ 1029}#
                           #{s\ 1030}#
                           #{mod\ 1032}#)
                         (if (memv #{type\ 1036}# (quote (global)))
                           (values
                             #{type\ 1036}#
                             #{n\ 1034}#
                             #{e\ 1027}#
                             #{w\ 1029}#
                             #{s\ 1030}#
                             #{mod\ 1032}#)
                           (if (memv #{type\ 1036}# (quote (macro)))
                             (if #{for-car?\ 1033}#
                               (values
                                 #{type\ 1036}#
                                 (#{binding-value\ 354}# #{b\ 1035}#)
                                 #{e\ 1027}#
                                 #{w\ 1029}#
                                 #{s\ 1030}#
                                 #{mod\ 1032}#)
                               (#{syntax-type\ 395}#
                                 (#{chi-macro\ 400}#
                                   (#{binding-value\ 354}# #{b\ 1035}#)
                                   #{e\ 1027}#
                                   #{r\ 1028}#
                                   #{w\ 1029}#
                                   #{rib\ 1031}#
                                   #{mod\ 1032}#)
                                 #{r\ 1028}#
                                 '(())
                                 #{s\ 1030}#
                                 #{rib\ 1031}#
                                 #{mod\ 1032}#
                                 #f))
                             (values
                               #{type\ 1036}#
                               (#{binding-value\ 354}# #{b\ 1035}#)
                               #{e\ 1027}#
                               #{w\ 1029}#
                               #{s\ 1030}#
                               #{mod\ 1032}#)))))))
                 (if (pair? #{e\ 1027}#)
                   (let ((#{first\ 1037}# (car #{e\ 1027}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 395}#
                           #{first\ 1037}#
                           #{r\ 1028}#
                           #{w\ 1029}#
                           #{s\ 1030}#
                           #{rib\ 1031}#
                           #{mod\ 1032}#
                           #t))
                       (lambda (#{ftype\ 1038}#
                                #{fval\ 1039}#
                                #{fe\ 1040}#
                                #{fw\ 1041}#
                                #{fs\ 1042}#
                                #{fmod\ 1043}#)
                         (if (memv #{ftype\ 1038}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 1039}#
                             #{e\ 1027}#
                             #{w\ 1029}#
                             #{s\ 1030}#
                             #{mod\ 1032}#)
                           (if (memv #{ftype\ 1038}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 344}#
                                 #{fval\ 1039}#
                                 #{w\ 1029}#
                                 #{fmod\ 1043}#)
                               #{e\ 1027}#
                               #{w\ 1029}#
                               #{s\ 1030}#
                               #{mod\ 1032}#)
                             (if (memv #{ftype\ 1038}# (quote (macro)))
                               (#{syntax-type\ 395}#
                                 (#{chi-macro\ 400}#
                                   #{fval\ 1039}#
                                   #{e\ 1027}#
                                   #{r\ 1028}#
                                   #{w\ 1029}#
                                   #{rib\ 1031}#
                                   #{mod\ 1032}#)
                                 #{r\ 1028}#
                                 '(())
                                 #{s\ 1030}#
                                 #{rib\ 1031}#
                                 #{mod\ 1032}#
                                 #{for-car?\ 1033}#)
                               (if (memv #{ftype\ 1038}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 1039}# #{e\ 1027}#))
                                   (lambda (#{sym\ 1044}# #{mod\ 1045}#)
                                     (#{syntax-type\ 395}#
                                       #{sym\ 1044}#
                                       #{r\ 1028}#
                                       #{w\ 1029}#
                                       #{s\ 1030}#
                                       #{rib\ 1031}#
                                       #{mod\ 1045}#
                                       #{for-car?\ 1033}#)))
                                 (if (memv #{ftype\ 1038}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 1039}#
                                     #{e\ 1027}#
                                     #{w\ 1029}#
                                     #{s\ 1030}#
                                     #{mod\ 1032}#)
                                   (if (memv #{ftype\ 1038}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 1039}#
                                       #{e\ 1027}#
                                       #{w\ 1029}#
                                       #{s\ 1030}#
                                       #{mod\ 1032}#)
                                     (if (memv #{ftype\ 1038}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 1027}#
                                         #{w\ 1029}#
                                         #{s\ 1030}#
                                         #{mod\ 1032}#)
                                       (if (memv #{ftype\ 1038}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 1027}#
                                           #{w\ 1029}#
                                           #{s\ 1030}#
                                           #{mod\ 1032}#)
                                         (if (memv #{ftype\ 1038}#
                                                   '(define))
                                           ((lambda (#{tmp\ 1046}#)
                                              ((lambda (#{tmp\ 1047}#)
                                                 (if (if #{tmp\ 1047}#
                                                       (apply (lambda (#{_\ 1048}#
                                                                       #{name\ 1049}#
                                                                       #{val\ 1050}#)
                                                                (#{id?\ 361}#
                                                                  #{name\ 1049}#))
                                                              #{tmp\ 1047}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 1051}#
                                                                   #{name\ 1052}#
                                                                   #{val\ 1053}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 1052}#
                                                              #{val\ 1053}#
                                                              #{w\ 1029}#
                                                              #{s\ 1030}#
                                                              #{mod\ 1032}#))
                                                          #{tmp\ 1047}#)
                                                   ((lambda (#{tmp\ 1054}#)
                                                      (if (if #{tmp\ 1054}#
                                                            (apply (lambda (#{_\ 1055}#
                                                                            #{name\ 1056}#
                                                                            #{args\ 1057}#
                                                                            #{e1\ 1058}#
                                                                            #{e2\ 1059}#)
                                                                     (if (#{id?\ 361}#
                                                                           #{name\ 1056}#)
                                                                       (#{valid-bound-ids?\ 386}#
                                                                         (#{lambda-var-list\ 412}#
                                                                           #{args\ 1057}#))
                                                                       #f))
                                                                   #{tmp\ 1054}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 1060}#
                                                                        #{name\ 1061}#
                                                                        #{args\ 1062}#
                                                                        #{e1\ 1063}#
                                                                        #{e2\ 1064}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 389}#
                                                                     #{name\ 1061}#
                                                                     #{w\ 1029}#
                                                                     #{mod\ 1032}#)
                                                                   (#{decorate-source\ 324}#
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
                                                                           (#{wrap\ 389}#
                                                                             (cons #{args\ 1062}#
                                                                                   (cons #{e1\ 1063}#
                                                                                         #{e2\ 1064}#))
                                                                             #{w\ 1029}#
                                                                             #{mod\ 1032}#))
                                                                     #{s\ 1030}#)
                                                                   '(())
                                                                   #{s\ 1030}#
                                                                   #{mod\ 1032}#))
                                                               #{tmp\ 1054}#)
                                                        ((lambda (#{tmp\ 1066}#)
                                                           (if (if #{tmp\ 1066}#
                                                                 (apply (lambda (#{_\ 1067}#
                                                                                 #{name\ 1068}#)
                                                                          (#{id?\ 361}#
                                                                            #{name\ 1068}#))
                                                                        #{tmp\ 1066}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 1069}#
                                                                             #{name\ 1070}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 389}#
                                                                          #{name\ 1070}#
                                                                          #{w\ 1029}#
                                                                          #{mod\ 1032}#)
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
                                                                        #{s\ 1030}#
                                                                        #{mod\ 1032}#))
                                                                    #{tmp\ 1066}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 1046}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 1046}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 1046}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 1046}#
                                                 '(any any any))))
                                            #{e\ 1027}#)
                                           (if (memv #{ftype\ 1038}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 1071}#)
                                                ((lambda (#{tmp\ 1072}#)
                                                   (if (if #{tmp\ 1072}#
                                                         (apply (lambda (#{_\ 1073}#
                                                                         #{name\ 1074}#
                                                                         #{val\ 1075}#)
                                                                  (#{id?\ 361}#
                                                                    #{name\ 1074}#))
                                                                #{tmp\ 1072}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 1076}#
                                                                     #{name\ 1077}#
                                                                     #{val\ 1078}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 1077}#
                                                                #{val\ 1078}#
                                                                #{w\ 1029}#
                                                                #{s\ 1030}#
                                                                #{mod\ 1032}#))
                                                            #{tmp\ 1072}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1071}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1071}#
                                                   '(any any any))))
                                              #{e\ 1027}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 1027}#
                                               #{w\ 1029}#
                                               #{s\ 1030}#
                                               #{mod\ 1032}#))))))))))))))
                   (if (#{syntax-object?\ 345}# #{e\ 1027}#)
                     (#{syntax-type\ 395}#
                       (#{syntax-object-expression\ 346}# #{e\ 1027}#)
                       #{r\ 1028}#
                       (#{join-wraps\ 380}#
                         #{w\ 1029}#
                         (#{syntax-object-wrap\ 347}# #{e\ 1027}#))
                       #{s\ 1030}#
                       #{rib\ 1031}#
                       (let ((#{t\ 1079}#
                               (#{syntax-object-module\ 348}# #{e\ 1027}#)))
                         (if #{t\ 1079}# #{t\ 1079}# #{mod\ 1032}#))
                       #{for-car?\ 1033}#)
                     (if (self-evaluating? #{e\ 1027}#)
                       (values
                         'constant
                         #f
                         #{e\ 1027}#
                         #{w\ 1029}#
                         #{s\ 1030}#
                         #{mod\ 1032}#)
                       (values
                         'other
                         #f
                         #{e\ 1027}#
                         #{w\ 1029}#
                         #{s\ 1030}#
                         #{mod\ 1032}#)))))))
           (#{chi-when-list\ 394}#
             (lambda (#{e\ 1080}# #{when-list\ 1081}# #{w\ 1082}#)
               (letrec ((#{f\ 1083}#
                          (lambda (#{when-list\ 1084}# #{situations\ 1085}#)
                            (if (null? #{when-list\ 1084}#)
                              #{situations\ 1085}#
                              (#{f\ 1083}#
                                (cdr #{when-list\ 1084}#)
                                (cons (let ((#{x\ 1086}#
                                              (car #{when-list\ 1084}#)))
                                        (if (#{free-id=?\ 384}#
                                              #{x\ 1086}#
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
                                          (if (#{free-id=?\ 384}#
                                                #{x\ 1086}#
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
                                            (if (#{free-id=?\ 384}#
                                                  #{x\ 1086}#
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
                                                #{e\ 1080}#
                                                (#{wrap\ 389}#
                                                  #{x\ 1086}#
                                                  #{w\ 1082}#
                                                  #f))))))
                                      #{situations\ 1085}#))))))
                 (#{f\ 1083}# #{when-list\ 1081}# (quote ())))))
           (#{chi-install-global\ 393}#
             (lambda (#{name\ 1087}# #{e\ 1088}#)
               (#{build-global-definition\ 334}#
                 #f
                 #{name\ 1087}#
                 (if (let ((#{v\ 1089}#
                             (module-variable
                               (current-module)
                               #{name\ 1087}#)))
                       (if #{v\ 1089}#
                         (if (variable-bound? #{v\ 1089}#)
                           (if (macro? (variable-ref #{v\ 1089}#))
                             (not (eq? (macro-type (variable-ref #{v\ 1089}#))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (#{build-application\ 326}#
                     #f
                     (#{build-primref\ 338}#
                       #f
                       'make-extended-syncase-macro)
                     (list (#{build-application\ 326}#
                             #f
                             (#{build-primref\ 338}# #f (quote module-ref))
                             (list (#{build-application\ 326}#
                                     #f
                                     (#{build-primref\ 338}#
                                       #f
                                       'current-module)
                                     '())
                                   (#{build-data\ 339}# #f #{name\ 1087}#)))
                           (#{build-data\ 339}# #f (quote macro))
                           #{e\ 1088}#))
                   (#{build-application\ 326}#
                     #f
                     (#{build-primref\ 338}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 339}# #f (quote macro))
                           #{e\ 1088}#))))))
           (#{chi-top-sequence\ 392}#
             (lambda (#{body\ 1090}#
                      #{r\ 1091}#
                      #{w\ 1092}#
                      #{s\ 1093}#
                      #{m\ 1094}#
                      #{esew\ 1095}#
                      #{mod\ 1096}#)
               (#{build-sequence\ 340}#
                 #{s\ 1093}#
                 (letrec ((#{dobody\ 1097}#
                            (lambda (#{body\ 1098}#
                                     #{r\ 1099}#
                                     #{w\ 1100}#
                                     #{m\ 1101}#
                                     #{esew\ 1102}#
                                     #{mod\ 1103}#)
                              (if (null? #{body\ 1098}#)
                                '()
                                (let ((#{first\ 1104}#
                                        (#{chi-top\ 396}#
                                          (car #{body\ 1098}#)
                                          #{r\ 1099}#
                                          #{w\ 1100}#
                                          #{m\ 1101}#
                                          #{esew\ 1102}#
                                          #{mod\ 1103}#)))
                                  (cons #{first\ 1104}#
                                        (#{dobody\ 1097}#
                                          (cdr #{body\ 1098}#)
                                          #{r\ 1099}#
                                          #{w\ 1100}#
                                          #{m\ 1101}#
                                          #{esew\ 1102}#
                                          #{mod\ 1103}#)))))))
                   (#{dobody\ 1097}#
                     #{body\ 1090}#
                     #{r\ 1091}#
                     #{w\ 1092}#
                     #{m\ 1094}#
                     #{esew\ 1095}#
                     #{mod\ 1096}#)))))
           (#{chi-sequence\ 391}#
             (lambda (#{body\ 1105}#
                      #{r\ 1106}#
                      #{w\ 1107}#
                      #{s\ 1108}#
                      #{mod\ 1109}#)
               (#{build-sequence\ 340}#
                 #{s\ 1108}#
                 (letrec ((#{dobody\ 1110}#
                            (lambda (#{body\ 1111}#
                                     #{r\ 1112}#
                                     #{w\ 1113}#
                                     #{mod\ 1114}#)
                              (if (null? #{body\ 1111}#)
                                '()
                                (let ((#{first\ 1115}#
                                        (#{chi\ 397}#
                                          (car #{body\ 1111}#)
                                          #{r\ 1112}#
                                          #{w\ 1113}#
                                          #{mod\ 1114}#)))
                                  (cons #{first\ 1115}#
                                        (#{dobody\ 1110}#
                                          (cdr #{body\ 1111}#)
                                          #{r\ 1112}#
                                          #{w\ 1113}#
                                          #{mod\ 1114}#)))))))
                   (#{dobody\ 1110}#
                     #{body\ 1105}#
                     #{r\ 1106}#
                     #{w\ 1107}#
                     #{mod\ 1109}#)))))
           (#{source-wrap\ 390}#
             (lambda (#{x\ 1116}#
                      #{w\ 1117}#
                      #{s\ 1118}#
                      #{defmod\ 1119}#)
               (#{wrap\ 389}#
                 (#{decorate-source\ 324}#
                   #{x\ 1116}#
                   #{s\ 1118}#)
                 #{w\ 1117}#
                 #{defmod\ 1119}#)))
           (#{wrap\ 389}#
             (lambda (#{x\ 1120}# #{w\ 1121}# #{defmod\ 1122}#)
               (if (if (null? (#{wrap-marks\ 364}# #{w\ 1121}#))
                     (null? (#{wrap-subst\ 365}# #{w\ 1121}#))
                     #f)
                 #{x\ 1120}#
                 (if (#{syntax-object?\ 345}# #{x\ 1120}#)
                   (#{make-syntax-object\ 344}#
                     (#{syntax-object-expression\ 346}# #{x\ 1120}#)
                     (#{join-wraps\ 380}#
                       #{w\ 1121}#
                       (#{syntax-object-wrap\ 347}# #{x\ 1120}#))
                     (#{syntax-object-module\ 348}# #{x\ 1120}#))
                   (if (null? #{x\ 1120}#)
                     #{x\ 1120}#
                     (#{make-syntax-object\ 344}#
                       #{x\ 1120}#
                       #{w\ 1121}#
                       #{defmod\ 1122}#))))))
           (#{bound-id-member?\ 388}#
             (lambda (#{x\ 1123}# #{list\ 1124}#)
               (if (not (null? #{list\ 1124}#))
                 (let ((#{t\ 1125}#
                         (#{bound-id=?\ 385}#
                           #{x\ 1123}#
                           (car #{list\ 1124}#))))
                   (if #{t\ 1125}#
                     #{t\ 1125}#
                     (#{bound-id-member?\ 388}#
                       #{x\ 1123}#
                       (cdr #{list\ 1124}#))))
                 #f)))
           (#{distinct-bound-ids?\ 387}#
             (lambda (#{ids\ 1126}#)
               (letrec ((#{distinct?\ 1127}#
                          (lambda (#{ids\ 1128}#)
                            (let ((#{t\ 1129}# (null? #{ids\ 1128}#)))
                              (if #{t\ 1129}#
                                #{t\ 1129}#
                                (if (not (#{bound-id-member?\ 388}#
                                           (car #{ids\ 1128}#)
                                           (cdr #{ids\ 1128}#)))
                                  (#{distinct?\ 1127}# (cdr #{ids\ 1128}#))
                                  #f))))))
                 (#{distinct?\ 1127}# #{ids\ 1126}#))))
           (#{valid-bound-ids?\ 386}#
             (lambda (#{ids\ 1130}#)
               (if (letrec ((#{all-ids?\ 1131}#
                              (lambda (#{ids\ 1132}#)
                                (let ((#{t\ 1133}# (null? #{ids\ 1132}#)))
                                  (if #{t\ 1133}#
                                    #{t\ 1133}#
                                    (if (#{id?\ 361}# (car #{ids\ 1132}#))
                                      (#{all-ids?\ 1131}# (cdr #{ids\ 1132}#))
                                      #f))))))
                     (#{all-ids?\ 1131}# #{ids\ 1130}#))
                 (#{distinct-bound-ids?\ 387}# #{ids\ 1130}#)
                 #f)))
           (#{bound-id=?\ 385}#
             (lambda (#{i\ 1134}# #{j\ 1135}#)
               (if (if (#{syntax-object?\ 345}# #{i\ 1134}#)
                     (#{syntax-object?\ 345}# #{j\ 1135}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 346}# #{i\ 1134}#)
                          (#{syntax-object-expression\ 346}# #{j\ 1135}#))
                   (#{same-marks?\ 382}#
                     (#{wrap-marks\ 364}#
                       (#{syntax-object-wrap\ 347}# #{i\ 1134}#))
                     (#{wrap-marks\ 364}#
                       (#{syntax-object-wrap\ 347}# #{j\ 1135}#)))
                   #f)
                 (eq? #{i\ 1134}# #{j\ 1135}#))))
           (#{free-id=?\ 384}#
             (lambda (#{i\ 1136}# #{j\ 1137}#)
               (if (eq? (let ((#{x\ 1138}# #{i\ 1136}#))
                          (if (#{syntax-object?\ 345}# #{x\ 1138}#)
                            (#{syntax-object-expression\ 346}# #{x\ 1138}#)
                            #{x\ 1138}#))
                        (let ((#{x\ 1139}# #{j\ 1137}#))
                          (if (#{syntax-object?\ 345}# #{x\ 1139}#)
                            (#{syntax-object-expression\ 346}# #{x\ 1139}#)
                            #{x\ 1139}#)))
                 (eq? (#{id-var-name\ 383}# #{i\ 1136}# (quote (())))
                      (#{id-var-name\ 383}# #{j\ 1137}# (quote (()))))
                 #f)))
           (#{id-var-name\ 383}#
             (lambda (#{id\ 1140}# #{w\ 1141}#)
               (letrec ((#{search-vector-rib\ 1144}#
                          (lambda (#{sym\ 1150}#
                                   #{subst\ 1151}#
                                   #{marks\ 1152}#
                                   #{symnames\ 1153}#
                                   #{ribcage\ 1154}#)
                            (let ((#{n\ 1155}#
                                    (vector-length #{symnames\ 1153}#)))
                              (letrec ((#{f\ 1156}#
                                         (lambda (#{i\ 1157}#)
                                           (if (#{fx=\ 318}#
                                                 #{i\ 1157}#
                                                 #{n\ 1155}#)
                                             (#{search\ 1142}#
                                               #{sym\ 1150}#
                                               (cdr #{subst\ 1151}#)
                                               #{marks\ 1152}#)
                                             (if (if (eq? (vector-ref
                                                            #{symnames\ 1153}#
                                                            #{i\ 1157}#)
                                                          #{sym\ 1150}#)
                                                   (#{same-marks?\ 382}#
                                                     #{marks\ 1152}#
                                                     (vector-ref
                                                       (#{ribcage-marks\ 371}#
                                                         #{ribcage\ 1154}#)
                                                       #{i\ 1157}#))
                                                   #f)
                                               (values
                                                 (vector-ref
                                                   (#{ribcage-labels\ 372}#
                                                     #{ribcage\ 1154}#)
                                                   #{i\ 1157}#)
                                                 #{marks\ 1152}#)
                                               (#{f\ 1156}#
                                                 (#{fx+\ 316}#
                                                   #{i\ 1157}#
                                                   1)))))))
                                (#{f\ 1156}# 0)))))
                        (#{search-list-rib\ 1143}#
                          (lambda (#{sym\ 1158}#
                                   #{subst\ 1159}#
                                   #{marks\ 1160}#
                                   #{symnames\ 1161}#
                                   #{ribcage\ 1162}#)
                            (letrec ((#{f\ 1163}#
                                       (lambda (#{symnames\ 1164}# #{i\ 1165}#)
                                         (if (null? #{symnames\ 1164}#)
                                           (#{search\ 1142}#
                                             #{sym\ 1158}#
                                             (cdr #{subst\ 1159}#)
                                             #{marks\ 1160}#)
                                           (if (if (eq? (car #{symnames\ 1164}#)
                                                        #{sym\ 1158}#)
                                                 (#{same-marks?\ 382}#
                                                   #{marks\ 1160}#
                                                   (list-ref
                                                     (#{ribcage-marks\ 371}#
                                                       #{ribcage\ 1162}#)
                                                     #{i\ 1165}#))
                                                 #f)
                                             (values
                                               (list-ref
                                                 (#{ribcage-labels\ 372}#
                                                   #{ribcage\ 1162}#)
                                                 #{i\ 1165}#)
                                               #{marks\ 1160}#)
                                             (#{f\ 1163}#
                                               (cdr #{symnames\ 1164}#)
                                               (#{fx+\ 316}#
                                                 #{i\ 1165}#
                                                 1)))))))
                              (#{f\ 1163}# #{symnames\ 1161}# 0))))
                        (#{search\ 1142}#
                          (lambda (#{sym\ 1166}#
                                   #{subst\ 1167}#
                                   #{marks\ 1168}#)
                            (if (null? #{subst\ 1167}#)
                              (values #f #{marks\ 1168}#)
                              (let ((#{fst\ 1169}# (car #{subst\ 1167}#)))
                                (if (eq? #{fst\ 1169}# (quote shift))
                                  (#{search\ 1142}#
                                    #{sym\ 1166}#
                                    (cdr #{subst\ 1167}#)
                                    (cdr #{marks\ 1168}#))
                                  (let ((#{symnames\ 1170}#
                                          (#{ribcage-symnames\ 370}#
                                            #{fst\ 1169}#)))
                                    (if (vector? #{symnames\ 1170}#)
                                      (#{search-vector-rib\ 1144}#
                                        #{sym\ 1166}#
                                        #{subst\ 1167}#
                                        #{marks\ 1168}#
                                        #{symnames\ 1170}#
                                        #{fst\ 1169}#)
                                      (#{search-list-rib\ 1143}#
                                        #{sym\ 1166}#
                                        #{subst\ 1167}#
                                        #{marks\ 1168}#
                                        #{symnames\ 1170}#
                                        #{fst\ 1169}#)))))))))
                 (if (symbol? #{id\ 1140}#)
                   (let ((#{t\ 1171}#
                           (call-with-values
                             (lambda ()
                               (#{search\ 1142}#
                                 #{id\ 1140}#
                                 (#{wrap-subst\ 365}# #{w\ 1141}#)
                                 (#{wrap-marks\ 364}# #{w\ 1141}#)))
                             (lambda (#{x\ 1172}# . #{ignore\ 1173}#)
                               #{x\ 1172}#))))
                     (if #{t\ 1171}# #{t\ 1171}# #{id\ 1140}#))
                   (if (#{syntax-object?\ 345}# #{id\ 1140}#)
                     (let ((#{id\ 1174}#
                             (#{syntax-object-expression\ 346}# #{id\ 1140}#))
                           (#{w1\ 1175}#
                             (#{syntax-object-wrap\ 347}# #{id\ 1140}#)))
                       (let ((#{marks\ 1176}#
                               (#{join-marks\ 381}#
                                 (#{wrap-marks\ 364}# #{w\ 1141}#)
                                 (#{wrap-marks\ 364}# #{w1\ 1175}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 1142}#
                               #{id\ 1174}#
                               (#{wrap-subst\ 365}# #{w\ 1141}#)
                               #{marks\ 1176}#))
                           (lambda (#{new-id\ 1177}# #{marks\ 1178}#)
                             (let ((#{t\ 1179}# #{new-id\ 1177}#))
                               (if #{t\ 1179}#
                                 #{t\ 1179}#
                                 (let ((#{t\ 1180}#
                                         (call-with-values
                                           (lambda ()
                                             (#{search\ 1142}#
                                               #{id\ 1174}#
                                               (#{wrap-subst\ 365}#
                                                 #{w1\ 1175}#)
                                               #{marks\ 1178}#))
                                           (lambda (#{x\ 1181}#
                                                    .
                                                    #{ignore\ 1182}#)
                                             #{x\ 1181}#))))
                                   (if #{t\ 1180}#
                                     #{t\ 1180}#
                                     #{id\ 1174}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 1140}#))))))
           (#{same-marks?\ 382}#
             (lambda (#{x\ 1183}# #{y\ 1184}#)
               (let ((#{t\ 1185}# (eq? #{x\ 1183}# #{y\ 1184}#)))
                 (if #{t\ 1185}#
                   #{t\ 1185}#
                   (if (not (null? #{x\ 1183}#))
                     (if (not (null? #{y\ 1184}#))
                       (if (eq? (car #{x\ 1183}#) (car #{y\ 1184}#))
                         (#{same-marks?\ 382}#
                           (cdr #{x\ 1183}#)
                           (cdr #{y\ 1184}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 381}#
             (lambda (#{m1\ 1186}# #{m2\ 1187}#)
               (#{smart-append\ 379}# #{m1\ 1186}# #{m2\ 1187}#)))
           (#{join-wraps\ 380}#
             (lambda (#{w1\ 1188}# #{w2\ 1189}#)
               (let ((#{m1\ 1190}# (#{wrap-marks\ 364}# #{w1\ 1188}#))
                     (#{s1\ 1191}# (#{wrap-subst\ 365}# #{w1\ 1188}#)))
                 (if (null? #{m1\ 1190}#)
                   (if (null? #{s1\ 1191}#)
                     #{w2\ 1189}#
                     (#{make-wrap\ 363}#
                       (#{wrap-marks\ 364}# #{w2\ 1189}#)
                       (#{smart-append\ 379}#
                         #{s1\ 1191}#
                         (#{wrap-subst\ 365}# #{w2\ 1189}#))))
                   (#{make-wrap\ 363}#
                     (#{smart-append\ 379}#
                       #{m1\ 1190}#
                       (#{wrap-marks\ 364}# #{w2\ 1189}#))
                     (#{smart-append\ 379}#
                       #{s1\ 1191}#
                       (#{wrap-subst\ 365}# #{w2\ 1189}#)))))))
           (#{smart-append\ 379}#
             (lambda (#{m1\ 1192}# #{m2\ 1193}#)
               (if (null? #{m2\ 1193}#)
                 #{m1\ 1192}#
                 (append #{m1\ 1192}# #{m2\ 1193}#))))
           (#{make-binding-wrap\ 378}#
             (lambda (#{ids\ 1194}# #{labels\ 1195}# #{w\ 1196}#)
               (if (null? #{ids\ 1194}#)
                 #{w\ 1196}#
                 (#{make-wrap\ 363}#
                   (#{wrap-marks\ 364}# #{w\ 1196}#)
                   (cons (let ((#{labelvec\ 1197}#
                                 (list->vector #{labels\ 1195}#)))
                           (let ((#{n\ 1198}#
                                   (vector-length #{labelvec\ 1197}#)))
                             (let ((#{symnamevec\ 1199}#
                                     (make-vector #{n\ 1198}#))
                                   (#{marksvec\ 1200}#
                                     (make-vector #{n\ 1198}#)))
                               (begin
                                 (letrec ((#{f\ 1201}#
                                            (lambda (#{ids\ 1202}# #{i\ 1203}#)
                                              (if (not (null? #{ids\ 1202}#))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{id-sym-name&marks\ 362}#
                                                      (car #{ids\ 1202}#)
                                                      #{w\ 1196}#))
                                                  (lambda (#{symname\ 1204}#
                                                           #{marks\ 1205}#)
                                                    (begin
                                                      (vector-set!
                                                        #{symnamevec\ 1199}#
                                                        #{i\ 1203}#
                                                        #{symname\ 1204}#)
                                                      (vector-set!
                                                        #{marksvec\ 1200}#
                                                        #{i\ 1203}#
                                                        #{marks\ 1205}#)
                                                      (#{f\ 1201}#
                                                        (cdr #{ids\ 1202}#)
                                                        (#{fx+\ 316}#
                                                          #{i\ 1203}#
                                                          1)))))))))
                                   (#{f\ 1201}# #{ids\ 1194}# 0))
                                 (#{make-ribcage\ 368}#
                                   #{symnamevec\ 1199}#
                                   #{marksvec\ 1200}#
                                   #{labelvec\ 1197}#)))))
                         (#{wrap-subst\ 365}# #{w\ 1196}#))))))
           (#{extend-ribcage!\ 377}#
             (lambda (#{ribcage\ 1206}# #{id\ 1207}# #{label\ 1208}#)
               (begin
                 (#{set-ribcage-symnames!\ 373}#
                   #{ribcage\ 1206}#
                   (cons (#{syntax-object-expression\ 346}# #{id\ 1207}#)
                         (#{ribcage-symnames\ 370}# #{ribcage\ 1206}#)))
                 (#{set-ribcage-marks!\ 374}#
                   #{ribcage\ 1206}#
                   (cons (#{wrap-marks\ 364}#
                           (#{syntax-object-wrap\ 347}# #{id\ 1207}#))
                         (#{ribcage-marks\ 371}# #{ribcage\ 1206}#)))
                 (#{set-ribcage-labels!\ 375}#
                   #{ribcage\ 1206}#
                   (cons #{label\ 1208}#
                         (#{ribcage-labels\ 372}# #{ribcage\ 1206}#))))))
           (#{anti-mark\ 376}#
             (lambda (#{w\ 1209}#)
               (#{make-wrap\ 363}#
                 (cons #f (#{wrap-marks\ 364}# #{w\ 1209}#))
                 (cons 'shift
                       (#{wrap-subst\ 365}# #{w\ 1209}#)))))
           (#{set-ribcage-labels!\ 375}#
             (lambda (#{x\ 1210}# #{update\ 1211}#)
               (vector-set! #{x\ 1210}# 3 #{update\ 1211}#)))
           (#{set-ribcage-marks!\ 374}#
             (lambda (#{x\ 1212}# #{update\ 1213}#)
               (vector-set! #{x\ 1212}# 2 #{update\ 1213}#)))
           (#{set-ribcage-symnames!\ 373}#
             (lambda (#{x\ 1214}# #{update\ 1215}#)
               (vector-set! #{x\ 1214}# 1 #{update\ 1215}#)))
           (#{ribcage-labels\ 372}#
             (lambda (#{x\ 1216}#) (vector-ref #{x\ 1216}# 3)))
           (#{ribcage-marks\ 371}#
             (lambda (#{x\ 1217}#) (vector-ref #{x\ 1217}# 2)))
           (#{ribcage-symnames\ 370}#
             (lambda (#{x\ 1218}#) (vector-ref #{x\ 1218}# 1)))
           (#{ribcage?\ 369}#
             (lambda (#{x\ 1219}#)
               (if (vector? #{x\ 1219}#)
                 (if (= (vector-length #{x\ 1219}#) 4)
                   (eq? (vector-ref #{x\ 1219}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 368}#
             (lambda (#{symnames\ 1220}#
                      #{marks\ 1221}#
                      #{labels\ 1222}#)
               (vector
                 'ribcage
                 #{symnames\ 1220}#
                 #{marks\ 1221}#
                 #{labels\ 1222}#)))
           (#{gen-labels\ 367}#
             (lambda (#{ls\ 1223}#)
               (if (null? #{ls\ 1223}#)
                 '()
                 (cons (#{gen-label\ 366}#)
                       (#{gen-labels\ 367}# (cdr #{ls\ 1223}#))))))
           (#{gen-label\ 366}# (lambda () (string #\i)))
           (#{wrap-subst\ 365}# cdr)
           (#{wrap-marks\ 364}# car)
           (#{make-wrap\ 363}# cons)
           (#{id-sym-name&marks\ 362}#
             (lambda (#{x\ 1224}# #{w\ 1225}#)
               (if (#{syntax-object?\ 345}# #{x\ 1224}#)
                 (values
                   (#{syntax-object-expression\ 346}# #{x\ 1224}#)
                   (#{join-marks\ 381}#
                     (#{wrap-marks\ 364}# #{w\ 1225}#)
                     (#{wrap-marks\ 364}#
                       (#{syntax-object-wrap\ 347}# #{x\ 1224}#))))
                 (values
                   #{x\ 1224}#
                   (#{wrap-marks\ 364}# #{w\ 1225}#)))))
           (#{id?\ 361}#
             (lambda (#{x\ 1226}#)
               (if (symbol? #{x\ 1226}#)
                 #t
                 (if (#{syntax-object?\ 345}# #{x\ 1226}#)
                   (symbol?
                     (#{syntax-object-expression\ 346}# #{x\ 1226}#))
                   #f))))
           (#{nonsymbol-id?\ 360}#
             (lambda (#{x\ 1227}#)
               (if (#{syntax-object?\ 345}# #{x\ 1227}#)
                 (symbol?
                   (#{syntax-object-expression\ 346}# #{x\ 1227}#))
                 #f)))
           (#{global-extend\ 359}#
             (lambda (#{type\ 1228}# #{sym\ 1229}# #{val\ 1230}#)
               (#{put-global-definition-hook\ 322}#
                 #{sym\ 1229}#
                 #{type\ 1228}#
                 #{val\ 1230}#)))
           (#{lookup\ 358}#
             (lambda (#{x\ 1231}# #{r\ 1232}# #{mod\ 1233}#)
               (let ((#{t\ 1234}# (assq #{x\ 1231}# #{r\ 1232}#)))
                 (if #{t\ 1234}#
                   (cdr #{t\ 1234}#)
                   (if (symbol? #{x\ 1231}#)
                     (let ((#{t\ 1235}#
                             (#{get-global-definition-hook\ 323}#
                               #{x\ 1231}#
                               #{mod\ 1233}#)))
                       (if #{t\ 1235}# #{t\ 1235}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 357}#
             (lambda (#{r\ 1236}#)
               (if (null? #{r\ 1236}#)
                 '()
                 (let ((#{a\ 1237}# (car #{r\ 1236}#)))
                   (if (eq? (cadr #{a\ 1237}#) (quote macro))
                     (cons #{a\ 1237}#
                           (#{macros-only-env\ 357}# (cdr #{r\ 1236}#)))
                     (#{macros-only-env\ 357}# (cdr #{r\ 1236}#)))))))
           (#{extend-var-env\ 356}#
             (lambda (#{labels\ 1238}# #{vars\ 1239}# #{r\ 1240}#)
               (if (null? #{labels\ 1238}#)
                 #{r\ 1240}#
                 (#{extend-var-env\ 356}#
                   (cdr #{labels\ 1238}#)
                   (cdr #{vars\ 1239}#)
                   (cons (cons (car #{labels\ 1238}#)
                               (cons (quote lexical) (car #{vars\ 1239}#)))
                         #{r\ 1240}#)))))
           (#{extend-env\ 355}#
             (lambda (#{labels\ 1241}# #{bindings\ 1242}# #{r\ 1243}#)
               (if (null? #{labels\ 1241}#)
                 #{r\ 1243}#
                 (#{extend-env\ 355}#
                   (cdr #{labels\ 1241}#)
                   (cdr #{bindings\ 1242}#)
                   (cons (cons (car #{labels\ 1241}#)
                               (car #{bindings\ 1242}#))
                         #{r\ 1243}#)))))
           (#{binding-value\ 354}# cdr)
           (#{binding-type\ 353}# car)
           (#{source-annotation\ 352}#
             (lambda (#{x\ 1244}#)
               (if (#{syntax-object?\ 345}# #{x\ 1244}#)
                 (#{source-annotation\ 352}#
                   (#{syntax-object-expression\ 346}# #{x\ 1244}#))
                 (if (pair? #{x\ 1244}#)
                   (let ((#{props\ 1245}# (source-properties #{x\ 1244}#)))
                     (if (pair? #{props\ 1245}#) #{props\ 1245}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 351}#
             (lambda (#{x\ 1246}# #{update\ 1247}#)
               (vector-set! #{x\ 1246}# 3 #{update\ 1247}#)))
           (#{set-syntax-object-wrap!\ 350}#
             (lambda (#{x\ 1248}# #{update\ 1249}#)
               (vector-set! #{x\ 1248}# 2 #{update\ 1249}#)))
           (#{set-syntax-object-expression!\ 349}#
             (lambda (#{x\ 1250}# #{update\ 1251}#)
               (vector-set! #{x\ 1250}# 1 #{update\ 1251}#)))
           (#{syntax-object-module\ 348}#
             (lambda (#{x\ 1252}#) (vector-ref #{x\ 1252}# 3)))
           (#{syntax-object-wrap\ 347}#
             (lambda (#{x\ 1253}#) (vector-ref #{x\ 1253}# 2)))
           (#{syntax-object-expression\ 346}#
             (lambda (#{x\ 1254}#) (vector-ref #{x\ 1254}# 1)))
           (#{syntax-object?\ 345}#
             (lambda (#{x\ 1255}#)
               (if (vector? #{x\ 1255}#)
                 (if (= (vector-length #{x\ 1255}#) 4)
                   (eq? (vector-ref #{x\ 1255}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 344}#
             (lambda (#{expression\ 1256}#
                      #{wrap\ 1257}#
                      #{module\ 1258}#)
               (vector
                 'syntax-object
                 #{expression\ 1256}#
                 #{wrap\ 1257}#
                 #{module\ 1258}#)))
           (#{build-letrec\ 343}#
             (lambda (#{src\ 1259}#
                      #{ids\ 1260}#
                      #{vars\ 1261}#
                      #{val-exps\ 1262}#
                      #{body-exp\ 1263}#)
               (if (null? #{vars\ 1261}#)
                 #{body-exp\ 1263}#
                 (let ((#{atom-key\ 1264}# (fluid-ref #{*mode*\ 315}#)))
                   (if (memv #{atom-key\ 1264}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 333}#
                         #{ids\ 1260}#
                         #{val-exps\ 1262}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 1259}#
                        #{ids\ 1260}#
                        #{vars\ 1261}#
                        #{val-exps\ 1262}#
                        #{body-exp\ 1263}#))
                     (#{decorate-source\ 324}#
                       (list 'letrec
                             (map list #{vars\ 1261}# #{val-exps\ 1262}#)
                             #{body-exp\ 1263}#)
                       #{src\ 1259}#))))))
           (#{build-named-let\ 342}#
             (lambda (#{src\ 1265}#
                      #{ids\ 1266}#
                      #{vars\ 1267}#
                      #{val-exps\ 1268}#
                      #{body-exp\ 1269}#)
               (let ((#{f\ 1270}# (car #{vars\ 1267}#))
                     (#{f-name\ 1271}# (car #{ids\ 1266}#))
                     (#{vars\ 1272}# (cdr #{vars\ 1267}#))
                     (#{ids\ 1273}# (cdr #{ids\ 1266}#)))
                 (let ((#{atom-key\ 1274}# (fluid-ref #{*mode*\ 315}#)))
                   (if (memv #{atom-key\ 1274}# (quote (c)))
                     (let ((#{proc\ 1275}#
                             (#{build-simple-lambda\ 335}#
                               #{src\ 1265}#
                               #{ids\ 1273}#
                               #f
                               #{vars\ 1272}#
                               #f
                               #{body-exp\ 1269}#)))
                       (begin
                         (#{maybe-name-value!\ 333}#
                           #{f-name\ 1271}#
                           #{proc\ 1275}#)
                         (for-each
                           #{maybe-name-value!\ 333}#
                           #{ids\ 1273}#
                           #{val-exps\ 1268}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 1265}#
                          (list #{f-name\ 1271}#)
                          (list #{f\ 1270}#)
                          (list #{proc\ 1275}#)
                          (#{build-application\ 326}#
                            #{src\ 1265}#
                            (#{build-lexical-reference\ 328}#
                              'fun
                              #{src\ 1265}#
                              #{f-name\ 1271}#
                              #{f\ 1270}#)
                            #{val-exps\ 1268}#))))
                     (#{decorate-source\ 324}#
                       (list 'let
                             #{f\ 1270}#
                             (map list #{vars\ 1272}# #{val-exps\ 1268}#)
                             #{body-exp\ 1269}#)
                       #{src\ 1265}#))))))
           (#{build-let\ 341}#
             (lambda (#{src\ 1276}#
                      #{ids\ 1277}#
                      #{vars\ 1278}#
                      #{val-exps\ 1279}#
                      #{body-exp\ 1280}#)
               (if (null? #{vars\ 1278}#)
                 #{body-exp\ 1280}#
                 (let ((#{atom-key\ 1281}# (fluid-ref #{*mode*\ 315}#)))
                   (if (memv #{atom-key\ 1281}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 333}#
                         #{ids\ 1277}#
                         #{val-exps\ 1279}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 1276}#
                        #{ids\ 1277}#
                        #{vars\ 1278}#
                        #{val-exps\ 1279}#
                        #{body-exp\ 1280}#))
                     (#{decorate-source\ 324}#
                       (list 'let
                             (map list #{vars\ 1278}# #{val-exps\ 1279}#)
                             #{body-exp\ 1280}#)
                       #{src\ 1276}#))))))
           (#{build-sequence\ 340}#
             (lambda (#{src\ 1282}# #{exps\ 1283}#)
               (if (null? (cdr #{exps\ 1283}#))
                 (car #{exps\ 1283}#)
                 (let ((#{atom-key\ 1284}# (fluid-ref #{*mode*\ 315}#)))
                   (if (memv #{atom-key\ 1284}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 1282}#
                      #{exps\ 1283}#)
                     (#{decorate-source\ 324}#
                       (cons (quote begin) #{exps\ 1283}#)
                       #{src\ 1282}#))))))
           (#{build-data\ 339}#
             (lambda (#{src\ 1285}# #{exp\ 1286}#)
               (let ((#{atom-key\ 1287}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1287}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 1285}#
                    #{exp\ 1286}#)
                   (#{decorate-source\ 324}#
                     (if (if (self-evaluating? #{exp\ 1286}#)
                           (not (vector? #{exp\ 1286}#))
                           #f)
                       #{exp\ 1286}#
                       (list (quote quote) #{exp\ 1286}#))
                     #{src\ 1285}#)))))
           (#{build-primref\ 338}#
             (lambda (#{src\ 1288}# #{name\ 1289}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 1290}# (fluid-ref #{*mode*\ 315}#)))
                   (if (memv #{atom-key\ 1290}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 1288}#
                      #{name\ 1289}#)
                     (#{decorate-source\ 324}#
                       #{name\ 1289}#
                       #{src\ 1288}#)))
                 (let ((#{atom-key\ 1291}# (fluid-ref #{*mode*\ 315}#)))
                   (if (memv #{atom-key\ 1291}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 1288}#
                      '(guile)
                      #{name\ 1289}#
                      #f)
                     (#{decorate-source\ 324}#
                       (list (quote @@) (quote (guile)) #{name\ 1289}#)
                       #{src\ 1288}#))))))
           (#{build-lambda-case\ 337}#
             (lambda (#{src\ 1292}#
                      #{req\ 1293}#
                      #{opt\ 1294}#
                      #{rest\ 1295}#
                      #{kw\ 1296}#
                      #{inits\ 1297}#
                      #{vars\ 1298}#
                      #{predicate\ 1299}#
                      #{body\ 1300}#
                      #{else-case\ 1301}#)
               (let ((#{atom-key\ 1302}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1302}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 1292}#
                    #{req\ 1293}#
                    #{opt\ 1294}#
                    #{rest\ 1295}#
                    #{kw\ 1296}#
                    #{inits\ 1297}#
                    #{vars\ 1298}#
                    #{predicate\ 1299}#
                    #{body\ 1300}#
                    #{else-case\ 1301}#)
                   (let ((#{nreq\ 1303}# (length #{req\ 1293}#)))
                     (let ((#{nopt\ 1304}#
                             (if #{opt\ 1294}# (length #{opt\ 1294}#) 0)))
                       (let ((#{rest-idx\ 1305}#
                               (if #{rest\ 1295}#
                                 (+ #{nreq\ 1303}# #{nopt\ 1304}#)
                                 #f)))
                         (let ((#{allow-other-keys?\ 1306}#
                                 (if #{kw\ 1296}# (car #{kw\ 1296}#) #f)))
                           (let ((#{kw-indices\ 1307}#
                                   (map (lambda (#{x\ 1308}#)
                                          (cons (car #{x\ 1308}#)
                                                (list-index
                                                  #{vars\ 1298}#
                                                  (caddr #{x\ 1308}#))))
                                        (if #{kw\ 1296}#
                                          (cdr #{kw\ 1296}#)
                                          '()))))
                             (let ((#{nargs\ 1309}#
                                     (apply max
                                            (+ #{nreq\ 1303}#
                                               #{nopt\ 1304}#
                                               (if #{rest\ 1295}# 1 0))
                                            (map 1+
                                                 (map cdr
                                                      #{kw-indices\ 1307}#)))))
                               (begin
                                 (let ((#{t\ 1310}#
                                         (= #{nargs\ 1309}#
                                            (length #{vars\ 1298}#)
                                            (+ #{nreq\ 1303}#
                                               (length #{inits\ 1297}#)
                                               (if #{rest\ 1295}# 1 0)))))
                                   (if #{t\ 1310}#
                                     #{t\ 1310}#
                                     (error "something went wrong"
                                            #{req\ 1293}#
                                            #{opt\ 1294}#
                                            #{rest\ 1295}#
                                            #{kw\ 1296}#
                                            #{inits\ 1297}#
                                            #{vars\ 1298}#
                                            #{nreq\ 1303}#
                                            #{nopt\ 1304}#
                                            #{kw-indices\ 1307}#
                                            #{nargs\ 1309}#)))
                                 (#{decorate-source\ 324}#
                                   (cons (list (cons '(@@ (ice-9 optargs)
                                                          parse-lambda-case)
                                                     (cons (list 'quote
                                                                 (list #{nreq\ 1303}#
                                                                       #{nopt\ 1304}#
                                                                       #{rest-idx\ 1305}#
                                                                       #{nargs\ 1309}#
                                                                       #{allow-other-keys?\ 1306}#
                                                                       #{kw-indices\ 1307}#))
                                                           (cons (cons 'list
                                                                       (map (lambda (#{i\ 1311}#)
                                                                              (list 'lambda
                                                                                    #{vars\ 1298}#
                                                                                    #{i\ 1311}#))
                                                                            #{inits\ 1297}#))
                                                                 (cons (if #{predicate\ 1299}#
                                                                         (list 'lambda
                                                                               #{vars\ 1298}#
                                                                               #{predicate\ 1299}#)
                                                                         #f)
                                                                       '(%%args)))))
                                               '=>
                                               (list 'lambda
                                                     '(%%%args . _)
                                                     (cons 'apply
                                                           (cons (list 'lambda
                                                                       #{vars\ 1298}#
                                                                       #{body\ 1300}#)
                                                                 '(%%%args)))))
                                         (let ((#{t\ 1312}#
                                                 #{else-case\ 1301}#))
                                           (if #{t\ 1312}#
                                             #{t\ 1312}#
                                             '((%%args
                                                 (error "wrong number of arguments"
                                                        %%args))))))
                                   #{src\ 1292}#))))))))))))
           (#{build-case-lambda\ 336}#
             (lambda (#{src\ 1313}#
                      #{docstring\ 1314}#
                      #{body\ 1315}#)
               (let ((#{atom-key\ 1316}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1316}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1313}#
                    (if #{docstring\ 1314}#
                      (list (cons (quote documentation) #{docstring\ 1314}#))
                      '())
                    #{body\ 1315}#)
                   (#{decorate-source\ 324}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 1314}#
                                     (list #{docstring\ 1314}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 1315}#)))))
                     #{src\ 1313}#)))))
           (#{build-simple-lambda\ 335}#
             (lambda (#{src\ 1317}#
                      #{req\ 1318}#
                      #{rest\ 1319}#
                      #{vars\ 1320}#
                      #{docstring\ 1321}#
                      #{exp\ 1322}#)
               (let ((#{atom-key\ 1323}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1323}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1317}#
                    (if #{docstring\ 1321}#
                      (list (cons (quote documentation) #{docstring\ 1321}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 1317}#
                     #{req\ 1318}#
                     #f
                     #{rest\ 1319}#
                     #f
                     '()
                     #{vars\ 1320}#
                     #f
                     #{exp\ 1322}#
                     #f))
                   (#{decorate-source\ 324}#
                     (cons 'lambda
                           (cons (if #{rest\ 1319}#
                                   (apply cons* #{vars\ 1320}#)
                                   #{vars\ 1320}#)
                                 (append
                                   (if #{docstring\ 1321}#
                                     (list #{docstring\ 1321}#)
                                     '())
                                   (list #{exp\ 1322}#))))
                     #{src\ 1317}#)))))
           (#{build-global-definition\ 334}#
             (lambda (#{source\ 1324}# #{var\ 1325}# #{exp\ 1326}#)
               (let ((#{atom-key\ 1327}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1327}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 333}#
                       #{var\ 1325}#
                       #{exp\ 1326}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 1324}#
                      #{var\ 1325}#
                      #{exp\ 1326}#))
                   (#{decorate-source\ 324}#
                     (list (quote define) #{var\ 1325}# #{exp\ 1326}#)
                     #{source\ 1324}#)))))
           (#{maybe-name-value!\ 333}#
             (lambda (#{name\ 1328}# #{val\ 1329}#)
               (if ((@ (language tree-il) lambda?) #{val\ 1329}#)
                 (let ((#{meta\ 1330}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 1329}#)))
                   (if (not (assq (quote name) #{meta\ 1330}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 1329}#
                      (acons 'name
                             #{name\ 1328}#
                             #{meta\ 1330}#)))))))
           (#{build-global-assignment\ 332}#
             (lambda (#{source\ 1331}#
                      #{var\ 1332}#
                      #{exp\ 1333}#
                      #{mod\ 1334}#)
               (#{analyze-variable\ 330}#
                 #{mod\ 1334}#
                 #{var\ 1332}#
                 (lambda (#{mod\ 1335}# #{var\ 1336}# #{public?\ 1337}#)
                   (let ((#{atom-key\ 1338}# (fluid-ref #{*mode*\ 315}#)))
                     (if (memv #{atom-key\ 1338}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 1331}#
                        #{mod\ 1335}#
                        #{var\ 1336}#
                        #{public?\ 1337}#
                        #{exp\ 1333}#)
                       (#{decorate-source\ 324}#
                         (list 'set!
                               (list (if #{public?\ 1337}#
                                       '@
                                       '@@)
                                     #{mod\ 1335}#
                                     #{var\ 1336}#)
                               #{exp\ 1333}#)
                         #{source\ 1331}#))))
                 (lambda (#{var\ 1339}#)
                   (let ((#{atom-key\ 1340}# (fluid-ref #{*mode*\ 315}#)))
                     (if (memv #{atom-key\ 1340}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 1331}#
                        #{var\ 1339}#
                        #{exp\ 1333}#)
                       (#{decorate-source\ 324}#
                         (list (quote set!) #{var\ 1339}# #{exp\ 1333}#)
                         #{source\ 1331}#)))))))
           (#{build-global-reference\ 331}#
             (lambda (#{source\ 1341}# #{var\ 1342}# #{mod\ 1343}#)
               (#{analyze-variable\ 330}#
                 #{mod\ 1343}#
                 #{var\ 1342}#
                 (lambda (#{mod\ 1344}# #{var\ 1345}# #{public?\ 1346}#)
                   (let ((#{atom-key\ 1347}# (fluid-ref #{*mode*\ 315}#)))
                     (if (memv #{atom-key\ 1347}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 1341}#
                        #{mod\ 1344}#
                        #{var\ 1345}#
                        #{public?\ 1346}#)
                       (#{decorate-source\ 324}#
                         (list (if #{public?\ 1346}# (quote @) (quote @@))
                               #{mod\ 1344}#
                               #{var\ 1345}#)
                         #{source\ 1341}#))))
                 (lambda (#{var\ 1348}#)
                   (let ((#{atom-key\ 1349}# (fluid-ref #{*mode*\ 315}#)))
                     (if (memv #{atom-key\ 1349}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 1341}#
                        #{var\ 1348}#)
                       (#{decorate-source\ 324}#
                         #{var\ 1348}#
                         #{source\ 1341}#)))))))
           (#{analyze-variable\ 330}#
             (lambda (#{mod\ 1350}#
                      #{var\ 1351}#
                      #{modref-cont\ 1352}#
                      #{bare-cont\ 1353}#)
               (if (not #{mod\ 1350}#)
                 (#{bare-cont\ 1353}# #{var\ 1351}#)
                 (let ((#{kind\ 1354}# (car #{mod\ 1350}#))
                       (#{mod\ 1355}# (cdr #{mod\ 1350}#)))
                   (if (memv #{kind\ 1354}# (quote (public)))
                     (#{modref-cont\ 1352}#
                       #{mod\ 1355}#
                       #{var\ 1351}#
                       #t)
                     (if (memv #{kind\ 1354}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 1355}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 1352}#
                           #{mod\ 1355}#
                           #{var\ 1351}#
                           #f)
                         (#{bare-cont\ 1353}# #{var\ 1351}#))
                       (if (memv #{kind\ 1354}# (quote (bare)))
                         (#{bare-cont\ 1353}# #{var\ 1351}#)
                         (if (memv #{kind\ 1354}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 1355}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 1355}#)
                                   #{var\ 1351}#)
                                 #f)
                             (#{modref-cont\ 1352}#
                               #{mod\ 1355}#
                               #{var\ 1351}#
                               #f)
                             (#{bare-cont\ 1353}# #{var\ 1351}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 1351}#
                             #{mod\ 1355}#)))))))))
           (#{build-lexical-assignment\ 329}#
             (lambda (#{source\ 1356}#
                      #{name\ 1357}#
                      #{var\ 1358}#
                      #{exp\ 1359}#)
               (let ((#{atom-key\ 1360}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1360}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 1356}#
                    #{name\ 1357}#
                    #{var\ 1358}#
                    #{exp\ 1359}#)
                   (#{decorate-source\ 324}#
                     (list (quote set!) #{var\ 1358}# #{exp\ 1359}#)
                     #{source\ 1356}#)))))
           (#{build-lexical-reference\ 328}#
             (lambda (#{type\ 1361}#
                      #{source\ 1362}#
                      #{name\ 1363}#
                      #{var\ 1364}#)
               (let ((#{atom-key\ 1365}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1365}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 1362}#
                    #{name\ 1363}#
                    #{var\ 1364}#)
                   (#{decorate-source\ 324}#
                     #{var\ 1364}#
                     #{source\ 1362}#)))))
           (#{build-conditional\ 327}#
             (lambda (#{source\ 1366}#
                      #{test-exp\ 1367}#
                      #{then-exp\ 1368}#
                      #{else-exp\ 1369}#)
               (let ((#{atom-key\ 1370}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1370}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 1366}#
                    #{test-exp\ 1367}#
                    #{then-exp\ 1368}#
                    #{else-exp\ 1369}#)
                   (#{decorate-source\ 324}#
                     (if (equal? #{else-exp\ 1369}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 1367}#
                             #{then-exp\ 1368}#)
                       (list 'if
                             #{test-exp\ 1367}#
                             #{then-exp\ 1368}#
                             #{else-exp\ 1369}#))
                     #{source\ 1366}#)))))
           (#{build-application\ 326}#
             (lambda (#{source\ 1371}#
                      #{fun-exp\ 1372}#
                      #{arg-exps\ 1373}#)
               (let ((#{atom-key\ 1374}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1374}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 1371}#
                    #{fun-exp\ 1372}#
                    #{arg-exps\ 1373}#)
                   (#{decorate-source\ 324}#
                     (cons #{fun-exp\ 1372}# #{arg-exps\ 1373}#)
                     #{source\ 1371}#)))))
           (#{build-void\ 325}#
             (lambda (#{source\ 1375}#)
               (let ((#{atom-key\ 1376}# (fluid-ref #{*mode*\ 315}#)))
                 (if (memv #{atom-key\ 1376}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 1375}#)
                   (#{decorate-source\ 324}#
                     '(if #f #f)
                     #{source\ 1375}#)))))
           (#{decorate-source\ 324}#
             (lambda (#{e\ 1377}# #{s\ 1378}#)
               (begin
                 (if (if (pair? #{e\ 1377}#) #{s\ 1378}# #f)
                   (set-source-properties! #{e\ 1377}# #{s\ 1378}#))
                 #{e\ 1377}#)))
           (#{get-global-definition-hook\ 323}#
             (lambda (#{symbol\ 1379}# #{module\ 1380}#)
               (begin
                 (if (if (not #{module\ 1380}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 1379}#))
                 (let ((#{v\ 1381}#
                         (module-variable
                           (if #{module\ 1380}#
                             (resolve-module (cdr #{module\ 1380}#))
                             (current-module))
                           #{symbol\ 1379}#)))
                   (if #{v\ 1381}#
                     (if (variable-bound? #{v\ 1381}#)
                       (let ((#{val\ 1382}# (variable-ref #{v\ 1381}#)))
                         (if (macro? #{val\ 1382}#)
                           (if (syncase-macro-type #{val\ 1382}#)
                             (cons (syncase-macro-type #{val\ 1382}#)
                                   (syncase-macro-binding #{val\ 1382}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 322}#
             (lambda (#{symbol\ 1383}# #{type\ 1384}# #{val\ 1385}#)
               (let ((#{existing\ 1386}#
                       (let ((#{v\ 1387}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 1383}#)))
                         (if #{v\ 1387}#
                           (if (variable-bound? #{v\ 1387}#)
                             (let ((#{val\ 1388}# (variable-ref #{v\ 1387}#)))
                               (if (macro? #{val\ 1388}#)
                                 (if (not (syncase-macro-type #{val\ 1388}#))
                                   #{val\ 1388}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 1383}#
                   (if #{existing\ 1386}#
                     (make-extended-syncase-macro
                       #{existing\ 1386}#
                       #{type\ 1384}#
                       #{val\ 1385}#)
                     (make-syncase-macro #{type\ 1384}# #{val\ 1385}#))))))
           (#{local-eval-hook\ 321}#
             (lambda (#{x\ 1389}# #{mod\ 1390}#)
               (primitive-eval
                 (list #{noexpand\ 314}#
                       (let ((#{atom-key\ 1391}# (fluid-ref #{*mode*\ 315}#)))
                         (if (memv #{atom-key\ 1391}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1389}#)
                           #{x\ 1389}#))))))
           (#{top-level-eval-hook\ 320}#
             (lambda (#{x\ 1392}# #{mod\ 1393}#)
               (primitive-eval
                 (list #{noexpand\ 314}#
                       (let ((#{atom-key\ 1394}# (fluid-ref #{*mode*\ 315}#)))
                         (if (memv #{atom-key\ 1394}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1392}#)
                           #{x\ 1392}#))))))
           (#{fx<\ 319}# <)
           (#{fx=\ 318}# =)
           (#{fx-\ 317}# -)
           (#{fx+\ 316}# +)
           (#{*mode*\ 315}# (make-fluid))
           (#{noexpand\ 314}# "noexpand"))
    (begin
      (#{global-extend\ 359}#
        'local-syntax
        'letrec-syntax
        #t)
      (#{global-extend\ 359}#
        'local-syntax
        'let-syntax
        #f)
      (#{global-extend\ 359}#
        'core
        'fluid-let-syntax
        (lambda (#{e\ 1395}#
                 #{r\ 1396}#
                 #{w\ 1397}#
                 #{s\ 1398}#
                 #{mod\ 1399}#)
          ((lambda (#{tmp\ 1400}#)
             ((lambda (#{tmp\ 1401}#)
                (if (if #{tmp\ 1401}#
                      (apply (lambda (#{_\ 1402}#
                                      #{var\ 1403}#
                                      #{val\ 1404}#
                                      #{e1\ 1405}#
                                      #{e2\ 1406}#)
                               (#{valid-bound-ids?\ 386}# #{var\ 1403}#))
                             #{tmp\ 1401}#)
                      #f)
                  (apply (lambda (#{_\ 1408}#
                                  #{var\ 1409}#
                                  #{val\ 1410}#
                                  #{e1\ 1411}#
                                  #{e2\ 1412}#)
                           (let ((#{names\ 1413}#
                                   (map (lambda (#{x\ 1414}#)
                                          (#{id-var-name\ 383}#
                                            #{x\ 1414}#
                                            #{w\ 1397}#))
                                        #{var\ 1409}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 1416}# #{n\ 1417}#)
                                   (let ((#{atom-key\ 1418}#
                                           (#{binding-type\ 353}#
                                             (#{lookup\ 358}#
                                               #{n\ 1417}#
                                               #{r\ 1396}#
                                               #{mod\ 1399}#))))
                                     (if (memv #{atom-key\ 1418}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 1395}#
                                         (#{source-wrap\ 390}#
                                           #{id\ 1416}#
                                           #{w\ 1397}#
                                           #{s\ 1398}#
                                           #{mod\ 1399}#)))))
                                 #{var\ 1409}#
                                 #{names\ 1413}#)
                               (#{chi-body\ 401}#
                                 (cons #{e1\ 1411}# #{e2\ 1412}#)
                                 (#{source-wrap\ 390}#
                                   #{e\ 1395}#
                                   #{w\ 1397}#
                                   #{s\ 1398}#
                                   #{mod\ 1399}#)
                                 (#{extend-env\ 355}#
                                   #{names\ 1413}#
                                   (let ((#{trans-r\ 1421}#
                                           (#{macros-only-env\ 357}#
                                             #{r\ 1396}#)))
                                     (map (lambda (#{x\ 1422}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 403}#
                                                    (#{chi\ 397}#
                                                      #{x\ 1422}#
                                                      #{trans-r\ 1421}#
                                                      #{w\ 1397}#
                                                      #{mod\ 1399}#)
                                                    #{mod\ 1399}#)))
                                          #{val\ 1410}#))
                                   #{r\ 1396}#)
                                 #{w\ 1397}#
                                 #{mod\ 1399}#))))
                         #{tmp\ 1401}#)
                  ((lambda (#{_\ 1424}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 390}#
                         #{e\ 1395}#
                         #{w\ 1397}#
                         #{s\ 1398}#
                         #{mod\ 1399}#)))
                   #{tmp\ 1400}#)))
              ($sc-dispatch
                #{tmp\ 1400}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1395}#)))
      (#{global-extend\ 359}#
        'core
        'quote
        (lambda (#{e\ 1425}#
                 #{r\ 1426}#
                 #{w\ 1427}#
                 #{s\ 1428}#
                 #{mod\ 1429}#)
          ((lambda (#{tmp\ 1430}#)
             ((lambda (#{tmp\ 1431}#)
                (if #{tmp\ 1431}#
                  (apply (lambda (#{_\ 1432}# #{e\ 1433}#)
                           (#{build-data\ 339}#
                             #{s\ 1428}#
                             (#{strip\ 410}# #{e\ 1433}# #{w\ 1427}#)))
                         #{tmp\ 1431}#)
                  ((lambda (#{_\ 1434}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 390}#
                         #{e\ 1425}#
                         #{w\ 1427}#
                         #{s\ 1428}#
                         #{mod\ 1429}#)))
                   #{tmp\ 1430}#)))
              ($sc-dispatch #{tmp\ 1430}# (quote (any any)))))
           #{e\ 1425}#)))
      (#{global-extend\ 359}#
        'core
        'syntax
        (letrec ((#{regen\ 1442}#
                   (lambda (#{x\ 1443}#)
                     (let ((#{atom-key\ 1444}# (car #{x\ 1443}#)))
                       (if (memv #{atom-key\ 1444}# (quote (ref)))
                         (#{build-lexical-reference\ 328}#
                           'value
                           #f
                           (cadr #{x\ 1443}#)
                           (cadr #{x\ 1443}#))
                         (if (memv #{atom-key\ 1444}# (quote (primitive)))
                           (#{build-primref\ 338}# #f (cadr #{x\ 1443}#))
                           (if (memv #{atom-key\ 1444}# (quote (quote)))
                             (#{build-data\ 339}# #f (cadr #{x\ 1443}#))
                             (if (memv #{atom-key\ 1444}# (quote (lambda)))
                               (if (list? (cadr #{x\ 1443}#))
                                 (#{build-simple-lambda\ 335}#
                                   #f
                                   (cadr #{x\ 1443}#)
                                   #f
                                   (cadr #{x\ 1443}#)
                                   #f
                                   (#{regen\ 1442}# (caddr #{x\ 1443}#)))
                                 (error "how did we get here" #{x\ 1443}#))
                               (#{build-application\ 326}#
                                 #f
                                 (#{build-primref\ 338}# #f (car #{x\ 1443}#))
                                 (map #{regen\ 1442}#
                                      (cdr #{x\ 1443}#))))))))))
                 (#{gen-vector\ 1441}#
                   (lambda (#{x\ 1445}#)
                     (if (eq? (car #{x\ 1445}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 1445}#))
                       (if (eq? (car #{x\ 1445}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 1445}#)))
                         (list (quote list->vector) #{x\ 1445}#)))))
                 (#{gen-append\ 1440}#
                   (lambda (#{x\ 1446}# #{y\ 1447}#)
                     (if (equal? #{y\ 1447}# (quote (quote ())))
                       #{x\ 1446}#
                       (list (quote append) #{x\ 1446}# #{y\ 1447}#))))
                 (#{gen-cons\ 1439}#
                   (lambda (#{x\ 1448}# #{y\ 1449}#)
                     (let ((#{atom-key\ 1450}# (car #{y\ 1449}#)))
                       (if (memv #{atom-key\ 1450}# (quote (quote)))
                         (if (eq? (car #{x\ 1448}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 1448}#) (cadr #{y\ 1449}#)))
                           (if (eq? (cadr #{y\ 1449}#) (quote ()))
                             (list (quote list) #{x\ 1448}#)
                             (list (quote cons) #{x\ 1448}# #{y\ 1449}#)))
                         (if (memv #{atom-key\ 1450}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 1448}# (cdr #{y\ 1449}#)))
                           (list (quote cons) #{x\ 1448}# #{y\ 1449}#))))))
                 (#{gen-map\ 1438}#
                   (lambda (#{e\ 1451}# #{map-env\ 1452}#)
                     (let ((#{formals\ 1453}# (map cdr #{map-env\ 1452}#))
                           (#{actuals\ 1454}#
                             (map (lambda (#{x\ 1455}#)
                                    (list (quote ref) (car #{x\ 1455}#)))
                                  #{map-env\ 1452}#)))
                       (if (eq? (car #{e\ 1451}#) (quote ref))
                         (car #{actuals\ 1454}#)
                         (if (and-map
                               (lambda (#{x\ 1456}#)
                                 (if (eq? (car #{x\ 1456}#) (quote ref))
                                   (memq (cadr #{x\ 1456}#) #{formals\ 1453}#)
                                   #f))
                               (cdr #{e\ 1451}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 1451}#))
                                       (map (let ((#{r\ 1457}#
                                                    (map cons
                                                         #{formals\ 1453}#
                                                         #{actuals\ 1454}#)))
                                              (lambda (#{x\ 1458}#)
                                                (cdr (assq (cadr #{x\ 1458}#)
                                                           #{r\ 1457}#))))
                                            (cdr #{e\ 1451}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 1453}#
                                             #{e\ 1451}#)
                                       #{actuals\ 1454}#)))))))
                 (#{gen-mappend\ 1437}#
                   (lambda (#{e\ 1459}# #{map-env\ 1460}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 1438}# #{e\ 1459}# #{map-env\ 1460}#))))
                 (#{gen-ref\ 1436}#
                   (lambda (#{src\ 1461}#
                            #{var\ 1462}#
                            #{level\ 1463}#
                            #{maps\ 1464}#)
                     (if (#{fx=\ 318}# #{level\ 1463}# 0)
                       (values #{var\ 1462}# #{maps\ 1464}#)
                       (if (null? #{maps\ 1464}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 1461}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 1436}#
                               #{src\ 1461}#
                               #{var\ 1462}#
                               (#{fx-\ 317}# #{level\ 1463}# 1)
                               (cdr #{maps\ 1464}#)))
                           (lambda (#{outer-var\ 1465}# #{outer-maps\ 1466}#)
                             (let ((#{b\ 1467}#
                                     (assq #{outer-var\ 1465}#
                                           (car #{maps\ 1464}#))))
                               (if #{b\ 1467}#
                                 (values (cdr #{b\ 1467}#) #{maps\ 1464}#)
                                 (let ((#{inner-var\ 1468}#
                                         (#{gen-var\ 411}# (quote tmp))))
                                   (values
                                     #{inner-var\ 1468}#
                                     (cons (cons (cons #{outer-var\ 1465}#
                                                       #{inner-var\ 1468}#)
                                                 (car #{maps\ 1464}#))
                                           #{outer-maps\ 1466}#)))))))))))
                 (#{gen-syntax\ 1435}#
                   (lambda (#{src\ 1469}#
                            #{e\ 1470}#
                            #{r\ 1471}#
                            #{maps\ 1472}#
                            #{ellipsis?\ 1473}#
                            #{mod\ 1474}#)
                     (if (#{id?\ 361}# #{e\ 1470}#)
                       (let ((#{label\ 1475}#
                               (#{id-var-name\ 383}#
                                 #{e\ 1470}#
                                 '(()))))
                         (let ((#{b\ 1476}#
                                 (#{lookup\ 358}#
                                   #{label\ 1475}#
                                   #{r\ 1471}#
                                   #{mod\ 1474}#)))
                           (if (eq? (#{binding-type\ 353}# #{b\ 1476}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 1477}#
                                         (#{binding-value\ 354}# #{b\ 1476}#)))
                                   (#{gen-ref\ 1436}#
                                     #{src\ 1469}#
                                     (car #{var.lev\ 1477}#)
                                     (cdr #{var.lev\ 1477}#)
                                     #{maps\ 1472}#)))
                               (lambda (#{var\ 1478}# #{maps\ 1479}#)
                                 (values
                                   (list (quote ref) #{var\ 1478}#)
                                   #{maps\ 1479}#)))
                             (if (#{ellipsis?\ 1473}# #{e\ 1470}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 1469}#)
                               (values
                                 (list (quote quote) #{e\ 1470}#)
                                 #{maps\ 1472}#)))))
                       ((lambda (#{tmp\ 1480}#)
                          ((lambda (#{tmp\ 1481}#)
                             (if (if #{tmp\ 1481}#
                                   (apply (lambda (#{dots\ 1482}# #{e\ 1483}#)
                                            (#{ellipsis?\ 1473}#
                                              #{dots\ 1482}#))
                                          #{tmp\ 1481}#)
                                   #f)
                               (apply (lambda (#{dots\ 1484}# #{e\ 1485}#)
                                        (#{gen-syntax\ 1435}#
                                          #{src\ 1469}#
                                          #{e\ 1485}#
                                          #{r\ 1471}#
                                          #{maps\ 1472}#
                                          (lambda (#{x\ 1486}#) #f)
                                          #{mod\ 1474}#))
                                      #{tmp\ 1481}#)
                               ((lambda (#{tmp\ 1487}#)
                                  (if (if #{tmp\ 1487}#
                                        (apply (lambda (#{x\ 1488}#
                                                        #{dots\ 1489}#
                                                        #{y\ 1490}#)
                                                 (#{ellipsis?\ 1473}#
                                                   #{dots\ 1489}#))
                                               #{tmp\ 1487}#)
                                        #f)
                                    (apply (lambda (#{x\ 1491}#
                                                    #{dots\ 1492}#
                                                    #{y\ 1493}#)
                                             (letrec ((#{f\ 1494}#
                                                        (lambda (#{y\ 1495}#
                                                                 #{k\ 1496}#)
                                                          ((lambda (#{tmp\ 1500}#)
                                                             ((lambda (#{tmp\ 1501}#)
                                                                (if (if #{tmp\ 1501}#
                                                                      (apply (lambda (#{dots\ 1502}#
                                                                                      #{y\ 1503}#)
                                                                               (#{ellipsis?\ 1473}#
                                                                                 #{dots\ 1502}#))
                                                                             #{tmp\ 1501}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 1504}#
                                                                                  #{y\ 1505}#)
                                                                           (#{f\ 1494}#
                                                                             #{y\ 1505}#
                                                                             (lambda (#{maps\ 1506}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 1496}#
                                                                                     (cons '()
                                                                                           #{maps\ 1506}#)))
                                                                                 (lambda (#{x\ 1507}#
                                                                                          #{maps\ 1508}#)
                                                                                   (if (null? (car #{maps\ 1508}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 1469}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 1437}#
                                                                                         #{x\ 1507}#
                                                                                         (car #{maps\ 1508}#))
                                                                                       (cdr #{maps\ 1508}#))))))))
                                                                         #{tmp\ 1501}#)
                                                                  ((lambda (#{_\ 1509}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 1435}#
                                                                           #{src\ 1469}#
                                                                           #{y\ 1495}#
                                                                           #{r\ 1471}#
                                                                           #{maps\ 1472}#
                                                                           #{ellipsis?\ 1473}#
                                                                           #{mod\ 1474}#))
                                                                       (lambda (#{y\ 1510}#
                                                                                #{maps\ 1511}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 1496}#
                                                                               #{maps\ 1511}#))
                                                                           (lambda (#{x\ 1512}#
                                                                                    #{maps\ 1513}#)
                                                                             (values
                                                                               (#{gen-append\ 1440}#
                                                                                 #{x\ 1512}#
                                                                                 #{y\ 1510}#)
                                                                               #{maps\ 1513}#))))))
                                                                   #{tmp\ 1500}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 1500}#
                                                                '(any . any))))
                                                           #{y\ 1495}#))))
                                               (#{f\ 1494}#
                                                 #{y\ 1493}#
                                                 (lambda (#{maps\ 1497}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 1435}#
                                                         #{src\ 1469}#
                                                         #{x\ 1491}#
                                                         #{r\ 1471}#
                                                         (cons '()
                                                               #{maps\ 1497}#)
                                                         #{ellipsis?\ 1473}#
                                                         #{mod\ 1474}#))
                                                     (lambda (#{x\ 1498}#
                                                              #{maps\ 1499}#)
                                                       (if (null? (car #{maps\ 1499}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 1469}#)
                                                         (values
                                                           (#{gen-map\ 1438}#
                                                             #{x\ 1498}#
                                                             (car #{maps\ 1499}#))
                                                           (cdr #{maps\ 1499}#)))))))))
                                           #{tmp\ 1487}#)
                                    ((lambda (#{tmp\ 1514}#)
                                       (if #{tmp\ 1514}#
                                         (apply (lambda (#{x\ 1515}#
                                                         #{y\ 1516}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 1435}#
                                                        #{src\ 1469}#
                                                        #{x\ 1515}#
                                                        #{r\ 1471}#
                                                        #{maps\ 1472}#
                                                        #{ellipsis?\ 1473}#
                                                        #{mod\ 1474}#))
                                                    (lambda (#{x\ 1517}#
                                                             #{maps\ 1518}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 1435}#
                                                            #{src\ 1469}#
                                                            #{y\ 1516}#
                                                            #{r\ 1471}#
                                                            #{maps\ 1518}#
                                                            #{ellipsis?\ 1473}#
                                                            #{mod\ 1474}#))
                                                        (lambda (#{y\ 1519}#
                                                                 #{maps\ 1520}#)
                                                          (values
                                                            (#{gen-cons\ 1439}#
                                                              #{x\ 1517}#
                                                              #{y\ 1519}#)
                                                            #{maps\ 1520}#))))))
                                                #{tmp\ 1514}#)
                                         ((lambda (#{tmp\ 1521}#)
                                            (if #{tmp\ 1521}#
                                              (apply (lambda (#{e1\ 1522}#
                                                              #{e2\ 1523}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 1435}#
                                                             #{src\ 1469}#
                                                             (cons #{e1\ 1522}#
                                                                   #{e2\ 1523}#)
                                                             #{r\ 1471}#
                                                             #{maps\ 1472}#
                                                             #{ellipsis?\ 1473}#
                                                             #{mod\ 1474}#))
                                                         (lambda (#{e\ 1525}#
                                                                  #{maps\ 1526}#)
                                                           (values
                                                             (#{gen-vector\ 1441}#
                                                               #{e\ 1525}#)
                                                             #{maps\ 1526}#))))
                                                     #{tmp\ 1521}#)
                                              ((lambda (#{_\ 1527}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 1470}#)
                                                   #{maps\ 1472}#))
                                               #{tmp\ 1480}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1480}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 1480}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 1480}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 1480}# (quote (any any)))))
                        #{e\ 1470}#)))))
          (lambda (#{e\ 1528}#
                   #{r\ 1529}#
                   #{w\ 1530}#
                   #{s\ 1531}#
                   #{mod\ 1532}#)
            (let ((#{e\ 1533}#
                    (#{source-wrap\ 390}#
                      #{e\ 1528}#
                      #{w\ 1530}#
                      #{s\ 1531}#
                      #{mod\ 1532}#)))
              ((lambda (#{tmp\ 1534}#)
                 ((lambda (#{tmp\ 1535}#)
                    (if #{tmp\ 1535}#
                      (apply (lambda (#{_\ 1536}# #{x\ 1537}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 1435}#
                                     #{e\ 1533}#
                                     #{x\ 1537}#
                                     #{r\ 1529}#
                                     '()
                                     #{ellipsis?\ 405}#
                                     #{mod\ 1532}#))
                                 (lambda (#{e\ 1538}# #{maps\ 1539}#)
                                   (#{regen\ 1442}# #{e\ 1538}#))))
                             #{tmp\ 1535}#)
                      ((lambda (#{_\ 1540}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 1533}#))
                       #{tmp\ 1534}#)))
                  ($sc-dispatch #{tmp\ 1534}# (quote (any any)))))
               #{e\ 1533}#)))))
      (#{global-extend\ 359}#
        'core
        'lambda
        (lambda (#{e\ 1541}#
                 #{r\ 1542}#
                 #{w\ 1543}#
                 #{s\ 1544}#
                 #{mod\ 1545}#)
          ((lambda (#{tmp\ 1546}#)
             ((lambda (#{tmp\ 1547}#)
                (if (if #{tmp\ 1547}#
                      (apply (lambda (#{_\ 1548}#
                                      #{args\ 1549}#
                                      #{docstring\ 1550}#
                                      #{e1\ 1551}#
                                      #{e2\ 1552}#)
                               (string? (syntax->datum #{docstring\ 1550}#)))
                             #{tmp\ 1547}#)
                      #f)
                  (apply (lambda (#{_\ 1553}#
                                  #{args\ 1554}#
                                  #{docstring\ 1555}#
                                  #{e1\ 1556}#
                                  #{e2\ 1557}#)
                           (call-with-values
                             (lambda ()
                               (#{lambda-formals\ 406}# #{args\ 1554}#))
                             (lambda (#{req\ 1558}#
                                      #{opt\ 1559}#
                                      #{rest\ 1560}#
                                      #{kw\ 1561}#)
                               (#{chi-simple-lambda\ 407}#
                                 #{e\ 1541}#
                                 #{r\ 1542}#
                                 #{w\ 1543}#
                                 #{s\ 1544}#
                                 #{mod\ 1545}#
                                 #{req\ 1558}#
                                 #{rest\ 1560}#
                                 (syntax->datum #{docstring\ 1555}#)
                                 (cons #{e1\ 1556}# #{e2\ 1557}#)))))
                         #{tmp\ 1547}#)
                  ((lambda (#{tmp\ 1563}#)
                     (if #{tmp\ 1563}#
                       (apply (lambda (#{_\ 1564}#
                                       #{args\ 1565}#
                                       #{e1\ 1566}#
                                       #{e2\ 1567}#)
                                (call-with-values
                                  (lambda ()
                                    (#{lambda-formals\ 406}# #{args\ 1565}#))
                                  (lambda (#{req\ 1568}#
                                           #{opt\ 1569}#
                                           #{rest\ 1570}#
                                           #{kw\ 1571}#)
                                    (#{chi-simple-lambda\ 407}#
                                      #{e\ 1541}#
                                      #{r\ 1542}#
                                      #{w\ 1543}#
                                      #{s\ 1544}#
                                      #{mod\ 1545}#
                                      #{req\ 1568}#
                                      #{rest\ 1570}#
                                      #f
                                      (cons #{e1\ 1566}# #{e2\ 1567}#)))))
                              #{tmp\ 1563}#)
                       ((lambda (#{_\ 1573}#)
                          (syntax-violation
                            'lambda
                            "bad lambda"
                            #{e\ 1541}#))
                        #{tmp\ 1546}#)))
                   ($sc-dispatch
                     #{tmp\ 1546}#
                     '(any any any . each-any)))))
              ($sc-dispatch
                #{tmp\ 1546}#
                '(any any any any . each-any))))
           #{e\ 1541}#)))
      (#{global-extend\ 359}#
        'core
        'lambda*
        (lambda (#{e\ 1574}#
                 #{r\ 1575}#
                 #{w\ 1576}#
                 #{s\ 1577}#
                 #{mod\ 1578}#)
          ((lambda (#{tmp\ 1579}#)
             ((lambda (#{tmp\ 1580}#)
                (if #{tmp\ 1580}#
                  (apply (lambda (#{_\ 1581}#
                                  #{args\ 1582}#
                                  #{e1\ 1583}#
                                  #{e2\ 1584}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 409}#
                                 #{e\ 1574}#
                                 #{r\ 1575}#
                                 #{w\ 1576}#
                                 #{s\ 1577}#
                                 #{mod\ 1578}#
                                 #{lambda*-formals\ 408}#
                                 (list (cons #{args\ 1582}#
                                             (cons #{e1\ 1583}#
                                                   #{e2\ 1584}#)))))
                             (lambda (#{docstring\ 1586}# #{lcase\ 1587}#)
                               (#{build-case-lambda\ 336}#
                                 #{s\ 1577}#
                                 #{docstring\ 1586}#
                                 #{lcase\ 1587}#))))
                         #{tmp\ 1580}#)
                  ((lambda (#{_\ 1588}#)
                     (syntax-violation
                       'lambda
                       "bad lambda*"
                       #{e\ 1574}#))
                   #{tmp\ 1579}#)))
              ($sc-dispatch
                #{tmp\ 1579}#
                '(any any any . each-any))))
           #{e\ 1574}#)))
      (#{global-extend\ 359}#
        'core
        'let
        (letrec ((#{chi-let\ 1589}#
                   (lambda (#{e\ 1590}#
                            #{r\ 1591}#
                            #{w\ 1592}#
                            #{s\ 1593}#
                            #{mod\ 1594}#
                            #{constructor\ 1595}#
                            #{ids\ 1596}#
                            #{vals\ 1597}#
                            #{exps\ 1598}#)
                     (if (not (#{valid-bound-ids?\ 386}# #{ids\ 1596}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 1590}#)
                       (let ((#{labels\ 1599}#
                               (#{gen-labels\ 367}# #{ids\ 1596}#))
                             (#{new-vars\ 1600}#
                               (map #{gen-var\ 411}# #{ids\ 1596}#)))
                         (let ((#{nw\ 1601}#
                                 (#{make-binding-wrap\ 378}#
                                   #{ids\ 1596}#
                                   #{labels\ 1599}#
                                   #{w\ 1592}#))
                               (#{nr\ 1602}#
                                 (#{extend-var-env\ 356}#
                                   #{labels\ 1599}#
                                   #{new-vars\ 1600}#
                                   #{r\ 1591}#)))
                           (#{constructor\ 1595}#
                             #{s\ 1593}#
                             (map syntax->datum #{ids\ 1596}#)
                             #{new-vars\ 1600}#
                             (map (lambda (#{x\ 1603}#)
                                    (#{chi\ 397}#
                                      #{x\ 1603}#
                                      #{r\ 1591}#
                                      #{w\ 1592}#
                                      #{mod\ 1594}#))
                                  #{vals\ 1597}#)
                             (#{chi-body\ 401}#
                               #{exps\ 1598}#
                               (#{source-wrap\ 390}#
                                 #{e\ 1590}#
                                 #{nw\ 1601}#
                                 #{s\ 1593}#
                                 #{mod\ 1594}#)
                               #{nr\ 1602}#
                               #{nw\ 1601}#
                               #{mod\ 1594}#))))))))
          (lambda (#{e\ 1604}#
                   #{r\ 1605}#
                   #{w\ 1606}#
                   #{s\ 1607}#
                   #{mod\ 1608}#)
            ((lambda (#{tmp\ 1609}#)
               ((lambda (#{tmp\ 1610}#)
                  (if (if #{tmp\ 1610}#
                        (apply (lambda (#{_\ 1611}#
                                        #{id\ 1612}#
                                        #{val\ 1613}#
                                        #{e1\ 1614}#
                                        #{e2\ 1615}#)
                                 (and-map #{id?\ 361}# #{id\ 1612}#))
                               #{tmp\ 1610}#)
                        #f)
                    (apply (lambda (#{_\ 1617}#
                                    #{id\ 1618}#
                                    #{val\ 1619}#
                                    #{e1\ 1620}#
                                    #{e2\ 1621}#)
                             (#{chi-let\ 1589}#
                               #{e\ 1604}#
                               #{r\ 1605}#
                               #{w\ 1606}#
                               #{s\ 1607}#
                               #{mod\ 1608}#
                               #{build-let\ 341}#
                               #{id\ 1618}#
                               #{val\ 1619}#
                               (cons #{e1\ 1620}# #{e2\ 1621}#)))
                           #{tmp\ 1610}#)
                    ((lambda (#{tmp\ 1625}#)
                       (if (if #{tmp\ 1625}#
                             (apply (lambda (#{_\ 1626}#
                                             #{f\ 1627}#
                                             #{id\ 1628}#
                                             #{val\ 1629}#
                                             #{e1\ 1630}#
                                             #{e2\ 1631}#)
                                      (if (#{id?\ 361}# #{f\ 1627}#)
                                        (and-map #{id?\ 361}# #{id\ 1628}#)
                                        #f))
                                    #{tmp\ 1625}#)
                             #f)
                         (apply (lambda (#{_\ 1633}#
                                         #{f\ 1634}#
                                         #{id\ 1635}#
                                         #{val\ 1636}#
                                         #{e1\ 1637}#
                                         #{e2\ 1638}#)
                                  (#{chi-let\ 1589}#
                                    #{e\ 1604}#
                                    #{r\ 1605}#
                                    #{w\ 1606}#
                                    #{s\ 1607}#
                                    #{mod\ 1608}#
                                    #{build-named-let\ 342}#
                                    (cons #{f\ 1634}# #{id\ 1635}#)
                                    #{val\ 1636}#
                                    (cons #{e1\ 1637}# #{e2\ 1638}#)))
                                #{tmp\ 1625}#)
                         ((lambda (#{_\ 1642}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 390}#
                                #{e\ 1604}#
                                #{w\ 1606}#
                                #{s\ 1607}#
                                #{mod\ 1608}#)))
                          #{tmp\ 1609}#)))
                     ($sc-dispatch
                       #{tmp\ 1609}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1609}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 1604}#))))
      (#{global-extend\ 359}#
        'core
        'letrec
        (lambda (#{e\ 1643}#
                 #{r\ 1644}#
                 #{w\ 1645}#
                 #{s\ 1646}#
                 #{mod\ 1647}#)
          ((lambda (#{tmp\ 1648}#)
             ((lambda (#{tmp\ 1649}#)
                (if (if #{tmp\ 1649}#
                      (apply (lambda (#{_\ 1650}#
                                      #{id\ 1651}#
                                      #{val\ 1652}#
                                      #{e1\ 1653}#
                                      #{e2\ 1654}#)
                               (and-map #{id?\ 361}# #{id\ 1651}#))
                             #{tmp\ 1649}#)
                      #f)
                  (apply (lambda (#{_\ 1656}#
                                  #{id\ 1657}#
                                  #{val\ 1658}#
                                  #{e1\ 1659}#
                                  #{e2\ 1660}#)
                           (let ((#{ids\ 1661}# #{id\ 1657}#))
                             (if (not (#{valid-bound-ids?\ 386}#
                                        #{ids\ 1661}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 1643}#)
                               (let ((#{labels\ 1663}#
                                       (#{gen-labels\ 367}# #{ids\ 1661}#))
                                     (#{new-vars\ 1664}#
                                       (map #{gen-var\ 411}# #{ids\ 1661}#)))
                                 (let ((#{w\ 1665}#
                                         (#{make-binding-wrap\ 378}#
                                           #{ids\ 1661}#
                                           #{labels\ 1663}#
                                           #{w\ 1645}#))
                                       (#{r\ 1666}#
                                         (#{extend-var-env\ 356}#
                                           #{labels\ 1663}#
                                           #{new-vars\ 1664}#
                                           #{r\ 1644}#)))
                                   (#{build-letrec\ 343}#
                                     #{s\ 1646}#
                                     (map syntax->datum #{ids\ 1661}#)
                                     #{new-vars\ 1664}#
                                     (map (lambda (#{x\ 1667}#)
                                            (#{chi\ 397}#
                                              #{x\ 1667}#
                                              #{r\ 1666}#
                                              #{w\ 1665}#
                                              #{mod\ 1647}#))
                                          #{val\ 1658}#)
                                     (#{chi-body\ 401}#
                                       (cons #{e1\ 1659}# #{e2\ 1660}#)
                                       (#{source-wrap\ 390}#
                                         #{e\ 1643}#
                                         #{w\ 1665}#
                                         #{s\ 1646}#
                                         #{mod\ 1647}#)
                                       #{r\ 1666}#
                                       #{w\ 1665}#
                                       #{mod\ 1647}#)))))))
                         #{tmp\ 1649}#)
                  ((lambda (#{_\ 1670}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 390}#
                         #{e\ 1643}#
                         #{w\ 1645}#
                         #{s\ 1646}#
                         #{mod\ 1647}#)))
                   #{tmp\ 1648}#)))
              ($sc-dispatch
                #{tmp\ 1648}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1643}#)))
      (#{global-extend\ 359}#
        'core
        'set!
        (lambda (#{e\ 1671}#
                 #{r\ 1672}#
                 #{w\ 1673}#
                 #{s\ 1674}#
                 #{mod\ 1675}#)
          ((lambda (#{tmp\ 1676}#)
             ((lambda (#{tmp\ 1677}#)
                (if (if #{tmp\ 1677}#
                      (apply (lambda (#{_\ 1678}# #{id\ 1679}# #{val\ 1680}#)
                               (#{id?\ 361}# #{id\ 1679}#))
                             #{tmp\ 1677}#)
                      #f)
                  (apply (lambda (#{_\ 1681}# #{id\ 1682}# #{val\ 1683}#)
                           (let ((#{val\ 1684}#
                                   (#{chi\ 397}#
                                     #{val\ 1683}#
                                     #{r\ 1672}#
                                     #{w\ 1673}#
                                     #{mod\ 1675}#))
                                 (#{n\ 1685}#
                                   (#{id-var-name\ 383}#
                                     #{id\ 1682}#
                                     #{w\ 1673}#)))
                             (let ((#{b\ 1686}#
                                     (#{lookup\ 358}#
                                       #{n\ 1685}#
                                       #{r\ 1672}#
                                       #{mod\ 1675}#)))
                               (let ((#{atom-key\ 1687}#
                                       (#{binding-type\ 353}# #{b\ 1686}#)))
                                 (if (memv #{atom-key\ 1687}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 329}#
                                     #{s\ 1674}#
                                     (syntax->datum #{id\ 1682}#)
                                     (#{binding-value\ 354}# #{b\ 1686}#)
                                     #{val\ 1684}#)
                                   (if (memv #{atom-key\ 1687}#
                                             '(global))
                                     (#{build-global-assignment\ 332}#
                                       #{s\ 1674}#
                                       #{n\ 1685}#
                                       #{val\ 1684}#
                                       #{mod\ 1675}#)
                                     (if (memv #{atom-key\ 1687}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 389}#
                                           #{id\ 1682}#
                                           #{w\ 1673}#
                                           #{mod\ 1675}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 390}#
                                           #{e\ 1671}#
                                           #{w\ 1673}#
                                           #{s\ 1674}#
                                           #{mod\ 1675}#)))))))))
                         #{tmp\ 1677}#)
                  ((lambda (#{tmp\ 1688}#)
                     (if #{tmp\ 1688}#
                       (apply (lambda (#{_\ 1689}#
                                       #{head\ 1690}#
                                       #{tail\ 1691}#
                                       #{val\ 1692}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 395}#
                                      #{head\ 1690}#
                                      #{r\ 1672}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 1675}#
                                      #t))
                                  (lambda (#{type\ 1693}#
                                           #{value\ 1694}#
                                           #{ee\ 1695}#
                                           #{ww\ 1696}#
                                           #{ss\ 1697}#
                                           #{modmod\ 1698}#)
                                    (if (memv #{type\ 1693}#
                                              '(module-ref))
                                      (let ((#{val\ 1699}#
                                              (#{chi\ 397}#
                                                #{val\ 1692}#
                                                #{r\ 1672}#
                                                #{w\ 1673}#
                                                #{mod\ 1675}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 1694}#
                                              (cons #{head\ 1690}#
                                                    #{tail\ 1691}#)))
                                          (lambda (#{id\ 1701}# #{mod\ 1702}#)
                                            (#{build-global-assignment\ 332}#
                                              #{s\ 1674}#
                                              #{id\ 1701}#
                                              #{val\ 1699}#
                                              #{mod\ 1702}#))))
                                      (#{build-application\ 326}#
                                        #{s\ 1674}#
                                        (#{chi\ 397}#
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
                                                #{head\ 1690}#)
                                          #{r\ 1672}#
                                          #{w\ 1673}#
                                          #{mod\ 1675}#)
                                        (map (lambda (#{e\ 1703}#)
                                               (#{chi\ 397}#
                                                 #{e\ 1703}#
                                                 #{r\ 1672}#
                                                 #{w\ 1673}#
                                                 #{mod\ 1675}#))
                                             (append
                                               #{tail\ 1691}#
                                               (list #{val\ 1692}#))))))))
                              #{tmp\ 1688}#)
                       ((lambda (#{_\ 1705}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 390}#
                              #{e\ 1671}#
                              #{w\ 1673}#
                              #{s\ 1674}#
                              #{mod\ 1675}#)))
                        #{tmp\ 1676}#)))
                   ($sc-dispatch
                     #{tmp\ 1676}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 1676}#
                '(any any any))))
           #{e\ 1671}#)))
      (#{global-extend\ 359}#
        'module-ref
        '@
        (lambda (#{e\ 1706}#)
          ((lambda (#{tmp\ 1707}#)
             ((lambda (#{tmp\ 1708}#)
                (if (if #{tmp\ 1708}#
                      (apply (lambda (#{_\ 1709}# #{mod\ 1710}# #{id\ 1711}#)
                               (if (and-map #{id?\ 361}# #{mod\ 1710}#)
                                 (#{id?\ 361}# #{id\ 1711}#)
                                 #f))
                             #{tmp\ 1708}#)
                      #f)
                  (apply (lambda (#{_\ 1713}# #{mod\ 1714}# #{id\ 1715}#)
                           (values
                             (syntax->datum #{id\ 1715}#)
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
                                     #{mod\ 1714}#))))
                         #{tmp\ 1708}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1707}#)))
              ($sc-dispatch
                #{tmp\ 1707}#
                '(any each-any any))))
           #{e\ 1706}#)))
      (#{global-extend\ 359}#
        'module-ref
        '@@
        (lambda (#{e\ 1717}#)
          ((lambda (#{tmp\ 1718}#)
             ((lambda (#{tmp\ 1719}#)
                (if (if #{tmp\ 1719}#
                      (apply (lambda (#{_\ 1720}# #{mod\ 1721}# #{id\ 1722}#)
                               (if (and-map #{id?\ 361}# #{mod\ 1721}#)
                                 (#{id?\ 361}# #{id\ 1722}#)
                                 #f))
                             #{tmp\ 1719}#)
                      #f)
                  (apply (lambda (#{_\ 1724}# #{mod\ 1725}# #{id\ 1726}#)
                           (values
                             (syntax->datum #{id\ 1726}#)
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
                                     #{mod\ 1725}#))))
                         #{tmp\ 1719}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1718}#)))
              ($sc-dispatch
                #{tmp\ 1718}#
                '(any each-any any))))
           #{e\ 1717}#)))
      (#{global-extend\ 359}#
        'core
        'if
        (lambda (#{e\ 1728}#
                 #{r\ 1729}#
                 #{w\ 1730}#
                 #{s\ 1731}#
                 #{mod\ 1732}#)
          ((lambda (#{tmp\ 1733}#)
             ((lambda (#{tmp\ 1734}#)
                (if #{tmp\ 1734}#
                  (apply (lambda (#{_\ 1735}# #{test\ 1736}# #{then\ 1737}#)
                           (#{build-conditional\ 327}#
                             #{s\ 1731}#
                             (#{chi\ 397}#
                               #{test\ 1736}#
                               #{r\ 1729}#
                               #{w\ 1730}#
                               #{mod\ 1732}#)
                             (#{chi\ 397}#
                               #{then\ 1737}#
                               #{r\ 1729}#
                               #{w\ 1730}#
                               #{mod\ 1732}#)
                             (#{build-void\ 325}# #f)))
                         #{tmp\ 1734}#)
                  ((lambda (#{tmp\ 1738}#)
                     (if #{tmp\ 1738}#
                       (apply (lambda (#{_\ 1739}#
                                       #{test\ 1740}#
                                       #{then\ 1741}#
                                       #{else\ 1742}#)
                                (#{build-conditional\ 327}#
                                  #{s\ 1731}#
                                  (#{chi\ 397}#
                                    #{test\ 1740}#
                                    #{r\ 1729}#
                                    #{w\ 1730}#
                                    #{mod\ 1732}#)
                                  (#{chi\ 397}#
                                    #{then\ 1741}#
                                    #{r\ 1729}#
                                    #{w\ 1730}#
                                    #{mod\ 1732}#)
                                  (#{chi\ 397}#
                                    #{else\ 1742}#
                                    #{r\ 1729}#
                                    #{w\ 1730}#
                                    #{mod\ 1732}#)))
                              #{tmp\ 1738}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1733}#)))
                   ($sc-dispatch
                     #{tmp\ 1733}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 1733}#
                '(any any any))))
           #{e\ 1728}#)))
      (#{global-extend\ 359}#
        'begin
        'begin
        '())
      (#{global-extend\ 359}#
        'define
        'define
        '())
      (#{global-extend\ 359}#
        'define-syntax
        'define-syntax
        '())
      (#{global-extend\ 359}#
        'eval-when
        'eval-when
        '())
      (#{global-extend\ 359}#
        'core
        'syntax-case
        (letrec ((#{gen-syntax-case\ 1746}#
                   (lambda (#{x\ 1747}#
                            #{keys\ 1748}#
                            #{clauses\ 1749}#
                            #{r\ 1750}#
                            #{mod\ 1751}#)
                     (if (null? #{clauses\ 1749}#)
                       (#{build-application\ 326}#
                         #f
                         (#{build-primref\ 338}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 339}# #f #f)
                               (#{build-data\ 339}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 1747}#))
                       ((lambda (#{tmp\ 1752}#)
                          ((lambda (#{tmp\ 1753}#)
                             (if #{tmp\ 1753}#
                               (apply (lambda (#{pat\ 1754}# #{exp\ 1755}#)
                                        (if (if (#{id?\ 361}# #{pat\ 1754}#)
                                              (and-map
                                                (lambda (#{x\ 1756}#)
                                                  (not (#{free-id=?\ 384}#
                                                         #{pat\ 1754}#
                                                         #{x\ 1756}#)))
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
                                                      #{keys\ 1748}#))
                                              #f)
                                          (let ((#{labels\ 1757}#
                                                  (list (#{gen-label\ 366}#)))
                                                (#{var\ 1758}#
                                                  (#{gen-var\ 411}#
                                                    #{pat\ 1754}#)))
                                            (#{build-application\ 326}#
                                              #f
                                              (#{build-simple-lambda\ 335}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 1754}#))
                                                #f
                                                (list #{var\ 1758}#)
                                                #f
                                                (#{chi\ 397}#
                                                  #{exp\ 1755}#
                                                  (#{extend-env\ 355}#
                                                    #{labels\ 1757}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 1758}#
                                                                      0)))
                                                    #{r\ 1750}#)
                                                  (#{make-binding-wrap\ 378}#
                                                    (list #{pat\ 1754}#)
                                                    #{labels\ 1757}#
                                                    '(()))
                                                  #{mod\ 1751}#))
                                              (list #{x\ 1747}#)))
                                          (#{gen-clause\ 1745}#
                                            #{x\ 1747}#
                                            #{keys\ 1748}#
                                            (cdr #{clauses\ 1749}#)
                                            #{r\ 1750}#
                                            #{pat\ 1754}#
                                            #t
                                            #{exp\ 1755}#
                                            #{mod\ 1751}#)))
                                      #{tmp\ 1753}#)
                               ((lambda (#{tmp\ 1759}#)
                                  (if #{tmp\ 1759}#
                                    (apply (lambda (#{pat\ 1760}#
                                                    #{fender\ 1761}#
                                                    #{exp\ 1762}#)
                                             (#{gen-clause\ 1745}#
                                               #{x\ 1747}#
                                               #{keys\ 1748}#
                                               (cdr #{clauses\ 1749}#)
                                               #{r\ 1750}#
                                               #{pat\ 1760}#
                                               #{fender\ 1761}#
                                               #{exp\ 1762}#
                                               #{mod\ 1751}#))
                                           #{tmp\ 1759}#)
                                    ((lambda (#{_\ 1763}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 1749}#)))
                                     #{tmp\ 1752}#)))
                                ($sc-dispatch
                                  #{tmp\ 1752}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 1752}# (quote (any any)))))
                        (car #{clauses\ 1749}#)))))
                 (#{gen-clause\ 1745}#
                   (lambda (#{x\ 1764}#
                            #{keys\ 1765}#
                            #{clauses\ 1766}#
                            #{r\ 1767}#
                            #{pat\ 1768}#
                            #{fender\ 1769}#
                            #{exp\ 1770}#
                            #{mod\ 1771}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 1743}#
                           #{pat\ 1768}#
                           #{keys\ 1765}#))
                       (lambda (#{p\ 1772}# #{pvars\ 1773}#)
                         (if (not (#{distinct-bound-ids?\ 387}#
                                    (map car #{pvars\ 1773}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 1768}#)
                           (if (not (and-map
                                      (lambda (#{x\ 1774}#)
                                        (not (#{ellipsis?\ 405}#
                                               (car #{x\ 1774}#))))
                                      #{pvars\ 1773}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 1768}#)
                             (let ((#{y\ 1775}#
                                     (#{gen-var\ 411}# (quote tmp))))
                               (#{build-application\ 326}#
                                 #f
                                 (#{build-simple-lambda\ 335}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 1775}#)
                                   #f
                                   (let ((#{y\ 1776}#
                                           (#{build-lexical-reference\ 328}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 1775}#)))
                                     (#{build-conditional\ 327}#
                                       #f
                                       ((lambda (#{tmp\ 1777}#)
                                          ((lambda (#{tmp\ 1778}#)
                                             (if #{tmp\ 1778}#
                                               (apply (lambda () #{y\ 1776}#)
                                                      #{tmp\ 1778}#)
                                               ((lambda (#{_\ 1779}#)
                                                  (#{build-conditional\ 327}#
                                                    #f
                                                    #{y\ 1776}#
                                                    (#{build-dispatch-call\ 1744}#
                                                      #{pvars\ 1773}#
                                                      #{fender\ 1769}#
                                                      #{y\ 1776}#
                                                      #{r\ 1767}#
                                                      #{mod\ 1771}#)
                                                    (#{build-data\ 339}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 1777}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1777}#
                                             '#(atom #t))))
                                        #{fender\ 1769}#)
                                       (#{build-dispatch-call\ 1744}#
                                         #{pvars\ 1773}#
                                         #{exp\ 1770}#
                                         #{y\ 1776}#
                                         #{r\ 1767}#
                                         #{mod\ 1771}#)
                                       (#{gen-syntax-case\ 1746}#
                                         #{x\ 1764}#
                                         #{keys\ 1765}#
                                         #{clauses\ 1766}#
                                         #{r\ 1767}#
                                         #{mod\ 1771}#))))
                                 (list (if (eq? #{p\ 1772}# (quote any))
                                         (#{build-application\ 326}#
                                           #f
                                           (#{build-primref\ 338}#
                                             #f
                                             'list)
                                           (list #{x\ 1764}#))
                                         (#{build-application\ 326}#
                                           #f
                                           (#{build-primref\ 338}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 1764}#
                                                 (#{build-data\ 339}#
                                                   #f
                                                   #{p\ 1772}#)))))))))))))
                 (#{build-dispatch-call\ 1744}#
                   (lambda (#{pvars\ 1780}#
                            #{exp\ 1781}#
                            #{y\ 1782}#
                            #{r\ 1783}#
                            #{mod\ 1784}#)
                     (let ((#{ids\ 1785}# (map car #{pvars\ 1780}#))
                           (#{levels\ 1786}# (map cdr #{pvars\ 1780}#)))
                       (let ((#{labels\ 1787}#
                               (#{gen-labels\ 367}# #{ids\ 1785}#))
                             (#{new-vars\ 1788}#
                               (map #{gen-var\ 411}# #{ids\ 1785}#)))
                         (#{build-application\ 326}#
                           #f
                           (#{build-primref\ 338}# #f (quote apply))
                           (list (#{build-simple-lambda\ 335}#
                                   #f
                                   (map syntax->datum #{ids\ 1785}#)
                                   #f
                                   #{new-vars\ 1788}#
                                   #f
                                   (#{chi\ 397}#
                                     #{exp\ 1781}#
                                     (#{extend-env\ 355}#
                                       #{labels\ 1787}#
                                       (map (lambda (#{var\ 1789}#
                                                     #{level\ 1790}#)
                                              (cons 'syntax
                                                    (cons #{var\ 1789}#
                                                          #{level\ 1790}#)))
                                            #{new-vars\ 1788}#
                                            (map cdr #{pvars\ 1780}#))
                                       #{r\ 1783}#)
                                     (#{make-binding-wrap\ 378}#
                                       #{ids\ 1785}#
                                       #{labels\ 1787}#
                                       '(()))
                                     #{mod\ 1784}#))
                                 #{y\ 1782}#))))))
                 (#{convert-pattern\ 1743}#
                   (lambda (#{pattern\ 1791}# #{keys\ 1792}#)
                     (letrec ((#{cvt\ 1793}#
                                (lambda (#{p\ 1794}# #{n\ 1795}# #{ids\ 1796}#)
                                  (if (#{id?\ 361}# #{p\ 1794}#)
                                    (if (#{bound-id-member?\ 388}#
                                          #{p\ 1794}#
                                          #{keys\ 1792}#)
                                      (values
                                        (vector (quote free-id) #{p\ 1794}#)
                                        #{ids\ 1796}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 1794}# #{n\ 1795}#)
                                              #{ids\ 1796}#)))
                                    ((lambda (#{tmp\ 1797}#)
                                       ((lambda (#{tmp\ 1798}#)
                                          (if (if #{tmp\ 1798}#
                                                (apply (lambda (#{x\ 1799}#
                                                                #{dots\ 1800}#)
                                                         (#{ellipsis?\ 405}#
                                                           #{dots\ 1800}#))
                                                       #{tmp\ 1798}#)
                                                #f)
                                            (apply (lambda (#{x\ 1801}#
                                                            #{dots\ 1802}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 1793}#
                                                           #{x\ 1801}#
                                                           (#{fx+\ 316}#
                                                             #{n\ 1795}#
                                                             1)
                                                           #{ids\ 1796}#))
                                                       (lambda (#{p\ 1803}#
                                                                #{ids\ 1804}#)
                                                         (values
                                                           (if (eq? #{p\ 1803}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 1803}#))
                                                           #{ids\ 1804}#))))
                                                   #{tmp\ 1798}#)
                                            ((lambda (#{tmp\ 1805}#)
                                               (if #{tmp\ 1805}#
                                                 (apply (lambda (#{x\ 1806}#
                                                                 #{y\ 1807}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 1793}#
                                                                #{y\ 1807}#
                                                                #{n\ 1795}#
                                                                #{ids\ 1796}#))
                                                            (lambda (#{y\ 1808}#
                                                                     #{ids\ 1809}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 1793}#
                                                                    #{x\ 1806}#
                                                                    #{n\ 1795}#
                                                                    #{ids\ 1809}#))
                                                                (lambda (#{x\ 1810}#
                                                                         #{ids\ 1811}#)
                                                                  (values
                                                                    (cons #{x\ 1810}#
                                                                          #{y\ 1808}#)
                                                                    #{ids\ 1811}#))))))
                                                        #{tmp\ 1805}#)
                                                 ((lambda (#{tmp\ 1812}#)
                                                    (if #{tmp\ 1812}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 1796}#))
                                                             #{tmp\ 1812}#)
                                                      ((lambda (#{tmp\ 1813}#)
                                                         (if #{tmp\ 1813}#
                                                           (apply (lambda (#{x\ 1814}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 1793}#
                                                                          #{x\ 1814}#
                                                                          #{n\ 1795}#
                                                                          #{ids\ 1796}#))
                                                                      (lambda (#{p\ 1816}#
                                                                               #{ids\ 1817}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 1816}#)
                                                                          #{ids\ 1817}#))))
                                                                  #{tmp\ 1813}#)
                                                           ((lambda (#{x\ 1818}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 410}#
                                                                    #{p\ 1794}#
                                                                    '(())))
                                                                #{ids\ 1796}#))
                                                            #{tmp\ 1797}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1797}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 1797}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 1797}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 1797}#
                                          '(any any))))
                                     #{p\ 1794}#)))))
                       (#{cvt\ 1793}# #{pattern\ 1791}# 0 (quote ()))))))
          (lambda (#{e\ 1819}#
                   #{r\ 1820}#
                   #{w\ 1821}#
                   #{s\ 1822}#
                   #{mod\ 1823}#)
            (let ((#{e\ 1824}#
                    (#{source-wrap\ 390}#
                      #{e\ 1819}#
                      #{w\ 1821}#
                      #{s\ 1822}#
                      #{mod\ 1823}#)))
              ((lambda (#{tmp\ 1825}#)
                 ((lambda (#{tmp\ 1826}#)
                    (if #{tmp\ 1826}#
                      (apply (lambda (#{_\ 1827}#
                                      #{val\ 1828}#
                                      #{key\ 1829}#
                                      #{m\ 1830}#)
                               (if (and-map
                                     (lambda (#{x\ 1831}#)
                                       (if (#{id?\ 361}# #{x\ 1831}#)
                                         (not (#{ellipsis?\ 405}# #{x\ 1831}#))
                                         #f))
                                     #{key\ 1829}#)
                                 (let ((#{x\ 1833}#
                                         (#{gen-var\ 411}# (quote tmp))))
                                   (#{build-application\ 326}#
                                     #{s\ 1822}#
                                     (#{build-simple-lambda\ 335}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 1833}#)
                                       #f
                                       (#{gen-syntax-case\ 1746}#
                                         (#{build-lexical-reference\ 328}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 1833}#)
                                         #{key\ 1829}#
                                         #{m\ 1830}#
                                         #{r\ 1820}#
                                         #{mod\ 1823}#))
                                     (list (#{chi\ 397}#
                                             #{val\ 1828}#
                                             #{r\ 1820}#
                                             '(())
                                             #{mod\ 1823}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 1824}#)))
                             #{tmp\ 1826}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 1825}#)))
                  ($sc-dispatch
                    #{tmp\ 1825}#
                    '(any any each-any . each-any))))
               #{e\ 1824}#)))))
      (set! sc-expand
        (lambda (#{x\ 1836}# . #{rest\ 1837}#)
          (if (if (pair? #{x\ 1836}#)
                (equal? (car #{x\ 1836}#) #{noexpand\ 314}#)
                #f)
            (cadr #{x\ 1836}#)
            (let ((#{m\ 1838}#
                    (if (null? #{rest\ 1837}#)
                      'e
                      (car #{rest\ 1837}#)))
                  (#{esew\ 1839}#
                    (if (let ((#{t\ 1840}# (null? #{rest\ 1837}#)))
                          (if #{t\ 1840}#
                            #{t\ 1840}#
                            (null? (cdr #{rest\ 1837}#))))
                      '(eval)
                      (cadr #{rest\ 1837}#))))
              (with-fluid*
                #{*mode*\ 315}#
                #{m\ 1838}#
                (lambda ()
                  (#{chi-top\ 396}#
                    #{x\ 1836}#
                    '()
                    '((top))
                    #{m\ 1838}#
                    #{esew\ 1839}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 1841}#)
          (#{nonsymbol-id?\ 360}# #{x\ 1841}#)))
      (set! datum->syntax
        (lambda (#{id\ 1842}# #{datum\ 1843}#)
          (#{make-syntax-object\ 344}#
            #{datum\ 1843}#
            (#{syntax-object-wrap\ 347}# #{id\ 1842}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 1844}#)
          (#{strip\ 410}# #{x\ 1844}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 1845}#)
          (begin
            (let ((#{x\ 1846}# #{ls\ 1845}#))
              (if (not (list? #{x\ 1846}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 1846}#)))
            (map (lambda (#{x\ 1847}#)
                   (#{wrap\ 389}# (gensym) (quote ((top))) #f))
                 #{ls\ 1845}#))))
      (set! free-identifier=?
        (lambda (#{x\ 1848}# #{y\ 1849}#)
          (begin
            (let ((#{x\ 1850}# #{x\ 1848}#))
              (if (not (#{nonsymbol-id?\ 360}# #{x\ 1850}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1850}#)))
            (let ((#{x\ 1851}# #{y\ 1849}#))
              (if (not (#{nonsymbol-id?\ 360}# #{x\ 1851}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1851}#)))
            (#{free-id=?\ 384}# #{x\ 1848}# #{y\ 1849}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 1852}# #{y\ 1853}#)
          (begin
            (let ((#{x\ 1854}# #{x\ 1852}#))
              (if (not (#{nonsymbol-id?\ 360}# #{x\ 1854}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1854}#)))
            (let ((#{x\ 1855}# #{y\ 1853}#))
              (if (not (#{nonsymbol-id?\ 360}# #{x\ 1855}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1855}#)))
            (#{bound-id=?\ 385}# #{x\ 1852}# #{y\ 1853}#))))
      (set! syntax-violation
        (lambda (#{who\ 1856}#
                 #{message\ 1857}#
                 #{form\ 1858}#
                 .
                 #{subform\ 1859}#)
          (begin
            (let ((#{x\ 1860}# #{who\ 1856}#))
              (if (not ((lambda (#{x\ 1861}#)
                          (let ((#{t\ 1862}# (not #{x\ 1861}#)))
                            (if #{t\ 1862}#
                              #{t\ 1862}#
                              (let ((#{t\ 1863}# (string? #{x\ 1861}#)))
                                (if #{t\ 1863}#
                                  #{t\ 1863}#
                                  (symbol? #{x\ 1861}#))))))
                        #{x\ 1860}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1860}#)))
            (let ((#{x\ 1864}# #{message\ 1857}#))
              (if (not (string? #{x\ 1864}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1864}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 1856}# "~a: " "")
                "~a "
                (if (null? #{subform\ 1859}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 1865}#
                      (cons #{message\ 1857}#
                            (map (lambda (#{x\ 1866}#)
                                   (#{strip\ 410}# #{x\ 1866}# (quote (()))))
                                 (append
                                   #{subform\ 1859}#
                                   (list #{form\ 1858}#))))))
                (if #{who\ 1856}#
                  (cons #{who\ 1856}# #{tail\ 1865}#)
                  #{tail\ 1865}#))
              #f))))
      (letrec ((#{match\ 1871}#
                 (lambda (#{e\ 1872}#
                          #{p\ 1873}#
                          #{w\ 1874}#
                          #{r\ 1875}#
                          #{mod\ 1876}#)
                   (if (not #{r\ 1875}#)
                     #f
                     (if (eq? #{p\ 1873}# (quote any))
                       (cons (#{wrap\ 389}#
                               #{e\ 1872}#
                               #{w\ 1874}#
                               #{mod\ 1876}#)
                             #{r\ 1875}#)
                       (if (#{syntax-object?\ 345}# #{e\ 1872}#)
                         (#{match*\ 1870}#
                           (#{syntax-object-expression\ 346}# #{e\ 1872}#)
                           #{p\ 1873}#
                           (#{join-wraps\ 380}#
                             #{w\ 1874}#
                             (#{syntax-object-wrap\ 347}# #{e\ 1872}#))
                           #{r\ 1875}#
                           (#{syntax-object-module\ 348}# #{e\ 1872}#))
                         (#{match*\ 1870}#
                           #{e\ 1872}#
                           #{p\ 1873}#
                           #{w\ 1874}#
                           #{r\ 1875}#
                           #{mod\ 1876}#))))))
               (#{match*\ 1870}#
                 (lambda (#{e\ 1877}#
                          #{p\ 1878}#
                          #{w\ 1879}#
                          #{r\ 1880}#
                          #{mod\ 1881}#)
                   (if (null? #{p\ 1878}#)
                     (if (null? #{e\ 1877}#) #{r\ 1880}# #f)
                     (if (pair? #{p\ 1878}#)
                       (if (pair? #{e\ 1877}#)
                         (#{match\ 1871}#
                           (car #{e\ 1877}#)
                           (car #{p\ 1878}#)
                           #{w\ 1879}#
                           (#{match\ 1871}#
                             (cdr #{e\ 1877}#)
                             (cdr #{p\ 1878}#)
                             #{w\ 1879}#
                             #{r\ 1880}#
                             #{mod\ 1881}#)
                           #{mod\ 1881}#)
                         #f)
                       (if (eq? #{p\ 1878}# (quote each-any))
                         (let ((#{l\ 1882}#
                                 (#{match-each-any\ 1868}#
                                   #{e\ 1877}#
                                   #{w\ 1879}#
                                   #{mod\ 1881}#)))
                           (if #{l\ 1882}#
                             (cons #{l\ 1882}# #{r\ 1880}#)
                             #f))
                         (let ((#{atom-key\ 1883}# (vector-ref #{p\ 1878}# 0)))
                           (if (memv #{atom-key\ 1883}# (quote (each)))
                             (if (null? #{e\ 1877}#)
                               (#{match-empty\ 1869}#
                                 (vector-ref #{p\ 1878}# 1)
                                 #{r\ 1880}#)
                               (let ((#{l\ 1884}#
                                       (#{match-each\ 1867}#
                                         #{e\ 1877}#
                                         (vector-ref #{p\ 1878}# 1)
                                         #{w\ 1879}#
                                         #{mod\ 1881}#)))
                                 (if #{l\ 1884}#
                                   (letrec ((#{collect\ 1885}#
                                              (lambda (#{l\ 1886}#)
                                                (if (null? (car #{l\ 1886}#))
                                                  #{r\ 1880}#
                                                  (cons (map car #{l\ 1886}#)
                                                        (#{collect\ 1885}#
                                                          (map cdr
                                                               #{l\ 1886}#)))))))
                                     (#{collect\ 1885}# #{l\ 1884}#))
                                   #f)))
                             (if (memv #{atom-key\ 1883}# (quote (free-id)))
                               (if (#{id?\ 361}# #{e\ 1877}#)
                                 (if (#{free-id=?\ 384}#
                                       (#{wrap\ 389}#
                                         #{e\ 1877}#
                                         #{w\ 1879}#
                                         #{mod\ 1881}#)
                                       (vector-ref #{p\ 1878}# 1))
                                   #{r\ 1880}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 1883}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 1878}# 1)
                                       (#{strip\ 410}#
                                         #{e\ 1877}#
                                         #{w\ 1879}#))
                                   #{r\ 1880}#
                                   #f)
                                 (if (memv #{atom-key\ 1883}# (quote (vector)))
                                   (if (vector? #{e\ 1877}#)
                                     (#{match\ 1871}#
                                       (vector->list #{e\ 1877}#)
                                       (vector-ref #{p\ 1878}# 1)
                                       #{w\ 1879}#
                                       #{r\ 1880}#
                                       #{mod\ 1881}#)
                                     #f)))))))))))
               (#{match-empty\ 1869}#
                 (lambda (#{p\ 1887}# #{r\ 1888}#)
                   (if (null? #{p\ 1887}#)
                     #{r\ 1888}#
                     (if (eq? #{p\ 1887}# (quote any))
                       (cons (quote ()) #{r\ 1888}#)
                       (if (pair? #{p\ 1887}#)
                         (#{match-empty\ 1869}#
                           (car #{p\ 1887}#)
                           (#{match-empty\ 1869}#
                             (cdr #{p\ 1887}#)
                             #{r\ 1888}#))
                         (if (eq? #{p\ 1887}# (quote each-any))
                           (cons (quote ()) #{r\ 1888}#)
                           (let ((#{atom-key\ 1889}#
                                   (vector-ref #{p\ 1887}# 0)))
                             (if (memv #{atom-key\ 1889}# (quote (each)))
                               (#{match-empty\ 1869}#
                                 (vector-ref #{p\ 1887}# 1)
                                 #{r\ 1888}#)
                               (if (memv #{atom-key\ 1889}#
                                         '(free-id atom))
                                 #{r\ 1888}#
                                 (if (memv #{atom-key\ 1889}# (quote (vector)))
                                   (#{match-empty\ 1869}#
                                     (vector-ref #{p\ 1887}# 1)
                                     #{r\ 1888}#)))))))))))
               (#{match-each-any\ 1868}#
                 (lambda (#{e\ 1890}# #{w\ 1891}# #{mod\ 1892}#)
                   (if (pair? #{e\ 1890}#)
                     (let ((#{l\ 1893}#
                             (#{match-each-any\ 1868}#
                               (cdr #{e\ 1890}#)
                               #{w\ 1891}#
                               #{mod\ 1892}#)))
                       (if #{l\ 1893}#
                         (cons (#{wrap\ 389}#
                                 (car #{e\ 1890}#)
                                 #{w\ 1891}#
                                 #{mod\ 1892}#)
                               #{l\ 1893}#)
                         #f))
                     (if (null? #{e\ 1890}#)
                       '()
                       (if (#{syntax-object?\ 345}# #{e\ 1890}#)
                         (#{match-each-any\ 1868}#
                           (#{syntax-object-expression\ 346}# #{e\ 1890}#)
                           (#{join-wraps\ 380}#
                             #{w\ 1891}#
                             (#{syntax-object-wrap\ 347}# #{e\ 1890}#))
                           #{mod\ 1892}#)
                         #f)))))
               (#{match-each\ 1867}#
                 (lambda (#{e\ 1894}#
                          #{p\ 1895}#
                          #{w\ 1896}#
                          #{mod\ 1897}#)
                   (if (pair? #{e\ 1894}#)
                     (let ((#{first\ 1898}#
                             (#{match\ 1871}#
                               (car #{e\ 1894}#)
                               #{p\ 1895}#
                               #{w\ 1896}#
                               '()
                               #{mod\ 1897}#)))
                       (if #{first\ 1898}#
                         (let ((#{rest\ 1899}#
                                 (#{match-each\ 1867}#
                                   (cdr #{e\ 1894}#)
                                   #{p\ 1895}#
                                   #{w\ 1896}#
                                   #{mod\ 1897}#)))
                           (if #{rest\ 1899}#
                             (cons #{first\ 1898}# #{rest\ 1899}#)
                             #f))
                         #f))
                     (if (null? #{e\ 1894}#)
                       '()
                       (if (#{syntax-object?\ 345}# #{e\ 1894}#)
                         (#{match-each\ 1867}#
                           (#{syntax-object-expression\ 346}# #{e\ 1894}#)
                           #{p\ 1895}#
                           (#{join-wraps\ 380}#
                             #{w\ 1896}#
                             (#{syntax-object-wrap\ 347}# #{e\ 1894}#))
                           (#{syntax-object-module\ 348}# #{e\ 1894}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 1900}# #{p\ 1901}#)
            (if (eq? #{p\ 1901}# (quote any))
              (list #{e\ 1900}#)
              (if (#{syntax-object?\ 345}# #{e\ 1900}#)
                (#{match*\ 1870}#
                  (#{syntax-object-expression\ 346}# #{e\ 1900}#)
                  #{p\ 1901}#
                  (#{syntax-object-wrap\ 347}# #{e\ 1900}#)
                  '()
                  (#{syntax-object-module\ 348}# #{e\ 1900}#))
                (#{match*\ 1870}#
                  #{e\ 1900}#
                  #{p\ 1901}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1902}#)
      ((lambda (#{tmp\ 1903}#)
         ((lambda (#{tmp\ 1904}#)
            (if #{tmp\ 1904}#
              (apply (lambda (#{_\ 1905}# #{e1\ 1906}# #{e2\ 1907}#)
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
                             (cons #{e1\ 1906}# #{e2\ 1907}#)))
                     #{tmp\ 1904}#)
              ((lambda (#{tmp\ 1909}#)
                 (if #{tmp\ 1909}#
                   (apply (lambda (#{_\ 1910}#
                                   #{out\ 1911}#
                                   #{in\ 1912}#
                                   #{e1\ 1913}#
                                   #{e2\ 1914}#)
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
                                  #{in\ 1912}#
                                  '()
                                  (list #{out\ 1911}#
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
                                              (cons #{e1\ 1913}#
                                                    #{e2\ 1914}#)))))
                          #{tmp\ 1909}#)
                   ((lambda (#{tmp\ 1916}#)
                      (if #{tmp\ 1916}#
                        (apply (lambda (#{_\ 1917}#
                                        #{out\ 1918}#
                                        #{in\ 1919}#
                                        #{e1\ 1920}#
                                        #{e2\ 1921}#)
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
                                             #{in\ 1919}#)
                                       '()
                                       (list #{out\ 1918}#
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
                                                   (cons #{e1\ 1920}#
                                                         #{e2\ 1921}#)))))
                               #{tmp\ 1916}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 1903}#)))
                    ($sc-dispatch
                      #{tmp\ 1903}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 1903}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 1903}#
            '(any () any . each-any))))
       #{x\ 1902}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1925}#)
      ((lambda (#{tmp\ 1926}#)
         ((lambda (#{tmp\ 1927}#)
            (if #{tmp\ 1927}#
              (apply (lambda (#{_\ 1928}#
                              #{k\ 1929}#
                              #{keyword\ 1930}#
                              #{pattern\ 1931}#
                              #{template\ 1932}#)
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
                                         (cons #{k\ 1929}#
                                               (map (lambda (#{tmp\ 1935}#
                                                             #{tmp\ 1934}#)
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
                                                                  #{tmp\ 1934}#)
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
                                                                  #{tmp\ 1935}#)))
                                                    #{template\ 1932}#
                                                    #{pattern\ 1931}#))))))
                     #{tmp\ 1927}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1926}#)))
          ($sc-dispatch
            #{tmp\ 1926}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 1925}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 1936}#)
      ((lambda (#{tmp\ 1937}#)
         ((lambda (#{tmp\ 1938}#)
            (if (if #{tmp\ 1938}#
                  (apply (lambda (#{let*\ 1939}#
                                  #{x\ 1940}#
                                  #{v\ 1941}#
                                  #{e1\ 1942}#
                                  #{e2\ 1943}#)
                           (and-map identifier? #{x\ 1940}#))
                         #{tmp\ 1938}#)
                  #f)
              (apply (lambda (#{let*\ 1945}#
                              #{x\ 1946}#
                              #{v\ 1947}#
                              #{e1\ 1948}#
                              #{e2\ 1949}#)
                       (letrec ((#{f\ 1950}#
                                  (lambda (#{bindings\ 1951}#)
                                    (if (null? #{bindings\ 1951}#)
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
                                                  (cons #{e1\ 1948}#
                                                        #{e2\ 1949}#)))
                                      ((lambda (#{tmp\ 1955}#)
                                         ((lambda (#{tmp\ 1956}#)
                                            (if #{tmp\ 1956}#
                                              (apply (lambda (#{body\ 1957}#
                                                              #{binding\ 1958}#)
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
                                                             (list #{binding\ 1958}#)
                                                             #{body\ 1957}#))
                                                     #{tmp\ 1956}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 1955}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1955}#
                                            '(any any))))
                                       (list (#{f\ 1950}#
                                               (cdr #{bindings\ 1951}#))
                                             (car #{bindings\ 1951}#)))))))
                         (#{f\ 1950}# (map list #{x\ 1946}# #{v\ 1947}#))))
                     #{tmp\ 1938}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1937}#)))
          ($sc-dispatch
            #{tmp\ 1937}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 1936}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 1959}#)
      ((lambda (#{tmp\ 1960}#)
         ((lambda (#{tmp\ 1961}#)
            (if #{tmp\ 1961}#
              (apply (lambda (#{_\ 1962}#
                              #{var\ 1963}#
                              #{init\ 1964}#
                              #{step\ 1965}#
                              #{e0\ 1966}#
                              #{e1\ 1967}#
                              #{c\ 1968}#)
                       ((lambda (#{tmp\ 1969}#)
                          ((lambda (#{tmp\ 1970}#)
                             (if #{tmp\ 1970}#
                               (apply (lambda (#{step\ 1971}#)
                                        ((lambda (#{tmp\ 1972}#)
                                           ((lambda (#{tmp\ 1973}#)
                                              (if #{tmp\ 1973}#
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
                                                                    #{var\ 1963}#
                                                                    #{init\ 1964}#)
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
                                                                           #{e0\ 1966}#)
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
                                                                             #{c\ 1968}#
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
                                                                                         #{step\ 1971}#)))))))
                                                       #{tmp\ 1973}#)
                                                ((lambda (#{tmp\ 1978}#)
                                                   (if #{tmp\ 1978}#
                                                     (apply (lambda (#{e1\ 1979}#
                                                                     #{e2\ 1980}#)
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
                                                                         #{var\ 1963}#
                                                                         #{init\ 1964}#)
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
                                                                          #{e0\ 1966}#
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
                                                                                (cons #{e1\ 1979}#
                                                                                      #{e2\ 1980}#))
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
                                                                                  #{c\ 1968}#
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
                                                                                              #{step\ 1971}#)))))))
                                                            #{tmp\ 1978}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1972}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1972}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 1972}#
                                              '())))
                                         #{e1\ 1967}#))
                                      #{tmp\ 1970}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 1969}#)))
                           ($sc-dispatch #{tmp\ 1969}# (quote each-any))))
                        (map (lambda (#{v\ 1987}# #{s\ 1988}#)
                               ((lambda (#{tmp\ 1989}#)
                                  ((lambda (#{tmp\ 1990}#)
                                     (if #{tmp\ 1990}#
                                       (apply (lambda () #{v\ 1987}#)
                                              #{tmp\ 1990}#)
                                       ((lambda (#{tmp\ 1991}#)
                                          (if #{tmp\ 1991}#
                                            (apply (lambda (#{e\ 1992}#)
                                                     #{e\ 1992}#)
                                                   #{tmp\ 1991}#)
                                            ((lambda (#{_\ 1993}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 1959}#
                                                 #{s\ 1988}#))
                                             #{tmp\ 1989}#)))
                                        ($sc-dispatch
                                          #{tmp\ 1989}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 1989}# (quote ()))))
                                #{s\ 1988}#))
                             #{var\ 1963}#
                             #{step\ 1965}#)))
                     #{tmp\ 1961}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1960}#)))
          ($sc-dispatch
            #{tmp\ 1960}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 1959}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 1996}#
               (lambda (#{x\ 2000}# #{y\ 2001}#)
                 ((lambda (#{tmp\ 2002}#)
                    ((lambda (#{tmp\ 2003}#)
                       (if #{tmp\ 2003}#
                         (apply (lambda (#{x\ 2004}# #{y\ 2005}#)
                                  ((lambda (#{tmp\ 2006}#)
                                     ((lambda (#{tmp\ 2007}#)
                                        (if #{tmp\ 2007}#
                                          (apply (lambda (#{dy\ 2008}#)
                                                   ((lambda (#{tmp\ 2009}#)
                                                      ((lambda (#{tmp\ 2010}#)
                                                         (if #{tmp\ 2010}#
                                                           (apply (lambda (#{dx\ 2011}#)
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
                                                                          (cons #{dx\ 2011}#
                                                                                #{dy\ 2008}#)))
                                                                  #{tmp\ 2010}#)
                                                           ((lambda (#{_\ 2012}#)
                                                              (if (null? #{dy\ 2008}#)
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
                                                                      #{x\ 2004}#)
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
                                                                      #{x\ 2004}#
                                                                      #{y\ 2005}#)))
                                                            #{tmp\ 2009}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 2009}#
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
                                                    #{x\ 2004}#))
                                                 #{tmp\ 2007}#)
                                          ((lambda (#{tmp\ 2013}#)
                                             (if #{tmp\ 2013}#
                                               (apply (lambda (#{stuff\ 2014}#)
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
                                                              (cons #{x\ 2004}#
                                                                    #{stuff\ 2014}#)))
                                                      #{tmp\ 2013}#)
                                               ((lambda (#{else\ 2015}#)
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
                                                        #{x\ 2004}#
                                                        #{y\ 2005}#))
                                                #{tmp\ 2006}#)))
                                           ($sc-dispatch
                                             #{tmp\ 2006}#
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
                                        #{tmp\ 2006}#
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
                                   #{y\ 2005}#))
                                #{tmp\ 2003}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 2002}#)))
                     ($sc-dispatch #{tmp\ 2002}# (quote (any any)))))
                  (list #{x\ 2000}# #{y\ 2001}#))))
             (#{quasiappend\ 1997}#
               (lambda (#{x\ 2016}# #{y\ 2017}#)
                 ((lambda (#{tmp\ 2018}#)
                    ((lambda (#{tmp\ 2019}#)
                       (if #{tmp\ 2019}#
                         (apply (lambda (#{x\ 2020}# #{y\ 2021}#)
                                  ((lambda (#{tmp\ 2022}#)
                                     ((lambda (#{tmp\ 2023}#)
                                        (if #{tmp\ 2023}#
                                          (apply (lambda () #{x\ 2020}#)
                                                 #{tmp\ 2023}#)
                                          ((lambda (#{_\ 2024}#)
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
                                                   #{x\ 2020}#
                                                   #{y\ 2021}#))
                                           #{tmp\ 2022}#)))
                                      ($sc-dispatch
                                        #{tmp\ 2022}#
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
                                   #{y\ 2021}#))
                                #{tmp\ 2019}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 2018}#)))
                     ($sc-dispatch #{tmp\ 2018}# (quote (any any)))))
                  (list #{x\ 2016}# #{y\ 2017}#))))
             (#{quasivector\ 1998}#
               (lambda (#{x\ 2025}#)
                 ((lambda (#{tmp\ 2026}#)
                    ((lambda (#{x\ 2027}#)
                       ((lambda (#{tmp\ 2028}#)
                          ((lambda (#{tmp\ 2029}#)
                             (if #{tmp\ 2029}#
                               (apply (lambda (#{x\ 2030}#)
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
                                              (list->vector #{x\ 2030}#)))
                                      #{tmp\ 2029}#)
                               ((lambda (#{tmp\ 2032}#)
                                  (if #{tmp\ 2032}#
                                    (apply (lambda (#{x\ 2033}#)
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
                                                   #{x\ 2033}#))
                                           #{tmp\ 2032}#)
                                    ((lambda (#{_\ 2035}#)
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
                                             #{x\ 2027}#))
                                     #{tmp\ 2028}#)))
                                ($sc-dispatch
                                  #{tmp\ 2028}#
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
                             #{tmp\ 2028}#
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
                        #{x\ 2027}#))
                     #{tmp\ 2026}#))
                  #{x\ 2025}#)))
             (#{quasi\ 1999}#
               (lambda (#{p\ 2036}# #{lev\ 2037}#)
                 ((lambda (#{tmp\ 2038}#)
                    ((lambda (#{tmp\ 2039}#)
                       (if #{tmp\ 2039}#
                         (apply (lambda (#{p\ 2040}#)
                                  (if (= #{lev\ 2037}# 0)
                                    #{p\ 2040}#
                                    (#{quasicons\ 1996}#
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
                                      (#{quasi\ 1999}#
                                        (list #{p\ 2040}#)
                                        (- #{lev\ 2037}# 1)))))
                                #{tmp\ 2039}#)
                         ((lambda (#{tmp\ 2041}#)
                            (if (if #{tmp\ 2041}#
                                  (apply (lambda (#{args\ 2042}#)
                                           (= #{lev\ 2037}# 0))
                                         #{tmp\ 2041}#)
                                  #f)
                              (apply (lambda (#{args\ 2043}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 2036}#
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
                                               #{args\ 2043}#)))
                                     #{tmp\ 2041}#)
                              ((lambda (#{tmp\ 2044}#)
                                 (if #{tmp\ 2044}#
                                   (apply (lambda (#{p\ 2045}# #{q\ 2046}#)
                                            (if (= #{lev\ 2037}# 0)
                                              (#{quasiappend\ 1997}#
                                                #{p\ 2045}#
                                                (#{quasi\ 1999}#
                                                  #{q\ 2046}#
                                                  #{lev\ 2037}#))
                                              (#{quasicons\ 1996}#
                                                (#{quasicons\ 1996}#
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
                                                  (#{quasi\ 1999}#
                                                    (list #{p\ 2045}#)
                                                    (- #{lev\ 2037}# 1)))
                                                (#{quasi\ 1999}#
                                                  #{q\ 2046}#
                                                  #{lev\ 2037}#))))
                                          #{tmp\ 2044}#)
                                   ((lambda (#{tmp\ 2047}#)
                                      (if (if #{tmp\ 2047}#
                                            (apply (lambda (#{args\ 2048}#
                                                            #{q\ 2049}#)
                                                     (= #{lev\ 2037}# 0))
                                                   #{tmp\ 2047}#)
                                            #f)
                                        (apply (lambda (#{args\ 2050}#
                                                        #{q\ 2051}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 2036}#
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
                                                         #{args\ 2050}#)))
                                               #{tmp\ 2047}#)
                                        ((lambda (#{tmp\ 2052}#)
                                           (if #{tmp\ 2052}#
                                             (apply (lambda (#{p\ 2053}#)
                                                      (#{quasicons\ 1996}#
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
                                                        (#{quasi\ 1999}#
                                                          (list #{p\ 2053}#)
                                                          (+ #{lev\ 2037}#
                                                             1))))
                                                    #{tmp\ 2052}#)
                                             ((lambda (#{tmp\ 2054}#)
                                                (if #{tmp\ 2054}#
                                                  (apply (lambda (#{p\ 2055}#
                                                                  #{q\ 2056}#)
                                                           (#{quasicons\ 1996}#
                                                             (#{quasi\ 1999}#
                                                               #{p\ 2055}#
                                                               #{lev\ 2037}#)
                                                             (#{quasi\ 1999}#
                                                               #{q\ 2056}#
                                                               #{lev\ 2037}#)))
                                                         #{tmp\ 2054}#)
                                                  ((lambda (#{tmp\ 2057}#)
                                                     (if #{tmp\ 2057}#
                                                       (apply (lambda (#{x\ 2058}#)
                                                                (#{quasivector\ 1998}#
                                                                  (#{quasi\ 1999}#
                                                                    #{x\ 2058}#
                                                                    #{lev\ 2037}#)))
                                                              #{tmp\ 2057}#)
                                                       ((lambda (#{p\ 2060}#)
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
                                                                #{p\ 2060}#))
                                                        #{tmp\ 2038}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 2038}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 2038}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 2038}#
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
                                      #{tmp\ 2038}#
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
                                 #{tmp\ 2038}#
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
                            #{tmp\ 2038}#
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
                       #{tmp\ 2038}#
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
                  #{p\ 2036}#))))
      (lambda (#{x\ 2061}#)
        ((lambda (#{tmp\ 2062}#)
           ((lambda (#{tmp\ 2063}#)
              (if #{tmp\ 2063}#
                (apply (lambda (#{_\ 2064}# #{e\ 2065}#)
                         (#{quasi\ 1999}# #{e\ 2065}# 0))
                       #{tmp\ 2063}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 2062}#)))
            ($sc-dispatch #{tmp\ 2062}# (quote (any any)))))
         #{x\ 2061}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2066}#)
      (letrec ((#{read-file\ 2067}#
                 (lambda (#{fn\ 2068}# #{k\ 2069}#)
                   (let ((#{p\ 2070}# (open-input-file #{fn\ 2068}#)))
                     (letrec ((#{f\ 2071}#
                                (lambda (#{x\ 2072}#)
                                  (if (eof-object? #{x\ 2072}#)
                                    (begin
                                      (close-input-port #{p\ 2070}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 2069}#
                                            #{x\ 2072}#)
                                          (#{f\ 2071}# (read #{p\ 2070}#)))))))
                       (#{f\ 2071}# (read #{p\ 2070}#)))))))
        ((lambda (#{tmp\ 2073}#)
           ((lambda (#{tmp\ 2074}#)
              (if #{tmp\ 2074}#
                (apply (lambda (#{k\ 2075}# #{filename\ 2076}#)
                         (let ((#{fn\ 2077}#
                                 (syntax->datum #{filename\ 2076}#)))
                           ((lambda (#{tmp\ 2078}#)
                              ((lambda (#{tmp\ 2079}#)
                                 (if #{tmp\ 2079}#
                                   (apply (lambda (#{exp\ 2080}#)
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
                                                  #{exp\ 2080}#))
                                          #{tmp\ 2079}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 2078}#)))
                               ($sc-dispatch #{tmp\ 2078}# (quote each-any))))
                            (#{read-file\ 2067}# #{fn\ 2077}# #{k\ 2075}#))))
                       #{tmp\ 2074}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 2073}#)))
            ($sc-dispatch #{tmp\ 2073}# (quote (any any)))))
         #{x\ 2066}#)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2082}#)
      ((lambda (#{tmp\ 2083}#)
         ((lambda (#{tmp\ 2084}#)
            (if #{tmp\ 2084}#
              (apply (lambda (#{_\ 2085}# #{e\ 2086}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 2082}#))
                     #{tmp\ 2084}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2083}#)))
          ($sc-dispatch #{tmp\ 2083}# (quote (any any)))))
       #{x\ 2082}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2087}#)
      ((lambda (#{tmp\ 2088}#)
         ((lambda (#{tmp\ 2089}#)
            (if #{tmp\ 2089}#
              (apply (lambda (#{_\ 2090}# #{e\ 2091}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 2087}#))
                     #{tmp\ 2089}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2088}#)))
          ($sc-dispatch #{tmp\ 2088}# (quote (any any)))))
       #{x\ 2087}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 2092}#)
      ((lambda (#{tmp\ 2093}#)
         ((lambda (#{tmp\ 2094}#)
            (if #{tmp\ 2094}#
              (apply (lambda (#{_\ 2095}#
                              #{e\ 2096}#
                              #{m1\ 2097}#
                              #{m2\ 2098}#)
                       ((lambda (#{tmp\ 2099}#)
                          ((lambda (#{body\ 2100}#)
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
                                               #{e\ 2096}#))
                                   #{body\ 2100}#))
                           #{tmp\ 2099}#))
                        (letrec ((#{f\ 2101}#
                                   (lambda (#{clause\ 2102}# #{clauses\ 2103}#)
                                     (if (null? #{clauses\ 2103}#)
                                       ((lambda (#{tmp\ 2105}#)
                                          ((lambda (#{tmp\ 2106}#)
                                             (if #{tmp\ 2106}#
                                               (apply (lambda (#{e1\ 2107}#
                                                               #{e2\ 2108}#)
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
                                                              (cons #{e1\ 2107}#
                                                                    #{e2\ 2108}#)))
                                                      #{tmp\ 2106}#)
                                               ((lambda (#{tmp\ 2110}#)
                                                  (if #{tmp\ 2110}#
                                                    (apply (lambda (#{k\ 2111}#
                                                                    #{e1\ 2112}#
                                                                    #{e2\ 2113}#)
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
                                                                               #{k\ 2111}#))
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
                                                                         (cons #{e1\ 2112}#
                                                                               #{e2\ 2113}#))))
                                                           #{tmp\ 2110}#)
                                                    ((lambda (#{_\ 2116}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 2092}#
                                                         #{clause\ 2102}#))
                                                     #{tmp\ 2105}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 2105}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 2105}#
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
                                        #{clause\ 2102}#)
                                       ((lambda (#{tmp\ 2117}#)
                                          ((lambda (#{rest\ 2118}#)
                                             ((lambda (#{tmp\ 2119}#)
                                                ((lambda (#{tmp\ 2120}#)
                                                   (if #{tmp\ 2120}#
                                                     (apply (lambda (#{k\ 2121}#
                                                                     #{e1\ 2122}#
                                                                     #{e2\ 2123}#)
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
                                                                                #{k\ 2121}#))
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
                                                                          (cons #{e1\ 2122}#
                                                                                #{e2\ 2123}#))
                                                                    #{rest\ 2118}#))
                                                            #{tmp\ 2120}#)
                                                     ((lambda (#{_\ 2126}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 2092}#
                                                          #{clause\ 2102}#))
                                                      #{tmp\ 2119}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 2119}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 2102}#))
                                           #{tmp\ 2117}#))
                                        (#{f\ 2101}#
                                          (car #{clauses\ 2103}#)
                                          (cdr #{clauses\ 2103}#)))))))
                          (#{f\ 2101}# #{m1\ 2097}# #{m2\ 2098}#))))
                     #{tmp\ 2094}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2093}#)))
          ($sc-dispatch
            #{tmp\ 2093}#
            '(any any any . each-any))))
       #{x\ 2092}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2127}#)
      ((lambda (#{tmp\ 2128}#)
         ((lambda (#{tmp\ 2129}#)
            (if #{tmp\ 2129}#
              (apply (lambda (#{_\ 2130}# #{e\ 2131}#)
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
                                               #{e\ 2131}#))
                                   (list (cons #{_\ 2130}#
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
                                               (cons #{e\ 2131}#
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
                     #{tmp\ 2129}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2128}#)))
          ($sc-dispatch #{tmp\ 2128}# (quote (any any)))))
       #{x\ 2127}#))))

(define define*
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2132}#)
      ((lambda (#{tmp\ 2133}#)
         ((lambda (#{tmp\ 2134}#)
            (if #{tmp\ 2134}#
              (apply (lambda (#{dummy\ 2135}#
                              #{id\ 2136}#
                              #{args\ 2137}#
                              #{b0\ 2138}#
                              #{b1\ 2139}#)
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
                             #{id\ 2136}#
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
                                   (cons #{args\ 2137}#
                                         (cons #{b0\ 2138}# #{b1\ 2139}#)))))
                     #{tmp\ 2134}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2133}#)))
          ($sc-dispatch
            #{tmp\ 2133}#
            '(any (any . any) any . each-any))))
       #{x\ 2132}#))))

