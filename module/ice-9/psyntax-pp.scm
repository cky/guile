(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((#{and-map*\ 31}#
           (lambda (#{f\ 69}# #{first\ 70}# . #{rest\ 71}#)
             (let ((#{t\ 72}# (null? #{first\ 70}#)))
               (if #{t\ 72}#
                 #{t\ 72}#
                 (if (null? #{rest\ 71}#)
                   (letrec ((#{andmap\ 73}#
                              (lambda (#{first\ 74}#)
                                (let ((#{x\ 75}# (car #{first\ 74}#))
                                      (#{first\ 76}# (cdr #{first\ 74}#)))
                                  (if (null? #{first\ 76}#)
                                    (#{f\ 69}# #{x\ 75}#)
                                    (if (#{f\ 69}# #{x\ 75}#)
                                      (#{andmap\ 73}# #{first\ 76}#)
                                      #f))))))
                     (#{andmap\ 73}# #{first\ 70}#))
                   (letrec ((#{andmap\ 77}#
                              (lambda (#{first\ 78}# #{rest\ 79}#)
                                (let ((#{x\ 80}# (car #{first\ 78}#))
                                      (#{xr\ 81}# (map car #{rest\ 79}#))
                                      (#{first\ 82}# (cdr #{first\ 78}#))
                                      (#{rest\ 83}# (map cdr #{rest\ 79}#)))
                                  (if (null? #{first\ 82}#)
                                    (apply #{f\ 69}#
                                           (cons #{x\ 80}# #{xr\ 81}#))
                                    (if (apply #{f\ 69}#
                                               (cons #{x\ 80}# #{xr\ 81}#))
                                      (#{andmap\ 77}#
                                        #{first\ 82}#
                                        #{rest\ 83}#)
                                      #f))))))
                     (#{andmap\ 77}# #{first\ 70}# #{rest\ 71}#))))))))
  (letrec ((#{lambda-var-list\ 182}#
             (lambda (#{vars\ 306}#)
               (letrec ((#{lvl\ 307}#
                          (lambda (#{vars\ 308}# #{ls\ 309}# #{w\ 310}#)
                            (if (pair? #{vars\ 308}#)
                              (#{lvl\ 307}#
                                (cdr #{vars\ 308}#)
                                (cons (#{wrap\ 159}#
                                        (car #{vars\ 308}#)
                                        #{w\ 310}#
                                        #f)
                                      #{ls\ 309}#)
                                #{w\ 310}#)
                              (if (#{id?\ 131}# #{vars\ 308}#)
                                (cons (#{wrap\ 159}#
                                        #{vars\ 308}#
                                        #{w\ 310}#
                                        #f)
                                      #{ls\ 309}#)
                                (if (null? #{vars\ 308}#)
                                  #{ls\ 309}#
                                  (if (#{syntax-object?\ 115}# #{vars\ 308}#)
                                    (#{lvl\ 307}#
                                      (#{syntax-object-expression\ 116}#
                                        #{vars\ 308}#)
                                      #{ls\ 309}#
                                      (#{join-wraps\ 150}#
                                        #{w\ 310}#
                                        (#{syntax-object-wrap\ 117}#
                                          #{vars\ 308}#)))
                                    (cons #{vars\ 308}# #{ls\ 309}#))))))))
                 (#{lvl\ 307}#
                   #{vars\ 306}#
                   '()
                   '(())))))
           (#{gen-var\ 181}#
             (lambda (#{id\ 311}#)
               (let ((#{id\ 312}#
                       (if (#{syntax-object?\ 115}# #{id\ 311}#)
                         (#{syntax-object-expression\ 116}# #{id\ 311}#)
                         #{id\ 311}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 312}#) " ")))))
           (#{strip\ 180}#
             (lambda (#{x\ 313}# #{w\ 314}#)
               (if (memq 'top
                         (#{wrap-marks\ 134}# #{w\ 314}#))
                 #{x\ 313}#
                 (letrec ((#{f\ 315}# (lambda (#{x\ 316}#)
                                        (if (#{syntax-object?\ 115}#
                                              #{x\ 316}#)
                                          (#{strip\ 180}#
                                            (#{syntax-object-expression\ 116}#
                                              #{x\ 316}#)
                                            (#{syntax-object-wrap\ 117}#
                                              #{x\ 316}#))
                                          (if (pair? #{x\ 316}#)
                                            (let ((#{a\ 317}# (#{f\ 315}# (car #{x\ 316}#)))
                                                  (#{d\ 318}# (#{f\ 315}# (cdr #{x\ 316}#))))
                                              (if (if (eq? #{a\ 317}#
                                                           (car #{x\ 316}#))
                                                    (eq? #{d\ 318}#
                                                         (cdr #{x\ 316}#))
                                                    #f)
                                                #{x\ 316}#
                                                (cons #{a\ 317}# #{d\ 318}#)))
                                            (if (vector? #{x\ 316}#)
                                              (let ((#{old\ 319}#
                                                      (vector->list
                                                        #{x\ 316}#)))
                                                (let ((#{new\ 320}#
                                                        (map #{f\ 315}#
                                                             #{old\ 319}#)))
                                                  (if (#{and-map*\ 31}#
                                                        eq?
                                                        #{old\ 319}#
                                                        #{new\ 320}#)
                                                    #{x\ 316}#
                                                    (list->vector
                                                      #{new\ 320}#))))
                                              #{x\ 316}#))))))
                   (#{f\ 315}# #{x\ 313}#)))))
           (#{chi-lambda-case\ 179}#
             (lambda (#{e\ 321}#
                      #{r\ 322}#
                      #{w\ 323}#
                      #{s\ 324}#
                      #{mod\ 325}#
                      #{get-formals\ 326}#
                      #{clauses\ 327}#)
               (letrec ((#{expand-body\ 332}#
                          (lambda (#{req\ 333}#
                                   #{opt\ 334}#
                                   #{rest\ 335}#
                                   #{kw\ 336}#
                                   #{pred\ 337}#
                                   #{body\ 338}#
                                   #{vars\ 339}#
                                   #{r*\ 340}#
                                   #{w*\ 341}#
                                   #{inits\ 342}#)
                            ((lambda (#{tmp\ 343}#)
                               ((lambda (#{tmp\ 344}#)
                                  (if (if #{tmp\ 344}#
                                        (apply (lambda (#{docstring\ 345}#
                                                        #{e1\ 346}#
                                                        #{e2\ 347}#)
                                                 (string?
                                                   (syntax->datum
                                                     #{docstring\ 345}#)))
                                               #{tmp\ 344}#)
                                        #f)
                                    (apply (lambda (#{docstring\ 348}#
                                                    #{e1\ 349}#
                                                    #{e2\ 350}#)
                                             (values
                                               (syntax->datum
                                                 #{docstring\ 348}#)
                                               #{req\ 333}#
                                               #{opt\ 334}#
                                               #{rest\ 335}#
                                               #{kw\ 336}#
                                               #{inits\ 342}#
                                               #{vars\ 339}#
                                               #{pred\ 337}#
                                               (#{chi-body\ 171}#
                                                 (cons #{e1\ 349}# #{e2\ 350}#)
                                                 (#{source-wrap\ 160}#
                                                   #{e\ 321}#
                                                   #{w\ 323}#
                                                   #{s\ 324}#
                                                   #{mod\ 325}#)
                                                 #{r*\ 340}#
                                                 #{w*\ 341}#
                                                 #{mod\ 325}#)))
                                           #{tmp\ 344}#)
                                    ((lambda (#{tmp\ 352}#)
                                       (if #{tmp\ 352}#
                                         (apply (lambda (#{e1\ 353}#
                                                         #{e2\ 354}#)
                                                  (values
                                                    #f
                                                    #{req\ 333}#
                                                    #{opt\ 334}#
                                                    #{rest\ 335}#
                                                    #{kw\ 336}#
                                                    #{inits\ 342}#
                                                    #{vars\ 339}#
                                                    #{pred\ 337}#
                                                    (#{chi-body\ 171}#
                                                      (cons #{e1\ 353}#
                                                            #{e2\ 354}#)
                                                      (#{source-wrap\ 160}#
                                                        #{e\ 321}#
                                                        #{w\ 323}#
                                                        #{s\ 324}#
                                                        #{mod\ 325}#)
                                                      #{r*\ 340}#
                                                      #{w*\ 341}#
                                                      #{mod\ 325}#)))
                                                #{tmp\ 352}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 343}#)))
                                     ($sc-dispatch
                                       #{tmp\ 343}#
                                       '(any . each-any)))))
                                ($sc-dispatch
                                  #{tmp\ 343}#
                                  '(any any . each-any))))
                             #{body\ 338}#)))
                        (#{expand-pred\ 331}#
                          (lambda (#{req\ 356}#
                                   #{opt\ 357}#
                                   #{rest\ 358}#
                                   #{kw\ 359}#
                                   #{pred\ 360}#
                                   #{body\ 361}#
                                   #{vars\ 362}#
                                   #{r*\ 363}#
                                   #{w*\ 364}#
                                   #{inits\ 365}#)
                            (#{expand-body\ 332}#
                              #{req\ 356}#
                              #{opt\ 357}#
                              #{rest\ 358}#
                              #{kw\ 359}#
                              (if #{pred\ 360}#
                                (#{chi\ 167}#
                                  #{pred\ 360}#
                                  #{r*\ 363}#
                                  #{w*\ 364}#
                                  #{mod\ 325}#)
                                #f)
                              #{body\ 361}#
                              #{vars\ 362}#
                              #{r*\ 363}#
                              #{w*\ 364}#
                              #{inits\ 365}#)))
                        (#{expand-kw\ 330}#
                          (lambda (#{req\ 366}#
                                   #{opt\ 367}#
                                   #{rest\ 368}#
                                   #{kw\ 369}#
                                   #{pred\ 370}#
                                   #{body\ 371}#
                                   #{vars\ 372}#
                                   #{r*\ 373}#
                                   #{w*\ 374}#
                                   #{aok\ 375}#
                                   #{out\ 376}#
                                   #{inits\ 377}#)
                            (if (pair? #{kw\ 369}#)
                              ((lambda (#{tmp\ 378}#)
                                 ((lambda (#{tmp\ 379}#)
                                    (if #{tmp\ 379}#
                                      (apply (lambda (#{k\ 380}#
                                                      #{id\ 381}#
                                                      #{i\ 382}#)
                                               (let ((#{v\ 383}# (#{gen-var\ 181}#
                                                                   #{id\ 381}#)))
                                                 (let ((#{l\ 384}# (#{gen-labels\ 137}#
                                                                     (list #{v\ 383}#))))
                                                   (let ((#{r**\ 385}#
                                                           (#{extend-var-env\ 126}#
                                                             #{l\ 384}#
                                                             (list #{v\ 383}#)
                                                             #{r*\ 373}#)))
                                                     (let ((#{w**\ 386}#
                                                             (#{make-binding-wrap\ 148}#
                                                               (list #{id\ 381}#)
                                                               #{l\ 384}#
                                                               #{w*\ 374}#)))
                                                       (#{expand-kw\ 330}#
                                                         #{req\ 366}#
                                                         #{opt\ 367}#
                                                         #{rest\ 368}#
                                                         (cdr #{kw\ 369}#)
                                                         #{pred\ 370}#
                                                         #{body\ 371}#
                                                         (cons #{v\ 383}#
                                                               #{vars\ 372}#)
                                                         #{r**\ 385}#
                                                         #{w**\ 386}#
                                                         #{aok\ 375}#
                                                         (cons (list (syntax->datum
                                                                       #{k\ 380}#)
                                                                     (syntax->datum
                                                                       #{id\ 381}#)
                                                                     #{v\ 383}#)
                                                               #{out\ 376}#)
                                                         (cons (#{chi\ 167}#
                                                                 #{i\ 382}#
                                                                 #{r*\ 373}#
                                                                 #{w*\ 374}#
                                                                 #{mod\ 325}#)
                                                               #{inits\ 377}#)))))))
                                             #{tmp\ 379}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 378}#)))
                                  ($sc-dispatch
                                    #{tmp\ 378}#
                                    '(any any any))))
                               (car #{kw\ 369}#))
                              (#{expand-pred\ 331}#
                                #{req\ 366}#
                                #{opt\ 367}#
                                #{rest\ 368}#
                                (if (let ((#{t\ 387}# #{aok\ 375}#))
                                      (if #{t\ 387}#
                                        #{t\ 387}#
                                        (pair? #{out\ 376}#)))
                                  (cons #{aok\ 375}# (reverse #{out\ 376}#))
                                  #f)
                                #{pred\ 370}#
                                #{body\ 371}#
                                (reverse #{vars\ 372}#)
                                #{r*\ 373}#
                                #{w*\ 374}#
                                (reverse #{inits\ 377}#)))))
                        (#{expand-opt\ 329}#
                          (lambda (#{req\ 388}#
                                   #{opt\ 389}#
                                   #{rest\ 390}#
                                   #{kw\ 391}#
                                   #{pred\ 392}#
                                   #{body\ 393}#
                                   #{vars\ 394}#
                                   #{r*\ 395}#
                                   #{w*\ 396}#
                                   #{out\ 397}#
                                   #{inits\ 398}#)
                            (if (pair? #{opt\ 389}#)
                              ((lambda (#{tmp\ 399}#)
                                 ((lambda (#{tmp\ 400}#)
                                    (if #{tmp\ 400}#
                                      (apply (lambda (#{id\ 401}# #{i\ 402}#)
                                               (let ((#{v\ 403}# (#{gen-var\ 181}#
                                                                   #{id\ 401}#)))
                                                 (let ((#{l\ 404}# (#{gen-labels\ 137}#
                                                                     (list #{v\ 403}#))))
                                                   (let ((#{r**\ 405}#
                                                           (#{extend-var-env\ 126}#
                                                             #{l\ 404}#
                                                             (list #{v\ 403}#)
                                                             #{r*\ 395}#)))
                                                     (let ((#{w**\ 406}#
                                                             (#{make-binding-wrap\ 148}#
                                                               (list #{id\ 401}#)
                                                               #{l\ 404}#
                                                               #{w*\ 396}#)))
                                                       (#{expand-opt\ 329}#
                                                         #{req\ 388}#
                                                         (cdr #{opt\ 389}#)
                                                         #{rest\ 390}#
                                                         #{kw\ 391}#
                                                         #{pred\ 392}#
                                                         #{body\ 393}#
                                                         (cons #{v\ 403}#
                                                               #{vars\ 394}#)
                                                         #{r**\ 405}#
                                                         #{w**\ 406}#
                                                         (cons (syntax->datum
                                                                 #{id\ 401}#)
                                                               #{out\ 397}#)
                                                         (cons (#{chi\ 167}#
                                                                 #{i\ 402}#
                                                                 #{r*\ 395}#
                                                                 #{w*\ 396}#
                                                                 #{mod\ 325}#)
                                                               #{inits\ 398}#)))))))
                                             #{tmp\ 400}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 399}#)))
                                  ($sc-dispatch
                                    #{tmp\ 399}#
                                    '(any any))))
                               (car #{opt\ 389}#))
                              (if #{rest\ 390}#
                                (let ((#{v\ 407}# (#{gen-var\ 181}#
                                                    #{rest\ 390}#)))
                                  (let ((#{l\ 408}# (#{gen-labels\ 137}#
                                                      (list #{v\ 407}#))))
                                    (let ((#{r*\ 409}#
                                            (#{extend-var-env\ 126}#
                                              #{l\ 408}#
                                              (list #{v\ 407}#)
                                              #{r*\ 395}#)))
                                      (let ((#{w*\ 410}#
                                              (#{make-binding-wrap\ 148}#
                                                (list #{rest\ 390}#)
                                                #{l\ 408}#
                                                #{w*\ 396}#)))
                                        (#{expand-kw\ 330}#
                                          #{req\ 388}#
                                          (if (pair? #{out\ 397}#)
                                            (reverse #{out\ 397}#)
                                            #f)
                                          (syntax->datum #{rest\ 390}#)
                                          (if (pair? #{kw\ 391}#)
                                            (cdr #{kw\ 391}#)
                                            #{kw\ 391}#)
                                          #{pred\ 392}#
                                          #{body\ 393}#
                                          (cons #{v\ 407}# #{vars\ 394}#)
                                          #{r*\ 409}#
                                          #{w*\ 410}#
                                          (if (pair? #{kw\ 391}#)
                                            (car #{kw\ 391}#)
                                            #f)
                                          '()
                                          #{inits\ 398}#)))))
                                (#{expand-kw\ 330}#
                                  #{req\ 388}#
                                  (if (pair? #{out\ 397}#)
                                    (reverse #{out\ 397}#)
                                    #f)
                                  #f
                                  (if (pair? #{kw\ 391}#)
                                    (cdr #{kw\ 391}#)
                                    #{kw\ 391}#)
                                  #{pred\ 392}#
                                  #{body\ 393}#
                                  #{vars\ 394}#
                                  #{r*\ 395}#
                                  #{w*\ 396}#
                                  (if (pair? #{kw\ 391}#) (car #{kw\ 391}#) #f)
                                  '()
                                  #{inits\ 398}#)))))
                        (#{expand-req\ 328}#
                          (lambda (#{req\ 411}#
                                   #{opt\ 412}#
                                   #{rest\ 413}#
                                   #{kw\ 414}#
                                   #{pred\ 415}#
                                   #{body\ 416}#)
                            (let ((#{vars\ 417}#
                                    (map #{gen-var\ 181}# #{req\ 411}#))
                                  (#{labels\ 418}#
                                    (#{gen-labels\ 137}# #{req\ 411}#)))
                              (let ((#{r*\ 419}#
                                      (#{extend-var-env\ 126}#
                                        #{labels\ 418}#
                                        #{vars\ 417}#
                                        #{r\ 322}#))
                                    (#{w*\ 420}#
                                      (#{make-binding-wrap\ 148}#
                                        #{req\ 411}#
                                        #{labels\ 418}#
                                        #{w\ 323}#)))
                                (#{expand-opt\ 329}#
                                  (map syntax->datum #{req\ 411}#)
                                  #{opt\ 412}#
                                  #{rest\ 413}#
                                  #{kw\ 414}#
                                  #{pred\ 415}#
                                  #{body\ 416}#
                                  (reverse #{vars\ 417}#)
                                  #{r*\ 419}#
                                  #{w*\ 420}#
                                  '()
                                  '()))))))
                 ((lambda (#{tmp\ 421}#)
                    ((lambda (#{tmp\ 422}#)
                       (if #{tmp\ 422}#
                         (apply (lambda () (values #f #f)) #{tmp\ 422}#)
                         ((lambda (#{tmp\ 423}#)
                            (if #{tmp\ 423}#
                              (apply (lambda (#{args\ 424}#
                                              #{e1\ 425}#
                                              #{e2\ 426}#
                                              #{args*\ 427}#
                                              #{e1*\ 428}#
                                              #{e2*\ 429}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{get-formals\ 326}#
                                             #{args\ 424}#))
                                         (lambda (#{req\ 430}#
                                                  #{opt\ 431}#
                                                  #{rest\ 432}#
                                                  #{kw\ 433}#
                                                  #{pred\ 434}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{expand-req\ 328}#
                                                 #{req\ 430}#
                                                 #{opt\ 431}#
                                                 #{rest\ 432}#
                                                 #{kw\ 433}#
                                                 #{pred\ 434}#
                                                 (cons #{e1\ 425}#
                                                       #{e2\ 426}#)))
                                             (lambda (#{docstring\ 436}#
                                                      #{req\ 437}#
                                                      #{opt\ 438}#
                                                      #{rest\ 439}#
                                                      #{kw\ 440}#
                                                      #{inits\ 441}#
                                                      #{vars\ 442}#
                                                      #{pred\ 443}#
                                                      #{body\ 444}#)
                                               (call-with-values
                                                 (lambda ()
                                                   (#{chi-lambda-case\ 179}#
                                                     #{e\ 321}#
                                                     #{r\ 322}#
                                                     #{w\ 323}#
                                                     #{s\ 324}#
                                                     #{mod\ 325}#
                                                     #{get-formals\ 326}#
                                                     (map (lambda (#{tmp\ 447}#
                                                                   #{tmp\ 446}#
                                                                   #{tmp\ 445}#)
                                                            (cons #{tmp\ 445}#
                                                                  (cons #{tmp\ 446}#
                                                                        #{tmp\ 447}#)))
                                                          #{e2*\ 429}#
                                                          #{e1*\ 428}#
                                                          #{args*\ 427}#)))
                                                 (lambda (#{docstring*\ 449}#
                                                          #{else*\ 450}#)
                                                   (values
                                                     (let ((#{t\ 451}# #{docstring\ 436}#))
                                                       (if #{t\ 451}#
                                                         #{t\ 451}#
                                                         #{docstring*\ 449}#))
                                                     (#{build-lambda-case\ 107}#
                                                       #{s\ 324}#
                                                       #{req\ 437}#
                                                       #{opt\ 438}#
                                                       #{rest\ 439}#
                                                       #{kw\ 440}#
                                                       #{inits\ 441}#
                                                       #{vars\ 442}#
                                                       #{pred\ 443}#
                                                       #{body\ 444}#
                                                       #{else*\ 450}#)))))))))
                                     #{tmp\ 423}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 421}#)))
                          ($sc-dispatch
                            #{tmp\ 421}#
                            '((any any . each-any)
                              .
                              #(each (any any . each-any)))))))
                     ($sc-dispatch #{tmp\ 421}# (quote ()))))
                  #{clauses\ 327}#))))
           (#{lambda*-formals\ 178}#
             (lambda (#{orig-args\ 452}#)
               (letrec ((#{check\ 458}#
                          (lambda (#{req\ 459}#
                                   #{opt\ 460}#
                                   #{rest\ 461}#
                                   #{kw\ 462}#
                                   #{pred\ 463}#)
                            (if (#{distinct-bound-ids?\ 157}#
                                  (append
                                    #{req\ 459}#
                                    (map car #{opt\ 460}#)
                                    (if #{rest\ 461}#
                                      (list #{rest\ 461}#)
                                      '())
                                    (if (pair? #{kw\ 462}#)
                                      (map cadr (cdr #{kw\ 462}#))
                                      '())))
                              (values
                                #{req\ 459}#
                                #{opt\ 460}#
                                #{rest\ 461}#
                                #{kw\ 462}#
                                #{pred\ 463}#)
                              (syntax-violation
                                'lambda*
                                "duplicate identifier in argument list"
                                #{orig-args\ 452}#))))
                        (#{rest\ 457}#
                          (lambda (#{args\ 464}#
                                   #{req\ 465}#
                                   #{opt\ 466}#
                                   #{kw\ 467}#
                                   #{pred\ 468}#)
                            ((lambda (#{tmp\ 469}#)
                               ((lambda (#{tmp\ 470}#)
                                  (if (if #{tmp\ 470}#
                                        (apply (lambda (#{r\ 471}#)
                                                 (#{id?\ 131}# #{r\ 471}#))
                                               #{tmp\ 470}#)
                                        #f)
                                    (apply (lambda (#{r\ 472}#)
                                             (#{check\ 458}#
                                               #{req\ 465}#
                                               #{opt\ 466}#
                                               #{r\ 472}#
                                               #{kw\ 467}#
                                               #{pred\ 468}#))
                                           #{tmp\ 470}#)
                                    ((lambda (#{else\ 473}#)
                                       (syntax-violation
                                         'lambda*
                                         "invalid rest argument"
                                         #{orig-args\ 452}#
                                         #{args\ 464}#))
                                     #{tmp\ 469}#)))
                                (list #{tmp\ 469}#)))
                             #{args\ 464}#)))
                        (#{pred\ 456}#
                          (lambda (#{args\ 474}#
                                   #{req\ 475}#
                                   #{opt\ 476}#
                                   #{kw\ 477}#)
                            ((lambda (#{tmp\ 478}#)
                               ((lambda (#{tmp\ 479}#)
                                  (if #{tmp\ 479}#
                                    (apply (lambda (#{x\ 480}#)
                                             (#{check\ 458}#
                                               #{req\ 475}#
                                               #{opt\ 476}#
                                               #f
                                               #{kw\ 477}#
                                               #{x\ 480}#))
                                           #{tmp\ 479}#)
                                    ((lambda (#{tmp\ 481}#)
                                       (if (if #{tmp\ 481}#
                                             (apply (lambda (#{x\ 482}#
                                                             #{a\ 483}#
                                                             #{b\ 484}#)
                                                      (eq? (syntax->datum
                                                             #{a\ 483}#)
                                                           #:rest))
                                                    #{tmp\ 481}#)
                                             #f)
                                         (apply (lambda (#{x\ 485}#
                                                         #{a\ 486}#
                                                         #{b\ 487}#)
                                                  (#{rest\ 457}#
                                                    #{b\ 487}#
                                                    #{req\ 475}#
                                                    #{opt\ 476}#
                                                    #{kw\ 477}#
                                                    #f))
                                                #{tmp\ 481}#)
                                         ((lambda (#{tmp\ 488}#)
                                            (if (if #{tmp\ 488}#
                                                  (apply (lambda (#{x\ 489}#
                                                                  #{b\ 490}#)
                                                           (#{id?\ 131}#
                                                             #{b\ 490}#))
                                                         #{tmp\ 488}#)
                                                  #f)
                                              (apply (lambda (#{x\ 491}#
                                                              #{b\ 492}#)
                                                       (#{rest\ 457}#
                                                         #{b\ 492}#
                                                         #{req\ 475}#
                                                         #{opt\ 476}#
                                                         #{kw\ 477}#
                                                         #f))
                                                     #{tmp\ 488}#)
                                              ((lambda (#{else\ 493}#)
                                                 (syntax-violation
                                                   'lambda*
                                                   "invalid argument list following #:predicate"
                                                   #{orig-args\ 452}#
                                                   #{args\ 474}#))
                                               #{tmp\ 478}#)))
                                          ($sc-dispatch
                                            #{tmp\ 478}#
                                            '(any . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 478}#
                                       '(any any any)))))
                                ($sc-dispatch #{tmp\ 478}# (quote (any)))))
                             #{args\ 474}#)))
                        (#{key\ 455}#
                          (lambda (#{args\ 494}#
                                   #{req\ 495}#
                                   #{opt\ 496}#
                                   #{rkey\ 497}#)
                            ((lambda (#{tmp\ 498}#)
                               ((lambda (#{tmp\ 499}#)
                                  (if #{tmp\ 499}#
                                    (apply (lambda ()
                                             (#{check\ 458}#
                                               #{req\ 495}#
                                               #{opt\ 496}#
                                               #f
                                               (cons #f
                                                     (reverse #{rkey\ 497}#))
                                               #f))
                                           #{tmp\ 499}#)
                                    ((lambda (#{tmp\ 500}#)
                                       (if (if #{tmp\ 500}#
                                             (apply (lambda (#{a\ 501}#
                                                             #{b\ 502}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 501}#))
                                                    #{tmp\ 500}#)
                                             #f)
                                         (apply (lambda (#{a\ 503}# #{b\ 504}#)
                                                  ((lambda (#{tmp\ 505}#)
                                                     ((lambda (#{k\ 506}#)
                                                        (#{key\ 455}#
                                                          #{b\ 504}#
                                                          #{req\ 495}#
                                                          #{opt\ 496}#
                                                          (cons (cons #{k\ 506}#
                                                                      (cons #{a\ 503}#
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
                                                                                          pred
                                                                                          key
                                                                                          opt
                                                                                          req)
                                                                                   ((top)
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
                                                                #{rkey\ 497}#)))
                                                      #{tmp\ 505}#))
                                                   (symbol->keyword
                                                     (syntax->datum
                                                       #{a\ 503}#))))
                                                #{tmp\ 500}#)
                                         ((lambda (#{tmp\ 507}#)
                                            (if (if #{tmp\ 507}#
                                                  (apply (lambda (#{a\ 508}#
                                                                  #{init\ 509}#
                                                                  #{b\ 510}#)
                                                           (#{id?\ 131}#
                                                             #{a\ 508}#))
                                                         #{tmp\ 507}#)
                                                  #f)
                                              (apply (lambda (#{a\ 511}#
                                                              #{init\ 512}#
                                                              #{b\ 513}#)
                                                       ((lambda (#{tmp\ 514}#)
                                                          ((lambda (#{k\ 515}#)
                                                             (#{key\ 455}#
                                                               #{b\ 513}#
                                                               #{req\ 495}#
                                                               #{opt\ 496}#
                                                               (cons (list #{k\ 515}#
                                                                           #{a\ 511}#
                                                                           #{init\ 512}#)
                                                                     #{rkey\ 497}#)))
                                                           #{tmp\ 514}#))
                                                        (symbol->keyword
                                                          (syntax->datum
                                                            #{a\ 511}#))))
                                                     #{tmp\ 507}#)
                                              ((lambda (#{tmp\ 516}#)
                                                 (if (if #{tmp\ 516}#
                                                       (apply (lambda (#{a\ 517}#
                                                                       #{init\ 518}#
                                                                       #{k\ 519}#
                                                                       #{b\ 520}#)
                                                                (if (#{id?\ 131}#
                                                                      #{a\ 517}#)
                                                                  (keyword?
                                                                    (syntax->datum
                                                                      #{k\ 519}#))
                                                                  #f))
                                                              #{tmp\ 516}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 521}#
                                                                   #{init\ 522}#
                                                                   #{k\ 523}#
                                                                   #{b\ 524}#)
                                                            (#{key\ 455}#
                                                              #{b\ 524}#
                                                              #{req\ 495}#
                                                              #{opt\ 496}#
                                                              (cons (list #{k\ 523}#
                                                                          #{a\ 521}#
                                                                          #{init\ 522}#)
                                                                    #{rkey\ 497}#)))
                                                          #{tmp\ 516}#)
                                                   ((lambda (#{tmp\ 525}#)
                                                      (if (if #{tmp\ 525}#
                                                            (apply (lambda (#{aok\ 526}#)
                                                                     (eq? (syntax->datum
                                                                            #{aok\ 526}#)
                                                                          #:allow-other-keys))
                                                                   #{tmp\ 525}#)
                                                            #f)
                                                        (apply (lambda (#{aok\ 527}#)
                                                                 (#{check\ 458}#
                                                                   #{req\ 495}#
                                                                   #{opt\ 496}#
                                                                   #f
                                                                   (cons #t
                                                                         (reverse
                                                                           #{rkey\ 497}#))
                                                                   #f))
                                                               #{tmp\ 525}#)
                                                        ((lambda (#{tmp\ 528}#)
                                                           (if (if #{tmp\ 528}#
                                                                 (apply (lambda (#{aok\ 529}#
                                                                                 #{a\ 530}#
                                                                                 #{b\ 531}#)
                                                                          (if (eq? (syntax->datum
                                                                                     #{aok\ 529}#)
                                                                                   #:allow-other-keys)
                                                                            (eq? (syntax->datum
                                                                                   #{a\ 530}#)
                                                                                 #:predicate)
                                                                            #f))
                                                                        #{tmp\ 528}#)
                                                                 #f)
                                                             (apply (lambda (#{aok\ 532}#
                                                                             #{a\ 533}#
                                                                             #{b\ 534}#)
                                                                      (#{pred\ 456}#
                                                                        #{b\ 534}#
                                                                        #{req\ 495}#
                                                                        #{opt\ 496}#
                                                                        (cons #t
                                                                              (reverse
                                                                                #{rkey\ 497}#))))
                                                                    #{tmp\ 528}#)
                                                             ((lambda (#{tmp\ 535}#)
                                                                (if (if #{tmp\ 535}#
                                                                      (apply (lambda (#{aok\ 536}#
                                                                                      #{a\ 537}#
                                                                                      #{b\ 538}#)
                                                                               (if (eq? (syntax->datum
                                                                                          #{aok\ 536}#)
                                                                                        #:allow-other-keys)
                                                                                 (eq? (syntax->datum
                                                                                        #{a\ 537}#)
                                                                                      #:rest)
                                                                                 #f))
                                                                             #{tmp\ 535}#)
                                                                      #f)
                                                                  (apply (lambda (#{aok\ 539}#
                                                                                  #{a\ 540}#
                                                                                  #{b\ 541}#)
                                                                           (#{rest\ 457}#
                                                                             #{b\ 541}#
                                                                             #{req\ 495}#
                                                                             #{opt\ 496}#
                                                                             (cons #t
                                                                                   (reverse
                                                                                     #{rkey\ 497}#))
                                                                             #f))
                                                                         #{tmp\ 535}#)
                                                                  ((lambda (#{tmp\ 542}#)
                                                                     (if (if #{tmp\ 542}#
                                                                           (apply (lambda (#{aok\ 543}#
                                                                                           #{r\ 544}#)
                                                                                    (if (eq? (syntax->datum
                                                                                               #{aok\ 543}#)
                                                                                             #:allow-other-keys)
                                                                                      (#{id?\ 131}#
                                                                                        #{r\ 544}#)
                                                                                      #f))
                                                                                  #{tmp\ 542}#)
                                                                           #f)
                                                                       (apply (lambda (#{aok\ 545}#
                                                                                       #{r\ 546}#)
                                                                                (#{rest\ 457}#
                                                                                  #{r\ 546}#
                                                                                  #{req\ 495}#
                                                                                  #{opt\ 496}#
                                                                                  (cons #t
                                                                                        (reverse
                                                                                          #{rkey\ 497}#))
                                                                                  #f))
                                                                              #{tmp\ 542}#)
                                                                       ((lambda (#{tmp\ 547}#)
                                                                          (if (if #{tmp\ 547}#
                                                                                (apply (lambda (#{a\ 548}#
                                                                                                #{b\ 549}#)
                                                                                         (eq? (syntax->datum
                                                                                                #{a\ 548}#)
                                                                                              #:predicate))
                                                                                       #{tmp\ 547}#)
                                                                                #f)
                                                                            (apply (lambda (#{a\ 550}#
                                                                                            #{b\ 551}#)
                                                                                     (#{pred\ 456}#
                                                                                       #{b\ 551}#
                                                                                       #{req\ 495}#
                                                                                       #{opt\ 496}#
                                                                                       (cons #f
                                                                                             (reverse
                                                                                               #{rkey\ 497}#))))
                                                                                   #{tmp\ 547}#)
                                                                            ((lambda (#{tmp\ 552}#)
                                                                               (if (if #{tmp\ 552}#
                                                                                     (apply (lambda (#{a\ 553}#
                                                                                                     #{b\ 554}#)
                                                                                              (eq? (syntax->datum
                                                                                                     #{a\ 553}#)
                                                                                                   #:rest))
                                                                                            #{tmp\ 552}#)
                                                                                     #f)
                                                                                 (apply (lambda (#{a\ 555}#
                                                                                                 #{b\ 556}#)
                                                                                          (#{rest\ 457}#
                                                                                            #{b\ 556}#
                                                                                            #{req\ 495}#
                                                                                            #{opt\ 496}#
                                                                                            (cons #f
                                                                                                  (reverse
                                                                                                    #{rkey\ 497}#))
                                                                                            #f))
                                                                                        #{tmp\ 552}#)
                                                                                 ((lambda (#{tmp\ 557}#)
                                                                                    (if (if #{tmp\ 557}#
                                                                                          (apply (lambda (#{r\ 558}#)
                                                                                                   (#{id?\ 131}#
                                                                                                     #{r\ 558}#))
                                                                                                 #{tmp\ 557}#)
                                                                                          #f)
                                                                                      (apply (lambda (#{r\ 559}#)
                                                                                               (#{rest\ 457}#
                                                                                                 #{r\ 559}#
                                                                                                 #{req\ 495}#
                                                                                                 #{opt\ 496}#
                                                                                                 (cons #f
                                                                                                       (reverse
                                                                                                         #{rkey\ 497}#))
                                                                                                 #f))
                                                                                             #{tmp\ 557}#)
                                                                                      ((lambda (#{else\ 560}#)
                                                                                         (syntax-violation
                                                                                           'lambda*
                                                                                           "invalid keyword argument list"
                                                                                           #{orig-args\ 452}#
                                                                                           #{args\ 494}#))
                                                                                       #{tmp\ 498}#)))
                                                                                  (list #{tmp\ 498}#))))
                                                                             ($sc-dispatch
                                                                               #{tmp\ 498}#
                                                                               '(any any)))))
                                                                        ($sc-dispatch
                                                                          #{tmp\ 498}#
                                                                          '(any .
                                                                                any)))))
                                                                   ($sc-dispatch
                                                                     #{tmp\ 498}#
                                                                     '(any .
                                                                           any)))))
                                                              ($sc-dispatch
                                                                #{tmp\ 498}#
                                                                '(any any
                                                                      any)))))
                                                         ($sc-dispatch
                                                           #{tmp\ 498}#
                                                           '(any any . any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 498}#
                                                      '(any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 498}#
                                                 '((any any any) . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 498}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 498}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 498}# (quote ()))))
                             #{args\ 494}#)))
                        (#{opt\ 454}#
                          (lambda (#{args\ 561}# #{req\ 562}# #{ropt\ 563}#)
                            ((lambda (#{tmp\ 564}#)
                               ((lambda (#{tmp\ 565}#)
                                  (if #{tmp\ 565}#
                                    (apply (lambda ()
                                             (#{check\ 458}#
                                               #{req\ 562}#
                                               (reverse #{ropt\ 563}#)
                                               #f
                                               '()
                                               #f))
                                           #{tmp\ 565}#)
                                    ((lambda (#{tmp\ 566}#)
                                       (if (if #{tmp\ 566}#
                                             (apply (lambda (#{a\ 567}#
                                                             #{b\ 568}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 567}#))
                                                    #{tmp\ 566}#)
                                             #f)
                                         (apply (lambda (#{a\ 569}# #{b\ 570}#)
                                                  (#{opt\ 454}#
                                                    #{b\ 570}#
                                                    #{req\ 562}#
                                                    (cons (cons #{a\ 569}#
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
                                                                              pred
                                                                              key
                                                                              opt
                                                                              req)
                                                                       ((top)
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
                                                          #{ropt\ 563}#)))
                                                #{tmp\ 566}#)
                                         ((lambda (#{tmp\ 571}#)
                                            (if (if #{tmp\ 571}#
                                                  (apply (lambda (#{a\ 572}#
                                                                  #{init\ 573}#
                                                                  #{b\ 574}#)
                                                           (#{id?\ 131}#
                                                             #{a\ 572}#))
                                                         #{tmp\ 571}#)
                                                  #f)
                                              (apply (lambda (#{a\ 575}#
                                                              #{init\ 576}#
                                                              #{b\ 577}#)
                                                       (#{opt\ 454}#
                                                         #{b\ 577}#
                                                         #{req\ 562}#
                                                         (cons (list #{a\ 575}#
                                                                     #{init\ 576}#)
                                                               #{ropt\ 563}#)))
                                                     #{tmp\ 571}#)
                                              ((lambda (#{tmp\ 578}#)
                                                 (if (if #{tmp\ 578}#
                                                       (apply (lambda (#{a\ 579}#
                                                                       #{b\ 580}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 579}#)
                                                                     #:key))
                                                              #{tmp\ 578}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 581}#
                                                                   #{b\ 582}#)
                                                            (#{key\ 455}#
                                                              #{b\ 582}#
                                                              #{req\ 562}#
                                                              (reverse
                                                                #{ropt\ 563}#)
                                                              '()))
                                                          #{tmp\ 578}#)
                                                   ((lambda (#{tmp\ 583}#)
                                                      (if (if #{tmp\ 583}#
                                                            (apply (lambda (#{a\ 584}#
                                                                            #{b\ 585}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 584}#)
                                                                          #:predicate))
                                                                   #{tmp\ 583}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 586}#
                                                                        #{b\ 587}#)
                                                                 (#{pred\ 456}#
                                                                   #{b\ 587}#
                                                                   #{req\ 562}#
                                                                   (reverse
                                                                     #{ropt\ 563}#)
                                                                   '()))
                                                               #{tmp\ 583}#)
                                                        ((lambda (#{tmp\ 588}#)
                                                           (if (if #{tmp\ 588}#
                                                                 (apply (lambda (#{a\ 589}#
                                                                                 #{b\ 590}#)
                                                                          (eq? (syntax->datum
                                                                                 #{a\ 589}#)
                                                                               #:rest))
                                                                        #{tmp\ 588}#)
                                                                 #f)
                                                             (apply (lambda (#{a\ 591}#
                                                                             #{b\ 592}#)
                                                                      (#{rest\ 457}#
                                                                        #{b\ 592}#
                                                                        #{req\ 562}#
                                                                        (reverse
                                                                          #{ropt\ 563}#)
                                                                        '()
                                                                        #f))
                                                                    #{tmp\ 588}#)
                                                             ((lambda (#{tmp\ 593}#)
                                                                (if (if #{tmp\ 593}#
                                                                      (apply (lambda (#{r\ 594}#)
                                                                               (#{id?\ 131}#
                                                                                 #{r\ 594}#))
                                                                             #{tmp\ 593}#)
                                                                      #f)
                                                                  (apply (lambda (#{r\ 595}#)
                                                                           (#{rest\ 457}#
                                                                             #{r\ 595}#
                                                                             #{req\ 562}#
                                                                             (reverse
                                                                               #{ropt\ 563}#)
                                                                             '()
                                                                             #f))
                                                                         #{tmp\ 593}#)
                                                                  ((lambda (#{else\ 596}#)
                                                                     (syntax-violation
                                                                       'lambda*
                                                                       "invalid optional argument list"
                                                                       #{orig-args\ 452}#
                                                                       #{args\ 561}#))
                                                                   #{tmp\ 564}#)))
                                                              (list #{tmp\ 564}#))))
                                                         ($sc-dispatch
                                                           #{tmp\ 564}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 564}#
                                                      '(any . any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 564}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 564}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 564}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 564}# (quote ()))))
                             #{args\ 561}#)))
                        (#{req\ 453}#
                          (lambda (#{args\ 597}# #{rreq\ 598}#)
                            ((lambda (#{tmp\ 599}#)
                               ((lambda (#{tmp\ 600}#)
                                  (if #{tmp\ 600}#
                                    (apply (lambda ()
                                             (#{check\ 458}#
                                               (reverse #{rreq\ 598}#)
                                               '()
                                               #f
                                               '()
                                               #f))
                                           #{tmp\ 600}#)
                                    ((lambda (#{tmp\ 601}#)
                                       (if (if #{tmp\ 601}#
                                             (apply (lambda (#{a\ 602}#
                                                             #{b\ 603}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 602}#))
                                                    #{tmp\ 601}#)
                                             #f)
                                         (apply (lambda (#{a\ 604}# #{b\ 605}#)
                                                  (#{req\ 453}#
                                                    #{b\ 605}#
                                                    (cons #{a\ 604}#
                                                          #{rreq\ 598}#)))
                                                #{tmp\ 601}#)
                                         ((lambda (#{tmp\ 606}#)
                                            (if (if #{tmp\ 606}#
                                                  (apply (lambda (#{a\ 607}#
                                                                  #{b\ 608}#)
                                                           (eq? (syntax->datum
                                                                  #{a\ 607}#)
                                                                #:optional))
                                                         #{tmp\ 606}#)
                                                  #f)
                                              (apply (lambda (#{a\ 609}#
                                                              #{b\ 610}#)
                                                       (#{opt\ 454}#
                                                         #{b\ 610}#
                                                         (reverse
                                                           #{rreq\ 598}#)
                                                         '()))
                                                     #{tmp\ 606}#)
                                              ((lambda (#{tmp\ 611}#)
                                                 (if (if #{tmp\ 611}#
                                                       (apply (lambda (#{a\ 612}#
                                                                       #{b\ 613}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 612}#)
                                                                     #:key))
                                                              #{tmp\ 611}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 614}#
                                                                   #{b\ 615}#)
                                                            (#{key\ 455}#
                                                              #{b\ 615}#
                                                              (reverse
                                                                #{rreq\ 598}#)
                                                              '()
                                                              '()))
                                                          #{tmp\ 611}#)
                                                   ((lambda (#{tmp\ 616}#)
                                                      (if (if #{tmp\ 616}#
                                                            (apply (lambda (#{a\ 617}#
                                                                            #{b\ 618}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 617}#)
                                                                          #:predicate))
                                                                   #{tmp\ 616}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 619}#
                                                                        #{b\ 620}#)
                                                                 (#{pred\ 456}#
                                                                   #{b\ 620}#
                                                                   (reverse
                                                                     #{rreq\ 598}#)
                                                                   '()
                                                                   '()))
                                                               #{tmp\ 616}#)
                                                        ((lambda (#{tmp\ 621}#)
                                                           (if (if #{tmp\ 621}#
                                                                 (apply (lambda (#{a\ 622}#
                                                                                 #{b\ 623}#)
                                                                          (eq? (syntax->datum
                                                                                 #{a\ 622}#)
                                                                               #:rest))
                                                                        #{tmp\ 621}#)
                                                                 #f)
                                                             (apply (lambda (#{a\ 624}#
                                                                             #{b\ 625}#)
                                                                      (#{rest\ 457}#
                                                                        #{b\ 625}#
                                                                        (reverse
                                                                          #{rreq\ 598}#)
                                                                        '()
                                                                        '()
                                                                        #f))
                                                                    #{tmp\ 621}#)
                                                             ((lambda (#{tmp\ 626}#)
                                                                (if (if #{tmp\ 626}#
                                                                      (apply (lambda (#{r\ 627}#)
                                                                               (#{id?\ 131}#
                                                                                 #{r\ 627}#))
                                                                             #{tmp\ 626}#)
                                                                      #f)
                                                                  (apply (lambda (#{r\ 628}#)
                                                                           (#{rest\ 457}#
                                                                             #{r\ 628}#
                                                                             (reverse
                                                                               #{rreq\ 598}#)
                                                                             '()
                                                                             '()
                                                                             #f))
                                                                         #{tmp\ 626}#)
                                                                  ((lambda (#{else\ 629}#)
                                                                     (syntax-violation
                                                                       'lambda*
                                                                       "invalid argument list"
                                                                       #{orig-args\ 452}#
                                                                       #{args\ 597}#))
                                                                   #{tmp\ 599}#)))
                                                              (list #{tmp\ 599}#))))
                                                         ($sc-dispatch
                                                           #{tmp\ 599}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 599}#
                                                      '(any . any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 599}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 599}#
                                            '(any . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 599}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 599}# (quote ()))))
                             #{args\ 597}#))))
                 (#{req\ 453}# #{orig-args\ 452}# (quote ())))))
           (#{chi-simple-lambda\ 177}#
             (lambda (#{e\ 630}#
                      #{r\ 631}#
                      #{w\ 632}#
                      #{s\ 633}#
                      #{mod\ 634}#
                      #{req\ 635}#
                      #{rest\ 636}#
                      #{docstring\ 637}#
                      #{body\ 638}#)
               (let ((#{ids\ 639}#
                       (if #{rest\ 636}#
                         (append #{req\ 635}# (list #{rest\ 636}#))
                         #{req\ 635}#)))
                 (let ((#{vars\ 640}#
                         (map #{gen-var\ 181}# #{ids\ 639}#)))
                   (let ((#{labels\ 641}#
                           (#{gen-labels\ 137}# #{ids\ 639}#)))
                     (#{build-simple-lambda\ 105}#
                       #{s\ 633}#
                       (map syntax->datum #{req\ 635}#)
                       (if #{rest\ 636}#
                         (syntax->datum #{rest\ 636}#)
                         #f)
                       #{vars\ 640}#
                       #{docstring\ 637}#
                       (#{chi-body\ 171}#
                         #{body\ 638}#
                         (#{source-wrap\ 160}#
                           #{e\ 630}#
                           #{w\ 632}#
                           #{s\ 633}#
                           #{mod\ 634}#)
                         (#{extend-var-env\ 126}#
                           #{labels\ 641}#
                           #{vars\ 640}#
                           #{r\ 631}#)
                         (#{make-binding-wrap\ 148}#
                           #{ids\ 639}#
                           #{labels\ 641}#
                           #{w\ 632}#)
                         #{mod\ 634}#)))))))
           (#{lambda-formals\ 176}#
             (lambda (#{orig-args\ 642}#)
               (letrec ((#{check\ 644}#
                          (lambda (#{req\ 645}# #{rest\ 646}#)
                            (if (#{distinct-bound-ids?\ 157}#
                                  (if #{rest\ 646}#
                                    (cons #{rest\ 646}# #{req\ 645}#)
                                    #{req\ 645}#))
                              (values #{req\ 645}# #f #{rest\ 646}# #f #f)
                              (syntax-violation
                                'lambda
                                "duplicate identifier in argument list"
                                #{orig-args\ 642}#))))
                        (#{req\ 643}#
                          (lambda (#{args\ 647}# #{rreq\ 648}#)
                            ((lambda (#{tmp\ 649}#)
                               ((lambda (#{tmp\ 650}#)
                                  (if #{tmp\ 650}#
                                    (apply (lambda ()
                                             (#{check\ 644}#
                                               (reverse #{rreq\ 648}#)
                                               #f))
                                           #{tmp\ 650}#)
                                    ((lambda (#{tmp\ 651}#)
                                       (if (if #{tmp\ 651}#
                                             (apply (lambda (#{a\ 652}#
                                                             #{b\ 653}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 652}#))
                                                    #{tmp\ 651}#)
                                             #f)
                                         (apply (lambda (#{a\ 654}# #{b\ 655}#)
                                                  (#{req\ 643}#
                                                    #{b\ 655}#
                                                    (cons #{a\ 654}#
                                                          #{rreq\ 648}#)))
                                                #{tmp\ 651}#)
                                         ((lambda (#{tmp\ 656}#)
                                            (if (if #{tmp\ 656}#
                                                  (apply (lambda (#{r\ 657}#)
                                                           (#{id?\ 131}#
                                                             #{r\ 657}#))
                                                         #{tmp\ 656}#)
                                                  #f)
                                              (apply (lambda (#{r\ 658}#)
                                                       (#{check\ 644}#
                                                         (reverse
                                                           #{rreq\ 648}#)
                                                         #{r\ 658}#))
                                                     #{tmp\ 656}#)
                                              ((lambda (#{else\ 659}#)
                                                 (syntax-violation
                                                   'lambda
                                                   "invalid argument list"
                                                   #{orig-args\ 642}#
                                                   #{args\ 647}#))
                                               #{tmp\ 649}#)))
                                          (list #{tmp\ 649}#))))
                                     ($sc-dispatch
                                       #{tmp\ 649}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 649}# (quote ()))))
                             #{args\ 647}#))))
                 (#{req\ 643}# #{orig-args\ 642}# (quote ())))))
           (#{ellipsis?\ 175}#
             (lambda (#{x\ 660}#)
               (if (#{nonsymbol-id?\ 130}# #{x\ 660}#)
                 (#{free-id=?\ 154}#
                   #{x\ 660}#
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
           (#{chi-void\ 174}#
             (lambda () (#{build-void\ 95}# #f)))
           (#{eval-local-transformer\ 173}#
             (lambda (#{expanded\ 661}# #{mod\ 662}#)
               (let ((#{p\ 663}# (#{local-eval-hook\ 91}#
                                   #{expanded\ 661}#
                                   #{mod\ 662}#)))
                 (if (procedure? #{p\ 663}#)
                   #{p\ 663}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 663}#)))))
           (#{chi-local-syntax\ 172}#
             (lambda (#{rec?\ 664}#
                      #{e\ 665}#
                      #{r\ 666}#
                      #{w\ 667}#
                      #{s\ 668}#
                      #{mod\ 669}#
                      #{k\ 670}#)
               ((lambda (#{tmp\ 671}#)
                  ((lambda (#{tmp\ 672}#)
                     (if #{tmp\ 672}#
                       (apply (lambda (#{_\ 673}#
                                       #{id\ 674}#
                                       #{val\ 675}#
                                       #{e1\ 676}#
                                       #{e2\ 677}#)
                                (let ((#{ids\ 678}# #{id\ 674}#))
                                  (if (not (#{valid-bound-ids?\ 156}#
                                             #{ids\ 678}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 665}#)
                                    (let ((#{labels\ 680}#
                                            (#{gen-labels\ 137}#
                                              #{ids\ 678}#)))
                                      (let ((#{new-w\ 681}#
                                              (#{make-binding-wrap\ 148}#
                                                #{ids\ 678}#
                                                #{labels\ 680}#
                                                #{w\ 667}#)))
                                        (#{k\ 670}# (cons #{e1\ 676}#
                                                          #{e2\ 677}#)
                                                    (#{extend-env\ 125}#
                                                      #{labels\ 680}#
                                                      (let ((#{w\ 683}# (if #{rec?\ 664}#
                                                                          #{new-w\ 681}#
                                                                          #{w\ 667}#))
                                                            (#{trans-r\ 684}#
                                                              (#{macros-only-env\ 127}#
                                                                #{r\ 666}#)))
                                                        (map (lambda (#{x\ 685}#)
                                                               (cons 'macro
                                                                     (#{eval-local-transformer\ 173}#
                                                                       (#{chi\ 167}#
                                                                         #{x\ 685}#
                                                                         #{trans-r\ 684}#
                                                                         #{w\ 683}#
                                                                         #{mod\ 669}#)
                                                                       #{mod\ 669}#)))
                                                             #{val\ 675}#))
                                                      #{r\ 666}#)
                                                    #{new-w\ 681}#
                                                    #{s\ 668}#
                                                    #{mod\ 669}#))))))
                              #{tmp\ 672}#)
                       ((lambda (#{_\ 687}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 160}#
                              #{e\ 665}#
                              #{w\ 667}#
                              #{s\ 668}#
                              #{mod\ 669}#)))
                        #{tmp\ 671}#)))
                   ($sc-dispatch
                     #{tmp\ 671}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 665}#)))
           (#{chi-body\ 171}#
             (lambda (#{body\ 688}#
                      #{outer-form\ 689}#
                      #{r\ 690}#
                      #{w\ 691}#
                      #{mod\ 692}#)
               (let ((#{r\ 693}# (cons '("placeholder" placeholder)
                                       #{r\ 690}#)))
                 (let ((#{ribcage\ 694}#
                         (#{make-ribcage\ 138}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 695}# (#{make-wrap\ 133}#
                                       (#{wrap-marks\ 134}# #{w\ 691}#)
                                       (cons #{ribcage\ 694}#
                                             (#{wrap-subst\ 135}#
                                               #{w\ 691}#)))))
                     (letrec ((#{parse\ 696}#
                                (lambda (#{body\ 697}#
                                         #{ids\ 698}#
                                         #{labels\ 699}#
                                         #{var-ids\ 700}#
                                         #{vars\ 701}#
                                         #{vals\ 702}#
                                         #{bindings\ 703}#)
                                  (if (null? #{body\ 697}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 689}#)
                                    (let ((#{e\ 705}# (cdar #{body\ 697}#))
                                          (#{er\ 706}# (caar #{body\ 697}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 165}#
                                            #{e\ 705}#
                                            #{er\ 706}#
                                            '(())
                                            (#{source-annotation\ 122}#
                                              #{er\ 706}#)
                                            #{ribcage\ 694}#
                                            #{mod\ 692}#
                                            #f))
                                        (lambda (#{type\ 707}#
                                                 #{value\ 708}#
                                                 #{e\ 709}#
                                                 #{w\ 710}#
                                                 #{s\ 711}#
                                                 #{mod\ 712}#)
                                          (if (memv #{type\ 707}#
                                                    '(define-form))
                                            (let ((#{id\ 713}#
                                                    (#{wrap\ 159}#
                                                      #{value\ 708}#
                                                      #{w\ 710}#
                                                      #{mod\ 712}#))
                                                  (#{label\ 714}#
                                                    (#{gen-label\ 136}#)))
                                              (let ((#{var\ 715}#
                                                      (#{gen-var\ 181}#
                                                        #{id\ 713}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 694}#
                                                    #{id\ 713}#
                                                    #{label\ 714}#)
                                                  (#{parse\ 696}#
                                                    (cdr #{body\ 697}#)
                                                    (cons #{id\ 713}#
                                                          #{ids\ 698}#)
                                                    (cons #{label\ 714}#
                                                          #{labels\ 699}#)
                                                    (cons #{id\ 713}#
                                                          #{var-ids\ 700}#)
                                                    (cons #{var\ 715}#
                                                          #{vars\ 701}#)
                                                    (cons (cons #{er\ 706}#
                                                                (#{wrap\ 159}#
                                                                  #{e\ 709}#
                                                                  #{w\ 710}#
                                                                  #{mod\ 712}#))
                                                          #{vals\ 702}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 715}#)
                                                          #{bindings\ 703}#)))))
                                            (if (memv #{type\ 707}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 716}#
                                                      (#{wrap\ 159}#
                                                        #{value\ 708}#
                                                        #{w\ 710}#
                                                        #{mod\ 712}#))
                                                    (#{label\ 717}#
                                                      (#{gen-label\ 136}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 694}#
                                                    #{id\ 716}#
                                                    #{label\ 717}#)
                                                  (#{parse\ 696}#
                                                    (cdr #{body\ 697}#)
                                                    (cons #{id\ 716}#
                                                          #{ids\ 698}#)
                                                    (cons #{label\ 717}#
                                                          #{labels\ 699}#)
                                                    #{var-ids\ 700}#
                                                    #{vars\ 701}#
                                                    #{vals\ 702}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 706}#
                                                                      (#{wrap\ 159}#
                                                                        #{e\ 709}#
                                                                        #{w\ 710}#
                                                                        #{mod\ 712}#)))
                                                          #{bindings\ 703}#))))
                                              (if (memv #{type\ 707}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 718}#)
                                                   ((lambda (#{tmp\ 719}#)
                                                      (if #{tmp\ 719}#
                                                        (apply (lambda (#{_\ 720}#
                                                                        #{e1\ 721}#)
                                                                 (#{parse\ 696}#
                                                                   (letrec ((#{f\ 722}# (lambda (#{forms\ 723}#)
                                                                                          (if (null? #{forms\ 723}#)
                                                                                            (cdr #{body\ 697}#)
                                                                                            (cons (cons #{er\ 706}#
                                                                                                        (#{wrap\ 159}#
                                                                                                          (car #{forms\ 723}#)
                                                                                                          #{w\ 710}#
                                                                                                          #{mod\ 712}#))
                                                                                                  (#{f\ 722}# (cdr #{forms\ 723}#)))))))
                                                                     (#{f\ 722}# #{e1\ 721}#))
                                                                   #{ids\ 698}#
                                                                   #{labels\ 699}#
                                                                   #{var-ids\ 700}#
                                                                   #{vars\ 701}#
                                                                   #{vals\ 702}#
                                                                   #{bindings\ 703}#))
                                                               #{tmp\ 719}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 718}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 718}#
                                                      '(any . each-any))))
                                                 #{e\ 709}#)
                                                (if (memv #{type\ 707}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 172}#
                                                    #{value\ 708}#
                                                    #{e\ 709}#
                                                    #{er\ 706}#
                                                    #{w\ 710}#
                                                    #{s\ 711}#
                                                    #{mod\ 712}#
                                                    (lambda (#{forms\ 725}#
                                                             #{er\ 726}#
                                                             #{w\ 727}#
                                                             #{s\ 728}#
                                                             #{mod\ 729}#)
                                                      (#{parse\ 696}#
                                                        (letrec ((#{f\ 730}# (lambda (#{forms\ 731}#)
                                                                               (if (null? #{forms\ 731}#)
                                                                                 (cdr #{body\ 697}#)
                                                                                 (cons (cons #{er\ 726}#
                                                                                             (#{wrap\ 159}#
                                                                                               (car #{forms\ 731}#)
                                                                                               #{w\ 727}#
                                                                                               #{mod\ 729}#))
                                                                                       (#{f\ 730}# (cdr #{forms\ 731}#)))))))
                                                          (#{f\ 730}# #{forms\ 725}#))
                                                        #{ids\ 698}#
                                                        #{labels\ 699}#
                                                        #{var-ids\ 700}#
                                                        #{vars\ 701}#
                                                        #{vals\ 702}#
                                                        #{bindings\ 703}#)))
                                                  (if (null? #{ids\ 698}#)
                                                    (#{build-sequence\ 110}#
                                                      #f
                                                      (map (lambda (#{x\ 732}#)
                                                             (#{chi\ 167}#
                                                               (cdr #{x\ 732}#)
                                                               (car #{x\ 732}#)
                                                               '(())
                                                               #{mod\ 712}#))
                                                           (cons (cons #{er\ 706}#
                                                                       (#{source-wrap\ 160}#
                                                                         #{e\ 709}#
                                                                         #{w\ 710}#
                                                                         #{s\ 711}#
                                                                         #{mod\ 712}#))
                                                                 (cdr #{body\ 697}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 156}#
                                                                 #{ids\ 698}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 689}#))
                                                      (letrec ((#{loop\ 733}#
                                                                 (lambda (#{bs\ 734}#
                                                                          #{er-cache\ 735}#
                                                                          #{r-cache\ 736}#)
                                                                   (if (not (null? #{bs\ 734}#))
                                                                     (let ((#{b\ 737}# (car #{bs\ 734}#)))
                                                                       (if (eq? (car #{b\ 737}#)
                                                                                'macro)
                                                                         (let ((#{er\ 738}#
                                                                                 (cadr #{b\ 737}#)))
                                                                           (let ((#{r-cache\ 739}#
                                                                                   (if (eq? #{er\ 738}#
                                                                                            #{er-cache\ 735}#)
                                                                                     #{r-cache\ 736}#
                                                                                     (#{macros-only-env\ 127}#
                                                                                       #{er\ 738}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 737}#
                                                                                 (#{eval-local-transformer\ 173}#
                                                                                   (#{chi\ 167}#
                                                                                     (cddr #{b\ 737}#)
                                                                                     #{r-cache\ 739}#
                                                                                     '(())
                                                                                     #{mod\ 712}#)
                                                                                   #{mod\ 712}#))
                                                                               (#{loop\ 733}#
                                                                                 (cdr #{bs\ 734}#)
                                                                                 #{er\ 738}#
                                                                                 #{r-cache\ 739}#))))
                                                                         (#{loop\ 733}#
                                                                           (cdr #{bs\ 734}#)
                                                                           #{er-cache\ 735}#
                                                                           #{r-cache\ 736}#)))))))
                                                        (#{loop\ 733}#
                                                          #{bindings\ 703}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 693}#
                                                        (#{extend-env\ 125}#
                                                          #{labels\ 699}#
                                                          #{bindings\ 703}#
                                                          (cdr #{r\ 693}#)))
                                                      (#{build-letrec\ 113}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 700}#)
                                                        #{vars\ 701}#
                                                        (map (lambda (#{x\ 740}#)
                                                               (#{chi\ 167}#
                                                                 (cdr #{x\ 740}#)
                                                                 (car #{x\ 740}#)
                                                                 '(())
                                                                 #{mod\ 712}#))
                                                             #{vals\ 702}#)
                                                        (#{build-sequence\ 110}#
                                                          #f
                                                          (map (lambda (#{x\ 741}#)
                                                                 (#{chi\ 167}#
                                                                   (cdr #{x\ 741}#)
                                                                   (car #{x\ 741}#)
                                                                   '(())
                                                                   #{mod\ 712}#))
                                                               (cons (cons #{er\ 706}#
                                                                           (#{source-wrap\ 160}#
                                                                             #{e\ 709}#
                                                                             #{w\ 710}#
                                                                             #{s\ 711}#
                                                                             #{mod\ 712}#))
                                                                     (cdr #{body\ 697}#))))))))))))))))))
                       (#{parse\ 696}#
                         (map (lambda (#{x\ 704}#)
                                (cons #{r\ 693}#
                                      (#{wrap\ 159}#
                                        #{x\ 704}#
                                        #{w\ 695}#
                                        #{mod\ 692}#)))
                              #{body\ 688}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 170}#
             (lambda (#{p\ 742}#
                      #{e\ 743}#
                      #{r\ 744}#
                      #{w\ 745}#
                      #{rib\ 746}#
                      #{mod\ 747}#)
               (letrec ((#{rebuild-macro-output\ 748}#
                          (lambda (#{x\ 749}# #{m\ 750}#)
                            (if (pair? #{x\ 749}#)
                              (cons (#{rebuild-macro-output\ 748}#
                                      (car #{x\ 749}#)
                                      #{m\ 750}#)
                                    (#{rebuild-macro-output\ 748}#
                                      (cdr #{x\ 749}#)
                                      #{m\ 750}#))
                              (if (#{syntax-object?\ 115}# #{x\ 749}#)
                                (let ((#{w\ 751}# (#{syntax-object-wrap\ 117}#
                                                    #{x\ 749}#)))
                                  (let ((#{ms\ 752}#
                                          (#{wrap-marks\ 134}# #{w\ 751}#))
                                        (#{s\ 753}# (#{wrap-subst\ 135}#
                                                      #{w\ 751}#)))
                                    (if (if (pair? #{ms\ 752}#)
                                          (eq? (car #{ms\ 752}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 749}#)
                                        (#{make-wrap\ 133}#
                                          (cdr #{ms\ 752}#)
                                          (if #{rib\ 746}#
                                            (cons #{rib\ 746}#
                                                  (cdr #{s\ 753}#))
                                            (cdr #{s\ 753}#)))
                                        (#{syntax-object-module\ 118}#
                                          #{x\ 749}#))
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 749}#)
                                        (#{make-wrap\ 133}#
                                          (cons #{m\ 750}# #{ms\ 752}#)
                                          (if #{rib\ 746}#
                                            (cons #{rib\ 746}#
                                                  (cons 'shift
                                                        #{s\ 753}#))
                                            (cons (quote shift) #{s\ 753}#)))
                                        (let ((#{pmod\ 754}#
                                                (procedure-module #{p\ 742}#)))
                                          (if #{pmod\ 754}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 754}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 749}#)
                                  (let ((#{n\ 755}# (vector-length
                                                      #{x\ 749}#)))
                                    (let ((#{v\ 756}# (make-vector
                                                        #{n\ 755}#)))
                                      (letrec ((#{loop\ 757}#
                                                 (lambda (#{i\ 758}#)
                                                   (if (#{fx=\ 88}#
                                                         #{i\ 758}#
                                                         #{n\ 755}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 756}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 756}#
                                                         #{i\ 758}#
                                                         (#{rebuild-macro-output\ 748}#
                                                           (vector-ref
                                                             #{x\ 749}#
                                                             #{i\ 758}#)
                                                           #{m\ 750}#))
                                                       (#{loop\ 757}#
                                                         (#{fx+\ 86}#
                                                           #{i\ 758}#
                                                           1)))))))
                                        (#{loop\ 757}# 0))))
                                  (if (symbol? #{x\ 749}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 160}#
                                        #{e\ 743}#
                                        #{w\ 745}#
                                        (#{wrap-subst\ 135}# #{w\ 745}#)
                                        #{mod\ 747}#)
                                      #{x\ 749}#)
                                    #{x\ 749}#)))))))
                 (#{rebuild-macro-output\ 748}#
                   (#{p\ 742}# (#{wrap\ 159}#
                                 #{e\ 743}#
                                 (#{anti-mark\ 146}# #{w\ 745}#)
                                 #{mod\ 747}#))
                   (string #\m)))))
           (#{chi-application\ 169}#
             (lambda (#{x\ 759}#
                      #{e\ 760}#
                      #{r\ 761}#
                      #{w\ 762}#
                      #{s\ 763}#
                      #{mod\ 764}#)
               ((lambda (#{tmp\ 765}#)
                  ((lambda (#{tmp\ 766}#)
                     (if #{tmp\ 766}#
                       (apply (lambda (#{e0\ 767}# #{e1\ 768}#)
                                (#{build-application\ 96}#
                                  #{s\ 763}#
                                  #{x\ 759}#
                                  (map (lambda (#{e\ 769}#)
                                         (#{chi\ 167}#
                                           #{e\ 769}#
                                           #{r\ 761}#
                                           #{w\ 762}#
                                           #{mod\ 764}#))
                                       #{e1\ 768}#)))
                              #{tmp\ 766}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 765}#)))
                   ($sc-dispatch
                     #{tmp\ 765}#
                     '(any . each-any))))
                #{e\ 760}#)))
           (#{chi-expr\ 168}#
             (lambda (#{type\ 771}#
                      #{value\ 772}#
                      #{e\ 773}#
                      #{r\ 774}#
                      #{w\ 775}#
                      #{s\ 776}#
                      #{mod\ 777}#)
               (if (memv #{type\ 771}# (quote (lexical)))
                 (#{build-lexical-reference\ 98}#
                   'value
                   #{s\ 776}#
                   #{e\ 773}#
                   #{value\ 772}#)
                 (if (memv #{type\ 771}# (quote (core core-form)))
                   (#{value\ 772}#
                     #{e\ 773}#
                     #{r\ 774}#
                     #{w\ 775}#
                     #{s\ 776}#
                     #{mod\ 777}#)
                   (if (memv #{type\ 771}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 772}# #{e\ 773}#))
                       (lambda (#{id\ 778}# #{mod\ 779}#)
                         (#{build-global-reference\ 101}#
                           #{s\ 776}#
                           #{id\ 778}#
                           #{mod\ 779}#)))
                     (if (memv #{type\ 771}# (quote (lexical-call)))
                       (#{chi-application\ 169}#
                         (#{build-lexical-reference\ 98}#
                           'fun
                           (#{source-annotation\ 122}# (car #{e\ 773}#))
                           (car #{e\ 773}#)
                           #{value\ 772}#)
                         #{e\ 773}#
                         #{r\ 774}#
                         #{w\ 775}#
                         #{s\ 776}#
                         #{mod\ 777}#)
                       (if (memv #{type\ 771}# (quote (global-call)))
                         (#{chi-application\ 169}#
                           (#{build-global-reference\ 101}#
                             (#{source-annotation\ 122}# (car #{e\ 773}#))
                             (if (#{syntax-object?\ 115}# #{value\ 772}#)
                               (#{syntax-object-expression\ 116}#
                                 #{value\ 772}#)
                               #{value\ 772}#)
                             (if (#{syntax-object?\ 115}# #{value\ 772}#)
                               (#{syntax-object-module\ 118}# #{value\ 772}#)
                               #{mod\ 777}#))
                           #{e\ 773}#
                           #{r\ 774}#
                           #{w\ 775}#
                           #{s\ 776}#
                           #{mod\ 777}#)
                         (if (memv #{type\ 771}# (quote (constant)))
                           (#{build-data\ 109}#
                             #{s\ 776}#
                             (#{strip\ 180}#
                               (#{source-wrap\ 160}#
                                 #{e\ 773}#
                                 #{w\ 775}#
                                 #{s\ 776}#
                                 #{mod\ 777}#)
                               '(())))
                           (if (memv #{type\ 771}# (quote (global)))
                             (#{build-global-reference\ 101}#
                               #{s\ 776}#
                               #{value\ 772}#
                               #{mod\ 777}#)
                             (if (memv #{type\ 771}# (quote (call)))
                               (#{chi-application\ 169}#
                                 (#{chi\ 167}#
                                   (car #{e\ 773}#)
                                   #{r\ 774}#
                                   #{w\ 775}#
                                   #{mod\ 777}#)
                                 #{e\ 773}#
                                 #{r\ 774}#
                                 #{w\ 775}#
                                 #{s\ 776}#
                                 #{mod\ 777}#)
                               (if (memv #{type\ 771}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 780}#)
                                    ((lambda (#{tmp\ 781}#)
                                       (if #{tmp\ 781}#
                                         (apply (lambda (#{_\ 782}#
                                                         #{e1\ 783}#
                                                         #{e2\ 784}#)
                                                  (#{chi-sequence\ 161}#
                                                    (cons #{e1\ 783}#
                                                          #{e2\ 784}#)
                                                    #{r\ 774}#
                                                    #{w\ 775}#
                                                    #{s\ 776}#
                                                    #{mod\ 777}#))
                                                #{tmp\ 781}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 780}#)))
                                     ($sc-dispatch
                                       #{tmp\ 780}#
                                       '(any any . each-any))))
                                  #{e\ 773}#)
                                 (if (memv #{type\ 771}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 172}#
                                     #{value\ 772}#
                                     #{e\ 773}#
                                     #{r\ 774}#
                                     #{w\ 775}#
                                     #{s\ 776}#
                                     #{mod\ 777}#
                                     #{chi-sequence\ 161}#)
                                   (if (memv #{type\ 771}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 786}#)
                                        ((lambda (#{tmp\ 787}#)
                                           (if #{tmp\ 787}#
                                             (apply (lambda (#{_\ 788}#
                                                             #{x\ 789}#
                                                             #{e1\ 790}#
                                                             #{e2\ 791}#)
                                                      (let ((#{when-list\ 792}#
                                                              (#{chi-when-list\ 164}#
                                                                #{e\ 773}#
                                                                #{x\ 789}#
                                                                #{w\ 775}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 792}#)
                                                          (#{chi-sequence\ 161}#
                                                            (cons #{e1\ 790}#
                                                                  #{e2\ 791}#)
                                                            #{r\ 774}#
                                                            #{w\ 775}#
                                                            #{s\ 776}#
                                                            #{mod\ 777}#)
                                                          (#{chi-void\ 174}#))))
                                                    #{tmp\ 787}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 786}#)))
                                         ($sc-dispatch
                                           #{tmp\ 786}#
                                           '(any each-any any . each-any))))
                                      #{e\ 773}#)
                                     (if (memv #{type\ 771}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 773}#
                                         (#{wrap\ 159}#
                                           #{value\ 772}#
                                           #{w\ 775}#
                                           #{mod\ 777}#))
                                       (if (memv #{type\ 771}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 160}#
                                             #{e\ 773}#
                                             #{w\ 775}#
                                             #{s\ 776}#
                                             #{mod\ 777}#))
                                         (if (memv #{type\ 771}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 160}#
                                               #{e\ 773}#
                                               #{w\ 775}#
                                               #{s\ 776}#
                                               #{mod\ 777}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 160}#
                                               #{e\ 773}#
                                               #{w\ 775}#
                                               #{s\ 776}#
                                               #{mod\ 777}#))))))))))))))))))
           (#{chi\ 167}#
             (lambda (#{e\ 795}# #{r\ 796}# #{w\ 797}# #{mod\ 798}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 795}#
                     #{r\ 796}#
                     #{w\ 797}#
                     (#{source-annotation\ 122}# #{e\ 795}#)
                     #f
                     #{mod\ 798}#
                     #f))
                 (lambda (#{type\ 799}#
                          #{value\ 800}#
                          #{e\ 801}#
                          #{w\ 802}#
                          #{s\ 803}#
                          #{mod\ 804}#)
                   (#{chi-expr\ 168}#
                     #{type\ 799}#
                     #{value\ 800}#
                     #{e\ 801}#
                     #{r\ 796}#
                     #{w\ 802}#
                     #{s\ 803}#
                     #{mod\ 804}#)))))
           (#{chi-top\ 166}#
             (lambda (#{e\ 805}#
                      #{r\ 806}#
                      #{w\ 807}#
                      #{m\ 808}#
                      #{esew\ 809}#
                      #{mod\ 810}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 805}#
                     #{r\ 806}#
                     #{w\ 807}#
                     (#{source-annotation\ 122}# #{e\ 805}#)
                     #f
                     #{mod\ 810}#
                     #f))
                 (lambda (#{type\ 818}#
                          #{value\ 819}#
                          #{e\ 820}#
                          #{w\ 821}#
                          #{s\ 822}#
                          #{mod\ 823}#)
                   (if (memv #{type\ 818}# (quote (begin-form)))
                     ((lambda (#{tmp\ 824}#)
                        ((lambda (#{tmp\ 825}#)
                           (if #{tmp\ 825}#
                             (apply (lambda (#{_\ 826}#) (#{chi-void\ 174}#))
                                    #{tmp\ 825}#)
                             ((lambda (#{tmp\ 827}#)
                                (if #{tmp\ 827}#
                                  (apply (lambda (#{_\ 828}#
                                                  #{e1\ 829}#
                                                  #{e2\ 830}#)
                                           (#{chi-top-sequence\ 162}#
                                             (cons #{e1\ 829}# #{e2\ 830}#)
                                             #{r\ 806}#
                                             #{w\ 821}#
                                             #{s\ 822}#
                                             #{m\ 808}#
                                             #{esew\ 809}#
                                             #{mod\ 823}#))
                                         #{tmp\ 827}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 824}#)))
                              ($sc-dispatch
                                #{tmp\ 824}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 824}# (quote (any)))))
                      #{e\ 820}#)
                     (if (memv #{type\ 818}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 172}#
                         #{value\ 819}#
                         #{e\ 820}#
                         #{r\ 806}#
                         #{w\ 821}#
                         #{s\ 822}#
                         #{mod\ 823}#
                         (lambda (#{body\ 832}#
                                  #{r\ 833}#
                                  #{w\ 834}#
                                  #{s\ 835}#
                                  #{mod\ 836}#)
                           (#{chi-top-sequence\ 162}#
                             #{body\ 832}#
                             #{r\ 833}#
                             #{w\ 834}#
                             #{s\ 835}#
                             #{m\ 808}#
                             #{esew\ 809}#
                             #{mod\ 836}#)))
                       (if (memv #{type\ 818}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 837}#)
                            ((lambda (#{tmp\ 838}#)
                               (if #{tmp\ 838}#
                                 (apply (lambda (#{_\ 839}#
                                                 #{x\ 840}#
                                                 #{e1\ 841}#
                                                 #{e2\ 842}#)
                                          (let ((#{when-list\ 843}#
                                                  (#{chi-when-list\ 164}#
                                                    #{e\ 820}#
                                                    #{x\ 840}#
                                                    #{w\ 821}#))
                                                (#{body\ 844}#
                                                  (cons #{e1\ 841}#
                                                        #{e2\ 842}#)))
                                            (if (eq? #{m\ 808}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 843}#)
                                                (#{chi-top-sequence\ 162}#
                                                  #{body\ 844}#
                                                  #{r\ 806}#
                                                  #{w\ 821}#
                                                  #{s\ 822}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 823}#)
                                                (#{chi-void\ 174}#))
                                              (if (memq 'load
                                                        #{when-list\ 843}#)
                                                (if (let ((#{t\ 847}# (memq 'compile
                                                                            #{when-list\ 843}#)))
                                                      (if #{t\ 847}#
                                                        #{t\ 847}#
                                                        (if (eq? #{m\ 808}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 843}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 162}#
                                                    #{body\ 844}#
                                                    #{r\ 806}#
                                                    #{w\ 821}#
                                                    #{s\ 822}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 823}#)
                                                  (if (memq #{m\ 808}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 162}#
                                                      #{body\ 844}#
                                                      #{r\ 806}#
                                                      #{w\ 821}#
                                                      #{s\ 822}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 823}#)
                                                    (#{chi-void\ 174}#)))
                                                (if (let ((#{t\ 848}# (memq 'compile
                                                                            #{when-list\ 843}#)))
                                                      (if #{t\ 848}#
                                                        #{t\ 848}#
                                                        (if (eq? #{m\ 808}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 843}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 90}#
                                                      (#{chi-top-sequence\ 162}#
                                                        #{body\ 844}#
                                                        #{r\ 806}#
                                                        #{w\ 821}#
                                                        #{s\ 822}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 823}#)
                                                      #{mod\ 823}#)
                                                    (#{chi-void\ 174}#))
                                                  (#{chi-void\ 174}#))))))
                                        #{tmp\ 838}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 837}#)))
                             ($sc-dispatch
                               #{tmp\ 837}#
                               '(any each-any any . each-any))))
                          #{e\ 820}#)
                         (if (memv #{type\ 818}# (quote (define-syntax-form)))
                           (let ((#{n\ 849}# (#{id-var-name\ 153}#
                                               #{value\ 819}#
                                               #{w\ 821}#))
                                 (#{r\ 850}# (#{macros-only-env\ 127}#
                                               #{r\ 806}#)))
                             (if (memv #{m\ 808}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 809}#)
                                 (let ((#{e\ 851}# (#{chi-install-global\ 163}#
                                                     #{n\ 849}#
                                                     (#{chi\ 167}#
                                                       #{e\ 820}#
                                                       #{r\ 850}#
                                                       #{w\ 821}#
                                                       #{mod\ 823}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 851}#
                                       #{mod\ 823}#)
                                     (if (memq (quote load) #{esew\ 809}#)
                                       #{e\ 851}#
                                       (#{chi-void\ 174}#))))
                                 (if (memq (quote load) #{esew\ 809}#)
                                   (#{chi-install-global\ 163}#
                                     #{n\ 849}#
                                     (#{chi\ 167}#
                                       #{e\ 820}#
                                       #{r\ 850}#
                                       #{w\ 821}#
                                       #{mod\ 823}#))
                                   (#{chi-void\ 174}#)))
                               (if (memv #{m\ 808}# (quote (c&e)))
                                 (let ((#{e\ 852}# (#{chi-install-global\ 163}#
                                                     #{n\ 849}#
                                                     (#{chi\ 167}#
                                                       #{e\ 820}#
                                                       #{r\ 850}#
                                                       #{w\ 821}#
                                                       #{mod\ 823}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 852}#
                                       #{mod\ 823}#)
                                     #{e\ 852}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 809}#)
                                     (#{top-level-eval-hook\ 90}#
                                       (#{chi-install-global\ 163}#
                                         #{n\ 849}#
                                         (#{chi\ 167}#
                                           #{e\ 820}#
                                           #{r\ 850}#
                                           #{w\ 821}#
                                           #{mod\ 823}#))
                                       #{mod\ 823}#))
                                   (#{chi-void\ 174}#)))))
                           (if (memv #{type\ 818}# (quote (define-form)))
                             (let ((#{n\ 853}# (#{id-var-name\ 153}#
                                                 #{value\ 819}#
                                                 #{w\ 821}#)))
                               (let ((#{type\ 854}#
                                       (#{binding-type\ 123}#
                                         (#{lookup\ 128}#
                                           #{n\ 853}#
                                           #{r\ 806}#
                                           #{mod\ 823}#))))
                                 (if (memv #{type\ 854}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 853}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 855}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 853}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 853}#
                                           (if (variable? #{old\ 855}#)
                                             (variable-ref #{old\ 855}#)
                                             #f))))
                                     (let ((#{x\ 856}# (#{build-global-definition\ 104}#
                                                         #{s\ 822}#
                                                         #{n\ 853}#
                                                         (#{chi\ 167}#
                                                           #{e\ 820}#
                                                           #{r\ 806}#
                                                           #{w\ 821}#
                                                           #{mod\ 823}#))))
                                       (begin
                                         (if (eq? #{m\ 808}# (quote c&e))
                                           (#{top-level-eval-hook\ 90}#
                                             #{x\ 856}#
                                             #{mod\ 823}#))
                                         #{x\ 856}#)))
                                   (if (memv #{type\ 854}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 820}#
                                       (#{wrap\ 159}#
                                         #{value\ 819}#
                                         #{w\ 821}#
                                         #{mod\ 823}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 820}#
                                       (#{wrap\ 159}#
                                         #{value\ 819}#
                                         #{w\ 821}#
                                         #{mod\ 823}#))))))
                             (let ((#{x\ 857}# (#{chi-expr\ 168}#
                                                 #{type\ 818}#
                                                 #{value\ 819}#
                                                 #{e\ 820}#
                                                 #{r\ 806}#
                                                 #{w\ 821}#
                                                 #{s\ 822}#
                                                 #{mod\ 823}#)))
                               (begin
                                 (if (eq? #{m\ 808}# (quote c&e))
                                   (#{top-level-eval-hook\ 90}#
                                     #{x\ 857}#
                                     #{mod\ 823}#))
                                 #{x\ 857}#)))))))))))
           (#{syntax-type\ 165}#
             (lambda (#{e\ 858}#
                      #{r\ 859}#
                      #{w\ 860}#
                      #{s\ 861}#
                      #{rib\ 862}#
                      #{mod\ 863}#
                      #{for-car?\ 864}#)
               (if (symbol? #{e\ 858}#)
                 (let ((#{n\ 865}# (#{id-var-name\ 153}#
                                     #{e\ 858}#
                                     #{w\ 860}#)))
                   (let ((#{b\ 866}# (#{lookup\ 128}#
                                       #{n\ 865}#
                                       #{r\ 859}#
                                       #{mod\ 863}#)))
                     (let ((#{type\ 867}#
                             (#{binding-type\ 123}# #{b\ 866}#)))
                       (if (memv #{type\ 867}# (quote (lexical)))
                         (values
                           #{type\ 867}#
                           (#{binding-value\ 124}# #{b\ 866}#)
                           #{e\ 858}#
                           #{w\ 860}#
                           #{s\ 861}#
                           #{mod\ 863}#)
                         (if (memv #{type\ 867}# (quote (global)))
                           (values
                             #{type\ 867}#
                             #{n\ 865}#
                             #{e\ 858}#
                             #{w\ 860}#
                             #{s\ 861}#
                             #{mod\ 863}#)
                           (if (memv #{type\ 867}# (quote (macro)))
                             (if #{for-car?\ 864}#
                               (values
                                 #{type\ 867}#
                                 (#{binding-value\ 124}# #{b\ 866}#)
                                 #{e\ 858}#
                                 #{w\ 860}#
                                 #{s\ 861}#
                                 #{mod\ 863}#)
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   (#{binding-value\ 124}# #{b\ 866}#)
                                   #{e\ 858}#
                                   #{r\ 859}#
                                   #{w\ 860}#
                                   #{rib\ 862}#
                                   #{mod\ 863}#)
                                 #{r\ 859}#
                                 '(())
                                 #{s\ 861}#
                                 #{rib\ 862}#
                                 #{mod\ 863}#
                                 #f))
                             (values
                               #{type\ 867}#
                               (#{binding-value\ 124}# #{b\ 866}#)
                               #{e\ 858}#
                               #{w\ 860}#
                               #{s\ 861}#
                               #{mod\ 863}#)))))))
                 (if (pair? #{e\ 858}#)
                   (let ((#{first\ 868}# (car #{e\ 858}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 165}#
                           #{first\ 868}#
                           #{r\ 859}#
                           #{w\ 860}#
                           #{s\ 861}#
                           #{rib\ 862}#
                           #{mod\ 863}#
                           #t))
                       (lambda (#{ftype\ 869}#
                                #{fval\ 870}#
                                #{fe\ 871}#
                                #{fw\ 872}#
                                #{fs\ 873}#
                                #{fmod\ 874}#)
                         (if (memv #{ftype\ 869}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 870}#
                             #{e\ 858}#
                             #{w\ 860}#
                             #{s\ 861}#
                             #{mod\ 863}#)
                           (if (memv #{ftype\ 869}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 114}#
                                 #{fval\ 870}#
                                 #{w\ 860}#
                                 #{fmod\ 874}#)
                               #{e\ 858}#
                               #{w\ 860}#
                               #{s\ 861}#
                               #{mod\ 863}#)
                             (if (memv #{ftype\ 869}# (quote (macro)))
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   #{fval\ 870}#
                                   #{e\ 858}#
                                   #{r\ 859}#
                                   #{w\ 860}#
                                   #{rib\ 862}#
                                   #{mod\ 863}#)
                                 #{r\ 859}#
                                 '(())
                                 #{s\ 861}#
                                 #{rib\ 862}#
                                 #{mod\ 863}#
                                 #{for-car?\ 864}#)
                               (if (memv #{ftype\ 869}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 870}# #{e\ 858}#))
                                   (lambda (#{sym\ 875}# #{mod\ 876}#)
                                     (#{syntax-type\ 165}#
                                       #{sym\ 875}#
                                       #{r\ 859}#
                                       #{w\ 860}#
                                       #{s\ 861}#
                                       #{rib\ 862}#
                                       #{mod\ 876}#
                                       #{for-car?\ 864}#)))
                                 (if (memv #{ftype\ 869}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 870}#
                                     #{e\ 858}#
                                     #{w\ 860}#
                                     #{s\ 861}#
                                     #{mod\ 863}#)
                                   (if (memv #{ftype\ 869}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 870}#
                                       #{e\ 858}#
                                       #{w\ 860}#
                                       #{s\ 861}#
                                       #{mod\ 863}#)
                                     (if (memv #{ftype\ 869}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 858}#
                                         #{w\ 860}#
                                         #{s\ 861}#
                                         #{mod\ 863}#)
                                       (if (memv #{ftype\ 869}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 858}#
                                           #{w\ 860}#
                                           #{s\ 861}#
                                           #{mod\ 863}#)
                                         (if (memv #{ftype\ 869}#
                                                   '(define))
                                           ((lambda (#{tmp\ 877}#)
                                              ((lambda (#{tmp\ 878}#)
                                                 (if (if #{tmp\ 878}#
                                                       (apply (lambda (#{_\ 879}#
                                                                       #{name\ 880}#
                                                                       #{val\ 881}#)
                                                                (#{id?\ 131}#
                                                                  #{name\ 880}#))
                                                              #{tmp\ 878}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 882}#
                                                                   #{name\ 883}#
                                                                   #{val\ 884}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 883}#
                                                              #{val\ 884}#
                                                              #{w\ 860}#
                                                              #{s\ 861}#
                                                              #{mod\ 863}#))
                                                          #{tmp\ 878}#)
                                                   ((lambda (#{tmp\ 885}#)
                                                      (if (if #{tmp\ 885}#
                                                            (apply (lambda (#{_\ 886}#
                                                                            #{name\ 887}#
                                                                            #{args\ 888}#
                                                                            #{e1\ 889}#
                                                                            #{e2\ 890}#)
                                                                     (if (#{id?\ 131}#
                                                                           #{name\ 887}#)
                                                                       (#{valid-bound-ids?\ 156}#
                                                                         (#{lambda-var-list\ 182}#
                                                                           #{args\ 888}#))
                                                                       #f))
                                                                   #{tmp\ 885}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 891}#
                                                                        #{name\ 892}#
                                                                        #{args\ 893}#
                                                                        #{e1\ 894}#
                                                                        #{e2\ 895}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 159}#
                                                                     #{name\ 892}#
                                                                     #{w\ 860}#
                                                                     #{mod\ 863}#)
                                                                   (#{decorate-source\ 94}#
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
                                                                           (#{wrap\ 159}#
                                                                             (cons #{args\ 893}#
                                                                                   (cons #{e1\ 894}#
                                                                                         #{e2\ 895}#))
                                                                             #{w\ 860}#
                                                                             #{mod\ 863}#))
                                                                     #{s\ 861}#)
                                                                   '(())
                                                                   #{s\ 861}#
                                                                   #{mod\ 863}#))
                                                               #{tmp\ 885}#)
                                                        ((lambda (#{tmp\ 897}#)
                                                           (if (if #{tmp\ 897}#
                                                                 (apply (lambda (#{_\ 898}#
                                                                                 #{name\ 899}#)
                                                                          (#{id?\ 131}#
                                                                            #{name\ 899}#))
                                                                        #{tmp\ 897}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 900}#
                                                                             #{name\ 901}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 159}#
                                                                          #{name\ 901}#
                                                                          #{w\ 860}#
                                                                          #{mod\ 863}#)
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
                                                                        #{s\ 861}#
                                                                        #{mod\ 863}#))
                                                                    #{tmp\ 897}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 877}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 877}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 877}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 877}#
                                                 '(any any any))))
                                            #{e\ 858}#)
                                           (if (memv #{ftype\ 869}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 902}#)
                                                ((lambda (#{tmp\ 903}#)
                                                   (if (if #{tmp\ 903}#
                                                         (apply (lambda (#{_\ 904}#
                                                                         #{name\ 905}#
                                                                         #{val\ 906}#)
                                                                  (#{id?\ 131}#
                                                                    #{name\ 905}#))
                                                                #{tmp\ 903}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 907}#
                                                                     #{name\ 908}#
                                                                     #{val\ 909}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 908}#
                                                                #{val\ 909}#
                                                                #{w\ 860}#
                                                                #{s\ 861}#
                                                                #{mod\ 863}#))
                                                            #{tmp\ 903}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 902}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 902}#
                                                   '(any any any))))
                                              #{e\ 858}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 858}#
                                               #{w\ 860}#
                                               #{s\ 861}#
                                               #{mod\ 863}#))))))))))))))
                   (if (#{syntax-object?\ 115}# #{e\ 858}#)
                     (#{syntax-type\ 165}#
                       (#{syntax-object-expression\ 116}# #{e\ 858}#)
                       #{r\ 859}#
                       (#{join-wraps\ 150}#
                         #{w\ 860}#
                         (#{syntax-object-wrap\ 117}# #{e\ 858}#))
                       #{s\ 861}#
                       #{rib\ 862}#
                       (let ((#{t\ 910}# (#{syntax-object-module\ 118}#
                                           #{e\ 858}#)))
                         (if #{t\ 910}# #{t\ 910}# #{mod\ 863}#))
                       #{for-car?\ 864}#)
                     (if (self-evaluating? #{e\ 858}#)
                       (values
                         'constant
                         #f
                         #{e\ 858}#
                         #{w\ 860}#
                         #{s\ 861}#
                         #{mod\ 863}#)
                       (values
                         'other
                         #f
                         #{e\ 858}#
                         #{w\ 860}#
                         #{s\ 861}#
                         #{mod\ 863}#)))))))
           (#{chi-when-list\ 164}#
             (lambda (#{e\ 911}# #{when-list\ 912}# #{w\ 913}#)
               (letrec ((#{f\ 914}# (lambda (#{when-list\ 915}#
                                             #{situations\ 916}#)
                                      (if (null? #{when-list\ 915}#)
                                        #{situations\ 916}#
                                        (#{f\ 914}# (cdr #{when-list\ 915}#)
                                                    (cons (let ((#{x\ 917}# (car #{when-list\ 915}#)))
                                                            (if (#{free-id=?\ 154}#
                                                                  #{x\ 917}#
                                                                  '#(syntax-object
                                                                     compile
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
                                                              'compile
                                                              (if (#{free-id=?\ 154}#
                                                                    #{x\ 917}#
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
                                                                'load
                                                                (if (#{free-id=?\ 154}#
                                                                      #{x\ 917}#
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
                                                                  'eval
                                                                  (syntax-violation
                                                                    'eval-when
                                                                    "invalid situation"
                                                                    #{e\ 911}#
                                                                    (#{wrap\ 159}#
                                                                      #{x\ 917}#
                                                                      #{w\ 913}#
                                                                      #f))))))
                                                          #{situations\ 916}#))))))
                 (#{f\ 914}# #{when-list\ 912}# (quote ())))))
           (#{chi-install-global\ 163}#
             (lambda (#{name\ 918}# #{e\ 919}#)
               (#{build-global-definition\ 104}#
                 #f
                 #{name\ 918}#
                 (if (let ((#{v\ 920}# (module-variable
                                         (current-module)
                                         #{name\ 918}#)))
                       (if #{v\ 920}#
                         (if (variable-bound? #{v\ 920}#)
                           (if (macro? (variable-ref #{v\ 920}#))
                             (not (eq? (macro-type (variable-ref #{v\ 920}#))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (#{build-application\ 96}#
                     #f
                     (#{build-primref\ 108}#
                       #f
                       'make-extended-syncase-macro)
                     (list (#{build-application\ 96}#
                             #f
                             (#{build-primref\ 108}# #f (quote module-ref))
                             (list (#{build-application\ 96}#
                                     #f
                                     (#{build-primref\ 108}#
                                       #f
                                       'current-module)
                                     '())
                                   (#{build-data\ 109}# #f #{name\ 918}#)))
                           (#{build-data\ 109}# #f (quote macro))
                           #{e\ 919}#))
                   (#{build-application\ 96}#
                     #f
                     (#{build-primref\ 108}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 109}# #f (quote macro))
                           #{e\ 919}#))))))
           (#{chi-top-sequence\ 162}#
             (lambda (#{body\ 921}#
                      #{r\ 922}#
                      #{w\ 923}#
                      #{s\ 924}#
                      #{m\ 925}#
                      #{esew\ 926}#
                      #{mod\ 927}#)
               (#{build-sequence\ 110}#
                 #{s\ 924}#
                 (letrec ((#{dobody\ 928}#
                            (lambda (#{body\ 929}#
                                     #{r\ 930}#
                                     #{w\ 931}#
                                     #{m\ 932}#
                                     #{esew\ 933}#
                                     #{mod\ 934}#)
                              (if (null? #{body\ 929}#)
                                '()
                                (let ((#{first\ 935}#
                                        (#{chi-top\ 166}#
                                          (car #{body\ 929}#)
                                          #{r\ 930}#
                                          #{w\ 931}#
                                          #{m\ 932}#
                                          #{esew\ 933}#
                                          #{mod\ 934}#)))
                                  (cons #{first\ 935}#
                                        (#{dobody\ 928}#
                                          (cdr #{body\ 929}#)
                                          #{r\ 930}#
                                          #{w\ 931}#
                                          #{m\ 932}#
                                          #{esew\ 933}#
                                          #{mod\ 934}#)))))))
                   (#{dobody\ 928}#
                     #{body\ 921}#
                     #{r\ 922}#
                     #{w\ 923}#
                     #{m\ 925}#
                     #{esew\ 926}#
                     #{mod\ 927}#)))))
           (#{chi-sequence\ 161}#
             (lambda (#{body\ 936}#
                      #{r\ 937}#
                      #{w\ 938}#
                      #{s\ 939}#
                      #{mod\ 940}#)
               (#{build-sequence\ 110}#
                 #{s\ 939}#
                 (letrec ((#{dobody\ 941}#
                            (lambda (#{body\ 942}#
                                     #{r\ 943}#
                                     #{w\ 944}#
                                     #{mod\ 945}#)
                              (if (null? #{body\ 942}#)
                                '()
                                (let ((#{first\ 946}#
                                        (#{chi\ 167}#
                                          (car #{body\ 942}#)
                                          #{r\ 943}#
                                          #{w\ 944}#
                                          #{mod\ 945}#)))
                                  (cons #{first\ 946}#
                                        (#{dobody\ 941}#
                                          (cdr #{body\ 942}#)
                                          #{r\ 943}#
                                          #{w\ 944}#
                                          #{mod\ 945}#)))))))
                   (#{dobody\ 941}#
                     #{body\ 936}#
                     #{r\ 937}#
                     #{w\ 938}#
                     #{mod\ 940}#)))))
           (#{source-wrap\ 160}#
             (lambda (#{x\ 947}#
                      #{w\ 948}#
                      #{s\ 949}#
                      #{defmod\ 950}#)
               (#{wrap\ 159}#
                 (#{decorate-source\ 94}# #{x\ 947}# #{s\ 949}#)
                 #{w\ 948}#
                 #{defmod\ 950}#)))
           (#{wrap\ 159}#
             (lambda (#{x\ 951}# #{w\ 952}# #{defmod\ 953}#)
               (if (if (null? (#{wrap-marks\ 134}# #{w\ 952}#))
                     (null? (#{wrap-subst\ 135}# #{w\ 952}#))
                     #f)
                 #{x\ 951}#
                 (if (#{syntax-object?\ 115}# #{x\ 951}#)
                   (#{make-syntax-object\ 114}#
                     (#{syntax-object-expression\ 116}# #{x\ 951}#)
                     (#{join-wraps\ 150}#
                       #{w\ 952}#
                       (#{syntax-object-wrap\ 117}# #{x\ 951}#))
                     (#{syntax-object-module\ 118}# #{x\ 951}#))
                   (if (null? #{x\ 951}#)
                     #{x\ 951}#
                     (#{make-syntax-object\ 114}#
                       #{x\ 951}#
                       #{w\ 952}#
                       #{defmod\ 953}#))))))
           (#{bound-id-member?\ 158}#
             (lambda (#{x\ 954}# #{list\ 955}#)
               (if (not (null? #{list\ 955}#))
                 (let ((#{t\ 956}# (#{bound-id=?\ 155}#
                                     #{x\ 954}#
                                     (car #{list\ 955}#))))
                   (if #{t\ 956}#
                     #{t\ 956}#
                     (#{bound-id-member?\ 158}#
                       #{x\ 954}#
                       (cdr #{list\ 955}#))))
                 #f)))
           (#{distinct-bound-ids?\ 157}#
             (lambda (#{ids\ 957}#)
               (letrec ((#{distinct?\ 958}#
                          (lambda (#{ids\ 959}#)
                            (let ((#{t\ 960}# (null? #{ids\ 959}#)))
                              (if #{t\ 960}#
                                #{t\ 960}#
                                (if (not (#{bound-id-member?\ 158}#
                                           (car #{ids\ 959}#)
                                           (cdr #{ids\ 959}#)))
                                  (#{distinct?\ 958}# (cdr #{ids\ 959}#))
                                  #f))))))
                 (#{distinct?\ 958}# #{ids\ 957}#))))
           (#{valid-bound-ids?\ 156}#
             (lambda (#{ids\ 961}#)
               (if (letrec ((#{all-ids?\ 962}#
                              (lambda (#{ids\ 963}#)
                                (let ((#{t\ 964}# (null? #{ids\ 963}#)))
                                  (if #{t\ 964}#
                                    #{t\ 964}#
                                    (if (#{id?\ 131}# (car #{ids\ 963}#))
                                      (#{all-ids?\ 962}# (cdr #{ids\ 963}#))
                                      #f))))))
                     (#{all-ids?\ 962}# #{ids\ 961}#))
                 (#{distinct-bound-ids?\ 157}# #{ids\ 961}#)
                 #f)))
           (#{bound-id=?\ 155}#
             (lambda (#{i\ 965}# #{j\ 966}#)
               (if (if (#{syntax-object?\ 115}# #{i\ 965}#)
                     (#{syntax-object?\ 115}# #{j\ 966}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 116}# #{i\ 965}#)
                          (#{syntax-object-expression\ 116}# #{j\ 966}#))
                   (#{same-marks?\ 152}#
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{i\ 965}#))
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{j\ 966}#)))
                   #f)
                 (eq? #{i\ 965}# #{j\ 966}#))))
           (#{free-id=?\ 154}#
             (lambda (#{i\ 967}# #{j\ 968}#)
               (if (eq? (let ((#{x\ 969}# #{i\ 967}#))
                          (if (#{syntax-object?\ 115}# #{x\ 969}#)
                            (#{syntax-object-expression\ 116}# #{x\ 969}#)
                            #{x\ 969}#))
                        (let ((#{x\ 970}# #{j\ 968}#))
                          (if (#{syntax-object?\ 115}# #{x\ 970}#)
                            (#{syntax-object-expression\ 116}# #{x\ 970}#)
                            #{x\ 970}#)))
                 (eq? (#{id-var-name\ 153}# #{i\ 967}# (quote (())))
                      (#{id-var-name\ 153}# #{j\ 968}# (quote (()))))
                 #f)))
           (#{id-var-name\ 153}#
             (lambda (#{id\ 971}# #{w\ 972}#)
               (letrec ((#{search-vector-rib\ 975}#
                          (lambda (#{sym\ 981}#
                                   #{subst\ 982}#
                                   #{marks\ 983}#
                                   #{symnames\ 984}#
                                   #{ribcage\ 985}#)
                            (let ((#{n\ 986}# (vector-length
                                                #{symnames\ 984}#)))
                              (letrec ((#{f\ 987}# (lambda (#{i\ 988}#)
                                                     (if (#{fx=\ 88}#
                                                           #{i\ 988}#
                                                           #{n\ 986}#)
                                                       (#{search\ 973}#
                                                         #{sym\ 981}#
                                                         (cdr #{subst\ 982}#)
                                                         #{marks\ 983}#)
                                                       (if (if (eq? (vector-ref
                                                                      #{symnames\ 984}#
                                                                      #{i\ 988}#)
                                                                    #{sym\ 981}#)
                                                             (#{same-marks?\ 152}#
                                                               #{marks\ 983}#
                                                               (vector-ref
                                                                 (#{ribcage-marks\ 141}#
                                                                   #{ribcage\ 985}#)
                                                                 #{i\ 988}#))
                                                             #f)
                                                         (values
                                                           (vector-ref
                                                             (#{ribcage-labels\ 142}#
                                                               #{ribcage\ 985}#)
                                                             #{i\ 988}#)
                                                           #{marks\ 983}#)
                                                         (#{f\ 987}# (#{fx+\ 86}#
                                                                       #{i\ 988}#
                                                                       1)))))))
                                (#{f\ 987}# 0)))))
                        (#{search-list-rib\ 974}#
                          (lambda (#{sym\ 989}#
                                   #{subst\ 990}#
                                   #{marks\ 991}#
                                   #{symnames\ 992}#
                                   #{ribcage\ 993}#)
                            (letrec ((#{f\ 994}# (lambda (#{symnames\ 995}#
                                                          #{i\ 996}#)
                                                   (if (null? #{symnames\ 995}#)
                                                     (#{search\ 973}#
                                                       #{sym\ 989}#
                                                       (cdr #{subst\ 990}#)
                                                       #{marks\ 991}#)
                                                     (if (if (eq? (car #{symnames\ 995}#)
                                                                  #{sym\ 989}#)
                                                           (#{same-marks?\ 152}#
                                                             #{marks\ 991}#
                                                             (list-ref
                                                               (#{ribcage-marks\ 141}#
                                                                 #{ribcage\ 993}#)
                                                               #{i\ 996}#))
                                                           #f)
                                                       (values
                                                         (list-ref
                                                           (#{ribcage-labels\ 142}#
                                                             #{ribcage\ 993}#)
                                                           #{i\ 996}#)
                                                         #{marks\ 991}#)
                                                       (#{f\ 994}# (cdr #{symnames\ 995}#)
                                                                   (#{fx+\ 86}#
                                                                     #{i\ 996}#
                                                                     1)))))))
                              (#{f\ 994}# #{symnames\ 992}# 0))))
                        (#{search\ 973}#
                          (lambda (#{sym\ 997}# #{subst\ 998}# #{marks\ 999}#)
                            (if (null? #{subst\ 998}#)
                              (values #f #{marks\ 999}#)
                              (let ((#{fst\ 1000}# (car #{subst\ 998}#)))
                                (if (eq? #{fst\ 1000}# (quote shift))
                                  (#{search\ 973}#
                                    #{sym\ 997}#
                                    (cdr #{subst\ 998}#)
                                    (cdr #{marks\ 999}#))
                                  (let ((#{symnames\ 1001}#
                                          (#{ribcage-symnames\ 140}#
                                            #{fst\ 1000}#)))
                                    (if (vector? #{symnames\ 1001}#)
                                      (#{search-vector-rib\ 975}#
                                        #{sym\ 997}#
                                        #{subst\ 998}#
                                        #{marks\ 999}#
                                        #{symnames\ 1001}#
                                        #{fst\ 1000}#)
                                      (#{search-list-rib\ 974}#
                                        #{sym\ 997}#
                                        #{subst\ 998}#
                                        #{marks\ 999}#
                                        #{symnames\ 1001}#
                                        #{fst\ 1000}#)))))))))
                 (if (symbol? #{id\ 971}#)
                   (let ((#{t\ 1002}#
                           (call-with-values
                             (lambda ()
                               (#{search\ 973}#
                                 #{id\ 971}#
                                 (#{wrap-subst\ 135}# #{w\ 972}#)
                                 (#{wrap-marks\ 134}# #{w\ 972}#)))
                             (lambda (#{x\ 1003}# . #{ignore\ 1004}#)
                               #{x\ 1003}#))))
                     (if #{t\ 1002}# #{t\ 1002}# #{id\ 971}#))
                   (if (#{syntax-object?\ 115}# #{id\ 971}#)
                     (let ((#{id\ 1005}#
                             (#{syntax-object-expression\ 116}# #{id\ 971}#))
                           (#{w1\ 1006}#
                             (#{syntax-object-wrap\ 117}# #{id\ 971}#)))
                       (let ((#{marks\ 1007}#
                               (#{join-marks\ 151}#
                                 (#{wrap-marks\ 134}# #{w\ 972}#)
                                 (#{wrap-marks\ 134}# #{w1\ 1006}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 973}#
                               #{id\ 1005}#
                               (#{wrap-subst\ 135}# #{w\ 972}#)
                               #{marks\ 1007}#))
                           (lambda (#{new-id\ 1008}# #{marks\ 1009}#)
                             (let ((#{t\ 1010}# #{new-id\ 1008}#))
                               (if #{t\ 1010}#
                                 #{t\ 1010}#
                                 (let ((#{t\ 1011}#
                                         (call-with-values
                                           (lambda ()
                                             (#{search\ 973}#
                                               #{id\ 1005}#
                                               (#{wrap-subst\ 135}#
                                                 #{w1\ 1006}#)
                                               #{marks\ 1009}#))
                                           (lambda (#{x\ 1012}#
                                                    .
                                                    #{ignore\ 1013}#)
                                             #{x\ 1012}#))))
                                   (if #{t\ 1011}#
                                     #{t\ 1011}#
                                     #{id\ 1005}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 971}#))))))
           (#{same-marks?\ 152}#
             (lambda (#{x\ 1014}# #{y\ 1015}#)
               (let ((#{t\ 1016}# (eq? #{x\ 1014}# #{y\ 1015}#)))
                 (if #{t\ 1016}#
                   #{t\ 1016}#
                   (if (not (null? #{x\ 1014}#))
                     (if (not (null? #{y\ 1015}#))
                       (if (eq? (car #{x\ 1014}#) (car #{y\ 1015}#))
                         (#{same-marks?\ 152}#
                           (cdr #{x\ 1014}#)
                           (cdr #{y\ 1015}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 151}#
             (lambda (#{m1\ 1017}# #{m2\ 1018}#)
               (#{smart-append\ 149}# #{m1\ 1017}# #{m2\ 1018}#)))
           (#{join-wraps\ 150}#
             (lambda (#{w1\ 1019}# #{w2\ 1020}#)
               (let ((#{m1\ 1021}# (#{wrap-marks\ 134}# #{w1\ 1019}#))
                     (#{s1\ 1022}# (#{wrap-subst\ 135}# #{w1\ 1019}#)))
                 (if (null? #{m1\ 1021}#)
                   (if (null? #{s1\ 1022}#)
                     #{w2\ 1020}#
                     (#{make-wrap\ 133}#
                       (#{wrap-marks\ 134}# #{w2\ 1020}#)
                       (#{smart-append\ 149}#
                         #{s1\ 1022}#
                         (#{wrap-subst\ 135}# #{w2\ 1020}#))))
                   (#{make-wrap\ 133}#
                     (#{smart-append\ 149}#
                       #{m1\ 1021}#
                       (#{wrap-marks\ 134}# #{w2\ 1020}#))
                     (#{smart-append\ 149}#
                       #{s1\ 1022}#
                       (#{wrap-subst\ 135}# #{w2\ 1020}#)))))))
           (#{smart-append\ 149}#
             (lambda (#{m1\ 1023}# #{m2\ 1024}#)
               (if (null? #{m2\ 1024}#)
                 #{m1\ 1023}#
                 (append #{m1\ 1023}# #{m2\ 1024}#))))
           (#{make-binding-wrap\ 148}#
             (lambda (#{ids\ 1025}# #{labels\ 1026}# #{w\ 1027}#)
               (if (null? #{ids\ 1025}#)
                 #{w\ 1027}#
                 (#{make-wrap\ 133}#
                   (#{wrap-marks\ 134}# #{w\ 1027}#)
                   (cons (let ((#{labelvec\ 1028}#
                                 (list->vector #{labels\ 1026}#)))
                           (let ((#{n\ 1029}#
                                   (vector-length #{labelvec\ 1028}#)))
                             (let ((#{symnamevec\ 1030}#
                                     (make-vector #{n\ 1029}#))
                                   (#{marksvec\ 1031}#
                                     (make-vector #{n\ 1029}#)))
                               (begin
                                 (letrec ((#{f\ 1032}#
                                            (lambda (#{ids\ 1033}# #{i\ 1034}#)
                                              (if (not (null? #{ids\ 1033}#))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{id-sym-name&marks\ 132}#
                                                      (car #{ids\ 1033}#)
                                                      #{w\ 1027}#))
                                                  (lambda (#{symname\ 1035}#
                                                           #{marks\ 1036}#)
                                                    (begin
                                                      (vector-set!
                                                        #{symnamevec\ 1030}#
                                                        #{i\ 1034}#
                                                        #{symname\ 1035}#)
                                                      (vector-set!
                                                        #{marksvec\ 1031}#
                                                        #{i\ 1034}#
                                                        #{marks\ 1036}#)
                                                      (#{f\ 1032}#
                                                        (cdr #{ids\ 1033}#)
                                                        (#{fx+\ 86}#
                                                          #{i\ 1034}#
                                                          1)))))))))
                                   (#{f\ 1032}# #{ids\ 1025}# 0))
                                 (#{make-ribcage\ 138}#
                                   #{symnamevec\ 1030}#
                                   #{marksvec\ 1031}#
                                   #{labelvec\ 1028}#)))))
                         (#{wrap-subst\ 135}# #{w\ 1027}#))))))
           (#{extend-ribcage!\ 147}#
             (lambda (#{ribcage\ 1037}# #{id\ 1038}# #{label\ 1039}#)
               (begin
                 (#{set-ribcage-symnames!\ 143}#
                   #{ribcage\ 1037}#
                   (cons (#{syntax-object-expression\ 116}# #{id\ 1038}#)
                         (#{ribcage-symnames\ 140}# #{ribcage\ 1037}#)))
                 (#{set-ribcage-marks!\ 144}#
                   #{ribcage\ 1037}#
                   (cons (#{wrap-marks\ 134}#
                           (#{syntax-object-wrap\ 117}# #{id\ 1038}#))
                         (#{ribcage-marks\ 141}# #{ribcage\ 1037}#)))
                 (#{set-ribcage-labels!\ 145}#
                   #{ribcage\ 1037}#
                   (cons #{label\ 1039}#
                         (#{ribcage-labels\ 142}# #{ribcage\ 1037}#))))))
           (#{anti-mark\ 146}#
             (lambda (#{w\ 1040}#)
               (#{make-wrap\ 133}#
                 (cons #f (#{wrap-marks\ 134}# #{w\ 1040}#))
                 (cons 'shift
                       (#{wrap-subst\ 135}# #{w\ 1040}#)))))
           (#{set-ribcage-labels!\ 145}#
             (lambda (#{x\ 1041}# #{update\ 1042}#)
               (vector-set! #{x\ 1041}# 3 #{update\ 1042}#)))
           (#{set-ribcage-marks!\ 144}#
             (lambda (#{x\ 1043}# #{update\ 1044}#)
               (vector-set! #{x\ 1043}# 2 #{update\ 1044}#)))
           (#{set-ribcage-symnames!\ 143}#
             (lambda (#{x\ 1045}# #{update\ 1046}#)
               (vector-set! #{x\ 1045}# 1 #{update\ 1046}#)))
           (#{ribcage-labels\ 142}#
             (lambda (#{x\ 1047}#) (vector-ref #{x\ 1047}# 3)))
           (#{ribcage-marks\ 141}#
             (lambda (#{x\ 1048}#) (vector-ref #{x\ 1048}# 2)))
           (#{ribcage-symnames\ 140}#
             (lambda (#{x\ 1049}#) (vector-ref #{x\ 1049}# 1)))
           (#{ribcage?\ 139}#
             (lambda (#{x\ 1050}#)
               (if (vector? #{x\ 1050}#)
                 (if (= (vector-length #{x\ 1050}#) 4)
                   (eq? (vector-ref #{x\ 1050}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 138}#
             (lambda (#{symnames\ 1051}#
                      #{marks\ 1052}#
                      #{labels\ 1053}#)
               (vector
                 'ribcage
                 #{symnames\ 1051}#
                 #{marks\ 1052}#
                 #{labels\ 1053}#)))
           (#{gen-labels\ 137}#
             (lambda (#{ls\ 1054}#)
               (if (null? #{ls\ 1054}#)
                 '()
                 (cons (#{gen-label\ 136}#)
                       (#{gen-labels\ 137}# (cdr #{ls\ 1054}#))))))
           (#{gen-label\ 136}# (lambda () (string #\i)))
           (#{wrap-subst\ 135}# cdr)
           (#{wrap-marks\ 134}# car)
           (#{make-wrap\ 133}# cons)
           (#{id-sym-name&marks\ 132}#
             (lambda (#{x\ 1055}# #{w\ 1056}#)
               (if (#{syntax-object?\ 115}# #{x\ 1055}#)
                 (values
                   (#{syntax-object-expression\ 116}# #{x\ 1055}#)
                   (#{join-marks\ 151}#
                     (#{wrap-marks\ 134}# #{w\ 1056}#)
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{x\ 1055}#))))
                 (values
                   #{x\ 1055}#
                   (#{wrap-marks\ 134}# #{w\ 1056}#)))))
           (#{id?\ 131}#
             (lambda (#{x\ 1057}#)
               (if (symbol? #{x\ 1057}#)
                 #t
                 (if (#{syntax-object?\ 115}# #{x\ 1057}#)
                   (symbol?
                     (#{syntax-object-expression\ 116}# #{x\ 1057}#))
                   #f))))
           (#{nonsymbol-id?\ 130}#
             (lambda (#{x\ 1058}#)
               (if (#{syntax-object?\ 115}# #{x\ 1058}#)
                 (symbol?
                   (#{syntax-object-expression\ 116}# #{x\ 1058}#))
                 #f)))
           (#{global-extend\ 129}#
             (lambda (#{type\ 1059}# #{sym\ 1060}# #{val\ 1061}#)
               (#{put-global-definition-hook\ 92}#
                 #{sym\ 1060}#
                 #{type\ 1059}#
                 #{val\ 1061}#)))
           (#{lookup\ 128}#
             (lambda (#{x\ 1062}# #{r\ 1063}# #{mod\ 1064}#)
               (let ((#{t\ 1065}# (assq #{x\ 1062}# #{r\ 1063}#)))
                 (if #{t\ 1065}#
                   (cdr #{t\ 1065}#)
                   (if (symbol? #{x\ 1062}#)
                     (let ((#{t\ 1066}#
                             (#{get-global-definition-hook\ 93}#
                               #{x\ 1062}#
                               #{mod\ 1064}#)))
                       (if #{t\ 1066}# #{t\ 1066}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 127}#
             (lambda (#{r\ 1067}#)
               (if (null? #{r\ 1067}#)
                 '()
                 (let ((#{a\ 1068}# (car #{r\ 1067}#)))
                   (if (eq? (cadr #{a\ 1068}#) (quote macro))
                     (cons #{a\ 1068}#
                           (#{macros-only-env\ 127}# (cdr #{r\ 1067}#)))
                     (#{macros-only-env\ 127}# (cdr #{r\ 1067}#)))))))
           (#{extend-var-env\ 126}#
             (lambda (#{labels\ 1069}# #{vars\ 1070}# #{r\ 1071}#)
               (if (null? #{labels\ 1069}#)
                 #{r\ 1071}#
                 (#{extend-var-env\ 126}#
                   (cdr #{labels\ 1069}#)
                   (cdr #{vars\ 1070}#)
                   (cons (cons (car #{labels\ 1069}#)
                               (cons (quote lexical) (car #{vars\ 1070}#)))
                         #{r\ 1071}#)))))
           (#{extend-env\ 125}#
             (lambda (#{labels\ 1072}# #{bindings\ 1073}# #{r\ 1074}#)
               (if (null? #{labels\ 1072}#)
                 #{r\ 1074}#
                 (#{extend-env\ 125}#
                   (cdr #{labels\ 1072}#)
                   (cdr #{bindings\ 1073}#)
                   (cons (cons (car #{labels\ 1072}#)
                               (car #{bindings\ 1073}#))
                         #{r\ 1074}#)))))
           (#{binding-value\ 124}# cdr)
           (#{binding-type\ 123}# car)
           (#{source-annotation\ 122}#
             (lambda (#{x\ 1075}#)
               (if (#{syntax-object?\ 115}# #{x\ 1075}#)
                 (#{source-annotation\ 122}#
                   (#{syntax-object-expression\ 116}# #{x\ 1075}#))
                 (if (pair? #{x\ 1075}#)
                   (let ((#{props\ 1076}# (source-properties #{x\ 1075}#)))
                     (if (pair? #{props\ 1076}#) #{props\ 1076}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 121}#
             (lambda (#{x\ 1077}# #{update\ 1078}#)
               (vector-set! #{x\ 1077}# 3 #{update\ 1078}#)))
           (#{set-syntax-object-wrap!\ 120}#
             (lambda (#{x\ 1079}# #{update\ 1080}#)
               (vector-set! #{x\ 1079}# 2 #{update\ 1080}#)))
           (#{set-syntax-object-expression!\ 119}#
             (lambda (#{x\ 1081}# #{update\ 1082}#)
               (vector-set! #{x\ 1081}# 1 #{update\ 1082}#)))
           (#{syntax-object-module\ 118}#
             (lambda (#{x\ 1083}#) (vector-ref #{x\ 1083}# 3)))
           (#{syntax-object-wrap\ 117}#
             (lambda (#{x\ 1084}#) (vector-ref #{x\ 1084}# 2)))
           (#{syntax-object-expression\ 116}#
             (lambda (#{x\ 1085}#) (vector-ref #{x\ 1085}# 1)))
           (#{syntax-object?\ 115}#
             (lambda (#{x\ 1086}#)
               (if (vector? #{x\ 1086}#)
                 (if (= (vector-length #{x\ 1086}#) 4)
                   (eq? (vector-ref #{x\ 1086}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 114}#
             (lambda (#{expression\ 1087}#
                      #{wrap\ 1088}#
                      #{module\ 1089}#)
               (vector
                 'syntax-object
                 #{expression\ 1087}#
                 #{wrap\ 1088}#
                 #{module\ 1089}#)))
           (#{build-letrec\ 113}#
             (lambda (#{src\ 1090}#
                      #{ids\ 1091}#
                      #{vars\ 1092}#
                      #{val-exps\ 1093}#
                      #{body-exp\ 1094}#)
               (if (null? #{vars\ 1092}#)
                 #{body-exp\ 1094}#
                 (let ((#{atom-key\ 1095}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1095}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 1091}#
                         #{val-exps\ 1093}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 1090}#
                        #{ids\ 1091}#
                        #{vars\ 1092}#
                        #{val-exps\ 1093}#
                        #{body-exp\ 1094}#))
                     (#{decorate-source\ 94}#
                       (list 'letrec
                             (map list #{vars\ 1092}# #{val-exps\ 1093}#)
                             #{body-exp\ 1094}#)
                       #{src\ 1090}#))))))
           (#{build-named-let\ 112}#
             (lambda (#{src\ 1096}#
                      #{ids\ 1097}#
                      #{vars\ 1098}#
                      #{val-exps\ 1099}#
                      #{body-exp\ 1100}#)
               (let ((#{f\ 1101}# (car #{vars\ 1098}#))
                     (#{f-name\ 1102}# (car #{ids\ 1097}#))
                     (#{vars\ 1103}# (cdr #{vars\ 1098}#))
                     (#{ids\ 1104}# (cdr #{ids\ 1097}#)))
                 (let ((#{atom-key\ 1105}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1105}# (quote (c)))
                     (let ((#{proc\ 1106}#
                             (#{build-simple-lambda\ 105}#
                               #{src\ 1096}#
                               #{ids\ 1104}#
                               #f
                               #{vars\ 1103}#
                               #f
                               #{body-exp\ 1100}#)))
                       (begin
                         (#{maybe-name-value!\ 103}#
                           #{f-name\ 1102}#
                           #{proc\ 1106}#)
                         (for-each
                           #{maybe-name-value!\ 103}#
                           #{ids\ 1104}#
                           #{val-exps\ 1099}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 1096}#
                          (list #{f-name\ 1102}#)
                          (list #{f\ 1101}#)
                          (list #{proc\ 1106}#)
                          (#{build-application\ 96}#
                            #{src\ 1096}#
                            (#{build-lexical-reference\ 98}#
                              'fun
                              #{src\ 1096}#
                              #{f-name\ 1102}#
                              #{f\ 1101}#)
                            #{val-exps\ 1099}#))))
                     (#{decorate-source\ 94}#
                       (list 'let
                             #{f\ 1101}#
                             (map list #{vars\ 1103}# #{val-exps\ 1099}#)
                             #{body-exp\ 1100}#)
                       #{src\ 1096}#))))))
           (#{build-let\ 111}#
             (lambda (#{src\ 1107}#
                      #{ids\ 1108}#
                      #{vars\ 1109}#
                      #{val-exps\ 1110}#
                      #{body-exp\ 1111}#)
               (if (null? #{vars\ 1109}#)
                 #{body-exp\ 1111}#
                 (let ((#{atom-key\ 1112}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1112}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 1108}#
                         #{val-exps\ 1110}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 1107}#
                        #{ids\ 1108}#
                        #{vars\ 1109}#
                        #{val-exps\ 1110}#
                        #{body-exp\ 1111}#))
                     (#{decorate-source\ 94}#
                       (list 'let
                             (map list #{vars\ 1109}# #{val-exps\ 1110}#)
                             #{body-exp\ 1111}#)
                       #{src\ 1107}#))))))
           (#{build-sequence\ 110}#
             (lambda (#{src\ 1113}# #{exps\ 1114}#)
               (if (null? (cdr #{exps\ 1114}#))
                 (car #{exps\ 1114}#)
                 (let ((#{atom-key\ 1115}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1115}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 1113}#
                      #{exps\ 1114}#)
                     (#{decorate-source\ 94}#
                       (cons (quote begin) #{exps\ 1114}#)
                       #{src\ 1113}#))))))
           (#{build-data\ 109}#
             (lambda (#{src\ 1116}# #{exp\ 1117}#)
               (let ((#{atom-key\ 1118}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1118}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 1116}#
                    #{exp\ 1117}#)
                   (#{decorate-source\ 94}#
                     (if (if (self-evaluating? #{exp\ 1117}#)
                           (not (vector? #{exp\ 1117}#))
                           #f)
                       #{exp\ 1117}#
                       (list (quote quote) #{exp\ 1117}#))
                     #{src\ 1116}#)))))
           (#{build-primref\ 108}#
             (lambda (#{src\ 1119}# #{name\ 1120}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 1121}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1121}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 1119}#
                      #{name\ 1120}#)
                     (#{decorate-source\ 94}#
                       #{name\ 1120}#
                       #{src\ 1119}#)))
                 (let ((#{atom-key\ 1122}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1122}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 1119}#
                      '(guile)
                      #{name\ 1120}#
                      #f)
                     (#{decorate-source\ 94}#
                       (list (quote @@) (quote (guile)) #{name\ 1120}#)
                       #{src\ 1119}#))))))
           (#{build-lambda-case\ 107}#
             (lambda (#{src\ 1123}#
                      #{req\ 1124}#
                      #{opt\ 1125}#
                      #{rest\ 1126}#
                      #{kw\ 1127}#
                      #{inits\ 1128}#
                      #{vars\ 1129}#
                      #{predicate\ 1130}#
                      #{body\ 1131}#
                      #{else-case\ 1132}#)
               (let ((#{atom-key\ 1133}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1133}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 1123}#
                    #{req\ 1124}#
                    #{opt\ 1125}#
                    #{rest\ 1126}#
                    #{kw\ 1127}#
                    #{inits\ 1128}#
                    #{vars\ 1129}#
                    #{predicate\ 1130}#
                    #{body\ 1131}#
                    #{else-case\ 1132}#)
                   (let ((#{nreq\ 1134}# (length #{req\ 1124}#)))
                     (let ((#{nopt\ 1135}#
                             (if #{opt\ 1125}# (length #{opt\ 1125}#) 0)))
                       (let ((#{rest-idx\ 1136}#
                               (if #{rest\ 1126}#
                                 (+ #{nreq\ 1134}# #{nopt\ 1135}#)
                                 #f)))
                         (let ((#{allow-other-keys?\ 1137}#
                                 (if #{kw\ 1127}# (car #{kw\ 1127}#) #f)))
                           (let ((#{kw-indices\ 1138}#
                                   (map (lambda (#{x\ 1139}#)
                                          (cons (car #{x\ 1139}#)
                                                (list-index
                                                  #{vars\ 1129}#
                                                  (caddr #{x\ 1139}#))))
                                        (if #{kw\ 1127}#
                                          (cdr #{kw\ 1127}#)
                                          '()))))
                             (let ((#{nargs\ 1140}#
                                     (apply max
                                            (+ #{nreq\ 1134}#
                                               #{nopt\ 1135}#
                                               (if #{rest\ 1126}# 1 0))
                                            (map 1+
                                                 (map cdr
                                                      #{kw-indices\ 1138}#)))))
                               (begin
                                 (let ((#{t\ 1141}#
                                         (= #{nargs\ 1140}#
                                            (length #{vars\ 1129}#)
                                            (+ #{nreq\ 1134}#
                                               (length #{inits\ 1128}#)
                                               (if #{rest\ 1126}# 1 0)))))
                                   (if #{t\ 1141}#
                                     #{t\ 1141}#
                                     (error "something went wrong"
                                            #{req\ 1124}#
                                            #{opt\ 1125}#
                                            #{rest\ 1126}#
                                            #{kw\ 1127}#
                                            #{inits\ 1128}#
                                            #{vars\ 1129}#
                                            #{nreq\ 1134}#
                                            #{nopt\ 1135}#
                                            #{kw-indices\ 1138}#
                                            #{nargs\ 1140}#)))
                                 (#{decorate-source\ 94}#
                                   (cons (list (cons '(@@ (ice-9 optargs)
                                                          parse-lambda-case)
                                                     (cons (list 'quote
                                                                 (list #{nreq\ 1134}#
                                                                       #{nopt\ 1135}#
                                                                       #{rest-idx\ 1136}#
                                                                       #{nargs\ 1140}#
                                                                       #{allow-other-keys?\ 1137}#
                                                                       #{kw-indices\ 1138}#))
                                                           (cons (cons 'list
                                                                       (map (lambda (#{i\ 1142}#)
                                                                              (list 'lambda
                                                                                    #{vars\ 1129}#
                                                                                    #{i\ 1142}#))
                                                                            #{inits\ 1128}#))
                                                                 (cons (if #{predicate\ 1130}#
                                                                         (list 'lambda
                                                                               #{vars\ 1129}#
                                                                               #{predicate\ 1130}#)
                                                                         #f)
                                                                       '(%%args)))))
                                               '=>
                                               (list 'lambda
                                                     '(%%%args . _)
                                                     (cons 'apply
                                                           (cons (list 'lambda
                                                                       #{vars\ 1129}#
                                                                       #{body\ 1131}#)
                                                                 '(%%%args)))))
                                         (let ((#{t\ 1143}#
                                                 #{else-case\ 1132}#))
                                           (if #{t\ 1143}#
                                             #{t\ 1143}#
                                             '((%%args
                                                 (error "wrong number of arguments"
                                                        %%args))))))
                                   #{src\ 1123}#))))))))))))
           (#{build-case-lambda\ 106}#
             (lambda (#{src\ 1144}#
                      #{docstring\ 1145}#
                      #{body\ 1146}#)
               (let ((#{atom-key\ 1147}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1147}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1144}#
                    (if #{docstring\ 1145}#
                      (list (cons (quote documentation) #{docstring\ 1145}#))
                      '())
                    #{body\ 1146}#)
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 1145}#
                                     (list #{docstring\ 1145}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 1146}#)))))
                     #{src\ 1144}#)))))
           (#{build-simple-lambda\ 105}#
             (lambda (#{src\ 1148}#
                      #{req\ 1149}#
                      #{rest\ 1150}#
                      #{vars\ 1151}#
                      #{docstring\ 1152}#
                      #{exp\ 1153}#)
               (let ((#{atom-key\ 1154}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1154}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1148}#
                    (if #{docstring\ 1152}#
                      (list (cons (quote documentation) #{docstring\ 1152}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 1148}#
                     #{req\ 1149}#
                     #f
                     #{rest\ 1150}#
                     #f
                     '()
                     #{vars\ 1151}#
                     #f
                     #{exp\ 1153}#
                     #f))
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons (if #{rest\ 1150}#
                                   (apply cons* #{vars\ 1151}#)
                                   #{vars\ 1151}#)
                                 (append
                                   (if #{docstring\ 1152}#
                                     (list #{docstring\ 1152}#)
                                     '())
                                   (list #{exp\ 1153}#))))
                     #{src\ 1148}#)))))
           (#{build-global-definition\ 104}#
             (lambda (#{source\ 1155}# #{var\ 1156}# #{exp\ 1157}#)
               (let ((#{atom-key\ 1158}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1158}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 103}#
                       #{var\ 1156}#
                       #{exp\ 1157}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 1155}#
                      #{var\ 1156}#
                      #{exp\ 1157}#))
                   (#{decorate-source\ 94}#
                     (list (quote define) #{var\ 1156}# #{exp\ 1157}#)
                     #{source\ 1155}#)))))
           (#{maybe-name-value!\ 103}#
             (lambda (#{name\ 1159}# #{val\ 1160}#)
               (if ((@ (language tree-il) lambda?) #{val\ 1160}#)
                 (let ((#{meta\ 1161}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 1160}#)))
                   (if (not (assq (quote name) #{meta\ 1161}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 1160}#
                      (acons 'name
                             #{name\ 1159}#
                             #{meta\ 1161}#)))))))
           (#{build-global-assignment\ 102}#
             (lambda (#{source\ 1162}#
                      #{var\ 1163}#
                      #{exp\ 1164}#
                      #{mod\ 1165}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 1165}#
                 #{var\ 1163}#
                 (lambda (#{mod\ 1166}# #{var\ 1167}# #{public?\ 1168}#)
                   (let ((#{atom-key\ 1169}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1169}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 1162}#
                        #{mod\ 1166}#
                        #{var\ 1167}#
                        #{public?\ 1168}#
                        #{exp\ 1164}#)
                       (#{decorate-source\ 94}#
                         (list 'set!
                               (list (if #{public?\ 1168}#
                                       '@
                                       '@@)
                                     #{mod\ 1166}#
                                     #{var\ 1167}#)
                               #{exp\ 1164}#)
                         #{source\ 1162}#))))
                 (lambda (#{var\ 1170}#)
                   (let ((#{atom-key\ 1171}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1171}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 1162}#
                        #{var\ 1170}#
                        #{exp\ 1164}#)
                       (#{decorate-source\ 94}#
                         (list (quote set!) #{var\ 1170}# #{exp\ 1164}#)
                         #{source\ 1162}#)))))))
           (#{build-global-reference\ 101}#
             (lambda (#{source\ 1172}# #{var\ 1173}# #{mod\ 1174}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 1174}#
                 #{var\ 1173}#
                 (lambda (#{mod\ 1175}# #{var\ 1176}# #{public?\ 1177}#)
                   (let ((#{atom-key\ 1178}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1178}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 1172}#
                        #{mod\ 1175}#
                        #{var\ 1176}#
                        #{public?\ 1177}#)
                       (#{decorate-source\ 94}#
                         (list (if #{public?\ 1177}# (quote @) (quote @@))
                               #{mod\ 1175}#
                               #{var\ 1176}#)
                         #{source\ 1172}#))))
                 (lambda (#{var\ 1179}#)
                   (let ((#{atom-key\ 1180}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1180}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 1172}#
                        #{var\ 1179}#)
                       (#{decorate-source\ 94}#
                         #{var\ 1179}#
                         #{source\ 1172}#)))))))
           (#{analyze-variable\ 100}#
             (lambda (#{mod\ 1181}#
                      #{var\ 1182}#
                      #{modref-cont\ 1183}#
                      #{bare-cont\ 1184}#)
               (if (not #{mod\ 1181}#)
                 (#{bare-cont\ 1184}# #{var\ 1182}#)
                 (let ((#{kind\ 1185}# (car #{mod\ 1181}#))
                       (#{mod\ 1186}# (cdr #{mod\ 1181}#)))
                   (if (memv #{kind\ 1185}# (quote (public)))
                     (#{modref-cont\ 1183}#
                       #{mod\ 1186}#
                       #{var\ 1182}#
                       #t)
                     (if (memv #{kind\ 1185}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 1186}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 1183}#
                           #{mod\ 1186}#
                           #{var\ 1182}#
                           #f)
                         (#{bare-cont\ 1184}# #{var\ 1182}#))
                       (if (memv #{kind\ 1185}# (quote (bare)))
                         (#{bare-cont\ 1184}# #{var\ 1182}#)
                         (if (memv #{kind\ 1185}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 1186}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 1186}#)
                                   #{var\ 1182}#)
                                 #f)
                             (#{modref-cont\ 1183}#
                               #{mod\ 1186}#
                               #{var\ 1182}#
                               #f)
                             (#{bare-cont\ 1184}# #{var\ 1182}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 1182}#
                             #{mod\ 1186}#)))))))))
           (#{build-lexical-assignment\ 99}#
             (lambda (#{source\ 1187}#
                      #{name\ 1188}#
                      #{var\ 1189}#
                      #{exp\ 1190}#)
               (let ((#{atom-key\ 1191}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1191}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 1187}#
                    #{name\ 1188}#
                    #{var\ 1189}#
                    #{exp\ 1190}#)
                   (#{decorate-source\ 94}#
                     (list (quote set!) #{var\ 1189}# #{exp\ 1190}#)
                     #{source\ 1187}#)))))
           (#{build-lexical-reference\ 98}#
             (lambda (#{type\ 1192}#
                      #{source\ 1193}#
                      #{name\ 1194}#
                      #{var\ 1195}#)
               (let ((#{atom-key\ 1196}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1196}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 1193}#
                    #{name\ 1194}#
                    #{var\ 1195}#)
                   (#{decorate-source\ 94}#
                     #{var\ 1195}#
                     #{source\ 1193}#)))))
           (#{build-conditional\ 97}#
             (lambda (#{source\ 1197}#
                      #{test-exp\ 1198}#
                      #{then-exp\ 1199}#
                      #{else-exp\ 1200}#)
               (let ((#{atom-key\ 1201}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1201}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 1197}#
                    #{test-exp\ 1198}#
                    #{then-exp\ 1199}#
                    #{else-exp\ 1200}#)
                   (#{decorate-source\ 94}#
                     (if (equal? #{else-exp\ 1200}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 1198}#
                             #{then-exp\ 1199}#)
                       (list 'if
                             #{test-exp\ 1198}#
                             #{then-exp\ 1199}#
                             #{else-exp\ 1200}#))
                     #{source\ 1197}#)))))
           (#{build-application\ 96}#
             (lambda (#{source\ 1202}#
                      #{fun-exp\ 1203}#
                      #{arg-exps\ 1204}#)
               (let ((#{atom-key\ 1205}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1205}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 1202}#
                    #{fun-exp\ 1203}#
                    #{arg-exps\ 1204}#)
                   (#{decorate-source\ 94}#
                     (cons #{fun-exp\ 1203}# #{arg-exps\ 1204}#)
                     #{source\ 1202}#)))))
           (#{build-void\ 95}#
             (lambda (#{source\ 1206}#)
               (let ((#{atom-key\ 1207}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1207}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 1206}#)
                   (#{decorate-source\ 94}#
                     '(if #f #f)
                     #{source\ 1206}#)))))
           (#{decorate-source\ 94}#
             (lambda (#{e\ 1208}# #{s\ 1209}#)
               (begin
                 (if (if (pair? #{e\ 1208}#) #{s\ 1209}# #f)
                   (set-source-properties! #{e\ 1208}# #{s\ 1209}#))
                 #{e\ 1208}#)))
           (#{get-global-definition-hook\ 93}#
             (lambda (#{symbol\ 1210}# #{module\ 1211}#)
               (begin
                 (if (if (not #{module\ 1211}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 1210}#))
                 (let ((#{v\ 1212}#
                         (module-variable
                           (if #{module\ 1211}#
                             (resolve-module (cdr #{module\ 1211}#))
                             (current-module))
                           #{symbol\ 1210}#)))
                   (if #{v\ 1212}#
                     (if (variable-bound? #{v\ 1212}#)
                       (let ((#{val\ 1213}# (variable-ref #{v\ 1212}#)))
                         (if (macro? #{val\ 1213}#)
                           (if (syncase-macro-type #{val\ 1213}#)
                             (cons (syncase-macro-type #{val\ 1213}#)
                                   (syncase-macro-binding #{val\ 1213}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 92}#
             (lambda (#{symbol\ 1214}# #{type\ 1215}# #{val\ 1216}#)
               (let ((#{existing\ 1217}#
                       (let ((#{v\ 1218}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 1214}#)))
                         (if #{v\ 1218}#
                           (if (variable-bound? #{v\ 1218}#)
                             (let ((#{val\ 1219}# (variable-ref #{v\ 1218}#)))
                               (if (macro? #{val\ 1219}#)
                                 (if (not (syncase-macro-type #{val\ 1219}#))
                                   #{val\ 1219}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 1214}#
                   (if #{existing\ 1217}#
                     (make-extended-syncase-macro
                       #{existing\ 1217}#
                       #{type\ 1215}#
                       #{val\ 1216}#)
                     (make-syncase-macro #{type\ 1215}# #{val\ 1216}#))))))
           (#{local-eval-hook\ 91}#
             (lambda (#{x\ 1220}# #{mod\ 1221}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 1222}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 1222}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1220}#)
                           #{x\ 1220}#))))))
           (#{top-level-eval-hook\ 90}#
             (lambda (#{x\ 1223}# #{mod\ 1224}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 1225}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 1225}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1223}#)
                           #{x\ 1223}#))))))
           (#{fx<\ 89}# <)
           (#{fx=\ 88}# =)
           (#{fx-\ 87}# -)
           (#{fx+\ 86}# +)
           (#{*mode*\ 85}# (make-fluid))
           (#{noexpand\ 84}# "noexpand"))
    (begin
      (#{global-extend\ 129}#
        'local-syntax
        'letrec-syntax
        #t)
      (#{global-extend\ 129}#
        'local-syntax
        'let-syntax
        #f)
      (#{global-extend\ 129}#
        'core
        'fluid-let-syntax
        (lambda (#{e\ 1226}#
                 #{r\ 1227}#
                 #{w\ 1228}#
                 #{s\ 1229}#
                 #{mod\ 1230}#)
          ((lambda (#{tmp\ 1231}#)
             ((lambda (#{tmp\ 1232}#)
                (if (if #{tmp\ 1232}#
                      (apply (lambda (#{_\ 1233}#
                                      #{var\ 1234}#
                                      #{val\ 1235}#
                                      #{e1\ 1236}#
                                      #{e2\ 1237}#)
                               (#{valid-bound-ids?\ 156}# #{var\ 1234}#))
                             #{tmp\ 1232}#)
                      #f)
                  (apply (lambda (#{_\ 1239}#
                                  #{var\ 1240}#
                                  #{val\ 1241}#
                                  #{e1\ 1242}#
                                  #{e2\ 1243}#)
                           (let ((#{names\ 1244}#
                                   (map (lambda (#{x\ 1245}#)
                                          (#{id-var-name\ 153}#
                                            #{x\ 1245}#
                                            #{w\ 1228}#))
                                        #{var\ 1240}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 1247}# #{n\ 1248}#)
                                   (let ((#{atom-key\ 1249}#
                                           (#{binding-type\ 123}#
                                             (#{lookup\ 128}#
                                               #{n\ 1248}#
                                               #{r\ 1227}#
                                               #{mod\ 1230}#))))
                                     (if (memv #{atom-key\ 1249}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 1226}#
                                         (#{source-wrap\ 160}#
                                           #{id\ 1247}#
                                           #{w\ 1228}#
                                           #{s\ 1229}#
                                           #{mod\ 1230}#)))))
                                 #{var\ 1240}#
                                 #{names\ 1244}#)
                               (#{chi-body\ 171}#
                                 (cons #{e1\ 1242}# #{e2\ 1243}#)
                                 (#{source-wrap\ 160}#
                                   #{e\ 1226}#
                                   #{w\ 1228}#
                                   #{s\ 1229}#
                                   #{mod\ 1230}#)
                                 (#{extend-env\ 125}#
                                   #{names\ 1244}#
                                   (let ((#{trans-r\ 1252}#
                                           (#{macros-only-env\ 127}#
                                             #{r\ 1227}#)))
                                     (map (lambda (#{x\ 1253}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 173}#
                                                    (#{chi\ 167}#
                                                      #{x\ 1253}#
                                                      #{trans-r\ 1252}#
                                                      #{w\ 1228}#
                                                      #{mod\ 1230}#)
                                                    #{mod\ 1230}#)))
                                          #{val\ 1241}#))
                                   #{r\ 1227}#)
                                 #{w\ 1228}#
                                 #{mod\ 1230}#))))
                         #{tmp\ 1232}#)
                  ((lambda (#{_\ 1255}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 1226}#
                         #{w\ 1228}#
                         #{s\ 1229}#
                         #{mod\ 1230}#)))
                   #{tmp\ 1231}#)))
              ($sc-dispatch
                #{tmp\ 1231}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1226}#)))
      (#{global-extend\ 129}#
        'core
        'quote
        (lambda (#{e\ 1256}#
                 #{r\ 1257}#
                 #{w\ 1258}#
                 #{s\ 1259}#
                 #{mod\ 1260}#)
          ((lambda (#{tmp\ 1261}#)
             ((lambda (#{tmp\ 1262}#)
                (if #{tmp\ 1262}#
                  (apply (lambda (#{_\ 1263}# #{e\ 1264}#)
                           (#{build-data\ 109}#
                             #{s\ 1259}#
                             (#{strip\ 180}# #{e\ 1264}# #{w\ 1258}#)))
                         #{tmp\ 1262}#)
                  ((lambda (#{_\ 1265}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 1256}#
                         #{w\ 1258}#
                         #{s\ 1259}#
                         #{mod\ 1260}#)))
                   #{tmp\ 1261}#)))
              ($sc-dispatch #{tmp\ 1261}# (quote (any any)))))
           #{e\ 1256}#)))
      (#{global-extend\ 129}#
        'core
        'syntax
        (letrec ((#{regen\ 1273}#
                   (lambda (#{x\ 1274}#)
                     (let ((#{atom-key\ 1275}# (car #{x\ 1274}#)))
                       (if (memv #{atom-key\ 1275}# (quote (ref)))
                         (#{build-lexical-reference\ 98}#
                           'value
                           #f
                           (cadr #{x\ 1274}#)
                           (cadr #{x\ 1274}#))
                         (if (memv #{atom-key\ 1275}# (quote (primitive)))
                           (#{build-primref\ 108}# #f (cadr #{x\ 1274}#))
                           (if (memv #{atom-key\ 1275}# (quote (quote)))
                             (#{build-data\ 109}# #f (cadr #{x\ 1274}#))
                             (if (memv #{atom-key\ 1275}# (quote (lambda)))
                               (if (list? (cadr #{x\ 1274}#))
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (cadr #{x\ 1274}#)
                                   #f
                                   (cadr #{x\ 1274}#)
                                   #f
                                   (#{regen\ 1273}# (caddr #{x\ 1274}#)))
                                 (error "how did we get here" #{x\ 1274}#))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-primref\ 108}# #f (car #{x\ 1274}#))
                                 (map #{regen\ 1273}#
                                      (cdr #{x\ 1274}#))))))))))
                 (#{gen-vector\ 1272}#
                   (lambda (#{x\ 1276}#)
                     (if (eq? (car #{x\ 1276}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 1276}#))
                       (if (eq? (car #{x\ 1276}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 1276}#)))
                         (list (quote list->vector) #{x\ 1276}#)))))
                 (#{gen-append\ 1271}#
                   (lambda (#{x\ 1277}# #{y\ 1278}#)
                     (if (equal? #{y\ 1278}# (quote (quote ())))
                       #{x\ 1277}#
                       (list (quote append) #{x\ 1277}# #{y\ 1278}#))))
                 (#{gen-cons\ 1270}#
                   (lambda (#{x\ 1279}# #{y\ 1280}#)
                     (let ((#{atom-key\ 1281}# (car #{y\ 1280}#)))
                       (if (memv #{atom-key\ 1281}# (quote (quote)))
                         (if (eq? (car #{x\ 1279}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 1279}#) (cadr #{y\ 1280}#)))
                           (if (eq? (cadr #{y\ 1280}#) (quote ()))
                             (list (quote list) #{x\ 1279}#)
                             (list (quote cons) #{x\ 1279}# #{y\ 1280}#)))
                         (if (memv #{atom-key\ 1281}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 1279}# (cdr #{y\ 1280}#)))
                           (list (quote cons) #{x\ 1279}# #{y\ 1280}#))))))
                 (#{gen-map\ 1269}#
                   (lambda (#{e\ 1282}# #{map-env\ 1283}#)
                     (let ((#{formals\ 1284}# (map cdr #{map-env\ 1283}#))
                           (#{actuals\ 1285}#
                             (map (lambda (#{x\ 1286}#)
                                    (list (quote ref) (car #{x\ 1286}#)))
                                  #{map-env\ 1283}#)))
                       (if (eq? (car #{e\ 1282}#) (quote ref))
                         (car #{actuals\ 1285}#)
                         (if (and-map
                               (lambda (#{x\ 1287}#)
                                 (if (eq? (car #{x\ 1287}#) (quote ref))
                                   (memq (cadr #{x\ 1287}#) #{formals\ 1284}#)
                                   #f))
                               (cdr #{e\ 1282}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 1282}#))
                                       (map (let ((#{r\ 1288}#
                                                    (map cons
                                                         #{formals\ 1284}#
                                                         #{actuals\ 1285}#)))
                                              (lambda (#{x\ 1289}#)
                                                (cdr (assq (cadr #{x\ 1289}#)
                                                           #{r\ 1288}#))))
                                            (cdr #{e\ 1282}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 1284}#
                                             #{e\ 1282}#)
                                       #{actuals\ 1285}#)))))))
                 (#{gen-mappend\ 1268}#
                   (lambda (#{e\ 1290}# #{map-env\ 1291}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 1269}# #{e\ 1290}# #{map-env\ 1291}#))))
                 (#{gen-ref\ 1267}#
                   (lambda (#{src\ 1292}#
                            #{var\ 1293}#
                            #{level\ 1294}#
                            #{maps\ 1295}#)
                     (if (#{fx=\ 88}# #{level\ 1294}# 0)
                       (values #{var\ 1293}# #{maps\ 1295}#)
                       (if (null? #{maps\ 1295}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 1292}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 1267}#
                               #{src\ 1292}#
                               #{var\ 1293}#
                               (#{fx-\ 87}# #{level\ 1294}# 1)
                               (cdr #{maps\ 1295}#)))
                           (lambda (#{outer-var\ 1296}# #{outer-maps\ 1297}#)
                             (let ((#{b\ 1298}#
                                     (assq #{outer-var\ 1296}#
                                           (car #{maps\ 1295}#))))
                               (if #{b\ 1298}#
                                 (values (cdr #{b\ 1298}#) #{maps\ 1295}#)
                                 (let ((#{inner-var\ 1299}#
                                         (#{gen-var\ 181}# (quote tmp))))
                                   (values
                                     #{inner-var\ 1299}#
                                     (cons (cons (cons #{outer-var\ 1296}#
                                                       #{inner-var\ 1299}#)
                                                 (car #{maps\ 1295}#))
                                           #{outer-maps\ 1297}#)))))))))))
                 (#{gen-syntax\ 1266}#
                   (lambda (#{src\ 1300}#
                            #{e\ 1301}#
                            #{r\ 1302}#
                            #{maps\ 1303}#
                            #{ellipsis?\ 1304}#
                            #{mod\ 1305}#)
                     (if (#{id?\ 131}# #{e\ 1301}#)
                       (let ((#{label\ 1306}#
                               (#{id-var-name\ 153}#
                                 #{e\ 1301}#
                                 '(()))))
                         (let ((#{b\ 1307}#
                                 (#{lookup\ 128}#
                                   #{label\ 1306}#
                                   #{r\ 1302}#
                                   #{mod\ 1305}#)))
                           (if (eq? (#{binding-type\ 123}# #{b\ 1307}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 1308}#
                                         (#{binding-value\ 124}# #{b\ 1307}#)))
                                   (#{gen-ref\ 1267}#
                                     #{src\ 1300}#
                                     (car #{var.lev\ 1308}#)
                                     (cdr #{var.lev\ 1308}#)
                                     #{maps\ 1303}#)))
                               (lambda (#{var\ 1309}# #{maps\ 1310}#)
                                 (values
                                   (list (quote ref) #{var\ 1309}#)
                                   #{maps\ 1310}#)))
                             (if (#{ellipsis?\ 1304}# #{e\ 1301}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 1300}#)
                               (values
                                 (list (quote quote) #{e\ 1301}#)
                                 #{maps\ 1303}#)))))
                       ((lambda (#{tmp\ 1311}#)
                          ((lambda (#{tmp\ 1312}#)
                             (if (if #{tmp\ 1312}#
                                   (apply (lambda (#{dots\ 1313}# #{e\ 1314}#)
                                            (#{ellipsis?\ 1304}#
                                              #{dots\ 1313}#))
                                          #{tmp\ 1312}#)
                                   #f)
                               (apply (lambda (#{dots\ 1315}# #{e\ 1316}#)
                                        (#{gen-syntax\ 1266}#
                                          #{src\ 1300}#
                                          #{e\ 1316}#
                                          #{r\ 1302}#
                                          #{maps\ 1303}#
                                          (lambda (#{x\ 1317}#) #f)
                                          #{mod\ 1305}#))
                                      #{tmp\ 1312}#)
                               ((lambda (#{tmp\ 1318}#)
                                  (if (if #{tmp\ 1318}#
                                        (apply (lambda (#{x\ 1319}#
                                                        #{dots\ 1320}#
                                                        #{y\ 1321}#)
                                                 (#{ellipsis?\ 1304}#
                                                   #{dots\ 1320}#))
                                               #{tmp\ 1318}#)
                                        #f)
                                    (apply (lambda (#{x\ 1322}#
                                                    #{dots\ 1323}#
                                                    #{y\ 1324}#)
                                             (letrec ((#{f\ 1325}#
                                                        (lambda (#{y\ 1326}#
                                                                 #{k\ 1327}#)
                                                          ((lambda (#{tmp\ 1331}#)
                                                             ((lambda (#{tmp\ 1332}#)
                                                                (if (if #{tmp\ 1332}#
                                                                      (apply (lambda (#{dots\ 1333}#
                                                                                      #{y\ 1334}#)
                                                                               (#{ellipsis?\ 1304}#
                                                                                 #{dots\ 1333}#))
                                                                             #{tmp\ 1332}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 1335}#
                                                                                  #{y\ 1336}#)
                                                                           (#{f\ 1325}#
                                                                             #{y\ 1336}#
                                                                             (lambda (#{maps\ 1337}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 1327}#
                                                                                     (cons '()
                                                                                           #{maps\ 1337}#)))
                                                                                 (lambda (#{x\ 1338}#
                                                                                          #{maps\ 1339}#)
                                                                                   (if (null? (car #{maps\ 1339}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 1300}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 1268}#
                                                                                         #{x\ 1338}#
                                                                                         (car #{maps\ 1339}#))
                                                                                       (cdr #{maps\ 1339}#))))))))
                                                                         #{tmp\ 1332}#)
                                                                  ((lambda (#{_\ 1340}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 1266}#
                                                                           #{src\ 1300}#
                                                                           #{y\ 1326}#
                                                                           #{r\ 1302}#
                                                                           #{maps\ 1303}#
                                                                           #{ellipsis?\ 1304}#
                                                                           #{mod\ 1305}#))
                                                                       (lambda (#{y\ 1341}#
                                                                                #{maps\ 1342}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 1327}#
                                                                               #{maps\ 1342}#))
                                                                           (lambda (#{x\ 1343}#
                                                                                    #{maps\ 1344}#)
                                                                             (values
                                                                               (#{gen-append\ 1271}#
                                                                                 #{x\ 1343}#
                                                                                 #{y\ 1341}#)
                                                                               #{maps\ 1344}#))))))
                                                                   #{tmp\ 1331}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 1331}#
                                                                '(any . any))))
                                                           #{y\ 1326}#))))
                                               (#{f\ 1325}#
                                                 #{y\ 1324}#
                                                 (lambda (#{maps\ 1328}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 1266}#
                                                         #{src\ 1300}#
                                                         #{x\ 1322}#
                                                         #{r\ 1302}#
                                                         (cons '()
                                                               #{maps\ 1328}#)
                                                         #{ellipsis?\ 1304}#
                                                         #{mod\ 1305}#))
                                                     (lambda (#{x\ 1329}#
                                                              #{maps\ 1330}#)
                                                       (if (null? (car #{maps\ 1330}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 1300}#)
                                                         (values
                                                           (#{gen-map\ 1269}#
                                                             #{x\ 1329}#
                                                             (car #{maps\ 1330}#))
                                                           (cdr #{maps\ 1330}#)))))))))
                                           #{tmp\ 1318}#)
                                    ((lambda (#{tmp\ 1345}#)
                                       (if #{tmp\ 1345}#
                                         (apply (lambda (#{x\ 1346}#
                                                         #{y\ 1347}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 1266}#
                                                        #{src\ 1300}#
                                                        #{x\ 1346}#
                                                        #{r\ 1302}#
                                                        #{maps\ 1303}#
                                                        #{ellipsis?\ 1304}#
                                                        #{mod\ 1305}#))
                                                    (lambda (#{x\ 1348}#
                                                             #{maps\ 1349}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 1266}#
                                                            #{src\ 1300}#
                                                            #{y\ 1347}#
                                                            #{r\ 1302}#
                                                            #{maps\ 1349}#
                                                            #{ellipsis?\ 1304}#
                                                            #{mod\ 1305}#))
                                                        (lambda (#{y\ 1350}#
                                                                 #{maps\ 1351}#)
                                                          (values
                                                            (#{gen-cons\ 1270}#
                                                              #{x\ 1348}#
                                                              #{y\ 1350}#)
                                                            #{maps\ 1351}#))))))
                                                #{tmp\ 1345}#)
                                         ((lambda (#{tmp\ 1352}#)
                                            (if #{tmp\ 1352}#
                                              (apply (lambda (#{e1\ 1353}#
                                                              #{e2\ 1354}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 1266}#
                                                             #{src\ 1300}#
                                                             (cons #{e1\ 1353}#
                                                                   #{e2\ 1354}#)
                                                             #{r\ 1302}#
                                                             #{maps\ 1303}#
                                                             #{ellipsis?\ 1304}#
                                                             #{mod\ 1305}#))
                                                         (lambda (#{e\ 1356}#
                                                                  #{maps\ 1357}#)
                                                           (values
                                                             (#{gen-vector\ 1272}#
                                                               #{e\ 1356}#)
                                                             #{maps\ 1357}#))))
                                                     #{tmp\ 1352}#)
                                              ((lambda (#{_\ 1358}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 1301}#)
                                                   #{maps\ 1303}#))
                                               #{tmp\ 1311}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1311}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 1311}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 1311}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 1311}# (quote (any any)))))
                        #{e\ 1301}#)))))
          (lambda (#{e\ 1359}#
                   #{r\ 1360}#
                   #{w\ 1361}#
                   #{s\ 1362}#
                   #{mod\ 1363}#)
            (let ((#{e\ 1364}#
                    (#{source-wrap\ 160}#
                      #{e\ 1359}#
                      #{w\ 1361}#
                      #{s\ 1362}#
                      #{mod\ 1363}#)))
              ((lambda (#{tmp\ 1365}#)
                 ((lambda (#{tmp\ 1366}#)
                    (if #{tmp\ 1366}#
                      (apply (lambda (#{_\ 1367}# #{x\ 1368}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 1266}#
                                     #{e\ 1364}#
                                     #{x\ 1368}#
                                     #{r\ 1360}#
                                     '()
                                     #{ellipsis?\ 175}#
                                     #{mod\ 1363}#))
                                 (lambda (#{e\ 1369}# #{maps\ 1370}#)
                                   (#{regen\ 1273}# #{e\ 1369}#))))
                             #{tmp\ 1366}#)
                      ((lambda (#{_\ 1371}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 1364}#))
                       #{tmp\ 1365}#)))
                  ($sc-dispatch #{tmp\ 1365}# (quote (any any)))))
               #{e\ 1364}#)))))
      (#{global-extend\ 129}#
        'core
        'lambda
        (lambda (#{e\ 1372}#
                 #{r\ 1373}#
                 #{w\ 1374}#
                 #{s\ 1375}#
                 #{mod\ 1376}#)
          ((lambda (#{tmp\ 1377}#)
             ((lambda (#{tmp\ 1378}#)
                (if (if #{tmp\ 1378}#
                      (apply (lambda (#{_\ 1379}#
                                      #{args\ 1380}#
                                      #{docstring\ 1381}#
                                      #{e1\ 1382}#
                                      #{e2\ 1383}#)
                               (string? (syntax->datum #{docstring\ 1381}#)))
                             #{tmp\ 1378}#)
                      #f)
                  (apply (lambda (#{_\ 1384}#
                                  #{args\ 1385}#
                                  #{docstring\ 1386}#
                                  #{e1\ 1387}#
                                  #{e2\ 1388}#)
                           (call-with-values
                             (lambda ()
                               (#{lambda-formals\ 176}# #{args\ 1385}#))
                             (lambda (#{req\ 1389}#
                                      #{opt\ 1390}#
                                      #{rest\ 1391}#
                                      #{kw\ 1392}#
                                      #{pred\ 1393}#)
                               (#{chi-simple-lambda\ 177}#
                                 #{e\ 1372}#
                                 #{r\ 1373}#
                                 #{w\ 1374}#
                                 #{s\ 1375}#
                                 #{mod\ 1376}#
                                 #{req\ 1389}#
                                 #{rest\ 1391}#
                                 (syntax->datum #{docstring\ 1386}#)
                                 (cons #{e1\ 1387}# #{e2\ 1388}#)))))
                         #{tmp\ 1378}#)
                  ((lambda (#{tmp\ 1395}#)
                     (if #{tmp\ 1395}#
                       (apply (lambda (#{_\ 1396}#
                                       #{args\ 1397}#
                                       #{e1\ 1398}#
                                       #{e2\ 1399}#)
                                (call-with-values
                                  (lambda ()
                                    (#{lambda-formals\ 176}# #{args\ 1397}#))
                                  (lambda (#{req\ 1400}#
                                           #{opt\ 1401}#
                                           #{rest\ 1402}#
                                           #{kw\ 1403}#
                                           #{pred\ 1404}#)
                                    (#{chi-simple-lambda\ 177}#
                                      #{e\ 1372}#
                                      #{r\ 1373}#
                                      #{w\ 1374}#
                                      #{s\ 1375}#
                                      #{mod\ 1376}#
                                      #{req\ 1400}#
                                      #{rest\ 1402}#
                                      #f
                                      (cons #{e1\ 1398}# #{e2\ 1399}#)))))
                              #{tmp\ 1395}#)
                       ((lambda (#{_\ 1406}#)
                          (syntax-violation
                            'lambda
                            "bad lambda"
                            #{e\ 1372}#))
                        #{tmp\ 1377}#)))
                   ($sc-dispatch
                     #{tmp\ 1377}#
                     '(any any any . each-any)))))
              ($sc-dispatch
                #{tmp\ 1377}#
                '(any any any any . each-any))))
           #{e\ 1372}#)))
      (#{global-extend\ 129}#
        'core
        'lambda*
        (lambda (#{e\ 1407}#
                 #{r\ 1408}#
                 #{w\ 1409}#
                 #{s\ 1410}#
                 #{mod\ 1411}#)
          ((lambda (#{tmp\ 1412}#)
             ((lambda (#{tmp\ 1413}#)
                (if #{tmp\ 1413}#
                  (apply (lambda (#{_\ 1414}#
                                  #{args\ 1415}#
                                  #{e1\ 1416}#
                                  #{e2\ 1417}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1407}#
                                 #{r\ 1408}#
                                 #{w\ 1409}#
                                 #{s\ 1410}#
                                 #{mod\ 1411}#
                                 #{lambda*-formals\ 178}#
                                 (list (cons #{args\ 1415}#
                                             (cons #{e1\ 1416}#
                                                   #{e2\ 1417}#)))))
                             (lambda (#{docstring\ 1419}# #{lcase\ 1420}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1410}#
                                 #{docstring\ 1419}#
                                 #{lcase\ 1420}#))))
                         #{tmp\ 1413}#)
                  ((lambda (#{_\ 1421}#)
                     (syntax-violation
                       'lambda
                       "bad lambda*"
                       #{e\ 1407}#))
                   #{tmp\ 1412}#)))
              ($sc-dispatch
                #{tmp\ 1412}#
                '(any any any . each-any))))
           #{e\ 1407}#)))
      (#{global-extend\ 129}#
        'core
        'case-lambda
        (lambda (#{e\ 1422}#
                 #{r\ 1423}#
                 #{w\ 1424}#
                 #{s\ 1425}#
                 #{mod\ 1426}#)
          ((lambda (#{tmp\ 1427}#)
             ((lambda (#{tmp\ 1428}#)
                (if #{tmp\ 1428}#
                  (apply (lambda (#{_\ 1429}#
                                  #{args\ 1430}#
                                  #{e1\ 1431}#
                                  #{e2\ 1432}#
                                  #{args*\ 1433}#
                                  #{e1*\ 1434}#
                                  #{e2*\ 1435}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1422}#
                                 #{r\ 1423}#
                                 #{w\ 1424}#
                                 #{s\ 1425}#
                                 #{mod\ 1426}#
                                 #{lambda-formals\ 176}#
                                 (cons (cons #{args\ 1430}#
                                             (cons #{e1\ 1431}# #{e2\ 1432}#))
                                       (map (lambda (#{tmp\ 1439}#
                                                     #{tmp\ 1438}#
                                                     #{tmp\ 1437}#)
                                              (cons #{tmp\ 1437}#
                                                    (cons #{tmp\ 1438}#
                                                          #{tmp\ 1439}#)))
                                            #{e2*\ 1435}#
                                            #{e1*\ 1434}#
                                            #{args*\ 1433}#))))
                             (lambda (#{docstring\ 1441}# #{lcase\ 1442}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1425}#
                                 #{docstring\ 1441}#
                                 #{lcase\ 1442}#))))
                         #{tmp\ 1428}#)
                  ((lambda (#{_\ 1443}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda"
                       #{e\ 1422}#))
                   #{tmp\ 1427}#)))
              ($sc-dispatch
                #{tmp\ 1427}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 1422}#)))
      (#{global-extend\ 129}#
        'core
        'case-lambda*
        (lambda (#{e\ 1444}#
                 #{r\ 1445}#
                 #{w\ 1446}#
                 #{s\ 1447}#
                 #{mod\ 1448}#)
          ((lambda (#{tmp\ 1449}#)
             ((lambda (#{tmp\ 1450}#)
                (if #{tmp\ 1450}#
                  (apply (lambda (#{_\ 1451}#
                                  #{args\ 1452}#
                                  #{e1\ 1453}#
                                  #{e2\ 1454}#
                                  #{args*\ 1455}#
                                  #{e1*\ 1456}#
                                  #{e2*\ 1457}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1444}#
                                 #{r\ 1445}#
                                 #{w\ 1446}#
                                 #{s\ 1447}#
                                 #{mod\ 1448}#
                                 #{lambda*-formals\ 178}#
                                 (cons (cons #{args\ 1452}#
                                             (cons #{e1\ 1453}# #{e2\ 1454}#))
                                       (map (lambda (#{tmp\ 1461}#
                                                     #{tmp\ 1460}#
                                                     #{tmp\ 1459}#)
                                              (cons #{tmp\ 1459}#
                                                    (cons #{tmp\ 1460}#
                                                          #{tmp\ 1461}#)))
                                            #{e2*\ 1457}#
                                            #{e1*\ 1456}#
                                            #{args*\ 1455}#))))
                             (lambda (#{docstring\ 1463}# #{lcase\ 1464}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1447}#
                                 #{docstring\ 1463}#
                                 #{lcase\ 1464}#))))
                         #{tmp\ 1450}#)
                  ((lambda (#{_\ 1465}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda*"
                       #{e\ 1444}#))
                   #{tmp\ 1449}#)))
              ($sc-dispatch
                #{tmp\ 1449}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 1444}#)))
      (#{global-extend\ 129}#
        'core
        'let
        (letrec ((#{chi-let\ 1466}#
                   (lambda (#{e\ 1467}#
                            #{r\ 1468}#
                            #{w\ 1469}#
                            #{s\ 1470}#
                            #{mod\ 1471}#
                            #{constructor\ 1472}#
                            #{ids\ 1473}#
                            #{vals\ 1474}#
                            #{exps\ 1475}#)
                     (if (not (#{valid-bound-ids?\ 156}# #{ids\ 1473}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 1467}#)
                       (let ((#{labels\ 1476}#
                               (#{gen-labels\ 137}# #{ids\ 1473}#))
                             (#{new-vars\ 1477}#
                               (map #{gen-var\ 181}# #{ids\ 1473}#)))
                         (let ((#{nw\ 1478}#
                                 (#{make-binding-wrap\ 148}#
                                   #{ids\ 1473}#
                                   #{labels\ 1476}#
                                   #{w\ 1469}#))
                               (#{nr\ 1479}#
                                 (#{extend-var-env\ 126}#
                                   #{labels\ 1476}#
                                   #{new-vars\ 1477}#
                                   #{r\ 1468}#)))
                           (#{constructor\ 1472}#
                             #{s\ 1470}#
                             (map syntax->datum #{ids\ 1473}#)
                             #{new-vars\ 1477}#
                             (map (lambda (#{x\ 1480}#)
                                    (#{chi\ 167}#
                                      #{x\ 1480}#
                                      #{r\ 1468}#
                                      #{w\ 1469}#
                                      #{mod\ 1471}#))
                                  #{vals\ 1474}#)
                             (#{chi-body\ 171}#
                               #{exps\ 1475}#
                               (#{source-wrap\ 160}#
                                 #{e\ 1467}#
                                 #{nw\ 1478}#
                                 #{s\ 1470}#
                                 #{mod\ 1471}#)
                               #{nr\ 1479}#
                               #{nw\ 1478}#
                               #{mod\ 1471}#))))))))
          (lambda (#{e\ 1481}#
                   #{r\ 1482}#
                   #{w\ 1483}#
                   #{s\ 1484}#
                   #{mod\ 1485}#)
            ((lambda (#{tmp\ 1486}#)
               ((lambda (#{tmp\ 1487}#)
                  (if (if #{tmp\ 1487}#
                        (apply (lambda (#{_\ 1488}#
                                        #{id\ 1489}#
                                        #{val\ 1490}#
                                        #{e1\ 1491}#
                                        #{e2\ 1492}#)
                                 (and-map #{id?\ 131}# #{id\ 1489}#))
                               #{tmp\ 1487}#)
                        #f)
                    (apply (lambda (#{_\ 1494}#
                                    #{id\ 1495}#
                                    #{val\ 1496}#
                                    #{e1\ 1497}#
                                    #{e2\ 1498}#)
                             (#{chi-let\ 1466}#
                               #{e\ 1481}#
                               #{r\ 1482}#
                               #{w\ 1483}#
                               #{s\ 1484}#
                               #{mod\ 1485}#
                               #{build-let\ 111}#
                               #{id\ 1495}#
                               #{val\ 1496}#
                               (cons #{e1\ 1497}# #{e2\ 1498}#)))
                           #{tmp\ 1487}#)
                    ((lambda (#{tmp\ 1502}#)
                       (if (if #{tmp\ 1502}#
                             (apply (lambda (#{_\ 1503}#
                                             #{f\ 1504}#
                                             #{id\ 1505}#
                                             #{val\ 1506}#
                                             #{e1\ 1507}#
                                             #{e2\ 1508}#)
                                      (if (#{id?\ 131}# #{f\ 1504}#)
                                        (and-map #{id?\ 131}# #{id\ 1505}#)
                                        #f))
                                    #{tmp\ 1502}#)
                             #f)
                         (apply (lambda (#{_\ 1510}#
                                         #{f\ 1511}#
                                         #{id\ 1512}#
                                         #{val\ 1513}#
                                         #{e1\ 1514}#
                                         #{e2\ 1515}#)
                                  (#{chi-let\ 1466}#
                                    #{e\ 1481}#
                                    #{r\ 1482}#
                                    #{w\ 1483}#
                                    #{s\ 1484}#
                                    #{mod\ 1485}#
                                    #{build-named-let\ 112}#
                                    (cons #{f\ 1511}# #{id\ 1512}#)
                                    #{val\ 1513}#
                                    (cons #{e1\ 1514}# #{e2\ 1515}#)))
                                #{tmp\ 1502}#)
                         ((lambda (#{_\ 1519}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 160}#
                                #{e\ 1481}#
                                #{w\ 1483}#
                                #{s\ 1484}#
                                #{mod\ 1485}#)))
                          #{tmp\ 1486}#)))
                     ($sc-dispatch
                       #{tmp\ 1486}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1486}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 1481}#))))
      (#{global-extend\ 129}#
        'core
        'letrec
        (lambda (#{e\ 1520}#
                 #{r\ 1521}#
                 #{w\ 1522}#
                 #{s\ 1523}#
                 #{mod\ 1524}#)
          ((lambda (#{tmp\ 1525}#)
             ((lambda (#{tmp\ 1526}#)
                (if (if #{tmp\ 1526}#
                      (apply (lambda (#{_\ 1527}#
                                      #{id\ 1528}#
                                      #{val\ 1529}#
                                      #{e1\ 1530}#
                                      #{e2\ 1531}#)
                               (and-map #{id?\ 131}# #{id\ 1528}#))
                             #{tmp\ 1526}#)
                      #f)
                  (apply (lambda (#{_\ 1533}#
                                  #{id\ 1534}#
                                  #{val\ 1535}#
                                  #{e1\ 1536}#
                                  #{e2\ 1537}#)
                           (let ((#{ids\ 1538}# #{id\ 1534}#))
                             (if (not (#{valid-bound-ids?\ 156}#
                                        #{ids\ 1538}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 1520}#)
                               (let ((#{labels\ 1540}#
                                       (#{gen-labels\ 137}# #{ids\ 1538}#))
                                     (#{new-vars\ 1541}#
                                       (map #{gen-var\ 181}# #{ids\ 1538}#)))
                                 (let ((#{w\ 1542}#
                                         (#{make-binding-wrap\ 148}#
                                           #{ids\ 1538}#
                                           #{labels\ 1540}#
                                           #{w\ 1522}#))
                                       (#{r\ 1543}#
                                         (#{extend-var-env\ 126}#
                                           #{labels\ 1540}#
                                           #{new-vars\ 1541}#
                                           #{r\ 1521}#)))
                                   (#{build-letrec\ 113}#
                                     #{s\ 1523}#
                                     (map syntax->datum #{ids\ 1538}#)
                                     #{new-vars\ 1541}#
                                     (map (lambda (#{x\ 1544}#)
                                            (#{chi\ 167}#
                                              #{x\ 1544}#
                                              #{r\ 1543}#
                                              #{w\ 1542}#
                                              #{mod\ 1524}#))
                                          #{val\ 1535}#)
                                     (#{chi-body\ 171}#
                                       (cons #{e1\ 1536}# #{e2\ 1537}#)
                                       (#{source-wrap\ 160}#
                                         #{e\ 1520}#
                                         #{w\ 1542}#
                                         #{s\ 1523}#
                                         #{mod\ 1524}#)
                                       #{r\ 1543}#
                                       #{w\ 1542}#
                                       #{mod\ 1524}#)))))))
                         #{tmp\ 1526}#)
                  ((lambda (#{_\ 1547}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 160}#
                         #{e\ 1520}#
                         #{w\ 1522}#
                         #{s\ 1523}#
                         #{mod\ 1524}#)))
                   #{tmp\ 1525}#)))
              ($sc-dispatch
                #{tmp\ 1525}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1520}#)))
      (#{global-extend\ 129}#
        'core
        'set!
        (lambda (#{e\ 1548}#
                 #{r\ 1549}#
                 #{w\ 1550}#
                 #{s\ 1551}#
                 #{mod\ 1552}#)
          ((lambda (#{tmp\ 1553}#)
             ((lambda (#{tmp\ 1554}#)
                (if (if #{tmp\ 1554}#
                      (apply (lambda (#{_\ 1555}# #{id\ 1556}# #{val\ 1557}#)
                               (#{id?\ 131}# #{id\ 1556}#))
                             #{tmp\ 1554}#)
                      #f)
                  (apply (lambda (#{_\ 1558}# #{id\ 1559}# #{val\ 1560}#)
                           (let ((#{val\ 1561}#
                                   (#{chi\ 167}#
                                     #{val\ 1560}#
                                     #{r\ 1549}#
                                     #{w\ 1550}#
                                     #{mod\ 1552}#))
                                 (#{n\ 1562}#
                                   (#{id-var-name\ 153}#
                                     #{id\ 1559}#
                                     #{w\ 1550}#)))
                             (let ((#{b\ 1563}#
                                     (#{lookup\ 128}#
                                       #{n\ 1562}#
                                       #{r\ 1549}#
                                       #{mod\ 1552}#)))
                               (let ((#{atom-key\ 1564}#
                                       (#{binding-type\ 123}# #{b\ 1563}#)))
                                 (if (memv #{atom-key\ 1564}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 99}#
                                     #{s\ 1551}#
                                     (syntax->datum #{id\ 1559}#)
                                     (#{binding-value\ 124}# #{b\ 1563}#)
                                     #{val\ 1561}#)
                                   (if (memv #{atom-key\ 1564}#
                                             '(global))
                                     (#{build-global-assignment\ 102}#
                                       #{s\ 1551}#
                                       #{n\ 1562}#
                                       #{val\ 1561}#
                                       #{mod\ 1552}#)
                                     (if (memv #{atom-key\ 1564}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 159}#
                                           #{id\ 1559}#
                                           #{w\ 1550}#
                                           #{mod\ 1552}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 160}#
                                           #{e\ 1548}#
                                           #{w\ 1550}#
                                           #{s\ 1551}#
                                           #{mod\ 1552}#)))))))))
                         #{tmp\ 1554}#)
                  ((lambda (#{tmp\ 1565}#)
                     (if #{tmp\ 1565}#
                       (apply (lambda (#{_\ 1566}#
                                       #{head\ 1567}#
                                       #{tail\ 1568}#
                                       #{val\ 1569}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 165}#
                                      #{head\ 1567}#
                                      #{r\ 1549}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 1552}#
                                      #t))
                                  (lambda (#{type\ 1570}#
                                           #{value\ 1571}#
                                           #{ee\ 1572}#
                                           #{ww\ 1573}#
                                           #{ss\ 1574}#
                                           #{modmod\ 1575}#)
                                    (if (memv #{type\ 1570}#
                                              '(module-ref))
                                      (let ((#{val\ 1576}#
                                              (#{chi\ 167}#
                                                #{val\ 1569}#
                                                #{r\ 1549}#
                                                #{w\ 1550}#
                                                #{mod\ 1552}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 1571}#
                                              (cons #{head\ 1567}#
                                                    #{tail\ 1568}#)))
                                          (lambda (#{id\ 1578}# #{mod\ 1579}#)
                                            (#{build-global-assignment\ 102}#
                                              #{s\ 1551}#
                                              #{id\ 1578}#
                                              #{val\ 1576}#
                                              #{mod\ 1579}#))))
                                      (#{build-application\ 96}#
                                        #{s\ 1551}#
                                        (#{chi\ 167}#
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
                                                #{head\ 1567}#)
                                          #{r\ 1549}#
                                          #{w\ 1550}#
                                          #{mod\ 1552}#)
                                        (map (lambda (#{e\ 1580}#)
                                               (#{chi\ 167}#
                                                 #{e\ 1580}#
                                                 #{r\ 1549}#
                                                 #{w\ 1550}#
                                                 #{mod\ 1552}#))
                                             (append
                                               #{tail\ 1568}#
                                               (list #{val\ 1569}#))))))))
                              #{tmp\ 1565}#)
                       ((lambda (#{_\ 1582}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 160}#
                              #{e\ 1548}#
                              #{w\ 1550}#
                              #{s\ 1551}#
                              #{mod\ 1552}#)))
                        #{tmp\ 1553}#)))
                   ($sc-dispatch
                     #{tmp\ 1553}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 1553}#
                '(any any any))))
           #{e\ 1548}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@
        (lambda (#{e\ 1583}#)
          ((lambda (#{tmp\ 1584}#)
             ((lambda (#{tmp\ 1585}#)
                (if (if #{tmp\ 1585}#
                      (apply (lambda (#{_\ 1586}# #{mod\ 1587}# #{id\ 1588}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1587}#)
                                 (#{id?\ 131}# #{id\ 1588}#)
                                 #f))
                             #{tmp\ 1585}#)
                      #f)
                  (apply (lambda (#{_\ 1590}# #{mod\ 1591}# #{id\ 1592}#)
                           (values
                             (syntax->datum #{id\ 1592}#)
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
                                     #{mod\ 1591}#))))
                         #{tmp\ 1585}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1584}#)))
              ($sc-dispatch
                #{tmp\ 1584}#
                '(any each-any any))))
           #{e\ 1583}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@@
        (lambda (#{e\ 1594}#)
          ((lambda (#{tmp\ 1595}#)
             ((lambda (#{tmp\ 1596}#)
                (if (if #{tmp\ 1596}#
                      (apply (lambda (#{_\ 1597}# #{mod\ 1598}# #{id\ 1599}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1598}#)
                                 (#{id?\ 131}# #{id\ 1599}#)
                                 #f))
                             #{tmp\ 1596}#)
                      #f)
                  (apply (lambda (#{_\ 1601}# #{mod\ 1602}# #{id\ 1603}#)
                           (values
                             (syntax->datum #{id\ 1603}#)
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
                                     #{mod\ 1602}#))))
                         #{tmp\ 1596}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1595}#)))
              ($sc-dispatch
                #{tmp\ 1595}#
                '(any each-any any))))
           #{e\ 1594}#)))
      (#{global-extend\ 129}#
        'core
        'if
        (lambda (#{e\ 1605}#
                 #{r\ 1606}#
                 #{w\ 1607}#
                 #{s\ 1608}#
                 #{mod\ 1609}#)
          ((lambda (#{tmp\ 1610}#)
             ((lambda (#{tmp\ 1611}#)
                (if #{tmp\ 1611}#
                  (apply (lambda (#{_\ 1612}# #{test\ 1613}# #{then\ 1614}#)
                           (#{build-conditional\ 97}#
                             #{s\ 1608}#
                             (#{chi\ 167}#
                               #{test\ 1613}#
                               #{r\ 1606}#
                               #{w\ 1607}#
                               #{mod\ 1609}#)
                             (#{chi\ 167}#
                               #{then\ 1614}#
                               #{r\ 1606}#
                               #{w\ 1607}#
                               #{mod\ 1609}#)
                             (#{build-void\ 95}# #f)))
                         #{tmp\ 1611}#)
                  ((lambda (#{tmp\ 1615}#)
                     (if #{tmp\ 1615}#
                       (apply (lambda (#{_\ 1616}#
                                       #{test\ 1617}#
                                       #{then\ 1618}#
                                       #{else\ 1619}#)
                                (#{build-conditional\ 97}#
                                  #{s\ 1608}#
                                  (#{chi\ 167}#
                                    #{test\ 1617}#
                                    #{r\ 1606}#
                                    #{w\ 1607}#
                                    #{mod\ 1609}#)
                                  (#{chi\ 167}#
                                    #{then\ 1618}#
                                    #{r\ 1606}#
                                    #{w\ 1607}#
                                    #{mod\ 1609}#)
                                  (#{chi\ 167}#
                                    #{else\ 1619}#
                                    #{r\ 1606}#
                                    #{w\ 1607}#
                                    #{mod\ 1609}#)))
                              #{tmp\ 1615}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1610}#)))
                   ($sc-dispatch
                     #{tmp\ 1610}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 1610}#
                '(any any any))))
           #{e\ 1605}#)))
      (#{global-extend\ 129}#
        'begin
        'begin
        '())
      (#{global-extend\ 129}#
        'define
        'define
        '())
      (#{global-extend\ 129}#
        'define-syntax
        'define-syntax
        '())
      (#{global-extend\ 129}#
        'eval-when
        'eval-when
        '())
      (#{global-extend\ 129}#
        'core
        'syntax-case
        (letrec ((#{gen-syntax-case\ 1623}#
                   (lambda (#{x\ 1624}#
                            #{keys\ 1625}#
                            #{clauses\ 1626}#
                            #{r\ 1627}#
                            #{mod\ 1628}#)
                     (if (null? #{clauses\ 1626}#)
                       (#{build-application\ 96}#
                         #f
                         (#{build-primref\ 108}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 109}# #f #f)
                               (#{build-data\ 109}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 1624}#))
                       ((lambda (#{tmp\ 1629}#)
                          ((lambda (#{tmp\ 1630}#)
                             (if #{tmp\ 1630}#
                               (apply (lambda (#{pat\ 1631}# #{exp\ 1632}#)
                                        (if (if (#{id?\ 131}# #{pat\ 1631}#)
                                              (and-map
                                                (lambda (#{x\ 1633}#)
                                                  (not (#{free-id=?\ 154}#
                                                         #{pat\ 1631}#
                                                         #{x\ 1633}#)))
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
                                                      #{keys\ 1625}#))
                                              #f)
                                          (let ((#{labels\ 1634}#
                                                  (list (#{gen-label\ 136}#)))
                                                (#{var\ 1635}#
                                                  (#{gen-var\ 181}#
                                                    #{pat\ 1631}#)))
                                            (#{build-application\ 96}#
                                              #f
                                              (#{build-simple-lambda\ 105}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 1631}#))
                                                #f
                                                (list #{var\ 1635}#)
                                                #f
                                                (#{chi\ 167}#
                                                  #{exp\ 1632}#
                                                  (#{extend-env\ 125}#
                                                    #{labels\ 1634}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 1635}#
                                                                      0)))
                                                    #{r\ 1627}#)
                                                  (#{make-binding-wrap\ 148}#
                                                    (list #{pat\ 1631}#)
                                                    #{labels\ 1634}#
                                                    '(()))
                                                  #{mod\ 1628}#))
                                              (list #{x\ 1624}#)))
                                          (#{gen-clause\ 1622}#
                                            #{x\ 1624}#
                                            #{keys\ 1625}#
                                            (cdr #{clauses\ 1626}#)
                                            #{r\ 1627}#
                                            #{pat\ 1631}#
                                            #t
                                            #{exp\ 1632}#
                                            #{mod\ 1628}#)))
                                      #{tmp\ 1630}#)
                               ((lambda (#{tmp\ 1636}#)
                                  (if #{tmp\ 1636}#
                                    (apply (lambda (#{pat\ 1637}#
                                                    #{fender\ 1638}#
                                                    #{exp\ 1639}#)
                                             (#{gen-clause\ 1622}#
                                               #{x\ 1624}#
                                               #{keys\ 1625}#
                                               (cdr #{clauses\ 1626}#)
                                               #{r\ 1627}#
                                               #{pat\ 1637}#
                                               #{fender\ 1638}#
                                               #{exp\ 1639}#
                                               #{mod\ 1628}#))
                                           #{tmp\ 1636}#)
                                    ((lambda (#{_\ 1640}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 1626}#)))
                                     #{tmp\ 1629}#)))
                                ($sc-dispatch
                                  #{tmp\ 1629}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 1629}# (quote (any any)))))
                        (car #{clauses\ 1626}#)))))
                 (#{gen-clause\ 1622}#
                   (lambda (#{x\ 1641}#
                            #{keys\ 1642}#
                            #{clauses\ 1643}#
                            #{r\ 1644}#
                            #{pat\ 1645}#
                            #{fender\ 1646}#
                            #{exp\ 1647}#
                            #{mod\ 1648}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 1620}#
                           #{pat\ 1645}#
                           #{keys\ 1642}#))
                       (lambda (#{p\ 1649}# #{pvars\ 1650}#)
                         (if (not (#{distinct-bound-ids?\ 157}#
                                    (map car #{pvars\ 1650}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 1645}#)
                           (if (not (and-map
                                      (lambda (#{x\ 1651}#)
                                        (not (#{ellipsis?\ 175}#
                                               (car #{x\ 1651}#))))
                                      #{pvars\ 1650}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 1645}#)
                             (let ((#{y\ 1652}#
                                     (#{gen-var\ 181}# (quote tmp))))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 1652}#)
                                   #f
                                   (let ((#{y\ 1653}#
                                           (#{build-lexical-reference\ 98}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 1652}#)))
                                     (#{build-conditional\ 97}#
                                       #f
                                       ((lambda (#{tmp\ 1654}#)
                                          ((lambda (#{tmp\ 1655}#)
                                             (if #{tmp\ 1655}#
                                               (apply (lambda () #{y\ 1653}#)
                                                      #{tmp\ 1655}#)
                                               ((lambda (#{_\ 1656}#)
                                                  (#{build-conditional\ 97}#
                                                    #f
                                                    #{y\ 1653}#
                                                    (#{build-dispatch-call\ 1621}#
                                                      #{pvars\ 1650}#
                                                      #{fender\ 1646}#
                                                      #{y\ 1653}#
                                                      #{r\ 1644}#
                                                      #{mod\ 1648}#)
                                                    (#{build-data\ 109}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 1654}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1654}#
                                             '#(atom #t))))
                                        #{fender\ 1646}#)
                                       (#{build-dispatch-call\ 1621}#
                                         #{pvars\ 1650}#
                                         #{exp\ 1647}#
                                         #{y\ 1653}#
                                         #{r\ 1644}#
                                         #{mod\ 1648}#)
                                       (#{gen-syntax-case\ 1623}#
                                         #{x\ 1641}#
                                         #{keys\ 1642}#
                                         #{clauses\ 1643}#
                                         #{r\ 1644}#
                                         #{mod\ 1648}#))))
                                 (list (if (eq? #{p\ 1649}# (quote any))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             'list)
                                           (list #{x\ 1641}#))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 1641}#
                                                 (#{build-data\ 109}#
                                                   #f
                                                   #{p\ 1649}#)))))))))))))
                 (#{build-dispatch-call\ 1621}#
                   (lambda (#{pvars\ 1657}#
                            #{exp\ 1658}#
                            #{y\ 1659}#
                            #{r\ 1660}#
                            #{mod\ 1661}#)
                     (let ((#{ids\ 1662}# (map car #{pvars\ 1657}#))
                           (#{levels\ 1663}# (map cdr #{pvars\ 1657}#)))
                       (let ((#{labels\ 1664}#
                               (#{gen-labels\ 137}# #{ids\ 1662}#))
                             (#{new-vars\ 1665}#
                               (map #{gen-var\ 181}# #{ids\ 1662}#)))
                         (#{build-application\ 96}#
                           #f
                           (#{build-primref\ 108}# #f (quote apply))
                           (list (#{build-simple-lambda\ 105}#
                                   #f
                                   (map syntax->datum #{ids\ 1662}#)
                                   #f
                                   #{new-vars\ 1665}#
                                   #f
                                   (#{chi\ 167}#
                                     #{exp\ 1658}#
                                     (#{extend-env\ 125}#
                                       #{labels\ 1664}#
                                       (map (lambda (#{var\ 1666}#
                                                     #{level\ 1667}#)
                                              (cons 'syntax
                                                    (cons #{var\ 1666}#
                                                          #{level\ 1667}#)))
                                            #{new-vars\ 1665}#
                                            (map cdr #{pvars\ 1657}#))
                                       #{r\ 1660}#)
                                     (#{make-binding-wrap\ 148}#
                                       #{ids\ 1662}#
                                       #{labels\ 1664}#
                                       '(()))
                                     #{mod\ 1661}#))
                                 #{y\ 1659}#))))))
                 (#{convert-pattern\ 1620}#
                   (lambda (#{pattern\ 1668}# #{keys\ 1669}#)
                     (letrec ((#{cvt\ 1670}#
                                (lambda (#{p\ 1671}# #{n\ 1672}# #{ids\ 1673}#)
                                  (if (#{id?\ 131}# #{p\ 1671}#)
                                    (if (#{bound-id-member?\ 158}#
                                          #{p\ 1671}#
                                          #{keys\ 1669}#)
                                      (values
                                        (vector (quote free-id) #{p\ 1671}#)
                                        #{ids\ 1673}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 1671}# #{n\ 1672}#)
                                              #{ids\ 1673}#)))
                                    ((lambda (#{tmp\ 1674}#)
                                       ((lambda (#{tmp\ 1675}#)
                                          (if (if #{tmp\ 1675}#
                                                (apply (lambda (#{x\ 1676}#
                                                                #{dots\ 1677}#)
                                                         (#{ellipsis?\ 175}#
                                                           #{dots\ 1677}#))
                                                       #{tmp\ 1675}#)
                                                #f)
                                            (apply (lambda (#{x\ 1678}#
                                                            #{dots\ 1679}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 1670}#
                                                           #{x\ 1678}#
                                                           (#{fx+\ 86}#
                                                             #{n\ 1672}#
                                                             1)
                                                           #{ids\ 1673}#))
                                                       (lambda (#{p\ 1680}#
                                                                #{ids\ 1681}#)
                                                         (values
                                                           (if (eq? #{p\ 1680}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 1680}#))
                                                           #{ids\ 1681}#))))
                                                   #{tmp\ 1675}#)
                                            ((lambda (#{tmp\ 1682}#)
                                               (if #{tmp\ 1682}#
                                                 (apply (lambda (#{x\ 1683}#
                                                                 #{y\ 1684}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 1670}#
                                                                #{y\ 1684}#
                                                                #{n\ 1672}#
                                                                #{ids\ 1673}#))
                                                            (lambda (#{y\ 1685}#
                                                                     #{ids\ 1686}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 1670}#
                                                                    #{x\ 1683}#
                                                                    #{n\ 1672}#
                                                                    #{ids\ 1686}#))
                                                                (lambda (#{x\ 1687}#
                                                                         #{ids\ 1688}#)
                                                                  (values
                                                                    (cons #{x\ 1687}#
                                                                          #{y\ 1685}#)
                                                                    #{ids\ 1688}#))))))
                                                        #{tmp\ 1682}#)
                                                 ((lambda (#{tmp\ 1689}#)
                                                    (if #{tmp\ 1689}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 1673}#))
                                                             #{tmp\ 1689}#)
                                                      ((lambda (#{tmp\ 1690}#)
                                                         (if #{tmp\ 1690}#
                                                           (apply (lambda (#{x\ 1691}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 1670}#
                                                                          #{x\ 1691}#
                                                                          #{n\ 1672}#
                                                                          #{ids\ 1673}#))
                                                                      (lambda (#{p\ 1693}#
                                                                               #{ids\ 1694}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 1693}#)
                                                                          #{ids\ 1694}#))))
                                                                  #{tmp\ 1690}#)
                                                           ((lambda (#{x\ 1695}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 180}#
                                                                    #{p\ 1671}#
                                                                    '(())))
                                                                #{ids\ 1673}#))
                                                            #{tmp\ 1674}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1674}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 1674}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 1674}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 1674}#
                                          '(any any))))
                                     #{p\ 1671}#)))))
                       (#{cvt\ 1670}# #{pattern\ 1668}# 0 (quote ()))))))
          (lambda (#{e\ 1696}#
                   #{r\ 1697}#
                   #{w\ 1698}#
                   #{s\ 1699}#
                   #{mod\ 1700}#)
            (let ((#{e\ 1701}#
                    (#{source-wrap\ 160}#
                      #{e\ 1696}#
                      #{w\ 1698}#
                      #{s\ 1699}#
                      #{mod\ 1700}#)))
              ((lambda (#{tmp\ 1702}#)
                 ((lambda (#{tmp\ 1703}#)
                    (if #{tmp\ 1703}#
                      (apply (lambda (#{_\ 1704}#
                                      #{val\ 1705}#
                                      #{key\ 1706}#
                                      #{m\ 1707}#)
                               (if (and-map
                                     (lambda (#{x\ 1708}#)
                                       (if (#{id?\ 131}# #{x\ 1708}#)
                                         (not (#{ellipsis?\ 175}# #{x\ 1708}#))
                                         #f))
                                     #{key\ 1706}#)
                                 (let ((#{x\ 1710}#
                                         (#{gen-var\ 181}# (quote tmp))))
                                   (#{build-application\ 96}#
                                     #{s\ 1699}#
                                     (#{build-simple-lambda\ 105}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 1710}#)
                                       #f
                                       (#{gen-syntax-case\ 1623}#
                                         (#{build-lexical-reference\ 98}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 1710}#)
                                         #{key\ 1706}#
                                         #{m\ 1707}#
                                         #{r\ 1697}#
                                         #{mod\ 1700}#))
                                     (list (#{chi\ 167}#
                                             #{val\ 1705}#
                                             #{r\ 1697}#
                                             '(())
                                             #{mod\ 1700}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 1701}#)))
                             #{tmp\ 1703}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 1702}#)))
                  ($sc-dispatch
                    #{tmp\ 1702}#
                    '(any any each-any . each-any))))
               #{e\ 1701}#)))))
      (set! sc-expand
        (lambda (#{x\ 1713}# . #{rest\ 1714}#)
          (if (if (pair? #{x\ 1713}#)
                (equal? (car #{x\ 1713}#) #{noexpand\ 84}#)
                #f)
            (cadr #{x\ 1713}#)
            (let ((#{m\ 1715}#
                    (if (null? #{rest\ 1714}#)
                      'e
                      (car #{rest\ 1714}#)))
                  (#{esew\ 1716}#
                    (if (let ((#{t\ 1717}# (null? #{rest\ 1714}#)))
                          (if #{t\ 1717}#
                            #{t\ 1717}#
                            (null? (cdr #{rest\ 1714}#))))
                      '(eval)
                      (cadr #{rest\ 1714}#))))
              (with-fluid*
                #{*mode*\ 85}#
                #{m\ 1715}#
                (lambda ()
                  (#{chi-top\ 166}#
                    #{x\ 1713}#
                    '()
                    '((top))
                    #{m\ 1715}#
                    #{esew\ 1716}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 1718}#)
          (#{nonsymbol-id?\ 130}# #{x\ 1718}#)))
      (set! datum->syntax
        (lambda (#{id\ 1719}# #{datum\ 1720}#)
          (#{make-syntax-object\ 114}#
            #{datum\ 1720}#
            (#{syntax-object-wrap\ 117}# #{id\ 1719}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 1721}#)
          (#{strip\ 180}# #{x\ 1721}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 1722}#)
          (begin
            (let ((#{x\ 1723}# #{ls\ 1722}#))
              (if (not (list? #{x\ 1723}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 1723}#)))
            (map (lambda (#{x\ 1724}#)
                   (#{wrap\ 159}# (gensym) (quote ((top))) #f))
                 #{ls\ 1722}#))))
      (set! free-identifier=?
        (lambda (#{x\ 1725}# #{y\ 1726}#)
          (begin
            (let ((#{x\ 1727}# #{x\ 1725}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1727}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1727}#)))
            (let ((#{x\ 1728}# #{y\ 1726}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1728}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1728}#)))
            (#{free-id=?\ 154}# #{x\ 1725}# #{y\ 1726}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 1729}# #{y\ 1730}#)
          (begin
            (let ((#{x\ 1731}# #{x\ 1729}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1731}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1731}#)))
            (let ((#{x\ 1732}# #{y\ 1730}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1732}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1732}#)))
            (#{bound-id=?\ 155}# #{x\ 1729}# #{y\ 1730}#))))
      (set! syntax-violation
        (lambda (#{who\ 1733}#
                 #{message\ 1734}#
                 #{form\ 1735}#
                 .
                 #{subform\ 1736}#)
          (begin
            (let ((#{x\ 1737}# #{who\ 1733}#))
              (if (not ((lambda (#{x\ 1738}#)
                          (let ((#{t\ 1739}# (not #{x\ 1738}#)))
                            (if #{t\ 1739}#
                              #{t\ 1739}#
                              (let ((#{t\ 1740}# (string? #{x\ 1738}#)))
                                (if #{t\ 1740}#
                                  #{t\ 1740}#
                                  (symbol? #{x\ 1738}#))))))
                        #{x\ 1737}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1737}#)))
            (let ((#{x\ 1741}# #{message\ 1734}#))
              (if (not (string? #{x\ 1741}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1741}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 1733}# "~a: " "")
                "~a "
                (if (null? #{subform\ 1736}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 1742}#
                      (cons #{message\ 1734}#
                            (map (lambda (#{x\ 1743}#)
                                   (#{strip\ 180}# #{x\ 1743}# (quote (()))))
                                 (append
                                   #{subform\ 1736}#
                                   (list #{form\ 1735}#))))))
                (if #{who\ 1733}#
                  (cons #{who\ 1733}# #{tail\ 1742}#)
                  #{tail\ 1742}#))
              #f))))
      (letrec ((#{match\ 1748}#
                 (lambda (#{e\ 1749}#
                          #{p\ 1750}#
                          #{w\ 1751}#
                          #{r\ 1752}#
                          #{mod\ 1753}#)
                   (if (not #{r\ 1752}#)
                     #f
                     (if (eq? #{p\ 1750}# (quote any))
                       (cons (#{wrap\ 159}#
                               #{e\ 1749}#
                               #{w\ 1751}#
                               #{mod\ 1753}#)
                             #{r\ 1752}#)
                       (if (#{syntax-object?\ 115}# #{e\ 1749}#)
                         (#{match*\ 1747}#
                           (#{syntax-object-expression\ 116}# #{e\ 1749}#)
                           #{p\ 1750}#
                           (#{join-wraps\ 150}#
                             #{w\ 1751}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1749}#))
                           #{r\ 1752}#
                           (#{syntax-object-module\ 118}# #{e\ 1749}#))
                         (#{match*\ 1747}#
                           #{e\ 1749}#
                           #{p\ 1750}#
                           #{w\ 1751}#
                           #{r\ 1752}#
                           #{mod\ 1753}#))))))
               (#{match*\ 1747}#
                 (lambda (#{e\ 1754}#
                          #{p\ 1755}#
                          #{w\ 1756}#
                          #{r\ 1757}#
                          #{mod\ 1758}#)
                   (if (null? #{p\ 1755}#)
                     (if (null? #{e\ 1754}#) #{r\ 1757}# #f)
                     (if (pair? #{p\ 1755}#)
                       (if (pair? #{e\ 1754}#)
                         (#{match\ 1748}#
                           (car #{e\ 1754}#)
                           (car #{p\ 1755}#)
                           #{w\ 1756}#
                           (#{match\ 1748}#
                             (cdr #{e\ 1754}#)
                             (cdr #{p\ 1755}#)
                             #{w\ 1756}#
                             #{r\ 1757}#
                             #{mod\ 1758}#)
                           #{mod\ 1758}#)
                         #f)
                       (if (eq? #{p\ 1755}# (quote each-any))
                         (let ((#{l\ 1759}#
                                 (#{match-each-any\ 1745}#
                                   #{e\ 1754}#
                                   #{w\ 1756}#
                                   #{mod\ 1758}#)))
                           (if #{l\ 1759}#
                             (cons #{l\ 1759}# #{r\ 1757}#)
                             #f))
                         (let ((#{atom-key\ 1760}# (vector-ref #{p\ 1755}# 0)))
                           (if (memv #{atom-key\ 1760}# (quote (each)))
                             (if (null? #{e\ 1754}#)
                               (#{match-empty\ 1746}#
                                 (vector-ref #{p\ 1755}# 1)
                                 #{r\ 1757}#)
                               (let ((#{l\ 1761}#
                                       (#{match-each\ 1744}#
                                         #{e\ 1754}#
                                         (vector-ref #{p\ 1755}# 1)
                                         #{w\ 1756}#
                                         #{mod\ 1758}#)))
                                 (if #{l\ 1761}#
                                   (letrec ((#{collect\ 1762}#
                                              (lambda (#{l\ 1763}#)
                                                (if (null? (car #{l\ 1763}#))
                                                  #{r\ 1757}#
                                                  (cons (map car #{l\ 1763}#)
                                                        (#{collect\ 1762}#
                                                          (map cdr
                                                               #{l\ 1763}#)))))))
                                     (#{collect\ 1762}# #{l\ 1761}#))
                                   #f)))
                             (if (memv #{atom-key\ 1760}# (quote (free-id)))
                               (if (#{id?\ 131}# #{e\ 1754}#)
                                 (if (#{free-id=?\ 154}#
                                       (#{wrap\ 159}#
                                         #{e\ 1754}#
                                         #{w\ 1756}#
                                         #{mod\ 1758}#)
                                       (vector-ref #{p\ 1755}# 1))
                                   #{r\ 1757}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 1760}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 1755}# 1)
                                       (#{strip\ 180}#
                                         #{e\ 1754}#
                                         #{w\ 1756}#))
                                   #{r\ 1757}#
                                   #f)
                                 (if (memv #{atom-key\ 1760}# (quote (vector)))
                                   (if (vector? #{e\ 1754}#)
                                     (#{match\ 1748}#
                                       (vector->list #{e\ 1754}#)
                                       (vector-ref #{p\ 1755}# 1)
                                       #{w\ 1756}#
                                       #{r\ 1757}#
                                       #{mod\ 1758}#)
                                     #f)))))))))))
               (#{match-empty\ 1746}#
                 (lambda (#{p\ 1764}# #{r\ 1765}#)
                   (if (null? #{p\ 1764}#)
                     #{r\ 1765}#
                     (if (eq? #{p\ 1764}# (quote any))
                       (cons (quote ()) #{r\ 1765}#)
                       (if (pair? #{p\ 1764}#)
                         (#{match-empty\ 1746}#
                           (car #{p\ 1764}#)
                           (#{match-empty\ 1746}#
                             (cdr #{p\ 1764}#)
                             #{r\ 1765}#))
                         (if (eq? #{p\ 1764}# (quote each-any))
                           (cons (quote ()) #{r\ 1765}#)
                           (let ((#{atom-key\ 1766}#
                                   (vector-ref #{p\ 1764}# 0)))
                             (if (memv #{atom-key\ 1766}# (quote (each)))
                               (#{match-empty\ 1746}#
                                 (vector-ref #{p\ 1764}# 1)
                                 #{r\ 1765}#)
                               (if (memv #{atom-key\ 1766}#
                                         '(free-id atom))
                                 #{r\ 1765}#
                                 (if (memv #{atom-key\ 1766}# (quote (vector)))
                                   (#{match-empty\ 1746}#
                                     (vector-ref #{p\ 1764}# 1)
                                     #{r\ 1765}#)))))))))))
               (#{match-each-any\ 1745}#
                 (lambda (#{e\ 1767}# #{w\ 1768}# #{mod\ 1769}#)
                   (if (pair? #{e\ 1767}#)
                     (let ((#{l\ 1770}#
                             (#{match-each-any\ 1745}#
                               (cdr #{e\ 1767}#)
                               #{w\ 1768}#
                               #{mod\ 1769}#)))
                       (if #{l\ 1770}#
                         (cons (#{wrap\ 159}#
                                 (car #{e\ 1767}#)
                                 #{w\ 1768}#
                                 #{mod\ 1769}#)
                               #{l\ 1770}#)
                         #f))
                     (if (null? #{e\ 1767}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1767}#)
                         (#{match-each-any\ 1745}#
                           (#{syntax-object-expression\ 116}# #{e\ 1767}#)
                           (#{join-wraps\ 150}#
                             #{w\ 1768}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1767}#))
                           #{mod\ 1769}#)
                         #f)))))
               (#{match-each\ 1744}#
                 (lambda (#{e\ 1771}#
                          #{p\ 1772}#
                          #{w\ 1773}#
                          #{mod\ 1774}#)
                   (if (pair? #{e\ 1771}#)
                     (let ((#{first\ 1775}#
                             (#{match\ 1748}#
                               (car #{e\ 1771}#)
                               #{p\ 1772}#
                               #{w\ 1773}#
                               '()
                               #{mod\ 1774}#)))
                       (if #{first\ 1775}#
                         (let ((#{rest\ 1776}#
                                 (#{match-each\ 1744}#
                                   (cdr #{e\ 1771}#)
                                   #{p\ 1772}#
                                   #{w\ 1773}#
                                   #{mod\ 1774}#)))
                           (if #{rest\ 1776}#
                             (cons #{first\ 1775}# #{rest\ 1776}#)
                             #f))
                         #f))
                     (if (null? #{e\ 1771}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1771}#)
                         (#{match-each\ 1744}#
                           (#{syntax-object-expression\ 116}# #{e\ 1771}#)
                           #{p\ 1772}#
                           (#{join-wraps\ 150}#
                             #{w\ 1773}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1771}#))
                           (#{syntax-object-module\ 118}# #{e\ 1771}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 1777}# #{p\ 1778}#)
            (if (eq? #{p\ 1778}# (quote any))
              (list #{e\ 1777}#)
              (if (#{syntax-object?\ 115}# #{e\ 1777}#)
                (#{match*\ 1747}#
                  (#{syntax-object-expression\ 116}# #{e\ 1777}#)
                  #{p\ 1778}#
                  (#{syntax-object-wrap\ 117}# #{e\ 1777}#)
                  '()
                  (#{syntax-object-module\ 118}# #{e\ 1777}#))
                (#{match*\ 1747}#
                  #{e\ 1777}#
                  #{p\ 1778}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1779}#)
      ((lambda (#{tmp\ 1780}#)
         ((lambda (#{tmp\ 1781}#)
            (if #{tmp\ 1781}#
              (apply (lambda (#{_\ 1782}# #{e1\ 1783}# #{e2\ 1784}#)
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
                             (cons #{e1\ 1783}# #{e2\ 1784}#)))
                     #{tmp\ 1781}#)
              ((lambda (#{tmp\ 1786}#)
                 (if #{tmp\ 1786}#
                   (apply (lambda (#{_\ 1787}#
                                   #{out\ 1788}#
                                   #{in\ 1789}#
                                   #{e1\ 1790}#
                                   #{e2\ 1791}#)
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
                                  #{in\ 1789}#
                                  '()
                                  (list #{out\ 1788}#
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
                                              (cons #{e1\ 1790}#
                                                    #{e2\ 1791}#)))))
                          #{tmp\ 1786}#)
                   ((lambda (#{tmp\ 1793}#)
                      (if #{tmp\ 1793}#
                        (apply (lambda (#{_\ 1794}#
                                        #{out\ 1795}#
                                        #{in\ 1796}#
                                        #{e1\ 1797}#
                                        #{e2\ 1798}#)
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
                                             #{in\ 1796}#)
                                       '()
                                       (list #{out\ 1795}#
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
                                                   (cons #{e1\ 1797}#
                                                         #{e2\ 1798}#)))))
                               #{tmp\ 1793}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 1780}#)))
                    ($sc-dispatch
                      #{tmp\ 1780}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 1780}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 1780}#
            '(any () any . each-any))))
       #{x\ 1779}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1802}#)
      ((lambda (#{tmp\ 1803}#)
         ((lambda (#{tmp\ 1804}#)
            (if #{tmp\ 1804}#
              (apply (lambda (#{_\ 1805}#
                              #{k\ 1806}#
                              #{keyword\ 1807}#
                              #{pattern\ 1808}#
                              #{template\ 1809}#)
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
                                         (cons #{k\ 1806}#
                                               (map (lambda (#{tmp\ 1812}#
                                                             #{tmp\ 1811}#)
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
                                                                  #{tmp\ 1811}#)
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
                                                                  #{tmp\ 1812}#)))
                                                    #{template\ 1809}#
                                                    #{pattern\ 1808}#))))))
                     #{tmp\ 1804}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1803}#)))
          ($sc-dispatch
            #{tmp\ 1803}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 1802}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 1813}#)
      ((lambda (#{tmp\ 1814}#)
         ((lambda (#{tmp\ 1815}#)
            (if (if #{tmp\ 1815}#
                  (apply (lambda (#{let*\ 1816}#
                                  #{x\ 1817}#
                                  #{v\ 1818}#
                                  #{e1\ 1819}#
                                  #{e2\ 1820}#)
                           (and-map identifier? #{x\ 1817}#))
                         #{tmp\ 1815}#)
                  #f)
              (apply (lambda (#{let*\ 1822}#
                              #{x\ 1823}#
                              #{v\ 1824}#
                              #{e1\ 1825}#
                              #{e2\ 1826}#)
                       (letrec ((#{f\ 1827}#
                                  (lambda (#{bindings\ 1828}#)
                                    (if (null? #{bindings\ 1828}#)
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
                                                  (cons #{e1\ 1825}#
                                                        #{e2\ 1826}#)))
                                      ((lambda (#{tmp\ 1832}#)
                                         ((lambda (#{tmp\ 1833}#)
                                            (if #{tmp\ 1833}#
                                              (apply (lambda (#{body\ 1834}#
                                                              #{binding\ 1835}#)
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
                                                             (list #{binding\ 1835}#)
                                                             #{body\ 1834}#))
                                                     #{tmp\ 1833}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 1832}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1832}#
                                            '(any any))))
                                       (list (#{f\ 1827}#
                                               (cdr #{bindings\ 1828}#))
                                             (car #{bindings\ 1828}#)))))))
                         (#{f\ 1827}# (map list #{x\ 1823}# #{v\ 1824}#))))
                     #{tmp\ 1815}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1814}#)))
          ($sc-dispatch
            #{tmp\ 1814}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 1813}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 1836}#)
      ((lambda (#{tmp\ 1837}#)
         ((lambda (#{tmp\ 1838}#)
            (if #{tmp\ 1838}#
              (apply (lambda (#{_\ 1839}#
                              #{var\ 1840}#
                              #{init\ 1841}#
                              #{step\ 1842}#
                              #{e0\ 1843}#
                              #{e1\ 1844}#
                              #{c\ 1845}#)
                       ((lambda (#{tmp\ 1846}#)
                          ((lambda (#{tmp\ 1847}#)
                             (if #{tmp\ 1847}#
                               (apply (lambda (#{step\ 1848}#)
                                        ((lambda (#{tmp\ 1849}#)
                                           ((lambda (#{tmp\ 1850}#)
                                              (if #{tmp\ 1850}#
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
                                                                    #{var\ 1840}#
                                                                    #{init\ 1841}#)
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
                                                                           #{e0\ 1843}#)
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
                                                                             #{c\ 1845}#
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
                                                                                         #{step\ 1848}#)))))))
                                                       #{tmp\ 1850}#)
                                                ((lambda (#{tmp\ 1855}#)
                                                   (if #{tmp\ 1855}#
                                                     (apply (lambda (#{e1\ 1856}#
                                                                     #{e2\ 1857}#)
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
                                                                         #{var\ 1840}#
                                                                         #{init\ 1841}#)
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
                                                                          #{e0\ 1843}#
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
                                                                                (cons #{e1\ 1856}#
                                                                                      #{e2\ 1857}#))
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
                                                                                  #{c\ 1845}#
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
                                                                                              #{step\ 1848}#)))))))
                                                            #{tmp\ 1855}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1849}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1849}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 1849}#
                                              '())))
                                         #{e1\ 1844}#))
                                      #{tmp\ 1847}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 1846}#)))
                           ($sc-dispatch #{tmp\ 1846}# (quote each-any))))
                        (map (lambda (#{v\ 1864}# #{s\ 1865}#)
                               ((lambda (#{tmp\ 1866}#)
                                  ((lambda (#{tmp\ 1867}#)
                                     (if #{tmp\ 1867}#
                                       (apply (lambda () #{v\ 1864}#)
                                              #{tmp\ 1867}#)
                                       ((lambda (#{tmp\ 1868}#)
                                          (if #{tmp\ 1868}#
                                            (apply (lambda (#{e\ 1869}#)
                                                     #{e\ 1869}#)
                                                   #{tmp\ 1868}#)
                                            ((lambda (#{_\ 1870}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 1836}#
                                                 #{s\ 1865}#))
                                             #{tmp\ 1866}#)))
                                        ($sc-dispatch
                                          #{tmp\ 1866}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 1866}# (quote ()))))
                                #{s\ 1865}#))
                             #{var\ 1840}#
                             #{step\ 1842}#)))
                     #{tmp\ 1838}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1837}#)))
          ($sc-dispatch
            #{tmp\ 1837}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 1836}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 1873}#
               (lambda (#{x\ 1877}# #{y\ 1878}#)
                 ((lambda (#{tmp\ 1879}#)
                    ((lambda (#{tmp\ 1880}#)
                       (if #{tmp\ 1880}#
                         (apply (lambda (#{x\ 1881}# #{y\ 1882}#)
                                  ((lambda (#{tmp\ 1883}#)
                                     ((lambda (#{tmp\ 1884}#)
                                        (if #{tmp\ 1884}#
                                          (apply (lambda (#{dy\ 1885}#)
                                                   ((lambda (#{tmp\ 1886}#)
                                                      ((lambda (#{tmp\ 1887}#)
                                                         (if #{tmp\ 1887}#
                                                           (apply (lambda (#{dx\ 1888}#)
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
                                                                          (cons #{dx\ 1888}#
                                                                                #{dy\ 1885}#)))
                                                                  #{tmp\ 1887}#)
                                                           ((lambda (#{_\ 1889}#)
                                                              (if (null? #{dy\ 1885}#)
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
                                                                      #{x\ 1881}#)
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
                                                                      #{x\ 1881}#
                                                                      #{y\ 1882}#)))
                                                            #{tmp\ 1886}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1886}#
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
                                                    #{x\ 1881}#))
                                                 #{tmp\ 1884}#)
                                          ((lambda (#{tmp\ 1890}#)
                                             (if #{tmp\ 1890}#
                                               (apply (lambda (#{stuff\ 1891}#)
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
                                                              (cons #{x\ 1881}#
                                                                    #{stuff\ 1891}#)))
                                                      #{tmp\ 1890}#)
                                               ((lambda (#{else\ 1892}#)
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
                                                        #{x\ 1881}#
                                                        #{y\ 1882}#))
                                                #{tmp\ 1883}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1883}#
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
                                        #{tmp\ 1883}#
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
                                   #{y\ 1882}#))
                                #{tmp\ 1880}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1879}#)))
                     ($sc-dispatch #{tmp\ 1879}# (quote (any any)))))
                  (list #{x\ 1877}# #{y\ 1878}#))))
             (#{quasiappend\ 1874}#
               (lambda (#{x\ 1893}# #{y\ 1894}#)
                 ((lambda (#{tmp\ 1895}#)
                    ((lambda (#{tmp\ 1896}#)
                       (if #{tmp\ 1896}#
                         (apply (lambda (#{x\ 1897}# #{y\ 1898}#)
                                  ((lambda (#{tmp\ 1899}#)
                                     ((lambda (#{tmp\ 1900}#)
                                        (if #{tmp\ 1900}#
                                          (apply (lambda () #{x\ 1897}#)
                                                 #{tmp\ 1900}#)
                                          ((lambda (#{_\ 1901}#)
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
                                                   #{x\ 1897}#
                                                   #{y\ 1898}#))
                                           #{tmp\ 1899}#)))
                                      ($sc-dispatch
                                        #{tmp\ 1899}#
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
                                   #{y\ 1898}#))
                                #{tmp\ 1896}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1895}#)))
                     ($sc-dispatch #{tmp\ 1895}# (quote (any any)))))
                  (list #{x\ 1893}# #{y\ 1894}#))))
             (#{quasivector\ 1875}#
               (lambda (#{x\ 1902}#)
                 ((lambda (#{tmp\ 1903}#)
                    ((lambda (#{x\ 1904}#)
                       ((lambda (#{tmp\ 1905}#)
                          ((lambda (#{tmp\ 1906}#)
                             (if #{tmp\ 1906}#
                               (apply (lambda (#{x\ 1907}#)
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
                                              (list->vector #{x\ 1907}#)))
                                      #{tmp\ 1906}#)
                               ((lambda (#{tmp\ 1909}#)
                                  (if #{tmp\ 1909}#
                                    (apply (lambda (#{x\ 1910}#)
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
                                                   #{x\ 1910}#))
                                           #{tmp\ 1909}#)
                                    ((lambda (#{_\ 1912}#)
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
                                             #{x\ 1904}#))
                                     #{tmp\ 1905}#)))
                                ($sc-dispatch
                                  #{tmp\ 1905}#
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
                             #{tmp\ 1905}#
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
                        #{x\ 1904}#))
                     #{tmp\ 1903}#))
                  #{x\ 1902}#)))
             (#{quasi\ 1876}#
               (lambda (#{p\ 1913}# #{lev\ 1914}#)
                 ((lambda (#{tmp\ 1915}#)
                    ((lambda (#{tmp\ 1916}#)
                       (if #{tmp\ 1916}#
                         (apply (lambda (#{p\ 1917}#)
                                  (if (= #{lev\ 1914}# 0)
                                    #{p\ 1917}#
                                    (#{quasicons\ 1873}#
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
                                      (#{quasi\ 1876}#
                                        (list #{p\ 1917}#)
                                        (- #{lev\ 1914}# 1)))))
                                #{tmp\ 1916}#)
                         ((lambda (#{tmp\ 1918}#)
                            (if (if #{tmp\ 1918}#
                                  (apply (lambda (#{args\ 1919}#)
                                           (= #{lev\ 1914}# 0))
                                         #{tmp\ 1918}#)
                                  #f)
                              (apply (lambda (#{args\ 1920}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 1913}#
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
                                               #{args\ 1920}#)))
                                     #{tmp\ 1918}#)
                              ((lambda (#{tmp\ 1921}#)
                                 (if #{tmp\ 1921}#
                                   (apply (lambda (#{p\ 1922}# #{q\ 1923}#)
                                            (if (= #{lev\ 1914}# 0)
                                              (#{quasiappend\ 1874}#
                                                #{p\ 1922}#
                                                (#{quasi\ 1876}#
                                                  #{q\ 1923}#
                                                  #{lev\ 1914}#))
                                              (#{quasicons\ 1873}#
                                                (#{quasicons\ 1873}#
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
                                                  (#{quasi\ 1876}#
                                                    (list #{p\ 1922}#)
                                                    (- #{lev\ 1914}# 1)))
                                                (#{quasi\ 1876}#
                                                  #{q\ 1923}#
                                                  #{lev\ 1914}#))))
                                          #{tmp\ 1921}#)
                                   ((lambda (#{tmp\ 1924}#)
                                      (if (if #{tmp\ 1924}#
                                            (apply (lambda (#{args\ 1925}#
                                                            #{q\ 1926}#)
                                                     (= #{lev\ 1914}# 0))
                                                   #{tmp\ 1924}#)
                                            #f)
                                        (apply (lambda (#{args\ 1927}#
                                                        #{q\ 1928}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 1913}#
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
                                                         #{args\ 1927}#)))
                                               #{tmp\ 1924}#)
                                        ((lambda (#{tmp\ 1929}#)
                                           (if #{tmp\ 1929}#
                                             (apply (lambda (#{p\ 1930}#)
                                                      (#{quasicons\ 1873}#
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
                                                        (#{quasi\ 1876}#
                                                          (list #{p\ 1930}#)
                                                          (+ #{lev\ 1914}#
                                                             1))))
                                                    #{tmp\ 1929}#)
                                             ((lambda (#{tmp\ 1931}#)
                                                (if #{tmp\ 1931}#
                                                  (apply (lambda (#{p\ 1932}#
                                                                  #{q\ 1933}#)
                                                           (#{quasicons\ 1873}#
                                                             (#{quasi\ 1876}#
                                                               #{p\ 1932}#
                                                               #{lev\ 1914}#)
                                                             (#{quasi\ 1876}#
                                                               #{q\ 1933}#
                                                               #{lev\ 1914}#)))
                                                         #{tmp\ 1931}#)
                                                  ((lambda (#{tmp\ 1934}#)
                                                     (if #{tmp\ 1934}#
                                                       (apply (lambda (#{x\ 1935}#)
                                                                (#{quasivector\ 1875}#
                                                                  (#{quasi\ 1876}#
                                                                    #{x\ 1935}#
                                                                    #{lev\ 1914}#)))
                                                              #{tmp\ 1934}#)
                                                       ((lambda (#{p\ 1937}#)
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
                                                                #{p\ 1937}#))
                                                        #{tmp\ 1915}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 1915}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 1915}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 1915}#
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
                                      #{tmp\ 1915}#
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
                                 #{tmp\ 1915}#
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
                            #{tmp\ 1915}#
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
                       #{tmp\ 1915}#
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
                  #{p\ 1913}#))))
      (lambda (#{x\ 1938}#)
        ((lambda (#{tmp\ 1939}#)
           ((lambda (#{tmp\ 1940}#)
              (if #{tmp\ 1940}#
                (apply (lambda (#{_\ 1941}# #{e\ 1942}#)
                         (#{quasi\ 1876}# #{e\ 1942}# 0))
                       #{tmp\ 1940}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1939}#)))
            ($sc-dispatch #{tmp\ 1939}# (quote (any any)))))
         #{x\ 1938}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1943}#)
      (letrec ((#{read-file\ 1944}#
                 (lambda (#{fn\ 1945}# #{k\ 1946}#)
                   (let ((#{p\ 1947}# (open-input-file #{fn\ 1945}#)))
                     (letrec ((#{f\ 1948}#
                                (lambda (#{x\ 1949}#)
                                  (if (eof-object? #{x\ 1949}#)
                                    (begin
                                      (close-input-port #{p\ 1947}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 1946}#
                                            #{x\ 1949}#)
                                          (#{f\ 1948}# (read #{p\ 1947}#)))))))
                       (#{f\ 1948}# (read #{p\ 1947}#)))))))
        ((lambda (#{tmp\ 1950}#)
           ((lambda (#{tmp\ 1951}#)
              (if #{tmp\ 1951}#
                (apply (lambda (#{k\ 1952}# #{filename\ 1953}#)
                         (let ((#{fn\ 1954}#
                                 (syntax->datum #{filename\ 1953}#)))
                           ((lambda (#{tmp\ 1955}#)
                              ((lambda (#{tmp\ 1956}#)
                                 (if #{tmp\ 1956}#
                                   (apply (lambda (#{exp\ 1957}#)
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
                                                  #{exp\ 1957}#))
                                          #{tmp\ 1956}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 1955}#)))
                               ($sc-dispatch #{tmp\ 1955}# (quote each-any))))
                            (#{read-file\ 1944}# #{fn\ 1954}# #{k\ 1952}#))))
                       #{tmp\ 1951}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1950}#)))
            ($sc-dispatch #{tmp\ 1950}# (quote (any any)))))
         #{x\ 1943}#)))))

(define include-from-path
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1959}#)
      ((lambda (#{tmp\ 1960}#)
         ((lambda (#{tmp\ 1961}#)
            (if #{tmp\ 1961}#
              (apply (lambda (#{k\ 1962}# #{filename\ 1963}#)
                       (let ((#{fn\ 1964}# (syntax->datum #{filename\ 1963}#)))
                         ((lambda (#{tmp\ 1965}#)
                            ((lambda (#{fn\ 1966}#)
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
                                     #{fn\ 1966}#))
                             #{tmp\ 1965}#))
                          (let ((#{t\ 1967}# (%search-load-path #{fn\ 1964}#)))
                            (if #{t\ 1967}#
                              #{t\ 1967}#
                              (syntax-violation
                                'include-from-path
                                "file not found in path"
                                #{x\ 1959}#
                                #{filename\ 1963}#))))))
                     #{tmp\ 1961}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1960}#)))
          ($sc-dispatch #{tmp\ 1960}# (quote (any any)))))
       #{x\ 1959}#))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1968}#)
      ((lambda (#{tmp\ 1969}#)
         ((lambda (#{tmp\ 1970}#)
            (if #{tmp\ 1970}#
              (apply (lambda (#{_\ 1971}# #{e\ 1972}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 1968}#))
                     #{tmp\ 1970}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1969}#)))
          ($sc-dispatch #{tmp\ 1969}# (quote (any any)))))
       #{x\ 1968}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1973}#)
      ((lambda (#{tmp\ 1974}#)
         ((lambda (#{tmp\ 1975}#)
            (if #{tmp\ 1975}#
              (apply (lambda (#{_\ 1976}# #{e\ 1977}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 1973}#))
                     #{tmp\ 1975}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1974}#)))
          ($sc-dispatch #{tmp\ 1974}# (quote (any any)))))
       #{x\ 1973}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 1978}#)
      ((lambda (#{tmp\ 1979}#)
         ((lambda (#{tmp\ 1980}#)
            (if #{tmp\ 1980}#
              (apply (lambda (#{_\ 1981}#
                              #{e\ 1982}#
                              #{m1\ 1983}#
                              #{m2\ 1984}#)
                       ((lambda (#{tmp\ 1985}#)
                          ((lambda (#{body\ 1986}#)
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
                                               #{e\ 1982}#))
                                   #{body\ 1986}#))
                           #{tmp\ 1985}#))
                        (letrec ((#{f\ 1987}#
                                   (lambda (#{clause\ 1988}# #{clauses\ 1989}#)
                                     (if (null? #{clauses\ 1989}#)
                                       ((lambda (#{tmp\ 1991}#)
                                          ((lambda (#{tmp\ 1992}#)
                                             (if #{tmp\ 1992}#
                                               (apply (lambda (#{e1\ 1993}#
                                                               #{e2\ 1994}#)
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
                                                              (cons #{e1\ 1993}#
                                                                    #{e2\ 1994}#)))
                                                      #{tmp\ 1992}#)
                                               ((lambda (#{tmp\ 1996}#)
                                                  (if #{tmp\ 1996}#
                                                    (apply (lambda (#{k\ 1997}#
                                                                    #{e1\ 1998}#
                                                                    #{e2\ 1999}#)
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
                                                                               #{k\ 1997}#))
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
                                                                         (cons #{e1\ 1998}#
                                                                               #{e2\ 1999}#))))
                                                           #{tmp\ 1996}#)
                                                    ((lambda (#{_\ 2002}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 1978}#
                                                         #{clause\ 1988}#))
                                                     #{tmp\ 1991}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 1991}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 1991}#
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
                                        #{clause\ 1988}#)
                                       ((lambda (#{tmp\ 2003}#)
                                          ((lambda (#{rest\ 2004}#)
                                             ((lambda (#{tmp\ 2005}#)
                                                ((lambda (#{tmp\ 2006}#)
                                                   (if #{tmp\ 2006}#
                                                     (apply (lambda (#{k\ 2007}#
                                                                     #{e1\ 2008}#
                                                                     #{e2\ 2009}#)
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
                                                                                #{k\ 2007}#))
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
                                                                          (cons #{e1\ 2008}#
                                                                                #{e2\ 2009}#))
                                                                    #{rest\ 2004}#))
                                                            #{tmp\ 2006}#)
                                                     ((lambda (#{_\ 2012}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 1978}#
                                                          #{clause\ 1988}#))
                                                      #{tmp\ 2005}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 2005}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 1988}#))
                                           #{tmp\ 2003}#))
                                        (#{f\ 1987}#
                                          (car #{clauses\ 1989}#)
                                          (cdr #{clauses\ 1989}#)))))))
                          (#{f\ 1987}# #{m1\ 1983}# #{m2\ 1984}#))))
                     #{tmp\ 1980}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1979}#)))
          ($sc-dispatch
            #{tmp\ 1979}#
            '(any any any . each-any))))
       #{x\ 1978}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2013}#)
      ((lambda (#{tmp\ 2014}#)
         ((lambda (#{tmp\ 2015}#)
            (if #{tmp\ 2015}#
              (apply (lambda (#{_\ 2016}# #{e\ 2017}#)
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
                                               #{e\ 2017}#))
                                   (list (cons #{_\ 2016}#
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
                                               (cons #{e\ 2017}#
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
                     #{tmp\ 2015}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2014}#)))
          ($sc-dispatch #{tmp\ 2014}# (quote (any any)))))
       #{x\ 2013}#))))

(define define*
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2018}#)
      ((lambda (#{tmp\ 2019}#)
         ((lambda (#{tmp\ 2020}#)
            (if #{tmp\ 2020}#
              (apply (lambda (#{dummy\ 2021}#
                              #{id\ 2022}#
                              #{args\ 2023}#
                              #{b0\ 2024}#
                              #{b1\ 2025}#)
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
                             #{id\ 2022}#
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
                                   (cons #{args\ 2023}#
                                         (cons #{b0\ 2024}# #{b1\ 2025}#)))))
                     #{tmp\ 2020}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2019}#)))
          ($sc-dispatch
            #{tmp\ 2019}#
            '(any (any . any) any . each-any))))
       #{x\ 2018}#))))

