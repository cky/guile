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
               (letrec ((#{expand-body\ 331}#
                          (lambda (#{req\ 332}#
                                   #{opt\ 333}#
                                   #{rest\ 334}#
                                   #{kw\ 335}#
                                   #{body\ 336}#
                                   #{vars\ 337}#
                                   #{r*\ 338}#
                                   #{w*\ 339}#
                                   #{inits\ 340}#)
                            ((lambda (#{tmp\ 341}#)
                               ((lambda (#{tmp\ 342}#)
                                  (if (if #{tmp\ 342}#
                                        (apply (lambda (#{docstring\ 343}#
                                                        #{e1\ 344}#
                                                        #{e2\ 345}#)
                                                 (string?
                                                   (syntax->datum
                                                     #{docstring\ 343}#)))
                                               #{tmp\ 342}#)
                                        #f)
                                    (apply (lambda (#{docstring\ 346}#
                                                    #{e1\ 347}#
                                                    #{e2\ 348}#)
                                             (values
                                               (syntax->datum
                                                 #{docstring\ 346}#)
                                               #{req\ 332}#
                                               #{opt\ 333}#
                                               #{rest\ 334}#
                                               #{kw\ 335}#
                                               #{inits\ 340}#
                                               #{vars\ 337}#
                                               (#{chi-body\ 171}#
                                                 (cons #{e1\ 347}# #{e2\ 348}#)
                                                 (#{source-wrap\ 160}#
                                                   #{e\ 321}#
                                                   #{w\ 323}#
                                                   #{s\ 324}#
                                                   #{mod\ 325}#)
                                                 #{r*\ 338}#
                                                 #{w*\ 339}#
                                                 #{mod\ 325}#)))
                                           #{tmp\ 342}#)
                                    ((lambda (#{tmp\ 350}#)
                                       (if #{tmp\ 350}#
                                         (apply (lambda (#{e1\ 351}#
                                                         #{e2\ 352}#)
                                                  (values
                                                    #f
                                                    #{req\ 332}#
                                                    #{opt\ 333}#
                                                    #{rest\ 334}#
                                                    #{kw\ 335}#
                                                    #{inits\ 340}#
                                                    #{vars\ 337}#
                                                    (#{chi-body\ 171}#
                                                      (cons #{e1\ 351}#
                                                            #{e2\ 352}#)
                                                      (#{source-wrap\ 160}#
                                                        #{e\ 321}#
                                                        #{w\ 323}#
                                                        #{s\ 324}#
                                                        #{mod\ 325}#)
                                                      #{r*\ 338}#
                                                      #{w*\ 339}#
                                                      #{mod\ 325}#)))
                                                #{tmp\ 350}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 341}#)))
                                     ($sc-dispatch
                                       #{tmp\ 341}#
                                       '(any . each-any)))))
                                ($sc-dispatch
                                  #{tmp\ 341}#
                                  '(any any . each-any))))
                             #{body\ 336}#)))
                        (#{expand-kw\ 330}#
                          (lambda (#{req\ 354}#
                                   #{opt\ 355}#
                                   #{rest\ 356}#
                                   #{kw\ 357}#
                                   #{body\ 358}#
                                   #{vars\ 359}#
                                   #{r*\ 360}#
                                   #{w*\ 361}#
                                   #{aok\ 362}#
                                   #{out\ 363}#
                                   #{inits\ 364}#)
                            (if (pair? #{kw\ 357}#)
                              ((lambda (#{tmp\ 365}#)
                                 ((lambda (#{tmp\ 366}#)
                                    (if #{tmp\ 366}#
                                      (apply (lambda (#{k\ 367}#
                                                      #{id\ 368}#
                                                      #{i\ 369}#)
                                               (let ((#{v\ 370}# (#{gen-var\ 181}#
                                                                   #{id\ 368}#)))
                                                 (let ((#{l\ 371}# (#{gen-labels\ 137}#
                                                                     (list #{v\ 370}#))))
                                                   (let ((#{r**\ 372}#
                                                           (#{extend-var-env\ 126}#
                                                             #{l\ 371}#
                                                             (list #{v\ 370}#)
                                                             #{r*\ 360}#)))
                                                     (let ((#{w**\ 373}#
                                                             (#{make-binding-wrap\ 148}#
                                                               (list #{id\ 368}#)
                                                               #{l\ 371}#
                                                               #{w*\ 361}#)))
                                                       (#{expand-kw\ 330}#
                                                         #{req\ 354}#
                                                         #{opt\ 355}#
                                                         #{rest\ 356}#
                                                         (cdr #{kw\ 357}#)
                                                         #{body\ 358}#
                                                         (cons #{v\ 370}#
                                                               #{vars\ 359}#)
                                                         #{r**\ 372}#
                                                         #{w**\ 373}#
                                                         #{aok\ 362}#
                                                         (cons (list (syntax->datum
                                                                       #{k\ 367}#)
                                                                     (syntax->datum
                                                                       #{id\ 368}#)
                                                                     #{v\ 370}#)
                                                               #{out\ 363}#)
                                                         (cons (#{chi\ 167}#
                                                                 #{i\ 369}#
                                                                 #{r*\ 360}#
                                                                 #{w*\ 361}#
                                                                 #{mod\ 325}#)
                                                               #{inits\ 364}#)))))))
                                             #{tmp\ 366}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 365}#)))
                                  ($sc-dispatch
                                    #{tmp\ 365}#
                                    '(any any any))))
                               (car #{kw\ 357}#))
                              (#{expand-body\ 331}#
                                #{req\ 354}#
                                #{opt\ 355}#
                                #{rest\ 356}#
                                (if (let ((#{t\ 374}# #{aok\ 362}#))
                                      (if #{t\ 374}#
                                        #{t\ 374}#
                                        (pair? #{out\ 363}#)))
                                  (cons #{aok\ 362}# (reverse #{out\ 363}#))
                                  #f)
                                #{body\ 358}#
                                (reverse #{vars\ 359}#)
                                #{r*\ 360}#
                                #{w*\ 361}#
                                (reverse #{inits\ 364}#)))))
                        (#{expand-opt\ 329}#
                          (lambda (#{req\ 375}#
                                   #{opt\ 376}#
                                   #{rest\ 377}#
                                   #{kw\ 378}#
                                   #{body\ 379}#
                                   #{vars\ 380}#
                                   #{r*\ 381}#
                                   #{w*\ 382}#
                                   #{out\ 383}#
                                   #{inits\ 384}#)
                            (if (pair? #{opt\ 376}#)
                              ((lambda (#{tmp\ 385}#)
                                 ((lambda (#{tmp\ 386}#)
                                    (if #{tmp\ 386}#
                                      (apply (lambda (#{id\ 387}# #{i\ 388}#)
                                               (let ((#{v\ 389}# (#{gen-var\ 181}#
                                                                   #{id\ 387}#)))
                                                 (let ((#{l\ 390}# (#{gen-labels\ 137}#
                                                                     (list #{v\ 389}#))))
                                                   (let ((#{r**\ 391}#
                                                           (#{extend-var-env\ 126}#
                                                             #{l\ 390}#
                                                             (list #{v\ 389}#)
                                                             #{r*\ 381}#)))
                                                     (let ((#{w**\ 392}#
                                                             (#{make-binding-wrap\ 148}#
                                                               (list #{id\ 387}#)
                                                               #{l\ 390}#
                                                               #{w*\ 382}#)))
                                                       (#{expand-opt\ 329}#
                                                         #{req\ 375}#
                                                         (cdr #{opt\ 376}#)
                                                         #{rest\ 377}#
                                                         #{kw\ 378}#
                                                         #{body\ 379}#
                                                         (cons #{v\ 389}#
                                                               #{vars\ 380}#)
                                                         #{r**\ 391}#
                                                         #{w**\ 392}#
                                                         (cons (syntax->datum
                                                                 #{id\ 387}#)
                                                               #{out\ 383}#)
                                                         (cons (#{chi\ 167}#
                                                                 #{i\ 388}#
                                                                 #{r*\ 381}#
                                                                 #{w*\ 382}#
                                                                 #{mod\ 325}#)
                                                               #{inits\ 384}#)))))))
                                             #{tmp\ 386}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 385}#)))
                                  ($sc-dispatch
                                    #{tmp\ 385}#
                                    '(any any))))
                               (car #{opt\ 376}#))
                              (if #{rest\ 377}#
                                (let ((#{v\ 393}# (#{gen-var\ 181}#
                                                    #{rest\ 377}#)))
                                  (let ((#{l\ 394}# (#{gen-labels\ 137}#
                                                      (list #{v\ 393}#))))
                                    (let ((#{r*\ 395}#
                                            (#{extend-var-env\ 126}#
                                              #{l\ 394}#
                                              (list #{v\ 393}#)
                                              #{r*\ 381}#)))
                                      (let ((#{w*\ 396}#
                                              (#{make-binding-wrap\ 148}#
                                                (list #{rest\ 377}#)
                                                #{l\ 394}#
                                                #{w*\ 382}#)))
                                        (#{expand-kw\ 330}#
                                          #{req\ 375}#
                                          (if (pair? #{out\ 383}#)
                                            (reverse #{out\ 383}#)
                                            #f)
                                          (syntax->datum #{rest\ 377}#)
                                          (if (pair? #{kw\ 378}#)
                                            (cdr #{kw\ 378}#)
                                            #{kw\ 378}#)
                                          #{body\ 379}#
                                          (cons #{v\ 393}# #{vars\ 380}#)
                                          #{r*\ 395}#
                                          #{w*\ 396}#
                                          (if (pair? #{kw\ 378}#)
                                            (car #{kw\ 378}#)
                                            #f)
                                          '()
                                          #{inits\ 384}#)))))
                                (#{expand-kw\ 330}#
                                  #{req\ 375}#
                                  (if (pair? #{out\ 383}#)
                                    (reverse #{out\ 383}#)
                                    #f)
                                  #f
                                  (if (pair? #{kw\ 378}#)
                                    (cdr #{kw\ 378}#)
                                    #{kw\ 378}#)
                                  #{body\ 379}#
                                  #{vars\ 380}#
                                  #{r*\ 381}#
                                  #{w*\ 382}#
                                  (if (pair? #{kw\ 378}#) (car #{kw\ 378}#) #f)
                                  '()
                                  #{inits\ 384}#)))))
                        (#{expand-req\ 328}#
                          (lambda (#{req\ 397}#
                                   #{opt\ 398}#
                                   #{rest\ 399}#
                                   #{kw\ 400}#
                                   #{body\ 401}#)
                            (let ((#{vars\ 402}#
                                    (map #{gen-var\ 181}# #{req\ 397}#))
                                  (#{labels\ 403}#
                                    (#{gen-labels\ 137}# #{req\ 397}#)))
                              (let ((#{r*\ 404}#
                                      (#{extend-var-env\ 126}#
                                        #{labels\ 403}#
                                        #{vars\ 402}#
                                        #{r\ 322}#))
                                    (#{w*\ 405}#
                                      (#{make-binding-wrap\ 148}#
                                        #{req\ 397}#
                                        #{labels\ 403}#
                                        #{w\ 323}#)))
                                (#{expand-opt\ 329}#
                                  (map syntax->datum #{req\ 397}#)
                                  #{opt\ 398}#
                                  #{rest\ 399}#
                                  #{kw\ 400}#
                                  #{body\ 401}#
                                  (reverse #{vars\ 402}#)
                                  #{r*\ 404}#
                                  #{w*\ 405}#
                                  '()
                                  '()))))))
                 ((lambda (#{tmp\ 406}#)
                    ((lambda (#{tmp\ 407}#)
                       (if #{tmp\ 407}#
                         (apply (lambda () (values #f #f)) #{tmp\ 407}#)
                         ((lambda (#{tmp\ 408}#)
                            (if #{tmp\ 408}#
                              (apply (lambda (#{args\ 409}#
                                              #{e1\ 410}#
                                              #{e2\ 411}#
                                              #{args*\ 412}#
                                              #{e1*\ 413}#
                                              #{e2*\ 414}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{get-formals\ 326}#
                                             #{args\ 409}#))
                                         (lambda (#{req\ 415}#
                                                  #{opt\ 416}#
                                                  #{rest\ 417}#
                                                  #{kw\ 418}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{expand-req\ 328}#
                                                 #{req\ 415}#
                                                 #{opt\ 416}#
                                                 #{rest\ 417}#
                                                 #{kw\ 418}#
                                                 (cons #{e1\ 410}#
                                                       #{e2\ 411}#)))
                                             (lambda (#{docstring\ 420}#
                                                      #{req\ 421}#
                                                      #{opt\ 422}#
                                                      #{rest\ 423}#
                                                      #{kw\ 424}#
                                                      #{inits\ 425}#
                                                      #{vars\ 426}#
                                                      #{body\ 427}#)
                                               (call-with-values
                                                 (lambda ()
                                                   (#{chi-lambda-case\ 179}#
                                                     #{e\ 321}#
                                                     #{r\ 322}#
                                                     #{w\ 323}#
                                                     #{s\ 324}#
                                                     #{mod\ 325}#
                                                     #{get-formals\ 326}#
                                                     (map (lambda (#{tmp\ 430}#
                                                                   #{tmp\ 429}#
                                                                   #{tmp\ 428}#)
                                                            (cons #{tmp\ 428}#
                                                                  (cons #{tmp\ 429}#
                                                                        #{tmp\ 430}#)))
                                                          #{e2*\ 414}#
                                                          #{e1*\ 413}#
                                                          #{args*\ 412}#)))
                                                 (lambda (#{docstring*\ 432}#
                                                          #{else*\ 433}#)
                                                   (values
                                                     (let ((#{t\ 434}# #{docstring\ 420}#))
                                                       (if #{t\ 434}#
                                                         #{t\ 434}#
                                                         #{docstring*\ 432}#))
                                                     (#{build-lambda-case\ 107}#
                                                       #{s\ 324}#
                                                       #{req\ 421}#
                                                       #{opt\ 422}#
                                                       #{rest\ 423}#
                                                       #{kw\ 424}#
                                                       #{inits\ 425}#
                                                       #{vars\ 426}#
                                                       #{body\ 427}#
                                                       #{else*\ 433}#)))))))))
                                     #{tmp\ 408}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 406}#)))
                          ($sc-dispatch
                            #{tmp\ 406}#
                            '((any any . each-any)
                              .
                              #(each (any any . each-any)))))))
                     ($sc-dispatch #{tmp\ 406}# (quote ()))))
                  #{clauses\ 327}#))))
           (#{lambda*-formals\ 178}#
             (lambda (#{orig-args\ 435}#)
               (letrec ((#{check\ 440}#
                          (lambda (#{req\ 441}#
                                   #{opt\ 442}#
                                   #{rest\ 443}#
                                   #{kw\ 444}#)
                            (if (#{distinct-bound-ids?\ 157}#
                                  (append
                                    #{req\ 441}#
                                    (map car #{opt\ 442}#)
                                    (if #{rest\ 443}#
                                      (list #{rest\ 443}#)
                                      '())
                                    (if (pair? #{kw\ 444}#)
                                      (map cadr (cdr #{kw\ 444}#))
                                      '())))
                              (values
                                #{req\ 441}#
                                #{opt\ 442}#
                                #{rest\ 443}#
                                #{kw\ 444}#)
                              (syntax-violation
                                'lambda*
                                "duplicate identifier in argument list"
                                #{orig-args\ 435}#))))
                        (#{rest\ 439}#
                          (lambda (#{args\ 445}#
                                   #{req\ 446}#
                                   #{opt\ 447}#
                                   #{kw\ 448}#)
                            ((lambda (#{tmp\ 449}#)
                               ((lambda (#{tmp\ 450}#)
                                  (if (if #{tmp\ 450}#
                                        (apply (lambda (#{r\ 451}#)
                                                 (#{id?\ 131}# #{r\ 451}#))
                                               #{tmp\ 450}#)
                                        #f)
                                    (apply (lambda (#{r\ 452}#)
                                             (#{check\ 440}#
                                               #{req\ 446}#
                                               #{opt\ 447}#
                                               #{r\ 452}#
                                               #{kw\ 448}#))
                                           #{tmp\ 450}#)
                                    ((lambda (#{else\ 453}#)
                                       (syntax-violation
                                         'lambda*
                                         "invalid rest argument"
                                         #{orig-args\ 435}#
                                         #{args\ 445}#))
                                     #{tmp\ 449}#)))
                                (list #{tmp\ 449}#)))
                             #{args\ 445}#)))
                        (#{key\ 438}#
                          (lambda (#{args\ 454}#
                                   #{req\ 455}#
                                   #{opt\ 456}#
                                   #{rkey\ 457}#)
                            ((lambda (#{tmp\ 458}#)
                               ((lambda (#{tmp\ 459}#)
                                  (if #{tmp\ 459}#
                                    (apply (lambda ()
                                             (#{check\ 440}#
                                               #{req\ 455}#
                                               #{opt\ 456}#
                                               #f
                                               (cons #f
                                                     (reverse #{rkey\ 457}#))))
                                           #{tmp\ 459}#)
                                    ((lambda (#{tmp\ 460}#)
                                       (if (if #{tmp\ 460}#
                                             (apply (lambda (#{a\ 461}#
                                                             #{b\ 462}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 461}#))
                                                    #{tmp\ 460}#)
                                             #f)
                                         (apply (lambda (#{a\ 463}# #{b\ 464}#)
                                                  ((lambda (#{tmp\ 465}#)
                                                     ((lambda (#{k\ 466}#)
                                                        (#{key\ 438}#
                                                          #{b\ 464}#
                                                          #{req\ 455}#
                                                          #{opt\ 456}#
                                                          (cons (cons #{k\ 466}#
                                                                      (cons #{a\ 463}#
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
                                                                #{rkey\ 457}#)))
                                                      #{tmp\ 465}#))
                                                   (symbol->keyword
                                                     (syntax->datum
                                                       #{a\ 463}#))))
                                                #{tmp\ 460}#)
                                         ((lambda (#{tmp\ 467}#)
                                            (if (if #{tmp\ 467}#
                                                  (apply (lambda (#{a\ 468}#
                                                                  #{init\ 469}#
                                                                  #{b\ 470}#)
                                                           (#{id?\ 131}#
                                                             #{a\ 468}#))
                                                         #{tmp\ 467}#)
                                                  #f)
                                              (apply (lambda (#{a\ 471}#
                                                              #{init\ 472}#
                                                              #{b\ 473}#)
                                                       ((lambda (#{tmp\ 474}#)
                                                          ((lambda (#{k\ 475}#)
                                                             (#{key\ 438}#
                                                               #{b\ 473}#
                                                               #{req\ 455}#
                                                               #{opt\ 456}#
                                                               (cons (list #{k\ 475}#
                                                                           #{a\ 471}#
                                                                           #{init\ 472}#)
                                                                     #{rkey\ 457}#)))
                                                           #{tmp\ 474}#))
                                                        (symbol->keyword
                                                          (syntax->datum
                                                            #{a\ 471}#))))
                                                     #{tmp\ 467}#)
                                              ((lambda (#{tmp\ 476}#)
                                                 (if (if #{tmp\ 476}#
                                                       (apply (lambda (#{a\ 477}#
                                                                       #{init\ 478}#
                                                                       #{k\ 479}#
                                                                       #{b\ 480}#)
                                                                (if (#{id?\ 131}#
                                                                      #{a\ 477}#)
                                                                  (keyword?
                                                                    (syntax->datum
                                                                      #{k\ 479}#))
                                                                  #f))
                                                              #{tmp\ 476}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 481}#
                                                                   #{init\ 482}#
                                                                   #{k\ 483}#
                                                                   #{b\ 484}#)
                                                            (#{key\ 438}#
                                                              #{b\ 484}#
                                                              #{req\ 455}#
                                                              #{opt\ 456}#
                                                              (cons (list #{k\ 483}#
                                                                          #{a\ 481}#
                                                                          #{init\ 482}#)
                                                                    #{rkey\ 457}#)))
                                                          #{tmp\ 476}#)
                                                   ((lambda (#{tmp\ 485}#)
                                                      (if (if #{tmp\ 485}#
                                                            (apply (lambda (#{aok\ 486}#)
                                                                     (eq? (syntax->datum
                                                                            #{aok\ 486}#)
                                                                          #:allow-other-keys))
                                                                   #{tmp\ 485}#)
                                                            #f)
                                                        (apply (lambda (#{aok\ 487}#)
                                                                 (#{check\ 440}#
                                                                   #{req\ 455}#
                                                                   #{opt\ 456}#
                                                                   #f
                                                                   (cons #t
                                                                         (reverse
                                                                           #{rkey\ 457}#))))
                                                               #{tmp\ 485}#)
                                                        ((lambda (#{tmp\ 488}#)
                                                           (if (if #{tmp\ 488}#
                                                                 (apply (lambda (#{aok\ 489}#
                                                                                 #{a\ 490}#
                                                                                 #{b\ 491}#)
                                                                          (if (eq? (syntax->datum
                                                                                     #{aok\ 489}#)
                                                                                   #:allow-other-keys)
                                                                            (eq? (syntax->datum
                                                                                   #{a\ 490}#)
                                                                                 #:rest)
                                                                            #f))
                                                                        #{tmp\ 488}#)
                                                                 #f)
                                                             (apply (lambda (#{aok\ 492}#
                                                                             #{a\ 493}#
                                                                             #{b\ 494}#)
                                                                      (#{rest\ 439}#
                                                                        #{b\ 494}#
                                                                        #{req\ 455}#
                                                                        #{opt\ 456}#
                                                                        (cons #t
                                                                              (reverse
                                                                                #{rkey\ 457}#))))
                                                                    #{tmp\ 488}#)
                                                             ((lambda (#{tmp\ 495}#)
                                                                (if (if #{tmp\ 495}#
                                                                      (apply (lambda (#{aok\ 496}#
                                                                                      #{r\ 497}#)
                                                                               (if (eq? (syntax->datum
                                                                                          #{aok\ 496}#)
                                                                                        #:allow-other-keys)
                                                                                 (#{id?\ 131}#
                                                                                   #{r\ 497}#)
                                                                                 #f))
                                                                             #{tmp\ 495}#)
                                                                      #f)
                                                                  (apply (lambda (#{aok\ 498}#
                                                                                  #{r\ 499}#)
                                                                           (#{rest\ 439}#
                                                                             #{r\ 499}#
                                                                             #{req\ 455}#
                                                                             #{opt\ 456}#
                                                                             (cons #t
                                                                                   (reverse
                                                                                     #{rkey\ 457}#))))
                                                                         #{tmp\ 495}#)
                                                                  ((lambda (#{tmp\ 500}#)
                                                                     (if (if #{tmp\ 500}#
                                                                           (apply (lambda (#{a\ 501}#
                                                                                           #{b\ 502}#)
                                                                                    (eq? (syntax->datum
                                                                                           #{a\ 501}#)
                                                                                         #:rest))
                                                                                  #{tmp\ 500}#)
                                                                           #f)
                                                                       (apply (lambda (#{a\ 503}#
                                                                                       #{b\ 504}#)
                                                                                (#{rest\ 439}#
                                                                                  #{b\ 504}#
                                                                                  #{req\ 455}#
                                                                                  #{opt\ 456}#
                                                                                  (cons #f
                                                                                        (reverse
                                                                                          #{rkey\ 457}#))))
                                                                              #{tmp\ 500}#)
                                                                       ((lambda (#{tmp\ 505}#)
                                                                          (if (if #{tmp\ 505}#
                                                                                (apply (lambda (#{r\ 506}#)
                                                                                         (#{id?\ 131}#
                                                                                           #{r\ 506}#))
                                                                                       #{tmp\ 505}#)
                                                                                #f)
                                                                            (apply (lambda (#{r\ 507}#)
                                                                                     (#{rest\ 439}#
                                                                                       #{r\ 507}#
                                                                                       #{req\ 455}#
                                                                                       #{opt\ 456}#
                                                                                       (cons #f
                                                                                             (reverse
                                                                                               #{rkey\ 457}#))))
                                                                                   #{tmp\ 505}#)
                                                                            ((lambda (#{else\ 508}#)
                                                                               (syntax-violation
                                                                                 'lambda*
                                                                                 "invalid keyword argument list"
                                                                                 #{orig-args\ 435}#
                                                                                 #{args\ 454}#))
                                                                             #{tmp\ 458}#)))
                                                                        (list #{tmp\ 458}#))))
                                                                   ($sc-dispatch
                                                                     #{tmp\ 458}#
                                                                     '(any any)))))
                                                              ($sc-dispatch
                                                                #{tmp\ 458}#
                                                                '(any .
                                                                      any)))))
                                                         ($sc-dispatch
                                                           #{tmp\ 458}#
                                                           '(any any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 458}#
                                                      '(any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 458}#
                                                 '((any any any) . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 458}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 458}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 458}# (quote ()))))
                             #{args\ 454}#)))
                        (#{opt\ 437}#
                          (lambda (#{args\ 509}# #{req\ 510}# #{ropt\ 511}#)
                            ((lambda (#{tmp\ 512}#)
                               ((lambda (#{tmp\ 513}#)
                                  (if #{tmp\ 513}#
                                    (apply (lambda ()
                                             (#{check\ 440}#
                                               #{req\ 510}#
                                               (reverse #{ropt\ 511}#)
                                               #f
                                               '()))
                                           #{tmp\ 513}#)
                                    ((lambda (#{tmp\ 514}#)
                                       (if (if #{tmp\ 514}#
                                             (apply (lambda (#{a\ 515}#
                                                             #{b\ 516}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 515}#))
                                                    #{tmp\ 514}#)
                                             #f)
                                         (apply (lambda (#{a\ 517}# #{b\ 518}#)
                                                  (#{opt\ 437}#
                                                    #{b\ 518}#
                                                    #{req\ 510}#
                                                    (cons (cons #{a\ 517}#
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
                                                          #{ropt\ 511}#)))
                                                #{tmp\ 514}#)
                                         ((lambda (#{tmp\ 519}#)
                                            (if (if #{tmp\ 519}#
                                                  (apply (lambda (#{a\ 520}#
                                                                  #{init\ 521}#
                                                                  #{b\ 522}#)
                                                           (#{id?\ 131}#
                                                             #{a\ 520}#))
                                                         #{tmp\ 519}#)
                                                  #f)
                                              (apply (lambda (#{a\ 523}#
                                                              #{init\ 524}#
                                                              #{b\ 525}#)
                                                       (#{opt\ 437}#
                                                         #{b\ 525}#
                                                         #{req\ 510}#
                                                         (cons (list #{a\ 523}#
                                                                     #{init\ 524}#)
                                                               #{ropt\ 511}#)))
                                                     #{tmp\ 519}#)
                                              ((lambda (#{tmp\ 526}#)
                                                 (if (if #{tmp\ 526}#
                                                       (apply (lambda (#{a\ 527}#
                                                                       #{b\ 528}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 527}#)
                                                                     #:key))
                                                              #{tmp\ 526}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 529}#
                                                                   #{b\ 530}#)
                                                            (#{key\ 438}#
                                                              #{b\ 530}#
                                                              #{req\ 510}#
                                                              (reverse
                                                                #{ropt\ 511}#)
                                                              '()))
                                                          #{tmp\ 526}#)
                                                   ((lambda (#{tmp\ 531}#)
                                                      (if (if #{tmp\ 531}#
                                                            (apply (lambda (#{a\ 532}#
                                                                            #{b\ 533}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 532}#)
                                                                          #:rest))
                                                                   #{tmp\ 531}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 534}#
                                                                        #{b\ 535}#)
                                                                 (#{rest\ 439}#
                                                                   #{b\ 535}#
                                                                   #{req\ 510}#
                                                                   (reverse
                                                                     #{ropt\ 511}#)
                                                                   '()))
                                                               #{tmp\ 531}#)
                                                        ((lambda (#{tmp\ 536}#)
                                                           (if (if #{tmp\ 536}#
                                                                 (apply (lambda (#{r\ 537}#)
                                                                          (#{id?\ 131}#
                                                                            #{r\ 537}#))
                                                                        #{tmp\ 536}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 538}#)
                                                                      (#{rest\ 439}#
                                                                        #{r\ 538}#
                                                                        #{req\ 510}#
                                                                        (reverse
                                                                          #{ropt\ 511}#)
                                                                        '()))
                                                                    #{tmp\ 536}#)
                                                             ((lambda (#{else\ 539}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid optional argument list"
                                                                  #{orig-args\ 435}#
                                                                  #{args\ 509}#))
                                                              #{tmp\ 512}#)))
                                                         (list #{tmp\ 512}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 512}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 512}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 512}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 512}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 512}# (quote ()))))
                             #{args\ 509}#)))
                        (#{req\ 436}#
                          (lambda (#{args\ 540}# #{rreq\ 541}#)
                            ((lambda (#{tmp\ 542}#)
                               ((lambda (#{tmp\ 543}#)
                                  (if #{tmp\ 543}#
                                    (apply (lambda ()
                                             (#{check\ 440}#
                                               (reverse #{rreq\ 541}#)
                                               '()
                                               #f
                                               '()))
                                           #{tmp\ 543}#)
                                    ((lambda (#{tmp\ 544}#)
                                       (if (if #{tmp\ 544}#
                                             (apply (lambda (#{a\ 545}#
                                                             #{b\ 546}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 545}#))
                                                    #{tmp\ 544}#)
                                             #f)
                                         (apply (lambda (#{a\ 547}# #{b\ 548}#)
                                                  (#{req\ 436}#
                                                    #{b\ 548}#
                                                    (cons #{a\ 547}#
                                                          #{rreq\ 541}#)))
                                                #{tmp\ 544}#)
                                         ((lambda (#{tmp\ 549}#)
                                            (if (if #{tmp\ 549}#
                                                  (apply (lambda (#{a\ 550}#
                                                                  #{b\ 551}#)
                                                           (eq? (syntax->datum
                                                                  #{a\ 550}#)
                                                                #:optional))
                                                         #{tmp\ 549}#)
                                                  #f)
                                              (apply (lambda (#{a\ 552}#
                                                              #{b\ 553}#)
                                                       (#{opt\ 437}#
                                                         #{b\ 553}#
                                                         (reverse
                                                           #{rreq\ 541}#)
                                                         '()))
                                                     #{tmp\ 549}#)
                                              ((lambda (#{tmp\ 554}#)
                                                 (if (if #{tmp\ 554}#
                                                       (apply (lambda (#{a\ 555}#
                                                                       #{b\ 556}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 555}#)
                                                                     #:key))
                                                              #{tmp\ 554}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 557}#
                                                                   #{b\ 558}#)
                                                            (#{key\ 438}#
                                                              #{b\ 558}#
                                                              (reverse
                                                                #{rreq\ 541}#)
                                                              '()
                                                              '()))
                                                          #{tmp\ 554}#)
                                                   ((lambda (#{tmp\ 559}#)
                                                      (if (if #{tmp\ 559}#
                                                            (apply (lambda (#{a\ 560}#
                                                                            #{b\ 561}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 560}#)
                                                                          #:rest))
                                                                   #{tmp\ 559}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 562}#
                                                                        #{b\ 563}#)
                                                                 (#{rest\ 439}#
                                                                   #{b\ 563}#
                                                                   (reverse
                                                                     #{rreq\ 541}#)
                                                                   '()
                                                                   '()))
                                                               #{tmp\ 559}#)
                                                        ((lambda (#{tmp\ 564}#)
                                                           (if (if #{tmp\ 564}#
                                                                 (apply (lambda (#{r\ 565}#)
                                                                          (#{id?\ 131}#
                                                                            #{r\ 565}#))
                                                                        #{tmp\ 564}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 566}#)
                                                                      (#{rest\ 439}#
                                                                        #{r\ 566}#
                                                                        (reverse
                                                                          #{rreq\ 541}#)
                                                                        '()
                                                                        '()))
                                                                    #{tmp\ 564}#)
                                                             ((lambda (#{else\ 567}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid argument list"
                                                                  #{orig-args\ 435}#
                                                                  #{args\ 540}#))
                                                              #{tmp\ 542}#)))
                                                         (list #{tmp\ 542}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 542}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 542}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 542}#
                                            '(any . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 542}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 542}# (quote ()))))
                             #{args\ 540}#))))
                 (#{req\ 436}# #{orig-args\ 435}# (quote ())))))
           (#{chi-simple-lambda\ 177}#
             (lambda (#{e\ 568}#
                      #{r\ 569}#
                      #{w\ 570}#
                      #{s\ 571}#
                      #{mod\ 572}#
                      #{req\ 573}#
                      #{rest\ 574}#
                      #{docstring\ 575}#
                      #{body\ 576}#)
               (let ((#{ids\ 577}#
                       (if #{rest\ 574}#
                         (append #{req\ 573}# (list #{rest\ 574}#))
                         #{req\ 573}#)))
                 (let ((#{vars\ 578}#
                         (map #{gen-var\ 181}# #{ids\ 577}#)))
                   (let ((#{labels\ 579}#
                           (#{gen-labels\ 137}# #{ids\ 577}#)))
                     (#{build-simple-lambda\ 105}#
                       #{s\ 571}#
                       (map syntax->datum #{req\ 573}#)
                       (if #{rest\ 574}#
                         (syntax->datum #{rest\ 574}#)
                         #f)
                       #{vars\ 578}#
                       #{docstring\ 575}#
                       (#{chi-body\ 171}#
                         #{body\ 576}#
                         (#{source-wrap\ 160}#
                           #{e\ 568}#
                           #{w\ 570}#
                           #{s\ 571}#
                           #{mod\ 572}#)
                         (#{extend-var-env\ 126}#
                           #{labels\ 579}#
                           #{vars\ 578}#
                           #{r\ 569}#)
                         (#{make-binding-wrap\ 148}#
                           #{ids\ 577}#
                           #{labels\ 579}#
                           #{w\ 570}#)
                         #{mod\ 572}#)))))))
           (#{lambda-formals\ 176}#
             (lambda (#{orig-args\ 580}#)
               (letrec ((#{check\ 582}#
                          (lambda (#{req\ 583}# #{rest\ 584}#)
                            (if (#{distinct-bound-ids?\ 157}#
                                  (if #{rest\ 584}#
                                    (cons #{rest\ 584}# #{req\ 583}#)
                                    #{req\ 583}#))
                              (values #{req\ 583}# #f #{rest\ 584}# #f)
                              (syntax-violation
                                'lambda
                                "duplicate identifier in argument list"
                                #{orig-args\ 580}#))))
                        (#{req\ 581}#
                          (lambda (#{args\ 585}# #{rreq\ 586}#)
                            ((lambda (#{tmp\ 587}#)
                               ((lambda (#{tmp\ 588}#)
                                  (if #{tmp\ 588}#
                                    (apply (lambda ()
                                             (#{check\ 582}#
                                               (reverse #{rreq\ 586}#)
                                               #f))
                                           #{tmp\ 588}#)
                                    ((lambda (#{tmp\ 589}#)
                                       (if (if #{tmp\ 589}#
                                             (apply (lambda (#{a\ 590}#
                                                             #{b\ 591}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 590}#))
                                                    #{tmp\ 589}#)
                                             #f)
                                         (apply (lambda (#{a\ 592}# #{b\ 593}#)
                                                  (#{req\ 581}#
                                                    #{b\ 593}#
                                                    (cons #{a\ 592}#
                                                          #{rreq\ 586}#)))
                                                #{tmp\ 589}#)
                                         ((lambda (#{tmp\ 594}#)
                                            (if (if #{tmp\ 594}#
                                                  (apply (lambda (#{r\ 595}#)
                                                           (#{id?\ 131}#
                                                             #{r\ 595}#))
                                                         #{tmp\ 594}#)
                                                  #f)
                                              (apply (lambda (#{r\ 596}#)
                                                       (#{check\ 582}#
                                                         (reverse
                                                           #{rreq\ 586}#)
                                                         #{r\ 596}#))
                                                     #{tmp\ 594}#)
                                              ((lambda (#{else\ 597}#)
                                                 (syntax-violation
                                                   'lambda
                                                   "invalid argument list"
                                                   #{orig-args\ 580}#
                                                   #{args\ 585}#))
                                               #{tmp\ 587}#)))
                                          (list #{tmp\ 587}#))))
                                     ($sc-dispatch
                                       #{tmp\ 587}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 587}# (quote ()))))
                             #{args\ 585}#))))
                 (#{req\ 581}# #{orig-args\ 580}# (quote ())))))
           (#{ellipsis?\ 175}#
             (lambda (#{x\ 598}#)
               (if (#{nonsymbol-id?\ 130}# #{x\ 598}#)
                 (#{free-id=?\ 154}#
                   #{x\ 598}#
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
             (lambda (#{expanded\ 599}# #{mod\ 600}#)
               (let ((#{p\ 601}# (#{local-eval-hook\ 91}#
                                   #{expanded\ 599}#
                                   #{mod\ 600}#)))
                 (if (procedure? #{p\ 601}#)
                   #{p\ 601}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 601}#)))))
           (#{chi-local-syntax\ 172}#
             (lambda (#{rec?\ 602}#
                      #{e\ 603}#
                      #{r\ 604}#
                      #{w\ 605}#
                      #{s\ 606}#
                      #{mod\ 607}#
                      #{k\ 608}#)
               ((lambda (#{tmp\ 609}#)
                  ((lambda (#{tmp\ 610}#)
                     (if #{tmp\ 610}#
                       (apply (lambda (#{_\ 611}#
                                       #{id\ 612}#
                                       #{val\ 613}#
                                       #{e1\ 614}#
                                       #{e2\ 615}#)
                                (let ((#{ids\ 616}# #{id\ 612}#))
                                  (if (not (#{valid-bound-ids?\ 156}#
                                             #{ids\ 616}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 603}#)
                                    (let ((#{labels\ 618}#
                                            (#{gen-labels\ 137}#
                                              #{ids\ 616}#)))
                                      (let ((#{new-w\ 619}#
                                              (#{make-binding-wrap\ 148}#
                                                #{ids\ 616}#
                                                #{labels\ 618}#
                                                #{w\ 605}#)))
                                        (#{k\ 608}# (cons #{e1\ 614}#
                                                          #{e2\ 615}#)
                                                    (#{extend-env\ 125}#
                                                      #{labels\ 618}#
                                                      (let ((#{w\ 621}# (if #{rec?\ 602}#
                                                                          #{new-w\ 619}#
                                                                          #{w\ 605}#))
                                                            (#{trans-r\ 622}#
                                                              (#{macros-only-env\ 127}#
                                                                #{r\ 604}#)))
                                                        (map (lambda (#{x\ 623}#)
                                                               (cons 'macro
                                                                     (#{eval-local-transformer\ 173}#
                                                                       (#{chi\ 167}#
                                                                         #{x\ 623}#
                                                                         #{trans-r\ 622}#
                                                                         #{w\ 621}#
                                                                         #{mod\ 607}#)
                                                                       #{mod\ 607}#)))
                                                             #{val\ 613}#))
                                                      #{r\ 604}#)
                                                    #{new-w\ 619}#
                                                    #{s\ 606}#
                                                    #{mod\ 607}#))))))
                              #{tmp\ 610}#)
                       ((lambda (#{_\ 625}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 160}#
                              #{e\ 603}#
                              #{w\ 605}#
                              #{s\ 606}#
                              #{mod\ 607}#)))
                        #{tmp\ 609}#)))
                   ($sc-dispatch
                     #{tmp\ 609}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 603}#)))
           (#{chi-body\ 171}#
             (lambda (#{body\ 626}#
                      #{outer-form\ 627}#
                      #{r\ 628}#
                      #{w\ 629}#
                      #{mod\ 630}#)
               (let ((#{r\ 631}# (cons '("placeholder" placeholder)
                                       #{r\ 628}#)))
                 (let ((#{ribcage\ 632}#
                         (#{make-ribcage\ 138}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 633}# (#{make-wrap\ 133}#
                                       (#{wrap-marks\ 134}# #{w\ 629}#)
                                       (cons #{ribcage\ 632}#
                                             (#{wrap-subst\ 135}#
                                               #{w\ 629}#)))))
                     (letrec ((#{parse\ 634}#
                                (lambda (#{body\ 635}#
                                         #{ids\ 636}#
                                         #{labels\ 637}#
                                         #{var-ids\ 638}#
                                         #{vars\ 639}#
                                         #{vals\ 640}#
                                         #{bindings\ 641}#)
                                  (if (null? #{body\ 635}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 627}#)
                                    (let ((#{e\ 643}# (cdar #{body\ 635}#))
                                          (#{er\ 644}# (caar #{body\ 635}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 165}#
                                            #{e\ 643}#
                                            #{er\ 644}#
                                            '(())
                                            (#{source-annotation\ 122}#
                                              #{er\ 644}#)
                                            #{ribcage\ 632}#
                                            #{mod\ 630}#
                                            #f))
                                        (lambda (#{type\ 645}#
                                                 #{value\ 646}#
                                                 #{e\ 647}#
                                                 #{w\ 648}#
                                                 #{s\ 649}#
                                                 #{mod\ 650}#)
                                          (if (memv #{type\ 645}#
                                                    '(define-form))
                                            (let ((#{id\ 651}#
                                                    (#{wrap\ 159}#
                                                      #{value\ 646}#
                                                      #{w\ 648}#
                                                      #{mod\ 650}#))
                                                  (#{label\ 652}#
                                                    (#{gen-label\ 136}#)))
                                              (let ((#{var\ 653}#
                                                      (#{gen-var\ 181}#
                                                        #{id\ 651}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 632}#
                                                    #{id\ 651}#
                                                    #{label\ 652}#)
                                                  (#{parse\ 634}#
                                                    (cdr #{body\ 635}#)
                                                    (cons #{id\ 651}#
                                                          #{ids\ 636}#)
                                                    (cons #{label\ 652}#
                                                          #{labels\ 637}#)
                                                    (cons #{id\ 651}#
                                                          #{var-ids\ 638}#)
                                                    (cons #{var\ 653}#
                                                          #{vars\ 639}#)
                                                    (cons (cons #{er\ 644}#
                                                                (#{wrap\ 159}#
                                                                  #{e\ 647}#
                                                                  #{w\ 648}#
                                                                  #{mod\ 650}#))
                                                          #{vals\ 640}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 653}#)
                                                          #{bindings\ 641}#)))))
                                            (if (memv #{type\ 645}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 654}#
                                                      (#{wrap\ 159}#
                                                        #{value\ 646}#
                                                        #{w\ 648}#
                                                        #{mod\ 650}#))
                                                    (#{label\ 655}#
                                                      (#{gen-label\ 136}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 632}#
                                                    #{id\ 654}#
                                                    #{label\ 655}#)
                                                  (#{parse\ 634}#
                                                    (cdr #{body\ 635}#)
                                                    (cons #{id\ 654}#
                                                          #{ids\ 636}#)
                                                    (cons #{label\ 655}#
                                                          #{labels\ 637}#)
                                                    #{var-ids\ 638}#
                                                    #{vars\ 639}#
                                                    #{vals\ 640}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 644}#
                                                                      (#{wrap\ 159}#
                                                                        #{e\ 647}#
                                                                        #{w\ 648}#
                                                                        #{mod\ 650}#)))
                                                          #{bindings\ 641}#))))
                                              (if (memv #{type\ 645}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 656}#)
                                                   ((lambda (#{tmp\ 657}#)
                                                      (if #{tmp\ 657}#
                                                        (apply (lambda (#{_\ 658}#
                                                                        #{e1\ 659}#)
                                                                 (#{parse\ 634}#
                                                                   (letrec ((#{f\ 660}# (lambda (#{forms\ 661}#)
                                                                                          (if (null? #{forms\ 661}#)
                                                                                            (cdr #{body\ 635}#)
                                                                                            (cons (cons #{er\ 644}#
                                                                                                        (#{wrap\ 159}#
                                                                                                          (car #{forms\ 661}#)
                                                                                                          #{w\ 648}#
                                                                                                          #{mod\ 650}#))
                                                                                                  (#{f\ 660}# (cdr #{forms\ 661}#)))))))
                                                                     (#{f\ 660}# #{e1\ 659}#))
                                                                   #{ids\ 636}#
                                                                   #{labels\ 637}#
                                                                   #{var-ids\ 638}#
                                                                   #{vars\ 639}#
                                                                   #{vals\ 640}#
                                                                   #{bindings\ 641}#))
                                                               #{tmp\ 657}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 656}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 656}#
                                                      '(any . each-any))))
                                                 #{e\ 647}#)
                                                (if (memv #{type\ 645}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 172}#
                                                    #{value\ 646}#
                                                    #{e\ 647}#
                                                    #{er\ 644}#
                                                    #{w\ 648}#
                                                    #{s\ 649}#
                                                    #{mod\ 650}#
                                                    (lambda (#{forms\ 663}#
                                                             #{er\ 664}#
                                                             #{w\ 665}#
                                                             #{s\ 666}#
                                                             #{mod\ 667}#)
                                                      (#{parse\ 634}#
                                                        (letrec ((#{f\ 668}# (lambda (#{forms\ 669}#)
                                                                               (if (null? #{forms\ 669}#)
                                                                                 (cdr #{body\ 635}#)
                                                                                 (cons (cons #{er\ 664}#
                                                                                             (#{wrap\ 159}#
                                                                                               (car #{forms\ 669}#)
                                                                                               #{w\ 665}#
                                                                                               #{mod\ 667}#))
                                                                                       (#{f\ 668}# (cdr #{forms\ 669}#)))))))
                                                          (#{f\ 668}# #{forms\ 663}#))
                                                        #{ids\ 636}#
                                                        #{labels\ 637}#
                                                        #{var-ids\ 638}#
                                                        #{vars\ 639}#
                                                        #{vals\ 640}#
                                                        #{bindings\ 641}#)))
                                                  (if (null? #{ids\ 636}#)
                                                    (#{build-sequence\ 110}#
                                                      #f
                                                      (map (lambda (#{x\ 670}#)
                                                             (#{chi\ 167}#
                                                               (cdr #{x\ 670}#)
                                                               (car #{x\ 670}#)
                                                               '(())
                                                               #{mod\ 650}#))
                                                           (cons (cons #{er\ 644}#
                                                                       (#{source-wrap\ 160}#
                                                                         #{e\ 647}#
                                                                         #{w\ 648}#
                                                                         #{s\ 649}#
                                                                         #{mod\ 650}#))
                                                                 (cdr #{body\ 635}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 156}#
                                                                 #{ids\ 636}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 627}#))
                                                      (letrec ((#{loop\ 671}#
                                                                 (lambda (#{bs\ 672}#
                                                                          #{er-cache\ 673}#
                                                                          #{r-cache\ 674}#)
                                                                   (if (not (null? #{bs\ 672}#))
                                                                     (let ((#{b\ 675}# (car #{bs\ 672}#)))
                                                                       (if (eq? (car #{b\ 675}#)
                                                                                'macro)
                                                                         (let ((#{er\ 676}#
                                                                                 (cadr #{b\ 675}#)))
                                                                           (let ((#{r-cache\ 677}#
                                                                                   (if (eq? #{er\ 676}#
                                                                                            #{er-cache\ 673}#)
                                                                                     #{r-cache\ 674}#
                                                                                     (#{macros-only-env\ 127}#
                                                                                       #{er\ 676}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 675}#
                                                                                 (#{eval-local-transformer\ 173}#
                                                                                   (#{chi\ 167}#
                                                                                     (cddr #{b\ 675}#)
                                                                                     #{r-cache\ 677}#
                                                                                     '(())
                                                                                     #{mod\ 650}#)
                                                                                   #{mod\ 650}#))
                                                                               (#{loop\ 671}#
                                                                                 (cdr #{bs\ 672}#)
                                                                                 #{er\ 676}#
                                                                                 #{r-cache\ 677}#))))
                                                                         (#{loop\ 671}#
                                                                           (cdr #{bs\ 672}#)
                                                                           #{er-cache\ 673}#
                                                                           #{r-cache\ 674}#)))))))
                                                        (#{loop\ 671}#
                                                          #{bindings\ 641}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 631}#
                                                        (#{extend-env\ 125}#
                                                          #{labels\ 637}#
                                                          #{bindings\ 641}#
                                                          (cdr #{r\ 631}#)))
                                                      (#{build-letrec\ 113}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 638}#)
                                                        #{vars\ 639}#
                                                        (map (lambda (#{x\ 678}#)
                                                               (#{chi\ 167}#
                                                                 (cdr #{x\ 678}#)
                                                                 (car #{x\ 678}#)
                                                                 '(())
                                                                 #{mod\ 650}#))
                                                             #{vals\ 640}#)
                                                        (#{build-sequence\ 110}#
                                                          #f
                                                          (map (lambda (#{x\ 679}#)
                                                                 (#{chi\ 167}#
                                                                   (cdr #{x\ 679}#)
                                                                   (car #{x\ 679}#)
                                                                   '(())
                                                                   #{mod\ 650}#))
                                                               (cons (cons #{er\ 644}#
                                                                           (#{source-wrap\ 160}#
                                                                             #{e\ 647}#
                                                                             #{w\ 648}#
                                                                             #{s\ 649}#
                                                                             #{mod\ 650}#))
                                                                     (cdr #{body\ 635}#))))))))))))))))))
                       (#{parse\ 634}#
                         (map (lambda (#{x\ 642}#)
                                (cons #{r\ 631}#
                                      (#{wrap\ 159}#
                                        #{x\ 642}#
                                        #{w\ 633}#
                                        #{mod\ 630}#)))
                              #{body\ 626}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 170}#
             (lambda (#{p\ 680}#
                      #{e\ 681}#
                      #{r\ 682}#
                      #{w\ 683}#
                      #{rib\ 684}#
                      #{mod\ 685}#)
               (letrec ((#{rebuild-macro-output\ 686}#
                          (lambda (#{x\ 687}# #{m\ 688}#)
                            (if (pair? #{x\ 687}#)
                              (cons (#{rebuild-macro-output\ 686}#
                                      (car #{x\ 687}#)
                                      #{m\ 688}#)
                                    (#{rebuild-macro-output\ 686}#
                                      (cdr #{x\ 687}#)
                                      #{m\ 688}#))
                              (if (#{syntax-object?\ 115}# #{x\ 687}#)
                                (let ((#{w\ 689}# (#{syntax-object-wrap\ 117}#
                                                    #{x\ 687}#)))
                                  (let ((#{ms\ 690}#
                                          (#{wrap-marks\ 134}# #{w\ 689}#))
                                        (#{s\ 691}# (#{wrap-subst\ 135}#
                                                      #{w\ 689}#)))
                                    (if (if (pair? #{ms\ 690}#)
                                          (eq? (car #{ms\ 690}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 687}#)
                                        (#{make-wrap\ 133}#
                                          (cdr #{ms\ 690}#)
                                          (if #{rib\ 684}#
                                            (cons #{rib\ 684}#
                                                  (cdr #{s\ 691}#))
                                            (cdr #{s\ 691}#)))
                                        (#{syntax-object-module\ 118}#
                                          #{x\ 687}#))
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 687}#)
                                        (#{make-wrap\ 133}#
                                          (cons #{m\ 688}# #{ms\ 690}#)
                                          (if #{rib\ 684}#
                                            (cons #{rib\ 684}#
                                                  (cons 'shift
                                                        #{s\ 691}#))
                                            (cons (quote shift) #{s\ 691}#)))
                                        (let ((#{pmod\ 692}#
                                                (procedure-module #{p\ 680}#)))
                                          (if #{pmod\ 692}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 692}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 687}#)
                                  (let ((#{n\ 693}# (vector-length
                                                      #{x\ 687}#)))
                                    (let ((#{v\ 694}# (make-vector
                                                        #{n\ 693}#)))
                                      (letrec ((#{loop\ 695}#
                                                 (lambda (#{i\ 696}#)
                                                   (if (#{fx=\ 88}#
                                                         #{i\ 696}#
                                                         #{n\ 693}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 694}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 694}#
                                                         #{i\ 696}#
                                                         (#{rebuild-macro-output\ 686}#
                                                           (vector-ref
                                                             #{x\ 687}#
                                                             #{i\ 696}#)
                                                           #{m\ 688}#))
                                                       (#{loop\ 695}#
                                                         (#{fx+\ 86}#
                                                           #{i\ 696}#
                                                           1)))))))
                                        (#{loop\ 695}# 0))))
                                  (if (symbol? #{x\ 687}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 160}#
                                        #{e\ 681}#
                                        #{w\ 683}#
                                        (#{wrap-subst\ 135}# #{w\ 683}#)
                                        #{mod\ 685}#)
                                      #{x\ 687}#)
                                    #{x\ 687}#)))))))
                 (#{rebuild-macro-output\ 686}#
                   (#{p\ 680}# (#{wrap\ 159}#
                                 #{e\ 681}#
                                 (#{anti-mark\ 146}# #{w\ 683}#)
                                 #{mod\ 685}#))
                   (string #\m)))))
           (#{chi-application\ 169}#
             (lambda (#{x\ 697}#
                      #{e\ 698}#
                      #{r\ 699}#
                      #{w\ 700}#
                      #{s\ 701}#
                      #{mod\ 702}#)
               ((lambda (#{tmp\ 703}#)
                  ((lambda (#{tmp\ 704}#)
                     (if #{tmp\ 704}#
                       (apply (lambda (#{e0\ 705}# #{e1\ 706}#)
                                (#{build-application\ 96}#
                                  #{s\ 701}#
                                  #{x\ 697}#
                                  (map (lambda (#{e\ 707}#)
                                         (#{chi\ 167}#
                                           #{e\ 707}#
                                           #{r\ 699}#
                                           #{w\ 700}#
                                           #{mod\ 702}#))
                                       #{e1\ 706}#)))
                              #{tmp\ 704}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 703}#)))
                   ($sc-dispatch
                     #{tmp\ 703}#
                     '(any . each-any))))
                #{e\ 698}#)))
           (#{chi-expr\ 168}#
             (lambda (#{type\ 709}#
                      #{value\ 710}#
                      #{e\ 711}#
                      #{r\ 712}#
                      #{w\ 713}#
                      #{s\ 714}#
                      #{mod\ 715}#)
               (if (memv #{type\ 709}# (quote (lexical)))
                 (#{build-lexical-reference\ 98}#
                   'value
                   #{s\ 714}#
                   #{e\ 711}#
                   #{value\ 710}#)
                 (if (memv #{type\ 709}# (quote (core core-form)))
                   (#{value\ 710}#
                     #{e\ 711}#
                     #{r\ 712}#
                     #{w\ 713}#
                     #{s\ 714}#
                     #{mod\ 715}#)
                   (if (memv #{type\ 709}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 710}# #{e\ 711}#))
                       (lambda (#{id\ 716}# #{mod\ 717}#)
                         (#{build-global-reference\ 101}#
                           #{s\ 714}#
                           #{id\ 716}#
                           #{mod\ 717}#)))
                     (if (memv #{type\ 709}# (quote (lexical-call)))
                       (#{chi-application\ 169}#
                         (#{build-lexical-reference\ 98}#
                           'fun
                           (#{source-annotation\ 122}# (car #{e\ 711}#))
                           (car #{e\ 711}#)
                           #{value\ 710}#)
                         #{e\ 711}#
                         #{r\ 712}#
                         #{w\ 713}#
                         #{s\ 714}#
                         #{mod\ 715}#)
                       (if (memv #{type\ 709}# (quote (global-call)))
                         (#{chi-application\ 169}#
                           (#{build-global-reference\ 101}#
                             (#{source-annotation\ 122}# (car #{e\ 711}#))
                             (if (#{syntax-object?\ 115}# #{value\ 710}#)
                               (#{syntax-object-expression\ 116}#
                                 #{value\ 710}#)
                               #{value\ 710}#)
                             (if (#{syntax-object?\ 115}# #{value\ 710}#)
                               (#{syntax-object-module\ 118}# #{value\ 710}#)
                               #{mod\ 715}#))
                           #{e\ 711}#
                           #{r\ 712}#
                           #{w\ 713}#
                           #{s\ 714}#
                           #{mod\ 715}#)
                         (if (memv #{type\ 709}# (quote (constant)))
                           (#{build-data\ 109}#
                             #{s\ 714}#
                             (#{strip\ 180}#
                               (#{source-wrap\ 160}#
                                 #{e\ 711}#
                                 #{w\ 713}#
                                 #{s\ 714}#
                                 #{mod\ 715}#)
                               '(())))
                           (if (memv #{type\ 709}# (quote (global)))
                             (#{build-global-reference\ 101}#
                               #{s\ 714}#
                               #{value\ 710}#
                               #{mod\ 715}#)
                             (if (memv #{type\ 709}# (quote (call)))
                               (#{chi-application\ 169}#
                                 (#{chi\ 167}#
                                   (car #{e\ 711}#)
                                   #{r\ 712}#
                                   #{w\ 713}#
                                   #{mod\ 715}#)
                                 #{e\ 711}#
                                 #{r\ 712}#
                                 #{w\ 713}#
                                 #{s\ 714}#
                                 #{mod\ 715}#)
                               (if (memv #{type\ 709}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 718}#)
                                    ((lambda (#{tmp\ 719}#)
                                       (if #{tmp\ 719}#
                                         (apply (lambda (#{_\ 720}#
                                                         #{e1\ 721}#
                                                         #{e2\ 722}#)
                                                  (#{chi-sequence\ 161}#
                                                    (cons #{e1\ 721}#
                                                          #{e2\ 722}#)
                                                    #{r\ 712}#
                                                    #{w\ 713}#
                                                    #{s\ 714}#
                                                    #{mod\ 715}#))
                                                #{tmp\ 719}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 718}#)))
                                     ($sc-dispatch
                                       #{tmp\ 718}#
                                       '(any any . each-any))))
                                  #{e\ 711}#)
                                 (if (memv #{type\ 709}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 172}#
                                     #{value\ 710}#
                                     #{e\ 711}#
                                     #{r\ 712}#
                                     #{w\ 713}#
                                     #{s\ 714}#
                                     #{mod\ 715}#
                                     #{chi-sequence\ 161}#)
                                   (if (memv #{type\ 709}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 724}#)
                                        ((lambda (#{tmp\ 725}#)
                                           (if #{tmp\ 725}#
                                             (apply (lambda (#{_\ 726}#
                                                             #{x\ 727}#
                                                             #{e1\ 728}#
                                                             #{e2\ 729}#)
                                                      (let ((#{when-list\ 730}#
                                                              (#{chi-when-list\ 164}#
                                                                #{e\ 711}#
                                                                #{x\ 727}#
                                                                #{w\ 713}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 730}#)
                                                          (#{chi-sequence\ 161}#
                                                            (cons #{e1\ 728}#
                                                                  #{e2\ 729}#)
                                                            #{r\ 712}#
                                                            #{w\ 713}#
                                                            #{s\ 714}#
                                                            #{mod\ 715}#)
                                                          (#{chi-void\ 174}#))))
                                                    #{tmp\ 725}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 724}#)))
                                         ($sc-dispatch
                                           #{tmp\ 724}#
                                           '(any each-any any . each-any))))
                                      #{e\ 711}#)
                                     (if (memv #{type\ 709}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 711}#
                                         (#{wrap\ 159}#
                                           #{value\ 710}#
                                           #{w\ 713}#
                                           #{mod\ 715}#))
                                       (if (memv #{type\ 709}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 160}#
                                             #{e\ 711}#
                                             #{w\ 713}#
                                             #{s\ 714}#
                                             #{mod\ 715}#))
                                         (if (memv #{type\ 709}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 160}#
                                               #{e\ 711}#
                                               #{w\ 713}#
                                               #{s\ 714}#
                                               #{mod\ 715}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 160}#
                                               #{e\ 711}#
                                               #{w\ 713}#
                                               #{s\ 714}#
                                               #{mod\ 715}#))))))))))))))))))
           (#{chi\ 167}#
             (lambda (#{e\ 733}# #{r\ 734}# #{w\ 735}# #{mod\ 736}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 733}#
                     #{r\ 734}#
                     #{w\ 735}#
                     (#{source-annotation\ 122}# #{e\ 733}#)
                     #f
                     #{mod\ 736}#
                     #f))
                 (lambda (#{type\ 737}#
                          #{value\ 738}#
                          #{e\ 739}#
                          #{w\ 740}#
                          #{s\ 741}#
                          #{mod\ 742}#)
                   (#{chi-expr\ 168}#
                     #{type\ 737}#
                     #{value\ 738}#
                     #{e\ 739}#
                     #{r\ 734}#
                     #{w\ 740}#
                     #{s\ 741}#
                     #{mod\ 742}#)))))
           (#{chi-top\ 166}#
             (lambda (#{e\ 743}#
                      #{r\ 744}#
                      #{w\ 745}#
                      #{m\ 746}#
                      #{esew\ 747}#
                      #{mod\ 748}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 743}#
                     #{r\ 744}#
                     #{w\ 745}#
                     (#{source-annotation\ 122}# #{e\ 743}#)
                     #f
                     #{mod\ 748}#
                     #f))
                 (lambda (#{type\ 756}#
                          #{value\ 757}#
                          #{e\ 758}#
                          #{w\ 759}#
                          #{s\ 760}#
                          #{mod\ 761}#)
                   (if (memv #{type\ 756}# (quote (begin-form)))
                     ((lambda (#{tmp\ 762}#)
                        ((lambda (#{tmp\ 763}#)
                           (if #{tmp\ 763}#
                             (apply (lambda (#{_\ 764}#) (#{chi-void\ 174}#))
                                    #{tmp\ 763}#)
                             ((lambda (#{tmp\ 765}#)
                                (if #{tmp\ 765}#
                                  (apply (lambda (#{_\ 766}#
                                                  #{e1\ 767}#
                                                  #{e2\ 768}#)
                                           (#{chi-top-sequence\ 162}#
                                             (cons #{e1\ 767}# #{e2\ 768}#)
                                             #{r\ 744}#
                                             #{w\ 759}#
                                             #{s\ 760}#
                                             #{m\ 746}#
                                             #{esew\ 747}#
                                             #{mod\ 761}#))
                                         #{tmp\ 765}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 762}#)))
                              ($sc-dispatch
                                #{tmp\ 762}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 762}# (quote (any)))))
                      #{e\ 758}#)
                     (if (memv #{type\ 756}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 172}#
                         #{value\ 757}#
                         #{e\ 758}#
                         #{r\ 744}#
                         #{w\ 759}#
                         #{s\ 760}#
                         #{mod\ 761}#
                         (lambda (#{body\ 770}#
                                  #{r\ 771}#
                                  #{w\ 772}#
                                  #{s\ 773}#
                                  #{mod\ 774}#)
                           (#{chi-top-sequence\ 162}#
                             #{body\ 770}#
                             #{r\ 771}#
                             #{w\ 772}#
                             #{s\ 773}#
                             #{m\ 746}#
                             #{esew\ 747}#
                             #{mod\ 774}#)))
                       (if (memv #{type\ 756}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 775}#)
                            ((lambda (#{tmp\ 776}#)
                               (if #{tmp\ 776}#
                                 (apply (lambda (#{_\ 777}#
                                                 #{x\ 778}#
                                                 #{e1\ 779}#
                                                 #{e2\ 780}#)
                                          (let ((#{when-list\ 781}#
                                                  (#{chi-when-list\ 164}#
                                                    #{e\ 758}#
                                                    #{x\ 778}#
                                                    #{w\ 759}#))
                                                (#{body\ 782}#
                                                  (cons #{e1\ 779}#
                                                        #{e2\ 780}#)))
                                            (if (eq? #{m\ 746}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 781}#)
                                                (#{chi-top-sequence\ 162}#
                                                  #{body\ 782}#
                                                  #{r\ 744}#
                                                  #{w\ 759}#
                                                  #{s\ 760}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 761}#)
                                                (#{chi-void\ 174}#))
                                              (if (memq 'load
                                                        #{when-list\ 781}#)
                                                (if (let ((#{t\ 785}# (memq 'compile
                                                                            #{when-list\ 781}#)))
                                                      (if #{t\ 785}#
                                                        #{t\ 785}#
                                                        (if (eq? #{m\ 746}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 781}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 162}#
                                                    #{body\ 782}#
                                                    #{r\ 744}#
                                                    #{w\ 759}#
                                                    #{s\ 760}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 761}#)
                                                  (if (memq #{m\ 746}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 162}#
                                                      #{body\ 782}#
                                                      #{r\ 744}#
                                                      #{w\ 759}#
                                                      #{s\ 760}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 761}#)
                                                    (#{chi-void\ 174}#)))
                                                (if (let ((#{t\ 786}# (memq 'compile
                                                                            #{when-list\ 781}#)))
                                                      (if #{t\ 786}#
                                                        #{t\ 786}#
                                                        (if (eq? #{m\ 746}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 781}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 90}#
                                                      (#{chi-top-sequence\ 162}#
                                                        #{body\ 782}#
                                                        #{r\ 744}#
                                                        #{w\ 759}#
                                                        #{s\ 760}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 761}#)
                                                      #{mod\ 761}#)
                                                    (#{chi-void\ 174}#))
                                                  (#{chi-void\ 174}#))))))
                                        #{tmp\ 776}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 775}#)))
                             ($sc-dispatch
                               #{tmp\ 775}#
                               '(any each-any any . each-any))))
                          #{e\ 758}#)
                         (if (memv #{type\ 756}# (quote (define-syntax-form)))
                           (let ((#{n\ 787}# (#{id-var-name\ 153}#
                                               #{value\ 757}#
                                               #{w\ 759}#))
                                 (#{r\ 788}# (#{macros-only-env\ 127}#
                                               #{r\ 744}#)))
                             (if (memv #{m\ 746}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 747}#)
                                 (let ((#{e\ 789}# (#{chi-install-global\ 163}#
                                                     #{n\ 787}#
                                                     (#{chi\ 167}#
                                                       #{e\ 758}#
                                                       #{r\ 788}#
                                                       #{w\ 759}#
                                                       #{mod\ 761}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 789}#
                                       #{mod\ 761}#)
                                     (if (memq (quote load) #{esew\ 747}#)
                                       #{e\ 789}#
                                       (#{chi-void\ 174}#))))
                                 (if (memq (quote load) #{esew\ 747}#)
                                   (#{chi-install-global\ 163}#
                                     #{n\ 787}#
                                     (#{chi\ 167}#
                                       #{e\ 758}#
                                       #{r\ 788}#
                                       #{w\ 759}#
                                       #{mod\ 761}#))
                                   (#{chi-void\ 174}#)))
                               (if (memv #{m\ 746}# (quote (c&e)))
                                 (let ((#{e\ 790}# (#{chi-install-global\ 163}#
                                                     #{n\ 787}#
                                                     (#{chi\ 167}#
                                                       #{e\ 758}#
                                                       #{r\ 788}#
                                                       #{w\ 759}#
                                                       #{mod\ 761}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 790}#
                                       #{mod\ 761}#)
                                     #{e\ 790}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 747}#)
                                     (#{top-level-eval-hook\ 90}#
                                       (#{chi-install-global\ 163}#
                                         #{n\ 787}#
                                         (#{chi\ 167}#
                                           #{e\ 758}#
                                           #{r\ 788}#
                                           #{w\ 759}#
                                           #{mod\ 761}#))
                                       #{mod\ 761}#))
                                   (#{chi-void\ 174}#)))))
                           (if (memv #{type\ 756}# (quote (define-form)))
                             (let ((#{n\ 791}# (#{id-var-name\ 153}#
                                                 #{value\ 757}#
                                                 #{w\ 759}#)))
                               (let ((#{type\ 792}#
                                       (#{binding-type\ 123}#
                                         (#{lookup\ 128}#
                                           #{n\ 791}#
                                           #{r\ 744}#
                                           #{mod\ 761}#))))
                                 (if (memv #{type\ 792}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 791}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 793}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 791}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 791}#
                                           (if (variable? #{old\ 793}#)
                                             (variable-ref #{old\ 793}#)
                                             #f))))
                                     (let ((#{x\ 794}# (#{build-global-definition\ 104}#
                                                         #{s\ 760}#
                                                         #{n\ 791}#
                                                         (#{chi\ 167}#
                                                           #{e\ 758}#
                                                           #{r\ 744}#
                                                           #{w\ 759}#
                                                           #{mod\ 761}#))))
                                       (begin
                                         (if (eq? #{m\ 746}# (quote c&e))
                                           (#{top-level-eval-hook\ 90}#
                                             #{x\ 794}#
                                             #{mod\ 761}#))
                                         #{x\ 794}#)))
                                   (if (memv #{type\ 792}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 758}#
                                       (#{wrap\ 159}#
                                         #{value\ 757}#
                                         #{w\ 759}#
                                         #{mod\ 761}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 758}#
                                       (#{wrap\ 159}#
                                         #{value\ 757}#
                                         #{w\ 759}#
                                         #{mod\ 761}#))))))
                             (let ((#{x\ 795}# (#{chi-expr\ 168}#
                                                 #{type\ 756}#
                                                 #{value\ 757}#
                                                 #{e\ 758}#
                                                 #{r\ 744}#
                                                 #{w\ 759}#
                                                 #{s\ 760}#
                                                 #{mod\ 761}#)))
                               (begin
                                 (if (eq? #{m\ 746}# (quote c&e))
                                   (#{top-level-eval-hook\ 90}#
                                     #{x\ 795}#
                                     #{mod\ 761}#))
                                 #{x\ 795}#)))))))))))
           (#{syntax-type\ 165}#
             (lambda (#{e\ 796}#
                      #{r\ 797}#
                      #{w\ 798}#
                      #{s\ 799}#
                      #{rib\ 800}#
                      #{mod\ 801}#
                      #{for-car?\ 802}#)
               (if (symbol? #{e\ 796}#)
                 (let ((#{n\ 803}# (#{id-var-name\ 153}#
                                     #{e\ 796}#
                                     #{w\ 798}#)))
                   (let ((#{b\ 804}# (#{lookup\ 128}#
                                       #{n\ 803}#
                                       #{r\ 797}#
                                       #{mod\ 801}#)))
                     (let ((#{type\ 805}#
                             (#{binding-type\ 123}# #{b\ 804}#)))
                       (if (memv #{type\ 805}# (quote (lexical)))
                         (values
                           #{type\ 805}#
                           (#{binding-value\ 124}# #{b\ 804}#)
                           #{e\ 796}#
                           #{w\ 798}#
                           #{s\ 799}#
                           #{mod\ 801}#)
                         (if (memv #{type\ 805}# (quote (global)))
                           (values
                             #{type\ 805}#
                             #{n\ 803}#
                             #{e\ 796}#
                             #{w\ 798}#
                             #{s\ 799}#
                             #{mod\ 801}#)
                           (if (memv #{type\ 805}# (quote (macro)))
                             (if #{for-car?\ 802}#
                               (values
                                 #{type\ 805}#
                                 (#{binding-value\ 124}# #{b\ 804}#)
                                 #{e\ 796}#
                                 #{w\ 798}#
                                 #{s\ 799}#
                                 #{mod\ 801}#)
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   (#{binding-value\ 124}# #{b\ 804}#)
                                   #{e\ 796}#
                                   #{r\ 797}#
                                   #{w\ 798}#
                                   #{rib\ 800}#
                                   #{mod\ 801}#)
                                 #{r\ 797}#
                                 '(())
                                 #{s\ 799}#
                                 #{rib\ 800}#
                                 #{mod\ 801}#
                                 #f))
                             (values
                               #{type\ 805}#
                               (#{binding-value\ 124}# #{b\ 804}#)
                               #{e\ 796}#
                               #{w\ 798}#
                               #{s\ 799}#
                               #{mod\ 801}#)))))))
                 (if (pair? #{e\ 796}#)
                   (let ((#{first\ 806}# (car #{e\ 796}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 165}#
                           #{first\ 806}#
                           #{r\ 797}#
                           #{w\ 798}#
                           #{s\ 799}#
                           #{rib\ 800}#
                           #{mod\ 801}#
                           #t))
                       (lambda (#{ftype\ 807}#
                                #{fval\ 808}#
                                #{fe\ 809}#
                                #{fw\ 810}#
                                #{fs\ 811}#
                                #{fmod\ 812}#)
                         (if (memv #{ftype\ 807}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 808}#
                             #{e\ 796}#
                             #{w\ 798}#
                             #{s\ 799}#
                             #{mod\ 801}#)
                           (if (memv #{ftype\ 807}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 114}#
                                 #{fval\ 808}#
                                 #{w\ 798}#
                                 #{fmod\ 812}#)
                               #{e\ 796}#
                               #{w\ 798}#
                               #{s\ 799}#
                               #{mod\ 801}#)
                             (if (memv #{ftype\ 807}# (quote (macro)))
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   #{fval\ 808}#
                                   #{e\ 796}#
                                   #{r\ 797}#
                                   #{w\ 798}#
                                   #{rib\ 800}#
                                   #{mod\ 801}#)
                                 #{r\ 797}#
                                 '(())
                                 #{s\ 799}#
                                 #{rib\ 800}#
                                 #{mod\ 801}#
                                 #{for-car?\ 802}#)
                               (if (memv #{ftype\ 807}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 808}# #{e\ 796}#))
                                   (lambda (#{sym\ 813}# #{mod\ 814}#)
                                     (#{syntax-type\ 165}#
                                       #{sym\ 813}#
                                       #{r\ 797}#
                                       #{w\ 798}#
                                       #{s\ 799}#
                                       #{rib\ 800}#
                                       #{mod\ 814}#
                                       #{for-car?\ 802}#)))
                                 (if (memv #{ftype\ 807}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 808}#
                                     #{e\ 796}#
                                     #{w\ 798}#
                                     #{s\ 799}#
                                     #{mod\ 801}#)
                                   (if (memv #{ftype\ 807}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 808}#
                                       #{e\ 796}#
                                       #{w\ 798}#
                                       #{s\ 799}#
                                       #{mod\ 801}#)
                                     (if (memv #{ftype\ 807}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 796}#
                                         #{w\ 798}#
                                         #{s\ 799}#
                                         #{mod\ 801}#)
                                       (if (memv #{ftype\ 807}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 796}#
                                           #{w\ 798}#
                                           #{s\ 799}#
                                           #{mod\ 801}#)
                                         (if (memv #{ftype\ 807}#
                                                   '(define))
                                           ((lambda (#{tmp\ 815}#)
                                              ((lambda (#{tmp\ 816}#)
                                                 (if (if #{tmp\ 816}#
                                                       (apply (lambda (#{_\ 817}#
                                                                       #{name\ 818}#
                                                                       #{val\ 819}#)
                                                                (#{id?\ 131}#
                                                                  #{name\ 818}#))
                                                              #{tmp\ 816}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 820}#
                                                                   #{name\ 821}#
                                                                   #{val\ 822}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 821}#
                                                              #{val\ 822}#
                                                              #{w\ 798}#
                                                              #{s\ 799}#
                                                              #{mod\ 801}#))
                                                          #{tmp\ 816}#)
                                                   ((lambda (#{tmp\ 823}#)
                                                      (if (if #{tmp\ 823}#
                                                            (apply (lambda (#{_\ 824}#
                                                                            #{name\ 825}#
                                                                            #{args\ 826}#
                                                                            #{e1\ 827}#
                                                                            #{e2\ 828}#)
                                                                     (if (#{id?\ 131}#
                                                                           #{name\ 825}#)
                                                                       (#{valid-bound-ids?\ 156}#
                                                                         (#{lambda-var-list\ 182}#
                                                                           #{args\ 826}#))
                                                                       #f))
                                                                   #{tmp\ 823}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 829}#
                                                                        #{name\ 830}#
                                                                        #{args\ 831}#
                                                                        #{e1\ 832}#
                                                                        #{e2\ 833}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 159}#
                                                                     #{name\ 830}#
                                                                     #{w\ 798}#
                                                                     #{mod\ 801}#)
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
                                                                             (cons #{args\ 831}#
                                                                                   (cons #{e1\ 832}#
                                                                                         #{e2\ 833}#))
                                                                             #{w\ 798}#
                                                                             #{mod\ 801}#))
                                                                     #{s\ 799}#)
                                                                   '(())
                                                                   #{s\ 799}#
                                                                   #{mod\ 801}#))
                                                               #{tmp\ 823}#)
                                                        ((lambda (#{tmp\ 835}#)
                                                           (if (if #{tmp\ 835}#
                                                                 (apply (lambda (#{_\ 836}#
                                                                                 #{name\ 837}#)
                                                                          (#{id?\ 131}#
                                                                            #{name\ 837}#))
                                                                        #{tmp\ 835}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 838}#
                                                                             #{name\ 839}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 159}#
                                                                          #{name\ 839}#
                                                                          #{w\ 798}#
                                                                          #{mod\ 801}#)
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
                                                                        #{s\ 799}#
                                                                        #{mod\ 801}#))
                                                                    #{tmp\ 835}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 815}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 815}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 815}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 815}#
                                                 '(any any any))))
                                            #{e\ 796}#)
                                           (if (memv #{ftype\ 807}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 840}#)
                                                ((lambda (#{tmp\ 841}#)
                                                   (if (if #{tmp\ 841}#
                                                         (apply (lambda (#{_\ 842}#
                                                                         #{name\ 843}#
                                                                         #{val\ 844}#)
                                                                  (#{id?\ 131}#
                                                                    #{name\ 843}#))
                                                                #{tmp\ 841}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 845}#
                                                                     #{name\ 846}#
                                                                     #{val\ 847}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 846}#
                                                                #{val\ 847}#
                                                                #{w\ 798}#
                                                                #{s\ 799}#
                                                                #{mod\ 801}#))
                                                            #{tmp\ 841}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 840}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 840}#
                                                   '(any any any))))
                                              #{e\ 796}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 796}#
                                               #{w\ 798}#
                                               #{s\ 799}#
                                               #{mod\ 801}#))))))))))))))
                   (if (#{syntax-object?\ 115}# #{e\ 796}#)
                     (#{syntax-type\ 165}#
                       (#{syntax-object-expression\ 116}# #{e\ 796}#)
                       #{r\ 797}#
                       (#{join-wraps\ 150}#
                         #{w\ 798}#
                         (#{syntax-object-wrap\ 117}# #{e\ 796}#))
                       #{s\ 799}#
                       #{rib\ 800}#
                       (let ((#{t\ 848}# (#{syntax-object-module\ 118}#
                                           #{e\ 796}#)))
                         (if #{t\ 848}# #{t\ 848}# #{mod\ 801}#))
                       #{for-car?\ 802}#)
                     (if (self-evaluating? #{e\ 796}#)
                       (values
                         'constant
                         #f
                         #{e\ 796}#
                         #{w\ 798}#
                         #{s\ 799}#
                         #{mod\ 801}#)
                       (values
                         'other
                         #f
                         #{e\ 796}#
                         #{w\ 798}#
                         #{s\ 799}#
                         #{mod\ 801}#)))))))
           (#{chi-when-list\ 164}#
             (lambda (#{e\ 849}# #{when-list\ 850}# #{w\ 851}#)
               (letrec ((#{f\ 852}# (lambda (#{when-list\ 853}#
                                             #{situations\ 854}#)
                                      (if (null? #{when-list\ 853}#)
                                        #{situations\ 854}#
                                        (#{f\ 852}# (cdr #{when-list\ 853}#)
                                                    (cons (let ((#{x\ 855}# (car #{when-list\ 853}#)))
                                                            (if (#{free-id=?\ 154}#
                                                                  #{x\ 855}#
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
                                                                    #{x\ 855}#
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
                                                                      #{x\ 855}#
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
                                                                    #{e\ 849}#
                                                                    (#{wrap\ 159}#
                                                                      #{x\ 855}#
                                                                      #{w\ 851}#
                                                                      #f))))))
                                                          #{situations\ 854}#))))))
                 (#{f\ 852}# #{when-list\ 850}# (quote ())))))
           (#{chi-install-global\ 163}#
             (lambda (#{name\ 856}# #{e\ 857}#)
               (#{build-global-definition\ 104}#
                 #f
                 #{name\ 856}#
                 (if (let ((#{v\ 858}# (module-variable
                                         (current-module)
                                         #{name\ 856}#)))
                       (if #{v\ 858}#
                         (if (variable-bound? #{v\ 858}#)
                           (if (macro? (variable-ref #{v\ 858}#))
                             (not (eq? (macro-type (variable-ref #{v\ 858}#))
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
                                   (#{build-data\ 109}# #f #{name\ 856}#)))
                           (#{build-data\ 109}# #f (quote macro))
                           #{e\ 857}#))
                   (#{build-application\ 96}#
                     #f
                     (#{build-primref\ 108}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 109}# #f (quote macro))
                           #{e\ 857}#))))))
           (#{chi-top-sequence\ 162}#
             (lambda (#{body\ 859}#
                      #{r\ 860}#
                      #{w\ 861}#
                      #{s\ 862}#
                      #{m\ 863}#
                      #{esew\ 864}#
                      #{mod\ 865}#)
               (#{build-sequence\ 110}#
                 #{s\ 862}#
                 (letrec ((#{dobody\ 866}#
                            (lambda (#{body\ 867}#
                                     #{r\ 868}#
                                     #{w\ 869}#
                                     #{m\ 870}#
                                     #{esew\ 871}#
                                     #{mod\ 872}#)
                              (if (null? #{body\ 867}#)
                                '()
                                (let ((#{first\ 873}#
                                        (#{chi-top\ 166}#
                                          (car #{body\ 867}#)
                                          #{r\ 868}#
                                          #{w\ 869}#
                                          #{m\ 870}#
                                          #{esew\ 871}#
                                          #{mod\ 872}#)))
                                  (cons #{first\ 873}#
                                        (#{dobody\ 866}#
                                          (cdr #{body\ 867}#)
                                          #{r\ 868}#
                                          #{w\ 869}#
                                          #{m\ 870}#
                                          #{esew\ 871}#
                                          #{mod\ 872}#)))))))
                   (#{dobody\ 866}#
                     #{body\ 859}#
                     #{r\ 860}#
                     #{w\ 861}#
                     #{m\ 863}#
                     #{esew\ 864}#
                     #{mod\ 865}#)))))
           (#{chi-sequence\ 161}#
             (lambda (#{body\ 874}#
                      #{r\ 875}#
                      #{w\ 876}#
                      #{s\ 877}#
                      #{mod\ 878}#)
               (#{build-sequence\ 110}#
                 #{s\ 877}#
                 (letrec ((#{dobody\ 879}#
                            (lambda (#{body\ 880}#
                                     #{r\ 881}#
                                     #{w\ 882}#
                                     #{mod\ 883}#)
                              (if (null? #{body\ 880}#)
                                '()
                                (let ((#{first\ 884}#
                                        (#{chi\ 167}#
                                          (car #{body\ 880}#)
                                          #{r\ 881}#
                                          #{w\ 882}#
                                          #{mod\ 883}#)))
                                  (cons #{first\ 884}#
                                        (#{dobody\ 879}#
                                          (cdr #{body\ 880}#)
                                          #{r\ 881}#
                                          #{w\ 882}#
                                          #{mod\ 883}#)))))))
                   (#{dobody\ 879}#
                     #{body\ 874}#
                     #{r\ 875}#
                     #{w\ 876}#
                     #{mod\ 878}#)))))
           (#{source-wrap\ 160}#
             (lambda (#{x\ 885}#
                      #{w\ 886}#
                      #{s\ 887}#
                      #{defmod\ 888}#)
               (#{wrap\ 159}#
                 (#{decorate-source\ 94}# #{x\ 885}# #{s\ 887}#)
                 #{w\ 886}#
                 #{defmod\ 888}#)))
           (#{wrap\ 159}#
             (lambda (#{x\ 889}# #{w\ 890}# #{defmod\ 891}#)
               (if (if (null? (#{wrap-marks\ 134}# #{w\ 890}#))
                     (null? (#{wrap-subst\ 135}# #{w\ 890}#))
                     #f)
                 #{x\ 889}#
                 (if (#{syntax-object?\ 115}# #{x\ 889}#)
                   (#{make-syntax-object\ 114}#
                     (#{syntax-object-expression\ 116}# #{x\ 889}#)
                     (#{join-wraps\ 150}#
                       #{w\ 890}#
                       (#{syntax-object-wrap\ 117}# #{x\ 889}#))
                     (#{syntax-object-module\ 118}# #{x\ 889}#))
                   (if (null? #{x\ 889}#)
                     #{x\ 889}#
                     (#{make-syntax-object\ 114}#
                       #{x\ 889}#
                       #{w\ 890}#
                       #{defmod\ 891}#))))))
           (#{bound-id-member?\ 158}#
             (lambda (#{x\ 892}# #{list\ 893}#)
               (if (not (null? #{list\ 893}#))
                 (let ((#{t\ 894}# (#{bound-id=?\ 155}#
                                     #{x\ 892}#
                                     (car #{list\ 893}#))))
                   (if #{t\ 894}#
                     #{t\ 894}#
                     (#{bound-id-member?\ 158}#
                       #{x\ 892}#
                       (cdr #{list\ 893}#))))
                 #f)))
           (#{distinct-bound-ids?\ 157}#
             (lambda (#{ids\ 895}#)
               (letrec ((#{distinct?\ 896}#
                          (lambda (#{ids\ 897}#)
                            (let ((#{t\ 898}# (null? #{ids\ 897}#)))
                              (if #{t\ 898}#
                                #{t\ 898}#
                                (if (not (#{bound-id-member?\ 158}#
                                           (car #{ids\ 897}#)
                                           (cdr #{ids\ 897}#)))
                                  (#{distinct?\ 896}# (cdr #{ids\ 897}#))
                                  #f))))))
                 (#{distinct?\ 896}# #{ids\ 895}#))))
           (#{valid-bound-ids?\ 156}#
             (lambda (#{ids\ 899}#)
               (if (letrec ((#{all-ids?\ 900}#
                              (lambda (#{ids\ 901}#)
                                (let ((#{t\ 902}# (null? #{ids\ 901}#)))
                                  (if #{t\ 902}#
                                    #{t\ 902}#
                                    (if (#{id?\ 131}# (car #{ids\ 901}#))
                                      (#{all-ids?\ 900}# (cdr #{ids\ 901}#))
                                      #f))))))
                     (#{all-ids?\ 900}# #{ids\ 899}#))
                 (#{distinct-bound-ids?\ 157}# #{ids\ 899}#)
                 #f)))
           (#{bound-id=?\ 155}#
             (lambda (#{i\ 903}# #{j\ 904}#)
               (if (if (#{syntax-object?\ 115}# #{i\ 903}#)
                     (#{syntax-object?\ 115}# #{j\ 904}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 116}# #{i\ 903}#)
                          (#{syntax-object-expression\ 116}# #{j\ 904}#))
                   (#{same-marks?\ 152}#
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{i\ 903}#))
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{j\ 904}#)))
                   #f)
                 (eq? #{i\ 903}# #{j\ 904}#))))
           (#{free-id=?\ 154}#
             (lambda (#{i\ 905}# #{j\ 906}#)
               (if (eq? (let ((#{x\ 907}# #{i\ 905}#))
                          (if (#{syntax-object?\ 115}# #{x\ 907}#)
                            (#{syntax-object-expression\ 116}# #{x\ 907}#)
                            #{x\ 907}#))
                        (let ((#{x\ 908}# #{j\ 906}#))
                          (if (#{syntax-object?\ 115}# #{x\ 908}#)
                            (#{syntax-object-expression\ 116}# #{x\ 908}#)
                            #{x\ 908}#)))
                 (eq? (#{id-var-name\ 153}# #{i\ 905}# (quote (())))
                      (#{id-var-name\ 153}# #{j\ 906}# (quote (()))))
                 #f)))
           (#{id-var-name\ 153}#
             (lambda (#{id\ 909}# #{w\ 910}#)
               (letrec ((#{search-vector-rib\ 913}#
                          (lambda (#{sym\ 919}#
                                   #{subst\ 920}#
                                   #{marks\ 921}#
                                   #{symnames\ 922}#
                                   #{ribcage\ 923}#)
                            (let ((#{n\ 924}# (vector-length
                                                #{symnames\ 922}#)))
                              (letrec ((#{f\ 925}# (lambda (#{i\ 926}#)
                                                     (if (#{fx=\ 88}#
                                                           #{i\ 926}#
                                                           #{n\ 924}#)
                                                       (#{search\ 911}#
                                                         #{sym\ 919}#
                                                         (cdr #{subst\ 920}#)
                                                         #{marks\ 921}#)
                                                       (if (if (eq? (vector-ref
                                                                      #{symnames\ 922}#
                                                                      #{i\ 926}#)
                                                                    #{sym\ 919}#)
                                                             (#{same-marks?\ 152}#
                                                               #{marks\ 921}#
                                                               (vector-ref
                                                                 (#{ribcage-marks\ 141}#
                                                                   #{ribcage\ 923}#)
                                                                 #{i\ 926}#))
                                                             #f)
                                                         (values
                                                           (vector-ref
                                                             (#{ribcage-labels\ 142}#
                                                               #{ribcage\ 923}#)
                                                             #{i\ 926}#)
                                                           #{marks\ 921}#)
                                                         (#{f\ 925}# (#{fx+\ 86}#
                                                                       #{i\ 926}#
                                                                       1)))))))
                                (#{f\ 925}# 0)))))
                        (#{search-list-rib\ 912}#
                          (lambda (#{sym\ 927}#
                                   #{subst\ 928}#
                                   #{marks\ 929}#
                                   #{symnames\ 930}#
                                   #{ribcage\ 931}#)
                            (letrec ((#{f\ 932}# (lambda (#{symnames\ 933}#
                                                          #{i\ 934}#)
                                                   (if (null? #{symnames\ 933}#)
                                                     (#{search\ 911}#
                                                       #{sym\ 927}#
                                                       (cdr #{subst\ 928}#)
                                                       #{marks\ 929}#)
                                                     (if (if (eq? (car #{symnames\ 933}#)
                                                                  #{sym\ 927}#)
                                                           (#{same-marks?\ 152}#
                                                             #{marks\ 929}#
                                                             (list-ref
                                                               (#{ribcage-marks\ 141}#
                                                                 #{ribcage\ 931}#)
                                                               #{i\ 934}#))
                                                           #f)
                                                       (values
                                                         (list-ref
                                                           (#{ribcage-labels\ 142}#
                                                             #{ribcage\ 931}#)
                                                           #{i\ 934}#)
                                                         #{marks\ 929}#)
                                                       (#{f\ 932}# (cdr #{symnames\ 933}#)
                                                                   (#{fx+\ 86}#
                                                                     #{i\ 934}#
                                                                     1)))))))
                              (#{f\ 932}# #{symnames\ 930}# 0))))
                        (#{search\ 911}#
                          (lambda (#{sym\ 935}# #{subst\ 936}# #{marks\ 937}#)
                            (if (null? #{subst\ 936}#)
                              (values #f #{marks\ 937}#)
                              (let ((#{fst\ 938}# (car #{subst\ 936}#)))
                                (if (eq? #{fst\ 938}# (quote shift))
                                  (#{search\ 911}#
                                    #{sym\ 935}#
                                    (cdr #{subst\ 936}#)
                                    (cdr #{marks\ 937}#))
                                  (let ((#{symnames\ 939}#
                                          (#{ribcage-symnames\ 140}#
                                            #{fst\ 938}#)))
                                    (if (vector? #{symnames\ 939}#)
                                      (#{search-vector-rib\ 913}#
                                        #{sym\ 935}#
                                        #{subst\ 936}#
                                        #{marks\ 937}#
                                        #{symnames\ 939}#
                                        #{fst\ 938}#)
                                      (#{search-list-rib\ 912}#
                                        #{sym\ 935}#
                                        #{subst\ 936}#
                                        #{marks\ 937}#
                                        #{symnames\ 939}#
                                        #{fst\ 938}#)))))))))
                 (if (symbol? #{id\ 909}#)
                   (let ((#{t\ 940}# (call-with-values
                                       (lambda ()
                                         (#{search\ 911}#
                                           #{id\ 909}#
                                           (#{wrap-subst\ 135}# #{w\ 910}#)
                                           (#{wrap-marks\ 134}# #{w\ 910}#)))
                                       (lambda (#{x\ 941}# . #{ignore\ 942}#)
                                         #{x\ 941}#))))
                     (if #{t\ 940}# #{t\ 940}# #{id\ 909}#))
                   (if (#{syntax-object?\ 115}# #{id\ 909}#)
                     (let ((#{id\ 943}#
                             (#{syntax-object-expression\ 116}# #{id\ 909}#))
                           (#{w1\ 944}#
                             (#{syntax-object-wrap\ 117}# #{id\ 909}#)))
                       (let ((#{marks\ 945}#
                               (#{join-marks\ 151}#
                                 (#{wrap-marks\ 134}# #{w\ 910}#)
                                 (#{wrap-marks\ 134}# #{w1\ 944}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 911}#
                               #{id\ 943}#
                               (#{wrap-subst\ 135}# #{w\ 910}#)
                               #{marks\ 945}#))
                           (lambda (#{new-id\ 946}# #{marks\ 947}#)
                             (let ((#{t\ 948}# #{new-id\ 946}#))
                               (if #{t\ 948}#
                                 #{t\ 948}#
                                 (let ((#{t\ 949}# (call-with-values
                                                     (lambda ()
                                                       (#{search\ 911}#
                                                         #{id\ 943}#
                                                         (#{wrap-subst\ 135}#
                                                           #{w1\ 944}#)
                                                         #{marks\ 947}#))
                                                     (lambda (#{x\ 950}#
                                                              .
                                                              #{ignore\ 951}#)
                                                       #{x\ 950}#))))
                                   (if #{t\ 949}#
                                     #{t\ 949}#
                                     #{id\ 943}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 909}#))))))
           (#{same-marks?\ 152}#
             (lambda (#{x\ 952}# #{y\ 953}#)
               (let ((#{t\ 954}# (eq? #{x\ 952}# #{y\ 953}#)))
                 (if #{t\ 954}#
                   #{t\ 954}#
                   (if (not (null? #{x\ 952}#))
                     (if (not (null? #{y\ 953}#))
                       (if (eq? (car #{x\ 952}#) (car #{y\ 953}#))
                         (#{same-marks?\ 152}#
                           (cdr #{x\ 952}#)
                           (cdr #{y\ 953}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 151}#
             (lambda (#{m1\ 955}# #{m2\ 956}#)
               (#{smart-append\ 149}# #{m1\ 955}# #{m2\ 956}#)))
           (#{join-wraps\ 150}#
             (lambda (#{w1\ 957}# #{w2\ 958}#)
               (let ((#{m1\ 959}# (#{wrap-marks\ 134}# #{w1\ 957}#))
                     (#{s1\ 960}# (#{wrap-subst\ 135}# #{w1\ 957}#)))
                 (if (null? #{m1\ 959}#)
                   (if (null? #{s1\ 960}#)
                     #{w2\ 958}#
                     (#{make-wrap\ 133}#
                       (#{wrap-marks\ 134}# #{w2\ 958}#)
                       (#{smart-append\ 149}#
                         #{s1\ 960}#
                         (#{wrap-subst\ 135}# #{w2\ 958}#))))
                   (#{make-wrap\ 133}#
                     (#{smart-append\ 149}#
                       #{m1\ 959}#
                       (#{wrap-marks\ 134}# #{w2\ 958}#))
                     (#{smart-append\ 149}#
                       #{s1\ 960}#
                       (#{wrap-subst\ 135}# #{w2\ 958}#)))))))
           (#{smart-append\ 149}#
             (lambda (#{m1\ 961}# #{m2\ 962}#)
               (if (null? #{m2\ 962}#)
                 #{m1\ 961}#
                 (append #{m1\ 961}# #{m2\ 962}#))))
           (#{make-binding-wrap\ 148}#
             (lambda (#{ids\ 963}# #{labels\ 964}# #{w\ 965}#)
               (if (null? #{ids\ 963}#)
                 #{w\ 965}#
                 (#{make-wrap\ 133}#
                   (#{wrap-marks\ 134}# #{w\ 965}#)
                   (cons (let ((#{labelvec\ 966}#
                                 (list->vector #{labels\ 964}#)))
                           (let ((#{n\ 967}# (vector-length
                                               #{labelvec\ 966}#)))
                             (let ((#{symnamevec\ 968}#
                                     (make-vector #{n\ 967}#))
                                   (#{marksvec\ 969}#
                                     (make-vector #{n\ 967}#)))
                               (begin
                                 (letrec ((#{f\ 970}# (lambda (#{ids\ 971}#
                                                               #{i\ 972}#)
                                                        (if (not (null? #{ids\ 971}#))
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{id-sym-name&marks\ 132}#
                                                                (car #{ids\ 971}#)
                                                                #{w\ 965}#))
                                                            (lambda (#{symname\ 973}#
                                                                     #{marks\ 974}#)
                                                              (begin
                                                                (vector-set!
                                                                  #{symnamevec\ 968}#
                                                                  #{i\ 972}#
                                                                  #{symname\ 973}#)
                                                                (vector-set!
                                                                  #{marksvec\ 969}#
                                                                  #{i\ 972}#
                                                                  #{marks\ 974}#)
                                                                (#{f\ 970}# (cdr #{ids\ 971}#)
                                                                            (#{fx+\ 86}#
                                                                              #{i\ 972}#
                                                                              1)))))))))
                                   (#{f\ 970}# #{ids\ 963}# 0))
                                 (#{make-ribcage\ 138}#
                                   #{symnamevec\ 968}#
                                   #{marksvec\ 969}#
                                   #{labelvec\ 966}#)))))
                         (#{wrap-subst\ 135}# #{w\ 965}#))))))
           (#{extend-ribcage!\ 147}#
             (lambda (#{ribcage\ 975}# #{id\ 976}# #{label\ 977}#)
               (begin
                 (#{set-ribcage-symnames!\ 143}#
                   #{ribcage\ 975}#
                   (cons (#{syntax-object-expression\ 116}# #{id\ 976}#)
                         (#{ribcage-symnames\ 140}# #{ribcage\ 975}#)))
                 (#{set-ribcage-marks!\ 144}#
                   #{ribcage\ 975}#
                   (cons (#{wrap-marks\ 134}#
                           (#{syntax-object-wrap\ 117}# #{id\ 976}#))
                         (#{ribcage-marks\ 141}# #{ribcage\ 975}#)))
                 (#{set-ribcage-labels!\ 145}#
                   #{ribcage\ 975}#
                   (cons #{label\ 977}#
                         (#{ribcage-labels\ 142}# #{ribcage\ 975}#))))))
           (#{anti-mark\ 146}#
             (lambda (#{w\ 978}#)
               (#{make-wrap\ 133}#
                 (cons #f (#{wrap-marks\ 134}# #{w\ 978}#))
                 (cons 'shift
                       (#{wrap-subst\ 135}# #{w\ 978}#)))))
           (#{set-ribcage-labels!\ 145}#
             (lambda (#{x\ 979}# #{update\ 980}#)
               (vector-set! #{x\ 979}# 3 #{update\ 980}#)))
           (#{set-ribcage-marks!\ 144}#
             (lambda (#{x\ 981}# #{update\ 982}#)
               (vector-set! #{x\ 981}# 2 #{update\ 982}#)))
           (#{set-ribcage-symnames!\ 143}#
             (lambda (#{x\ 983}# #{update\ 984}#)
               (vector-set! #{x\ 983}# 1 #{update\ 984}#)))
           (#{ribcage-labels\ 142}#
             (lambda (#{x\ 985}#) (vector-ref #{x\ 985}# 3)))
           (#{ribcage-marks\ 141}#
             (lambda (#{x\ 986}#) (vector-ref #{x\ 986}# 2)))
           (#{ribcage-symnames\ 140}#
             (lambda (#{x\ 987}#) (vector-ref #{x\ 987}# 1)))
           (#{ribcage?\ 139}#
             (lambda (#{x\ 988}#)
               (if (vector? #{x\ 988}#)
                 (if (= (vector-length #{x\ 988}#) 4)
                   (eq? (vector-ref #{x\ 988}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 138}#
             (lambda (#{symnames\ 989}#
                      #{marks\ 990}#
                      #{labels\ 991}#)
               (vector
                 'ribcage
                 #{symnames\ 989}#
                 #{marks\ 990}#
                 #{labels\ 991}#)))
           (#{gen-labels\ 137}#
             (lambda (#{ls\ 992}#)
               (if (null? #{ls\ 992}#)
                 '()
                 (cons (#{gen-label\ 136}#)
                       (#{gen-labels\ 137}# (cdr #{ls\ 992}#))))))
           (#{gen-label\ 136}# (lambda () (string #\i)))
           (#{wrap-subst\ 135}# cdr)
           (#{wrap-marks\ 134}# car)
           (#{make-wrap\ 133}# cons)
           (#{id-sym-name&marks\ 132}#
             (lambda (#{x\ 993}# #{w\ 994}#)
               (if (#{syntax-object?\ 115}# #{x\ 993}#)
                 (values
                   (#{syntax-object-expression\ 116}# #{x\ 993}#)
                   (#{join-marks\ 151}#
                     (#{wrap-marks\ 134}# #{w\ 994}#)
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{x\ 993}#))))
                 (values
                   #{x\ 993}#
                   (#{wrap-marks\ 134}# #{w\ 994}#)))))
           (#{id?\ 131}#
             (lambda (#{x\ 995}#)
               (if (symbol? #{x\ 995}#)
                 #t
                 (if (#{syntax-object?\ 115}# #{x\ 995}#)
                   (symbol?
                     (#{syntax-object-expression\ 116}# #{x\ 995}#))
                   #f))))
           (#{nonsymbol-id?\ 130}#
             (lambda (#{x\ 996}#)
               (if (#{syntax-object?\ 115}# #{x\ 996}#)
                 (symbol?
                   (#{syntax-object-expression\ 116}# #{x\ 996}#))
                 #f)))
           (#{global-extend\ 129}#
             (lambda (#{type\ 997}# #{sym\ 998}# #{val\ 999}#)
               (#{put-global-definition-hook\ 92}#
                 #{sym\ 998}#
                 #{type\ 997}#
                 #{val\ 999}#)))
           (#{lookup\ 128}#
             (lambda (#{x\ 1000}# #{r\ 1001}# #{mod\ 1002}#)
               (let ((#{t\ 1003}# (assq #{x\ 1000}# #{r\ 1001}#)))
                 (if #{t\ 1003}#
                   (cdr #{t\ 1003}#)
                   (if (symbol? #{x\ 1000}#)
                     (let ((#{t\ 1004}#
                             (#{get-global-definition-hook\ 93}#
                               #{x\ 1000}#
                               #{mod\ 1002}#)))
                       (if #{t\ 1004}# #{t\ 1004}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 127}#
             (lambda (#{r\ 1005}#)
               (if (null? #{r\ 1005}#)
                 '()
                 (let ((#{a\ 1006}# (car #{r\ 1005}#)))
                   (if (eq? (cadr #{a\ 1006}#) (quote macro))
                     (cons #{a\ 1006}#
                           (#{macros-only-env\ 127}# (cdr #{r\ 1005}#)))
                     (#{macros-only-env\ 127}# (cdr #{r\ 1005}#)))))))
           (#{extend-var-env\ 126}#
             (lambda (#{labels\ 1007}# #{vars\ 1008}# #{r\ 1009}#)
               (if (null? #{labels\ 1007}#)
                 #{r\ 1009}#
                 (#{extend-var-env\ 126}#
                   (cdr #{labels\ 1007}#)
                   (cdr #{vars\ 1008}#)
                   (cons (cons (car #{labels\ 1007}#)
                               (cons (quote lexical) (car #{vars\ 1008}#)))
                         #{r\ 1009}#)))))
           (#{extend-env\ 125}#
             (lambda (#{labels\ 1010}# #{bindings\ 1011}# #{r\ 1012}#)
               (if (null? #{labels\ 1010}#)
                 #{r\ 1012}#
                 (#{extend-env\ 125}#
                   (cdr #{labels\ 1010}#)
                   (cdr #{bindings\ 1011}#)
                   (cons (cons (car #{labels\ 1010}#)
                               (car #{bindings\ 1011}#))
                         #{r\ 1012}#)))))
           (#{binding-value\ 124}# cdr)
           (#{binding-type\ 123}# car)
           (#{source-annotation\ 122}#
             (lambda (#{x\ 1013}#)
               (if (#{syntax-object?\ 115}# #{x\ 1013}#)
                 (#{source-annotation\ 122}#
                   (#{syntax-object-expression\ 116}# #{x\ 1013}#))
                 (if (pair? #{x\ 1013}#)
                   (let ((#{props\ 1014}# (source-properties #{x\ 1013}#)))
                     (if (pair? #{props\ 1014}#) #{props\ 1014}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 121}#
             (lambda (#{x\ 1015}# #{update\ 1016}#)
               (vector-set! #{x\ 1015}# 3 #{update\ 1016}#)))
           (#{set-syntax-object-wrap!\ 120}#
             (lambda (#{x\ 1017}# #{update\ 1018}#)
               (vector-set! #{x\ 1017}# 2 #{update\ 1018}#)))
           (#{set-syntax-object-expression!\ 119}#
             (lambda (#{x\ 1019}# #{update\ 1020}#)
               (vector-set! #{x\ 1019}# 1 #{update\ 1020}#)))
           (#{syntax-object-module\ 118}#
             (lambda (#{x\ 1021}#) (vector-ref #{x\ 1021}# 3)))
           (#{syntax-object-wrap\ 117}#
             (lambda (#{x\ 1022}#) (vector-ref #{x\ 1022}# 2)))
           (#{syntax-object-expression\ 116}#
             (lambda (#{x\ 1023}#) (vector-ref #{x\ 1023}# 1)))
           (#{syntax-object?\ 115}#
             (lambda (#{x\ 1024}#)
               (if (vector? #{x\ 1024}#)
                 (if (= (vector-length #{x\ 1024}#) 4)
                   (eq? (vector-ref #{x\ 1024}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 114}#
             (lambda (#{expression\ 1025}#
                      #{wrap\ 1026}#
                      #{module\ 1027}#)
               (vector
                 'syntax-object
                 #{expression\ 1025}#
                 #{wrap\ 1026}#
                 #{module\ 1027}#)))
           (#{build-letrec\ 113}#
             (lambda (#{src\ 1028}#
                      #{ids\ 1029}#
                      #{vars\ 1030}#
                      #{val-exps\ 1031}#
                      #{body-exp\ 1032}#)
               (if (null? #{vars\ 1030}#)
                 #{body-exp\ 1032}#
                 (let ((#{atom-key\ 1033}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1033}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 1029}#
                         #{val-exps\ 1031}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 1028}#
                        #{ids\ 1029}#
                        #{vars\ 1030}#
                        #{val-exps\ 1031}#
                        #{body-exp\ 1032}#))
                     (#{decorate-source\ 94}#
                       (list 'letrec
                             (map list #{vars\ 1030}# #{val-exps\ 1031}#)
                             #{body-exp\ 1032}#)
                       #{src\ 1028}#))))))
           (#{build-named-let\ 112}#
             (lambda (#{src\ 1034}#
                      #{ids\ 1035}#
                      #{vars\ 1036}#
                      #{val-exps\ 1037}#
                      #{body-exp\ 1038}#)
               (let ((#{f\ 1039}# (car #{vars\ 1036}#))
                     (#{f-name\ 1040}# (car #{ids\ 1035}#))
                     (#{vars\ 1041}# (cdr #{vars\ 1036}#))
                     (#{ids\ 1042}# (cdr #{ids\ 1035}#)))
                 (let ((#{atom-key\ 1043}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1043}# (quote (c)))
                     (let ((#{proc\ 1044}#
                             (#{build-simple-lambda\ 105}#
                               #{src\ 1034}#
                               #{ids\ 1042}#
                               #f
                               #{vars\ 1041}#
                               #f
                               #{body-exp\ 1038}#)))
                       (begin
                         (#{maybe-name-value!\ 103}#
                           #{f-name\ 1040}#
                           #{proc\ 1044}#)
                         (for-each
                           #{maybe-name-value!\ 103}#
                           #{ids\ 1042}#
                           #{val-exps\ 1037}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 1034}#
                          (list #{f-name\ 1040}#)
                          (list #{f\ 1039}#)
                          (list #{proc\ 1044}#)
                          (#{build-application\ 96}#
                            #{src\ 1034}#
                            (#{build-lexical-reference\ 98}#
                              'fun
                              #{src\ 1034}#
                              #{f-name\ 1040}#
                              #{f\ 1039}#)
                            #{val-exps\ 1037}#))))
                     (#{decorate-source\ 94}#
                       (list 'let
                             #{f\ 1039}#
                             (map list #{vars\ 1041}# #{val-exps\ 1037}#)
                             #{body-exp\ 1038}#)
                       #{src\ 1034}#))))))
           (#{build-let\ 111}#
             (lambda (#{src\ 1045}#
                      #{ids\ 1046}#
                      #{vars\ 1047}#
                      #{val-exps\ 1048}#
                      #{body-exp\ 1049}#)
               (if (null? #{vars\ 1047}#)
                 #{body-exp\ 1049}#
                 (let ((#{atom-key\ 1050}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1050}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 1046}#
                         #{val-exps\ 1048}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 1045}#
                        #{ids\ 1046}#
                        #{vars\ 1047}#
                        #{val-exps\ 1048}#
                        #{body-exp\ 1049}#))
                     (#{decorate-source\ 94}#
                       (list 'let
                             (map list #{vars\ 1047}# #{val-exps\ 1048}#)
                             #{body-exp\ 1049}#)
                       #{src\ 1045}#))))))
           (#{build-sequence\ 110}#
             (lambda (#{src\ 1051}# #{exps\ 1052}#)
               (if (null? (cdr #{exps\ 1052}#))
                 (car #{exps\ 1052}#)
                 (let ((#{atom-key\ 1053}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1053}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 1051}#
                      #{exps\ 1052}#)
                     (#{decorate-source\ 94}#
                       (cons (quote begin) #{exps\ 1052}#)
                       #{src\ 1051}#))))))
           (#{build-data\ 109}#
             (lambda (#{src\ 1054}# #{exp\ 1055}#)
               (let ((#{atom-key\ 1056}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1056}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 1054}#
                    #{exp\ 1055}#)
                   (#{decorate-source\ 94}#
                     (if (if (self-evaluating? #{exp\ 1055}#)
                           (not (vector? #{exp\ 1055}#))
                           #f)
                       #{exp\ 1055}#
                       (list (quote quote) #{exp\ 1055}#))
                     #{src\ 1054}#)))))
           (#{build-primref\ 108}#
             (lambda (#{src\ 1057}# #{name\ 1058}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 1059}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1059}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 1057}#
                      #{name\ 1058}#)
                     (#{decorate-source\ 94}#
                       #{name\ 1058}#
                       #{src\ 1057}#)))
                 (let ((#{atom-key\ 1060}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1060}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 1057}#
                      '(guile)
                      #{name\ 1058}#
                      #f)
                     (#{decorate-source\ 94}#
                       (list (quote @@) (quote (guile)) #{name\ 1058}#)
                       #{src\ 1057}#))))))
           (#{build-lambda-case\ 107}#
             (lambda (#{src\ 1061}#
                      #{req\ 1062}#
                      #{opt\ 1063}#
                      #{rest\ 1064}#
                      #{kw\ 1065}#
                      #{inits\ 1066}#
                      #{vars\ 1067}#
                      #{body\ 1068}#
                      #{else-case\ 1069}#)
               (let ((#{atom-key\ 1070}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1070}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 1061}#
                    #{req\ 1062}#
                    #{opt\ 1063}#
                    #{rest\ 1064}#
                    #{kw\ 1065}#
                    #{inits\ 1066}#
                    #{vars\ 1067}#
                    #{body\ 1068}#
                    #{else-case\ 1069}#)
                   (let ((#{nreq\ 1071}# (length #{req\ 1062}#)))
                     (let ((#{nopt\ 1072}#
                             (if #{opt\ 1063}# (length #{opt\ 1063}#) 0)))
                       (let ((#{rest-idx\ 1073}#
                               (if #{rest\ 1064}#
                                 (+ #{nreq\ 1071}# #{nopt\ 1072}#)
                                 #f)))
                         (let ((#{allow-other-keys?\ 1074}#
                                 (if #{kw\ 1065}# (car #{kw\ 1065}#) #f)))
                           (let ((#{kw-indices\ 1075}#
                                   (map (lambda (#{x\ 1076}#)
                                          (cons (car #{x\ 1076}#)
                                                (list-index
                                                  #{vars\ 1067}#
                                                  (caddr #{x\ 1076}#))))
                                        (if #{kw\ 1065}#
                                          (cdr #{kw\ 1065}#)
                                          '()))))
                             (let ((#{nargs\ 1077}#
                                     (apply max
                                            (+ #{nreq\ 1071}#
                                               #{nopt\ 1072}#
                                               (if #{rest\ 1064}# 1 0))
                                            (map 1+
                                                 (map cdr
                                                      #{kw-indices\ 1075}#)))))
                               (begin
                                 (let ((#{t\ 1078}#
                                         (= #{nargs\ 1077}#
                                            (length #{vars\ 1067}#)
                                            (+ #{nreq\ 1071}#
                                               (length #{inits\ 1066}#)
                                               (if #{rest\ 1064}# 1 0)))))
                                   (if #{t\ 1078}#
                                     #{t\ 1078}#
                                     (error "something went wrong"
                                            #{req\ 1062}#
                                            #{opt\ 1063}#
                                            #{rest\ 1064}#
                                            #{kw\ 1065}#
                                            #{inits\ 1066}#
                                            #{vars\ 1067}#
                                            #{nreq\ 1071}#
                                            #{nopt\ 1072}#
                                            #{kw-indices\ 1075}#
                                            #{nargs\ 1077}#)))
                                 (#{decorate-source\ 94}#
                                   (cons (list (cons '(@@ (ice-9 optargs)
                                                          parse-lambda-case)
                                                     (cons (list 'quote
                                                                 (list #{nreq\ 1071}#
                                                                       #{nopt\ 1072}#
                                                                       #{rest-idx\ 1073}#
                                                                       #{nargs\ 1077}#
                                                                       #{allow-other-keys?\ 1074}#
                                                                       #{kw-indices\ 1075}#))
                                                           (cons (cons 'list
                                                                       (map (lambda (#{i\ 1079}#)
                                                                              (list 'lambda
                                                                                    #{vars\ 1067}#
                                                                                    #{i\ 1079}#))
                                                                            #{inits\ 1066}#))
                                                                 '(%%args))))
                                               '=>
                                               (list 'lambda
                                                     '(%%%args . _)
                                                     (cons 'apply
                                                           (cons (list 'lambda
                                                                       #{vars\ 1067}#
                                                                       #{body\ 1068}#)
                                                                 '(%%%args)))))
                                         (let ((#{t\ 1080}#
                                                 #{else-case\ 1069}#))
                                           (if #{t\ 1080}#
                                             #{t\ 1080}#
                                             '((%%args
                                                 (error "wrong number of arguments"
                                                        %%args))))))
                                   #{src\ 1061}#))))))))))))
           (#{build-case-lambda\ 106}#
             (lambda (#{src\ 1081}#
                      #{docstring\ 1082}#
                      #{body\ 1083}#)
               (let ((#{atom-key\ 1084}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1084}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1081}#
                    (if #{docstring\ 1082}#
                      (list (cons (quote documentation) #{docstring\ 1082}#))
                      '())
                    #{body\ 1083}#)
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 1082}#
                                     (list #{docstring\ 1082}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 1083}#)))))
                     #{src\ 1081}#)))))
           (#{build-simple-lambda\ 105}#
             (lambda (#{src\ 1085}#
                      #{req\ 1086}#
                      #{rest\ 1087}#
                      #{vars\ 1088}#
                      #{docstring\ 1089}#
                      #{exp\ 1090}#)
               (let ((#{atom-key\ 1091}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1091}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1085}#
                    (if #{docstring\ 1089}#
                      (list (cons (quote documentation) #{docstring\ 1089}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 1085}#
                     #{req\ 1086}#
                     #f
                     #{rest\ 1087}#
                     #f
                     '()
                     #{vars\ 1088}#
                     #{exp\ 1090}#
                     #f))
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons (if #{rest\ 1087}#
                                   (apply cons* #{vars\ 1088}#)
                                   #{vars\ 1088}#)
                                 (append
                                   (if #{docstring\ 1089}#
                                     (list #{docstring\ 1089}#)
                                     '())
                                   (list #{exp\ 1090}#))))
                     #{src\ 1085}#)))))
           (#{build-global-definition\ 104}#
             (lambda (#{source\ 1092}# #{var\ 1093}# #{exp\ 1094}#)
               (let ((#{atom-key\ 1095}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1095}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 103}#
                       #{var\ 1093}#
                       #{exp\ 1094}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 1092}#
                      #{var\ 1093}#
                      #{exp\ 1094}#))
                   (#{decorate-source\ 94}#
                     (list (quote define) #{var\ 1093}# #{exp\ 1094}#)
                     #{source\ 1092}#)))))
           (#{maybe-name-value!\ 103}#
             (lambda (#{name\ 1096}# #{val\ 1097}#)
               (if ((@ (language tree-il) lambda?) #{val\ 1097}#)
                 (let ((#{meta\ 1098}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 1097}#)))
                   (if (not (assq (quote name) #{meta\ 1098}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 1097}#
                      (acons 'name
                             #{name\ 1096}#
                             #{meta\ 1098}#)))))))
           (#{build-global-assignment\ 102}#
             (lambda (#{source\ 1099}#
                      #{var\ 1100}#
                      #{exp\ 1101}#
                      #{mod\ 1102}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 1102}#
                 #{var\ 1100}#
                 (lambda (#{mod\ 1103}# #{var\ 1104}# #{public?\ 1105}#)
                   (let ((#{atom-key\ 1106}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1106}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 1099}#
                        #{mod\ 1103}#
                        #{var\ 1104}#
                        #{public?\ 1105}#
                        #{exp\ 1101}#)
                       (#{decorate-source\ 94}#
                         (list 'set!
                               (list (if #{public?\ 1105}#
                                       '@
                                       '@@)
                                     #{mod\ 1103}#
                                     #{var\ 1104}#)
                               #{exp\ 1101}#)
                         #{source\ 1099}#))))
                 (lambda (#{var\ 1107}#)
                   (let ((#{atom-key\ 1108}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1108}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 1099}#
                        #{var\ 1107}#
                        #{exp\ 1101}#)
                       (#{decorate-source\ 94}#
                         (list (quote set!) #{var\ 1107}# #{exp\ 1101}#)
                         #{source\ 1099}#)))))))
           (#{build-global-reference\ 101}#
             (lambda (#{source\ 1109}# #{var\ 1110}# #{mod\ 1111}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 1111}#
                 #{var\ 1110}#
                 (lambda (#{mod\ 1112}# #{var\ 1113}# #{public?\ 1114}#)
                   (let ((#{atom-key\ 1115}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1115}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 1109}#
                        #{mod\ 1112}#
                        #{var\ 1113}#
                        #{public?\ 1114}#)
                       (#{decorate-source\ 94}#
                         (list (if #{public?\ 1114}# (quote @) (quote @@))
                               #{mod\ 1112}#
                               #{var\ 1113}#)
                         #{source\ 1109}#))))
                 (lambda (#{var\ 1116}#)
                   (let ((#{atom-key\ 1117}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1117}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 1109}#
                        #{var\ 1116}#)
                       (#{decorate-source\ 94}#
                         #{var\ 1116}#
                         #{source\ 1109}#)))))))
           (#{analyze-variable\ 100}#
             (lambda (#{mod\ 1118}#
                      #{var\ 1119}#
                      #{modref-cont\ 1120}#
                      #{bare-cont\ 1121}#)
               (if (not #{mod\ 1118}#)
                 (#{bare-cont\ 1121}# #{var\ 1119}#)
                 (let ((#{kind\ 1122}# (car #{mod\ 1118}#))
                       (#{mod\ 1123}# (cdr #{mod\ 1118}#)))
                   (if (memv #{kind\ 1122}# (quote (public)))
                     (#{modref-cont\ 1120}#
                       #{mod\ 1123}#
                       #{var\ 1119}#
                       #t)
                     (if (memv #{kind\ 1122}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 1123}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 1120}#
                           #{mod\ 1123}#
                           #{var\ 1119}#
                           #f)
                         (#{bare-cont\ 1121}# #{var\ 1119}#))
                       (if (memv #{kind\ 1122}# (quote (bare)))
                         (#{bare-cont\ 1121}# #{var\ 1119}#)
                         (if (memv #{kind\ 1122}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 1123}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 1123}#)
                                   #{var\ 1119}#)
                                 #f)
                             (#{modref-cont\ 1120}#
                               #{mod\ 1123}#
                               #{var\ 1119}#
                               #f)
                             (#{bare-cont\ 1121}# #{var\ 1119}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 1119}#
                             #{mod\ 1123}#)))))))))
           (#{build-lexical-assignment\ 99}#
             (lambda (#{source\ 1124}#
                      #{name\ 1125}#
                      #{var\ 1126}#
                      #{exp\ 1127}#)
               (let ((#{atom-key\ 1128}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1128}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 1124}#
                    #{name\ 1125}#
                    #{var\ 1126}#
                    #{exp\ 1127}#)
                   (#{decorate-source\ 94}#
                     (list (quote set!) #{var\ 1126}# #{exp\ 1127}#)
                     #{source\ 1124}#)))))
           (#{build-lexical-reference\ 98}#
             (lambda (#{type\ 1129}#
                      #{source\ 1130}#
                      #{name\ 1131}#
                      #{var\ 1132}#)
               (let ((#{atom-key\ 1133}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1133}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 1130}#
                    #{name\ 1131}#
                    #{var\ 1132}#)
                   (#{decorate-source\ 94}#
                     #{var\ 1132}#
                     #{source\ 1130}#)))))
           (#{build-conditional\ 97}#
             (lambda (#{source\ 1134}#
                      #{test-exp\ 1135}#
                      #{then-exp\ 1136}#
                      #{else-exp\ 1137}#)
               (let ((#{atom-key\ 1138}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1138}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 1134}#
                    #{test-exp\ 1135}#
                    #{then-exp\ 1136}#
                    #{else-exp\ 1137}#)
                   (#{decorate-source\ 94}#
                     (if (equal? #{else-exp\ 1137}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 1135}#
                             #{then-exp\ 1136}#)
                       (list 'if
                             #{test-exp\ 1135}#
                             #{then-exp\ 1136}#
                             #{else-exp\ 1137}#))
                     #{source\ 1134}#)))))
           (#{build-application\ 96}#
             (lambda (#{source\ 1139}#
                      #{fun-exp\ 1140}#
                      #{arg-exps\ 1141}#)
               (let ((#{atom-key\ 1142}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1142}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 1139}#
                    #{fun-exp\ 1140}#
                    #{arg-exps\ 1141}#)
                   (#{decorate-source\ 94}#
                     (cons #{fun-exp\ 1140}# #{arg-exps\ 1141}#)
                     #{source\ 1139}#)))))
           (#{build-void\ 95}#
             (lambda (#{source\ 1143}#)
               (let ((#{atom-key\ 1144}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1144}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 1143}#)
                   (#{decorate-source\ 94}#
                     '(if #f #f)
                     #{source\ 1143}#)))))
           (#{decorate-source\ 94}#
             (lambda (#{e\ 1145}# #{s\ 1146}#)
               (begin
                 (if (if (pair? #{e\ 1145}#) #{s\ 1146}# #f)
                   (set-source-properties! #{e\ 1145}# #{s\ 1146}#))
                 #{e\ 1145}#)))
           (#{get-global-definition-hook\ 93}#
             (lambda (#{symbol\ 1147}# #{module\ 1148}#)
               (begin
                 (if (if (not #{module\ 1148}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 1147}#))
                 (let ((#{v\ 1149}#
                         (module-variable
                           (if #{module\ 1148}#
                             (resolve-module (cdr #{module\ 1148}#))
                             (current-module))
                           #{symbol\ 1147}#)))
                   (if #{v\ 1149}#
                     (if (variable-bound? #{v\ 1149}#)
                       (let ((#{val\ 1150}# (variable-ref #{v\ 1149}#)))
                         (if (macro? #{val\ 1150}#)
                           (if (syncase-macro-type #{val\ 1150}#)
                             (cons (syncase-macro-type #{val\ 1150}#)
                                   (syncase-macro-binding #{val\ 1150}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 92}#
             (lambda (#{symbol\ 1151}# #{type\ 1152}# #{val\ 1153}#)
               (let ((#{existing\ 1154}#
                       (let ((#{v\ 1155}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 1151}#)))
                         (if #{v\ 1155}#
                           (if (variable-bound? #{v\ 1155}#)
                             (let ((#{val\ 1156}# (variable-ref #{v\ 1155}#)))
                               (if (macro? #{val\ 1156}#)
                                 (if (not (syncase-macro-type #{val\ 1156}#))
                                   #{val\ 1156}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 1151}#
                   (if #{existing\ 1154}#
                     (make-extended-syncase-macro
                       #{existing\ 1154}#
                       #{type\ 1152}#
                       #{val\ 1153}#)
                     (make-syncase-macro #{type\ 1152}# #{val\ 1153}#))))))
           (#{local-eval-hook\ 91}#
             (lambda (#{x\ 1157}# #{mod\ 1158}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 1159}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 1159}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1157}#)
                           #{x\ 1157}#))))))
           (#{top-level-eval-hook\ 90}#
             (lambda (#{x\ 1160}# #{mod\ 1161}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 1162}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 1162}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1160}#)
                           #{x\ 1160}#))))))
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
        (lambda (#{e\ 1163}#
                 #{r\ 1164}#
                 #{w\ 1165}#
                 #{s\ 1166}#
                 #{mod\ 1167}#)
          ((lambda (#{tmp\ 1168}#)
             ((lambda (#{tmp\ 1169}#)
                (if (if #{tmp\ 1169}#
                      (apply (lambda (#{_\ 1170}#
                                      #{var\ 1171}#
                                      #{val\ 1172}#
                                      #{e1\ 1173}#
                                      #{e2\ 1174}#)
                               (#{valid-bound-ids?\ 156}# #{var\ 1171}#))
                             #{tmp\ 1169}#)
                      #f)
                  (apply (lambda (#{_\ 1176}#
                                  #{var\ 1177}#
                                  #{val\ 1178}#
                                  #{e1\ 1179}#
                                  #{e2\ 1180}#)
                           (let ((#{names\ 1181}#
                                   (map (lambda (#{x\ 1182}#)
                                          (#{id-var-name\ 153}#
                                            #{x\ 1182}#
                                            #{w\ 1165}#))
                                        #{var\ 1177}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 1184}# #{n\ 1185}#)
                                   (let ((#{atom-key\ 1186}#
                                           (#{binding-type\ 123}#
                                             (#{lookup\ 128}#
                                               #{n\ 1185}#
                                               #{r\ 1164}#
                                               #{mod\ 1167}#))))
                                     (if (memv #{atom-key\ 1186}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 1163}#
                                         (#{source-wrap\ 160}#
                                           #{id\ 1184}#
                                           #{w\ 1165}#
                                           #{s\ 1166}#
                                           #{mod\ 1167}#)))))
                                 #{var\ 1177}#
                                 #{names\ 1181}#)
                               (#{chi-body\ 171}#
                                 (cons #{e1\ 1179}# #{e2\ 1180}#)
                                 (#{source-wrap\ 160}#
                                   #{e\ 1163}#
                                   #{w\ 1165}#
                                   #{s\ 1166}#
                                   #{mod\ 1167}#)
                                 (#{extend-env\ 125}#
                                   #{names\ 1181}#
                                   (let ((#{trans-r\ 1189}#
                                           (#{macros-only-env\ 127}#
                                             #{r\ 1164}#)))
                                     (map (lambda (#{x\ 1190}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 173}#
                                                    (#{chi\ 167}#
                                                      #{x\ 1190}#
                                                      #{trans-r\ 1189}#
                                                      #{w\ 1165}#
                                                      #{mod\ 1167}#)
                                                    #{mod\ 1167}#)))
                                          #{val\ 1178}#))
                                   #{r\ 1164}#)
                                 #{w\ 1165}#
                                 #{mod\ 1167}#))))
                         #{tmp\ 1169}#)
                  ((lambda (#{_\ 1192}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 1163}#
                         #{w\ 1165}#
                         #{s\ 1166}#
                         #{mod\ 1167}#)))
                   #{tmp\ 1168}#)))
              ($sc-dispatch
                #{tmp\ 1168}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1163}#)))
      (#{global-extend\ 129}#
        'core
        'quote
        (lambda (#{e\ 1193}#
                 #{r\ 1194}#
                 #{w\ 1195}#
                 #{s\ 1196}#
                 #{mod\ 1197}#)
          ((lambda (#{tmp\ 1198}#)
             ((lambda (#{tmp\ 1199}#)
                (if #{tmp\ 1199}#
                  (apply (lambda (#{_\ 1200}# #{e\ 1201}#)
                           (#{build-data\ 109}#
                             #{s\ 1196}#
                             (#{strip\ 180}# #{e\ 1201}# #{w\ 1195}#)))
                         #{tmp\ 1199}#)
                  ((lambda (#{_\ 1202}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 1193}#
                         #{w\ 1195}#
                         #{s\ 1196}#
                         #{mod\ 1197}#)))
                   #{tmp\ 1198}#)))
              ($sc-dispatch #{tmp\ 1198}# (quote (any any)))))
           #{e\ 1193}#)))
      (#{global-extend\ 129}#
        'core
        'syntax
        (letrec ((#{regen\ 1210}#
                   (lambda (#{x\ 1211}#)
                     (let ((#{atom-key\ 1212}# (car #{x\ 1211}#)))
                       (if (memv #{atom-key\ 1212}# (quote (ref)))
                         (#{build-lexical-reference\ 98}#
                           'value
                           #f
                           (cadr #{x\ 1211}#)
                           (cadr #{x\ 1211}#))
                         (if (memv #{atom-key\ 1212}# (quote (primitive)))
                           (#{build-primref\ 108}# #f (cadr #{x\ 1211}#))
                           (if (memv #{atom-key\ 1212}# (quote (quote)))
                             (#{build-data\ 109}# #f (cadr #{x\ 1211}#))
                             (if (memv #{atom-key\ 1212}# (quote (lambda)))
                               (if (list? (cadr #{x\ 1211}#))
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (cadr #{x\ 1211}#)
                                   #f
                                   (cadr #{x\ 1211}#)
                                   #f
                                   (#{regen\ 1210}# (caddr #{x\ 1211}#)))
                                 (error "how did we get here" #{x\ 1211}#))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-primref\ 108}# #f (car #{x\ 1211}#))
                                 (map #{regen\ 1210}#
                                      (cdr #{x\ 1211}#))))))))))
                 (#{gen-vector\ 1209}#
                   (lambda (#{x\ 1213}#)
                     (if (eq? (car #{x\ 1213}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 1213}#))
                       (if (eq? (car #{x\ 1213}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 1213}#)))
                         (list (quote list->vector) #{x\ 1213}#)))))
                 (#{gen-append\ 1208}#
                   (lambda (#{x\ 1214}# #{y\ 1215}#)
                     (if (equal? #{y\ 1215}# (quote (quote ())))
                       #{x\ 1214}#
                       (list (quote append) #{x\ 1214}# #{y\ 1215}#))))
                 (#{gen-cons\ 1207}#
                   (lambda (#{x\ 1216}# #{y\ 1217}#)
                     (let ((#{atom-key\ 1218}# (car #{y\ 1217}#)))
                       (if (memv #{atom-key\ 1218}# (quote (quote)))
                         (if (eq? (car #{x\ 1216}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 1216}#) (cadr #{y\ 1217}#)))
                           (if (eq? (cadr #{y\ 1217}#) (quote ()))
                             (list (quote list) #{x\ 1216}#)
                             (list (quote cons) #{x\ 1216}# #{y\ 1217}#)))
                         (if (memv #{atom-key\ 1218}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 1216}# (cdr #{y\ 1217}#)))
                           (list (quote cons) #{x\ 1216}# #{y\ 1217}#))))))
                 (#{gen-map\ 1206}#
                   (lambda (#{e\ 1219}# #{map-env\ 1220}#)
                     (let ((#{formals\ 1221}# (map cdr #{map-env\ 1220}#))
                           (#{actuals\ 1222}#
                             (map (lambda (#{x\ 1223}#)
                                    (list (quote ref) (car #{x\ 1223}#)))
                                  #{map-env\ 1220}#)))
                       (if (eq? (car #{e\ 1219}#) (quote ref))
                         (car #{actuals\ 1222}#)
                         (if (and-map
                               (lambda (#{x\ 1224}#)
                                 (if (eq? (car #{x\ 1224}#) (quote ref))
                                   (memq (cadr #{x\ 1224}#) #{formals\ 1221}#)
                                   #f))
                               (cdr #{e\ 1219}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 1219}#))
                                       (map (let ((#{r\ 1225}#
                                                    (map cons
                                                         #{formals\ 1221}#
                                                         #{actuals\ 1222}#)))
                                              (lambda (#{x\ 1226}#)
                                                (cdr (assq (cadr #{x\ 1226}#)
                                                           #{r\ 1225}#))))
                                            (cdr #{e\ 1219}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 1221}#
                                             #{e\ 1219}#)
                                       #{actuals\ 1222}#)))))))
                 (#{gen-mappend\ 1205}#
                   (lambda (#{e\ 1227}# #{map-env\ 1228}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 1206}# #{e\ 1227}# #{map-env\ 1228}#))))
                 (#{gen-ref\ 1204}#
                   (lambda (#{src\ 1229}#
                            #{var\ 1230}#
                            #{level\ 1231}#
                            #{maps\ 1232}#)
                     (if (#{fx=\ 88}# #{level\ 1231}# 0)
                       (values #{var\ 1230}# #{maps\ 1232}#)
                       (if (null? #{maps\ 1232}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 1229}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 1204}#
                               #{src\ 1229}#
                               #{var\ 1230}#
                               (#{fx-\ 87}# #{level\ 1231}# 1)
                               (cdr #{maps\ 1232}#)))
                           (lambda (#{outer-var\ 1233}# #{outer-maps\ 1234}#)
                             (let ((#{b\ 1235}#
                                     (assq #{outer-var\ 1233}#
                                           (car #{maps\ 1232}#))))
                               (if #{b\ 1235}#
                                 (values (cdr #{b\ 1235}#) #{maps\ 1232}#)
                                 (let ((#{inner-var\ 1236}#
                                         (#{gen-var\ 181}# (quote tmp))))
                                   (values
                                     #{inner-var\ 1236}#
                                     (cons (cons (cons #{outer-var\ 1233}#
                                                       #{inner-var\ 1236}#)
                                                 (car #{maps\ 1232}#))
                                           #{outer-maps\ 1234}#)))))))))))
                 (#{gen-syntax\ 1203}#
                   (lambda (#{src\ 1237}#
                            #{e\ 1238}#
                            #{r\ 1239}#
                            #{maps\ 1240}#
                            #{ellipsis?\ 1241}#
                            #{mod\ 1242}#)
                     (if (#{id?\ 131}# #{e\ 1238}#)
                       (let ((#{label\ 1243}#
                               (#{id-var-name\ 153}#
                                 #{e\ 1238}#
                                 '(()))))
                         (let ((#{b\ 1244}#
                                 (#{lookup\ 128}#
                                   #{label\ 1243}#
                                   #{r\ 1239}#
                                   #{mod\ 1242}#)))
                           (if (eq? (#{binding-type\ 123}# #{b\ 1244}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 1245}#
                                         (#{binding-value\ 124}# #{b\ 1244}#)))
                                   (#{gen-ref\ 1204}#
                                     #{src\ 1237}#
                                     (car #{var.lev\ 1245}#)
                                     (cdr #{var.lev\ 1245}#)
                                     #{maps\ 1240}#)))
                               (lambda (#{var\ 1246}# #{maps\ 1247}#)
                                 (values
                                   (list (quote ref) #{var\ 1246}#)
                                   #{maps\ 1247}#)))
                             (if (#{ellipsis?\ 1241}# #{e\ 1238}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 1237}#)
                               (values
                                 (list (quote quote) #{e\ 1238}#)
                                 #{maps\ 1240}#)))))
                       ((lambda (#{tmp\ 1248}#)
                          ((lambda (#{tmp\ 1249}#)
                             (if (if #{tmp\ 1249}#
                                   (apply (lambda (#{dots\ 1250}# #{e\ 1251}#)
                                            (#{ellipsis?\ 1241}#
                                              #{dots\ 1250}#))
                                          #{tmp\ 1249}#)
                                   #f)
                               (apply (lambda (#{dots\ 1252}# #{e\ 1253}#)
                                        (#{gen-syntax\ 1203}#
                                          #{src\ 1237}#
                                          #{e\ 1253}#
                                          #{r\ 1239}#
                                          #{maps\ 1240}#
                                          (lambda (#{x\ 1254}#) #f)
                                          #{mod\ 1242}#))
                                      #{tmp\ 1249}#)
                               ((lambda (#{tmp\ 1255}#)
                                  (if (if #{tmp\ 1255}#
                                        (apply (lambda (#{x\ 1256}#
                                                        #{dots\ 1257}#
                                                        #{y\ 1258}#)
                                                 (#{ellipsis?\ 1241}#
                                                   #{dots\ 1257}#))
                                               #{tmp\ 1255}#)
                                        #f)
                                    (apply (lambda (#{x\ 1259}#
                                                    #{dots\ 1260}#
                                                    #{y\ 1261}#)
                                             (letrec ((#{f\ 1262}#
                                                        (lambda (#{y\ 1263}#
                                                                 #{k\ 1264}#)
                                                          ((lambda (#{tmp\ 1268}#)
                                                             ((lambda (#{tmp\ 1269}#)
                                                                (if (if #{tmp\ 1269}#
                                                                      (apply (lambda (#{dots\ 1270}#
                                                                                      #{y\ 1271}#)
                                                                               (#{ellipsis?\ 1241}#
                                                                                 #{dots\ 1270}#))
                                                                             #{tmp\ 1269}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 1272}#
                                                                                  #{y\ 1273}#)
                                                                           (#{f\ 1262}#
                                                                             #{y\ 1273}#
                                                                             (lambda (#{maps\ 1274}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 1264}#
                                                                                     (cons '()
                                                                                           #{maps\ 1274}#)))
                                                                                 (lambda (#{x\ 1275}#
                                                                                          #{maps\ 1276}#)
                                                                                   (if (null? (car #{maps\ 1276}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 1237}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 1205}#
                                                                                         #{x\ 1275}#
                                                                                         (car #{maps\ 1276}#))
                                                                                       (cdr #{maps\ 1276}#))))))))
                                                                         #{tmp\ 1269}#)
                                                                  ((lambda (#{_\ 1277}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 1203}#
                                                                           #{src\ 1237}#
                                                                           #{y\ 1263}#
                                                                           #{r\ 1239}#
                                                                           #{maps\ 1240}#
                                                                           #{ellipsis?\ 1241}#
                                                                           #{mod\ 1242}#))
                                                                       (lambda (#{y\ 1278}#
                                                                                #{maps\ 1279}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 1264}#
                                                                               #{maps\ 1279}#))
                                                                           (lambda (#{x\ 1280}#
                                                                                    #{maps\ 1281}#)
                                                                             (values
                                                                               (#{gen-append\ 1208}#
                                                                                 #{x\ 1280}#
                                                                                 #{y\ 1278}#)
                                                                               #{maps\ 1281}#))))))
                                                                   #{tmp\ 1268}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 1268}#
                                                                '(any . any))))
                                                           #{y\ 1263}#))))
                                               (#{f\ 1262}#
                                                 #{y\ 1261}#
                                                 (lambda (#{maps\ 1265}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 1203}#
                                                         #{src\ 1237}#
                                                         #{x\ 1259}#
                                                         #{r\ 1239}#
                                                         (cons '()
                                                               #{maps\ 1265}#)
                                                         #{ellipsis?\ 1241}#
                                                         #{mod\ 1242}#))
                                                     (lambda (#{x\ 1266}#
                                                              #{maps\ 1267}#)
                                                       (if (null? (car #{maps\ 1267}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 1237}#)
                                                         (values
                                                           (#{gen-map\ 1206}#
                                                             #{x\ 1266}#
                                                             (car #{maps\ 1267}#))
                                                           (cdr #{maps\ 1267}#)))))))))
                                           #{tmp\ 1255}#)
                                    ((lambda (#{tmp\ 1282}#)
                                       (if #{tmp\ 1282}#
                                         (apply (lambda (#{x\ 1283}#
                                                         #{y\ 1284}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 1203}#
                                                        #{src\ 1237}#
                                                        #{x\ 1283}#
                                                        #{r\ 1239}#
                                                        #{maps\ 1240}#
                                                        #{ellipsis?\ 1241}#
                                                        #{mod\ 1242}#))
                                                    (lambda (#{x\ 1285}#
                                                             #{maps\ 1286}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 1203}#
                                                            #{src\ 1237}#
                                                            #{y\ 1284}#
                                                            #{r\ 1239}#
                                                            #{maps\ 1286}#
                                                            #{ellipsis?\ 1241}#
                                                            #{mod\ 1242}#))
                                                        (lambda (#{y\ 1287}#
                                                                 #{maps\ 1288}#)
                                                          (values
                                                            (#{gen-cons\ 1207}#
                                                              #{x\ 1285}#
                                                              #{y\ 1287}#)
                                                            #{maps\ 1288}#))))))
                                                #{tmp\ 1282}#)
                                         ((lambda (#{tmp\ 1289}#)
                                            (if #{tmp\ 1289}#
                                              (apply (lambda (#{e1\ 1290}#
                                                              #{e2\ 1291}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 1203}#
                                                             #{src\ 1237}#
                                                             (cons #{e1\ 1290}#
                                                                   #{e2\ 1291}#)
                                                             #{r\ 1239}#
                                                             #{maps\ 1240}#
                                                             #{ellipsis?\ 1241}#
                                                             #{mod\ 1242}#))
                                                         (lambda (#{e\ 1293}#
                                                                  #{maps\ 1294}#)
                                                           (values
                                                             (#{gen-vector\ 1209}#
                                                               #{e\ 1293}#)
                                                             #{maps\ 1294}#))))
                                                     #{tmp\ 1289}#)
                                              ((lambda (#{_\ 1295}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 1238}#)
                                                   #{maps\ 1240}#))
                                               #{tmp\ 1248}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1248}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 1248}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 1248}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 1248}# (quote (any any)))))
                        #{e\ 1238}#)))))
          (lambda (#{e\ 1296}#
                   #{r\ 1297}#
                   #{w\ 1298}#
                   #{s\ 1299}#
                   #{mod\ 1300}#)
            (let ((#{e\ 1301}#
                    (#{source-wrap\ 160}#
                      #{e\ 1296}#
                      #{w\ 1298}#
                      #{s\ 1299}#
                      #{mod\ 1300}#)))
              ((lambda (#{tmp\ 1302}#)
                 ((lambda (#{tmp\ 1303}#)
                    (if #{tmp\ 1303}#
                      (apply (lambda (#{_\ 1304}# #{x\ 1305}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 1203}#
                                     #{e\ 1301}#
                                     #{x\ 1305}#
                                     #{r\ 1297}#
                                     '()
                                     #{ellipsis?\ 175}#
                                     #{mod\ 1300}#))
                                 (lambda (#{e\ 1306}# #{maps\ 1307}#)
                                   (#{regen\ 1210}# #{e\ 1306}#))))
                             #{tmp\ 1303}#)
                      ((lambda (#{_\ 1308}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 1301}#))
                       #{tmp\ 1302}#)))
                  ($sc-dispatch #{tmp\ 1302}# (quote (any any)))))
               #{e\ 1301}#)))))
      (#{global-extend\ 129}#
        'core
        'lambda
        (lambda (#{e\ 1309}#
                 #{r\ 1310}#
                 #{w\ 1311}#
                 #{s\ 1312}#
                 #{mod\ 1313}#)
          ((lambda (#{tmp\ 1314}#)
             ((lambda (#{tmp\ 1315}#)
                (if (if #{tmp\ 1315}#
                      (apply (lambda (#{_\ 1316}#
                                      #{args\ 1317}#
                                      #{docstring\ 1318}#
                                      #{e1\ 1319}#
                                      #{e2\ 1320}#)
                               (string? (syntax->datum #{docstring\ 1318}#)))
                             #{tmp\ 1315}#)
                      #f)
                  (apply (lambda (#{_\ 1321}#
                                  #{args\ 1322}#
                                  #{docstring\ 1323}#
                                  #{e1\ 1324}#
                                  #{e2\ 1325}#)
                           (call-with-values
                             (lambda ()
                               (#{lambda-formals\ 176}# #{args\ 1322}#))
                             (lambda (#{req\ 1326}#
                                      #{opt\ 1327}#
                                      #{rest\ 1328}#
                                      #{kw\ 1329}#)
                               (#{chi-simple-lambda\ 177}#
                                 #{e\ 1309}#
                                 #{r\ 1310}#
                                 #{w\ 1311}#
                                 #{s\ 1312}#
                                 #{mod\ 1313}#
                                 #{req\ 1326}#
                                 #{rest\ 1328}#
                                 (syntax->datum #{docstring\ 1323}#)
                                 (cons #{e1\ 1324}# #{e2\ 1325}#)))))
                         #{tmp\ 1315}#)
                  ((lambda (#{tmp\ 1331}#)
                     (if #{tmp\ 1331}#
                       (apply (lambda (#{_\ 1332}#
                                       #{args\ 1333}#
                                       #{e1\ 1334}#
                                       #{e2\ 1335}#)
                                (call-with-values
                                  (lambda ()
                                    (#{lambda-formals\ 176}# #{args\ 1333}#))
                                  (lambda (#{req\ 1336}#
                                           #{opt\ 1337}#
                                           #{rest\ 1338}#
                                           #{kw\ 1339}#)
                                    (#{chi-simple-lambda\ 177}#
                                      #{e\ 1309}#
                                      #{r\ 1310}#
                                      #{w\ 1311}#
                                      #{s\ 1312}#
                                      #{mod\ 1313}#
                                      #{req\ 1336}#
                                      #{rest\ 1338}#
                                      #f
                                      (cons #{e1\ 1334}# #{e2\ 1335}#)))))
                              #{tmp\ 1331}#)
                       ((lambda (#{_\ 1341}#)
                          (syntax-violation
                            'lambda
                            "bad lambda"
                            #{e\ 1309}#))
                        #{tmp\ 1314}#)))
                   ($sc-dispatch
                     #{tmp\ 1314}#
                     '(any any any . each-any)))))
              ($sc-dispatch
                #{tmp\ 1314}#
                '(any any any any . each-any))))
           #{e\ 1309}#)))
      (#{global-extend\ 129}#
        'core
        'lambda*
        (lambda (#{e\ 1342}#
                 #{r\ 1343}#
                 #{w\ 1344}#
                 #{s\ 1345}#
                 #{mod\ 1346}#)
          ((lambda (#{tmp\ 1347}#)
             ((lambda (#{tmp\ 1348}#)
                (if #{tmp\ 1348}#
                  (apply (lambda (#{_\ 1349}#
                                  #{args\ 1350}#
                                  #{e1\ 1351}#
                                  #{e2\ 1352}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1342}#
                                 #{r\ 1343}#
                                 #{w\ 1344}#
                                 #{s\ 1345}#
                                 #{mod\ 1346}#
                                 #{lambda*-formals\ 178}#
                                 (list (cons #{args\ 1350}#
                                             (cons #{e1\ 1351}#
                                                   #{e2\ 1352}#)))))
                             (lambda (#{docstring\ 1354}# #{lcase\ 1355}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1345}#
                                 #{docstring\ 1354}#
                                 #{lcase\ 1355}#))))
                         #{tmp\ 1348}#)
                  ((lambda (#{_\ 1356}#)
                     (syntax-violation
                       'lambda
                       "bad lambda*"
                       #{e\ 1342}#))
                   #{tmp\ 1347}#)))
              ($sc-dispatch
                #{tmp\ 1347}#
                '(any any any . each-any))))
           #{e\ 1342}#)))
      (#{global-extend\ 129}#
        'core
        'case-lambda
        (lambda (#{e\ 1357}#
                 #{r\ 1358}#
                 #{w\ 1359}#
                 #{s\ 1360}#
                 #{mod\ 1361}#)
          ((lambda (#{tmp\ 1362}#)
             ((lambda (#{tmp\ 1363}#)
                (if #{tmp\ 1363}#
                  (apply (lambda (#{_\ 1364}#
                                  #{args\ 1365}#
                                  #{e1\ 1366}#
                                  #{e2\ 1367}#
                                  #{args*\ 1368}#
                                  #{e1*\ 1369}#
                                  #{e2*\ 1370}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1357}#
                                 #{r\ 1358}#
                                 #{w\ 1359}#
                                 #{s\ 1360}#
                                 #{mod\ 1361}#
                                 #{lambda-formals\ 176}#
                                 (cons (cons #{args\ 1365}#
                                             (cons #{e1\ 1366}# #{e2\ 1367}#))
                                       (map (lambda (#{tmp\ 1374}#
                                                     #{tmp\ 1373}#
                                                     #{tmp\ 1372}#)
                                              (cons #{tmp\ 1372}#
                                                    (cons #{tmp\ 1373}#
                                                          #{tmp\ 1374}#)))
                                            #{e2*\ 1370}#
                                            #{e1*\ 1369}#
                                            #{args*\ 1368}#))))
                             (lambda (#{docstring\ 1376}# #{lcase\ 1377}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1360}#
                                 #{docstring\ 1376}#
                                 #{lcase\ 1377}#))))
                         #{tmp\ 1363}#)
                  ((lambda (#{_\ 1378}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda"
                       #{e\ 1357}#))
                   #{tmp\ 1362}#)))
              ($sc-dispatch
                #{tmp\ 1362}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 1357}#)))
      (#{global-extend\ 129}#
        'core
        'case-lambda*
        (lambda (#{e\ 1379}#
                 #{r\ 1380}#
                 #{w\ 1381}#
                 #{s\ 1382}#
                 #{mod\ 1383}#)
          ((lambda (#{tmp\ 1384}#)
             ((lambda (#{tmp\ 1385}#)
                (if #{tmp\ 1385}#
                  (apply (lambda (#{_\ 1386}#
                                  #{args\ 1387}#
                                  #{e1\ 1388}#
                                  #{e2\ 1389}#
                                  #{args*\ 1390}#
                                  #{e1*\ 1391}#
                                  #{e2*\ 1392}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1379}#
                                 #{r\ 1380}#
                                 #{w\ 1381}#
                                 #{s\ 1382}#
                                 #{mod\ 1383}#
                                 #{lambda*-formals\ 178}#
                                 (cons (cons #{args\ 1387}#
                                             (cons #{e1\ 1388}# #{e2\ 1389}#))
                                       (map (lambda (#{tmp\ 1396}#
                                                     #{tmp\ 1395}#
                                                     #{tmp\ 1394}#)
                                              (cons #{tmp\ 1394}#
                                                    (cons #{tmp\ 1395}#
                                                          #{tmp\ 1396}#)))
                                            #{e2*\ 1392}#
                                            #{e1*\ 1391}#
                                            #{args*\ 1390}#))))
                             (lambda (#{docstring\ 1398}# #{lcase\ 1399}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1382}#
                                 #{docstring\ 1398}#
                                 #{lcase\ 1399}#))))
                         #{tmp\ 1385}#)
                  ((lambda (#{_\ 1400}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda*"
                       #{e\ 1379}#))
                   #{tmp\ 1384}#)))
              ($sc-dispatch
                #{tmp\ 1384}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 1379}#)))
      (#{global-extend\ 129}#
        'core
        'let
        (letrec ((#{chi-let\ 1401}#
                   (lambda (#{e\ 1402}#
                            #{r\ 1403}#
                            #{w\ 1404}#
                            #{s\ 1405}#
                            #{mod\ 1406}#
                            #{constructor\ 1407}#
                            #{ids\ 1408}#
                            #{vals\ 1409}#
                            #{exps\ 1410}#)
                     (if (not (#{valid-bound-ids?\ 156}# #{ids\ 1408}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 1402}#)
                       (let ((#{labels\ 1411}#
                               (#{gen-labels\ 137}# #{ids\ 1408}#))
                             (#{new-vars\ 1412}#
                               (map #{gen-var\ 181}# #{ids\ 1408}#)))
                         (let ((#{nw\ 1413}#
                                 (#{make-binding-wrap\ 148}#
                                   #{ids\ 1408}#
                                   #{labels\ 1411}#
                                   #{w\ 1404}#))
                               (#{nr\ 1414}#
                                 (#{extend-var-env\ 126}#
                                   #{labels\ 1411}#
                                   #{new-vars\ 1412}#
                                   #{r\ 1403}#)))
                           (#{constructor\ 1407}#
                             #{s\ 1405}#
                             (map syntax->datum #{ids\ 1408}#)
                             #{new-vars\ 1412}#
                             (map (lambda (#{x\ 1415}#)
                                    (#{chi\ 167}#
                                      #{x\ 1415}#
                                      #{r\ 1403}#
                                      #{w\ 1404}#
                                      #{mod\ 1406}#))
                                  #{vals\ 1409}#)
                             (#{chi-body\ 171}#
                               #{exps\ 1410}#
                               (#{source-wrap\ 160}#
                                 #{e\ 1402}#
                                 #{nw\ 1413}#
                                 #{s\ 1405}#
                                 #{mod\ 1406}#)
                               #{nr\ 1414}#
                               #{nw\ 1413}#
                               #{mod\ 1406}#))))))))
          (lambda (#{e\ 1416}#
                   #{r\ 1417}#
                   #{w\ 1418}#
                   #{s\ 1419}#
                   #{mod\ 1420}#)
            ((lambda (#{tmp\ 1421}#)
               ((lambda (#{tmp\ 1422}#)
                  (if (if #{tmp\ 1422}#
                        (apply (lambda (#{_\ 1423}#
                                        #{id\ 1424}#
                                        #{val\ 1425}#
                                        #{e1\ 1426}#
                                        #{e2\ 1427}#)
                                 (and-map #{id?\ 131}# #{id\ 1424}#))
                               #{tmp\ 1422}#)
                        #f)
                    (apply (lambda (#{_\ 1429}#
                                    #{id\ 1430}#
                                    #{val\ 1431}#
                                    #{e1\ 1432}#
                                    #{e2\ 1433}#)
                             (#{chi-let\ 1401}#
                               #{e\ 1416}#
                               #{r\ 1417}#
                               #{w\ 1418}#
                               #{s\ 1419}#
                               #{mod\ 1420}#
                               #{build-let\ 111}#
                               #{id\ 1430}#
                               #{val\ 1431}#
                               (cons #{e1\ 1432}# #{e2\ 1433}#)))
                           #{tmp\ 1422}#)
                    ((lambda (#{tmp\ 1437}#)
                       (if (if #{tmp\ 1437}#
                             (apply (lambda (#{_\ 1438}#
                                             #{f\ 1439}#
                                             #{id\ 1440}#
                                             #{val\ 1441}#
                                             #{e1\ 1442}#
                                             #{e2\ 1443}#)
                                      (if (#{id?\ 131}# #{f\ 1439}#)
                                        (and-map #{id?\ 131}# #{id\ 1440}#)
                                        #f))
                                    #{tmp\ 1437}#)
                             #f)
                         (apply (lambda (#{_\ 1445}#
                                         #{f\ 1446}#
                                         #{id\ 1447}#
                                         #{val\ 1448}#
                                         #{e1\ 1449}#
                                         #{e2\ 1450}#)
                                  (#{chi-let\ 1401}#
                                    #{e\ 1416}#
                                    #{r\ 1417}#
                                    #{w\ 1418}#
                                    #{s\ 1419}#
                                    #{mod\ 1420}#
                                    #{build-named-let\ 112}#
                                    (cons #{f\ 1446}# #{id\ 1447}#)
                                    #{val\ 1448}#
                                    (cons #{e1\ 1449}# #{e2\ 1450}#)))
                                #{tmp\ 1437}#)
                         ((lambda (#{_\ 1454}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 160}#
                                #{e\ 1416}#
                                #{w\ 1418}#
                                #{s\ 1419}#
                                #{mod\ 1420}#)))
                          #{tmp\ 1421}#)))
                     ($sc-dispatch
                       #{tmp\ 1421}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1421}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 1416}#))))
      (#{global-extend\ 129}#
        'core
        'letrec
        (lambda (#{e\ 1455}#
                 #{r\ 1456}#
                 #{w\ 1457}#
                 #{s\ 1458}#
                 #{mod\ 1459}#)
          ((lambda (#{tmp\ 1460}#)
             ((lambda (#{tmp\ 1461}#)
                (if (if #{tmp\ 1461}#
                      (apply (lambda (#{_\ 1462}#
                                      #{id\ 1463}#
                                      #{val\ 1464}#
                                      #{e1\ 1465}#
                                      #{e2\ 1466}#)
                               (and-map #{id?\ 131}# #{id\ 1463}#))
                             #{tmp\ 1461}#)
                      #f)
                  (apply (lambda (#{_\ 1468}#
                                  #{id\ 1469}#
                                  #{val\ 1470}#
                                  #{e1\ 1471}#
                                  #{e2\ 1472}#)
                           (let ((#{ids\ 1473}# #{id\ 1469}#))
                             (if (not (#{valid-bound-ids?\ 156}#
                                        #{ids\ 1473}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 1455}#)
                               (let ((#{labels\ 1475}#
                                       (#{gen-labels\ 137}# #{ids\ 1473}#))
                                     (#{new-vars\ 1476}#
                                       (map #{gen-var\ 181}# #{ids\ 1473}#)))
                                 (let ((#{w\ 1477}#
                                         (#{make-binding-wrap\ 148}#
                                           #{ids\ 1473}#
                                           #{labels\ 1475}#
                                           #{w\ 1457}#))
                                       (#{r\ 1478}#
                                         (#{extend-var-env\ 126}#
                                           #{labels\ 1475}#
                                           #{new-vars\ 1476}#
                                           #{r\ 1456}#)))
                                   (#{build-letrec\ 113}#
                                     #{s\ 1458}#
                                     (map syntax->datum #{ids\ 1473}#)
                                     #{new-vars\ 1476}#
                                     (map (lambda (#{x\ 1479}#)
                                            (#{chi\ 167}#
                                              #{x\ 1479}#
                                              #{r\ 1478}#
                                              #{w\ 1477}#
                                              #{mod\ 1459}#))
                                          #{val\ 1470}#)
                                     (#{chi-body\ 171}#
                                       (cons #{e1\ 1471}# #{e2\ 1472}#)
                                       (#{source-wrap\ 160}#
                                         #{e\ 1455}#
                                         #{w\ 1477}#
                                         #{s\ 1458}#
                                         #{mod\ 1459}#)
                                       #{r\ 1478}#
                                       #{w\ 1477}#
                                       #{mod\ 1459}#)))))))
                         #{tmp\ 1461}#)
                  ((lambda (#{_\ 1482}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 160}#
                         #{e\ 1455}#
                         #{w\ 1457}#
                         #{s\ 1458}#
                         #{mod\ 1459}#)))
                   #{tmp\ 1460}#)))
              ($sc-dispatch
                #{tmp\ 1460}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1455}#)))
      (#{global-extend\ 129}#
        'core
        'set!
        (lambda (#{e\ 1483}#
                 #{r\ 1484}#
                 #{w\ 1485}#
                 #{s\ 1486}#
                 #{mod\ 1487}#)
          ((lambda (#{tmp\ 1488}#)
             ((lambda (#{tmp\ 1489}#)
                (if (if #{tmp\ 1489}#
                      (apply (lambda (#{_\ 1490}# #{id\ 1491}# #{val\ 1492}#)
                               (#{id?\ 131}# #{id\ 1491}#))
                             #{tmp\ 1489}#)
                      #f)
                  (apply (lambda (#{_\ 1493}# #{id\ 1494}# #{val\ 1495}#)
                           (let ((#{val\ 1496}#
                                   (#{chi\ 167}#
                                     #{val\ 1495}#
                                     #{r\ 1484}#
                                     #{w\ 1485}#
                                     #{mod\ 1487}#))
                                 (#{n\ 1497}#
                                   (#{id-var-name\ 153}#
                                     #{id\ 1494}#
                                     #{w\ 1485}#)))
                             (let ((#{b\ 1498}#
                                     (#{lookup\ 128}#
                                       #{n\ 1497}#
                                       #{r\ 1484}#
                                       #{mod\ 1487}#)))
                               (let ((#{atom-key\ 1499}#
                                       (#{binding-type\ 123}# #{b\ 1498}#)))
                                 (if (memv #{atom-key\ 1499}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 99}#
                                     #{s\ 1486}#
                                     (syntax->datum #{id\ 1494}#)
                                     (#{binding-value\ 124}# #{b\ 1498}#)
                                     #{val\ 1496}#)
                                   (if (memv #{atom-key\ 1499}#
                                             '(global))
                                     (#{build-global-assignment\ 102}#
                                       #{s\ 1486}#
                                       #{n\ 1497}#
                                       #{val\ 1496}#
                                       #{mod\ 1487}#)
                                     (if (memv #{atom-key\ 1499}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 159}#
                                           #{id\ 1494}#
                                           #{w\ 1485}#
                                           #{mod\ 1487}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 160}#
                                           #{e\ 1483}#
                                           #{w\ 1485}#
                                           #{s\ 1486}#
                                           #{mod\ 1487}#)))))))))
                         #{tmp\ 1489}#)
                  ((lambda (#{tmp\ 1500}#)
                     (if #{tmp\ 1500}#
                       (apply (lambda (#{_\ 1501}#
                                       #{head\ 1502}#
                                       #{tail\ 1503}#
                                       #{val\ 1504}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 165}#
                                      #{head\ 1502}#
                                      #{r\ 1484}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 1487}#
                                      #t))
                                  (lambda (#{type\ 1505}#
                                           #{value\ 1506}#
                                           #{ee\ 1507}#
                                           #{ww\ 1508}#
                                           #{ss\ 1509}#
                                           #{modmod\ 1510}#)
                                    (if (memv #{type\ 1505}#
                                              '(module-ref))
                                      (let ((#{val\ 1511}#
                                              (#{chi\ 167}#
                                                #{val\ 1504}#
                                                #{r\ 1484}#
                                                #{w\ 1485}#
                                                #{mod\ 1487}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 1506}#
                                              (cons #{head\ 1502}#
                                                    #{tail\ 1503}#)))
                                          (lambda (#{id\ 1513}# #{mod\ 1514}#)
                                            (#{build-global-assignment\ 102}#
                                              #{s\ 1486}#
                                              #{id\ 1513}#
                                              #{val\ 1511}#
                                              #{mod\ 1514}#))))
                                      (#{build-application\ 96}#
                                        #{s\ 1486}#
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
                                                #{head\ 1502}#)
                                          #{r\ 1484}#
                                          #{w\ 1485}#
                                          #{mod\ 1487}#)
                                        (map (lambda (#{e\ 1515}#)
                                               (#{chi\ 167}#
                                                 #{e\ 1515}#
                                                 #{r\ 1484}#
                                                 #{w\ 1485}#
                                                 #{mod\ 1487}#))
                                             (append
                                               #{tail\ 1503}#
                                               (list #{val\ 1504}#))))))))
                              #{tmp\ 1500}#)
                       ((lambda (#{_\ 1517}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 160}#
                              #{e\ 1483}#
                              #{w\ 1485}#
                              #{s\ 1486}#
                              #{mod\ 1487}#)))
                        #{tmp\ 1488}#)))
                   ($sc-dispatch
                     #{tmp\ 1488}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 1488}#
                '(any any any))))
           #{e\ 1483}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@
        (lambda (#{e\ 1518}#)
          ((lambda (#{tmp\ 1519}#)
             ((lambda (#{tmp\ 1520}#)
                (if (if #{tmp\ 1520}#
                      (apply (lambda (#{_\ 1521}# #{mod\ 1522}# #{id\ 1523}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1522}#)
                                 (#{id?\ 131}# #{id\ 1523}#)
                                 #f))
                             #{tmp\ 1520}#)
                      #f)
                  (apply (lambda (#{_\ 1525}# #{mod\ 1526}# #{id\ 1527}#)
                           (values
                             (syntax->datum #{id\ 1527}#)
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
                                     #{mod\ 1526}#))))
                         #{tmp\ 1520}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1519}#)))
              ($sc-dispatch
                #{tmp\ 1519}#
                '(any each-any any))))
           #{e\ 1518}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@@
        (lambda (#{e\ 1529}#)
          ((lambda (#{tmp\ 1530}#)
             ((lambda (#{tmp\ 1531}#)
                (if (if #{tmp\ 1531}#
                      (apply (lambda (#{_\ 1532}# #{mod\ 1533}# #{id\ 1534}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1533}#)
                                 (#{id?\ 131}# #{id\ 1534}#)
                                 #f))
                             #{tmp\ 1531}#)
                      #f)
                  (apply (lambda (#{_\ 1536}# #{mod\ 1537}# #{id\ 1538}#)
                           (values
                             (syntax->datum #{id\ 1538}#)
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
                                     #{mod\ 1537}#))))
                         #{tmp\ 1531}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1530}#)))
              ($sc-dispatch
                #{tmp\ 1530}#
                '(any each-any any))))
           #{e\ 1529}#)))
      (#{global-extend\ 129}#
        'core
        'if
        (lambda (#{e\ 1540}#
                 #{r\ 1541}#
                 #{w\ 1542}#
                 #{s\ 1543}#
                 #{mod\ 1544}#)
          ((lambda (#{tmp\ 1545}#)
             ((lambda (#{tmp\ 1546}#)
                (if #{tmp\ 1546}#
                  (apply (lambda (#{_\ 1547}# #{test\ 1548}# #{then\ 1549}#)
                           (#{build-conditional\ 97}#
                             #{s\ 1543}#
                             (#{chi\ 167}#
                               #{test\ 1548}#
                               #{r\ 1541}#
                               #{w\ 1542}#
                               #{mod\ 1544}#)
                             (#{chi\ 167}#
                               #{then\ 1549}#
                               #{r\ 1541}#
                               #{w\ 1542}#
                               #{mod\ 1544}#)
                             (#{build-void\ 95}# #f)))
                         #{tmp\ 1546}#)
                  ((lambda (#{tmp\ 1550}#)
                     (if #{tmp\ 1550}#
                       (apply (lambda (#{_\ 1551}#
                                       #{test\ 1552}#
                                       #{then\ 1553}#
                                       #{else\ 1554}#)
                                (#{build-conditional\ 97}#
                                  #{s\ 1543}#
                                  (#{chi\ 167}#
                                    #{test\ 1552}#
                                    #{r\ 1541}#
                                    #{w\ 1542}#
                                    #{mod\ 1544}#)
                                  (#{chi\ 167}#
                                    #{then\ 1553}#
                                    #{r\ 1541}#
                                    #{w\ 1542}#
                                    #{mod\ 1544}#)
                                  (#{chi\ 167}#
                                    #{else\ 1554}#
                                    #{r\ 1541}#
                                    #{w\ 1542}#
                                    #{mod\ 1544}#)))
                              #{tmp\ 1550}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1545}#)))
                   ($sc-dispatch
                     #{tmp\ 1545}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 1545}#
                '(any any any))))
           #{e\ 1540}#)))
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
        (letrec ((#{gen-syntax-case\ 1558}#
                   (lambda (#{x\ 1559}#
                            #{keys\ 1560}#
                            #{clauses\ 1561}#
                            #{r\ 1562}#
                            #{mod\ 1563}#)
                     (if (null? #{clauses\ 1561}#)
                       (#{build-application\ 96}#
                         #f
                         (#{build-primref\ 108}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 109}# #f #f)
                               (#{build-data\ 109}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 1559}#))
                       ((lambda (#{tmp\ 1564}#)
                          ((lambda (#{tmp\ 1565}#)
                             (if #{tmp\ 1565}#
                               (apply (lambda (#{pat\ 1566}# #{exp\ 1567}#)
                                        (if (if (#{id?\ 131}# #{pat\ 1566}#)
                                              (and-map
                                                (lambda (#{x\ 1568}#)
                                                  (not (#{free-id=?\ 154}#
                                                         #{pat\ 1566}#
                                                         #{x\ 1568}#)))
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
                                                      #{keys\ 1560}#))
                                              #f)
                                          (let ((#{labels\ 1569}#
                                                  (list (#{gen-label\ 136}#)))
                                                (#{var\ 1570}#
                                                  (#{gen-var\ 181}#
                                                    #{pat\ 1566}#)))
                                            (#{build-application\ 96}#
                                              #f
                                              (#{build-simple-lambda\ 105}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 1566}#))
                                                #f
                                                (list #{var\ 1570}#)
                                                #f
                                                (#{chi\ 167}#
                                                  #{exp\ 1567}#
                                                  (#{extend-env\ 125}#
                                                    #{labels\ 1569}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 1570}#
                                                                      0)))
                                                    #{r\ 1562}#)
                                                  (#{make-binding-wrap\ 148}#
                                                    (list #{pat\ 1566}#)
                                                    #{labels\ 1569}#
                                                    '(()))
                                                  #{mod\ 1563}#))
                                              (list #{x\ 1559}#)))
                                          (#{gen-clause\ 1557}#
                                            #{x\ 1559}#
                                            #{keys\ 1560}#
                                            (cdr #{clauses\ 1561}#)
                                            #{r\ 1562}#
                                            #{pat\ 1566}#
                                            #t
                                            #{exp\ 1567}#
                                            #{mod\ 1563}#)))
                                      #{tmp\ 1565}#)
                               ((lambda (#{tmp\ 1571}#)
                                  (if #{tmp\ 1571}#
                                    (apply (lambda (#{pat\ 1572}#
                                                    #{fender\ 1573}#
                                                    #{exp\ 1574}#)
                                             (#{gen-clause\ 1557}#
                                               #{x\ 1559}#
                                               #{keys\ 1560}#
                                               (cdr #{clauses\ 1561}#)
                                               #{r\ 1562}#
                                               #{pat\ 1572}#
                                               #{fender\ 1573}#
                                               #{exp\ 1574}#
                                               #{mod\ 1563}#))
                                           #{tmp\ 1571}#)
                                    ((lambda (#{_\ 1575}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 1561}#)))
                                     #{tmp\ 1564}#)))
                                ($sc-dispatch
                                  #{tmp\ 1564}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 1564}# (quote (any any)))))
                        (car #{clauses\ 1561}#)))))
                 (#{gen-clause\ 1557}#
                   (lambda (#{x\ 1576}#
                            #{keys\ 1577}#
                            #{clauses\ 1578}#
                            #{r\ 1579}#
                            #{pat\ 1580}#
                            #{fender\ 1581}#
                            #{exp\ 1582}#
                            #{mod\ 1583}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 1555}#
                           #{pat\ 1580}#
                           #{keys\ 1577}#))
                       (lambda (#{p\ 1584}# #{pvars\ 1585}#)
                         (if (not (#{distinct-bound-ids?\ 157}#
                                    (map car #{pvars\ 1585}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 1580}#)
                           (if (not (and-map
                                      (lambda (#{x\ 1586}#)
                                        (not (#{ellipsis?\ 175}#
                                               (car #{x\ 1586}#))))
                                      #{pvars\ 1585}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 1580}#)
                             (let ((#{y\ 1587}#
                                     (#{gen-var\ 181}# (quote tmp))))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 1587}#)
                                   #f
                                   (let ((#{y\ 1588}#
                                           (#{build-lexical-reference\ 98}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 1587}#)))
                                     (#{build-conditional\ 97}#
                                       #f
                                       ((lambda (#{tmp\ 1589}#)
                                          ((lambda (#{tmp\ 1590}#)
                                             (if #{tmp\ 1590}#
                                               (apply (lambda () #{y\ 1588}#)
                                                      #{tmp\ 1590}#)
                                               ((lambda (#{_\ 1591}#)
                                                  (#{build-conditional\ 97}#
                                                    #f
                                                    #{y\ 1588}#
                                                    (#{build-dispatch-call\ 1556}#
                                                      #{pvars\ 1585}#
                                                      #{fender\ 1581}#
                                                      #{y\ 1588}#
                                                      #{r\ 1579}#
                                                      #{mod\ 1583}#)
                                                    (#{build-data\ 109}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 1589}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1589}#
                                             '#(atom #t))))
                                        #{fender\ 1581}#)
                                       (#{build-dispatch-call\ 1556}#
                                         #{pvars\ 1585}#
                                         #{exp\ 1582}#
                                         #{y\ 1588}#
                                         #{r\ 1579}#
                                         #{mod\ 1583}#)
                                       (#{gen-syntax-case\ 1558}#
                                         #{x\ 1576}#
                                         #{keys\ 1577}#
                                         #{clauses\ 1578}#
                                         #{r\ 1579}#
                                         #{mod\ 1583}#))))
                                 (list (if (eq? #{p\ 1584}# (quote any))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             'list)
                                           (list #{x\ 1576}#))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 1576}#
                                                 (#{build-data\ 109}#
                                                   #f
                                                   #{p\ 1584}#)))))))))))))
                 (#{build-dispatch-call\ 1556}#
                   (lambda (#{pvars\ 1592}#
                            #{exp\ 1593}#
                            #{y\ 1594}#
                            #{r\ 1595}#
                            #{mod\ 1596}#)
                     (let ((#{ids\ 1597}# (map car #{pvars\ 1592}#))
                           (#{levels\ 1598}# (map cdr #{pvars\ 1592}#)))
                       (let ((#{labels\ 1599}#
                               (#{gen-labels\ 137}# #{ids\ 1597}#))
                             (#{new-vars\ 1600}#
                               (map #{gen-var\ 181}# #{ids\ 1597}#)))
                         (#{build-application\ 96}#
                           #f
                           (#{build-primref\ 108}# #f (quote apply))
                           (list (#{build-simple-lambda\ 105}#
                                   #f
                                   (map syntax->datum #{ids\ 1597}#)
                                   #f
                                   #{new-vars\ 1600}#
                                   #f
                                   (#{chi\ 167}#
                                     #{exp\ 1593}#
                                     (#{extend-env\ 125}#
                                       #{labels\ 1599}#
                                       (map (lambda (#{var\ 1601}#
                                                     #{level\ 1602}#)
                                              (cons 'syntax
                                                    (cons #{var\ 1601}#
                                                          #{level\ 1602}#)))
                                            #{new-vars\ 1600}#
                                            (map cdr #{pvars\ 1592}#))
                                       #{r\ 1595}#)
                                     (#{make-binding-wrap\ 148}#
                                       #{ids\ 1597}#
                                       #{labels\ 1599}#
                                       '(()))
                                     #{mod\ 1596}#))
                                 #{y\ 1594}#))))))
                 (#{convert-pattern\ 1555}#
                   (lambda (#{pattern\ 1603}# #{keys\ 1604}#)
                     (letrec ((#{cvt\ 1605}#
                                (lambda (#{p\ 1606}# #{n\ 1607}# #{ids\ 1608}#)
                                  (if (#{id?\ 131}# #{p\ 1606}#)
                                    (if (#{bound-id-member?\ 158}#
                                          #{p\ 1606}#
                                          #{keys\ 1604}#)
                                      (values
                                        (vector (quote free-id) #{p\ 1606}#)
                                        #{ids\ 1608}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 1606}# #{n\ 1607}#)
                                              #{ids\ 1608}#)))
                                    ((lambda (#{tmp\ 1609}#)
                                       ((lambda (#{tmp\ 1610}#)
                                          (if (if #{tmp\ 1610}#
                                                (apply (lambda (#{x\ 1611}#
                                                                #{dots\ 1612}#)
                                                         (#{ellipsis?\ 175}#
                                                           #{dots\ 1612}#))
                                                       #{tmp\ 1610}#)
                                                #f)
                                            (apply (lambda (#{x\ 1613}#
                                                            #{dots\ 1614}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 1605}#
                                                           #{x\ 1613}#
                                                           (#{fx+\ 86}#
                                                             #{n\ 1607}#
                                                             1)
                                                           #{ids\ 1608}#))
                                                       (lambda (#{p\ 1615}#
                                                                #{ids\ 1616}#)
                                                         (values
                                                           (if (eq? #{p\ 1615}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 1615}#))
                                                           #{ids\ 1616}#))))
                                                   #{tmp\ 1610}#)
                                            ((lambda (#{tmp\ 1617}#)
                                               (if #{tmp\ 1617}#
                                                 (apply (lambda (#{x\ 1618}#
                                                                 #{y\ 1619}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 1605}#
                                                                #{y\ 1619}#
                                                                #{n\ 1607}#
                                                                #{ids\ 1608}#))
                                                            (lambda (#{y\ 1620}#
                                                                     #{ids\ 1621}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 1605}#
                                                                    #{x\ 1618}#
                                                                    #{n\ 1607}#
                                                                    #{ids\ 1621}#))
                                                                (lambda (#{x\ 1622}#
                                                                         #{ids\ 1623}#)
                                                                  (values
                                                                    (cons #{x\ 1622}#
                                                                          #{y\ 1620}#)
                                                                    #{ids\ 1623}#))))))
                                                        #{tmp\ 1617}#)
                                                 ((lambda (#{tmp\ 1624}#)
                                                    (if #{tmp\ 1624}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 1608}#))
                                                             #{tmp\ 1624}#)
                                                      ((lambda (#{tmp\ 1625}#)
                                                         (if #{tmp\ 1625}#
                                                           (apply (lambda (#{x\ 1626}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 1605}#
                                                                          #{x\ 1626}#
                                                                          #{n\ 1607}#
                                                                          #{ids\ 1608}#))
                                                                      (lambda (#{p\ 1628}#
                                                                               #{ids\ 1629}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 1628}#)
                                                                          #{ids\ 1629}#))))
                                                                  #{tmp\ 1625}#)
                                                           ((lambda (#{x\ 1630}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 180}#
                                                                    #{p\ 1606}#
                                                                    '(())))
                                                                #{ids\ 1608}#))
                                                            #{tmp\ 1609}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1609}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 1609}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 1609}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 1609}#
                                          '(any any))))
                                     #{p\ 1606}#)))))
                       (#{cvt\ 1605}# #{pattern\ 1603}# 0 (quote ()))))))
          (lambda (#{e\ 1631}#
                   #{r\ 1632}#
                   #{w\ 1633}#
                   #{s\ 1634}#
                   #{mod\ 1635}#)
            (let ((#{e\ 1636}#
                    (#{source-wrap\ 160}#
                      #{e\ 1631}#
                      #{w\ 1633}#
                      #{s\ 1634}#
                      #{mod\ 1635}#)))
              ((lambda (#{tmp\ 1637}#)
                 ((lambda (#{tmp\ 1638}#)
                    (if #{tmp\ 1638}#
                      (apply (lambda (#{_\ 1639}#
                                      #{val\ 1640}#
                                      #{key\ 1641}#
                                      #{m\ 1642}#)
                               (if (and-map
                                     (lambda (#{x\ 1643}#)
                                       (if (#{id?\ 131}# #{x\ 1643}#)
                                         (not (#{ellipsis?\ 175}# #{x\ 1643}#))
                                         #f))
                                     #{key\ 1641}#)
                                 (let ((#{x\ 1645}#
                                         (#{gen-var\ 181}# (quote tmp))))
                                   (#{build-application\ 96}#
                                     #{s\ 1634}#
                                     (#{build-simple-lambda\ 105}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 1645}#)
                                       #f
                                       (#{gen-syntax-case\ 1558}#
                                         (#{build-lexical-reference\ 98}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 1645}#)
                                         #{key\ 1641}#
                                         #{m\ 1642}#
                                         #{r\ 1632}#
                                         #{mod\ 1635}#))
                                     (list (#{chi\ 167}#
                                             #{val\ 1640}#
                                             #{r\ 1632}#
                                             '(())
                                             #{mod\ 1635}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 1636}#)))
                             #{tmp\ 1638}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 1637}#)))
                  ($sc-dispatch
                    #{tmp\ 1637}#
                    '(any any each-any . each-any))))
               #{e\ 1636}#)))))
      (set! sc-expand
        (lambda (#{x\ 1648}# . #{rest\ 1649}#)
          (if (if (pair? #{x\ 1648}#)
                (equal? (car #{x\ 1648}#) #{noexpand\ 84}#)
                #f)
            (cadr #{x\ 1648}#)
            (let ((#{m\ 1650}#
                    (if (null? #{rest\ 1649}#)
                      'e
                      (car #{rest\ 1649}#)))
                  (#{esew\ 1651}#
                    (if (let ((#{t\ 1652}# (null? #{rest\ 1649}#)))
                          (if #{t\ 1652}#
                            #{t\ 1652}#
                            (null? (cdr #{rest\ 1649}#))))
                      '(eval)
                      (cadr #{rest\ 1649}#))))
              (with-fluid*
                #{*mode*\ 85}#
                #{m\ 1650}#
                (lambda ()
                  (#{chi-top\ 166}#
                    #{x\ 1648}#
                    '()
                    '((top))
                    #{m\ 1650}#
                    #{esew\ 1651}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 1653}#)
          (#{nonsymbol-id?\ 130}# #{x\ 1653}#)))
      (set! datum->syntax
        (lambda (#{id\ 1654}# #{datum\ 1655}#)
          (#{make-syntax-object\ 114}#
            #{datum\ 1655}#
            (#{syntax-object-wrap\ 117}# #{id\ 1654}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 1656}#)
          (#{strip\ 180}# #{x\ 1656}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 1657}#)
          (begin
            (let ((#{x\ 1658}# #{ls\ 1657}#))
              (if (not (list? #{x\ 1658}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 1658}#)))
            (map (lambda (#{x\ 1659}#)
                   (#{wrap\ 159}# (gensym) (quote ((top))) #f))
                 #{ls\ 1657}#))))
      (set! free-identifier=?
        (lambda (#{x\ 1660}# #{y\ 1661}#)
          (begin
            (let ((#{x\ 1662}# #{x\ 1660}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1662}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1662}#)))
            (let ((#{x\ 1663}# #{y\ 1661}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1663}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1663}#)))
            (#{free-id=?\ 154}# #{x\ 1660}# #{y\ 1661}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 1664}# #{y\ 1665}#)
          (begin
            (let ((#{x\ 1666}# #{x\ 1664}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1666}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1666}#)))
            (let ((#{x\ 1667}# #{y\ 1665}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1667}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1667}#)))
            (#{bound-id=?\ 155}# #{x\ 1664}# #{y\ 1665}#))))
      (set! syntax-violation
        (lambda (#{who\ 1668}#
                 #{message\ 1669}#
                 #{form\ 1670}#
                 .
                 #{subform\ 1671}#)
          (begin
            (let ((#{x\ 1672}# #{who\ 1668}#))
              (if (not ((lambda (#{x\ 1673}#)
                          (let ((#{t\ 1674}# (not #{x\ 1673}#)))
                            (if #{t\ 1674}#
                              #{t\ 1674}#
                              (let ((#{t\ 1675}# (string? #{x\ 1673}#)))
                                (if #{t\ 1675}#
                                  #{t\ 1675}#
                                  (symbol? #{x\ 1673}#))))))
                        #{x\ 1672}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1672}#)))
            (let ((#{x\ 1676}# #{message\ 1669}#))
              (if (not (string? #{x\ 1676}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1676}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 1668}# "~a: " "")
                "~a "
                (if (null? #{subform\ 1671}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 1677}#
                      (cons #{message\ 1669}#
                            (map (lambda (#{x\ 1678}#)
                                   (#{strip\ 180}# #{x\ 1678}# (quote (()))))
                                 (append
                                   #{subform\ 1671}#
                                   (list #{form\ 1670}#))))))
                (if #{who\ 1668}#
                  (cons #{who\ 1668}# #{tail\ 1677}#)
                  #{tail\ 1677}#))
              #f))))
      (letrec ((#{match\ 1683}#
                 (lambda (#{e\ 1684}#
                          #{p\ 1685}#
                          #{w\ 1686}#
                          #{r\ 1687}#
                          #{mod\ 1688}#)
                   (if (not #{r\ 1687}#)
                     #f
                     (if (eq? #{p\ 1685}# (quote any))
                       (cons (#{wrap\ 159}#
                               #{e\ 1684}#
                               #{w\ 1686}#
                               #{mod\ 1688}#)
                             #{r\ 1687}#)
                       (if (#{syntax-object?\ 115}# #{e\ 1684}#)
                         (#{match*\ 1682}#
                           (#{syntax-object-expression\ 116}# #{e\ 1684}#)
                           #{p\ 1685}#
                           (#{join-wraps\ 150}#
                             #{w\ 1686}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1684}#))
                           #{r\ 1687}#
                           (#{syntax-object-module\ 118}# #{e\ 1684}#))
                         (#{match*\ 1682}#
                           #{e\ 1684}#
                           #{p\ 1685}#
                           #{w\ 1686}#
                           #{r\ 1687}#
                           #{mod\ 1688}#))))))
               (#{match*\ 1682}#
                 (lambda (#{e\ 1689}#
                          #{p\ 1690}#
                          #{w\ 1691}#
                          #{r\ 1692}#
                          #{mod\ 1693}#)
                   (if (null? #{p\ 1690}#)
                     (if (null? #{e\ 1689}#) #{r\ 1692}# #f)
                     (if (pair? #{p\ 1690}#)
                       (if (pair? #{e\ 1689}#)
                         (#{match\ 1683}#
                           (car #{e\ 1689}#)
                           (car #{p\ 1690}#)
                           #{w\ 1691}#
                           (#{match\ 1683}#
                             (cdr #{e\ 1689}#)
                             (cdr #{p\ 1690}#)
                             #{w\ 1691}#
                             #{r\ 1692}#
                             #{mod\ 1693}#)
                           #{mod\ 1693}#)
                         #f)
                       (if (eq? #{p\ 1690}# (quote each-any))
                         (let ((#{l\ 1694}#
                                 (#{match-each-any\ 1680}#
                                   #{e\ 1689}#
                                   #{w\ 1691}#
                                   #{mod\ 1693}#)))
                           (if #{l\ 1694}#
                             (cons #{l\ 1694}# #{r\ 1692}#)
                             #f))
                         (let ((#{atom-key\ 1695}# (vector-ref #{p\ 1690}# 0)))
                           (if (memv #{atom-key\ 1695}# (quote (each)))
                             (if (null? #{e\ 1689}#)
                               (#{match-empty\ 1681}#
                                 (vector-ref #{p\ 1690}# 1)
                                 #{r\ 1692}#)
                               (let ((#{l\ 1696}#
                                       (#{match-each\ 1679}#
                                         #{e\ 1689}#
                                         (vector-ref #{p\ 1690}# 1)
                                         #{w\ 1691}#
                                         #{mod\ 1693}#)))
                                 (if #{l\ 1696}#
                                   (letrec ((#{collect\ 1697}#
                                              (lambda (#{l\ 1698}#)
                                                (if (null? (car #{l\ 1698}#))
                                                  #{r\ 1692}#
                                                  (cons (map car #{l\ 1698}#)
                                                        (#{collect\ 1697}#
                                                          (map cdr
                                                               #{l\ 1698}#)))))))
                                     (#{collect\ 1697}# #{l\ 1696}#))
                                   #f)))
                             (if (memv #{atom-key\ 1695}# (quote (free-id)))
                               (if (#{id?\ 131}# #{e\ 1689}#)
                                 (if (#{free-id=?\ 154}#
                                       (#{wrap\ 159}#
                                         #{e\ 1689}#
                                         #{w\ 1691}#
                                         #{mod\ 1693}#)
                                       (vector-ref #{p\ 1690}# 1))
                                   #{r\ 1692}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 1695}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 1690}# 1)
                                       (#{strip\ 180}#
                                         #{e\ 1689}#
                                         #{w\ 1691}#))
                                   #{r\ 1692}#
                                   #f)
                                 (if (memv #{atom-key\ 1695}# (quote (vector)))
                                   (if (vector? #{e\ 1689}#)
                                     (#{match\ 1683}#
                                       (vector->list #{e\ 1689}#)
                                       (vector-ref #{p\ 1690}# 1)
                                       #{w\ 1691}#
                                       #{r\ 1692}#
                                       #{mod\ 1693}#)
                                     #f)))))))))))
               (#{match-empty\ 1681}#
                 (lambda (#{p\ 1699}# #{r\ 1700}#)
                   (if (null? #{p\ 1699}#)
                     #{r\ 1700}#
                     (if (eq? #{p\ 1699}# (quote any))
                       (cons (quote ()) #{r\ 1700}#)
                       (if (pair? #{p\ 1699}#)
                         (#{match-empty\ 1681}#
                           (car #{p\ 1699}#)
                           (#{match-empty\ 1681}#
                             (cdr #{p\ 1699}#)
                             #{r\ 1700}#))
                         (if (eq? #{p\ 1699}# (quote each-any))
                           (cons (quote ()) #{r\ 1700}#)
                           (let ((#{atom-key\ 1701}#
                                   (vector-ref #{p\ 1699}# 0)))
                             (if (memv #{atom-key\ 1701}# (quote (each)))
                               (#{match-empty\ 1681}#
                                 (vector-ref #{p\ 1699}# 1)
                                 #{r\ 1700}#)
                               (if (memv #{atom-key\ 1701}#
                                         '(free-id atom))
                                 #{r\ 1700}#
                                 (if (memv #{atom-key\ 1701}# (quote (vector)))
                                   (#{match-empty\ 1681}#
                                     (vector-ref #{p\ 1699}# 1)
                                     #{r\ 1700}#)))))))))))
               (#{match-each-any\ 1680}#
                 (lambda (#{e\ 1702}# #{w\ 1703}# #{mod\ 1704}#)
                   (if (pair? #{e\ 1702}#)
                     (let ((#{l\ 1705}#
                             (#{match-each-any\ 1680}#
                               (cdr #{e\ 1702}#)
                               #{w\ 1703}#
                               #{mod\ 1704}#)))
                       (if #{l\ 1705}#
                         (cons (#{wrap\ 159}#
                                 (car #{e\ 1702}#)
                                 #{w\ 1703}#
                                 #{mod\ 1704}#)
                               #{l\ 1705}#)
                         #f))
                     (if (null? #{e\ 1702}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1702}#)
                         (#{match-each-any\ 1680}#
                           (#{syntax-object-expression\ 116}# #{e\ 1702}#)
                           (#{join-wraps\ 150}#
                             #{w\ 1703}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1702}#))
                           #{mod\ 1704}#)
                         #f)))))
               (#{match-each\ 1679}#
                 (lambda (#{e\ 1706}#
                          #{p\ 1707}#
                          #{w\ 1708}#
                          #{mod\ 1709}#)
                   (if (pair? #{e\ 1706}#)
                     (let ((#{first\ 1710}#
                             (#{match\ 1683}#
                               (car #{e\ 1706}#)
                               #{p\ 1707}#
                               #{w\ 1708}#
                               '()
                               #{mod\ 1709}#)))
                       (if #{first\ 1710}#
                         (let ((#{rest\ 1711}#
                                 (#{match-each\ 1679}#
                                   (cdr #{e\ 1706}#)
                                   #{p\ 1707}#
                                   #{w\ 1708}#
                                   #{mod\ 1709}#)))
                           (if #{rest\ 1711}#
                             (cons #{first\ 1710}# #{rest\ 1711}#)
                             #f))
                         #f))
                     (if (null? #{e\ 1706}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1706}#)
                         (#{match-each\ 1679}#
                           (#{syntax-object-expression\ 116}# #{e\ 1706}#)
                           #{p\ 1707}#
                           (#{join-wraps\ 150}#
                             #{w\ 1708}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1706}#))
                           (#{syntax-object-module\ 118}# #{e\ 1706}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 1712}# #{p\ 1713}#)
            (if (eq? #{p\ 1713}# (quote any))
              (list #{e\ 1712}#)
              (if (#{syntax-object?\ 115}# #{e\ 1712}#)
                (#{match*\ 1682}#
                  (#{syntax-object-expression\ 116}# #{e\ 1712}#)
                  #{p\ 1713}#
                  (#{syntax-object-wrap\ 117}# #{e\ 1712}#)
                  '()
                  (#{syntax-object-module\ 118}# #{e\ 1712}#))
                (#{match*\ 1682}#
                  #{e\ 1712}#
                  #{p\ 1713}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1714}#)
      ((lambda (#{tmp\ 1715}#)
         ((lambda (#{tmp\ 1716}#)
            (if #{tmp\ 1716}#
              (apply (lambda (#{_\ 1717}# #{e1\ 1718}# #{e2\ 1719}#)
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
                             (cons #{e1\ 1718}# #{e2\ 1719}#)))
                     #{tmp\ 1716}#)
              ((lambda (#{tmp\ 1721}#)
                 (if #{tmp\ 1721}#
                   (apply (lambda (#{_\ 1722}#
                                   #{out\ 1723}#
                                   #{in\ 1724}#
                                   #{e1\ 1725}#
                                   #{e2\ 1726}#)
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
                                  #{in\ 1724}#
                                  '()
                                  (list #{out\ 1723}#
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
                                              (cons #{e1\ 1725}#
                                                    #{e2\ 1726}#)))))
                          #{tmp\ 1721}#)
                   ((lambda (#{tmp\ 1728}#)
                      (if #{tmp\ 1728}#
                        (apply (lambda (#{_\ 1729}#
                                        #{out\ 1730}#
                                        #{in\ 1731}#
                                        #{e1\ 1732}#
                                        #{e2\ 1733}#)
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
                                             #{in\ 1731}#)
                                       '()
                                       (list #{out\ 1730}#
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
                                                   (cons #{e1\ 1732}#
                                                         #{e2\ 1733}#)))))
                               #{tmp\ 1728}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 1715}#)))
                    ($sc-dispatch
                      #{tmp\ 1715}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 1715}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 1715}#
            '(any () any . each-any))))
       #{x\ 1714}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1737}#)
      ((lambda (#{tmp\ 1738}#)
         ((lambda (#{tmp\ 1739}#)
            (if #{tmp\ 1739}#
              (apply (lambda (#{_\ 1740}#
                              #{k\ 1741}#
                              #{keyword\ 1742}#
                              #{pattern\ 1743}#
                              #{template\ 1744}#)
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
                                         (cons #{k\ 1741}#
                                               (map (lambda (#{tmp\ 1747}#
                                                             #{tmp\ 1746}#)
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
                                                                  #{tmp\ 1746}#)
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
                                                                  #{tmp\ 1747}#)))
                                                    #{template\ 1744}#
                                                    #{pattern\ 1743}#))))))
                     #{tmp\ 1739}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1738}#)))
          ($sc-dispatch
            #{tmp\ 1738}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 1737}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 1748}#)
      ((lambda (#{tmp\ 1749}#)
         ((lambda (#{tmp\ 1750}#)
            (if (if #{tmp\ 1750}#
                  (apply (lambda (#{let*\ 1751}#
                                  #{x\ 1752}#
                                  #{v\ 1753}#
                                  #{e1\ 1754}#
                                  #{e2\ 1755}#)
                           (and-map identifier? #{x\ 1752}#))
                         #{tmp\ 1750}#)
                  #f)
              (apply (lambda (#{let*\ 1757}#
                              #{x\ 1758}#
                              #{v\ 1759}#
                              #{e1\ 1760}#
                              #{e2\ 1761}#)
                       (letrec ((#{f\ 1762}#
                                  (lambda (#{bindings\ 1763}#)
                                    (if (null? #{bindings\ 1763}#)
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
                                                  (cons #{e1\ 1760}#
                                                        #{e2\ 1761}#)))
                                      ((lambda (#{tmp\ 1767}#)
                                         ((lambda (#{tmp\ 1768}#)
                                            (if #{tmp\ 1768}#
                                              (apply (lambda (#{body\ 1769}#
                                                              #{binding\ 1770}#)
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
                                                             (list #{binding\ 1770}#)
                                                             #{body\ 1769}#))
                                                     #{tmp\ 1768}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 1767}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1767}#
                                            '(any any))))
                                       (list (#{f\ 1762}#
                                               (cdr #{bindings\ 1763}#))
                                             (car #{bindings\ 1763}#)))))))
                         (#{f\ 1762}# (map list #{x\ 1758}# #{v\ 1759}#))))
                     #{tmp\ 1750}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1749}#)))
          ($sc-dispatch
            #{tmp\ 1749}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 1748}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 1771}#)
      ((lambda (#{tmp\ 1772}#)
         ((lambda (#{tmp\ 1773}#)
            (if #{tmp\ 1773}#
              (apply (lambda (#{_\ 1774}#
                              #{var\ 1775}#
                              #{init\ 1776}#
                              #{step\ 1777}#
                              #{e0\ 1778}#
                              #{e1\ 1779}#
                              #{c\ 1780}#)
                       ((lambda (#{tmp\ 1781}#)
                          ((lambda (#{tmp\ 1782}#)
                             (if #{tmp\ 1782}#
                               (apply (lambda (#{step\ 1783}#)
                                        ((lambda (#{tmp\ 1784}#)
                                           ((lambda (#{tmp\ 1785}#)
                                              (if #{tmp\ 1785}#
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
                                                                    #{var\ 1775}#
                                                                    #{init\ 1776}#)
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
                                                                           #{e0\ 1778}#)
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
                                                                             #{c\ 1780}#
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
                                                                                         #{step\ 1783}#)))))))
                                                       #{tmp\ 1785}#)
                                                ((lambda (#{tmp\ 1790}#)
                                                   (if #{tmp\ 1790}#
                                                     (apply (lambda (#{e1\ 1791}#
                                                                     #{e2\ 1792}#)
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
                                                                         #{var\ 1775}#
                                                                         #{init\ 1776}#)
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
                                                                          #{e0\ 1778}#
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
                                                                                (cons #{e1\ 1791}#
                                                                                      #{e2\ 1792}#))
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
                                                                                  #{c\ 1780}#
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
                                                                                              #{step\ 1783}#)))))))
                                                            #{tmp\ 1790}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1784}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1784}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 1784}#
                                              '())))
                                         #{e1\ 1779}#))
                                      #{tmp\ 1782}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 1781}#)))
                           ($sc-dispatch #{tmp\ 1781}# (quote each-any))))
                        (map (lambda (#{v\ 1799}# #{s\ 1800}#)
                               ((lambda (#{tmp\ 1801}#)
                                  ((lambda (#{tmp\ 1802}#)
                                     (if #{tmp\ 1802}#
                                       (apply (lambda () #{v\ 1799}#)
                                              #{tmp\ 1802}#)
                                       ((lambda (#{tmp\ 1803}#)
                                          (if #{tmp\ 1803}#
                                            (apply (lambda (#{e\ 1804}#)
                                                     #{e\ 1804}#)
                                                   #{tmp\ 1803}#)
                                            ((lambda (#{_\ 1805}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 1771}#
                                                 #{s\ 1800}#))
                                             #{tmp\ 1801}#)))
                                        ($sc-dispatch
                                          #{tmp\ 1801}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 1801}# (quote ()))))
                                #{s\ 1800}#))
                             #{var\ 1775}#
                             #{step\ 1777}#)))
                     #{tmp\ 1773}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1772}#)))
          ($sc-dispatch
            #{tmp\ 1772}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 1771}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 1808}#
               (lambda (#{x\ 1812}# #{y\ 1813}#)
                 ((lambda (#{tmp\ 1814}#)
                    ((lambda (#{tmp\ 1815}#)
                       (if #{tmp\ 1815}#
                         (apply (lambda (#{x\ 1816}# #{y\ 1817}#)
                                  ((lambda (#{tmp\ 1818}#)
                                     ((lambda (#{tmp\ 1819}#)
                                        (if #{tmp\ 1819}#
                                          (apply (lambda (#{dy\ 1820}#)
                                                   ((lambda (#{tmp\ 1821}#)
                                                      ((lambda (#{tmp\ 1822}#)
                                                         (if #{tmp\ 1822}#
                                                           (apply (lambda (#{dx\ 1823}#)
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
                                                                          (cons #{dx\ 1823}#
                                                                                #{dy\ 1820}#)))
                                                                  #{tmp\ 1822}#)
                                                           ((lambda (#{_\ 1824}#)
                                                              (if (null? #{dy\ 1820}#)
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
                                                                      #{x\ 1816}#)
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
                                                                      #{x\ 1816}#
                                                                      #{y\ 1817}#)))
                                                            #{tmp\ 1821}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1821}#
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
                                                    #{x\ 1816}#))
                                                 #{tmp\ 1819}#)
                                          ((lambda (#{tmp\ 1825}#)
                                             (if #{tmp\ 1825}#
                                               (apply (lambda (#{stuff\ 1826}#)
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
                                                              (cons #{x\ 1816}#
                                                                    #{stuff\ 1826}#)))
                                                      #{tmp\ 1825}#)
                                               ((lambda (#{else\ 1827}#)
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
                                                        #{x\ 1816}#
                                                        #{y\ 1817}#))
                                                #{tmp\ 1818}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1818}#
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
                                        #{tmp\ 1818}#
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
                                   #{y\ 1817}#))
                                #{tmp\ 1815}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1814}#)))
                     ($sc-dispatch #{tmp\ 1814}# (quote (any any)))))
                  (list #{x\ 1812}# #{y\ 1813}#))))
             (#{quasiappend\ 1809}#
               (lambda (#{x\ 1828}# #{y\ 1829}#)
                 ((lambda (#{tmp\ 1830}#)
                    ((lambda (#{tmp\ 1831}#)
                       (if #{tmp\ 1831}#
                         (apply (lambda (#{x\ 1832}# #{y\ 1833}#)
                                  ((lambda (#{tmp\ 1834}#)
                                     ((lambda (#{tmp\ 1835}#)
                                        (if #{tmp\ 1835}#
                                          (apply (lambda () #{x\ 1832}#)
                                                 #{tmp\ 1835}#)
                                          ((lambda (#{_\ 1836}#)
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
                                                   #{x\ 1832}#
                                                   #{y\ 1833}#))
                                           #{tmp\ 1834}#)))
                                      ($sc-dispatch
                                        #{tmp\ 1834}#
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
                                   #{y\ 1833}#))
                                #{tmp\ 1831}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1830}#)))
                     ($sc-dispatch #{tmp\ 1830}# (quote (any any)))))
                  (list #{x\ 1828}# #{y\ 1829}#))))
             (#{quasivector\ 1810}#
               (lambda (#{x\ 1837}#)
                 ((lambda (#{tmp\ 1838}#)
                    ((lambda (#{x\ 1839}#)
                       ((lambda (#{tmp\ 1840}#)
                          ((lambda (#{tmp\ 1841}#)
                             (if #{tmp\ 1841}#
                               (apply (lambda (#{x\ 1842}#)
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
                                              (list->vector #{x\ 1842}#)))
                                      #{tmp\ 1841}#)
                               ((lambda (#{tmp\ 1844}#)
                                  (if #{tmp\ 1844}#
                                    (apply (lambda (#{x\ 1845}#)
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
                                                   #{x\ 1845}#))
                                           #{tmp\ 1844}#)
                                    ((lambda (#{_\ 1847}#)
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
                                             #{x\ 1839}#))
                                     #{tmp\ 1840}#)))
                                ($sc-dispatch
                                  #{tmp\ 1840}#
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
                             #{tmp\ 1840}#
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
                        #{x\ 1839}#))
                     #{tmp\ 1838}#))
                  #{x\ 1837}#)))
             (#{quasi\ 1811}#
               (lambda (#{p\ 1848}# #{lev\ 1849}#)
                 ((lambda (#{tmp\ 1850}#)
                    ((lambda (#{tmp\ 1851}#)
                       (if #{tmp\ 1851}#
                         (apply (lambda (#{p\ 1852}#)
                                  (if (= #{lev\ 1849}# 0)
                                    #{p\ 1852}#
                                    (#{quasicons\ 1808}#
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
                                      (#{quasi\ 1811}#
                                        (list #{p\ 1852}#)
                                        (- #{lev\ 1849}# 1)))))
                                #{tmp\ 1851}#)
                         ((lambda (#{tmp\ 1853}#)
                            (if (if #{tmp\ 1853}#
                                  (apply (lambda (#{args\ 1854}#)
                                           (= #{lev\ 1849}# 0))
                                         #{tmp\ 1853}#)
                                  #f)
                              (apply (lambda (#{args\ 1855}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 1848}#
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
                                               #{args\ 1855}#)))
                                     #{tmp\ 1853}#)
                              ((lambda (#{tmp\ 1856}#)
                                 (if #{tmp\ 1856}#
                                   (apply (lambda (#{p\ 1857}# #{q\ 1858}#)
                                            (if (= #{lev\ 1849}# 0)
                                              (#{quasiappend\ 1809}#
                                                #{p\ 1857}#
                                                (#{quasi\ 1811}#
                                                  #{q\ 1858}#
                                                  #{lev\ 1849}#))
                                              (#{quasicons\ 1808}#
                                                (#{quasicons\ 1808}#
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
                                                  (#{quasi\ 1811}#
                                                    (list #{p\ 1857}#)
                                                    (- #{lev\ 1849}# 1)))
                                                (#{quasi\ 1811}#
                                                  #{q\ 1858}#
                                                  #{lev\ 1849}#))))
                                          #{tmp\ 1856}#)
                                   ((lambda (#{tmp\ 1859}#)
                                      (if (if #{tmp\ 1859}#
                                            (apply (lambda (#{args\ 1860}#
                                                            #{q\ 1861}#)
                                                     (= #{lev\ 1849}# 0))
                                                   #{tmp\ 1859}#)
                                            #f)
                                        (apply (lambda (#{args\ 1862}#
                                                        #{q\ 1863}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 1848}#
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
                                                         #{args\ 1862}#)))
                                               #{tmp\ 1859}#)
                                        ((lambda (#{tmp\ 1864}#)
                                           (if #{tmp\ 1864}#
                                             (apply (lambda (#{p\ 1865}#)
                                                      (#{quasicons\ 1808}#
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
                                                        (#{quasi\ 1811}#
                                                          (list #{p\ 1865}#)
                                                          (+ #{lev\ 1849}#
                                                             1))))
                                                    #{tmp\ 1864}#)
                                             ((lambda (#{tmp\ 1866}#)
                                                (if #{tmp\ 1866}#
                                                  (apply (lambda (#{p\ 1867}#
                                                                  #{q\ 1868}#)
                                                           (#{quasicons\ 1808}#
                                                             (#{quasi\ 1811}#
                                                               #{p\ 1867}#
                                                               #{lev\ 1849}#)
                                                             (#{quasi\ 1811}#
                                                               #{q\ 1868}#
                                                               #{lev\ 1849}#)))
                                                         #{tmp\ 1866}#)
                                                  ((lambda (#{tmp\ 1869}#)
                                                     (if #{tmp\ 1869}#
                                                       (apply (lambda (#{x\ 1870}#)
                                                                (#{quasivector\ 1810}#
                                                                  (#{quasi\ 1811}#
                                                                    #{x\ 1870}#
                                                                    #{lev\ 1849}#)))
                                                              #{tmp\ 1869}#)
                                                       ((lambda (#{p\ 1872}#)
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
                                                                #{p\ 1872}#))
                                                        #{tmp\ 1850}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 1850}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 1850}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 1850}#
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
                                      #{tmp\ 1850}#
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
                                 #{tmp\ 1850}#
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
                            #{tmp\ 1850}#
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
                       #{tmp\ 1850}#
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
                  #{p\ 1848}#))))
      (lambda (#{x\ 1873}#)
        ((lambda (#{tmp\ 1874}#)
           ((lambda (#{tmp\ 1875}#)
              (if #{tmp\ 1875}#
                (apply (lambda (#{_\ 1876}# #{e\ 1877}#)
                         (#{quasi\ 1811}# #{e\ 1877}# 0))
                       #{tmp\ 1875}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1874}#)))
            ($sc-dispatch #{tmp\ 1874}# (quote (any any)))))
         #{x\ 1873}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1878}#)
      (letrec ((#{read-file\ 1879}#
                 (lambda (#{fn\ 1880}# #{k\ 1881}#)
                   (let ((#{p\ 1882}# (open-input-file #{fn\ 1880}#)))
                     (letrec ((#{f\ 1883}#
                                (lambda (#{x\ 1884}#)
                                  (if (eof-object? #{x\ 1884}#)
                                    (begin
                                      (close-input-port #{p\ 1882}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 1881}#
                                            #{x\ 1884}#)
                                          (#{f\ 1883}# (read #{p\ 1882}#)))))))
                       (#{f\ 1883}# (read #{p\ 1882}#)))))))
        ((lambda (#{tmp\ 1885}#)
           ((lambda (#{tmp\ 1886}#)
              (if #{tmp\ 1886}#
                (apply (lambda (#{k\ 1887}# #{filename\ 1888}#)
                         (let ((#{fn\ 1889}#
                                 (syntax->datum #{filename\ 1888}#)))
                           ((lambda (#{tmp\ 1890}#)
                              ((lambda (#{tmp\ 1891}#)
                                 (if #{tmp\ 1891}#
                                   (apply (lambda (#{exp\ 1892}#)
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
                                                  #{exp\ 1892}#))
                                          #{tmp\ 1891}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 1890}#)))
                               ($sc-dispatch #{tmp\ 1890}# (quote each-any))))
                            (#{read-file\ 1879}# #{fn\ 1889}# #{k\ 1887}#))))
                       #{tmp\ 1886}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1885}#)))
            ($sc-dispatch #{tmp\ 1885}# (quote (any any)))))
         #{x\ 1878}#)))))

(define include-from-path
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1894}#)
      ((lambda (#{tmp\ 1895}#)
         ((lambda (#{tmp\ 1896}#)
            (if #{tmp\ 1896}#
              (apply (lambda (#{k\ 1897}# #{filename\ 1898}#)
                       (let ((#{fn\ 1899}# (syntax->datum #{filename\ 1898}#)))
                         ((lambda (#{tmp\ 1900}#)
                            ((lambda (#{fn\ 1901}#)
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
                                     #{fn\ 1901}#))
                             #{tmp\ 1900}#))
                          (let ((#{t\ 1902}# (%search-load-path #{fn\ 1899}#)))
                            (if #{t\ 1902}#
                              #{t\ 1902}#
                              (syntax-violation
                                'include-from-path
                                "file not found in path"
                                #{x\ 1894}#
                                #{filename\ 1898}#))))))
                     #{tmp\ 1896}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1895}#)))
          ($sc-dispatch #{tmp\ 1895}# (quote (any any)))))
       #{x\ 1894}#))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1903}#)
      ((lambda (#{tmp\ 1904}#)
         ((lambda (#{tmp\ 1905}#)
            (if #{tmp\ 1905}#
              (apply (lambda (#{_\ 1906}# #{e\ 1907}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 1903}#))
                     #{tmp\ 1905}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1904}#)))
          ($sc-dispatch #{tmp\ 1904}# (quote (any any)))))
       #{x\ 1903}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1908}#)
      ((lambda (#{tmp\ 1909}#)
         ((lambda (#{tmp\ 1910}#)
            (if #{tmp\ 1910}#
              (apply (lambda (#{_\ 1911}# #{e\ 1912}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 1908}#))
                     #{tmp\ 1910}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1909}#)))
          ($sc-dispatch #{tmp\ 1909}# (quote (any any)))))
       #{x\ 1908}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 1913}#)
      ((lambda (#{tmp\ 1914}#)
         ((lambda (#{tmp\ 1915}#)
            (if #{tmp\ 1915}#
              (apply (lambda (#{_\ 1916}#
                              #{e\ 1917}#
                              #{m1\ 1918}#
                              #{m2\ 1919}#)
                       ((lambda (#{tmp\ 1920}#)
                          ((lambda (#{body\ 1921}#)
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
                                               #{e\ 1917}#))
                                   #{body\ 1921}#))
                           #{tmp\ 1920}#))
                        (letrec ((#{f\ 1922}#
                                   (lambda (#{clause\ 1923}# #{clauses\ 1924}#)
                                     (if (null? #{clauses\ 1924}#)
                                       ((lambda (#{tmp\ 1926}#)
                                          ((lambda (#{tmp\ 1927}#)
                                             (if #{tmp\ 1927}#
                                               (apply (lambda (#{e1\ 1928}#
                                                               #{e2\ 1929}#)
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
                                                              (cons #{e1\ 1928}#
                                                                    #{e2\ 1929}#)))
                                                      #{tmp\ 1927}#)
                                               ((lambda (#{tmp\ 1931}#)
                                                  (if #{tmp\ 1931}#
                                                    (apply (lambda (#{k\ 1932}#
                                                                    #{e1\ 1933}#
                                                                    #{e2\ 1934}#)
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
                                                                               #{k\ 1932}#))
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
                                                                         (cons #{e1\ 1933}#
                                                                               #{e2\ 1934}#))))
                                                           #{tmp\ 1931}#)
                                                    ((lambda (#{_\ 1937}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 1913}#
                                                         #{clause\ 1923}#))
                                                     #{tmp\ 1926}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 1926}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 1926}#
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
                                        #{clause\ 1923}#)
                                       ((lambda (#{tmp\ 1938}#)
                                          ((lambda (#{rest\ 1939}#)
                                             ((lambda (#{tmp\ 1940}#)
                                                ((lambda (#{tmp\ 1941}#)
                                                   (if #{tmp\ 1941}#
                                                     (apply (lambda (#{k\ 1942}#
                                                                     #{e1\ 1943}#
                                                                     #{e2\ 1944}#)
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
                                                                                #{k\ 1942}#))
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
                                                                          (cons #{e1\ 1943}#
                                                                                #{e2\ 1944}#))
                                                                    #{rest\ 1939}#))
                                                            #{tmp\ 1941}#)
                                                     ((lambda (#{_\ 1947}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 1913}#
                                                          #{clause\ 1923}#))
                                                      #{tmp\ 1940}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1940}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 1923}#))
                                           #{tmp\ 1938}#))
                                        (#{f\ 1922}#
                                          (car #{clauses\ 1924}#)
                                          (cdr #{clauses\ 1924}#)))))))
                          (#{f\ 1922}# #{m1\ 1918}# #{m2\ 1919}#))))
                     #{tmp\ 1915}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1914}#)))
          ($sc-dispatch
            #{tmp\ 1914}#
            '(any any any . each-any))))
       #{x\ 1913}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1948}#)
      ((lambda (#{tmp\ 1949}#)
         ((lambda (#{tmp\ 1950}#)
            (if #{tmp\ 1950}#
              (apply (lambda (#{_\ 1951}# #{e\ 1952}#)
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
                                               #{e\ 1952}#))
                                   (list (cons #{_\ 1951}#
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
                                               (cons #{e\ 1952}#
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
                     #{tmp\ 1950}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1949}#)))
          ($sc-dispatch #{tmp\ 1949}# (quote (any any)))))
       #{x\ 1948}#))))

(define define*
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1953}#)
      ((lambda (#{tmp\ 1954}#)
         ((lambda (#{tmp\ 1955}#)
            (if #{tmp\ 1955}#
              (apply (lambda (#{dummy\ 1956}#
                              #{id\ 1957}#
                              #{args\ 1958}#
                              #{b0\ 1959}#
                              #{b1\ 1960}#)
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
                             #{id\ 1957}#
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
                                   (cons #{args\ 1958}#
                                         (cons #{b0\ 1959}# #{b1\ 1960}#)))))
                     #{tmp\ 1955}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1954}#)))
          ($sc-dispatch
            #{tmp\ 1954}#
            '(any (any . any) any . each-any))))
       #{x\ 1953}#))))

