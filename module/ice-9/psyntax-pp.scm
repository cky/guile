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
                                               #f
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
                                                    #f
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
                                                      #{pred\ 427}#
                                                      #{body\ 428}#)
                                               (call-with-values
                                                 (lambda ()
                                                   (#{chi-lambda-case\ 179}#
                                                     #{e\ 321}#
                                                     #{r\ 322}#
                                                     #{w\ 323}#
                                                     #{s\ 324}#
                                                     #{mod\ 325}#
                                                     #{get-formals\ 326}#
                                                     (map (lambda (#{tmp\ 431}#
                                                                   #{tmp\ 430}#
                                                                   #{tmp\ 429}#)
                                                            (cons #{tmp\ 429}#
                                                                  (cons #{tmp\ 430}#
                                                                        #{tmp\ 431}#)))
                                                          #{e2*\ 414}#
                                                          #{e1*\ 413}#
                                                          #{args*\ 412}#)))
                                                 (lambda (#{docstring*\ 433}#
                                                          #{else*\ 434}#)
                                                   (values
                                                     (let ((#{t\ 435}# #{docstring\ 420}#))
                                                       (if #{t\ 435}#
                                                         #{t\ 435}#
                                                         #{docstring*\ 433}#))
                                                     (#{build-lambda-case\ 107}#
                                                       #{s\ 324}#
                                                       #{req\ 421}#
                                                       #{opt\ 422}#
                                                       #{rest\ 423}#
                                                       #{kw\ 424}#
                                                       #{inits\ 425}#
                                                       #{vars\ 426}#
                                                       #{pred\ 427}#
                                                       #{body\ 428}#
                                                       #{else*\ 434}#)))))))))
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
             (lambda (#{orig-args\ 436}#)
               (letrec ((#{check\ 441}#
                          (lambda (#{req\ 442}#
                                   #{opt\ 443}#
                                   #{rest\ 444}#
                                   #{kw\ 445}#)
                            (if (#{distinct-bound-ids?\ 157}#
                                  (append
                                    #{req\ 442}#
                                    (map car #{opt\ 443}#)
                                    (if #{rest\ 444}#
                                      (list #{rest\ 444}#)
                                      '())
                                    (if (pair? #{kw\ 445}#)
                                      (map cadr (cdr #{kw\ 445}#))
                                      '())))
                              (values
                                #{req\ 442}#
                                #{opt\ 443}#
                                #{rest\ 444}#
                                #{kw\ 445}#)
                              (syntax-violation
                                'lambda*
                                "duplicate identifier in argument list"
                                #{orig-args\ 436}#))))
                        (#{rest\ 440}#
                          (lambda (#{args\ 446}#
                                   #{req\ 447}#
                                   #{opt\ 448}#
                                   #{kw\ 449}#)
                            ((lambda (#{tmp\ 450}#)
                               ((lambda (#{tmp\ 451}#)
                                  (if (if #{tmp\ 451}#
                                        (apply (lambda (#{r\ 452}#)
                                                 (#{id?\ 131}# #{r\ 452}#))
                                               #{tmp\ 451}#)
                                        #f)
                                    (apply (lambda (#{r\ 453}#)
                                             (#{check\ 441}#
                                               #{req\ 447}#
                                               #{opt\ 448}#
                                               #{r\ 453}#
                                               #{kw\ 449}#))
                                           #{tmp\ 451}#)
                                    ((lambda (#{else\ 454}#)
                                       (syntax-violation
                                         'lambda*
                                         "invalid rest argument"
                                         #{orig-args\ 436}#
                                         #{args\ 446}#))
                                     #{tmp\ 450}#)))
                                (list #{tmp\ 450}#)))
                             #{args\ 446}#)))
                        (#{key\ 439}#
                          (lambda (#{args\ 455}#
                                   #{req\ 456}#
                                   #{opt\ 457}#
                                   #{rkey\ 458}#)
                            ((lambda (#{tmp\ 459}#)
                               ((lambda (#{tmp\ 460}#)
                                  (if #{tmp\ 460}#
                                    (apply (lambda ()
                                             (#{check\ 441}#
                                               #{req\ 456}#
                                               #{opt\ 457}#
                                               #f
                                               (cons #f
                                                     (reverse #{rkey\ 458}#))))
                                           #{tmp\ 460}#)
                                    ((lambda (#{tmp\ 461}#)
                                       (if (if #{tmp\ 461}#
                                             (apply (lambda (#{a\ 462}#
                                                             #{b\ 463}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 462}#))
                                                    #{tmp\ 461}#)
                                             #f)
                                         (apply (lambda (#{a\ 464}# #{b\ 465}#)
                                                  ((lambda (#{tmp\ 466}#)
                                                     ((lambda (#{k\ 467}#)
                                                        (#{key\ 439}#
                                                          #{b\ 465}#
                                                          #{req\ 456}#
                                                          #{opt\ 457}#
                                                          (cons (cons #{k\ 467}#
                                                                      (cons #{a\ 464}#
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
                                                                #{rkey\ 458}#)))
                                                      #{tmp\ 466}#))
                                                   (symbol->keyword
                                                     (syntax->datum
                                                       #{a\ 464}#))))
                                                #{tmp\ 461}#)
                                         ((lambda (#{tmp\ 468}#)
                                            (if (if #{tmp\ 468}#
                                                  (apply (lambda (#{a\ 469}#
                                                                  #{init\ 470}#
                                                                  #{b\ 471}#)
                                                           (#{id?\ 131}#
                                                             #{a\ 469}#))
                                                         #{tmp\ 468}#)
                                                  #f)
                                              (apply (lambda (#{a\ 472}#
                                                              #{init\ 473}#
                                                              #{b\ 474}#)
                                                       ((lambda (#{tmp\ 475}#)
                                                          ((lambda (#{k\ 476}#)
                                                             (#{key\ 439}#
                                                               #{b\ 474}#
                                                               #{req\ 456}#
                                                               #{opt\ 457}#
                                                               (cons (list #{k\ 476}#
                                                                           #{a\ 472}#
                                                                           #{init\ 473}#)
                                                                     #{rkey\ 458}#)))
                                                           #{tmp\ 475}#))
                                                        (symbol->keyword
                                                          (syntax->datum
                                                            #{a\ 472}#))))
                                                     #{tmp\ 468}#)
                                              ((lambda (#{tmp\ 477}#)
                                                 (if (if #{tmp\ 477}#
                                                       (apply (lambda (#{a\ 478}#
                                                                       #{init\ 479}#
                                                                       #{k\ 480}#
                                                                       #{b\ 481}#)
                                                                (if (#{id?\ 131}#
                                                                      #{a\ 478}#)
                                                                  (keyword?
                                                                    (syntax->datum
                                                                      #{k\ 480}#))
                                                                  #f))
                                                              #{tmp\ 477}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 482}#
                                                                   #{init\ 483}#
                                                                   #{k\ 484}#
                                                                   #{b\ 485}#)
                                                            (#{key\ 439}#
                                                              #{b\ 485}#
                                                              #{req\ 456}#
                                                              #{opt\ 457}#
                                                              (cons (list #{k\ 484}#
                                                                          #{a\ 482}#
                                                                          #{init\ 483}#)
                                                                    #{rkey\ 458}#)))
                                                          #{tmp\ 477}#)
                                                   ((lambda (#{tmp\ 486}#)
                                                      (if (if #{tmp\ 486}#
                                                            (apply (lambda (#{aok\ 487}#)
                                                                     (eq? (syntax->datum
                                                                            #{aok\ 487}#)
                                                                          #:allow-other-keys))
                                                                   #{tmp\ 486}#)
                                                            #f)
                                                        (apply (lambda (#{aok\ 488}#)
                                                                 (#{check\ 441}#
                                                                   #{req\ 456}#
                                                                   #{opt\ 457}#
                                                                   #f
                                                                   (cons #t
                                                                         (reverse
                                                                           #{rkey\ 458}#))))
                                                               #{tmp\ 486}#)
                                                        ((lambda (#{tmp\ 489}#)
                                                           (if (if #{tmp\ 489}#
                                                                 (apply (lambda (#{aok\ 490}#
                                                                                 #{a\ 491}#
                                                                                 #{b\ 492}#)
                                                                          (if (eq? (syntax->datum
                                                                                     #{aok\ 490}#)
                                                                                   #:allow-other-keys)
                                                                            (eq? (syntax->datum
                                                                                   #{a\ 491}#)
                                                                                 #:rest)
                                                                            #f))
                                                                        #{tmp\ 489}#)
                                                                 #f)
                                                             (apply (lambda (#{aok\ 493}#
                                                                             #{a\ 494}#
                                                                             #{b\ 495}#)
                                                                      (#{rest\ 440}#
                                                                        #{b\ 495}#
                                                                        #{req\ 456}#
                                                                        #{opt\ 457}#
                                                                        (cons #t
                                                                              (reverse
                                                                                #{rkey\ 458}#))))
                                                                    #{tmp\ 489}#)
                                                             ((lambda (#{tmp\ 496}#)
                                                                (if (if #{tmp\ 496}#
                                                                      (apply (lambda (#{aok\ 497}#
                                                                                      #{r\ 498}#)
                                                                               (if (eq? (syntax->datum
                                                                                          #{aok\ 497}#)
                                                                                        #:allow-other-keys)
                                                                                 (#{id?\ 131}#
                                                                                   #{r\ 498}#)
                                                                                 #f))
                                                                             #{tmp\ 496}#)
                                                                      #f)
                                                                  (apply (lambda (#{aok\ 499}#
                                                                                  #{r\ 500}#)
                                                                           (#{rest\ 440}#
                                                                             #{r\ 500}#
                                                                             #{req\ 456}#
                                                                             #{opt\ 457}#
                                                                             (cons #t
                                                                                   (reverse
                                                                                     #{rkey\ 458}#))))
                                                                         #{tmp\ 496}#)
                                                                  ((lambda (#{tmp\ 501}#)
                                                                     (if (if #{tmp\ 501}#
                                                                           (apply (lambda (#{a\ 502}#
                                                                                           #{b\ 503}#)
                                                                                    (eq? (syntax->datum
                                                                                           #{a\ 502}#)
                                                                                         #:rest))
                                                                                  #{tmp\ 501}#)
                                                                           #f)
                                                                       (apply (lambda (#{a\ 504}#
                                                                                       #{b\ 505}#)
                                                                                (#{rest\ 440}#
                                                                                  #{b\ 505}#
                                                                                  #{req\ 456}#
                                                                                  #{opt\ 457}#
                                                                                  (cons #f
                                                                                        (reverse
                                                                                          #{rkey\ 458}#))))
                                                                              #{tmp\ 501}#)
                                                                       ((lambda (#{tmp\ 506}#)
                                                                          (if (if #{tmp\ 506}#
                                                                                (apply (lambda (#{r\ 507}#)
                                                                                         (#{id?\ 131}#
                                                                                           #{r\ 507}#))
                                                                                       #{tmp\ 506}#)
                                                                                #f)
                                                                            (apply (lambda (#{r\ 508}#)
                                                                                     (#{rest\ 440}#
                                                                                       #{r\ 508}#
                                                                                       #{req\ 456}#
                                                                                       #{opt\ 457}#
                                                                                       (cons #f
                                                                                             (reverse
                                                                                               #{rkey\ 458}#))))
                                                                                   #{tmp\ 506}#)
                                                                            ((lambda (#{else\ 509}#)
                                                                               (syntax-violation
                                                                                 'lambda*
                                                                                 "invalid keyword argument list"
                                                                                 #{orig-args\ 436}#
                                                                                 #{args\ 455}#))
                                                                             #{tmp\ 459}#)))
                                                                        (list #{tmp\ 459}#))))
                                                                   ($sc-dispatch
                                                                     #{tmp\ 459}#
                                                                     '(any any)))))
                                                              ($sc-dispatch
                                                                #{tmp\ 459}#
                                                                '(any .
                                                                      any)))))
                                                         ($sc-dispatch
                                                           #{tmp\ 459}#
                                                           '(any any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 459}#
                                                      '(any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 459}#
                                                 '((any any any) . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 459}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 459}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 459}# (quote ()))))
                             #{args\ 455}#)))
                        (#{opt\ 438}#
                          (lambda (#{args\ 510}# #{req\ 511}# #{ropt\ 512}#)
                            ((lambda (#{tmp\ 513}#)
                               ((lambda (#{tmp\ 514}#)
                                  (if #{tmp\ 514}#
                                    (apply (lambda ()
                                             (#{check\ 441}#
                                               #{req\ 511}#
                                               (reverse #{ropt\ 512}#)
                                               #f
                                               '()))
                                           #{tmp\ 514}#)
                                    ((lambda (#{tmp\ 515}#)
                                       (if (if #{tmp\ 515}#
                                             (apply (lambda (#{a\ 516}#
                                                             #{b\ 517}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 516}#))
                                                    #{tmp\ 515}#)
                                             #f)
                                         (apply (lambda (#{a\ 518}# #{b\ 519}#)
                                                  (#{opt\ 438}#
                                                    #{b\ 519}#
                                                    #{req\ 511}#
                                                    (cons (cons #{a\ 518}#
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
                                                          #{ropt\ 512}#)))
                                                #{tmp\ 515}#)
                                         ((lambda (#{tmp\ 520}#)
                                            (if (if #{tmp\ 520}#
                                                  (apply (lambda (#{a\ 521}#
                                                                  #{init\ 522}#
                                                                  #{b\ 523}#)
                                                           (#{id?\ 131}#
                                                             #{a\ 521}#))
                                                         #{tmp\ 520}#)
                                                  #f)
                                              (apply (lambda (#{a\ 524}#
                                                              #{init\ 525}#
                                                              #{b\ 526}#)
                                                       (#{opt\ 438}#
                                                         #{b\ 526}#
                                                         #{req\ 511}#
                                                         (cons (list #{a\ 524}#
                                                                     #{init\ 525}#)
                                                               #{ropt\ 512}#)))
                                                     #{tmp\ 520}#)
                                              ((lambda (#{tmp\ 527}#)
                                                 (if (if #{tmp\ 527}#
                                                       (apply (lambda (#{a\ 528}#
                                                                       #{b\ 529}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 528}#)
                                                                     #:key))
                                                              #{tmp\ 527}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 530}#
                                                                   #{b\ 531}#)
                                                            (#{key\ 439}#
                                                              #{b\ 531}#
                                                              #{req\ 511}#
                                                              (reverse
                                                                #{ropt\ 512}#)
                                                              '()))
                                                          #{tmp\ 527}#)
                                                   ((lambda (#{tmp\ 532}#)
                                                      (if (if #{tmp\ 532}#
                                                            (apply (lambda (#{a\ 533}#
                                                                            #{b\ 534}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 533}#)
                                                                          #:rest))
                                                                   #{tmp\ 532}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 535}#
                                                                        #{b\ 536}#)
                                                                 (#{rest\ 440}#
                                                                   #{b\ 536}#
                                                                   #{req\ 511}#
                                                                   (reverse
                                                                     #{ropt\ 512}#)
                                                                   '()))
                                                               #{tmp\ 532}#)
                                                        ((lambda (#{tmp\ 537}#)
                                                           (if (if #{tmp\ 537}#
                                                                 (apply (lambda (#{r\ 538}#)
                                                                          (#{id?\ 131}#
                                                                            #{r\ 538}#))
                                                                        #{tmp\ 537}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 539}#)
                                                                      (#{rest\ 440}#
                                                                        #{r\ 539}#
                                                                        #{req\ 511}#
                                                                        (reverse
                                                                          #{ropt\ 512}#)
                                                                        '()))
                                                                    #{tmp\ 537}#)
                                                             ((lambda (#{else\ 540}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid optional argument list"
                                                                  #{orig-args\ 436}#
                                                                  #{args\ 510}#))
                                                              #{tmp\ 513}#)))
                                                         (list #{tmp\ 513}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 513}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 513}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 513}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 513}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 513}# (quote ()))))
                             #{args\ 510}#)))
                        (#{req\ 437}#
                          (lambda (#{args\ 541}# #{rreq\ 542}#)
                            ((lambda (#{tmp\ 543}#)
                               ((lambda (#{tmp\ 544}#)
                                  (if #{tmp\ 544}#
                                    (apply (lambda ()
                                             (#{check\ 441}#
                                               (reverse #{rreq\ 542}#)
                                               '()
                                               #f
                                               '()))
                                           #{tmp\ 544}#)
                                    ((lambda (#{tmp\ 545}#)
                                       (if (if #{tmp\ 545}#
                                             (apply (lambda (#{a\ 546}#
                                                             #{b\ 547}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 546}#))
                                                    #{tmp\ 545}#)
                                             #f)
                                         (apply (lambda (#{a\ 548}# #{b\ 549}#)
                                                  (#{req\ 437}#
                                                    #{b\ 549}#
                                                    (cons #{a\ 548}#
                                                          #{rreq\ 542}#)))
                                                #{tmp\ 545}#)
                                         ((lambda (#{tmp\ 550}#)
                                            (if (if #{tmp\ 550}#
                                                  (apply (lambda (#{a\ 551}#
                                                                  #{b\ 552}#)
                                                           (eq? (syntax->datum
                                                                  #{a\ 551}#)
                                                                #:optional))
                                                         #{tmp\ 550}#)
                                                  #f)
                                              (apply (lambda (#{a\ 553}#
                                                              #{b\ 554}#)
                                                       (#{opt\ 438}#
                                                         #{b\ 554}#
                                                         (reverse
                                                           #{rreq\ 542}#)
                                                         '()))
                                                     #{tmp\ 550}#)
                                              ((lambda (#{tmp\ 555}#)
                                                 (if (if #{tmp\ 555}#
                                                       (apply (lambda (#{a\ 556}#
                                                                       #{b\ 557}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 556}#)
                                                                     #:key))
                                                              #{tmp\ 555}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 558}#
                                                                   #{b\ 559}#)
                                                            (#{key\ 439}#
                                                              #{b\ 559}#
                                                              (reverse
                                                                #{rreq\ 542}#)
                                                              '()
                                                              '()))
                                                          #{tmp\ 555}#)
                                                   ((lambda (#{tmp\ 560}#)
                                                      (if (if #{tmp\ 560}#
                                                            (apply (lambda (#{a\ 561}#
                                                                            #{b\ 562}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 561}#)
                                                                          #:rest))
                                                                   #{tmp\ 560}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 563}#
                                                                        #{b\ 564}#)
                                                                 (#{rest\ 440}#
                                                                   #{b\ 564}#
                                                                   (reverse
                                                                     #{rreq\ 542}#)
                                                                   '()
                                                                   '()))
                                                               #{tmp\ 560}#)
                                                        ((lambda (#{tmp\ 565}#)
                                                           (if (if #{tmp\ 565}#
                                                                 (apply (lambda (#{r\ 566}#)
                                                                          (#{id?\ 131}#
                                                                            #{r\ 566}#))
                                                                        #{tmp\ 565}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 567}#)
                                                                      (#{rest\ 440}#
                                                                        #{r\ 567}#
                                                                        (reverse
                                                                          #{rreq\ 542}#)
                                                                        '()
                                                                        '()))
                                                                    #{tmp\ 565}#)
                                                             ((lambda (#{else\ 568}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid argument list"
                                                                  #{orig-args\ 436}#
                                                                  #{args\ 541}#))
                                                              #{tmp\ 543}#)))
                                                         (list #{tmp\ 543}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 543}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 543}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 543}#
                                            '(any . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 543}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 543}# (quote ()))))
                             #{args\ 541}#))))
                 (#{req\ 437}# #{orig-args\ 436}# (quote ())))))
           (#{chi-simple-lambda\ 177}#
             (lambda (#{e\ 569}#
                      #{r\ 570}#
                      #{w\ 571}#
                      #{s\ 572}#
                      #{mod\ 573}#
                      #{req\ 574}#
                      #{rest\ 575}#
                      #{docstring\ 576}#
                      #{body\ 577}#)
               (let ((#{ids\ 578}#
                       (if #{rest\ 575}#
                         (append #{req\ 574}# (list #{rest\ 575}#))
                         #{req\ 574}#)))
                 (let ((#{vars\ 579}#
                         (map #{gen-var\ 181}# #{ids\ 578}#)))
                   (let ((#{labels\ 580}#
                           (#{gen-labels\ 137}# #{ids\ 578}#)))
                     (#{build-simple-lambda\ 105}#
                       #{s\ 572}#
                       (map syntax->datum #{req\ 574}#)
                       (if #{rest\ 575}#
                         (syntax->datum #{rest\ 575}#)
                         #f)
                       #{vars\ 579}#
                       #{docstring\ 576}#
                       (#{chi-body\ 171}#
                         #{body\ 577}#
                         (#{source-wrap\ 160}#
                           #{e\ 569}#
                           #{w\ 571}#
                           #{s\ 572}#
                           #{mod\ 573}#)
                         (#{extend-var-env\ 126}#
                           #{labels\ 580}#
                           #{vars\ 579}#
                           #{r\ 570}#)
                         (#{make-binding-wrap\ 148}#
                           #{ids\ 578}#
                           #{labels\ 580}#
                           #{w\ 571}#)
                         #{mod\ 573}#)))))))
           (#{lambda-formals\ 176}#
             (lambda (#{orig-args\ 581}#)
               (letrec ((#{check\ 583}#
                          (lambda (#{req\ 584}# #{rest\ 585}#)
                            (if (#{distinct-bound-ids?\ 157}#
                                  (if #{rest\ 585}#
                                    (cons #{rest\ 585}# #{req\ 584}#)
                                    #{req\ 584}#))
                              (values #{req\ 584}# #f #{rest\ 585}# #f)
                              (syntax-violation
                                'lambda
                                "duplicate identifier in argument list"
                                #{orig-args\ 581}#))))
                        (#{req\ 582}#
                          (lambda (#{args\ 586}# #{rreq\ 587}#)
                            ((lambda (#{tmp\ 588}#)
                               ((lambda (#{tmp\ 589}#)
                                  (if #{tmp\ 589}#
                                    (apply (lambda ()
                                             (#{check\ 583}#
                                               (reverse #{rreq\ 587}#)
                                               #f))
                                           #{tmp\ 589}#)
                                    ((lambda (#{tmp\ 590}#)
                                       (if (if #{tmp\ 590}#
                                             (apply (lambda (#{a\ 591}#
                                                             #{b\ 592}#)
                                                      (#{id?\ 131}#
                                                        #{a\ 591}#))
                                                    #{tmp\ 590}#)
                                             #f)
                                         (apply (lambda (#{a\ 593}# #{b\ 594}#)
                                                  (#{req\ 582}#
                                                    #{b\ 594}#
                                                    (cons #{a\ 593}#
                                                          #{rreq\ 587}#)))
                                                #{tmp\ 590}#)
                                         ((lambda (#{tmp\ 595}#)
                                            (if (if #{tmp\ 595}#
                                                  (apply (lambda (#{r\ 596}#)
                                                           (#{id?\ 131}#
                                                             #{r\ 596}#))
                                                         #{tmp\ 595}#)
                                                  #f)
                                              (apply (lambda (#{r\ 597}#)
                                                       (#{check\ 583}#
                                                         (reverse
                                                           #{rreq\ 587}#)
                                                         #{r\ 597}#))
                                                     #{tmp\ 595}#)
                                              ((lambda (#{else\ 598}#)
                                                 (syntax-violation
                                                   'lambda
                                                   "invalid argument list"
                                                   #{orig-args\ 581}#
                                                   #{args\ 586}#))
                                               #{tmp\ 588}#)))
                                          (list #{tmp\ 588}#))))
                                     ($sc-dispatch
                                       #{tmp\ 588}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 588}# (quote ()))))
                             #{args\ 586}#))))
                 (#{req\ 582}# #{orig-args\ 581}# (quote ())))))
           (#{ellipsis?\ 175}#
             (lambda (#{x\ 599}#)
               (if (#{nonsymbol-id?\ 130}# #{x\ 599}#)
                 (#{free-id=?\ 154}#
                   #{x\ 599}#
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
             (lambda (#{expanded\ 600}# #{mod\ 601}#)
               (let ((#{p\ 602}# (#{local-eval-hook\ 91}#
                                   #{expanded\ 600}#
                                   #{mod\ 601}#)))
                 (if (procedure? #{p\ 602}#)
                   #{p\ 602}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 602}#)))))
           (#{chi-local-syntax\ 172}#
             (lambda (#{rec?\ 603}#
                      #{e\ 604}#
                      #{r\ 605}#
                      #{w\ 606}#
                      #{s\ 607}#
                      #{mod\ 608}#
                      #{k\ 609}#)
               ((lambda (#{tmp\ 610}#)
                  ((lambda (#{tmp\ 611}#)
                     (if #{tmp\ 611}#
                       (apply (lambda (#{_\ 612}#
                                       #{id\ 613}#
                                       #{val\ 614}#
                                       #{e1\ 615}#
                                       #{e2\ 616}#)
                                (let ((#{ids\ 617}# #{id\ 613}#))
                                  (if (not (#{valid-bound-ids?\ 156}#
                                             #{ids\ 617}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 604}#)
                                    (let ((#{labels\ 619}#
                                            (#{gen-labels\ 137}#
                                              #{ids\ 617}#)))
                                      (let ((#{new-w\ 620}#
                                              (#{make-binding-wrap\ 148}#
                                                #{ids\ 617}#
                                                #{labels\ 619}#
                                                #{w\ 606}#)))
                                        (#{k\ 609}# (cons #{e1\ 615}#
                                                          #{e2\ 616}#)
                                                    (#{extend-env\ 125}#
                                                      #{labels\ 619}#
                                                      (let ((#{w\ 622}# (if #{rec?\ 603}#
                                                                          #{new-w\ 620}#
                                                                          #{w\ 606}#))
                                                            (#{trans-r\ 623}#
                                                              (#{macros-only-env\ 127}#
                                                                #{r\ 605}#)))
                                                        (map (lambda (#{x\ 624}#)
                                                               (cons 'macro
                                                                     (#{eval-local-transformer\ 173}#
                                                                       (#{chi\ 167}#
                                                                         #{x\ 624}#
                                                                         #{trans-r\ 623}#
                                                                         #{w\ 622}#
                                                                         #{mod\ 608}#)
                                                                       #{mod\ 608}#)))
                                                             #{val\ 614}#))
                                                      #{r\ 605}#)
                                                    #{new-w\ 620}#
                                                    #{s\ 607}#
                                                    #{mod\ 608}#))))))
                              #{tmp\ 611}#)
                       ((lambda (#{_\ 626}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 160}#
                              #{e\ 604}#
                              #{w\ 606}#
                              #{s\ 607}#
                              #{mod\ 608}#)))
                        #{tmp\ 610}#)))
                   ($sc-dispatch
                     #{tmp\ 610}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 604}#)))
           (#{chi-body\ 171}#
             (lambda (#{body\ 627}#
                      #{outer-form\ 628}#
                      #{r\ 629}#
                      #{w\ 630}#
                      #{mod\ 631}#)
               (let ((#{r\ 632}# (cons '("placeholder" placeholder)
                                       #{r\ 629}#)))
                 (let ((#{ribcage\ 633}#
                         (#{make-ribcage\ 138}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 634}# (#{make-wrap\ 133}#
                                       (#{wrap-marks\ 134}# #{w\ 630}#)
                                       (cons #{ribcage\ 633}#
                                             (#{wrap-subst\ 135}#
                                               #{w\ 630}#)))))
                     (letrec ((#{parse\ 635}#
                                (lambda (#{body\ 636}#
                                         #{ids\ 637}#
                                         #{labels\ 638}#
                                         #{var-ids\ 639}#
                                         #{vars\ 640}#
                                         #{vals\ 641}#
                                         #{bindings\ 642}#)
                                  (if (null? #{body\ 636}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 628}#)
                                    (let ((#{e\ 644}# (cdar #{body\ 636}#))
                                          (#{er\ 645}# (caar #{body\ 636}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 165}#
                                            #{e\ 644}#
                                            #{er\ 645}#
                                            '(())
                                            (#{source-annotation\ 122}#
                                              #{er\ 645}#)
                                            #{ribcage\ 633}#
                                            #{mod\ 631}#
                                            #f))
                                        (lambda (#{type\ 646}#
                                                 #{value\ 647}#
                                                 #{e\ 648}#
                                                 #{w\ 649}#
                                                 #{s\ 650}#
                                                 #{mod\ 651}#)
                                          (if (memv #{type\ 646}#
                                                    '(define-form))
                                            (let ((#{id\ 652}#
                                                    (#{wrap\ 159}#
                                                      #{value\ 647}#
                                                      #{w\ 649}#
                                                      #{mod\ 651}#))
                                                  (#{label\ 653}#
                                                    (#{gen-label\ 136}#)))
                                              (let ((#{var\ 654}#
                                                      (#{gen-var\ 181}#
                                                        #{id\ 652}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 633}#
                                                    #{id\ 652}#
                                                    #{label\ 653}#)
                                                  (#{parse\ 635}#
                                                    (cdr #{body\ 636}#)
                                                    (cons #{id\ 652}#
                                                          #{ids\ 637}#)
                                                    (cons #{label\ 653}#
                                                          #{labels\ 638}#)
                                                    (cons #{id\ 652}#
                                                          #{var-ids\ 639}#)
                                                    (cons #{var\ 654}#
                                                          #{vars\ 640}#)
                                                    (cons (cons #{er\ 645}#
                                                                (#{wrap\ 159}#
                                                                  #{e\ 648}#
                                                                  #{w\ 649}#
                                                                  #{mod\ 651}#))
                                                          #{vals\ 641}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 654}#)
                                                          #{bindings\ 642}#)))))
                                            (if (memv #{type\ 646}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 655}#
                                                      (#{wrap\ 159}#
                                                        #{value\ 647}#
                                                        #{w\ 649}#
                                                        #{mod\ 651}#))
                                                    (#{label\ 656}#
                                                      (#{gen-label\ 136}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 633}#
                                                    #{id\ 655}#
                                                    #{label\ 656}#)
                                                  (#{parse\ 635}#
                                                    (cdr #{body\ 636}#)
                                                    (cons #{id\ 655}#
                                                          #{ids\ 637}#)
                                                    (cons #{label\ 656}#
                                                          #{labels\ 638}#)
                                                    #{var-ids\ 639}#
                                                    #{vars\ 640}#
                                                    #{vals\ 641}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 645}#
                                                                      (#{wrap\ 159}#
                                                                        #{e\ 648}#
                                                                        #{w\ 649}#
                                                                        #{mod\ 651}#)))
                                                          #{bindings\ 642}#))))
                                              (if (memv #{type\ 646}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 657}#)
                                                   ((lambda (#{tmp\ 658}#)
                                                      (if #{tmp\ 658}#
                                                        (apply (lambda (#{_\ 659}#
                                                                        #{e1\ 660}#)
                                                                 (#{parse\ 635}#
                                                                   (letrec ((#{f\ 661}# (lambda (#{forms\ 662}#)
                                                                                          (if (null? #{forms\ 662}#)
                                                                                            (cdr #{body\ 636}#)
                                                                                            (cons (cons #{er\ 645}#
                                                                                                        (#{wrap\ 159}#
                                                                                                          (car #{forms\ 662}#)
                                                                                                          #{w\ 649}#
                                                                                                          #{mod\ 651}#))
                                                                                                  (#{f\ 661}# (cdr #{forms\ 662}#)))))))
                                                                     (#{f\ 661}# #{e1\ 660}#))
                                                                   #{ids\ 637}#
                                                                   #{labels\ 638}#
                                                                   #{var-ids\ 639}#
                                                                   #{vars\ 640}#
                                                                   #{vals\ 641}#
                                                                   #{bindings\ 642}#))
                                                               #{tmp\ 658}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 657}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 657}#
                                                      '(any . each-any))))
                                                 #{e\ 648}#)
                                                (if (memv #{type\ 646}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 172}#
                                                    #{value\ 647}#
                                                    #{e\ 648}#
                                                    #{er\ 645}#
                                                    #{w\ 649}#
                                                    #{s\ 650}#
                                                    #{mod\ 651}#
                                                    (lambda (#{forms\ 664}#
                                                             #{er\ 665}#
                                                             #{w\ 666}#
                                                             #{s\ 667}#
                                                             #{mod\ 668}#)
                                                      (#{parse\ 635}#
                                                        (letrec ((#{f\ 669}# (lambda (#{forms\ 670}#)
                                                                               (if (null? #{forms\ 670}#)
                                                                                 (cdr #{body\ 636}#)
                                                                                 (cons (cons #{er\ 665}#
                                                                                             (#{wrap\ 159}#
                                                                                               (car #{forms\ 670}#)
                                                                                               #{w\ 666}#
                                                                                               #{mod\ 668}#))
                                                                                       (#{f\ 669}# (cdr #{forms\ 670}#)))))))
                                                          (#{f\ 669}# #{forms\ 664}#))
                                                        #{ids\ 637}#
                                                        #{labels\ 638}#
                                                        #{var-ids\ 639}#
                                                        #{vars\ 640}#
                                                        #{vals\ 641}#
                                                        #{bindings\ 642}#)))
                                                  (if (null? #{ids\ 637}#)
                                                    (#{build-sequence\ 110}#
                                                      #f
                                                      (map (lambda (#{x\ 671}#)
                                                             (#{chi\ 167}#
                                                               (cdr #{x\ 671}#)
                                                               (car #{x\ 671}#)
                                                               '(())
                                                               #{mod\ 651}#))
                                                           (cons (cons #{er\ 645}#
                                                                       (#{source-wrap\ 160}#
                                                                         #{e\ 648}#
                                                                         #{w\ 649}#
                                                                         #{s\ 650}#
                                                                         #{mod\ 651}#))
                                                                 (cdr #{body\ 636}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 156}#
                                                                 #{ids\ 637}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 628}#))
                                                      (letrec ((#{loop\ 672}#
                                                                 (lambda (#{bs\ 673}#
                                                                          #{er-cache\ 674}#
                                                                          #{r-cache\ 675}#)
                                                                   (if (not (null? #{bs\ 673}#))
                                                                     (let ((#{b\ 676}# (car #{bs\ 673}#)))
                                                                       (if (eq? (car #{b\ 676}#)
                                                                                'macro)
                                                                         (let ((#{er\ 677}#
                                                                                 (cadr #{b\ 676}#)))
                                                                           (let ((#{r-cache\ 678}#
                                                                                   (if (eq? #{er\ 677}#
                                                                                            #{er-cache\ 674}#)
                                                                                     #{r-cache\ 675}#
                                                                                     (#{macros-only-env\ 127}#
                                                                                       #{er\ 677}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 676}#
                                                                                 (#{eval-local-transformer\ 173}#
                                                                                   (#{chi\ 167}#
                                                                                     (cddr #{b\ 676}#)
                                                                                     #{r-cache\ 678}#
                                                                                     '(())
                                                                                     #{mod\ 651}#)
                                                                                   #{mod\ 651}#))
                                                                               (#{loop\ 672}#
                                                                                 (cdr #{bs\ 673}#)
                                                                                 #{er\ 677}#
                                                                                 #{r-cache\ 678}#))))
                                                                         (#{loop\ 672}#
                                                                           (cdr #{bs\ 673}#)
                                                                           #{er-cache\ 674}#
                                                                           #{r-cache\ 675}#)))))))
                                                        (#{loop\ 672}#
                                                          #{bindings\ 642}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 632}#
                                                        (#{extend-env\ 125}#
                                                          #{labels\ 638}#
                                                          #{bindings\ 642}#
                                                          (cdr #{r\ 632}#)))
                                                      (#{build-letrec\ 113}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 639}#)
                                                        #{vars\ 640}#
                                                        (map (lambda (#{x\ 679}#)
                                                               (#{chi\ 167}#
                                                                 (cdr #{x\ 679}#)
                                                                 (car #{x\ 679}#)
                                                                 '(())
                                                                 #{mod\ 651}#))
                                                             #{vals\ 641}#)
                                                        (#{build-sequence\ 110}#
                                                          #f
                                                          (map (lambda (#{x\ 680}#)
                                                                 (#{chi\ 167}#
                                                                   (cdr #{x\ 680}#)
                                                                   (car #{x\ 680}#)
                                                                   '(())
                                                                   #{mod\ 651}#))
                                                               (cons (cons #{er\ 645}#
                                                                           (#{source-wrap\ 160}#
                                                                             #{e\ 648}#
                                                                             #{w\ 649}#
                                                                             #{s\ 650}#
                                                                             #{mod\ 651}#))
                                                                     (cdr #{body\ 636}#))))))))))))))))))
                       (#{parse\ 635}#
                         (map (lambda (#{x\ 643}#)
                                (cons #{r\ 632}#
                                      (#{wrap\ 159}#
                                        #{x\ 643}#
                                        #{w\ 634}#
                                        #{mod\ 631}#)))
                              #{body\ 627}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 170}#
             (lambda (#{p\ 681}#
                      #{e\ 682}#
                      #{r\ 683}#
                      #{w\ 684}#
                      #{rib\ 685}#
                      #{mod\ 686}#)
               (letrec ((#{rebuild-macro-output\ 687}#
                          (lambda (#{x\ 688}# #{m\ 689}#)
                            (if (pair? #{x\ 688}#)
                              (cons (#{rebuild-macro-output\ 687}#
                                      (car #{x\ 688}#)
                                      #{m\ 689}#)
                                    (#{rebuild-macro-output\ 687}#
                                      (cdr #{x\ 688}#)
                                      #{m\ 689}#))
                              (if (#{syntax-object?\ 115}# #{x\ 688}#)
                                (let ((#{w\ 690}# (#{syntax-object-wrap\ 117}#
                                                    #{x\ 688}#)))
                                  (let ((#{ms\ 691}#
                                          (#{wrap-marks\ 134}# #{w\ 690}#))
                                        (#{s\ 692}# (#{wrap-subst\ 135}#
                                                      #{w\ 690}#)))
                                    (if (if (pair? #{ms\ 691}#)
                                          (eq? (car #{ms\ 691}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 688}#)
                                        (#{make-wrap\ 133}#
                                          (cdr #{ms\ 691}#)
                                          (if #{rib\ 685}#
                                            (cons #{rib\ 685}#
                                                  (cdr #{s\ 692}#))
                                            (cdr #{s\ 692}#)))
                                        (#{syntax-object-module\ 118}#
                                          #{x\ 688}#))
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 688}#)
                                        (#{make-wrap\ 133}#
                                          (cons #{m\ 689}# #{ms\ 691}#)
                                          (if #{rib\ 685}#
                                            (cons #{rib\ 685}#
                                                  (cons 'shift
                                                        #{s\ 692}#))
                                            (cons (quote shift) #{s\ 692}#)))
                                        (let ((#{pmod\ 693}#
                                                (procedure-module #{p\ 681}#)))
                                          (if #{pmod\ 693}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 693}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 688}#)
                                  (let ((#{n\ 694}# (vector-length
                                                      #{x\ 688}#)))
                                    (let ((#{v\ 695}# (make-vector
                                                        #{n\ 694}#)))
                                      (letrec ((#{loop\ 696}#
                                                 (lambda (#{i\ 697}#)
                                                   (if (#{fx=\ 88}#
                                                         #{i\ 697}#
                                                         #{n\ 694}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 695}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 695}#
                                                         #{i\ 697}#
                                                         (#{rebuild-macro-output\ 687}#
                                                           (vector-ref
                                                             #{x\ 688}#
                                                             #{i\ 697}#)
                                                           #{m\ 689}#))
                                                       (#{loop\ 696}#
                                                         (#{fx+\ 86}#
                                                           #{i\ 697}#
                                                           1)))))))
                                        (#{loop\ 696}# 0))))
                                  (if (symbol? #{x\ 688}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 160}#
                                        #{e\ 682}#
                                        #{w\ 684}#
                                        (#{wrap-subst\ 135}# #{w\ 684}#)
                                        #{mod\ 686}#)
                                      #{x\ 688}#)
                                    #{x\ 688}#)))))))
                 (#{rebuild-macro-output\ 687}#
                   (#{p\ 681}# (#{wrap\ 159}#
                                 #{e\ 682}#
                                 (#{anti-mark\ 146}# #{w\ 684}#)
                                 #{mod\ 686}#))
                   (string #\m)))))
           (#{chi-application\ 169}#
             (lambda (#{x\ 698}#
                      #{e\ 699}#
                      #{r\ 700}#
                      #{w\ 701}#
                      #{s\ 702}#
                      #{mod\ 703}#)
               ((lambda (#{tmp\ 704}#)
                  ((lambda (#{tmp\ 705}#)
                     (if #{tmp\ 705}#
                       (apply (lambda (#{e0\ 706}# #{e1\ 707}#)
                                (#{build-application\ 96}#
                                  #{s\ 702}#
                                  #{x\ 698}#
                                  (map (lambda (#{e\ 708}#)
                                         (#{chi\ 167}#
                                           #{e\ 708}#
                                           #{r\ 700}#
                                           #{w\ 701}#
                                           #{mod\ 703}#))
                                       #{e1\ 707}#)))
                              #{tmp\ 705}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 704}#)))
                   ($sc-dispatch
                     #{tmp\ 704}#
                     '(any . each-any))))
                #{e\ 699}#)))
           (#{chi-expr\ 168}#
             (lambda (#{type\ 710}#
                      #{value\ 711}#
                      #{e\ 712}#
                      #{r\ 713}#
                      #{w\ 714}#
                      #{s\ 715}#
                      #{mod\ 716}#)
               (if (memv #{type\ 710}# (quote (lexical)))
                 (#{build-lexical-reference\ 98}#
                   'value
                   #{s\ 715}#
                   #{e\ 712}#
                   #{value\ 711}#)
                 (if (memv #{type\ 710}# (quote (core core-form)))
                   (#{value\ 711}#
                     #{e\ 712}#
                     #{r\ 713}#
                     #{w\ 714}#
                     #{s\ 715}#
                     #{mod\ 716}#)
                   (if (memv #{type\ 710}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 711}# #{e\ 712}#))
                       (lambda (#{id\ 717}# #{mod\ 718}#)
                         (#{build-global-reference\ 101}#
                           #{s\ 715}#
                           #{id\ 717}#
                           #{mod\ 718}#)))
                     (if (memv #{type\ 710}# (quote (lexical-call)))
                       (#{chi-application\ 169}#
                         (#{build-lexical-reference\ 98}#
                           'fun
                           (#{source-annotation\ 122}# (car #{e\ 712}#))
                           (car #{e\ 712}#)
                           #{value\ 711}#)
                         #{e\ 712}#
                         #{r\ 713}#
                         #{w\ 714}#
                         #{s\ 715}#
                         #{mod\ 716}#)
                       (if (memv #{type\ 710}# (quote (global-call)))
                         (#{chi-application\ 169}#
                           (#{build-global-reference\ 101}#
                             (#{source-annotation\ 122}# (car #{e\ 712}#))
                             (if (#{syntax-object?\ 115}# #{value\ 711}#)
                               (#{syntax-object-expression\ 116}#
                                 #{value\ 711}#)
                               #{value\ 711}#)
                             (if (#{syntax-object?\ 115}# #{value\ 711}#)
                               (#{syntax-object-module\ 118}# #{value\ 711}#)
                               #{mod\ 716}#))
                           #{e\ 712}#
                           #{r\ 713}#
                           #{w\ 714}#
                           #{s\ 715}#
                           #{mod\ 716}#)
                         (if (memv #{type\ 710}# (quote (constant)))
                           (#{build-data\ 109}#
                             #{s\ 715}#
                             (#{strip\ 180}#
                               (#{source-wrap\ 160}#
                                 #{e\ 712}#
                                 #{w\ 714}#
                                 #{s\ 715}#
                                 #{mod\ 716}#)
                               '(())))
                           (if (memv #{type\ 710}# (quote (global)))
                             (#{build-global-reference\ 101}#
                               #{s\ 715}#
                               #{value\ 711}#
                               #{mod\ 716}#)
                             (if (memv #{type\ 710}# (quote (call)))
                               (#{chi-application\ 169}#
                                 (#{chi\ 167}#
                                   (car #{e\ 712}#)
                                   #{r\ 713}#
                                   #{w\ 714}#
                                   #{mod\ 716}#)
                                 #{e\ 712}#
                                 #{r\ 713}#
                                 #{w\ 714}#
                                 #{s\ 715}#
                                 #{mod\ 716}#)
                               (if (memv #{type\ 710}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 719}#)
                                    ((lambda (#{tmp\ 720}#)
                                       (if #{tmp\ 720}#
                                         (apply (lambda (#{_\ 721}#
                                                         #{e1\ 722}#
                                                         #{e2\ 723}#)
                                                  (#{chi-sequence\ 161}#
                                                    (cons #{e1\ 722}#
                                                          #{e2\ 723}#)
                                                    #{r\ 713}#
                                                    #{w\ 714}#
                                                    #{s\ 715}#
                                                    #{mod\ 716}#))
                                                #{tmp\ 720}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 719}#)))
                                     ($sc-dispatch
                                       #{tmp\ 719}#
                                       '(any any . each-any))))
                                  #{e\ 712}#)
                                 (if (memv #{type\ 710}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 172}#
                                     #{value\ 711}#
                                     #{e\ 712}#
                                     #{r\ 713}#
                                     #{w\ 714}#
                                     #{s\ 715}#
                                     #{mod\ 716}#
                                     #{chi-sequence\ 161}#)
                                   (if (memv #{type\ 710}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 725}#)
                                        ((lambda (#{tmp\ 726}#)
                                           (if #{tmp\ 726}#
                                             (apply (lambda (#{_\ 727}#
                                                             #{x\ 728}#
                                                             #{e1\ 729}#
                                                             #{e2\ 730}#)
                                                      (let ((#{when-list\ 731}#
                                                              (#{chi-when-list\ 164}#
                                                                #{e\ 712}#
                                                                #{x\ 728}#
                                                                #{w\ 714}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 731}#)
                                                          (#{chi-sequence\ 161}#
                                                            (cons #{e1\ 729}#
                                                                  #{e2\ 730}#)
                                                            #{r\ 713}#
                                                            #{w\ 714}#
                                                            #{s\ 715}#
                                                            #{mod\ 716}#)
                                                          (#{chi-void\ 174}#))))
                                                    #{tmp\ 726}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 725}#)))
                                         ($sc-dispatch
                                           #{tmp\ 725}#
                                           '(any each-any any . each-any))))
                                      #{e\ 712}#)
                                     (if (memv #{type\ 710}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 712}#
                                         (#{wrap\ 159}#
                                           #{value\ 711}#
                                           #{w\ 714}#
                                           #{mod\ 716}#))
                                       (if (memv #{type\ 710}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 160}#
                                             #{e\ 712}#
                                             #{w\ 714}#
                                             #{s\ 715}#
                                             #{mod\ 716}#))
                                         (if (memv #{type\ 710}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 160}#
                                               #{e\ 712}#
                                               #{w\ 714}#
                                               #{s\ 715}#
                                               #{mod\ 716}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 160}#
                                               #{e\ 712}#
                                               #{w\ 714}#
                                               #{s\ 715}#
                                               #{mod\ 716}#))))))))))))))))))
           (#{chi\ 167}#
             (lambda (#{e\ 734}# #{r\ 735}# #{w\ 736}# #{mod\ 737}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 734}#
                     #{r\ 735}#
                     #{w\ 736}#
                     (#{source-annotation\ 122}# #{e\ 734}#)
                     #f
                     #{mod\ 737}#
                     #f))
                 (lambda (#{type\ 738}#
                          #{value\ 739}#
                          #{e\ 740}#
                          #{w\ 741}#
                          #{s\ 742}#
                          #{mod\ 743}#)
                   (#{chi-expr\ 168}#
                     #{type\ 738}#
                     #{value\ 739}#
                     #{e\ 740}#
                     #{r\ 735}#
                     #{w\ 741}#
                     #{s\ 742}#
                     #{mod\ 743}#)))))
           (#{chi-top\ 166}#
             (lambda (#{e\ 744}#
                      #{r\ 745}#
                      #{w\ 746}#
                      #{m\ 747}#
                      #{esew\ 748}#
                      #{mod\ 749}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 744}#
                     #{r\ 745}#
                     #{w\ 746}#
                     (#{source-annotation\ 122}# #{e\ 744}#)
                     #f
                     #{mod\ 749}#
                     #f))
                 (lambda (#{type\ 757}#
                          #{value\ 758}#
                          #{e\ 759}#
                          #{w\ 760}#
                          #{s\ 761}#
                          #{mod\ 762}#)
                   (if (memv #{type\ 757}# (quote (begin-form)))
                     ((lambda (#{tmp\ 763}#)
                        ((lambda (#{tmp\ 764}#)
                           (if #{tmp\ 764}#
                             (apply (lambda (#{_\ 765}#) (#{chi-void\ 174}#))
                                    #{tmp\ 764}#)
                             ((lambda (#{tmp\ 766}#)
                                (if #{tmp\ 766}#
                                  (apply (lambda (#{_\ 767}#
                                                  #{e1\ 768}#
                                                  #{e2\ 769}#)
                                           (#{chi-top-sequence\ 162}#
                                             (cons #{e1\ 768}# #{e2\ 769}#)
                                             #{r\ 745}#
                                             #{w\ 760}#
                                             #{s\ 761}#
                                             #{m\ 747}#
                                             #{esew\ 748}#
                                             #{mod\ 762}#))
                                         #{tmp\ 766}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 763}#)))
                              ($sc-dispatch
                                #{tmp\ 763}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 763}# (quote (any)))))
                      #{e\ 759}#)
                     (if (memv #{type\ 757}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 172}#
                         #{value\ 758}#
                         #{e\ 759}#
                         #{r\ 745}#
                         #{w\ 760}#
                         #{s\ 761}#
                         #{mod\ 762}#
                         (lambda (#{body\ 771}#
                                  #{r\ 772}#
                                  #{w\ 773}#
                                  #{s\ 774}#
                                  #{mod\ 775}#)
                           (#{chi-top-sequence\ 162}#
                             #{body\ 771}#
                             #{r\ 772}#
                             #{w\ 773}#
                             #{s\ 774}#
                             #{m\ 747}#
                             #{esew\ 748}#
                             #{mod\ 775}#)))
                       (if (memv #{type\ 757}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 776}#)
                            ((lambda (#{tmp\ 777}#)
                               (if #{tmp\ 777}#
                                 (apply (lambda (#{_\ 778}#
                                                 #{x\ 779}#
                                                 #{e1\ 780}#
                                                 #{e2\ 781}#)
                                          (let ((#{when-list\ 782}#
                                                  (#{chi-when-list\ 164}#
                                                    #{e\ 759}#
                                                    #{x\ 779}#
                                                    #{w\ 760}#))
                                                (#{body\ 783}#
                                                  (cons #{e1\ 780}#
                                                        #{e2\ 781}#)))
                                            (if (eq? #{m\ 747}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 782}#)
                                                (#{chi-top-sequence\ 162}#
                                                  #{body\ 783}#
                                                  #{r\ 745}#
                                                  #{w\ 760}#
                                                  #{s\ 761}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 762}#)
                                                (#{chi-void\ 174}#))
                                              (if (memq 'load
                                                        #{when-list\ 782}#)
                                                (if (let ((#{t\ 786}# (memq 'compile
                                                                            #{when-list\ 782}#)))
                                                      (if #{t\ 786}#
                                                        #{t\ 786}#
                                                        (if (eq? #{m\ 747}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 782}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 162}#
                                                    #{body\ 783}#
                                                    #{r\ 745}#
                                                    #{w\ 760}#
                                                    #{s\ 761}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 762}#)
                                                  (if (memq #{m\ 747}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 162}#
                                                      #{body\ 783}#
                                                      #{r\ 745}#
                                                      #{w\ 760}#
                                                      #{s\ 761}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 762}#)
                                                    (#{chi-void\ 174}#)))
                                                (if (let ((#{t\ 787}# (memq 'compile
                                                                            #{when-list\ 782}#)))
                                                      (if #{t\ 787}#
                                                        #{t\ 787}#
                                                        (if (eq? #{m\ 747}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 782}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 90}#
                                                      (#{chi-top-sequence\ 162}#
                                                        #{body\ 783}#
                                                        #{r\ 745}#
                                                        #{w\ 760}#
                                                        #{s\ 761}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 762}#)
                                                      #{mod\ 762}#)
                                                    (#{chi-void\ 174}#))
                                                  (#{chi-void\ 174}#))))))
                                        #{tmp\ 777}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 776}#)))
                             ($sc-dispatch
                               #{tmp\ 776}#
                               '(any each-any any . each-any))))
                          #{e\ 759}#)
                         (if (memv #{type\ 757}# (quote (define-syntax-form)))
                           (let ((#{n\ 788}# (#{id-var-name\ 153}#
                                               #{value\ 758}#
                                               #{w\ 760}#))
                                 (#{r\ 789}# (#{macros-only-env\ 127}#
                                               #{r\ 745}#)))
                             (if (memv #{m\ 747}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 748}#)
                                 (let ((#{e\ 790}# (#{chi-install-global\ 163}#
                                                     #{n\ 788}#
                                                     (#{chi\ 167}#
                                                       #{e\ 759}#
                                                       #{r\ 789}#
                                                       #{w\ 760}#
                                                       #{mod\ 762}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 790}#
                                       #{mod\ 762}#)
                                     (if (memq (quote load) #{esew\ 748}#)
                                       #{e\ 790}#
                                       (#{chi-void\ 174}#))))
                                 (if (memq (quote load) #{esew\ 748}#)
                                   (#{chi-install-global\ 163}#
                                     #{n\ 788}#
                                     (#{chi\ 167}#
                                       #{e\ 759}#
                                       #{r\ 789}#
                                       #{w\ 760}#
                                       #{mod\ 762}#))
                                   (#{chi-void\ 174}#)))
                               (if (memv #{m\ 747}# (quote (c&e)))
                                 (let ((#{e\ 791}# (#{chi-install-global\ 163}#
                                                     #{n\ 788}#
                                                     (#{chi\ 167}#
                                                       #{e\ 759}#
                                                       #{r\ 789}#
                                                       #{w\ 760}#
                                                       #{mod\ 762}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 791}#
                                       #{mod\ 762}#)
                                     #{e\ 791}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 748}#)
                                     (#{top-level-eval-hook\ 90}#
                                       (#{chi-install-global\ 163}#
                                         #{n\ 788}#
                                         (#{chi\ 167}#
                                           #{e\ 759}#
                                           #{r\ 789}#
                                           #{w\ 760}#
                                           #{mod\ 762}#))
                                       #{mod\ 762}#))
                                   (#{chi-void\ 174}#)))))
                           (if (memv #{type\ 757}# (quote (define-form)))
                             (let ((#{n\ 792}# (#{id-var-name\ 153}#
                                                 #{value\ 758}#
                                                 #{w\ 760}#)))
                               (let ((#{type\ 793}#
                                       (#{binding-type\ 123}#
                                         (#{lookup\ 128}#
                                           #{n\ 792}#
                                           #{r\ 745}#
                                           #{mod\ 762}#))))
                                 (if (memv #{type\ 793}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 792}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 794}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 792}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 792}#
                                           (if (variable? #{old\ 794}#)
                                             (variable-ref #{old\ 794}#)
                                             #f))))
                                     (let ((#{x\ 795}# (#{build-global-definition\ 104}#
                                                         #{s\ 761}#
                                                         #{n\ 792}#
                                                         (#{chi\ 167}#
                                                           #{e\ 759}#
                                                           #{r\ 745}#
                                                           #{w\ 760}#
                                                           #{mod\ 762}#))))
                                       (begin
                                         (if (eq? #{m\ 747}# (quote c&e))
                                           (#{top-level-eval-hook\ 90}#
                                             #{x\ 795}#
                                             #{mod\ 762}#))
                                         #{x\ 795}#)))
                                   (if (memv #{type\ 793}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 759}#
                                       (#{wrap\ 159}#
                                         #{value\ 758}#
                                         #{w\ 760}#
                                         #{mod\ 762}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 759}#
                                       (#{wrap\ 159}#
                                         #{value\ 758}#
                                         #{w\ 760}#
                                         #{mod\ 762}#))))))
                             (let ((#{x\ 796}# (#{chi-expr\ 168}#
                                                 #{type\ 757}#
                                                 #{value\ 758}#
                                                 #{e\ 759}#
                                                 #{r\ 745}#
                                                 #{w\ 760}#
                                                 #{s\ 761}#
                                                 #{mod\ 762}#)))
                               (begin
                                 (if (eq? #{m\ 747}# (quote c&e))
                                   (#{top-level-eval-hook\ 90}#
                                     #{x\ 796}#
                                     #{mod\ 762}#))
                                 #{x\ 796}#)))))))))))
           (#{syntax-type\ 165}#
             (lambda (#{e\ 797}#
                      #{r\ 798}#
                      #{w\ 799}#
                      #{s\ 800}#
                      #{rib\ 801}#
                      #{mod\ 802}#
                      #{for-car?\ 803}#)
               (if (symbol? #{e\ 797}#)
                 (let ((#{n\ 804}# (#{id-var-name\ 153}#
                                     #{e\ 797}#
                                     #{w\ 799}#)))
                   (let ((#{b\ 805}# (#{lookup\ 128}#
                                       #{n\ 804}#
                                       #{r\ 798}#
                                       #{mod\ 802}#)))
                     (let ((#{type\ 806}#
                             (#{binding-type\ 123}# #{b\ 805}#)))
                       (if (memv #{type\ 806}# (quote (lexical)))
                         (values
                           #{type\ 806}#
                           (#{binding-value\ 124}# #{b\ 805}#)
                           #{e\ 797}#
                           #{w\ 799}#
                           #{s\ 800}#
                           #{mod\ 802}#)
                         (if (memv #{type\ 806}# (quote (global)))
                           (values
                             #{type\ 806}#
                             #{n\ 804}#
                             #{e\ 797}#
                             #{w\ 799}#
                             #{s\ 800}#
                             #{mod\ 802}#)
                           (if (memv #{type\ 806}# (quote (macro)))
                             (if #{for-car?\ 803}#
                               (values
                                 #{type\ 806}#
                                 (#{binding-value\ 124}# #{b\ 805}#)
                                 #{e\ 797}#
                                 #{w\ 799}#
                                 #{s\ 800}#
                                 #{mod\ 802}#)
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   (#{binding-value\ 124}# #{b\ 805}#)
                                   #{e\ 797}#
                                   #{r\ 798}#
                                   #{w\ 799}#
                                   #{rib\ 801}#
                                   #{mod\ 802}#)
                                 #{r\ 798}#
                                 '(())
                                 #{s\ 800}#
                                 #{rib\ 801}#
                                 #{mod\ 802}#
                                 #f))
                             (values
                               #{type\ 806}#
                               (#{binding-value\ 124}# #{b\ 805}#)
                               #{e\ 797}#
                               #{w\ 799}#
                               #{s\ 800}#
                               #{mod\ 802}#)))))))
                 (if (pair? #{e\ 797}#)
                   (let ((#{first\ 807}# (car #{e\ 797}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 165}#
                           #{first\ 807}#
                           #{r\ 798}#
                           #{w\ 799}#
                           #{s\ 800}#
                           #{rib\ 801}#
                           #{mod\ 802}#
                           #t))
                       (lambda (#{ftype\ 808}#
                                #{fval\ 809}#
                                #{fe\ 810}#
                                #{fw\ 811}#
                                #{fs\ 812}#
                                #{fmod\ 813}#)
                         (if (memv #{ftype\ 808}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 809}#
                             #{e\ 797}#
                             #{w\ 799}#
                             #{s\ 800}#
                             #{mod\ 802}#)
                           (if (memv #{ftype\ 808}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 114}#
                                 #{fval\ 809}#
                                 #{w\ 799}#
                                 #{fmod\ 813}#)
                               #{e\ 797}#
                               #{w\ 799}#
                               #{s\ 800}#
                               #{mod\ 802}#)
                             (if (memv #{ftype\ 808}# (quote (macro)))
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   #{fval\ 809}#
                                   #{e\ 797}#
                                   #{r\ 798}#
                                   #{w\ 799}#
                                   #{rib\ 801}#
                                   #{mod\ 802}#)
                                 #{r\ 798}#
                                 '(())
                                 #{s\ 800}#
                                 #{rib\ 801}#
                                 #{mod\ 802}#
                                 #{for-car?\ 803}#)
                               (if (memv #{ftype\ 808}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 809}# #{e\ 797}#))
                                   (lambda (#{sym\ 814}# #{mod\ 815}#)
                                     (#{syntax-type\ 165}#
                                       #{sym\ 814}#
                                       #{r\ 798}#
                                       #{w\ 799}#
                                       #{s\ 800}#
                                       #{rib\ 801}#
                                       #{mod\ 815}#
                                       #{for-car?\ 803}#)))
                                 (if (memv #{ftype\ 808}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 809}#
                                     #{e\ 797}#
                                     #{w\ 799}#
                                     #{s\ 800}#
                                     #{mod\ 802}#)
                                   (if (memv #{ftype\ 808}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 809}#
                                       #{e\ 797}#
                                       #{w\ 799}#
                                       #{s\ 800}#
                                       #{mod\ 802}#)
                                     (if (memv #{ftype\ 808}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 797}#
                                         #{w\ 799}#
                                         #{s\ 800}#
                                         #{mod\ 802}#)
                                       (if (memv #{ftype\ 808}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 797}#
                                           #{w\ 799}#
                                           #{s\ 800}#
                                           #{mod\ 802}#)
                                         (if (memv #{ftype\ 808}#
                                                   '(define))
                                           ((lambda (#{tmp\ 816}#)
                                              ((lambda (#{tmp\ 817}#)
                                                 (if (if #{tmp\ 817}#
                                                       (apply (lambda (#{_\ 818}#
                                                                       #{name\ 819}#
                                                                       #{val\ 820}#)
                                                                (#{id?\ 131}#
                                                                  #{name\ 819}#))
                                                              #{tmp\ 817}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 821}#
                                                                   #{name\ 822}#
                                                                   #{val\ 823}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 822}#
                                                              #{val\ 823}#
                                                              #{w\ 799}#
                                                              #{s\ 800}#
                                                              #{mod\ 802}#))
                                                          #{tmp\ 817}#)
                                                   ((lambda (#{tmp\ 824}#)
                                                      (if (if #{tmp\ 824}#
                                                            (apply (lambda (#{_\ 825}#
                                                                            #{name\ 826}#
                                                                            #{args\ 827}#
                                                                            #{e1\ 828}#
                                                                            #{e2\ 829}#)
                                                                     (if (#{id?\ 131}#
                                                                           #{name\ 826}#)
                                                                       (#{valid-bound-ids?\ 156}#
                                                                         (#{lambda-var-list\ 182}#
                                                                           #{args\ 827}#))
                                                                       #f))
                                                                   #{tmp\ 824}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 830}#
                                                                        #{name\ 831}#
                                                                        #{args\ 832}#
                                                                        #{e1\ 833}#
                                                                        #{e2\ 834}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 159}#
                                                                     #{name\ 831}#
                                                                     #{w\ 799}#
                                                                     #{mod\ 802}#)
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
                                                                             (cons #{args\ 832}#
                                                                                   (cons #{e1\ 833}#
                                                                                         #{e2\ 834}#))
                                                                             #{w\ 799}#
                                                                             #{mod\ 802}#))
                                                                     #{s\ 800}#)
                                                                   '(())
                                                                   #{s\ 800}#
                                                                   #{mod\ 802}#))
                                                               #{tmp\ 824}#)
                                                        ((lambda (#{tmp\ 836}#)
                                                           (if (if #{tmp\ 836}#
                                                                 (apply (lambda (#{_\ 837}#
                                                                                 #{name\ 838}#)
                                                                          (#{id?\ 131}#
                                                                            #{name\ 838}#))
                                                                        #{tmp\ 836}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 839}#
                                                                             #{name\ 840}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 159}#
                                                                          #{name\ 840}#
                                                                          #{w\ 799}#
                                                                          #{mod\ 802}#)
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
                                                                        #{s\ 800}#
                                                                        #{mod\ 802}#))
                                                                    #{tmp\ 836}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 816}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 816}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 816}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 816}#
                                                 '(any any any))))
                                            #{e\ 797}#)
                                           (if (memv #{ftype\ 808}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 841}#)
                                                ((lambda (#{tmp\ 842}#)
                                                   (if (if #{tmp\ 842}#
                                                         (apply (lambda (#{_\ 843}#
                                                                         #{name\ 844}#
                                                                         #{val\ 845}#)
                                                                  (#{id?\ 131}#
                                                                    #{name\ 844}#))
                                                                #{tmp\ 842}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 846}#
                                                                     #{name\ 847}#
                                                                     #{val\ 848}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 847}#
                                                                #{val\ 848}#
                                                                #{w\ 799}#
                                                                #{s\ 800}#
                                                                #{mod\ 802}#))
                                                            #{tmp\ 842}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 841}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 841}#
                                                   '(any any any))))
                                              #{e\ 797}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 797}#
                                               #{w\ 799}#
                                               #{s\ 800}#
                                               #{mod\ 802}#))))))))))))))
                   (if (#{syntax-object?\ 115}# #{e\ 797}#)
                     (#{syntax-type\ 165}#
                       (#{syntax-object-expression\ 116}# #{e\ 797}#)
                       #{r\ 798}#
                       (#{join-wraps\ 150}#
                         #{w\ 799}#
                         (#{syntax-object-wrap\ 117}# #{e\ 797}#))
                       #{s\ 800}#
                       #{rib\ 801}#
                       (let ((#{t\ 849}# (#{syntax-object-module\ 118}#
                                           #{e\ 797}#)))
                         (if #{t\ 849}# #{t\ 849}# #{mod\ 802}#))
                       #{for-car?\ 803}#)
                     (if (self-evaluating? #{e\ 797}#)
                       (values
                         'constant
                         #f
                         #{e\ 797}#
                         #{w\ 799}#
                         #{s\ 800}#
                         #{mod\ 802}#)
                       (values
                         'other
                         #f
                         #{e\ 797}#
                         #{w\ 799}#
                         #{s\ 800}#
                         #{mod\ 802}#)))))))
           (#{chi-when-list\ 164}#
             (lambda (#{e\ 850}# #{when-list\ 851}# #{w\ 852}#)
               (letrec ((#{f\ 853}# (lambda (#{when-list\ 854}#
                                             #{situations\ 855}#)
                                      (if (null? #{when-list\ 854}#)
                                        #{situations\ 855}#
                                        (#{f\ 853}# (cdr #{when-list\ 854}#)
                                                    (cons (let ((#{x\ 856}# (car #{when-list\ 854}#)))
                                                            (if (#{free-id=?\ 154}#
                                                                  #{x\ 856}#
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
                                                                    #{x\ 856}#
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
                                                                      #{x\ 856}#
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
                                                                    #{e\ 850}#
                                                                    (#{wrap\ 159}#
                                                                      #{x\ 856}#
                                                                      #{w\ 852}#
                                                                      #f))))))
                                                          #{situations\ 855}#))))))
                 (#{f\ 853}# #{when-list\ 851}# (quote ())))))
           (#{chi-install-global\ 163}#
             (lambda (#{name\ 857}# #{e\ 858}#)
               (#{build-global-definition\ 104}#
                 #f
                 #{name\ 857}#
                 (if (let ((#{v\ 859}# (module-variable
                                         (current-module)
                                         #{name\ 857}#)))
                       (if #{v\ 859}#
                         (if (variable-bound? #{v\ 859}#)
                           (if (macro? (variable-ref #{v\ 859}#))
                             (not (eq? (macro-type (variable-ref #{v\ 859}#))
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
                                   (#{build-data\ 109}# #f #{name\ 857}#)))
                           (#{build-data\ 109}# #f (quote macro))
                           #{e\ 858}#))
                   (#{build-application\ 96}#
                     #f
                     (#{build-primref\ 108}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 109}# #f (quote macro))
                           #{e\ 858}#))))))
           (#{chi-top-sequence\ 162}#
             (lambda (#{body\ 860}#
                      #{r\ 861}#
                      #{w\ 862}#
                      #{s\ 863}#
                      #{m\ 864}#
                      #{esew\ 865}#
                      #{mod\ 866}#)
               (#{build-sequence\ 110}#
                 #{s\ 863}#
                 (letrec ((#{dobody\ 867}#
                            (lambda (#{body\ 868}#
                                     #{r\ 869}#
                                     #{w\ 870}#
                                     #{m\ 871}#
                                     #{esew\ 872}#
                                     #{mod\ 873}#)
                              (if (null? #{body\ 868}#)
                                '()
                                (let ((#{first\ 874}#
                                        (#{chi-top\ 166}#
                                          (car #{body\ 868}#)
                                          #{r\ 869}#
                                          #{w\ 870}#
                                          #{m\ 871}#
                                          #{esew\ 872}#
                                          #{mod\ 873}#)))
                                  (cons #{first\ 874}#
                                        (#{dobody\ 867}#
                                          (cdr #{body\ 868}#)
                                          #{r\ 869}#
                                          #{w\ 870}#
                                          #{m\ 871}#
                                          #{esew\ 872}#
                                          #{mod\ 873}#)))))))
                   (#{dobody\ 867}#
                     #{body\ 860}#
                     #{r\ 861}#
                     #{w\ 862}#
                     #{m\ 864}#
                     #{esew\ 865}#
                     #{mod\ 866}#)))))
           (#{chi-sequence\ 161}#
             (lambda (#{body\ 875}#
                      #{r\ 876}#
                      #{w\ 877}#
                      #{s\ 878}#
                      #{mod\ 879}#)
               (#{build-sequence\ 110}#
                 #{s\ 878}#
                 (letrec ((#{dobody\ 880}#
                            (lambda (#{body\ 881}#
                                     #{r\ 882}#
                                     #{w\ 883}#
                                     #{mod\ 884}#)
                              (if (null? #{body\ 881}#)
                                '()
                                (let ((#{first\ 885}#
                                        (#{chi\ 167}#
                                          (car #{body\ 881}#)
                                          #{r\ 882}#
                                          #{w\ 883}#
                                          #{mod\ 884}#)))
                                  (cons #{first\ 885}#
                                        (#{dobody\ 880}#
                                          (cdr #{body\ 881}#)
                                          #{r\ 882}#
                                          #{w\ 883}#
                                          #{mod\ 884}#)))))))
                   (#{dobody\ 880}#
                     #{body\ 875}#
                     #{r\ 876}#
                     #{w\ 877}#
                     #{mod\ 879}#)))))
           (#{source-wrap\ 160}#
             (lambda (#{x\ 886}#
                      #{w\ 887}#
                      #{s\ 888}#
                      #{defmod\ 889}#)
               (#{wrap\ 159}#
                 (#{decorate-source\ 94}# #{x\ 886}# #{s\ 888}#)
                 #{w\ 887}#
                 #{defmod\ 889}#)))
           (#{wrap\ 159}#
             (lambda (#{x\ 890}# #{w\ 891}# #{defmod\ 892}#)
               (if (if (null? (#{wrap-marks\ 134}# #{w\ 891}#))
                     (null? (#{wrap-subst\ 135}# #{w\ 891}#))
                     #f)
                 #{x\ 890}#
                 (if (#{syntax-object?\ 115}# #{x\ 890}#)
                   (#{make-syntax-object\ 114}#
                     (#{syntax-object-expression\ 116}# #{x\ 890}#)
                     (#{join-wraps\ 150}#
                       #{w\ 891}#
                       (#{syntax-object-wrap\ 117}# #{x\ 890}#))
                     (#{syntax-object-module\ 118}# #{x\ 890}#))
                   (if (null? #{x\ 890}#)
                     #{x\ 890}#
                     (#{make-syntax-object\ 114}#
                       #{x\ 890}#
                       #{w\ 891}#
                       #{defmod\ 892}#))))))
           (#{bound-id-member?\ 158}#
             (lambda (#{x\ 893}# #{list\ 894}#)
               (if (not (null? #{list\ 894}#))
                 (let ((#{t\ 895}# (#{bound-id=?\ 155}#
                                     #{x\ 893}#
                                     (car #{list\ 894}#))))
                   (if #{t\ 895}#
                     #{t\ 895}#
                     (#{bound-id-member?\ 158}#
                       #{x\ 893}#
                       (cdr #{list\ 894}#))))
                 #f)))
           (#{distinct-bound-ids?\ 157}#
             (lambda (#{ids\ 896}#)
               (letrec ((#{distinct?\ 897}#
                          (lambda (#{ids\ 898}#)
                            (let ((#{t\ 899}# (null? #{ids\ 898}#)))
                              (if #{t\ 899}#
                                #{t\ 899}#
                                (if (not (#{bound-id-member?\ 158}#
                                           (car #{ids\ 898}#)
                                           (cdr #{ids\ 898}#)))
                                  (#{distinct?\ 897}# (cdr #{ids\ 898}#))
                                  #f))))))
                 (#{distinct?\ 897}# #{ids\ 896}#))))
           (#{valid-bound-ids?\ 156}#
             (lambda (#{ids\ 900}#)
               (if (letrec ((#{all-ids?\ 901}#
                              (lambda (#{ids\ 902}#)
                                (let ((#{t\ 903}# (null? #{ids\ 902}#)))
                                  (if #{t\ 903}#
                                    #{t\ 903}#
                                    (if (#{id?\ 131}# (car #{ids\ 902}#))
                                      (#{all-ids?\ 901}# (cdr #{ids\ 902}#))
                                      #f))))))
                     (#{all-ids?\ 901}# #{ids\ 900}#))
                 (#{distinct-bound-ids?\ 157}# #{ids\ 900}#)
                 #f)))
           (#{bound-id=?\ 155}#
             (lambda (#{i\ 904}# #{j\ 905}#)
               (if (if (#{syntax-object?\ 115}# #{i\ 904}#)
                     (#{syntax-object?\ 115}# #{j\ 905}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 116}# #{i\ 904}#)
                          (#{syntax-object-expression\ 116}# #{j\ 905}#))
                   (#{same-marks?\ 152}#
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{i\ 904}#))
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{j\ 905}#)))
                   #f)
                 (eq? #{i\ 904}# #{j\ 905}#))))
           (#{free-id=?\ 154}#
             (lambda (#{i\ 906}# #{j\ 907}#)
               (if (eq? (let ((#{x\ 908}# #{i\ 906}#))
                          (if (#{syntax-object?\ 115}# #{x\ 908}#)
                            (#{syntax-object-expression\ 116}# #{x\ 908}#)
                            #{x\ 908}#))
                        (let ((#{x\ 909}# #{j\ 907}#))
                          (if (#{syntax-object?\ 115}# #{x\ 909}#)
                            (#{syntax-object-expression\ 116}# #{x\ 909}#)
                            #{x\ 909}#)))
                 (eq? (#{id-var-name\ 153}# #{i\ 906}# (quote (())))
                      (#{id-var-name\ 153}# #{j\ 907}# (quote (()))))
                 #f)))
           (#{id-var-name\ 153}#
             (lambda (#{id\ 910}# #{w\ 911}#)
               (letrec ((#{search-vector-rib\ 914}#
                          (lambda (#{sym\ 920}#
                                   #{subst\ 921}#
                                   #{marks\ 922}#
                                   #{symnames\ 923}#
                                   #{ribcage\ 924}#)
                            (let ((#{n\ 925}# (vector-length
                                                #{symnames\ 923}#)))
                              (letrec ((#{f\ 926}# (lambda (#{i\ 927}#)
                                                     (if (#{fx=\ 88}#
                                                           #{i\ 927}#
                                                           #{n\ 925}#)
                                                       (#{search\ 912}#
                                                         #{sym\ 920}#
                                                         (cdr #{subst\ 921}#)
                                                         #{marks\ 922}#)
                                                       (if (if (eq? (vector-ref
                                                                      #{symnames\ 923}#
                                                                      #{i\ 927}#)
                                                                    #{sym\ 920}#)
                                                             (#{same-marks?\ 152}#
                                                               #{marks\ 922}#
                                                               (vector-ref
                                                                 (#{ribcage-marks\ 141}#
                                                                   #{ribcage\ 924}#)
                                                                 #{i\ 927}#))
                                                             #f)
                                                         (values
                                                           (vector-ref
                                                             (#{ribcage-labels\ 142}#
                                                               #{ribcage\ 924}#)
                                                             #{i\ 927}#)
                                                           #{marks\ 922}#)
                                                         (#{f\ 926}# (#{fx+\ 86}#
                                                                       #{i\ 927}#
                                                                       1)))))))
                                (#{f\ 926}# 0)))))
                        (#{search-list-rib\ 913}#
                          (lambda (#{sym\ 928}#
                                   #{subst\ 929}#
                                   #{marks\ 930}#
                                   #{symnames\ 931}#
                                   #{ribcage\ 932}#)
                            (letrec ((#{f\ 933}# (lambda (#{symnames\ 934}#
                                                          #{i\ 935}#)
                                                   (if (null? #{symnames\ 934}#)
                                                     (#{search\ 912}#
                                                       #{sym\ 928}#
                                                       (cdr #{subst\ 929}#)
                                                       #{marks\ 930}#)
                                                     (if (if (eq? (car #{symnames\ 934}#)
                                                                  #{sym\ 928}#)
                                                           (#{same-marks?\ 152}#
                                                             #{marks\ 930}#
                                                             (list-ref
                                                               (#{ribcage-marks\ 141}#
                                                                 #{ribcage\ 932}#)
                                                               #{i\ 935}#))
                                                           #f)
                                                       (values
                                                         (list-ref
                                                           (#{ribcage-labels\ 142}#
                                                             #{ribcage\ 932}#)
                                                           #{i\ 935}#)
                                                         #{marks\ 930}#)
                                                       (#{f\ 933}# (cdr #{symnames\ 934}#)
                                                                   (#{fx+\ 86}#
                                                                     #{i\ 935}#
                                                                     1)))))))
                              (#{f\ 933}# #{symnames\ 931}# 0))))
                        (#{search\ 912}#
                          (lambda (#{sym\ 936}# #{subst\ 937}# #{marks\ 938}#)
                            (if (null? #{subst\ 937}#)
                              (values #f #{marks\ 938}#)
                              (let ((#{fst\ 939}# (car #{subst\ 937}#)))
                                (if (eq? #{fst\ 939}# (quote shift))
                                  (#{search\ 912}#
                                    #{sym\ 936}#
                                    (cdr #{subst\ 937}#)
                                    (cdr #{marks\ 938}#))
                                  (let ((#{symnames\ 940}#
                                          (#{ribcage-symnames\ 140}#
                                            #{fst\ 939}#)))
                                    (if (vector? #{symnames\ 940}#)
                                      (#{search-vector-rib\ 914}#
                                        #{sym\ 936}#
                                        #{subst\ 937}#
                                        #{marks\ 938}#
                                        #{symnames\ 940}#
                                        #{fst\ 939}#)
                                      (#{search-list-rib\ 913}#
                                        #{sym\ 936}#
                                        #{subst\ 937}#
                                        #{marks\ 938}#
                                        #{symnames\ 940}#
                                        #{fst\ 939}#)))))))))
                 (if (symbol? #{id\ 910}#)
                   (let ((#{t\ 941}# (call-with-values
                                       (lambda ()
                                         (#{search\ 912}#
                                           #{id\ 910}#
                                           (#{wrap-subst\ 135}# #{w\ 911}#)
                                           (#{wrap-marks\ 134}# #{w\ 911}#)))
                                       (lambda (#{x\ 942}# . #{ignore\ 943}#)
                                         #{x\ 942}#))))
                     (if #{t\ 941}# #{t\ 941}# #{id\ 910}#))
                   (if (#{syntax-object?\ 115}# #{id\ 910}#)
                     (let ((#{id\ 944}#
                             (#{syntax-object-expression\ 116}# #{id\ 910}#))
                           (#{w1\ 945}#
                             (#{syntax-object-wrap\ 117}# #{id\ 910}#)))
                       (let ((#{marks\ 946}#
                               (#{join-marks\ 151}#
                                 (#{wrap-marks\ 134}# #{w\ 911}#)
                                 (#{wrap-marks\ 134}# #{w1\ 945}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 912}#
                               #{id\ 944}#
                               (#{wrap-subst\ 135}# #{w\ 911}#)
                               #{marks\ 946}#))
                           (lambda (#{new-id\ 947}# #{marks\ 948}#)
                             (let ((#{t\ 949}# #{new-id\ 947}#))
                               (if #{t\ 949}#
                                 #{t\ 949}#
                                 (let ((#{t\ 950}# (call-with-values
                                                     (lambda ()
                                                       (#{search\ 912}#
                                                         #{id\ 944}#
                                                         (#{wrap-subst\ 135}#
                                                           #{w1\ 945}#)
                                                         #{marks\ 948}#))
                                                     (lambda (#{x\ 951}#
                                                              .
                                                              #{ignore\ 952}#)
                                                       #{x\ 951}#))))
                                   (if #{t\ 950}#
                                     #{t\ 950}#
                                     #{id\ 944}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 910}#))))))
           (#{same-marks?\ 152}#
             (lambda (#{x\ 953}# #{y\ 954}#)
               (let ((#{t\ 955}# (eq? #{x\ 953}# #{y\ 954}#)))
                 (if #{t\ 955}#
                   #{t\ 955}#
                   (if (not (null? #{x\ 953}#))
                     (if (not (null? #{y\ 954}#))
                       (if (eq? (car #{x\ 953}#) (car #{y\ 954}#))
                         (#{same-marks?\ 152}#
                           (cdr #{x\ 953}#)
                           (cdr #{y\ 954}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 151}#
             (lambda (#{m1\ 956}# #{m2\ 957}#)
               (#{smart-append\ 149}# #{m1\ 956}# #{m2\ 957}#)))
           (#{join-wraps\ 150}#
             (lambda (#{w1\ 958}# #{w2\ 959}#)
               (let ((#{m1\ 960}# (#{wrap-marks\ 134}# #{w1\ 958}#))
                     (#{s1\ 961}# (#{wrap-subst\ 135}# #{w1\ 958}#)))
                 (if (null? #{m1\ 960}#)
                   (if (null? #{s1\ 961}#)
                     #{w2\ 959}#
                     (#{make-wrap\ 133}#
                       (#{wrap-marks\ 134}# #{w2\ 959}#)
                       (#{smart-append\ 149}#
                         #{s1\ 961}#
                         (#{wrap-subst\ 135}# #{w2\ 959}#))))
                   (#{make-wrap\ 133}#
                     (#{smart-append\ 149}#
                       #{m1\ 960}#
                       (#{wrap-marks\ 134}# #{w2\ 959}#))
                     (#{smart-append\ 149}#
                       #{s1\ 961}#
                       (#{wrap-subst\ 135}# #{w2\ 959}#)))))))
           (#{smart-append\ 149}#
             (lambda (#{m1\ 962}# #{m2\ 963}#)
               (if (null? #{m2\ 963}#)
                 #{m1\ 962}#
                 (append #{m1\ 962}# #{m2\ 963}#))))
           (#{make-binding-wrap\ 148}#
             (lambda (#{ids\ 964}# #{labels\ 965}# #{w\ 966}#)
               (if (null? #{ids\ 964}#)
                 #{w\ 966}#
                 (#{make-wrap\ 133}#
                   (#{wrap-marks\ 134}# #{w\ 966}#)
                   (cons (let ((#{labelvec\ 967}#
                                 (list->vector #{labels\ 965}#)))
                           (let ((#{n\ 968}# (vector-length
                                               #{labelvec\ 967}#)))
                             (let ((#{symnamevec\ 969}#
                                     (make-vector #{n\ 968}#))
                                   (#{marksvec\ 970}#
                                     (make-vector #{n\ 968}#)))
                               (begin
                                 (letrec ((#{f\ 971}# (lambda (#{ids\ 972}#
                                                               #{i\ 973}#)
                                                        (if (not (null? #{ids\ 972}#))
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{id-sym-name&marks\ 132}#
                                                                (car #{ids\ 972}#)
                                                                #{w\ 966}#))
                                                            (lambda (#{symname\ 974}#
                                                                     #{marks\ 975}#)
                                                              (begin
                                                                (vector-set!
                                                                  #{symnamevec\ 969}#
                                                                  #{i\ 973}#
                                                                  #{symname\ 974}#)
                                                                (vector-set!
                                                                  #{marksvec\ 970}#
                                                                  #{i\ 973}#
                                                                  #{marks\ 975}#)
                                                                (#{f\ 971}# (cdr #{ids\ 972}#)
                                                                            (#{fx+\ 86}#
                                                                              #{i\ 973}#
                                                                              1)))))))))
                                   (#{f\ 971}# #{ids\ 964}# 0))
                                 (#{make-ribcage\ 138}#
                                   #{symnamevec\ 969}#
                                   #{marksvec\ 970}#
                                   #{labelvec\ 967}#)))))
                         (#{wrap-subst\ 135}# #{w\ 966}#))))))
           (#{extend-ribcage!\ 147}#
             (lambda (#{ribcage\ 976}# #{id\ 977}# #{label\ 978}#)
               (begin
                 (#{set-ribcage-symnames!\ 143}#
                   #{ribcage\ 976}#
                   (cons (#{syntax-object-expression\ 116}# #{id\ 977}#)
                         (#{ribcage-symnames\ 140}# #{ribcage\ 976}#)))
                 (#{set-ribcage-marks!\ 144}#
                   #{ribcage\ 976}#
                   (cons (#{wrap-marks\ 134}#
                           (#{syntax-object-wrap\ 117}# #{id\ 977}#))
                         (#{ribcage-marks\ 141}# #{ribcage\ 976}#)))
                 (#{set-ribcage-labels!\ 145}#
                   #{ribcage\ 976}#
                   (cons #{label\ 978}#
                         (#{ribcage-labels\ 142}# #{ribcage\ 976}#))))))
           (#{anti-mark\ 146}#
             (lambda (#{w\ 979}#)
               (#{make-wrap\ 133}#
                 (cons #f (#{wrap-marks\ 134}# #{w\ 979}#))
                 (cons 'shift
                       (#{wrap-subst\ 135}# #{w\ 979}#)))))
           (#{set-ribcage-labels!\ 145}#
             (lambda (#{x\ 980}# #{update\ 981}#)
               (vector-set! #{x\ 980}# 3 #{update\ 981}#)))
           (#{set-ribcage-marks!\ 144}#
             (lambda (#{x\ 982}# #{update\ 983}#)
               (vector-set! #{x\ 982}# 2 #{update\ 983}#)))
           (#{set-ribcage-symnames!\ 143}#
             (lambda (#{x\ 984}# #{update\ 985}#)
               (vector-set! #{x\ 984}# 1 #{update\ 985}#)))
           (#{ribcage-labels\ 142}#
             (lambda (#{x\ 986}#) (vector-ref #{x\ 986}# 3)))
           (#{ribcage-marks\ 141}#
             (lambda (#{x\ 987}#) (vector-ref #{x\ 987}# 2)))
           (#{ribcage-symnames\ 140}#
             (lambda (#{x\ 988}#) (vector-ref #{x\ 988}# 1)))
           (#{ribcage?\ 139}#
             (lambda (#{x\ 989}#)
               (if (vector? #{x\ 989}#)
                 (if (= (vector-length #{x\ 989}#) 4)
                   (eq? (vector-ref #{x\ 989}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 138}#
             (lambda (#{symnames\ 990}#
                      #{marks\ 991}#
                      #{labels\ 992}#)
               (vector
                 'ribcage
                 #{symnames\ 990}#
                 #{marks\ 991}#
                 #{labels\ 992}#)))
           (#{gen-labels\ 137}#
             (lambda (#{ls\ 993}#)
               (if (null? #{ls\ 993}#)
                 '()
                 (cons (#{gen-label\ 136}#)
                       (#{gen-labels\ 137}# (cdr #{ls\ 993}#))))))
           (#{gen-label\ 136}# (lambda () (string #\i)))
           (#{wrap-subst\ 135}# cdr)
           (#{wrap-marks\ 134}# car)
           (#{make-wrap\ 133}# cons)
           (#{id-sym-name&marks\ 132}#
             (lambda (#{x\ 994}# #{w\ 995}#)
               (if (#{syntax-object?\ 115}# #{x\ 994}#)
                 (values
                   (#{syntax-object-expression\ 116}# #{x\ 994}#)
                   (#{join-marks\ 151}#
                     (#{wrap-marks\ 134}# #{w\ 995}#)
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{x\ 994}#))))
                 (values
                   #{x\ 994}#
                   (#{wrap-marks\ 134}# #{w\ 995}#)))))
           (#{id?\ 131}#
             (lambda (#{x\ 996}#)
               (if (symbol? #{x\ 996}#)
                 #t
                 (if (#{syntax-object?\ 115}# #{x\ 996}#)
                   (symbol?
                     (#{syntax-object-expression\ 116}# #{x\ 996}#))
                   #f))))
           (#{nonsymbol-id?\ 130}#
             (lambda (#{x\ 997}#)
               (if (#{syntax-object?\ 115}# #{x\ 997}#)
                 (symbol?
                   (#{syntax-object-expression\ 116}# #{x\ 997}#))
                 #f)))
           (#{global-extend\ 129}#
             (lambda (#{type\ 998}# #{sym\ 999}# #{val\ 1000}#)
               (#{put-global-definition-hook\ 92}#
                 #{sym\ 999}#
                 #{type\ 998}#
                 #{val\ 1000}#)))
           (#{lookup\ 128}#
             (lambda (#{x\ 1001}# #{r\ 1002}# #{mod\ 1003}#)
               (let ((#{t\ 1004}# (assq #{x\ 1001}# #{r\ 1002}#)))
                 (if #{t\ 1004}#
                   (cdr #{t\ 1004}#)
                   (if (symbol? #{x\ 1001}#)
                     (let ((#{t\ 1005}#
                             (#{get-global-definition-hook\ 93}#
                               #{x\ 1001}#
                               #{mod\ 1003}#)))
                       (if #{t\ 1005}# #{t\ 1005}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 127}#
             (lambda (#{r\ 1006}#)
               (if (null? #{r\ 1006}#)
                 '()
                 (let ((#{a\ 1007}# (car #{r\ 1006}#)))
                   (if (eq? (cadr #{a\ 1007}#) (quote macro))
                     (cons #{a\ 1007}#
                           (#{macros-only-env\ 127}# (cdr #{r\ 1006}#)))
                     (#{macros-only-env\ 127}# (cdr #{r\ 1006}#)))))))
           (#{extend-var-env\ 126}#
             (lambda (#{labels\ 1008}# #{vars\ 1009}# #{r\ 1010}#)
               (if (null? #{labels\ 1008}#)
                 #{r\ 1010}#
                 (#{extend-var-env\ 126}#
                   (cdr #{labels\ 1008}#)
                   (cdr #{vars\ 1009}#)
                   (cons (cons (car #{labels\ 1008}#)
                               (cons (quote lexical) (car #{vars\ 1009}#)))
                         #{r\ 1010}#)))))
           (#{extend-env\ 125}#
             (lambda (#{labels\ 1011}# #{bindings\ 1012}# #{r\ 1013}#)
               (if (null? #{labels\ 1011}#)
                 #{r\ 1013}#
                 (#{extend-env\ 125}#
                   (cdr #{labels\ 1011}#)
                   (cdr #{bindings\ 1012}#)
                   (cons (cons (car #{labels\ 1011}#)
                               (car #{bindings\ 1012}#))
                         #{r\ 1013}#)))))
           (#{binding-value\ 124}# cdr)
           (#{binding-type\ 123}# car)
           (#{source-annotation\ 122}#
             (lambda (#{x\ 1014}#)
               (if (#{syntax-object?\ 115}# #{x\ 1014}#)
                 (#{source-annotation\ 122}#
                   (#{syntax-object-expression\ 116}# #{x\ 1014}#))
                 (if (pair? #{x\ 1014}#)
                   (let ((#{props\ 1015}# (source-properties #{x\ 1014}#)))
                     (if (pair? #{props\ 1015}#) #{props\ 1015}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 121}#
             (lambda (#{x\ 1016}# #{update\ 1017}#)
               (vector-set! #{x\ 1016}# 3 #{update\ 1017}#)))
           (#{set-syntax-object-wrap!\ 120}#
             (lambda (#{x\ 1018}# #{update\ 1019}#)
               (vector-set! #{x\ 1018}# 2 #{update\ 1019}#)))
           (#{set-syntax-object-expression!\ 119}#
             (lambda (#{x\ 1020}# #{update\ 1021}#)
               (vector-set! #{x\ 1020}# 1 #{update\ 1021}#)))
           (#{syntax-object-module\ 118}#
             (lambda (#{x\ 1022}#) (vector-ref #{x\ 1022}# 3)))
           (#{syntax-object-wrap\ 117}#
             (lambda (#{x\ 1023}#) (vector-ref #{x\ 1023}# 2)))
           (#{syntax-object-expression\ 116}#
             (lambda (#{x\ 1024}#) (vector-ref #{x\ 1024}# 1)))
           (#{syntax-object?\ 115}#
             (lambda (#{x\ 1025}#)
               (if (vector? #{x\ 1025}#)
                 (if (= (vector-length #{x\ 1025}#) 4)
                   (eq? (vector-ref #{x\ 1025}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 114}#
             (lambda (#{expression\ 1026}#
                      #{wrap\ 1027}#
                      #{module\ 1028}#)
               (vector
                 'syntax-object
                 #{expression\ 1026}#
                 #{wrap\ 1027}#
                 #{module\ 1028}#)))
           (#{build-letrec\ 113}#
             (lambda (#{src\ 1029}#
                      #{ids\ 1030}#
                      #{vars\ 1031}#
                      #{val-exps\ 1032}#
                      #{body-exp\ 1033}#)
               (if (null? #{vars\ 1031}#)
                 #{body-exp\ 1033}#
                 (let ((#{atom-key\ 1034}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1034}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 1030}#
                         #{val-exps\ 1032}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 1029}#
                        #{ids\ 1030}#
                        #{vars\ 1031}#
                        #{val-exps\ 1032}#
                        #{body-exp\ 1033}#))
                     (#{decorate-source\ 94}#
                       (list 'letrec
                             (map list #{vars\ 1031}# #{val-exps\ 1032}#)
                             #{body-exp\ 1033}#)
                       #{src\ 1029}#))))))
           (#{build-named-let\ 112}#
             (lambda (#{src\ 1035}#
                      #{ids\ 1036}#
                      #{vars\ 1037}#
                      #{val-exps\ 1038}#
                      #{body-exp\ 1039}#)
               (let ((#{f\ 1040}# (car #{vars\ 1037}#))
                     (#{f-name\ 1041}# (car #{ids\ 1036}#))
                     (#{vars\ 1042}# (cdr #{vars\ 1037}#))
                     (#{ids\ 1043}# (cdr #{ids\ 1036}#)))
                 (let ((#{atom-key\ 1044}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1044}# (quote (c)))
                     (let ((#{proc\ 1045}#
                             (#{build-simple-lambda\ 105}#
                               #{src\ 1035}#
                               #{ids\ 1043}#
                               #f
                               #{vars\ 1042}#
                               #f
                               #{body-exp\ 1039}#)))
                       (begin
                         (#{maybe-name-value!\ 103}#
                           #{f-name\ 1041}#
                           #{proc\ 1045}#)
                         (for-each
                           #{maybe-name-value!\ 103}#
                           #{ids\ 1043}#
                           #{val-exps\ 1038}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 1035}#
                          (list #{f-name\ 1041}#)
                          (list #{f\ 1040}#)
                          (list #{proc\ 1045}#)
                          (#{build-application\ 96}#
                            #{src\ 1035}#
                            (#{build-lexical-reference\ 98}#
                              'fun
                              #{src\ 1035}#
                              #{f-name\ 1041}#
                              #{f\ 1040}#)
                            #{val-exps\ 1038}#))))
                     (#{decorate-source\ 94}#
                       (list 'let
                             #{f\ 1040}#
                             (map list #{vars\ 1042}# #{val-exps\ 1038}#)
                             #{body-exp\ 1039}#)
                       #{src\ 1035}#))))))
           (#{build-let\ 111}#
             (lambda (#{src\ 1046}#
                      #{ids\ 1047}#
                      #{vars\ 1048}#
                      #{val-exps\ 1049}#
                      #{body-exp\ 1050}#)
               (if (null? #{vars\ 1048}#)
                 #{body-exp\ 1050}#
                 (let ((#{atom-key\ 1051}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1051}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 1047}#
                         #{val-exps\ 1049}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 1046}#
                        #{ids\ 1047}#
                        #{vars\ 1048}#
                        #{val-exps\ 1049}#
                        #{body-exp\ 1050}#))
                     (#{decorate-source\ 94}#
                       (list 'let
                             (map list #{vars\ 1048}# #{val-exps\ 1049}#)
                             #{body-exp\ 1050}#)
                       #{src\ 1046}#))))))
           (#{build-sequence\ 110}#
             (lambda (#{src\ 1052}# #{exps\ 1053}#)
               (if (null? (cdr #{exps\ 1053}#))
                 (car #{exps\ 1053}#)
                 (let ((#{atom-key\ 1054}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1054}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 1052}#
                      #{exps\ 1053}#)
                     (#{decorate-source\ 94}#
                       (cons (quote begin) #{exps\ 1053}#)
                       #{src\ 1052}#))))))
           (#{build-data\ 109}#
             (lambda (#{src\ 1055}# #{exp\ 1056}#)
               (let ((#{atom-key\ 1057}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1057}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 1055}#
                    #{exp\ 1056}#)
                   (#{decorate-source\ 94}#
                     (if (if (self-evaluating? #{exp\ 1056}#)
                           (not (vector? #{exp\ 1056}#))
                           #f)
                       #{exp\ 1056}#
                       (list (quote quote) #{exp\ 1056}#))
                     #{src\ 1055}#)))))
           (#{build-primref\ 108}#
             (lambda (#{src\ 1058}# #{name\ 1059}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 1060}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1060}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 1058}#
                      #{name\ 1059}#)
                     (#{decorate-source\ 94}#
                       #{name\ 1059}#
                       #{src\ 1058}#)))
                 (let ((#{atom-key\ 1061}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 1061}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 1058}#
                      '(guile)
                      #{name\ 1059}#
                      #f)
                     (#{decorate-source\ 94}#
                       (list (quote @@) (quote (guile)) #{name\ 1059}#)
                       #{src\ 1058}#))))))
           (#{build-lambda-case\ 107}#
             (lambda (#{src\ 1062}#
                      #{req\ 1063}#
                      #{opt\ 1064}#
                      #{rest\ 1065}#
                      #{kw\ 1066}#
                      #{inits\ 1067}#
                      #{vars\ 1068}#
                      #{predicate\ 1069}#
                      #{body\ 1070}#
                      #{else-case\ 1071}#)
               (let ((#{atom-key\ 1072}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1072}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 1062}#
                    #{req\ 1063}#
                    #{opt\ 1064}#
                    #{rest\ 1065}#
                    #{kw\ 1066}#
                    #{inits\ 1067}#
                    #{vars\ 1068}#
                    #{predicate\ 1069}#
                    #{body\ 1070}#
                    #{else-case\ 1071}#)
                   (let ((#{nreq\ 1073}# (length #{req\ 1063}#)))
                     (let ((#{nopt\ 1074}#
                             (if #{opt\ 1064}# (length #{opt\ 1064}#) 0)))
                       (let ((#{rest-idx\ 1075}#
                               (if #{rest\ 1065}#
                                 (+ #{nreq\ 1073}# #{nopt\ 1074}#)
                                 #f)))
                         (let ((#{allow-other-keys?\ 1076}#
                                 (if #{kw\ 1066}# (car #{kw\ 1066}#) #f)))
                           (let ((#{kw-indices\ 1077}#
                                   (map (lambda (#{x\ 1078}#)
                                          (cons (car #{x\ 1078}#)
                                                (list-index
                                                  #{vars\ 1068}#
                                                  (caddr #{x\ 1078}#))))
                                        (if #{kw\ 1066}#
                                          (cdr #{kw\ 1066}#)
                                          '()))))
                             (let ((#{nargs\ 1079}#
                                     (apply max
                                            (+ #{nreq\ 1073}#
                                               #{nopt\ 1074}#
                                               (if #{rest\ 1065}# 1 0))
                                            (map 1+
                                                 (map cdr
                                                      #{kw-indices\ 1077}#)))))
                               (begin
                                 (let ((#{t\ 1080}#
                                         (= #{nargs\ 1079}#
                                            (length #{vars\ 1068}#)
                                            (+ #{nreq\ 1073}#
                                               (length #{inits\ 1067}#)
                                               (if #{rest\ 1065}# 1 0)))))
                                   (if #{t\ 1080}#
                                     #{t\ 1080}#
                                     (error "something went wrong"
                                            #{req\ 1063}#
                                            #{opt\ 1064}#
                                            #{rest\ 1065}#
                                            #{kw\ 1066}#
                                            #{inits\ 1067}#
                                            #{vars\ 1068}#
                                            #{nreq\ 1073}#
                                            #{nopt\ 1074}#
                                            #{kw-indices\ 1077}#
                                            #{nargs\ 1079}#)))
                                 (#{decorate-source\ 94}#
                                   (cons (list (cons '(@@ (ice-9 optargs)
                                                          parse-lambda-case)
                                                     (cons (list 'quote
                                                                 (list #{nreq\ 1073}#
                                                                       #{nopt\ 1074}#
                                                                       #{rest-idx\ 1075}#
                                                                       #{nargs\ 1079}#
                                                                       #{allow-other-keys?\ 1076}#
                                                                       #{kw-indices\ 1077}#))
                                                           (cons (cons 'list
                                                                       (map (lambda (#{i\ 1081}#)
                                                                              (list 'lambda
                                                                                    #{vars\ 1068}#
                                                                                    #{i\ 1081}#))
                                                                            #{inits\ 1067}#))
                                                                 (cons (if #{predicate\ 1069}#
                                                                         (list 'lambda
                                                                               #{vars\ 1068}#
                                                                               #{predicate\ 1069}#)
                                                                         #f)
                                                                       '(%%args)))))
                                               '=>
                                               (list 'lambda
                                                     '(%%%args . _)
                                                     (cons 'apply
                                                           (cons (list 'lambda
                                                                       #{vars\ 1068}#
                                                                       #{body\ 1070}#)
                                                                 '(%%%args)))))
                                         (let ((#{t\ 1082}#
                                                 #{else-case\ 1071}#))
                                           (if #{t\ 1082}#
                                             #{t\ 1082}#
                                             '((%%args
                                                 (error "wrong number of arguments"
                                                        %%args))))))
                                   #{src\ 1062}#))))))))))))
           (#{build-case-lambda\ 106}#
             (lambda (#{src\ 1083}#
                      #{docstring\ 1084}#
                      #{body\ 1085}#)
               (let ((#{atom-key\ 1086}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1086}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1083}#
                    (if #{docstring\ 1084}#
                      (list (cons (quote documentation) #{docstring\ 1084}#))
                      '())
                    #{body\ 1085}#)
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 1084}#
                                     (list #{docstring\ 1084}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 1085}#)))))
                     #{src\ 1083}#)))))
           (#{build-simple-lambda\ 105}#
             (lambda (#{src\ 1087}#
                      #{req\ 1088}#
                      #{rest\ 1089}#
                      #{vars\ 1090}#
                      #{docstring\ 1091}#
                      #{exp\ 1092}#)
               (let ((#{atom-key\ 1093}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1093}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1087}#
                    (if #{docstring\ 1091}#
                      (list (cons (quote documentation) #{docstring\ 1091}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 1087}#
                     #{req\ 1088}#
                     #f
                     #{rest\ 1089}#
                     #f
                     '()
                     #{vars\ 1090}#
                     #f
                     #{exp\ 1092}#
                     #f))
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons (if #{rest\ 1089}#
                                   (apply cons* #{vars\ 1090}#)
                                   #{vars\ 1090}#)
                                 (append
                                   (if #{docstring\ 1091}#
                                     (list #{docstring\ 1091}#)
                                     '())
                                   (list #{exp\ 1092}#))))
                     #{src\ 1087}#)))))
           (#{build-global-definition\ 104}#
             (lambda (#{source\ 1094}# #{var\ 1095}# #{exp\ 1096}#)
               (let ((#{atom-key\ 1097}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1097}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 103}#
                       #{var\ 1095}#
                       #{exp\ 1096}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 1094}#
                      #{var\ 1095}#
                      #{exp\ 1096}#))
                   (#{decorate-source\ 94}#
                     (list (quote define) #{var\ 1095}# #{exp\ 1096}#)
                     #{source\ 1094}#)))))
           (#{maybe-name-value!\ 103}#
             (lambda (#{name\ 1098}# #{val\ 1099}#)
               (if ((@ (language tree-il) lambda?) #{val\ 1099}#)
                 (let ((#{meta\ 1100}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 1099}#)))
                   (if (not (assq (quote name) #{meta\ 1100}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 1099}#
                      (acons 'name
                             #{name\ 1098}#
                             #{meta\ 1100}#)))))))
           (#{build-global-assignment\ 102}#
             (lambda (#{source\ 1101}#
                      #{var\ 1102}#
                      #{exp\ 1103}#
                      #{mod\ 1104}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 1104}#
                 #{var\ 1102}#
                 (lambda (#{mod\ 1105}# #{var\ 1106}# #{public?\ 1107}#)
                   (let ((#{atom-key\ 1108}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1108}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 1101}#
                        #{mod\ 1105}#
                        #{var\ 1106}#
                        #{public?\ 1107}#
                        #{exp\ 1103}#)
                       (#{decorate-source\ 94}#
                         (list 'set!
                               (list (if #{public?\ 1107}#
                                       '@
                                       '@@)
                                     #{mod\ 1105}#
                                     #{var\ 1106}#)
                               #{exp\ 1103}#)
                         #{source\ 1101}#))))
                 (lambda (#{var\ 1109}#)
                   (let ((#{atom-key\ 1110}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1110}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 1101}#
                        #{var\ 1109}#
                        #{exp\ 1103}#)
                       (#{decorate-source\ 94}#
                         (list (quote set!) #{var\ 1109}# #{exp\ 1103}#)
                         #{source\ 1101}#)))))))
           (#{build-global-reference\ 101}#
             (lambda (#{source\ 1111}# #{var\ 1112}# #{mod\ 1113}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 1113}#
                 #{var\ 1112}#
                 (lambda (#{mod\ 1114}# #{var\ 1115}# #{public?\ 1116}#)
                   (let ((#{atom-key\ 1117}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1117}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 1111}#
                        #{mod\ 1114}#
                        #{var\ 1115}#
                        #{public?\ 1116}#)
                       (#{decorate-source\ 94}#
                         (list (if #{public?\ 1116}# (quote @) (quote @@))
                               #{mod\ 1114}#
                               #{var\ 1115}#)
                         #{source\ 1111}#))))
                 (lambda (#{var\ 1118}#)
                   (let ((#{atom-key\ 1119}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 1119}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 1111}#
                        #{var\ 1118}#)
                       (#{decorate-source\ 94}#
                         #{var\ 1118}#
                         #{source\ 1111}#)))))))
           (#{analyze-variable\ 100}#
             (lambda (#{mod\ 1120}#
                      #{var\ 1121}#
                      #{modref-cont\ 1122}#
                      #{bare-cont\ 1123}#)
               (if (not #{mod\ 1120}#)
                 (#{bare-cont\ 1123}# #{var\ 1121}#)
                 (let ((#{kind\ 1124}# (car #{mod\ 1120}#))
                       (#{mod\ 1125}# (cdr #{mod\ 1120}#)))
                   (if (memv #{kind\ 1124}# (quote (public)))
                     (#{modref-cont\ 1122}#
                       #{mod\ 1125}#
                       #{var\ 1121}#
                       #t)
                     (if (memv #{kind\ 1124}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 1125}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 1122}#
                           #{mod\ 1125}#
                           #{var\ 1121}#
                           #f)
                         (#{bare-cont\ 1123}# #{var\ 1121}#))
                       (if (memv #{kind\ 1124}# (quote (bare)))
                         (#{bare-cont\ 1123}# #{var\ 1121}#)
                         (if (memv #{kind\ 1124}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 1125}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 1125}#)
                                   #{var\ 1121}#)
                                 #f)
                             (#{modref-cont\ 1122}#
                               #{mod\ 1125}#
                               #{var\ 1121}#
                               #f)
                             (#{bare-cont\ 1123}# #{var\ 1121}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 1121}#
                             #{mod\ 1125}#)))))))))
           (#{build-lexical-assignment\ 99}#
             (lambda (#{source\ 1126}#
                      #{name\ 1127}#
                      #{var\ 1128}#
                      #{exp\ 1129}#)
               (let ((#{atom-key\ 1130}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1130}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 1126}#
                    #{name\ 1127}#
                    #{var\ 1128}#
                    #{exp\ 1129}#)
                   (#{decorate-source\ 94}#
                     (list (quote set!) #{var\ 1128}# #{exp\ 1129}#)
                     #{source\ 1126}#)))))
           (#{build-lexical-reference\ 98}#
             (lambda (#{type\ 1131}#
                      #{source\ 1132}#
                      #{name\ 1133}#
                      #{var\ 1134}#)
               (let ((#{atom-key\ 1135}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1135}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 1132}#
                    #{name\ 1133}#
                    #{var\ 1134}#)
                   (#{decorate-source\ 94}#
                     #{var\ 1134}#
                     #{source\ 1132}#)))))
           (#{build-conditional\ 97}#
             (lambda (#{source\ 1136}#
                      #{test-exp\ 1137}#
                      #{then-exp\ 1138}#
                      #{else-exp\ 1139}#)
               (let ((#{atom-key\ 1140}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1140}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 1136}#
                    #{test-exp\ 1137}#
                    #{then-exp\ 1138}#
                    #{else-exp\ 1139}#)
                   (#{decorate-source\ 94}#
                     (if (equal? #{else-exp\ 1139}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 1137}#
                             #{then-exp\ 1138}#)
                       (list 'if
                             #{test-exp\ 1137}#
                             #{then-exp\ 1138}#
                             #{else-exp\ 1139}#))
                     #{source\ 1136}#)))))
           (#{build-application\ 96}#
             (lambda (#{source\ 1141}#
                      #{fun-exp\ 1142}#
                      #{arg-exps\ 1143}#)
               (let ((#{atom-key\ 1144}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1144}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 1141}#
                    #{fun-exp\ 1142}#
                    #{arg-exps\ 1143}#)
                   (#{decorate-source\ 94}#
                     (cons #{fun-exp\ 1142}# #{arg-exps\ 1143}#)
                     #{source\ 1141}#)))))
           (#{build-void\ 95}#
             (lambda (#{source\ 1145}#)
               (let ((#{atom-key\ 1146}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 1146}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 1145}#)
                   (#{decorate-source\ 94}#
                     '(if #f #f)
                     #{source\ 1145}#)))))
           (#{decorate-source\ 94}#
             (lambda (#{e\ 1147}# #{s\ 1148}#)
               (begin
                 (if (if (pair? #{e\ 1147}#) #{s\ 1148}# #f)
                   (set-source-properties! #{e\ 1147}# #{s\ 1148}#))
                 #{e\ 1147}#)))
           (#{get-global-definition-hook\ 93}#
             (lambda (#{symbol\ 1149}# #{module\ 1150}#)
               (begin
                 (if (if (not #{module\ 1150}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 1149}#))
                 (let ((#{v\ 1151}#
                         (module-variable
                           (if #{module\ 1150}#
                             (resolve-module (cdr #{module\ 1150}#))
                             (current-module))
                           #{symbol\ 1149}#)))
                   (if #{v\ 1151}#
                     (if (variable-bound? #{v\ 1151}#)
                       (let ((#{val\ 1152}# (variable-ref #{v\ 1151}#)))
                         (if (macro? #{val\ 1152}#)
                           (if (syncase-macro-type #{val\ 1152}#)
                             (cons (syncase-macro-type #{val\ 1152}#)
                                   (syncase-macro-binding #{val\ 1152}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 92}#
             (lambda (#{symbol\ 1153}# #{type\ 1154}# #{val\ 1155}#)
               (let ((#{existing\ 1156}#
                       (let ((#{v\ 1157}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 1153}#)))
                         (if #{v\ 1157}#
                           (if (variable-bound? #{v\ 1157}#)
                             (let ((#{val\ 1158}# (variable-ref #{v\ 1157}#)))
                               (if (macro? #{val\ 1158}#)
                                 (if (not (syncase-macro-type #{val\ 1158}#))
                                   #{val\ 1158}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 1153}#
                   (if #{existing\ 1156}#
                     (make-extended-syncase-macro
                       #{existing\ 1156}#
                       #{type\ 1154}#
                       #{val\ 1155}#)
                     (make-syncase-macro #{type\ 1154}# #{val\ 1155}#))))))
           (#{local-eval-hook\ 91}#
             (lambda (#{x\ 1159}# #{mod\ 1160}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 1161}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 1161}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1159}#)
                           #{x\ 1159}#))))))
           (#{top-level-eval-hook\ 90}#
             (lambda (#{x\ 1162}# #{mod\ 1163}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 1164}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 1164}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 1162}#)
                           #{x\ 1162}#))))))
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
        (lambda (#{e\ 1165}#
                 #{r\ 1166}#
                 #{w\ 1167}#
                 #{s\ 1168}#
                 #{mod\ 1169}#)
          ((lambda (#{tmp\ 1170}#)
             ((lambda (#{tmp\ 1171}#)
                (if (if #{tmp\ 1171}#
                      (apply (lambda (#{_\ 1172}#
                                      #{var\ 1173}#
                                      #{val\ 1174}#
                                      #{e1\ 1175}#
                                      #{e2\ 1176}#)
                               (#{valid-bound-ids?\ 156}# #{var\ 1173}#))
                             #{tmp\ 1171}#)
                      #f)
                  (apply (lambda (#{_\ 1178}#
                                  #{var\ 1179}#
                                  #{val\ 1180}#
                                  #{e1\ 1181}#
                                  #{e2\ 1182}#)
                           (let ((#{names\ 1183}#
                                   (map (lambda (#{x\ 1184}#)
                                          (#{id-var-name\ 153}#
                                            #{x\ 1184}#
                                            #{w\ 1167}#))
                                        #{var\ 1179}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 1186}# #{n\ 1187}#)
                                   (let ((#{atom-key\ 1188}#
                                           (#{binding-type\ 123}#
                                             (#{lookup\ 128}#
                                               #{n\ 1187}#
                                               #{r\ 1166}#
                                               #{mod\ 1169}#))))
                                     (if (memv #{atom-key\ 1188}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 1165}#
                                         (#{source-wrap\ 160}#
                                           #{id\ 1186}#
                                           #{w\ 1167}#
                                           #{s\ 1168}#
                                           #{mod\ 1169}#)))))
                                 #{var\ 1179}#
                                 #{names\ 1183}#)
                               (#{chi-body\ 171}#
                                 (cons #{e1\ 1181}# #{e2\ 1182}#)
                                 (#{source-wrap\ 160}#
                                   #{e\ 1165}#
                                   #{w\ 1167}#
                                   #{s\ 1168}#
                                   #{mod\ 1169}#)
                                 (#{extend-env\ 125}#
                                   #{names\ 1183}#
                                   (let ((#{trans-r\ 1191}#
                                           (#{macros-only-env\ 127}#
                                             #{r\ 1166}#)))
                                     (map (lambda (#{x\ 1192}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 173}#
                                                    (#{chi\ 167}#
                                                      #{x\ 1192}#
                                                      #{trans-r\ 1191}#
                                                      #{w\ 1167}#
                                                      #{mod\ 1169}#)
                                                    #{mod\ 1169}#)))
                                          #{val\ 1180}#))
                                   #{r\ 1166}#)
                                 #{w\ 1167}#
                                 #{mod\ 1169}#))))
                         #{tmp\ 1171}#)
                  ((lambda (#{_\ 1194}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 1165}#
                         #{w\ 1167}#
                         #{s\ 1168}#
                         #{mod\ 1169}#)))
                   #{tmp\ 1170}#)))
              ($sc-dispatch
                #{tmp\ 1170}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1165}#)))
      (#{global-extend\ 129}#
        'core
        'quote
        (lambda (#{e\ 1195}#
                 #{r\ 1196}#
                 #{w\ 1197}#
                 #{s\ 1198}#
                 #{mod\ 1199}#)
          ((lambda (#{tmp\ 1200}#)
             ((lambda (#{tmp\ 1201}#)
                (if #{tmp\ 1201}#
                  (apply (lambda (#{_\ 1202}# #{e\ 1203}#)
                           (#{build-data\ 109}#
                             #{s\ 1198}#
                             (#{strip\ 180}# #{e\ 1203}# #{w\ 1197}#)))
                         #{tmp\ 1201}#)
                  ((lambda (#{_\ 1204}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 1195}#
                         #{w\ 1197}#
                         #{s\ 1198}#
                         #{mod\ 1199}#)))
                   #{tmp\ 1200}#)))
              ($sc-dispatch #{tmp\ 1200}# (quote (any any)))))
           #{e\ 1195}#)))
      (#{global-extend\ 129}#
        'core
        'syntax
        (letrec ((#{regen\ 1212}#
                   (lambda (#{x\ 1213}#)
                     (let ((#{atom-key\ 1214}# (car #{x\ 1213}#)))
                       (if (memv #{atom-key\ 1214}# (quote (ref)))
                         (#{build-lexical-reference\ 98}#
                           'value
                           #f
                           (cadr #{x\ 1213}#)
                           (cadr #{x\ 1213}#))
                         (if (memv #{atom-key\ 1214}# (quote (primitive)))
                           (#{build-primref\ 108}# #f (cadr #{x\ 1213}#))
                           (if (memv #{atom-key\ 1214}# (quote (quote)))
                             (#{build-data\ 109}# #f (cadr #{x\ 1213}#))
                             (if (memv #{atom-key\ 1214}# (quote (lambda)))
                               (if (list? (cadr #{x\ 1213}#))
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (cadr #{x\ 1213}#)
                                   #f
                                   (cadr #{x\ 1213}#)
                                   #f
                                   (#{regen\ 1212}# (caddr #{x\ 1213}#)))
                                 (error "how did we get here" #{x\ 1213}#))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-primref\ 108}# #f (car #{x\ 1213}#))
                                 (map #{regen\ 1212}#
                                      (cdr #{x\ 1213}#))))))))))
                 (#{gen-vector\ 1211}#
                   (lambda (#{x\ 1215}#)
                     (if (eq? (car #{x\ 1215}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 1215}#))
                       (if (eq? (car #{x\ 1215}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 1215}#)))
                         (list (quote list->vector) #{x\ 1215}#)))))
                 (#{gen-append\ 1210}#
                   (lambda (#{x\ 1216}# #{y\ 1217}#)
                     (if (equal? #{y\ 1217}# (quote (quote ())))
                       #{x\ 1216}#
                       (list (quote append) #{x\ 1216}# #{y\ 1217}#))))
                 (#{gen-cons\ 1209}#
                   (lambda (#{x\ 1218}# #{y\ 1219}#)
                     (let ((#{atom-key\ 1220}# (car #{y\ 1219}#)))
                       (if (memv #{atom-key\ 1220}# (quote (quote)))
                         (if (eq? (car #{x\ 1218}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 1218}#) (cadr #{y\ 1219}#)))
                           (if (eq? (cadr #{y\ 1219}#) (quote ()))
                             (list (quote list) #{x\ 1218}#)
                             (list (quote cons) #{x\ 1218}# #{y\ 1219}#)))
                         (if (memv #{atom-key\ 1220}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 1218}# (cdr #{y\ 1219}#)))
                           (list (quote cons) #{x\ 1218}# #{y\ 1219}#))))))
                 (#{gen-map\ 1208}#
                   (lambda (#{e\ 1221}# #{map-env\ 1222}#)
                     (let ((#{formals\ 1223}# (map cdr #{map-env\ 1222}#))
                           (#{actuals\ 1224}#
                             (map (lambda (#{x\ 1225}#)
                                    (list (quote ref) (car #{x\ 1225}#)))
                                  #{map-env\ 1222}#)))
                       (if (eq? (car #{e\ 1221}#) (quote ref))
                         (car #{actuals\ 1224}#)
                         (if (and-map
                               (lambda (#{x\ 1226}#)
                                 (if (eq? (car #{x\ 1226}#) (quote ref))
                                   (memq (cadr #{x\ 1226}#) #{formals\ 1223}#)
                                   #f))
                               (cdr #{e\ 1221}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 1221}#))
                                       (map (let ((#{r\ 1227}#
                                                    (map cons
                                                         #{formals\ 1223}#
                                                         #{actuals\ 1224}#)))
                                              (lambda (#{x\ 1228}#)
                                                (cdr (assq (cadr #{x\ 1228}#)
                                                           #{r\ 1227}#))))
                                            (cdr #{e\ 1221}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 1223}#
                                             #{e\ 1221}#)
                                       #{actuals\ 1224}#)))))))
                 (#{gen-mappend\ 1207}#
                   (lambda (#{e\ 1229}# #{map-env\ 1230}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 1208}# #{e\ 1229}# #{map-env\ 1230}#))))
                 (#{gen-ref\ 1206}#
                   (lambda (#{src\ 1231}#
                            #{var\ 1232}#
                            #{level\ 1233}#
                            #{maps\ 1234}#)
                     (if (#{fx=\ 88}# #{level\ 1233}# 0)
                       (values #{var\ 1232}# #{maps\ 1234}#)
                       (if (null? #{maps\ 1234}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 1231}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 1206}#
                               #{src\ 1231}#
                               #{var\ 1232}#
                               (#{fx-\ 87}# #{level\ 1233}# 1)
                               (cdr #{maps\ 1234}#)))
                           (lambda (#{outer-var\ 1235}# #{outer-maps\ 1236}#)
                             (let ((#{b\ 1237}#
                                     (assq #{outer-var\ 1235}#
                                           (car #{maps\ 1234}#))))
                               (if #{b\ 1237}#
                                 (values (cdr #{b\ 1237}#) #{maps\ 1234}#)
                                 (let ((#{inner-var\ 1238}#
                                         (#{gen-var\ 181}# (quote tmp))))
                                   (values
                                     #{inner-var\ 1238}#
                                     (cons (cons (cons #{outer-var\ 1235}#
                                                       #{inner-var\ 1238}#)
                                                 (car #{maps\ 1234}#))
                                           #{outer-maps\ 1236}#)))))))))))
                 (#{gen-syntax\ 1205}#
                   (lambda (#{src\ 1239}#
                            #{e\ 1240}#
                            #{r\ 1241}#
                            #{maps\ 1242}#
                            #{ellipsis?\ 1243}#
                            #{mod\ 1244}#)
                     (if (#{id?\ 131}# #{e\ 1240}#)
                       (let ((#{label\ 1245}#
                               (#{id-var-name\ 153}#
                                 #{e\ 1240}#
                                 '(()))))
                         (let ((#{b\ 1246}#
                                 (#{lookup\ 128}#
                                   #{label\ 1245}#
                                   #{r\ 1241}#
                                   #{mod\ 1244}#)))
                           (if (eq? (#{binding-type\ 123}# #{b\ 1246}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 1247}#
                                         (#{binding-value\ 124}# #{b\ 1246}#)))
                                   (#{gen-ref\ 1206}#
                                     #{src\ 1239}#
                                     (car #{var.lev\ 1247}#)
                                     (cdr #{var.lev\ 1247}#)
                                     #{maps\ 1242}#)))
                               (lambda (#{var\ 1248}# #{maps\ 1249}#)
                                 (values
                                   (list (quote ref) #{var\ 1248}#)
                                   #{maps\ 1249}#)))
                             (if (#{ellipsis?\ 1243}# #{e\ 1240}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 1239}#)
                               (values
                                 (list (quote quote) #{e\ 1240}#)
                                 #{maps\ 1242}#)))))
                       ((lambda (#{tmp\ 1250}#)
                          ((lambda (#{tmp\ 1251}#)
                             (if (if #{tmp\ 1251}#
                                   (apply (lambda (#{dots\ 1252}# #{e\ 1253}#)
                                            (#{ellipsis?\ 1243}#
                                              #{dots\ 1252}#))
                                          #{tmp\ 1251}#)
                                   #f)
                               (apply (lambda (#{dots\ 1254}# #{e\ 1255}#)
                                        (#{gen-syntax\ 1205}#
                                          #{src\ 1239}#
                                          #{e\ 1255}#
                                          #{r\ 1241}#
                                          #{maps\ 1242}#
                                          (lambda (#{x\ 1256}#) #f)
                                          #{mod\ 1244}#))
                                      #{tmp\ 1251}#)
                               ((lambda (#{tmp\ 1257}#)
                                  (if (if #{tmp\ 1257}#
                                        (apply (lambda (#{x\ 1258}#
                                                        #{dots\ 1259}#
                                                        #{y\ 1260}#)
                                                 (#{ellipsis?\ 1243}#
                                                   #{dots\ 1259}#))
                                               #{tmp\ 1257}#)
                                        #f)
                                    (apply (lambda (#{x\ 1261}#
                                                    #{dots\ 1262}#
                                                    #{y\ 1263}#)
                                             (letrec ((#{f\ 1264}#
                                                        (lambda (#{y\ 1265}#
                                                                 #{k\ 1266}#)
                                                          ((lambda (#{tmp\ 1270}#)
                                                             ((lambda (#{tmp\ 1271}#)
                                                                (if (if #{tmp\ 1271}#
                                                                      (apply (lambda (#{dots\ 1272}#
                                                                                      #{y\ 1273}#)
                                                                               (#{ellipsis?\ 1243}#
                                                                                 #{dots\ 1272}#))
                                                                             #{tmp\ 1271}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 1274}#
                                                                                  #{y\ 1275}#)
                                                                           (#{f\ 1264}#
                                                                             #{y\ 1275}#
                                                                             (lambda (#{maps\ 1276}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 1266}#
                                                                                     (cons '()
                                                                                           #{maps\ 1276}#)))
                                                                                 (lambda (#{x\ 1277}#
                                                                                          #{maps\ 1278}#)
                                                                                   (if (null? (car #{maps\ 1278}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 1239}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 1207}#
                                                                                         #{x\ 1277}#
                                                                                         (car #{maps\ 1278}#))
                                                                                       (cdr #{maps\ 1278}#))))))))
                                                                         #{tmp\ 1271}#)
                                                                  ((lambda (#{_\ 1279}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 1205}#
                                                                           #{src\ 1239}#
                                                                           #{y\ 1265}#
                                                                           #{r\ 1241}#
                                                                           #{maps\ 1242}#
                                                                           #{ellipsis?\ 1243}#
                                                                           #{mod\ 1244}#))
                                                                       (lambda (#{y\ 1280}#
                                                                                #{maps\ 1281}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 1266}#
                                                                               #{maps\ 1281}#))
                                                                           (lambda (#{x\ 1282}#
                                                                                    #{maps\ 1283}#)
                                                                             (values
                                                                               (#{gen-append\ 1210}#
                                                                                 #{x\ 1282}#
                                                                                 #{y\ 1280}#)
                                                                               #{maps\ 1283}#))))))
                                                                   #{tmp\ 1270}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 1270}#
                                                                '(any . any))))
                                                           #{y\ 1265}#))))
                                               (#{f\ 1264}#
                                                 #{y\ 1263}#
                                                 (lambda (#{maps\ 1267}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 1205}#
                                                         #{src\ 1239}#
                                                         #{x\ 1261}#
                                                         #{r\ 1241}#
                                                         (cons '()
                                                               #{maps\ 1267}#)
                                                         #{ellipsis?\ 1243}#
                                                         #{mod\ 1244}#))
                                                     (lambda (#{x\ 1268}#
                                                              #{maps\ 1269}#)
                                                       (if (null? (car #{maps\ 1269}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 1239}#)
                                                         (values
                                                           (#{gen-map\ 1208}#
                                                             #{x\ 1268}#
                                                             (car #{maps\ 1269}#))
                                                           (cdr #{maps\ 1269}#)))))))))
                                           #{tmp\ 1257}#)
                                    ((lambda (#{tmp\ 1284}#)
                                       (if #{tmp\ 1284}#
                                         (apply (lambda (#{x\ 1285}#
                                                         #{y\ 1286}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 1205}#
                                                        #{src\ 1239}#
                                                        #{x\ 1285}#
                                                        #{r\ 1241}#
                                                        #{maps\ 1242}#
                                                        #{ellipsis?\ 1243}#
                                                        #{mod\ 1244}#))
                                                    (lambda (#{x\ 1287}#
                                                             #{maps\ 1288}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 1205}#
                                                            #{src\ 1239}#
                                                            #{y\ 1286}#
                                                            #{r\ 1241}#
                                                            #{maps\ 1288}#
                                                            #{ellipsis?\ 1243}#
                                                            #{mod\ 1244}#))
                                                        (lambda (#{y\ 1289}#
                                                                 #{maps\ 1290}#)
                                                          (values
                                                            (#{gen-cons\ 1209}#
                                                              #{x\ 1287}#
                                                              #{y\ 1289}#)
                                                            #{maps\ 1290}#))))))
                                                #{tmp\ 1284}#)
                                         ((lambda (#{tmp\ 1291}#)
                                            (if #{tmp\ 1291}#
                                              (apply (lambda (#{e1\ 1292}#
                                                              #{e2\ 1293}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 1205}#
                                                             #{src\ 1239}#
                                                             (cons #{e1\ 1292}#
                                                                   #{e2\ 1293}#)
                                                             #{r\ 1241}#
                                                             #{maps\ 1242}#
                                                             #{ellipsis?\ 1243}#
                                                             #{mod\ 1244}#))
                                                         (lambda (#{e\ 1295}#
                                                                  #{maps\ 1296}#)
                                                           (values
                                                             (#{gen-vector\ 1211}#
                                                               #{e\ 1295}#)
                                                             #{maps\ 1296}#))))
                                                     #{tmp\ 1291}#)
                                              ((lambda (#{_\ 1297}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 1240}#)
                                                   #{maps\ 1242}#))
                                               #{tmp\ 1250}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1250}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 1250}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 1250}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 1250}# (quote (any any)))))
                        #{e\ 1240}#)))))
          (lambda (#{e\ 1298}#
                   #{r\ 1299}#
                   #{w\ 1300}#
                   #{s\ 1301}#
                   #{mod\ 1302}#)
            (let ((#{e\ 1303}#
                    (#{source-wrap\ 160}#
                      #{e\ 1298}#
                      #{w\ 1300}#
                      #{s\ 1301}#
                      #{mod\ 1302}#)))
              ((lambda (#{tmp\ 1304}#)
                 ((lambda (#{tmp\ 1305}#)
                    (if #{tmp\ 1305}#
                      (apply (lambda (#{_\ 1306}# #{x\ 1307}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 1205}#
                                     #{e\ 1303}#
                                     #{x\ 1307}#
                                     #{r\ 1299}#
                                     '()
                                     #{ellipsis?\ 175}#
                                     #{mod\ 1302}#))
                                 (lambda (#{e\ 1308}# #{maps\ 1309}#)
                                   (#{regen\ 1212}# #{e\ 1308}#))))
                             #{tmp\ 1305}#)
                      ((lambda (#{_\ 1310}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 1303}#))
                       #{tmp\ 1304}#)))
                  ($sc-dispatch #{tmp\ 1304}# (quote (any any)))))
               #{e\ 1303}#)))))
      (#{global-extend\ 129}#
        'core
        'lambda
        (lambda (#{e\ 1311}#
                 #{r\ 1312}#
                 #{w\ 1313}#
                 #{s\ 1314}#
                 #{mod\ 1315}#)
          ((lambda (#{tmp\ 1316}#)
             ((lambda (#{tmp\ 1317}#)
                (if (if #{tmp\ 1317}#
                      (apply (lambda (#{_\ 1318}#
                                      #{args\ 1319}#
                                      #{docstring\ 1320}#
                                      #{e1\ 1321}#
                                      #{e2\ 1322}#)
                               (string? (syntax->datum #{docstring\ 1320}#)))
                             #{tmp\ 1317}#)
                      #f)
                  (apply (lambda (#{_\ 1323}#
                                  #{args\ 1324}#
                                  #{docstring\ 1325}#
                                  #{e1\ 1326}#
                                  #{e2\ 1327}#)
                           (call-with-values
                             (lambda ()
                               (#{lambda-formals\ 176}# #{args\ 1324}#))
                             (lambda (#{req\ 1328}#
                                      #{opt\ 1329}#
                                      #{rest\ 1330}#
                                      #{kw\ 1331}#)
                               (#{chi-simple-lambda\ 177}#
                                 #{e\ 1311}#
                                 #{r\ 1312}#
                                 #{w\ 1313}#
                                 #{s\ 1314}#
                                 #{mod\ 1315}#
                                 #{req\ 1328}#
                                 #{rest\ 1330}#
                                 (syntax->datum #{docstring\ 1325}#)
                                 (cons #{e1\ 1326}# #{e2\ 1327}#)))))
                         #{tmp\ 1317}#)
                  ((lambda (#{tmp\ 1333}#)
                     (if #{tmp\ 1333}#
                       (apply (lambda (#{_\ 1334}#
                                       #{args\ 1335}#
                                       #{e1\ 1336}#
                                       #{e2\ 1337}#)
                                (call-with-values
                                  (lambda ()
                                    (#{lambda-formals\ 176}# #{args\ 1335}#))
                                  (lambda (#{req\ 1338}#
                                           #{opt\ 1339}#
                                           #{rest\ 1340}#
                                           #{kw\ 1341}#)
                                    (#{chi-simple-lambda\ 177}#
                                      #{e\ 1311}#
                                      #{r\ 1312}#
                                      #{w\ 1313}#
                                      #{s\ 1314}#
                                      #{mod\ 1315}#
                                      #{req\ 1338}#
                                      #{rest\ 1340}#
                                      #f
                                      (cons #{e1\ 1336}# #{e2\ 1337}#)))))
                              #{tmp\ 1333}#)
                       ((lambda (#{_\ 1343}#)
                          (syntax-violation
                            'lambda
                            "bad lambda"
                            #{e\ 1311}#))
                        #{tmp\ 1316}#)))
                   ($sc-dispatch
                     #{tmp\ 1316}#
                     '(any any any . each-any)))))
              ($sc-dispatch
                #{tmp\ 1316}#
                '(any any any any . each-any))))
           #{e\ 1311}#)))
      (#{global-extend\ 129}#
        'core
        'lambda*
        (lambda (#{e\ 1344}#
                 #{r\ 1345}#
                 #{w\ 1346}#
                 #{s\ 1347}#
                 #{mod\ 1348}#)
          ((lambda (#{tmp\ 1349}#)
             ((lambda (#{tmp\ 1350}#)
                (if #{tmp\ 1350}#
                  (apply (lambda (#{_\ 1351}#
                                  #{args\ 1352}#
                                  #{e1\ 1353}#
                                  #{e2\ 1354}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1344}#
                                 #{r\ 1345}#
                                 #{w\ 1346}#
                                 #{s\ 1347}#
                                 #{mod\ 1348}#
                                 #{lambda*-formals\ 178}#
                                 (list (cons #{args\ 1352}#
                                             (cons #{e1\ 1353}#
                                                   #{e2\ 1354}#)))))
                             (lambda (#{docstring\ 1356}# #{lcase\ 1357}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1347}#
                                 #{docstring\ 1356}#
                                 #{lcase\ 1357}#))))
                         #{tmp\ 1350}#)
                  ((lambda (#{_\ 1358}#)
                     (syntax-violation
                       'lambda
                       "bad lambda*"
                       #{e\ 1344}#))
                   #{tmp\ 1349}#)))
              ($sc-dispatch
                #{tmp\ 1349}#
                '(any any any . each-any))))
           #{e\ 1344}#)))
      (#{global-extend\ 129}#
        'core
        'case-lambda
        (lambda (#{e\ 1359}#
                 #{r\ 1360}#
                 #{w\ 1361}#
                 #{s\ 1362}#
                 #{mod\ 1363}#)
          ((lambda (#{tmp\ 1364}#)
             ((lambda (#{tmp\ 1365}#)
                (if #{tmp\ 1365}#
                  (apply (lambda (#{_\ 1366}#
                                  #{args\ 1367}#
                                  #{e1\ 1368}#
                                  #{e2\ 1369}#
                                  #{args*\ 1370}#
                                  #{e1*\ 1371}#
                                  #{e2*\ 1372}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1359}#
                                 #{r\ 1360}#
                                 #{w\ 1361}#
                                 #{s\ 1362}#
                                 #{mod\ 1363}#
                                 #{lambda-formals\ 176}#
                                 (cons (cons #{args\ 1367}#
                                             (cons #{e1\ 1368}# #{e2\ 1369}#))
                                       (map (lambda (#{tmp\ 1376}#
                                                     #{tmp\ 1375}#
                                                     #{tmp\ 1374}#)
                                              (cons #{tmp\ 1374}#
                                                    (cons #{tmp\ 1375}#
                                                          #{tmp\ 1376}#)))
                                            #{e2*\ 1372}#
                                            #{e1*\ 1371}#
                                            #{args*\ 1370}#))))
                             (lambda (#{docstring\ 1378}# #{lcase\ 1379}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1362}#
                                 #{docstring\ 1378}#
                                 #{lcase\ 1379}#))))
                         #{tmp\ 1365}#)
                  ((lambda (#{_\ 1380}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda"
                       #{e\ 1359}#))
                   #{tmp\ 1364}#)))
              ($sc-dispatch
                #{tmp\ 1364}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 1359}#)))
      (#{global-extend\ 129}#
        'core
        'case-lambda*
        (lambda (#{e\ 1381}#
                 #{r\ 1382}#
                 #{w\ 1383}#
                 #{s\ 1384}#
                 #{mod\ 1385}#)
          ((lambda (#{tmp\ 1386}#)
             ((lambda (#{tmp\ 1387}#)
                (if #{tmp\ 1387}#
                  (apply (lambda (#{_\ 1388}#
                                  #{args\ 1389}#
                                  #{e1\ 1390}#
                                  #{e2\ 1391}#
                                  #{args*\ 1392}#
                                  #{e1*\ 1393}#
                                  #{e2*\ 1394}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 179}#
                                 #{e\ 1381}#
                                 #{r\ 1382}#
                                 #{w\ 1383}#
                                 #{s\ 1384}#
                                 #{mod\ 1385}#
                                 #{lambda*-formals\ 178}#
                                 (cons (cons #{args\ 1389}#
                                             (cons #{e1\ 1390}# #{e2\ 1391}#))
                                       (map (lambda (#{tmp\ 1398}#
                                                     #{tmp\ 1397}#
                                                     #{tmp\ 1396}#)
                                              (cons #{tmp\ 1396}#
                                                    (cons #{tmp\ 1397}#
                                                          #{tmp\ 1398}#)))
                                            #{e2*\ 1394}#
                                            #{e1*\ 1393}#
                                            #{args*\ 1392}#))))
                             (lambda (#{docstring\ 1400}# #{lcase\ 1401}#)
                               (#{build-case-lambda\ 106}#
                                 #{s\ 1384}#
                                 #{docstring\ 1400}#
                                 #{lcase\ 1401}#))))
                         #{tmp\ 1387}#)
                  ((lambda (#{_\ 1402}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda*"
                       #{e\ 1381}#))
                   #{tmp\ 1386}#)))
              ($sc-dispatch
                #{tmp\ 1386}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 1381}#)))
      (#{global-extend\ 129}#
        'core
        'let
        (letrec ((#{chi-let\ 1403}#
                   (lambda (#{e\ 1404}#
                            #{r\ 1405}#
                            #{w\ 1406}#
                            #{s\ 1407}#
                            #{mod\ 1408}#
                            #{constructor\ 1409}#
                            #{ids\ 1410}#
                            #{vals\ 1411}#
                            #{exps\ 1412}#)
                     (if (not (#{valid-bound-ids?\ 156}# #{ids\ 1410}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 1404}#)
                       (let ((#{labels\ 1413}#
                               (#{gen-labels\ 137}# #{ids\ 1410}#))
                             (#{new-vars\ 1414}#
                               (map #{gen-var\ 181}# #{ids\ 1410}#)))
                         (let ((#{nw\ 1415}#
                                 (#{make-binding-wrap\ 148}#
                                   #{ids\ 1410}#
                                   #{labels\ 1413}#
                                   #{w\ 1406}#))
                               (#{nr\ 1416}#
                                 (#{extend-var-env\ 126}#
                                   #{labels\ 1413}#
                                   #{new-vars\ 1414}#
                                   #{r\ 1405}#)))
                           (#{constructor\ 1409}#
                             #{s\ 1407}#
                             (map syntax->datum #{ids\ 1410}#)
                             #{new-vars\ 1414}#
                             (map (lambda (#{x\ 1417}#)
                                    (#{chi\ 167}#
                                      #{x\ 1417}#
                                      #{r\ 1405}#
                                      #{w\ 1406}#
                                      #{mod\ 1408}#))
                                  #{vals\ 1411}#)
                             (#{chi-body\ 171}#
                               #{exps\ 1412}#
                               (#{source-wrap\ 160}#
                                 #{e\ 1404}#
                                 #{nw\ 1415}#
                                 #{s\ 1407}#
                                 #{mod\ 1408}#)
                               #{nr\ 1416}#
                               #{nw\ 1415}#
                               #{mod\ 1408}#))))))))
          (lambda (#{e\ 1418}#
                   #{r\ 1419}#
                   #{w\ 1420}#
                   #{s\ 1421}#
                   #{mod\ 1422}#)
            ((lambda (#{tmp\ 1423}#)
               ((lambda (#{tmp\ 1424}#)
                  (if (if #{tmp\ 1424}#
                        (apply (lambda (#{_\ 1425}#
                                        #{id\ 1426}#
                                        #{val\ 1427}#
                                        #{e1\ 1428}#
                                        #{e2\ 1429}#)
                                 (and-map #{id?\ 131}# #{id\ 1426}#))
                               #{tmp\ 1424}#)
                        #f)
                    (apply (lambda (#{_\ 1431}#
                                    #{id\ 1432}#
                                    #{val\ 1433}#
                                    #{e1\ 1434}#
                                    #{e2\ 1435}#)
                             (#{chi-let\ 1403}#
                               #{e\ 1418}#
                               #{r\ 1419}#
                               #{w\ 1420}#
                               #{s\ 1421}#
                               #{mod\ 1422}#
                               #{build-let\ 111}#
                               #{id\ 1432}#
                               #{val\ 1433}#
                               (cons #{e1\ 1434}# #{e2\ 1435}#)))
                           #{tmp\ 1424}#)
                    ((lambda (#{tmp\ 1439}#)
                       (if (if #{tmp\ 1439}#
                             (apply (lambda (#{_\ 1440}#
                                             #{f\ 1441}#
                                             #{id\ 1442}#
                                             #{val\ 1443}#
                                             #{e1\ 1444}#
                                             #{e2\ 1445}#)
                                      (if (#{id?\ 131}# #{f\ 1441}#)
                                        (and-map #{id?\ 131}# #{id\ 1442}#)
                                        #f))
                                    #{tmp\ 1439}#)
                             #f)
                         (apply (lambda (#{_\ 1447}#
                                         #{f\ 1448}#
                                         #{id\ 1449}#
                                         #{val\ 1450}#
                                         #{e1\ 1451}#
                                         #{e2\ 1452}#)
                                  (#{chi-let\ 1403}#
                                    #{e\ 1418}#
                                    #{r\ 1419}#
                                    #{w\ 1420}#
                                    #{s\ 1421}#
                                    #{mod\ 1422}#
                                    #{build-named-let\ 112}#
                                    (cons #{f\ 1448}# #{id\ 1449}#)
                                    #{val\ 1450}#
                                    (cons #{e1\ 1451}# #{e2\ 1452}#)))
                                #{tmp\ 1439}#)
                         ((lambda (#{_\ 1456}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 160}#
                                #{e\ 1418}#
                                #{w\ 1420}#
                                #{s\ 1421}#
                                #{mod\ 1422}#)))
                          #{tmp\ 1423}#)))
                     ($sc-dispatch
                       #{tmp\ 1423}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1423}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 1418}#))))
      (#{global-extend\ 129}#
        'core
        'letrec
        (lambda (#{e\ 1457}#
                 #{r\ 1458}#
                 #{w\ 1459}#
                 #{s\ 1460}#
                 #{mod\ 1461}#)
          ((lambda (#{tmp\ 1462}#)
             ((lambda (#{tmp\ 1463}#)
                (if (if #{tmp\ 1463}#
                      (apply (lambda (#{_\ 1464}#
                                      #{id\ 1465}#
                                      #{val\ 1466}#
                                      #{e1\ 1467}#
                                      #{e2\ 1468}#)
                               (and-map #{id?\ 131}# #{id\ 1465}#))
                             #{tmp\ 1463}#)
                      #f)
                  (apply (lambda (#{_\ 1470}#
                                  #{id\ 1471}#
                                  #{val\ 1472}#
                                  #{e1\ 1473}#
                                  #{e2\ 1474}#)
                           (let ((#{ids\ 1475}# #{id\ 1471}#))
                             (if (not (#{valid-bound-ids?\ 156}#
                                        #{ids\ 1475}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 1457}#)
                               (let ((#{labels\ 1477}#
                                       (#{gen-labels\ 137}# #{ids\ 1475}#))
                                     (#{new-vars\ 1478}#
                                       (map #{gen-var\ 181}# #{ids\ 1475}#)))
                                 (let ((#{w\ 1479}#
                                         (#{make-binding-wrap\ 148}#
                                           #{ids\ 1475}#
                                           #{labels\ 1477}#
                                           #{w\ 1459}#))
                                       (#{r\ 1480}#
                                         (#{extend-var-env\ 126}#
                                           #{labels\ 1477}#
                                           #{new-vars\ 1478}#
                                           #{r\ 1458}#)))
                                   (#{build-letrec\ 113}#
                                     #{s\ 1460}#
                                     (map syntax->datum #{ids\ 1475}#)
                                     #{new-vars\ 1478}#
                                     (map (lambda (#{x\ 1481}#)
                                            (#{chi\ 167}#
                                              #{x\ 1481}#
                                              #{r\ 1480}#
                                              #{w\ 1479}#
                                              #{mod\ 1461}#))
                                          #{val\ 1472}#)
                                     (#{chi-body\ 171}#
                                       (cons #{e1\ 1473}# #{e2\ 1474}#)
                                       (#{source-wrap\ 160}#
                                         #{e\ 1457}#
                                         #{w\ 1479}#
                                         #{s\ 1460}#
                                         #{mod\ 1461}#)
                                       #{r\ 1480}#
                                       #{w\ 1479}#
                                       #{mod\ 1461}#)))))))
                         #{tmp\ 1463}#)
                  ((lambda (#{_\ 1484}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 160}#
                         #{e\ 1457}#
                         #{w\ 1459}#
                         #{s\ 1460}#
                         #{mod\ 1461}#)))
                   #{tmp\ 1462}#)))
              ($sc-dispatch
                #{tmp\ 1462}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1457}#)))
      (#{global-extend\ 129}#
        'core
        'set!
        (lambda (#{e\ 1485}#
                 #{r\ 1486}#
                 #{w\ 1487}#
                 #{s\ 1488}#
                 #{mod\ 1489}#)
          ((lambda (#{tmp\ 1490}#)
             ((lambda (#{tmp\ 1491}#)
                (if (if #{tmp\ 1491}#
                      (apply (lambda (#{_\ 1492}# #{id\ 1493}# #{val\ 1494}#)
                               (#{id?\ 131}# #{id\ 1493}#))
                             #{tmp\ 1491}#)
                      #f)
                  (apply (lambda (#{_\ 1495}# #{id\ 1496}# #{val\ 1497}#)
                           (let ((#{val\ 1498}#
                                   (#{chi\ 167}#
                                     #{val\ 1497}#
                                     #{r\ 1486}#
                                     #{w\ 1487}#
                                     #{mod\ 1489}#))
                                 (#{n\ 1499}#
                                   (#{id-var-name\ 153}#
                                     #{id\ 1496}#
                                     #{w\ 1487}#)))
                             (let ((#{b\ 1500}#
                                     (#{lookup\ 128}#
                                       #{n\ 1499}#
                                       #{r\ 1486}#
                                       #{mod\ 1489}#)))
                               (let ((#{atom-key\ 1501}#
                                       (#{binding-type\ 123}# #{b\ 1500}#)))
                                 (if (memv #{atom-key\ 1501}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 99}#
                                     #{s\ 1488}#
                                     (syntax->datum #{id\ 1496}#)
                                     (#{binding-value\ 124}# #{b\ 1500}#)
                                     #{val\ 1498}#)
                                   (if (memv #{atom-key\ 1501}#
                                             '(global))
                                     (#{build-global-assignment\ 102}#
                                       #{s\ 1488}#
                                       #{n\ 1499}#
                                       #{val\ 1498}#
                                       #{mod\ 1489}#)
                                     (if (memv #{atom-key\ 1501}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 159}#
                                           #{id\ 1496}#
                                           #{w\ 1487}#
                                           #{mod\ 1489}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 160}#
                                           #{e\ 1485}#
                                           #{w\ 1487}#
                                           #{s\ 1488}#
                                           #{mod\ 1489}#)))))))))
                         #{tmp\ 1491}#)
                  ((lambda (#{tmp\ 1502}#)
                     (if #{tmp\ 1502}#
                       (apply (lambda (#{_\ 1503}#
                                       #{head\ 1504}#
                                       #{tail\ 1505}#
                                       #{val\ 1506}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 165}#
                                      #{head\ 1504}#
                                      #{r\ 1486}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 1489}#
                                      #t))
                                  (lambda (#{type\ 1507}#
                                           #{value\ 1508}#
                                           #{ee\ 1509}#
                                           #{ww\ 1510}#
                                           #{ss\ 1511}#
                                           #{modmod\ 1512}#)
                                    (if (memv #{type\ 1507}#
                                              '(module-ref))
                                      (let ((#{val\ 1513}#
                                              (#{chi\ 167}#
                                                #{val\ 1506}#
                                                #{r\ 1486}#
                                                #{w\ 1487}#
                                                #{mod\ 1489}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 1508}#
                                              (cons #{head\ 1504}#
                                                    #{tail\ 1505}#)))
                                          (lambda (#{id\ 1515}# #{mod\ 1516}#)
                                            (#{build-global-assignment\ 102}#
                                              #{s\ 1488}#
                                              #{id\ 1515}#
                                              #{val\ 1513}#
                                              #{mod\ 1516}#))))
                                      (#{build-application\ 96}#
                                        #{s\ 1488}#
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
                                                #{head\ 1504}#)
                                          #{r\ 1486}#
                                          #{w\ 1487}#
                                          #{mod\ 1489}#)
                                        (map (lambda (#{e\ 1517}#)
                                               (#{chi\ 167}#
                                                 #{e\ 1517}#
                                                 #{r\ 1486}#
                                                 #{w\ 1487}#
                                                 #{mod\ 1489}#))
                                             (append
                                               #{tail\ 1505}#
                                               (list #{val\ 1506}#))))))))
                              #{tmp\ 1502}#)
                       ((lambda (#{_\ 1519}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 160}#
                              #{e\ 1485}#
                              #{w\ 1487}#
                              #{s\ 1488}#
                              #{mod\ 1489}#)))
                        #{tmp\ 1490}#)))
                   ($sc-dispatch
                     #{tmp\ 1490}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 1490}#
                '(any any any))))
           #{e\ 1485}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@
        (lambda (#{e\ 1520}#)
          ((lambda (#{tmp\ 1521}#)
             ((lambda (#{tmp\ 1522}#)
                (if (if #{tmp\ 1522}#
                      (apply (lambda (#{_\ 1523}# #{mod\ 1524}# #{id\ 1525}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1524}#)
                                 (#{id?\ 131}# #{id\ 1525}#)
                                 #f))
                             #{tmp\ 1522}#)
                      #f)
                  (apply (lambda (#{_\ 1527}# #{mod\ 1528}# #{id\ 1529}#)
                           (values
                             (syntax->datum #{id\ 1529}#)
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
                                     #{mod\ 1528}#))))
                         #{tmp\ 1522}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1521}#)))
              ($sc-dispatch
                #{tmp\ 1521}#
                '(any each-any any))))
           #{e\ 1520}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@@
        (lambda (#{e\ 1531}#)
          ((lambda (#{tmp\ 1532}#)
             ((lambda (#{tmp\ 1533}#)
                (if (if #{tmp\ 1533}#
                      (apply (lambda (#{_\ 1534}# #{mod\ 1535}# #{id\ 1536}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1535}#)
                                 (#{id?\ 131}# #{id\ 1536}#)
                                 #f))
                             #{tmp\ 1533}#)
                      #f)
                  (apply (lambda (#{_\ 1538}# #{mod\ 1539}# #{id\ 1540}#)
                           (values
                             (syntax->datum #{id\ 1540}#)
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
                                     #{mod\ 1539}#))))
                         #{tmp\ 1533}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1532}#)))
              ($sc-dispatch
                #{tmp\ 1532}#
                '(any each-any any))))
           #{e\ 1531}#)))
      (#{global-extend\ 129}#
        'core
        'if
        (lambda (#{e\ 1542}#
                 #{r\ 1543}#
                 #{w\ 1544}#
                 #{s\ 1545}#
                 #{mod\ 1546}#)
          ((lambda (#{tmp\ 1547}#)
             ((lambda (#{tmp\ 1548}#)
                (if #{tmp\ 1548}#
                  (apply (lambda (#{_\ 1549}# #{test\ 1550}# #{then\ 1551}#)
                           (#{build-conditional\ 97}#
                             #{s\ 1545}#
                             (#{chi\ 167}#
                               #{test\ 1550}#
                               #{r\ 1543}#
                               #{w\ 1544}#
                               #{mod\ 1546}#)
                             (#{chi\ 167}#
                               #{then\ 1551}#
                               #{r\ 1543}#
                               #{w\ 1544}#
                               #{mod\ 1546}#)
                             (#{build-void\ 95}# #f)))
                         #{tmp\ 1548}#)
                  ((lambda (#{tmp\ 1552}#)
                     (if #{tmp\ 1552}#
                       (apply (lambda (#{_\ 1553}#
                                       #{test\ 1554}#
                                       #{then\ 1555}#
                                       #{else\ 1556}#)
                                (#{build-conditional\ 97}#
                                  #{s\ 1545}#
                                  (#{chi\ 167}#
                                    #{test\ 1554}#
                                    #{r\ 1543}#
                                    #{w\ 1544}#
                                    #{mod\ 1546}#)
                                  (#{chi\ 167}#
                                    #{then\ 1555}#
                                    #{r\ 1543}#
                                    #{w\ 1544}#
                                    #{mod\ 1546}#)
                                  (#{chi\ 167}#
                                    #{else\ 1556}#
                                    #{r\ 1543}#
                                    #{w\ 1544}#
                                    #{mod\ 1546}#)))
                              #{tmp\ 1552}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1547}#)))
                   ($sc-dispatch
                     #{tmp\ 1547}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 1547}#
                '(any any any))))
           #{e\ 1542}#)))
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
        (letrec ((#{gen-syntax-case\ 1560}#
                   (lambda (#{x\ 1561}#
                            #{keys\ 1562}#
                            #{clauses\ 1563}#
                            #{r\ 1564}#
                            #{mod\ 1565}#)
                     (if (null? #{clauses\ 1563}#)
                       (#{build-application\ 96}#
                         #f
                         (#{build-primref\ 108}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 109}# #f #f)
                               (#{build-data\ 109}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 1561}#))
                       ((lambda (#{tmp\ 1566}#)
                          ((lambda (#{tmp\ 1567}#)
                             (if #{tmp\ 1567}#
                               (apply (lambda (#{pat\ 1568}# #{exp\ 1569}#)
                                        (if (if (#{id?\ 131}# #{pat\ 1568}#)
                                              (and-map
                                                (lambda (#{x\ 1570}#)
                                                  (not (#{free-id=?\ 154}#
                                                         #{pat\ 1568}#
                                                         #{x\ 1570}#)))
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
                                                      #{keys\ 1562}#))
                                              #f)
                                          (let ((#{labels\ 1571}#
                                                  (list (#{gen-label\ 136}#)))
                                                (#{var\ 1572}#
                                                  (#{gen-var\ 181}#
                                                    #{pat\ 1568}#)))
                                            (#{build-application\ 96}#
                                              #f
                                              (#{build-simple-lambda\ 105}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 1568}#))
                                                #f
                                                (list #{var\ 1572}#)
                                                #f
                                                (#{chi\ 167}#
                                                  #{exp\ 1569}#
                                                  (#{extend-env\ 125}#
                                                    #{labels\ 1571}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 1572}#
                                                                      0)))
                                                    #{r\ 1564}#)
                                                  (#{make-binding-wrap\ 148}#
                                                    (list #{pat\ 1568}#)
                                                    #{labels\ 1571}#
                                                    '(()))
                                                  #{mod\ 1565}#))
                                              (list #{x\ 1561}#)))
                                          (#{gen-clause\ 1559}#
                                            #{x\ 1561}#
                                            #{keys\ 1562}#
                                            (cdr #{clauses\ 1563}#)
                                            #{r\ 1564}#
                                            #{pat\ 1568}#
                                            #t
                                            #{exp\ 1569}#
                                            #{mod\ 1565}#)))
                                      #{tmp\ 1567}#)
                               ((lambda (#{tmp\ 1573}#)
                                  (if #{tmp\ 1573}#
                                    (apply (lambda (#{pat\ 1574}#
                                                    #{fender\ 1575}#
                                                    #{exp\ 1576}#)
                                             (#{gen-clause\ 1559}#
                                               #{x\ 1561}#
                                               #{keys\ 1562}#
                                               (cdr #{clauses\ 1563}#)
                                               #{r\ 1564}#
                                               #{pat\ 1574}#
                                               #{fender\ 1575}#
                                               #{exp\ 1576}#
                                               #{mod\ 1565}#))
                                           #{tmp\ 1573}#)
                                    ((lambda (#{_\ 1577}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 1563}#)))
                                     #{tmp\ 1566}#)))
                                ($sc-dispatch
                                  #{tmp\ 1566}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 1566}# (quote (any any)))))
                        (car #{clauses\ 1563}#)))))
                 (#{gen-clause\ 1559}#
                   (lambda (#{x\ 1578}#
                            #{keys\ 1579}#
                            #{clauses\ 1580}#
                            #{r\ 1581}#
                            #{pat\ 1582}#
                            #{fender\ 1583}#
                            #{exp\ 1584}#
                            #{mod\ 1585}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 1557}#
                           #{pat\ 1582}#
                           #{keys\ 1579}#))
                       (lambda (#{p\ 1586}# #{pvars\ 1587}#)
                         (if (not (#{distinct-bound-ids?\ 157}#
                                    (map car #{pvars\ 1587}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 1582}#)
                           (if (not (and-map
                                      (lambda (#{x\ 1588}#)
                                        (not (#{ellipsis?\ 175}#
                                               (car #{x\ 1588}#))))
                                      #{pvars\ 1587}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 1582}#)
                             (let ((#{y\ 1589}#
                                     (#{gen-var\ 181}# (quote tmp))))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 1589}#)
                                   #f
                                   (let ((#{y\ 1590}#
                                           (#{build-lexical-reference\ 98}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 1589}#)))
                                     (#{build-conditional\ 97}#
                                       #f
                                       ((lambda (#{tmp\ 1591}#)
                                          ((lambda (#{tmp\ 1592}#)
                                             (if #{tmp\ 1592}#
                                               (apply (lambda () #{y\ 1590}#)
                                                      #{tmp\ 1592}#)
                                               ((lambda (#{_\ 1593}#)
                                                  (#{build-conditional\ 97}#
                                                    #f
                                                    #{y\ 1590}#
                                                    (#{build-dispatch-call\ 1558}#
                                                      #{pvars\ 1587}#
                                                      #{fender\ 1583}#
                                                      #{y\ 1590}#
                                                      #{r\ 1581}#
                                                      #{mod\ 1585}#)
                                                    (#{build-data\ 109}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 1591}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1591}#
                                             '#(atom #t))))
                                        #{fender\ 1583}#)
                                       (#{build-dispatch-call\ 1558}#
                                         #{pvars\ 1587}#
                                         #{exp\ 1584}#
                                         #{y\ 1590}#
                                         #{r\ 1581}#
                                         #{mod\ 1585}#)
                                       (#{gen-syntax-case\ 1560}#
                                         #{x\ 1578}#
                                         #{keys\ 1579}#
                                         #{clauses\ 1580}#
                                         #{r\ 1581}#
                                         #{mod\ 1585}#))))
                                 (list (if (eq? #{p\ 1586}# (quote any))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             'list)
                                           (list #{x\ 1578}#))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 1578}#
                                                 (#{build-data\ 109}#
                                                   #f
                                                   #{p\ 1586}#)))))))))))))
                 (#{build-dispatch-call\ 1558}#
                   (lambda (#{pvars\ 1594}#
                            #{exp\ 1595}#
                            #{y\ 1596}#
                            #{r\ 1597}#
                            #{mod\ 1598}#)
                     (let ((#{ids\ 1599}# (map car #{pvars\ 1594}#))
                           (#{levels\ 1600}# (map cdr #{pvars\ 1594}#)))
                       (let ((#{labels\ 1601}#
                               (#{gen-labels\ 137}# #{ids\ 1599}#))
                             (#{new-vars\ 1602}#
                               (map #{gen-var\ 181}# #{ids\ 1599}#)))
                         (#{build-application\ 96}#
                           #f
                           (#{build-primref\ 108}# #f (quote apply))
                           (list (#{build-simple-lambda\ 105}#
                                   #f
                                   (map syntax->datum #{ids\ 1599}#)
                                   #f
                                   #{new-vars\ 1602}#
                                   #f
                                   (#{chi\ 167}#
                                     #{exp\ 1595}#
                                     (#{extend-env\ 125}#
                                       #{labels\ 1601}#
                                       (map (lambda (#{var\ 1603}#
                                                     #{level\ 1604}#)
                                              (cons 'syntax
                                                    (cons #{var\ 1603}#
                                                          #{level\ 1604}#)))
                                            #{new-vars\ 1602}#
                                            (map cdr #{pvars\ 1594}#))
                                       #{r\ 1597}#)
                                     (#{make-binding-wrap\ 148}#
                                       #{ids\ 1599}#
                                       #{labels\ 1601}#
                                       '(()))
                                     #{mod\ 1598}#))
                                 #{y\ 1596}#))))))
                 (#{convert-pattern\ 1557}#
                   (lambda (#{pattern\ 1605}# #{keys\ 1606}#)
                     (letrec ((#{cvt\ 1607}#
                                (lambda (#{p\ 1608}# #{n\ 1609}# #{ids\ 1610}#)
                                  (if (#{id?\ 131}# #{p\ 1608}#)
                                    (if (#{bound-id-member?\ 158}#
                                          #{p\ 1608}#
                                          #{keys\ 1606}#)
                                      (values
                                        (vector (quote free-id) #{p\ 1608}#)
                                        #{ids\ 1610}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 1608}# #{n\ 1609}#)
                                              #{ids\ 1610}#)))
                                    ((lambda (#{tmp\ 1611}#)
                                       ((lambda (#{tmp\ 1612}#)
                                          (if (if #{tmp\ 1612}#
                                                (apply (lambda (#{x\ 1613}#
                                                                #{dots\ 1614}#)
                                                         (#{ellipsis?\ 175}#
                                                           #{dots\ 1614}#))
                                                       #{tmp\ 1612}#)
                                                #f)
                                            (apply (lambda (#{x\ 1615}#
                                                            #{dots\ 1616}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 1607}#
                                                           #{x\ 1615}#
                                                           (#{fx+\ 86}#
                                                             #{n\ 1609}#
                                                             1)
                                                           #{ids\ 1610}#))
                                                       (lambda (#{p\ 1617}#
                                                                #{ids\ 1618}#)
                                                         (values
                                                           (if (eq? #{p\ 1617}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 1617}#))
                                                           #{ids\ 1618}#))))
                                                   #{tmp\ 1612}#)
                                            ((lambda (#{tmp\ 1619}#)
                                               (if #{tmp\ 1619}#
                                                 (apply (lambda (#{x\ 1620}#
                                                                 #{y\ 1621}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 1607}#
                                                                #{y\ 1621}#
                                                                #{n\ 1609}#
                                                                #{ids\ 1610}#))
                                                            (lambda (#{y\ 1622}#
                                                                     #{ids\ 1623}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 1607}#
                                                                    #{x\ 1620}#
                                                                    #{n\ 1609}#
                                                                    #{ids\ 1623}#))
                                                                (lambda (#{x\ 1624}#
                                                                         #{ids\ 1625}#)
                                                                  (values
                                                                    (cons #{x\ 1624}#
                                                                          #{y\ 1622}#)
                                                                    #{ids\ 1625}#))))))
                                                        #{tmp\ 1619}#)
                                                 ((lambda (#{tmp\ 1626}#)
                                                    (if #{tmp\ 1626}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 1610}#))
                                                             #{tmp\ 1626}#)
                                                      ((lambda (#{tmp\ 1627}#)
                                                         (if #{tmp\ 1627}#
                                                           (apply (lambda (#{x\ 1628}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 1607}#
                                                                          #{x\ 1628}#
                                                                          #{n\ 1609}#
                                                                          #{ids\ 1610}#))
                                                                      (lambda (#{p\ 1630}#
                                                                               #{ids\ 1631}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 1630}#)
                                                                          #{ids\ 1631}#))))
                                                                  #{tmp\ 1627}#)
                                                           ((lambda (#{x\ 1632}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 180}#
                                                                    #{p\ 1608}#
                                                                    '(())))
                                                                #{ids\ 1610}#))
                                                            #{tmp\ 1611}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1611}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 1611}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 1611}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 1611}#
                                          '(any any))))
                                     #{p\ 1608}#)))))
                       (#{cvt\ 1607}# #{pattern\ 1605}# 0 (quote ()))))))
          (lambda (#{e\ 1633}#
                   #{r\ 1634}#
                   #{w\ 1635}#
                   #{s\ 1636}#
                   #{mod\ 1637}#)
            (let ((#{e\ 1638}#
                    (#{source-wrap\ 160}#
                      #{e\ 1633}#
                      #{w\ 1635}#
                      #{s\ 1636}#
                      #{mod\ 1637}#)))
              ((lambda (#{tmp\ 1639}#)
                 ((lambda (#{tmp\ 1640}#)
                    (if #{tmp\ 1640}#
                      (apply (lambda (#{_\ 1641}#
                                      #{val\ 1642}#
                                      #{key\ 1643}#
                                      #{m\ 1644}#)
                               (if (and-map
                                     (lambda (#{x\ 1645}#)
                                       (if (#{id?\ 131}# #{x\ 1645}#)
                                         (not (#{ellipsis?\ 175}# #{x\ 1645}#))
                                         #f))
                                     #{key\ 1643}#)
                                 (let ((#{x\ 1647}#
                                         (#{gen-var\ 181}# (quote tmp))))
                                   (#{build-application\ 96}#
                                     #{s\ 1636}#
                                     (#{build-simple-lambda\ 105}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 1647}#)
                                       #f
                                       (#{gen-syntax-case\ 1560}#
                                         (#{build-lexical-reference\ 98}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 1647}#)
                                         #{key\ 1643}#
                                         #{m\ 1644}#
                                         #{r\ 1634}#
                                         #{mod\ 1637}#))
                                     (list (#{chi\ 167}#
                                             #{val\ 1642}#
                                             #{r\ 1634}#
                                             '(())
                                             #{mod\ 1637}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 1638}#)))
                             #{tmp\ 1640}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 1639}#)))
                  ($sc-dispatch
                    #{tmp\ 1639}#
                    '(any any each-any . each-any))))
               #{e\ 1638}#)))))
      (set! sc-expand
        (lambda (#{x\ 1650}# . #{rest\ 1651}#)
          (if (if (pair? #{x\ 1650}#)
                (equal? (car #{x\ 1650}#) #{noexpand\ 84}#)
                #f)
            (cadr #{x\ 1650}#)
            (let ((#{m\ 1652}#
                    (if (null? #{rest\ 1651}#)
                      'e
                      (car #{rest\ 1651}#)))
                  (#{esew\ 1653}#
                    (if (let ((#{t\ 1654}# (null? #{rest\ 1651}#)))
                          (if #{t\ 1654}#
                            #{t\ 1654}#
                            (null? (cdr #{rest\ 1651}#))))
                      '(eval)
                      (cadr #{rest\ 1651}#))))
              (with-fluid*
                #{*mode*\ 85}#
                #{m\ 1652}#
                (lambda ()
                  (#{chi-top\ 166}#
                    #{x\ 1650}#
                    '()
                    '((top))
                    #{m\ 1652}#
                    #{esew\ 1653}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 1655}#)
          (#{nonsymbol-id?\ 130}# #{x\ 1655}#)))
      (set! datum->syntax
        (lambda (#{id\ 1656}# #{datum\ 1657}#)
          (#{make-syntax-object\ 114}#
            #{datum\ 1657}#
            (#{syntax-object-wrap\ 117}# #{id\ 1656}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 1658}#)
          (#{strip\ 180}# #{x\ 1658}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 1659}#)
          (begin
            (let ((#{x\ 1660}# #{ls\ 1659}#))
              (if (not (list? #{x\ 1660}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 1660}#)))
            (map (lambda (#{x\ 1661}#)
                   (#{wrap\ 159}# (gensym) (quote ((top))) #f))
                 #{ls\ 1659}#))))
      (set! free-identifier=?
        (lambda (#{x\ 1662}# #{y\ 1663}#)
          (begin
            (let ((#{x\ 1664}# #{x\ 1662}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1664}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1664}#)))
            (let ((#{x\ 1665}# #{y\ 1663}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1665}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1665}#)))
            (#{free-id=?\ 154}# #{x\ 1662}# #{y\ 1663}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 1666}# #{y\ 1667}#)
          (begin
            (let ((#{x\ 1668}# #{x\ 1666}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1668}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1668}#)))
            (let ((#{x\ 1669}# #{y\ 1667}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1669}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1669}#)))
            (#{bound-id=?\ 155}# #{x\ 1666}# #{y\ 1667}#))))
      (set! syntax-violation
        (lambda (#{who\ 1670}#
                 #{message\ 1671}#
                 #{form\ 1672}#
                 .
                 #{subform\ 1673}#)
          (begin
            (let ((#{x\ 1674}# #{who\ 1670}#))
              (if (not ((lambda (#{x\ 1675}#)
                          (let ((#{t\ 1676}# (not #{x\ 1675}#)))
                            (if #{t\ 1676}#
                              #{t\ 1676}#
                              (let ((#{t\ 1677}# (string? #{x\ 1675}#)))
                                (if #{t\ 1677}#
                                  #{t\ 1677}#
                                  (symbol? #{x\ 1675}#))))))
                        #{x\ 1674}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1674}#)))
            (let ((#{x\ 1678}# #{message\ 1671}#))
              (if (not (string? #{x\ 1678}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1678}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 1670}# "~a: " "")
                "~a "
                (if (null? #{subform\ 1673}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 1679}#
                      (cons #{message\ 1671}#
                            (map (lambda (#{x\ 1680}#)
                                   (#{strip\ 180}# #{x\ 1680}# (quote (()))))
                                 (append
                                   #{subform\ 1673}#
                                   (list #{form\ 1672}#))))))
                (if #{who\ 1670}#
                  (cons #{who\ 1670}# #{tail\ 1679}#)
                  #{tail\ 1679}#))
              #f))))
      (letrec ((#{match\ 1685}#
                 (lambda (#{e\ 1686}#
                          #{p\ 1687}#
                          #{w\ 1688}#
                          #{r\ 1689}#
                          #{mod\ 1690}#)
                   (if (not #{r\ 1689}#)
                     #f
                     (if (eq? #{p\ 1687}# (quote any))
                       (cons (#{wrap\ 159}#
                               #{e\ 1686}#
                               #{w\ 1688}#
                               #{mod\ 1690}#)
                             #{r\ 1689}#)
                       (if (#{syntax-object?\ 115}# #{e\ 1686}#)
                         (#{match*\ 1684}#
                           (#{syntax-object-expression\ 116}# #{e\ 1686}#)
                           #{p\ 1687}#
                           (#{join-wraps\ 150}#
                             #{w\ 1688}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1686}#))
                           #{r\ 1689}#
                           (#{syntax-object-module\ 118}# #{e\ 1686}#))
                         (#{match*\ 1684}#
                           #{e\ 1686}#
                           #{p\ 1687}#
                           #{w\ 1688}#
                           #{r\ 1689}#
                           #{mod\ 1690}#))))))
               (#{match*\ 1684}#
                 (lambda (#{e\ 1691}#
                          #{p\ 1692}#
                          #{w\ 1693}#
                          #{r\ 1694}#
                          #{mod\ 1695}#)
                   (if (null? #{p\ 1692}#)
                     (if (null? #{e\ 1691}#) #{r\ 1694}# #f)
                     (if (pair? #{p\ 1692}#)
                       (if (pair? #{e\ 1691}#)
                         (#{match\ 1685}#
                           (car #{e\ 1691}#)
                           (car #{p\ 1692}#)
                           #{w\ 1693}#
                           (#{match\ 1685}#
                             (cdr #{e\ 1691}#)
                             (cdr #{p\ 1692}#)
                             #{w\ 1693}#
                             #{r\ 1694}#
                             #{mod\ 1695}#)
                           #{mod\ 1695}#)
                         #f)
                       (if (eq? #{p\ 1692}# (quote each-any))
                         (let ((#{l\ 1696}#
                                 (#{match-each-any\ 1682}#
                                   #{e\ 1691}#
                                   #{w\ 1693}#
                                   #{mod\ 1695}#)))
                           (if #{l\ 1696}#
                             (cons #{l\ 1696}# #{r\ 1694}#)
                             #f))
                         (let ((#{atom-key\ 1697}# (vector-ref #{p\ 1692}# 0)))
                           (if (memv #{atom-key\ 1697}# (quote (each)))
                             (if (null? #{e\ 1691}#)
                               (#{match-empty\ 1683}#
                                 (vector-ref #{p\ 1692}# 1)
                                 #{r\ 1694}#)
                               (let ((#{l\ 1698}#
                                       (#{match-each\ 1681}#
                                         #{e\ 1691}#
                                         (vector-ref #{p\ 1692}# 1)
                                         #{w\ 1693}#
                                         #{mod\ 1695}#)))
                                 (if #{l\ 1698}#
                                   (letrec ((#{collect\ 1699}#
                                              (lambda (#{l\ 1700}#)
                                                (if (null? (car #{l\ 1700}#))
                                                  #{r\ 1694}#
                                                  (cons (map car #{l\ 1700}#)
                                                        (#{collect\ 1699}#
                                                          (map cdr
                                                               #{l\ 1700}#)))))))
                                     (#{collect\ 1699}# #{l\ 1698}#))
                                   #f)))
                             (if (memv #{atom-key\ 1697}# (quote (free-id)))
                               (if (#{id?\ 131}# #{e\ 1691}#)
                                 (if (#{free-id=?\ 154}#
                                       (#{wrap\ 159}#
                                         #{e\ 1691}#
                                         #{w\ 1693}#
                                         #{mod\ 1695}#)
                                       (vector-ref #{p\ 1692}# 1))
                                   #{r\ 1694}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 1697}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 1692}# 1)
                                       (#{strip\ 180}#
                                         #{e\ 1691}#
                                         #{w\ 1693}#))
                                   #{r\ 1694}#
                                   #f)
                                 (if (memv #{atom-key\ 1697}# (quote (vector)))
                                   (if (vector? #{e\ 1691}#)
                                     (#{match\ 1685}#
                                       (vector->list #{e\ 1691}#)
                                       (vector-ref #{p\ 1692}# 1)
                                       #{w\ 1693}#
                                       #{r\ 1694}#
                                       #{mod\ 1695}#)
                                     #f)))))))))))
               (#{match-empty\ 1683}#
                 (lambda (#{p\ 1701}# #{r\ 1702}#)
                   (if (null? #{p\ 1701}#)
                     #{r\ 1702}#
                     (if (eq? #{p\ 1701}# (quote any))
                       (cons (quote ()) #{r\ 1702}#)
                       (if (pair? #{p\ 1701}#)
                         (#{match-empty\ 1683}#
                           (car #{p\ 1701}#)
                           (#{match-empty\ 1683}#
                             (cdr #{p\ 1701}#)
                             #{r\ 1702}#))
                         (if (eq? #{p\ 1701}# (quote each-any))
                           (cons (quote ()) #{r\ 1702}#)
                           (let ((#{atom-key\ 1703}#
                                   (vector-ref #{p\ 1701}# 0)))
                             (if (memv #{atom-key\ 1703}# (quote (each)))
                               (#{match-empty\ 1683}#
                                 (vector-ref #{p\ 1701}# 1)
                                 #{r\ 1702}#)
                               (if (memv #{atom-key\ 1703}#
                                         '(free-id atom))
                                 #{r\ 1702}#
                                 (if (memv #{atom-key\ 1703}# (quote (vector)))
                                   (#{match-empty\ 1683}#
                                     (vector-ref #{p\ 1701}# 1)
                                     #{r\ 1702}#)))))))))))
               (#{match-each-any\ 1682}#
                 (lambda (#{e\ 1704}# #{w\ 1705}# #{mod\ 1706}#)
                   (if (pair? #{e\ 1704}#)
                     (let ((#{l\ 1707}#
                             (#{match-each-any\ 1682}#
                               (cdr #{e\ 1704}#)
                               #{w\ 1705}#
                               #{mod\ 1706}#)))
                       (if #{l\ 1707}#
                         (cons (#{wrap\ 159}#
                                 (car #{e\ 1704}#)
                                 #{w\ 1705}#
                                 #{mod\ 1706}#)
                               #{l\ 1707}#)
                         #f))
                     (if (null? #{e\ 1704}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1704}#)
                         (#{match-each-any\ 1682}#
                           (#{syntax-object-expression\ 116}# #{e\ 1704}#)
                           (#{join-wraps\ 150}#
                             #{w\ 1705}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1704}#))
                           #{mod\ 1706}#)
                         #f)))))
               (#{match-each\ 1681}#
                 (lambda (#{e\ 1708}#
                          #{p\ 1709}#
                          #{w\ 1710}#
                          #{mod\ 1711}#)
                   (if (pair? #{e\ 1708}#)
                     (let ((#{first\ 1712}#
                             (#{match\ 1685}#
                               (car #{e\ 1708}#)
                               #{p\ 1709}#
                               #{w\ 1710}#
                               '()
                               #{mod\ 1711}#)))
                       (if #{first\ 1712}#
                         (let ((#{rest\ 1713}#
                                 (#{match-each\ 1681}#
                                   (cdr #{e\ 1708}#)
                                   #{p\ 1709}#
                                   #{w\ 1710}#
                                   #{mod\ 1711}#)))
                           (if #{rest\ 1713}#
                             (cons #{first\ 1712}# #{rest\ 1713}#)
                             #f))
                         #f))
                     (if (null? #{e\ 1708}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1708}#)
                         (#{match-each\ 1681}#
                           (#{syntax-object-expression\ 116}# #{e\ 1708}#)
                           #{p\ 1709}#
                           (#{join-wraps\ 150}#
                             #{w\ 1710}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1708}#))
                           (#{syntax-object-module\ 118}# #{e\ 1708}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 1714}# #{p\ 1715}#)
            (if (eq? #{p\ 1715}# (quote any))
              (list #{e\ 1714}#)
              (if (#{syntax-object?\ 115}# #{e\ 1714}#)
                (#{match*\ 1684}#
                  (#{syntax-object-expression\ 116}# #{e\ 1714}#)
                  #{p\ 1715}#
                  (#{syntax-object-wrap\ 117}# #{e\ 1714}#)
                  '()
                  (#{syntax-object-module\ 118}# #{e\ 1714}#))
                (#{match*\ 1684}#
                  #{e\ 1714}#
                  #{p\ 1715}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1716}#)
      ((lambda (#{tmp\ 1717}#)
         ((lambda (#{tmp\ 1718}#)
            (if #{tmp\ 1718}#
              (apply (lambda (#{_\ 1719}# #{e1\ 1720}# #{e2\ 1721}#)
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
                             (cons #{e1\ 1720}# #{e2\ 1721}#)))
                     #{tmp\ 1718}#)
              ((lambda (#{tmp\ 1723}#)
                 (if #{tmp\ 1723}#
                   (apply (lambda (#{_\ 1724}#
                                   #{out\ 1725}#
                                   #{in\ 1726}#
                                   #{e1\ 1727}#
                                   #{e2\ 1728}#)
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
                                  #{in\ 1726}#
                                  '()
                                  (list #{out\ 1725}#
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
                                              (cons #{e1\ 1727}#
                                                    #{e2\ 1728}#)))))
                          #{tmp\ 1723}#)
                   ((lambda (#{tmp\ 1730}#)
                      (if #{tmp\ 1730}#
                        (apply (lambda (#{_\ 1731}#
                                        #{out\ 1732}#
                                        #{in\ 1733}#
                                        #{e1\ 1734}#
                                        #{e2\ 1735}#)
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
                                             #{in\ 1733}#)
                                       '()
                                       (list #{out\ 1732}#
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
                                                   (cons #{e1\ 1734}#
                                                         #{e2\ 1735}#)))))
                               #{tmp\ 1730}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 1717}#)))
                    ($sc-dispatch
                      #{tmp\ 1717}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 1717}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 1717}#
            '(any () any . each-any))))
       #{x\ 1716}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1739}#)
      ((lambda (#{tmp\ 1740}#)
         ((lambda (#{tmp\ 1741}#)
            (if #{tmp\ 1741}#
              (apply (lambda (#{_\ 1742}#
                              #{k\ 1743}#
                              #{keyword\ 1744}#
                              #{pattern\ 1745}#
                              #{template\ 1746}#)
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
                                         (cons #{k\ 1743}#
                                               (map (lambda (#{tmp\ 1749}#
                                                             #{tmp\ 1748}#)
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
                                                                  #{tmp\ 1748}#)
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
                                                                  #{tmp\ 1749}#)))
                                                    #{template\ 1746}#
                                                    #{pattern\ 1745}#))))))
                     #{tmp\ 1741}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1740}#)))
          ($sc-dispatch
            #{tmp\ 1740}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 1739}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 1750}#)
      ((lambda (#{tmp\ 1751}#)
         ((lambda (#{tmp\ 1752}#)
            (if (if #{tmp\ 1752}#
                  (apply (lambda (#{let*\ 1753}#
                                  #{x\ 1754}#
                                  #{v\ 1755}#
                                  #{e1\ 1756}#
                                  #{e2\ 1757}#)
                           (and-map identifier? #{x\ 1754}#))
                         #{tmp\ 1752}#)
                  #f)
              (apply (lambda (#{let*\ 1759}#
                              #{x\ 1760}#
                              #{v\ 1761}#
                              #{e1\ 1762}#
                              #{e2\ 1763}#)
                       (letrec ((#{f\ 1764}#
                                  (lambda (#{bindings\ 1765}#)
                                    (if (null? #{bindings\ 1765}#)
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
                                                  (cons #{e1\ 1762}#
                                                        #{e2\ 1763}#)))
                                      ((lambda (#{tmp\ 1769}#)
                                         ((lambda (#{tmp\ 1770}#)
                                            (if #{tmp\ 1770}#
                                              (apply (lambda (#{body\ 1771}#
                                                              #{binding\ 1772}#)
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
                                                             (list #{binding\ 1772}#)
                                                             #{body\ 1771}#))
                                                     #{tmp\ 1770}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 1769}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1769}#
                                            '(any any))))
                                       (list (#{f\ 1764}#
                                               (cdr #{bindings\ 1765}#))
                                             (car #{bindings\ 1765}#)))))))
                         (#{f\ 1764}# (map list #{x\ 1760}# #{v\ 1761}#))))
                     #{tmp\ 1752}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1751}#)))
          ($sc-dispatch
            #{tmp\ 1751}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 1750}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 1773}#)
      ((lambda (#{tmp\ 1774}#)
         ((lambda (#{tmp\ 1775}#)
            (if #{tmp\ 1775}#
              (apply (lambda (#{_\ 1776}#
                              #{var\ 1777}#
                              #{init\ 1778}#
                              #{step\ 1779}#
                              #{e0\ 1780}#
                              #{e1\ 1781}#
                              #{c\ 1782}#)
                       ((lambda (#{tmp\ 1783}#)
                          ((lambda (#{tmp\ 1784}#)
                             (if #{tmp\ 1784}#
                               (apply (lambda (#{step\ 1785}#)
                                        ((lambda (#{tmp\ 1786}#)
                                           ((lambda (#{tmp\ 1787}#)
                                              (if #{tmp\ 1787}#
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
                                                                    #{var\ 1777}#
                                                                    #{init\ 1778}#)
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
                                                                           #{e0\ 1780}#)
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
                                                                             #{c\ 1782}#
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
                                                                                         #{step\ 1785}#)))))))
                                                       #{tmp\ 1787}#)
                                                ((lambda (#{tmp\ 1792}#)
                                                   (if #{tmp\ 1792}#
                                                     (apply (lambda (#{e1\ 1793}#
                                                                     #{e2\ 1794}#)
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
                                                                         #{var\ 1777}#
                                                                         #{init\ 1778}#)
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
                                                                          #{e0\ 1780}#
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
                                                                                (cons #{e1\ 1793}#
                                                                                      #{e2\ 1794}#))
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
                                                                                  #{c\ 1782}#
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
                                                                                              #{step\ 1785}#)))))))
                                                            #{tmp\ 1792}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1786}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1786}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 1786}#
                                              '())))
                                         #{e1\ 1781}#))
                                      #{tmp\ 1784}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 1783}#)))
                           ($sc-dispatch #{tmp\ 1783}# (quote each-any))))
                        (map (lambda (#{v\ 1801}# #{s\ 1802}#)
                               ((lambda (#{tmp\ 1803}#)
                                  ((lambda (#{tmp\ 1804}#)
                                     (if #{tmp\ 1804}#
                                       (apply (lambda () #{v\ 1801}#)
                                              #{tmp\ 1804}#)
                                       ((lambda (#{tmp\ 1805}#)
                                          (if #{tmp\ 1805}#
                                            (apply (lambda (#{e\ 1806}#)
                                                     #{e\ 1806}#)
                                                   #{tmp\ 1805}#)
                                            ((lambda (#{_\ 1807}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 1773}#
                                                 #{s\ 1802}#))
                                             #{tmp\ 1803}#)))
                                        ($sc-dispatch
                                          #{tmp\ 1803}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 1803}# (quote ()))))
                                #{s\ 1802}#))
                             #{var\ 1777}#
                             #{step\ 1779}#)))
                     #{tmp\ 1775}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1774}#)))
          ($sc-dispatch
            #{tmp\ 1774}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 1773}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 1810}#
               (lambda (#{x\ 1814}# #{y\ 1815}#)
                 ((lambda (#{tmp\ 1816}#)
                    ((lambda (#{tmp\ 1817}#)
                       (if #{tmp\ 1817}#
                         (apply (lambda (#{x\ 1818}# #{y\ 1819}#)
                                  ((lambda (#{tmp\ 1820}#)
                                     ((lambda (#{tmp\ 1821}#)
                                        (if #{tmp\ 1821}#
                                          (apply (lambda (#{dy\ 1822}#)
                                                   ((lambda (#{tmp\ 1823}#)
                                                      ((lambda (#{tmp\ 1824}#)
                                                         (if #{tmp\ 1824}#
                                                           (apply (lambda (#{dx\ 1825}#)
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
                                                                          (cons #{dx\ 1825}#
                                                                                #{dy\ 1822}#)))
                                                                  #{tmp\ 1824}#)
                                                           ((lambda (#{_\ 1826}#)
                                                              (if (null? #{dy\ 1822}#)
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
                                                                      #{x\ 1818}#)
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
                                                                      #{x\ 1818}#
                                                                      #{y\ 1819}#)))
                                                            #{tmp\ 1823}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1823}#
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
                                                    #{x\ 1818}#))
                                                 #{tmp\ 1821}#)
                                          ((lambda (#{tmp\ 1827}#)
                                             (if #{tmp\ 1827}#
                                               (apply (lambda (#{stuff\ 1828}#)
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
                                                              (cons #{x\ 1818}#
                                                                    #{stuff\ 1828}#)))
                                                      #{tmp\ 1827}#)
                                               ((lambda (#{else\ 1829}#)
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
                                                        #{x\ 1818}#
                                                        #{y\ 1819}#))
                                                #{tmp\ 1820}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1820}#
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
                                        #{tmp\ 1820}#
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
                                   #{y\ 1819}#))
                                #{tmp\ 1817}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1816}#)))
                     ($sc-dispatch #{tmp\ 1816}# (quote (any any)))))
                  (list #{x\ 1814}# #{y\ 1815}#))))
             (#{quasiappend\ 1811}#
               (lambda (#{x\ 1830}# #{y\ 1831}#)
                 ((lambda (#{tmp\ 1832}#)
                    ((lambda (#{tmp\ 1833}#)
                       (if #{tmp\ 1833}#
                         (apply (lambda (#{x\ 1834}# #{y\ 1835}#)
                                  ((lambda (#{tmp\ 1836}#)
                                     ((lambda (#{tmp\ 1837}#)
                                        (if #{tmp\ 1837}#
                                          (apply (lambda () #{x\ 1834}#)
                                                 #{tmp\ 1837}#)
                                          ((lambda (#{_\ 1838}#)
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
                                                   #{x\ 1834}#
                                                   #{y\ 1835}#))
                                           #{tmp\ 1836}#)))
                                      ($sc-dispatch
                                        #{tmp\ 1836}#
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
                                   #{y\ 1835}#))
                                #{tmp\ 1833}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1832}#)))
                     ($sc-dispatch #{tmp\ 1832}# (quote (any any)))))
                  (list #{x\ 1830}# #{y\ 1831}#))))
             (#{quasivector\ 1812}#
               (lambda (#{x\ 1839}#)
                 ((lambda (#{tmp\ 1840}#)
                    ((lambda (#{x\ 1841}#)
                       ((lambda (#{tmp\ 1842}#)
                          ((lambda (#{tmp\ 1843}#)
                             (if #{tmp\ 1843}#
                               (apply (lambda (#{x\ 1844}#)
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
                                              (list->vector #{x\ 1844}#)))
                                      #{tmp\ 1843}#)
                               ((lambda (#{tmp\ 1846}#)
                                  (if #{tmp\ 1846}#
                                    (apply (lambda (#{x\ 1847}#)
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
                                                   #{x\ 1847}#))
                                           #{tmp\ 1846}#)
                                    ((lambda (#{_\ 1849}#)
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
                                             #{x\ 1841}#))
                                     #{tmp\ 1842}#)))
                                ($sc-dispatch
                                  #{tmp\ 1842}#
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
                             #{tmp\ 1842}#
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
                        #{x\ 1841}#))
                     #{tmp\ 1840}#))
                  #{x\ 1839}#)))
             (#{quasi\ 1813}#
               (lambda (#{p\ 1850}# #{lev\ 1851}#)
                 ((lambda (#{tmp\ 1852}#)
                    ((lambda (#{tmp\ 1853}#)
                       (if #{tmp\ 1853}#
                         (apply (lambda (#{p\ 1854}#)
                                  (if (= #{lev\ 1851}# 0)
                                    #{p\ 1854}#
                                    (#{quasicons\ 1810}#
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
                                      (#{quasi\ 1813}#
                                        (list #{p\ 1854}#)
                                        (- #{lev\ 1851}# 1)))))
                                #{tmp\ 1853}#)
                         ((lambda (#{tmp\ 1855}#)
                            (if (if #{tmp\ 1855}#
                                  (apply (lambda (#{args\ 1856}#)
                                           (= #{lev\ 1851}# 0))
                                         #{tmp\ 1855}#)
                                  #f)
                              (apply (lambda (#{args\ 1857}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 1850}#
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
                                               #{args\ 1857}#)))
                                     #{tmp\ 1855}#)
                              ((lambda (#{tmp\ 1858}#)
                                 (if #{tmp\ 1858}#
                                   (apply (lambda (#{p\ 1859}# #{q\ 1860}#)
                                            (if (= #{lev\ 1851}# 0)
                                              (#{quasiappend\ 1811}#
                                                #{p\ 1859}#
                                                (#{quasi\ 1813}#
                                                  #{q\ 1860}#
                                                  #{lev\ 1851}#))
                                              (#{quasicons\ 1810}#
                                                (#{quasicons\ 1810}#
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
                                                  (#{quasi\ 1813}#
                                                    (list #{p\ 1859}#)
                                                    (- #{lev\ 1851}# 1)))
                                                (#{quasi\ 1813}#
                                                  #{q\ 1860}#
                                                  #{lev\ 1851}#))))
                                          #{tmp\ 1858}#)
                                   ((lambda (#{tmp\ 1861}#)
                                      (if (if #{tmp\ 1861}#
                                            (apply (lambda (#{args\ 1862}#
                                                            #{q\ 1863}#)
                                                     (= #{lev\ 1851}# 0))
                                                   #{tmp\ 1861}#)
                                            #f)
                                        (apply (lambda (#{args\ 1864}#
                                                        #{q\ 1865}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 1850}#
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
                                                         #{args\ 1864}#)))
                                               #{tmp\ 1861}#)
                                        ((lambda (#{tmp\ 1866}#)
                                           (if #{tmp\ 1866}#
                                             (apply (lambda (#{p\ 1867}#)
                                                      (#{quasicons\ 1810}#
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
                                                        (#{quasi\ 1813}#
                                                          (list #{p\ 1867}#)
                                                          (+ #{lev\ 1851}#
                                                             1))))
                                                    #{tmp\ 1866}#)
                                             ((lambda (#{tmp\ 1868}#)
                                                (if #{tmp\ 1868}#
                                                  (apply (lambda (#{p\ 1869}#
                                                                  #{q\ 1870}#)
                                                           (#{quasicons\ 1810}#
                                                             (#{quasi\ 1813}#
                                                               #{p\ 1869}#
                                                               #{lev\ 1851}#)
                                                             (#{quasi\ 1813}#
                                                               #{q\ 1870}#
                                                               #{lev\ 1851}#)))
                                                         #{tmp\ 1868}#)
                                                  ((lambda (#{tmp\ 1871}#)
                                                     (if #{tmp\ 1871}#
                                                       (apply (lambda (#{x\ 1872}#)
                                                                (#{quasivector\ 1812}#
                                                                  (#{quasi\ 1813}#
                                                                    #{x\ 1872}#
                                                                    #{lev\ 1851}#)))
                                                              #{tmp\ 1871}#)
                                                       ((lambda (#{p\ 1874}#)
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
                                                                #{p\ 1874}#))
                                                        #{tmp\ 1852}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 1852}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 1852}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 1852}#
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
                                      #{tmp\ 1852}#
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
                                 #{tmp\ 1852}#
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
                            #{tmp\ 1852}#
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
                       #{tmp\ 1852}#
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
                  #{p\ 1850}#))))
      (lambda (#{x\ 1875}#)
        ((lambda (#{tmp\ 1876}#)
           ((lambda (#{tmp\ 1877}#)
              (if #{tmp\ 1877}#
                (apply (lambda (#{_\ 1878}# #{e\ 1879}#)
                         (#{quasi\ 1813}# #{e\ 1879}# 0))
                       #{tmp\ 1877}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1876}#)))
            ($sc-dispatch #{tmp\ 1876}# (quote (any any)))))
         #{x\ 1875}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1880}#)
      (letrec ((#{read-file\ 1881}#
                 (lambda (#{fn\ 1882}# #{k\ 1883}#)
                   (let ((#{p\ 1884}# (open-input-file #{fn\ 1882}#)))
                     (letrec ((#{f\ 1885}#
                                (lambda (#{x\ 1886}#)
                                  (if (eof-object? #{x\ 1886}#)
                                    (begin
                                      (close-input-port #{p\ 1884}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 1883}#
                                            #{x\ 1886}#)
                                          (#{f\ 1885}# (read #{p\ 1884}#)))))))
                       (#{f\ 1885}# (read #{p\ 1884}#)))))))
        ((lambda (#{tmp\ 1887}#)
           ((lambda (#{tmp\ 1888}#)
              (if #{tmp\ 1888}#
                (apply (lambda (#{k\ 1889}# #{filename\ 1890}#)
                         (let ((#{fn\ 1891}#
                                 (syntax->datum #{filename\ 1890}#)))
                           ((lambda (#{tmp\ 1892}#)
                              ((lambda (#{tmp\ 1893}#)
                                 (if #{tmp\ 1893}#
                                   (apply (lambda (#{exp\ 1894}#)
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
                                                  #{exp\ 1894}#))
                                          #{tmp\ 1893}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 1892}#)))
                               ($sc-dispatch #{tmp\ 1892}# (quote each-any))))
                            (#{read-file\ 1881}# #{fn\ 1891}# #{k\ 1889}#))))
                       #{tmp\ 1888}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1887}#)))
            ($sc-dispatch #{tmp\ 1887}# (quote (any any)))))
         #{x\ 1880}#)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1896}#)
      ((lambda (#{tmp\ 1897}#)
         ((lambda (#{tmp\ 1898}#)
            (if #{tmp\ 1898}#
              (apply (lambda (#{_\ 1899}# #{e\ 1900}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 1896}#))
                     #{tmp\ 1898}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1897}#)))
          ($sc-dispatch #{tmp\ 1897}# (quote (any any)))))
       #{x\ 1896}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1901}#)
      ((lambda (#{tmp\ 1902}#)
         ((lambda (#{tmp\ 1903}#)
            (if #{tmp\ 1903}#
              (apply (lambda (#{_\ 1904}# #{e\ 1905}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 1901}#))
                     #{tmp\ 1903}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1902}#)))
          ($sc-dispatch #{tmp\ 1902}# (quote (any any)))))
       #{x\ 1901}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 1906}#)
      ((lambda (#{tmp\ 1907}#)
         ((lambda (#{tmp\ 1908}#)
            (if #{tmp\ 1908}#
              (apply (lambda (#{_\ 1909}#
                              #{e\ 1910}#
                              #{m1\ 1911}#
                              #{m2\ 1912}#)
                       ((lambda (#{tmp\ 1913}#)
                          ((lambda (#{body\ 1914}#)
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
                                               #{e\ 1910}#))
                                   #{body\ 1914}#))
                           #{tmp\ 1913}#))
                        (letrec ((#{f\ 1915}#
                                   (lambda (#{clause\ 1916}# #{clauses\ 1917}#)
                                     (if (null? #{clauses\ 1917}#)
                                       ((lambda (#{tmp\ 1919}#)
                                          ((lambda (#{tmp\ 1920}#)
                                             (if #{tmp\ 1920}#
                                               (apply (lambda (#{e1\ 1921}#
                                                               #{e2\ 1922}#)
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
                                                              (cons #{e1\ 1921}#
                                                                    #{e2\ 1922}#)))
                                                      #{tmp\ 1920}#)
                                               ((lambda (#{tmp\ 1924}#)
                                                  (if #{tmp\ 1924}#
                                                    (apply (lambda (#{k\ 1925}#
                                                                    #{e1\ 1926}#
                                                                    #{e2\ 1927}#)
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
                                                                               #{k\ 1925}#))
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
                                                                         (cons #{e1\ 1926}#
                                                                               #{e2\ 1927}#))))
                                                           #{tmp\ 1924}#)
                                                    ((lambda (#{_\ 1930}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 1906}#
                                                         #{clause\ 1916}#))
                                                     #{tmp\ 1919}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 1919}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 1919}#
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
                                        #{clause\ 1916}#)
                                       ((lambda (#{tmp\ 1931}#)
                                          ((lambda (#{rest\ 1932}#)
                                             ((lambda (#{tmp\ 1933}#)
                                                ((lambda (#{tmp\ 1934}#)
                                                   (if #{tmp\ 1934}#
                                                     (apply (lambda (#{k\ 1935}#
                                                                     #{e1\ 1936}#
                                                                     #{e2\ 1937}#)
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
                                                                                #{k\ 1935}#))
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
                                                                          (cons #{e1\ 1936}#
                                                                                #{e2\ 1937}#))
                                                                    #{rest\ 1932}#))
                                                            #{tmp\ 1934}#)
                                                     ((lambda (#{_\ 1940}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 1906}#
                                                          #{clause\ 1916}#))
                                                      #{tmp\ 1933}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1933}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 1916}#))
                                           #{tmp\ 1931}#))
                                        (#{f\ 1915}#
                                          (car #{clauses\ 1917}#)
                                          (cdr #{clauses\ 1917}#)))))))
                          (#{f\ 1915}# #{m1\ 1911}# #{m2\ 1912}#))))
                     #{tmp\ 1908}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1907}#)))
          ($sc-dispatch
            #{tmp\ 1907}#
            '(any any any . each-any))))
       #{x\ 1906}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1941}#)
      ((lambda (#{tmp\ 1942}#)
         ((lambda (#{tmp\ 1943}#)
            (if #{tmp\ 1943}#
              (apply (lambda (#{_\ 1944}# #{e\ 1945}#)
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
                                               #{e\ 1945}#))
                                   (list (cons #{_\ 1944}#
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
                                               (cons #{e\ 1945}#
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
                     #{tmp\ 1943}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1942}#)))
          ($sc-dispatch #{tmp\ 1942}# (quote (any any)))))
       #{x\ 1941}#))))

(define define*
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1946}#)
      ((lambda (#{tmp\ 1947}#)
         ((lambda (#{tmp\ 1948}#)
            (if #{tmp\ 1948}#
              (apply (lambda (#{dummy\ 1949}#
                              #{id\ 1950}#
                              #{args\ 1951}#
                              #{b0\ 1952}#
                              #{b1\ 1953}#)
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
                             #{id\ 1950}#
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
                                   (cons #{args\ 1951}#
                                         (cons #{b0\ 1952}# #{b1\ 1953}#)))))
                     #{tmp\ 1948}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1947}#)))
          ($sc-dispatch
            #{tmp\ 1947}#
            '(any (any . any) any . each-any))))
       #{x\ 1946}#))))

