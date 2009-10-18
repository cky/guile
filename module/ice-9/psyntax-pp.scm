(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((#{and-map*\ 31}#
           (lambda (#{f\ 71}# #{first\ 70}# . #{rest\ 69}#)
             (let ((#{t\ 72}# (null? #{first\ 70}#)))
               (if #{t\ 72}#
                 #{t\ 72}#
                 (if (null? #{rest\ 69}#)
                   (letrec ((#{andmap\ 73}#
                              (lambda (#{first\ 74}#)
                                (let ((#{x\ 75}# (car #{first\ 74}#))
                                      (#{first\ 76}# (cdr #{first\ 74}#)))
                                  (if (null? #{first\ 76}#)
                                    (#{f\ 71}# #{x\ 75}#)
                                    (if (#{f\ 71}# #{x\ 75}#)
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
                                    (apply #{f\ 71}#
                                           (cons #{x\ 80}# #{xr\ 81}#))
                                    (if (apply #{f\ 71}#
                                               (cons #{x\ 80}# #{xr\ 81}#))
                                      (#{andmap\ 77}#
                                        #{first\ 82}#
                                        #{rest\ 83}#)
                                      #f))))))
                     (#{andmap\ 77}# #{first\ 70}# #{rest\ 69}#))))))))
  (letrec ((#{lambda-var-list\ 177}#
             (lambda (#{vars\ 301}#)
               (letrec ((#{lvl\ 302}#
                          (lambda (#{vars\ 303}# #{ls\ 304}# #{w\ 305}#)
                            (if (pair? #{vars\ 303}#)
                              (#{lvl\ 302}#
                                (cdr #{vars\ 303}#)
                                (cons (#{wrap\ 157}#
                                        (car #{vars\ 303}#)
                                        #{w\ 305}#
                                        #f)
                                      #{ls\ 304}#)
                                #{w\ 305}#)
                              (if (#{id?\ 129}# #{vars\ 303}#)
                                (cons (#{wrap\ 157}#
                                        #{vars\ 303}#
                                        #{w\ 305}#
                                        #f)
                                      #{ls\ 304}#)
                                (if (null? #{vars\ 303}#)
                                  #{ls\ 304}#
                                  (if (#{syntax-object?\ 113}# #{vars\ 303}#)
                                    (#{lvl\ 302}#
                                      (#{syntax-object-expression\ 114}#
                                        #{vars\ 303}#)
                                      #{ls\ 304}#
                                      (#{join-wraps\ 148}#
                                        #{w\ 305}#
                                        (#{syntax-object-wrap\ 115}#
                                          #{vars\ 303}#)))
                                    (cons #{vars\ 303}# #{ls\ 304}#))))))))
                 (#{lvl\ 302}#
                   #{vars\ 301}#
                   '()
                   '(())))))
           (#{gen-var\ 176}#
             (lambda (#{id\ 306}#)
               (let ((#{id\ 307}#
                       (if (#{syntax-object?\ 113}# #{id\ 306}#)
                         (#{syntax-object-expression\ 114}# #{id\ 306}#)
                         #{id\ 306}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 307}#) " ")))))
           (#{strip\ 175}#
             (lambda (#{x\ 308}# #{w\ 309}#)
               (if (memq 'top
                         (#{wrap-marks\ 132}# #{w\ 309}#))
                 #{x\ 308}#
                 (letrec ((#{f\ 310}# (lambda (#{x\ 311}#)
                                        (if (#{syntax-object?\ 113}#
                                              #{x\ 311}#)
                                          (#{strip\ 175}#
                                            (#{syntax-object-expression\ 114}#
                                              #{x\ 311}#)
                                            (#{syntax-object-wrap\ 115}#
                                              #{x\ 311}#))
                                          (if (pair? #{x\ 311}#)
                                            (let ((#{a\ 312}# (#{f\ 310}# (car #{x\ 311}#)))
                                                  (#{d\ 313}# (#{f\ 310}# (cdr #{x\ 311}#))))
                                              (if (if (eq? #{a\ 312}#
                                                           (car #{x\ 311}#))
                                                    (eq? #{d\ 313}#
                                                         (cdr #{x\ 311}#))
                                                    #f)
                                                #{x\ 311}#
                                                (cons #{a\ 312}# #{d\ 313}#)))
                                            (if (vector? #{x\ 311}#)
                                              (let ((#{old\ 314}#
                                                      (vector->list
                                                        #{x\ 311}#)))
                                                (let ((#{new\ 315}#
                                                        (map #{f\ 310}#
                                                             #{old\ 314}#)))
                                                  (if (#{and-map*\ 31}#
                                                        eq?
                                                        #{old\ 314}#
                                                        #{new\ 315}#)
                                                    #{x\ 311}#
                                                    (list->vector
                                                      #{new\ 315}#))))
                                              #{x\ 311}#))))))
                   (#{f\ 310}# #{x\ 308}#)))))
           (#{ellipsis?\ 174}#
             (lambda (#{x\ 316}#)
               (if (#{nonsymbol-id?\ 128}# #{x\ 316}#)
                 (#{free-id=?\ 152}#
                   #{x\ 316}#
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
                           ellipsis?
                           chi-void
                           eval-local-transformer
                           chi-local-syntax
                           chi-lambda-clause
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
                           build-lambda
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
                          "i"))
                       #(ribcage
                         (define-structure and-map*)
                         ((top) (top))
                         ("i" "i")))
                      (hygiene guile)))
                 #f)))
           (#{chi-void\ 173}#
             (lambda () (#{build-void\ 95}# #f)))
           (#{eval-local-transformer\ 172}#
             (lambda (#{expanded\ 317}# #{mod\ 318}#)
               (let ((#{p\ 319}# (#{local-eval-hook\ 91}#
                                   #{expanded\ 317}#
                                   #{mod\ 318}#)))
                 (if (procedure? #{p\ 319}#)
                   #{p\ 319}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 319}#)))))
           (#{chi-local-syntax\ 171}#
             (lambda (#{rec?\ 320}#
                      #{e\ 321}#
                      #{r\ 322}#
                      #{w\ 323}#
                      #{s\ 324}#
                      #{mod\ 325}#
                      #{k\ 326}#)
               ((lambda (#{tmp\ 327}#)
                  ((lambda (#{tmp\ 328}#)
                     (if #{tmp\ 328}#
                       (apply (lambda (#{_\ 329}#
                                       #{id\ 330}#
                                       #{val\ 331}#
                                       #{e1\ 332}#
                                       #{e2\ 333}#)
                                (let ((#{ids\ 334}# #{id\ 330}#))
                                  (if (not (#{valid-bound-ids?\ 154}#
                                             #{ids\ 334}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 321}#)
                                    (let ((#{labels\ 336}#
                                            (#{gen-labels\ 135}#
                                              #{ids\ 334}#)))
                                      (let ((#{new-w\ 337}#
                                              (#{make-binding-wrap\ 146}#
                                                #{ids\ 334}#
                                                #{labels\ 336}#
                                                #{w\ 323}#)))
                                        (#{k\ 326}# (cons #{e1\ 332}#
                                                          #{e2\ 333}#)
                                                    (#{extend-env\ 123}#
                                                      #{labels\ 336}#
                                                      (let ((#{w\ 339}# (if #{rec?\ 320}#
                                                                          #{new-w\ 337}#
                                                                          #{w\ 323}#))
                                                            (#{trans-r\ 340}#
                                                              (#{macros-only-env\ 125}#
                                                                #{r\ 322}#)))
                                                        (map (lambda (#{x\ 341}#)
                                                               (cons 'macro
                                                                     (#{eval-local-transformer\ 172}#
                                                                       (#{chi\ 165}#
                                                                         #{x\ 341}#
                                                                         #{trans-r\ 340}#
                                                                         #{w\ 339}#
                                                                         #{mod\ 325}#)
                                                                       #{mod\ 325}#)))
                                                             #{val\ 331}#))
                                                      #{r\ 322}#)
                                                    #{new-w\ 337}#
                                                    #{s\ 324}#
                                                    #{mod\ 325}#))))))
                              #{tmp\ 328}#)
                       ((lambda (#{_\ 343}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 158}#
                              #{e\ 321}#
                              #{w\ 323}#
                              #{s\ 324}#
                              #{mod\ 325}#)))
                        #{tmp\ 327}#)))
                   ($sc-dispatch
                     #{tmp\ 327}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 321}#)))
           (#{chi-lambda-clause\ 170}#
             (lambda (#{e\ 344}#
                      #{docstring\ 345}#
                      #{c\ 346}#
                      #{r\ 347}#
                      #{w\ 348}#
                      #{mod\ 349}#
                      #{k\ 350}#)
               ((lambda (#{tmp\ 351}#)
                  ((lambda (#{tmp\ 352}#)
                     (if (if #{tmp\ 352}#
                           (apply (lambda (#{args\ 353}#
                                           #{doc\ 354}#
                                           #{e1\ 355}#
                                           #{e2\ 356}#)
                                    (if (string? (syntax->datum #{doc\ 354}#))
                                      (not #{docstring\ 345}#)
                                      #f))
                                  #{tmp\ 352}#)
                           #f)
                       (apply (lambda (#{args\ 357}#
                                       #{doc\ 358}#
                                       #{e1\ 359}#
                                       #{e2\ 360}#)
                                (#{chi-lambda-clause\ 170}#
                                  #{e\ 344}#
                                  #{doc\ 358}#
                                  (cons #{args\ 357}#
                                        (cons #{e1\ 359}# #{e2\ 360}#))
                                  #{r\ 347}#
                                  #{w\ 348}#
                                  #{mod\ 349}#
                                  #{k\ 350}#))
                              #{tmp\ 352}#)
                       ((lambda (#{tmp\ 362}#)
                          (if #{tmp\ 362}#
                            (apply (lambda (#{id\ 363}#
                                            #{e1\ 364}#
                                            #{e2\ 365}#)
                                     (let ((#{ids\ 366}# #{id\ 363}#))
                                       (if (not (#{valid-bound-ids?\ 154}#
                                                  #{ids\ 366}#))
                                         (syntax-violation
                                           'lambda
                                           "invalid parameter list"
                                           #{e\ 344}#)
                                         (let ((#{labels\ 368}#
                                                 (#{gen-labels\ 135}#
                                                   #{ids\ 366}#))
                                               (#{new-vars\ 369}#
                                                 (map #{gen-var\ 176}#
                                                      #{ids\ 366}#)))
                                           (#{k\ 350}# (map syntax->datum
                                                            #{ids\ 366}#)
                                                       #{new-vars\ 369}#
                                                       (if #{docstring\ 345}#
                                                         (syntax->datum
                                                           #{docstring\ 345}#)
                                                         #f)
                                                       (#{chi-body\ 169}#
                                                         (cons #{e1\ 364}#
                                                               #{e2\ 365}#)
                                                         #{e\ 344}#
                                                         (#{extend-var-env\ 124}#
                                                           #{labels\ 368}#
                                                           #{new-vars\ 369}#
                                                           #{r\ 347}#)
                                                         (#{make-binding-wrap\ 146}#
                                                           #{ids\ 366}#
                                                           #{labels\ 368}#
                                                           #{w\ 348}#)
                                                         #{mod\ 349}#))))))
                                   #{tmp\ 362}#)
                            ((lambda (#{tmp\ 371}#)
                               (if #{tmp\ 371}#
                                 (apply (lambda (#{ids\ 372}#
                                                 #{e1\ 373}#
                                                 #{e2\ 374}#)
                                          (let ((#{old-ids\ 375}#
                                                  (#{lambda-var-list\ 177}#
                                                    #{ids\ 372}#)))
                                            (if (not (#{valid-bound-ids?\ 154}#
                                                       #{old-ids\ 375}#))
                                              (syntax-violation
                                                'lambda
                                                "invalid parameter list"
                                                #{e\ 344}#)
                                              (let ((#{labels\ 376}#
                                                      (#{gen-labels\ 135}#
                                                        #{old-ids\ 375}#))
                                                    (#{new-vars\ 377}#
                                                      (map #{gen-var\ 176}#
                                                           #{old-ids\ 375}#)))
                                                (#{k\ 350}# (letrec ((#{f\ 378}# (lambda (#{ls1\ 379}#
                                                                                          #{ls2\ 380}#)
                                                                                   (if (null? #{ls1\ 379}#)
                                                                                     (syntax->datum
                                                                                       #{ls2\ 380}#)
                                                                                     (#{f\ 378}# (cdr #{ls1\ 379}#)
                                                                                                 (cons (syntax->datum
                                                                                                         (car #{ls1\ 379}#))
                                                                                                       #{ls2\ 380}#))))))
                                                              (#{f\ 378}# (cdr #{old-ids\ 375}#)
                                                                          (car #{old-ids\ 375}#)))
                                                            (letrec ((#{f\ 381}# (lambda (#{ls1\ 382}#
                                                                                          #{ls2\ 383}#)
                                                                                   (if (null? #{ls1\ 382}#)
                                                                                     #{ls2\ 383}#
                                                                                     (#{f\ 381}# (cdr #{ls1\ 382}#)
                                                                                                 (cons (car #{ls1\ 382}#)
                                                                                                       #{ls2\ 383}#))))))
                                                              (#{f\ 381}# (cdr #{new-vars\ 377}#)
                                                                          (car #{new-vars\ 377}#)))
                                                            (if #{docstring\ 345}#
                                                              (syntax->datum
                                                                #{docstring\ 345}#)
                                                              #f)
                                                            (#{chi-body\ 169}#
                                                              (cons #{e1\ 373}#
                                                                    #{e2\ 374}#)
                                                              #{e\ 344}#
                                                              (#{extend-var-env\ 124}#
                                                                #{labels\ 376}#
                                                                #{new-vars\ 377}#
                                                                #{r\ 347}#)
                                                              (#{make-binding-wrap\ 146}#
                                                                #{old-ids\ 375}#
                                                                #{labels\ 376}#
                                                                #{w\ 348}#)
                                                              #{mod\ 349}#))))))
                                        #{tmp\ 371}#)
                                 ((lambda (#{_\ 385}#)
                                    (syntax-violation
                                      'lambda
                                      "bad lambda"
                                      #{e\ 344}#))
                                  #{tmp\ 351}#)))
                             ($sc-dispatch
                               #{tmp\ 351}#
                               '(any any . each-any)))))
                        ($sc-dispatch
                          #{tmp\ 351}#
                          '(each-any any . each-any)))))
                   ($sc-dispatch
                     #{tmp\ 351}#
                     '(any any any . each-any))))
                #{c\ 346}#)))
           (#{chi-body\ 169}#
             (lambda (#{body\ 386}#
                      #{outer-form\ 387}#
                      #{r\ 388}#
                      #{w\ 389}#
                      #{mod\ 390}#)
               (let ((#{r\ 391}# (cons '("placeholder" placeholder)
                                       #{r\ 388}#)))
                 (let ((#{ribcage\ 392}#
                         (#{make-ribcage\ 136}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 393}# (#{make-wrap\ 131}#
                                       (#{wrap-marks\ 132}# #{w\ 389}#)
                                       (cons #{ribcage\ 392}#
                                             (#{wrap-subst\ 133}#
                                               #{w\ 389}#)))))
                     (letrec ((#{parse\ 394}#
                                (lambda (#{body\ 395}#
                                         #{ids\ 396}#
                                         #{labels\ 397}#
                                         #{var-ids\ 398}#
                                         #{vars\ 399}#
                                         #{vals\ 400}#
                                         #{bindings\ 401}#)
                                  (if (null? #{body\ 395}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 387}#)
                                    (let ((#{e\ 403}# (cdar #{body\ 395}#))
                                          (#{er\ 404}# (caar #{body\ 395}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 163}#
                                            #{e\ 403}#
                                            #{er\ 404}#
                                            '(())
                                            (#{source-annotation\ 120}#
                                              #{er\ 404}#)
                                            #{ribcage\ 392}#
                                            #{mod\ 390}#
                                            #f))
                                        (lambda (#{type\ 405}#
                                                 #{value\ 406}#
                                                 #{e\ 407}#
                                                 #{w\ 408}#
                                                 #{s\ 409}#
                                                 #{mod\ 410}#)
                                          (if (memv #{type\ 405}#
                                                    '(define-form))
                                            (let ((#{id\ 411}#
                                                    (#{wrap\ 157}#
                                                      #{value\ 406}#
                                                      #{w\ 408}#
                                                      #{mod\ 410}#))
                                                  (#{label\ 412}#
                                                    (#{gen-label\ 134}#)))
                                              (let ((#{var\ 413}#
                                                      (#{gen-var\ 176}#
                                                        #{id\ 411}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 145}#
                                                    #{ribcage\ 392}#
                                                    #{id\ 411}#
                                                    #{label\ 412}#)
                                                  (#{parse\ 394}#
                                                    (cdr #{body\ 395}#)
                                                    (cons #{id\ 411}#
                                                          #{ids\ 396}#)
                                                    (cons #{label\ 412}#
                                                          #{labels\ 397}#)
                                                    (cons #{id\ 411}#
                                                          #{var-ids\ 398}#)
                                                    (cons #{var\ 413}#
                                                          #{vars\ 399}#)
                                                    (cons (cons #{er\ 404}#
                                                                (#{wrap\ 157}#
                                                                  #{e\ 407}#
                                                                  #{w\ 408}#
                                                                  #{mod\ 410}#))
                                                          #{vals\ 400}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 413}#)
                                                          #{bindings\ 401}#)))))
                                            (if (memv #{type\ 405}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 414}#
                                                      (#{wrap\ 157}#
                                                        #{value\ 406}#
                                                        #{w\ 408}#
                                                        #{mod\ 410}#))
                                                    (#{label\ 415}#
                                                      (#{gen-label\ 134}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 145}#
                                                    #{ribcage\ 392}#
                                                    #{id\ 414}#
                                                    #{label\ 415}#)
                                                  (#{parse\ 394}#
                                                    (cdr #{body\ 395}#)
                                                    (cons #{id\ 414}#
                                                          #{ids\ 396}#)
                                                    (cons #{label\ 415}#
                                                          #{labels\ 397}#)
                                                    #{var-ids\ 398}#
                                                    #{vars\ 399}#
                                                    #{vals\ 400}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 404}#
                                                                      (#{wrap\ 157}#
                                                                        #{e\ 407}#
                                                                        #{w\ 408}#
                                                                        #{mod\ 410}#)))
                                                          #{bindings\ 401}#))))
                                              (if (memv #{type\ 405}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 416}#)
                                                   ((lambda (#{tmp\ 417}#)
                                                      (if #{tmp\ 417}#
                                                        (apply (lambda (#{_\ 418}#
                                                                        #{e1\ 419}#)
                                                                 (#{parse\ 394}#
                                                                   (letrec ((#{f\ 420}# (lambda (#{forms\ 421}#)
                                                                                          (if (null? #{forms\ 421}#)
                                                                                            (cdr #{body\ 395}#)
                                                                                            (cons (cons #{er\ 404}#
                                                                                                        (#{wrap\ 157}#
                                                                                                          (car #{forms\ 421}#)
                                                                                                          #{w\ 408}#
                                                                                                          #{mod\ 410}#))
                                                                                                  (#{f\ 420}# (cdr #{forms\ 421}#)))))))
                                                                     (#{f\ 420}# #{e1\ 419}#))
                                                                   #{ids\ 396}#
                                                                   #{labels\ 397}#
                                                                   #{var-ids\ 398}#
                                                                   #{vars\ 399}#
                                                                   #{vals\ 400}#
                                                                   #{bindings\ 401}#))
                                                               #{tmp\ 417}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 416}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 416}#
                                                      '(any . each-any))))
                                                 #{e\ 407}#)
                                                (if (memv #{type\ 405}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 171}#
                                                    #{value\ 406}#
                                                    #{e\ 407}#
                                                    #{er\ 404}#
                                                    #{w\ 408}#
                                                    #{s\ 409}#
                                                    #{mod\ 410}#
                                                    (lambda (#{forms\ 423}#
                                                             #{er\ 424}#
                                                             #{w\ 425}#
                                                             #{s\ 426}#
                                                             #{mod\ 427}#)
                                                      (#{parse\ 394}#
                                                        (letrec ((#{f\ 428}# (lambda (#{forms\ 429}#)
                                                                               (if (null? #{forms\ 429}#)
                                                                                 (cdr #{body\ 395}#)
                                                                                 (cons (cons #{er\ 424}#
                                                                                             (#{wrap\ 157}#
                                                                                               (car #{forms\ 429}#)
                                                                                               #{w\ 425}#
                                                                                               #{mod\ 427}#))
                                                                                       (#{f\ 428}# (cdr #{forms\ 429}#)))))))
                                                          (#{f\ 428}# #{forms\ 423}#))
                                                        #{ids\ 396}#
                                                        #{labels\ 397}#
                                                        #{var-ids\ 398}#
                                                        #{vars\ 399}#
                                                        #{vals\ 400}#
                                                        #{bindings\ 401}#)))
                                                  (if (null? #{ids\ 396}#)
                                                    (#{build-sequence\ 108}#
                                                      #f
                                                      (map (lambda (#{x\ 430}#)
                                                             (#{chi\ 165}#
                                                               (cdr #{x\ 430}#)
                                                               (car #{x\ 430}#)
                                                               '(())
                                                               #{mod\ 410}#))
                                                           (cons (cons #{er\ 404}#
                                                                       (#{source-wrap\ 158}#
                                                                         #{e\ 407}#
                                                                         #{w\ 408}#
                                                                         #{s\ 409}#
                                                                         #{mod\ 410}#))
                                                                 (cdr #{body\ 395}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 154}#
                                                                 #{ids\ 396}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 387}#))
                                                      (letrec ((#{loop\ 431}#
                                                                 (lambda (#{bs\ 432}#
                                                                          #{er-cache\ 433}#
                                                                          #{r-cache\ 434}#)
                                                                   (if (not (null? #{bs\ 432}#))
                                                                     (let ((#{b\ 435}# (car #{bs\ 432}#)))
                                                                       (if (eq? (car #{b\ 435}#)
                                                                                'macro)
                                                                         (let ((#{er\ 436}#
                                                                                 (cadr #{b\ 435}#)))
                                                                           (let ((#{r-cache\ 437}#
                                                                                   (if (eq? #{er\ 436}#
                                                                                            #{er-cache\ 433}#)
                                                                                     #{r-cache\ 434}#
                                                                                     (#{macros-only-env\ 125}#
                                                                                       #{er\ 436}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 435}#
                                                                                 (#{eval-local-transformer\ 172}#
                                                                                   (#{chi\ 165}#
                                                                                     (cddr #{b\ 435}#)
                                                                                     #{r-cache\ 437}#
                                                                                     '(())
                                                                                     #{mod\ 410}#)
                                                                                   #{mod\ 410}#))
                                                                               (#{loop\ 431}#
                                                                                 (cdr #{bs\ 432}#)
                                                                                 #{er\ 436}#
                                                                                 #{r-cache\ 437}#))))
                                                                         (#{loop\ 431}#
                                                                           (cdr #{bs\ 432}#)
                                                                           #{er-cache\ 433}#
                                                                           #{r-cache\ 434}#)))))))
                                                        (#{loop\ 431}#
                                                          #{bindings\ 401}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 391}#
                                                        (#{extend-env\ 123}#
                                                          #{labels\ 397}#
                                                          #{bindings\ 401}#
                                                          (cdr #{r\ 391}#)))
                                                      (#{build-letrec\ 111}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 398}#)
                                                        #{vars\ 399}#
                                                        (map (lambda (#{x\ 438}#)
                                                               (#{chi\ 165}#
                                                                 (cdr #{x\ 438}#)
                                                                 (car #{x\ 438}#)
                                                                 '(())
                                                                 #{mod\ 410}#))
                                                             #{vals\ 400}#)
                                                        (#{build-sequence\ 108}#
                                                          #f
                                                          (map (lambda (#{x\ 439}#)
                                                                 (#{chi\ 165}#
                                                                   (cdr #{x\ 439}#)
                                                                   (car #{x\ 439}#)
                                                                   '(())
                                                                   #{mod\ 410}#))
                                                               (cons (cons #{er\ 404}#
                                                                           (#{source-wrap\ 158}#
                                                                             #{e\ 407}#
                                                                             #{w\ 408}#
                                                                             #{s\ 409}#
                                                                             #{mod\ 410}#))
                                                                     (cdr #{body\ 395}#))))))))))))))))))
                       (#{parse\ 394}#
                         (map (lambda (#{x\ 402}#)
                                (cons #{r\ 391}#
                                      (#{wrap\ 157}#
                                        #{x\ 402}#
                                        #{w\ 393}#
                                        #{mod\ 390}#)))
                              #{body\ 386}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 168}#
             (lambda (#{p\ 440}#
                      #{e\ 441}#
                      #{r\ 442}#
                      #{w\ 443}#
                      #{rib\ 444}#
                      #{mod\ 445}#)
               (letrec ((#{rebuild-macro-output\ 446}#
                          (lambda (#{x\ 447}# #{m\ 448}#)
                            (if (pair? #{x\ 447}#)
                              (cons (#{rebuild-macro-output\ 446}#
                                      (car #{x\ 447}#)
                                      #{m\ 448}#)
                                    (#{rebuild-macro-output\ 446}#
                                      (cdr #{x\ 447}#)
                                      #{m\ 448}#))
                              (if (#{syntax-object?\ 113}# #{x\ 447}#)
                                (let ((#{w\ 449}# (#{syntax-object-wrap\ 115}#
                                                    #{x\ 447}#)))
                                  (let ((#{ms\ 450}#
                                          (#{wrap-marks\ 132}# #{w\ 449}#))
                                        (#{s\ 451}# (#{wrap-subst\ 133}#
                                                      #{w\ 449}#)))
                                    (if (if (pair? #{ms\ 450}#)
                                          (eq? (car #{ms\ 450}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 112}#
                                        (#{syntax-object-expression\ 114}#
                                          #{x\ 447}#)
                                        (#{make-wrap\ 131}#
                                          (cdr #{ms\ 450}#)
                                          (if #{rib\ 444}#
                                            (cons #{rib\ 444}#
                                                  (cdr #{s\ 451}#))
                                            (cdr #{s\ 451}#)))
                                        (#{syntax-object-module\ 116}#
                                          #{x\ 447}#))
                                      (#{make-syntax-object\ 112}#
                                        (#{syntax-object-expression\ 114}#
                                          #{x\ 447}#)
                                        (#{make-wrap\ 131}#
                                          (cons #{m\ 448}# #{ms\ 450}#)
                                          (if #{rib\ 444}#
                                            (cons #{rib\ 444}#
                                                  (cons 'shift
                                                        #{s\ 451}#))
                                            (cons (quote shift) #{s\ 451}#)))
                                        (let ((#{pmod\ 452}#
                                                (procedure-module #{p\ 440}#)))
                                          (if #{pmod\ 452}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 452}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 447}#)
                                  (let ((#{n\ 453}# (vector-length
                                                      #{x\ 447}#)))
                                    (let ((#{v\ 454}# (make-vector
                                                        #{n\ 453}#)))
                                      (letrec ((#{loop\ 455}#
                                                 (lambda (#{i\ 456}#)
                                                   (if (#{fx=\ 88}#
                                                         #{i\ 456}#
                                                         #{n\ 453}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 454}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 454}#
                                                         #{i\ 456}#
                                                         (#{rebuild-macro-output\ 446}#
                                                           (vector-ref
                                                             #{x\ 447}#
                                                             #{i\ 456}#)
                                                           #{m\ 448}#))
                                                       (#{loop\ 455}#
                                                         (#{fx+\ 86}#
                                                           #{i\ 456}#
                                                           1)))))))
                                        (#{loop\ 455}# 0))))
                                  (if (symbol? #{x\ 447}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 158}#
                                        #{e\ 441}#
                                        #{w\ 443}#
                                        (#{wrap-subst\ 133}# #{w\ 443}#)
                                        #{mod\ 445}#)
                                      #{x\ 447}#)
                                    #{x\ 447}#)))))))
                 (#{rebuild-macro-output\ 446}#
                   (#{p\ 440}# (#{wrap\ 157}#
                                 #{e\ 441}#
                                 (#{anti-mark\ 144}# #{w\ 443}#)
                                 #{mod\ 445}#))
                   (string #\m)))))
           (#{chi-application\ 167}#
             (lambda (#{x\ 457}#
                      #{e\ 458}#
                      #{r\ 459}#
                      #{w\ 460}#
                      #{s\ 461}#
                      #{mod\ 462}#)
               ((lambda (#{tmp\ 463}#)
                  ((lambda (#{tmp\ 464}#)
                     (if #{tmp\ 464}#
                       (apply (lambda (#{e0\ 465}# #{e1\ 466}#)
                                (#{build-application\ 96}#
                                  #{s\ 461}#
                                  #{x\ 457}#
                                  (map (lambda (#{e\ 467}#)
                                         (#{chi\ 165}#
                                           #{e\ 467}#
                                           #{r\ 459}#
                                           #{w\ 460}#
                                           #{mod\ 462}#))
                                       #{e1\ 466}#)))
                              #{tmp\ 464}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 463}#)))
                   ($sc-dispatch
                     #{tmp\ 463}#
                     '(any . each-any))))
                #{e\ 458}#)))
           (#{chi-expr\ 166}#
             (lambda (#{type\ 469}#
                      #{value\ 470}#
                      #{e\ 471}#
                      #{r\ 472}#
                      #{w\ 473}#
                      #{s\ 474}#
                      #{mod\ 475}#)
               (if (memv #{type\ 469}# (quote (lexical)))
                 (#{build-lexical-reference\ 98}#
                   'value
                   #{s\ 474}#
                   #{e\ 471}#
                   #{value\ 470}#)
                 (if (memv #{type\ 469}# (quote (core core-form)))
                   (#{value\ 470}#
                     #{e\ 471}#
                     #{r\ 472}#
                     #{w\ 473}#
                     #{s\ 474}#
                     #{mod\ 475}#)
                   (if (memv #{type\ 469}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 470}# #{e\ 471}#))
                       (lambda (#{id\ 476}# #{mod\ 477}#)
                         (#{build-global-reference\ 101}#
                           #{s\ 474}#
                           #{id\ 476}#
                           #{mod\ 477}#)))
                     (if (memv #{type\ 469}# (quote (lexical-call)))
                       (#{chi-application\ 167}#
                         (#{build-lexical-reference\ 98}#
                           'fun
                           (#{source-annotation\ 120}# (car #{e\ 471}#))
                           (car #{e\ 471}#)
                           #{value\ 470}#)
                         #{e\ 471}#
                         #{r\ 472}#
                         #{w\ 473}#
                         #{s\ 474}#
                         #{mod\ 475}#)
                       (if (memv #{type\ 469}# (quote (global-call)))
                         (#{chi-application\ 167}#
                           (#{build-global-reference\ 101}#
                             (#{source-annotation\ 120}# (car #{e\ 471}#))
                             (if (#{syntax-object?\ 113}# #{value\ 470}#)
                               (#{syntax-object-expression\ 114}#
                                 #{value\ 470}#)
                               #{value\ 470}#)
                             (if (#{syntax-object?\ 113}# #{value\ 470}#)
                               (#{syntax-object-module\ 116}# #{value\ 470}#)
                               #{mod\ 475}#))
                           #{e\ 471}#
                           #{r\ 472}#
                           #{w\ 473}#
                           #{s\ 474}#
                           #{mod\ 475}#)
                         (if (memv #{type\ 469}# (quote (constant)))
                           (#{build-data\ 107}#
                             #{s\ 474}#
                             (#{strip\ 175}#
                               (#{source-wrap\ 158}#
                                 #{e\ 471}#
                                 #{w\ 473}#
                                 #{s\ 474}#
                                 #{mod\ 475}#)
                               '(())))
                           (if (memv #{type\ 469}# (quote (global)))
                             (#{build-global-reference\ 101}#
                               #{s\ 474}#
                               #{value\ 470}#
                               #{mod\ 475}#)
                             (if (memv #{type\ 469}# (quote (call)))
                               (#{chi-application\ 167}#
                                 (#{chi\ 165}#
                                   (car #{e\ 471}#)
                                   #{r\ 472}#
                                   #{w\ 473}#
                                   #{mod\ 475}#)
                                 #{e\ 471}#
                                 #{r\ 472}#
                                 #{w\ 473}#
                                 #{s\ 474}#
                                 #{mod\ 475}#)
                               (if (memv #{type\ 469}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 478}#)
                                    ((lambda (#{tmp\ 479}#)
                                       (if #{tmp\ 479}#
                                         (apply (lambda (#{_\ 480}#
                                                         #{e1\ 481}#
                                                         #{e2\ 482}#)
                                                  (#{chi-sequence\ 159}#
                                                    (cons #{e1\ 481}#
                                                          #{e2\ 482}#)
                                                    #{r\ 472}#
                                                    #{w\ 473}#
                                                    #{s\ 474}#
                                                    #{mod\ 475}#))
                                                #{tmp\ 479}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 478}#)))
                                     ($sc-dispatch
                                       #{tmp\ 478}#
                                       '(any any . each-any))))
                                  #{e\ 471}#)
                                 (if (memv #{type\ 469}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 171}#
                                     #{value\ 470}#
                                     #{e\ 471}#
                                     #{r\ 472}#
                                     #{w\ 473}#
                                     #{s\ 474}#
                                     #{mod\ 475}#
                                     #{chi-sequence\ 159}#)
                                   (if (memv #{type\ 469}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 484}#)
                                        ((lambda (#{tmp\ 485}#)
                                           (if #{tmp\ 485}#
                                             (apply (lambda (#{_\ 486}#
                                                             #{x\ 487}#
                                                             #{e1\ 488}#
                                                             #{e2\ 489}#)
                                                      (let ((#{when-list\ 490}#
                                                              (#{chi-when-list\ 162}#
                                                                #{e\ 471}#
                                                                #{x\ 487}#
                                                                #{w\ 473}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 490}#)
                                                          (#{chi-sequence\ 159}#
                                                            (cons #{e1\ 488}#
                                                                  #{e2\ 489}#)
                                                            #{r\ 472}#
                                                            #{w\ 473}#
                                                            #{s\ 474}#
                                                            #{mod\ 475}#)
                                                          (#{chi-void\ 173}#))))
                                                    #{tmp\ 485}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 484}#)))
                                         ($sc-dispatch
                                           #{tmp\ 484}#
                                           '(any each-any any . each-any))))
                                      #{e\ 471}#)
                                     (if (memv #{type\ 469}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 471}#
                                         (#{wrap\ 157}#
                                           #{value\ 470}#
                                           #{w\ 473}#
                                           #{mod\ 475}#))
                                       (if (memv #{type\ 469}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 158}#
                                             #{e\ 471}#
                                             #{w\ 473}#
                                             #{s\ 474}#
                                             #{mod\ 475}#))
                                         (if (memv #{type\ 469}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 158}#
                                               #{e\ 471}#
                                               #{w\ 473}#
                                               #{s\ 474}#
                                               #{mod\ 475}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 158}#
                                               #{e\ 471}#
                                               #{w\ 473}#
                                               #{s\ 474}#
                                               #{mod\ 475}#))))))))))))))))))
           (#{chi\ 165}#
             (lambda (#{e\ 493}# #{r\ 494}# #{w\ 495}# #{mod\ 496}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 163}#
                     #{e\ 493}#
                     #{r\ 494}#
                     #{w\ 495}#
                     (#{source-annotation\ 120}# #{e\ 493}#)
                     #f
                     #{mod\ 496}#
                     #f))
                 (lambda (#{type\ 497}#
                          #{value\ 498}#
                          #{e\ 499}#
                          #{w\ 500}#
                          #{s\ 501}#
                          #{mod\ 502}#)
                   (#{chi-expr\ 166}#
                     #{type\ 497}#
                     #{value\ 498}#
                     #{e\ 499}#
                     #{r\ 494}#
                     #{w\ 500}#
                     #{s\ 501}#
                     #{mod\ 502}#)))))
           (#{chi-top\ 164}#
             (lambda (#{e\ 503}#
                      #{r\ 504}#
                      #{w\ 505}#
                      #{m\ 506}#
                      #{esew\ 507}#
                      #{mod\ 508}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 163}#
                     #{e\ 503}#
                     #{r\ 504}#
                     #{w\ 505}#
                     (#{source-annotation\ 120}# #{e\ 503}#)
                     #f
                     #{mod\ 508}#
                     #f))
                 (lambda (#{type\ 516}#
                          #{value\ 517}#
                          #{e\ 518}#
                          #{w\ 519}#
                          #{s\ 520}#
                          #{mod\ 521}#)
                   (if (memv #{type\ 516}# (quote (begin-form)))
                     ((lambda (#{tmp\ 522}#)
                        ((lambda (#{tmp\ 523}#)
                           (if #{tmp\ 523}#
                             (apply (lambda (#{_\ 524}#) (#{chi-void\ 173}#))
                                    #{tmp\ 523}#)
                             ((lambda (#{tmp\ 525}#)
                                (if #{tmp\ 525}#
                                  (apply (lambda (#{_\ 526}#
                                                  #{e1\ 527}#
                                                  #{e2\ 528}#)
                                           (#{chi-top-sequence\ 160}#
                                             (cons #{e1\ 527}# #{e2\ 528}#)
                                             #{r\ 504}#
                                             #{w\ 519}#
                                             #{s\ 520}#
                                             #{m\ 506}#
                                             #{esew\ 507}#
                                             #{mod\ 521}#))
                                         #{tmp\ 525}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 522}#)))
                              ($sc-dispatch
                                #{tmp\ 522}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 522}# (quote (any)))))
                      #{e\ 518}#)
                     (if (memv #{type\ 516}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 171}#
                         #{value\ 517}#
                         #{e\ 518}#
                         #{r\ 504}#
                         #{w\ 519}#
                         #{s\ 520}#
                         #{mod\ 521}#
                         (lambda (#{body\ 530}#
                                  #{r\ 531}#
                                  #{w\ 532}#
                                  #{s\ 533}#
                                  #{mod\ 534}#)
                           (#{chi-top-sequence\ 160}#
                             #{body\ 530}#
                             #{r\ 531}#
                             #{w\ 532}#
                             #{s\ 533}#
                             #{m\ 506}#
                             #{esew\ 507}#
                             #{mod\ 534}#)))
                       (if (memv #{type\ 516}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 535}#)
                            ((lambda (#{tmp\ 536}#)
                               (if #{tmp\ 536}#
                                 (apply (lambda (#{_\ 537}#
                                                 #{x\ 538}#
                                                 #{e1\ 539}#
                                                 #{e2\ 540}#)
                                          (let ((#{when-list\ 541}#
                                                  (#{chi-when-list\ 162}#
                                                    #{e\ 518}#
                                                    #{x\ 538}#
                                                    #{w\ 519}#))
                                                (#{body\ 542}#
                                                  (cons #{e1\ 539}#
                                                        #{e2\ 540}#)))
                                            (if (eq? #{m\ 506}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 541}#)
                                                (#{chi-top-sequence\ 160}#
                                                  #{body\ 542}#
                                                  #{r\ 504}#
                                                  #{w\ 519}#
                                                  #{s\ 520}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 521}#)
                                                (#{chi-void\ 173}#))
                                              (if (memq 'load
                                                        #{when-list\ 541}#)
                                                (if (let ((#{t\ 545}# (memq 'compile
                                                                            #{when-list\ 541}#)))
                                                      (if #{t\ 545}#
                                                        #{t\ 545}#
                                                        (if (eq? #{m\ 506}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 541}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 160}#
                                                    #{body\ 542}#
                                                    #{r\ 504}#
                                                    #{w\ 519}#
                                                    #{s\ 520}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 521}#)
                                                  (if (memq #{m\ 506}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 160}#
                                                      #{body\ 542}#
                                                      #{r\ 504}#
                                                      #{w\ 519}#
                                                      #{s\ 520}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 521}#)
                                                    (#{chi-void\ 173}#)))
                                                (if (let ((#{t\ 546}# (memq 'compile
                                                                            #{when-list\ 541}#)))
                                                      (if #{t\ 546}#
                                                        #{t\ 546}#
                                                        (if (eq? #{m\ 506}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 541}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 90}#
                                                      (#{chi-top-sequence\ 160}#
                                                        #{body\ 542}#
                                                        #{r\ 504}#
                                                        #{w\ 519}#
                                                        #{s\ 520}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 521}#)
                                                      #{mod\ 521}#)
                                                    (#{chi-void\ 173}#))
                                                  (#{chi-void\ 173}#))))))
                                        #{tmp\ 536}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 535}#)))
                             ($sc-dispatch
                               #{tmp\ 535}#
                               '(any each-any any . each-any))))
                          #{e\ 518}#)
                         (if (memv #{type\ 516}# (quote (define-syntax-form)))
                           (let ((#{n\ 547}# (#{id-var-name\ 151}#
                                               #{value\ 517}#
                                               #{w\ 519}#))
                                 (#{r\ 548}# (#{macros-only-env\ 125}#
                                               #{r\ 504}#)))
                             (if (memv #{m\ 506}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 507}#)
                                 (let ((#{e\ 549}# (#{chi-install-global\ 161}#
                                                     #{n\ 547}#
                                                     (#{chi\ 165}#
                                                       #{e\ 518}#
                                                       #{r\ 548}#
                                                       #{w\ 519}#
                                                       #{mod\ 521}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 549}#
                                       #{mod\ 521}#)
                                     (if (memq (quote load) #{esew\ 507}#)
                                       #{e\ 549}#
                                       (#{chi-void\ 173}#))))
                                 (if (memq (quote load) #{esew\ 507}#)
                                   (#{chi-install-global\ 161}#
                                     #{n\ 547}#
                                     (#{chi\ 165}#
                                       #{e\ 518}#
                                       #{r\ 548}#
                                       #{w\ 519}#
                                       #{mod\ 521}#))
                                   (#{chi-void\ 173}#)))
                               (if (memv #{m\ 506}# (quote (c&e)))
                                 (let ((#{e\ 550}# (#{chi-install-global\ 161}#
                                                     #{n\ 547}#
                                                     (#{chi\ 165}#
                                                       #{e\ 518}#
                                                       #{r\ 548}#
                                                       #{w\ 519}#
                                                       #{mod\ 521}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 550}#
                                       #{mod\ 521}#)
                                     #{e\ 550}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 507}#)
                                     (#{top-level-eval-hook\ 90}#
                                       (#{chi-install-global\ 161}#
                                         #{n\ 547}#
                                         (#{chi\ 165}#
                                           #{e\ 518}#
                                           #{r\ 548}#
                                           #{w\ 519}#
                                           #{mod\ 521}#))
                                       #{mod\ 521}#))
                                   (#{chi-void\ 173}#)))))
                           (if (memv #{type\ 516}# (quote (define-form)))
                             (let ((#{n\ 551}# (#{id-var-name\ 151}#
                                                 #{value\ 517}#
                                                 #{w\ 519}#)))
                               (let ((#{type\ 552}#
                                       (#{binding-type\ 121}#
                                         (#{lookup\ 126}#
                                           #{n\ 551}#
                                           #{r\ 504}#
                                           #{mod\ 521}#))))
                                 (if (memv #{type\ 552}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 551}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 553}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 551}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 551}#
                                           (if (variable? #{old\ 553}#)
                                             (variable-ref #{old\ 553}#)
                                             #f))))
                                     (let ((#{x\ 554}# (#{build-global-definition\ 104}#
                                                         #{s\ 520}#
                                                         #{n\ 551}#
                                                         (#{chi\ 165}#
                                                           #{e\ 518}#
                                                           #{r\ 504}#
                                                           #{w\ 519}#
                                                           #{mod\ 521}#))))
                                       (begin
                                         (if (eq? #{m\ 506}# (quote c&e))
                                           (#{top-level-eval-hook\ 90}#
                                             #{x\ 554}#
                                             #{mod\ 521}#))
                                         #{x\ 554}#)))
                                   (if (memv #{type\ 552}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 518}#
                                       (#{wrap\ 157}#
                                         #{value\ 517}#
                                         #{w\ 519}#
                                         #{mod\ 521}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 518}#
                                       (#{wrap\ 157}#
                                         #{value\ 517}#
                                         #{w\ 519}#
                                         #{mod\ 521}#))))))
                             (let ((#{x\ 555}# (#{chi-expr\ 166}#
                                                 #{type\ 516}#
                                                 #{value\ 517}#
                                                 #{e\ 518}#
                                                 #{r\ 504}#
                                                 #{w\ 519}#
                                                 #{s\ 520}#
                                                 #{mod\ 521}#)))
                               (begin
                                 (if (eq? #{m\ 506}# (quote c&e))
                                   (#{top-level-eval-hook\ 90}#
                                     #{x\ 555}#
                                     #{mod\ 521}#))
                                 #{x\ 555}#)))))))))))
           (#{syntax-type\ 163}#
             (lambda (#{e\ 556}#
                      #{r\ 557}#
                      #{w\ 558}#
                      #{s\ 559}#
                      #{rib\ 560}#
                      #{mod\ 561}#
                      #{for-car?\ 562}#)
               (if (symbol? #{e\ 556}#)
                 (let ((#{n\ 563}# (#{id-var-name\ 151}#
                                     #{e\ 556}#
                                     #{w\ 558}#)))
                   (let ((#{b\ 564}# (#{lookup\ 126}#
                                       #{n\ 563}#
                                       #{r\ 557}#
                                       #{mod\ 561}#)))
                     (let ((#{type\ 565}#
                             (#{binding-type\ 121}# #{b\ 564}#)))
                       (if (memv #{type\ 565}# (quote (lexical)))
                         (values
                           #{type\ 565}#
                           (#{binding-value\ 122}# #{b\ 564}#)
                           #{e\ 556}#
                           #{w\ 558}#
                           #{s\ 559}#
                           #{mod\ 561}#)
                         (if (memv #{type\ 565}# (quote (global)))
                           (values
                             #{type\ 565}#
                             #{n\ 563}#
                             #{e\ 556}#
                             #{w\ 558}#
                             #{s\ 559}#
                             #{mod\ 561}#)
                           (if (memv #{type\ 565}# (quote (macro)))
                             (if #{for-car?\ 562}#
                               (values
                                 #{type\ 565}#
                                 (#{binding-value\ 122}# #{b\ 564}#)
                                 #{e\ 556}#
                                 #{w\ 558}#
                                 #{s\ 559}#
                                 #{mod\ 561}#)
                               (#{syntax-type\ 163}#
                                 (#{chi-macro\ 168}#
                                   (#{binding-value\ 122}# #{b\ 564}#)
                                   #{e\ 556}#
                                   #{r\ 557}#
                                   #{w\ 558}#
                                   #{rib\ 560}#
                                   #{mod\ 561}#)
                                 #{r\ 557}#
                                 '(())
                                 #{s\ 559}#
                                 #{rib\ 560}#
                                 #{mod\ 561}#
                                 #f))
                             (values
                               #{type\ 565}#
                               (#{binding-value\ 122}# #{b\ 564}#)
                               #{e\ 556}#
                               #{w\ 558}#
                               #{s\ 559}#
                               #{mod\ 561}#)))))))
                 (if (pair? #{e\ 556}#)
                   (let ((#{first\ 566}# (car #{e\ 556}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 163}#
                           #{first\ 566}#
                           #{r\ 557}#
                           #{w\ 558}#
                           #{s\ 559}#
                           #{rib\ 560}#
                           #{mod\ 561}#
                           #t))
                       (lambda (#{ftype\ 567}#
                                #{fval\ 568}#
                                #{fe\ 569}#
                                #{fw\ 570}#
                                #{fs\ 571}#
                                #{fmod\ 572}#)
                         (if (memv #{ftype\ 567}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 568}#
                             #{e\ 556}#
                             #{w\ 558}#
                             #{s\ 559}#
                             #{mod\ 561}#)
                           (if (memv #{ftype\ 567}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 112}#
                                 #{fval\ 568}#
                                 #{w\ 558}#
                                 #{fmod\ 572}#)
                               #{e\ 556}#
                               #{w\ 558}#
                               #{s\ 559}#
                               #{mod\ 561}#)
                             (if (memv #{ftype\ 567}# (quote (macro)))
                               (#{syntax-type\ 163}#
                                 (#{chi-macro\ 168}#
                                   #{fval\ 568}#
                                   #{e\ 556}#
                                   #{r\ 557}#
                                   #{w\ 558}#
                                   #{rib\ 560}#
                                   #{mod\ 561}#)
                                 #{r\ 557}#
                                 '(())
                                 #{s\ 559}#
                                 #{rib\ 560}#
                                 #{mod\ 561}#
                                 #{for-car?\ 562}#)
                               (if (memv #{ftype\ 567}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 568}# #{e\ 556}#))
                                   (lambda (#{sym\ 573}# #{mod\ 574}#)
                                     (#{syntax-type\ 163}#
                                       #{sym\ 573}#
                                       #{r\ 557}#
                                       #{w\ 558}#
                                       #{s\ 559}#
                                       #{rib\ 560}#
                                       #{mod\ 574}#
                                       #{for-car?\ 562}#)))
                                 (if (memv #{ftype\ 567}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 568}#
                                     #{e\ 556}#
                                     #{w\ 558}#
                                     #{s\ 559}#
                                     #{mod\ 561}#)
                                   (if (memv #{ftype\ 567}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 568}#
                                       #{e\ 556}#
                                       #{w\ 558}#
                                       #{s\ 559}#
                                       #{mod\ 561}#)
                                     (if (memv #{ftype\ 567}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 556}#
                                         #{w\ 558}#
                                         #{s\ 559}#
                                         #{mod\ 561}#)
                                       (if (memv #{ftype\ 567}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 556}#
                                           #{w\ 558}#
                                           #{s\ 559}#
                                           #{mod\ 561}#)
                                         (if (memv #{ftype\ 567}#
                                                   '(define))
                                           ((lambda (#{tmp\ 575}#)
                                              ((lambda (#{tmp\ 576}#)
                                                 (if (if #{tmp\ 576}#
                                                       (apply (lambda (#{_\ 577}#
                                                                       #{name\ 578}#
                                                                       #{val\ 579}#)
                                                                (#{id?\ 129}#
                                                                  #{name\ 578}#))
                                                              #{tmp\ 576}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 580}#
                                                                   #{name\ 581}#
                                                                   #{val\ 582}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 581}#
                                                              #{val\ 582}#
                                                              #{w\ 558}#
                                                              #{s\ 559}#
                                                              #{mod\ 561}#))
                                                          #{tmp\ 576}#)
                                                   ((lambda (#{tmp\ 583}#)
                                                      (if (if #{tmp\ 583}#
                                                            (apply (lambda (#{_\ 584}#
                                                                            #{name\ 585}#
                                                                            #{args\ 586}#
                                                                            #{e1\ 587}#
                                                                            #{e2\ 588}#)
                                                                     (if (#{id?\ 129}#
                                                                           #{name\ 585}#)
                                                                       (#{valid-bound-ids?\ 154}#
                                                                         (#{lambda-var-list\ 177}#
                                                                           #{args\ 586}#))
                                                                       #f))
                                                                   #{tmp\ 583}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 589}#
                                                                        #{name\ 590}#
                                                                        #{args\ 591}#
                                                                        #{e1\ 592}#
                                                                        #{e2\ 593}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 157}#
                                                                     #{name\ 590}#
                                                                     #{w\ 558}#
                                                                     #{mod\ 561}#)
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
                                                                                   ellipsis?
                                                                                   chi-void
                                                                                   eval-local-transformer
                                                                                   chi-local-syntax
                                                                                   chi-lambda-clause
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
                                                                                   build-lambda
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
                                                                           (#{wrap\ 157}#
                                                                             (cons #{args\ 591}#
                                                                                   (cons #{e1\ 592}#
                                                                                         #{e2\ 593}#))
                                                                             #{w\ 558}#
                                                                             #{mod\ 561}#))
                                                                     #{s\ 559}#)
                                                                   '(())
                                                                   #{s\ 559}#
                                                                   #{mod\ 561}#))
                                                               #{tmp\ 583}#)
                                                        ((lambda (#{tmp\ 595}#)
                                                           (if (if #{tmp\ 595}#
                                                                 (apply (lambda (#{_\ 596}#
                                                                                 #{name\ 597}#)
                                                                          (#{id?\ 129}#
                                                                            #{name\ 597}#))
                                                                        #{tmp\ 595}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 598}#
                                                                             #{name\ 599}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 157}#
                                                                          #{name\ 599}#
                                                                          #{w\ 558}#
                                                                          #{mod\ 561}#)
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
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
                                                                                 chi-lambda-clause
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
                                                                                 build-lambda
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
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
                                                                                 chi-lambda-clause
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
                                                                                 build-lambda
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
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
                                                                                 chi-lambda-clause
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
                                                                                 build-lambda
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
                                                                        #{s\ 559}#
                                                                        #{mod\ 561}#))
                                                                    #{tmp\ 595}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 575}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 575}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 575}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 575}#
                                                 '(any any any))))
                                            #{e\ 556}#)
                                           (if (memv #{ftype\ 567}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 600}#)
                                                ((lambda (#{tmp\ 601}#)
                                                   (if (if #{tmp\ 601}#
                                                         (apply (lambda (#{_\ 602}#
                                                                         #{name\ 603}#
                                                                         #{val\ 604}#)
                                                                  (#{id?\ 129}#
                                                                    #{name\ 603}#))
                                                                #{tmp\ 601}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 605}#
                                                                     #{name\ 606}#
                                                                     #{val\ 607}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 606}#
                                                                #{val\ 607}#
                                                                #{w\ 558}#
                                                                #{s\ 559}#
                                                                #{mod\ 561}#))
                                                            #{tmp\ 601}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 600}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 600}#
                                                   '(any any any))))
                                              #{e\ 556}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 556}#
                                               #{w\ 558}#
                                               #{s\ 559}#
                                               #{mod\ 561}#))))))))))))))
                   (if (#{syntax-object?\ 113}# #{e\ 556}#)
                     (#{syntax-type\ 163}#
                       (#{syntax-object-expression\ 114}# #{e\ 556}#)
                       #{r\ 557}#
                       (#{join-wraps\ 148}#
                         #{w\ 558}#
                         (#{syntax-object-wrap\ 115}# #{e\ 556}#))
                       #{s\ 559}#
                       #{rib\ 560}#
                       (let ((#{t\ 608}# (#{syntax-object-module\ 116}#
                                           #{e\ 556}#)))
                         (if #{t\ 608}# #{t\ 608}# #{mod\ 561}#))
                       #{for-car?\ 562}#)
                     (if (self-evaluating? #{e\ 556}#)
                       (values
                         'constant
                         #f
                         #{e\ 556}#
                         #{w\ 558}#
                         #{s\ 559}#
                         #{mod\ 561}#)
                       (values
                         'other
                         #f
                         #{e\ 556}#
                         #{w\ 558}#
                         #{s\ 559}#
                         #{mod\ 561}#)))))))
           (#{chi-when-list\ 162}#
             (lambda (#{e\ 609}# #{when-list\ 610}# #{w\ 611}#)
               (letrec ((#{f\ 612}# (lambda (#{when-list\ 613}#
                                             #{situations\ 614}#)
                                      (if (null? #{when-list\ 613}#)
                                        #{situations\ 614}#
                                        (#{f\ 612}# (cdr #{when-list\ 613}#)
                                                    (cons (let ((#{x\ 615}# (car #{when-list\ 613}#)))
                                                            (if (#{free-id=?\ 152}#
                                                                  #{x\ 615}#
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
                                                                          ellipsis?
                                                                          chi-void
                                                                          eval-local-transformer
                                                                          chi-local-syntax
                                                                          chi-lambda-clause
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
                                                                          build-lambda
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
                                                              (if (#{free-id=?\ 152}#
                                                                    #{x\ 615}#
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
                                                                            ellipsis?
                                                                            chi-void
                                                                            eval-local-transformer
                                                                            chi-local-syntax
                                                                            chi-lambda-clause
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
                                                                            build-lambda
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
                                                                (if (#{free-id=?\ 152}#
                                                                      #{x\ 615}#
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
                                                                              ellipsis?
                                                                              chi-void
                                                                              eval-local-transformer
                                                                              chi-local-syntax
                                                                              chi-lambda-clause
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
                                                                              build-lambda
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
                                                                    #{e\ 609}#
                                                                    (#{wrap\ 157}#
                                                                      #{x\ 615}#
                                                                      #{w\ 611}#
                                                                      #f))))))
                                                          #{situations\ 614}#))))))
                 (#{f\ 612}# #{when-list\ 610}# (quote ())))))
           (#{chi-install-global\ 161}#
             (lambda (#{name\ 616}# #{e\ 617}#)
               (#{build-global-definition\ 104}#
                 #f
                 #{name\ 616}#
                 (if (let ((#{v\ 618}# (module-variable
                                         (current-module)
                                         #{name\ 616}#)))
                       (if #{v\ 618}#
                         (if (variable-bound? #{v\ 618}#)
                           (if (macro? (variable-ref #{v\ 618}#))
                             (not (eq? (macro-type (variable-ref #{v\ 618}#))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (#{build-application\ 96}#
                     #f
                     (#{build-primref\ 106}#
                       #f
                       'make-extended-syncase-macro)
                     (list (#{build-application\ 96}#
                             #f
                             (#{build-primref\ 106}# #f (quote module-ref))
                             (list (#{build-application\ 96}#
                                     #f
                                     (#{build-primref\ 106}#
                                       #f
                                       'current-module)
                                     '())
                                   (#{build-data\ 107}# #f #{name\ 616}#)))
                           (#{build-data\ 107}# #f (quote macro))
                           #{e\ 617}#))
                   (#{build-application\ 96}#
                     #f
                     (#{build-primref\ 106}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 107}# #f (quote macro))
                           #{e\ 617}#))))))
           (#{chi-top-sequence\ 160}#
             (lambda (#{body\ 619}#
                      #{r\ 620}#
                      #{w\ 621}#
                      #{s\ 622}#
                      #{m\ 623}#
                      #{esew\ 624}#
                      #{mod\ 625}#)
               (#{build-sequence\ 108}#
                 #{s\ 622}#
                 (letrec ((#{dobody\ 626}#
                            (lambda (#{body\ 627}#
                                     #{r\ 628}#
                                     #{w\ 629}#
                                     #{m\ 630}#
                                     #{esew\ 631}#
                                     #{mod\ 632}#)
                              (if (null? #{body\ 627}#)
                                '()
                                (let ((#{first\ 633}#
                                        (#{chi-top\ 164}#
                                          (car #{body\ 627}#)
                                          #{r\ 628}#
                                          #{w\ 629}#
                                          #{m\ 630}#
                                          #{esew\ 631}#
                                          #{mod\ 632}#)))
                                  (cons #{first\ 633}#
                                        (#{dobody\ 626}#
                                          (cdr #{body\ 627}#)
                                          #{r\ 628}#
                                          #{w\ 629}#
                                          #{m\ 630}#
                                          #{esew\ 631}#
                                          #{mod\ 632}#)))))))
                   (#{dobody\ 626}#
                     #{body\ 619}#
                     #{r\ 620}#
                     #{w\ 621}#
                     #{m\ 623}#
                     #{esew\ 624}#
                     #{mod\ 625}#)))))
           (#{chi-sequence\ 159}#
             (lambda (#{body\ 634}#
                      #{r\ 635}#
                      #{w\ 636}#
                      #{s\ 637}#
                      #{mod\ 638}#)
               (#{build-sequence\ 108}#
                 #{s\ 637}#
                 (letrec ((#{dobody\ 639}#
                            (lambda (#{body\ 640}#
                                     #{r\ 641}#
                                     #{w\ 642}#
                                     #{mod\ 643}#)
                              (if (null? #{body\ 640}#)
                                '()
                                (let ((#{first\ 644}#
                                        (#{chi\ 165}#
                                          (car #{body\ 640}#)
                                          #{r\ 641}#
                                          #{w\ 642}#
                                          #{mod\ 643}#)))
                                  (cons #{first\ 644}#
                                        (#{dobody\ 639}#
                                          (cdr #{body\ 640}#)
                                          #{r\ 641}#
                                          #{w\ 642}#
                                          #{mod\ 643}#)))))))
                   (#{dobody\ 639}#
                     #{body\ 634}#
                     #{r\ 635}#
                     #{w\ 636}#
                     #{mod\ 638}#)))))
           (#{source-wrap\ 158}#
             (lambda (#{x\ 645}#
                      #{w\ 646}#
                      #{s\ 647}#
                      #{defmod\ 648}#)
               (#{wrap\ 157}#
                 (#{decorate-source\ 94}# #{x\ 645}# #{s\ 647}#)
                 #{w\ 646}#
                 #{defmod\ 648}#)))
           (#{wrap\ 157}#
             (lambda (#{x\ 649}# #{w\ 650}# #{defmod\ 651}#)
               (if (if (null? (#{wrap-marks\ 132}# #{w\ 650}#))
                     (null? (#{wrap-subst\ 133}# #{w\ 650}#))
                     #f)
                 #{x\ 649}#
                 (if (#{syntax-object?\ 113}# #{x\ 649}#)
                   (#{make-syntax-object\ 112}#
                     (#{syntax-object-expression\ 114}# #{x\ 649}#)
                     (#{join-wraps\ 148}#
                       #{w\ 650}#
                       (#{syntax-object-wrap\ 115}# #{x\ 649}#))
                     (#{syntax-object-module\ 116}# #{x\ 649}#))
                   (if (null? #{x\ 649}#)
                     #{x\ 649}#
                     (#{make-syntax-object\ 112}#
                       #{x\ 649}#
                       #{w\ 650}#
                       #{defmod\ 651}#))))))
           (#{bound-id-member?\ 156}#
             (lambda (#{x\ 652}# #{list\ 653}#)
               (if (not (null? #{list\ 653}#))
                 (let ((#{t\ 654}# (#{bound-id=?\ 153}#
                                     #{x\ 652}#
                                     (car #{list\ 653}#))))
                   (if #{t\ 654}#
                     #{t\ 654}#
                     (#{bound-id-member?\ 156}#
                       #{x\ 652}#
                       (cdr #{list\ 653}#))))
                 #f)))
           (#{distinct-bound-ids?\ 155}#
             (lambda (#{ids\ 655}#)
               (letrec ((#{distinct?\ 656}#
                          (lambda (#{ids\ 657}#)
                            (let ((#{t\ 658}# (null? #{ids\ 657}#)))
                              (if #{t\ 658}#
                                #{t\ 658}#
                                (if (not (#{bound-id-member?\ 156}#
                                           (car #{ids\ 657}#)
                                           (cdr #{ids\ 657}#)))
                                  (#{distinct?\ 656}# (cdr #{ids\ 657}#))
                                  #f))))))
                 (#{distinct?\ 656}# #{ids\ 655}#))))
           (#{valid-bound-ids?\ 154}#
             (lambda (#{ids\ 659}#)
               (if (letrec ((#{all-ids?\ 660}#
                              (lambda (#{ids\ 661}#)
                                (let ((#{t\ 662}# (null? #{ids\ 661}#)))
                                  (if #{t\ 662}#
                                    #{t\ 662}#
                                    (if (#{id?\ 129}# (car #{ids\ 661}#))
                                      (#{all-ids?\ 660}# (cdr #{ids\ 661}#))
                                      #f))))))
                     (#{all-ids?\ 660}# #{ids\ 659}#))
                 (#{distinct-bound-ids?\ 155}# #{ids\ 659}#)
                 #f)))
           (#{bound-id=?\ 153}#
             (lambda (#{i\ 663}# #{j\ 664}#)
               (if (if (#{syntax-object?\ 113}# #{i\ 663}#)
                     (#{syntax-object?\ 113}# #{j\ 664}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 114}# #{i\ 663}#)
                          (#{syntax-object-expression\ 114}# #{j\ 664}#))
                   (#{same-marks?\ 150}#
                     (#{wrap-marks\ 132}#
                       (#{syntax-object-wrap\ 115}# #{i\ 663}#))
                     (#{wrap-marks\ 132}#
                       (#{syntax-object-wrap\ 115}# #{j\ 664}#)))
                   #f)
                 (eq? #{i\ 663}# #{j\ 664}#))))
           (#{free-id=?\ 152}#
             (lambda (#{i\ 665}# #{j\ 666}#)
               (if (eq? (let ((#{x\ 667}# #{i\ 665}#))
                          (if (#{syntax-object?\ 113}# #{x\ 667}#)
                            (#{syntax-object-expression\ 114}# #{x\ 667}#)
                            #{x\ 667}#))
                        (let ((#{x\ 668}# #{j\ 666}#))
                          (if (#{syntax-object?\ 113}# #{x\ 668}#)
                            (#{syntax-object-expression\ 114}# #{x\ 668}#)
                            #{x\ 668}#)))
                 (eq? (#{id-var-name\ 151}# #{i\ 665}# (quote (())))
                      (#{id-var-name\ 151}# #{j\ 666}# (quote (()))))
                 #f)))
           (#{id-var-name\ 151}#
             (lambda (#{id\ 669}# #{w\ 670}#)
               (letrec ((#{search-vector-rib\ 673}#
                          (lambda (#{sym\ 679}#
                                   #{subst\ 680}#
                                   #{marks\ 681}#
                                   #{symnames\ 682}#
                                   #{ribcage\ 683}#)
                            (let ((#{n\ 684}# (vector-length
                                                #{symnames\ 682}#)))
                              (letrec ((#{f\ 685}# (lambda (#{i\ 686}#)
                                                     (if (#{fx=\ 88}#
                                                           #{i\ 686}#
                                                           #{n\ 684}#)
                                                       (#{search\ 671}#
                                                         #{sym\ 679}#
                                                         (cdr #{subst\ 680}#)
                                                         #{marks\ 681}#)
                                                       (if (if (eq? (vector-ref
                                                                      #{symnames\ 682}#
                                                                      #{i\ 686}#)
                                                                    #{sym\ 679}#)
                                                             (#{same-marks?\ 150}#
                                                               #{marks\ 681}#
                                                               (vector-ref
                                                                 (#{ribcage-marks\ 139}#
                                                                   #{ribcage\ 683}#)
                                                                 #{i\ 686}#))
                                                             #f)
                                                         (values
                                                           (vector-ref
                                                             (#{ribcage-labels\ 140}#
                                                               #{ribcage\ 683}#)
                                                             #{i\ 686}#)
                                                           #{marks\ 681}#)
                                                         (#{f\ 685}# (#{fx+\ 86}#
                                                                       #{i\ 686}#
                                                                       1)))))))
                                (#{f\ 685}# 0)))))
                        (#{search-list-rib\ 672}#
                          (lambda (#{sym\ 687}#
                                   #{subst\ 688}#
                                   #{marks\ 689}#
                                   #{symnames\ 690}#
                                   #{ribcage\ 691}#)
                            (letrec ((#{f\ 692}# (lambda (#{symnames\ 693}#
                                                          #{i\ 694}#)
                                                   (if (null? #{symnames\ 693}#)
                                                     (#{search\ 671}#
                                                       #{sym\ 687}#
                                                       (cdr #{subst\ 688}#)
                                                       #{marks\ 689}#)
                                                     (if (if (eq? (car #{symnames\ 693}#)
                                                                  #{sym\ 687}#)
                                                           (#{same-marks?\ 150}#
                                                             #{marks\ 689}#
                                                             (list-ref
                                                               (#{ribcage-marks\ 139}#
                                                                 #{ribcage\ 691}#)
                                                               #{i\ 694}#))
                                                           #f)
                                                       (values
                                                         (list-ref
                                                           (#{ribcage-labels\ 140}#
                                                             #{ribcage\ 691}#)
                                                           #{i\ 694}#)
                                                         #{marks\ 689}#)
                                                       (#{f\ 692}# (cdr #{symnames\ 693}#)
                                                                   (#{fx+\ 86}#
                                                                     #{i\ 694}#
                                                                     1)))))))
                              (#{f\ 692}# #{symnames\ 690}# 0))))
                        (#{search\ 671}#
                          (lambda (#{sym\ 695}# #{subst\ 696}# #{marks\ 697}#)
                            (if (null? #{subst\ 696}#)
                              (values #f #{marks\ 697}#)
                              (let ((#{fst\ 698}# (car #{subst\ 696}#)))
                                (if (eq? #{fst\ 698}# (quote shift))
                                  (#{search\ 671}#
                                    #{sym\ 695}#
                                    (cdr #{subst\ 696}#)
                                    (cdr #{marks\ 697}#))
                                  (let ((#{symnames\ 699}#
                                          (#{ribcage-symnames\ 138}#
                                            #{fst\ 698}#)))
                                    (if (vector? #{symnames\ 699}#)
                                      (#{search-vector-rib\ 673}#
                                        #{sym\ 695}#
                                        #{subst\ 696}#
                                        #{marks\ 697}#
                                        #{symnames\ 699}#
                                        #{fst\ 698}#)
                                      (#{search-list-rib\ 672}#
                                        #{sym\ 695}#
                                        #{subst\ 696}#
                                        #{marks\ 697}#
                                        #{symnames\ 699}#
                                        #{fst\ 698}#)))))))))
                 (if (symbol? #{id\ 669}#)
                   (let ((#{t\ 700}# (call-with-values
                                       (lambda ()
                                         (#{search\ 671}#
                                           #{id\ 669}#
                                           (#{wrap-subst\ 133}# #{w\ 670}#)
                                           (#{wrap-marks\ 132}# #{w\ 670}#)))
                                       (lambda (#{x\ 702}# . #{ignore\ 701}#)
                                         #{x\ 702}#))))
                     (if #{t\ 700}# #{t\ 700}# #{id\ 669}#))
                   (if (#{syntax-object?\ 113}# #{id\ 669}#)
                     (let ((#{id\ 703}#
                             (#{syntax-object-expression\ 114}# #{id\ 669}#))
                           (#{w1\ 704}#
                             (#{syntax-object-wrap\ 115}# #{id\ 669}#)))
                       (let ((#{marks\ 705}#
                               (#{join-marks\ 149}#
                                 (#{wrap-marks\ 132}# #{w\ 670}#)
                                 (#{wrap-marks\ 132}# #{w1\ 704}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 671}#
                               #{id\ 703}#
                               (#{wrap-subst\ 133}# #{w\ 670}#)
                               #{marks\ 705}#))
                           (lambda (#{new-id\ 706}# #{marks\ 707}#)
                             (let ((#{t\ 708}# #{new-id\ 706}#))
                               (if #{t\ 708}#
                                 #{t\ 708}#
                                 (let ((#{t\ 709}# (call-with-values
                                                     (lambda ()
                                                       (#{search\ 671}#
                                                         #{id\ 703}#
                                                         (#{wrap-subst\ 133}#
                                                           #{w1\ 704}#)
                                                         #{marks\ 707}#))
                                                     (lambda (#{x\ 711}#
                                                              .
                                                              #{ignore\ 710}#)
                                                       #{x\ 711}#))))
                                   (if #{t\ 709}#
                                     #{t\ 709}#
                                     #{id\ 703}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 669}#))))))
           (#{same-marks?\ 150}#
             (lambda (#{x\ 712}# #{y\ 713}#)
               (let ((#{t\ 714}# (eq? #{x\ 712}# #{y\ 713}#)))
                 (if #{t\ 714}#
                   #{t\ 714}#
                   (if (not (null? #{x\ 712}#))
                     (if (not (null? #{y\ 713}#))
                       (if (eq? (car #{x\ 712}#) (car #{y\ 713}#))
                         (#{same-marks?\ 150}#
                           (cdr #{x\ 712}#)
                           (cdr #{y\ 713}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 149}#
             (lambda (#{m1\ 715}# #{m2\ 716}#)
               (#{smart-append\ 147}# #{m1\ 715}# #{m2\ 716}#)))
           (#{join-wraps\ 148}#
             (lambda (#{w1\ 717}# #{w2\ 718}#)
               (let ((#{m1\ 719}# (#{wrap-marks\ 132}# #{w1\ 717}#))
                     (#{s1\ 720}# (#{wrap-subst\ 133}# #{w1\ 717}#)))
                 (if (null? #{m1\ 719}#)
                   (if (null? #{s1\ 720}#)
                     #{w2\ 718}#
                     (#{make-wrap\ 131}#
                       (#{wrap-marks\ 132}# #{w2\ 718}#)
                       (#{smart-append\ 147}#
                         #{s1\ 720}#
                         (#{wrap-subst\ 133}# #{w2\ 718}#))))
                   (#{make-wrap\ 131}#
                     (#{smart-append\ 147}#
                       #{m1\ 719}#
                       (#{wrap-marks\ 132}# #{w2\ 718}#))
                     (#{smart-append\ 147}#
                       #{s1\ 720}#
                       (#{wrap-subst\ 133}# #{w2\ 718}#)))))))
           (#{smart-append\ 147}#
             (lambda (#{m1\ 721}# #{m2\ 722}#)
               (if (null? #{m2\ 722}#)
                 #{m1\ 721}#
                 (append #{m1\ 721}# #{m2\ 722}#))))
           (#{make-binding-wrap\ 146}#
             (lambda (#{ids\ 723}# #{labels\ 724}# #{w\ 725}#)
               (if (null? #{ids\ 723}#)
                 #{w\ 725}#
                 (#{make-wrap\ 131}#
                   (#{wrap-marks\ 132}# #{w\ 725}#)
                   (cons (let ((#{labelvec\ 726}#
                                 (list->vector #{labels\ 724}#)))
                           (let ((#{n\ 727}# (vector-length
                                               #{labelvec\ 726}#)))
                             (let ((#{symnamevec\ 728}#
                                     (make-vector #{n\ 727}#))
                                   (#{marksvec\ 729}#
                                     (make-vector #{n\ 727}#)))
                               (begin
                                 (letrec ((#{f\ 730}# (lambda (#{ids\ 731}#
                                                               #{i\ 732}#)
                                                        (if (not (null? #{ids\ 731}#))
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{id-sym-name&marks\ 130}#
                                                                (car #{ids\ 731}#)
                                                                #{w\ 725}#))
                                                            (lambda (#{symname\ 733}#
                                                                     #{marks\ 734}#)
                                                              (begin
                                                                (vector-set!
                                                                  #{symnamevec\ 728}#
                                                                  #{i\ 732}#
                                                                  #{symname\ 733}#)
                                                                (vector-set!
                                                                  #{marksvec\ 729}#
                                                                  #{i\ 732}#
                                                                  #{marks\ 734}#)
                                                                (#{f\ 730}# (cdr #{ids\ 731}#)
                                                                            (#{fx+\ 86}#
                                                                              #{i\ 732}#
                                                                              1)))))))))
                                   (#{f\ 730}# #{ids\ 723}# 0))
                                 (#{make-ribcage\ 136}#
                                   #{symnamevec\ 728}#
                                   #{marksvec\ 729}#
                                   #{labelvec\ 726}#)))))
                         (#{wrap-subst\ 133}# #{w\ 725}#))))))
           (#{extend-ribcage!\ 145}#
             (lambda (#{ribcage\ 735}# #{id\ 736}# #{label\ 737}#)
               (begin
                 (#{set-ribcage-symnames!\ 141}#
                   #{ribcage\ 735}#
                   (cons (#{syntax-object-expression\ 114}# #{id\ 736}#)
                         (#{ribcage-symnames\ 138}# #{ribcage\ 735}#)))
                 (#{set-ribcage-marks!\ 142}#
                   #{ribcage\ 735}#
                   (cons (#{wrap-marks\ 132}#
                           (#{syntax-object-wrap\ 115}# #{id\ 736}#))
                         (#{ribcage-marks\ 139}# #{ribcage\ 735}#)))
                 (#{set-ribcage-labels!\ 143}#
                   #{ribcage\ 735}#
                   (cons #{label\ 737}#
                         (#{ribcage-labels\ 140}# #{ribcage\ 735}#))))))
           (#{anti-mark\ 144}#
             (lambda (#{w\ 738}#)
               (#{make-wrap\ 131}#
                 (cons #f (#{wrap-marks\ 132}# #{w\ 738}#))
                 (cons 'shift
                       (#{wrap-subst\ 133}# #{w\ 738}#)))))
           (#{set-ribcage-labels!\ 143}#
             (lambda (#{x\ 739}# #{update\ 740}#)
               (vector-set! #{x\ 739}# 3 #{update\ 740}#)))
           (#{set-ribcage-marks!\ 142}#
             (lambda (#{x\ 741}# #{update\ 742}#)
               (vector-set! #{x\ 741}# 2 #{update\ 742}#)))
           (#{set-ribcage-symnames!\ 141}#
             (lambda (#{x\ 743}# #{update\ 744}#)
               (vector-set! #{x\ 743}# 1 #{update\ 744}#)))
           (#{ribcage-labels\ 140}#
             (lambda (#{x\ 745}#) (vector-ref #{x\ 745}# 3)))
           (#{ribcage-marks\ 139}#
             (lambda (#{x\ 746}#) (vector-ref #{x\ 746}# 2)))
           (#{ribcage-symnames\ 138}#
             (lambda (#{x\ 747}#) (vector-ref #{x\ 747}# 1)))
           (#{ribcage?\ 137}#
             (lambda (#{x\ 748}#)
               (if (vector? #{x\ 748}#)
                 (if (= (vector-length #{x\ 748}#) 4)
                   (eq? (vector-ref #{x\ 748}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 136}#
             (lambda (#{symnames\ 749}#
                      #{marks\ 750}#
                      #{labels\ 751}#)
               (vector
                 'ribcage
                 #{symnames\ 749}#
                 #{marks\ 750}#
                 #{labels\ 751}#)))
           (#{gen-labels\ 135}#
             (lambda (#{ls\ 752}#)
               (if (null? #{ls\ 752}#)
                 '()
                 (cons (#{gen-label\ 134}#)
                       (#{gen-labels\ 135}# (cdr #{ls\ 752}#))))))
           (#{gen-label\ 134}# (lambda () (string #\i)))
           (#{wrap-subst\ 133}# cdr)
           (#{wrap-marks\ 132}# car)
           (#{make-wrap\ 131}# cons)
           (#{id-sym-name&marks\ 130}#
             (lambda (#{x\ 753}# #{w\ 754}#)
               (if (#{syntax-object?\ 113}# #{x\ 753}#)
                 (values
                   (#{syntax-object-expression\ 114}# #{x\ 753}#)
                   (#{join-marks\ 149}#
                     (#{wrap-marks\ 132}# #{w\ 754}#)
                     (#{wrap-marks\ 132}#
                       (#{syntax-object-wrap\ 115}# #{x\ 753}#))))
                 (values
                   #{x\ 753}#
                   (#{wrap-marks\ 132}# #{w\ 754}#)))))
           (#{id?\ 129}#
             (lambda (#{x\ 755}#)
               (if (symbol? #{x\ 755}#)
                 #t
                 (if (#{syntax-object?\ 113}# #{x\ 755}#)
                   (symbol?
                     (#{syntax-object-expression\ 114}# #{x\ 755}#))
                   #f))))
           (#{nonsymbol-id?\ 128}#
             (lambda (#{x\ 756}#)
               (if (#{syntax-object?\ 113}# #{x\ 756}#)
                 (symbol?
                   (#{syntax-object-expression\ 114}# #{x\ 756}#))
                 #f)))
           (#{global-extend\ 127}#
             (lambda (#{type\ 757}# #{sym\ 758}# #{val\ 759}#)
               (#{put-global-definition-hook\ 92}#
                 #{sym\ 758}#
                 #{type\ 757}#
                 #{val\ 759}#)))
           (#{lookup\ 126}#
             (lambda (#{x\ 760}# #{r\ 761}# #{mod\ 762}#)
               (let ((#{t\ 763}# (assq #{x\ 760}# #{r\ 761}#)))
                 (if #{t\ 763}#
                   (cdr #{t\ 763}#)
                   (if (symbol? #{x\ 760}#)
                     (let ((#{t\ 764}# (#{get-global-definition-hook\ 93}#
                                         #{x\ 760}#
                                         #{mod\ 762}#)))
                       (if #{t\ 764}# #{t\ 764}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 125}#
             (lambda (#{r\ 765}#)
               (if (null? #{r\ 765}#)
                 '()
                 (let ((#{a\ 766}# (car #{r\ 765}#)))
                   (if (eq? (cadr #{a\ 766}#) (quote macro))
                     (cons #{a\ 766}#
                           (#{macros-only-env\ 125}# (cdr #{r\ 765}#)))
                     (#{macros-only-env\ 125}# (cdr #{r\ 765}#)))))))
           (#{extend-var-env\ 124}#
             (lambda (#{labels\ 767}# #{vars\ 768}# #{r\ 769}#)
               (if (null? #{labels\ 767}#)
                 #{r\ 769}#
                 (#{extend-var-env\ 124}#
                   (cdr #{labels\ 767}#)
                   (cdr #{vars\ 768}#)
                   (cons (cons (car #{labels\ 767}#)
                               (cons (quote lexical) (car #{vars\ 768}#)))
                         #{r\ 769}#)))))
           (#{extend-env\ 123}#
             (lambda (#{labels\ 770}# #{bindings\ 771}# #{r\ 772}#)
               (if (null? #{labels\ 770}#)
                 #{r\ 772}#
                 (#{extend-env\ 123}#
                   (cdr #{labels\ 770}#)
                   (cdr #{bindings\ 771}#)
                   (cons (cons (car #{labels\ 770}#)
                               (car #{bindings\ 771}#))
                         #{r\ 772}#)))))
           (#{binding-value\ 122}# cdr)
           (#{binding-type\ 121}# car)
           (#{source-annotation\ 120}#
             (lambda (#{x\ 773}#)
               (if (#{syntax-object?\ 113}# #{x\ 773}#)
                 (#{source-annotation\ 120}#
                   (#{syntax-object-expression\ 114}# #{x\ 773}#))
                 (if (pair? #{x\ 773}#)
                   (let ((#{props\ 774}# (source-properties #{x\ 773}#)))
                     (if (pair? #{props\ 774}#) #{props\ 774}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 119}#
             (lambda (#{x\ 775}# #{update\ 776}#)
               (vector-set! #{x\ 775}# 3 #{update\ 776}#)))
           (#{set-syntax-object-wrap!\ 118}#
             (lambda (#{x\ 777}# #{update\ 778}#)
               (vector-set! #{x\ 777}# 2 #{update\ 778}#)))
           (#{set-syntax-object-expression!\ 117}#
             (lambda (#{x\ 779}# #{update\ 780}#)
               (vector-set! #{x\ 779}# 1 #{update\ 780}#)))
           (#{syntax-object-module\ 116}#
             (lambda (#{x\ 781}#) (vector-ref #{x\ 781}# 3)))
           (#{syntax-object-wrap\ 115}#
             (lambda (#{x\ 782}#) (vector-ref #{x\ 782}# 2)))
           (#{syntax-object-expression\ 114}#
             (lambda (#{x\ 783}#) (vector-ref #{x\ 783}# 1)))
           (#{syntax-object?\ 113}#
             (lambda (#{x\ 784}#)
               (if (vector? #{x\ 784}#)
                 (if (= (vector-length #{x\ 784}#) 4)
                   (eq? (vector-ref #{x\ 784}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 112}#
             (lambda (#{expression\ 785}#
                      #{wrap\ 786}#
                      #{module\ 787}#)
               (vector
                 'syntax-object
                 #{expression\ 785}#
                 #{wrap\ 786}#
                 #{module\ 787}#)))
           (#{build-letrec\ 111}#
             (lambda (#{src\ 788}#
                      #{ids\ 789}#
                      #{vars\ 790}#
                      #{val-exps\ 791}#
                      #{body-exp\ 792}#)
               (if (null? #{vars\ 790}#)
                 #{body-exp\ 792}#
                 (let ((#{atom-key\ 793}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 793}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 789}#
                         #{val-exps\ 791}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 788}#
                        #{ids\ 789}#
                        #{vars\ 790}#
                        #{val-exps\ 791}#
                        #{body-exp\ 792}#))
                     (#{decorate-source\ 94}#
                       (list 'letrec
                             (map list #{vars\ 790}# #{val-exps\ 791}#)
                             #{body-exp\ 792}#)
                       #{src\ 788}#))))))
           (#{build-named-let\ 110}#
             (lambda (#{src\ 794}#
                      #{ids\ 795}#
                      #{vars\ 796}#
                      #{val-exps\ 797}#
                      #{body-exp\ 798}#)
               (let ((#{f\ 799}# (car #{vars\ 796}#))
                     (#{f-name\ 800}# (car #{ids\ 795}#))
                     (#{vars\ 801}# (cdr #{vars\ 796}#))
                     (#{ids\ 802}# (cdr #{ids\ 795}#)))
                 (let ((#{atom-key\ 803}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 803}# (quote (c)))
                     (let ((#{proc\ 804}#
                             (#{build-lambda\ 105}#
                               #{src\ 794}#
                               #{ids\ 802}#
                               #{vars\ 801}#
                               #f
                               #{body-exp\ 798}#)))
                       (begin
                         (#{maybe-name-value!\ 103}#
                           #{f-name\ 800}#
                           #{proc\ 804}#)
                         (for-each
                           #{maybe-name-value!\ 103}#
                           #{ids\ 802}#
                           #{val-exps\ 797}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 794}#
                          (list #{f-name\ 800}#)
                          (list #{f\ 799}#)
                          (list #{proc\ 804}#)
                          (#{build-application\ 96}#
                            #{src\ 794}#
                            (#{build-lexical-reference\ 98}#
                              'fun
                              #{src\ 794}#
                              #{f-name\ 800}#
                              #{f\ 799}#)
                            #{val-exps\ 797}#))))
                     (#{decorate-source\ 94}#
                       (list 'let
                             #{f\ 799}#
                             (map list #{vars\ 801}# #{val-exps\ 797}#)
                             #{body-exp\ 798}#)
                       #{src\ 794}#))))))
           (#{build-let\ 109}#
             (lambda (#{src\ 805}#
                      #{ids\ 806}#
                      #{vars\ 807}#
                      #{val-exps\ 808}#
                      #{body-exp\ 809}#)
               (if (null? #{vars\ 807}#)
                 #{body-exp\ 809}#
                 (let ((#{atom-key\ 810}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 810}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 806}#
                         #{val-exps\ 808}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 805}#
                        #{ids\ 806}#
                        #{vars\ 807}#
                        #{val-exps\ 808}#
                        #{body-exp\ 809}#))
                     (#{decorate-source\ 94}#
                       (list 'let
                             (map list #{vars\ 807}# #{val-exps\ 808}#)
                             #{body-exp\ 809}#)
                       #{src\ 805}#))))))
           (#{build-sequence\ 108}#
             (lambda (#{src\ 811}# #{exps\ 812}#)
               (if (null? (cdr #{exps\ 812}#))
                 (car #{exps\ 812}#)
                 (let ((#{atom-key\ 813}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 813}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 811}#
                      #{exps\ 812}#)
                     (#{decorate-source\ 94}#
                       (cons (quote begin) #{exps\ 812}#)
                       #{src\ 811}#))))))
           (#{build-data\ 107}#
             (lambda (#{src\ 814}# #{exp\ 815}#)
               (let ((#{atom-key\ 816}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 816}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 814}#
                    #{exp\ 815}#)
                   (#{decorate-source\ 94}#
                     (if (if (self-evaluating? #{exp\ 815}#)
                           (not (vector? #{exp\ 815}#))
                           #f)
                       #{exp\ 815}#
                       (list (quote quote) #{exp\ 815}#))
                     #{src\ 814}#)))))
           (#{build-primref\ 106}#
             (lambda (#{src\ 817}# #{name\ 818}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 819}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 819}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 817}#
                      #{name\ 818}#)
                     (#{decorate-source\ 94}#
                       #{name\ 818}#
                       #{src\ 817}#)))
                 (let ((#{atom-key\ 820}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 820}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 817}#
                      '(guile)
                      #{name\ 818}#
                      #f)
                     (#{decorate-source\ 94}#
                       (list (quote @@) (quote (guile)) #{name\ 818}#)
                       #{src\ 817}#))))))
           (#{build-lambda\ 105}#
             (lambda (#{src\ 821}#
                      #{ids\ 822}#
                      #{vars\ 823}#
                      #{docstring\ 824}#
                      #{exp\ 825}#)
               (let ((#{atom-key\ 826}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 826}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 821}#
                    #{ids\ 822}#
                    #{vars\ 823}#
                    (if #{docstring\ 824}#
                      (list (cons (quote documentation) #{docstring\ 824}#))
                      '())
                    #{exp\ 825}#)
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons #{vars\ 823}#
                                 (append
                                   (if #{docstring\ 824}#
                                     (list #{docstring\ 824}#)
                                     '())
                                   (list #{exp\ 825}#))))
                     #{src\ 821}#)))))
           (#{build-global-definition\ 104}#
             (lambda (#{source\ 827}# #{var\ 828}# #{exp\ 829}#)
               (let ((#{atom-key\ 830}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 830}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 103}#
                       #{var\ 828}#
                       #{exp\ 829}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 827}#
                      #{var\ 828}#
                      #{exp\ 829}#))
                   (#{decorate-source\ 94}#
                     (list (quote define) #{var\ 828}# #{exp\ 829}#)
                     #{source\ 827}#)))))
           (#{maybe-name-value!\ 103}#
             (lambda (#{name\ 831}# #{val\ 832}#)
               (if ((@ (language tree-il) lambda?) #{val\ 832}#)
                 (let ((#{meta\ 833}#
                         ((@ (language tree-il) lambda-meta) #{val\ 832}#)))
                   (if (not (assq (quote name) #{meta\ 833}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 832}#
                      (acons (quote name) #{name\ 831}# #{meta\ 833}#)))))))
           (#{build-global-assignment\ 102}#
             (lambda (#{source\ 834}#
                      #{var\ 835}#
                      #{exp\ 836}#
                      #{mod\ 837}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 837}#
                 #{var\ 835}#
                 (lambda (#{mod\ 838}# #{var\ 839}# #{public?\ 840}#)
                   (let ((#{atom-key\ 841}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 841}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 834}#
                        #{mod\ 838}#
                        #{var\ 839}#
                        #{public?\ 840}#
                        #{exp\ 836}#)
                       (#{decorate-source\ 94}#
                         (list 'set!
                               (list (if #{public?\ 840}# (quote @) (quote @@))
                                     #{mod\ 838}#
                                     #{var\ 839}#)
                               #{exp\ 836}#)
                         #{source\ 834}#))))
                 (lambda (#{var\ 842}#)
                   (let ((#{atom-key\ 843}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 843}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 834}#
                        #{var\ 842}#
                        #{exp\ 836}#)
                       (#{decorate-source\ 94}#
                         (list (quote set!) #{var\ 842}# #{exp\ 836}#)
                         #{source\ 834}#)))))))
           (#{build-global-reference\ 101}#
             (lambda (#{source\ 844}# #{var\ 845}# #{mod\ 846}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 846}#
                 #{var\ 845}#
                 (lambda (#{mod\ 847}# #{var\ 848}# #{public?\ 849}#)
                   (let ((#{atom-key\ 850}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 850}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 844}#
                        #{mod\ 847}#
                        #{var\ 848}#
                        #{public?\ 849}#)
                       (#{decorate-source\ 94}#
                         (list (if #{public?\ 849}# (quote @) (quote @@))
                               #{mod\ 847}#
                               #{var\ 848}#)
                         #{source\ 844}#))))
                 (lambda (#{var\ 851}#)
                   (let ((#{atom-key\ 852}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 852}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 844}#
                        #{var\ 851}#)
                       (#{decorate-source\ 94}#
                         #{var\ 851}#
                         #{source\ 844}#)))))))
           (#{analyze-variable\ 100}#
             (lambda (#{mod\ 853}#
                      #{var\ 854}#
                      #{modref-cont\ 855}#
                      #{bare-cont\ 856}#)
               (if (not #{mod\ 853}#)
                 (#{bare-cont\ 856}# #{var\ 854}#)
                 (let ((#{kind\ 857}# (car #{mod\ 853}#))
                       (#{mod\ 858}# (cdr #{mod\ 853}#)))
                   (if (memv #{kind\ 857}# (quote (public)))
                     (#{modref-cont\ 855}#
                       #{mod\ 858}#
                       #{var\ 854}#
                       #t)
                     (if (memv #{kind\ 857}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 858}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 855}#
                           #{mod\ 858}#
                           #{var\ 854}#
                           #f)
                         (#{bare-cont\ 856}# #{var\ 854}#))
                       (if (memv #{kind\ 857}# (quote (bare)))
                         (#{bare-cont\ 856}# #{var\ 854}#)
                         (if (memv #{kind\ 857}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 858}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 858}#)
                                   #{var\ 854}#)
                                 #f)
                             (#{modref-cont\ 855}#
                               #{mod\ 858}#
                               #{var\ 854}#
                               #f)
                             (#{bare-cont\ 856}# #{var\ 854}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 854}#
                             #{mod\ 858}#)))))))))
           (#{build-lexical-assignment\ 99}#
             (lambda (#{source\ 859}#
                      #{name\ 860}#
                      #{var\ 861}#
                      #{exp\ 862}#)
               (let ((#{atom-key\ 863}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 863}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 859}#
                    #{name\ 860}#
                    #{var\ 861}#
                    #{exp\ 862}#)
                   (#{decorate-source\ 94}#
                     (list (quote set!) #{var\ 861}# #{exp\ 862}#)
                     #{source\ 859}#)))))
           (#{build-lexical-reference\ 98}#
             (lambda (#{type\ 864}#
                      #{source\ 865}#
                      #{name\ 866}#
                      #{var\ 867}#)
               (let ((#{atom-key\ 868}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 868}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 865}#
                    #{name\ 866}#
                    #{var\ 867}#)
                   (#{decorate-source\ 94}#
                     #{var\ 867}#
                     #{source\ 865}#)))))
           (#{build-conditional\ 97}#
             (lambda (#{source\ 869}#
                      #{test-exp\ 870}#
                      #{then-exp\ 871}#
                      #{else-exp\ 872}#)
               (let ((#{atom-key\ 873}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 873}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 869}#
                    #{test-exp\ 870}#
                    #{then-exp\ 871}#
                    #{else-exp\ 872}#)
                   (#{decorate-source\ 94}#
                     (if (equal? #{else-exp\ 872}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 870}#
                             #{then-exp\ 871}#)
                       (list 'if
                             #{test-exp\ 870}#
                             #{then-exp\ 871}#
                             #{else-exp\ 872}#))
                     #{source\ 869}#)))))
           (#{build-application\ 96}#
             (lambda (#{source\ 874}#
                      #{fun-exp\ 875}#
                      #{arg-exps\ 876}#)
               (let ((#{atom-key\ 877}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 877}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 874}#
                    #{fun-exp\ 875}#
                    #{arg-exps\ 876}#)
                   (#{decorate-source\ 94}#
                     (cons #{fun-exp\ 875}# #{arg-exps\ 876}#)
                     #{source\ 874}#)))))
           (#{build-void\ 95}#
             (lambda (#{source\ 878}#)
               (let ((#{atom-key\ 879}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 879}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 878}#)
                   (#{decorate-source\ 94}#
                     '(if #f #f)
                     #{source\ 878}#)))))
           (#{decorate-source\ 94}#
             (lambda (#{e\ 880}# #{s\ 881}#)
               (begin
                 (if (if (pair? #{e\ 880}#) #{s\ 881}# #f)
                   (set-source-properties! #{e\ 880}# #{s\ 881}#))
                 #{e\ 880}#)))
           (#{get-global-definition-hook\ 93}#
             (lambda (#{symbol\ 882}# #{module\ 883}#)
               (begin
                 (if (if (not #{module\ 883}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 882}#))
                 (let ((#{v\ 884}# (module-variable
                                     (if #{module\ 883}#
                                       (resolve-module (cdr #{module\ 883}#))
                                       (current-module))
                                     #{symbol\ 882}#)))
                   (if #{v\ 884}#
                     (if (variable-bound? #{v\ 884}#)
                       (let ((#{val\ 885}# (variable-ref #{v\ 884}#)))
                         (if (macro? #{val\ 885}#)
                           (if (syncase-macro-type #{val\ 885}#)
                             (cons (syncase-macro-type #{val\ 885}#)
                                   (syncase-macro-binding #{val\ 885}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 92}#
             (lambda (#{symbol\ 886}# #{type\ 887}# #{val\ 888}#)
               (let ((#{existing\ 889}#
                       (let ((#{v\ 890}# (module-variable
                                           (current-module)
                                           #{symbol\ 886}#)))
                         (if #{v\ 890}#
                           (if (variable-bound? #{v\ 890}#)
                             (let ((#{val\ 891}# (variable-ref #{v\ 890}#)))
                               (if (macro? #{val\ 891}#)
                                 (if (not (syncase-macro-type #{val\ 891}#))
                                   #{val\ 891}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 886}#
                   (if #{existing\ 889}#
                     (make-extended-syncase-macro
                       #{existing\ 889}#
                       #{type\ 887}#
                       #{val\ 888}#)
                     (make-syncase-macro #{type\ 887}# #{val\ 888}#))))))
           (#{local-eval-hook\ 91}#
             (lambda (#{x\ 892}# #{mod\ 893}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 894}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 894}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 892}#)
                           #{x\ 892}#))))))
           (#{top-level-eval-hook\ 90}#
             (lambda (#{x\ 895}# #{mod\ 896}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 897}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 897}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 895}#)
                           #{x\ 895}#))))))
           (#{fx<\ 89}# <)
           (#{fx=\ 88}# =)
           (#{fx-\ 87}# -)
           (#{fx+\ 86}# +)
           (#{*mode*\ 85}# (make-fluid))
           (#{noexpand\ 84}# "noexpand"))
    (begin
      (#{global-extend\ 127}#
        'local-syntax
        'letrec-syntax
        #t)
      (#{global-extend\ 127}#
        'local-syntax
        'let-syntax
        #f)
      (#{global-extend\ 127}#
        'core
        'fluid-let-syntax
        (lambda (#{e\ 898}#
                 #{r\ 899}#
                 #{w\ 900}#
                 #{s\ 901}#
                 #{mod\ 902}#)
          ((lambda (#{tmp\ 903}#)
             ((lambda (#{tmp\ 904}#)
                (if (if #{tmp\ 904}#
                      (apply (lambda (#{_\ 905}#
                                      #{var\ 906}#
                                      #{val\ 907}#
                                      #{e1\ 908}#
                                      #{e2\ 909}#)
                               (#{valid-bound-ids?\ 154}# #{var\ 906}#))
                             #{tmp\ 904}#)
                      #f)
                  (apply (lambda (#{_\ 911}#
                                  #{var\ 912}#
                                  #{val\ 913}#
                                  #{e1\ 914}#
                                  #{e2\ 915}#)
                           (let ((#{names\ 916}#
                                   (map (lambda (#{x\ 917}#)
                                          (#{id-var-name\ 151}#
                                            #{x\ 917}#
                                            #{w\ 900}#))
                                        #{var\ 912}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 919}# #{n\ 920}#)
                                   (let ((#{atom-key\ 921}#
                                           (#{binding-type\ 121}#
                                             (#{lookup\ 126}#
                                               #{n\ 920}#
                                               #{r\ 899}#
                                               #{mod\ 902}#))))
                                     (if (memv #{atom-key\ 921}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 898}#
                                         (#{source-wrap\ 158}#
                                           #{id\ 919}#
                                           #{w\ 900}#
                                           #{s\ 901}#
                                           #{mod\ 902}#)))))
                                 #{var\ 912}#
                                 #{names\ 916}#)
                               (#{chi-body\ 169}#
                                 (cons #{e1\ 914}# #{e2\ 915}#)
                                 (#{source-wrap\ 158}#
                                   #{e\ 898}#
                                   #{w\ 900}#
                                   #{s\ 901}#
                                   #{mod\ 902}#)
                                 (#{extend-env\ 123}#
                                   #{names\ 916}#
                                   (let ((#{trans-r\ 924}#
                                           (#{macros-only-env\ 125}#
                                             #{r\ 899}#)))
                                     (map (lambda (#{x\ 925}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 172}#
                                                    (#{chi\ 165}#
                                                      #{x\ 925}#
                                                      #{trans-r\ 924}#
                                                      #{w\ 900}#
                                                      #{mod\ 902}#)
                                                    #{mod\ 902}#)))
                                          #{val\ 913}#))
                                   #{r\ 899}#)
                                 #{w\ 900}#
                                 #{mod\ 902}#))))
                         #{tmp\ 904}#)
                  ((lambda (#{_\ 927}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 158}#
                         #{e\ 898}#
                         #{w\ 900}#
                         #{s\ 901}#
                         #{mod\ 902}#)))
                   #{tmp\ 903}#)))
              ($sc-dispatch
                #{tmp\ 903}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 898}#)))
      (#{global-extend\ 127}#
        'core
        'quote
        (lambda (#{e\ 928}#
                 #{r\ 929}#
                 #{w\ 930}#
                 #{s\ 931}#
                 #{mod\ 932}#)
          ((lambda (#{tmp\ 933}#)
             ((lambda (#{tmp\ 934}#)
                (if #{tmp\ 934}#
                  (apply (lambda (#{_\ 935}# #{e\ 936}#)
                           (#{build-data\ 107}#
                             #{s\ 931}#
                             (#{strip\ 175}# #{e\ 936}# #{w\ 930}#)))
                         #{tmp\ 934}#)
                  ((lambda (#{_\ 937}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 158}#
                         #{e\ 928}#
                         #{w\ 930}#
                         #{s\ 931}#
                         #{mod\ 932}#)))
                   #{tmp\ 933}#)))
              ($sc-dispatch #{tmp\ 933}# (quote (any any)))))
           #{e\ 928}#)))
      (#{global-extend\ 127}#
        'core
        'syntax
        (letrec ((#{regen\ 945}#
                   (lambda (#{x\ 946}#)
                     (let ((#{atom-key\ 947}# (car #{x\ 946}#)))
                       (if (memv #{atom-key\ 947}# (quote (ref)))
                         (#{build-lexical-reference\ 98}#
                           'value
                           #f
                           (cadr #{x\ 946}#)
                           (cadr #{x\ 946}#))
                         (if (memv #{atom-key\ 947}# (quote (primitive)))
                           (#{build-primref\ 106}# #f (cadr #{x\ 946}#))
                           (if (memv #{atom-key\ 947}# (quote (quote)))
                             (#{build-data\ 107}# #f (cadr #{x\ 946}#))
                             (if (memv #{atom-key\ 947}# (quote (lambda)))
                               (#{build-lambda\ 105}#
                                 #f
                                 (cadr #{x\ 946}#)
                                 (cadr #{x\ 946}#)
                                 #f
                                 (#{regen\ 945}# (caddr #{x\ 946}#)))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-primref\ 106}# #f (car #{x\ 946}#))
                                 (map #{regen\ 945}# (cdr #{x\ 946}#))))))))))
                 (#{gen-vector\ 944}#
                   (lambda (#{x\ 948}#)
                     (if (eq? (car #{x\ 948}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 948}#))
                       (if (eq? (car #{x\ 948}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 948}#)))
                         (list (quote list->vector) #{x\ 948}#)))))
                 (#{gen-append\ 943}#
                   (lambda (#{x\ 949}# #{y\ 950}#)
                     (if (equal? #{y\ 950}# (quote (quote ())))
                       #{x\ 949}#
                       (list (quote append) #{x\ 949}# #{y\ 950}#))))
                 (#{gen-cons\ 942}#
                   (lambda (#{x\ 951}# #{y\ 952}#)
                     (let ((#{atom-key\ 953}# (car #{y\ 952}#)))
                       (if (memv #{atom-key\ 953}# (quote (quote)))
                         (if (eq? (car #{x\ 951}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 951}#) (cadr #{y\ 952}#)))
                           (if (eq? (cadr #{y\ 952}#) (quote ()))
                             (list (quote list) #{x\ 951}#)
                             (list (quote cons) #{x\ 951}# #{y\ 952}#)))
                         (if (memv #{atom-key\ 953}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 951}# (cdr #{y\ 952}#)))
                           (list (quote cons) #{x\ 951}# #{y\ 952}#))))))
                 (#{gen-map\ 941}#
                   (lambda (#{e\ 954}# #{map-env\ 955}#)
                     (let ((#{formals\ 956}# (map cdr #{map-env\ 955}#))
                           (#{actuals\ 957}#
                             (map (lambda (#{x\ 958}#)
                                    (list (quote ref) (car #{x\ 958}#)))
                                  #{map-env\ 955}#)))
                       (if (eq? (car #{e\ 954}#) (quote ref))
                         (car #{actuals\ 957}#)
                         (if (and-map
                               (lambda (#{x\ 959}#)
                                 (if (eq? (car #{x\ 959}#) (quote ref))
                                   (memq (cadr #{x\ 959}#) #{formals\ 956}#)
                                   #f))
                               (cdr #{e\ 954}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 954}#))
                                       (map (let ((#{r\ 960}# (map cons
                                                                   #{formals\ 956}#
                                                                   #{actuals\ 957}#)))
                                              (lambda (#{x\ 961}#)
                                                (cdr (assq (cadr #{x\ 961}#)
                                                           #{r\ 960}#))))
                                            (cdr #{e\ 954}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 956}#
                                             #{e\ 954}#)
                                       #{actuals\ 957}#)))))))
                 (#{gen-mappend\ 940}#
                   (lambda (#{e\ 962}# #{map-env\ 963}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 941}# #{e\ 962}# #{map-env\ 963}#))))
                 (#{gen-ref\ 939}#
                   (lambda (#{src\ 964}#
                            #{var\ 965}#
                            #{level\ 966}#
                            #{maps\ 967}#)
                     (if (#{fx=\ 88}# #{level\ 966}# 0)
                       (values #{var\ 965}# #{maps\ 967}#)
                       (if (null? #{maps\ 967}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 964}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 939}#
                               #{src\ 964}#
                               #{var\ 965}#
                               (#{fx-\ 87}# #{level\ 966}# 1)
                               (cdr #{maps\ 967}#)))
                           (lambda (#{outer-var\ 968}# #{outer-maps\ 969}#)
                             (let ((#{b\ 970}# (assq #{outer-var\ 968}#
                                                     (car #{maps\ 967}#))))
                               (if #{b\ 970}#
                                 (values (cdr #{b\ 970}#) #{maps\ 967}#)
                                 (let ((#{inner-var\ 971}#
                                         (#{gen-var\ 176}# (quote tmp))))
                                   (values
                                     #{inner-var\ 971}#
                                     (cons (cons (cons #{outer-var\ 968}#
                                                       #{inner-var\ 971}#)
                                                 (car #{maps\ 967}#))
                                           #{outer-maps\ 969}#)))))))))))
                 (#{gen-syntax\ 938}#
                   (lambda (#{src\ 972}#
                            #{e\ 973}#
                            #{r\ 974}#
                            #{maps\ 975}#
                            #{ellipsis?\ 976}#
                            #{mod\ 977}#)
                     (if (#{id?\ 129}# #{e\ 973}#)
                       (let ((#{label\ 978}#
                               (#{id-var-name\ 151}# #{e\ 973}# (quote (())))))
                         (let ((#{b\ 979}# (#{lookup\ 126}#
                                             #{label\ 978}#
                                             #{r\ 974}#
                                             #{mod\ 977}#)))
                           (if (eq? (#{binding-type\ 121}# #{b\ 979}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 980}#
                                         (#{binding-value\ 122}# #{b\ 979}#)))
                                   (#{gen-ref\ 939}#
                                     #{src\ 972}#
                                     (car #{var.lev\ 980}#)
                                     (cdr #{var.lev\ 980}#)
                                     #{maps\ 975}#)))
                               (lambda (#{var\ 981}# #{maps\ 982}#)
                                 (values
                                   (list (quote ref) #{var\ 981}#)
                                   #{maps\ 982}#)))
                             (if (#{ellipsis?\ 976}# #{e\ 973}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 972}#)
                               (values
                                 (list (quote quote) #{e\ 973}#)
                                 #{maps\ 975}#)))))
                       ((lambda (#{tmp\ 983}#)
                          ((lambda (#{tmp\ 984}#)
                             (if (if #{tmp\ 984}#
                                   (apply (lambda (#{dots\ 985}# #{e\ 986}#)
                                            (#{ellipsis?\ 976}# #{dots\ 985}#))
                                          #{tmp\ 984}#)
                                   #f)
                               (apply (lambda (#{dots\ 987}# #{e\ 988}#)
                                        (#{gen-syntax\ 938}#
                                          #{src\ 972}#
                                          #{e\ 988}#
                                          #{r\ 974}#
                                          #{maps\ 975}#
                                          (lambda (#{x\ 989}#) #f)
                                          #{mod\ 977}#))
                                      #{tmp\ 984}#)
                               ((lambda (#{tmp\ 990}#)
                                  (if (if #{tmp\ 990}#
                                        (apply (lambda (#{x\ 991}#
                                                        #{dots\ 992}#
                                                        #{y\ 993}#)
                                                 (#{ellipsis?\ 976}#
                                                   #{dots\ 992}#))
                                               #{tmp\ 990}#)
                                        #f)
                                    (apply (lambda (#{x\ 994}#
                                                    #{dots\ 995}#
                                                    #{y\ 996}#)
                                             (letrec ((#{f\ 997}# (lambda (#{y\ 998}#
                                                                           #{k\ 999}#)
                                                                    ((lambda (#{tmp\ 1003}#)
                                                                       ((lambda (#{tmp\ 1004}#)
                                                                          (if (if #{tmp\ 1004}#
                                                                                (apply (lambda (#{dots\ 1005}#
                                                                                                #{y\ 1006}#)
                                                                                         (#{ellipsis?\ 976}#
                                                                                           #{dots\ 1005}#))
                                                                                       #{tmp\ 1004}#)
                                                                                #f)
                                                                            (apply (lambda (#{dots\ 1007}#
                                                                                            #{y\ 1008}#)
                                                                                     (#{f\ 997}# #{y\ 1008}#
                                                                                                 (lambda (#{maps\ 1009}#)
                                                                                                   (call-with-values
                                                                                                     (lambda ()
                                                                                                       (#{k\ 999}# (cons '()
                                                                                                                         #{maps\ 1009}#)))
                                                                                                     (lambda (#{x\ 1010}#
                                                                                                              #{maps\ 1011}#)
                                                                                                       (if (null? (car #{maps\ 1011}#))
                                                                                                         (syntax-violation
                                                                                                           'syntax
                                                                                                           "extra ellipsis"
                                                                                                           #{src\ 972}#)
                                                                                                         (values
                                                                                                           (#{gen-mappend\ 940}#
                                                                                                             #{x\ 1010}#
                                                                                                             (car #{maps\ 1011}#))
                                                                                                           (cdr #{maps\ 1011}#))))))))
                                                                                   #{tmp\ 1004}#)
                                                                            ((lambda (#{_\ 1012}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{gen-syntax\ 938}#
                                                                                     #{src\ 972}#
                                                                                     #{y\ 998}#
                                                                                     #{r\ 974}#
                                                                                     #{maps\ 975}#
                                                                                     #{ellipsis?\ 976}#
                                                                                     #{mod\ 977}#))
                                                                                 (lambda (#{y\ 1013}#
                                                                                          #{maps\ 1014}#)
                                                                                   (call-with-values
                                                                                     (lambda ()
                                                                                       (#{k\ 999}# #{maps\ 1014}#))
                                                                                     (lambda (#{x\ 1015}#
                                                                                              #{maps\ 1016}#)
                                                                                       (values
                                                                                         (#{gen-append\ 943}#
                                                                                           #{x\ 1015}#
                                                                                           #{y\ 1013}#)
                                                                                         #{maps\ 1016}#))))))
                                                                             #{tmp\ 1003}#)))
                                                                        ($sc-dispatch
                                                                          #{tmp\ 1003}#
                                                                          '(any .
                                                                                any))))
                                                                     #{y\ 998}#))))
                                               (#{f\ 997}# #{y\ 996}#
                                                           (lambda (#{maps\ 1000}#)
                                                             (call-with-values
                                                               (lambda ()
                                                                 (#{gen-syntax\ 938}#
                                                                   #{src\ 972}#
                                                                   #{x\ 994}#
                                                                   #{r\ 974}#
                                                                   (cons '()
                                                                         #{maps\ 1000}#)
                                                                   #{ellipsis?\ 976}#
                                                                   #{mod\ 977}#))
                                                               (lambda (#{x\ 1001}#
                                                                        #{maps\ 1002}#)
                                                                 (if (null? (car #{maps\ 1002}#))
                                                                   (syntax-violation
                                                                     'syntax
                                                                     "extra ellipsis"
                                                                     #{src\ 972}#)
                                                                   (values
                                                                     (#{gen-map\ 941}#
                                                                       #{x\ 1001}#
                                                                       (car #{maps\ 1002}#))
                                                                     (cdr #{maps\ 1002}#)))))))))
                                           #{tmp\ 990}#)
                                    ((lambda (#{tmp\ 1017}#)
                                       (if #{tmp\ 1017}#
                                         (apply (lambda (#{x\ 1018}#
                                                         #{y\ 1019}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 938}#
                                                        #{src\ 972}#
                                                        #{x\ 1018}#
                                                        #{r\ 974}#
                                                        #{maps\ 975}#
                                                        #{ellipsis?\ 976}#
                                                        #{mod\ 977}#))
                                                    (lambda (#{x\ 1020}#
                                                             #{maps\ 1021}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 938}#
                                                            #{src\ 972}#
                                                            #{y\ 1019}#
                                                            #{r\ 974}#
                                                            #{maps\ 1021}#
                                                            #{ellipsis?\ 976}#
                                                            #{mod\ 977}#))
                                                        (lambda (#{y\ 1022}#
                                                                 #{maps\ 1023}#)
                                                          (values
                                                            (#{gen-cons\ 942}#
                                                              #{x\ 1020}#
                                                              #{y\ 1022}#)
                                                            #{maps\ 1023}#))))))
                                                #{tmp\ 1017}#)
                                         ((lambda (#{tmp\ 1024}#)
                                            (if #{tmp\ 1024}#
                                              (apply (lambda (#{e1\ 1025}#
                                                              #{e2\ 1026}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 938}#
                                                             #{src\ 972}#
                                                             (cons #{e1\ 1025}#
                                                                   #{e2\ 1026}#)
                                                             #{r\ 974}#
                                                             #{maps\ 975}#
                                                             #{ellipsis?\ 976}#
                                                             #{mod\ 977}#))
                                                         (lambda (#{e\ 1028}#
                                                                  #{maps\ 1029}#)
                                                           (values
                                                             (#{gen-vector\ 944}#
                                                               #{e\ 1028}#)
                                                             #{maps\ 1029}#))))
                                                     #{tmp\ 1024}#)
                                              ((lambda (#{_\ 1030}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 973}#)
                                                   #{maps\ 975}#))
                                               #{tmp\ 983}#)))
                                          ($sc-dispatch
                                            #{tmp\ 983}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 983}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 983}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 983}# (quote (any any)))))
                        #{e\ 973}#)))))
          (lambda (#{e\ 1031}#
                   #{r\ 1032}#
                   #{w\ 1033}#
                   #{s\ 1034}#
                   #{mod\ 1035}#)
            (let ((#{e\ 1036}#
                    (#{source-wrap\ 158}#
                      #{e\ 1031}#
                      #{w\ 1033}#
                      #{s\ 1034}#
                      #{mod\ 1035}#)))
              ((lambda (#{tmp\ 1037}#)
                 ((lambda (#{tmp\ 1038}#)
                    (if #{tmp\ 1038}#
                      (apply (lambda (#{_\ 1039}# #{x\ 1040}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 938}#
                                     #{e\ 1036}#
                                     #{x\ 1040}#
                                     #{r\ 1032}#
                                     '()
                                     #{ellipsis?\ 174}#
                                     #{mod\ 1035}#))
                                 (lambda (#{e\ 1041}# #{maps\ 1042}#)
                                   (#{regen\ 945}# #{e\ 1041}#))))
                             #{tmp\ 1038}#)
                      ((lambda (#{_\ 1043}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 1036}#))
                       #{tmp\ 1037}#)))
                  ($sc-dispatch #{tmp\ 1037}# (quote (any any)))))
               #{e\ 1036}#)))))
      (#{global-extend\ 127}#
        'core
        'lambda
        (lambda (#{e\ 1044}#
                 #{r\ 1045}#
                 #{w\ 1046}#
                 #{s\ 1047}#
                 #{mod\ 1048}#)
          ((lambda (#{tmp\ 1049}#)
             ((lambda (#{tmp\ 1050}#)
                (if #{tmp\ 1050}#
                  (apply (lambda (#{_\ 1051}# #{c\ 1052}#)
                           (#{chi-lambda-clause\ 170}#
                             (#{source-wrap\ 158}#
                               #{e\ 1044}#
                               #{w\ 1046}#
                               #{s\ 1047}#
                               #{mod\ 1048}#)
                             #f
                             #{c\ 1052}#
                             #{r\ 1045}#
                             #{w\ 1046}#
                             #{mod\ 1048}#
                             (lambda (#{names\ 1053}#
                                      #{vars\ 1054}#
                                      #{docstring\ 1055}#
                                      #{body\ 1056}#)
                               (#{build-lambda\ 105}#
                                 #{s\ 1047}#
                                 #{names\ 1053}#
                                 #{vars\ 1054}#
                                 #{docstring\ 1055}#
                                 #{body\ 1056}#))))
                         #{tmp\ 1050}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1049}#)))
              ($sc-dispatch #{tmp\ 1049}# (quote (any . any)))))
           #{e\ 1044}#)))
      (#{global-extend\ 127}#
        'core
        'let
        (letrec ((#{chi-let\ 1057}#
                   (lambda (#{e\ 1058}#
                            #{r\ 1059}#
                            #{w\ 1060}#
                            #{s\ 1061}#
                            #{mod\ 1062}#
                            #{constructor\ 1063}#
                            #{ids\ 1064}#
                            #{vals\ 1065}#
                            #{exps\ 1066}#)
                     (if (not (#{valid-bound-ids?\ 154}# #{ids\ 1064}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 1058}#)
                       (let ((#{labels\ 1067}#
                               (#{gen-labels\ 135}# #{ids\ 1064}#))
                             (#{new-vars\ 1068}#
                               (map #{gen-var\ 176}# #{ids\ 1064}#)))
                         (let ((#{nw\ 1069}#
                                 (#{make-binding-wrap\ 146}#
                                   #{ids\ 1064}#
                                   #{labels\ 1067}#
                                   #{w\ 1060}#))
                               (#{nr\ 1070}#
                                 (#{extend-var-env\ 124}#
                                   #{labels\ 1067}#
                                   #{new-vars\ 1068}#
                                   #{r\ 1059}#)))
                           (#{constructor\ 1063}#
                             #{s\ 1061}#
                             (map syntax->datum #{ids\ 1064}#)
                             #{new-vars\ 1068}#
                             (map (lambda (#{x\ 1071}#)
                                    (#{chi\ 165}#
                                      #{x\ 1071}#
                                      #{r\ 1059}#
                                      #{w\ 1060}#
                                      #{mod\ 1062}#))
                                  #{vals\ 1065}#)
                             (#{chi-body\ 169}#
                               #{exps\ 1066}#
                               (#{source-wrap\ 158}#
                                 #{e\ 1058}#
                                 #{nw\ 1069}#
                                 #{s\ 1061}#
                                 #{mod\ 1062}#)
                               #{nr\ 1070}#
                               #{nw\ 1069}#
                               #{mod\ 1062}#))))))))
          (lambda (#{e\ 1072}#
                   #{r\ 1073}#
                   #{w\ 1074}#
                   #{s\ 1075}#
                   #{mod\ 1076}#)
            ((lambda (#{tmp\ 1077}#)
               ((lambda (#{tmp\ 1078}#)
                  (if (if #{tmp\ 1078}#
                        (apply (lambda (#{_\ 1079}#
                                        #{id\ 1080}#
                                        #{val\ 1081}#
                                        #{e1\ 1082}#
                                        #{e2\ 1083}#)
                                 (and-map #{id?\ 129}# #{id\ 1080}#))
                               #{tmp\ 1078}#)
                        #f)
                    (apply (lambda (#{_\ 1085}#
                                    #{id\ 1086}#
                                    #{val\ 1087}#
                                    #{e1\ 1088}#
                                    #{e2\ 1089}#)
                             (#{chi-let\ 1057}#
                               #{e\ 1072}#
                               #{r\ 1073}#
                               #{w\ 1074}#
                               #{s\ 1075}#
                               #{mod\ 1076}#
                               #{build-let\ 109}#
                               #{id\ 1086}#
                               #{val\ 1087}#
                               (cons #{e1\ 1088}# #{e2\ 1089}#)))
                           #{tmp\ 1078}#)
                    ((lambda (#{tmp\ 1093}#)
                       (if (if #{tmp\ 1093}#
                             (apply (lambda (#{_\ 1094}#
                                             #{f\ 1095}#
                                             #{id\ 1096}#
                                             #{val\ 1097}#
                                             #{e1\ 1098}#
                                             #{e2\ 1099}#)
                                      (if (#{id?\ 129}# #{f\ 1095}#)
                                        (and-map #{id?\ 129}# #{id\ 1096}#)
                                        #f))
                                    #{tmp\ 1093}#)
                             #f)
                         (apply (lambda (#{_\ 1101}#
                                         #{f\ 1102}#
                                         #{id\ 1103}#
                                         #{val\ 1104}#
                                         #{e1\ 1105}#
                                         #{e2\ 1106}#)
                                  (#{chi-let\ 1057}#
                                    #{e\ 1072}#
                                    #{r\ 1073}#
                                    #{w\ 1074}#
                                    #{s\ 1075}#
                                    #{mod\ 1076}#
                                    #{build-named-let\ 110}#
                                    (cons #{f\ 1102}# #{id\ 1103}#)
                                    #{val\ 1104}#
                                    (cons #{e1\ 1105}# #{e2\ 1106}#)))
                                #{tmp\ 1093}#)
                         ((lambda (#{_\ 1110}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 158}#
                                #{e\ 1072}#
                                #{w\ 1074}#
                                #{s\ 1075}#
                                #{mod\ 1076}#)))
                          #{tmp\ 1077}#)))
                     ($sc-dispatch
                       #{tmp\ 1077}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1077}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 1072}#))))
      (#{global-extend\ 127}#
        'core
        'letrec
        (lambda (#{e\ 1111}#
                 #{r\ 1112}#
                 #{w\ 1113}#
                 #{s\ 1114}#
                 #{mod\ 1115}#)
          ((lambda (#{tmp\ 1116}#)
             ((lambda (#{tmp\ 1117}#)
                (if (if #{tmp\ 1117}#
                      (apply (lambda (#{_\ 1118}#
                                      #{id\ 1119}#
                                      #{val\ 1120}#
                                      #{e1\ 1121}#
                                      #{e2\ 1122}#)
                               (and-map #{id?\ 129}# #{id\ 1119}#))
                             #{tmp\ 1117}#)
                      #f)
                  (apply (lambda (#{_\ 1124}#
                                  #{id\ 1125}#
                                  #{val\ 1126}#
                                  #{e1\ 1127}#
                                  #{e2\ 1128}#)
                           (let ((#{ids\ 1129}# #{id\ 1125}#))
                             (if (not (#{valid-bound-ids?\ 154}#
                                        #{ids\ 1129}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 1111}#)
                               (let ((#{labels\ 1131}#
                                       (#{gen-labels\ 135}# #{ids\ 1129}#))
                                     (#{new-vars\ 1132}#
                                       (map #{gen-var\ 176}# #{ids\ 1129}#)))
                                 (let ((#{w\ 1133}#
                                         (#{make-binding-wrap\ 146}#
                                           #{ids\ 1129}#
                                           #{labels\ 1131}#
                                           #{w\ 1113}#))
                                       (#{r\ 1134}#
                                         (#{extend-var-env\ 124}#
                                           #{labels\ 1131}#
                                           #{new-vars\ 1132}#
                                           #{r\ 1112}#)))
                                   (#{build-letrec\ 111}#
                                     #{s\ 1114}#
                                     (map syntax->datum #{ids\ 1129}#)
                                     #{new-vars\ 1132}#
                                     (map (lambda (#{x\ 1135}#)
                                            (#{chi\ 165}#
                                              #{x\ 1135}#
                                              #{r\ 1134}#
                                              #{w\ 1133}#
                                              #{mod\ 1115}#))
                                          #{val\ 1126}#)
                                     (#{chi-body\ 169}#
                                       (cons #{e1\ 1127}# #{e2\ 1128}#)
                                       (#{source-wrap\ 158}#
                                         #{e\ 1111}#
                                         #{w\ 1133}#
                                         #{s\ 1114}#
                                         #{mod\ 1115}#)
                                       #{r\ 1134}#
                                       #{w\ 1133}#
                                       #{mod\ 1115}#)))))))
                         #{tmp\ 1117}#)
                  ((lambda (#{_\ 1138}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 158}#
                         #{e\ 1111}#
                         #{w\ 1113}#
                         #{s\ 1114}#
                         #{mod\ 1115}#)))
                   #{tmp\ 1116}#)))
              ($sc-dispatch
                #{tmp\ 1116}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1111}#)))
      (#{global-extend\ 127}#
        'core
        'set!
        (lambda (#{e\ 1139}#
                 #{r\ 1140}#
                 #{w\ 1141}#
                 #{s\ 1142}#
                 #{mod\ 1143}#)
          ((lambda (#{tmp\ 1144}#)
             ((lambda (#{tmp\ 1145}#)
                (if (if #{tmp\ 1145}#
                      (apply (lambda (#{_\ 1146}# #{id\ 1147}# #{val\ 1148}#)
                               (#{id?\ 129}# #{id\ 1147}#))
                             #{tmp\ 1145}#)
                      #f)
                  (apply (lambda (#{_\ 1149}# #{id\ 1150}# #{val\ 1151}#)
                           (let ((#{val\ 1152}#
                                   (#{chi\ 165}#
                                     #{val\ 1151}#
                                     #{r\ 1140}#
                                     #{w\ 1141}#
                                     #{mod\ 1143}#))
                                 (#{n\ 1153}#
                                   (#{id-var-name\ 151}#
                                     #{id\ 1150}#
                                     #{w\ 1141}#)))
                             (let ((#{b\ 1154}#
                                     (#{lookup\ 126}#
                                       #{n\ 1153}#
                                       #{r\ 1140}#
                                       #{mod\ 1143}#)))
                               (let ((#{atom-key\ 1155}#
                                       (#{binding-type\ 121}# #{b\ 1154}#)))
                                 (if (memv #{atom-key\ 1155}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 99}#
                                     #{s\ 1142}#
                                     (syntax->datum #{id\ 1150}#)
                                     (#{binding-value\ 122}# #{b\ 1154}#)
                                     #{val\ 1152}#)
                                   (if (memv #{atom-key\ 1155}#
                                             '(global))
                                     (#{build-global-assignment\ 102}#
                                       #{s\ 1142}#
                                       #{n\ 1153}#
                                       #{val\ 1152}#
                                       #{mod\ 1143}#)
                                     (if (memv #{atom-key\ 1155}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 157}#
                                           #{id\ 1150}#
                                           #{w\ 1141}#
                                           #{mod\ 1143}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 158}#
                                           #{e\ 1139}#
                                           #{w\ 1141}#
                                           #{s\ 1142}#
                                           #{mod\ 1143}#)))))))))
                         #{tmp\ 1145}#)
                  ((lambda (#{tmp\ 1156}#)
                     (if #{tmp\ 1156}#
                       (apply (lambda (#{_\ 1157}#
                                       #{head\ 1158}#
                                       #{tail\ 1159}#
                                       #{val\ 1160}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 163}#
                                      #{head\ 1158}#
                                      #{r\ 1140}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 1143}#
                                      #t))
                                  (lambda (#{type\ 1161}#
                                           #{value\ 1162}#
                                           #{ee\ 1163}#
                                           #{ww\ 1164}#
                                           #{ss\ 1165}#
                                           #{modmod\ 1166}#)
                                    (if (memv #{type\ 1161}#
                                              '(module-ref))
                                      (let ((#{val\ 1167}#
                                              (#{chi\ 165}#
                                                #{val\ 1160}#
                                                #{r\ 1140}#
                                                #{w\ 1141}#
                                                #{mod\ 1143}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 1162}#
                                              (cons #{head\ 1158}#
                                                    #{tail\ 1159}#)))
                                          (lambda (#{id\ 1169}# #{mod\ 1170}#)
                                            (#{build-global-assignment\ 102}#
                                              #{s\ 1142}#
                                              #{id\ 1169}#
                                              #{val\ 1167}#
                                              #{mod\ 1170}#))))
                                      (#{build-application\ 96}#
                                        #{s\ 1142}#
                                        (#{chi\ 165}#
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
                                                        ellipsis?
                                                        chi-void
                                                        eval-local-transformer
                                                        chi-local-syntax
                                                        chi-lambda-clause
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
                                                        build-lambda
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
                                                       "i"))
                                                    #(ribcage
                                                      (define-structure
                                                        and-map*)
                                                      ((top) (top))
                                                      ("i" "i")))
                                                   (hygiene guile))
                                                #{head\ 1158}#)
                                          #{r\ 1140}#
                                          #{w\ 1141}#
                                          #{mod\ 1143}#)
                                        (map (lambda (#{e\ 1171}#)
                                               (#{chi\ 165}#
                                                 #{e\ 1171}#
                                                 #{r\ 1140}#
                                                 #{w\ 1141}#
                                                 #{mod\ 1143}#))
                                             (append
                                               #{tail\ 1159}#
                                               (list #{val\ 1160}#))))))))
                              #{tmp\ 1156}#)
                       ((lambda (#{_\ 1173}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 158}#
                              #{e\ 1139}#
                              #{w\ 1141}#
                              #{s\ 1142}#
                              #{mod\ 1143}#)))
                        #{tmp\ 1144}#)))
                   ($sc-dispatch
                     #{tmp\ 1144}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 1144}#
                '(any any any))))
           #{e\ 1139}#)))
      (#{global-extend\ 127}#
        'module-ref
        '@
        (lambda (#{e\ 1174}#)
          ((lambda (#{tmp\ 1175}#)
             ((lambda (#{tmp\ 1176}#)
                (if (if #{tmp\ 1176}#
                      (apply (lambda (#{_\ 1177}# #{mod\ 1178}# #{id\ 1179}#)
                               (if (and-map #{id?\ 129}# #{mod\ 1178}#)
                                 (#{id?\ 129}# #{id\ 1179}#)
                                 #f))
                             #{tmp\ 1176}#)
                      #f)
                  (apply (lambda (#{_\ 1181}# #{mod\ 1182}# #{id\ 1183}#)
                           (values
                             (syntax->datum #{id\ 1183}#)
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
                                             ellipsis?
                                             chi-void
                                             eval-local-transformer
                                             chi-local-syntax
                                             chi-lambda-clause
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
                                             build-lambda
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
                                            "i"))
                                         #(ribcage
                                           (define-structure and-map*)
                                           ((top) (top))
                                           ("i" "i")))
                                        (hygiene guile))
                                     #{mod\ 1182}#))))
                         #{tmp\ 1176}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1175}#)))
              ($sc-dispatch
                #{tmp\ 1175}#
                '(any each-any any))))
           #{e\ 1174}#)))
      (#{global-extend\ 127}#
        'module-ref
        '@@
        (lambda (#{e\ 1185}#)
          ((lambda (#{tmp\ 1186}#)
             ((lambda (#{tmp\ 1187}#)
                (if (if #{tmp\ 1187}#
                      (apply (lambda (#{_\ 1188}# #{mod\ 1189}# #{id\ 1190}#)
                               (if (and-map #{id?\ 129}# #{mod\ 1189}#)
                                 (#{id?\ 129}# #{id\ 1190}#)
                                 #f))
                             #{tmp\ 1187}#)
                      #f)
                  (apply (lambda (#{_\ 1192}# #{mod\ 1193}# #{id\ 1194}#)
                           (values
                             (syntax->datum #{id\ 1194}#)
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
                                             ellipsis?
                                             chi-void
                                             eval-local-transformer
                                             chi-local-syntax
                                             chi-lambda-clause
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
                                             build-lambda
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
                                            "i"))
                                         #(ribcage
                                           (define-structure and-map*)
                                           ((top) (top))
                                           ("i" "i")))
                                        (hygiene guile))
                                     #{mod\ 1193}#))))
                         #{tmp\ 1187}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1186}#)))
              ($sc-dispatch
                #{tmp\ 1186}#
                '(any each-any any))))
           #{e\ 1185}#)))
      (#{global-extend\ 127}#
        'core
        'if
        (lambda (#{e\ 1196}#
                 #{r\ 1197}#
                 #{w\ 1198}#
                 #{s\ 1199}#
                 #{mod\ 1200}#)
          ((lambda (#{tmp\ 1201}#)
             ((lambda (#{tmp\ 1202}#)
                (if #{tmp\ 1202}#
                  (apply (lambda (#{_\ 1203}# #{test\ 1204}# #{then\ 1205}#)
                           (#{build-conditional\ 97}#
                             #{s\ 1199}#
                             (#{chi\ 165}#
                               #{test\ 1204}#
                               #{r\ 1197}#
                               #{w\ 1198}#
                               #{mod\ 1200}#)
                             (#{chi\ 165}#
                               #{then\ 1205}#
                               #{r\ 1197}#
                               #{w\ 1198}#
                               #{mod\ 1200}#)
                             (#{build-void\ 95}# #f)))
                         #{tmp\ 1202}#)
                  ((lambda (#{tmp\ 1206}#)
                     (if #{tmp\ 1206}#
                       (apply (lambda (#{_\ 1207}#
                                       #{test\ 1208}#
                                       #{then\ 1209}#
                                       #{else\ 1210}#)
                                (#{build-conditional\ 97}#
                                  #{s\ 1199}#
                                  (#{chi\ 165}#
                                    #{test\ 1208}#
                                    #{r\ 1197}#
                                    #{w\ 1198}#
                                    #{mod\ 1200}#)
                                  (#{chi\ 165}#
                                    #{then\ 1209}#
                                    #{r\ 1197}#
                                    #{w\ 1198}#
                                    #{mod\ 1200}#)
                                  (#{chi\ 165}#
                                    #{else\ 1210}#
                                    #{r\ 1197}#
                                    #{w\ 1198}#
                                    #{mod\ 1200}#)))
                              #{tmp\ 1206}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1201}#)))
                   ($sc-dispatch
                     #{tmp\ 1201}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 1201}#
                '(any any any))))
           #{e\ 1196}#)))
      (#{global-extend\ 127}#
        'begin
        'begin
        '())
      (#{global-extend\ 127}#
        'define
        'define
        '())
      (#{global-extend\ 127}#
        'define-syntax
        'define-syntax
        '())
      (#{global-extend\ 127}#
        'eval-when
        'eval-when
        '())
      (#{global-extend\ 127}#
        'core
        'syntax-case
        (letrec ((#{gen-syntax-case\ 1214}#
                   (lambda (#{x\ 1215}#
                            #{keys\ 1216}#
                            #{clauses\ 1217}#
                            #{r\ 1218}#
                            #{mod\ 1219}#)
                     (if (null? #{clauses\ 1217}#)
                       (#{build-application\ 96}#
                         #f
                         (#{build-primref\ 106}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 107}# #f #f)
                               (#{build-data\ 107}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 1215}#))
                       ((lambda (#{tmp\ 1220}#)
                          ((lambda (#{tmp\ 1221}#)
                             (if #{tmp\ 1221}#
                               (apply (lambda (#{pat\ 1222}# #{exp\ 1223}#)
                                        (if (if (#{id?\ 129}# #{pat\ 1222}#)
                                              (and-map
                                                (lambda (#{x\ 1224}#)
                                                  (not (#{free-id=?\ 152}#
                                                         #{pat\ 1222}#
                                                         #{x\ 1224}#)))
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
                                                              ellipsis?
                                                              chi-void
                                                              eval-local-transformer
                                                              chi-local-syntax
                                                              chi-lambda-clause
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
                                                              build-lambda
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
                                                             "i"))
                                                          #(ribcage
                                                            (define-structure
                                                              and-map*)
                                                            ((top) (top))
                                                            ("i" "i")))
                                                         (hygiene guile))
                                                      #{keys\ 1216}#))
                                              #f)
                                          (let ((#{labels\ 1225}#
                                                  (list (#{gen-label\ 134}#)))
                                                (#{var\ 1226}#
                                                  (#{gen-var\ 176}#
                                                    #{pat\ 1222}#)))
                                            (#{build-application\ 96}#
                                              #f
                                              (#{build-lambda\ 105}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 1222}#))
                                                (list #{var\ 1226}#)
                                                #f
                                                (#{chi\ 165}#
                                                  #{exp\ 1223}#
                                                  (#{extend-env\ 123}#
                                                    #{labels\ 1225}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 1226}#
                                                                      0)))
                                                    #{r\ 1218}#)
                                                  (#{make-binding-wrap\ 146}#
                                                    (list #{pat\ 1222}#)
                                                    #{labels\ 1225}#
                                                    '(()))
                                                  #{mod\ 1219}#))
                                              (list #{x\ 1215}#)))
                                          (#{gen-clause\ 1213}#
                                            #{x\ 1215}#
                                            #{keys\ 1216}#
                                            (cdr #{clauses\ 1217}#)
                                            #{r\ 1218}#
                                            #{pat\ 1222}#
                                            #t
                                            #{exp\ 1223}#
                                            #{mod\ 1219}#)))
                                      #{tmp\ 1221}#)
                               ((lambda (#{tmp\ 1227}#)
                                  (if #{tmp\ 1227}#
                                    (apply (lambda (#{pat\ 1228}#
                                                    #{fender\ 1229}#
                                                    #{exp\ 1230}#)
                                             (#{gen-clause\ 1213}#
                                               #{x\ 1215}#
                                               #{keys\ 1216}#
                                               (cdr #{clauses\ 1217}#)
                                               #{r\ 1218}#
                                               #{pat\ 1228}#
                                               #{fender\ 1229}#
                                               #{exp\ 1230}#
                                               #{mod\ 1219}#))
                                           #{tmp\ 1227}#)
                                    ((lambda (#{_\ 1231}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 1217}#)))
                                     #{tmp\ 1220}#)))
                                ($sc-dispatch
                                  #{tmp\ 1220}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 1220}# (quote (any any)))))
                        (car #{clauses\ 1217}#)))))
                 (#{gen-clause\ 1213}#
                   (lambda (#{x\ 1232}#
                            #{keys\ 1233}#
                            #{clauses\ 1234}#
                            #{r\ 1235}#
                            #{pat\ 1236}#
                            #{fender\ 1237}#
                            #{exp\ 1238}#
                            #{mod\ 1239}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 1211}#
                           #{pat\ 1236}#
                           #{keys\ 1233}#))
                       (lambda (#{p\ 1240}# #{pvars\ 1241}#)
                         (if (not (#{distinct-bound-ids?\ 155}#
                                    (map car #{pvars\ 1241}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 1236}#)
                           (if (not (and-map
                                      (lambda (#{x\ 1242}#)
                                        (not (#{ellipsis?\ 174}#
                                               (car #{x\ 1242}#))))
                                      #{pvars\ 1241}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 1236}#)
                             (let ((#{y\ 1243}#
                                     (#{gen-var\ 176}# (quote tmp))))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-lambda\ 105}#
                                   #f
                                   (list (quote tmp))
                                   (list #{y\ 1243}#)
                                   #f
                                   (let ((#{y\ 1244}#
                                           (#{build-lexical-reference\ 98}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 1243}#)))
                                     (#{build-conditional\ 97}#
                                       #f
                                       ((lambda (#{tmp\ 1245}#)
                                          ((lambda (#{tmp\ 1246}#)
                                             (if #{tmp\ 1246}#
                                               (apply (lambda () #{y\ 1244}#)
                                                      #{tmp\ 1246}#)
                                               ((lambda (#{_\ 1247}#)
                                                  (#{build-conditional\ 97}#
                                                    #f
                                                    #{y\ 1244}#
                                                    (#{build-dispatch-call\ 1212}#
                                                      #{pvars\ 1241}#
                                                      #{fender\ 1237}#
                                                      #{y\ 1244}#
                                                      #{r\ 1235}#
                                                      #{mod\ 1239}#)
                                                    (#{build-data\ 107}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 1245}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1245}#
                                             '#(atom #t))))
                                        #{fender\ 1237}#)
                                       (#{build-dispatch-call\ 1212}#
                                         #{pvars\ 1241}#
                                         #{exp\ 1238}#
                                         #{y\ 1244}#
                                         #{r\ 1235}#
                                         #{mod\ 1239}#)
                                       (#{gen-syntax-case\ 1214}#
                                         #{x\ 1232}#
                                         #{keys\ 1233}#
                                         #{clauses\ 1234}#
                                         #{r\ 1235}#
                                         #{mod\ 1239}#))))
                                 (list (if (eq? #{p\ 1240}# (quote any))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 106}#
                                             #f
                                             'list)
                                           (list #{x\ 1232}#))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 106}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 1232}#
                                                 (#{build-data\ 107}#
                                                   #f
                                                   #{p\ 1240}#)))))))))))))
                 (#{build-dispatch-call\ 1212}#
                   (lambda (#{pvars\ 1248}#
                            #{exp\ 1249}#
                            #{y\ 1250}#
                            #{r\ 1251}#
                            #{mod\ 1252}#)
                     (let ((#{ids\ 1253}# (map car #{pvars\ 1248}#))
                           (#{levels\ 1254}# (map cdr #{pvars\ 1248}#)))
                       (let ((#{labels\ 1255}#
                               (#{gen-labels\ 135}# #{ids\ 1253}#))
                             (#{new-vars\ 1256}#
                               (map #{gen-var\ 176}# #{ids\ 1253}#)))
                         (#{build-application\ 96}#
                           #f
                           (#{build-primref\ 106}# #f (quote apply))
                           (list (#{build-lambda\ 105}#
                                   #f
                                   (map syntax->datum #{ids\ 1253}#)
                                   #{new-vars\ 1256}#
                                   #f
                                   (#{chi\ 165}#
                                     #{exp\ 1249}#
                                     (#{extend-env\ 123}#
                                       #{labels\ 1255}#
                                       (map (lambda (#{var\ 1257}#
                                                     #{level\ 1258}#)
                                              (cons 'syntax
                                                    (cons #{var\ 1257}#
                                                          #{level\ 1258}#)))
                                            #{new-vars\ 1256}#
                                            (map cdr #{pvars\ 1248}#))
                                       #{r\ 1251}#)
                                     (#{make-binding-wrap\ 146}#
                                       #{ids\ 1253}#
                                       #{labels\ 1255}#
                                       '(()))
                                     #{mod\ 1252}#))
                                 #{y\ 1250}#))))))
                 (#{convert-pattern\ 1211}#
                   (lambda (#{pattern\ 1259}# #{keys\ 1260}#)
                     (letrec ((#{cvt\ 1261}#
                                (lambda (#{p\ 1262}# #{n\ 1263}# #{ids\ 1264}#)
                                  (if (#{id?\ 129}# #{p\ 1262}#)
                                    (if (#{bound-id-member?\ 156}#
                                          #{p\ 1262}#
                                          #{keys\ 1260}#)
                                      (values
                                        (vector (quote free-id) #{p\ 1262}#)
                                        #{ids\ 1264}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 1262}# #{n\ 1263}#)
                                              #{ids\ 1264}#)))
                                    ((lambda (#{tmp\ 1265}#)
                                       ((lambda (#{tmp\ 1266}#)
                                          (if (if #{tmp\ 1266}#
                                                (apply (lambda (#{x\ 1267}#
                                                                #{dots\ 1268}#)
                                                         (#{ellipsis?\ 174}#
                                                           #{dots\ 1268}#))
                                                       #{tmp\ 1266}#)
                                                #f)
                                            (apply (lambda (#{x\ 1269}#
                                                            #{dots\ 1270}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 1261}#
                                                           #{x\ 1269}#
                                                           (#{fx+\ 86}#
                                                             #{n\ 1263}#
                                                             1)
                                                           #{ids\ 1264}#))
                                                       (lambda (#{p\ 1271}#
                                                                #{ids\ 1272}#)
                                                         (values
                                                           (if (eq? #{p\ 1271}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 1271}#))
                                                           #{ids\ 1272}#))))
                                                   #{tmp\ 1266}#)
                                            ((lambda (#{tmp\ 1273}#)
                                               (if #{tmp\ 1273}#
                                                 (apply (lambda (#{x\ 1274}#
                                                                 #{y\ 1275}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 1261}#
                                                                #{y\ 1275}#
                                                                #{n\ 1263}#
                                                                #{ids\ 1264}#))
                                                            (lambda (#{y\ 1276}#
                                                                     #{ids\ 1277}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 1261}#
                                                                    #{x\ 1274}#
                                                                    #{n\ 1263}#
                                                                    #{ids\ 1277}#))
                                                                (lambda (#{x\ 1278}#
                                                                         #{ids\ 1279}#)
                                                                  (values
                                                                    (cons #{x\ 1278}#
                                                                          #{y\ 1276}#)
                                                                    #{ids\ 1279}#))))))
                                                        #{tmp\ 1273}#)
                                                 ((lambda (#{tmp\ 1280}#)
                                                    (if #{tmp\ 1280}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 1264}#))
                                                             #{tmp\ 1280}#)
                                                      ((lambda (#{tmp\ 1281}#)
                                                         (if #{tmp\ 1281}#
                                                           (apply (lambda (#{x\ 1282}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 1261}#
                                                                          #{x\ 1282}#
                                                                          #{n\ 1263}#
                                                                          #{ids\ 1264}#))
                                                                      (lambda (#{p\ 1284}#
                                                                               #{ids\ 1285}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 1284}#)
                                                                          #{ids\ 1285}#))))
                                                                  #{tmp\ 1281}#)
                                                           ((lambda (#{x\ 1286}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 175}#
                                                                    #{p\ 1262}#
                                                                    '(())))
                                                                #{ids\ 1264}#))
                                                            #{tmp\ 1265}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1265}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 1265}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 1265}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 1265}#
                                          '(any any))))
                                     #{p\ 1262}#)))))
                       (#{cvt\ 1261}# #{pattern\ 1259}# 0 (quote ()))))))
          (lambda (#{e\ 1287}#
                   #{r\ 1288}#
                   #{w\ 1289}#
                   #{s\ 1290}#
                   #{mod\ 1291}#)
            (let ((#{e\ 1292}#
                    (#{source-wrap\ 158}#
                      #{e\ 1287}#
                      #{w\ 1289}#
                      #{s\ 1290}#
                      #{mod\ 1291}#)))
              ((lambda (#{tmp\ 1293}#)
                 ((lambda (#{tmp\ 1294}#)
                    (if #{tmp\ 1294}#
                      (apply (lambda (#{_\ 1295}#
                                      #{val\ 1296}#
                                      #{key\ 1297}#
                                      #{m\ 1298}#)
                               (if (and-map
                                     (lambda (#{x\ 1299}#)
                                       (if (#{id?\ 129}# #{x\ 1299}#)
                                         (not (#{ellipsis?\ 174}# #{x\ 1299}#))
                                         #f))
                                     #{key\ 1297}#)
                                 (let ((#{x\ 1301}#
                                         (#{gen-var\ 176}# (quote tmp))))
                                   (#{build-application\ 96}#
                                     #{s\ 1290}#
                                     (#{build-lambda\ 105}#
                                       #f
                                       (list (quote tmp))
                                       (list #{x\ 1301}#)
                                       #f
                                       (#{gen-syntax-case\ 1214}#
                                         (#{build-lexical-reference\ 98}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 1301}#)
                                         #{key\ 1297}#
                                         #{m\ 1298}#
                                         #{r\ 1288}#
                                         #{mod\ 1291}#))
                                     (list (#{chi\ 165}#
                                             #{val\ 1296}#
                                             #{r\ 1288}#
                                             '(())
                                             #{mod\ 1291}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 1292}#)))
                             #{tmp\ 1294}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 1293}#)))
                  ($sc-dispatch
                    #{tmp\ 1293}#
                    '(any any each-any . each-any))))
               #{e\ 1292}#)))))
      (set! sc-expand
        (lambda (#{x\ 1305}# . #{rest\ 1304}#)
          (if (if (pair? #{x\ 1305}#)
                (equal? (car #{x\ 1305}#) #{noexpand\ 84}#)
                #f)
            (cadr #{x\ 1305}#)
            (let ((#{m\ 1306}#
                    (if (null? #{rest\ 1304}#)
                      'e
                      (car #{rest\ 1304}#)))
                  (#{esew\ 1307}#
                    (if (let ((#{t\ 1308}# (null? #{rest\ 1304}#)))
                          (if #{t\ 1308}#
                            #{t\ 1308}#
                            (null? (cdr #{rest\ 1304}#))))
                      '(eval)
                      (cadr #{rest\ 1304}#))))
              (with-fluid*
                #{*mode*\ 85}#
                #{m\ 1306}#
                (lambda ()
                  (#{chi-top\ 164}#
                    #{x\ 1305}#
                    '()
                    '((top))
                    #{m\ 1306}#
                    #{esew\ 1307}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 1309}#)
          (#{nonsymbol-id?\ 128}# #{x\ 1309}#)))
      (set! datum->syntax
        (lambda (#{id\ 1310}# #{datum\ 1311}#)
          (#{make-syntax-object\ 112}#
            #{datum\ 1311}#
            (#{syntax-object-wrap\ 115}# #{id\ 1310}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 1312}#)
          (#{strip\ 175}# #{x\ 1312}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 1313}#)
          (begin
            (let ((#{x\ 1314}# #{ls\ 1313}#))
              (if (not (list? #{x\ 1314}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 1314}#)))
            (map (lambda (#{x\ 1315}#)
                   (#{wrap\ 157}# (gensym) (quote ((top))) #f))
                 #{ls\ 1313}#))))
      (set! free-identifier=?
        (lambda (#{x\ 1316}# #{y\ 1317}#)
          (begin
            (let ((#{x\ 1318}# #{x\ 1316}#))
              (if (not (#{nonsymbol-id?\ 128}# #{x\ 1318}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1318}#)))
            (let ((#{x\ 1319}# #{y\ 1317}#))
              (if (not (#{nonsymbol-id?\ 128}# #{x\ 1319}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1319}#)))
            (#{free-id=?\ 152}# #{x\ 1316}# #{y\ 1317}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 1320}# #{y\ 1321}#)
          (begin
            (let ((#{x\ 1322}# #{x\ 1320}#))
              (if (not (#{nonsymbol-id?\ 128}# #{x\ 1322}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1322}#)))
            (let ((#{x\ 1323}# #{y\ 1321}#))
              (if (not (#{nonsymbol-id?\ 128}# #{x\ 1323}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1323}#)))
            (#{bound-id=?\ 153}# #{x\ 1320}# #{y\ 1321}#))))
      (set! syntax-violation
        (lambda (#{who\ 1327}#
                 #{message\ 1326}#
                 #{form\ 1325}#
                 .
                 #{subform\ 1324}#)
          (begin
            (let ((#{x\ 1328}# #{who\ 1327}#))
              (if (not ((lambda (#{x\ 1329}#)
                          (let ((#{t\ 1330}# (not #{x\ 1329}#)))
                            (if #{t\ 1330}#
                              #{t\ 1330}#
                              (let ((#{t\ 1331}# (string? #{x\ 1329}#)))
                                (if #{t\ 1331}#
                                  #{t\ 1331}#
                                  (symbol? #{x\ 1329}#))))))
                        #{x\ 1328}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1328}#)))
            (let ((#{x\ 1332}# #{message\ 1326}#))
              (if (not (string? #{x\ 1332}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1332}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 1327}# "~a: " "")
                "~a "
                (if (null? #{subform\ 1324}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 1333}#
                      (cons #{message\ 1326}#
                            (map (lambda (#{x\ 1334}#)
                                   (#{strip\ 175}# #{x\ 1334}# (quote (()))))
                                 (append
                                   #{subform\ 1324}#
                                   (list #{form\ 1325}#))))))
                (if #{who\ 1327}#
                  (cons #{who\ 1327}# #{tail\ 1333}#)
                  #{tail\ 1333}#))
              #f))))
      (letrec ((#{match\ 1339}#
                 (lambda (#{e\ 1340}#
                          #{p\ 1341}#
                          #{w\ 1342}#
                          #{r\ 1343}#
                          #{mod\ 1344}#)
                   (if (not #{r\ 1343}#)
                     #f
                     (if (eq? #{p\ 1341}# (quote any))
                       (cons (#{wrap\ 157}#
                               #{e\ 1340}#
                               #{w\ 1342}#
                               #{mod\ 1344}#)
                             #{r\ 1343}#)
                       (if (#{syntax-object?\ 113}# #{e\ 1340}#)
                         (#{match*\ 1338}#
                           (#{syntax-object-expression\ 114}# #{e\ 1340}#)
                           #{p\ 1341}#
                           (#{join-wraps\ 148}#
                             #{w\ 1342}#
                             (#{syntax-object-wrap\ 115}# #{e\ 1340}#))
                           #{r\ 1343}#
                           (#{syntax-object-module\ 116}# #{e\ 1340}#))
                         (#{match*\ 1338}#
                           #{e\ 1340}#
                           #{p\ 1341}#
                           #{w\ 1342}#
                           #{r\ 1343}#
                           #{mod\ 1344}#))))))
               (#{match*\ 1338}#
                 (lambda (#{e\ 1345}#
                          #{p\ 1346}#
                          #{w\ 1347}#
                          #{r\ 1348}#
                          #{mod\ 1349}#)
                   (if (null? #{p\ 1346}#)
                     (if (null? #{e\ 1345}#) #{r\ 1348}# #f)
                     (if (pair? #{p\ 1346}#)
                       (if (pair? #{e\ 1345}#)
                         (#{match\ 1339}#
                           (car #{e\ 1345}#)
                           (car #{p\ 1346}#)
                           #{w\ 1347}#
                           (#{match\ 1339}#
                             (cdr #{e\ 1345}#)
                             (cdr #{p\ 1346}#)
                             #{w\ 1347}#
                             #{r\ 1348}#
                             #{mod\ 1349}#)
                           #{mod\ 1349}#)
                         #f)
                       (if (eq? #{p\ 1346}# (quote each-any))
                         (let ((#{l\ 1350}#
                                 (#{match-each-any\ 1336}#
                                   #{e\ 1345}#
                                   #{w\ 1347}#
                                   #{mod\ 1349}#)))
                           (if #{l\ 1350}#
                             (cons #{l\ 1350}# #{r\ 1348}#)
                             #f))
                         (let ((#{atom-key\ 1351}# (vector-ref #{p\ 1346}# 0)))
                           (if (memv #{atom-key\ 1351}# (quote (each)))
                             (if (null? #{e\ 1345}#)
                               (#{match-empty\ 1337}#
                                 (vector-ref #{p\ 1346}# 1)
                                 #{r\ 1348}#)
                               (let ((#{l\ 1352}#
                                       (#{match-each\ 1335}#
                                         #{e\ 1345}#
                                         (vector-ref #{p\ 1346}# 1)
                                         #{w\ 1347}#
                                         #{mod\ 1349}#)))
                                 (if #{l\ 1352}#
                                   (letrec ((#{collect\ 1353}#
                                              (lambda (#{l\ 1354}#)
                                                (if (null? (car #{l\ 1354}#))
                                                  #{r\ 1348}#
                                                  (cons (map car #{l\ 1354}#)
                                                        (#{collect\ 1353}#
                                                          (map cdr
                                                               #{l\ 1354}#)))))))
                                     (#{collect\ 1353}# #{l\ 1352}#))
                                   #f)))
                             (if (memv #{atom-key\ 1351}# (quote (free-id)))
                               (if (#{id?\ 129}# #{e\ 1345}#)
                                 (if (#{free-id=?\ 152}#
                                       (#{wrap\ 157}#
                                         #{e\ 1345}#
                                         #{w\ 1347}#
                                         #{mod\ 1349}#)
                                       (vector-ref #{p\ 1346}# 1))
                                   #{r\ 1348}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 1351}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 1346}# 1)
                                       (#{strip\ 175}#
                                         #{e\ 1345}#
                                         #{w\ 1347}#))
                                   #{r\ 1348}#
                                   #f)
                                 (if (memv #{atom-key\ 1351}# (quote (vector)))
                                   (if (vector? #{e\ 1345}#)
                                     (#{match\ 1339}#
                                       (vector->list #{e\ 1345}#)
                                       (vector-ref #{p\ 1346}# 1)
                                       #{w\ 1347}#
                                       #{r\ 1348}#
                                       #{mod\ 1349}#)
                                     #f)))))))))))
               (#{match-empty\ 1337}#
                 (lambda (#{p\ 1355}# #{r\ 1356}#)
                   (if (null? #{p\ 1355}#)
                     #{r\ 1356}#
                     (if (eq? #{p\ 1355}# (quote any))
                       (cons (quote ()) #{r\ 1356}#)
                       (if (pair? #{p\ 1355}#)
                         (#{match-empty\ 1337}#
                           (car #{p\ 1355}#)
                           (#{match-empty\ 1337}#
                             (cdr #{p\ 1355}#)
                             #{r\ 1356}#))
                         (if (eq? #{p\ 1355}# (quote each-any))
                           (cons (quote ()) #{r\ 1356}#)
                           (let ((#{atom-key\ 1357}#
                                   (vector-ref #{p\ 1355}# 0)))
                             (if (memv #{atom-key\ 1357}# (quote (each)))
                               (#{match-empty\ 1337}#
                                 (vector-ref #{p\ 1355}# 1)
                                 #{r\ 1356}#)
                               (if (memv #{atom-key\ 1357}#
                                         '(free-id atom))
                                 #{r\ 1356}#
                                 (if (memv #{atom-key\ 1357}# (quote (vector)))
                                   (#{match-empty\ 1337}#
                                     (vector-ref #{p\ 1355}# 1)
                                     #{r\ 1356}#)))))))))))
               (#{match-each-any\ 1336}#
                 (lambda (#{e\ 1358}# #{w\ 1359}# #{mod\ 1360}#)
                   (if (pair? #{e\ 1358}#)
                     (let ((#{l\ 1361}#
                             (#{match-each-any\ 1336}#
                               (cdr #{e\ 1358}#)
                               #{w\ 1359}#
                               #{mod\ 1360}#)))
                       (if #{l\ 1361}#
                         (cons (#{wrap\ 157}#
                                 (car #{e\ 1358}#)
                                 #{w\ 1359}#
                                 #{mod\ 1360}#)
                               #{l\ 1361}#)
                         #f))
                     (if (null? #{e\ 1358}#)
                       '()
                       (if (#{syntax-object?\ 113}# #{e\ 1358}#)
                         (#{match-each-any\ 1336}#
                           (#{syntax-object-expression\ 114}# #{e\ 1358}#)
                           (#{join-wraps\ 148}#
                             #{w\ 1359}#
                             (#{syntax-object-wrap\ 115}# #{e\ 1358}#))
                           #{mod\ 1360}#)
                         #f)))))
               (#{match-each\ 1335}#
                 (lambda (#{e\ 1362}#
                          #{p\ 1363}#
                          #{w\ 1364}#
                          #{mod\ 1365}#)
                   (if (pair? #{e\ 1362}#)
                     (let ((#{first\ 1366}#
                             (#{match\ 1339}#
                               (car #{e\ 1362}#)
                               #{p\ 1363}#
                               #{w\ 1364}#
                               '()
                               #{mod\ 1365}#)))
                       (if #{first\ 1366}#
                         (let ((#{rest\ 1367}#
                                 (#{match-each\ 1335}#
                                   (cdr #{e\ 1362}#)
                                   #{p\ 1363}#
                                   #{w\ 1364}#
                                   #{mod\ 1365}#)))
                           (if #{rest\ 1367}#
                             (cons #{first\ 1366}# #{rest\ 1367}#)
                             #f))
                         #f))
                     (if (null? #{e\ 1362}#)
                       '()
                       (if (#{syntax-object?\ 113}# #{e\ 1362}#)
                         (#{match-each\ 1335}#
                           (#{syntax-object-expression\ 114}# #{e\ 1362}#)
                           #{p\ 1363}#
                           (#{join-wraps\ 148}#
                             #{w\ 1364}#
                             (#{syntax-object-wrap\ 115}# #{e\ 1362}#))
                           (#{syntax-object-module\ 116}# #{e\ 1362}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 1368}# #{p\ 1369}#)
            (if (eq? #{p\ 1369}# (quote any))
              (list #{e\ 1368}#)
              (if (#{syntax-object?\ 113}# #{e\ 1368}#)
                (#{match*\ 1338}#
                  (#{syntax-object-expression\ 114}# #{e\ 1368}#)
                  #{p\ 1369}#
                  (#{syntax-object-wrap\ 115}# #{e\ 1368}#)
                  '()
                  (#{syntax-object-module\ 116}# #{e\ 1368}#))
                (#{match*\ 1338}#
                  #{e\ 1368}#
                  #{p\ 1369}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1370}#)
      ((lambda (#{tmp\ 1371}#)
         ((lambda (#{tmp\ 1372}#)
            (if #{tmp\ 1372}#
              (apply (lambda (#{_\ 1373}# #{e1\ 1374}# #{e2\ 1375}#)
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
                             (cons #{e1\ 1374}# #{e2\ 1375}#)))
                     #{tmp\ 1372}#)
              ((lambda (#{tmp\ 1377}#)
                 (if #{tmp\ 1377}#
                   (apply (lambda (#{_\ 1378}#
                                   #{out\ 1379}#
                                   #{in\ 1380}#
                                   #{e1\ 1381}#
                                   #{e2\ 1382}#)
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
                                  #{in\ 1380}#
                                  '()
                                  (list #{out\ 1379}#
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
                                              (cons #{e1\ 1381}#
                                                    #{e2\ 1382}#)))))
                          #{tmp\ 1377}#)
                   ((lambda (#{tmp\ 1384}#)
                      (if #{tmp\ 1384}#
                        (apply (lambda (#{_\ 1385}#
                                        #{out\ 1386}#
                                        #{in\ 1387}#
                                        #{e1\ 1388}#
                                        #{e2\ 1389}#)
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
                                             #{in\ 1387}#)
                                       '()
                                       (list #{out\ 1386}#
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
                                                   (cons #{e1\ 1388}#
                                                         #{e2\ 1389}#)))))
                               #{tmp\ 1384}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 1371}#)))
                    ($sc-dispatch
                      #{tmp\ 1371}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 1371}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 1371}#
            '(any () any . each-any))))
       #{x\ 1370}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1393}#)
      ((lambda (#{tmp\ 1394}#)
         ((lambda (#{tmp\ 1395}#)
            (if #{tmp\ 1395}#
              (apply (lambda (#{_\ 1396}#
                              #{k\ 1397}#
                              #{keyword\ 1398}#
                              #{pattern\ 1399}#
                              #{template\ 1400}#)
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
                                         (cons #{k\ 1397}#
                                               (map (lambda (#{tmp\ 1403}#
                                                             #{tmp\ 1402}#)
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
                                                                  #{tmp\ 1402}#)
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
                                                                  #{tmp\ 1403}#)))
                                                    #{template\ 1400}#
                                                    #{pattern\ 1399}#))))))
                     #{tmp\ 1395}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1394}#)))
          ($sc-dispatch
            #{tmp\ 1394}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 1393}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 1404}#)
      ((lambda (#{tmp\ 1405}#)
         ((lambda (#{tmp\ 1406}#)
            (if (if #{tmp\ 1406}#
                  (apply (lambda (#{let*\ 1407}#
                                  #{x\ 1408}#
                                  #{v\ 1409}#
                                  #{e1\ 1410}#
                                  #{e2\ 1411}#)
                           (and-map identifier? #{x\ 1408}#))
                         #{tmp\ 1406}#)
                  #f)
              (apply (lambda (#{let*\ 1413}#
                              #{x\ 1414}#
                              #{v\ 1415}#
                              #{e1\ 1416}#
                              #{e2\ 1417}#)
                       (letrec ((#{f\ 1418}#
                                  (lambda (#{bindings\ 1419}#)
                                    (if (null? #{bindings\ 1419}#)
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
                                                  (cons #{e1\ 1416}#
                                                        #{e2\ 1417}#)))
                                      ((lambda (#{tmp\ 1423}#)
                                         ((lambda (#{tmp\ 1424}#)
                                            (if #{tmp\ 1424}#
                                              (apply (lambda (#{body\ 1425}#
                                                              #{binding\ 1426}#)
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
                                                             (list #{binding\ 1426}#)
                                                             #{body\ 1425}#))
                                                     #{tmp\ 1424}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 1423}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1423}#
                                            '(any any))))
                                       (list (#{f\ 1418}#
                                               (cdr #{bindings\ 1419}#))
                                             (car #{bindings\ 1419}#)))))))
                         (#{f\ 1418}# (map list #{x\ 1414}# #{v\ 1415}#))))
                     #{tmp\ 1406}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1405}#)))
          ($sc-dispatch
            #{tmp\ 1405}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 1404}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 1427}#)
      ((lambda (#{tmp\ 1428}#)
         ((lambda (#{tmp\ 1429}#)
            (if #{tmp\ 1429}#
              (apply (lambda (#{_\ 1430}#
                              #{var\ 1431}#
                              #{init\ 1432}#
                              #{step\ 1433}#
                              #{e0\ 1434}#
                              #{e1\ 1435}#
                              #{c\ 1436}#)
                       ((lambda (#{tmp\ 1437}#)
                          ((lambda (#{tmp\ 1438}#)
                             (if #{tmp\ 1438}#
                               (apply (lambda (#{step\ 1439}#)
                                        ((lambda (#{tmp\ 1440}#)
                                           ((lambda (#{tmp\ 1441}#)
                                              (if #{tmp\ 1441}#
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
                                                                    #{var\ 1431}#
                                                                    #{init\ 1432}#)
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
                                                                           #{e0\ 1434}#)
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
                                                                             #{c\ 1436}#
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
                                                                                         #{step\ 1439}#)))))))
                                                       #{tmp\ 1441}#)
                                                ((lambda (#{tmp\ 1446}#)
                                                   (if #{tmp\ 1446}#
                                                     (apply (lambda (#{e1\ 1447}#
                                                                     #{e2\ 1448}#)
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
                                                                         #{var\ 1431}#
                                                                         #{init\ 1432}#)
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
                                                                          #{e0\ 1434}#
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
                                                                                (cons #{e1\ 1447}#
                                                                                      #{e2\ 1448}#))
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
                                                                                  #{c\ 1436}#
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
                                                                                              #{step\ 1439}#)))))))
                                                            #{tmp\ 1446}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1440}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1440}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 1440}#
                                              '())))
                                         #{e1\ 1435}#))
                                      #{tmp\ 1438}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 1437}#)))
                           ($sc-dispatch #{tmp\ 1437}# (quote each-any))))
                        (map (lambda (#{v\ 1455}# #{s\ 1456}#)
                               ((lambda (#{tmp\ 1457}#)
                                  ((lambda (#{tmp\ 1458}#)
                                     (if #{tmp\ 1458}#
                                       (apply (lambda () #{v\ 1455}#)
                                              #{tmp\ 1458}#)
                                       ((lambda (#{tmp\ 1459}#)
                                          (if #{tmp\ 1459}#
                                            (apply (lambda (#{e\ 1460}#)
                                                     #{e\ 1460}#)
                                                   #{tmp\ 1459}#)
                                            ((lambda (#{_\ 1461}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 1427}#
                                                 #{s\ 1456}#))
                                             #{tmp\ 1457}#)))
                                        ($sc-dispatch
                                          #{tmp\ 1457}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 1457}# (quote ()))))
                                #{s\ 1456}#))
                             #{var\ 1431}#
                             #{step\ 1433}#)))
                     #{tmp\ 1429}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1428}#)))
          ($sc-dispatch
            #{tmp\ 1428}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 1427}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 1464}#
               (lambda (#{x\ 1468}# #{y\ 1469}#)
                 ((lambda (#{tmp\ 1470}#)
                    ((lambda (#{tmp\ 1471}#)
                       (if #{tmp\ 1471}#
                         (apply (lambda (#{x\ 1472}# #{y\ 1473}#)
                                  ((lambda (#{tmp\ 1474}#)
                                     ((lambda (#{tmp\ 1475}#)
                                        (if #{tmp\ 1475}#
                                          (apply (lambda (#{dy\ 1476}#)
                                                   ((lambda (#{tmp\ 1477}#)
                                                      ((lambda (#{tmp\ 1478}#)
                                                         (if #{tmp\ 1478}#
                                                           (apply (lambda (#{dx\ 1479}#)
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
                                                                          (cons #{dx\ 1479}#
                                                                                #{dy\ 1476}#)))
                                                                  #{tmp\ 1478}#)
                                                           ((lambda (#{_\ 1480}#)
                                                              (if (null? #{dy\ 1476}#)
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
                                                                      #{x\ 1472}#)
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
                                                                      #{x\ 1472}#
                                                                      #{y\ 1473}#)))
                                                            #{tmp\ 1477}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1477}#
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
                                                    #{x\ 1472}#))
                                                 #{tmp\ 1475}#)
                                          ((lambda (#{tmp\ 1481}#)
                                             (if #{tmp\ 1481}#
                                               (apply (lambda (#{stuff\ 1482}#)
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
                                                              (cons #{x\ 1472}#
                                                                    #{stuff\ 1482}#)))
                                                      #{tmp\ 1481}#)
                                               ((lambda (#{else\ 1483}#)
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
                                                        #{x\ 1472}#
                                                        #{y\ 1473}#))
                                                #{tmp\ 1474}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1474}#
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
                                        #{tmp\ 1474}#
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
                                   #{y\ 1473}#))
                                #{tmp\ 1471}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1470}#)))
                     ($sc-dispatch #{tmp\ 1470}# (quote (any any)))))
                  (list #{x\ 1468}# #{y\ 1469}#))))
             (#{quasiappend\ 1465}#
               (lambda (#{x\ 1484}# #{y\ 1485}#)
                 ((lambda (#{tmp\ 1486}#)
                    ((lambda (#{tmp\ 1487}#)
                       (if #{tmp\ 1487}#
                         (apply (lambda (#{x\ 1488}# #{y\ 1489}#)
                                  ((lambda (#{tmp\ 1490}#)
                                     ((lambda (#{tmp\ 1491}#)
                                        (if #{tmp\ 1491}#
                                          (apply (lambda () #{x\ 1488}#)
                                                 #{tmp\ 1491}#)
                                          ((lambda (#{_\ 1492}#)
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
                                                   #{x\ 1488}#
                                                   #{y\ 1489}#))
                                           #{tmp\ 1490}#)))
                                      ($sc-dispatch
                                        #{tmp\ 1490}#
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
                                   #{y\ 1489}#))
                                #{tmp\ 1487}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1486}#)))
                     ($sc-dispatch #{tmp\ 1486}# (quote (any any)))))
                  (list #{x\ 1484}# #{y\ 1485}#))))
             (#{quasivector\ 1466}#
               (lambda (#{x\ 1493}#)
                 ((lambda (#{tmp\ 1494}#)
                    ((lambda (#{x\ 1495}#)
                       ((lambda (#{tmp\ 1496}#)
                          ((lambda (#{tmp\ 1497}#)
                             (if #{tmp\ 1497}#
                               (apply (lambda (#{x\ 1498}#)
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
                                              (list->vector #{x\ 1498}#)))
                                      #{tmp\ 1497}#)
                               ((lambda (#{tmp\ 1500}#)
                                  (if #{tmp\ 1500}#
                                    (apply (lambda (#{x\ 1501}#)
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
                                                   #{x\ 1501}#))
                                           #{tmp\ 1500}#)
                                    ((lambda (#{_\ 1503}#)
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
                                             #{x\ 1495}#))
                                     #{tmp\ 1496}#)))
                                ($sc-dispatch
                                  #{tmp\ 1496}#
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
                             #{tmp\ 1496}#
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
                        #{x\ 1495}#))
                     #{tmp\ 1494}#))
                  #{x\ 1493}#)))
             (#{quasi\ 1467}#
               (lambda (#{p\ 1504}# #{lev\ 1505}#)
                 ((lambda (#{tmp\ 1506}#)
                    ((lambda (#{tmp\ 1507}#)
                       (if #{tmp\ 1507}#
                         (apply (lambda (#{p\ 1508}#)
                                  (if (= #{lev\ 1505}# 0)
                                    #{p\ 1508}#
                                    (#{quasicons\ 1464}#
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
                                      (#{quasi\ 1467}#
                                        (list #{p\ 1508}#)
                                        (- #{lev\ 1505}# 1)))))
                                #{tmp\ 1507}#)
                         ((lambda (#{tmp\ 1509}#)
                            (if (if #{tmp\ 1509}#
                                  (apply (lambda (#{args\ 1510}#)
                                           (= #{lev\ 1505}# 0))
                                         #{tmp\ 1509}#)
                                  #f)
                              (apply (lambda (#{args\ 1511}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 1504}#
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
                                               #{args\ 1511}#)))
                                     #{tmp\ 1509}#)
                              ((lambda (#{tmp\ 1512}#)
                                 (if #{tmp\ 1512}#
                                   (apply (lambda (#{p\ 1513}# #{q\ 1514}#)
                                            (if (= #{lev\ 1505}# 0)
                                              (#{quasiappend\ 1465}#
                                                #{p\ 1513}#
                                                (#{quasi\ 1467}#
                                                  #{q\ 1514}#
                                                  #{lev\ 1505}#))
                                              (#{quasicons\ 1464}#
                                                (#{quasicons\ 1464}#
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
                                                  (#{quasi\ 1467}#
                                                    (list #{p\ 1513}#)
                                                    (- #{lev\ 1505}# 1)))
                                                (#{quasi\ 1467}#
                                                  #{q\ 1514}#
                                                  #{lev\ 1505}#))))
                                          #{tmp\ 1512}#)
                                   ((lambda (#{tmp\ 1515}#)
                                      (if (if #{tmp\ 1515}#
                                            (apply (lambda (#{args\ 1516}#
                                                            #{q\ 1517}#)
                                                     (= #{lev\ 1505}# 0))
                                                   #{tmp\ 1515}#)
                                            #f)
                                        (apply (lambda (#{args\ 1518}#
                                                        #{q\ 1519}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 1504}#
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
                                                         #{args\ 1518}#)))
                                               #{tmp\ 1515}#)
                                        ((lambda (#{tmp\ 1520}#)
                                           (if #{tmp\ 1520}#
                                             (apply (lambda (#{p\ 1521}#)
                                                      (#{quasicons\ 1464}#
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
                                                        (#{quasi\ 1467}#
                                                          (list #{p\ 1521}#)
                                                          (+ #{lev\ 1505}#
                                                             1))))
                                                    #{tmp\ 1520}#)
                                             ((lambda (#{tmp\ 1522}#)
                                                (if #{tmp\ 1522}#
                                                  (apply (lambda (#{p\ 1523}#
                                                                  #{q\ 1524}#)
                                                           (#{quasicons\ 1464}#
                                                             (#{quasi\ 1467}#
                                                               #{p\ 1523}#
                                                               #{lev\ 1505}#)
                                                             (#{quasi\ 1467}#
                                                               #{q\ 1524}#
                                                               #{lev\ 1505}#)))
                                                         #{tmp\ 1522}#)
                                                  ((lambda (#{tmp\ 1525}#)
                                                     (if #{tmp\ 1525}#
                                                       (apply (lambda (#{x\ 1526}#)
                                                                (#{quasivector\ 1466}#
                                                                  (#{quasi\ 1467}#
                                                                    #{x\ 1526}#
                                                                    #{lev\ 1505}#)))
                                                              #{tmp\ 1525}#)
                                                       ((lambda (#{p\ 1528}#)
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
                                                                #{p\ 1528}#))
                                                        #{tmp\ 1506}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 1506}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 1506}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 1506}#
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
                                      #{tmp\ 1506}#
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
                                 #{tmp\ 1506}#
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
                            #{tmp\ 1506}#
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
                       #{tmp\ 1506}#
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
                  #{p\ 1504}#))))
      (lambda (#{x\ 1529}#)
        ((lambda (#{tmp\ 1530}#)
           ((lambda (#{tmp\ 1531}#)
              (if #{tmp\ 1531}#
                (apply (lambda (#{_\ 1532}# #{e\ 1533}#)
                         (#{quasi\ 1467}# #{e\ 1533}# 0))
                       #{tmp\ 1531}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1530}#)))
            ($sc-dispatch #{tmp\ 1530}# (quote (any any)))))
         #{x\ 1529}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1534}#)
      (letrec ((#{read-file\ 1535}#
                 (lambda (#{fn\ 1536}# #{k\ 1537}#)
                   (let ((#{p\ 1538}# (open-input-file #{fn\ 1536}#)))
                     (letrec ((#{f\ 1539}#
                                (lambda (#{x\ 1540}#)
                                  (if (eof-object? #{x\ 1540}#)
                                    (begin
                                      (close-input-port #{p\ 1538}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 1537}#
                                            #{x\ 1540}#)
                                          (#{f\ 1539}# (read #{p\ 1538}#)))))))
                       (#{f\ 1539}# (read #{p\ 1538}#)))))))
        ((lambda (#{tmp\ 1541}#)
           ((lambda (#{tmp\ 1542}#)
              (if #{tmp\ 1542}#
                (apply (lambda (#{k\ 1543}# #{filename\ 1544}#)
                         (let ((#{fn\ 1545}#
                                 (syntax->datum #{filename\ 1544}#)))
                           ((lambda (#{tmp\ 1546}#)
                              ((lambda (#{tmp\ 1547}#)
                                 (if #{tmp\ 1547}#
                                   (apply (lambda (#{exp\ 1548}#)
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
                                                  #{exp\ 1548}#))
                                          #{tmp\ 1547}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 1546}#)))
                               ($sc-dispatch #{tmp\ 1546}# (quote each-any))))
                            (#{read-file\ 1535}# #{fn\ 1545}# #{k\ 1543}#))))
                       #{tmp\ 1542}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1541}#)))
            ($sc-dispatch #{tmp\ 1541}# (quote (any any)))))
         #{x\ 1534}#)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1550}#)
      ((lambda (#{tmp\ 1551}#)
         ((lambda (#{tmp\ 1552}#)
            (if #{tmp\ 1552}#
              (apply (lambda (#{_\ 1553}# #{e\ 1554}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 1550}#))
                     #{tmp\ 1552}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1551}#)))
          ($sc-dispatch #{tmp\ 1551}# (quote (any any)))))
       #{x\ 1550}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1555}#)
      ((lambda (#{tmp\ 1556}#)
         ((lambda (#{tmp\ 1557}#)
            (if #{tmp\ 1557}#
              (apply (lambda (#{_\ 1558}# #{e\ 1559}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 1555}#))
                     #{tmp\ 1557}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1556}#)))
          ($sc-dispatch #{tmp\ 1556}# (quote (any any)))))
       #{x\ 1555}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 1560}#)
      ((lambda (#{tmp\ 1561}#)
         ((lambda (#{tmp\ 1562}#)
            (if #{tmp\ 1562}#
              (apply (lambda (#{_\ 1563}#
                              #{e\ 1564}#
                              #{m1\ 1565}#
                              #{m2\ 1566}#)
                       ((lambda (#{tmp\ 1567}#)
                          ((lambda (#{body\ 1568}#)
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
                                               #{e\ 1564}#))
                                   #{body\ 1568}#))
                           #{tmp\ 1567}#))
                        (letrec ((#{f\ 1569}#
                                   (lambda (#{clause\ 1570}# #{clauses\ 1571}#)
                                     (if (null? #{clauses\ 1571}#)
                                       ((lambda (#{tmp\ 1573}#)
                                          ((lambda (#{tmp\ 1574}#)
                                             (if #{tmp\ 1574}#
                                               (apply (lambda (#{e1\ 1575}#
                                                               #{e2\ 1576}#)
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
                                                              (cons #{e1\ 1575}#
                                                                    #{e2\ 1576}#)))
                                                      #{tmp\ 1574}#)
                                               ((lambda (#{tmp\ 1578}#)
                                                  (if #{tmp\ 1578}#
                                                    (apply (lambda (#{k\ 1579}#
                                                                    #{e1\ 1580}#
                                                                    #{e2\ 1581}#)
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
                                                                               #{k\ 1579}#))
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
                                                                         (cons #{e1\ 1580}#
                                                                               #{e2\ 1581}#))))
                                                           #{tmp\ 1578}#)
                                                    ((lambda (#{_\ 1584}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 1560}#
                                                         #{clause\ 1570}#))
                                                     #{tmp\ 1573}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 1573}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 1573}#
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
                                        #{clause\ 1570}#)
                                       ((lambda (#{tmp\ 1585}#)
                                          ((lambda (#{rest\ 1586}#)
                                             ((lambda (#{tmp\ 1587}#)
                                                ((lambda (#{tmp\ 1588}#)
                                                   (if #{tmp\ 1588}#
                                                     (apply (lambda (#{k\ 1589}#
                                                                     #{e1\ 1590}#
                                                                     #{e2\ 1591}#)
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
                                                                                #{k\ 1589}#))
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
                                                                          (cons #{e1\ 1590}#
                                                                                #{e2\ 1591}#))
                                                                    #{rest\ 1586}#))
                                                            #{tmp\ 1588}#)
                                                     ((lambda (#{_\ 1594}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 1560}#
                                                          #{clause\ 1570}#))
                                                      #{tmp\ 1587}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1587}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 1570}#))
                                           #{tmp\ 1585}#))
                                        (#{f\ 1569}#
                                          (car #{clauses\ 1571}#)
                                          (cdr #{clauses\ 1571}#)))))))
                          (#{f\ 1569}# #{m1\ 1565}# #{m2\ 1566}#))))
                     #{tmp\ 1562}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1561}#)))
          ($sc-dispatch
            #{tmp\ 1561}#
            '(any any any . each-any))))
       #{x\ 1560}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1595}#)
      ((lambda (#{tmp\ 1596}#)
         ((lambda (#{tmp\ 1597}#)
            (if #{tmp\ 1597}#
              (apply (lambda (#{_\ 1598}# #{e\ 1599}#)
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
                                               #{e\ 1599}#))
                                   (list (cons #{_\ 1598}#
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
                                               (cons #{e\ 1599}#
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
                     #{tmp\ 1597}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1596}#)))
          ($sc-dispatch #{tmp\ 1596}# (quote (any any)))))
       #{x\ 1595}#))))

