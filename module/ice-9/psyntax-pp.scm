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
  (letrec ((#{lambda-var-list\ 178}#
             (lambda (#{vars\ 302}#)
               (letrec ((#{lvl\ 303}#
                          (lambda (#{vars\ 304}# #{ls\ 305}# #{w\ 306}#)
                            (if (pair? #{vars\ 304}#)
                              (#{lvl\ 303}#
                                (cdr #{vars\ 304}#)
                                (cons (#{wrap\ 159}#
                                        (car #{vars\ 304}#)
                                        #{w\ 306}#
                                        #f)
                                      #{ls\ 305}#)
                                #{w\ 306}#)
                              (if (#{id?\ 131}# #{vars\ 304}#)
                                (cons (#{wrap\ 159}#
                                        #{vars\ 304}#
                                        #{w\ 306}#
                                        #f)
                                      #{ls\ 305}#)
                                (if (null? #{vars\ 304}#)
                                  #{ls\ 305}#
                                  (if (#{syntax-object?\ 115}# #{vars\ 304}#)
                                    (#{lvl\ 303}#
                                      (#{syntax-object-expression\ 116}#
                                        #{vars\ 304}#)
                                      #{ls\ 305}#
                                      (#{join-wraps\ 150}#
                                        #{w\ 306}#
                                        (#{syntax-object-wrap\ 117}#
                                          #{vars\ 304}#)))
                                    (cons #{vars\ 304}# #{ls\ 305}#))))))))
                 (#{lvl\ 303}#
                   #{vars\ 302}#
                   '()
                   '(())))))
           (#{gen-var\ 177}#
             (lambda (#{id\ 307}#)
               (let ((#{id\ 308}#
                       (if (#{syntax-object?\ 115}# #{id\ 307}#)
                         (#{syntax-object-expression\ 116}# #{id\ 307}#)
                         #{id\ 307}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 308}#) " ")))))
           (#{strip\ 176}#
             (lambda (#{x\ 309}# #{w\ 310}#)
               (if (memq 'top
                         (#{wrap-marks\ 134}# #{w\ 310}#))
                 #{x\ 309}#
                 (letrec ((#{f\ 311}# (lambda (#{x\ 312}#)
                                        (if (#{syntax-object?\ 115}#
                                              #{x\ 312}#)
                                          (#{strip\ 176}#
                                            (#{syntax-object-expression\ 116}#
                                              #{x\ 312}#)
                                            (#{syntax-object-wrap\ 117}#
                                              #{x\ 312}#))
                                          (if (pair? #{x\ 312}#)
                                            (let ((#{a\ 313}# (#{f\ 311}# (car #{x\ 312}#)))
                                                  (#{d\ 314}# (#{f\ 311}# (cdr #{x\ 312}#))))
                                              (if (if (eq? #{a\ 313}#
                                                           (car #{x\ 312}#))
                                                    (eq? #{d\ 314}#
                                                         (cdr #{x\ 312}#))
                                                    #f)
                                                #{x\ 312}#
                                                (cons #{a\ 313}# #{d\ 314}#)))
                                            (if (vector? #{x\ 312}#)
                                              (let ((#{old\ 315}#
                                                      (vector->list
                                                        #{x\ 312}#)))
                                                (let ((#{new\ 316}#
                                                        (map #{f\ 311}#
                                                             #{old\ 315}#)))
                                                  (if (#{and-map*\ 31}#
                                                        eq?
                                                        #{old\ 315}#
                                                        #{new\ 316}#)
                                                    #{x\ 312}#
                                                    (list->vector
                                                      #{new\ 316}#))))
                                              #{x\ 312}#))))))
                   (#{f\ 311}# #{x\ 309}#)))))
           (#{ellipsis?\ 175}#
             (lambda (#{x\ 317}#)
               (if (#{nonsymbol-id?\ 130}# #{x\ 317}#)
                 (#{free-id=?\ 154}#
                   #{x\ 317}#
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
             (lambda (#{expanded\ 318}# #{mod\ 319}#)
               (let ((#{p\ 320}# (#{local-eval-hook\ 91}#
                                   #{expanded\ 318}#
                                   #{mod\ 319}#)))
                 (if (procedure? #{p\ 320}#)
                   #{p\ 320}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 320}#)))))
           (#{chi-local-syntax\ 172}#
             (lambda (#{rec?\ 321}#
                      #{e\ 322}#
                      #{r\ 323}#
                      #{w\ 324}#
                      #{s\ 325}#
                      #{mod\ 326}#
                      #{k\ 327}#)
               ((lambda (#{tmp\ 328}#)
                  ((lambda (#{tmp\ 329}#)
                     (if #{tmp\ 329}#
                       (apply (lambda (#{_\ 330}#
                                       #{id\ 331}#
                                       #{val\ 332}#
                                       #{e1\ 333}#
                                       #{e2\ 334}#)
                                (let ((#{ids\ 335}# #{id\ 331}#))
                                  (if (not (#{valid-bound-ids?\ 156}#
                                             #{ids\ 335}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 322}#)
                                    (let ((#{labels\ 337}#
                                            (#{gen-labels\ 137}#
                                              #{ids\ 335}#)))
                                      (let ((#{new-w\ 338}#
                                              (#{make-binding-wrap\ 148}#
                                                #{ids\ 335}#
                                                #{labels\ 337}#
                                                #{w\ 324}#)))
                                        (#{k\ 327}# (cons #{e1\ 333}#
                                                          #{e2\ 334}#)
                                                    (#{extend-env\ 125}#
                                                      #{labels\ 337}#
                                                      (let ((#{w\ 340}# (if #{rec?\ 321}#
                                                                          #{new-w\ 338}#
                                                                          #{w\ 324}#))
                                                            (#{trans-r\ 341}#
                                                              (#{macros-only-env\ 127}#
                                                                #{r\ 323}#)))
                                                        (map (lambda (#{x\ 342}#)
                                                               (cons 'macro
                                                                     (#{eval-local-transformer\ 173}#
                                                                       (#{chi\ 167}#
                                                                         #{x\ 342}#
                                                                         #{trans-r\ 341}#
                                                                         #{w\ 340}#
                                                                         #{mod\ 326}#)
                                                                       #{mod\ 326}#)))
                                                             #{val\ 332}#))
                                                      #{r\ 323}#)
                                                    #{new-w\ 338}#
                                                    #{s\ 325}#
                                                    #{mod\ 326}#))))))
                              #{tmp\ 329}#)
                       ((lambda (#{_\ 344}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 160}#
                              #{e\ 322}#
                              #{w\ 324}#
                              #{s\ 325}#
                              #{mod\ 326}#)))
                        #{tmp\ 328}#)))
                   ($sc-dispatch
                     #{tmp\ 328}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 322}#)))
           (#{chi-body\ 171}#
             (lambda (#{body\ 345}#
                      #{outer-form\ 346}#
                      #{r\ 347}#
                      #{w\ 348}#
                      #{mod\ 349}#)
               (let ((#{r\ 350}# (cons '("placeholder" placeholder)
                                       #{r\ 347}#)))
                 (let ((#{ribcage\ 351}#
                         (#{make-ribcage\ 138}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 352}# (#{make-wrap\ 133}#
                                       (#{wrap-marks\ 134}# #{w\ 348}#)
                                       (cons #{ribcage\ 351}#
                                             (#{wrap-subst\ 135}#
                                               #{w\ 348}#)))))
                     (letrec ((#{parse\ 353}#
                                (lambda (#{body\ 354}#
                                         #{ids\ 355}#
                                         #{labels\ 356}#
                                         #{var-ids\ 357}#
                                         #{vars\ 358}#
                                         #{vals\ 359}#
                                         #{bindings\ 360}#)
                                  (if (null? #{body\ 354}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 346}#)
                                    (let ((#{e\ 362}# (cdar #{body\ 354}#))
                                          (#{er\ 363}# (caar #{body\ 354}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 165}#
                                            #{e\ 362}#
                                            #{er\ 363}#
                                            '(())
                                            (#{source-annotation\ 122}#
                                              #{er\ 363}#)
                                            #{ribcage\ 351}#
                                            #{mod\ 349}#
                                            #f))
                                        (lambda (#{type\ 364}#
                                                 #{value\ 365}#
                                                 #{e\ 366}#
                                                 #{w\ 367}#
                                                 #{s\ 368}#
                                                 #{mod\ 369}#)
                                          (if (memv #{type\ 364}#
                                                    '(define-form))
                                            (let ((#{id\ 370}#
                                                    (#{wrap\ 159}#
                                                      #{value\ 365}#
                                                      #{w\ 367}#
                                                      #{mod\ 369}#))
                                                  (#{label\ 371}#
                                                    (#{gen-label\ 136}#)))
                                              (let ((#{var\ 372}#
                                                      (#{gen-var\ 177}#
                                                        #{id\ 370}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 351}#
                                                    #{id\ 370}#
                                                    #{label\ 371}#)
                                                  (#{parse\ 353}#
                                                    (cdr #{body\ 354}#)
                                                    (cons #{id\ 370}#
                                                          #{ids\ 355}#)
                                                    (cons #{label\ 371}#
                                                          #{labels\ 356}#)
                                                    (cons #{id\ 370}#
                                                          #{var-ids\ 357}#)
                                                    (cons #{var\ 372}#
                                                          #{vars\ 358}#)
                                                    (cons (cons #{er\ 363}#
                                                                (#{wrap\ 159}#
                                                                  #{e\ 366}#
                                                                  #{w\ 367}#
                                                                  #{mod\ 369}#))
                                                          #{vals\ 359}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 372}#)
                                                          #{bindings\ 360}#)))))
                                            (if (memv #{type\ 364}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 373}#
                                                      (#{wrap\ 159}#
                                                        #{value\ 365}#
                                                        #{w\ 367}#
                                                        #{mod\ 369}#))
                                                    (#{label\ 374}#
                                                      (#{gen-label\ 136}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 147}#
                                                    #{ribcage\ 351}#
                                                    #{id\ 373}#
                                                    #{label\ 374}#)
                                                  (#{parse\ 353}#
                                                    (cdr #{body\ 354}#)
                                                    (cons #{id\ 373}#
                                                          #{ids\ 355}#)
                                                    (cons #{label\ 374}#
                                                          #{labels\ 356}#)
                                                    #{var-ids\ 357}#
                                                    #{vars\ 358}#
                                                    #{vals\ 359}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 363}#
                                                                      (#{wrap\ 159}#
                                                                        #{e\ 366}#
                                                                        #{w\ 367}#
                                                                        #{mod\ 369}#)))
                                                          #{bindings\ 360}#))))
                                              (if (memv #{type\ 364}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 375}#)
                                                   ((lambda (#{tmp\ 376}#)
                                                      (if #{tmp\ 376}#
                                                        (apply (lambda (#{_\ 377}#
                                                                        #{e1\ 378}#)
                                                                 (#{parse\ 353}#
                                                                   (letrec ((#{f\ 379}# (lambda (#{forms\ 380}#)
                                                                                          (if (null? #{forms\ 380}#)
                                                                                            (cdr #{body\ 354}#)
                                                                                            (cons (cons #{er\ 363}#
                                                                                                        (#{wrap\ 159}#
                                                                                                          (car #{forms\ 380}#)
                                                                                                          #{w\ 367}#
                                                                                                          #{mod\ 369}#))
                                                                                                  (#{f\ 379}# (cdr #{forms\ 380}#)))))))
                                                                     (#{f\ 379}# #{e1\ 378}#))
                                                                   #{ids\ 355}#
                                                                   #{labels\ 356}#
                                                                   #{var-ids\ 357}#
                                                                   #{vars\ 358}#
                                                                   #{vals\ 359}#
                                                                   #{bindings\ 360}#))
                                                               #{tmp\ 376}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 375}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 375}#
                                                      '(any . each-any))))
                                                 #{e\ 366}#)
                                                (if (memv #{type\ 364}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 172}#
                                                    #{value\ 365}#
                                                    #{e\ 366}#
                                                    #{er\ 363}#
                                                    #{w\ 367}#
                                                    #{s\ 368}#
                                                    #{mod\ 369}#
                                                    (lambda (#{forms\ 382}#
                                                             #{er\ 383}#
                                                             #{w\ 384}#
                                                             #{s\ 385}#
                                                             #{mod\ 386}#)
                                                      (#{parse\ 353}#
                                                        (letrec ((#{f\ 387}# (lambda (#{forms\ 388}#)
                                                                               (if (null? #{forms\ 388}#)
                                                                                 (cdr #{body\ 354}#)
                                                                                 (cons (cons #{er\ 383}#
                                                                                             (#{wrap\ 159}#
                                                                                               (car #{forms\ 388}#)
                                                                                               #{w\ 384}#
                                                                                               #{mod\ 386}#))
                                                                                       (#{f\ 387}# (cdr #{forms\ 388}#)))))))
                                                          (#{f\ 387}# #{forms\ 382}#))
                                                        #{ids\ 355}#
                                                        #{labels\ 356}#
                                                        #{var-ids\ 357}#
                                                        #{vars\ 358}#
                                                        #{vals\ 359}#
                                                        #{bindings\ 360}#)))
                                                  (if (null? #{ids\ 355}#)
                                                    (#{build-sequence\ 110}#
                                                      #f
                                                      (map (lambda (#{x\ 389}#)
                                                             (#{chi\ 167}#
                                                               (cdr #{x\ 389}#)
                                                               (car #{x\ 389}#)
                                                               '(())
                                                               #{mod\ 369}#))
                                                           (cons (cons #{er\ 363}#
                                                                       (#{source-wrap\ 160}#
                                                                         #{e\ 366}#
                                                                         #{w\ 367}#
                                                                         #{s\ 368}#
                                                                         #{mod\ 369}#))
                                                                 (cdr #{body\ 354}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 156}#
                                                                 #{ids\ 355}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 346}#))
                                                      (letrec ((#{loop\ 390}#
                                                                 (lambda (#{bs\ 391}#
                                                                          #{er-cache\ 392}#
                                                                          #{r-cache\ 393}#)
                                                                   (if (not (null? #{bs\ 391}#))
                                                                     (let ((#{b\ 394}# (car #{bs\ 391}#)))
                                                                       (if (eq? (car #{b\ 394}#)
                                                                                'macro)
                                                                         (let ((#{er\ 395}#
                                                                                 (cadr #{b\ 394}#)))
                                                                           (let ((#{r-cache\ 396}#
                                                                                   (if (eq? #{er\ 395}#
                                                                                            #{er-cache\ 392}#)
                                                                                     #{r-cache\ 393}#
                                                                                     (#{macros-only-env\ 127}#
                                                                                       #{er\ 395}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 394}#
                                                                                 (#{eval-local-transformer\ 173}#
                                                                                   (#{chi\ 167}#
                                                                                     (cddr #{b\ 394}#)
                                                                                     #{r-cache\ 396}#
                                                                                     '(())
                                                                                     #{mod\ 369}#)
                                                                                   #{mod\ 369}#))
                                                                               (#{loop\ 390}#
                                                                                 (cdr #{bs\ 391}#)
                                                                                 #{er\ 395}#
                                                                                 #{r-cache\ 396}#))))
                                                                         (#{loop\ 390}#
                                                                           (cdr #{bs\ 391}#)
                                                                           #{er-cache\ 392}#
                                                                           #{r-cache\ 393}#)))))))
                                                        (#{loop\ 390}#
                                                          #{bindings\ 360}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 350}#
                                                        (#{extend-env\ 125}#
                                                          #{labels\ 356}#
                                                          #{bindings\ 360}#
                                                          (cdr #{r\ 350}#)))
                                                      (#{build-letrec\ 113}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 357}#)
                                                        #{vars\ 358}#
                                                        (map (lambda (#{x\ 397}#)
                                                               (#{chi\ 167}#
                                                                 (cdr #{x\ 397}#)
                                                                 (car #{x\ 397}#)
                                                                 '(())
                                                                 #{mod\ 369}#))
                                                             #{vals\ 359}#)
                                                        (#{build-sequence\ 110}#
                                                          #f
                                                          (map (lambda (#{x\ 398}#)
                                                                 (#{chi\ 167}#
                                                                   (cdr #{x\ 398}#)
                                                                   (car #{x\ 398}#)
                                                                   '(())
                                                                   #{mod\ 369}#))
                                                               (cons (cons #{er\ 363}#
                                                                           (#{source-wrap\ 160}#
                                                                             #{e\ 366}#
                                                                             #{w\ 367}#
                                                                             #{s\ 368}#
                                                                             #{mod\ 369}#))
                                                                     (cdr #{body\ 354}#))))))))))))))))))
                       (#{parse\ 353}#
                         (map (lambda (#{x\ 361}#)
                                (cons #{r\ 350}#
                                      (#{wrap\ 159}#
                                        #{x\ 361}#
                                        #{w\ 352}#
                                        #{mod\ 349}#)))
                              #{body\ 345}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 170}#
             (lambda (#{p\ 399}#
                      #{e\ 400}#
                      #{r\ 401}#
                      #{w\ 402}#
                      #{rib\ 403}#
                      #{mod\ 404}#)
               (letrec ((#{rebuild-macro-output\ 405}#
                          (lambda (#{x\ 406}# #{m\ 407}#)
                            (if (pair? #{x\ 406}#)
                              (cons (#{rebuild-macro-output\ 405}#
                                      (car #{x\ 406}#)
                                      #{m\ 407}#)
                                    (#{rebuild-macro-output\ 405}#
                                      (cdr #{x\ 406}#)
                                      #{m\ 407}#))
                              (if (#{syntax-object?\ 115}# #{x\ 406}#)
                                (let ((#{w\ 408}# (#{syntax-object-wrap\ 117}#
                                                    #{x\ 406}#)))
                                  (let ((#{ms\ 409}#
                                          (#{wrap-marks\ 134}# #{w\ 408}#))
                                        (#{s\ 410}# (#{wrap-subst\ 135}#
                                                      #{w\ 408}#)))
                                    (if (if (pair? #{ms\ 409}#)
                                          (eq? (car #{ms\ 409}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 406}#)
                                        (#{make-wrap\ 133}#
                                          (cdr #{ms\ 409}#)
                                          (if #{rib\ 403}#
                                            (cons #{rib\ 403}#
                                                  (cdr #{s\ 410}#))
                                            (cdr #{s\ 410}#)))
                                        (#{syntax-object-module\ 118}#
                                          #{x\ 406}#))
                                      (#{make-syntax-object\ 114}#
                                        (#{syntax-object-expression\ 116}#
                                          #{x\ 406}#)
                                        (#{make-wrap\ 133}#
                                          (cons #{m\ 407}# #{ms\ 409}#)
                                          (if #{rib\ 403}#
                                            (cons #{rib\ 403}#
                                                  (cons 'shift
                                                        #{s\ 410}#))
                                            (cons (quote shift) #{s\ 410}#)))
                                        (let ((#{pmod\ 411}#
                                                (procedure-module #{p\ 399}#)))
                                          (if #{pmod\ 411}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 411}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 406}#)
                                  (let ((#{n\ 412}# (vector-length
                                                      #{x\ 406}#)))
                                    (let ((#{v\ 413}# (make-vector
                                                        #{n\ 412}#)))
                                      (letrec ((#{loop\ 414}#
                                                 (lambda (#{i\ 415}#)
                                                   (if (#{fx=\ 88}#
                                                         #{i\ 415}#
                                                         #{n\ 412}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 413}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 413}#
                                                         #{i\ 415}#
                                                         (#{rebuild-macro-output\ 405}#
                                                           (vector-ref
                                                             #{x\ 406}#
                                                             #{i\ 415}#)
                                                           #{m\ 407}#))
                                                       (#{loop\ 414}#
                                                         (#{fx+\ 86}#
                                                           #{i\ 415}#
                                                           1)))))))
                                        (#{loop\ 414}# 0))))
                                  (if (symbol? #{x\ 406}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 160}#
                                        #{e\ 400}#
                                        #{w\ 402}#
                                        (#{wrap-subst\ 135}# #{w\ 402}#)
                                        #{mod\ 404}#)
                                      #{x\ 406}#)
                                    #{x\ 406}#)))))))
                 (#{rebuild-macro-output\ 405}#
                   (#{p\ 399}# (#{wrap\ 159}#
                                 #{e\ 400}#
                                 (#{anti-mark\ 146}# #{w\ 402}#)
                                 #{mod\ 404}#))
                   (string #\m)))))
           (#{chi-application\ 169}#
             (lambda (#{x\ 416}#
                      #{e\ 417}#
                      #{r\ 418}#
                      #{w\ 419}#
                      #{s\ 420}#
                      #{mod\ 421}#)
               ((lambda (#{tmp\ 422}#)
                  ((lambda (#{tmp\ 423}#)
                     (if #{tmp\ 423}#
                       (apply (lambda (#{e0\ 424}# #{e1\ 425}#)
                                (#{build-application\ 96}#
                                  #{s\ 420}#
                                  #{x\ 416}#
                                  (map (lambda (#{e\ 426}#)
                                         (#{chi\ 167}#
                                           #{e\ 426}#
                                           #{r\ 418}#
                                           #{w\ 419}#
                                           #{mod\ 421}#))
                                       #{e1\ 425}#)))
                              #{tmp\ 423}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 422}#)))
                   ($sc-dispatch
                     #{tmp\ 422}#
                     '(any . each-any))))
                #{e\ 417}#)))
           (#{chi-expr\ 168}#
             (lambda (#{type\ 428}#
                      #{value\ 429}#
                      #{e\ 430}#
                      #{r\ 431}#
                      #{w\ 432}#
                      #{s\ 433}#
                      #{mod\ 434}#)
               (if (memv #{type\ 428}# (quote (lexical)))
                 (#{build-lexical-reference\ 98}#
                   'value
                   #{s\ 433}#
                   #{e\ 430}#
                   #{value\ 429}#)
                 (if (memv #{type\ 428}# (quote (core core-form)))
                   (#{value\ 429}#
                     #{e\ 430}#
                     #{r\ 431}#
                     #{w\ 432}#
                     #{s\ 433}#
                     #{mod\ 434}#)
                   (if (memv #{type\ 428}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 429}# #{e\ 430}#))
                       (lambda (#{id\ 435}# #{mod\ 436}#)
                         (#{build-global-reference\ 101}#
                           #{s\ 433}#
                           #{id\ 435}#
                           #{mod\ 436}#)))
                     (if (memv #{type\ 428}# (quote (lexical-call)))
                       (#{chi-application\ 169}#
                         (#{build-lexical-reference\ 98}#
                           'fun
                           (#{source-annotation\ 122}# (car #{e\ 430}#))
                           (car #{e\ 430}#)
                           #{value\ 429}#)
                         #{e\ 430}#
                         #{r\ 431}#
                         #{w\ 432}#
                         #{s\ 433}#
                         #{mod\ 434}#)
                       (if (memv #{type\ 428}# (quote (global-call)))
                         (#{chi-application\ 169}#
                           (#{build-global-reference\ 101}#
                             (#{source-annotation\ 122}# (car #{e\ 430}#))
                             (if (#{syntax-object?\ 115}# #{value\ 429}#)
                               (#{syntax-object-expression\ 116}#
                                 #{value\ 429}#)
                               #{value\ 429}#)
                             (if (#{syntax-object?\ 115}# #{value\ 429}#)
                               (#{syntax-object-module\ 118}# #{value\ 429}#)
                               #{mod\ 434}#))
                           #{e\ 430}#
                           #{r\ 431}#
                           #{w\ 432}#
                           #{s\ 433}#
                           #{mod\ 434}#)
                         (if (memv #{type\ 428}# (quote (constant)))
                           (#{build-data\ 109}#
                             #{s\ 433}#
                             (#{strip\ 176}#
                               (#{source-wrap\ 160}#
                                 #{e\ 430}#
                                 #{w\ 432}#
                                 #{s\ 433}#
                                 #{mod\ 434}#)
                               '(())))
                           (if (memv #{type\ 428}# (quote (global)))
                             (#{build-global-reference\ 101}#
                               #{s\ 433}#
                               #{value\ 429}#
                               #{mod\ 434}#)
                             (if (memv #{type\ 428}# (quote (call)))
                               (#{chi-application\ 169}#
                                 (#{chi\ 167}#
                                   (car #{e\ 430}#)
                                   #{r\ 431}#
                                   #{w\ 432}#
                                   #{mod\ 434}#)
                                 #{e\ 430}#
                                 #{r\ 431}#
                                 #{w\ 432}#
                                 #{s\ 433}#
                                 #{mod\ 434}#)
                               (if (memv #{type\ 428}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 437}#)
                                    ((lambda (#{tmp\ 438}#)
                                       (if #{tmp\ 438}#
                                         (apply (lambda (#{_\ 439}#
                                                         #{e1\ 440}#
                                                         #{e2\ 441}#)
                                                  (#{chi-sequence\ 161}#
                                                    (cons #{e1\ 440}#
                                                          #{e2\ 441}#)
                                                    #{r\ 431}#
                                                    #{w\ 432}#
                                                    #{s\ 433}#
                                                    #{mod\ 434}#))
                                                #{tmp\ 438}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 437}#)))
                                     ($sc-dispatch
                                       #{tmp\ 437}#
                                       '(any any . each-any))))
                                  #{e\ 430}#)
                                 (if (memv #{type\ 428}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 172}#
                                     #{value\ 429}#
                                     #{e\ 430}#
                                     #{r\ 431}#
                                     #{w\ 432}#
                                     #{s\ 433}#
                                     #{mod\ 434}#
                                     #{chi-sequence\ 161}#)
                                   (if (memv #{type\ 428}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 443}#)
                                        ((lambda (#{tmp\ 444}#)
                                           (if #{tmp\ 444}#
                                             (apply (lambda (#{_\ 445}#
                                                             #{x\ 446}#
                                                             #{e1\ 447}#
                                                             #{e2\ 448}#)
                                                      (let ((#{when-list\ 449}#
                                                              (#{chi-when-list\ 164}#
                                                                #{e\ 430}#
                                                                #{x\ 446}#
                                                                #{w\ 432}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 449}#)
                                                          (#{chi-sequence\ 161}#
                                                            (cons #{e1\ 447}#
                                                                  #{e2\ 448}#)
                                                            #{r\ 431}#
                                                            #{w\ 432}#
                                                            #{s\ 433}#
                                                            #{mod\ 434}#)
                                                          (#{chi-void\ 174}#))))
                                                    #{tmp\ 444}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 443}#)))
                                         ($sc-dispatch
                                           #{tmp\ 443}#
                                           '(any each-any any . each-any))))
                                      #{e\ 430}#)
                                     (if (memv #{type\ 428}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 430}#
                                         (#{wrap\ 159}#
                                           #{value\ 429}#
                                           #{w\ 432}#
                                           #{mod\ 434}#))
                                       (if (memv #{type\ 428}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 160}#
                                             #{e\ 430}#
                                             #{w\ 432}#
                                             #{s\ 433}#
                                             #{mod\ 434}#))
                                         (if (memv #{type\ 428}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 160}#
                                               #{e\ 430}#
                                               #{w\ 432}#
                                               #{s\ 433}#
                                               #{mod\ 434}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 160}#
                                               #{e\ 430}#
                                               #{w\ 432}#
                                               #{s\ 433}#
                                               #{mod\ 434}#))))))))))))))))))
           (#{chi\ 167}#
             (lambda (#{e\ 452}# #{r\ 453}# #{w\ 454}# #{mod\ 455}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 452}#
                     #{r\ 453}#
                     #{w\ 454}#
                     (#{source-annotation\ 122}# #{e\ 452}#)
                     #f
                     #{mod\ 455}#
                     #f))
                 (lambda (#{type\ 456}#
                          #{value\ 457}#
                          #{e\ 458}#
                          #{w\ 459}#
                          #{s\ 460}#
                          #{mod\ 461}#)
                   (#{chi-expr\ 168}#
                     #{type\ 456}#
                     #{value\ 457}#
                     #{e\ 458}#
                     #{r\ 453}#
                     #{w\ 459}#
                     #{s\ 460}#
                     #{mod\ 461}#)))))
           (#{chi-top\ 166}#
             (lambda (#{e\ 462}#
                      #{r\ 463}#
                      #{w\ 464}#
                      #{m\ 465}#
                      #{esew\ 466}#
                      #{mod\ 467}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 165}#
                     #{e\ 462}#
                     #{r\ 463}#
                     #{w\ 464}#
                     (#{source-annotation\ 122}# #{e\ 462}#)
                     #f
                     #{mod\ 467}#
                     #f))
                 (lambda (#{type\ 475}#
                          #{value\ 476}#
                          #{e\ 477}#
                          #{w\ 478}#
                          #{s\ 479}#
                          #{mod\ 480}#)
                   (if (memv #{type\ 475}# (quote (begin-form)))
                     ((lambda (#{tmp\ 481}#)
                        ((lambda (#{tmp\ 482}#)
                           (if #{tmp\ 482}#
                             (apply (lambda (#{_\ 483}#) (#{chi-void\ 174}#))
                                    #{tmp\ 482}#)
                             ((lambda (#{tmp\ 484}#)
                                (if #{tmp\ 484}#
                                  (apply (lambda (#{_\ 485}#
                                                  #{e1\ 486}#
                                                  #{e2\ 487}#)
                                           (#{chi-top-sequence\ 162}#
                                             (cons #{e1\ 486}# #{e2\ 487}#)
                                             #{r\ 463}#
                                             #{w\ 478}#
                                             #{s\ 479}#
                                             #{m\ 465}#
                                             #{esew\ 466}#
                                             #{mod\ 480}#))
                                         #{tmp\ 484}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 481}#)))
                              ($sc-dispatch
                                #{tmp\ 481}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 481}# (quote (any)))))
                      #{e\ 477}#)
                     (if (memv #{type\ 475}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 172}#
                         #{value\ 476}#
                         #{e\ 477}#
                         #{r\ 463}#
                         #{w\ 478}#
                         #{s\ 479}#
                         #{mod\ 480}#
                         (lambda (#{body\ 489}#
                                  #{r\ 490}#
                                  #{w\ 491}#
                                  #{s\ 492}#
                                  #{mod\ 493}#)
                           (#{chi-top-sequence\ 162}#
                             #{body\ 489}#
                             #{r\ 490}#
                             #{w\ 491}#
                             #{s\ 492}#
                             #{m\ 465}#
                             #{esew\ 466}#
                             #{mod\ 493}#)))
                       (if (memv #{type\ 475}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 494}#)
                            ((lambda (#{tmp\ 495}#)
                               (if #{tmp\ 495}#
                                 (apply (lambda (#{_\ 496}#
                                                 #{x\ 497}#
                                                 #{e1\ 498}#
                                                 #{e2\ 499}#)
                                          (let ((#{when-list\ 500}#
                                                  (#{chi-when-list\ 164}#
                                                    #{e\ 477}#
                                                    #{x\ 497}#
                                                    #{w\ 478}#))
                                                (#{body\ 501}#
                                                  (cons #{e1\ 498}#
                                                        #{e2\ 499}#)))
                                            (if (eq? #{m\ 465}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 500}#)
                                                (#{chi-top-sequence\ 162}#
                                                  #{body\ 501}#
                                                  #{r\ 463}#
                                                  #{w\ 478}#
                                                  #{s\ 479}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 480}#)
                                                (#{chi-void\ 174}#))
                                              (if (memq 'load
                                                        #{when-list\ 500}#)
                                                (if (let ((#{t\ 504}# (memq 'compile
                                                                            #{when-list\ 500}#)))
                                                      (if #{t\ 504}#
                                                        #{t\ 504}#
                                                        (if (eq? #{m\ 465}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 500}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 162}#
                                                    #{body\ 501}#
                                                    #{r\ 463}#
                                                    #{w\ 478}#
                                                    #{s\ 479}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 480}#)
                                                  (if (memq #{m\ 465}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 162}#
                                                      #{body\ 501}#
                                                      #{r\ 463}#
                                                      #{w\ 478}#
                                                      #{s\ 479}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 480}#)
                                                    (#{chi-void\ 174}#)))
                                                (if (let ((#{t\ 505}# (memq 'compile
                                                                            #{when-list\ 500}#)))
                                                      (if #{t\ 505}#
                                                        #{t\ 505}#
                                                        (if (eq? #{m\ 465}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 500}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 90}#
                                                      (#{chi-top-sequence\ 162}#
                                                        #{body\ 501}#
                                                        #{r\ 463}#
                                                        #{w\ 478}#
                                                        #{s\ 479}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 480}#)
                                                      #{mod\ 480}#)
                                                    (#{chi-void\ 174}#))
                                                  (#{chi-void\ 174}#))))))
                                        #{tmp\ 495}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 494}#)))
                             ($sc-dispatch
                               #{tmp\ 494}#
                               '(any each-any any . each-any))))
                          #{e\ 477}#)
                         (if (memv #{type\ 475}# (quote (define-syntax-form)))
                           (let ((#{n\ 506}# (#{id-var-name\ 153}#
                                               #{value\ 476}#
                                               #{w\ 478}#))
                                 (#{r\ 507}# (#{macros-only-env\ 127}#
                                               #{r\ 463}#)))
                             (if (memv #{m\ 465}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 466}#)
                                 (let ((#{e\ 508}# (#{chi-install-global\ 163}#
                                                     #{n\ 506}#
                                                     (#{chi\ 167}#
                                                       #{e\ 477}#
                                                       #{r\ 507}#
                                                       #{w\ 478}#
                                                       #{mod\ 480}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 508}#
                                       #{mod\ 480}#)
                                     (if (memq (quote load) #{esew\ 466}#)
                                       #{e\ 508}#
                                       (#{chi-void\ 174}#))))
                                 (if (memq (quote load) #{esew\ 466}#)
                                   (#{chi-install-global\ 163}#
                                     #{n\ 506}#
                                     (#{chi\ 167}#
                                       #{e\ 477}#
                                       #{r\ 507}#
                                       #{w\ 478}#
                                       #{mod\ 480}#))
                                   (#{chi-void\ 174}#)))
                               (if (memv #{m\ 465}# (quote (c&e)))
                                 (let ((#{e\ 509}# (#{chi-install-global\ 163}#
                                                     #{n\ 506}#
                                                     (#{chi\ 167}#
                                                       #{e\ 477}#
                                                       #{r\ 507}#
                                                       #{w\ 478}#
                                                       #{mod\ 480}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 90}#
                                       #{e\ 509}#
                                       #{mod\ 480}#)
                                     #{e\ 509}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 466}#)
                                     (#{top-level-eval-hook\ 90}#
                                       (#{chi-install-global\ 163}#
                                         #{n\ 506}#
                                         (#{chi\ 167}#
                                           #{e\ 477}#
                                           #{r\ 507}#
                                           #{w\ 478}#
                                           #{mod\ 480}#))
                                       #{mod\ 480}#))
                                   (#{chi-void\ 174}#)))))
                           (if (memv #{type\ 475}# (quote (define-form)))
                             (let ((#{n\ 510}# (#{id-var-name\ 153}#
                                                 #{value\ 476}#
                                                 #{w\ 478}#)))
                               (let ((#{type\ 511}#
                                       (#{binding-type\ 123}#
                                         (#{lookup\ 128}#
                                           #{n\ 510}#
                                           #{r\ 463}#
                                           #{mod\ 480}#))))
                                 (if (memv #{type\ 511}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 510}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 512}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 510}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 510}#
                                           (if (variable? #{old\ 512}#)
                                             (variable-ref #{old\ 512}#)
                                             #f))))
                                     (let ((#{x\ 513}# (#{build-global-definition\ 104}#
                                                         #{s\ 479}#
                                                         #{n\ 510}#
                                                         (#{chi\ 167}#
                                                           #{e\ 477}#
                                                           #{r\ 463}#
                                                           #{w\ 478}#
                                                           #{mod\ 480}#))))
                                       (begin
                                         (if (eq? #{m\ 465}# (quote c&e))
                                           (#{top-level-eval-hook\ 90}#
                                             #{x\ 513}#
                                             #{mod\ 480}#))
                                         #{x\ 513}#)))
                                   (if (memv #{type\ 511}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 477}#
                                       (#{wrap\ 159}#
                                         #{value\ 476}#
                                         #{w\ 478}#
                                         #{mod\ 480}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 477}#
                                       (#{wrap\ 159}#
                                         #{value\ 476}#
                                         #{w\ 478}#
                                         #{mod\ 480}#))))))
                             (let ((#{x\ 514}# (#{chi-expr\ 168}#
                                                 #{type\ 475}#
                                                 #{value\ 476}#
                                                 #{e\ 477}#
                                                 #{r\ 463}#
                                                 #{w\ 478}#
                                                 #{s\ 479}#
                                                 #{mod\ 480}#)))
                               (begin
                                 (if (eq? #{m\ 465}# (quote c&e))
                                   (#{top-level-eval-hook\ 90}#
                                     #{x\ 514}#
                                     #{mod\ 480}#))
                                 #{x\ 514}#)))))))))))
           (#{syntax-type\ 165}#
             (lambda (#{e\ 515}#
                      #{r\ 516}#
                      #{w\ 517}#
                      #{s\ 518}#
                      #{rib\ 519}#
                      #{mod\ 520}#
                      #{for-car?\ 521}#)
               (if (symbol? #{e\ 515}#)
                 (let ((#{n\ 522}# (#{id-var-name\ 153}#
                                     #{e\ 515}#
                                     #{w\ 517}#)))
                   (let ((#{b\ 523}# (#{lookup\ 128}#
                                       #{n\ 522}#
                                       #{r\ 516}#
                                       #{mod\ 520}#)))
                     (let ((#{type\ 524}#
                             (#{binding-type\ 123}# #{b\ 523}#)))
                       (if (memv #{type\ 524}# (quote (lexical)))
                         (values
                           #{type\ 524}#
                           (#{binding-value\ 124}# #{b\ 523}#)
                           #{e\ 515}#
                           #{w\ 517}#
                           #{s\ 518}#
                           #{mod\ 520}#)
                         (if (memv #{type\ 524}# (quote (global)))
                           (values
                             #{type\ 524}#
                             #{n\ 522}#
                             #{e\ 515}#
                             #{w\ 517}#
                             #{s\ 518}#
                             #{mod\ 520}#)
                           (if (memv #{type\ 524}# (quote (macro)))
                             (if #{for-car?\ 521}#
                               (values
                                 #{type\ 524}#
                                 (#{binding-value\ 124}# #{b\ 523}#)
                                 #{e\ 515}#
                                 #{w\ 517}#
                                 #{s\ 518}#
                                 #{mod\ 520}#)
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   (#{binding-value\ 124}# #{b\ 523}#)
                                   #{e\ 515}#
                                   #{r\ 516}#
                                   #{w\ 517}#
                                   #{rib\ 519}#
                                   #{mod\ 520}#)
                                 #{r\ 516}#
                                 '(())
                                 #{s\ 518}#
                                 #{rib\ 519}#
                                 #{mod\ 520}#
                                 #f))
                             (values
                               #{type\ 524}#
                               (#{binding-value\ 124}# #{b\ 523}#)
                               #{e\ 515}#
                               #{w\ 517}#
                               #{s\ 518}#
                               #{mod\ 520}#)))))))
                 (if (pair? #{e\ 515}#)
                   (let ((#{first\ 525}# (car #{e\ 515}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 165}#
                           #{first\ 525}#
                           #{r\ 516}#
                           #{w\ 517}#
                           #{s\ 518}#
                           #{rib\ 519}#
                           #{mod\ 520}#
                           #t))
                       (lambda (#{ftype\ 526}#
                                #{fval\ 527}#
                                #{fe\ 528}#
                                #{fw\ 529}#
                                #{fs\ 530}#
                                #{fmod\ 531}#)
                         (if (memv #{ftype\ 526}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 527}#
                             #{e\ 515}#
                             #{w\ 517}#
                             #{s\ 518}#
                             #{mod\ 520}#)
                           (if (memv #{ftype\ 526}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 114}#
                                 #{fval\ 527}#
                                 #{w\ 517}#
                                 #{fmod\ 531}#)
                               #{e\ 515}#
                               #{w\ 517}#
                               #{s\ 518}#
                               #{mod\ 520}#)
                             (if (memv #{ftype\ 526}# (quote (macro)))
                               (#{syntax-type\ 165}#
                                 (#{chi-macro\ 170}#
                                   #{fval\ 527}#
                                   #{e\ 515}#
                                   #{r\ 516}#
                                   #{w\ 517}#
                                   #{rib\ 519}#
                                   #{mod\ 520}#)
                                 #{r\ 516}#
                                 '(())
                                 #{s\ 518}#
                                 #{rib\ 519}#
                                 #{mod\ 520}#
                                 #{for-car?\ 521}#)
                               (if (memv #{ftype\ 526}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 527}# #{e\ 515}#))
                                   (lambda (#{sym\ 532}# #{mod\ 533}#)
                                     (#{syntax-type\ 165}#
                                       #{sym\ 532}#
                                       #{r\ 516}#
                                       #{w\ 517}#
                                       #{s\ 518}#
                                       #{rib\ 519}#
                                       #{mod\ 533}#
                                       #{for-car?\ 521}#)))
                                 (if (memv #{ftype\ 526}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 527}#
                                     #{e\ 515}#
                                     #{w\ 517}#
                                     #{s\ 518}#
                                     #{mod\ 520}#)
                                   (if (memv #{ftype\ 526}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 527}#
                                       #{e\ 515}#
                                       #{w\ 517}#
                                       #{s\ 518}#
                                       #{mod\ 520}#)
                                     (if (memv #{ftype\ 526}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 515}#
                                         #{w\ 517}#
                                         #{s\ 518}#
                                         #{mod\ 520}#)
                                       (if (memv #{ftype\ 526}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 515}#
                                           #{w\ 517}#
                                           #{s\ 518}#
                                           #{mod\ 520}#)
                                         (if (memv #{ftype\ 526}#
                                                   '(define))
                                           ((lambda (#{tmp\ 534}#)
                                              ((lambda (#{tmp\ 535}#)
                                                 (if (if #{tmp\ 535}#
                                                       (apply (lambda (#{_\ 536}#
                                                                       #{name\ 537}#
                                                                       #{val\ 538}#)
                                                                (#{id?\ 131}#
                                                                  #{name\ 537}#))
                                                              #{tmp\ 535}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 539}#
                                                                   #{name\ 540}#
                                                                   #{val\ 541}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 540}#
                                                              #{val\ 541}#
                                                              #{w\ 517}#
                                                              #{s\ 518}#
                                                              #{mod\ 520}#))
                                                          #{tmp\ 535}#)
                                                   ((lambda (#{tmp\ 542}#)
                                                      (if (if #{tmp\ 542}#
                                                            (apply (lambda (#{_\ 543}#
                                                                            #{name\ 544}#
                                                                            #{args\ 545}#
                                                                            #{e1\ 546}#
                                                                            #{e2\ 547}#)
                                                                     (if (#{id?\ 131}#
                                                                           #{name\ 544}#)
                                                                       (#{valid-bound-ids?\ 156}#
                                                                         (#{lambda-var-list\ 178}#
                                                                           #{args\ 545}#))
                                                                       #f))
                                                                   #{tmp\ 542}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 548}#
                                                                        #{name\ 549}#
                                                                        #{args\ 550}#
                                                                        #{e1\ 551}#
                                                                        #{e2\ 552}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 159}#
                                                                     #{name\ 549}#
                                                                     #{w\ 517}#
                                                                     #{mod\ 520}#)
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
                                                                             (cons #{args\ 550}#
                                                                                   (cons #{e1\ 551}#
                                                                                         #{e2\ 552}#))
                                                                             #{w\ 517}#
                                                                             #{mod\ 520}#))
                                                                     #{s\ 518}#)
                                                                   '(())
                                                                   #{s\ 518}#
                                                                   #{mod\ 520}#))
                                                               #{tmp\ 542}#)
                                                        ((lambda (#{tmp\ 554}#)
                                                           (if (if #{tmp\ 554}#
                                                                 (apply (lambda (#{_\ 555}#
                                                                                 #{name\ 556}#)
                                                                          (#{id?\ 131}#
                                                                            #{name\ 556}#))
                                                                        #{tmp\ 554}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 557}#
                                                                             #{name\ 558}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 159}#
                                                                          #{name\ 558}#
                                                                          #{w\ 517}#
                                                                          #{mod\ 520}#)
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
                                                                        #{s\ 518}#
                                                                        #{mod\ 520}#))
                                                                    #{tmp\ 554}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 534}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 534}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 534}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 534}#
                                                 '(any any any))))
                                            #{e\ 515}#)
                                           (if (memv #{ftype\ 526}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 559}#)
                                                ((lambda (#{tmp\ 560}#)
                                                   (if (if #{tmp\ 560}#
                                                         (apply (lambda (#{_\ 561}#
                                                                         #{name\ 562}#
                                                                         #{val\ 563}#)
                                                                  (#{id?\ 131}#
                                                                    #{name\ 562}#))
                                                                #{tmp\ 560}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 564}#
                                                                     #{name\ 565}#
                                                                     #{val\ 566}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 565}#
                                                                #{val\ 566}#
                                                                #{w\ 517}#
                                                                #{s\ 518}#
                                                                #{mod\ 520}#))
                                                            #{tmp\ 560}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 559}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 559}#
                                                   '(any any any))))
                                              #{e\ 515}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 515}#
                                               #{w\ 517}#
                                               #{s\ 518}#
                                               #{mod\ 520}#))))))))))))))
                   (if (#{syntax-object?\ 115}# #{e\ 515}#)
                     (#{syntax-type\ 165}#
                       (#{syntax-object-expression\ 116}# #{e\ 515}#)
                       #{r\ 516}#
                       (#{join-wraps\ 150}#
                         #{w\ 517}#
                         (#{syntax-object-wrap\ 117}# #{e\ 515}#))
                       #{s\ 518}#
                       #{rib\ 519}#
                       (let ((#{t\ 567}# (#{syntax-object-module\ 118}#
                                           #{e\ 515}#)))
                         (if #{t\ 567}# #{t\ 567}# #{mod\ 520}#))
                       #{for-car?\ 521}#)
                     (if (self-evaluating? #{e\ 515}#)
                       (values
                         'constant
                         #f
                         #{e\ 515}#
                         #{w\ 517}#
                         #{s\ 518}#
                         #{mod\ 520}#)
                       (values
                         'other
                         #f
                         #{e\ 515}#
                         #{w\ 517}#
                         #{s\ 518}#
                         #{mod\ 520}#)))))))
           (#{chi-when-list\ 164}#
             (lambda (#{e\ 568}# #{when-list\ 569}# #{w\ 570}#)
               (letrec ((#{f\ 571}# (lambda (#{when-list\ 572}#
                                             #{situations\ 573}#)
                                      (if (null? #{when-list\ 572}#)
                                        #{situations\ 573}#
                                        (#{f\ 571}# (cdr #{when-list\ 572}#)
                                                    (cons (let ((#{x\ 574}# (car #{when-list\ 572}#)))
                                                            (if (#{free-id=?\ 154}#
                                                                  #{x\ 574}#
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
                                                                    #{x\ 574}#
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
                                                                      #{x\ 574}#
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
                                                                    #{e\ 568}#
                                                                    (#{wrap\ 159}#
                                                                      #{x\ 574}#
                                                                      #{w\ 570}#
                                                                      #f))))))
                                                          #{situations\ 573}#))))))
                 (#{f\ 571}# #{when-list\ 569}# (quote ())))))
           (#{chi-install-global\ 163}#
             (lambda (#{name\ 575}# #{e\ 576}#)
               (#{build-global-definition\ 104}#
                 #f
                 #{name\ 575}#
                 (if (let ((#{v\ 577}# (module-variable
                                         (current-module)
                                         #{name\ 575}#)))
                       (if #{v\ 577}#
                         (if (variable-bound? #{v\ 577}#)
                           (if (macro? (variable-ref #{v\ 577}#))
                             (not (eq? (macro-type (variable-ref #{v\ 577}#))
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
                                   (#{build-data\ 109}# #f #{name\ 575}#)))
                           (#{build-data\ 109}# #f (quote macro))
                           #{e\ 576}#))
                   (#{build-application\ 96}#
                     #f
                     (#{build-primref\ 108}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 109}# #f (quote macro))
                           #{e\ 576}#))))))
           (#{chi-top-sequence\ 162}#
             (lambda (#{body\ 578}#
                      #{r\ 579}#
                      #{w\ 580}#
                      #{s\ 581}#
                      #{m\ 582}#
                      #{esew\ 583}#
                      #{mod\ 584}#)
               (#{build-sequence\ 110}#
                 #{s\ 581}#
                 (letrec ((#{dobody\ 585}#
                            (lambda (#{body\ 586}#
                                     #{r\ 587}#
                                     #{w\ 588}#
                                     #{m\ 589}#
                                     #{esew\ 590}#
                                     #{mod\ 591}#)
                              (if (null? #{body\ 586}#)
                                '()
                                (let ((#{first\ 592}#
                                        (#{chi-top\ 166}#
                                          (car #{body\ 586}#)
                                          #{r\ 587}#
                                          #{w\ 588}#
                                          #{m\ 589}#
                                          #{esew\ 590}#
                                          #{mod\ 591}#)))
                                  (cons #{first\ 592}#
                                        (#{dobody\ 585}#
                                          (cdr #{body\ 586}#)
                                          #{r\ 587}#
                                          #{w\ 588}#
                                          #{m\ 589}#
                                          #{esew\ 590}#
                                          #{mod\ 591}#)))))))
                   (#{dobody\ 585}#
                     #{body\ 578}#
                     #{r\ 579}#
                     #{w\ 580}#
                     #{m\ 582}#
                     #{esew\ 583}#
                     #{mod\ 584}#)))))
           (#{chi-sequence\ 161}#
             (lambda (#{body\ 593}#
                      #{r\ 594}#
                      #{w\ 595}#
                      #{s\ 596}#
                      #{mod\ 597}#)
               (#{build-sequence\ 110}#
                 #{s\ 596}#
                 (letrec ((#{dobody\ 598}#
                            (lambda (#{body\ 599}#
                                     #{r\ 600}#
                                     #{w\ 601}#
                                     #{mod\ 602}#)
                              (if (null? #{body\ 599}#)
                                '()
                                (let ((#{first\ 603}#
                                        (#{chi\ 167}#
                                          (car #{body\ 599}#)
                                          #{r\ 600}#
                                          #{w\ 601}#
                                          #{mod\ 602}#)))
                                  (cons #{first\ 603}#
                                        (#{dobody\ 598}#
                                          (cdr #{body\ 599}#)
                                          #{r\ 600}#
                                          #{w\ 601}#
                                          #{mod\ 602}#)))))))
                   (#{dobody\ 598}#
                     #{body\ 593}#
                     #{r\ 594}#
                     #{w\ 595}#
                     #{mod\ 597}#)))))
           (#{source-wrap\ 160}#
             (lambda (#{x\ 604}#
                      #{w\ 605}#
                      #{s\ 606}#
                      #{defmod\ 607}#)
               (#{wrap\ 159}#
                 (#{decorate-source\ 94}# #{x\ 604}# #{s\ 606}#)
                 #{w\ 605}#
                 #{defmod\ 607}#)))
           (#{wrap\ 159}#
             (lambda (#{x\ 608}# #{w\ 609}# #{defmod\ 610}#)
               (if (if (null? (#{wrap-marks\ 134}# #{w\ 609}#))
                     (null? (#{wrap-subst\ 135}# #{w\ 609}#))
                     #f)
                 #{x\ 608}#
                 (if (#{syntax-object?\ 115}# #{x\ 608}#)
                   (#{make-syntax-object\ 114}#
                     (#{syntax-object-expression\ 116}# #{x\ 608}#)
                     (#{join-wraps\ 150}#
                       #{w\ 609}#
                       (#{syntax-object-wrap\ 117}# #{x\ 608}#))
                     (#{syntax-object-module\ 118}# #{x\ 608}#))
                   (if (null? #{x\ 608}#)
                     #{x\ 608}#
                     (#{make-syntax-object\ 114}#
                       #{x\ 608}#
                       #{w\ 609}#
                       #{defmod\ 610}#))))))
           (#{bound-id-member?\ 158}#
             (lambda (#{x\ 611}# #{list\ 612}#)
               (if (not (null? #{list\ 612}#))
                 (let ((#{t\ 613}# (#{bound-id=?\ 155}#
                                     #{x\ 611}#
                                     (car #{list\ 612}#))))
                   (if #{t\ 613}#
                     #{t\ 613}#
                     (#{bound-id-member?\ 158}#
                       #{x\ 611}#
                       (cdr #{list\ 612}#))))
                 #f)))
           (#{distinct-bound-ids?\ 157}#
             (lambda (#{ids\ 614}#)
               (letrec ((#{distinct?\ 615}#
                          (lambda (#{ids\ 616}#)
                            (let ((#{t\ 617}# (null? #{ids\ 616}#)))
                              (if #{t\ 617}#
                                #{t\ 617}#
                                (if (not (#{bound-id-member?\ 158}#
                                           (car #{ids\ 616}#)
                                           (cdr #{ids\ 616}#)))
                                  (#{distinct?\ 615}# (cdr #{ids\ 616}#))
                                  #f))))))
                 (#{distinct?\ 615}# #{ids\ 614}#))))
           (#{valid-bound-ids?\ 156}#
             (lambda (#{ids\ 618}#)
               (if (letrec ((#{all-ids?\ 619}#
                              (lambda (#{ids\ 620}#)
                                (let ((#{t\ 621}# (null? #{ids\ 620}#)))
                                  (if #{t\ 621}#
                                    #{t\ 621}#
                                    (if (#{id?\ 131}# (car #{ids\ 620}#))
                                      (#{all-ids?\ 619}# (cdr #{ids\ 620}#))
                                      #f))))))
                     (#{all-ids?\ 619}# #{ids\ 618}#))
                 (#{distinct-bound-ids?\ 157}# #{ids\ 618}#)
                 #f)))
           (#{bound-id=?\ 155}#
             (lambda (#{i\ 622}# #{j\ 623}#)
               (if (if (#{syntax-object?\ 115}# #{i\ 622}#)
                     (#{syntax-object?\ 115}# #{j\ 623}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 116}# #{i\ 622}#)
                          (#{syntax-object-expression\ 116}# #{j\ 623}#))
                   (#{same-marks?\ 152}#
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{i\ 622}#))
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{j\ 623}#)))
                   #f)
                 (eq? #{i\ 622}# #{j\ 623}#))))
           (#{free-id=?\ 154}#
             (lambda (#{i\ 624}# #{j\ 625}#)
               (if (eq? (let ((#{x\ 626}# #{i\ 624}#))
                          (if (#{syntax-object?\ 115}# #{x\ 626}#)
                            (#{syntax-object-expression\ 116}# #{x\ 626}#)
                            #{x\ 626}#))
                        (let ((#{x\ 627}# #{j\ 625}#))
                          (if (#{syntax-object?\ 115}# #{x\ 627}#)
                            (#{syntax-object-expression\ 116}# #{x\ 627}#)
                            #{x\ 627}#)))
                 (eq? (#{id-var-name\ 153}# #{i\ 624}# (quote (())))
                      (#{id-var-name\ 153}# #{j\ 625}# (quote (()))))
                 #f)))
           (#{id-var-name\ 153}#
             (lambda (#{id\ 628}# #{w\ 629}#)
               (letrec ((#{search-vector-rib\ 632}#
                          (lambda (#{sym\ 638}#
                                   #{subst\ 639}#
                                   #{marks\ 640}#
                                   #{symnames\ 641}#
                                   #{ribcage\ 642}#)
                            (let ((#{n\ 643}# (vector-length
                                                #{symnames\ 641}#)))
                              (letrec ((#{f\ 644}# (lambda (#{i\ 645}#)
                                                     (if (#{fx=\ 88}#
                                                           #{i\ 645}#
                                                           #{n\ 643}#)
                                                       (#{search\ 630}#
                                                         #{sym\ 638}#
                                                         (cdr #{subst\ 639}#)
                                                         #{marks\ 640}#)
                                                       (if (if (eq? (vector-ref
                                                                      #{symnames\ 641}#
                                                                      #{i\ 645}#)
                                                                    #{sym\ 638}#)
                                                             (#{same-marks?\ 152}#
                                                               #{marks\ 640}#
                                                               (vector-ref
                                                                 (#{ribcage-marks\ 141}#
                                                                   #{ribcage\ 642}#)
                                                                 #{i\ 645}#))
                                                             #f)
                                                         (values
                                                           (vector-ref
                                                             (#{ribcage-labels\ 142}#
                                                               #{ribcage\ 642}#)
                                                             #{i\ 645}#)
                                                           #{marks\ 640}#)
                                                         (#{f\ 644}# (#{fx+\ 86}#
                                                                       #{i\ 645}#
                                                                       1)))))))
                                (#{f\ 644}# 0)))))
                        (#{search-list-rib\ 631}#
                          (lambda (#{sym\ 646}#
                                   #{subst\ 647}#
                                   #{marks\ 648}#
                                   #{symnames\ 649}#
                                   #{ribcage\ 650}#)
                            (letrec ((#{f\ 651}# (lambda (#{symnames\ 652}#
                                                          #{i\ 653}#)
                                                   (if (null? #{symnames\ 652}#)
                                                     (#{search\ 630}#
                                                       #{sym\ 646}#
                                                       (cdr #{subst\ 647}#)
                                                       #{marks\ 648}#)
                                                     (if (if (eq? (car #{symnames\ 652}#)
                                                                  #{sym\ 646}#)
                                                           (#{same-marks?\ 152}#
                                                             #{marks\ 648}#
                                                             (list-ref
                                                               (#{ribcage-marks\ 141}#
                                                                 #{ribcage\ 650}#)
                                                               #{i\ 653}#))
                                                           #f)
                                                       (values
                                                         (list-ref
                                                           (#{ribcage-labels\ 142}#
                                                             #{ribcage\ 650}#)
                                                           #{i\ 653}#)
                                                         #{marks\ 648}#)
                                                       (#{f\ 651}# (cdr #{symnames\ 652}#)
                                                                   (#{fx+\ 86}#
                                                                     #{i\ 653}#
                                                                     1)))))))
                              (#{f\ 651}# #{symnames\ 649}# 0))))
                        (#{search\ 630}#
                          (lambda (#{sym\ 654}# #{subst\ 655}# #{marks\ 656}#)
                            (if (null? #{subst\ 655}#)
                              (values #f #{marks\ 656}#)
                              (let ((#{fst\ 657}# (car #{subst\ 655}#)))
                                (if (eq? #{fst\ 657}# (quote shift))
                                  (#{search\ 630}#
                                    #{sym\ 654}#
                                    (cdr #{subst\ 655}#)
                                    (cdr #{marks\ 656}#))
                                  (let ((#{symnames\ 658}#
                                          (#{ribcage-symnames\ 140}#
                                            #{fst\ 657}#)))
                                    (if (vector? #{symnames\ 658}#)
                                      (#{search-vector-rib\ 632}#
                                        #{sym\ 654}#
                                        #{subst\ 655}#
                                        #{marks\ 656}#
                                        #{symnames\ 658}#
                                        #{fst\ 657}#)
                                      (#{search-list-rib\ 631}#
                                        #{sym\ 654}#
                                        #{subst\ 655}#
                                        #{marks\ 656}#
                                        #{symnames\ 658}#
                                        #{fst\ 657}#)))))))))
                 (if (symbol? #{id\ 628}#)
                   (let ((#{t\ 659}# (call-with-values
                                       (lambda ()
                                         (#{search\ 630}#
                                           #{id\ 628}#
                                           (#{wrap-subst\ 135}# #{w\ 629}#)
                                           (#{wrap-marks\ 134}# #{w\ 629}#)))
                                       (lambda (#{x\ 660}# . #{ignore\ 661}#)
                                         #{x\ 660}#))))
                     (if #{t\ 659}# #{t\ 659}# #{id\ 628}#))
                   (if (#{syntax-object?\ 115}# #{id\ 628}#)
                     (let ((#{id\ 662}#
                             (#{syntax-object-expression\ 116}# #{id\ 628}#))
                           (#{w1\ 663}#
                             (#{syntax-object-wrap\ 117}# #{id\ 628}#)))
                       (let ((#{marks\ 664}#
                               (#{join-marks\ 151}#
                                 (#{wrap-marks\ 134}# #{w\ 629}#)
                                 (#{wrap-marks\ 134}# #{w1\ 663}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 630}#
                               #{id\ 662}#
                               (#{wrap-subst\ 135}# #{w\ 629}#)
                               #{marks\ 664}#))
                           (lambda (#{new-id\ 665}# #{marks\ 666}#)
                             (let ((#{t\ 667}# #{new-id\ 665}#))
                               (if #{t\ 667}#
                                 #{t\ 667}#
                                 (let ((#{t\ 668}# (call-with-values
                                                     (lambda ()
                                                       (#{search\ 630}#
                                                         #{id\ 662}#
                                                         (#{wrap-subst\ 135}#
                                                           #{w1\ 663}#)
                                                         #{marks\ 666}#))
                                                     (lambda (#{x\ 669}#
                                                              .
                                                              #{ignore\ 670}#)
                                                       #{x\ 669}#))))
                                   (if #{t\ 668}#
                                     #{t\ 668}#
                                     #{id\ 662}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 628}#))))))
           (#{same-marks?\ 152}#
             (lambda (#{x\ 671}# #{y\ 672}#)
               (let ((#{t\ 673}# (eq? #{x\ 671}# #{y\ 672}#)))
                 (if #{t\ 673}#
                   #{t\ 673}#
                   (if (not (null? #{x\ 671}#))
                     (if (not (null? #{y\ 672}#))
                       (if (eq? (car #{x\ 671}#) (car #{y\ 672}#))
                         (#{same-marks?\ 152}#
                           (cdr #{x\ 671}#)
                           (cdr #{y\ 672}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 151}#
             (lambda (#{m1\ 674}# #{m2\ 675}#)
               (#{smart-append\ 149}# #{m1\ 674}# #{m2\ 675}#)))
           (#{join-wraps\ 150}#
             (lambda (#{w1\ 676}# #{w2\ 677}#)
               (let ((#{m1\ 678}# (#{wrap-marks\ 134}# #{w1\ 676}#))
                     (#{s1\ 679}# (#{wrap-subst\ 135}# #{w1\ 676}#)))
                 (if (null? #{m1\ 678}#)
                   (if (null? #{s1\ 679}#)
                     #{w2\ 677}#
                     (#{make-wrap\ 133}#
                       (#{wrap-marks\ 134}# #{w2\ 677}#)
                       (#{smart-append\ 149}#
                         #{s1\ 679}#
                         (#{wrap-subst\ 135}# #{w2\ 677}#))))
                   (#{make-wrap\ 133}#
                     (#{smart-append\ 149}#
                       #{m1\ 678}#
                       (#{wrap-marks\ 134}# #{w2\ 677}#))
                     (#{smart-append\ 149}#
                       #{s1\ 679}#
                       (#{wrap-subst\ 135}# #{w2\ 677}#)))))))
           (#{smart-append\ 149}#
             (lambda (#{m1\ 680}# #{m2\ 681}#)
               (if (null? #{m2\ 681}#)
                 #{m1\ 680}#
                 (append #{m1\ 680}# #{m2\ 681}#))))
           (#{make-binding-wrap\ 148}#
             (lambda (#{ids\ 682}# #{labels\ 683}# #{w\ 684}#)
               (if (null? #{ids\ 682}#)
                 #{w\ 684}#
                 (#{make-wrap\ 133}#
                   (#{wrap-marks\ 134}# #{w\ 684}#)
                   (cons (let ((#{labelvec\ 685}#
                                 (list->vector #{labels\ 683}#)))
                           (let ((#{n\ 686}# (vector-length
                                               #{labelvec\ 685}#)))
                             (let ((#{symnamevec\ 687}#
                                     (make-vector #{n\ 686}#))
                                   (#{marksvec\ 688}#
                                     (make-vector #{n\ 686}#)))
                               (begin
                                 (letrec ((#{f\ 689}# (lambda (#{ids\ 690}#
                                                               #{i\ 691}#)
                                                        (if (not (null? #{ids\ 690}#))
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{id-sym-name&marks\ 132}#
                                                                (car #{ids\ 690}#)
                                                                #{w\ 684}#))
                                                            (lambda (#{symname\ 692}#
                                                                     #{marks\ 693}#)
                                                              (begin
                                                                (vector-set!
                                                                  #{symnamevec\ 687}#
                                                                  #{i\ 691}#
                                                                  #{symname\ 692}#)
                                                                (vector-set!
                                                                  #{marksvec\ 688}#
                                                                  #{i\ 691}#
                                                                  #{marks\ 693}#)
                                                                (#{f\ 689}# (cdr #{ids\ 690}#)
                                                                            (#{fx+\ 86}#
                                                                              #{i\ 691}#
                                                                              1)))))))))
                                   (#{f\ 689}# #{ids\ 682}# 0))
                                 (#{make-ribcage\ 138}#
                                   #{symnamevec\ 687}#
                                   #{marksvec\ 688}#
                                   #{labelvec\ 685}#)))))
                         (#{wrap-subst\ 135}# #{w\ 684}#))))))
           (#{extend-ribcage!\ 147}#
             (lambda (#{ribcage\ 694}# #{id\ 695}# #{label\ 696}#)
               (begin
                 (#{set-ribcage-symnames!\ 143}#
                   #{ribcage\ 694}#
                   (cons (#{syntax-object-expression\ 116}# #{id\ 695}#)
                         (#{ribcage-symnames\ 140}# #{ribcage\ 694}#)))
                 (#{set-ribcage-marks!\ 144}#
                   #{ribcage\ 694}#
                   (cons (#{wrap-marks\ 134}#
                           (#{syntax-object-wrap\ 117}# #{id\ 695}#))
                         (#{ribcage-marks\ 141}# #{ribcage\ 694}#)))
                 (#{set-ribcage-labels!\ 145}#
                   #{ribcage\ 694}#
                   (cons #{label\ 696}#
                         (#{ribcage-labels\ 142}# #{ribcage\ 694}#))))))
           (#{anti-mark\ 146}#
             (lambda (#{w\ 697}#)
               (#{make-wrap\ 133}#
                 (cons #f (#{wrap-marks\ 134}# #{w\ 697}#))
                 (cons 'shift
                       (#{wrap-subst\ 135}# #{w\ 697}#)))))
           (#{set-ribcage-labels!\ 145}#
             (lambda (#{x\ 698}# #{update\ 699}#)
               (vector-set! #{x\ 698}# 3 #{update\ 699}#)))
           (#{set-ribcage-marks!\ 144}#
             (lambda (#{x\ 700}# #{update\ 701}#)
               (vector-set! #{x\ 700}# 2 #{update\ 701}#)))
           (#{set-ribcage-symnames!\ 143}#
             (lambda (#{x\ 702}# #{update\ 703}#)
               (vector-set! #{x\ 702}# 1 #{update\ 703}#)))
           (#{ribcage-labels\ 142}#
             (lambda (#{x\ 704}#) (vector-ref #{x\ 704}# 3)))
           (#{ribcage-marks\ 141}#
             (lambda (#{x\ 705}#) (vector-ref #{x\ 705}# 2)))
           (#{ribcage-symnames\ 140}#
             (lambda (#{x\ 706}#) (vector-ref #{x\ 706}# 1)))
           (#{ribcage?\ 139}#
             (lambda (#{x\ 707}#)
               (if (vector? #{x\ 707}#)
                 (if (= (vector-length #{x\ 707}#) 4)
                   (eq? (vector-ref #{x\ 707}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 138}#
             (lambda (#{symnames\ 708}#
                      #{marks\ 709}#
                      #{labels\ 710}#)
               (vector
                 'ribcage
                 #{symnames\ 708}#
                 #{marks\ 709}#
                 #{labels\ 710}#)))
           (#{gen-labels\ 137}#
             (lambda (#{ls\ 711}#)
               (if (null? #{ls\ 711}#)
                 '()
                 (cons (#{gen-label\ 136}#)
                       (#{gen-labels\ 137}# (cdr #{ls\ 711}#))))))
           (#{gen-label\ 136}# (lambda () (string #\i)))
           (#{wrap-subst\ 135}# cdr)
           (#{wrap-marks\ 134}# car)
           (#{make-wrap\ 133}# cons)
           (#{id-sym-name&marks\ 132}#
             (lambda (#{x\ 712}# #{w\ 713}#)
               (if (#{syntax-object?\ 115}# #{x\ 712}#)
                 (values
                   (#{syntax-object-expression\ 116}# #{x\ 712}#)
                   (#{join-marks\ 151}#
                     (#{wrap-marks\ 134}# #{w\ 713}#)
                     (#{wrap-marks\ 134}#
                       (#{syntax-object-wrap\ 117}# #{x\ 712}#))))
                 (values
                   #{x\ 712}#
                   (#{wrap-marks\ 134}# #{w\ 713}#)))))
           (#{id?\ 131}#
             (lambda (#{x\ 714}#)
               (if (symbol? #{x\ 714}#)
                 #t
                 (if (#{syntax-object?\ 115}# #{x\ 714}#)
                   (symbol?
                     (#{syntax-object-expression\ 116}# #{x\ 714}#))
                   #f))))
           (#{nonsymbol-id?\ 130}#
             (lambda (#{x\ 715}#)
               (if (#{syntax-object?\ 115}# #{x\ 715}#)
                 (symbol?
                   (#{syntax-object-expression\ 116}# #{x\ 715}#))
                 #f)))
           (#{global-extend\ 129}#
             (lambda (#{type\ 716}# #{sym\ 717}# #{val\ 718}#)
               (#{put-global-definition-hook\ 92}#
                 #{sym\ 717}#
                 #{type\ 716}#
                 #{val\ 718}#)))
           (#{lookup\ 128}#
             (lambda (#{x\ 719}# #{r\ 720}# #{mod\ 721}#)
               (let ((#{t\ 722}# (assq #{x\ 719}# #{r\ 720}#)))
                 (if #{t\ 722}#
                   (cdr #{t\ 722}#)
                   (if (symbol? #{x\ 719}#)
                     (let ((#{t\ 723}# (#{get-global-definition-hook\ 93}#
                                         #{x\ 719}#
                                         #{mod\ 721}#)))
                       (if #{t\ 723}# #{t\ 723}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 127}#
             (lambda (#{r\ 724}#)
               (if (null? #{r\ 724}#)
                 '()
                 (let ((#{a\ 725}# (car #{r\ 724}#)))
                   (if (eq? (cadr #{a\ 725}#) (quote macro))
                     (cons #{a\ 725}#
                           (#{macros-only-env\ 127}# (cdr #{r\ 724}#)))
                     (#{macros-only-env\ 127}# (cdr #{r\ 724}#)))))))
           (#{extend-var-env\ 126}#
             (lambda (#{labels\ 726}# #{vars\ 727}# #{r\ 728}#)
               (if (null? #{labels\ 726}#)
                 #{r\ 728}#
                 (#{extend-var-env\ 126}#
                   (cdr #{labels\ 726}#)
                   (cdr #{vars\ 727}#)
                   (cons (cons (car #{labels\ 726}#)
                               (cons (quote lexical) (car #{vars\ 727}#)))
                         #{r\ 728}#)))))
           (#{extend-env\ 125}#
             (lambda (#{labels\ 729}# #{bindings\ 730}# #{r\ 731}#)
               (if (null? #{labels\ 729}#)
                 #{r\ 731}#
                 (#{extend-env\ 125}#
                   (cdr #{labels\ 729}#)
                   (cdr #{bindings\ 730}#)
                   (cons (cons (car #{labels\ 729}#)
                               (car #{bindings\ 730}#))
                         #{r\ 731}#)))))
           (#{binding-value\ 124}# cdr)
           (#{binding-type\ 123}# car)
           (#{source-annotation\ 122}#
             (lambda (#{x\ 732}#)
               (if (#{syntax-object?\ 115}# #{x\ 732}#)
                 (#{source-annotation\ 122}#
                   (#{syntax-object-expression\ 116}# #{x\ 732}#))
                 (if (pair? #{x\ 732}#)
                   (let ((#{props\ 733}# (source-properties #{x\ 732}#)))
                     (if (pair? #{props\ 733}#) #{props\ 733}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 121}#
             (lambda (#{x\ 734}# #{update\ 735}#)
               (vector-set! #{x\ 734}# 3 #{update\ 735}#)))
           (#{set-syntax-object-wrap!\ 120}#
             (lambda (#{x\ 736}# #{update\ 737}#)
               (vector-set! #{x\ 736}# 2 #{update\ 737}#)))
           (#{set-syntax-object-expression!\ 119}#
             (lambda (#{x\ 738}# #{update\ 739}#)
               (vector-set! #{x\ 738}# 1 #{update\ 739}#)))
           (#{syntax-object-module\ 118}#
             (lambda (#{x\ 740}#) (vector-ref #{x\ 740}# 3)))
           (#{syntax-object-wrap\ 117}#
             (lambda (#{x\ 741}#) (vector-ref #{x\ 741}# 2)))
           (#{syntax-object-expression\ 116}#
             (lambda (#{x\ 742}#) (vector-ref #{x\ 742}# 1)))
           (#{syntax-object?\ 115}#
             (lambda (#{x\ 743}#)
               (if (vector? #{x\ 743}#)
                 (if (= (vector-length #{x\ 743}#) 4)
                   (eq? (vector-ref #{x\ 743}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 114}#
             (lambda (#{expression\ 744}#
                      #{wrap\ 745}#
                      #{module\ 746}#)
               (vector
                 'syntax-object
                 #{expression\ 744}#
                 #{wrap\ 745}#
                 #{module\ 746}#)))
           (#{build-letrec\ 113}#
             (lambda (#{src\ 747}#
                      #{ids\ 748}#
                      #{vars\ 749}#
                      #{val-exps\ 750}#
                      #{body-exp\ 751}#)
               (if (null? #{vars\ 749}#)
                 #{body-exp\ 751}#
                 (let ((#{atom-key\ 752}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 752}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 748}#
                         #{val-exps\ 750}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 747}#
                        #{ids\ 748}#
                        #{vars\ 749}#
                        #{val-exps\ 750}#
                        #{body-exp\ 751}#))
                     (#{decorate-source\ 94}#
                       (list 'letrec
                             (map list #{vars\ 749}# #{val-exps\ 750}#)
                             #{body-exp\ 751}#)
                       #{src\ 747}#))))))
           (#{build-named-let\ 112}#
             (lambda (#{src\ 753}#
                      #{ids\ 754}#
                      #{vars\ 755}#
                      #{val-exps\ 756}#
                      #{body-exp\ 757}#)
               (let ((#{f\ 758}# (car #{vars\ 755}#))
                     (#{f-name\ 759}# (car #{ids\ 754}#))
                     (#{vars\ 760}# (cdr #{vars\ 755}#))
                     (#{ids\ 761}# (cdr #{ids\ 754}#)))
                 (let ((#{atom-key\ 762}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 762}# (quote (c)))
                     (let ((#{proc\ 763}#
                             (#{build-simple-lambda\ 105}#
                               #{src\ 753}#
                               #{ids\ 761}#
                               #f
                               #{vars\ 760}#
                               #f
                               #{body-exp\ 757}#)))
                       (begin
                         (#{maybe-name-value!\ 103}#
                           #{f-name\ 759}#
                           #{proc\ 763}#)
                         (for-each
                           #{maybe-name-value!\ 103}#
                           #{ids\ 761}#
                           #{val-exps\ 756}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 753}#
                          (list #{f-name\ 759}#)
                          (list #{f\ 758}#)
                          (list #{proc\ 763}#)
                          (#{build-application\ 96}#
                            #{src\ 753}#
                            (#{build-lexical-reference\ 98}#
                              'fun
                              #{src\ 753}#
                              #{f-name\ 759}#
                              #{f\ 758}#)
                            #{val-exps\ 756}#))))
                     (#{decorate-source\ 94}#
                       (list 'let
                             #{f\ 758}#
                             (map list #{vars\ 760}# #{val-exps\ 756}#)
                             #{body-exp\ 757}#)
                       #{src\ 753}#))))))
           (#{build-let\ 111}#
             (lambda (#{src\ 764}#
                      #{ids\ 765}#
                      #{vars\ 766}#
                      #{val-exps\ 767}#
                      #{body-exp\ 768}#)
               (if (null? #{vars\ 766}#)
                 #{body-exp\ 768}#
                 (let ((#{atom-key\ 769}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 769}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 103}#
                         #{ids\ 765}#
                         #{val-exps\ 767}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 764}#
                        #{ids\ 765}#
                        #{vars\ 766}#
                        #{val-exps\ 767}#
                        #{body-exp\ 768}#))
                     (#{decorate-source\ 94}#
                       (list 'let
                             (map list #{vars\ 766}# #{val-exps\ 767}#)
                             #{body-exp\ 768}#)
                       #{src\ 764}#))))))
           (#{build-sequence\ 110}#
             (lambda (#{src\ 770}# #{exps\ 771}#)
               (if (null? (cdr #{exps\ 771}#))
                 (car #{exps\ 771}#)
                 (let ((#{atom-key\ 772}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 772}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 770}#
                      #{exps\ 771}#)
                     (#{decorate-source\ 94}#
                       (cons (quote begin) #{exps\ 771}#)
                       #{src\ 770}#))))))
           (#{build-data\ 109}#
             (lambda (#{src\ 773}# #{exp\ 774}#)
               (let ((#{atom-key\ 775}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 775}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 773}#
                    #{exp\ 774}#)
                   (#{decorate-source\ 94}#
                     (if (if (self-evaluating? #{exp\ 774}#)
                           (not (vector? #{exp\ 774}#))
                           #f)
                       #{exp\ 774}#
                       (list (quote quote) #{exp\ 774}#))
                     #{src\ 773}#)))))
           (#{build-primref\ 108}#
             (lambda (#{src\ 776}# #{name\ 777}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 778}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 778}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 776}#
                      #{name\ 777}#)
                     (#{decorate-source\ 94}#
                       #{name\ 777}#
                       #{src\ 776}#)))
                 (let ((#{atom-key\ 779}# (fluid-ref #{*mode*\ 85}#)))
                   (if (memv #{atom-key\ 779}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 776}#
                      '(guile)
                      #{name\ 777}#
                      #f)
                     (#{decorate-source\ 94}#
                       (list (quote @@) (quote (guile)) #{name\ 777}#)
                       #{src\ 776}#))))))
           (#{build-lambda-case\ 107}#
             (lambda (#{src\ 780}#
                      #{req\ 781}#
                      #{opt\ 782}#
                      #{rest\ 783}#
                      #{kw\ 784}#
                      #{vars\ 785}#
                      #{predicate\ 786}#
                      #{body\ 787}#
                      #{else-case\ 788}#)
               (let ((#{atom-key\ 789}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 789}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 780}#
                    #{req\ 781}#
                    #{opt\ 782}#
                    #{rest\ 783}#
                    #{kw\ 784}#
                    #{vars\ 785}#
                    #{predicate\ 786}#
                    #{body\ 787}#
                    #{else-case\ 788}#)
                   (let ((#{nreq\ 790}# (length #{req\ 781}#)))
                     (let ((#{nopt\ 791}#
                             (if #{opt\ 782}# (length #{opt\ 782}#) 0)))
                       (let ((#{rest-idx\ 792}#
                               (if #{rest\ 783}#
                                 (+ #{nreq\ 790}# #{nopt\ 791}#)
                                 #f)))
                         (let ((#{opt-inits\ 793}#
                                 (map (lambda (#{x\ 794}#)
                                        (list 'lambda
                                              #{vars\ 785}#
                                              (cdr #{x\ 794}#)))
                                      (let ((#{t\ 795}# #{opt\ 782}#))
                                        (if #{t\ 795}#
                                          #{t\ 795}#
                                          '())))))
                           (let ((#{allow-other-keys?\ 796}#
                                   (if #{kw\ 784}# (car #{kw\ 784}#) #f)))
                             (let ((#{kw-indices\ 797}#
                                     (map (lambda (#{x\ 798}#)
                                            (cons (car #{x\ 798}#)
                                                  (list-index
                                                    #{vars\ 785}#
                                                    (caddr #{x\ 798}#))))
                                          (if #{kw\ 784}#
                                            (cdr #{kw\ 784}#)
                                            '()))))
                               (let ((#{kw-inits\ 799}#
                                       (sort (filter
                                               identity
                                               (map (lambda (#{x\ 800}#)
                                                      (if (pair? (cdddr #{x\ 800}#))
                                                        (let ((#{i\ 801}# (list-index
                                                                            #{vars\ 785}#
                                                                            (caddr #{x\ 800}#))))
                                                          (if (> (+ #{nreq\ 790}#
                                                                    #{nopt\ 791}#)
                                                                 #{i\ 801}#)
                                                            (error "kw init for rest arg"
                                                                   #{x\ 800}#)
                                                            (if (if #{rest\ 783}#
                                                                  (= (+ #{nreq\ 790}#
                                                                        #{nopt\ 791}#)
                                                                     #{i\ 801}#)
                                                                  #f)
                                                              (error "kw init for positional arg"
                                                                     #{x\ 800}#)
                                                              (list 'lambda
                                                                    #{vars\ 785}#
                                                                    (cadddr
                                                                      #{x\ 800}#)))))
                                                        (let ((#{i\ 802}# (list-index
                                                                            #{vars\ 785}#
                                                                            (caddr #{x\ 800}#))))
                                                          (if (< (+ #{nreq\ 790}#
                                                                    #{nopt\ 791}#)
                                                                 #{i\ 802}#)
                                                            #f
                                                            (error "missing init for kw arg"
                                                                   #{x\ 800}#)))))
                                                    (if #{kw\ 784}#
                                                      (cdr #{kw\ 784}#)
                                                      '())))
                                             (lambda (#{x\ 803}# #{y\ 804}#)
                                               (< (cdr #{x\ 803}#)
                                                  (cdr #{y\ 804}#))))))
                                 (let ((#{nargs\ 805}#
                                         (apply max
                                                (pk (+ #{nreq\ 790}#
                                                       #{nopt\ 791}#
                                                       (if #{rest\ 783}# 1 0)))
                                                (map cdr
                                                     #{kw-indices\ 797}#))))
                                   (begin
                                     (let ((#{t\ 806}# (= #{nargs\ 805}#
                                                          (length
                                                            #{vars\ 785}#)
                                                          (+ #{nreq\ 790}#
                                                             (length
                                                               #{opt-inits\ 793}#)
                                                             (if #{rest\ 783}#
                                                               1
                                                               0)
                                                             (length
                                                               #{kw-inits\ 799}#)))))
                                       (if #{t\ 806}#
                                         #{t\ 806}#
                                         (error "something went wrong"
                                                #{req\ 781}#
                                                #{opt\ 782}#
                                                #{rest\ 783}#
                                                #{kw\ 784}#
                                                #{vars\ 785}#
                                                #{nreq\ 790}#
                                                #{nopt\ 791}#
                                                #{kw-indices\ 797}#
                                                #{kw-inits\ 799}#
                                                #{nargs\ 805}#)))
                                     (#{decorate-source\ 94}#
                                       (cons (list (cons '(@@ (ice-9 optargs)
                                                              parse-lambda-case)
                                                         (cons (list 'quote
                                                                     (list #{nreq\ 790}#
                                                                           #{nopt\ 791}#
                                                                           #{rest-idx\ 792}#
                                                                           #{nargs\ 805}#
                                                                           #{allow-other-keys?\ 796}#
                                                                           #{kw-indices\ 797}#))
                                                               (cons (cons 'list
                                                                           (append
                                                                             #{opt-inits\ 793}#
                                                                             #{kw-inits\ 799}#))
                                                                     (cons (if #{predicate\ 786}#
                                                                             (list 'lambda
                                                                                   #{vars\ 785}#
                                                                                   #{predicate\ 786}#)
                                                                             #f)
                                                                           '(%%args)))))
                                                   '=>
                                                   (list 'lambda
                                                         #{vars\ 785}#
                                                         #{body\ 787}#))
                                             (let ((#{t\ 807}# #{else-case\ 788}#))
                                               (if #{t\ 807}#
                                                 #{t\ 807}#
                                                 '((%%args
                                                     (error "wrong number of arguments"
                                                            %%args))))))
                                       #{src\ 780}#))))))))))))))
           (#{build-case-lambda\ 106}#
             (lambda (#{src\ 808}# #{docstring\ 809}# #{body\ 810}#)
               (let ((#{atom-key\ 811}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 811}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 808}#
                    (if #{docstring\ 809}#
                      (list (cons (quote documentation) #{docstring\ 809}#))
                      '())
                    #{body\ 810}#)
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 809}#
                                     (list #{docstring\ 809}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 810}#)))))
                     #{src\ 808}#)))))
           (#{build-simple-lambda\ 105}#
             (lambda (#{src\ 812}#
                      #{req\ 813}#
                      #{rest\ 814}#
                      #{vars\ 815}#
                      #{docstring\ 816}#
                      #{exp\ 817}#)
               (let ((#{atom-key\ 818}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 818}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 812}#
                    (if #{docstring\ 816}#
                      (list (cons (quote documentation) #{docstring\ 816}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 812}#
                     #{req\ 813}#
                     #f
                     #{rest\ 814}#
                     #f
                     #{vars\ 815}#
                     #f
                     #{exp\ 817}#
                     #f))
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons (if #{rest\ 814}#
                                   (apply cons* #{vars\ 815}#)
                                   #{vars\ 815}#)
                                 (append
                                   (if #{docstring\ 816}#
                                     (list #{docstring\ 816}#)
                                     '())
                                   (list #{exp\ 817}#))))
                     #{src\ 812}#)))))
           (#{build-global-definition\ 104}#
             (lambda (#{source\ 819}# #{var\ 820}# #{exp\ 821}#)
               (let ((#{atom-key\ 822}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 822}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 103}#
                       #{var\ 820}#
                       #{exp\ 821}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 819}#
                      #{var\ 820}#
                      #{exp\ 821}#))
                   (#{decorate-source\ 94}#
                     (list (quote define) #{var\ 820}# #{exp\ 821}#)
                     #{source\ 819}#)))))
           (#{maybe-name-value!\ 103}#
             (lambda (#{name\ 823}# #{val\ 824}#)
               (if ((@ (language tree-il) lambda?) #{val\ 824}#)
                 (let ((#{meta\ 825}#
                         ((@ (language tree-il) lambda-meta) #{val\ 824}#)))
                   (if (not (assq (quote name) #{meta\ 825}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 824}#
                      (acons (quote name) #{name\ 823}# #{meta\ 825}#)))))))
           (#{build-global-assignment\ 102}#
             (lambda (#{source\ 826}#
                      #{var\ 827}#
                      #{exp\ 828}#
                      #{mod\ 829}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 829}#
                 #{var\ 827}#
                 (lambda (#{mod\ 830}# #{var\ 831}# #{public?\ 832}#)
                   (let ((#{atom-key\ 833}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 833}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 826}#
                        #{mod\ 830}#
                        #{var\ 831}#
                        #{public?\ 832}#
                        #{exp\ 828}#)
                       (#{decorate-source\ 94}#
                         (list 'set!
                               (list (if #{public?\ 832}# (quote @) (quote @@))
                                     #{mod\ 830}#
                                     #{var\ 831}#)
                               #{exp\ 828}#)
                         #{source\ 826}#))))
                 (lambda (#{var\ 834}#)
                   (let ((#{atom-key\ 835}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 835}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 826}#
                        #{var\ 834}#
                        #{exp\ 828}#)
                       (#{decorate-source\ 94}#
                         (list (quote set!) #{var\ 834}# #{exp\ 828}#)
                         #{source\ 826}#)))))))
           (#{build-global-reference\ 101}#
             (lambda (#{source\ 836}# #{var\ 837}# #{mod\ 838}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 838}#
                 #{var\ 837}#
                 (lambda (#{mod\ 839}# #{var\ 840}# #{public?\ 841}#)
                   (let ((#{atom-key\ 842}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 842}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 836}#
                        #{mod\ 839}#
                        #{var\ 840}#
                        #{public?\ 841}#)
                       (#{decorate-source\ 94}#
                         (list (if #{public?\ 841}# (quote @) (quote @@))
                               #{mod\ 839}#
                               #{var\ 840}#)
                         #{source\ 836}#))))
                 (lambda (#{var\ 843}#)
                   (let ((#{atom-key\ 844}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 844}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 836}#
                        #{var\ 843}#)
                       (#{decorate-source\ 94}#
                         #{var\ 843}#
                         #{source\ 836}#)))))))
           (#{analyze-variable\ 100}#
             (lambda (#{mod\ 845}#
                      #{var\ 846}#
                      #{modref-cont\ 847}#
                      #{bare-cont\ 848}#)
               (if (not #{mod\ 845}#)
                 (#{bare-cont\ 848}# #{var\ 846}#)
                 (let ((#{kind\ 849}# (car #{mod\ 845}#))
                       (#{mod\ 850}# (cdr #{mod\ 845}#)))
                   (if (memv #{kind\ 849}# (quote (public)))
                     (#{modref-cont\ 847}#
                       #{mod\ 850}#
                       #{var\ 846}#
                       #t)
                     (if (memv #{kind\ 849}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 850}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 847}#
                           #{mod\ 850}#
                           #{var\ 846}#
                           #f)
                         (#{bare-cont\ 848}# #{var\ 846}#))
                       (if (memv #{kind\ 849}# (quote (bare)))
                         (#{bare-cont\ 848}# #{var\ 846}#)
                         (if (memv #{kind\ 849}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 850}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 850}#)
                                   #{var\ 846}#)
                                 #f)
                             (#{modref-cont\ 847}#
                               #{mod\ 850}#
                               #{var\ 846}#
                               #f)
                             (#{bare-cont\ 848}# #{var\ 846}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 846}#
                             #{mod\ 850}#)))))))))
           (#{build-lexical-assignment\ 99}#
             (lambda (#{source\ 851}#
                      #{name\ 852}#
                      #{var\ 853}#
                      #{exp\ 854}#)
               (let ((#{atom-key\ 855}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 855}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 851}#
                    #{name\ 852}#
                    #{var\ 853}#
                    #{exp\ 854}#)
                   (#{decorate-source\ 94}#
                     (list (quote set!) #{var\ 853}# #{exp\ 854}#)
                     #{source\ 851}#)))))
           (#{build-lexical-reference\ 98}#
             (lambda (#{type\ 856}#
                      #{source\ 857}#
                      #{name\ 858}#
                      #{var\ 859}#)
               (let ((#{atom-key\ 860}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 860}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 857}#
                    #{name\ 858}#
                    #{var\ 859}#)
                   (#{decorate-source\ 94}#
                     #{var\ 859}#
                     #{source\ 857}#)))))
           (#{build-conditional\ 97}#
             (lambda (#{source\ 861}#
                      #{test-exp\ 862}#
                      #{then-exp\ 863}#
                      #{else-exp\ 864}#)
               (let ((#{atom-key\ 865}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 865}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 861}#
                    #{test-exp\ 862}#
                    #{then-exp\ 863}#
                    #{else-exp\ 864}#)
                   (#{decorate-source\ 94}#
                     (if (equal? #{else-exp\ 864}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 862}#
                             #{then-exp\ 863}#)
                       (list 'if
                             #{test-exp\ 862}#
                             #{then-exp\ 863}#
                             #{else-exp\ 864}#))
                     #{source\ 861}#)))))
           (#{build-application\ 96}#
             (lambda (#{source\ 866}#
                      #{fun-exp\ 867}#
                      #{arg-exps\ 868}#)
               (let ((#{atom-key\ 869}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 869}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 866}#
                    #{fun-exp\ 867}#
                    #{arg-exps\ 868}#)
                   (#{decorate-source\ 94}#
                     (cons #{fun-exp\ 867}# #{arg-exps\ 868}#)
                     #{source\ 866}#)))))
           (#{build-void\ 95}#
             (lambda (#{source\ 870}#)
               (let ((#{atom-key\ 871}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 871}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 870}#)
                   (#{decorate-source\ 94}#
                     '(if #f #f)
                     #{source\ 870}#)))))
           (#{decorate-source\ 94}#
             (lambda (#{e\ 872}# #{s\ 873}#)
               (begin
                 (if (if (pair? #{e\ 872}#) #{s\ 873}# #f)
                   (set-source-properties! #{e\ 872}# #{s\ 873}#))
                 #{e\ 872}#)))
           (#{get-global-definition-hook\ 93}#
             (lambda (#{symbol\ 874}# #{module\ 875}#)
               (begin
                 (if (if (not #{module\ 875}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 874}#))
                 (let ((#{v\ 876}# (module-variable
                                     (if #{module\ 875}#
                                       (resolve-module (cdr #{module\ 875}#))
                                       (current-module))
                                     #{symbol\ 874}#)))
                   (if #{v\ 876}#
                     (if (variable-bound? #{v\ 876}#)
                       (let ((#{val\ 877}# (variable-ref #{v\ 876}#)))
                         (if (macro? #{val\ 877}#)
                           (if (syncase-macro-type #{val\ 877}#)
                             (cons (syncase-macro-type #{val\ 877}#)
                                   (syncase-macro-binding #{val\ 877}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 92}#
             (lambda (#{symbol\ 878}# #{type\ 879}# #{val\ 880}#)
               (let ((#{existing\ 881}#
                       (let ((#{v\ 882}# (module-variable
                                           (current-module)
                                           #{symbol\ 878}#)))
                         (if #{v\ 882}#
                           (if (variable-bound? #{v\ 882}#)
                             (let ((#{val\ 883}# (variable-ref #{v\ 882}#)))
                               (if (macro? #{val\ 883}#)
                                 (if (not (syncase-macro-type #{val\ 883}#))
                                   #{val\ 883}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 878}#
                   (if #{existing\ 881}#
                     (make-extended-syncase-macro
                       #{existing\ 881}#
                       #{type\ 879}#
                       #{val\ 880}#)
                     (make-syncase-macro #{type\ 879}# #{val\ 880}#))))))
           (#{local-eval-hook\ 91}#
             (lambda (#{x\ 884}# #{mod\ 885}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 886}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 886}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 884}#)
                           #{x\ 884}#))))))
           (#{top-level-eval-hook\ 90}#
             (lambda (#{x\ 887}# #{mod\ 888}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 889}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 889}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 887}#)
                           #{x\ 887}#))))))
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
        (lambda (#{e\ 890}#
                 #{r\ 891}#
                 #{w\ 892}#
                 #{s\ 893}#
                 #{mod\ 894}#)
          ((lambda (#{tmp\ 895}#)
             ((lambda (#{tmp\ 896}#)
                (if (if #{tmp\ 896}#
                      (apply (lambda (#{_\ 897}#
                                      #{var\ 898}#
                                      #{val\ 899}#
                                      #{e1\ 900}#
                                      #{e2\ 901}#)
                               (#{valid-bound-ids?\ 156}# #{var\ 898}#))
                             #{tmp\ 896}#)
                      #f)
                  (apply (lambda (#{_\ 903}#
                                  #{var\ 904}#
                                  #{val\ 905}#
                                  #{e1\ 906}#
                                  #{e2\ 907}#)
                           (let ((#{names\ 908}#
                                   (map (lambda (#{x\ 909}#)
                                          (#{id-var-name\ 153}#
                                            #{x\ 909}#
                                            #{w\ 892}#))
                                        #{var\ 904}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 911}# #{n\ 912}#)
                                   (let ((#{atom-key\ 913}#
                                           (#{binding-type\ 123}#
                                             (#{lookup\ 128}#
                                               #{n\ 912}#
                                               #{r\ 891}#
                                               #{mod\ 894}#))))
                                     (if (memv #{atom-key\ 913}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 890}#
                                         (#{source-wrap\ 160}#
                                           #{id\ 911}#
                                           #{w\ 892}#
                                           #{s\ 893}#
                                           #{mod\ 894}#)))))
                                 #{var\ 904}#
                                 #{names\ 908}#)
                               (#{chi-body\ 171}#
                                 (cons #{e1\ 906}# #{e2\ 907}#)
                                 (#{source-wrap\ 160}#
                                   #{e\ 890}#
                                   #{w\ 892}#
                                   #{s\ 893}#
                                   #{mod\ 894}#)
                                 (#{extend-env\ 125}#
                                   #{names\ 908}#
                                   (let ((#{trans-r\ 916}#
                                           (#{macros-only-env\ 127}#
                                             #{r\ 891}#)))
                                     (map (lambda (#{x\ 917}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 173}#
                                                    (#{chi\ 167}#
                                                      #{x\ 917}#
                                                      #{trans-r\ 916}#
                                                      #{w\ 892}#
                                                      #{mod\ 894}#)
                                                    #{mod\ 894}#)))
                                          #{val\ 905}#))
                                   #{r\ 891}#)
                                 #{w\ 892}#
                                 #{mod\ 894}#))))
                         #{tmp\ 896}#)
                  ((lambda (#{_\ 919}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 890}#
                         #{w\ 892}#
                         #{s\ 893}#
                         #{mod\ 894}#)))
                   #{tmp\ 895}#)))
              ($sc-dispatch
                #{tmp\ 895}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 890}#)))
      (#{global-extend\ 129}#
        'core
        'quote
        (lambda (#{e\ 920}#
                 #{r\ 921}#
                 #{w\ 922}#
                 #{s\ 923}#
                 #{mod\ 924}#)
          ((lambda (#{tmp\ 925}#)
             ((lambda (#{tmp\ 926}#)
                (if #{tmp\ 926}#
                  (apply (lambda (#{_\ 927}# #{e\ 928}#)
                           (#{build-data\ 109}#
                             #{s\ 923}#
                             (#{strip\ 176}# #{e\ 928}# #{w\ 922}#)))
                         #{tmp\ 926}#)
                  ((lambda (#{_\ 929}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 920}#
                         #{w\ 922}#
                         #{s\ 923}#
                         #{mod\ 924}#)))
                   #{tmp\ 925}#)))
              ($sc-dispatch #{tmp\ 925}# (quote (any any)))))
           #{e\ 920}#)))
      (#{global-extend\ 129}#
        'core
        'syntax
        (letrec ((#{regen\ 937}#
                   (lambda (#{x\ 938}#)
                     (let ((#{atom-key\ 939}# (car #{x\ 938}#)))
                       (if (memv #{atom-key\ 939}# (quote (ref)))
                         (#{build-lexical-reference\ 98}#
                           'value
                           #f
                           (cadr #{x\ 938}#)
                           (cadr #{x\ 938}#))
                         (if (memv #{atom-key\ 939}# (quote (primitive)))
                           (#{build-primref\ 108}# #f (cadr #{x\ 938}#))
                           (if (memv #{atom-key\ 939}# (quote (quote)))
                             (#{build-data\ 109}# #f (cadr #{x\ 938}#))
                             (if (memv #{atom-key\ 939}# (quote (lambda)))
                               (if (list? (cadr #{x\ 938}#))
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (cadr #{x\ 938}#)
                                   #f
                                   (cadr #{x\ 938}#)
                                   #f
                                   (#{regen\ 937}# (caddr #{x\ 938}#)))
                                 (error "how did we get here" #{x\ 938}#))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-primref\ 108}# #f (car #{x\ 938}#))
                                 (map #{regen\ 937}# (cdr #{x\ 938}#))))))))))
                 (#{gen-vector\ 936}#
                   (lambda (#{x\ 940}#)
                     (if (eq? (car #{x\ 940}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 940}#))
                       (if (eq? (car #{x\ 940}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 940}#)))
                         (list (quote list->vector) #{x\ 940}#)))))
                 (#{gen-append\ 935}#
                   (lambda (#{x\ 941}# #{y\ 942}#)
                     (if (equal? #{y\ 942}# (quote (quote ())))
                       #{x\ 941}#
                       (list (quote append) #{x\ 941}# #{y\ 942}#))))
                 (#{gen-cons\ 934}#
                   (lambda (#{x\ 943}# #{y\ 944}#)
                     (let ((#{atom-key\ 945}# (car #{y\ 944}#)))
                       (if (memv #{atom-key\ 945}# (quote (quote)))
                         (if (eq? (car #{x\ 943}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 943}#) (cadr #{y\ 944}#)))
                           (if (eq? (cadr #{y\ 944}#) (quote ()))
                             (list (quote list) #{x\ 943}#)
                             (list (quote cons) #{x\ 943}# #{y\ 944}#)))
                         (if (memv #{atom-key\ 945}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 943}# (cdr #{y\ 944}#)))
                           (list (quote cons) #{x\ 943}# #{y\ 944}#))))))
                 (#{gen-map\ 933}#
                   (lambda (#{e\ 946}# #{map-env\ 947}#)
                     (let ((#{formals\ 948}# (map cdr #{map-env\ 947}#))
                           (#{actuals\ 949}#
                             (map (lambda (#{x\ 950}#)
                                    (list (quote ref) (car #{x\ 950}#)))
                                  #{map-env\ 947}#)))
                       (if (eq? (car #{e\ 946}#) (quote ref))
                         (car #{actuals\ 949}#)
                         (if (and-map
                               (lambda (#{x\ 951}#)
                                 (if (eq? (car #{x\ 951}#) (quote ref))
                                   (memq (cadr #{x\ 951}#) #{formals\ 948}#)
                                   #f))
                               (cdr #{e\ 946}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 946}#))
                                       (map (let ((#{r\ 952}# (map cons
                                                                   #{formals\ 948}#
                                                                   #{actuals\ 949}#)))
                                              (lambda (#{x\ 953}#)
                                                (cdr (assq (cadr #{x\ 953}#)
                                                           #{r\ 952}#))))
                                            (cdr #{e\ 946}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 948}#
                                             #{e\ 946}#)
                                       #{actuals\ 949}#)))))))
                 (#{gen-mappend\ 932}#
                   (lambda (#{e\ 954}# #{map-env\ 955}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 933}# #{e\ 954}# #{map-env\ 955}#))))
                 (#{gen-ref\ 931}#
                   (lambda (#{src\ 956}#
                            #{var\ 957}#
                            #{level\ 958}#
                            #{maps\ 959}#)
                     (if (#{fx=\ 88}# #{level\ 958}# 0)
                       (values #{var\ 957}# #{maps\ 959}#)
                       (if (null? #{maps\ 959}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 956}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 931}#
                               #{src\ 956}#
                               #{var\ 957}#
                               (#{fx-\ 87}# #{level\ 958}# 1)
                               (cdr #{maps\ 959}#)))
                           (lambda (#{outer-var\ 960}# #{outer-maps\ 961}#)
                             (let ((#{b\ 962}# (assq #{outer-var\ 960}#
                                                     (car #{maps\ 959}#))))
                               (if #{b\ 962}#
                                 (values (cdr #{b\ 962}#) #{maps\ 959}#)
                                 (let ((#{inner-var\ 963}#
                                         (#{gen-var\ 177}# (quote tmp))))
                                   (values
                                     #{inner-var\ 963}#
                                     (cons (cons (cons #{outer-var\ 960}#
                                                       #{inner-var\ 963}#)
                                                 (car #{maps\ 959}#))
                                           #{outer-maps\ 961}#)))))))))))
                 (#{gen-syntax\ 930}#
                   (lambda (#{src\ 964}#
                            #{e\ 965}#
                            #{r\ 966}#
                            #{maps\ 967}#
                            #{ellipsis?\ 968}#
                            #{mod\ 969}#)
                     (if (#{id?\ 131}# #{e\ 965}#)
                       (let ((#{label\ 970}#
                               (#{id-var-name\ 153}# #{e\ 965}# (quote (())))))
                         (let ((#{b\ 971}# (#{lookup\ 128}#
                                             #{label\ 970}#
                                             #{r\ 966}#
                                             #{mod\ 969}#)))
                           (if (eq? (#{binding-type\ 123}# #{b\ 971}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 972}#
                                         (#{binding-value\ 124}# #{b\ 971}#)))
                                   (#{gen-ref\ 931}#
                                     #{src\ 964}#
                                     (car #{var.lev\ 972}#)
                                     (cdr #{var.lev\ 972}#)
                                     #{maps\ 967}#)))
                               (lambda (#{var\ 973}# #{maps\ 974}#)
                                 (values
                                   (list (quote ref) #{var\ 973}#)
                                   #{maps\ 974}#)))
                             (if (#{ellipsis?\ 968}# #{e\ 965}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 964}#)
                               (values
                                 (list (quote quote) #{e\ 965}#)
                                 #{maps\ 967}#)))))
                       ((lambda (#{tmp\ 975}#)
                          ((lambda (#{tmp\ 976}#)
                             (if (if #{tmp\ 976}#
                                   (apply (lambda (#{dots\ 977}# #{e\ 978}#)
                                            (#{ellipsis?\ 968}# #{dots\ 977}#))
                                          #{tmp\ 976}#)
                                   #f)
                               (apply (lambda (#{dots\ 979}# #{e\ 980}#)
                                        (#{gen-syntax\ 930}#
                                          #{src\ 964}#
                                          #{e\ 980}#
                                          #{r\ 966}#
                                          #{maps\ 967}#
                                          (lambda (#{x\ 981}#) #f)
                                          #{mod\ 969}#))
                                      #{tmp\ 976}#)
                               ((lambda (#{tmp\ 982}#)
                                  (if (if #{tmp\ 982}#
                                        (apply (lambda (#{x\ 983}#
                                                        #{dots\ 984}#
                                                        #{y\ 985}#)
                                                 (#{ellipsis?\ 968}#
                                                   #{dots\ 984}#))
                                               #{tmp\ 982}#)
                                        #f)
                                    (apply (lambda (#{x\ 986}#
                                                    #{dots\ 987}#
                                                    #{y\ 988}#)
                                             (letrec ((#{f\ 989}# (lambda (#{y\ 990}#
                                                                           #{k\ 991}#)
                                                                    ((lambda (#{tmp\ 995}#)
                                                                       ((lambda (#{tmp\ 996}#)
                                                                          (if (if #{tmp\ 996}#
                                                                                (apply (lambda (#{dots\ 997}#
                                                                                                #{y\ 998}#)
                                                                                         (#{ellipsis?\ 968}#
                                                                                           #{dots\ 997}#))
                                                                                       #{tmp\ 996}#)
                                                                                #f)
                                                                            (apply (lambda (#{dots\ 999}#
                                                                                            #{y\ 1000}#)
                                                                                     (#{f\ 989}# #{y\ 1000}#
                                                                                                 (lambda (#{maps\ 1001}#)
                                                                                                   (call-with-values
                                                                                                     (lambda ()
                                                                                                       (#{k\ 991}# (cons '()
                                                                                                                         #{maps\ 1001}#)))
                                                                                                     (lambda (#{x\ 1002}#
                                                                                                              #{maps\ 1003}#)
                                                                                                       (if (null? (car #{maps\ 1003}#))
                                                                                                         (syntax-violation
                                                                                                           'syntax
                                                                                                           "extra ellipsis"
                                                                                                           #{src\ 964}#)
                                                                                                         (values
                                                                                                           (#{gen-mappend\ 932}#
                                                                                                             #{x\ 1002}#
                                                                                                             (car #{maps\ 1003}#))
                                                                                                           (cdr #{maps\ 1003}#))))))))
                                                                                   #{tmp\ 996}#)
                                                                            ((lambda (#{_\ 1004}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{gen-syntax\ 930}#
                                                                                     #{src\ 964}#
                                                                                     #{y\ 990}#
                                                                                     #{r\ 966}#
                                                                                     #{maps\ 967}#
                                                                                     #{ellipsis?\ 968}#
                                                                                     #{mod\ 969}#))
                                                                                 (lambda (#{y\ 1005}#
                                                                                          #{maps\ 1006}#)
                                                                                   (call-with-values
                                                                                     (lambda ()
                                                                                       (#{k\ 991}# #{maps\ 1006}#))
                                                                                     (lambda (#{x\ 1007}#
                                                                                              #{maps\ 1008}#)
                                                                                       (values
                                                                                         (#{gen-append\ 935}#
                                                                                           #{x\ 1007}#
                                                                                           #{y\ 1005}#)
                                                                                         #{maps\ 1008}#))))))
                                                                             #{tmp\ 995}#)))
                                                                        ($sc-dispatch
                                                                          #{tmp\ 995}#
                                                                          '(any .
                                                                                any))))
                                                                     #{y\ 990}#))))
                                               (#{f\ 989}# #{y\ 988}#
                                                           (lambda (#{maps\ 992}#)
                                                             (call-with-values
                                                               (lambda ()
                                                                 (#{gen-syntax\ 930}#
                                                                   #{src\ 964}#
                                                                   #{x\ 986}#
                                                                   #{r\ 966}#
                                                                   (cons '()
                                                                         #{maps\ 992}#)
                                                                   #{ellipsis?\ 968}#
                                                                   #{mod\ 969}#))
                                                               (lambda (#{x\ 993}#
                                                                        #{maps\ 994}#)
                                                                 (if (null? (car #{maps\ 994}#))
                                                                   (syntax-violation
                                                                     'syntax
                                                                     "extra ellipsis"
                                                                     #{src\ 964}#)
                                                                   (values
                                                                     (#{gen-map\ 933}#
                                                                       #{x\ 993}#
                                                                       (car #{maps\ 994}#))
                                                                     (cdr #{maps\ 994}#)))))))))
                                           #{tmp\ 982}#)
                                    ((lambda (#{tmp\ 1009}#)
                                       (if #{tmp\ 1009}#
                                         (apply (lambda (#{x\ 1010}#
                                                         #{y\ 1011}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 930}#
                                                        #{src\ 964}#
                                                        #{x\ 1010}#
                                                        #{r\ 966}#
                                                        #{maps\ 967}#
                                                        #{ellipsis?\ 968}#
                                                        #{mod\ 969}#))
                                                    (lambda (#{x\ 1012}#
                                                             #{maps\ 1013}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 930}#
                                                            #{src\ 964}#
                                                            #{y\ 1011}#
                                                            #{r\ 966}#
                                                            #{maps\ 1013}#
                                                            #{ellipsis?\ 968}#
                                                            #{mod\ 969}#))
                                                        (lambda (#{y\ 1014}#
                                                                 #{maps\ 1015}#)
                                                          (values
                                                            (#{gen-cons\ 934}#
                                                              #{x\ 1012}#
                                                              #{y\ 1014}#)
                                                            #{maps\ 1015}#))))))
                                                #{tmp\ 1009}#)
                                         ((lambda (#{tmp\ 1016}#)
                                            (if #{tmp\ 1016}#
                                              (apply (lambda (#{e1\ 1017}#
                                                              #{e2\ 1018}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 930}#
                                                             #{src\ 964}#
                                                             (cons #{e1\ 1017}#
                                                                   #{e2\ 1018}#)
                                                             #{r\ 966}#
                                                             #{maps\ 967}#
                                                             #{ellipsis?\ 968}#
                                                             #{mod\ 969}#))
                                                         (lambda (#{e\ 1020}#
                                                                  #{maps\ 1021}#)
                                                           (values
                                                             (#{gen-vector\ 936}#
                                                               #{e\ 1020}#)
                                                             #{maps\ 1021}#))))
                                                     #{tmp\ 1016}#)
                                              ((lambda (#{_\ 1022}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 965}#)
                                                   #{maps\ 967}#))
                                               #{tmp\ 975}#)))
                                          ($sc-dispatch
                                            #{tmp\ 975}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 975}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 975}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 975}# (quote (any any)))))
                        #{e\ 965}#)))))
          (lambda (#{e\ 1023}#
                   #{r\ 1024}#
                   #{w\ 1025}#
                   #{s\ 1026}#
                   #{mod\ 1027}#)
            (let ((#{e\ 1028}#
                    (#{source-wrap\ 160}#
                      #{e\ 1023}#
                      #{w\ 1025}#
                      #{s\ 1026}#
                      #{mod\ 1027}#)))
              ((lambda (#{tmp\ 1029}#)
                 ((lambda (#{tmp\ 1030}#)
                    (if #{tmp\ 1030}#
                      (apply (lambda (#{_\ 1031}# #{x\ 1032}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 930}#
                                     #{e\ 1028}#
                                     #{x\ 1032}#
                                     #{r\ 1024}#
                                     '()
                                     #{ellipsis?\ 175}#
                                     #{mod\ 1027}#))
                                 (lambda (#{e\ 1033}# #{maps\ 1034}#)
                                   (#{regen\ 937}# #{e\ 1033}#))))
                             #{tmp\ 1030}#)
                      ((lambda (#{_\ 1035}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 1028}#))
                       #{tmp\ 1029}#)))
                  ($sc-dispatch #{tmp\ 1029}# (quote (any any)))))
               #{e\ 1028}#)))))
      (#{global-extend\ 129}#
        'core
        'lambda
        (lambda (#{e\ 1036}#
                 #{r\ 1037}#
                 #{w\ 1038}#
                 #{s\ 1039}#
                 #{mod\ 1040}#)
          (letrec ((#{docstring&body\ 1041}#
                     (lambda (#{ids\ 1042}#
                              #{vars\ 1043}#
                              #{labels\ 1044}#
                              #{c\ 1045}#)
                       ((lambda (#{tmp\ 1046}#)
                          ((lambda (#{tmp\ 1047}#)
                             (if (if #{tmp\ 1047}#
                                   (apply (lambda (#{docstring\ 1048}#
                                                   #{e1\ 1049}#
                                                   #{e2\ 1050}#)
                                            (string?
                                              (syntax->datum
                                                #{docstring\ 1048}#)))
                                          #{tmp\ 1047}#)
                                   #f)
                               (apply (lambda (#{docstring\ 1051}#
                                               #{e1\ 1052}#
                                               #{e2\ 1053}#)
                                        (values
                                          (syntax->datum #{docstring\ 1051}#)
                                          (#{chi-body\ 171}#
                                            (cons #{e1\ 1052}# #{e2\ 1053}#)
                                            (#{source-wrap\ 160}#
                                              #{e\ 1036}#
                                              #{w\ 1038}#
                                              #{s\ 1039}#
                                              #{mod\ 1040}#)
                                            (#{extend-var-env\ 126}#
                                              #{labels\ 1044}#
                                              #{vars\ 1043}#
                                              #{r\ 1037}#)
                                            (#{make-binding-wrap\ 148}#
                                              #{ids\ 1042}#
                                              #{labels\ 1044}#
                                              #{w\ 1038}#)
                                            #{mod\ 1040}#)))
                                      #{tmp\ 1047}#)
                               ((lambda (#{tmp\ 1055}#)
                                  (if #{tmp\ 1055}#
                                    (apply (lambda (#{e1\ 1056}# #{e2\ 1057}#)
                                             (values
                                               #f
                                               (#{chi-body\ 171}#
                                                 (cons #{e1\ 1056}#
                                                       #{e2\ 1057}#)
                                                 (#{source-wrap\ 160}#
                                                   #{e\ 1036}#
                                                   #{w\ 1038}#
                                                   #{s\ 1039}#
                                                   #{mod\ 1040}#)
                                                 (#{extend-var-env\ 126}#
                                                   #{labels\ 1044}#
                                                   #{vars\ 1043}#
                                                   #{r\ 1037}#)
                                                 (#{make-binding-wrap\ 148}#
                                                   #{ids\ 1042}#
                                                   #{labels\ 1044}#
                                                   #{w\ 1038}#)
                                                 #{mod\ 1040}#)))
                                           #{tmp\ 1055}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp\ 1046}#)))
                                ($sc-dispatch
                                  #{tmp\ 1046}#
                                  '(any . each-any)))))
                           ($sc-dispatch
                             #{tmp\ 1046}#
                             '(any any . each-any))))
                        #{c\ 1045}#))))
            ((lambda (#{tmp\ 1059}#)
               ((lambda (#{tmp\ 1060}#)
                  (if #{tmp\ 1060}#
                    (apply (lambda (#{_\ 1061}#
                                    #{id\ 1062}#
                                    #{e1\ 1063}#
                                    #{e2\ 1064}#)
                             (let ((#{ids\ 1065}# #{id\ 1062}#))
                               (if (not (#{valid-bound-ids?\ 156}#
                                          #{ids\ 1065}#))
                                 (syntax-violation
                                   'lambda
                                   "invalid parameter list"
                                   #{e\ 1036}#)
                                 (let ((#{vars\ 1067}#
                                         (map #{gen-var\ 177}# #{ids\ 1065}#))
                                       (#{labels\ 1068}#
                                         (#{gen-labels\ 137}# #{ids\ 1065}#)))
                                   (call-with-values
                                     (lambda ()
                                       (#{docstring&body\ 1041}#
                                         #{ids\ 1065}#
                                         #{vars\ 1067}#
                                         #{labels\ 1068}#
                                         (cons #{e1\ 1063}# #{e2\ 1064}#)))
                                     (lambda (#{docstring\ 1070}#
                                              #{body\ 1071}#)
                                       (#{build-simple-lambda\ 105}#
                                         #{s\ 1039}#
                                         (map syntax->datum #{ids\ 1065}#)
                                         #f
                                         #{vars\ 1067}#
                                         #{docstring\ 1070}#
                                         #{body\ 1071}#)))))))
                           #{tmp\ 1060}#)
                    ((lambda (#{tmp\ 1072}#)
                       (if #{tmp\ 1072}#
                         (apply (lambda (#{_\ 1073}#
                                         #{ids\ 1074}#
                                         #{e1\ 1075}#
                                         #{e2\ 1076}#)
                                  (let ((#{rids\ 1077}#
                                          (#{lambda-var-list\ 178}#
                                            #{ids\ 1074}#)))
                                    (if (not (#{valid-bound-ids?\ 156}#
                                               #{rids\ 1077}#))
                                      (syntax-violation
                                        'lambda
                                        "invalid parameter list"
                                        #{e\ 1036}#)
                                      (let ((#{req\ 1078}#
                                              (reverse (cdr #{rids\ 1077}#))))
                                        (let ((#{rest\ 1079}#
                                                (car #{rids\ 1077}#)))
                                          (let ((#{rrids\ 1080}#
                                                  (reverse #{rids\ 1077}#)))
                                            (let ((#{vars\ 1081}#
                                                    (map #{gen-var\ 177}#
                                                         #{rrids\ 1080}#)))
                                              (let ((#{labels\ 1082}#
                                                      (#{gen-labels\ 137}#
                                                        #{rrids\ 1080}#)))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{docstring&body\ 1041}#
                                                      #{rrids\ 1080}#
                                                      #{vars\ 1081}#
                                                      #{labels\ 1082}#
                                                      (cons #{e1\ 1075}#
                                                            #{e2\ 1076}#)))
                                                  (lambda (#{docstring\ 1084}#
                                                           #{body\ 1085}#)
                                                    (#{build-simple-lambda\ 105}#
                                                      #{s\ 1039}#
                                                      (map syntax->datum
                                                           #{req\ 1078}#)
                                                      (syntax->datum
                                                        #{rest\ 1079}#)
                                                      #{vars\ 1081}#
                                                      #{docstring\ 1084}#
                                                      #{body\ 1085}#)))))))))))
                                #{tmp\ 1072}#)
                         ((lambda (#{_\ 1086}#)
                            (syntax-violation
                              'lambda
                              "bad lambda"
                              #{e\ 1036}#))
                          #{tmp\ 1059}#)))
                     ($sc-dispatch
                       #{tmp\ 1059}#
                       '(any any any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1059}#
                  '(any each-any any . each-any))))
             #{e\ 1036}#))))
      (#{global-extend\ 129}#
        'core
        'let
        (letrec ((#{chi-let\ 1087}#
                   (lambda (#{e\ 1088}#
                            #{r\ 1089}#
                            #{w\ 1090}#
                            #{s\ 1091}#
                            #{mod\ 1092}#
                            #{constructor\ 1093}#
                            #{ids\ 1094}#
                            #{vals\ 1095}#
                            #{exps\ 1096}#)
                     (if (not (#{valid-bound-ids?\ 156}# #{ids\ 1094}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 1088}#)
                       (let ((#{labels\ 1097}#
                               (#{gen-labels\ 137}# #{ids\ 1094}#))
                             (#{new-vars\ 1098}#
                               (map #{gen-var\ 177}# #{ids\ 1094}#)))
                         (let ((#{nw\ 1099}#
                                 (#{make-binding-wrap\ 148}#
                                   #{ids\ 1094}#
                                   #{labels\ 1097}#
                                   #{w\ 1090}#))
                               (#{nr\ 1100}#
                                 (#{extend-var-env\ 126}#
                                   #{labels\ 1097}#
                                   #{new-vars\ 1098}#
                                   #{r\ 1089}#)))
                           (#{constructor\ 1093}#
                             #{s\ 1091}#
                             (map syntax->datum #{ids\ 1094}#)
                             #{new-vars\ 1098}#
                             (map (lambda (#{x\ 1101}#)
                                    (#{chi\ 167}#
                                      #{x\ 1101}#
                                      #{r\ 1089}#
                                      #{w\ 1090}#
                                      #{mod\ 1092}#))
                                  #{vals\ 1095}#)
                             (#{chi-body\ 171}#
                               #{exps\ 1096}#
                               (#{source-wrap\ 160}#
                                 #{e\ 1088}#
                                 #{nw\ 1099}#
                                 #{s\ 1091}#
                                 #{mod\ 1092}#)
                               #{nr\ 1100}#
                               #{nw\ 1099}#
                               #{mod\ 1092}#))))))))
          (lambda (#{e\ 1102}#
                   #{r\ 1103}#
                   #{w\ 1104}#
                   #{s\ 1105}#
                   #{mod\ 1106}#)
            ((lambda (#{tmp\ 1107}#)
               ((lambda (#{tmp\ 1108}#)
                  (if (if #{tmp\ 1108}#
                        (apply (lambda (#{_\ 1109}#
                                        #{id\ 1110}#
                                        #{val\ 1111}#
                                        #{e1\ 1112}#
                                        #{e2\ 1113}#)
                                 (and-map #{id?\ 131}# #{id\ 1110}#))
                               #{tmp\ 1108}#)
                        #f)
                    (apply (lambda (#{_\ 1115}#
                                    #{id\ 1116}#
                                    #{val\ 1117}#
                                    #{e1\ 1118}#
                                    #{e2\ 1119}#)
                             (#{chi-let\ 1087}#
                               #{e\ 1102}#
                               #{r\ 1103}#
                               #{w\ 1104}#
                               #{s\ 1105}#
                               #{mod\ 1106}#
                               #{build-let\ 111}#
                               #{id\ 1116}#
                               #{val\ 1117}#
                               (cons #{e1\ 1118}# #{e2\ 1119}#)))
                           #{tmp\ 1108}#)
                    ((lambda (#{tmp\ 1123}#)
                       (if (if #{tmp\ 1123}#
                             (apply (lambda (#{_\ 1124}#
                                             #{f\ 1125}#
                                             #{id\ 1126}#
                                             #{val\ 1127}#
                                             #{e1\ 1128}#
                                             #{e2\ 1129}#)
                                      (if (#{id?\ 131}# #{f\ 1125}#)
                                        (and-map #{id?\ 131}# #{id\ 1126}#)
                                        #f))
                                    #{tmp\ 1123}#)
                             #f)
                         (apply (lambda (#{_\ 1131}#
                                         #{f\ 1132}#
                                         #{id\ 1133}#
                                         #{val\ 1134}#
                                         #{e1\ 1135}#
                                         #{e2\ 1136}#)
                                  (#{chi-let\ 1087}#
                                    #{e\ 1102}#
                                    #{r\ 1103}#
                                    #{w\ 1104}#
                                    #{s\ 1105}#
                                    #{mod\ 1106}#
                                    #{build-named-let\ 112}#
                                    (cons #{f\ 1132}# #{id\ 1133}#)
                                    #{val\ 1134}#
                                    (cons #{e1\ 1135}# #{e2\ 1136}#)))
                                #{tmp\ 1123}#)
                         ((lambda (#{_\ 1140}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 160}#
                                #{e\ 1102}#
                                #{w\ 1104}#
                                #{s\ 1105}#
                                #{mod\ 1106}#)))
                          #{tmp\ 1107}#)))
                     ($sc-dispatch
                       #{tmp\ 1107}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1107}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 1102}#))))
      (#{global-extend\ 129}#
        'core
        'letrec
        (lambda (#{e\ 1141}#
                 #{r\ 1142}#
                 #{w\ 1143}#
                 #{s\ 1144}#
                 #{mod\ 1145}#)
          ((lambda (#{tmp\ 1146}#)
             ((lambda (#{tmp\ 1147}#)
                (if (if #{tmp\ 1147}#
                      (apply (lambda (#{_\ 1148}#
                                      #{id\ 1149}#
                                      #{val\ 1150}#
                                      #{e1\ 1151}#
                                      #{e2\ 1152}#)
                               (and-map #{id?\ 131}# #{id\ 1149}#))
                             #{tmp\ 1147}#)
                      #f)
                  (apply (lambda (#{_\ 1154}#
                                  #{id\ 1155}#
                                  #{val\ 1156}#
                                  #{e1\ 1157}#
                                  #{e2\ 1158}#)
                           (let ((#{ids\ 1159}# #{id\ 1155}#))
                             (if (not (#{valid-bound-ids?\ 156}#
                                        #{ids\ 1159}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 1141}#)
                               (let ((#{labels\ 1161}#
                                       (#{gen-labels\ 137}# #{ids\ 1159}#))
                                     (#{new-vars\ 1162}#
                                       (map #{gen-var\ 177}# #{ids\ 1159}#)))
                                 (let ((#{w\ 1163}#
                                         (#{make-binding-wrap\ 148}#
                                           #{ids\ 1159}#
                                           #{labels\ 1161}#
                                           #{w\ 1143}#))
                                       (#{r\ 1164}#
                                         (#{extend-var-env\ 126}#
                                           #{labels\ 1161}#
                                           #{new-vars\ 1162}#
                                           #{r\ 1142}#)))
                                   (#{build-letrec\ 113}#
                                     #{s\ 1144}#
                                     (map syntax->datum #{ids\ 1159}#)
                                     #{new-vars\ 1162}#
                                     (map (lambda (#{x\ 1165}#)
                                            (#{chi\ 167}#
                                              #{x\ 1165}#
                                              #{r\ 1164}#
                                              #{w\ 1163}#
                                              #{mod\ 1145}#))
                                          #{val\ 1156}#)
                                     (#{chi-body\ 171}#
                                       (cons #{e1\ 1157}# #{e2\ 1158}#)
                                       (#{source-wrap\ 160}#
                                         #{e\ 1141}#
                                         #{w\ 1163}#
                                         #{s\ 1144}#
                                         #{mod\ 1145}#)
                                       #{r\ 1164}#
                                       #{w\ 1163}#
                                       #{mod\ 1145}#)))))))
                         #{tmp\ 1147}#)
                  ((lambda (#{_\ 1168}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 160}#
                         #{e\ 1141}#
                         #{w\ 1143}#
                         #{s\ 1144}#
                         #{mod\ 1145}#)))
                   #{tmp\ 1146}#)))
              ($sc-dispatch
                #{tmp\ 1146}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1141}#)))
      (#{global-extend\ 129}#
        'core
        'set!
        (lambda (#{e\ 1169}#
                 #{r\ 1170}#
                 #{w\ 1171}#
                 #{s\ 1172}#
                 #{mod\ 1173}#)
          ((lambda (#{tmp\ 1174}#)
             ((lambda (#{tmp\ 1175}#)
                (if (if #{tmp\ 1175}#
                      (apply (lambda (#{_\ 1176}# #{id\ 1177}# #{val\ 1178}#)
                               (#{id?\ 131}# #{id\ 1177}#))
                             #{tmp\ 1175}#)
                      #f)
                  (apply (lambda (#{_\ 1179}# #{id\ 1180}# #{val\ 1181}#)
                           (let ((#{val\ 1182}#
                                   (#{chi\ 167}#
                                     #{val\ 1181}#
                                     #{r\ 1170}#
                                     #{w\ 1171}#
                                     #{mod\ 1173}#))
                                 (#{n\ 1183}#
                                   (#{id-var-name\ 153}#
                                     #{id\ 1180}#
                                     #{w\ 1171}#)))
                             (let ((#{b\ 1184}#
                                     (#{lookup\ 128}#
                                       #{n\ 1183}#
                                       #{r\ 1170}#
                                       #{mod\ 1173}#)))
                               (let ((#{atom-key\ 1185}#
                                       (#{binding-type\ 123}# #{b\ 1184}#)))
                                 (if (memv #{atom-key\ 1185}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 99}#
                                     #{s\ 1172}#
                                     (syntax->datum #{id\ 1180}#)
                                     (#{binding-value\ 124}# #{b\ 1184}#)
                                     #{val\ 1182}#)
                                   (if (memv #{atom-key\ 1185}#
                                             '(global))
                                     (#{build-global-assignment\ 102}#
                                       #{s\ 1172}#
                                       #{n\ 1183}#
                                       #{val\ 1182}#
                                       #{mod\ 1173}#)
                                     (if (memv #{atom-key\ 1185}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 159}#
                                           #{id\ 1180}#
                                           #{w\ 1171}#
                                           #{mod\ 1173}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 160}#
                                           #{e\ 1169}#
                                           #{w\ 1171}#
                                           #{s\ 1172}#
                                           #{mod\ 1173}#)))))))))
                         #{tmp\ 1175}#)
                  ((lambda (#{tmp\ 1186}#)
                     (if #{tmp\ 1186}#
                       (apply (lambda (#{_\ 1187}#
                                       #{head\ 1188}#
                                       #{tail\ 1189}#
                                       #{val\ 1190}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 165}#
                                      #{head\ 1188}#
                                      #{r\ 1170}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 1173}#
                                      #t))
                                  (lambda (#{type\ 1191}#
                                           #{value\ 1192}#
                                           #{ee\ 1193}#
                                           #{ww\ 1194}#
                                           #{ss\ 1195}#
                                           #{modmod\ 1196}#)
                                    (if (memv #{type\ 1191}#
                                              '(module-ref))
                                      (let ((#{val\ 1197}#
                                              (#{chi\ 167}#
                                                #{val\ 1190}#
                                                #{r\ 1170}#
                                                #{w\ 1171}#
                                                #{mod\ 1173}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 1192}#
                                              (cons #{head\ 1188}#
                                                    #{tail\ 1189}#)))
                                          (lambda (#{id\ 1199}# #{mod\ 1200}#)
                                            (#{build-global-assignment\ 102}#
                                              #{s\ 1172}#
                                              #{id\ 1199}#
                                              #{val\ 1197}#
                                              #{mod\ 1200}#))))
                                      (#{build-application\ 96}#
                                        #{s\ 1172}#
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
                                                       "i"))
                                                    #(ribcage
                                                      (define-structure
                                                        and-map*)
                                                      ((top) (top))
                                                      ("i" "i")))
                                                   (hygiene guile))
                                                #{head\ 1188}#)
                                          #{r\ 1170}#
                                          #{w\ 1171}#
                                          #{mod\ 1173}#)
                                        (map (lambda (#{e\ 1201}#)
                                               (#{chi\ 167}#
                                                 #{e\ 1201}#
                                                 #{r\ 1170}#
                                                 #{w\ 1171}#
                                                 #{mod\ 1173}#))
                                             (append
                                               #{tail\ 1189}#
                                               (list #{val\ 1190}#))))))))
                              #{tmp\ 1186}#)
                       ((lambda (#{_\ 1203}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 160}#
                              #{e\ 1169}#
                              #{w\ 1171}#
                              #{s\ 1172}#
                              #{mod\ 1173}#)))
                        #{tmp\ 1174}#)))
                   ($sc-dispatch
                     #{tmp\ 1174}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 1174}#
                '(any any any))))
           #{e\ 1169}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@
        (lambda (#{e\ 1204}#)
          ((lambda (#{tmp\ 1205}#)
             ((lambda (#{tmp\ 1206}#)
                (if (if #{tmp\ 1206}#
                      (apply (lambda (#{_\ 1207}# #{mod\ 1208}# #{id\ 1209}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1208}#)
                                 (#{id?\ 131}# #{id\ 1209}#)
                                 #f))
                             #{tmp\ 1206}#)
                      #f)
                  (apply (lambda (#{_\ 1211}# #{mod\ 1212}# #{id\ 1213}#)
                           (values
                             (syntax->datum #{id\ 1213}#)
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
                                            "i"))
                                         #(ribcage
                                           (define-structure and-map*)
                                           ((top) (top))
                                           ("i" "i")))
                                        (hygiene guile))
                                     #{mod\ 1212}#))))
                         #{tmp\ 1206}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1205}#)))
              ($sc-dispatch
                #{tmp\ 1205}#
                '(any each-any any))))
           #{e\ 1204}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@@
        (lambda (#{e\ 1215}#)
          ((lambda (#{tmp\ 1216}#)
             ((lambda (#{tmp\ 1217}#)
                (if (if #{tmp\ 1217}#
                      (apply (lambda (#{_\ 1218}# #{mod\ 1219}# #{id\ 1220}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1219}#)
                                 (#{id?\ 131}# #{id\ 1220}#)
                                 #f))
                             #{tmp\ 1217}#)
                      #f)
                  (apply (lambda (#{_\ 1222}# #{mod\ 1223}# #{id\ 1224}#)
                           (values
                             (syntax->datum #{id\ 1224}#)
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
                                            "i"))
                                         #(ribcage
                                           (define-structure and-map*)
                                           ((top) (top))
                                           ("i" "i")))
                                        (hygiene guile))
                                     #{mod\ 1223}#))))
                         #{tmp\ 1217}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1216}#)))
              ($sc-dispatch
                #{tmp\ 1216}#
                '(any each-any any))))
           #{e\ 1215}#)))
      (#{global-extend\ 129}#
        'core
        'if
        (lambda (#{e\ 1226}#
                 #{r\ 1227}#
                 #{w\ 1228}#
                 #{s\ 1229}#
                 #{mod\ 1230}#)
          ((lambda (#{tmp\ 1231}#)
             ((lambda (#{tmp\ 1232}#)
                (if #{tmp\ 1232}#
                  (apply (lambda (#{_\ 1233}# #{test\ 1234}# #{then\ 1235}#)
                           (#{build-conditional\ 97}#
                             #{s\ 1229}#
                             (#{chi\ 167}#
                               #{test\ 1234}#
                               #{r\ 1227}#
                               #{w\ 1228}#
                               #{mod\ 1230}#)
                             (#{chi\ 167}#
                               #{then\ 1235}#
                               #{r\ 1227}#
                               #{w\ 1228}#
                               #{mod\ 1230}#)
                             (#{build-void\ 95}# #f)))
                         #{tmp\ 1232}#)
                  ((lambda (#{tmp\ 1236}#)
                     (if #{tmp\ 1236}#
                       (apply (lambda (#{_\ 1237}#
                                       #{test\ 1238}#
                                       #{then\ 1239}#
                                       #{else\ 1240}#)
                                (#{build-conditional\ 97}#
                                  #{s\ 1229}#
                                  (#{chi\ 167}#
                                    #{test\ 1238}#
                                    #{r\ 1227}#
                                    #{w\ 1228}#
                                    #{mod\ 1230}#)
                                  (#{chi\ 167}#
                                    #{then\ 1239}#
                                    #{r\ 1227}#
                                    #{w\ 1228}#
                                    #{mod\ 1230}#)
                                  (#{chi\ 167}#
                                    #{else\ 1240}#
                                    #{r\ 1227}#
                                    #{w\ 1228}#
                                    #{mod\ 1230}#)))
                              #{tmp\ 1236}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1231}#)))
                   ($sc-dispatch
                     #{tmp\ 1231}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 1231}#
                '(any any any))))
           #{e\ 1226}#)))
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
        (letrec ((#{gen-syntax-case\ 1244}#
                   (lambda (#{x\ 1245}#
                            #{keys\ 1246}#
                            #{clauses\ 1247}#
                            #{r\ 1248}#
                            #{mod\ 1249}#)
                     (if (null? #{clauses\ 1247}#)
                       (#{build-application\ 96}#
                         #f
                         (#{build-primref\ 108}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 109}# #f #f)
                               (#{build-data\ 109}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 1245}#))
                       ((lambda (#{tmp\ 1250}#)
                          ((lambda (#{tmp\ 1251}#)
                             (if #{tmp\ 1251}#
                               (apply (lambda (#{pat\ 1252}# #{exp\ 1253}#)
                                        (if (if (#{id?\ 131}# #{pat\ 1252}#)
                                              (and-map
                                                (lambda (#{x\ 1254}#)
                                                  (not (#{free-id=?\ 154}#
                                                         #{pat\ 1252}#
                                                         #{x\ 1254}#)))
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
                                                             "i"))
                                                          #(ribcage
                                                            (define-structure
                                                              and-map*)
                                                            ((top) (top))
                                                            ("i" "i")))
                                                         (hygiene guile))
                                                      #{keys\ 1246}#))
                                              #f)
                                          (let ((#{labels\ 1255}#
                                                  (list (#{gen-label\ 136}#)))
                                                (#{var\ 1256}#
                                                  (#{gen-var\ 177}#
                                                    #{pat\ 1252}#)))
                                            (#{build-application\ 96}#
                                              #f
                                              (#{build-simple-lambda\ 105}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 1252}#))
                                                #f
                                                (list #{var\ 1256}#)
                                                #f
                                                (#{chi\ 167}#
                                                  #{exp\ 1253}#
                                                  (#{extend-env\ 125}#
                                                    #{labels\ 1255}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 1256}#
                                                                      0)))
                                                    #{r\ 1248}#)
                                                  (#{make-binding-wrap\ 148}#
                                                    (list #{pat\ 1252}#)
                                                    #{labels\ 1255}#
                                                    '(()))
                                                  #{mod\ 1249}#))
                                              (list #{x\ 1245}#)))
                                          (#{gen-clause\ 1243}#
                                            #{x\ 1245}#
                                            #{keys\ 1246}#
                                            (cdr #{clauses\ 1247}#)
                                            #{r\ 1248}#
                                            #{pat\ 1252}#
                                            #t
                                            #{exp\ 1253}#
                                            #{mod\ 1249}#)))
                                      #{tmp\ 1251}#)
                               ((lambda (#{tmp\ 1257}#)
                                  (if #{tmp\ 1257}#
                                    (apply (lambda (#{pat\ 1258}#
                                                    #{fender\ 1259}#
                                                    #{exp\ 1260}#)
                                             (#{gen-clause\ 1243}#
                                               #{x\ 1245}#
                                               #{keys\ 1246}#
                                               (cdr #{clauses\ 1247}#)
                                               #{r\ 1248}#
                                               #{pat\ 1258}#
                                               #{fender\ 1259}#
                                               #{exp\ 1260}#
                                               #{mod\ 1249}#))
                                           #{tmp\ 1257}#)
                                    ((lambda (#{_\ 1261}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 1247}#)))
                                     #{tmp\ 1250}#)))
                                ($sc-dispatch
                                  #{tmp\ 1250}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 1250}# (quote (any any)))))
                        (car #{clauses\ 1247}#)))))
                 (#{gen-clause\ 1243}#
                   (lambda (#{x\ 1262}#
                            #{keys\ 1263}#
                            #{clauses\ 1264}#
                            #{r\ 1265}#
                            #{pat\ 1266}#
                            #{fender\ 1267}#
                            #{exp\ 1268}#
                            #{mod\ 1269}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 1241}#
                           #{pat\ 1266}#
                           #{keys\ 1263}#))
                       (lambda (#{p\ 1270}# #{pvars\ 1271}#)
                         (if (not (#{distinct-bound-ids?\ 157}#
                                    (map car #{pvars\ 1271}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 1266}#)
                           (if (not (and-map
                                      (lambda (#{x\ 1272}#)
                                        (not (#{ellipsis?\ 175}#
                                               (car #{x\ 1272}#))))
                                      #{pvars\ 1271}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 1266}#)
                             (let ((#{y\ 1273}#
                                     (#{gen-var\ 177}# (quote tmp))))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 1273}#)
                                   #f
                                   (let ((#{y\ 1274}#
                                           (#{build-lexical-reference\ 98}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 1273}#)))
                                     (#{build-conditional\ 97}#
                                       #f
                                       ((lambda (#{tmp\ 1275}#)
                                          ((lambda (#{tmp\ 1276}#)
                                             (if #{tmp\ 1276}#
                                               (apply (lambda () #{y\ 1274}#)
                                                      #{tmp\ 1276}#)
                                               ((lambda (#{_\ 1277}#)
                                                  (#{build-conditional\ 97}#
                                                    #f
                                                    #{y\ 1274}#
                                                    (#{build-dispatch-call\ 1242}#
                                                      #{pvars\ 1271}#
                                                      #{fender\ 1267}#
                                                      #{y\ 1274}#
                                                      #{r\ 1265}#
                                                      #{mod\ 1269}#)
                                                    (#{build-data\ 109}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 1275}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1275}#
                                             '#(atom #t))))
                                        #{fender\ 1267}#)
                                       (#{build-dispatch-call\ 1242}#
                                         #{pvars\ 1271}#
                                         #{exp\ 1268}#
                                         #{y\ 1274}#
                                         #{r\ 1265}#
                                         #{mod\ 1269}#)
                                       (#{gen-syntax-case\ 1244}#
                                         #{x\ 1262}#
                                         #{keys\ 1263}#
                                         #{clauses\ 1264}#
                                         #{r\ 1265}#
                                         #{mod\ 1269}#))))
                                 (list (if (eq? #{p\ 1270}# (quote any))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             'list)
                                           (list #{x\ 1262}#))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 1262}#
                                                 (#{build-data\ 109}#
                                                   #f
                                                   #{p\ 1270}#)))))))))))))
                 (#{build-dispatch-call\ 1242}#
                   (lambda (#{pvars\ 1278}#
                            #{exp\ 1279}#
                            #{y\ 1280}#
                            #{r\ 1281}#
                            #{mod\ 1282}#)
                     (let ((#{ids\ 1283}# (map car #{pvars\ 1278}#))
                           (#{levels\ 1284}# (map cdr #{pvars\ 1278}#)))
                       (let ((#{labels\ 1285}#
                               (#{gen-labels\ 137}# #{ids\ 1283}#))
                             (#{new-vars\ 1286}#
                               (map #{gen-var\ 177}# #{ids\ 1283}#)))
                         (#{build-application\ 96}#
                           #f
                           (#{build-primref\ 108}# #f (quote apply))
                           (list (#{build-simple-lambda\ 105}#
                                   #f
                                   (map syntax->datum #{ids\ 1283}#)
                                   #f
                                   #{new-vars\ 1286}#
                                   #f
                                   (#{chi\ 167}#
                                     #{exp\ 1279}#
                                     (#{extend-env\ 125}#
                                       #{labels\ 1285}#
                                       (map (lambda (#{var\ 1287}#
                                                     #{level\ 1288}#)
                                              (cons 'syntax
                                                    (cons #{var\ 1287}#
                                                          #{level\ 1288}#)))
                                            #{new-vars\ 1286}#
                                            (map cdr #{pvars\ 1278}#))
                                       #{r\ 1281}#)
                                     (#{make-binding-wrap\ 148}#
                                       #{ids\ 1283}#
                                       #{labels\ 1285}#
                                       '(()))
                                     #{mod\ 1282}#))
                                 #{y\ 1280}#))))))
                 (#{convert-pattern\ 1241}#
                   (lambda (#{pattern\ 1289}# #{keys\ 1290}#)
                     (letrec ((#{cvt\ 1291}#
                                (lambda (#{p\ 1292}# #{n\ 1293}# #{ids\ 1294}#)
                                  (if (#{id?\ 131}# #{p\ 1292}#)
                                    (if (#{bound-id-member?\ 158}#
                                          #{p\ 1292}#
                                          #{keys\ 1290}#)
                                      (values
                                        (vector (quote free-id) #{p\ 1292}#)
                                        #{ids\ 1294}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 1292}# #{n\ 1293}#)
                                              #{ids\ 1294}#)))
                                    ((lambda (#{tmp\ 1295}#)
                                       ((lambda (#{tmp\ 1296}#)
                                          (if (if #{tmp\ 1296}#
                                                (apply (lambda (#{x\ 1297}#
                                                                #{dots\ 1298}#)
                                                         (#{ellipsis?\ 175}#
                                                           #{dots\ 1298}#))
                                                       #{tmp\ 1296}#)
                                                #f)
                                            (apply (lambda (#{x\ 1299}#
                                                            #{dots\ 1300}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 1291}#
                                                           #{x\ 1299}#
                                                           (#{fx+\ 86}#
                                                             #{n\ 1293}#
                                                             1)
                                                           #{ids\ 1294}#))
                                                       (lambda (#{p\ 1301}#
                                                                #{ids\ 1302}#)
                                                         (values
                                                           (if (eq? #{p\ 1301}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 1301}#))
                                                           #{ids\ 1302}#))))
                                                   #{tmp\ 1296}#)
                                            ((lambda (#{tmp\ 1303}#)
                                               (if #{tmp\ 1303}#
                                                 (apply (lambda (#{x\ 1304}#
                                                                 #{y\ 1305}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 1291}#
                                                                #{y\ 1305}#
                                                                #{n\ 1293}#
                                                                #{ids\ 1294}#))
                                                            (lambda (#{y\ 1306}#
                                                                     #{ids\ 1307}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 1291}#
                                                                    #{x\ 1304}#
                                                                    #{n\ 1293}#
                                                                    #{ids\ 1307}#))
                                                                (lambda (#{x\ 1308}#
                                                                         #{ids\ 1309}#)
                                                                  (values
                                                                    (cons #{x\ 1308}#
                                                                          #{y\ 1306}#)
                                                                    #{ids\ 1309}#))))))
                                                        #{tmp\ 1303}#)
                                                 ((lambda (#{tmp\ 1310}#)
                                                    (if #{tmp\ 1310}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 1294}#))
                                                             #{tmp\ 1310}#)
                                                      ((lambda (#{tmp\ 1311}#)
                                                         (if #{tmp\ 1311}#
                                                           (apply (lambda (#{x\ 1312}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 1291}#
                                                                          #{x\ 1312}#
                                                                          #{n\ 1293}#
                                                                          #{ids\ 1294}#))
                                                                      (lambda (#{p\ 1314}#
                                                                               #{ids\ 1315}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 1314}#)
                                                                          #{ids\ 1315}#))))
                                                                  #{tmp\ 1311}#)
                                                           ((lambda (#{x\ 1316}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 176}#
                                                                    #{p\ 1292}#
                                                                    '(())))
                                                                #{ids\ 1294}#))
                                                            #{tmp\ 1295}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1295}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 1295}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 1295}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 1295}#
                                          '(any any))))
                                     #{p\ 1292}#)))))
                       (#{cvt\ 1291}# #{pattern\ 1289}# 0 (quote ()))))))
          (lambda (#{e\ 1317}#
                   #{r\ 1318}#
                   #{w\ 1319}#
                   #{s\ 1320}#
                   #{mod\ 1321}#)
            (let ((#{e\ 1322}#
                    (#{source-wrap\ 160}#
                      #{e\ 1317}#
                      #{w\ 1319}#
                      #{s\ 1320}#
                      #{mod\ 1321}#)))
              ((lambda (#{tmp\ 1323}#)
                 ((lambda (#{tmp\ 1324}#)
                    (if #{tmp\ 1324}#
                      (apply (lambda (#{_\ 1325}#
                                      #{val\ 1326}#
                                      #{key\ 1327}#
                                      #{m\ 1328}#)
                               (if (and-map
                                     (lambda (#{x\ 1329}#)
                                       (if (#{id?\ 131}# #{x\ 1329}#)
                                         (not (#{ellipsis?\ 175}# #{x\ 1329}#))
                                         #f))
                                     #{key\ 1327}#)
                                 (let ((#{x\ 1331}#
                                         (#{gen-var\ 177}# (quote tmp))))
                                   (#{build-application\ 96}#
                                     #{s\ 1320}#
                                     (#{build-simple-lambda\ 105}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 1331}#)
                                       #f
                                       (#{gen-syntax-case\ 1244}#
                                         (#{build-lexical-reference\ 98}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 1331}#)
                                         #{key\ 1327}#
                                         #{m\ 1328}#
                                         #{r\ 1318}#
                                         #{mod\ 1321}#))
                                     (list (#{chi\ 167}#
                                             #{val\ 1326}#
                                             #{r\ 1318}#
                                             '(())
                                             #{mod\ 1321}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 1322}#)))
                             #{tmp\ 1324}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 1323}#)))
                  ($sc-dispatch
                    #{tmp\ 1323}#
                    '(any any each-any . each-any))))
               #{e\ 1322}#)))))
      (set! sc-expand
        (lambda (#{x\ 1334}# . #{rest\ 1335}#)
          (if (if (pair? #{x\ 1334}#)
                (equal? (car #{x\ 1334}#) #{noexpand\ 84}#)
                #f)
            (cadr #{x\ 1334}#)
            (let ((#{m\ 1336}#
                    (if (null? #{rest\ 1335}#)
                      'e
                      (car #{rest\ 1335}#)))
                  (#{esew\ 1337}#
                    (if (let ((#{t\ 1338}# (null? #{rest\ 1335}#)))
                          (if #{t\ 1338}#
                            #{t\ 1338}#
                            (null? (cdr #{rest\ 1335}#))))
                      '(eval)
                      (cadr #{rest\ 1335}#))))
              (with-fluid*
                #{*mode*\ 85}#
                #{m\ 1336}#
                (lambda ()
                  (#{chi-top\ 166}#
                    #{x\ 1334}#
                    '()
                    '((top))
                    #{m\ 1336}#
                    #{esew\ 1337}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 1339}#)
          (#{nonsymbol-id?\ 130}# #{x\ 1339}#)))
      (set! datum->syntax
        (lambda (#{id\ 1340}# #{datum\ 1341}#)
          (#{make-syntax-object\ 114}#
            #{datum\ 1341}#
            (#{syntax-object-wrap\ 117}# #{id\ 1340}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 1342}#)
          (#{strip\ 176}# #{x\ 1342}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 1343}#)
          (begin
            (let ((#{x\ 1344}# #{ls\ 1343}#))
              (if (not (list? #{x\ 1344}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 1344}#)))
            (map (lambda (#{x\ 1345}#)
                   (#{wrap\ 159}# (gensym) (quote ((top))) #f))
                 #{ls\ 1343}#))))
      (set! free-identifier=?
        (lambda (#{x\ 1346}# #{y\ 1347}#)
          (begin
            (let ((#{x\ 1348}# #{x\ 1346}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1348}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1348}#)))
            (let ((#{x\ 1349}# #{y\ 1347}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1349}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1349}#)))
            (#{free-id=?\ 154}# #{x\ 1346}# #{y\ 1347}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 1350}# #{y\ 1351}#)
          (begin
            (let ((#{x\ 1352}# #{x\ 1350}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1352}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1352}#)))
            (let ((#{x\ 1353}# #{y\ 1351}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1353}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1353}#)))
            (#{bound-id=?\ 155}# #{x\ 1350}# #{y\ 1351}#))))
      (set! syntax-violation
        (lambda (#{who\ 1354}#
                 #{message\ 1355}#
                 #{form\ 1356}#
                 .
                 #{subform\ 1357}#)
          (begin
            (let ((#{x\ 1358}# #{who\ 1354}#))
              (if (not ((lambda (#{x\ 1359}#)
                          (let ((#{t\ 1360}# (not #{x\ 1359}#)))
                            (if #{t\ 1360}#
                              #{t\ 1360}#
                              (let ((#{t\ 1361}# (string? #{x\ 1359}#)))
                                (if #{t\ 1361}#
                                  #{t\ 1361}#
                                  (symbol? #{x\ 1359}#))))))
                        #{x\ 1358}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1358}#)))
            (let ((#{x\ 1362}# #{message\ 1355}#))
              (if (not (string? #{x\ 1362}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1362}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 1354}# "~a: " "")
                "~a "
                (if (null? #{subform\ 1357}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 1363}#
                      (cons #{message\ 1355}#
                            (map (lambda (#{x\ 1364}#)
                                   (#{strip\ 176}# #{x\ 1364}# (quote (()))))
                                 (append
                                   #{subform\ 1357}#
                                   (list #{form\ 1356}#))))))
                (if #{who\ 1354}#
                  (cons #{who\ 1354}# #{tail\ 1363}#)
                  #{tail\ 1363}#))
              #f))))
      (letrec ((#{match\ 1369}#
                 (lambda (#{e\ 1370}#
                          #{p\ 1371}#
                          #{w\ 1372}#
                          #{r\ 1373}#
                          #{mod\ 1374}#)
                   (if (not #{r\ 1373}#)
                     #f
                     (if (eq? #{p\ 1371}# (quote any))
                       (cons (#{wrap\ 159}#
                               #{e\ 1370}#
                               #{w\ 1372}#
                               #{mod\ 1374}#)
                             #{r\ 1373}#)
                       (if (#{syntax-object?\ 115}# #{e\ 1370}#)
                         (#{match*\ 1368}#
                           (#{syntax-object-expression\ 116}# #{e\ 1370}#)
                           #{p\ 1371}#
                           (#{join-wraps\ 150}#
                             #{w\ 1372}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1370}#))
                           #{r\ 1373}#
                           (#{syntax-object-module\ 118}# #{e\ 1370}#))
                         (#{match*\ 1368}#
                           #{e\ 1370}#
                           #{p\ 1371}#
                           #{w\ 1372}#
                           #{r\ 1373}#
                           #{mod\ 1374}#))))))
               (#{match*\ 1368}#
                 (lambda (#{e\ 1375}#
                          #{p\ 1376}#
                          #{w\ 1377}#
                          #{r\ 1378}#
                          #{mod\ 1379}#)
                   (if (null? #{p\ 1376}#)
                     (if (null? #{e\ 1375}#) #{r\ 1378}# #f)
                     (if (pair? #{p\ 1376}#)
                       (if (pair? #{e\ 1375}#)
                         (#{match\ 1369}#
                           (car #{e\ 1375}#)
                           (car #{p\ 1376}#)
                           #{w\ 1377}#
                           (#{match\ 1369}#
                             (cdr #{e\ 1375}#)
                             (cdr #{p\ 1376}#)
                             #{w\ 1377}#
                             #{r\ 1378}#
                             #{mod\ 1379}#)
                           #{mod\ 1379}#)
                         #f)
                       (if (eq? #{p\ 1376}# (quote each-any))
                         (let ((#{l\ 1380}#
                                 (#{match-each-any\ 1366}#
                                   #{e\ 1375}#
                                   #{w\ 1377}#
                                   #{mod\ 1379}#)))
                           (if #{l\ 1380}#
                             (cons #{l\ 1380}# #{r\ 1378}#)
                             #f))
                         (let ((#{atom-key\ 1381}# (vector-ref #{p\ 1376}# 0)))
                           (if (memv #{atom-key\ 1381}# (quote (each)))
                             (if (null? #{e\ 1375}#)
                               (#{match-empty\ 1367}#
                                 (vector-ref #{p\ 1376}# 1)
                                 #{r\ 1378}#)
                               (let ((#{l\ 1382}#
                                       (#{match-each\ 1365}#
                                         #{e\ 1375}#
                                         (vector-ref #{p\ 1376}# 1)
                                         #{w\ 1377}#
                                         #{mod\ 1379}#)))
                                 (if #{l\ 1382}#
                                   (letrec ((#{collect\ 1383}#
                                              (lambda (#{l\ 1384}#)
                                                (if (null? (car #{l\ 1384}#))
                                                  #{r\ 1378}#
                                                  (cons (map car #{l\ 1384}#)
                                                        (#{collect\ 1383}#
                                                          (map cdr
                                                               #{l\ 1384}#)))))))
                                     (#{collect\ 1383}# #{l\ 1382}#))
                                   #f)))
                             (if (memv #{atom-key\ 1381}# (quote (free-id)))
                               (if (#{id?\ 131}# #{e\ 1375}#)
                                 (if (#{free-id=?\ 154}#
                                       (#{wrap\ 159}#
                                         #{e\ 1375}#
                                         #{w\ 1377}#
                                         #{mod\ 1379}#)
                                       (vector-ref #{p\ 1376}# 1))
                                   #{r\ 1378}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 1381}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 1376}# 1)
                                       (#{strip\ 176}#
                                         #{e\ 1375}#
                                         #{w\ 1377}#))
                                   #{r\ 1378}#
                                   #f)
                                 (if (memv #{atom-key\ 1381}# (quote (vector)))
                                   (if (vector? #{e\ 1375}#)
                                     (#{match\ 1369}#
                                       (vector->list #{e\ 1375}#)
                                       (vector-ref #{p\ 1376}# 1)
                                       #{w\ 1377}#
                                       #{r\ 1378}#
                                       #{mod\ 1379}#)
                                     #f)))))))))))
               (#{match-empty\ 1367}#
                 (lambda (#{p\ 1385}# #{r\ 1386}#)
                   (if (null? #{p\ 1385}#)
                     #{r\ 1386}#
                     (if (eq? #{p\ 1385}# (quote any))
                       (cons (quote ()) #{r\ 1386}#)
                       (if (pair? #{p\ 1385}#)
                         (#{match-empty\ 1367}#
                           (car #{p\ 1385}#)
                           (#{match-empty\ 1367}#
                             (cdr #{p\ 1385}#)
                             #{r\ 1386}#))
                         (if (eq? #{p\ 1385}# (quote each-any))
                           (cons (quote ()) #{r\ 1386}#)
                           (let ((#{atom-key\ 1387}#
                                   (vector-ref #{p\ 1385}# 0)))
                             (if (memv #{atom-key\ 1387}# (quote (each)))
                               (#{match-empty\ 1367}#
                                 (vector-ref #{p\ 1385}# 1)
                                 #{r\ 1386}#)
                               (if (memv #{atom-key\ 1387}#
                                         '(free-id atom))
                                 #{r\ 1386}#
                                 (if (memv #{atom-key\ 1387}# (quote (vector)))
                                   (#{match-empty\ 1367}#
                                     (vector-ref #{p\ 1385}# 1)
                                     #{r\ 1386}#)))))))))))
               (#{match-each-any\ 1366}#
                 (lambda (#{e\ 1388}# #{w\ 1389}# #{mod\ 1390}#)
                   (if (pair? #{e\ 1388}#)
                     (let ((#{l\ 1391}#
                             (#{match-each-any\ 1366}#
                               (cdr #{e\ 1388}#)
                               #{w\ 1389}#
                               #{mod\ 1390}#)))
                       (if #{l\ 1391}#
                         (cons (#{wrap\ 159}#
                                 (car #{e\ 1388}#)
                                 #{w\ 1389}#
                                 #{mod\ 1390}#)
                               #{l\ 1391}#)
                         #f))
                     (if (null? #{e\ 1388}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1388}#)
                         (#{match-each-any\ 1366}#
                           (#{syntax-object-expression\ 116}# #{e\ 1388}#)
                           (#{join-wraps\ 150}#
                             #{w\ 1389}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1388}#))
                           #{mod\ 1390}#)
                         #f)))))
               (#{match-each\ 1365}#
                 (lambda (#{e\ 1392}#
                          #{p\ 1393}#
                          #{w\ 1394}#
                          #{mod\ 1395}#)
                   (if (pair? #{e\ 1392}#)
                     (let ((#{first\ 1396}#
                             (#{match\ 1369}#
                               (car #{e\ 1392}#)
                               #{p\ 1393}#
                               #{w\ 1394}#
                               '()
                               #{mod\ 1395}#)))
                       (if #{first\ 1396}#
                         (let ((#{rest\ 1397}#
                                 (#{match-each\ 1365}#
                                   (cdr #{e\ 1392}#)
                                   #{p\ 1393}#
                                   #{w\ 1394}#
                                   #{mod\ 1395}#)))
                           (if #{rest\ 1397}#
                             (cons #{first\ 1396}# #{rest\ 1397}#)
                             #f))
                         #f))
                     (if (null? #{e\ 1392}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1392}#)
                         (#{match-each\ 1365}#
                           (#{syntax-object-expression\ 116}# #{e\ 1392}#)
                           #{p\ 1393}#
                           (#{join-wraps\ 150}#
                             #{w\ 1394}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1392}#))
                           (#{syntax-object-module\ 118}# #{e\ 1392}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 1398}# #{p\ 1399}#)
            (if (eq? #{p\ 1399}# (quote any))
              (list #{e\ 1398}#)
              (if (#{syntax-object?\ 115}# #{e\ 1398}#)
                (#{match*\ 1368}#
                  (#{syntax-object-expression\ 116}# #{e\ 1398}#)
                  #{p\ 1399}#
                  (#{syntax-object-wrap\ 117}# #{e\ 1398}#)
                  '()
                  (#{syntax-object-module\ 118}# #{e\ 1398}#))
                (#{match*\ 1368}#
                  #{e\ 1398}#
                  #{p\ 1399}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1400}#)
      ((lambda (#{tmp\ 1401}#)
         ((lambda (#{tmp\ 1402}#)
            (if #{tmp\ 1402}#
              (apply (lambda (#{_\ 1403}# #{e1\ 1404}# #{e2\ 1405}#)
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
                             (cons #{e1\ 1404}# #{e2\ 1405}#)))
                     #{tmp\ 1402}#)
              ((lambda (#{tmp\ 1407}#)
                 (if #{tmp\ 1407}#
                   (apply (lambda (#{_\ 1408}#
                                   #{out\ 1409}#
                                   #{in\ 1410}#
                                   #{e1\ 1411}#
                                   #{e2\ 1412}#)
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
                                  #{in\ 1410}#
                                  '()
                                  (list #{out\ 1409}#
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
                                              (cons #{e1\ 1411}#
                                                    #{e2\ 1412}#)))))
                          #{tmp\ 1407}#)
                   ((lambda (#{tmp\ 1414}#)
                      (if #{tmp\ 1414}#
                        (apply (lambda (#{_\ 1415}#
                                        #{out\ 1416}#
                                        #{in\ 1417}#
                                        #{e1\ 1418}#
                                        #{e2\ 1419}#)
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
                                             #{in\ 1417}#)
                                       '()
                                       (list #{out\ 1416}#
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
                                                   (cons #{e1\ 1418}#
                                                         #{e2\ 1419}#)))))
                               #{tmp\ 1414}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 1401}#)))
                    ($sc-dispatch
                      #{tmp\ 1401}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 1401}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 1401}#
            '(any () any . each-any))))
       #{x\ 1400}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1423}#)
      ((lambda (#{tmp\ 1424}#)
         ((lambda (#{tmp\ 1425}#)
            (if #{tmp\ 1425}#
              (apply (lambda (#{_\ 1426}#
                              #{k\ 1427}#
                              #{keyword\ 1428}#
                              #{pattern\ 1429}#
                              #{template\ 1430}#)
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
                                         (cons #{k\ 1427}#
                                               (map (lambda (#{tmp\ 1433}#
                                                             #{tmp\ 1432}#)
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
                                                                  #{tmp\ 1432}#)
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
                                                                  #{tmp\ 1433}#)))
                                                    #{template\ 1430}#
                                                    #{pattern\ 1429}#))))))
                     #{tmp\ 1425}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1424}#)))
          ($sc-dispatch
            #{tmp\ 1424}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 1423}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 1434}#)
      ((lambda (#{tmp\ 1435}#)
         ((lambda (#{tmp\ 1436}#)
            (if (if #{tmp\ 1436}#
                  (apply (lambda (#{let*\ 1437}#
                                  #{x\ 1438}#
                                  #{v\ 1439}#
                                  #{e1\ 1440}#
                                  #{e2\ 1441}#)
                           (and-map identifier? #{x\ 1438}#))
                         #{tmp\ 1436}#)
                  #f)
              (apply (lambda (#{let*\ 1443}#
                              #{x\ 1444}#
                              #{v\ 1445}#
                              #{e1\ 1446}#
                              #{e2\ 1447}#)
                       (letrec ((#{f\ 1448}#
                                  (lambda (#{bindings\ 1449}#)
                                    (if (null? #{bindings\ 1449}#)
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
                                                  (cons #{e1\ 1446}#
                                                        #{e2\ 1447}#)))
                                      ((lambda (#{tmp\ 1453}#)
                                         ((lambda (#{tmp\ 1454}#)
                                            (if #{tmp\ 1454}#
                                              (apply (lambda (#{body\ 1455}#
                                                              #{binding\ 1456}#)
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
                                                             (list #{binding\ 1456}#)
                                                             #{body\ 1455}#))
                                                     #{tmp\ 1454}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 1453}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1453}#
                                            '(any any))))
                                       (list (#{f\ 1448}#
                                               (cdr #{bindings\ 1449}#))
                                             (car #{bindings\ 1449}#)))))))
                         (#{f\ 1448}# (map list #{x\ 1444}# #{v\ 1445}#))))
                     #{tmp\ 1436}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1435}#)))
          ($sc-dispatch
            #{tmp\ 1435}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 1434}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 1457}#)
      ((lambda (#{tmp\ 1458}#)
         ((lambda (#{tmp\ 1459}#)
            (if #{tmp\ 1459}#
              (apply (lambda (#{_\ 1460}#
                              #{var\ 1461}#
                              #{init\ 1462}#
                              #{step\ 1463}#
                              #{e0\ 1464}#
                              #{e1\ 1465}#
                              #{c\ 1466}#)
                       ((lambda (#{tmp\ 1467}#)
                          ((lambda (#{tmp\ 1468}#)
                             (if #{tmp\ 1468}#
                               (apply (lambda (#{step\ 1469}#)
                                        ((lambda (#{tmp\ 1470}#)
                                           ((lambda (#{tmp\ 1471}#)
                                              (if #{tmp\ 1471}#
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
                                                                    #{var\ 1461}#
                                                                    #{init\ 1462}#)
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
                                                                           #{e0\ 1464}#)
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
                                                                             #{c\ 1466}#
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
                                                                                         #{step\ 1469}#)))))))
                                                       #{tmp\ 1471}#)
                                                ((lambda (#{tmp\ 1476}#)
                                                   (if #{tmp\ 1476}#
                                                     (apply (lambda (#{e1\ 1477}#
                                                                     #{e2\ 1478}#)
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
                                                                         #{var\ 1461}#
                                                                         #{init\ 1462}#)
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
                                                                          #{e0\ 1464}#
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
                                                                                (cons #{e1\ 1477}#
                                                                                      #{e2\ 1478}#))
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
                                                                                  #{c\ 1466}#
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
                                                                                              #{step\ 1469}#)))))))
                                                            #{tmp\ 1476}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1470}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1470}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 1470}#
                                              '())))
                                         #{e1\ 1465}#))
                                      #{tmp\ 1468}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 1467}#)))
                           ($sc-dispatch #{tmp\ 1467}# (quote each-any))))
                        (map (lambda (#{v\ 1485}# #{s\ 1486}#)
                               ((lambda (#{tmp\ 1487}#)
                                  ((lambda (#{tmp\ 1488}#)
                                     (if #{tmp\ 1488}#
                                       (apply (lambda () #{v\ 1485}#)
                                              #{tmp\ 1488}#)
                                       ((lambda (#{tmp\ 1489}#)
                                          (if #{tmp\ 1489}#
                                            (apply (lambda (#{e\ 1490}#)
                                                     #{e\ 1490}#)
                                                   #{tmp\ 1489}#)
                                            ((lambda (#{_\ 1491}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 1457}#
                                                 #{s\ 1486}#))
                                             #{tmp\ 1487}#)))
                                        ($sc-dispatch
                                          #{tmp\ 1487}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 1487}# (quote ()))))
                                #{s\ 1486}#))
                             #{var\ 1461}#
                             #{step\ 1463}#)))
                     #{tmp\ 1459}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1458}#)))
          ($sc-dispatch
            #{tmp\ 1458}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 1457}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 1494}#
               (lambda (#{x\ 1498}# #{y\ 1499}#)
                 ((lambda (#{tmp\ 1500}#)
                    ((lambda (#{tmp\ 1501}#)
                       (if #{tmp\ 1501}#
                         (apply (lambda (#{x\ 1502}# #{y\ 1503}#)
                                  ((lambda (#{tmp\ 1504}#)
                                     ((lambda (#{tmp\ 1505}#)
                                        (if #{tmp\ 1505}#
                                          (apply (lambda (#{dy\ 1506}#)
                                                   ((lambda (#{tmp\ 1507}#)
                                                      ((lambda (#{tmp\ 1508}#)
                                                         (if #{tmp\ 1508}#
                                                           (apply (lambda (#{dx\ 1509}#)
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
                                                                          (cons #{dx\ 1509}#
                                                                                #{dy\ 1506}#)))
                                                                  #{tmp\ 1508}#)
                                                           ((lambda (#{_\ 1510}#)
                                                              (if (null? #{dy\ 1506}#)
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
                                                                      #{x\ 1502}#)
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
                                                                      #{x\ 1502}#
                                                                      #{y\ 1503}#)))
                                                            #{tmp\ 1507}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1507}#
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
                                                    #{x\ 1502}#))
                                                 #{tmp\ 1505}#)
                                          ((lambda (#{tmp\ 1511}#)
                                             (if #{tmp\ 1511}#
                                               (apply (lambda (#{stuff\ 1512}#)
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
                                                              (cons #{x\ 1502}#
                                                                    #{stuff\ 1512}#)))
                                                      #{tmp\ 1511}#)
                                               ((lambda (#{else\ 1513}#)
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
                                                        #{x\ 1502}#
                                                        #{y\ 1503}#))
                                                #{tmp\ 1504}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1504}#
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
                                        #{tmp\ 1504}#
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
                                   #{y\ 1503}#))
                                #{tmp\ 1501}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1500}#)))
                     ($sc-dispatch #{tmp\ 1500}# (quote (any any)))))
                  (list #{x\ 1498}# #{y\ 1499}#))))
             (#{quasiappend\ 1495}#
               (lambda (#{x\ 1514}# #{y\ 1515}#)
                 ((lambda (#{tmp\ 1516}#)
                    ((lambda (#{tmp\ 1517}#)
                       (if #{tmp\ 1517}#
                         (apply (lambda (#{x\ 1518}# #{y\ 1519}#)
                                  ((lambda (#{tmp\ 1520}#)
                                     ((lambda (#{tmp\ 1521}#)
                                        (if #{tmp\ 1521}#
                                          (apply (lambda () #{x\ 1518}#)
                                                 #{tmp\ 1521}#)
                                          ((lambda (#{_\ 1522}#)
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
                                                   #{x\ 1518}#
                                                   #{y\ 1519}#))
                                           #{tmp\ 1520}#)))
                                      ($sc-dispatch
                                        #{tmp\ 1520}#
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
                                   #{y\ 1519}#))
                                #{tmp\ 1517}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1516}#)))
                     ($sc-dispatch #{tmp\ 1516}# (quote (any any)))))
                  (list #{x\ 1514}# #{y\ 1515}#))))
             (#{quasivector\ 1496}#
               (lambda (#{x\ 1523}#)
                 ((lambda (#{tmp\ 1524}#)
                    ((lambda (#{x\ 1525}#)
                       ((lambda (#{tmp\ 1526}#)
                          ((lambda (#{tmp\ 1527}#)
                             (if #{tmp\ 1527}#
                               (apply (lambda (#{x\ 1528}#)
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
                                              (list->vector #{x\ 1528}#)))
                                      #{tmp\ 1527}#)
                               ((lambda (#{tmp\ 1530}#)
                                  (if #{tmp\ 1530}#
                                    (apply (lambda (#{x\ 1531}#)
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
                                                   #{x\ 1531}#))
                                           #{tmp\ 1530}#)
                                    ((lambda (#{_\ 1533}#)
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
                                             #{x\ 1525}#))
                                     #{tmp\ 1526}#)))
                                ($sc-dispatch
                                  #{tmp\ 1526}#
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
                             #{tmp\ 1526}#
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
                        #{x\ 1525}#))
                     #{tmp\ 1524}#))
                  #{x\ 1523}#)))
             (#{quasi\ 1497}#
               (lambda (#{p\ 1534}# #{lev\ 1535}#)
                 ((lambda (#{tmp\ 1536}#)
                    ((lambda (#{tmp\ 1537}#)
                       (if #{tmp\ 1537}#
                         (apply (lambda (#{p\ 1538}#)
                                  (if (= #{lev\ 1535}# 0)
                                    #{p\ 1538}#
                                    (#{quasicons\ 1494}#
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
                                      (#{quasi\ 1497}#
                                        (list #{p\ 1538}#)
                                        (- #{lev\ 1535}# 1)))))
                                #{tmp\ 1537}#)
                         ((lambda (#{tmp\ 1539}#)
                            (if (if #{tmp\ 1539}#
                                  (apply (lambda (#{args\ 1540}#)
                                           (= #{lev\ 1535}# 0))
                                         #{tmp\ 1539}#)
                                  #f)
                              (apply (lambda (#{args\ 1541}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 1534}#
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
                                               #{args\ 1541}#)))
                                     #{tmp\ 1539}#)
                              ((lambda (#{tmp\ 1542}#)
                                 (if #{tmp\ 1542}#
                                   (apply (lambda (#{p\ 1543}# #{q\ 1544}#)
                                            (if (= #{lev\ 1535}# 0)
                                              (#{quasiappend\ 1495}#
                                                #{p\ 1543}#
                                                (#{quasi\ 1497}#
                                                  #{q\ 1544}#
                                                  #{lev\ 1535}#))
                                              (#{quasicons\ 1494}#
                                                (#{quasicons\ 1494}#
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
                                                  (#{quasi\ 1497}#
                                                    (list #{p\ 1543}#)
                                                    (- #{lev\ 1535}# 1)))
                                                (#{quasi\ 1497}#
                                                  #{q\ 1544}#
                                                  #{lev\ 1535}#))))
                                          #{tmp\ 1542}#)
                                   ((lambda (#{tmp\ 1545}#)
                                      (if (if #{tmp\ 1545}#
                                            (apply (lambda (#{args\ 1546}#
                                                            #{q\ 1547}#)
                                                     (= #{lev\ 1535}# 0))
                                                   #{tmp\ 1545}#)
                                            #f)
                                        (apply (lambda (#{args\ 1548}#
                                                        #{q\ 1549}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 1534}#
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
                                                         #{args\ 1548}#)))
                                               #{tmp\ 1545}#)
                                        ((lambda (#{tmp\ 1550}#)
                                           (if #{tmp\ 1550}#
                                             (apply (lambda (#{p\ 1551}#)
                                                      (#{quasicons\ 1494}#
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
                                                        (#{quasi\ 1497}#
                                                          (list #{p\ 1551}#)
                                                          (+ #{lev\ 1535}#
                                                             1))))
                                                    #{tmp\ 1550}#)
                                             ((lambda (#{tmp\ 1552}#)
                                                (if #{tmp\ 1552}#
                                                  (apply (lambda (#{p\ 1553}#
                                                                  #{q\ 1554}#)
                                                           (#{quasicons\ 1494}#
                                                             (#{quasi\ 1497}#
                                                               #{p\ 1553}#
                                                               #{lev\ 1535}#)
                                                             (#{quasi\ 1497}#
                                                               #{q\ 1554}#
                                                               #{lev\ 1535}#)))
                                                         #{tmp\ 1552}#)
                                                  ((lambda (#{tmp\ 1555}#)
                                                     (if #{tmp\ 1555}#
                                                       (apply (lambda (#{x\ 1556}#)
                                                                (#{quasivector\ 1496}#
                                                                  (#{quasi\ 1497}#
                                                                    #{x\ 1556}#
                                                                    #{lev\ 1535}#)))
                                                              #{tmp\ 1555}#)
                                                       ((lambda (#{p\ 1558}#)
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
                                                                #{p\ 1558}#))
                                                        #{tmp\ 1536}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 1536}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 1536}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 1536}#
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
                                      #{tmp\ 1536}#
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
                                 #{tmp\ 1536}#
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
                            #{tmp\ 1536}#
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
                       #{tmp\ 1536}#
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
                  #{p\ 1534}#))))
      (lambda (#{x\ 1559}#)
        ((lambda (#{tmp\ 1560}#)
           ((lambda (#{tmp\ 1561}#)
              (if #{tmp\ 1561}#
                (apply (lambda (#{_\ 1562}# #{e\ 1563}#)
                         (#{quasi\ 1497}# #{e\ 1563}# 0))
                       #{tmp\ 1561}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1560}#)))
            ($sc-dispatch #{tmp\ 1560}# (quote (any any)))))
         #{x\ 1559}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1564}#)
      (letrec ((#{read-file\ 1565}#
                 (lambda (#{fn\ 1566}# #{k\ 1567}#)
                   (let ((#{p\ 1568}# (open-input-file #{fn\ 1566}#)))
                     (letrec ((#{f\ 1569}#
                                (lambda (#{x\ 1570}#)
                                  (if (eof-object? #{x\ 1570}#)
                                    (begin
                                      (close-input-port #{p\ 1568}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 1567}#
                                            #{x\ 1570}#)
                                          (#{f\ 1569}# (read #{p\ 1568}#)))))))
                       (#{f\ 1569}# (read #{p\ 1568}#)))))))
        ((lambda (#{tmp\ 1571}#)
           ((lambda (#{tmp\ 1572}#)
              (if #{tmp\ 1572}#
                (apply (lambda (#{k\ 1573}# #{filename\ 1574}#)
                         (let ((#{fn\ 1575}#
                                 (syntax->datum #{filename\ 1574}#)))
                           ((lambda (#{tmp\ 1576}#)
                              ((lambda (#{tmp\ 1577}#)
                                 (if #{tmp\ 1577}#
                                   (apply (lambda (#{exp\ 1578}#)
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
                                                  #{exp\ 1578}#))
                                          #{tmp\ 1577}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 1576}#)))
                               ($sc-dispatch #{tmp\ 1576}# (quote each-any))))
                            (#{read-file\ 1565}# #{fn\ 1575}# #{k\ 1573}#))))
                       #{tmp\ 1572}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1571}#)))
            ($sc-dispatch #{tmp\ 1571}# (quote (any any)))))
         #{x\ 1564}#)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1580}#)
      ((lambda (#{tmp\ 1581}#)
         ((lambda (#{tmp\ 1582}#)
            (if #{tmp\ 1582}#
              (apply (lambda (#{_\ 1583}# #{e\ 1584}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 1580}#))
                     #{tmp\ 1582}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1581}#)))
          ($sc-dispatch #{tmp\ 1581}# (quote (any any)))))
       #{x\ 1580}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1585}#)
      ((lambda (#{tmp\ 1586}#)
         ((lambda (#{tmp\ 1587}#)
            (if #{tmp\ 1587}#
              (apply (lambda (#{_\ 1588}# #{e\ 1589}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 1585}#))
                     #{tmp\ 1587}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1586}#)))
          ($sc-dispatch #{tmp\ 1586}# (quote (any any)))))
       #{x\ 1585}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 1590}#)
      ((lambda (#{tmp\ 1591}#)
         ((lambda (#{tmp\ 1592}#)
            (if #{tmp\ 1592}#
              (apply (lambda (#{_\ 1593}#
                              #{e\ 1594}#
                              #{m1\ 1595}#
                              #{m2\ 1596}#)
                       ((lambda (#{tmp\ 1597}#)
                          ((lambda (#{body\ 1598}#)
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
                                               #{e\ 1594}#))
                                   #{body\ 1598}#))
                           #{tmp\ 1597}#))
                        (letrec ((#{f\ 1599}#
                                   (lambda (#{clause\ 1600}# #{clauses\ 1601}#)
                                     (if (null? #{clauses\ 1601}#)
                                       ((lambda (#{tmp\ 1603}#)
                                          ((lambda (#{tmp\ 1604}#)
                                             (if #{tmp\ 1604}#
                                               (apply (lambda (#{e1\ 1605}#
                                                               #{e2\ 1606}#)
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
                                                              (cons #{e1\ 1605}#
                                                                    #{e2\ 1606}#)))
                                                      #{tmp\ 1604}#)
                                               ((lambda (#{tmp\ 1608}#)
                                                  (if #{tmp\ 1608}#
                                                    (apply (lambda (#{k\ 1609}#
                                                                    #{e1\ 1610}#
                                                                    #{e2\ 1611}#)
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
                                                                               #{k\ 1609}#))
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
                                                                         (cons #{e1\ 1610}#
                                                                               #{e2\ 1611}#))))
                                                           #{tmp\ 1608}#)
                                                    ((lambda (#{_\ 1614}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 1590}#
                                                         #{clause\ 1600}#))
                                                     #{tmp\ 1603}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 1603}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 1603}#
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
                                        #{clause\ 1600}#)
                                       ((lambda (#{tmp\ 1615}#)
                                          ((lambda (#{rest\ 1616}#)
                                             ((lambda (#{tmp\ 1617}#)
                                                ((lambda (#{tmp\ 1618}#)
                                                   (if #{tmp\ 1618}#
                                                     (apply (lambda (#{k\ 1619}#
                                                                     #{e1\ 1620}#
                                                                     #{e2\ 1621}#)
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
                                                                                #{k\ 1619}#))
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
                                                                          (cons #{e1\ 1620}#
                                                                                #{e2\ 1621}#))
                                                                    #{rest\ 1616}#))
                                                            #{tmp\ 1618}#)
                                                     ((lambda (#{_\ 1624}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 1590}#
                                                          #{clause\ 1600}#))
                                                      #{tmp\ 1617}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1617}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 1600}#))
                                           #{tmp\ 1615}#))
                                        (#{f\ 1599}#
                                          (car #{clauses\ 1601}#)
                                          (cdr #{clauses\ 1601}#)))))))
                          (#{f\ 1599}# #{m1\ 1595}# #{m2\ 1596}#))))
                     #{tmp\ 1592}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1591}#)))
          ($sc-dispatch
            #{tmp\ 1591}#
            '(any any any . each-any))))
       #{x\ 1590}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1625}#)
      ((lambda (#{tmp\ 1626}#)
         ((lambda (#{tmp\ 1627}#)
            (if #{tmp\ 1627}#
              (apply (lambda (#{_\ 1628}# #{e\ 1629}#)
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
                                               #{e\ 1629}#))
                                   (list (cons #{_\ 1628}#
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
                                               (cons #{e\ 1629}#
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
                     #{tmp\ 1627}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1626}#)))
          ($sc-dispatch #{tmp\ 1626}# (quote (any any)))))
       #{x\ 1625}#))))

