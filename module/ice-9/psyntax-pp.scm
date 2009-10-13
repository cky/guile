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
                   (let ((#{nkw\ 790}#
                           (map (lambda (#{x\ 791}#)
                                  (list 'list
                                        (car #{x\ 791}#)
                                        (letrec ((#{lp\ 792}#
                                                   (lambda (#{vars\ 793}#
                                                            #{i\ 794}#)
                                                     (if (null? #{vars\ 793}#)
                                                       (error "bad kwarg"
                                                              #{x\ 791}#)
                                                       (if (eq? (cadr #{x\ 791}#)
                                                                (car #{vars\ 793}#))
                                                         #{i\ 794}#
                                                         (#{lp\ 792}#
                                                           (cdr #{vars\ 793}#)
                                                           (1+ #{i\ 794}#)))))))
                                          (#{lp\ 792}# #{vars\ 785}# 0))
                                        (list 'lambda
                                              '()
                                              (caddr #{x\ 791}#))))
                                #{kw\ 784}#)))
                     (#{decorate-source\ 94}#
                       (cons (list (cons '(@@ (ice-9 optargs)
                                              parse-lambda-case)
                                         (cons (list 'list
                                                     (length #{req\ 781}#)
                                                     (length #{opt\ 782}#)
                                                     (if #{rest\ 783}# #t #f)
                                                     #{nkw\ 790}#
                                                     (if #{predicate\ 786}#
                                                       (error "not yet implemented")
                                                       #f))
                                               '(%%args)))
                                   '=>
                                   (list 'lambda
                                         #{vars\ 785}#
                                         #{body\ 787}#))
                             (let ((#{t\ 795}# #{else-case\ 788}#))
                               (if #{t\ 795}#
                                 #{t\ 795}#
                                 '((%%args
                                     (error "wrong number of arguments"
                                            %%args))))))
                       #{src\ 780}#))))))
           (#{build-case-lambda\ 106}#
             (lambda (#{src\ 796}# #{docstring\ 797}# #{body\ 798}#)
               (let ((#{atom-key\ 799}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 799}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 796}#
                    (if #{docstring\ 797}#
                      (list (cons (quote documentation) #{docstring\ 797}#))
                      '())
                    #{body\ 798}#)
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 797}#
                                     (list #{docstring\ 797}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 798}#)))))
                     #{src\ 796}#)))))
           (#{build-simple-lambda\ 105}#
             (lambda (#{src\ 800}#
                      #{req\ 801}#
                      #{rest\ 802}#
                      #{vars\ 803}#
                      #{docstring\ 804}#
                      #{exp\ 805}#)
               (let ((#{atom-key\ 806}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 806}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 800}#
                    (if #{docstring\ 804}#
                      (list (cons (quote documentation) #{docstring\ 804}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 800}#
                     #{req\ 801}#
                     #f
                     #{rest\ 802}#
                     #f
                     #{vars\ 803}#
                     #f
                     #{exp\ 805}#
                     #f))
                   (#{decorate-source\ 94}#
                     (cons 'lambda
                           (cons (if #{rest\ 802}#
                                   (apply cons* #{vars\ 803}#)
                                   #{vars\ 803}#)
                                 (append
                                   (if #{docstring\ 804}#
                                     (list #{docstring\ 804}#)
                                     '())
                                   (list #{exp\ 805}#))))
                     #{src\ 800}#)))))
           (#{build-global-definition\ 104}#
             (lambda (#{source\ 807}# #{var\ 808}# #{exp\ 809}#)
               (let ((#{atom-key\ 810}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 810}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 103}#
                       #{var\ 808}#
                       #{exp\ 809}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 807}#
                      #{var\ 808}#
                      #{exp\ 809}#))
                   (#{decorate-source\ 94}#
                     (list (quote define) #{var\ 808}# #{exp\ 809}#)
                     #{source\ 807}#)))))
           (#{maybe-name-value!\ 103}#
             (lambda (#{name\ 811}# #{val\ 812}#)
               (if ((@ (language tree-il) lambda?) #{val\ 812}#)
                 (let ((#{meta\ 813}#
                         ((@ (language tree-il) lambda-meta) #{val\ 812}#)))
                   (if (not (assq (quote name) #{meta\ 813}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 812}#
                      (acons (quote name) #{name\ 811}# #{meta\ 813}#)))))))
           (#{build-global-assignment\ 102}#
             (lambda (#{source\ 814}#
                      #{var\ 815}#
                      #{exp\ 816}#
                      #{mod\ 817}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 817}#
                 #{var\ 815}#
                 (lambda (#{mod\ 818}# #{var\ 819}# #{public?\ 820}#)
                   (let ((#{atom-key\ 821}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 821}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 814}#
                        #{mod\ 818}#
                        #{var\ 819}#
                        #{public?\ 820}#
                        #{exp\ 816}#)
                       (#{decorate-source\ 94}#
                         (list 'set!
                               (list (if #{public?\ 820}# (quote @) (quote @@))
                                     #{mod\ 818}#
                                     #{var\ 819}#)
                               #{exp\ 816}#)
                         #{source\ 814}#))))
                 (lambda (#{var\ 822}#)
                   (let ((#{atom-key\ 823}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 823}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 814}#
                        #{var\ 822}#
                        #{exp\ 816}#)
                       (#{decorate-source\ 94}#
                         (list (quote set!) #{var\ 822}# #{exp\ 816}#)
                         #{source\ 814}#)))))))
           (#{build-global-reference\ 101}#
             (lambda (#{source\ 824}# #{var\ 825}# #{mod\ 826}#)
               (#{analyze-variable\ 100}#
                 #{mod\ 826}#
                 #{var\ 825}#
                 (lambda (#{mod\ 827}# #{var\ 828}# #{public?\ 829}#)
                   (let ((#{atom-key\ 830}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 830}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 824}#
                        #{mod\ 827}#
                        #{var\ 828}#
                        #{public?\ 829}#)
                       (#{decorate-source\ 94}#
                         (list (if #{public?\ 829}# (quote @) (quote @@))
                               #{mod\ 827}#
                               #{var\ 828}#)
                         #{source\ 824}#))))
                 (lambda (#{var\ 831}#)
                   (let ((#{atom-key\ 832}# (fluid-ref #{*mode*\ 85}#)))
                     (if (memv #{atom-key\ 832}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 824}#
                        #{var\ 831}#)
                       (#{decorate-source\ 94}#
                         #{var\ 831}#
                         #{source\ 824}#)))))))
           (#{analyze-variable\ 100}#
             (lambda (#{mod\ 833}#
                      #{var\ 834}#
                      #{modref-cont\ 835}#
                      #{bare-cont\ 836}#)
               (if (not #{mod\ 833}#)
                 (#{bare-cont\ 836}# #{var\ 834}#)
                 (let ((#{kind\ 837}# (car #{mod\ 833}#))
                       (#{mod\ 838}# (cdr #{mod\ 833}#)))
                   (if (memv #{kind\ 837}# (quote (public)))
                     (#{modref-cont\ 835}#
                       #{mod\ 838}#
                       #{var\ 834}#
                       #t)
                     (if (memv #{kind\ 837}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 838}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 835}#
                           #{mod\ 838}#
                           #{var\ 834}#
                           #f)
                         (#{bare-cont\ 836}# #{var\ 834}#))
                       (if (memv #{kind\ 837}# (quote (bare)))
                         (#{bare-cont\ 836}# #{var\ 834}#)
                         (if (memv #{kind\ 837}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 838}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 838}#)
                                   #{var\ 834}#)
                                 #f)
                             (#{modref-cont\ 835}#
                               #{mod\ 838}#
                               #{var\ 834}#
                               #f)
                             (#{bare-cont\ 836}# #{var\ 834}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 834}#
                             #{mod\ 838}#)))))))))
           (#{build-lexical-assignment\ 99}#
             (lambda (#{source\ 839}#
                      #{name\ 840}#
                      #{var\ 841}#
                      #{exp\ 842}#)
               (let ((#{atom-key\ 843}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 843}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 839}#
                    #{name\ 840}#
                    #{var\ 841}#
                    #{exp\ 842}#)
                   (#{decorate-source\ 94}#
                     (list (quote set!) #{var\ 841}# #{exp\ 842}#)
                     #{source\ 839}#)))))
           (#{build-lexical-reference\ 98}#
             (lambda (#{type\ 844}#
                      #{source\ 845}#
                      #{name\ 846}#
                      #{var\ 847}#)
               (let ((#{atom-key\ 848}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 848}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 845}#
                    #{name\ 846}#
                    #{var\ 847}#)
                   (#{decorate-source\ 94}#
                     #{var\ 847}#
                     #{source\ 845}#)))))
           (#{build-conditional\ 97}#
             (lambda (#{source\ 849}#
                      #{test-exp\ 850}#
                      #{then-exp\ 851}#
                      #{else-exp\ 852}#)
               (let ((#{atom-key\ 853}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 853}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 849}#
                    #{test-exp\ 850}#
                    #{then-exp\ 851}#
                    #{else-exp\ 852}#)
                   (#{decorate-source\ 94}#
                     (if (equal? #{else-exp\ 852}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 850}#
                             #{then-exp\ 851}#)
                       (list 'if
                             #{test-exp\ 850}#
                             #{then-exp\ 851}#
                             #{else-exp\ 852}#))
                     #{source\ 849}#)))))
           (#{build-application\ 96}#
             (lambda (#{source\ 854}#
                      #{fun-exp\ 855}#
                      #{arg-exps\ 856}#)
               (let ((#{atom-key\ 857}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 857}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 854}#
                    #{fun-exp\ 855}#
                    #{arg-exps\ 856}#)
                   (#{decorate-source\ 94}#
                     (cons #{fun-exp\ 855}# #{arg-exps\ 856}#)
                     #{source\ 854}#)))))
           (#{build-void\ 95}#
             (lambda (#{source\ 858}#)
               (let ((#{atom-key\ 859}# (fluid-ref #{*mode*\ 85}#)))
                 (if (memv #{atom-key\ 859}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 858}#)
                   (#{decorate-source\ 94}#
                     '(if #f #f)
                     #{source\ 858}#)))))
           (#{decorate-source\ 94}#
             (lambda (#{e\ 860}# #{s\ 861}#)
               (begin
                 (if (if (pair? #{e\ 860}#) #{s\ 861}# #f)
                   (set-source-properties! #{e\ 860}# #{s\ 861}#))
                 #{e\ 860}#)))
           (#{get-global-definition-hook\ 93}#
             (lambda (#{symbol\ 862}# #{module\ 863}#)
               (begin
                 (if (if (not #{module\ 863}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 862}#))
                 (let ((#{v\ 864}# (module-variable
                                     (if #{module\ 863}#
                                       (resolve-module (cdr #{module\ 863}#))
                                       (current-module))
                                     #{symbol\ 862}#)))
                   (if #{v\ 864}#
                     (if (variable-bound? #{v\ 864}#)
                       (let ((#{val\ 865}# (variable-ref #{v\ 864}#)))
                         (if (macro? #{val\ 865}#)
                           (if (syncase-macro-type #{val\ 865}#)
                             (cons (syncase-macro-type #{val\ 865}#)
                                   (syncase-macro-binding #{val\ 865}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 92}#
             (lambda (#{symbol\ 866}# #{type\ 867}# #{val\ 868}#)
               (let ((#{existing\ 869}#
                       (let ((#{v\ 870}# (module-variable
                                           (current-module)
                                           #{symbol\ 866}#)))
                         (if #{v\ 870}#
                           (if (variable-bound? #{v\ 870}#)
                             (let ((#{val\ 871}# (variable-ref #{v\ 870}#)))
                               (if (macro? #{val\ 871}#)
                                 (if (not (syncase-macro-type #{val\ 871}#))
                                   #{val\ 871}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 866}#
                   (if #{existing\ 869}#
                     (make-extended-syncase-macro
                       #{existing\ 869}#
                       #{type\ 867}#
                       #{val\ 868}#)
                     (make-syncase-macro #{type\ 867}# #{val\ 868}#))))))
           (#{local-eval-hook\ 91}#
             (lambda (#{x\ 872}# #{mod\ 873}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 874}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 874}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 872}#)
                           #{x\ 872}#))))))
           (#{top-level-eval-hook\ 90}#
             (lambda (#{x\ 875}# #{mod\ 876}#)
               (primitive-eval
                 (list #{noexpand\ 84}#
                       (let ((#{atom-key\ 877}# (fluid-ref #{*mode*\ 85}#)))
                         (if (memv #{atom-key\ 877}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 875}#)
                           #{x\ 875}#))))))
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
        (lambda (#{e\ 878}#
                 #{r\ 879}#
                 #{w\ 880}#
                 #{s\ 881}#
                 #{mod\ 882}#)
          ((lambda (#{tmp\ 883}#)
             ((lambda (#{tmp\ 884}#)
                (if (if #{tmp\ 884}#
                      (apply (lambda (#{_\ 885}#
                                      #{var\ 886}#
                                      #{val\ 887}#
                                      #{e1\ 888}#
                                      #{e2\ 889}#)
                               (#{valid-bound-ids?\ 156}# #{var\ 886}#))
                             #{tmp\ 884}#)
                      #f)
                  (apply (lambda (#{_\ 891}#
                                  #{var\ 892}#
                                  #{val\ 893}#
                                  #{e1\ 894}#
                                  #{e2\ 895}#)
                           (let ((#{names\ 896}#
                                   (map (lambda (#{x\ 897}#)
                                          (#{id-var-name\ 153}#
                                            #{x\ 897}#
                                            #{w\ 880}#))
                                        #{var\ 892}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 899}# #{n\ 900}#)
                                   (let ((#{atom-key\ 901}#
                                           (#{binding-type\ 123}#
                                             (#{lookup\ 128}#
                                               #{n\ 900}#
                                               #{r\ 879}#
                                               #{mod\ 882}#))))
                                     (if (memv #{atom-key\ 901}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 878}#
                                         (#{source-wrap\ 160}#
                                           #{id\ 899}#
                                           #{w\ 880}#
                                           #{s\ 881}#
                                           #{mod\ 882}#)))))
                                 #{var\ 892}#
                                 #{names\ 896}#)
                               (#{chi-body\ 171}#
                                 (cons #{e1\ 894}# #{e2\ 895}#)
                                 (#{source-wrap\ 160}#
                                   #{e\ 878}#
                                   #{w\ 880}#
                                   #{s\ 881}#
                                   #{mod\ 882}#)
                                 (#{extend-env\ 125}#
                                   #{names\ 896}#
                                   (let ((#{trans-r\ 904}#
                                           (#{macros-only-env\ 127}#
                                             #{r\ 879}#)))
                                     (map (lambda (#{x\ 905}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 173}#
                                                    (#{chi\ 167}#
                                                      #{x\ 905}#
                                                      #{trans-r\ 904}#
                                                      #{w\ 880}#
                                                      #{mod\ 882}#)
                                                    #{mod\ 882}#)))
                                          #{val\ 893}#))
                                   #{r\ 879}#)
                                 #{w\ 880}#
                                 #{mod\ 882}#))))
                         #{tmp\ 884}#)
                  ((lambda (#{_\ 907}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 878}#
                         #{w\ 880}#
                         #{s\ 881}#
                         #{mod\ 882}#)))
                   #{tmp\ 883}#)))
              ($sc-dispatch
                #{tmp\ 883}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 878}#)))
      (#{global-extend\ 129}#
        'core
        'quote
        (lambda (#{e\ 908}#
                 #{r\ 909}#
                 #{w\ 910}#
                 #{s\ 911}#
                 #{mod\ 912}#)
          ((lambda (#{tmp\ 913}#)
             ((lambda (#{tmp\ 914}#)
                (if #{tmp\ 914}#
                  (apply (lambda (#{_\ 915}# #{e\ 916}#)
                           (#{build-data\ 109}#
                             #{s\ 911}#
                             (#{strip\ 176}# #{e\ 916}# #{w\ 910}#)))
                         #{tmp\ 914}#)
                  ((lambda (#{_\ 917}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 160}#
                         #{e\ 908}#
                         #{w\ 910}#
                         #{s\ 911}#
                         #{mod\ 912}#)))
                   #{tmp\ 913}#)))
              ($sc-dispatch #{tmp\ 913}# (quote (any any)))))
           #{e\ 908}#)))
      (#{global-extend\ 129}#
        'core
        'syntax
        (letrec ((#{regen\ 925}#
                   (lambda (#{x\ 926}#)
                     (let ((#{atom-key\ 927}# (car #{x\ 926}#)))
                       (if (memv #{atom-key\ 927}# (quote (ref)))
                         (#{build-lexical-reference\ 98}#
                           'value
                           #f
                           (cadr #{x\ 926}#)
                           (cadr #{x\ 926}#))
                         (if (memv #{atom-key\ 927}# (quote (primitive)))
                           (#{build-primref\ 108}# #f (cadr #{x\ 926}#))
                           (if (memv #{atom-key\ 927}# (quote (quote)))
                             (#{build-data\ 109}# #f (cadr #{x\ 926}#))
                             (if (memv #{atom-key\ 927}# (quote (lambda)))
                               (if (list? (cadr #{x\ 926}#))
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (cadr #{x\ 926}#)
                                   #f
                                   (cadr #{x\ 926}#)
                                   #f
                                   (#{regen\ 925}# (caddr #{x\ 926}#)))
                                 (error "how did we get here" #{x\ 926}#))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-primref\ 108}# #f (car #{x\ 926}#))
                                 (map #{regen\ 925}# (cdr #{x\ 926}#))))))))))
                 (#{gen-vector\ 924}#
                   (lambda (#{x\ 928}#)
                     (if (eq? (car #{x\ 928}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 928}#))
                       (if (eq? (car #{x\ 928}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 928}#)))
                         (list (quote list->vector) #{x\ 928}#)))))
                 (#{gen-append\ 923}#
                   (lambda (#{x\ 929}# #{y\ 930}#)
                     (if (equal? #{y\ 930}# (quote (quote ())))
                       #{x\ 929}#
                       (list (quote append) #{x\ 929}# #{y\ 930}#))))
                 (#{gen-cons\ 922}#
                   (lambda (#{x\ 931}# #{y\ 932}#)
                     (let ((#{atom-key\ 933}# (car #{y\ 932}#)))
                       (if (memv #{atom-key\ 933}# (quote (quote)))
                         (if (eq? (car #{x\ 931}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 931}#) (cadr #{y\ 932}#)))
                           (if (eq? (cadr #{y\ 932}#) (quote ()))
                             (list (quote list) #{x\ 931}#)
                             (list (quote cons) #{x\ 931}# #{y\ 932}#)))
                         (if (memv #{atom-key\ 933}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 931}# (cdr #{y\ 932}#)))
                           (list (quote cons) #{x\ 931}# #{y\ 932}#))))))
                 (#{gen-map\ 921}#
                   (lambda (#{e\ 934}# #{map-env\ 935}#)
                     (let ((#{formals\ 936}# (map cdr #{map-env\ 935}#))
                           (#{actuals\ 937}#
                             (map (lambda (#{x\ 938}#)
                                    (list (quote ref) (car #{x\ 938}#)))
                                  #{map-env\ 935}#)))
                       (if (eq? (car #{e\ 934}#) (quote ref))
                         (car #{actuals\ 937}#)
                         (if (and-map
                               (lambda (#{x\ 939}#)
                                 (if (eq? (car #{x\ 939}#) (quote ref))
                                   (memq (cadr #{x\ 939}#) #{formals\ 936}#)
                                   #f))
                               (cdr #{e\ 934}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 934}#))
                                       (map (let ((#{r\ 940}# (map cons
                                                                   #{formals\ 936}#
                                                                   #{actuals\ 937}#)))
                                              (lambda (#{x\ 941}#)
                                                (cdr (assq (cadr #{x\ 941}#)
                                                           #{r\ 940}#))))
                                            (cdr #{e\ 934}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 936}#
                                             #{e\ 934}#)
                                       #{actuals\ 937}#)))))))
                 (#{gen-mappend\ 920}#
                   (lambda (#{e\ 942}# #{map-env\ 943}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 921}# #{e\ 942}# #{map-env\ 943}#))))
                 (#{gen-ref\ 919}#
                   (lambda (#{src\ 944}#
                            #{var\ 945}#
                            #{level\ 946}#
                            #{maps\ 947}#)
                     (if (#{fx=\ 88}# #{level\ 946}# 0)
                       (values #{var\ 945}# #{maps\ 947}#)
                       (if (null? #{maps\ 947}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 944}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 919}#
                               #{src\ 944}#
                               #{var\ 945}#
                               (#{fx-\ 87}# #{level\ 946}# 1)
                               (cdr #{maps\ 947}#)))
                           (lambda (#{outer-var\ 948}# #{outer-maps\ 949}#)
                             (let ((#{b\ 950}# (assq #{outer-var\ 948}#
                                                     (car #{maps\ 947}#))))
                               (if #{b\ 950}#
                                 (values (cdr #{b\ 950}#) #{maps\ 947}#)
                                 (let ((#{inner-var\ 951}#
                                         (#{gen-var\ 177}# (quote tmp))))
                                   (values
                                     #{inner-var\ 951}#
                                     (cons (cons (cons #{outer-var\ 948}#
                                                       #{inner-var\ 951}#)
                                                 (car #{maps\ 947}#))
                                           #{outer-maps\ 949}#)))))))))))
                 (#{gen-syntax\ 918}#
                   (lambda (#{src\ 952}#
                            #{e\ 953}#
                            #{r\ 954}#
                            #{maps\ 955}#
                            #{ellipsis?\ 956}#
                            #{mod\ 957}#)
                     (if (#{id?\ 131}# #{e\ 953}#)
                       (let ((#{label\ 958}#
                               (#{id-var-name\ 153}# #{e\ 953}# (quote (())))))
                         (let ((#{b\ 959}# (#{lookup\ 128}#
                                             #{label\ 958}#
                                             #{r\ 954}#
                                             #{mod\ 957}#)))
                           (if (eq? (#{binding-type\ 123}# #{b\ 959}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 960}#
                                         (#{binding-value\ 124}# #{b\ 959}#)))
                                   (#{gen-ref\ 919}#
                                     #{src\ 952}#
                                     (car #{var.lev\ 960}#)
                                     (cdr #{var.lev\ 960}#)
                                     #{maps\ 955}#)))
                               (lambda (#{var\ 961}# #{maps\ 962}#)
                                 (values
                                   (list (quote ref) #{var\ 961}#)
                                   #{maps\ 962}#)))
                             (if (#{ellipsis?\ 956}# #{e\ 953}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 952}#)
                               (values
                                 (list (quote quote) #{e\ 953}#)
                                 #{maps\ 955}#)))))
                       ((lambda (#{tmp\ 963}#)
                          ((lambda (#{tmp\ 964}#)
                             (if (if #{tmp\ 964}#
                                   (apply (lambda (#{dots\ 965}# #{e\ 966}#)
                                            (#{ellipsis?\ 956}# #{dots\ 965}#))
                                          #{tmp\ 964}#)
                                   #f)
                               (apply (lambda (#{dots\ 967}# #{e\ 968}#)
                                        (#{gen-syntax\ 918}#
                                          #{src\ 952}#
                                          #{e\ 968}#
                                          #{r\ 954}#
                                          #{maps\ 955}#
                                          (lambda (#{x\ 969}#) #f)
                                          #{mod\ 957}#))
                                      #{tmp\ 964}#)
                               ((lambda (#{tmp\ 970}#)
                                  (if (if #{tmp\ 970}#
                                        (apply (lambda (#{x\ 971}#
                                                        #{dots\ 972}#
                                                        #{y\ 973}#)
                                                 (#{ellipsis?\ 956}#
                                                   #{dots\ 972}#))
                                               #{tmp\ 970}#)
                                        #f)
                                    (apply (lambda (#{x\ 974}#
                                                    #{dots\ 975}#
                                                    #{y\ 976}#)
                                             (letrec ((#{f\ 977}# (lambda (#{y\ 978}#
                                                                           #{k\ 979}#)
                                                                    ((lambda (#{tmp\ 983}#)
                                                                       ((lambda (#{tmp\ 984}#)
                                                                          (if (if #{tmp\ 984}#
                                                                                (apply (lambda (#{dots\ 985}#
                                                                                                #{y\ 986}#)
                                                                                         (#{ellipsis?\ 956}#
                                                                                           #{dots\ 985}#))
                                                                                       #{tmp\ 984}#)
                                                                                #f)
                                                                            (apply (lambda (#{dots\ 987}#
                                                                                            #{y\ 988}#)
                                                                                     (#{f\ 977}# #{y\ 988}#
                                                                                                 (lambda (#{maps\ 989}#)
                                                                                                   (call-with-values
                                                                                                     (lambda ()
                                                                                                       (#{k\ 979}# (cons '()
                                                                                                                         #{maps\ 989}#)))
                                                                                                     (lambda (#{x\ 990}#
                                                                                                              #{maps\ 991}#)
                                                                                                       (if (null? (car #{maps\ 991}#))
                                                                                                         (syntax-violation
                                                                                                           'syntax
                                                                                                           "extra ellipsis"
                                                                                                           #{src\ 952}#)
                                                                                                         (values
                                                                                                           (#{gen-mappend\ 920}#
                                                                                                             #{x\ 990}#
                                                                                                             (car #{maps\ 991}#))
                                                                                                           (cdr #{maps\ 991}#))))))))
                                                                                   #{tmp\ 984}#)
                                                                            ((lambda (#{_\ 992}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{gen-syntax\ 918}#
                                                                                     #{src\ 952}#
                                                                                     #{y\ 978}#
                                                                                     #{r\ 954}#
                                                                                     #{maps\ 955}#
                                                                                     #{ellipsis?\ 956}#
                                                                                     #{mod\ 957}#))
                                                                                 (lambda (#{y\ 993}#
                                                                                          #{maps\ 994}#)
                                                                                   (call-with-values
                                                                                     (lambda ()
                                                                                       (#{k\ 979}# #{maps\ 994}#))
                                                                                     (lambda (#{x\ 995}#
                                                                                              #{maps\ 996}#)
                                                                                       (values
                                                                                         (#{gen-append\ 923}#
                                                                                           #{x\ 995}#
                                                                                           #{y\ 993}#)
                                                                                         #{maps\ 996}#))))))
                                                                             #{tmp\ 983}#)))
                                                                        ($sc-dispatch
                                                                          #{tmp\ 983}#
                                                                          '(any .
                                                                                any))))
                                                                     #{y\ 978}#))))
                                               (#{f\ 977}# #{y\ 976}#
                                                           (lambda (#{maps\ 980}#)
                                                             (call-with-values
                                                               (lambda ()
                                                                 (#{gen-syntax\ 918}#
                                                                   #{src\ 952}#
                                                                   #{x\ 974}#
                                                                   #{r\ 954}#
                                                                   (cons '()
                                                                         #{maps\ 980}#)
                                                                   #{ellipsis?\ 956}#
                                                                   #{mod\ 957}#))
                                                               (lambda (#{x\ 981}#
                                                                        #{maps\ 982}#)
                                                                 (if (null? (car #{maps\ 982}#))
                                                                   (syntax-violation
                                                                     'syntax
                                                                     "extra ellipsis"
                                                                     #{src\ 952}#)
                                                                   (values
                                                                     (#{gen-map\ 921}#
                                                                       #{x\ 981}#
                                                                       (car #{maps\ 982}#))
                                                                     (cdr #{maps\ 982}#)))))))))
                                           #{tmp\ 970}#)
                                    ((lambda (#{tmp\ 997}#)
                                       (if #{tmp\ 997}#
                                         (apply (lambda (#{x\ 998}# #{y\ 999}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 918}#
                                                        #{src\ 952}#
                                                        #{x\ 998}#
                                                        #{r\ 954}#
                                                        #{maps\ 955}#
                                                        #{ellipsis?\ 956}#
                                                        #{mod\ 957}#))
                                                    (lambda (#{x\ 1000}#
                                                             #{maps\ 1001}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 918}#
                                                            #{src\ 952}#
                                                            #{y\ 999}#
                                                            #{r\ 954}#
                                                            #{maps\ 1001}#
                                                            #{ellipsis?\ 956}#
                                                            #{mod\ 957}#))
                                                        (lambda (#{y\ 1002}#
                                                                 #{maps\ 1003}#)
                                                          (values
                                                            (#{gen-cons\ 922}#
                                                              #{x\ 1000}#
                                                              #{y\ 1002}#)
                                                            #{maps\ 1003}#))))))
                                                #{tmp\ 997}#)
                                         ((lambda (#{tmp\ 1004}#)
                                            (if #{tmp\ 1004}#
                                              (apply (lambda (#{e1\ 1005}#
                                                              #{e2\ 1006}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 918}#
                                                             #{src\ 952}#
                                                             (cons #{e1\ 1005}#
                                                                   #{e2\ 1006}#)
                                                             #{r\ 954}#
                                                             #{maps\ 955}#
                                                             #{ellipsis?\ 956}#
                                                             #{mod\ 957}#))
                                                         (lambda (#{e\ 1008}#
                                                                  #{maps\ 1009}#)
                                                           (values
                                                             (#{gen-vector\ 924}#
                                                               #{e\ 1008}#)
                                                             #{maps\ 1009}#))))
                                                     #{tmp\ 1004}#)
                                              ((lambda (#{_\ 1010}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 953}#)
                                                   #{maps\ 955}#))
                                               #{tmp\ 963}#)))
                                          ($sc-dispatch
                                            #{tmp\ 963}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 963}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 963}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 963}# (quote (any any)))))
                        #{e\ 953}#)))))
          (lambda (#{e\ 1011}#
                   #{r\ 1012}#
                   #{w\ 1013}#
                   #{s\ 1014}#
                   #{mod\ 1015}#)
            (let ((#{e\ 1016}#
                    (#{source-wrap\ 160}#
                      #{e\ 1011}#
                      #{w\ 1013}#
                      #{s\ 1014}#
                      #{mod\ 1015}#)))
              ((lambda (#{tmp\ 1017}#)
                 ((lambda (#{tmp\ 1018}#)
                    (if #{tmp\ 1018}#
                      (apply (lambda (#{_\ 1019}# #{x\ 1020}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 918}#
                                     #{e\ 1016}#
                                     #{x\ 1020}#
                                     #{r\ 1012}#
                                     '()
                                     #{ellipsis?\ 175}#
                                     #{mod\ 1015}#))
                                 (lambda (#{e\ 1021}# #{maps\ 1022}#)
                                   (#{regen\ 925}# #{e\ 1021}#))))
                             #{tmp\ 1018}#)
                      ((lambda (#{_\ 1023}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 1016}#))
                       #{tmp\ 1017}#)))
                  ($sc-dispatch #{tmp\ 1017}# (quote (any any)))))
               #{e\ 1016}#)))))
      (#{global-extend\ 129}#
        'core
        'lambda
        (lambda (#{e\ 1024}#
                 #{r\ 1025}#
                 #{w\ 1026}#
                 #{s\ 1027}#
                 #{mod\ 1028}#)
          (letrec ((#{docstring&body\ 1029}#
                     (lambda (#{ids\ 1030}#
                              #{vars\ 1031}#
                              #{labels\ 1032}#
                              #{c\ 1033}#)
                       ((lambda (#{tmp\ 1034}#)
                          ((lambda (#{tmp\ 1035}#)
                             (if (if #{tmp\ 1035}#
                                   (apply (lambda (#{docstring\ 1036}#
                                                   #{e1\ 1037}#
                                                   #{e2\ 1038}#)
                                            (string?
                                              (syntax->datum
                                                #{docstring\ 1036}#)))
                                          #{tmp\ 1035}#)
                                   #f)
                               (apply (lambda (#{docstring\ 1039}#
                                               #{e1\ 1040}#
                                               #{e2\ 1041}#)
                                        (values
                                          (syntax->datum #{docstring\ 1039}#)
                                          (#{chi-body\ 171}#
                                            (cons #{e1\ 1040}# #{e2\ 1041}#)
                                            (#{source-wrap\ 160}#
                                              #{e\ 1024}#
                                              #{w\ 1026}#
                                              #{s\ 1027}#
                                              #{mod\ 1028}#)
                                            (#{extend-var-env\ 126}#
                                              #{labels\ 1032}#
                                              #{vars\ 1031}#
                                              #{r\ 1025}#)
                                            (#{make-binding-wrap\ 148}#
                                              #{ids\ 1030}#
                                              #{labels\ 1032}#
                                              #{w\ 1026}#)
                                            #{mod\ 1028}#)))
                                      #{tmp\ 1035}#)
                               ((lambda (#{tmp\ 1043}#)
                                  (if #{tmp\ 1043}#
                                    (apply (lambda (#{e1\ 1044}# #{e2\ 1045}#)
                                             (values
                                               #f
                                               (#{chi-body\ 171}#
                                                 (cons #{e1\ 1044}#
                                                       #{e2\ 1045}#)
                                                 (#{source-wrap\ 160}#
                                                   #{e\ 1024}#
                                                   #{w\ 1026}#
                                                   #{s\ 1027}#
                                                   #{mod\ 1028}#)
                                                 (#{extend-var-env\ 126}#
                                                   #{labels\ 1032}#
                                                   #{vars\ 1031}#
                                                   #{r\ 1025}#)
                                                 (#{make-binding-wrap\ 148}#
                                                   #{ids\ 1030}#
                                                   #{labels\ 1032}#
                                                   #{w\ 1026}#)
                                                 #{mod\ 1028}#)))
                                           #{tmp\ 1043}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp\ 1034}#)))
                                ($sc-dispatch
                                  #{tmp\ 1034}#
                                  '(any . each-any)))))
                           ($sc-dispatch
                             #{tmp\ 1034}#
                             '(any any . each-any))))
                        #{c\ 1033}#))))
            ((lambda (#{tmp\ 1047}#)
               ((lambda (#{tmp\ 1048}#)
                  (if #{tmp\ 1048}#
                    (apply (lambda (#{_\ 1049}#
                                    #{id\ 1050}#
                                    #{e1\ 1051}#
                                    #{e2\ 1052}#)
                             (let ((#{ids\ 1053}# #{id\ 1050}#))
                               (if (not (#{valid-bound-ids?\ 156}#
                                          #{ids\ 1053}#))
                                 (syntax-violation
                                   'lambda
                                   "invalid parameter list"
                                   #{e\ 1024}#)
                                 (let ((#{vars\ 1055}#
                                         (map #{gen-var\ 177}# #{ids\ 1053}#))
                                       (#{labels\ 1056}#
                                         (#{gen-labels\ 137}# #{ids\ 1053}#)))
                                   (call-with-values
                                     (lambda ()
                                       (#{docstring&body\ 1029}#
                                         #{ids\ 1053}#
                                         #{vars\ 1055}#
                                         #{labels\ 1056}#
                                         (cons #{e1\ 1051}# #{e2\ 1052}#)))
                                     (lambda (#{docstring\ 1058}#
                                              #{body\ 1059}#)
                                       (#{build-simple-lambda\ 105}#
                                         #{s\ 1027}#
                                         (map syntax->datum #{ids\ 1053}#)
                                         #f
                                         #{vars\ 1055}#
                                         #{docstring\ 1058}#
                                         #{body\ 1059}#)))))))
                           #{tmp\ 1048}#)
                    ((lambda (#{tmp\ 1060}#)
                       (if #{tmp\ 1060}#
                         (apply (lambda (#{_\ 1061}#
                                         #{ids\ 1062}#
                                         #{e1\ 1063}#
                                         #{e2\ 1064}#)
                                  (let ((#{rids\ 1065}#
                                          (#{lambda-var-list\ 178}#
                                            #{ids\ 1062}#)))
                                    (if (not (#{valid-bound-ids?\ 156}#
                                               #{rids\ 1065}#))
                                      (syntax-violation
                                        'lambda
                                        "invalid parameter list"
                                        #{e\ 1024}#)
                                      (let ((#{req\ 1066}#
                                              (reverse (cdr #{rids\ 1065}#))))
                                        (let ((#{rest\ 1067}#
                                                (car #{rids\ 1065}#)))
                                          (let ((#{rrids\ 1068}#
                                                  (reverse #{rids\ 1065}#)))
                                            (let ((#{vars\ 1069}#
                                                    (map #{gen-var\ 177}#
                                                         #{rrids\ 1068}#)))
                                              (let ((#{labels\ 1070}#
                                                      (#{gen-labels\ 137}#
                                                        #{rrids\ 1068}#)))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{docstring&body\ 1029}#
                                                      #{rrids\ 1068}#
                                                      #{vars\ 1069}#
                                                      #{labels\ 1070}#
                                                      (cons #{e1\ 1063}#
                                                            #{e2\ 1064}#)))
                                                  (lambda (#{docstring\ 1072}#
                                                           #{body\ 1073}#)
                                                    (#{build-simple-lambda\ 105}#
                                                      #{s\ 1027}#
                                                      (map syntax->datum
                                                           #{req\ 1066}#)
                                                      (syntax->datum
                                                        #{rest\ 1067}#)
                                                      #{vars\ 1069}#
                                                      #{docstring\ 1072}#
                                                      #{body\ 1073}#)))))))))))
                                #{tmp\ 1060}#)
                         ((lambda (#{_\ 1074}#)
                            (syntax-violation
                              'lambda
                              "bad lambda"
                              #{e\ 1024}#))
                          #{tmp\ 1047}#)))
                     ($sc-dispatch
                       #{tmp\ 1047}#
                       '(any any any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1047}#
                  '(any each-any any . each-any))))
             #{e\ 1024}#))))
      (#{global-extend\ 129}#
        'core
        'let
        (letrec ((#{chi-let\ 1075}#
                   (lambda (#{e\ 1076}#
                            #{r\ 1077}#
                            #{w\ 1078}#
                            #{s\ 1079}#
                            #{mod\ 1080}#
                            #{constructor\ 1081}#
                            #{ids\ 1082}#
                            #{vals\ 1083}#
                            #{exps\ 1084}#)
                     (if (not (#{valid-bound-ids?\ 156}# #{ids\ 1082}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 1076}#)
                       (let ((#{labels\ 1085}#
                               (#{gen-labels\ 137}# #{ids\ 1082}#))
                             (#{new-vars\ 1086}#
                               (map #{gen-var\ 177}# #{ids\ 1082}#)))
                         (let ((#{nw\ 1087}#
                                 (#{make-binding-wrap\ 148}#
                                   #{ids\ 1082}#
                                   #{labels\ 1085}#
                                   #{w\ 1078}#))
                               (#{nr\ 1088}#
                                 (#{extend-var-env\ 126}#
                                   #{labels\ 1085}#
                                   #{new-vars\ 1086}#
                                   #{r\ 1077}#)))
                           (#{constructor\ 1081}#
                             #{s\ 1079}#
                             (map syntax->datum #{ids\ 1082}#)
                             #{new-vars\ 1086}#
                             (map (lambda (#{x\ 1089}#)
                                    (#{chi\ 167}#
                                      #{x\ 1089}#
                                      #{r\ 1077}#
                                      #{w\ 1078}#
                                      #{mod\ 1080}#))
                                  #{vals\ 1083}#)
                             (#{chi-body\ 171}#
                               #{exps\ 1084}#
                               (#{source-wrap\ 160}#
                                 #{e\ 1076}#
                                 #{nw\ 1087}#
                                 #{s\ 1079}#
                                 #{mod\ 1080}#)
                               #{nr\ 1088}#
                               #{nw\ 1087}#
                               #{mod\ 1080}#))))))))
          (lambda (#{e\ 1090}#
                   #{r\ 1091}#
                   #{w\ 1092}#
                   #{s\ 1093}#
                   #{mod\ 1094}#)
            ((lambda (#{tmp\ 1095}#)
               ((lambda (#{tmp\ 1096}#)
                  (if (if #{tmp\ 1096}#
                        (apply (lambda (#{_\ 1097}#
                                        #{id\ 1098}#
                                        #{val\ 1099}#
                                        #{e1\ 1100}#
                                        #{e2\ 1101}#)
                                 (and-map #{id?\ 131}# #{id\ 1098}#))
                               #{tmp\ 1096}#)
                        #f)
                    (apply (lambda (#{_\ 1103}#
                                    #{id\ 1104}#
                                    #{val\ 1105}#
                                    #{e1\ 1106}#
                                    #{e2\ 1107}#)
                             (#{chi-let\ 1075}#
                               #{e\ 1090}#
                               #{r\ 1091}#
                               #{w\ 1092}#
                               #{s\ 1093}#
                               #{mod\ 1094}#
                               #{build-let\ 111}#
                               #{id\ 1104}#
                               #{val\ 1105}#
                               (cons #{e1\ 1106}# #{e2\ 1107}#)))
                           #{tmp\ 1096}#)
                    ((lambda (#{tmp\ 1111}#)
                       (if (if #{tmp\ 1111}#
                             (apply (lambda (#{_\ 1112}#
                                             #{f\ 1113}#
                                             #{id\ 1114}#
                                             #{val\ 1115}#
                                             #{e1\ 1116}#
                                             #{e2\ 1117}#)
                                      (if (#{id?\ 131}# #{f\ 1113}#)
                                        (and-map #{id?\ 131}# #{id\ 1114}#)
                                        #f))
                                    #{tmp\ 1111}#)
                             #f)
                         (apply (lambda (#{_\ 1119}#
                                         #{f\ 1120}#
                                         #{id\ 1121}#
                                         #{val\ 1122}#
                                         #{e1\ 1123}#
                                         #{e2\ 1124}#)
                                  (#{chi-let\ 1075}#
                                    #{e\ 1090}#
                                    #{r\ 1091}#
                                    #{w\ 1092}#
                                    #{s\ 1093}#
                                    #{mod\ 1094}#
                                    #{build-named-let\ 112}#
                                    (cons #{f\ 1120}# #{id\ 1121}#)
                                    #{val\ 1122}#
                                    (cons #{e1\ 1123}# #{e2\ 1124}#)))
                                #{tmp\ 1111}#)
                         ((lambda (#{_\ 1128}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 160}#
                                #{e\ 1090}#
                                #{w\ 1092}#
                                #{s\ 1093}#
                                #{mod\ 1094}#)))
                          #{tmp\ 1095}#)))
                     ($sc-dispatch
                       #{tmp\ 1095}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 1095}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 1090}#))))
      (#{global-extend\ 129}#
        'core
        'letrec
        (lambda (#{e\ 1129}#
                 #{r\ 1130}#
                 #{w\ 1131}#
                 #{s\ 1132}#
                 #{mod\ 1133}#)
          ((lambda (#{tmp\ 1134}#)
             ((lambda (#{tmp\ 1135}#)
                (if (if #{tmp\ 1135}#
                      (apply (lambda (#{_\ 1136}#
                                      #{id\ 1137}#
                                      #{val\ 1138}#
                                      #{e1\ 1139}#
                                      #{e2\ 1140}#)
                               (and-map #{id?\ 131}# #{id\ 1137}#))
                             #{tmp\ 1135}#)
                      #f)
                  (apply (lambda (#{_\ 1142}#
                                  #{id\ 1143}#
                                  #{val\ 1144}#
                                  #{e1\ 1145}#
                                  #{e2\ 1146}#)
                           (let ((#{ids\ 1147}# #{id\ 1143}#))
                             (if (not (#{valid-bound-ids?\ 156}#
                                        #{ids\ 1147}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 1129}#)
                               (let ((#{labels\ 1149}#
                                       (#{gen-labels\ 137}# #{ids\ 1147}#))
                                     (#{new-vars\ 1150}#
                                       (map #{gen-var\ 177}# #{ids\ 1147}#)))
                                 (let ((#{w\ 1151}#
                                         (#{make-binding-wrap\ 148}#
                                           #{ids\ 1147}#
                                           #{labels\ 1149}#
                                           #{w\ 1131}#))
                                       (#{r\ 1152}#
                                         (#{extend-var-env\ 126}#
                                           #{labels\ 1149}#
                                           #{new-vars\ 1150}#
                                           #{r\ 1130}#)))
                                   (#{build-letrec\ 113}#
                                     #{s\ 1132}#
                                     (map syntax->datum #{ids\ 1147}#)
                                     #{new-vars\ 1150}#
                                     (map (lambda (#{x\ 1153}#)
                                            (#{chi\ 167}#
                                              #{x\ 1153}#
                                              #{r\ 1152}#
                                              #{w\ 1151}#
                                              #{mod\ 1133}#))
                                          #{val\ 1144}#)
                                     (#{chi-body\ 171}#
                                       (cons #{e1\ 1145}# #{e2\ 1146}#)
                                       (#{source-wrap\ 160}#
                                         #{e\ 1129}#
                                         #{w\ 1151}#
                                         #{s\ 1132}#
                                         #{mod\ 1133}#)
                                       #{r\ 1152}#
                                       #{w\ 1151}#
                                       #{mod\ 1133}#)))))))
                         #{tmp\ 1135}#)
                  ((lambda (#{_\ 1156}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 160}#
                         #{e\ 1129}#
                         #{w\ 1131}#
                         #{s\ 1132}#
                         #{mod\ 1133}#)))
                   #{tmp\ 1134}#)))
              ($sc-dispatch
                #{tmp\ 1134}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 1129}#)))
      (#{global-extend\ 129}#
        'core
        'set!
        (lambda (#{e\ 1157}#
                 #{r\ 1158}#
                 #{w\ 1159}#
                 #{s\ 1160}#
                 #{mod\ 1161}#)
          ((lambda (#{tmp\ 1162}#)
             ((lambda (#{tmp\ 1163}#)
                (if (if #{tmp\ 1163}#
                      (apply (lambda (#{_\ 1164}# #{id\ 1165}# #{val\ 1166}#)
                               (#{id?\ 131}# #{id\ 1165}#))
                             #{tmp\ 1163}#)
                      #f)
                  (apply (lambda (#{_\ 1167}# #{id\ 1168}# #{val\ 1169}#)
                           (let ((#{val\ 1170}#
                                   (#{chi\ 167}#
                                     #{val\ 1169}#
                                     #{r\ 1158}#
                                     #{w\ 1159}#
                                     #{mod\ 1161}#))
                                 (#{n\ 1171}#
                                   (#{id-var-name\ 153}#
                                     #{id\ 1168}#
                                     #{w\ 1159}#)))
                             (let ((#{b\ 1172}#
                                     (#{lookup\ 128}#
                                       #{n\ 1171}#
                                       #{r\ 1158}#
                                       #{mod\ 1161}#)))
                               (let ((#{atom-key\ 1173}#
                                       (#{binding-type\ 123}# #{b\ 1172}#)))
                                 (if (memv #{atom-key\ 1173}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 99}#
                                     #{s\ 1160}#
                                     (syntax->datum #{id\ 1168}#)
                                     (#{binding-value\ 124}# #{b\ 1172}#)
                                     #{val\ 1170}#)
                                   (if (memv #{atom-key\ 1173}#
                                             '(global))
                                     (#{build-global-assignment\ 102}#
                                       #{s\ 1160}#
                                       #{n\ 1171}#
                                       #{val\ 1170}#
                                       #{mod\ 1161}#)
                                     (if (memv #{atom-key\ 1173}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 159}#
                                           #{id\ 1168}#
                                           #{w\ 1159}#
                                           #{mod\ 1161}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 160}#
                                           #{e\ 1157}#
                                           #{w\ 1159}#
                                           #{s\ 1160}#
                                           #{mod\ 1161}#)))))))))
                         #{tmp\ 1163}#)
                  ((lambda (#{tmp\ 1174}#)
                     (if #{tmp\ 1174}#
                       (apply (lambda (#{_\ 1175}#
                                       #{head\ 1176}#
                                       #{tail\ 1177}#
                                       #{val\ 1178}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 165}#
                                      #{head\ 1176}#
                                      #{r\ 1158}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 1161}#
                                      #t))
                                  (lambda (#{type\ 1179}#
                                           #{value\ 1180}#
                                           #{ee\ 1181}#
                                           #{ww\ 1182}#
                                           #{ss\ 1183}#
                                           #{modmod\ 1184}#)
                                    (if (memv #{type\ 1179}#
                                              '(module-ref))
                                      (let ((#{val\ 1185}#
                                              (#{chi\ 167}#
                                                #{val\ 1178}#
                                                #{r\ 1158}#
                                                #{w\ 1159}#
                                                #{mod\ 1161}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 1180}#
                                              (cons #{head\ 1176}#
                                                    #{tail\ 1177}#)))
                                          (lambda (#{id\ 1187}# #{mod\ 1188}#)
                                            (#{build-global-assignment\ 102}#
                                              #{s\ 1160}#
                                              #{id\ 1187}#
                                              #{val\ 1185}#
                                              #{mod\ 1188}#))))
                                      (#{build-application\ 96}#
                                        #{s\ 1160}#
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
                                                #{head\ 1176}#)
                                          #{r\ 1158}#
                                          #{w\ 1159}#
                                          #{mod\ 1161}#)
                                        (map (lambda (#{e\ 1189}#)
                                               (#{chi\ 167}#
                                                 #{e\ 1189}#
                                                 #{r\ 1158}#
                                                 #{w\ 1159}#
                                                 #{mod\ 1161}#))
                                             (append
                                               #{tail\ 1177}#
                                               (list #{val\ 1178}#))))))))
                              #{tmp\ 1174}#)
                       ((lambda (#{_\ 1191}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 160}#
                              #{e\ 1157}#
                              #{w\ 1159}#
                              #{s\ 1160}#
                              #{mod\ 1161}#)))
                        #{tmp\ 1162}#)))
                   ($sc-dispatch
                     #{tmp\ 1162}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 1162}#
                '(any any any))))
           #{e\ 1157}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@
        (lambda (#{e\ 1192}#)
          ((lambda (#{tmp\ 1193}#)
             ((lambda (#{tmp\ 1194}#)
                (if (if #{tmp\ 1194}#
                      (apply (lambda (#{_\ 1195}# #{mod\ 1196}# #{id\ 1197}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1196}#)
                                 (#{id?\ 131}# #{id\ 1197}#)
                                 #f))
                             #{tmp\ 1194}#)
                      #f)
                  (apply (lambda (#{_\ 1199}# #{mod\ 1200}# #{id\ 1201}#)
                           (values
                             (syntax->datum #{id\ 1201}#)
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
                                     #{mod\ 1200}#))))
                         #{tmp\ 1194}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1193}#)))
              ($sc-dispatch
                #{tmp\ 1193}#
                '(any each-any any))))
           #{e\ 1192}#)))
      (#{global-extend\ 129}#
        'module-ref
        '@@
        (lambda (#{e\ 1203}#)
          ((lambda (#{tmp\ 1204}#)
             ((lambda (#{tmp\ 1205}#)
                (if (if #{tmp\ 1205}#
                      (apply (lambda (#{_\ 1206}# #{mod\ 1207}# #{id\ 1208}#)
                               (if (and-map #{id?\ 131}# #{mod\ 1207}#)
                                 (#{id?\ 131}# #{id\ 1208}#)
                                 #f))
                             #{tmp\ 1205}#)
                      #f)
                  (apply (lambda (#{_\ 1210}# #{mod\ 1211}# #{id\ 1212}#)
                           (values
                             (syntax->datum #{id\ 1212}#)
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
                                     #{mod\ 1211}#))))
                         #{tmp\ 1205}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 1204}#)))
              ($sc-dispatch
                #{tmp\ 1204}#
                '(any each-any any))))
           #{e\ 1203}#)))
      (#{global-extend\ 129}#
        'core
        'if
        (lambda (#{e\ 1214}#
                 #{r\ 1215}#
                 #{w\ 1216}#
                 #{s\ 1217}#
                 #{mod\ 1218}#)
          ((lambda (#{tmp\ 1219}#)
             ((lambda (#{tmp\ 1220}#)
                (if #{tmp\ 1220}#
                  (apply (lambda (#{_\ 1221}# #{test\ 1222}# #{then\ 1223}#)
                           (#{build-conditional\ 97}#
                             #{s\ 1217}#
                             (#{chi\ 167}#
                               #{test\ 1222}#
                               #{r\ 1215}#
                               #{w\ 1216}#
                               #{mod\ 1218}#)
                             (#{chi\ 167}#
                               #{then\ 1223}#
                               #{r\ 1215}#
                               #{w\ 1216}#
                               #{mod\ 1218}#)
                             (#{build-void\ 95}# #f)))
                         #{tmp\ 1220}#)
                  ((lambda (#{tmp\ 1224}#)
                     (if #{tmp\ 1224}#
                       (apply (lambda (#{_\ 1225}#
                                       #{test\ 1226}#
                                       #{then\ 1227}#
                                       #{else\ 1228}#)
                                (#{build-conditional\ 97}#
                                  #{s\ 1217}#
                                  (#{chi\ 167}#
                                    #{test\ 1226}#
                                    #{r\ 1215}#
                                    #{w\ 1216}#
                                    #{mod\ 1218}#)
                                  (#{chi\ 167}#
                                    #{then\ 1227}#
                                    #{r\ 1215}#
                                    #{w\ 1216}#
                                    #{mod\ 1218}#)
                                  (#{chi\ 167}#
                                    #{else\ 1228}#
                                    #{r\ 1215}#
                                    #{w\ 1216}#
                                    #{mod\ 1218}#)))
                              #{tmp\ 1224}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1219}#)))
                   ($sc-dispatch
                     #{tmp\ 1219}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 1219}#
                '(any any any))))
           #{e\ 1214}#)))
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
        (letrec ((#{gen-syntax-case\ 1232}#
                   (lambda (#{x\ 1233}#
                            #{keys\ 1234}#
                            #{clauses\ 1235}#
                            #{r\ 1236}#
                            #{mod\ 1237}#)
                     (if (null? #{clauses\ 1235}#)
                       (#{build-application\ 96}#
                         #f
                         (#{build-primref\ 108}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 109}# #f #f)
                               (#{build-data\ 109}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 1233}#))
                       ((lambda (#{tmp\ 1238}#)
                          ((lambda (#{tmp\ 1239}#)
                             (if #{tmp\ 1239}#
                               (apply (lambda (#{pat\ 1240}# #{exp\ 1241}#)
                                        (if (if (#{id?\ 131}# #{pat\ 1240}#)
                                              (and-map
                                                (lambda (#{x\ 1242}#)
                                                  (not (#{free-id=?\ 154}#
                                                         #{pat\ 1240}#
                                                         #{x\ 1242}#)))
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
                                                      #{keys\ 1234}#))
                                              #f)
                                          (let ((#{labels\ 1243}#
                                                  (list (#{gen-label\ 136}#)))
                                                (#{var\ 1244}#
                                                  (#{gen-var\ 177}#
                                                    #{pat\ 1240}#)))
                                            (#{build-application\ 96}#
                                              #f
                                              (#{build-simple-lambda\ 105}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 1240}#))
                                                #f
                                                (list #{var\ 1244}#)
                                                #f
                                                (#{chi\ 167}#
                                                  #{exp\ 1241}#
                                                  (#{extend-env\ 125}#
                                                    #{labels\ 1243}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 1244}#
                                                                      0)))
                                                    #{r\ 1236}#)
                                                  (#{make-binding-wrap\ 148}#
                                                    (list #{pat\ 1240}#)
                                                    #{labels\ 1243}#
                                                    '(()))
                                                  #{mod\ 1237}#))
                                              (list #{x\ 1233}#)))
                                          (#{gen-clause\ 1231}#
                                            #{x\ 1233}#
                                            #{keys\ 1234}#
                                            (cdr #{clauses\ 1235}#)
                                            #{r\ 1236}#
                                            #{pat\ 1240}#
                                            #t
                                            #{exp\ 1241}#
                                            #{mod\ 1237}#)))
                                      #{tmp\ 1239}#)
                               ((lambda (#{tmp\ 1245}#)
                                  (if #{tmp\ 1245}#
                                    (apply (lambda (#{pat\ 1246}#
                                                    #{fender\ 1247}#
                                                    #{exp\ 1248}#)
                                             (#{gen-clause\ 1231}#
                                               #{x\ 1233}#
                                               #{keys\ 1234}#
                                               (cdr #{clauses\ 1235}#)
                                               #{r\ 1236}#
                                               #{pat\ 1246}#
                                               #{fender\ 1247}#
                                               #{exp\ 1248}#
                                               #{mod\ 1237}#))
                                           #{tmp\ 1245}#)
                                    ((lambda (#{_\ 1249}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 1235}#)))
                                     #{tmp\ 1238}#)))
                                ($sc-dispatch
                                  #{tmp\ 1238}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 1238}# (quote (any any)))))
                        (car #{clauses\ 1235}#)))))
                 (#{gen-clause\ 1231}#
                   (lambda (#{x\ 1250}#
                            #{keys\ 1251}#
                            #{clauses\ 1252}#
                            #{r\ 1253}#
                            #{pat\ 1254}#
                            #{fender\ 1255}#
                            #{exp\ 1256}#
                            #{mod\ 1257}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 1229}#
                           #{pat\ 1254}#
                           #{keys\ 1251}#))
                       (lambda (#{p\ 1258}# #{pvars\ 1259}#)
                         (if (not (#{distinct-bound-ids?\ 157}#
                                    (map car #{pvars\ 1259}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 1254}#)
                           (if (not (and-map
                                      (lambda (#{x\ 1260}#)
                                        (not (#{ellipsis?\ 175}#
                                               (car #{x\ 1260}#))))
                                      #{pvars\ 1259}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 1254}#)
                             (let ((#{y\ 1261}#
                                     (#{gen-var\ 177}# (quote tmp))))
                               (#{build-application\ 96}#
                                 #f
                                 (#{build-simple-lambda\ 105}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 1261}#)
                                   #f
                                   (let ((#{y\ 1262}#
                                           (#{build-lexical-reference\ 98}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 1261}#)))
                                     (#{build-conditional\ 97}#
                                       #f
                                       ((lambda (#{tmp\ 1263}#)
                                          ((lambda (#{tmp\ 1264}#)
                                             (if #{tmp\ 1264}#
                                               (apply (lambda () #{y\ 1262}#)
                                                      #{tmp\ 1264}#)
                                               ((lambda (#{_\ 1265}#)
                                                  (#{build-conditional\ 97}#
                                                    #f
                                                    #{y\ 1262}#
                                                    (#{build-dispatch-call\ 1230}#
                                                      #{pvars\ 1259}#
                                                      #{fender\ 1255}#
                                                      #{y\ 1262}#
                                                      #{r\ 1253}#
                                                      #{mod\ 1257}#)
                                                    (#{build-data\ 109}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 1263}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1263}#
                                             '#(atom #t))))
                                        #{fender\ 1255}#)
                                       (#{build-dispatch-call\ 1230}#
                                         #{pvars\ 1259}#
                                         #{exp\ 1256}#
                                         #{y\ 1262}#
                                         #{r\ 1253}#
                                         #{mod\ 1257}#)
                                       (#{gen-syntax-case\ 1232}#
                                         #{x\ 1250}#
                                         #{keys\ 1251}#
                                         #{clauses\ 1252}#
                                         #{r\ 1253}#
                                         #{mod\ 1257}#))))
                                 (list (if (eq? #{p\ 1258}# (quote any))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             'list)
                                           (list #{x\ 1250}#))
                                         (#{build-application\ 96}#
                                           #f
                                           (#{build-primref\ 108}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 1250}#
                                                 (#{build-data\ 109}#
                                                   #f
                                                   #{p\ 1258}#)))))))))))))
                 (#{build-dispatch-call\ 1230}#
                   (lambda (#{pvars\ 1266}#
                            #{exp\ 1267}#
                            #{y\ 1268}#
                            #{r\ 1269}#
                            #{mod\ 1270}#)
                     (let ((#{ids\ 1271}# (map car #{pvars\ 1266}#))
                           (#{levels\ 1272}# (map cdr #{pvars\ 1266}#)))
                       (let ((#{labels\ 1273}#
                               (#{gen-labels\ 137}# #{ids\ 1271}#))
                             (#{new-vars\ 1274}#
                               (map #{gen-var\ 177}# #{ids\ 1271}#)))
                         (#{build-application\ 96}#
                           #f
                           (#{build-primref\ 108}# #f (quote apply))
                           (list (#{build-simple-lambda\ 105}#
                                   #f
                                   (map syntax->datum #{ids\ 1271}#)
                                   #f
                                   #{new-vars\ 1274}#
                                   #f
                                   (#{chi\ 167}#
                                     #{exp\ 1267}#
                                     (#{extend-env\ 125}#
                                       #{labels\ 1273}#
                                       (map (lambda (#{var\ 1275}#
                                                     #{level\ 1276}#)
                                              (cons 'syntax
                                                    (cons #{var\ 1275}#
                                                          #{level\ 1276}#)))
                                            #{new-vars\ 1274}#
                                            (map cdr #{pvars\ 1266}#))
                                       #{r\ 1269}#)
                                     (#{make-binding-wrap\ 148}#
                                       #{ids\ 1271}#
                                       #{labels\ 1273}#
                                       '(()))
                                     #{mod\ 1270}#))
                                 #{y\ 1268}#))))))
                 (#{convert-pattern\ 1229}#
                   (lambda (#{pattern\ 1277}# #{keys\ 1278}#)
                     (letrec ((#{cvt\ 1279}#
                                (lambda (#{p\ 1280}# #{n\ 1281}# #{ids\ 1282}#)
                                  (if (#{id?\ 131}# #{p\ 1280}#)
                                    (if (#{bound-id-member?\ 158}#
                                          #{p\ 1280}#
                                          #{keys\ 1278}#)
                                      (values
                                        (vector (quote free-id) #{p\ 1280}#)
                                        #{ids\ 1282}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 1280}# #{n\ 1281}#)
                                              #{ids\ 1282}#)))
                                    ((lambda (#{tmp\ 1283}#)
                                       ((lambda (#{tmp\ 1284}#)
                                          (if (if #{tmp\ 1284}#
                                                (apply (lambda (#{x\ 1285}#
                                                                #{dots\ 1286}#)
                                                         (#{ellipsis?\ 175}#
                                                           #{dots\ 1286}#))
                                                       #{tmp\ 1284}#)
                                                #f)
                                            (apply (lambda (#{x\ 1287}#
                                                            #{dots\ 1288}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 1279}#
                                                           #{x\ 1287}#
                                                           (#{fx+\ 86}#
                                                             #{n\ 1281}#
                                                             1)
                                                           #{ids\ 1282}#))
                                                       (lambda (#{p\ 1289}#
                                                                #{ids\ 1290}#)
                                                         (values
                                                           (if (eq? #{p\ 1289}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 1289}#))
                                                           #{ids\ 1290}#))))
                                                   #{tmp\ 1284}#)
                                            ((lambda (#{tmp\ 1291}#)
                                               (if #{tmp\ 1291}#
                                                 (apply (lambda (#{x\ 1292}#
                                                                 #{y\ 1293}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 1279}#
                                                                #{y\ 1293}#
                                                                #{n\ 1281}#
                                                                #{ids\ 1282}#))
                                                            (lambda (#{y\ 1294}#
                                                                     #{ids\ 1295}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 1279}#
                                                                    #{x\ 1292}#
                                                                    #{n\ 1281}#
                                                                    #{ids\ 1295}#))
                                                                (lambda (#{x\ 1296}#
                                                                         #{ids\ 1297}#)
                                                                  (values
                                                                    (cons #{x\ 1296}#
                                                                          #{y\ 1294}#)
                                                                    #{ids\ 1297}#))))))
                                                        #{tmp\ 1291}#)
                                                 ((lambda (#{tmp\ 1298}#)
                                                    (if #{tmp\ 1298}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 1282}#))
                                                             #{tmp\ 1298}#)
                                                      ((lambda (#{tmp\ 1299}#)
                                                         (if #{tmp\ 1299}#
                                                           (apply (lambda (#{x\ 1300}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 1279}#
                                                                          #{x\ 1300}#
                                                                          #{n\ 1281}#
                                                                          #{ids\ 1282}#))
                                                                      (lambda (#{p\ 1302}#
                                                                               #{ids\ 1303}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 1302}#)
                                                                          #{ids\ 1303}#))))
                                                                  #{tmp\ 1299}#)
                                                           ((lambda (#{x\ 1304}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 176}#
                                                                    #{p\ 1280}#
                                                                    '(())))
                                                                #{ids\ 1282}#))
                                                            #{tmp\ 1283}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1283}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 1283}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 1283}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 1283}#
                                          '(any any))))
                                     #{p\ 1280}#)))))
                       (#{cvt\ 1279}# #{pattern\ 1277}# 0 (quote ()))))))
          (lambda (#{e\ 1305}#
                   #{r\ 1306}#
                   #{w\ 1307}#
                   #{s\ 1308}#
                   #{mod\ 1309}#)
            (let ((#{e\ 1310}#
                    (#{source-wrap\ 160}#
                      #{e\ 1305}#
                      #{w\ 1307}#
                      #{s\ 1308}#
                      #{mod\ 1309}#)))
              ((lambda (#{tmp\ 1311}#)
                 ((lambda (#{tmp\ 1312}#)
                    (if #{tmp\ 1312}#
                      (apply (lambda (#{_\ 1313}#
                                      #{val\ 1314}#
                                      #{key\ 1315}#
                                      #{m\ 1316}#)
                               (if (and-map
                                     (lambda (#{x\ 1317}#)
                                       (if (#{id?\ 131}# #{x\ 1317}#)
                                         (not (#{ellipsis?\ 175}# #{x\ 1317}#))
                                         #f))
                                     #{key\ 1315}#)
                                 (let ((#{x\ 1319}#
                                         (#{gen-var\ 177}# (quote tmp))))
                                   (#{build-application\ 96}#
                                     #{s\ 1308}#
                                     (#{build-simple-lambda\ 105}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 1319}#)
                                       #f
                                       (#{gen-syntax-case\ 1232}#
                                         (#{build-lexical-reference\ 98}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 1319}#)
                                         #{key\ 1315}#
                                         #{m\ 1316}#
                                         #{r\ 1306}#
                                         #{mod\ 1309}#))
                                     (list (#{chi\ 167}#
                                             #{val\ 1314}#
                                             #{r\ 1306}#
                                             '(())
                                             #{mod\ 1309}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 1310}#)))
                             #{tmp\ 1312}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 1311}#)))
                  ($sc-dispatch
                    #{tmp\ 1311}#
                    '(any any each-any . each-any))))
               #{e\ 1310}#)))))
      (set! sc-expand
        (lambda (#{x\ 1322}# . #{rest\ 1323}#)
          (if (if (pair? #{x\ 1322}#)
                (equal? (car #{x\ 1322}#) #{noexpand\ 84}#)
                #f)
            (cadr #{x\ 1322}#)
            (let ((#{m\ 1324}#
                    (if (null? #{rest\ 1323}#)
                      'e
                      (car #{rest\ 1323}#)))
                  (#{esew\ 1325}#
                    (if (let ((#{t\ 1326}# (null? #{rest\ 1323}#)))
                          (if #{t\ 1326}#
                            #{t\ 1326}#
                            (null? (cdr #{rest\ 1323}#))))
                      '(eval)
                      (cadr #{rest\ 1323}#))))
              (with-fluid*
                #{*mode*\ 85}#
                #{m\ 1324}#
                (lambda ()
                  (#{chi-top\ 166}#
                    #{x\ 1322}#
                    '()
                    '((top))
                    #{m\ 1324}#
                    #{esew\ 1325}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 1327}#)
          (#{nonsymbol-id?\ 130}# #{x\ 1327}#)))
      (set! datum->syntax
        (lambda (#{id\ 1328}# #{datum\ 1329}#)
          (#{make-syntax-object\ 114}#
            #{datum\ 1329}#
            (#{syntax-object-wrap\ 117}# #{id\ 1328}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 1330}#)
          (#{strip\ 176}# #{x\ 1330}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 1331}#)
          (begin
            (let ((#{x\ 1332}# #{ls\ 1331}#))
              (if (not (list? #{x\ 1332}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 1332}#)))
            (map (lambda (#{x\ 1333}#)
                   (#{wrap\ 159}# (gensym) (quote ((top))) #f))
                 #{ls\ 1331}#))))
      (set! free-identifier=?
        (lambda (#{x\ 1334}# #{y\ 1335}#)
          (begin
            (let ((#{x\ 1336}# #{x\ 1334}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1336}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1336}#)))
            (let ((#{x\ 1337}# #{y\ 1335}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1337}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 1337}#)))
            (#{free-id=?\ 154}# #{x\ 1334}# #{y\ 1335}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 1338}# #{y\ 1339}#)
          (begin
            (let ((#{x\ 1340}# #{x\ 1338}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1340}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1340}#)))
            (let ((#{x\ 1341}# #{y\ 1339}#))
              (if (not (#{nonsymbol-id?\ 130}# #{x\ 1341}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 1341}#)))
            (#{bound-id=?\ 155}# #{x\ 1338}# #{y\ 1339}#))))
      (set! syntax-violation
        (lambda (#{who\ 1342}#
                 #{message\ 1343}#
                 #{form\ 1344}#
                 .
                 #{subform\ 1345}#)
          (begin
            (let ((#{x\ 1346}# #{who\ 1342}#))
              (if (not ((lambda (#{x\ 1347}#)
                          (let ((#{t\ 1348}# (not #{x\ 1347}#)))
                            (if #{t\ 1348}#
                              #{t\ 1348}#
                              (let ((#{t\ 1349}# (string? #{x\ 1347}#)))
                                (if #{t\ 1349}#
                                  #{t\ 1349}#
                                  (symbol? #{x\ 1347}#))))))
                        #{x\ 1346}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1346}#)))
            (let ((#{x\ 1350}# #{message\ 1343}#))
              (if (not (string? #{x\ 1350}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 1350}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 1342}# "~a: " "")
                "~a "
                (if (null? #{subform\ 1345}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 1351}#
                      (cons #{message\ 1343}#
                            (map (lambda (#{x\ 1352}#)
                                   (#{strip\ 176}# #{x\ 1352}# (quote (()))))
                                 (append
                                   #{subform\ 1345}#
                                   (list #{form\ 1344}#))))))
                (if #{who\ 1342}#
                  (cons #{who\ 1342}# #{tail\ 1351}#)
                  #{tail\ 1351}#))
              #f))))
      (letrec ((#{match\ 1357}#
                 (lambda (#{e\ 1358}#
                          #{p\ 1359}#
                          #{w\ 1360}#
                          #{r\ 1361}#
                          #{mod\ 1362}#)
                   (if (not #{r\ 1361}#)
                     #f
                     (if (eq? #{p\ 1359}# (quote any))
                       (cons (#{wrap\ 159}#
                               #{e\ 1358}#
                               #{w\ 1360}#
                               #{mod\ 1362}#)
                             #{r\ 1361}#)
                       (if (#{syntax-object?\ 115}# #{e\ 1358}#)
                         (#{match*\ 1356}#
                           (#{syntax-object-expression\ 116}# #{e\ 1358}#)
                           #{p\ 1359}#
                           (#{join-wraps\ 150}#
                             #{w\ 1360}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1358}#))
                           #{r\ 1361}#
                           (#{syntax-object-module\ 118}# #{e\ 1358}#))
                         (#{match*\ 1356}#
                           #{e\ 1358}#
                           #{p\ 1359}#
                           #{w\ 1360}#
                           #{r\ 1361}#
                           #{mod\ 1362}#))))))
               (#{match*\ 1356}#
                 (lambda (#{e\ 1363}#
                          #{p\ 1364}#
                          #{w\ 1365}#
                          #{r\ 1366}#
                          #{mod\ 1367}#)
                   (if (null? #{p\ 1364}#)
                     (if (null? #{e\ 1363}#) #{r\ 1366}# #f)
                     (if (pair? #{p\ 1364}#)
                       (if (pair? #{e\ 1363}#)
                         (#{match\ 1357}#
                           (car #{e\ 1363}#)
                           (car #{p\ 1364}#)
                           #{w\ 1365}#
                           (#{match\ 1357}#
                             (cdr #{e\ 1363}#)
                             (cdr #{p\ 1364}#)
                             #{w\ 1365}#
                             #{r\ 1366}#
                             #{mod\ 1367}#)
                           #{mod\ 1367}#)
                         #f)
                       (if (eq? #{p\ 1364}# (quote each-any))
                         (let ((#{l\ 1368}#
                                 (#{match-each-any\ 1354}#
                                   #{e\ 1363}#
                                   #{w\ 1365}#
                                   #{mod\ 1367}#)))
                           (if #{l\ 1368}#
                             (cons #{l\ 1368}# #{r\ 1366}#)
                             #f))
                         (let ((#{atom-key\ 1369}# (vector-ref #{p\ 1364}# 0)))
                           (if (memv #{atom-key\ 1369}# (quote (each)))
                             (if (null? #{e\ 1363}#)
                               (#{match-empty\ 1355}#
                                 (vector-ref #{p\ 1364}# 1)
                                 #{r\ 1366}#)
                               (let ((#{l\ 1370}#
                                       (#{match-each\ 1353}#
                                         #{e\ 1363}#
                                         (vector-ref #{p\ 1364}# 1)
                                         #{w\ 1365}#
                                         #{mod\ 1367}#)))
                                 (if #{l\ 1370}#
                                   (letrec ((#{collect\ 1371}#
                                              (lambda (#{l\ 1372}#)
                                                (if (null? (car #{l\ 1372}#))
                                                  #{r\ 1366}#
                                                  (cons (map car #{l\ 1372}#)
                                                        (#{collect\ 1371}#
                                                          (map cdr
                                                               #{l\ 1372}#)))))))
                                     (#{collect\ 1371}# #{l\ 1370}#))
                                   #f)))
                             (if (memv #{atom-key\ 1369}# (quote (free-id)))
                               (if (#{id?\ 131}# #{e\ 1363}#)
                                 (if (#{free-id=?\ 154}#
                                       (#{wrap\ 159}#
                                         #{e\ 1363}#
                                         #{w\ 1365}#
                                         #{mod\ 1367}#)
                                       (vector-ref #{p\ 1364}# 1))
                                   #{r\ 1366}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 1369}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 1364}# 1)
                                       (#{strip\ 176}#
                                         #{e\ 1363}#
                                         #{w\ 1365}#))
                                   #{r\ 1366}#
                                   #f)
                                 (if (memv #{atom-key\ 1369}# (quote (vector)))
                                   (if (vector? #{e\ 1363}#)
                                     (#{match\ 1357}#
                                       (vector->list #{e\ 1363}#)
                                       (vector-ref #{p\ 1364}# 1)
                                       #{w\ 1365}#
                                       #{r\ 1366}#
                                       #{mod\ 1367}#)
                                     #f)))))))))))
               (#{match-empty\ 1355}#
                 (lambda (#{p\ 1373}# #{r\ 1374}#)
                   (if (null? #{p\ 1373}#)
                     #{r\ 1374}#
                     (if (eq? #{p\ 1373}# (quote any))
                       (cons (quote ()) #{r\ 1374}#)
                       (if (pair? #{p\ 1373}#)
                         (#{match-empty\ 1355}#
                           (car #{p\ 1373}#)
                           (#{match-empty\ 1355}#
                             (cdr #{p\ 1373}#)
                             #{r\ 1374}#))
                         (if (eq? #{p\ 1373}# (quote each-any))
                           (cons (quote ()) #{r\ 1374}#)
                           (let ((#{atom-key\ 1375}#
                                   (vector-ref #{p\ 1373}# 0)))
                             (if (memv #{atom-key\ 1375}# (quote (each)))
                               (#{match-empty\ 1355}#
                                 (vector-ref #{p\ 1373}# 1)
                                 #{r\ 1374}#)
                               (if (memv #{atom-key\ 1375}#
                                         '(free-id atom))
                                 #{r\ 1374}#
                                 (if (memv #{atom-key\ 1375}# (quote (vector)))
                                   (#{match-empty\ 1355}#
                                     (vector-ref #{p\ 1373}# 1)
                                     #{r\ 1374}#)))))))))))
               (#{match-each-any\ 1354}#
                 (lambda (#{e\ 1376}# #{w\ 1377}# #{mod\ 1378}#)
                   (if (pair? #{e\ 1376}#)
                     (let ((#{l\ 1379}#
                             (#{match-each-any\ 1354}#
                               (cdr #{e\ 1376}#)
                               #{w\ 1377}#
                               #{mod\ 1378}#)))
                       (if #{l\ 1379}#
                         (cons (#{wrap\ 159}#
                                 (car #{e\ 1376}#)
                                 #{w\ 1377}#
                                 #{mod\ 1378}#)
                               #{l\ 1379}#)
                         #f))
                     (if (null? #{e\ 1376}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1376}#)
                         (#{match-each-any\ 1354}#
                           (#{syntax-object-expression\ 116}# #{e\ 1376}#)
                           (#{join-wraps\ 150}#
                             #{w\ 1377}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1376}#))
                           #{mod\ 1378}#)
                         #f)))))
               (#{match-each\ 1353}#
                 (lambda (#{e\ 1380}#
                          #{p\ 1381}#
                          #{w\ 1382}#
                          #{mod\ 1383}#)
                   (if (pair? #{e\ 1380}#)
                     (let ((#{first\ 1384}#
                             (#{match\ 1357}#
                               (car #{e\ 1380}#)
                               #{p\ 1381}#
                               #{w\ 1382}#
                               '()
                               #{mod\ 1383}#)))
                       (if #{first\ 1384}#
                         (let ((#{rest\ 1385}#
                                 (#{match-each\ 1353}#
                                   (cdr #{e\ 1380}#)
                                   #{p\ 1381}#
                                   #{w\ 1382}#
                                   #{mod\ 1383}#)))
                           (if #{rest\ 1385}#
                             (cons #{first\ 1384}# #{rest\ 1385}#)
                             #f))
                         #f))
                     (if (null? #{e\ 1380}#)
                       '()
                       (if (#{syntax-object?\ 115}# #{e\ 1380}#)
                         (#{match-each\ 1353}#
                           (#{syntax-object-expression\ 116}# #{e\ 1380}#)
                           #{p\ 1381}#
                           (#{join-wraps\ 150}#
                             #{w\ 1382}#
                             (#{syntax-object-wrap\ 117}# #{e\ 1380}#))
                           (#{syntax-object-module\ 118}# #{e\ 1380}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 1386}# #{p\ 1387}#)
            (if (eq? #{p\ 1387}# (quote any))
              (list #{e\ 1386}#)
              (if (#{syntax-object?\ 115}# #{e\ 1386}#)
                (#{match*\ 1356}#
                  (#{syntax-object-expression\ 116}# #{e\ 1386}#)
                  #{p\ 1387}#
                  (#{syntax-object-wrap\ 117}# #{e\ 1386}#)
                  '()
                  (#{syntax-object-module\ 118}# #{e\ 1386}#))
                (#{match*\ 1356}#
                  #{e\ 1386}#
                  #{p\ 1387}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1388}#)
      ((lambda (#{tmp\ 1389}#)
         ((lambda (#{tmp\ 1390}#)
            (if #{tmp\ 1390}#
              (apply (lambda (#{_\ 1391}# #{e1\ 1392}# #{e2\ 1393}#)
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
                             (cons #{e1\ 1392}# #{e2\ 1393}#)))
                     #{tmp\ 1390}#)
              ((lambda (#{tmp\ 1395}#)
                 (if #{tmp\ 1395}#
                   (apply (lambda (#{_\ 1396}#
                                   #{out\ 1397}#
                                   #{in\ 1398}#
                                   #{e1\ 1399}#
                                   #{e2\ 1400}#)
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
                                  #{in\ 1398}#
                                  '()
                                  (list #{out\ 1397}#
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
                                              (cons #{e1\ 1399}#
                                                    #{e2\ 1400}#)))))
                          #{tmp\ 1395}#)
                   ((lambda (#{tmp\ 1402}#)
                      (if #{tmp\ 1402}#
                        (apply (lambda (#{_\ 1403}#
                                        #{out\ 1404}#
                                        #{in\ 1405}#
                                        #{e1\ 1406}#
                                        #{e2\ 1407}#)
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
                                             #{in\ 1405}#)
                                       '()
                                       (list #{out\ 1404}#
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
                                                   (cons #{e1\ 1406}#
                                                         #{e2\ 1407}#)))))
                               #{tmp\ 1402}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 1389}#)))
                    ($sc-dispatch
                      #{tmp\ 1389}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 1389}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 1389}#
            '(any () any . each-any))))
       #{x\ 1388}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1411}#)
      ((lambda (#{tmp\ 1412}#)
         ((lambda (#{tmp\ 1413}#)
            (if #{tmp\ 1413}#
              (apply (lambda (#{_\ 1414}#
                              #{k\ 1415}#
                              #{keyword\ 1416}#
                              #{pattern\ 1417}#
                              #{template\ 1418}#)
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
                                         (cons #{k\ 1415}#
                                               (map (lambda (#{tmp\ 1421}#
                                                             #{tmp\ 1420}#)
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
                                                                  #{tmp\ 1420}#)
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
                                                                  #{tmp\ 1421}#)))
                                                    #{template\ 1418}#
                                                    #{pattern\ 1417}#))))))
                     #{tmp\ 1413}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1412}#)))
          ($sc-dispatch
            #{tmp\ 1412}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 1411}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 1422}#)
      ((lambda (#{tmp\ 1423}#)
         ((lambda (#{tmp\ 1424}#)
            (if (if #{tmp\ 1424}#
                  (apply (lambda (#{let*\ 1425}#
                                  #{x\ 1426}#
                                  #{v\ 1427}#
                                  #{e1\ 1428}#
                                  #{e2\ 1429}#)
                           (and-map identifier? #{x\ 1426}#))
                         #{tmp\ 1424}#)
                  #f)
              (apply (lambda (#{let*\ 1431}#
                              #{x\ 1432}#
                              #{v\ 1433}#
                              #{e1\ 1434}#
                              #{e2\ 1435}#)
                       (letrec ((#{f\ 1436}#
                                  (lambda (#{bindings\ 1437}#)
                                    (if (null? #{bindings\ 1437}#)
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
                                                  (cons #{e1\ 1434}#
                                                        #{e2\ 1435}#)))
                                      ((lambda (#{tmp\ 1441}#)
                                         ((lambda (#{tmp\ 1442}#)
                                            (if #{tmp\ 1442}#
                                              (apply (lambda (#{body\ 1443}#
                                                              #{binding\ 1444}#)
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
                                                             (list #{binding\ 1444}#)
                                                             #{body\ 1443}#))
                                                     #{tmp\ 1442}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 1441}#)))
                                          ($sc-dispatch
                                            #{tmp\ 1441}#
                                            '(any any))))
                                       (list (#{f\ 1436}#
                                               (cdr #{bindings\ 1437}#))
                                             (car #{bindings\ 1437}#)))))))
                         (#{f\ 1436}# (map list #{x\ 1432}# #{v\ 1433}#))))
                     #{tmp\ 1424}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1423}#)))
          ($sc-dispatch
            #{tmp\ 1423}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 1422}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 1445}#)
      ((lambda (#{tmp\ 1446}#)
         ((lambda (#{tmp\ 1447}#)
            (if #{tmp\ 1447}#
              (apply (lambda (#{_\ 1448}#
                              #{var\ 1449}#
                              #{init\ 1450}#
                              #{step\ 1451}#
                              #{e0\ 1452}#
                              #{e1\ 1453}#
                              #{c\ 1454}#)
                       ((lambda (#{tmp\ 1455}#)
                          ((lambda (#{tmp\ 1456}#)
                             (if #{tmp\ 1456}#
                               (apply (lambda (#{step\ 1457}#)
                                        ((lambda (#{tmp\ 1458}#)
                                           ((lambda (#{tmp\ 1459}#)
                                              (if #{tmp\ 1459}#
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
                                                                    #{var\ 1449}#
                                                                    #{init\ 1450}#)
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
                                                                           #{e0\ 1452}#)
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
                                                                             #{c\ 1454}#
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
                                                                                         #{step\ 1457}#)))))))
                                                       #{tmp\ 1459}#)
                                                ((lambda (#{tmp\ 1464}#)
                                                   (if #{tmp\ 1464}#
                                                     (apply (lambda (#{e1\ 1465}#
                                                                     #{e2\ 1466}#)
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
                                                                         #{var\ 1449}#
                                                                         #{init\ 1450}#)
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
                                                                          #{e0\ 1452}#
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
                                                                                (cons #{e1\ 1465}#
                                                                                      #{e2\ 1466}#))
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
                                                                                  #{c\ 1454}#
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
                                                                                              #{step\ 1457}#)))))))
                                                            #{tmp\ 1464}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1458}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1458}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 1458}#
                                              '())))
                                         #{e1\ 1453}#))
                                      #{tmp\ 1456}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 1455}#)))
                           ($sc-dispatch #{tmp\ 1455}# (quote each-any))))
                        (map (lambda (#{v\ 1473}# #{s\ 1474}#)
                               ((lambda (#{tmp\ 1475}#)
                                  ((lambda (#{tmp\ 1476}#)
                                     (if #{tmp\ 1476}#
                                       (apply (lambda () #{v\ 1473}#)
                                              #{tmp\ 1476}#)
                                       ((lambda (#{tmp\ 1477}#)
                                          (if #{tmp\ 1477}#
                                            (apply (lambda (#{e\ 1478}#)
                                                     #{e\ 1478}#)
                                                   #{tmp\ 1477}#)
                                            ((lambda (#{_\ 1479}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 1445}#
                                                 #{s\ 1474}#))
                                             #{tmp\ 1475}#)))
                                        ($sc-dispatch
                                          #{tmp\ 1475}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 1475}# (quote ()))))
                                #{s\ 1474}#))
                             #{var\ 1449}#
                             #{step\ 1451}#)))
                     #{tmp\ 1447}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1446}#)))
          ($sc-dispatch
            #{tmp\ 1446}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 1445}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 1482}#
               (lambda (#{x\ 1486}# #{y\ 1487}#)
                 ((lambda (#{tmp\ 1488}#)
                    ((lambda (#{tmp\ 1489}#)
                       (if #{tmp\ 1489}#
                         (apply (lambda (#{x\ 1490}# #{y\ 1491}#)
                                  ((lambda (#{tmp\ 1492}#)
                                     ((lambda (#{tmp\ 1493}#)
                                        (if #{tmp\ 1493}#
                                          (apply (lambda (#{dy\ 1494}#)
                                                   ((lambda (#{tmp\ 1495}#)
                                                      ((lambda (#{tmp\ 1496}#)
                                                         (if #{tmp\ 1496}#
                                                           (apply (lambda (#{dx\ 1497}#)
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
                                                                          (cons #{dx\ 1497}#
                                                                                #{dy\ 1494}#)))
                                                                  #{tmp\ 1496}#)
                                                           ((lambda (#{_\ 1498}#)
                                                              (if (null? #{dy\ 1494}#)
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
                                                                      #{x\ 1490}#)
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
                                                                      #{x\ 1490}#
                                                                      #{y\ 1491}#)))
                                                            #{tmp\ 1495}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 1495}#
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
                                                    #{x\ 1490}#))
                                                 #{tmp\ 1493}#)
                                          ((lambda (#{tmp\ 1499}#)
                                             (if #{tmp\ 1499}#
                                               (apply (lambda (#{stuff\ 1500}#)
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
                                                              (cons #{x\ 1490}#
                                                                    #{stuff\ 1500}#)))
                                                      #{tmp\ 1499}#)
                                               ((lambda (#{else\ 1501}#)
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
                                                        #{x\ 1490}#
                                                        #{y\ 1491}#))
                                                #{tmp\ 1492}#)))
                                           ($sc-dispatch
                                             #{tmp\ 1492}#
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
                                        #{tmp\ 1492}#
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
                                   #{y\ 1491}#))
                                #{tmp\ 1489}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1488}#)))
                     ($sc-dispatch #{tmp\ 1488}# (quote (any any)))))
                  (list #{x\ 1486}# #{y\ 1487}#))))
             (#{quasiappend\ 1483}#
               (lambda (#{x\ 1502}# #{y\ 1503}#)
                 ((lambda (#{tmp\ 1504}#)
                    ((lambda (#{tmp\ 1505}#)
                       (if #{tmp\ 1505}#
                         (apply (lambda (#{x\ 1506}# #{y\ 1507}#)
                                  ((lambda (#{tmp\ 1508}#)
                                     ((lambda (#{tmp\ 1509}#)
                                        (if #{tmp\ 1509}#
                                          (apply (lambda () #{x\ 1506}#)
                                                 #{tmp\ 1509}#)
                                          ((lambda (#{_\ 1510}#)
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
                                                   #{x\ 1506}#
                                                   #{y\ 1507}#))
                                           #{tmp\ 1508}#)))
                                      ($sc-dispatch
                                        #{tmp\ 1508}#
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
                                   #{y\ 1507}#))
                                #{tmp\ 1505}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 1504}#)))
                     ($sc-dispatch #{tmp\ 1504}# (quote (any any)))))
                  (list #{x\ 1502}# #{y\ 1503}#))))
             (#{quasivector\ 1484}#
               (lambda (#{x\ 1511}#)
                 ((lambda (#{tmp\ 1512}#)
                    ((lambda (#{x\ 1513}#)
                       ((lambda (#{tmp\ 1514}#)
                          ((lambda (#{tmp\ 1515}#)
                             (if #{tmp\ 1515}#
                               (apply (lambda (#{x\ 1516}#)
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
                                              (list->vector #{x\ 1516}#)))
                                      #{tmp\ 1515}#)
                               ((lambda (#{tmp\ 1518}#)
                                  (if #{tmp\ 1518}#
                                    (apply (lambda (#{x\ 1519}#)
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
                                                   #{x\ 1519}#))
                                           #{tmp\ 1518}#)
                                    ((lambda (#{_\ 1521}#)
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
                                             #{x\ 1513}#))
                                     #{tmp\ 1514}#)))
                                ($sc-dispatch
                                  #{tmp\ 1514}#
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
                             #{tmp\ 1514}#
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
                        #{x\ 1513}#))
                     #{tmp\ 1512}#))
                  #{x\ 1511}#)))
             (#{quasi\ 1485}#
               (lambda (#{p\ 1522}# #{lev\ 1523}#)
                 ((lambda (#{tmp\ 1524}#)
                    ((lambda (#{tmp\ 1525}#)
                       (if #{tmp\ 1525}#
                         (apply (lambda (#{p\ 1526}#)
                                  (if (= #{lev\ 1523}# 0)
                                    #{p\ 1526}#
                                    (#{quasicons\ 1482}#
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
                                      (#{quasi\ 1485}#
                                        (list #{p\ 1526}#)
                                        (- #{lev\ 1523}# 1)))))
                                #{tmp\ 1525}#)
                         ((lambda (#{tmp\ 1527}#)
                            (if (if #{tmp\ 1527}#
                                  (apply (lambda (#{args\ 1528}#)
                                           (= #{lev\ 1523}# 0))
                                         #{tmp\ 1527}#)
                                  #f)
                              (apply (lambda (#{args\ 1529}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 1522}#
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
                                               #{args\ 1529}#)))
                                     #{tmp\ 1527}#)
                              ((lambda (#{tmp\ 1530}#)
                                 (if #{tmp\ 1530}#
                                   (apply (lambda (#{p\ 1531}# #{q\ 1532}#)
                                            (if (= #{lev\ 1523}# 0)
                                              (#{quasiappend\ 1483}#
                                                #{p\ 1531}#
                                                (#{quasi\ 1485}#
                                                  #{q\ 1532}#
                                                  #{lev\ 1523}#))
                                              (#{quasicons\ 1482}#
                                                (#{quasicons\ 1482}#
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
                                                  (#{quasi\ 1485}#
                                                    (list #{p\ 1531}#)
                                                    (- #{lev\ 1523}# 1)))
                                                (#{quasi\ 1485}#
                                                  #{q\ 1532}#
                                                  #{lev\ 1523}#))))
                                          #{tmp\ 1530}#)
                                   ((lambda (#{tmp\ 1533}#)
                                      (if (if #{tmp\ 1533}#
                                            (apply (lambda (#{args\ 1534}#
                                                            #{q\ 1535}#)
                                                     (= #{lev\ 1523}# 0))
                                                   #{tmp\ 1533}#)
                                            #f)
                                        (apply (lambda (#{args\ 1536}#
                                                        #{q\ 1537}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 1522}#
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
                                                         #{args\ 1536}#)))
                                               #{tmp\ 1533}#)
                                        ((lambda (#{tmp\ 1538}#)
                                           (if #{tmp\ 1538}#
                                             (apply (lambda (#{p\ 1539}#)
                                                      (#{quasicons\ 1482}#
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
                                                        (#{quasi\ 1485}#
                                                          (list #{p\ 1539}#)
                                                          (+ #{lev\ 1523}#
                                                             1))))
                                                    #{tmp\ 1538}#)
                                             ((lambda (#{tmp\ 1540}#)
                                                (if #{tmp\ 1540}#
                                                  (apply (lambda (#{p\ 1541}#
                                                                  #{q\ 1542}#)
                                                           (#{quasicons\ 1482}#
                                                             (#{quasi\ 1485}#
                                                               #{p\ 1541}#
                                                               #{lev\ 1523}#)
                                                             (#{quasi\ 1485}#
                                                               #{q\ 1542}#
                                                               #{lev\ 1523}#)))
                                                         #{tmp\ 1540}#)
                                                  ((lambda (#{tmp\ 1543}#)
                                                     (if #{tmp\ 1543}#
                                                       (apply (lambda (#{x\ 1544}#)
                                                                (#{quasivector\ 1484}#
                                                                  (#{quasi\ 1485}#
                                                                    #{x\ 1544}#
                                                                    #{lev\ 1523}#)))
                                                              #{tmp\ 1543}#)
                                                       ((lambda (#{p\ 1546}#)
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
                                                                #{p\ 1546}#))
                                                        #{tmp\ 1524}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 1524}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 1524}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 1524}#
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
                                      #{tmp\ 1524}#
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
                                 #{tmp\ 1524}#
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
                            #{tmp\ 1524}#
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
                       #{tmp\ 1524}#
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
                  #{p\ 1522}#))))
      (lambda (#{x\ 1547}#)
        ((lambda (#{tmp\ 1548}#)
           ((lambda (#{tmp\ 1549}#)
              (if #{tmp\ 1549}#
                (apply (lambda (#{_\ 1550}# #{e\ 1551}#)
                         (#{quasi\ 1485}# #{e\ 1551}# 0))
                       #{tmp\ 1549}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1548}#)))
            ($sc-dispatch #{tmp\ 1548}# (quote (any any)))))
         #{x\ 1547}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1552}#)
      (letrec ((#{read-file\ 1553}#
                 (lambda (#{fn\ 1554}# #{k\ 1555}#)
                   (let ((#{p\ 1556}# (open-input-file #{fn\ 1554}#)))
                     (letrec ((#{f\ 1557}#
                                (lambda (#{x\ 1558}#)
                                  (if (eof-object? #{x\ 1558}#)
                                    (begin
                                      (close-input-port #{p\ 1556}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 1555}#
                                            #{x\ 1558}#)
                                          (#{f\ 1557}# (read #{p\ 1556}#)))))))
                       (#{f\ 1557}# (read #{p\ 1556}#)))))))
        ((lambda (#{tmp\ 1559}#)
           ((lambda (#{tmp\ 1560}#)
              (if #{tmp\ 1560}#
                (apply (lambda (#{k\ 1561}# #{filename\ 1562}#)
                         (let ((#{fn\ 1563}#
                                 (syntax->datum #{filename\ 1562}#)))
                           ((lambda (#{tmp\ 1564}#)
                              ((lambda (#{tmp\ 1565}#)
                                 (if #{tmp\ 1565}#
                                   (apply (lambda (#{exp\ 1566}#)
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
                                                  #{exp\ 1566}#))
                                          #{tmp\ 1565}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 1564}#)))
                               ($sc-dispatch #{tmp\ 1564}# (quote each-any))))
                            (#{read-file\ 1553}# #{fn\ 1563}# #{k\ 1561}#))))
                       #{tmp\ 1560}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 1559}#)))
            ($sc-dispatch #{tmp\ 1559}# (quote (any any)))))
         #{x\ 1552}#)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1568}#)
      ((lambda (#{tmp\ 1569}#)
         ((lambda (#{tmp\ 1570}#)
            (if #{tmp\ 1570}#
              (apply (lambda (#{_\ 1571}# #{e\ 1572}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 1568}#))
                     #{tmp\ 1570}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1569}#)))
          ($sc-dispatch #{tmp\ 1569}# (quote (any any)))))
       #{x\ 1568}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1573}#)
      ((lambda (#{tmp\ 1574}#)
         ((lambda (#{tmp\ 1575}#)
            (if #{tmp\ 1575}#
              (apply (lambda (#{_\ 1576}# #{e\ 1577}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 1573}#))
                     #{tmp\ 1575}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1574}#)))
          ($sc-dispatch #{tmp\ 1574}# (quote (any any)))))
       #{x\ 1573}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 1578}#)
      ((lambda (#{tmp\ 1579}#)
         ((lambda (#{tmp\ 1580}#)
            (if #{tmp\ 1580}#
              (apply (lambda (#{_\ 1581}#
                              #{e\ 1582}#
                              #{m1\ 1583}#
                              #{m2\ 1584}#)
                       ((lambda (#{tmp\ 1585}#)
                          ((lambda (#{body\ 1586}#)
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
                                               #{e\ 1582}#))
                                   #{body\ 1586}#))
                           #{tmp\ 1585}#))
                        (letrec ((#{f\ 1587}#
                                   (lambda (#{clause\ 1588}# #{clauses\ 1589}#)
                                     (if (null? #{clauses\ 1589}#)
                                       ((lambda (#{tmp\ 1591}#)
                                          ((lambda (#{tmp\ 1592}#)
                                             (if #{tmp\ 1592}#
                                               (apply (lambda (#{e1\ 1593}#
                                                               #{e2\ 1594}#)
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
                                                              (cons #{e1\ 1593}#
                                                                    #{e2\ 1594}#)))
                                                      #{tmp\ 1592}#)
                                               ((lambda (#{tmp\ 1596}#)
                                                  (if #{tmp\ 1596}#
                                                    (apply (lambda (#{k\ 1597}#
                                                                    #{e1\ 1598}#
                                                                    #{e2\ 1599}#)
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
                                                                               #{k\ 1597}#))
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
                                                                         (cons #{e1\ 1598}#
                                                                               #{e2\ 1599}#))))
                                                           #{tmp\ 1596}#)
                                                    ((lambda (#{_\ 1602}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 1578}#
                                                         #{clause\ 1588}#))
                                                     #{tmp\ 1591}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 1591}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 1591}#
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
                                        #{clause\ 1588}#)
                                       ((lambda (#{tmp\ 1603}#)
                                          ((lambda (#{rest\ 1604}#)
                                             ((lambda (#{tmp\ 1605}#)
                                                ((lambda (#{tmp\ 1606}#)
                                                   (if #{tmp\ 1606}#
                                                     (apply (lambda (#{k\ 1607}#
                                                                     #{e1\ 1608}#
                                                                     #{e2\ 1609}#)
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
                                                                                #{k\ 1607}#))
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
                                                                          (cons #{e1\ 1608}#
                                                                                #{e2\ 1609}#))
                                                                    #{rest\ 1604}#))
                                                            #{tmp\ 1606}#)
                                                     ((lambda (#{_\ 1612}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 1578}#
                                                          #{clause\ 1588}#))
                                                      #{tmp\ 1605}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1605}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 1588}#))
                                           #{tmp\ 1603}#))
                                        (#{f\ 1587}#
                                          (car #{clauses\ 1589}#)
                                          (cdr #{clauses\ 1589}#)))))))
                          (#{f\ 1587}# #{m1\ 1583}# #{m2\ 1584}#))))
                     #{tmp\ 1580}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1579}#)))
          ($sc-dispatch
            #{tmp\ 1579}#
            '(any any any . each-any))))
       #{x\ 1578}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 1613}#)
      ((lambda (#{tmp\ 1614}#)
         ((lambda (#{tmp\ 1615}#)
            (if #{tmp\ 1615}#
              (apply (lambda (#{_\ 1616}# #{e\ 1617}#)
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
                                               #{e\ 1617}#))
                                   (list (cons #{_\ 1616}#
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
                                               (cons #{e\ 1617}#
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
                     #{tmp\ 1615}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 1614}#)))
          ($sc-dispatch #{tmp\ 1614}# (quote (any any)))))
       #{x\ 1613}#))))

