(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((#{and-map*\ 3154}#
           (lambda (#{f\ 3192}# #{first\ 3193}# . #{rest\ 3194}#)
             (let ((#{t\ 3195}# (null? #{first\ 3193}#)))
               (if #{t\ 3195}#
                 #{t\ 3195}#
                 (if (null? #{rest\ 3194}#)
                   (letrec ((#{andmap\ 3196}#
                              (lambda (#{first\ 3197}#)
                                (let ((#{x\ 3198}# (car #{first\ 3197}#))
                                      (#{first\ 3199}# (cdr #{first\ 3197}#)))
                                  (if (null? #{first\ 3199}#)
                                    (#{f\ 3192}# #{x\ 3198}#)
                                    (if (#{f\ 3192}# #{x\ 3198}#)
                                      (#{andmap\ 3196}# #{first\ 3199}#)
                                      #f))))))
                     (#{andmap\ 3196}# #{first\ 3193}#))
                   (letrec ((#{andmap\ 3200}#
                              (lambda (#{first\ 3201}# #{rest\ 3202}#)
                                (let ((#{x\ 3203}# (car #{first\ 3201}#))
                                      (#{xr\ 3204}# (map car #{rest\ 3202}#))
                                      (#{first\ 3205}# (cdr #{first\ 3201}#))
                                      (#{rest\ 3206}#
                                        (map cdr #{rest\ 3202}#)))
                                  (if (null? #{first\ 3205}#)
                                    (apply #{f\ 3192}#
                                           (cons #{x\ 3203}# #{xr\ 3204}#))
                                    (if (apply #{f\ 3192}#
                                               (cons #{x\ 3203}# #{xr\ 3204}#))
                                      (#{andmap\ 3200}#
                                        #{first\ 3205}#
                                        #{rest\ 3206}#)
                                      #f))))))
                     (#{andmap\ 3200}# #{first\ 3193}# #{rest\ 3194}#))))))))
  (letrec ((#{lambda-var-list\ 3301}#
             (lambda (#{vars\ 3425}#)
               (letrec ((#{lvl\ 3426}#
                          (lambda (#{vars\ 3427}# #{ls\ 3428}# #{w\ 3429}#)
                            (if (pair? #{vars\ 3427}#)
                              (#{lvl\ 3426}#
                                (cdr #{vars\ 3427}#)
                                (cons (#{wrap\ 3282}#
                                        (car #{vars\ 3427}#)
                                        #{w\ 3429}#
                                        #f)
                                      #{ls\ 3428}#)
                                #{w\ 3429}#)
                              (if (#{id?\ 3254}# #{vars\ 3427}#)
                                (cons (#{wrap\ 3282}#
                                        #{vars\ 3427}#
                                        #{w\ 3429}#
                                        #f)
                                      #{ls\ 3428}#)
                                (if (null? #{vars\ 3427}#)
                                  #{ls\ 3428}#
                                  (if (#{syntax-object?\ 3238}# #{vars\ 3427}#)
                                    (#{lvl\ 3426}#
                                      (#{syntax-object-expression\ 3239}#
                                        #{vars\ 3427}#)
                                      #{ls\ 3428}#
                                      (#{join-wraps\ 3273}#
                                        #{w\ 3429}#
                                        (#{syntax-object-wrap\ 3240}#
                                          #{vars\ 3427}#)))
                                    (cons #{vars\ 3427}# #{ls\ 3428}#))))))))
                 (#{lvl\ 3426}#
                   #{vars\ 3425}#
                   '()
                   '(())))))
           (#{gen-var\ 3300}#
             (lambda (#{id\ 3430}#)
               (let ((#{id\ 3431}#
                       (if (#{syntax-object?\ 3238}# #{id\ 3430}#)
                         (#{syntax-object-expression\ 3239}# #{id\ 3430}#)
                         #{id\ 3430}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 3431}#) " ")))))
           (#{strip\ 3299}#
             (lambda (#{x\ 3432}# #{w\ 3433}#)
               (if (memq 'top
                         (#{wrap-marks\ 3257}# #{w\ 3433}#))
                 #{x\ 3432}#
                 (letrec ((#{f\ 3434}#
                            (lambda (#{x\ 3435}#)
                              (if (#{syntax-object?\ 3238}# #{x\ 3435}#)
                                (#{strip\ 3299}#
                                  (#{syntax-object-expression\ 3239}#
                                    #{x\ 3435}#)
                                  (#{syntax-object-wrap\ 3240}# #{x\ 3435}#))
                                (if (pair? #{x\ 3435}#)
                                  (let ((#{a\ 3436}#
                                          (#{f\ 3434}# (car #{x\ 3435}#)))
                                        (#{d\ 3437}#
                                          (#{f\ 3434}# (cdr #{x\ 3435}#))))
                                    (if (if (eq? #{a\ 3436}# (car #{x\ 3435}#))
                                          (eq? #{d\ 3437}# (cdr #{x\ 3435}#))
                                          #f)
                                      #{x\ 3435}#
                                      (cons #{a\ 3436}# #{d\ 3437}#)))
                                  (if (vector? #{x\ 3435}#)
                                    (let ((#{old\ 3438}#
                                            (vector->list #{x\ 3435}#)))
                                      (let ((#{new\ 3439}#
                                              (map #{f\ 3434}# #{old\ 3438}#)))
                                        (if (#{and-map*\ 3154}#
                                              eq?
                                              #{old\ 3438}#
                                              #{new\ 3439}#)
                                          #{x\ 3435}#
                                          (list->vector #{new\ 3439}#))))
                                    #{x\ 3435}#))))))
                   (#{f\ 3434}# #{x\ 3432}#)))))
           (#{ellipsis?\ 3298}#
             (lambda (#{x\ 3440}#)
               (if (#{nonsymbol-id?\ 3253}# #{x\ 3440}#)
                 (#{free-id=?\ 3277}#
                   #{x\ 3440}#
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
           (#{chi-void\ 3297}#
             (lambda () (#{build-void\ 3218}# #f)))
           (#{eval-local-transformer\ 3296}#
             (lambda (#{expanded\ 3441}# #{mod\ 3442}#)
               (let ((#{p\ 3443}#
                       (#{local-eval-hook\ 3214}#
                         #{expanded\ 3441}#
                         #{mod\ 3442}#)))
                 (if (procedure? #{p\ 3443}#)
                   #{p\ 3443}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 3443}#)))))
           (#{chi-local-syntax\ 3295}#
             (lambda (#{rec?\ 3444}#
                      #{e\ 3445}#
                      #{r\ 3446}#
                      #{w\ 3447}#
                      #{s\ 3448}#
                      #{mod\ 3449}#
                      #{k\ 3450}#)
               ((lambda (#{tmp\ 3451}#)
                  ((lambda (#{tmp\ 3452}#)
                     (if #{tmp\ 3452}#
                       (apply (lambda (#{_\ 3453}#
                                       #{id\ 3454}#
                                       #{val\ 3455}#
                                       #{e1\ 3456}#
                                       #{e2\ 3457}#)
                                (let ((#{ids\ 3458}# #{id\ 3454}#))
                                  (if (not (#{valid-bound-ids?\ 3279}#
                                             #{ids\ 3458}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 3445}#)
                                    (let ((#{labels\ 3460}#
                                            (#{gen-labels\ 3260}#
                                              #{ids\ 3458}#)))
                                      (let ((#{new-w\ 3461}#
                                              (#{make-binding-wrap\ 3271}#
                                                #{ids\ 3458}#
                                                #{labels\ 3460}#
                                                #{w\ 3447}#)))
                                        (#{k\ 3450}#
                                          (cons #{e1\ 3456}# #{e2\ 3457}#)
                                          (#{extend-env\ 3248}#
                                            #{labels\ 3460}#
                                            (let ((#{w\ 3463}#
                                                    (if #{rec?\ 3444}#
                                                      #{new-w\ 3461}#
                                                      #{w\ 3447}#))
                                                  (#{trans-r\ 3464}#
                                                    (#{macros-only-env\ 3250}#
                                                      #{r\ 3446}#)))
                                              (map (lambda (#{x\ 3465}#)
                                                     (cons 'macro
                                                           (#{eval-local-transformer\ 3296}#
                                                             (#{chi\ 3290}#
                                                               #{x\ 3465}#
                                                               #{trans-r\ 3464}#
                                                               #{w\ 3463}#
                                                               #{mod\ 3449}#)
                                                             #{mod\ 3449}#)))
                                                   #{val\ 3455}#))
                                            #{r\ 3446}#)
                                          #{new-w\ 3461}#
                                          #{s\ 3448}#
                                          #{mod\ 3449}#))))))
                              #{tmp\ 3452}#)
                       ((lambda (#{_\ 3467}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 3283}#
                              #{e\ 3445}#
                              #{w\ 3447}#
                              #{s\ 3448}#
                              #{mod\ 3449}#)))
                        #{tmp\ 3451}#)))
                   ($sc-dispatch
                     #{tmp\ 3451}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 3445}#)))
           (#{chi-body\ 3294}#
             (lambda (#{body\ 3468}#
                      #{outer-form\ 3469}#
                      #{r\ 3470}#
                      #{w\ 3471}#
                      #{mod\ 3472}#)
               (let ((#{r\ 3473}#
                       (cons '("placeholder" placeholder)
                             #{r\ 3470}#)))
                 (let ((#{ribcage\ 3474}#
                         (#{make-ribcage\ 3261}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 3475}#
                           (#{make-wrap\ 3256}#
                             (#{wrap-marks\ 3257}# #{w\ 3471}#)
                             (cons #{ribcage\ 3474}#
                                   (#{wrap-subst\ 3258}# #{w\ 3471}#)))))
                     (letrec ((#{parse\ 3476}#
                                (lambda (#{body\ 3477}#
                                         #{ids\ 3478}#
                                         #{labels\ 3479}#
                                         #{var-ids\ 3480}#
                                         #{vars\ 3481}#
                                         #{vals\ 3482}#
                                         #{bindings\ 3483}#)
                                  (if (null? #{body\ 3477}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 3469}#)
                                    (let ((#{e\ 3485}# (cdar #{body\ 3477}#))
                                          (#{er\ 3486}# (caar #{body\ 3477}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 3288}#
                                            #{e\ 3485}#
                                            #{er\ 3486}#
                                            '(())
                                            (#{source-annotation\ 3245}#
                                              #{er\ 3486}#)
                                            #{ribcage\ 3474}#
                                            #{mod\ 3472}#
                                            #f))
                                        (lambda (#{type\ 3487}#
                                                 #{value\ 3488}#
                                                 #{e\ 3489}#
                                                 #{w\ 3490}#
                                                 #{s\ 3491}#
                                                 #{mod\ 3492}#)
                                          (if (memv #{type\ 3487}#
                                                    '(define-form))
                                            (let ((#{id\ 3493}#
                                                    (#{wrap\ 3282}#
                                                      #{value\ 3488}#
                                                      #{w\ 3490}#
                                                      #{mod\ 3492}#))
                                                  (#{label\ 3494}#
                                                    (#{gen-label\ 3259}#)))
                                              (let ((#{var\ 3495}#
                                                      (#{gen-var\ 3300}#
                                                        #{id\ 3493}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 3270}#
                                                    #{ribcage\ 3474}#
                                                    #{id\ 3493}#
                                                    #{label\ 3494}#)
                                                  (#{parse\ 3476}#
                                                    (cdr #{body\ 3477}#)
                                                    (cons #{id\ 3493}#
                                                          #{ids\ 3478}#)
                                                    (cons #{label\ 3494}#
                                                          #{labels\ 3479}#)
                                                    (cons #{id\ 3493}#
                                                          #{var-ids\ 3480}#)
                                                    (cons #{var\ 3495}#
                                                          #{vars\ 3481}#)
                                                    (cons (cons #{er\ 3486}#
                                                                (#{wrap\ 3282}#
                                                                  #{e\ 3489}#
                                                                  #{w\ 3490}#
                                                                  #{mod\ 3492}#))
                                                          #{vals\ 3482}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 3495}#)
                                                          #{bindings\ 3483}#)))))
                                            (if (memv #{type\ 3487}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 3496}#
                                                      (#{wrap\ 3282}#
                                                        #{value\ 3488}#
                                                        #{w\ 3490}#
                                                        #{mod\ 3492}#))
                                                    (#{label\ 3497}#
                                                      (#{gen-label\ 3259}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 3270}#
                                                    #{ribcage\ 3474}#
                                                    #{id\ 3496}#
                                                    #{label\ 3497}#)
                                                  (#{parse\ 3476}#
                                                    (cdr #{body\ 3477}#)
                                                    (cons #{id\ 3496}#
                                                          #{ids\ 3478}#)
                                                    (cons #{label\ 3497}#
                                                          #{labels\ 3479}#)
                                                    #{var-ids\ 3480}#
                                                    #{vars\ 3481}#
                                                    #{vals\ 3482}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 3486}#
                                                                      (#{wrap\ 3282}#
                                                                        #{e\ 3489}#
                                                                        #{w\ 3490}#
                                                                        #{mod\ 3492}#)))
                                                          #{bindings\ 3483}#))))
                                              (if (memv #{type\ 3487}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 3498}#)
                                                   ((lambda (#{tmp\ 3499}#)
                                                      (if #{tmp\ 3499}#
                                                        (apply (lambda (#{_\ 3500}#
                                                                        #{e1\ 3501}#)
                                                                 (#{parse\ 3476}#
                                                                   (letrec ((#{f\ 3502}#
                                                                              (lambda (#{forms\ 3503}#)
                                                                                (if (null? #{forms\ 3503}#)
                                                                                  (cdr #{body\ 3477}#)
                                                                                  (cons (cons #{er\ 3486}#
                                                                                              (#{wrap\ 3282}#
                                                                                                (car #{forms\ 3503}#)
                                                                                                #{w\ 3490}#
                                                                                                #{mod\ 3492}#))
                                                                                        (#{f\ 3502}#
                                                                                          (cdr #{forms\ 3503}#)))))))
                                                                     (#{f\ 3502}#
                                                                       #{e1\ 3501}#))
                                                                   #{ids\ 3478}#
                                                                   #{labels\ 3479}#
                                                                   #{var-ids\ 3480}#
                                                                   #{vars\ 3481}#
                                                                   #{vals\ 3482}#
                                                                   #{bindings\ 3483}#))
                                                               #{tmp\ 3499}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 3498}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 3498}#
                                                      '(any . each-any))))
                                                 #{e\ 3489}#)
                                                (if (memv #{type\ 3487}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 3295}#
                                                    #{value\ 3488}#
                                                    #{e\ 3489}#
                                                    #{er\ 3486}#
                                                    #{w\ 3490}#
                                                    #{s\ 3491}#
                                                    #{mod\ 3492}#
                                                    (lambda (#{forms\ 3505}#
                                                             #{er\ 3506}#
                                                             #{w\ 3507}#
                                                             #{s\ 3508}#
                                                             #{mod\ 3509}#)
                                                      (#{parse\ 3476}#
                                                        (letrec ((#{f\ 3510}#
                                                                   (lambda (#{forms\ 3511}#)
                                                                     (if (null? #{forms\ 3511}#)
                                                                       (cdr #{body\ 3477}#)
                                                                       (cons (cons #{er\ 3506}#
                                                                                   (#{wrap\ 3282}#
                                                                                     (car #{forms\ 3511}#)
                                                                                     #{w\ 3507}#
                                                                                     #{mod\ 3509}#))
                                                                             (#{f\ 3510}#
                                                                               (cdr #{forms\ 3511}#)))))))
                                                          (#{f\ 3510}#
                                                            #{forms\ 3505}#))
                                                        #{ids\ 3478}#
                                                        #{labels\ 3479}#
                                                        #{var-ids\ 3480}#
                                                        #{vars\ 3481}#
                                                        #{vals\ 3482}#
                                                        #{bindings\ 3483}#)))
                                                  (if (null? #{ids\ 3478}#)
                                                    (#{build-sequence\ 3233}#
                                                      #f
                                                      (map (lambda (#{x\ 3512}#)
                                                             (#{chi\ 3290}#
                                                               (cdr #{x\ 3512}#)
                                                               (car #{x\ 3512}#)
                                                               '(())
                                                               #{mod\ 3492}#))
                                                           (cons (cons #{er\ 3486}#
                                                                       (#{source-wrap\ 3283}#
                                                                         #{e\ 3489}#
                                                                         #{w\ 3490}#
                                                                         #{s\ 3491}#
                                                                         #{mod\ 3492}#))
                                                                 (cdr #{body\ 3477}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 3279}#
                                                                 #{ids\ 3478}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 3469}#))
                                                      (letrec ((#{loop\ 3513}#
                                                                 (lambda (#{bs\ 3514}#
                                                                          #{er-cache\ 3515}#
                                                                          #{r-cache\ 3516}#)
                                                                   (if (not (null? #{bs\ 3514}#))
                                                                     (let ((#{b\ 3517}#
                                                                             (car #{bs\ 3514}#)))
                                                                       (if (eq? (car #{b\ 3517}#)
                                                                                'macro)
                                                                         (let ((#{er\ 3518}#
                                                                                 (cadr #{b\ 3517}#)))
                                                                           (let ((#{r-cache\ 3519}#
                                                                                   (if (eq? #{er\ 3518}#
                                                                                            #{er-cache\ 3515}#)
                                                                                     #{r-cache\ 3516}#
                                                                                     (#{macros-only-env\ 3250}#
                                                                                       #{er\ 3518}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 3517}#
                                                                                 (#{eval-local-transformer\ 3296}#
                                                                                   (#{chi\ 3290}#
                                                                                     (cddr #{b\ 3517}#)
                                                                                     #{r-cache\ 3519}#
                                                                                     '(())
                                                                                     #{mod\ 3492}#)
                                                                                   #{mod\ 3492}#))
                                                                               (#{loop\ 3513}#
                                                                                 (cdr #{bs\ 3514}#)
                                                                                 #{er\ 3518}#
                                                                                 #{r-cache\ 3519}#))))
                                                                         (#{loop\ 3513}#
                                                                           (cdr #{bs\ 3514}#)
                                                                           #{er-cache\ 3515}#
                                                                           #{r-cache\ 3516}#)))))))
                                                        (#{loop\ 3513}#
                                                          #{bindings\ 3483}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 3473}#
                                                        (#{extend-env\ 3248}#
                                                          #{labels\ 3479}#
                                                          #{bindings\ 3483}#
                                                          (cdr #{r\ 3473}#)))
                                                      (#{build-letrec\ 3236}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 3480}#)
                                                        #{vars\ 3481}#
                                                        (map (lambda (#{x\ 3520}#)
                                                               (#{chi\ 3290}#
                                                                 (cdr #{x\ 3520}#)
                                                                 (car #{x\ 3520}#)
                                                                 '(())
                                                                 #{mod\ 3492}#))
                                                             #{vals\ 3482}#)
                                                        (#{build-sequence\ 3233}#
                                                          #f
                                                          (map (lambda (#{x\ 3521}#)
                                                                 (#{chi\ 3290}#
                                                                   (cdr #{x\ 3521}#)
                                                                   (car #{x\ 3521}#)
                                                                   '(())
                                                                   #{mod\ 3492}#))
                                                               (cons (cons #{er\ 3486}#
                                                                           (#{source-wrap\ 3283}#
                                                                             #{e\ 3489}#
                                                                             #{w\ 3490}#
                                                                             #{s\ 3491}#
                                                                             #{mod\ 3492}#))
                                                                     (cdr #{body\ 3477}#))))))))))))))))))
                       (#{parse\ 3476}#
                         (map (lambda (#{x\ 3484}#)
                                (cons #{r\ 3473}#
                                      (#{wrap\ 3282}#
                                        #{x\ 3484}#
                                        #{w\ 3475}#
                                        #{mod\ 3472}#)))
                              #{body\ 3468}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 3293}#
             (lambda (#{p\ 3522}#
                      #{e\ 3523}#
                      #{r\ 3524}#
                      #{w\ 3525}#
                      #{rib\ 3526}#
                      #{mod\ 3527}#)
               (letrec ((#{rebuild-macro-output\ 3528}#
                          (lambda (#{x\ 3529}# #{m\ 3530}#)
                            (if (pair? #{x\ 3529}#)
                              (cons (#{rebuild-macro-output\ 3528}#
                                      (car #{x\ 3529}#)
                                      #{m\ 3530}#)
                                    (#{rebuild-macro-output\ 3528}#
                                      (cdr #{x\ 3529}#)
                                      #{m\ 3530}#))
                              (if (#{syntax-object?\ 3238}# #{x\ 3529}#)
                                (let ((#{w\ 3531}#
                                        (#{syntax-object-wrap\ 3240}#
                                          #{x\ 3529}#)))
                                  (let ((#{ms\ 3532}#
                                          (#{wrap-marks\ 3257}# #{w\ 3531}#))
                                        (#{s\ 3533}#
                                          (#{wrap-subst\ 3258}# #{w\ 3531}#)))
                                    (if (if (pair? #{ms\ 3532}#)
                                          (eq? (car #{ms\ 3532}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 3237}#
                                        (#{syntax-object-expression\ 3239}#
                                          #{x\ 3529}#)
                                        (#{make-wrap\ 3256}#
                                          (cdr #{ms\ 3532}#)
                                          (if #{rib\ 3526}#
                                            (cons #{rib\ 3526}#
                                                  (cdr #{s\ 3533}#))
                                            (cdr #{s\ 3533}#)))
                                        (#{syntax-object-module\ 3241}#
                                          #{x\ 3529}#))
                                      (#{make-syntax-object\ 3237}#
                                        (#{syntax-object-expression\ 3239}#
                                          #{x\ 3529}#)
                                        (#{make-wrap\ 3256}#
                                          (cons #{m\ 3530}# #{ms\ 3532}#)
                                          (if #{rib\ 3526}#
                                            (cons #{rib\ 3526}#
                                                  (cons 'shift
                                                        #{s\ 3533}#))
                                            (cons (quote shift) #{s\ 3533}#)))
                                        (let ((#{pmod\ 3534}#
                                                (procedure-module
                                                  #{p\ 3522}#)))
                                          (if #{pmod\ 3534}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 3534}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 3529}#)
                                  (let ((#{n\ 3535}#
                                          (vector-length #{x\ 3529}#)))
                                    (let ((#{v\ 3536}#
                                            (make-vector #{n\ 3535}#)))
                                      (letrec ((#{loop\ 3537}#
                                                 (lambda (#{i\ 3538}#)
                                                   (if (#{fx=\ 3211}#
                                                         #{i\ 3538}#
                                                         #{n\ 3535}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 3536}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 3536}#
                                                         #{i\ 3538}#
                                                         (#{rebuild-macro-output\ 3528}#
                                                           (vector-ref
                                                             #{x\ 3529}#
                                                             #{i\ 3538}#)
                                                           #{m\ 3530}#))
                                                       (#{loop\ 3537}#
                                                         (#{fx+\ 3209}#
                                                           #{i\ 3538}#
                                                           1)))))))
                                        (#{loop\ 3537}# 0))))
                                  (if (symbol? #{x\ 3529}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 3283}#
                                        #{e\ 3523}#
                                        #{w\ 3525}#
                                        (#{wrap-subst\ 3258}# #{w\ 3525}#)
                                        #{mod\ 3527}#)
                                      #{x\ 3529}#)
                                    #{x\ 3529}#)))))))
                 (#{rebuild-macro-output\ 3528}#
                   (#{p\ 3522}#
                     (#{wrap\ 3282}#
                       #{e\ 3523}#
                       (#{anti-mark\ 3269}# #{w\ 3525}#)
                       #{mod\ 3527}#))
                   (string #\m)))))
           (#{chi-application\ 3292}#
             (lambda (#{x\ 3539}#
                      #{e\ 3540}#
                      #{r\ 3541}#
                      #{w\ 3542}#
                      #{s\ 3543}#
                      #{mod\ 3544}#)
               ((lambda (#{tmp\ 3545}#)
                  ((lambda (#{tmp\ 3546}#)
                     (if #{tmp\ 3546}#
                       (apply (lambda (#{e0\ 3547}# #{e1\ 3548}#)
                                (#{build-application\ 3219}#
                                  #{s\ 3543}#
                                  #{x\ 3539}#
                                  (map (lambda (#{e\ 3549}#)
                                         (#{chi\ 3290}#
                                           #{e\ 3549}#
                                           #{r\ 3541}#
                                           #{w\ 3542}#
                                           #{mod\ 3544}#))
                                       #{e1\ 3548}#)))
                              #{tmp\ 3546}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 3545}#)))
                   ($sc-dispatch
                     #{tmp\ 3545}#
                     '(any . each-any))))
                #{e\ 3540}#)))
           (#{chi-expr\ 3291}#
             (lambda (#{type\ 3551}#
                      #{value\ 3552}#
                      #{e\ 3553}#
                      #{r\ 3554}#
                      #{w\ 3555}#
                      #{s\ 3556}#
                      #{mod\ 3557}#)
               (if (memv #{type\ 3551}# (quote (lexical)))
                 (#{build-lexical-reference\ 3221}#
                   'value
                   #{s\ 3556}#
                   #{e\ 3553}#
                   #{value\ 3552}#)
                 (if (memv #{type\ 3551}# (quote (core core-form)))
                   (#{value\ 3552}#
                     #{e\ 3553}#
                     #{r\ 3554}#
                     #{w\ 3555}#
                     #{s\ 3556}#
                     #{mod\ 3557}#)
                   (if (memv #{type\ 3551}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 3552}# #{e\ 3553}#))
                       (lambda (#{id\ 3558}# #{mod\ 3559}#)
                         (#{build-global-reference\ 3224}#
                           #{s\ 3556}#
                           #{id\ 3558}#
                           #{mod\ 3559}#)))
                     (if (memv #{type\ 3551}# (quote (lexical-call)))
                       (#{chi-application\ 3292}#
                         (#{build-lexical-reference\ 3221}#
                           'fun
                           (#{source-annotation\ 3245}# (car #{e\ 3553}#))
                           (car #{e\ 3553}#)
                           #{value\ 3552}#)
                         #{e\ 3553}#
                         #{r\ 3554}#
                         #{w\ 3555}#
                         #{s\ 3556}#
                         #{mod\ 3557}#)
                       (if (memv #{type\ 3551}# (quote (global-call)))
                         (#{chi-application\ 3292}#
                           (#{build-global-reference\ 3224}#
                             (#{source-annotation\ 3245}# (car #{e\ 3553}#))
                             (if (#{syntax-object?\ 3238}# #{value\ 3552}#)
                               (#{syntax-object-expression\ 3239}#
                                 #{value\ 3552}#)
                               #{value\ 3552}#)
                             (if (#{syntax-object?\ 3238}# #{value\ 3552}#)
                               (#{syntax-object-module\ 3241}# #{value\ 3552}#)
                               #{mod\ 3557}#))
                           #{e\ 3553}#
                           #{r\ 3554}#
                           #{w\ 3555}#
                           #{s\ 3556}#
                           #{mod\ 3557}#)
                         (if (memv #{type\ 3551}# (quote (constant)))
                           (#{build-data\ 3232}#
                             #{s\ 3556}#
                             (#{strip\ 3299}#
                               (#{source-wrap\ 3283}#
                                 #{e\ 3553}#
                                 #{w\ 3555}#
                                 #{s\ 3556}#
                                 #{mod\ 3557}#)
                               '(())))
                           (if (memv #{type\ 3551}# (quote (global)))
                             (#{build-global-reference\ 3224}#
                               #{s\ 3556}#
                               #{value\ 3552}#
                               #{mod\ 3557}#)
                             (if (memv #{type\ 3551}# (quote (call)))
                               (#{chi-application\ 3292}#
                                 (#{chi\ 3290}#
                                   (car #{e\ 3553}#)
                                   #{r\ 3554}#
                                   #{w\ 3555}#
                                   #{mod\ 3557}#)
                                 #{e\ 3553}#
                                 #{r\ 3554}#
                                 #{w\ 3555}#
                                 #{s\ 3556}#
                                 #{mod\ 3557}#)
                               (if (memv #{type\ 3551}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 3560}#)
                                    ((lambda (#{tmp\ 3561}#)
                                       (if #{tmp\ 3561}#
                                         (apply (lambda (#{_\ 3562}#
                                                         #{e1\ 3563}#
                                                         #{e2\ 3564}#)
                                                  (#{chi-sequence\ 3284}#
                                                    (cons #{e1\ 3563}#
                                                          #{e2\ 3564}#)
                                                    #{r\ 3554}#
                                                    #{w\ 3555}#
                                                    #{s\ 3556}#
                                                    #{mod\ 3557}#))
                                                #{tmp\ 3561}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 3560}#)))
                                     ($sc-dispatch
                                       #{tmp\ 3560}#
                                       '(any any . each-any))))
                                  #{e\ 3553}#)
                                 (if (memv #{type\ 3551}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 3295}#
                                     #{value\ 3552}#
                                     #{e\ 3553}#
                                     #{r\ 3554}#
                                     #{w\ 3555}#
                                     #{s\ 3556}#
                                     #{mod\ 3557}#
                                     #{chi-sequence\ 3284}#)
                                   (if (memv #{type\ 3551}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 3566}#)
                                        ((lambda (#{tmp\ 3567}#)
                                           (if #{tmp\ 3567}#
                                             (apply (lambda (#{_\ 3568}#
                                                             #{x\ 3569}#
                                                             #{e1\ 3570}#
                                                             #{e2\ 3571}#)
                                                      (let ((#{when-list\ 3572}#
                                                              (#{chi-when-list\ 3287}#
                                                                #{e\ 3553}#
                                                                #{x\ 3569}#
                                                                #{w\ 3555}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 3572}#)
                                                          (#{chi-sequence\ 3284}#
                                                            (cons #{e1\ 3570}#
                                                                  #{e2\ 3571}#)
                                                            #{r\ 3554}#
                                                            #{w\ 3555}#
                                                            #{s\ 3556}#
                                                            #{mod\ 3557}#)
                                                          (#{chi-void\ 3297}#))))
                                                    #{tmp\ 3567}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 3566}#)))
                                         ($sc-dispatch
                                           #{tmp\ 3566}#
                                           '(any each-any any . each-any))))
                                      #{e\ 3553}#)
                                     (if (memv #{type\ 3551}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 3553}#
                                         (#{wrap\ 3282}#
                                           #{value\ 3552}#
                                           #{w\ 3555}#
                                           #{mod\ 3557}#))
                                       (if (memv #{type\ 3551}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 3283}#
                                             #{e\ 3553}#
                                             #{w\ 3555}#
                                             #{s\ 3556}#
                                             #{mod\ 3557}#))
                                         (if (memv #{type\ 3551}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 3283}#
                                               #{e\ 3553}#
                                               #{w\ 3555}#
                                               #{s\ 3556}#
                                               #{mod\ 3557}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 3283}#
                                               #{e\ 3553}#
                                               #{w\ 3555}#
                                               #{s\ 3556}#
                                               #{mod\ 3557}#))))))))))))))))))
           (#{chi\ 3290}#
             (lambda (#{e\ 3575}#
                      #{r\ 3576}#
                      #{w\ 3577}#
                      #{mod\ 3578}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 3288}#
                     #{e\ 3575}#
                     #{r\ 3576}#
                     #{w\ 3577}#
                     (#{source-annotation\ 3245}# #{e\ 3575}#)
                     #f
                     #{mod\ 3578}#
                     #f))
                 (lambda (#{type\ 3579}#
                          #{value\ 3580}#
                          #{e\ 3581}#
                          #{w\ 3582}#
                          #{s\ 3583}#
                          #{mod\ 3584}#)
                   (#{chi-expr\ 3291}#
                     #{type\ 3579}#
                     #{value\ 3580}#
                     #{e\ 3581}#
                     #{r\ 3576}#
                     #{w\ 3582}#
                     #{s\ 3583}#
                     #{mod\ 3584}#)))))
           (#{chi-top\ 3289}#
             (lambda (#{e\ 3585}#
                      #{r\ 3586}#
                      #{w\ 3587}#
                      #{m\ 3588}#
                      #{esew\ 3589}#
                      #{mod\ 3590}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 3288}#
                     #{e\ 3585}#
                     #{r\ 3586}#
                     #{w\ 3587}#
                     (#{source-annotation\ 3245}# #{e\ 3585}#)
                     #f
                     #{mod\ 3590}#
                     #f))
                 (lambda (#{type\ 3598}#
                          #{value\ 3599}#
                          #{e\ 3600}#
                          #{w\ 3601}#
                          #{s\ 3602}#
                          #{mod\ 3603}#)
                   (if (memv #{type\ 3598}# (quote (begin-form)))
                     ((lambda (#{tmp\ 3604}#)
                        ((lambda (#{tmp\ 3605}#)
                           (if #{tmp\ 3605}#
                             (apply (lambda (#{_\ 3606}#) (#{chi-void\ 3297}#))
                                    #{tmp\ 3605}#)
                             ((lambda (#{tmp\ 3607}#)
                                (if #{tmp\ 3607}#
                                  (apply (lambda (#{_\ 3608}#
                                                  #{e1\ 3609}#
                                                  #{e2\ 3610}#)
                                           (#{chi-top-sequence\ 3285}#
                                             (cons #{e1\ 3609}# #{e2\ 3610}#)
                                             #{r\ 3586}#
                                             #{w\ 3601}#
                                             #{s\ 3602}#
                                             #{m\ 3588}#
                                             #{esew\ 3589}#
                                             #{mod\ 3603}#))
                                         #{tmp\ 3607}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 3604}#)))
                              ($sc-dispatch
                                #{tmp\ 3604}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 3604}# (quote (any)))))
                      #{e\ 3600}#)
                     (if (memv #{type\ 3598}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 3295}#
                         #{value\ 3599}#
                         #{e\ 3600}#
                         #{r\ 3586}#
                         #{w\ 3601}#
                         #{s\ 3602}#
                         #{mod\ 3603}#
                         (lambda (#{body\ 3612}#
                                  #{r\ 3613}#
                                  #{w\ 3614}#
                                  #{s\ 3615}#
                                  #{mod\ 3616}#)
                           (#{chi-top-sequence\ 3285}#
                             #{body\ 3612}#
                             #{r\ 3613}#
                             #{w\ 3614}#
                             #{s\ 3615}#
                             #{m\ 3588}#
                             #{esew\ 3589}#
                             #{mod\ 3616}#)))
                       (if (memv #{type\ 3598}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 3617}#)
                            ((lambda (#{tmp\ 3618}#)
                               (if #{tmp\ 3618}#
                                 (apply (lambda (#{_\ 3619}#
                                                 #{x\ 3620}#
                                                 #{e1\ 3621}#
                                                 #{e2\ 3622}#)
                                          (let ((#{when-list\ 3623}#
                                                  (#{chi-when-list\ 3287}#
                                                    #{e\ 3600}#
                                                    #{x\ 3620}#
                                                    #{w\ 3601}#))
                                                (#{body\ 3624}#
                                                  (cons #{e1\ 3621}#
                                                        #{e2\ 3622}#)))
                                            (if (eq? #{m\ 3588}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 3623}#)
                                                (#{chi-top-sequence\ 3285}#
                                                  #{body\ 3624}#
                                                  #{r\ 3586}#
                                                  #{w\ 3601}#
                                                  #{s\ 3602}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 3603}#)
                                                (#{chi-void\ 3297}#))
                                              (if (memq 'load
                                                        #{when-list\ 3623}#)
                                                (if (let ((#{t\ 3627}#
                                                            (memq 'compile
                                                                  #{when-list\ 3623}#)))
                                                      (if #{t\ 3627}#
                                                        #{t\ 3627}#
                                                        (if (eq? #{m\ 3588}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 3623}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 3285}#
                                                    #{body\ 3624}#
                                                    #{r\ 3586}#
                                                    #{w\ 3601}#
                                                    #{s\ 3602}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 3603}#)
                                                  (if (memq #{m\ 3588}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 3285}#
                                                      #{body\ 3624}#
                                                      #{r\ 3586}#
                                                      #{w\ 3601}#
                                                      #{s\ 3602}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 3603}#)
                                                    (#{chi-void\ 3297}#)))
                                                (if (let ((#{t\ 3628}#
                                                            (memq 'compile
                                                                  #{when-list\ 3623}#)))
                                                      (if #{t\ 3628}#
                                                        #{t\ 3628}#
                                                        (if (eq? #{m\ 3588}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 3623}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 3213}#
                                                      (#{chi-top-sequence\ 3285}#
                                                        #{body\ 3624}#
                                                        #{r\ 3586}#
                                                        #{w\ 3601}#
                                                        #{s\ 3602}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 3603}#)
                                                      #{mod\ 3603}#)
                                                    (#{chi-void\ 3297}#))
                                                  (#{chi-void\ 3297}#))))))
                                        #{tmp\ 3618}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 3617}#)))
                             ($sc-dispatch
                               #{tmp\ 3617}#
                               '(any each-any any . each-any))))
                          #{e\ 3600}#)
                         (if (memv #{type\ 3598}#
                                   '(define-syntax-form))
                           (let ((#{n\ 3629}#
                                   (#{id-var-name\ 3276}#
                                     #{value\ 3599}#
                                     #{w\ 3601}#))
                                 (#{r\ 3630}#
                                   (#{macros-only-env\ 3250}# #{r\ 3586}#)))
                             (if (memv #{m\ 3588}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 3589}#)
                                 (let ((#{e\ 3631}#
                                         (#{chi-install-global\ 3286}#
                                           #{n\ 3629}#
                                           (#{chi\ 3290}#
                                             #{e\ 3600}#
                                             #{r\ 3630}#
                                             #{w\ 3601}#
                                             #{mod\ 3603}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 3213}#
                                       #{e\ 3631}#
                                       #{mod\ 3603}#)
                                     (if (memq (quote load) #{esew\ 3589}#)
                                       #{e\ 3631}#
                                       (#{chi-void\ 3297}#))))
                                 (if (memq (quote load) #{esew\ 3589}#)
                                   (#{chi-install-global\ 3286}#
                                     #{n\ 3629}#
                                     (#{chi\ 3290}#
                                       #{e\ 3600}#
                                       #{r\ 3630}#
                                       #{w\ 3601}#
                                       #{mod\ 3603}#))
                                   (#{chi-void\ 3297}#)))
                               (if (memv #{m\ 3588}# (quote (c&e)))
                                 (let ((#{e\ 3632}#
                                         (#{chi-install-global\ 3286}#
                                           #{n\ 3629}#
                                           (#{chi\ 3290}#
                                             #{e\ 3600}#
                                             #{r\ 3630}#
                                             #{w\ 3601}#
                                             #{mod\ 3603}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 3213}#
                                       #{e\ 3632}#
                                       #{mod\ 3603}#)
                                     #{e\ 3632}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 3589}#)
                                     (#{top-level-eval-hook\ 3213}#
                                       (#{chi-install-global\ 3286}#
                                         #{n\ 3629}#
                                         (#{chi\ 3290}#
                                           #{e\ 3600}#
                                           #{r\ 3630}#
                                           #{w\ 3601}#
                                           #{mod\ 3603}#))
                                       #{mod\ 3603}#))
                                   (#{chi-void\ 3297}#)))))
                           (if (memv #{type\ 3598}# (quote (define-form)))
                             (let ((#{n\ 3633}#
                                     (#{id-var-name\ 3276}#
                                       #{value\ 3599}#
                                       #{w\ 3601}#)))
                               (let ((#{type\ 3634}#
                                       (#{binding-type\ 3246}#
                                         (#{lookup\ 3251}#
                                           #{n\ 3633}#
                                           #{r\ 3586}#
                                           #{mod\ 3603}#))))
                                 (if (memv #{type\ 3634}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 3633}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 3635}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 3633}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 3633}#
                                           (if (variable? #{old\ 3635}#)
                                             (variable-ref #{old\ 3635}#)
                                             #f))))
                                     (let ((#{x\ 3636}#
                                             (#{build-global-definition\ 3227}#
                                               #{s\ 3602}#
                                               #{n\ 3633}#
                                               (#{chi\ 3290}#
                                                 #{e\ 3600}#
                                                 #{r\ 3586}#
                                                 #{w\ 3601}#
                                                 #{mod\ 3603}#))))
                                       (begin
                                         (if (eq? #{m\ 3588}# (quote c&e))
                                           (#{top-level-eval-hook\ 3213}#
                                             #{x\ 3636}#
                                             #{mod\ 3603}#))
                                         #{x\ 3636}#)))
                                   (if (memv #{type\ 3634}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 3600}#
                                       (#{wrap\ 3282}#
                                         #{value\ 3599}#
                                         #{w\ 3601}#
                                         #{mod\ 3603}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 3600}#
                                       (#{wrap\ 3282}#
                                         #{value\ 3599}#
                                         #{w\ 3601}#
                                         #{mod\ 3603}#))))))
                             (let ((#{x\ 3637}#
                                     (#{chi-expr\ 3291}#
                                       #{type\ 3598}#
                                       #{value\ 3599}#
                                       #{e\ 3600}#
                                       #{r\ 3586}#
                                       #{w\ 3601}#
                                       #{s\ 3602}#
                                       #{mod\ 3603}#)))
                               (begin
                                 (if (eq? #{m\ 3588}# (quote c&e))
                                   (#{top-level-eval-hook\ 3213}#
                                     #{x\ 3637}#
                                     #{mod\ 3603}#))
                                 #{x\ 3637}#)))))))))))
           (#{syntax-type\ 3288}#
             (lambda (#{e\ 3638}#
                      #{r\ 3639}#
                      #{w\ 3640}#
                      #{s\ 3641}#
                      #{rib\ 3642}#
                      #{mod\ 3643}#
                      #{for-car?\ 3644}#)
               (if (symbol? #{e\ 3638}#)
                 (let ((#{n\ 3645}#
                         (#{id-var-name\ 3276}# #{e\ 3638}# #{w\ 3640}#)))
                   (let ((#{b\ 3646}#
                           (#{lookup\ 3251}#
                             #{n\ 3645}#
                             #{r\ 3639}#
                             #{mod\ 3643}#)))
                     (let ((#{type\ 3647}#
                             (#{binding-type\ 3246}# #{b\ 3646}#)))
                       (if (memv #{type\ 3647}# (quote (lexical)))
                         (values
                           #{type\ 3647}#
                           (#{binding-value\ 3247}# #{b\ 3646}#)
                           #{e\ 3638}#
                           #{w\ 3640}#
                           #{s\ 3641}#
                           #{mod\ 3643}#)
                         (if (memv #{type\ 3647}# (quote (global)))
                           (values
                             #{type\ 3647}#
                             #{n\ 3645}#
                             #{e\ 3638}#
                             #{w\ 3640}#
                             #{s\ 3641}#
                             #{mod\ 3643}#)
                           (if (memv #{type\ 3647}# (quote (macro)))
                             (if #{for-car?\ 3644}#
                               (values
                                 #{type\ 3647}#
                                 (#{binding-value\ 3247}# #{b\ 3646}#)
                                 #{e\ 3638}#
                                 #{w\ 3640}#
                                 #{s\ 3641}#
                                 #{mod\ 3643}#)
                               (#{syntax-type\ 3288}#
                                 (#{chi-macro\ 3293}#
                                   (#{binding-value\ 3247}# #{b\ 3646}#)
                                   #{e\ 3638}#
                                   #{r\ 3639}#
                                   #{w\ 3640}#
                                   #{rib\ 3642}#
                                   #{mod\ 3643}#)
                                 #{r\ 3639}#
                                 '(())
                                 #{s\ 3641}#
                                 #{rib\ 3642}#
                                 #{mod\ 3643}#
                                 #f))
                             (values
                               #{type\ 3647}#
                               (#{binding-value\ 3247}# #{b\ 3646}#)
                               #{e\ 3638}#
                               #{w\ 3640}#
                               #{s\ 3641}#
                               #{mod\ 3643}#)))))))
                 (if (pair? #{e\ 3638}#)
                   (let ((#{first\ 3648}# (car #{e\ 3638}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 3288}#
                           #{first\ 3648}#
                           #{r\ 3639}#
                           #{w\ 3640}#
                           #{s\ 3641}#
                           #{rib\ 3642}#
                           #{mod\ 3643}#
                           #t))
                       (lambda (#{ftype\ 3649}#
                                #{fval\ 3650}#
                                #{fe\ 3651}#
                                #{fw\ 3652}#
                                #{fs\ 3653}#
                                #{fmod\ 3654}#)
                         (if (memv #{ftype\ 3649}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 3650}#
                             #{e\ 3638}#
                             #{w\ 3640}#
                             #{s\ 3641}#
                             #{mod\ 3643}#)
                           (if (memv #{ftype\ 3649}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 3237}#
                                 #{fval\ 3650}#
                                 #{w\ 3640}#
                                 #{fmod\ 3654}#)
                               #{e\ 3638}#
                               #{w\ 3640}#
                               #{s\ 3641}#
                               #{mod\ 3643}#)
                             (if (memv #{ftype\ 3649}# (quote (macro)))
                               (#{syntax-type\ 3288}#
                                 (#{chi-macro\ 3293}#
                                   #{fval\ 3650}#
                                   #{e\ 3638}#
                                   #{r\ 3639}#
                                   #{w\ 3640}#
                                   #{rib\ 3642}#
                                   #{mod\ 3643}#)
                                 #{r\ 3639}#
                                 '(())
                                 #{s\ 3641}#
                                 #{rib\ 3642}#
                                 #{mod\ 3643}#
                                 #{for-car?\ 3644}#)
                               (if (memv #{ftype\ 3649}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 3650}# #{e\ 3638}#))
                                   (lambda (#{sym\ 3655}# #{mod\ 3656}#)
                                     (#{syntax-type\ 3288}#
                                       #{sym\ 3655}#
                                       #{r\ 3639}#
                                       #{w\ 3640}#
                                       #{s\ 3641}#
                                       #{rib\ 3642}#
                                       #{mod\ 3656}#
                                       #{for-car?\ 3644}#)))
                                 (if (memv #{ftype\ 3649}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 3650}#
                                     #{e\ 3638}#
                                     #{w\ 3640}#
                                     #{s\ 3641}#
                                     #{mod\ 3643}#)
                                   (if (memv #{ftype\ 3649}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 3650}#
                                       #{e\ 3638}#
                                       #{w\ 3640}#
                                       #{s\ 3641}#
                                       #{mod\ 3643}#)
                                     (if (memv #{ftype\ 3649}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 3638}#
                                         #{w\ 3640}#
                                         #{s\ 3641}#
                                         #{mod\ 3643}#)
                                       (if (memv #{ftype\ 3649}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 3638}#
                                           #{w\ 3640}#
                                           #{s\ 3641}#
                                           #{mod\ 3643}#)
                                         (if (memv #{ftype\ 3649}#
                                                   '(define))
                                           ((lambda (#{tmp\ 3657}#)
                                              ((lambda (#{tmp\ 3658}#)
                                                 (if (if #{tmp\ 3658}#
                                                       (apply (lambda (#{_\ 3659}#
                                                                       #{name\ 3660}#
                                                                       #{val\ 3661}#)
                                                                (#{id?\ 3254}#
                                                                  #{name\ 3660}#))
                                                              #{tmp\ 3658}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 3662}#
                                                                   #{name\ 3663}#
                                                                   #{val\ 3664}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 3663}#
                                                              #{val\ 3664}#
                                                              #{w\ 3640}#
                                                              #{s\ 3641}#
                                                              #{mod\ 3643}#))
                                                          #{tmp\ 3658}#)
                                                   ((lambda (#{tmp\ 3665}#)
                                                      (if (if #{tmp\ 3665}#
                                                            (apply (lambda (#{_\ 3666}#
                                                                            #{name\ 3667}#
                                                                            #{args\ 3668}#
                                                                            #{e1\ 3669}#
                                                                            #{e2\ 3670}#)
                                                                     (if (#{id?\ 3254}#
                                                                           #{name\ 3667}#)
                                                                       (#{valid-bound-ids?\ 3279}#
                                                                         (#{lambda-var-list\ 3301}#
                                                                           #{args\ 3668}#))
                                                                       #f))
                                                                   #{tmp\ 3665}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 3671}#
                                                                        #{name\ 3672}#
                                                                        #{args\ 3673}#
                                                                        #{e1\ 3674}#
                                                                        #{e2\ 3675}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 3282}#
                                                                     #{name\ 3672}#
                                                                     #{w\ 3640}#
                                                                     #{mod\ 3643}#)
                                                                   (#{decorate-source\ 3217}#
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
                                                                           (#{wrap\ 3282}#
                                                                             (cons #{args\ 3673}#
                                                                                   (cons #{e1\ 3674}#
                                                                                         #{e2\ 3675}#))
                                                                             #{w\ 3640}#
                                                                             #{mod\ 3643}#))
                                                                     #{s\ 3641}#)
                                                                   '(())
                                                                   #{s\ 3641}#
                                                                   #{mod\ 3643}#))
                                                               #{tmp\ 3665}#)
                                                        ((lambda (#{tmp\ 3677}#)
                                                           (if (if #{tmp\ 3677}#
                                                                 (apply (lambda (#{_\ 3678}#
                                                                                 #{name\ 3679}#)
                                                                          (#{id?\ 3254}#
                                                                            #{name\ 3679}#))
                                                                        #{tmp\ 3677}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 3680}#
                                                                             #{name\ 3681}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 3282}#
                                                                          #{name\ 3681}#
                                                                          #{w\ 3640}#
                                                                          #{mod\ 3643}#)
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
                                                                        #{s\ 3641}#
                                                                        #{mod\ 3643}#))
                                                                    #{tmp\ 3677}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 3657}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 3657}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 3657}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 3657}#
                                                 '(any any any))))
                                            #{e\ 3638}#)
                                           (if (memv #{ftype\ 3649}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 3682}#)
                                                ((lambda (#{tmp\ 3683}#)
                                                   (if (if #{tmp\ 3683}#
                                                         (apply (lambda (#{_\ 3684}#
                                                                         #{name\ 3685}#
                                                                         #{val\ 3686}#)
                                                                  (#{id?\ 3254}#
                                                                    #{name\ 3685}#))
                                                                #{tmp\ 3683}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 3687}#
                                                                     #{name\ 3688}#
                                                                     #{val\ 3689}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 3688}#
                                                                #{val\ 3689}#
                                                                #{w\ 3640}#
                                                                #{s\ 3641}#
                                                                #{mod\ 3643}#))
                                                            #{tmp\ 3683}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 3682}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 3682}#
                                                   '(any any any))))
                                              #{e\ 3638}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 3638}#
                                               #{w\ 3640}#
                                               #{s\ 3641}#
                                               #{mod\ 3643}#))))))))))))))
                   (if (#{syntax-object?\ 3238}# #{e\ 3638}#)
                     (#{syntax-type\ 3288}#
                       (#{syntax-object-expression\ 3239}# #{e\ 3638}#)
                       #{r\ 3639}#
                       (#{join-wraps\ 3273}#
                         #{w\ 3640}#
                         (#{syntax-object-wrap\ 3240}# #{e\ 3638}#))
                       #{s\ 3641}#
                       #{rib\ 3642}#
                       (let ((#{t\ 3690}#
                               (#{syntax-object-module\ 3241}# #{e\ 3638}#)))
                         (if #{t\ 3690}# #{t\ 3690}# #{mod\ 3643}#))
                       #{for-car?\ 3644}#)
                     (if (self-evaluating? #{e\ 3638}#)
                       (values
                         'constant
                         #f
                         #{e\ 3638}#
                         #{w\ 3640}#
                         #{s\ 3641}#
                         #{mod\ 3643}#)
                       (values
                         'other
                         #f
                         #{e\ 3638}#
                         #{w\ 3640}#
                         #{s\ 3641}#
                         #{mod\ 3643}#)))))))
           (#{chi-when-list\ 3287}#
             (lambda (#{e\ 3691}# #{when-list\ 3692}# #{w\ 3693}#)
               (letrec ((#{f\ 3694}#
                          (lambda (#{when-list\ 3695}# #{situations\ 3696}#)
                            (if (null? #{when-list\ 3695}#)
                              #{situations\ 3696}#
                              (#{f\ 3694}#
                                (cdr #{when-list\ 3695}#)
                                (cons (let ((#{x\ 3697}#
                                              (car #{when-list\ 3695}#)))
                                        (if (#{free-id=?\ 3277}#
                                              #{x\ 3697}#
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
                                          'compile
                                          (if (#{free-id=?\ 3277}#
                                                #{x\ 3697}#
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
                                                   (hygiene guile)))
                                            'load
                                            (if (#{free-id=?\ 3277}#
                                                  #{x\ 3697}#
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
                                                     (hygiene guile)))
                                              'eval
                                              (syntax-violation
                                                'eval-when
                                                "invalid situation"
                                                #{e\ 3691}#
                                                (#{wrap\ 3282}#
                                                  #{x\ 3697}#
                                                  #{w\ 3693}#
                                                  #f))))))
                                      #{situations\ 3696}#))))))
                 (#{f\ 3694}# #{when-list\ 3692}# (quote ())))))
           (#{chi-install-global\ 3286}#
             (lambda (#{name\ 3698}# #{e\ 3699}#)
               (#{build-global-definition\ 3227}#
                 #f
                 #{name\ 3698}#
                 (if (let ((#{v\ 3700}#
                             (module-variable
                               (current-module)
                               #{name\ 3698}#)))
                       (if #{v\ 3700}#
                         (if (variable-bound? #{v\ 3700}#)
                           (if (macro? (variable-ref #{v\ 3700}#))
                             (not (eq? (macro-type (variable-ref #{v\ 3700}#))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (#{build-application\ 3219}#
                     #f
                     (#{build-primref\ 3231}#
                       #f
                       'make-extended-syncase-macro)
                     (list (#{build-application\ 3219}#
                             #f
                             (#{build-primref\ 3231}# #f (quote module-ref))
                             (list (#{build-application\ 3219}#
                                     #f
                                     (#{build-primref\ 3231}#
                                       #f
                                       'current-module)
                                     '())
                                   (#{build-data\ 3232}# #f #{name\ 3698}#)))
                           (#{build-data\ 3232}# #f (quote macro))
                           #{e\ 3699}#))
                   (#{build-application\ 3219}#
                     #f
                     (#{build-primref\ 3231}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 3232}# #f (quote macro))
                           #{e\ 3699}#))))))
           (#{chi-top-sequence\ 3285}#
             (lambda (#{body\ 3701}#
                      #{r\ 3702}#
                      #{w\ 3703}#
                      #{s\ 3704}#
                      #{m\ 3705}#
                      #{esew\ 3706}#
                      #{mod\ 3707}#)
               (#{build-sequence\ 3233}#
                 #{s\ 3704}#
                 (letrec ((#{dobody\ 3708}#
                            (lambda (#{body\ 3709}#
                                     #{r\ 3710}#
                                     #{w\ 3711}#
                                     #{m\ 3712}#
                                     #{esew\ 3713}#
                                     #{mod\ 3714}#)
                              (if (null? #{body\ 3709}#)
                                '()
                                (let ((#{first\ 3715}#
                                        (#{chi-top\ 3289}#
                                          (car #{body\ 3709}#)
                                          #{r\ 3710}#
                                          #{w\ 3711}#
                                          #{m\ 3712}#
                                          #{esew\ 3713}#
                                          #{mod\ 3714}#)))
                                  (cons #{first\ 3715}#
                                        (#{dobody\ 3708}#
                                          (cdr #{body\ 3709}#)
                                          #{r\ 3710}#
                                          #{w\ 3711}#
                                          #{m\ 3712}#
                                          #{esew\ 3713}#
                                          #{mod\ 3714}#)))))))
                   (#{dobody\ 3708}#
                     #{body\ 3701}#
                     #{r\ 3702}#
                     #{w\ 3703}#
                     #{m\ 3705}#
                     #{esew\ 3706}#
                     #{mod\ 3707}#)))))
           (#{chi-sequence\ 3284}#
             (lambda (#{body\ 3716}#
                      #{r\ 3717}#
                      #{w\ 3718}#
                      #{s\ 3719}#
                      #{mod\ 3720}#)
               (#{build-sequence\ 3233}#
                 #{s\ 3719}#
                 (letrec ((#{dobody\ 3721}#
                            (lambda (#{body\ 3722}#
                                     #{r\ 3723}#
                                     #{w\ 3724}#
                                     #{mod\ 3725}#)
                              (if (null? #{body\ 3722}#)
                                '()
                                (let ((#{first\ 3726}#
                                        (#{chi\ 3290}#
                                          (car #{body\ 3722}#)
                                          #{r\ 3723}#
                                          #{w\ 3724}#
                                          #{mod\ 3725}#)))
                                  (cons #{first\ 3726}#
                                        (#{dobody\ 3721}#
                                          (cdr #{body\ 3722}#)
                                          #{r\ 3723}#
                                          #{w\ 3724}#
                                          #{mod\ 3725}#)))))))
                   (#{dobody\ 3721}#
                     #{body\ 3716}#
                     #{r\ 3717}#
                     #{w\ 3718}#
                     #{mod\ 3720}#)))))
           (#{source-wrap\ 3283}#
             (lambda (#{x\ 3727}#
                      #{w\ 3728}#
                      #{s\ 3729}#
                      #{defmod\ 3730}#)
               (#{wrap\ 3282}#
                 (#{decorate-source\ 3217}#
                   #{x\ 3727}#
                   #{s\ 3729}#)
                 #{w\ 3728}#
                 #{defmod\ 3730}#)))
           (#{wrap\ 3282}#
             (lambda (#{x\ 3731}# #{w\ 3732}# #{defmod\ 3733}#)
               (if (if (null? (#{wrap-marks\ 3257}# #{w\ 3732}#))
                     (null? (#{wrap-subst\ 3258}# #{w\ 3732}#))
                     #f)
                 #{x\ 3731}#
                 (if (#{syntax-object?\ 3238}# #{x\ 3731}#)
                   (#{make-syntax-object\ 3237}#
                     (#{syntax-object-expression\ 3239}# #{x\ 3731}#)
                     (#{join-wraps\ 3273}#
                       #{w\ 3732}#
                       (#{syntax-object-wrap\ 3240}# #{x\ 3731}#))
                     (#{syntax-object-module\ 3241}# #{x\ 3731}#))
                   (if (null? #{x\ 3731}#)
                     #{x\ 3731}#
                     (#{make-syntax-object\ 3237}#
                       #{x\ 3731}#
                       #{w\ 3732}#
                       #{defmod\ 3733}#))))))
           (#{bound-id-member?\ 3281}#
             (lambda (#{x\ 3734}# #{list\ 3735}#)
               (if (not (null? #{list\ 3735}#))
                 (let ((#{t\ 3736}#
                         (#{bound-id=?\ 3278}#
                           #{x\ 3734}#
                           (car #{list\ 3735}#))))
                   (if #{t\ 3736}#
                     #{t\ 3736}#
                     (#{bound-id-member?\ 3281}#
                       #{x\ 3734}#
                       (cdr #{list\ 3735}#))))
                 #f)))
           (#{distinct-bound-ids?\ 3280}#
             (lambda (#{ids\ 3737}#)
               (letrec ((#{distinct?\ 3738}#
                          (lambda (#{ids\ 3739}#)
                            (let ((#{t\ 3740}# (null? #{ids\ 3739}#)))
                              (if #{t\ 3740}#
                                #{t\ 3740}#
                                (if (not (#{bound-id-member?\ 3281}#
                                           (car #{ids\ 3739}#)
                                           (cdr #{ids\ 3739}#)))
                                  (#{distinct?\ 3738}# (cdr #{ids\ 3739}#))
                                  #f))))))
                 (#{distinct?\ 3738}# #{ids\ 3737}#))))
           (#{valid-bound-ids?\ 3279}#
             (lambda (#{ids\ 3741}#)
               (if (letrec ((#{all-ids?\ 3742}#
                              (lambda (#{ids\ 3743}#)
                                (let ((#{t\ 3744}# (null? #{ids\ 3743}#)))
                                  (if #{t\ 3744}#
                                    #{t\ 3744}#
                                    (if (#{id?\ 3254}# (car #{ids\ 3743}#))
                                      (#{all-ids?\ 3742}# (cdr #{ids\ 3743}#))
                                      #f))))))
                     (#{all-ids?\ 3742}# #{ids\ 3741}#))
                 (#{distinct-bound-ids?\ 3280}# #{ids\ 3741}#)
                 #f)))
           (#{bound-id=?\ 3278}#
             (lambda (#{i\ 3745}# #{j\ 3746}#)
               (if (if (#{syntax-object?\ 3238}# #{i\ 3745}#)
                     (#{syntax-object?\ 3238}# #{j\ 3746}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 3239}# #{i\ 3745}#)
                          (#{syntax-object-expression\ 3239}# #{j\ 3746}#))
                   (#{same-marks?\ 3275}#
                     (#{wrap-marks\ 3257}#
                       (#{syntax-object-wrap\ 3240}# #{i\ 3745}#))
                     (#{wrap-marks\ 3257}#
                       (#{syntax-object-wrap\ 3240}# #{j\ 3746}#)))
                   #f)
                 (eq? #{i\ 3745}# #{j\ 3746}#))))
           (#{free-id=?\ 3277}#
             (lambda (#{i\ 3747}# #{j\ 3748}#)
               (if (eq? (let ((#{x\ 3749}# #{i\ 3747}#))
                          (if (#{syntax-object?\ 3238}# #{x\ 3749}#)
                            (#{syntax-object-expression\ 3239}# #{x\ 3749}#)
                            #{x\ 3749}#))
                        (let ((#{x\ 3750}# #{j\ 3748}#))
                          (if (#{syntax-object?\ 3238}# #{x\ 3750}#)
                            (#{syntax-object-expression\ 3239}# #{x\ 3750}#)
                            #{x\ 3750}#)))
                 (eq? (#{id-var-name\ 3276}# #{i\ 3747}# (quote (())))
                      (#{id-var-name\ 3276}# #{j\ 3748}# (quote (()))))
                 #f)))
           (#{id-var-name\ 3276}#
             (lambda (#{id\ 3751}# #{w\ 3752}#)
               (letrec ((#{search-vector-rib\ 3755}#
                          (lambda (#{sym\ 3761}#
                                   #{subst\ 3762}#
                                   #{marks\ 3763}#
                                   #{symnames\ 3764}#
                                   #{ribcage\ 3765}#)
                            (let ((#{n\ 3766}#
                                    (vector-length #{symnames\ 3764}#)))
                              (letrec ((#{f\ 3767}#
                                         (lambda (#{i\ 3768}#)
                                           (if (#{fx=\ 3211}#
                                                 #{i\ 3768}#
                                                 #{n\ 3766}#)
                                             (#{search\ 3753}#
                                               #{sym\ 3761}#
                                               (cdr #{subst\ 3762}#)
                                               #{marks\ 3763}#)
                                             (if (if (eq? (vector-ref
                                                            #{symnames\ 3764}#
                                                            #{i\ 3768}#)
                                                          #{sym\ 3761}#)
                                                   (#{same-marks?\ 3275}#
                                                     #{marks\ 3763}#
                                                     (vector-ref
                                                       (#{ribcage-marks\ 3264}#
                                                         #{ribcage\ 3765}#)
                                                       #{i\ 3768}#))
                                                   #f)
                                               (values
                                                 (vector-ref
                                                   (#{ribcage-labels\ 3265}#
                                                     #{ribcage\ 3765}#)
                                                   #{i\ 3768}#)
                                                 #{marks\ 3763}#)
                                               (#{f\ 3767}#
                                                 (#{fx+\ 3209}#
                                                   #{i\ 3768}#
                                                   1)))))))
                                (#{f\ 3767}# 0)))))
                        (#{search-list-rib\ 3754}#
                          (lambda (#{sym\ 3769}#
                                   #{subst\ 3770}#
                                   #{marks\ 3771}#
                                   #{symnames\ 3772}#
                                   #{ribcage\ 3773}#)
                            (letrec ((#{f\ 3774}#
                                       (lambda (#{symnames\ 3775}# #{i\ 3776}#)
                                         (if (null? #{symnames\ 3775}#)
                                           (#{search\ 3753}#
                                             #{sym\ 3769}#
                                             (cdr #{subst\ 3770}#)
                                             #{marks\ 3771}#)
                                           (if (if (eq? (car #{symnames\ 3775}#)
                                                        #{sym\ 3769}#)
                                                 (#{same-marks?\ 3275}#
                                                   #{marks\ 3771}#
                                                   (list-ref
                                                     (#{ribcage-marks\ 3264}#
                                                       #{ribcage\ 3773}#)
                                                     #{i\ 3776}#))
                                                 #f)
                                             (values
                                               (list-ref
                                                 (#{ribcage-labels\ 3265}#
                                                   #{ribcage\ 3773}#)
                                                 #{i\ 3776}#)
                                               #{marks\ 3771}#)
                                             (#{f\ 3774}#
                                               (cdr #{symnames\ 3775}#)
                                               (#{fx+\ 3209}#
                                                 #{i\ 3776}#
                                                 1)))))))
                              (#{f\ 3774}# #{symnames\ 3772}# 0))))
                        (#{search\ 3753}#
                          (lambda (#{sym\ 3777}#
                                   #{subst\ 3778}#
                                   #{marks\ 3779}#)
                            (if (null? #{subst\ 3778}#)
                              (values #f #{marks\ 3779}#)
                              (let ((#{fst\ 3780}# (car #{subst\ 3778}#)))
                                (if (eq? #{fst\ 3780}# (quote shift))
                                  (#{search\ 3753}#
                                    #{sym\ 3777}#
                                    (cdr #{subst\ 3778}#)
                                    (cdr #{marks\ 3779}#))
                                  (let ((#{symnames\ 3781}#
                                          (#{ribcage-symnames\ 3263}#
                                            #{fst\ 3780}#)))
                                    (if (vector? #{symnames\ 3781}#)
                                      (#{search-vector-rib\ 3755}#
                                        #{sym\ 3777}#
                                        #{subst\ 3778}#
                                        #{marks\ 3779}#
                                        #{symnames\ 3781}#
                                        #{fst\ 3780}#)
                                      (#{search-list-rib\ 3754}#
                                        #{sym\ 3777}#
                                        #{subst\ 3778}#
                                        #{marks\ 3779}#
                                        #{symnames\ 3781}#
                                        #{fst\ 3780}#)))))))))
                 (if (symbol? #{id\ 3751}#)
                   (let ((#{t\ 3782}#
                           (call-with-values
                             (lambda ()
                               (#{search\ 3753}#
                                 #{id\ 3751}#
                                 (#{wrap-subst\ 3258}# #{w\ 3752}#)
                                 (#{wrap-marks\ 3257}# #{w\ 3752}#)))
                             (lambda (#{x\ 3783}# . #{ignore\ 3784}#)
                               #{x\ 3783}#))))
                     (if #{t\ 3782}# #{t\ 3782}# #{id\ 3751}#))
                   (if (#{syntax-object?\ 3238}# #{id\ 3751}#)
                     (let ((#{id\ 3785}#
                             (#{syntax-object-expression\ 3239}# #{id\ 3751}#))
                           (#{w1\ 3786}#
                             (#{syntax-object-wrap\ 3240}# #{id\ 3751}#)))
                       (let ((#{marks\ 3787}#
                               (#{join-marks\ 3274}#
                                 (#{wrap-marks\ 3257}# #{w\ 3752}#)
                                 (#{wrap-marks\ 3257}# #{w1\ 3786}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 3753}#
                               #{id\ 3785}#
                               (#{wrap-subst\ 3258}# #{w\ 3752}#)
                               #{marks\ 3787}#))
                           (lambda (#{new-id\ 3788}# #{marks\ 3789}#)
                             (let ((#{t\ 3790}# #{new-id\ 3788}#))
                               (if #{t\ 3790}#
                                 #{t\ 3790}#
                                 (let ((#{t\ 3791}#
                                         (call-with-values
                                           (lambda ()
                                             (#{search\ 3753}#
                                               #{id\ 3785}#
                                               (#{wrap-subst\ 3258}#
                                                 #{w1\ 3786}#)
                                               #{marks\ 3789}#))
                                           (lambda (#{x\ 3792}#
                                                    .
                                                    #{ignore\ 3793}#)
                                             #{x\ 3792}#))))
                                   (if #{t\ 3791}#
                                     #{t\ 3791}#
                                     #{id\ 3785}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 3751}#))))))
           (#{same-marks?\ 3275}#
             (lambda (#{x\ 3794}# #{y\ 3795}#)
               (let ((#{t\ 3796}# (eq? #{x\ 3794}# #{y\ 3795}#)))
                 (if #{t\ 3796}#
                   #{t\ 3796}#
                   (if (not (null? #{x\ 3794}#))
                     (if (not (null? #{y\ 3795}#))
                       (if (eq? (car #{x\ 3794}#) (car #{y\ 3795}#))
                         (#{same-marks?\ 3275}#
                           (cdr #{x\ 3794}#)
                           (cdr #{y\ 3795}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 3274}#
             (lambda (#{m1\ 3797}# #{m2\ 3798}#)
               (#{smart-append\ 3272}#
                 #{m1\ 3797}#
                 #{m2\ 3798}#)))
           (#{join-wraps\ 3273}#
             (lambda (#{w1\ 3799}# #{w2\ 3800}#)
               (let ((#{m1\ 3801}#
                       (#{wrap-marks\ 3257}# #{w1\ 3799}#))
                     (#{s1\ 3802}#
                       (#{wrap-subst\ 3258}# #{w1\ 3799}#)))
                 (if (null? #{m1\ 3801}#)
                   (if (null? #{s1\ 3802}#)
                     #{w2\ 3800}#
                     (#{make-wrap\ 3256}#
                       (#{wrap-marks\ 3257}# #{w2\ 3800}#)
                       (#{smart-append\ 3272}#
                         #{s1\ 3802}#
                         (#{wrap-subst\ 3258}# #{w2\ 3800}#))))
                   (#{make-wrap\ 3256}#
                     (#{smart-append\ 3272}#
                       #{m1\ 3801}#
                       (#{wrap-marks\ 3257}# #{w2\ 3800}#))
                     (#{smart-append\ 3272}#
                       #{s1\ 3802}#
                       (#{wrap-subst\ 3258}# #{w2\ 3800}#)))))))
           (#{smart-append\ 3272}#
             (lambda (#{m1\ 3803}# #{m2\ 3804}#)
               (if (null? #{m2\ 3804}#)
                 #{m1\ 3803}#
                 (append #{m1\ 3803}# #{m2\ 3804}#))))
           (#{make-binding-wrap\ 3271}#
             (lambda (#{ids\ 3805}# #{labels\ 3806}# #{w\ 3807}#)
               (if (null? #{ids\ 3805}#)
                 #{w\ 3807}#
                 (#{make-wrap\ 3256}#
                   (#{wrap-marks\ 3257}# #{w\ 3807}#)
                   (cons (let ((#{labelvec\ 3808}#
                                 (list->vector #{labels\ 3806}#)))
                           (let ((#{n\ 3809}#
                                   (vector-length #{labelvec\ 3808}#)))
                             (let ((#{symnamevec\ 3810}#
                                     (make-vector #{n\ 3809}#))
                                   (#{marksvec\ 3811}#
                                     (make-vector #{n\ 3809}#)))
                               (begin
                                 (letrec ((#{f\ 3812}#
                                            (lambda (#{ids\ 3813}# #{i\ 3814}#)
                                              (if (not (null? #{ids\ 3813}#))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{id-sym-name&marks\ 3255}#
                                                      (car #{ids\ 3813}#)
                                                      #{w\ 3807}#))
                                                  (lambda (#{symname\ 3815}#
                                                           #{marks\ 3816}#)
                                                    (begin
                                                      (vector-set!
                                                        #{symnamevec\ 3810}#
                                                        #{i\ 3814}#
                                                        #{symname\ 3815}#)
                                                      (vector-set!
                                                        #{marksvec\ 3811}#
                                                        #{i\ 3814}#
                                                        #{marks\ 3816}#)
                                                      (#{f\ 3812}#
                                                        (cdr #{ids\ 3813}#)
                                                        (#{fx+\ 3209}#
                                                          #{i\ 3814}#
                                                          1)))))))))
                                   (#{f\ 3812}# #{ids\ 3805}# 0))
                                 (#{make-ribcage\ 3261}#
                                   #{symnamevec\ 3810}#
                                   #{marksvec\ 3811}#
                                   #{labelvec\ 3808}#)))))
                         (#{wrap-subst\ 3258}# #{w\ 3807}#))))))
           (#{extend-ribcage!\ 3270}#
             (lambda (#{ribcage\ 3817}# #{id\ 3818}# #{label\ 3819}#)
               (begin
                 (#{set-ribcage-symnames!\ 3266}#
                   #{ribcage\ 3817}#
                   (cons (#{syntax-object-expression\ 3239}# #{id\ 3818}#)
                         (#{ribcage-symnames\ 3263}# #{ribcage\ 3817}#)))
                 (#{set-ribcage-marks!\ 3267}#
                   #{ribcage\ 3817}#
                   (cons (#{wrap-marks\ 3257}#
                           (#{syntax-object-wrap\ 3240}# #{id\ 3818}#))
                         (#{ribcage-marks\ 3264}# #{ribcage\ 3817}#)))
                 (#{set-ribcage-labels!\ 3268}#
                   #{ribcage\ 3817}#
                   (cons #{label\ 3819}#
                         (#{ribcage-labels\ 3265}# #{ribcage\ 3817}#))))))
           (#{anti-mark\ 3269}#
             (lambda (#{w\ 3820}#)
               (#{make-wrap\ 3256}#
                 (cons #f (#{wrap-marks\ 3257}# #{w\ 3820}#))
                 (cons 'shift
                       (#{wrap-subst\ 3258}# #{w\ 3820}#)))))
           (#{set-ribcage-labels!\ 3268}#
             (lambda (#{x\ 3821}# #{update\ 3822}#)
               (vector-set! #{x\ 3821}# 3 #{update\ 3822}#)))
           (#{set-ribcage-marks!\ 3267}#
             (lambda (#{x\ 3823}# #{update\ 3824}#)
               (vector-set! #{x\ 3823}# 2 #{update\ 3824}#)))
           (#{set-ribcage-symnames!\ 3266}#
             (lambda (#{x\ 3825}# #{update\ 3826}#)
               (vector-set! #{x\ 3825}# 1 #{update\ 3826}#)))
           (#{ribcage-labels\ 3265}#
             (lambda (#{x\ 3827}#) (vector-ref #{x\ 3827}# 3)))
           (#{ribcage-marks\ 3264}#
             (lambda (#{x\ 3828}#) (vector-ref #{x\ 3828}# 2)))
           (#{ribcage-symnames\ 3263}#
             (lambda (#{x\ 3829}#) (vector-ref #{x\ 3829}# 1)))
           (#{ribcage?\ 3262}#
             (lambda (#{x\ 3830}#)
               (if (vector? #{x\ 3830}#)
                 (if (= (vector-length #{x\ 3830}#) 4)
                   (eq? (vector-ref #{x\ 3830}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 3261}#
             (lambda (#{symnames\ 3831}#
                      #{marks\ 3832}#
                      #{labels\ 3833}#)
               (vector
                 'ribcage
                 #{symnames\ 3831}#
                 #{marks\ 3832}#
                 #{labels\ 3833}#)))
           (#{gen-labels\ 3260}#
             (lambda (#{ls\ 3834}#)
               (if (null? #{ls\ 3834}#)
                 '()
                 (cons (#{gen-label\ 3259}#)
                       (#{gen-labels\ 3260}# (cdr #{ls\ 3834}#))))))
           (#{gen-label\ 3259}# (lambda () (string #\i)))
           (#{wrap-subst\ 3258}# cdr)
           (#{wrap-marks\ 3257}# car)
           (#{make-wrap\ 3256}# cons)
           (#{id-sym-name&marks\ 3255}#
             (lambda (#{x\ 3835}# #{w\ 3836}#)
               (if (#{syntax-object?\ 3238}# #{x\ 3835}#)
                 (values
                   (#{syntax-object-expression\ 3239}# #{x\ 3835}#)
                   (#{join-marks\ 3274}#
                     (#{wrap-marks\ 3257}# #{w\ 3836}#)
                     (#{wrap-marks\ 3257}#
                       (#{syntax-object-wrap\ 3240}# #{x\ 3835}#))))
                 (values
                   #{x\ 3835}#
                   (#{wrap-marks\ 3257}# #{w\ 3836}#)))))
           (#{id?\ 3254}#
             (lambda (#{x\ 3837}#)
               (if (symbol? #{x\ 3837}#)
                 #t
                 (if (#{syntax-object?\ 3238}# #{x\ 3837}#)
                   (symbol?
                     (#{syntax-object-expression\ 3239}# #{x\ 3837}#))
                   #f))))
           (#{nonsymbol-id?\ 3253}#
             (lambda (#{x\ 3838}#)
               (if (#{syntax-object?\ 3238}# #{x\ 3838}#)
                 (symbol?
                   (#{syntax-object-expression\ 3239}# #{x\ 3838}#))
                 #f)))
           (#{global-extend\ 3252}#
             (lambda (#{type\ 3839}# #{sym\ 3840}# #{val\ 3841}#)
               (#{put-global-definition-hook\ 3215}#
                 #{sym\ 3840}#
                 #{type\ 3839}#
                 #{val\ 3841}#)))
           (#{lookup\ 3251}#
             (lambda (#{x\ 3842}# #{r\ 3843}# #{mod\ 3844}#)
               (let ((#{t\ 3845}# (assq #{x\ 3842}# #{r\ 3843}#)))
                 (if #{t\ 3845}#
                   (cdr #{t\ 3845}#)
                   (if (symbol? #{x\ 3842}#)
                     (let ((#{t\ 3846}#
                             (#{get-global-definition-hook\ 3216}#
                               #{x\ 3842}#
                               #{mod\ 3844}#)))
                       (if #{t\ 3846}# #{t\ 3846}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 3250}#
             (lambda (#{r\ 3847}#)
               (if (null? #{r\ 3847}#)
                 '()
                 (let ((#{a\ 3848}# (car #{r\ 3847}#)))
                   (if (eq? (cadr #{a\ 3848}#) (quote macro))
                     (cons #{a\ 3848}#
                           (#{macros-only-env\ 3250}# (cdr #{r\ 3847}#)))
                     (#{macros-only-env\ 3250}# (cdr #{r\ 3847}#)))))))
           (#{extend-var-env\ 3249}#
             (lambda (#{labels\ 3849}# #{vars\ 3850}# #{r\ 3851}#)
               (if (null? #{labels\ 3849}#)
                 #{r\ 3851}#
                 (#{extend-var-env\ 3249}#
                   (cdr #{labels\ 3849}#)
                   (cdr #{vars\ 3850}#)
                   (cons (cons (car #{labels\ 3849}#)
                               (cons (quote lexical) (car #{vars\ 3850}#)))
                         #{r\ 3851}#)))))
           (#{extend-env\ 3248}#
             (lambda (#{labels\ 3852}# #{bindings\ 3853}# #{r\ 3854}#)
               (if (null? #{labels\ 3852}#)
                 #{r\ 3854}#
                 (#{extend-env\ 3248}#
                   (cdr #{labels\ 3852}#)
                   (cdr #{bindings\ 3853}#)
                   (cons (cons (car #{labels\ 3852}#)
                               (car #{bindings\ 3853}#))
                         #{r\ 3854}#)))))
           (#{binding-value\ 3247}# cdr)
           (#{binding-type\ 3246}# car)
           (#{source-annotation\ 3245}#
             (lambda (#{x\ 3855}#)
               (if (#{syntax-object?\ 3238}# #{x\ 3855}#)
                 (#{source-annotation\ 3245}#
                   (#{syntax-object-expression\ 3239}# #{x\ 3855}#))
                 (if (pair? #{x\ 3855}#)
                   (let ((#{props\ 3856}# (source-properties #{x\ 3855}#)))
                     (if (pair? #{props\ 3856}#) #{props\ 3856}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 3244}#
             (lambda (#{x\ 3857}# #{update\ 3858}#)
               (vector-set! #{x\ 3857}# 3 #{update\ 3858}#)))
           (#{set-syntax-object-wrap!\ 3243}#
             (lambda (#{x\ 3859}# #{update\ 3860}#)
               (vector-set! #{x\ 3859}# 2 #{update\ 3860}#)))
           (#{set-syntax-object-expression!\ 3242}#
             (lambda (#{x\ 3861}# #{update\ 3862}#)
               (vector-set! #{x\ 3861}# 1 #{update\ 3862}#)))
           (#{syntax-object-module\ 3241}#
             (lambda (#{x\ 3863}#) (vector-ref #{x\ 3863}# 3)))
           (#{syntax-object-wrap\ 3240}#
             (lambda (#{x\ 3864}#) (vector-ref #{x\ 3864}# 2)))
           (#{syntax-object-expression\ 3239}#
             (lambda (#{x\ 3865}#) (vector-ref #{x\ 3865}# 1)))
           (#{syntax-object?\ 3238}#
             (lambda (#{x\ 3866}#)
               (if (vector? #{x\ 3866}#)
                 (if (= (vector-length #{x\ 3866}#) 4)
                   (eq? (vector-ref #{x\ 3866}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 3237}#
             (lambda (#{expression\ 3867}#
                      #{wrap\ 3868}#
                      #{module\ 3869}#)
               (vector
                 'syntax-object
                 #{expression\ 3867}#
                 #{wrap\ 3868}#
                 #{module\ 3869}#)))
           (#{build-letrec\ 3236}#
             (lambda (#{src\ 3870}#
                      #{ids\ 3871}#
                      #{vars\ 3872}#
                      #{val-exps\ 3873}#
                      #{body-exp\ 3874}#)
               (if (null? #{vars\ 3872}#)
                 #{body-exp\ 3874}#
                 (let ((#{atom-key\ 3875}# (fluid-ref #{*mode*\ 3208}#)))
                   (if (memv #{atom-key\ 3875}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 3226}#
                         #{ids\ 3871}#
                         #{val-exps\ 3873}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 3870}#
                        #{ids\ 3871}#
                        #{vars\ 3872}#
                        #{val-exps\ 3873}#
                        #{body-exp\ 3874}#))
                     (#{decorate-source\ 3217}#
                       (list 'letrec
                             (map list #{vars\ 3872}# #{val-exps\ 3873}#)
                             #{body-exp\ 3874}#)
                       #{src\ 3870}#))))))
           (#{build-named-let\ 3235}#
             (lambda (#{src\ 3876}#
                      #{ids\ 3877}#
                      #{vars\ 3878}#
                      #{val-exps\ 3879}#
                      #{body-exp\ 3880}#)
               (let ((#{f\ 3881}# (car #{vars\ 3878}#))
                     (#{f-name\ 3882}# (car #{ids\ 3877}#))
                     (#{vars\ 3883}# (cdr #{vars\ 3878}#))
                     (#{ids\ 3884}# (cdr #{ids\ 3877}#)))
                 (let ((#{atom-key\ 3885}# (fluid-ref #{*mode*\ 3208}#)))
                   (if (memv #{atom-key\ 3885}# (quote (c)))
                     (let ((#{proc\ 3886}#
                             (#{build-simple-lambda\ 3228}#
                               #{src\ 3876}#
                               #{ids\ 3884}#
                               #f
                               #{vars\ 3883}#
                               #f
                               #{body-exp\ 3880}#)))
                       (begin
                         (#{maybe-name-value!\ 3226}#
                           #{f-name\ 3882}#
                           #{proc\ 3886}#)
                         (for-each
                           #{maybe-name-value!\ 3226}#
                           #{ids\ 3884}#
                           #{val-exps\ 3879}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 3876}#
                          (list #{f-name\ 3882}#)
                          (list #{f\ 3881}#)
                          (list #{proc\ 3886}#)
                          (#{build-application\ 3219}#
                            #{src\ 3876}#
                            (#{build-lexical-reference\ 3221}#
                              'fun
                              #{src\ 3876}#
                              #{f-name\ 3882}#
                              #{f\ 3881}#)
                            #{val-exps\ 3879}#))))
                     (#{decorate-source\ 3217}#
                       (list 'let
                             #{f\ 3881}#
                             (map list #{vars\ 3883}# #{val-exps\ 3879}#)
                             #{body-exp\ 3880}#)
                       #{src\ 3876}#))))))
           (#{build-let\ 3234}#
             (lambda (#{src\ 3887}#
                      #{ids\ 3888}#
                      #{vars\ 3889}#
                      #{val-exps\ 3890}#
                      #{body-exp\ 3891}#)
               (if (null? #{vars\ 3889}#)
                 #{body-exp\ 3891}#
                 (let ((#{atom-key\ 3892}# (fluid-ref #{*mode*\ 3208}#)))
                   (if (memv #{atom-key\ 3892}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 3226}#
                         #{ids\ 3888}#
                         #{val-exps\ 3890}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 3887}#
                        #{ids\ 3888}#
                        #{vars\ 3889}#
                        #{val-exps\ 3890}#
                        #{body-exp\ 3891}#))
                     (#{decorate-source\ 3217}#
                       (list 'let
                             (map list #{vars\ 3889}# #{val-exps\ 3890}#)
                             #{body-exp\ 3891}#)
                       #{src\ 3887}#))))))
           (#{build-sequence\ 3233}#
             (lambda (#{src\ 3893}# #{exps\ 3894}#)
               (if (null? (cdr #{exps\ 3894}#))
                 (car #{exps\ 3894}#)
                 (let ((#{atom-key\ 3895}# (fluid-ref #{*mode*\ 3208}#)))
                   (if (memv #{atom-key\ 3895}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 3893}#
                      #{exps\ 3894}#)
                     (#{decorate-source\ 3217}#
                       (cons (quote begin) #{exps\ 3894}#)
                       #{src\ 3893}#))))))
           (#{build-data\ 3232}#
             (lambda (#{src\ 3896}# #{exp\ 3897}#)
               (let ((#{atom-key\ 3898}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3898}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 3896}#
                    #{exp\ 3897}#)
                   (#{decorate-source\ 3217}#
                     (if (if (self-evaluating? #{exp\ 3897}#)
                           (not (vector? #{exp\ 3897}#))
                           #f)
                       #{exp\ 3897}#
                       (list (quote quote) #{exp\ 3897}#))
                     #{src\ 3896}#)))))
           (#{build-primref\ 3231}#
             (lambda (#{src\ 3899}# #{name\ 3900}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 3901}# (fluid-ref #{*mode*\ 3208}#)))
                   (if (memv #{atom-key\ 3901}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 3899}#
                      #{name\ 3900}#)
                     (#{decorate-source\ 3217}#
                       #{name\ 3900}#
                       #{src\ 3899}#)))
                 (let ((#{atom-key\ 3902}# (fluid-ref #{*mode*\ 3208}#)))
                   (if (memv #{atom-key\ 3902}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 3899}#
                      '(guile)
                      #{name\ 3900}#
                      #f)
                     (#{decorate-source\ 3217}#
                       (list (quote @@) (quote (guile)) #{name\ 3900}#)
                       #{src\ 3899}#))))))
           (#{build-lambda-case\ 3230}#
             (lambda (#{src\ 3903}#
                      #{req\ 3904}#
                      #{opt\ 3905}#
                      #{rest\ 3906}#
                      #{kw\ 3907}#
                      #{vars\ 3908}#
                      #{predicate\ 3909}#
                      #{body\ 3910}#
                      #{else-case\ 3911}#)
               (let ((#{atom-key\ 3912}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3912}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 3903}#
                    #{req\ 3904}#
                    #{opt\ 3905}#
                    #{rest\ 3906}#
                    #{kw\ 3907}#
                    '()
                    #{vars\ 3908}#
                    #{predicate\ 3909}#
                    #{body\ 3910}#
                    #{else-case\ 3911}#)
                   (let ((#{nreq\ 3913}# (length #{req\ 3904}#)))
                     (let ((#{nopt\ 3914}#
                             (if #{opt\ 3905}# (length #{opt\ 3905}#) 0)))
                       (let ((#{rest-idx\ 3915}#
                               (if #{rest\ 3906}#
                                 (+ #{nreq\ 3913}# #{nopt\ 3914}#)
                                 #f)))
                         (let ((#{opt-inits\ 3916}#
                                 (map (lambda (#{x\ 3917}#)
                                        (list 'lambda
                                              #{vars\ 3908}#
                                              (cdr #{x\ 3917}#)))
                                      (let ((#{t\ 3918}# #{opt\ 3905}#))
                                        (if #{t\ 3918}#
                                          #{t\ 3918}#
                                          '())))))
                           (let ((#{allow-other-keys?\ 3919}#
                                   (if #{kw\ 3907}# (car #{kw\ 3907}#) #f)))
                             (let ((#{kw-indices\ 3920}#
                                     (map (lambda (#{x\ 3921}#)
                                            (cons (car #{x\ 3921}#)
                                                  (list-index
                                                    #{vars\ 3908}#
                                                    (caddr #{x\ 3921}#))))
                                          (if #{kw\ 3907}#
                                            (cdr #{kw\ 3907}#)
                                            '()))))
                               (let ((#{kw-inits\ 3922}#
                                       (sort (filter
                                               identity
                                               (map (lambda (#{x\ 3923}#)
                                                      (if (pair? (cdddr #{x\ 3923}#))
                                                        (let ((#{i\ 3924}#
                                                                (list-index
                                                                  #{vars\ 3908}#
                                                                  (caddr #{x\ 3923}#))))
                                                          (if (> (+ #{nreq\ 3913}#
                                                                    #{nopt\ 3914}#)
                                                                 #{i\ 3924}#)
                                                            (error "kw init for rest arg"
                                                                   #{x\ 3923}#)
                                                            (if (if #{rest\ 3906}#
                                                                  (= (+ #{nreq\ 3913}#
                                                                        #{nopt\ 3914}#)
                                                                     #{i\ 3924}#)
                                                                  #f)
                                                              (error "kw init for positional arg"
                                                                     #{x\ 3923}#)
                                                              (list 'lambda
                                                                    #{vars\ 3908}#
                                                                    (cadddr
                                                                      #{x\ 3923}#)))))
                                                        (let ((#{i\ 3925}#
                                                                (list-index
                                                                  #{vars\ 3908}#
                                                                  (caddr #{x\ 3923}#))))
                                                          (if (< (+ #{nreq\ 3913}#
                                                                    #{nopt\ 3914}#)
                                                                 #{i\ 3925}#)
                                                            #f
                                                            (error "missing init for kw arg"
                                                                   #{x\ 3923}#)))))
                                                    (if #{kw\ 3907}#
                                                      (cdr #{kw\ 3907}#)
                                                      '())))
                                             (lambda (#{x\ 3926}# #{y\ 3927}#)
                                               (< (cdr #{x\ 3926}#)
                                                  (cdr #{y\ 3927}#))))))
                                 (let ((#{nargs\ 3928}#
                                         (apply max
                                                (pk (+ #{nreq\ 3913}#
                                                       #{nopt\ 3914}#
                                                       (if #{rest\ 3906}#
                                                         1
                                                         0)))
                                                (map cdr
                                                     #{kw-indices\ 3920}#))))
                                   (begin
                                     (let ((#{t\ 3929}#
                                             (= #{nargs\ 3928}#
                                                (length #{vars\ 3908}#)
                                                (+ #{nreq\ 3913}#
                                                   (length #{opt-inits\ 3916}#)
                                                   (if #{rest\ 3906}# 1 0)
                                                   (length
                                                     #{kw-inits\ 3922}#)))))
                                       (if #{t\ 3929}#
                                         #{t\ 3929}#
                                         (error "something went wrong"
                                                #{req\ 3904}#
                                                #{opt\ 3905}#
                                                #{rest\ 3906}#
                                                #{kw\ 3907}#
                                                #{vars\ 3908}#
                                                #{nreq\ 3913}#
                                                #{nopt\ 3914}#
                                                #{kw-indices\ 3920}#
                                                #{kw-inits\ 3922}#
                                                #{nargs\ 3928}#)))
                                     (#{decorate-source\ 3217}#
                                       (cons (list (cons '(@@ (ice-9 optargs)
                                                              parse-lambda-case)
                                                         (cons (list 'quote
                                                                     (list #{nreq\ 3913}#
                                                                           #{nopt\ 3914}#
                                                                           #{rest-idx\ 3915}#
                                                                           #{nargs\ 3928}#
                                                                           #{allow-other-keys?\ 3919}#
                                                                           #{kw-indices\ 3920}#))
                                                               (cons (cons 'list
                                                                           (append
                                                                             #{opt-inits\ 3916}#
                                                                             #{kw-inits\ 3922}#))
                                                                     (cons (if #{predicate\ 3909}#
                                                                             (list 'lambda
                                                                                   #{vars\ 3908}#
                                                                                   #{predicate\ 3909}#)
                                                                             #f)
                                                                           '(%%args)))))
                                                   '=>
                                                   (list 'lambda
                                                         #{vars\ 3908}#
                                                         #{body\ 3910}#))
                                             (let ((#{t\ 3930}#
                                                     #{else-case\ 3911}#))
                                               (if #{t\ 3930}#
                                                 #{t\ 3930}#
                                                 '((%%args
                                                     (error "wrong number of arguments"
                                                            %%args))))))
                                       #{src\ 3903}#))))))))))))))
           (#{build-case-lambda\ 3229}#
             (lambda (#{src\ 3931}#
                      #{docstring\ 3932}#
                      #{body\ 3933}#)
               (let ((#{atom-key\ 3934}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3934}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 3931}#
                    (if #{docstring\ 3932}#
                      (list (cons (quote documentation) #{docstring\ 3932}#))
                      '())
                    #{body\ 3933}#)
                   (#{decorate-source\ 3217}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 3932}#
                                     (list #{docstring\ 3932}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 3933}#)))))
                     #{src\ 3931}#)))))
           (#{build-simple-lambda\ 3228}#
             (lambda (#{src\ 3935}#
                      #{req\ 3936}#
                      #{rest\ 3937}#
                      #{vars\ 3938}#
                      #{docstring\ 3939}#
                      #{exp\ 3940}#)
               (let ((#{atom-key\ 3941}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3941}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 3935}#
                    (if #{docstring\ 3939}#
                      (list (cons (quote documentation) #{docstring\ 3939}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 3935}#
                     #{req\ 3936}#
                     #f
                     #{rest\ 3937}#
                     #f
                     '()
                     #{vars\ 3938}#
                     #f
                     #{exp\ 3940}#
                     #f))
                   (#{decorate-source\ 3217}#
                     (cons 'lambda
                           (cons (if #{rest\ 3937}#
                                   (apply cons* #{vars\ 3938}#)
                                   #{vars\ 3938}#)
                                 (append
                                   (if #{docstring\ 3939}#
                                     (list #{docstring\ 3939}#)
                                     '())
                                   (list #{exp\ 3940}#))))
                     #{src\ 3935}#)))))
           (#{build-global-definition\ 3227}#
             (lambda (#{source\ 3942}# #{var\ 3943}# #{exp\ 3944}#)
               (let ((#{atom-key\ 3945}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3945}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 3226}#
                       #{var\ 3943}#
                       #{exp\ 3944}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 3942}#
                      #{var\ 3943}#
                      #{exp\ 3944}#))
                   (#{decorate-source\ 3217}#
                     (list (quote define) #{var\ 3943}# #{exp\ 3944}#)
                     #{source\ 3942}#)))))
           (#{maybe-name-value!\ 3226}#
             (lambda (#{name\ 3946}# #{val\ 3947}#)
               (if ((@ (language tree-il) lambda?) #{val\ 3947}#)
                 (let ((#{meta\ 3948}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 3947}#)))
                   (if (not (assq (quote name) #{meta\ 3948}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 3947}#
                      (acons 'name
                             #{name\ 3946}#
                             #{meta\ 3948}#)))))))
           (#{build-global-assignment\ 3225}#
             (lambda (#{source\ 3949}#
                      #{var\ 3950}#
                      #{exp\ 3951}#
                      #{mod\ 3952}#)
               (#{analyze-variable\ 3223}#
                 #{mod\ 3952}#
                 #{var\ 3950}#
                 (lambda (#{mod\ 3953}# #{var\ 3954}# #{public?\ 3955}#)
                   (let ((#{atom-key\ 3956}# (fluid-ref #{*mode*\ 3208}#)))
                     (if (memv #{atom-key\ 3956}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 3949}#
                        #{mod\ 3953}#
                        #{var\ 3954}#
                        #{public?\ 3955}#
                        #{exp\ 3951}#)
                       (#{decorate-source\ 3217}#
                         (list 'set!
                               (list (if #{public?\ 3955}#
                                       '@
                                       '@@)
                                     #{mod\ 3953}#
                                     #{var\ 3954}#)
                               #{exp\ 3951}#)
                         #{source\ 3949}#))))
                 (lambda (#{var\ 3957}#)
                   (let ((#{atom-key\ 3958}# (fluid-ref #{*mode*\ 3208}#)))
                     (if (memv #{atom-key\ 3958}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 3949}#
                        #{var\ 3957}#
                        #{exp\ 3951}#)
                       (#{decorate-source\ 3217}#
                         (list (quote set!) #{var\ 3957}# #{exp\ 3951}#)
                         #{source\ 3949}#)))))))
           (#{build-global-reference\ 3224}#
             (lambda (#{source\ 3959}# #{var\ 3960}# #{mod\ 3961}#)
               (#{analyze-variable\ 3223}#
                 #{mod\ 3961}#
                 #{var\ 3960}#
                 (lambda (#{mod\ 3962}# #{var\ 3963}# #{public?\ 3964}#)
                   (let ((#{atom-key\ 3965}# (fluid-ref #{*mode*\ 3208}#)))
                     (if (memv #{atom-key\ 3965}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 3959}#
                        #{mod\ 3962}#
                        #{var\ 3963}#
                        #{public?\ 3964}#)
                       (#{decorate-source\ 3217}#
                         (list (if #{public?\ 3964}# (quote @) (quote @@))
                               #{mod\ 3962}#
                               #{var\ 3963}#)
                         #{source\ 3959}#))))
                 (lambda (#{var\ 3966}#)
                   (let ((#{atom-key\ 3967}# (fluid-ref #{*mode*\ 3208}#)))
                     (if (memv #{atom-key\ 3967}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 3959}#
                        #{var\ 3966}#)
                       (#{decorate-source\ 3217}#
                         #{var\ 3966}#
                         #{source\ 3959}#)))))))
           (#{analyze-variable\ 3223}#
             (lambda (#{mod\ 3968}#
                      #{var\ 3969}#
                      #{modref-cont\ 3970}#
                      #{bare-cont\ 3971}#)
               (if (not #{mod\ 3968}#)
                 (#{bare-cont\ 3971}# #{var\ 3969}#)
                 (let ((#{kind\ 3972}# (car #{mod\ 3968}#))
                       (#{mod\ 3973}# (cdr #{mod\ 3968}#)))
                   (if (memv #{kind\ 3972}# (quote (public)))
                     (#{modref-cont\ 3970}#
                       #{mod\ 3973}#
                       #{var\ 3969}#
                       #t)
                     (if (memv #{kind\ 3972}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 3973}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 3970}#
                           #{mod\ 3973}#
                           #{var\ 3969}#
                           #f)
                         (#{bare-cont\ 3971}# #{var\ 3969}#))
                       (if (memv #{kind\ 3972}# (quote (bare)))
                         (#{bare-cont\ 3971}# #{var\ 3969}#)
                         (if (memv #{kind\ 3972}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 3973}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 3973}#)
                                   #{var\ 3969}#)
                                 #f)
                             (#{modref-cont\ 3970}#
                               #{mod\ 3973}#
                               #{var\ 3969}#
                               #f)
                             (#{bare-cont\ 3971}# #{var\ 3969}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 3969}#
                             #{mod\ 3973}#)))))))))
           (#{build-lexical-assignment\ 3222}#
             (lambda (#{source\ 3974}#
                      #{name\ 3975}#
                      #{var\ 3976}#
                      #{exp\ 3977}#)
               (let ((#{atom-key\ 3978}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3978}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 3974}#
                    #{name\ 3975}#
                    #{var\ 3976}#
                    #{exp\ 3977}#)
                   (#{decorate-source\ 3217}#
                     (list (quote set!) #{var\ 3976}# #{exp\ 3977}#)
                     #{source\ 3974}#)))))
           (#{build-lexical-reference\ 3221}#
             (lambda (#{type\ 3979}#
                      #{source\ 3980}#
                      #{name\ 3981}#
                      #{var\ 3982}#)
               (let ((#{atom-key\ 3983}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3983}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 3980}#
                    #{name\ 3981}#
                    #{var\ 3982}#)
                   (#{decorate-source\ 3217}#
                     #{var\ 3982}#
                     #{source\ 3980}#)))))
           (#{build-conditional\ 3220}#
             (lambda (#{source\ 3984}#
                      #{test-exp\ 3985}#
                      #{then-exp\ 3986}#
                      #{else-exp\ 3987}#)
               (let ((#{atom-key\ 3988}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3988}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 3984}#
                    #{test-exp\ 3985}#
                    #{then-exp\ 3986}#
                    #{else-exp\ 3987}#)
                   (#{decorate-source\ 3217}#
                     (if (equal? #{else-exp\ 3987}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 3985}#
                             #{then-exp\ 3986}#)
                       (list 'if
                             #{test-exp\ 3985}#
                             #{then-exp\ 3986}#
                             #{else-exp\ 3987}#))
                     #{source\ 3984}#)))))
           (#{build-application\ 3219}#
             (lambda (#{source\ 3989}#
                      #{fun-exp\ 3990}#
                      #{arg-exps\ 3991}#)
               (let ((#{atom-key\ 3992}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3992}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 3989}#
                    #{fun-exp\ 3990}#
                    #{arg-exps\ 3991}#)
                   (#{decorate-source\ 3217}#
                     (cons #{fun-exp\ 3990}# #{arg-exps\ 3991}#)
                     #{source\ 3989}#)))))
           (#{build-void\ 3218}#
             (lambda (#{source\ 3993}#)
               (let ((#{atom-key\ 3994}# (fluid-ref #{*mode*\ 3208}#)))
                 (if (memv #{atom-key\ 3994}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 3993}#)
                   (#{decorate-source\ 3217}#
                     '(if #f #f)
                     #{source\ 3993}#)))))
           (#{decorate-source\ 3217}#
             (lambda (#{e\ 3995}# #{s\ 3996}#)
               (begin
                 (if (if (pair? #{e\ 3995}#) #{s\ 3996}# #f)
                   (set-source-properties! #{e\ 3995}# #{s\ 3996}#))
                 #{e\ 3995}#)))
           (#{get-global-definition-hook\ 3216}#
             (lambda (#{symbol\ 3997}# #{module\ 3998}#)
               (begin
                 (if (if (not #{module\ 3998}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 3997}#))
                 (let ((#{v\ 3999}#
                         (module-variable
                           (if #{module\ 3998}#
                             (resolve-module (cdr #{module\ 3998}#))
                             (current-module))
                           #{symbol\ 3997}#)))
                   (if #{v\ 3999}#
                     (if (variable-bound? #{v\ 3999}#)
                       (let ((#{val\ 4000}# (variable-ref #{v\ 3999}#)))
                         (if (macro? #{val\ 4000}#)
                           (if (syncase-macro-type #{val\ 4000}#)
                             (cons (syncase-macro-type #{val\ 4000}#)
                                   (syncase-macro-binding #{val\ 4000}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 3215}#
             (lambda (#{symbol\ 4001}# #{type\ 4002}# #{val\ 4003}#)
               (let ((#{existing\ 4004}#
                       (let ((#{v\ 4005}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 4001}#)))
                         (if #{v\ 4005}#
                           (if (variable-bound? #{v\ 4005}#)
                             (let ((#{val\ 4006}# (variable-ref #{v\ 4005}#)))
                               (if (macro? #{val\ 4006}#)
                                 (if (not (syncase-macro-type #{val\ 4006}#))
                                   #{val\ 4006}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 4001}#
                   (if #{existing\ 4004}#
                     (make-extended-syncase-macro
                       #{existing\ 4004}#
                       #{type\ 4002}#
                       #{val\ 4003}#)
                     (make-syncase-macro #{type\ 4002}# #{val\ 4003}#))))))
           (#{local-eval-hook\ 3214}#
             (lambda (#{x\ 4007}# #{mod\ 4008}#)
               (primitive-eval
                 (list #{noexpand\ 3207}#
                       (let ((#{atom-key\ 4009}# (fluid-ref #{*mode*\ 3208}#)))
                         (if (memv #{atom-key\ 4009}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 4007}#)
                           #{x\ 4007}#))))))
           (#{top-level-eval-hook\ 3213}#
             (lambda (#{x\ 4010}# #{mod\ 4011}#)
               (primitive-eval
                 (list #{noexpand\ 3207}#
                       (let ((#{atom-key\ 4012}# (fluid-ref #{*mode*\ 3208}#)))
                         (if (memv #{atom-key\ 4012}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 4010}#)
                           #{x\ 4010}#))))))
           (#{fx<\ 3212}# <)
           (#{fx=\ 3211}# =)
           (#{fx-\ 3210}# -)
           (#{fx+\ 3209}# +)
           (#{*mode*\ 3208}# (make-fluid))
           (#{noexpand\ 3207}# "noexpand"))
    (begin
      (#{global-extend\ 3252}#
        'local-syntax
        'letrec-syntax
        #t)
      (#{global-extend\ 3252}#
        'local-syntax
        'let-syntax
        #f)
      (#{global-extend\ 3252}#
        'core
        'fluid-let-syntax
        (lambda (#{e\ 4013}#
                 #{r\ 4014}#
                 #{w\ 4015}#
                 #{s\ 4016}#
                 #{mod\ 4017}#)
          ((lambda (#{tmp\ 4018}#)
             ((lambda (#{tmp\ 4019}#)
                (if (if #{tmp\ 4019}#
                      (apply (lambda (#{_\ 4020}#
                                      #{var\ 4021}#
                                      #{val\ 4022}#
                                      #{e1\ 4023}#
                                      #{e2\ 4024}#)
                               (#{valid-bound-ids?\ 3279}# #{var\ 4021}#))
                             #{tmp\ 4019}#)
                      #f)
                  (apply (lambda (#{_\ 4026}#
                                  #{var\ 4027}#
                                  #{val\ 4028}#
                                  #{e1\ 4029}#
                                  #{e2\ 4030}#)
                           (let ((#{names\ 4031}#
                                   (map (lambda (#{x\ 4032}#)
                                          (#{id-var-name\ 3276}#
                                            #{x\ 4032}#
                                            #{w\ 4015}#))
                                        #{var\ 4027}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 4034}# #{n\ 4035}#)
                                   (let ((#{atom-key\ 4036}#
                                           (#{binding-type\ 3246}#
                                             (#{lookup\ 3251}#
                                               #{n\ 4035}#
                                               #{r\ 4014}#
                                               #{mod\ 4017}#))))
                                     (if (memv #{atom-key\ 4036}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 4013}#
                                         (#{source-wrap\ 3283}#
                                           #{id\ 4034}#
                                           #{w\ 4015}#
                                           #{s\ 4016}#
                                           #{mod\ 4017}#)))))
                                 #{var\ 4027}#
                                 #{names\ 4031}#)
                               (#{chi-body\ 3294}#
                                 (cons #{e1\ 4029}# #{e2\ 4030}#)
                                 (#{source-wrap\ 3283}#
                                   #{e\ 4013}#
                                   #{w\ 4015}#
                                   #{s\ 4016}#
                                   #{mod\ 4017}#)
                                 (#{extend-env\ 3248}#
                                   #{names\ 4031}#
                                   (let ((#{trans-r\ 4039}#
                                           (#{macros-only-env\ 3250}#
                                             #{r\ 4014}#)))
                                     (map (lambda (#{x\ 4040}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 3296}#
                                                    (#{chi\ 3290}#
                                                      #{x\ 4040}#
                                                      #{trans-r\ 4039}#
                                                      #{w\ 4015}#
                                                      #{mod\ 4017}#)
                                                    #{mod\ 4017}#)))
                                          #{val\ 4028}#))
                                   #{r\ 4014}#)
                                 #{w\ 4015}#
                                 #{mod\ 4017}#))))
                         #{tmp\ 4019}#)
                  ((lambda (#{_\ 4042}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 3283}#
                         #{e\ 4013}#
                         #{w\ 4015}#
                         #{s\ 4016}#
                         #{mod\ 4017}#)))
                   #{tmp\ 4018}#)))
              ($sc-dispatch
                #{tmp\ 4018}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 4013}#)))
      (#{global-extend\ 3252}#
        'core
        'quote
        (lambda (#{e\ 4043}#
                 #{r\ 4044}#
                 #{w\ 4045}#
                 #{s\ 4046}#
                 #{mod\ 4047}#)
          ((lambda (#{tmp\ 4048}#)
             ((lambda (#{tmp\ 4049}#)
                (if #{tmp\ 4049}#
                  (apply (lambda (#{_\ 4050}# #{e\ 4051}#)
                           (#{build-data\ 3232}#
                             #{s\ 4046}#
                             (#{strip\ 3299}# #{e\ 4051}# #{w\ 4045}#)))
                         #{tmp\ 4049}#)
                  ((lambda (#{_\ 4052}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 3283}#
                         #{e\ 4043}#
                         #{w\ 4045}#
                         #{s\ 4046}#
                         #{mod\ 4047}#)))
                   #{tmp\ 4048}#)))
              ($sc-dispatch #{tmp\ 4048}# (quote (any any)))))
           #{e\ 4043}#)))
      (#{global-extend\ 3252}#
        'core
        'syntax
        (letrec ((#{regen\ 4060}#
                   (lambda (#{x\ 4061}#)
                     (let ((#{atom-key\ 4062}# (car #{x\ 4061}#)))
                       (if (memv #{atom-key\ 4062}# (quote (ref)))
                         (#{build-lexical-reference\ 3221}#
                           'value
                           #f
                           (cadr #{x\ 4061}#)
                           (cadr #{x\ 4061}#))
                         (if (memv #{atom-key\ 4062}# (quote (primitive)))
                           (#{build-primref\ 3231}# #f (cadr #{x\ 4061}#))
                           (if (memv #{atom-key\ 4062}# (quote (quote)))
                             (#{build-data\ 3232}# #f (cadr #{x\ 4061}#))
                             (if (memv #{atom-key\ 4062}# (quote (lambda)))
                               (if (list? (cadr #{x\ 4061}#))
                                 (#{build-simple-lambda\ 3228}#
                                   #f
                                   (cadr #{x\ 4061}#)
                                   #f
                                   (cadr #{x\ 4061}#)
                                   #f
                                   (#{regen\ 4060}# (caddr #{x\ 4061}#)))
                                 (error "how did we get here" #{x\ 4061}#))
                               (#{build-application\ 3219}#
                                 #f
                                 (#{build-primref\ 3231}# #f (car #{x\ 4061}#))
                                 (map #{regen\ 4060}#
                                      (cdr #{x\ 4061}#))))))))))
                 (#{gen-vector\ 4059}#
                   (lambda (#{x\ 4063}#)
                     (if (eq? (car #{x\ 4063}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 4063}#))
                       (if (eq? (car #{x\ 4063}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 4063}#)))
                         (list (quote list->vector) #{x\ 4063}#)))))
                 (#{gen-append\ 4058}#
                   (lambda (#{x\ 4064}# #{y\ 4065}#)
                     (if (equal? #{y\ 4065}# (quote (quote ())))
                       #{x\ 4064}#
                       (list (quote append) #{x\ 4064}# #{y\ 4065}#))))
                 (#{gen-cons\ 4057}#
                   (lambda (#{x\ 4066}# #{y\ 4067}#)
                     (let ((#{atom-key\ 4068}# (car #{y\ 4067}#)))
                       (if (memv #{atom-key\ 4068}# (quote (quote)))
                         (if (eq? (car #{x\ 4066}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 4066}#) (cadr #{y\ 4067}#)))
                           (if (eq? (cadr #{y\ 4067}#) (quote ()))
                             (list (quote list) #{x\ 4066}#)
                             (list (quote cons) #{x\ 4066}# #{y\ 4067}#)))
                         (if (memv #{atom-key\ 4068}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 4066}# (cdr #{y\ 4067}#)))
                           (list (quote cons) #{x\ 4066}# #{y\ 4067}#))))))
                 (#{gen-map\ 4056}#
                   (lambda (#{e\ 4069}# #{map-env\ 4070}#)
                     (let ((#{formals\ 4071}# (map cdr #{map-env\ 4070}#))
                           (#{actuals\ 4072}#
                             (map (lambda (#{x\ 4073}#)
                                    (list (quote ref) (car #{x\ 4073}#)))
                                  #{map-env\ 4070}#)))
                       (if (eq? (car #{e\ 4069}#) (quote ref))
                         (car #{actuals\ 4072}#)
                         (if (and-map
                               (lambda (#{x\ 4074}#)
                                 (if (eq? (car #{x\ 4074}#) (quote ref))
                                   (memq (cadr #{x\ 4074}#) #{formals\ 4071}#)
                                   #f))
                               (cdr #{e\ 4069}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 4069}#))
                                       (map (let ((#{r\ 4075}#
                                                    (map cons
                                                         #{formals\ 4071}#
                                                         #{actuals\ 4072}#)))
                                              (lambda (#{x\ 4076}#)
                                                (cdr (assq (cadr #{x\ 4076}#)
                                                           #{r\ 4075}#))))
                                            (cdr #{e\ 4069}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 4071}#
                                             #{e\ 4069}#)
                                       #{actuals\ 4072}#)))))))
                 (#{gen-mappend\ 4055}#
                   (lambda (#{e\ 4077}# #{map-env\ 4078}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 4056}# #{e\ 4077}# #{map-env\ 4078}#))))
                 (#{gen-ref\ 4054}#
                   (lambda (#{src\ 4079}#
                            #{var\ 4080}#
                            #{level\ 4081}#
                            #{maps\ 4082}#)
                     (if (#{fx=\ 3211}# #{level\ 4081}# 0)
                       (values #{var\ 4080}# #{maps\ 4082}#)
                       (if (null? #{maps\ 4082}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 4079}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 4054}#
                               #{src\ 4079}#
                               #{var\ 4080}#
                               (#{fx-\ 3210}# #{level\ 4081}# 1)
                               (cdr #{maps\ 4082}#)))
                           (lambda (#{outer-var\ 4083}# #{outer-maps\ 4084}#)
                             (let ((#{b\ 4085}#
                                     (assq #{outer-var\ 4083}#
                                           (car #{maps\ 4082}#))))
                               (if #{b\ 4085}#
                                 (values (cdr #{b\ 4085}#) #{maps\ 4082}#)
                                 (let ((#{inner-var\ 4086}#
                                         (#{gen-var\ 3300}# (quote tmp))))
                                   (values
                                     #{inner-var\ 4086}#
                                     (cons (cons (cons #{outer-var\ 4083}#
                                                       #{inner-var\ 4086}#)
                                                 (car #{maps\ 4082}#))
                                           #{outer-maps\ 4084}#)))))))))))
                 (#{gen-syntax\ 4053}#
                   (lambda (#{src\ 4087}#
                            #{e\ 4088}#
                            #{r\ 4089}#
                            #{maps\ 4090}#
                            #{ellipsis?\ 4091}#
                            #{mod\ 4092}#)
                     (if (#{id?\ 3254}# #{e\ 4088}#)
                       (let ((#{label\ 4093}#
                               (#{id-var-name\ 3276}#
                                 #{e\ 4088}#
                                 '(()))))
                         (let ((#{b\ 4094}#
                                 (#{lookup\ 3251}#
                                   #{label\ 4093}#
                                   #{r\ 4089}#
                                   #{mod\ 4092}#)))
                           (if (eq? (#{binding-type\ 3246}# #{b\ 4094}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 4095}#
                                         (#{binding-value\ 3247}#
                                           #{b\ 4094}#)))
                                   (#{gen-ref\ 4054}#
                                     #{src\ 4087}#
                                     (car #{var.lev\ 4095}#)
                                     (cdr #{var.lev\ 4095}#)
                                     #{maps\ 4090}#)))
                               (lambda (#{var\ 4096}# #{maps\ 4097}#)
                                 (values
                                   (list (quote ref) #{var\ 4096}#)
                                   #{maps\ 4097}#)))
                             (if (#{ellipsis?\ 4091}# #{e\ 4088}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 4087}#)
                               (values
                                 (list (quote quote) #{e\ 4088}#)
                                 #{maps\ 4090}#)))))
                       ((lambda (#{tmp\ 4098}#)
                          ((lambda (#{tmp\ 4099}#)
                             (if (if #{tmp\ 4099}#
                                   (apply (lambda (#{dots\ 4100}# #{e\ 4101}#)
                                            (#{ellipsis?\ 4091}#
                                              #{dots\ 4100}#))
                                          #{tmp\ 4099}#)
                                   #f)
                               (apply (lambda (#{dots\ 4102}# #{e\ 4103}#)
                                        (#{gen-syntax\ 4053}#
                                          #{src\ 4087}#
                                          #{e\ 4103}#
                                          #{r\ 4089}#
                                          #{maps\ 4090}#
                                          (lambda (#{x\ 4104}#) #f)
                                          #{mod\ 4092}#))
                                      #{tmp\ 4099}#)
                               ((lambda (#{tmp\ 4105}#)
                                  (if (if #{tmp\ 4105}#
                                        (apply (lambda (#{x\ 4106}#
                                                        #{dots\ 4107}#
                                                        #{y\ 4108}#)
                                                 (#{ellipsis?\ 4091}#
                                                   #{dots\ 4107}#))
                                               #{tmp\ 4105}#)
                                        #f)
                                    (apply (lambda (#{x\ 4109}#
                                                    #{dots\ 4110}#
                                                    #{y\ 4111}#)
                                             (letrec ((#{f\ 4112}#
                                                        (lambda (#{y\ 4113}#
                                                                 #{k\ 4114}#)
                                                          ((lambda (#{tmp\ 4118}#)
                                                             ((lambda (#{tmp\ 4119}#)
                                                                (if (if #{tmp\ 4119}#
                                                                      (apply (lambda (#{dots\ 4120}#
                                                                                      #{y\ 4121}#)
                                                                               (#{ellipsis?\ 4091}#
                                                                                 #{dots\ 4120}#))
                                                                             #{tmp\ 4119}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 4122}#
                                                                                  #{y\ 4123}#)
                                                                           (#{f\ 4112}#
                                                                             #{y\ 4123}#
                                                                             (lambda (#{maps\ 4124}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 4114}#
                                                                                     (cons '()
                                                                                           #{maps\ 4124}#)))
                                                                                 (lambda (#{x\ 4125}#
                                                                                          #{maps\ 4126}#)
                                                                                   (if (null? (car #{maps\ 4126}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 4087}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 4055}#
                                                                                         #{x\ 4125}#
                                                                                         (car #{maps\ 4126}#))
                                                                                       (cdr #{maps\ 4126}#))))))))
                                                                         #{tmp\ 4119}#)
                                                                  ((lambda (#{_\ 4127}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 4053}#
                                                                           #{src\ 4087}#
                                                                           #{y\ 4113}#
                                                                           #{r\ 4089}#
                                                                           #{maps\ 4090}#
                                                                           #{ellipsis?\ 4091}#
                                                                           #{mod\ 4092}#))
                                                                       (lambda (#{y\ 4128}#
                                                                                #{maps\ 4129}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 4114}#
                                                                               #{maps\ 4129}#))
                                                                           (lambda (#{x\ 4130}#
                                                                                    #{maps\ 4131}#)
                                                                             (values
                                                                               (#{gen-append\ 4058}#
                                                                                 #{x\ 4130}#
                                                                                 #{y\ 4128}#)
                                                                               #{maps\ 4131}#))))))
                                                                   #{tmp\ 4118}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 4118}#
                                                                '(any . any))))
                                                           #{y\ 4113}#))))
                                               (#{f\ 4112}#
                                                 #{y\ 4111}#
                                                 (lambda (#{maps\ 4115}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 4053}#
                                                         #{src\ 4087}#
                                                         #{x\ 4109}#
                                                         #{r\ 4089}#
                                                         (cons '()
                                                               #{maps\ 4115}#)
                                                         #{ellipsis?\ 4091}#
                                                         #{mod\ 4092}#))
                                                     (lambda (#{x\ 4116}#
                                                              #{maps\ 4117}#)
                                                       (if (null? (car #{maps\ 4117}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 4087}#)
                                                         (values
                                                           (#{gen-map\ 4056}#
                                                             #{x\ 4116}#
                                                             (car #{maps\ 4117}#))
                                                           (cdr #{maps\ 4117}#)))))))))
                                           #{tmp\ 4105}#)
                                    ((lambda (#{tmp\ 4132}#)
                                       (if #{tmp\ 4132}#
                                         (apply (lambda (#{x\ 4133}#
                                                         #{y\ 4134}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 4053}#
                                                        #{src\ 4087}#
                                                        #{x\ 4133}#
                                                        #{r\ 4089}#
                                                        #{maps\ 4090}#
                                                        #{ellipsis?\ 4091}#
                                                        #{mod\ 4092}#))
                                                    (lambda (#{x\ 4135}#
                                                             #{maps\ 4136}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 4053}#
                                                            #{src\ 4087}#
                                                            #{y\ 4134}#
                                                            #{r\ 4089}#
                                                            #{maps\ 4136}#
                                                            #{ellipsis?\ 4091}#
                                                            #{mod\ 4092}#))
                                                        (lambda (#{y\ 4137}#
                                                                 #{maps\ 4138}#)
                                                          (values
                                                            (#{gen-cons\ 4057}#
                                                              #{x\ 4135}#
                                                              #{y\ 4137}#)
                                                            #{maps\ 4138}#))))))
                                                #{tmp\ 4132}#)
                                         ((lambda (#{tmp\ 4139}#)
                                            (if #{tmp\ 4139}#
                                              (apply (lambda (#{e1\ 4140}#
                                                              #{e2\ 4141}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 4053}#
                                                             #{src\ 4087}#
                                                             (cons #{e1\ 4140}#
                                                                   #{e2\ 4141}#)
                                                             #{r\ 4089}#
                                                             #{maps\ 4090}#
                                                             #{ellipsis?\ 4091}#
                                                             #{mod\ 4092}#))
                                                         (lambda (#{e\ 4143}#
                                                                  #{maps\ 4144}#)
                                                           (values
                                                             (#{gen-vector\ 4059}#
                                                               #{e\ 4143}#)
                                                             #{maps\ 4144}#))))
                                                     #{tmp\ 4139}#)
                                              ((lambda (#{_\ 4145}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 4088}#)
                                                   #{maps\ 4090}#))
                                               #{tmp\ 4098}#)))
                                          ($sc-dispatch
                                            #{tmp\ 4098}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 4098}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 4098}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 4098}# (quote (any any)))))
                        #{e\ 4088}#)))))
          (lambda (#{e\ 4146}#
                   #{r\ 4147}#
                   #{w\ 4148}#
                   #{s\ 4149}#
                   #{mod\ 4150}#)
            (let ((#{e\ 4151}#
                    (#{source-wrap\ 3283}#
                      #{e\ 4146}#
                      #{w\ 4148}#
                      #{s\ 4149}#
                      #{mod\ 4150}#)))
              ((lambda (#{tmp\ 4152}#)
                 ((lambda (#{tmp\ 4153}#)
                    (if #{tmp\ 4153}#
                      (apply (lambda (#{_\ 4154}# #{x\ 4155}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 4053}#
                                     #{e\ 4151}#
                                     #{x\ 4155}#
                                     #{r\ 4147}#
                                     '()
                                     #{ellipsis?\ 3298}#
                                     #{mod\ 4150}#))
                                 (lambda (#{e\ 4156}# #{maps\ 4157}#)
                                   (#{regen\ 4060}# #{e\ 4156}#))))
                             #{tmp\ 4153}#)
                      ((lambda (#{_\ 4158}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 4151}#))
                       #{tmp\ 4152}#)))
                  ($sc-dispatch #{tmp\ 4152}# (quote (any any)))))
               #{e\ 4151}#)))))
      (#{global-extend\ 3252}#
        'core
        'lambda
        (lambda (#{e\ 4159}#
                 #{r\ 4160}#
                 #{w\ 4161}#
                 #{s\ 4162}#
                 #{mod\ 4163}#)
          (letrec ((#{docstring&body\ 4164}#
                     (lambda (#{ids\ 4165}#
                              #{vars\ 4166}#
                              #{labels\ 4167}#
                              #{c\ 4168}#)
                       ((lambda (#{tmp\ 4169}#)
                          ((lambda (#{tmp\ 4170}#)
                             (if (if #{tmp\ 4170}#
                                   (apply (lambda (#{docstring\ 4171}#
                                                   #{e1\ 4172}#
                                                   #{e2\ 4173}#)
                                            (string?
                                              (syntax->datum
                                                #{docstring\ 4171}#)))
                                          #{tmp\ 4170}#)
                                   #f)
                               (apply (lambda (#{docstring\ 4174}#
                                               #{e1\ 4175}#
                                               #{e2\ 4176}#)
                                        (values
                                          (syntax->datum #{docstring\ 4174}#)
                                          (#{chi-body\ 3294}#
                                            (cons #{e1\ 4175}# #{e2\ 4176}#)
                                            (#{source-wrap\ 3283}#
                                              #{e\ 4159}#
                                              #{w\ 4161}#
                                              #{s\ 4162}#
                                              #{mod\ 4163}#)
                                            (#{extend-var-env\ 3249}#
                                              #{labels\ 4167}#
                                              #{vars\ 4166}#
                                              #{r\ 4160}#)
                                            (#{make-binding-wrap\ 3271}#
                                              #{ids\ 4165}#
                                              #{labels\ 4167}#
                                              #{w\ 4161}#)
                                            #{mod\ 4163}#)))
                                      #{tmp\ 4170}#)
                               ((lambda (#{tmp\ 4178}#)
                                  (if #{tmp\ 4178}#
                                    (apply (lambda (#{e1\ 4179}# #{e2\ 4180}#)
                                             (values
                                               #f
                                               (#{chi-body\ 3294}#
                                                 (cons #{e1\ 4179}#
                                                       #{e2\ 4180}#)
                                                 (#{source-wrap\ 3283}#
                                                   #{e\ 4159}#
                                                   #{w\ 4161}#
                                                   #{s\ 4162}#
                                                   #{mod\ 4163}#)
                                                 (#{extend-var-env\ 3249}#
                                                   #{labels\ 4167}#
                                                   #{vars\ 4166}#
                                                   #{r\ 4160}#)
                                                 (#{make-binding-wrap\ 3271}#
                                                   #{ids\ 4165}#
                                                   #{labels\ 4167}#
                                                   #{w\ 4161}#)
                                                 #{mod\ 4163}#)))
                                           #{tmp\ 4178}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp\ 4169}#)))
                                ($sc-dispatch
                                  #{tmp\ 4169}#
                                  '(any . each-any)))))
                           ($sc-dispatch
                             #{tmp\ 4169}#
                             '(any any . each-any))))
                        #{c\ 4168}#))))
            ((lambda (#{tmp\ 4182}#)
               ((lambda (#{tmp\ 4183}#)
                  (if #{tmp\ 4183}#
                    (apply (lambda (#{_\ 4184}#
                                    #{id\ 4185}#
                                    #{e1\ 4186}#
                                    #{e2\ 4187}#)
                             (let ((#{ids\ 4188}# #{id\ 4185}#))
                               (if (not (#{valid-bound-ids?\ 3279}#
                                          #{ids\ 4188}#))
                                 (syntax-violation
                                   'lambda
                                   "invalid parameter list"
                                   #{e\ 4159}#)
                                 (let ((#{vars\ 4190}#
                                         (map #{gen-var\ 3300}# #{ids\ 4188}#))
                                       (#{labels\ 4191}#
                                         (#{gen-labels\ 3260}# #{ids\ 4188}#)))
                                   (call-with-values
                                     (lambda ()
                                       (#{docstring&body\ 4164}#
                                         #{ids\ 4188}#
                                         #{vars\ 4190}#
                                         #{labels\ 4191}#
                                         (cons #{e1\ 4186}# #{e2\ 4187}#)))
                                     (lambda (#{docstring\ 4193}#
                                              #{body\ 4194}#)
                                       (#{build-simple-lambda\ 3228}#
                                         #{s\ 4162}#
                                         (map syntax->datum #{ids\ 4188}#)
                                         #f
                                         #{vars\ 4190}#
                                         #{docstring\ 4193}#
                                         #{body\ 4194}#)))))))
                           #{tmp\ 4183}#)
                    ((lambda (#{tmp\ 4195}#)
                       (if #{tmp\ 4195}#
                         (apply (lambda (#{_\ 4196}#
                                         #{ids\ 4197}#
                                         #{e1\ 4198}#
                                         #{e2\ 4199}#)
                                  (let ((#{rids\ 4200}#
                                          (#{lambda-var-list\ 3301}#
                                            #{ids\ 4197}#)))
                                    (if (not (#{valid-bound-ids?\ 3279}#
                                               #{rids\ 4200}#))
                                      (syntax-violation
                                        'lambda
                                        "invalid parameter list"
                                        #{e\ 4159}#)
                                      (let ((#{req\ 4201}#
                                              (reverse (cdr #{rids\ 4200}#))))
                                        (let ((#{rest\ 4202}#
                                                (car #{rids\ 4200}#)))
                                          (let ((#{rrids\ 4203}#
                                                  (reverse #{rids\ 4200}#)))
                                            (let ((#{vars\ 4204}#
                                                    (map #{gen-var\ 3300}#
                                                         #{rrids\ 4203}#)))
                                              (let ((#{labels\ 4205}#
                                                      (#{gen-labels\ 3260}#
                                                        #{rrids\ 4203}#)))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{docstring&body\ 4164}#
                                                      #{rrids\ 4203}#
                                                      #{vars\ 4204}#
                                                      #{labels\ 4205}#
                                                      (cons #{e1\ 4198}#
                                                            #{e2\ 4199}#)))
                                                  (lambda (#{docstring\ 4207}#
                                                           #{body\ 4208}#)
                                                    (#{build-simple-lambda\ 3228}#
                                                      #{s\ 4162}#
                                                      (map syntax->datum
                                                           #{req\ 4201}#)
                                                      (syntax->datum
                                                        #{rest\ 4202}#)
                                                      #{vars\ 4204}#
                                                      #{docstring\ 4207}#
                                                      #{body\ 4208}#)))))))))))
                                #{tmp\ 4195}#)
                         ((lambda (#{_\ 4209}#)
                            (syntax-violation
                              'lambda
                              "bad lambda"
                              #{e\ 4159}#))
                          #{tmp\ 4182}#)))
                     ($sc-dispatch
                       #{tmp\ 4182}#
                       '(any any any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 4182}#
                  '(any each-any any . each-any))))
             #{e\ 4159}#))))
      (#{global-extend\ 3252}#
        'core
        'let
        (letrec ((#{chi-let\ 4210}#
                   (lambda (#{e\ 4211}#
                            #{r\ 4212}#
                            #{w\ 4213}#
                            #{s\ 4214}#
                            #{mod\ 4215}#
                            #{constructor\ 4216}#
                            #{ids\ 4217}#
                            #{vals\ 4218}#
                            #{exps\ 4219}#)
                     (if (not (#{valid-bound-ids?\ 3279}# #{ids\ 4217}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 4211}#)
                       (let ((#{labels\ 4220}#
                               (#{gen-labels\ 3260}# #{ids\ 4217}#))
                             (#{new-vars\ 4221}#
                               (map #{gen-var\ 3300}# #{ids\ 4217}#)))
                         (let ((#{nw\ 4222}#
                                 (#{make-binding-wrap\ 3271}#
                                   #{ids\ 4217}#
                                   #{labels\ 4220}#
                                   #{w\ 4213}#))
                               (#{nr\ 4223}#
                                 (#{extend-var-env\ 3249}#
                                   #{labels\ 4220}#
                                   #{new-vars\ 4221}#
                                   #{r\ 4212}#)))
                           (#{constructor\ 4216}#
                             #{s\ 4214}#
                             (map syntax->datum #{ids\ 4217}#)
                             #{new-vars\ 4221}#
                             (map (lambda (#{x\ 4224}#)
                                    (#{chi\ 3290}#
                                      #{x\ 4224}#
                                      #{r\ 4212}#
                                      #{w\ 4213}#
                                      #{mod\ 4215}#))
                                  #{vals\ 4218}#)
                             (#{chi-body\ 3294}#
                               #{exps\ 4219}#
                               (#{source-wrap\ 3283}#
                                 #{e\ 4211}#
                                 #{nw\ 4222}#
                                 #{s\ 4214}#
                                 #{mod\ 4215}#)
                               #{nr\ 4223}#
                               #{nw\ 4222}#
                               #{mod\ 4215}#))))))))
          (lambda (#{e\ 4225}#
                   #{r\ 4226}#
                   #{w\ 4227}#
                   #{s\ 4228}#
                   #{mod\ 4229}#)
            ((lambda (#{tmp\ 4230}#)
               ((lambda (#{tmp\ 4231}#)
                  (if (if #{tmp\ 4231}#
                        (apply (lambda (#{_\ 4232}#
                                        #{id\ 4233}#
                                        #{val\ 4234}#
                                        #{e1\ 4235}#
                                        #{e2\ 4236}#)
                                 (and-map #{id?\ 3254}# #{id\ 4233}#))
                               #{tmp\ 4231}#)
                        #f)
                    (apply (lambda (#{_\ 4238}#
                                    #{id\ 4239}#
                                    #{val\ 4240}#
                                    #{e1\ 4241}#
                                    #{e2\ 4242}#)
                             (#{chi-let\ 4210}#
                               #{e\ 4225}#
                               #{r\ 4226}#
                               #{w\ 4227}#
                               #{s\ 4228}#
                               #{mod\ 4229}#
                               #{build-let\ 3234}#
                               #{id\ 4239}#
                               #{val\ 4240}#
                               (cons #{e1\ 4241}# #{e2\ 4242}#)))
                           #{tmp\ 4231}#)
                    ((lambda (#{tmp\ 4246}#)
                       (if (if #{tmp\ 4246}#
                             (apply (lambda (#{_\ 4247}#
                                             #{f\ 4248}#
                                             #{id\ 4249}#
                                             #{val\ 4250}#
                                             #{e1\ 4251}#
                                             #{e2\ 4252}#)
                                      (if (#{id?\ 3254}# #{f\ 4248}#)
                                        (and-map #{id?\ 3254}# #{id\ 4249}#)
                                        #f))
                                    #{tmp\ 4246}#)
                             #f)
                         (apply (lambda (#{_\ 4254}#
                                         #{f\ 4255}#
                                         #{id\ 4256}#
                                         #{val\ 4257}#
                                         #{e1\ 4258}#
                                         #{e2\ 4259}#)
                                  (#{chi-let\ 4210}#
                                    #{e\ 4225}#
                                    #{r\ 4226}#
                                    #{w\ 4227}#
                                    #{s\ 4228}#
                                    #{mod\ 4229}#
                                    #{build-named-let\ 3235}#
                                    (cons #{f\ 4255}# #{id\ 4256}#)
                                    #{val\ 4257}#
                                    (cons #{e1\ 4258}# #{e2\ 4259}#)))
                                #{tmp\ 4246}#)
                         ((lambda (#{_\ 4263}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 3283}#
                                #{e\ 4225}#
                                #{w\ 4227}#
                                #{s\ 4228}#
                                #{mod\ 4229}#)))
                          #{tmp\ 4230}#)))
                     ($sc-dispatch
                       #{tmp\ 4230}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 4230}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 4225}#))))
      (#{global-extend\ 3252}#
        'core
        'letrec
        (lambda (#{e\ 4264}#
                 #{r\ 4265}#
                 #{w\ 4266}#
                 #{s\ 4267}#
                 #{mod\ 4268}#)
          ((lambda (#{tmp\ 4269}#)
             ((lambda (#{tmp\ 4270}#)
                (if (if #{tmp\ 4270}#
                      (apply (lambda (#{_\ 4271}#
                                      #{id\ 4272}#
                                      #{val\ 4273}#
                                      #{e1\ 4274}#
                                      #{e2\ 4275}#)
                               (and-map #{id?\ 3254}# #{id\ 4272}#))
                             #{tmp\ 4270}#)
                      #f)
                  (apply (lambda (#{_\ 4277}#
                                  #{id\ 4278}#
                                  #{val\ 4279}#
                                  #{e1\ 4280}#
                                  #{e2\ 4281}#)
                           (let ((#{ids\ 4282}# #{id\ 4278}#))
                             (if (not (#{valid-bound-ids?\ 3279}#
                                        #{ids\ 4282}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 4264}#)
                               (let ((#{labels\ 4284}#
                                       (#{gen-labels\ 3260}# #{ids\ 4282}#))
                                     (#{new-vars\ 4285}#
                                       (map #{gen-var\ 3300}# #{ids\ 4282}#)))
                                 (let ((#{w\ 4286}#
                                         (#{make-binding-wrap\ 3271}#
                                           #{ids\ 4282}#
                                           #{labels\ 4284}#
                                           #{w\ 4266}#))
                                       (#{r\ 4287}#
                                         (#{extend-var-env\ 3249}#
                                           #{labels\ 4284}#
                                           #{new-vars\ 4285}#
                                           #{r\ 4265}#)))
                                   (#{build-letrec\ 3236}#
                                     #{s\ 4267}#
                                     (map syntax->datum #{ids\ 4282}#)
                                     #{new-vars\ 4285}#
                                     (map (lambda (#{x\ 4288}#)
                                            (#{chi\ 3290}#
                                              #{x\ 4288}#
                                              #{r\ 4287}#
                                              #{w\ 4286}#
                                              #{mod\ 4268}#))
                                          #{val\ 4279}#)
                                     (#{chi-body\ 3294}#
                                       (cons #{e1\ 4280}# #{e2\ 4281}#)
                                       (#{source-wrap\ 3283}#
                                         #{e\ 4264}#
                                         #{w\ 4286}#
                                         #{s\ 4267}#
                                         #{mod\ 4268}#)
                                       #{r\ 4287}#
                                       #{w\ 4286}#
                                       #{mod\ 4268}#)))))))
                         #{tmp\ 4270}#)
                  ((lambda (#{_\ 4291}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 3283}#
                         #{e\ 4264}#
                         #{w\ 4266}#
                         #{s\ 4267}#
                         #{mod\ 4268}#)))
                   #{tmp\ 4269}#)))
              ($sc-dispatch
                #{tmp\ 4269}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 4264}#)))
      (#{global-extend\ 3252}#
        'core
        'set!
        (lambda (#{e\ 4292}#
                 #{r\ 4293}#
                 #{w\ 4294}#
                 #{s\ 4295}#
                 #{mod\ 4296}#)
          ((lambda (#{tmp\ 4297}#)
             ((lambda (#{tmp\ 4298}#)
                (if (if #{tmp\ 4298}#
                      (apply (lambda (#{_\ 4299}# #{id\ 4300}# #{val\ 4301}#)
                               (#{id?\ 3254}# #{id\ 4300}#))
                             #{tmp\ 4298}#)
                      #f)
                  (apply (lambda (#{_\ 4302}# #{id\ 4303}# #{val\ 4304}#)
                           (let ((#{val\ 4305}#
                                   (#{chi\ 3290}#
                                     #{val\ 4304}#
                                     #{r\ 4293}#
                                     #{w\ 4294}#
                                     #{mod\ 4296}#))
                                 (#{n\ 4306}#
                                   (#{id-var-name\ 3276}#
                                     #{id\ 4303}#
                                     #{w\ 4294}#)))
                             (let ((#{b\ 4307}#
                                     (#{lookup\ 3251}#
                                       #{n\ 4306}#
                                       #{r\ 4293}#
                                       #{mod\ 4296}#)))
                               (let ((#{atom-key\ 4308}#
                                       (#{binding-type\ 3246}# #{b\ 4307}#)))
                                 (if (memv #{atom-key\ 4308}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 3222}#
                                     #{s\ 4295}#
                                     (syntax->datum #{id\ 4303}#)
                                     (#{binding-value\ 3247}# #{b\ 4307}#)
                                     #{val\ 4305}#)
                                   (if (memv #{atom-key\ 4308}#
                                             '(global))
                                     (#{build-global-assignment\ 3225}#
                                       #{s\ 4295}#
                                       #{n\ 4306}#
                                       #{val\ 4305}#
                                       #{mod\ 4296}#)
                                     (if (memv #{atom-key\ 4308}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 3282}#
                                           #{id\ 4303}#
                                           #{w\ 4294}#
                                           #{mod\ 4296}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 3283}#
                                           #{e\ 4292}#
                                           #{w\ 4294}#
                                           #{s\ 4295}#
                                           #{mod\ 4296}#)))))))))
                         #{tmp\ 4298}#)
                  ((lambda (#{tmp\ 4309}#)
                     (if #{tmp\ 4309}#
                       (apply (lambda (#{_\ 4310}#
                                       #{head\ 4311}#
                                       #{tail\ 4312}#
                                       #{val\ 4313}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 3288}#
                                      #{head\ 4311}#
                                      #{r\ 4293}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 4296}#
                                      #t))
                                  (lambda (#{type\ 4314}#
                                           #{value\ 4315}#
                                           #{ee\ 4316}#
                                           #{ww\ 4317}#
                                           #{ss\ 4318}#
                                           #{modmod\ 4319}#)
                                    (if (memv #{type\ 4314}#
                                              '(module-ref))
                                      (let ((#{val\ 4320}#
                                              (#{chi\ 3290}#
                                                #{val\ 4313}#
                                                #{r\ 4293}#
                                                #{w\ 4294}#
                                                #{mod\ 4296}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 4315}#
                                              (cons #{head\ 4311}#
                                                    #{tail\ 4312}#)))
                                          (lambda (#{id\ 4322}# #{mod\ 4323}#)
                                            (#{build-global-assignment\ 3225}#
                                              #{s\ 4295}#
                                              #{id\ 4322}#
                                              #{val\ 4320}#
                                              #{mod\ 4323}#))))
                                      (#{build-application\ 3219}#
                                        #{s\ 4295}#
                                        (#{chi\ 3290}#
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
                                                #{head\ 4311}#)
                                          #{r\ 4293}#
                                          #{w\ 4294}#
                                          #{mod\ 4296}#)
                                        (map (lambda (#{e\ 4324}#)
                                               (#{chi\ 3290}#
                                                 #{e\ 4324}#
                                                 #{r\ 4293}#
                                                 #{w\ 4294}#
                                                 #{mod\ 4296}#))
                                             (append
                                               #{tail\ 4312}#
                                               (list #{val\ 4313}#))))))))
                              #{tmp\ 4309}#)
                       ((lambda (#{_\ 4326}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 3283}#
                              #{e\ 4292}#
                              #{w\ 4294}#
                              #{s\ 4295}#
                              #{mod\ 4296}#)))
                        #{tmp\ 4297}#)))
                   ($sc-dispatch
                     #{tmp\ 4297}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 4297}#
                '(any any any))))
           #{e\ 4292}#)))
      (#{global-extend\ 3252}#
        'module-ref
        '@
        (lambda (#{e\ 4327}#)
          ((lambda (#{tmp\ 4328}#)
             ((lambda (#{tmp\ 4329}#)
                (if (if #{tmp\ 4329}#
                      (apply (lambda (#{_\ 4330}# #{mod\ 4331}# #{id\ 4332}#)
                               (if (and-map #{id?\ 3254}# #{mod\ 4331}#)
                                 (#{id?\ 3254}# #{id\ 4332}#)
                                 #f))
                             #{tmp\ 4329}#)
                      #f)
                  (apply (lambda (#{_\ 4334}# #{mod\ 4335}# #{id\ 4336}#)
                           (values
                             (syntax->datum #{id\ 4336}#)
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
                                     #{mod\ 4335}#))))
                         #{tmp\ 4329}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 4328}#)))
              ($sc-dispatch
                #{tmp\ 4328}#
                '(any each-any any))))
           #{e\ 4327}#)))
      (#{global-extend\ 3252}#
        'module-ref
        '@@
        (lambda (#{e\ 4338}#)
          ((lambda (#{tmp\ 4339}#)
             ((lambda (#{tmp\ 4340}#)
                (if (if #{tmp\ 4340}#
                      (apply (lambda (#{_\ 4341}# #{mod\ 4342}# #{id\ 4343}#)
                               (if (and-map #{id?\ 3254}# #{mod\ 4342}#)
                                 (#{id?\ 3254}# #{id\ 4343}#)
                                 #f))
                             #{tmp\ 4340}#)
                      #f)
                  (apply (lambda (#{_\ 4345}# #{mod\ 4346}# #{id\ 4347}#)
                           (values
                             (syntax->datum #{id\ 4347}#)
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
                                     #{mod\ 4346}#))))
                         #{tmp\ 4340}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 4339}#)))
              ($sc-dispatch
                #{tmp\ 4339}#
                '(any each-any any))))
           #{e\ 4338}#)))
      (#{global-extend\ 3252}#
        'core
        'if
        (lambda (#{e\ 4349}#
                 #{r\ 4350}#
                 #{w\ 4351}#
                 #{s\ 4352}#
                 #{mod\ 4353}#)
          ((lambda (#{tmp\ 4354}#)
             ((lambda (#{tmp\ 4355}#)
                (if #{tmp\ 4355}#
                  (apply (lambda (#{_\ 4356}# #{test\ 4357}# #{then\ 4358}#)
                           (#{build-conditional\ 3220}#
                             #{s\ 4352}#
                             (#{chi\ 3290}#
                               #{test\ 4357}#
                               #{r\ 4350}#
                               #{w\ 4351}#
                               #{mod\ 4353}#)
                             (#{chi\ 3290}#
                               #{then\ 4358}#
                               #{r\ 4350}#
                               #{w\ 4351}#
                               #{mod\ 4353}#)
                             (#{build-void\ 3218}# #f)))
                         #{tmp\ 4355}#)
                  ((lambda (#{tmp\ 4359}#)
                     (if #{tmp\ 4359}#
                       (apply (lambda (#{_\ 4360}#
                                       #{test\ 4361}#
                                       #{then\ 4362}#
                                       #{else\ 4363}#)
                                (#{build-conditional\ 3220}#
                                  #{s\ 4352}#
                                  (#{chi\ 3290}#
                                    #{test\ 4361}#
                                    #{r\ 4350}#
                                    #{w\ 4351}#
                                    #{mod\ 4353}#)
                                  (#{chi\ 3290}#
                                    #{then\ 4362}#
                                    #{r\ 4350}#
                                    #{w\ 4351}#
                                    #{mod\ 4353}#)
                                  (#{chi\ 3290}#
                                    #{else\ 4363}#
                                    #{r\ 4350}#
                                    #{w\ 4351}#
                                    #{mod\ 4353}#)))
                              #{tmp\ 4359}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 4354}#)))
                   ($sc-dispatch
                     #{tmp\ 4354}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 4354}#
                '(any any any))))
           #{e\ 4349}#)))
      (#{global-extend\ 3252}#
        'begin
        'begin
        '())
      (#{global-extend\ 3252}#
        'define
        'define
        '())
      (#{global-extend\ 3252}#
        'define-syntax
        'define-syntax
        '())
      (#{global-extend\ 3252}#
        'eval-when
        'eval-when
        '())
      (#{global-extend\ 3252}#
        'core
        'syntax-case
        (letrec ((#{gen-syntax-case\ 4367}#
                   (lambda (#{x\ 4368}#
                            #{keys\ 4369}#
                            #{clauses\ 4370}#
                            #{r\ 4371}#
                            #{mod\ 4372}#)
                     (if (null? #{clauses\ 4370}#)
                       (#{build-application\ 3219}#
                         #f
                         (#{build-primref\ 3231}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 3232}# #f #f)
                               (#{build-data\ 3232}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 4368}#))
                       ((lambda (#{tmp\ 4373}#)
                          ((lambda (#{tmp\ 4374}#)
                             (if #{tmp\ 4374}#
                               (apply (lambda (#{pat\ 4375}# #{exp\ 4376}#)
                                        (if (if (#{id?\ 3254}# #{pat\ 4375}#)
                                              (and-map
                                                (lambda (#{x\ 4377}#)
                                                  (not (#{free-id=?\ 3277}#
                                                         #{pat\ 4375}#
                                                         #{x\ 4377}#)))
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
                                                      #{keys\ 4369}#))
                                              #f)
                                          (let ((#{labels\ 4378}#
                                                  (list (#{gen-label\ 3259}#)))
                                                (#{var\ 4379}#
                                                  (#{gen-var\ 3300}#
                                                    #{pat\ 4375}#)))
                                            (#{build-application\ 3219}#
                                              #f
                                              (#{build-simple-lambda\ 3228}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 4375}#))
                                                #f
                                                (list #{var\ 4379}#)
                                                #f
                                                (#{chi\ 3290}#
                                                  #{exp\ 4376}#
                                                  (#{extend-env\ 3248}#
                                                    #{labels\ 4378}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 4379}#
                                                                      0)))
                                                    #{r\ 4371}#)
                                                  (#{make-binding-wrap\ 3271}#
                                                    (list #{pat\ 4375}#)
                                                    #{labels\ 4378}#
                                                    '(()))
                                                  #{mod\ 4372}#))
                                              (list #{x\ 4368}#)))
                                          (#{gen-clause\ 4366}#
                                            #{x\ 4368}#
                                            #{keys\ 4369}#
                                            (cdr #{clauses\ 4370}#)
                                            #{r\ 4371}#
                                            #{pat\ 4375}#
                                            #t
                                            #{exp\ 4376}#
                                            #{mod\ 4372}#)))
                                      #{tmp\ 4374}#)
                               ((lambda (#{tmp\ 4380}#)
                                  (if #{tmp\ 4380}#
                                    (apply (lambda (#{pat\ 4381}#
                                                    #{fender\ 4382}#
                                                    #{exp\ 4383}#)
                                             (#{gen-clause\ 4366}#
                                               #{x\ 4368}#
                                               #{keys\ 4369}#
                                               (cdr #{clauses\ 4370}#)
                                               #{r\ 4371}#
                                               #{pat\ 4381}#
                                               #{fender\ 4382}#
                                               #{exp\ 4383}#
                                               #{mod\ 4372}#))
                                           #{tmp\ 4380}#)
                                    ((lambda (#{_\ 4384}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 4370}#)))
                                     #{tmp\ 4373}#)))
                                ($sc-dispatch
                                  #{tmp\ 4373}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 4373}# (quote (any any)))))
                        (car #{clauses\ 4370}#)))))
                 (#{gen-clause\ 4366}#
                   (lambda (#{x\ 4385}#
                            #{keys\ 4386}#
                            #{clauses\ 4387}#
                            #{r\ 4388}#
                            #{pat\ 4389}#
                            #{fender\ 4390}#
                            #{exp\ 4391}#
                            #{mod\ 4392}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 4364}#
                           #{pat\ 4389}#
                           #{keys\ 4386}#))
                       (lambda (#{p\ 4393}# #{pvars\ 4394}#)
                         (if (not (#{distinct-bound-ids?\ 3280}#
                                    (map car #{pvars\ 4394}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 4389}#)
                           (if (not (and-map
                                      (lambda (#{x\ 4395}#)
                                        (not (#{ellipsis?\ 3298}#
                                               (car #{x\ 4395}#))))
                                      #{pvars\ 4394}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 4389}#)
                             (let ((#{y\ 4396}#
                                     (#{gen-var\ 3300}# (quote tmp))))
                               (#{build-application\ 3219}#
                                 #f
                                 (#{build-simple-lambda\ 3228}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 4396}#)
                                   #f
                                   (let ((#{y\ 4397}#
                                           (#{build-lexical-reference\ 3221}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 4396}#)))
                                     (#{build-conditional\ 3220}#
                                       #f
                                       ((lambda (#{tmp\ 4398}#)
                                          ((lambda (#{tmp\ 4399}#)
                                             (if #{tmp\ 4399}#
                                               (apply (lambda () #{y\ 4397}#)
                                                      #{tmp\ 4399}#)
                                               ((lambda (#{_\ 4400}#)
                                                  (#{build-conditional\ 3220}#
                                                    #f
                                                    #{y\ 4397}#
                                                    (#{build-dispatch-call\ 4365}#
                                                      #{pvars\ 4394}#
                                                      #{fender\ 4390}#
                                                      #{y\ 4397}#
                                                      #{r\ 4388}#
                                                      #{mod\ 4392}#)
                                                    (#{build-data\ 3232}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 4398}#)))
                                           ($sc-dispatch
                                             #{tmp\ 4398}#
                                             '#(atom #t))))
                                        #{fender\ 4390}#)
                                       (#{build-dispatch-call\ 4365}#
                                         #{pvars\ 4394}#
                                         #{exp\ 4391}#
                                         #{y\ 4397}#
                                         #{r\ 4388}#
                                         #{mod\ 4392}#)
                                       (#{gen-syntax-case\ 4367}#
                                         #{x\ 4385}#
                                         #{keys\ 4386}#
                                         #{clauses\ 4387}#
                                         #{r\ 4388}#
                                         #{mod\ 4392}#))))
                                 (list (if (eq? #{p\ 4393}# (quote any))
                                         (#{build-application\ 3219}#
                                           #f
                                           (#{build-primref\ 3231}#
                                             #f
                                             'list)
                                           (list #{x\ 4385}#))
                                         (#{build-application\ 3219}#
                                           #f
                                           (#{build-primref\ 3231}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 4385}#
                                                 (#{build-data\ 3232}#
                                                   #f
                                                   #{p\ 4393}#)))))))))))))
                 (#{build-dispatch-call\ 4365}#
                   (lambda (#{pvars\ 4401}#
                            #{exp\ 4402}#
                            #{y\ 4403}#
                            #{r\ 4404}#
                            #{mod\ 4405}#)
                     (let ((#{ids\ 4406}# (map car #{pvars\ 4401}#))
                           (#{levels\ 4407}# (map cdr #{pvars\ 4401}#)))
                       (let ((#{labels\ 4408}#
                               (#{gen-labels\ 3260}# #{ids\ 4406}#))
                             (#{new-vars\ 4409}#
                               (map #{gen-var\ 3300}# #{ids\ 4406}#)))
                         (#{build-application\ 3219}#
                           #f
                           (#{build-primref\ 3231}# #f (quote apply))
                           (list (#{build-simple-lambda\ 3228}#
                                   #f
                                   (map syntax->datum #{ids\ 4406}#)
                                   #f
                                   #{new-vars\ 4409}#
                                   #f
                                   (#{chi\ 3290}#
                                     #{exp\ 4402}#
                                     (#{extend-env\ 3248}#
                                       #{labels\ 4408}#
                                       (map (lambda (#{var\ 4410}#
                                                     #{level\ 4411}#)
                                              (cons 'syntax
                                                    (cons #{var\ 4410}#
                                                          #{level\ 4411}#)))
                                            #{new-vars\ 4409}#
                                            (map cdr #{pvars\ 4401}#))
                                       #{r\ 4404}#)
                                     (#{make-binding-wrap\ 3271}#
                                       #{ids\ 4406}#
                                       #{labels\ 4408}#
                                       '(()))
                                     #{mod\ 4405}#))
                                 #{y\ 4403}#))))))
                 (#{convert-pattern\ 4364}#
                   (lambda (#{pattern\ 4412}# #{keys\ 4413}#)
                     (letrec ((#{cvt\ 4414}#
                                (lambda (#{p\ 4415}# #{n\ 4416}# #{ids\ 4417}#)
                                  (if (#{id?\ 3254}# #{p\ 4415}#)
                                    (if (#{bound-id-member?\ 3281}#
                                          #{p\ 4415}#
                                          #{keys\ 4413}#)
                                      (values
                                        (vector (quote free-id) #{p\ 4415}#)
                                        #{ids\ 4417}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 4415}# #{n\ 4416}#)
                                              #{ids\ 4417}#)))
                                    ((lambda (#{tmp\ 4418}#)
                                       ((lambda (#{tmp\ 4419}#)
                                          (if (if #{tmp\ 4419}#
                                                (apply (lambda (#{x\ 4420}#
                                                                #{dots\ 4421}#)
                                                         (#{ellipsis?\ 3298}#
                                                           #{dots\ 4421}#))
                                                       #{tmp\ 4419}#)
                                                #f)
                                            (apply (lambda (#{x\ 4422}#
                                                            #{dots\ 4423}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 4414}#
                                                           #{x\ 4422}#
                                                           (#{fx+\ 3209}#
                                                             #{n\ 4416}#
                                                             1)
                                                           #{ids\ 4417}#))
                                                       (lambda (#{p\ 4424}#
                                                                #{ids\ 4425}#)
                                                         (values
                                                           (if (eq? #{p\ 4424}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 4424}#))
                                                           #{ids\ 4425}#))))
                                                   #{tmp\ 4419}#)
                                            ((lambda (#{tmp\ 4426}#)
                                               (if #{tmp\ 4426}#
                                                 (apply (lambda (#{x\ 4427}#
                                                                 #{y\ 4428}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 4414}#
                                                                #{y\ 4428}#
                                                                #{n\ 4416}#
                                                                #{ids\ 4417}#))
                                                            (lambda (#{y\ 4429}#
                                                                     #{ids\ 4430}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 4414}#
                                                                    #{x\ 4427}#
                                                                    #{n\ 4416}#
                                                                    #{ids\ 4430}#))
                                                                (lambda (#{x\ 4431}#
                                                                         #{ids\ 4432}#)
                                                                  (values
                                                                    (cons #{x\ 4431}#
                                                                          #{y\ 4429}#)
                                                                    #{ids\ 4432}#))))))
                                                        #{tmp\ 4426}#)
                                                 ((lambda (#{tmp\ 4433}#)
                                                    (if #{tmp\ 4433}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 4417}#))
                                                             #{tmp\ 4433}#)
                                                      ((lambda (#{tmp\ 4434}#)
                                                         (if #{tmp\ 4434}#
                                                           (apply (lambda (#{x\ 4435}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 4414}#
                                                                          #{x\ 4435}#
                                                                          #{n\ 4416}#
                                                                          #{ids\ 4417}#))
                                                                      (lambda (#{p\ 4437}#
                                                                               #{ids\ 4438}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 4437}#)
                                                                          #{ids\ 4438}#))))
                                                                  #{tmp\ 4434}#)
                                                           ((lambda (#{x\ 4439}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 3299}#
                                                                    #{p\ 4415}#
                                                                    '(())))
                                                                #{ids\ 4417}#))
                                                            #{tmp\ 4418}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 4418}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 4418}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 4418}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 4418}#
                                          '(any any))))
                                     #{p\ 4415}#)))))
                       (#{cvt\ 4414}# #{pattern\ 4412}# 0 (quote ()))))))
          (lambda (#{e\ 4440}#
                   #{r\ 4441}#
                   #{w\ 4442}#
                   #{s\ 4443}#
                   #{mod\ 4444}#)
            (let ((#{e\ 4445}#
                    (#{source-wrap\ 3283}#
                      #{e\ 4440}#
                      #{w\ 4442}#
                      #{s\ 4443}#
                      #{mod\ 4444}#)))
              ((lambda (#{tmp\ 4446}#)
                 ((lambda (#{tmp\ 4447}#)
                    (if #{tmp\ 4447}#
                      (apply (lambda (#{_\ 4448}#
                                      #{val\ 4449}#
                                      #{key\ 4450}#
                                      #{m\ 4451}#)
                               (if (and-map
                                     (lambda (#{x\ 4452}#)
                                       (if (#{id?\ 3254}# #{x\ 4452}#)
                                         (not (#{ellipsis?\ 3298}#
                                                #{x\ 4452}#))
                                         #f))
                                     #{key\ 4450}#)
                                 (let ((#{x\ 4454}#
                                         (#{gen-var\ 3300}# (quote tmp))))
                                   (#{build-application\ 3219}#
                                     #{s\ 4443}#
                                     (#{build-simple-lambda\ 3228}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 4454}#)
                                       #f
                                       (#{gen-syntax-case\ 4367}#
                                         (#{build-lexical-reference\ 3221}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 4454}#)
                                         #{key\ 4450}#
                                         #{m\ 4451}#
                                         #{r\ 4441}#
                                         #{mod\ 4444}#))
                                     (list (#{chi\ 3290}#
                                             #{val\ 4449}#
                                             #{r\ 4441}#
                                             '(())
                                             #{mod\ 4444}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 4445}#)))
                             #{tmp\ 4447}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 4446}#)))
                  ($sc-dispatch
                    #{tmp\ 4446}#
                    '(any any each-any . each-any))))
               #{e\ 4445}#)))))
      (set! sc-expand
        (lambda (#{x\ 4457}# . #{rest\ 4458}#)
          (if (if (pair? #{x\ 4457}#)
                (equal? (car #{x\ 4457}#) #{noexpand\ 3207}#)
                #f)
            (cadr #{x\ 4457}#)
            (let ((#{m\ 4459}#
                    (if (null? #{rest\ 4458}#)
                      'e
                      (car #{rest\ 4458}#)))
                  (#{esew\ 4460}#
                    (if (let ((#{t\ 4461}# (null? #{rest\ 4458}#)))
                          (if #{t\ 4461}#
                            #{t\ 4461}#
                            (null? (cdr #{rest\ 4458}#))))
                      '(eval)
                      (cadr #{rest\ 4458}#))))
              (with-fluid*
                #{*mode*\ 3208}#
                #{m\ 4459}#
                (lambda ()
                  (#{chi-top\ 3289}#
                    #{x\ 4457}#
                    '()
                    '((top))
                    #{m\ 4459}#
                    #{esew\ 4460}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 4462}#)
          (#{nonsymbol-id?\ 3253}# #{x\ 4462}#)))
      (set! datum->syntax
        (lambda (#{id\ 4463}# #{datum\ 4464}#)
          (#{make-syntax-object\ 3237}#
            #{datum\ 4464}#
            (#{syntax-object-wrap\ 3240}# #{id\ 4463}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 4465}#)
          (#{strip\ 3299}# #{x\ 4465}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 4466}#)
          (begin
            (let ((#{x\ 4467}# #{ls\ 4466}#))
              (if (not (list? #{x\ 4467}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 4467}#)))
            (map (lambda (#{x\ 4468}#)
                   (#{wrap\ 3282}# (gensym) (quote ((top))) #f))
                 #{ls\ 4466}#))))
      (set! free-identifier=?
        (lambda (#{x\ 4469}# #{y\ 4470}#)
          (begin
            (let ((#{x\ 4471}# #{x\ 4469}#))
              (if (not (#{nonsymbol-id?\ 3253}# #{x\ 4471}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 4471}#)))
            (let ((#{x\ 4472}# #{y\ 4470}#))
              (if (not (#{nonsymbol-id?\ 3253}# #{x\ 4472}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 4472}#)))
            (#{free-id=?\ 3277}# #{x\ 4469}# #{y\ 4470}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 4473}# #{y\ 4474}#)
          (begin
            (let ((#{x\ 4475}# #{x\ 4473}#))
              (if (not (#{nonsymbol-id?\ 3253}# #{x\ 4475}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 4475}#)))
            (let ((#{x\ 4476}# #{y\ 4474}#))
              (if (not (#{nonsymbol-id?\ 3253}# #{x\ 4476}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 4476}#)))
            (#{bound-id=?\ 3278}# #{x\ 4473}# #{y\ 4474}#))))
      (set! syntax-violation
        (lambda (#{who\ 4477}#
                 #{message\ 4478}#
                 #{form\ 4479}#
                 .
                 #{subform\ 4480}#)
          (begin
            (let ((#{x\ 4481}# #{who\ 4477}#))
              (if (not ((lambda (#{x\ 4482}#)
                          (let ((#{t\ 4483}# (not #{x\ 4482}#)))
                            (if #{t\ 4483}#
                              #{t\ 4483}#
                              (let ((#{t\ 4484}# (string? #{x\ 4482}#)))
                                (if #{t\ 4484}#
                                  #{t\ 4484}#
                                  (symbol? #{x\ 4482}#))))))
                        #{x\ 4481}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 4481}#)))
            (let ((#{x\ 4485}# #{message\ 4478}#))
              (if (not (string? #{x\ 4485}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 4485}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 4477}# "~a: " "")
                "~a "
                (if (null? #{subform\ 4480}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 4486}#
                      (cons #{message\ 4478}#
                            (map (lambda (#{x\ 4487}#)
                                   (#{strip\ 3299}# #{x\ 4487}# (quote (()))))
                                 (append
                                   #{subform\ 4480}#
                                   (list #{form\ 4479}#))))))
                (if #{who\ 4477}#
                  (cons #{who\ 4477}# #{tail\ 4486}#)
                  #{tail\ 4486}#))
              #f))))
      (letrec ((#{match\ 4492}#
                 (lambda (#{e\ 4493}#
                          #{p\ 4494}#
                          #{w\ 4495}#
                          #{r\ 4496}#
                          #{mod\ 4497}#)
                   (if (not #{r\ 4496}#)
                     #f
                     (if (eq? #{p\ 4494}# (quote any))
                       (cons (#{wrap\ 3282}#
                               #{e\ 4493}#
                               #{w\ 4495}#
                               #{mod\ 4497}#)
                             #{r\ 4496}#)
                       (if (#{syntax-object?\ 3238}# #{e\ 4493}#)
                         (#{match*\ 4491}#
                           (#{syntax-object-expression\ 3239}# #{e\ 4493}#)
                           #{p\ 4494}#
                           (#{join-wraps\ 3273}#
                             #{w\ 4495}#
                             (#{syntax-object-wrap\ 3240}# #{e\ 4493}#))
                           #{r\ 4496}#
                           (#{syntax-object-module\ 3241}# #{e\ 4493}#))
                         (#{match*\ 4491}#
                           #{e\ 4493}#
                           #{p\ 4494}#
                           #{w\ 4495}#
                           #{r\ 4496}#
                           #{mod\ 4497}#))))))
               (#{match*\ 4491}#
                 (lambda (#{e\ 4498}#
                          #{p\ 4499}#
                          #{w\ 4500}#
                          #{r\ 4501}#
                          #{mod\ 4502}#)
                   (if (null? #{p\ 4499}#)
                     (if (null? #{e\ 4498}#) #{r\ 4501}# #f)
                     (if (pair? #{p\ 4499}#)
                       (if (pair? #{e\ 4498}#)
                         (#{match\ 4492}#
                           (car #{e\ 4498}#)
                           (car #{p\ 4499}#)
                           #{w\ 4500}#
                           (#{match\ 4492}#
                             (cdr #{e\ 4498}#)
                             (cdr #{p\ 4499}#)
                             #{w\ 4500}#
                             #{r\ 4501}#
                             #{mod\ 4502}#)
                           #{mod\ 4502}#)
                         #f)
                       (if (eq? #{p\ 4499}# (quote each-any))
                         (let ((#{l\ 4503}#
                                 (#{match-each-any\ 4489}#
                                   #{e\ 4498}#
                                   #{w\ 4500}#
                                   #{mod\ 4502}#)))
                           (if #{l\ 4503}#
                             (cons #{l\ 4503}# #{r\ 4501}#)
                             #f))
                         (let ((#{atom-key\ 4504}# (vector-ref #{p\ 4499}# 0)))
                           (if (memv #{atom-key\ 4504}# (quote (each)))
                             (if (null? #{e\ 4498}#)
                               (#{match-empty\ 4490}#
                                 (vector-ref #{p\ 4499}# 1)
                                 #{r\ 4501}#)
                               (let ((#{l\ 4505}#
                                       (#{match-each\ 4488}#
                                         #{e\ 4498}#
                                         (vector-ref #{p\ 4499}# 1)
                                         #{w\ 4500}#
                                         #{mod\ 4502}#)))
                                 (if #{l\ 4505}#
                                   (letrec ((#{collect\ 4506}#
                                              (lambda (#{l\ 4507}#)
                                                (if (null? (car #{l\ 4507}#))
                                                  #{r\ 4501}#
                                                  (cons (map car #{l\ 4507}#)
                                                        (#{collect\ 4506}#
                                                          (map cdr
                                                               #{l\ 4507}#)))))))
                                     (#{collect\ 4506}# #{l\ 4505}#))
                                   #f)))
                             (if (memv #{atom-key\ 4504}# (quote (free-id)))
                               (if (#{id?\ 3254}# #{e\ 4498}#)
                                 (if (#{free-id=?\ 3277}#
                                       (#{wrap\ 3282}#
                                         #{e\ 4498}#
                                         #{w\ 4500}#
                                         #{mod\ 4502}#)
                                       (vector-ref #{p\ 4499}# 1))
                                   #{r\ 4501}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 4504}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 4499}# 1)
                                       (#{strip\ 3299}#
                                         #{e\ 4498}#
                                         #{w\ 4500}#))
                                   #{r\ 4501}#
                                   #f)
                                 (if (memv #{atom-key\ 4504}# (quote (vector)))
                                   (if (vector? #{e\ 4498}#)
                                     (#{match\ 4492}#
                                       (vector->list #{e\ 4498}#)
                                       (vector-ref #{p\ 4499}# 1)
                                       #{w\ 4500}#
                                       #{r\ 4501}#
                                       #{mod\ 4502}#)
                                     #f)))))))))))
               (#{match-empty\ 4490}#
                 (lambda (#{p\ 4508}# #{r\ 4509}#)
                   (if (null? #{p\ 4508}#)
                     #{r\ 4509}#
                     (if (eq? #{p\ 4508}# (quote any))
                       (cons (quote ()) #{r\ 4509}#)
                       (if (pair? #{p\ 4508}#)
                         (#{match-empty\ 4490}#
                           (car #{p\ 4508}#)
                           (#{match-empty\ 4490}#
                             (cdr #{p\ 4508}#)
                             #{r\ 4509}#))
                         (if (eq? #{p\ 4508}# (quote each-any))
                           (cons (quote ()) #{r\ 4509}#)
                           (let ((#{atom-key\ 4510}#
                                   (vector-ref #{p\ 4508}# 0)))
                             (if (memv #{atom-key\ 4510}# (quote (each)))
                               (#{match-empty\ 4490}#
                                 (vector-ref #{p\ 4508}# 1)
                                 #{r\ 4509}#)
                               (if (memv #{atom-key\ 4510}#
                                         '(free-id atom))
                                 #{r\ 4509}#
                                 (if (memv #{atom-key\ 4510}# (quote (vector)))
                                   (#{match-empty\ 4490}#
                                     (vector-ref #{p\ 4508}# 1)
                                     #{r\ 4509}#)))))))))))
               (#{match-each-any\ 4489}#
                 (lambda (#{e\ 4511}# #{w\ 4512}# #{mod\ 4513}#)
                   (if (pair? #{e\ 4511}#)
                     (let ((#{l\ 4514}#
                             (#{match-each-any\ 4489}#
                               (cdr #{e\ 4511}#)
                               #{w\ 4512}#
                               #{mod\ 4513}#)))
                       (if #{l\ 4514}#
                         (cons (#{wrap\ 3282}#
                                 (car #{e\ 4511}#)
                                 #{w\ 4512}#
                                 #{mod\ 4513}#)
                               #{l\ 4514}#)
                         #f))
                     (if (null? #{e\ 4511}#)
                       '()
                       (if (#{syntax-object?\ 3238}# #{e\ 4511}#)
                         (#{match-each-any\ 4489}#
                           (#{syntax-object-expression\ 3239}# #{e\ 4511}#)
                           (#{join-wraps\ 3273}#
                             #{w\ 4512}#
                             (#{syntax-object-wrap\ 3240}# #{e\ 4511}#))
                           #{mod\ 4513}#)
                         #f)))))
               (#{match-each\ 4488}#
                 (lambda (#{e\ 4515}#
                          #{p\ 4516}#
                          #{w\ 4517}#
                          #{mod\ 4518}#)
                   (if (pair? #{e\ 4515}#)
                     (let ((#{first\ 4519}#
                             (#{match\ 4492}#
                               (car #{e\ 4515}#)
                               #{p\ 4516}#
                               #{w\ 4517}#
                               '()
                               #{mod\ 4518}#)))
                       (if #{first\ 4519}#
                         (let ((#{rest\ 4520}#
                                 (#{match-each\ 4488}#
                                   (cdr #{e\ 4515}#)
                                   #{p\ 4516}#
                                   #{w\ 4517}#
                                   #{mod\ 4518}#)))
                           (if #{rest\ 4520}#
                             (cons #{first\ 4519}# #{rest\ 4520}#)
                             #f))
                         #f))
                     (if (null? #{e\ 4515}#)
                       '()
                       (if (#{syntax-object?\ 3238}# #{e\ 4515}#)
                         (#{match-each\ 4488}#
                           (#{syntax-object-expression\ 3239}# #{e\ 4515}#)
                           #{p\ 4516}#
                           (#{join-wraps\ 3273}#
                             #{w\ 4517}#
                             (#{syntax-object-wrap\ 3240}# #{e\ 4515}#))
                           (#{syntax-object-module\ 3241}# #{e\ 4515}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 4521}# #{p\ 4522}#)
            (if (eq? #{p\ 4522}# (quote any))
              (list #{e\ 4521}#)
              (if (#{syntax-object?\ 3238}# #{e\ 4521}#)
                (#{match*\ 4491}#
                  (#{syntax-object-expression\ 3239}# #{e\ 4521}#)
                  #{p\ 4522}#
                  (#{syntax-object-wrap\ 3240}# #{e\ 4521}#)
                  '()
                  (#{syntax-object-module\ 3241}# #{e\ 4521}#))
                (#{match*\ 4491}#
                  #{e\ 4521}#
                  #{p\ 4522}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4523}#)
      ((lambda (#{tmp\ 4524}#)
         ((lambda (#{tmp\ 4525}#)
            (if #{tmp\ 4525}#
              (apply (lambda (#{_\ 4526}# #{e1\ 4527}# #{e2\ 4528}#)
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
                             (cons #{e1\ 4527}# #{e2\ 4528}#)))
                     #{tmp\ 4525}#)
              ((lambda (#{tmp\ 4530}#)
                 (if #{tmp\ 4530}#
                   (apply (lambda (#{_\ 4531}#
                                   #{out\ 4532}#
                                   #{in\ 4533}#
                                   #{e1\ 4534}#
                                   #{e2\ 4535}#)
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
                                  #{in\ 4533}#
                                  '()
                                  (list #{out\ 4532}#
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
                                              (cons #{e1\ 4534}#
                                                    #{e2\ 4535}#)))))
                          #{tmp\ 4530}#)
                   ((lambda (#{tmp\ 4537}#)
                      (if #{tmp\ 4537}#
                        (apply (lambda (#{_\ 4538}#
                                        #{out\ 4539}#
                                        #{in\ 4540}#
                                        #{e1\ 4541}#
                                        #{e2\ 4542}#)
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
                                             #{in\ 4540}#)
                                       '()
                                       (list #{out\ 4539}#
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
                                                   (cons #{e1\ 4541}#
                                                         #{e2\ 4542}#)))))
                               #{tmp\ 4537}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 4524}#)))
                    ($sc-dispatch
                      #{tmp\ 4524}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 4524}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 4524}#
            '(any () any . each-any))))
       #{x\ 4523}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4546}#)
      ((lambda (#{tmp\ 4547}#)
         ((lambda (#{tmp\ 4548}#)
            (if #{tmp\ 4548}#
              (apply (lambda (#{_\ 4549}#
                              #{k\ 4550}#
                              #{keyword\ 4551}#
                              #{pattern\ 4552}#
                              #{template\ 4553}#)
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
                                         (cons #{k\ 4550}#
                                               (map (lambda (#{tmp\ 4556}#
                                                             #{tmp\ 4555}#)
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
                                                                  #{tmp\ 4555}#)
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
                                                                  #{tmp\ 4556}#)))
                                                    #{template\ 4553}#
                                                    #{pattern\ 4552}#))))))
                     #{tmp\ 4548}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4547}#)))
          ($sc-dispatch
            #{tmp\ 4547}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 4546}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 4557}#)
      ((lambda (#{tmp\ 4558}#)
         ((lambda (#{tmp\ 4559}#)
            (if (if #{tmp\ 4559}#
                  (apply (lambda (#{let*\ 4560}#
                                  #{x\ 4561}#
                                  #{v\ 4562}#
                                  #{e1\ 4563}#
                                  #{e2\ 4564}#)
                           (and-map identifier? #{x\ 4561}#))
                         #{tmp\ 4559}#)
                  #f)
              (apply (lambda (#{let*\ 4566}#
                              #{x\ 4567}#
                              #{v\ 4568}#
                              #{e1\ 4569}#
                              #{e2\ 4570}#)
                       (letrec ((#{f\ 4571}#
                                  (lambda (#{bindings\ 4572}#)
                                    (if (null? #{bindings\ 4572}#)
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
                                                  (cons #{e1\ 4569}#
                                                        #{e2\ 4570}#)))
                                      ((lambda (#{tmp\ 4576}#)
                                         ((lambda (#{tmp\ 4577}#)
                                            (if #{tmp\ 4577}#
                                              (apply (lambda (#{body\ 4578}#
                                                              #{binding\ 4579}#)
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
                                                             (list #{binding\ 4579}#)
                                                             #{body\ 4578}#))
                                                     #{tmp\ 4577}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 4576}#)))
                                          ($sc-dispatch
                                            #{tmp\ 4576}#
                                            '(any any))))
                                       (list (#{f\ 4571}#
                                               (cdr #{bindings\ 4572}#))
                                             (car #{bindings\ 4572}#)))))))
                         (#{f\ 4571}# (map list #{x\ 4567}# #{v\ 4568}#))))
                     #{tmp\ 4559}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4558}#)))
          ($sc-dispatch
            #{tmp\ 4558}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 4557}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 4580}#)
      ((lambda (#{tmp\ 4581}#)
         ((lambda (#{tmp\ 4582}#)
            (if #{tmp\ 4582}#
              (apply (lambda (#{_\ 4583}#
                              #{var\ 4584}#
                              #{init\ 4585}#
                              #{step\ 4586}#
                              #{e0\ 4587}#
                              #{e1\ 4588}#
                              #{c\ 4589}#)
                       ((lambda (#{tmp\ 4590}#)
                          ((lambda (#{tmp\ 4591}#)
                             (if #{tmp\ 4591}#
                               (apply (lambda (#{step\ 4592}#)
                                        ((lambda (#{tmp\ 4593}#)
                                           ((lambda (#{tmp\ 4594}#)
                                              (if #{tmp\ 4594}#
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
                                                                    #{var\ 4584}#
                                                                    #{init\ 4585}#)
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
                                                                           #{e0\ 4587}#)
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
                                                                             #{c\ 4589}#
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
                                                                                         #{step\ 4592}#)))))))
                                                       #{tmp\ 4594}#)
                                                ((lambda (#{tmp\ 4599}#)
                                                   (if #{tmp\ 4599}#
                                                     (apply (lambda (#{e1\ 4600}#
                                                                     #{e2\ 4601}#)
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
                                                                         #{var\ 4584}#
                                                                         #{init\ 4585}#)
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
                                                                          #{e0\ 4587}#
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
                                                                                (cons #{e1\ 4600}#
                                                                                      #{e2\ 4601}#))
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
                                                                                  #{c\ 4589}#
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
                                                                                              #{step\ 4592}#)))))))
                                                            #{tmp\ 4599}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 4593}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 4593}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 4593}#
                                              '())))
                                         #{e1\ 4588}#))
                                      #{tmp\ 4591}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 4590}#)))
                           ($sc-dispatch #{tmp\ 4590}# (quote each-any))))
                        (map (lambda (#{v\ 4608}# #{s\ 4609}#)
                               ((lambda (#{tmp\ 4610}#)
                                  ((lambda (#{tmp\ 4611}#)
                                     (if #{tmp\ 4611}#
                                       (apply (lambda () #{v\ 4608}#)
                                              #{tmp\ 4611}#)
                                       ((lambda (#{tmp\ 4612}#)
                                          (if #{tmp\ 4612}#
                                            (apply (lambda (#{e\ 4613}#)
                                                     #{e\ 4613}#)
                                                   #{tmp\ 4612}#)
                                            ((lambda (#{_\ 4614}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 4580}#
                                                 #{s\ 4609}#))
                                             #{tmp\ 4610}#)))
                                        ($sc-dispatch
                                          #{tmp\ 4610}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 4610}# (quote ()))))
                                #{s\ 4609}#))
                             #{var\ 4584}#
                             #{step\ 4586}#)))
                     #{tmp\ 4582}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4581}#)))
          ($sc-dispatch
            #{tmp\ 4581}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 4580}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 4617}#
               (lambda (#{x\ 4621}# #{y\ 4622}#)
                 ((lambda (#{tmp\ 4623}#)
                    ((lambda (#{tmp\ 4624}#)
                       (if #{tmp\ 4624}#
                         (apply (lambda (#{x\ 4625}# #{y\ 4626}#)
                                  ((lambda (#{tmp\ 4627}#)
                                     ((lambda (#{tmp\ 4628}#)
                                        (if #{tmp\ 4628}#
                                          (apply (lambda (#{dy\ 4629}#)
                                                   ((lambda (#{tmp\ 4630}#)
                                                      ((lambda (#{tmp\ 4631}#)
                                                         (if #{tmp\ 4631}#
                                                           (apply (lambda (#{dx\ 4632}#)
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
                                                                          (cons #{dx\ 4632}#
                                                                                #{dy\ 4629}#)))
                                                                  #{tmp\ 4631}#)
                                                           ((lambda (#{_\ 4633}#)
                                                              (if (null? #{dy\ 4629}#)
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
                                                                      #{x\ 4625}#)
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
                                                                      #{x\ 4625}#
                                                                      #{y\ 4626}#)))
                                                            #{tmp\ 4630}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 4630}#
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
                                                    #{x\ 4625}#))
                                                 #{tmp\ 4628}#)
                                          ((lambda (#{tmp\ 4634}#)
                                             (if #{tmp\ 4634}#
                                               (apply (lambda (#{stuff\ 4635}#)
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
                                                              (cons #{x\ 4625}#
                                                                    #{stuff\ 4635}#)))
                                                      #{tmp\ 4634}#)
                                               ((lambda (#{else\ 4636}#)
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
                                                        #{x\ 4625}#
                                                        #{y\ 4626}#))
                                                #{tmp\ 4627}#)))
                                           ($sc-dispatch
                                             #{tmp\ 4627}#
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
                                        #{tmp\ 4627}#
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
                                   #{y\ 4626}#))
                                #{tmp\ 4624}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 4623}#)))
                     ($sc-dispatch #{tmp\ 4623}# (quote (any any)))))
                  (list #{x\ 4621}# #{y\ 4622}#))))
             (#{quasiappend\ 4618}#
               (lambda (#{x\ 4637}# #{y\ 4638}#)
                 ((lambda (#{tmp\ 4639}#)
                    ((lambda (#{tmp\ 4640}#)
                       (if #{tmp\ 4640}#
                         (apply (lambda (#{x\ 4641}# #{y\ 4642}#)
                                  ((lambda (#{tmp\ 4643}#)
                                     ((lambda (#{tmp\ 4644}#)
                                        (if #{tmp\ 4644}#
                                          (apply (lambda () #{x\ 4641}#)
                                                 #{tmp\ 4644}#)
                                          ((lambda (#{_\ 4645}#)
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
                                                   #{x\ 4641}#
                                                   #{y\ 4642}#))
                                           #{tmp\ 4643}#)))
                                      ($sc-dispatch
                                        #{tmp\ 4643}#
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
                                   #{y\ 4642}#))
                                #{tmp\ 4640}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 4639}#)))
                     ($sc-dispatch #{tmp\ 4639}# (quote (any any)))))
                  (list #{x\ 4637}# #{y\ 4638}#))))
             (#{quasivector\ 4619}#
               (lambda (#{x\ 4646}#)
                 ((lambda (#{tmp\ 4647}#)
                    ((lambda (#{x\ 4648}#)
                       ((lambda (#{tmp\ 4649}#)
                          ((lambda (#{tmp\ 4650}#)
                             (if #{tmp\ 4650}#
                               (apply (lambda (#{x\ 4651}#)
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
                                              (list->vector #{x\ 4651}#)))
                                      #{tmp\ 4650}#)
                               ((lambda (#{tmp\ 4653}#)
                                  (if #{tmp\ 4653}#
                                    (apply (lambda (#{x\ 4654}#)
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
                                                   #{x\ 4654}#))
                                           #{tmp\ 4653}#)
                                    ((lambda (#{_\ 4656}#)
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
                                             #{x\ 4648}#))
                                     #{tmp\ 4649}#)))
                                ($sc-dispatch
                                  #{tmp\ 4649}#
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
                             #{tmp\ 4649}#
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
                        #{x\ 4648}#))
                     #{tmp\ 4647}#))
                  #{x\ 4646}#)))
             (#{quasi\ 4620}#
               (lambda (#{p\ 4657}# #{lev\ 4658}#)
                 ((lambda (#{tmp\ 4659}#)
                    ((lambda (#{tmp\ 4660}#)
                       (if #{tmp\ 4660}#
                         (apply (lambda (#{p\ 4661}#)
                                  (if (= #{lev\ 4658}# 0)
                                    #{p\ 4661}#
                                    (#{quasicons\ 4617}#
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
                                      (#{quasi\ 4620}#
                                        (list #{p\ 4661}#)
                                        (- #{lev\ 4658}# 1)))))
                                #{tmp\ 4660}#)
                         ((lambda (#{tmp\ 4662}#)
                            (if (if #{tmp\ 4662}#
                                  (apply (lambda (#{args\ 4663}#)
                                           (= #{lev\ 4658}# 0))
                                         #{tmp\ 4662}#)
                                  #f)
                              (apply (lambda (#{args\ 4664}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 4657}#
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
                                               #{args\ 4664}#)))
                                     #{tmp\ 4662}#)
                              ((lambda (#{tmp\ 4665}#)
                                 (if #{tmp\ 4665}#
                                   (apply (lambda (#{p\ 4666}# #{q\ 4667}#)
                                            (if (= #{lev\ 4658}# 0)
                                              (#{quasiappend\ 4618}#
                                                #{p\ 4666}#
                                                (#{quasi\ 4620}#
                                                  #{q\ 4667}#
                                                  #{lev\ 4658}#))
                                              (#{quasicons\ 4617}#
                                                (#{quasicons\ 4617}#
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
                                                  (#{quasi\ 4620}#
                                                    (list #{p\ 4666}#)
                                                    (- #{lev\ 4658}# 1)))
                                                (#{quasi\ 4620}#
                                                  #{q\ 4667}#
                                                  #{lev\ 4658}#))))
                                          #{tmp\ 4665}#)
                                   ((lambda (#{tmp\ 4668}#)
                                      (if (if #{tmp\ 4668}#
                                            (apply (lambda (#{args\ 4669}#
                                                            #{q\ 4670}#)
                                                     (= #{lev\ 4658}# 0))
                                                   #{tmp\ 4668}#)
                                            #f)
                                        (apply (lambda (#{args\ 4671}#
                                                        #{q\ 4672}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 4657}#
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
                                                         #{args\ 4671}#)))
                                               #{tmp\ 4668}#)
                                        ((lambda (#{tmp\ 4673}#)
                                           (if #{tmp\ 4673}#
                                             (apply (lambda (#{p\ 4674}#)
                                                      (#{quasicons\ 4617}#
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
                                                        (#{quasi\ 4620}#
                                                          (list #{p\ 4674}#)
                                                          (+ #{lev\ 4658}#
                                                             1))))
                                                    #{tmp\ 4673}#)
                                             ((lambda (#{tmp\ 4675}#)
                                                (if #{tmp\ 4675}#
                                                  (apply (lambda (#{p\ 4676}#
                                                                  #{q\ 4677}#)
                                                           (#{quasicons\ 4617}#
                                                             (#{quasi\ 4620}#
                                                               #{p\ 4676}#
                                                               #{lev\ 4658}#)
                                                             (#{quasi\ 4620}#
                                                               #{q\ 4677}#
                                                               #{lev\ 4658}#)))
                                                         #{tmp\ 4675}#)
                                                  ((lambda (#{tmp\ 4678}#)
                                                     (if #{tmp\ 4678}#
                                                       (apply (lambda (#{x\ 4679}#)
                                                                (#{quasivector\ 4619}#
                                                                  (#{quasi\ 4620}#
                                                                    #{x\ 4679}#
                                                                    #{lev\ 4658}#)))
                                                              #{tmp\ 4678}#)
                                                       ((lambda (#{p\ 4681}#)
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
                                                                #{p\ 4681}#))
                                                        #{tmp\ 4659}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 4659}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 4659}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 4659}#
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
                                      #{tmp\ 4659}#
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
                                 #{tmp\ 4659}#
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
                            #{tmp\ 4659}#
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
                       #{tmp\ 4659}#
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
                  #{p\ 4657}#))))
      (lambda (#{x\ 4682}#)
        ((lambda (#{tmp\ 4683}#)
           ((lambda (#{tmp\ 4684}#)
              (if #{tmp\ 4684}#
                (apply (lambda (#{_\ 4685}# #{e\ 4686}#)
                         (#{quasi\ 4620}# #{e\ 4686}# 0))
                       #{tmp\ 4684}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4683}#)))
            ($sc-dispatch #{tmp\ 4683}# (quote (any any)))))
         #{x\ 4682}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4687}#)
      (letrec ((#{read-file\ 4688}#
                 (lambda (#{fn\ 4689}# #{k\ 4690}#)
                   (let ((#{p\ 4691}# (open-input-file #{fn\ 4689}#)))
                     (letrec ((#{f\ 4692}#
                                (lambda (#{x\ 4693}#)
                                  (if (eof-object? #{x\ 4693}#)
                                    (begin
                                      (close-input-port #{p\ 4691}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 4690}#
                                            #{x\ 4693}#)
                                          (#{f\ 4692}# (read #{p\ 4691}#)))))))
                       (#{f\ 4692}# (read #{p\ 4691}#)))))))
        ((lambda (#{tmp\ 4694}#)
           ((lambda (#{tmp\ 4695}#)
              (if #{tmp\ 4695}#
                (apply (lambda (#{k\ 4696}# #{filename\ 4697}#)
                         (let ((#{fn\ 4698}#
                                 (syntax->datum #{filename\ 4697}#)))
                           ((lambda (#{tmp\ 4699}#)
                              ((lambda (#{tmp\ 4700}#)
                                 (if #{tmp\ 4700}#
                                   (apply (lambda (#{exp\ 4701}#)
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
                                                  #{exp\ 4701}#))
                                          #{tmp\ 4700}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 4699}#)))
                               ($sc-dispatch #{tmp\ 4699}# (quote each-any))))
                            (#{read-file\ 4688}# #{fn\ 4698}# #{k\ 4696}#))))
                       #{tmp\ 4695}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4694}#)))
            ($sc-dispatch #{tmp\ 4694}# (quote (any any)))))
         #{x\ 4687}#)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4703}#)
      ((lambda (#{tmp\ 4704}#)
         ((lambda (#{tmp\ 4705}#)
            (if #{tmp\ 4705}#
              (apply (lambda (#{_\ 4706}# #{e\ 4707}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 4703}#))
                     #{tmp\ 4705}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4704}#)))
          ($sc-dispatch #{tmp\ 4704}# (quote (any any)))))
       #{x\ 4703}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4708}#)
      ((lambda (#{tmp\ 4709}#)
         ((lambda (#{tmp\ 4710}#)
            (if #{tmp\ 4710}#
              (apply (lambda (#{_\ 4711}# #{e\ 4712}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 4708}#))
                     #{tmp\ 4710}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4709}#)))
          ($sc-dispatch #{tmp\ 4709}# (quote (any any)))))
       #{x\ 4708}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 4713}#)
      ((lambda (#{tmp\ 4714}#)
         ((lambda (#{tmp\ 4715}#)
            (if #{tmp\ 4715}#
              (apply (lambda (#{_\ 4716}#
                              #{e\ 4717}#
                              #{m1\ 4718}#
                              #{m2\ 4719}#)
                       ((lambda (#{tmp\ 4720}#)
                          ((lambda (#{body\ 4721}#)
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
                                               #{e\ 4717}#))
                                   #{body\ 4721}#))
                           #{tmp\ 4720}#))
                        (letrec ((#{f\ 4722}#
                                   (lambda (#{clause\ 4723}# #{clauses\ 4724}#)
                                     (if (null? #{clauses\ 4724}#)
                                       ((lambda (#{tmp\ 4726}#)
                                          ((lambda (#{tmp\ 4727}#)
                                             (if #{tmp\ 4727}#
                                               (apply (lambda (#{e1\ 4728}#
                                                               #{e2\ 4729}#)
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
                                                              (cons #{e1\ 4728}#
                                                                    #{e2\ 4729}#)))
                                                      #{tmp\ 4727}#)
                                               ((lambda (#{tmp\ 4731}#)
                                                  (if #{tmp\ 4731}#
                                                    (apply (lambda (#{k\ 4732}#
                                                                    #{e1\ 4733}#
                                                                    #{e2\ 4734}#)
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
                                                                               #{k\ 4732}#))
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
                                                                         (cons #{e1\ 4733}#
                                                                               #{e2\ 4734}#))))
                                                           #{tmp\ 4731}#)
                                                    ((lambda (#{_\ 4737}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 4713}#
                                                         #{clause\ 4723}#))
                                                     #{tmp\ 4726}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 4726}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 4726}#
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
                                        #{clause\ 4723}#)
                                       ((lambda (#{tmp\ 4738}#)
                                          ((lambda (#{rest\ 4739}#)
                                             ((lambda (#{tmp\ 4740}#)
                                                ((lambda (#{tmp\ 4741}#)
                                                   (if #{tmp\ 4741}#
                                                     (apply (lambda (#{k\ 4742}#
                                                                     #{e1\ 4743}#
                                                                     #{e2\ 4744}#)
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
                                                                                #{k\ 4742}#))
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
                                                                          (cons #{e1\ 4743}#
                                                                                #{e2\ 4744}#))
                                                                    #{rest\ 4739}#))
                                                            #{tmp\ 4741}#)
                                                     ((lambda (#{_\ 4747}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 4713}#
                                                          #{clause\ 4723}#))
                                                      #{tmp\ 4740}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 4740}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 4723}#))
                                           #{tmp\ 4738}#))
                                        (#{f\ 4722}#
                                          (car #{clauses\ 4724}#)
                                          (cdr #{clauses\ 4724}#)))))))
                          (#{f\ 4722}# #{m1\ 4718}# #{m2\ 4719}#))))
                     #{tmp\ 4715}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4714}#)))
          ($sc-dispatch
            #{tmp\ 4714}#
            '(any any any . each-any))))
       #{x\ 4713}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4748}#)
      ((lambda (#{tmp\ 4749}#)
         ((lambda (#{tmp\ 4750}#)
            (if #{tmp\ 4750}#
              (apply (lambda (#{_\ 4751}# #{e\ 4752}#)
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
                                               #{e\ 4752}#))
                                   (list (cons #{_\ 4751}#
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
                                               (cons #{e\ 4752}#
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
                     #{tmp\ 4750}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4749}#)))
          ($sc-dispatch #{tmp\ 4749}# (quote (any any)))))
       #{x\ 4748}#))))

