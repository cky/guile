(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((#{and-map*\ 1199}#
           (lambda (#{f\ 1239}# #{first\ 1238}# . #{rest\ 1237}#)
             (let ((#{t\ 1240}# (null? #{first\ 1238}#)))
               (if #{t\ 1240}#
                 #{t\ 1240}#
                 (if (null? #{rest\ 1237}#)
                   (letrec ((#{andmap\ 1241}#
                              (lambda (#{first\ 1242}#)
                                (let ((#{x\ 1243}# (car #{first\ 1242}#))
                                      (#{first\ 1244}# (cdr #{first\ 1242}#)))
                                  (if (null? #{first\ 1244}#)
                                    (#{f\ 1239}# #{x\ 1243}#)
                                    (if (#{f\ 1239}# #{x\ 1243}#)
                                      (#{andmap\ 1241}# #{first\ 1244}#)
                                      #f))))))
                     (#{andmap\ 1241}# #{first\ 1238}#))
                   (letrec ((#{andmap\ 1245}#
                              (lambda (#{first\ 1246}# #{rest\ 1247}#)
                                (let ((#{x\ 1248}# (car #{first\ 1246}#))
                                      (#{xr\ 1249}# (map car #{rest\ 1247}#))
                                      (#{first\ 1250}# (cdr #{first\ 1246}#))
                                      (#{rest\ 1251}#
                                        (map cdr #{rest\ 1247}#)))
                                  (if (null? #{first\ 1250}#)
                                    (apply #{f\ 1239}#
                                           (cons #{x\ 1248}# #{xr\ 1249}#))
                                    (if (apply #{f\ 1239}#
                                               (cons #{x\ 1248}# #{xr\ 1249}#))
                                      (#{andmap\ 1245}#
                                        #{first\ 1250}#
                                        #{rest\ 1251}#)
                                      #f))))))
                     (#{andmap\ 1245}# #{first\ 1238}# #{rest\ 1237}#))))))))
  (letrec ((#{lambda-var-list\ 1345}#
             (lambda (#{vars\ 1469}#)
               (letrec ((#{lvl\ 1470}#
                          (lambda (#{vars\ 1471}# #{ls\ 1472}# #{w\ 1473}#)
                            (if (pair? #{vars\ 1471}#)
                              (#{lvl\ 1470}#
                                (cdr #{vars\ 1471}#)
                                (cons (#{wrap\ 1325}#
                                        (car #{vars\ 1471}#)
                                        #{w\ 1473}#
                                        #f)
                                      #{ls\ 1472}#)
                                #{w\ 1473}#)
                              (if (#{id?\ 1297}# #{vars\ 1471}#)
                                (cons (#{wrap\ 1325}#
                                        #{vars\ 1471}#
                                        #{w\ 1473}#
                                        #f)
                                      #{ls\ 1472}#)
                                (if (null? #{vars\ 1471}#)
                                  #{ls\ 1472}#
                                  (if (#{syntax-object?\ 1281}# #{vars\ 1471}#)
                                    (#{lvl\ 1470}#
                                      (#{syntax-object-expression\ 1282}#
                                        #{vars\ 1471}#)
                                      #{ls\ 1472}#
                                      (#{join-wraps\ 1316}#
                                        #{w\ 1473}#
                                        (#{syntax-object-wrap\ 1283}#
                                          #{vars\ 1471}#)))
                                    (cons #{vars\ 1471}# #{ls\ 1472}#))))))))
                 (#{lvl\ 1470}#
                   #{vars\ 1469}#
                   '()
                   '(())))))
           (#{gen-var\ 1344}#
             (lambda (#{id\ 1474}#)
               (let ((#{id\ 1475}#
                       (if (#{syntax-object?\ 1281}# #{id\ 1474}#)
                         (#{syntax-object-expression\ 1282}# #{id\ 1474}#)
                         #{id\ 1474}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 1475}#) " ")))))
           (#{strip\ 1343}#
             (lambda (#{x\ 1476}# #{w\ 1477}#)
               (if (memq 'top
                         (#{wrap-marks\ 1300}# #{w\ 1477}#))
                 #{x\ 1476}#
                 (letrec ((#{f\ 1478}#
                            (lambda (#{x\ 1479}#)
                              (if (#{syntax-object?\ 1281}# #{x\ 1479}#)
                                (#{strip\ 1343}#
                                  (#{syntax-object-expression\ 1282}#
                                    #{x\ 1479}#)
                                  (#{syntax-object-wrap\ 1283}# #{x\ 1479}#))
                                (if (pair? #{x\ 1479}#)
                                  (let ((#{a\ 1480}#
                                          (#{f\ 1478}# (car #{x\ 1479}#)))
                                        (#{d\ 1481}#
                                          (#{f\ 1478}# (cdr #{x\ 1479}#))))
                                    (if (if (eq? #{a\ 1480}# (car #{x\ 1479}#))
                                          (eq? #{d\ 1481}# (cdr #{x\ 1479}#))
                                          #f)
                                      #{x\ 1479}#
                                      (cons #{a\ 1480}# #{d\ 1481}#)))
                                  (if (vector? #{x\ 1479}#)
                                    (let ((#{old\ 1482}#
                                            (vector->list #{x\ 1479}#)))
                                      (let ((#{new\ 1483}#
                                              (map #{f\ 1478}# #{old\ 1482}#)))
                                        (if (#{and-map*\ 1199}#
                                              eq?
                                              #{old\ 1482}#
                                              #{new\ 1483}#)
                                          #{x\ 1479}#
                                          (list->vector #{new\ 1483}#))))
                                    #{x\ 1479}#))))))
                   (#{f\ 1478}# #{x\ 1476}#)))))
           (#{ellipsis?\ 1342}#
             (lambda (#{x\ 1484}#)
               (if (#{nonsymbol-id?\ 1296}# #{x\ 1484}#)
                 (#{free-id=?\ 1320}#
                   #{x\ 1484}#
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
           (#{chi-void\ 1341}#
             (lambda () (#{build-void\ 1263}# #f)))
           (#{eval-local-transformer\ 1340}#
             (lambda (#{expanded\ 1485}# #{mod\ 1486}#)
               (let ((#{p\ 1487}#
                       (#{local-eval-hook\ 1259}#
                         #{expanded\ 1485}#
                         #{mod\ 1486}#)))
                 (if (procedure? #{p\ 1487}#)
                   #{p\ 1487}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 1487}#)))))
           (#{chi-local-syntax\ 1339}#
             (lambda (#{rec?\ 1488}#
                      #{e\ 1489}#
                      #{r\ 1490}#
                      #{w\ 1491}#
                      #{s\ 1492}#
                      #{mod\ 1493}#
                      #{k\ 1494}#)
               ((lambda (#{tmp\ 1495}#)
                  ((lambda (#{tmp\ 1496}#)
                     (if #{tmp\ 1496}#
                       (apply (lambda (#{_\ 1497}#
                                       #{id\ 1498}#
                                       #{val\ 1499}#
                                       #{e1\ 1500}#
                                       #{e2\ 1501}#)
                                (let ((#{ids\ 1502}# #{id\ 1498}#))
                                  (if (not (#{valid-bound-ids?\ 1322}#
                                             #{ids\ 1502}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 1489}#)
                                    (let ((#{labels\ 1504}#
                                            (#{gen-labels\ 1303}#
                                              #{ids\ 1502}#)))
                                      (let ((#{new-w\ 1505}#
                                              (#{make-binding-wrap\ 1314}#
                                                #{ids\ 1502}#
                                                #{labels\ 1504}#
                                                #{w\ 1491}#)))
                                        (#{k\ 1494}#
                                          (cons #{e1\ 1500}# #{e2\ 1501}#)
                                          (#{extend-env\ 1291}#
                                            #{labels\ 1504}#
                                            (let ((#{w\ 1507}#
                                                    (if #{rec?\ 1488}#
                                                      #{new-w\ 1505}#
                                                      #{w\ 1491}#))
                                                  (#{trans-r\ 1508}#
                                                    (#{macros-only-env\ 1293}#
                                                      #{r\ 1490}#)))
                                              (map (lambda (#{x\ 1509}#)
                                                     (cons 'macro
                                                           (#{eval-local-transformer\ 1340}#
                                                             (#{chi\ 1333}#
                                                               #{x\ 1509}#
                                                               #{trans-r\ 1508}#
                                                               #{w\ 1507}#
                                                               #{mod\ 1493}#)
                                                             #{mod\ 1493}#)))
                                                   #{val\ 1499}#))
                                            #{r\ 1490}#)
                                          #{new-w\ 1505}#
                                          #{s\ 1492}#
                                          #{mod\ 1493}#))))))
                              #{tmp\ 1496}#)
                       ((lambda (#{_\ 1511}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 1326}#
                              #{e\ 1489}#
                              #{w\ 1491}#
                              #{s\ 1492}#
                              #{mod\ 1493}#)))
                        #{tmp\ 1495}#)))
                   ($sc-dispatch
                     #{tmp\ 1495}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 1489}#)))
           (#{chi-lambda-clause\ 1338}#
             (lambda (#{e\ 1512}#
                      #{docstring\ 1513}#
                      #{c\ 1514}#
                      #{r\ 1515}#
                      #{w\ 1516}#
                      #{mod\ 1517}#
                      #{k\ 1518}#)
               ((lambda (#{tmp\ 1519}#)
                  ((lambda (#{tmp\ 1520}#)
                     (if (if #{tmp\ 1520}#
                           (apply (lambda (#{args\ 1521}#
                                           #{doc\ 1522}#
                                           #{e1\ 1523}#
                                           #{e2\ 1524}#)
                                    (if (string? (syntax->datum #{doc\ 1522}#))
                                      (not #{docstring\ 1513}#)
                                      #f))
                                  #{tmp\ 1520}#)
                           #f)
                       (apply (lambda (#{args\ 1525}#
                                       #{doc\ 1526}#
                                       #{e1\ 1527}#
                                       #{e2\ 1528}#)
                                (#{chi-lambda-clause\ 1338}#
                                  #{e\ 1512}#
                                  #{doc\ 1526}#
                                  (cons #{args\ 1525}#
                                        (cons #{e1\ 1527}# #{e2\ 1528}#))
                                  #{r\ 1515}#
                                  #{w\ 1516}#
                                  #{mod\ 1517}#
                                  #{k\ 1518}#))
                              #{tmp\ 1520}#)
                       ((lambda (#{tmp\ 1530}#)
                          (if #{tmp\ 1530}#
                            (apply (lambda (#{id\ 1531}#
                                            #{e1\ 1532}#
                                            #{e2\ 1533}#)
                                     (let ((#{ids\ 1534}# #{id\ 1531}#))
                                       (if (not (#{valid-bound-ids?\ 1322}#
                                                  #{ids\ 1534}#))
                                         (syntax-violation
                                           'lambda
                                           "invalid parameter list"
                                           #{e\ 1512}#)
                                         (let ((#{labels\ 1536}#
                                                 (#{gen-labels\ 1303}#
                                                   #{ids\ 1534}#))
                                               (#{new-vars\ 1537}#
                                                 (map #{gen-var\ 1344}#
                                                      #{ids\ 1534}#)))
                                           (#{k\ 1518}#
                                             (map syntax->datum #{ids\ 1534}#)
                                             #{new-vars\ 1537}#
                                             (if #{docstring\ 1513}#
                                               (syntax->datum
                                                 #{docstring\ 1513}#)
                                               #f)
                                             (#{chi-body\ 1337}#
                                               (cons #{e1\ 1532}# #{e2\ 1533}#)
                                               #{e\ 1512}#
                                               (#{extend-var-env\ 1292}#
                                                 #{labels\ 1536}#
                                                 #{new-vars\ 1537}#
                                                 #{r\ 1515}#)
                                               (#{make-binding-wrap\ 1314}#
                                                 #{ids\ 1534}#
                                                 #{labels\ 1536}#
                                                 #{w\ 1516}#)
                                               #{mod\ 1517}#))))))
                                   #{tmp\ 1530}#)
                            ((lambda (#{tmp\ 1539}#)
                               (if #{tmp\ 1539}#
                                 (apply (lambda (#{ids\ 1540}#
                                                 #{e1\ 1541}#
                                                 #{e2\ 1542}#)
                                          (let ((#{old-ids\ 1543}#
                                                  (#{lambda-var-list\ 1345}#
                                                    #{ids\ 1540}#)))
                                            (if (not (#{valid-bound-ids?\ 1322}#
                                                       #{old-ids\ 1543}#))
                                              (syntax-violation
                                                'lambda
                                                "invalid parameter list"
                                                #{e\ 1512}#)
                                              (let ((#{labels\ 1544}#
                                                      (#{gen-labels\ 1303}#
                                                        #{old-ids\ 1543}#))
                                                    (#{new-vars\ 1545}#
                                                      (map #{gen-var\ 1344}#
                                                           #{old-ids\ 1543}#)))
                                                (#{k\ 1518}#
                                                  (letrec ((#{f\ 1546}#
                                                             (lambda (#{ls1\ 1547}#
                                                                      #{ls2\ 1548}#)
                                                               (if (null? #{ls1\ 1547}#)
                                                                 (syntax->datum
                                                                   #{ls2\ 1548}#)
                                                                 (#{f\ 1546}#
                                                                   (cdr #{ls1\ 1547}#)
                                                                   (cons (syntax->datum
                                                                           (car #{ls1\ 1547}#))
                                                                         #{ls2\ 1548}#))))))
                                                    (#{f\ 1546}#
                                                      (cdr #{old-ids\ 1543}#)
                                                      (car #{old-ids\ 1543}#)))
                                                  (letrec ((#{f\ 1549}#
                                                             (lambda (#{ls1\ 1550}#
                                                                      #{ls2\ 1551}#)
                                                               (if (null? #{ls1\ 1550}#)
                                                                 #{ls2\ 1551}#
                                                                 (#{f\ 1549}#
                                                                   (cdr #{ls1\ 1550}#)
                                                                   (cons (car #{ls1\ 1550}#)
                                                                         #{ls2\ 1551}#))))))
                                                    (#{f\ 1549}#
                                                      (cdr #{new-vars\ 1545}#)
                                                      (car #{new-vars\ 1545}#)))
                                                  (if #{docstring\ 1513}#
                                                    (syntax->datum
                                                      #{docstring\ 1513}#)
                                                    #f)
                                                  (#{chi-body\ 1337}#
                                                    (cons #{e1\ 1541}#
                                                          #{e2\ 1542}#)
                                                    #{e\ 1512}#
                                                    (#{extend-var-env\ 1292}#
                                                      #{labels\ 1544}#
                                                      #{new-vars\ 1545}#
                                                      #{r\ 1515}#)
                                                    (#{make-binding-wrap\ 1314}#
                                                      #{old-ids\ 1543}#
                                                      #{labels\ 1544}#
                                                      #{w\ 1516}#)
                                                    #{mod\ 1517}#))))))
                                        #{tmp\ 1539}#)
                                 ((lambda (#{_\ 1553}#)
                                    (syntax-violation
                                      'lambda
                                      "bad lambda"
                                      #{e\ 1512}#))
                                  #{tmp\ 1519}#)))
                             ($sc-dispatch
                               #{tmp\ 1519}#
                               '(any any . each-any)))))
                        ($sc-dispatch
                          #{tmp\ 1519}#
                          '(each-any any . each-any)))))
                   ($sc-dispatch
                     #{tmp\ 1519}#
                     '(any any any . each-any))))
                #{c\ 1514}#)))
           (#{chi-body\ 1337}#
             (lambda (#{body\ 1554}#
                      #{outer-form\ 1555}#
                      #{r\ 1556}#
                      #{w\ 1557}#
                      #{mod\ 1558}#)
               (let ((#{r\ 1559}#
                       (cons '("placeholder" placeholder)
                             #{r\ 1556}#)))
                 (let ((#{ribcage\ 1560}#
                         (#{make-ribcage\ 1304}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 1561}#
                           (#{make-wrap\ 1299}#
                             (#{wrap-marks\ 1300}# #{w\ 1557}#)
                             (cons #{ribcage\ 1560}#
                                   (#{wrap-subst\ 1301}# #{w\ 1557}#)))))
                     (letrec ((#{parse\ 1562}#
                                (lambda (#{body\ 1563}#
                                         #{ids\ 1564}#
                                         #{labels\ 1565}#
                                         #{var-ids\ 1566}#
                                         #{vars\ 1567}#
                                         #{vals\ 1568}#
                                         #{bindings\ 1569}#)
                                  (if (null? #{body\ 1563}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 1555}#)
                                    (let ((#{e\ 1571}# (cdar #{body\ 1563}#))
                                          (#{er\ 1572}# (caar #{body\ 1563}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 1331}#
                                            #{e\ 1571}#
                                            #{er\ 1572}#
                                            '(())
                                            (#{source-annotation\ 1288}#
                                              #{er\ 1572}#)
                                            #{ribcage\ 1560}#
                                            #{mod\ 1558}#
                                            #f))
                                        (lambda (#{type\ 1573}#
                                                 #{value\ 1574}#
                                                 #{e\ 1575}#
                                                 #{w\ 1576}#
                                                 #{s\ 1577}#
                                                 #{mod\ 1578}#)
                                          (if (memv #{type\ 1573}#
                                                    '(define-form))
                                            (let ((#{id\ 1579}#
                                                    (#{wrap\ 1325}#
                                                      #{value\ 1574}#
                                                      #{w\ 1576}#
                                                      #{mod\ 1578}#))
                                                  (#{label\ 1580}#
                                                    (#{gen-label\ 1302}#)))
                                              (let ((#{var\ 1581}#
                                                      (#{gen-var\ 1344}#
                                                        #{id\ 1579}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 1313}#
                                                    #{ribcage\ 1560}#
                                                    #{id\ 1579}#
                                                    #{label\ 1580}#)
                                                  (#{parse\ 1562}#
                                                    (cdr #{body\ 1563}#)
                                                    (cons #{id\ 1579}#
                                                          #{ids\ 1564}#)
                                                    (cons #{label\ 1580}#
                                                          #{labels\ 1565}#)
                                                    (cons #{id\ 1579}#
                                                          #{var-ids\ 1566}#)
                                                    (cons #{var\ 1581}#
                                                          #{vars\ 1567}#)
                                                    (cons (cons #{er\ 1572}#
                                                                (#{wrap\ 1325}#
                                                                  #{e\ 1575}#
                                                                  #{w\ 1576}#
                                                                  #{mod\ 1578}#))
                                                          #{vals\ 1568}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 1581}#)
                                                          #{bindings\ 1569}#)))))
                                            (if (memv #{type\ 1573}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 1582}#
                                                      (#{wrap\ 1325}#
                                                        #{value\ 1574}#
                                                        #{w\ 1576}#
                                                        #{mod\ 1578}#))
                                                    (#{label\ 1583}#
                                                      (#{gen-label\ 1302}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 1313}#
                                                    #{ribcage\ 1560}#
                                                    #{id\ 1582}#
                                                    #{label\ 1583}#)
                                                  (#{parse\ 1562}#
                                                    (cdr #{body\ 1563}#)
                                                    (cons #{id\ 1582}#
                                                          #{ids\ 1564}#)
                                                    (cons #{label\ 1583}#
                                                          #{labels\ 1565}#)
                                                    #{var-ids\ 1566}#
                                                    #{vars\ 1567}#
                                                    #{vals\ 1568}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 1572}#
                                                                      (#{wrap\ 1325}#
                                                                        #{e\ 1575}#
                                                                        #{w\ 1576}#
                                                                        #{mod\ 1578}#)))
                                                          #{bindings\ 1569}#))))
                                              (if (memv #{type\ 1573}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 1584}#)
                                                   ((lambda (#{tmp\ 1585}#)
                                                      (if #{tmp\ 1585}#
                                                        (apply (lambda (#{_\ 1586}#
                                                                        #{e1\ 1587}#)
                                                                 (#{parse\ 1562}#
                                                                   (letrec ((#{f\ 1588}#
                                                                              (lambda (#{forms\ 1589}#)
                                                                                (if (null? #{forms\ 1589}#)
                                                                                  (cdr #{body\ 1563}#)
                                                                                  (cons (cons #{er\ 1572}#
                                                                                              (#{wrap\ 1325}#
                                                                                                (car #{forms\ 1589}#)
                                                                                                #{w\ 1576}#
                                                                                                #{mod\ 1578}#))
                                                                                        (#{f\ 1588}#
                                                                                          (cdr #{forms\ 1589}#)))))))
                                                                     (#{f\ 1588}#
                                                                       #{e1\ 1587}#))
                                                                   #{ids\ 1564}#
                                                                   #{labels\ 1565}#
                                                                   #{var-ids\ 1566}#
                                                                   #{vars\ 1567}#
                                                                   #{vals\ 1568}#
                                                                   #{bindings\ 1569}#))
                                                               #{tmp\ 1585}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 1584}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 1584}#
                                                      '(any . each-any))))
                                                 #{e\ 1575}#)
                                                (if (memv #{type\ 1573}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 1339}#
                                                    #{value\ 1574}#
                                                    #{e\ 1575}#
                                                    #{er\ 1572}#
                                                    #{w\ 1576}#
                                                    #{s\ 1577}#
                                                    #{mod\ 1578}#
                                                    (lambda (#{forms\ 1591}#
                                                             #{er\ 1592}#
                                                             #{w\ 1593}#
                                                             #{s\ 1594}#
                                                             #{mod\ 1595}#)
                                                      (#{parse\ 1562}#
                                                        (letrec ((#{f\ 1596}#
                                                                   (lambda (#{forms\ 1597}#)
                                                                     (if (null? #{forms\ 1597}#)
                                                                       (cdr #{body\ 1563}#)
                                                                       (cons (cons #{er\ 1592}#
                                                                                   (#{wrap\ 1325}#
                                                                                     (car #{forms\ 1597}#)
                                                                                     #{w\ 1593}#
                                                                                     #{mod\ 1595}#))
                                                                             (#{f\ 1596}#
                                                                               (cdr #{forms\ 1597}#)))))))
                                                          (#{f\ 1596}#
                                                            #{forms\ 1591}#))
                                                        #{ids\ 1564}#
                                                        #{labels\ 1565}#
                                                        #{var-ids\ 1566}#
                                                        #{vars\ 1567}#
                                                        #{vals\ 1568}#
                                                        #{bindings\ 1569}#)))
                                                  (if (null? #{ids\ 1564}#)
                                                    (#{build-sequence\ 1276}#
                                                      #f
                                                      (map (lambda (#{x\ 1598}#)
                                                             (#{chi\ 1333}#
                                                               (cdr #{x\ 1598}#)
                                                               (car #{x\ 1598}#)
                                                               '(())
                                                               #{mod\ 1578}#))
                                                           (cons (cons #{er\ 1572}#
                                                                       (#{source-wrap\ 1326}#
                                                                         #{e\ 1575}#
                                                                         #{w\ 1576}#
                                                                         #{s\ 1577}#
                                                                         #{mod\ 1578}#))
                                                                 (cdr #{body\ 1563}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 1322}#
                                                                 #{ids\ 1564}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 1555}#))
                                                      (letrec ((#{loop\ 1599}#
                                                                 (lambda (#{bs\ 1600}#
                                                                          #{er-cache\ 1601}#
                                                                          #{r-cache\ 1602}#)
                                                                   (if (not (null? #{bs\ 1600}#))
                                                                     (let ((#{b\ 1603}#
                                                                             (car #{bs\ 1600}#)))
                                                                       (if (eq? (car #{b\ 1603}#)
                                                                                'macro)
                                                                         (let ((#{er\ 1604}#
                                                                                 (cadr #{b\ 1603}#)))
                                                                           (let ((#{r-cache\ 1605}#
                                                                                   (if (eq? #{er\ 1604}#
                                                                                            #{er-cache\ 1601}#)
                                                                                     #{r-cache\ 1602}#
                                                                                     (#{macros-only-env\ 1293}#
                                                                                       #{er\ 1604}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 1603}#
                                                                                 (#{eval-local-transformer\ 1340}#
                                                                                   (#{chi\ 1333}#
                                                                                     (cddr #{b\ 1603}#)
                                                                                     #{r-cache\ 1605}#
                                                                                     '(())
                                                                                     #{mod\ 1578}#)
                                                                                   #{mod\ 1578}#))
                                                                               (#{loop\ 1599}#
                                                                                 (cdr #{bs\ 1600}#)
                                                                                 #{er\ 1604}#
                                                                                 #{r-cache\ 1605}#))))
                                                                         (#{loop\ 1599}#
                                                                           (cdr #{bs\ 1600}#)
                                                                           #{er-cache\ 1601}#
                                                                           #{r-cache\ 1602}#)))))))
                                                        (#{loop\ 1599}#
                                                          #{bindings\ 1569}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 1559}#
                                                        (#{extend-env\ 1291}#
                                                          #{labels\ 1565}#
                                                          #{bindings\ 1569}#
                                                          (cdr #{r\ 1559}#)))
                                                      (#{build-letrec\ 1279}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 1566}#)
                                                        #{vars\ 1567}#
                                                        (map (lambda (#{x\ 1606}#)
                                                               (#{chi\ 1333}#
                                                                 (cdr #{x\ 1606}#)
                                                                 (car #{x\ 1606}#)
                                                                 '(())
                                                                 #{mod\ 1578}#))
                                                             #{vals\ 1568}#)
                                                        (#{build-sequence\ 1276}#
                                                          #f
                                                          (map (lambda (#{x\ 1607}#)
                                                                 (#{chi\ 1333}#
                                                                   (cdr #{x\ 1607}#)
                                                                   (car #{x\ 1607}#)
                                                                   '(())
                                                                   #{mod\ 1578}#))
                                                               (cons (cons #{er\ 1572}#
                                                                           (#{source-wrap\ 1326}#
                                                                             #{e\ 1575}#
                                                                             #{w\ 1576}#
                                                                             #{s\ 1577}#
                                                                             #{mod\ 1578}#))
                                                                     (cdr #{body\ 1563}#))))))))))))))))))
                       (#{parse\ 1562}#
                         (map (lambda (#{x\ 1570}#)
                                (cons #{r\ 1559}#
                                      (#{wrap\ 1325}#
                                        #{x\ 1570}#
                                        #{w\ 1561}#
                                        #{mod\ 1558}#)))
                              #{body\ 1554}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 1336}#
             (lambda (#{p\ 1608}#
                      #{e\ 1609}#
                      #{r\ 1610}#
                      #{w\ 1611}#
                      #{rib\ 1612}#
                      #{mod\ 1613}#)
               (letrec ((#{rebuild-macro-output\ 1614}#
                          (lambda (#{x\ 1615}# #{m\ 1616}#)
                            (if (pair? #{x\ 1615}#)
                              (cons (#{rebuild-macro-output\ 1614}#
                                      (car #{x\ 1615}#)
                                      #{m\ 1616}#)
                                    (#{rebuild-macro-output\ 1614}#
                                      (cdr #{x\ 1615}#)
                                      #{m\ 1616}#))
                              (if (#{syntax-object?\ 1281}# #{x\ 1615}#)
                                (let ((#{w\ 1617}#
                                        (#{syntax-object-wrap\ 1283}#
                                          #{x\ 1615}#)))
                                  (let ((#{ms\ 1618}#
                                          (#{wrap-marks\ 1300}# #{w\ 1617}#))
                                        (#{s\ 1619}#
                                          (#{wrap-subst\ 1301}# #{w\ 1617}#)))
                                    (if (if (pair? #{ms\ 1618}#)
                                          (eq? (car #{ms\ 1618}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 1280}#
                                        (#{syntax-object-expression\ 1282}#
                                          #{x\ 1615}#)
                                        (#{make-wrap\ 1299}#
                                          (cdr #{ms\ 1618}#)
                                          (if #{rib\ 1612}#
                                            (cons #{rib\ 1612}#
                                                  (cdr #{s\ 1619}#))
                                            (cdr #{s\ 1619}#)))
                                        (#{syntax-object-module\ 1284}#
                                          #{x\ 1615}#))
                                      (#{make-syntax-object\ 1280}#
                                        (#{syntax-object-expression\ 1282}#
                                          #{x\ 1615}#)
                                        (#{make-wrap\ 1299}#
                                          (cons #{m\ 1616}# #{ms\ 1618}#)
                                          (if #{rib\ 1612}#
                                            (cons #{rib\ 1612}#
                                                  (cons 'shift
                                                        #{s\ 1619}#))
                                            (cons (quote shift) #{s\ 1619}#)))
                                        (let ((#{pmod\ 1620}#
                                                (procedure-module
                                                  #{p\ 1608}#)))
                                          (if #{pmod\ 1620}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 1620}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 1615}#)
                                  (let ((#{n\ 1621}#
                                          (vector-length #{x\ 1615}#)))
                                    (let ((#{v\ 1622}#
                                            (make-vector #{n\ 1621}#)))
                                      (letrec ((#{loop\ 1623}#
                                                 (lambda (#{i\ 1624}#)
                                                   (if (#{fx=\ 1256}#
                                                         #{i\ 1624}#
                                                         #{n\ 1621}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 1622}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 1622}#
                                                         #{i\ 1624}#
                                                         (#{rebuild-macro-output\ 1614}#
                                                           (vector-ref
                                                             #{x\ 1615}#
                                                             #{i\ 1624}#)
                                                           #{m\ 1616}#))
                                                       (#{loop\ 1623}#
                                                         (#{fx+\ 1254}#
                                                           #{i\ 1624}#
                                                           1)))))))
                                        (#{loop\ 1623}# 0))))
                                  (if (symbol? #{x\ 1615}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 1326}#
                                        #{e\ 1609}#
                                        #{w\ 1611}#
                                        s
                                        #{mod\ 1613}#)
                                      #{x\ 1615}#)
                                    #{x\ 1615}#)))))))
                 (#{rebuild-macro-output\ 1614}#
                   (#{p\ 1608}#
                     (#{wrap\ 1325}#
                       #{e\ 1609}#
                       (#{anti-mark\ 1312}# #{w\ 1611}#)
                       #{mod\ 1613}#))
                   (string #\m)))))
           (#{chi-application\ 1335}#
             (lambda (#{x\ 1625}#
                      #{e\ 1626}#
                      #{r\ 1627}#
                      #{w\ 1628}#
                      #{s\ 1629}#
                      #{mod\ 1630}#)
               ((lambda (#{tmp\ 1631}#)
                  ((lambda (#{tmp\ 1632}#)
                     (if #{tmp\ 1632}#
                       (apply (lambda (#{e0\ 1633}# #{e1\ 1634}#)
                                (#{build-application\ 1264}#
                                  #{s\ 1629}#
                                  #{x\ 1625}#
                                  (map (lambda (#{e\ 1635}#)
                                         (#{chi\ 1333}#
                                           #{e\ 1635}#
                                           #{r\ 1627}#
                                           #{w\ 1628}#
                                           #{mod\ 1630}#))
                                       #{e1\ 1634}#)))
                              #{tmp\ 1632}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 1631}#)))
                   ($sc-dispatch
                     #{tmp\ 1631}#
                     '(any . each-any))))
                #{e\ 1626}#)))
           (#{chi-expr\ 1334}#
             (lambda (#{type\ 1637}#
                      #{value\ 1638}#
                      #{e\ 1639}#
                      #{r\ 1640}#
                      #{w\ 1641}#
                      #{s\ 1642}#
                      #{mod\ 1643}#)
               (if (memv #{type\ 1637}# (quote (lexical)))
                 (#{build-lexical-reference\ 1266}#
                   'value
                   #{s\ 1642}#
                   #{e\ 1639}#
                   #{value\ 1638}#)
                 (if (memv #{type\ 1637}# (quote (core core-form)))
                   (#{value\ 1638}#
                     #{e\ 1639}#
                     #{r\ 1640}#
                     #{w\ 1641}#
                     #{s\ 1642}#
                     #{mod\ 1643}#)
                   (if (memv #{type\ 1637}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 1638}# #{e\ 1639}#))
                       (lambda (#{id\ 1644}# #{mod\ 1645}#)
                         (#{build-global-reference\ 1269}#
                           #{s\ 1642}#
                           #{id\ 1644}#
                           #{mod\ 1645}#)))
                     (if (memv #{type\ 1637}# (quote (lexical-call)))
                       (#{chi-application\ 1335}#
                         (#{build-lexical-reference\ 1266}#
                           'fun
                           (#{source-annotation\ 1288}# (car #{e\ 1639}#))
                           (car #{e\ 1639}#)
                           #{value\ 1638}#)
                         #{e\ 1639}#
                         #{r\ 1640}#
                         #{w\ 1641}#
                         #{s\ 1642}#
                         #{mod\ 1643}#)
                       (if (memv #{type\ 1637}# (quote (global-call)))
                         (#{chi-application\ 1335}#
                           (#{build-global-reference\ 1269}#
                             (#{source-annotation\ 1288}# (car #{e\ 1639}#))
                             (if (#{syntax-object?\ 1281}# #{value\ 1638}#)
                               (#{syntax-object-expression\ 1282}#
                                 #{value\ 1638}#)
                               #{value\ 1638}#)
                             (if (#{syntax-object?\ 1281}# #{value\ 1638}#)
                               (#{syntax-object-module\ 1284}# #{value\ 1638}#)
                               #{mod\ 1643}#))
                           #{e\ 1639}#
                           #{r\ 1640}#
                           #{w\ 1641}#
                           #{s\ 1642}#
                           #{mod\ 1643}#)
                         (if (memv #{type\ 1637}# (quote (constant)))
                           (#{build-data\ 1275}#
                             #{s\ 1642}#
                             (#{strip\ 1343}#
                               (#{source-wrap\ 1326}#
                                 #{e\ 1639}#
                                 #{w\ 1641}#
                                 #{s\ 1642}#
                                 #{mod\ 1643}#)
                               '(())))
                           (if (memv #{type\ 1637}# (quote (global)))
                             (#{build-global-reference\ 1269}#
                               #{s\ 1642}#
                               #{value\ 1638}#
                               #{mod\ 1643}#)
                             (if (memv #{type\ 1637}# (quote (call)))
                               (#{chi-application\ 1335}#
                                 (#{chi\ 1333}#
                                   (car #{e\ 1639}#)
                                   #{r\ 1640}#
                                   #{w\ 1641}#
                                   #{mod\ 1643}#)
                                 #{e\ 1639}#
                                 #{r\ 1640}#
                                 #{w\ 1641}#
                                 #{s\ 1642}#
                                 #{mod\ 1643}#)
                               (if (memv #{type\ 1637}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 1646}#)
                                    ((lambda (#{tmp\ 1647}#)
                                       (if #{tmp\ 1647}#
                                         (apply (lambda (#{_\ 1648}#
                                                         #{e1\ 1649}#
                                                         #{e2\ 1650}#)
                                                  (#{chi-sequence\ 1327}#
                                                    (cons #{e1\ 1649}#
                                                          #{e2\ 1650}#)
                                                    #{r\ 1640}#
                                                    #{w\ 1641}#
                                                    #{s\ 1642}#
                                                    #{mod\ 1643}#))
                                                #{tmp\ 1647}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 1646}#)))
                                     ($sc-dispatch
                                       #{tmp\ 1646}#
                                       '(any any . each-any))))
                                  #{e\ 1639}#)
                                 (if (memv #{type\ 1637}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 1339}#
                                     #{value\ 1638}#
                                     #{e\ 1639}#
                                     #{r\ 1640}#
                                     #{w\ 1641}#
                                     #{s\ 1642}#
                                     #{mod\ 1643}#
                                     #{chi-sequence\ 1327}#)
                                   (if (memv #{type\ 1637}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 1652}#)
                                        ((lambda (#{tmp\ 1653}#)
                                           (if #{tmp\ 1653}#
                                             (apply (lambda (#{_\ 1654}#
                                                             #{x\ 1655}#
                                                             #{e1\ 1656}#
                                                             #{e2\ 1657}#)
                                                      (let ((#{when-list\ 1658}#
                                                              (#{chi-when-list\ 1330}#
                                                                #{e\ 1639}#
                                                                #{x\ 1655}#
                                                                #{w\ 1641}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 1658}#)
                                                          (#{chi-sequence\ 1327}#
                                                            (cons #{e1\ 1656}#
                                                                  #{e2\ 1657}#)
                                                            #{r\ 1640}#
                                                            #{w\ 1641}#
                                                            #{s\ 1642}#
                                                            #{mod\ 1643}#)
                                                          (#{chi-void\ 1341}#))))
                                                    #{tmp\ 1653}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 1652}#)))
                                         ($sc-dispatch
                                           #{tmp\ 1652}#
                                           '(any each-any any . each-any))))
                                      #{e\ 1639}#)
                                     (if (memv #{type\ 1637}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 1639}#
                                         (#{wrap\ 1325}#
                                           #{value\ 1638}#
                                           #{w\ 1641}#
                                           #{mod\ 1643}#))
                                       (if (memv #{type\ 1637}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 1326}#
                                             #{e\ 1639}#
                                             #{w\ 1641}#
                                             #{s\ 1642}#
                                             #{mod\ 1643}#))
                                         (if (memv #{type\ 1637}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 1326}#
                                               #{e\ 1639}#
                                               #{w\ 1641}#
                                               #{s\ 1642}#
                                               #{mod\ 1643}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 1326}#
                                               #{e\ 1639}#
                                               #{w\ 1641}#
                                               #{s\ 1642}#
                                               #{mod\ 1643}#))))))))))))))))))
           (#{chi\ 1333}#
             (lambda (#{e\ 1661}#
                      #{r\ 1662}#
                      #{w\ 1663}#
                      #{mod\ 1664}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 1331}#
                     #{e\ 1661}#
                     #{r\ 1662}#
                     #{w\ 1663}#
                     (#{source-annotation\ 1288}# #{e\ 1661}#)
                     #f
                     #{mod\ 1664}#
                     #f))
                 (lambda (#{type\ 1665}#
                          #{value\ 1666}#
                          #{e\ 1667}#
                          #{w\ 1668}#
                          #{s\ 1669}#
                          #{mod\ 1670}#)
                   (#{chi-expr\ 1334}#
                     #{type\ 1665}#
                     #{value\ 1666}#
                     #{e\ 1667}#
                     #{r\ 1662}#
                     #{w\ 1668}#
                     #{s\ 1669}#
                     #{mod\ 1670}#)))))
           (#{chi-top\ 1332}#
             (lambda (#{e\ 1671}#
                      #{r\ 1672}#
                      #{w\ 1673}#
                      #{m\ 1674}#
                      #{esew\ 1675}#
                      #{mod\ 1676}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 1331}#
                     #{e\ 1671}#
                     #{r\ 1672}#
                     #{w\ 1673}#
                     (#{source-annotation\ 1288}# #{e\ 1671}#)
                     #f
                     #{mod\ 1676}#
                     #f))
                 (lambda (#{type\ 1684}#
                          #{value\ 1685}#
                          #{e\ 1686}#
                          #{w\ 1687}#
                          #{s\ 1688}#
                          #{mod\ 1689}#)
                   (if (memv #{type\ 1684}# (quote (begin-form)))
                     ((lambda (#{tmp\ 1690}#)
                        ((lambda (#{tmp\ 1691}#)
                           (if #{tmp\ 1691}#
                             (apply (lambda (#{_\ 1692}#) (#{chi-void\ 1341}#))
                                    #{tmp\ 1691}#)
                             ((lambda (#{tmp\ 1693}#)
                                (if #{tmp\ 1693}#
                                  (apply (lambda (#{_\ 1694}#
                                                  #{e1\ 1695}#
                                                  #{e2\ 1696}#)
                                           (#{chi-top-sequence\ 1328}#
                                             (cons #{e1\ 1695}# #{e2\ 1696}#)
                                             #{r\ 1672}#
                                             #{w\ 1687}#
                                             #{s\ 1688}#
                                             #{m\ 1674}#
                                             #{esew\ 1675}#
                                             #{mod\ 1689}#))
                                         #{tmp\ 1693}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 1690}#)))
                              ($sc-dispatch
                                #{tmp\ 1690}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 1690}# (quote (any)))))
                      #{e\ 1686}#)
                     (if (memv #{type\ 1684}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 1339}#
                         #{value\ 1685}#
                         #{e\ 1686}#
                         #{r\ 1672}#
                         #{w\ 1687}#
                         #{s\ 1688}#
                         #{mod\ 1689}#
                         (lambda (#{body\ 1698}#
                                  #{r\ 1699}#
                                  #{w\ 1700}#
                                  #{s\ 1701}#
                                  #{mod\ 1702}#)
                           (#{chi-top-sequence\ 1328}#
                             #{body\ 1698}#
                             #{r\ 1699}#
                             #{w\ 1700}#
                             #{s\ 1701}#
                             #{m\ 1674}#
                             #{esew\ 1675}#
                             #{mod\ 1702}#)))
                       (if (memv #{type\ 1684}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 1703}#)
                            ((lambda (#{tmp\ 1704}#)
                               (if #{tmp\ 1704}#
                                 (apply (lambda (#{_\ 1705}#
                                                 #{x\ 1706}#
                                                 #{e1\ 1707}#
                                                 #{e2\ 1708}#)
                                          (let ((#{when-list\ 1709}#
                                                  (#{chi-when-list\ 1330}#
                                                    #{e\ 1686}#
                                                    #{x\ 1706}#
                                                    #{w\ 1687}#))
                                                (#{body\ 1710}#
                                                  (cons #{e1\ 1707}#
                                                        #{e2\ 1708}#)))
                                            (if (eq? #{m\ 1674}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 1709}#)
                                                (#{chi-top-sequence\ 1328}#
                                                  #{body\ 1710}#
                                                  #{r\ 1672}#
                                                  #{w\ 1687}#
                                                  #{s\ 1688}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 1689}#)
                                                (#{chi-void\ 1341}#))
                                              (if (memq 'load
                                                        #{when-list\ 1709}#)
                                                (if (let ((#{t\ 1713}#
                                                            (memq 'compile
                                                                  #{when-list\ 1709}#)))
                                                      (if #{t\ 1713}#
                                                        #{t\ 1713}#
                                                        (if (eq? #{m\ 1674}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 1709}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 1328}#
                                                    #{body\ 1710}#
                                                    #{r\ 1672}#
                                                    #{w\ 1687}#
                                                    #{s\ 1688}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 1689}#)
                                                  (if (memq #{m\ 1674}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 1328}#
                                                      #{body\ 1710}#
                                                      #{r\ 1672}#
                                                      #{w\ 1687}#
                                                      #{s\ 1688}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 1689}#)
                                                    (#{chi-void\ 1341}#)))
                                                (if (let ((#{t\ 1714}#
                                                            (memq 'compile
                                                                  #{when-list\ 1709}#)))
                                                      (if #{t\ 1714}#
                                                        #{t\ 1714}#
                                                        (if (eq? #{m\ 1674}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 1709}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 1258}#
                                                      (#{chi-top-sequence\ 1328}#
                                                        #{body\ 1710}#
                                                        #{r\ 1672}#
                                                        #{w\ 1687}#
                                                        #{s\ 1688}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 1689}#)
                                                      #{mod\ 1689}#)
                                                    (#{chi-void\ 1341}#))
                                                  (#{chi-void\ 1341}#))))))
                                        #{tmp\ 1704}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 1703}#)))
                             ($sc-dispatch
                               #{tmp\ 1703}#
                               '(any each-any any . each-any))))
                          #{e\ 1686}#)
                         (if (memv #{type\ 1684}#
                                   '(define-syntax-form))
                           (let ((#{n\ 1715}#
                                   (#{id-var-name\ 1319}#
                                     #{value\ 1685}#
                                     #{w\ 1687}#))
                                 (#{r\ 1716}#
                                   (#{macros-only-env\ 1293}# #{r\ 1672}#)))
                             (if (memv #{m\ 1674}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 1675}#)
                                 (let ((#{e\ 1717}#
                                         (#{chi-install-global\ 1329}#
                                           #{n\ 1715}#
                                           (#{chi\ 1333}#
                                             #{e\ 1686}#
                                             #{r\ 1716}#
                                             #{w\ 1687}#
                                             #{mod\ 1689}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 1258}#
                                       #{e\ 1717}#
                                       #{mod\ 1689}#)
                                     (if (memq (quote load) #{esew\ 1675}#)
                                       #{e\ 1717}#
                                       (#{chi-void\ 1341}#))))
                                 (if (memq (quote load) #{esew\ 1675}#)
                                   (#{chi-install-global\ 1329}#
                                     #{n\ 1715}#
                                     (#{chi\ 1333}#
                                       #{e\ 1686}#
                                       #{r\ 1716}#
                                       #{w\ 1687}#
                                       #{mod\ 1689}#))
                                   (#{chi-void\ 1341}#)))
                               (if (memv #{m\ 1674}# (quote (c&e)))
                                 (let ((#{e\ 1718}#
                                         (#{chi-install-global\ 1329}#
                                           #{n\ 1715}#
                                           (#{chi\ 1333}#
                                             #{e\ 1686}#
                                             #{r\ 1716}#
                                             #{w\ 1687}#
                                             #{mod\ 1689}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 1258}#
                                       #{e\ 1718}#
                                       #{mod\ 1689}#)
                                     #{e\ 1718}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 1675}#)
                                     (#{top-level-eval-hook\ 1258}#
                                       (#{chi-install-global\ 1329}#
                                         #{n\ 1715}#
                                         (#{chi\ 1333}#
                                           #{e\ 1686}#
                                           #{r\ 1716}#
                                           #{w\ 1687}#
                                           #{mod\ 1689}#))
                                       #{mod\ 1689}#))
                                   (#{chi-void\ 1341}#)))))
                           (if (memv #{type\ 1684}# (quote (define-form)))
                             (let ((#{n\ 1719}#
                                     (#{id-var-name\ 1319}#
                                       #{value\ 1685}#
                                       #{w\ 1687}#)))
                               (let ((#{type\ 1720}#
                                       (#{binding-type\ 1289}#
                                         (#{lookup\ 1294}#
                                           #{n\ 1719}#
                                           #{r\ 1672}#
                                           #{mod\ 1689}#))))
                                 (if (memv #{type\ 1720}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 1719}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 1721}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 1719}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 1719}#
                                           (if (variable? #{old\ 1721}#)
                                             (variable-ref #{old\ 1721}#)
                                             #f))))
                                     (let ((#{x\ 1722}#
                                             (#{build-global-definition\ 1272}#
                                               #{s\ 1688}#
                                               #{n\ 1719}#
                                               (#{chi\ 1333}#
                                                 #{e\ 1686}#
                                                 #{r\ 1672}#
                                                 #{w\ 1687}#
                                                 #{mod\ 1689}#))))
                                       (begin
                                         (if (eq? #{m\ 1674}# (quote c&e))
                                           (#{top-level-eval-hook\ 1258}#
                                             #{x\ 1722}#
                                             #{mod\ 1689}#))
                                         #{x\ 1722}#)))
                                   (if (memv #{type\ 1720}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 1686}#
                                       (#{wrap\ 1325}#
                                         #{value\ 1685}#
                                         #{w\ 1687}#
                                         #{mod\ 1689}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 1686}#
                                       (#{wrap\ 1325}#
                                         #{value\ 1685}#
                                         #{w\ 1687}#
                                         #{mod\ 1689}#))))))
                             (let ((#{x\ 1723}#
                                     (#{chi-expr\ 1334}#
                                       #{type\ 1684}#
                                       #{value\ 1685}#
                                       #{e\ 1686}#
                                       #{r\ 1672}#
                                       #{w\ 1687}#
                                       #{s\ 1688}#
                                       #{mod\ 1689}#)))
                               (begin
                                 (if (eq? #{m\ 1674}# (quote c&e))
                                   (#{top-level-eval-hook\ 1258}#
                                     #{x\ 1723}#
                                     #{mod\ 1689}#))
                                 #{x\ 1723}#)))))))))))
           (#{syntax-type\ 1331}#
             (lambda (#{e\ 1724}#
                      #{r\ 1725}#
                      #{w\ 1726}#
                      #{s\ 1727}#
                      #{rib\ 1728}#
                      #{mod\ 1729}#
                      #{for-car?\ 1730}#)
               (if (symbol? #{e\ 1724}#)
                 (let ((#{n\ 1731}#
                         (#{id-var-name\ 1319}# #{e\ 1724}# #{w\ 1726}#)))
                   (let ((#{b\ 1732}#
                           (#{lookup\ 1294}#
                             #{n\ 1731}#
                             #{r\ 1725}#
                             #{mod\ 1729}#)))
                     (let ((#{type\ 1733}#
                             (#{binding-type\ 1289}# #{b\ 1732}#)))
                       (if (memv #{type\ 1733}# (quote (lexical)))
                         (values
                           #{type\ 1733}#
                           (#{binding-value\ 1290}# #{b\ 1732}#)
                           #{e\ 1724}#
                           #{w\ 1726}#
                           #{s\ 1727}#
                           #{mod\ 1729}#)
                         (if (memv #{type\ 1733}# (quote (global)))
                           (values
                             #{type\ 1733}#
                             #{n\ 1731}#
                             #{e\ 1724}#
                             #{w\ 1726}#
                             #{s\ 1727}#
                             #{mod\ 1729}#)
                           (if (memv #{type\ 1733}# (quote (macro)))
                             (if #{for-car?\ 1730}#
                               (values
                                 #{type\ 1733}#
                                 (#{binding-value\ 1290}# #{b\ 1732}#)
                                 #{e\ 1724}#
                                 #{w\ 1726}#
                                 #{s\ 1727}#
                                 #{mod\ 1729}#)
                               (#{syntax-type\ 1331}#
                                 (#{chi-macro\ 1336}#
                                   (#{binding-value\ 1290}# #{b\ 1732}#)
                                   #{e\ 1724}#
                                   #{r\ 1725}#
                                   #{w\ 1726}#
                                   #{rib\ 1728}#
                                   #{mod\ 1729}#)
                                 #{r\ 1725}#
                                 '(())
                                 #{s\ 1727}#
                                 #{rib\ 1728}#
                                 #{mod\ 1729}#
                                 #f))
                             (values
                               #{type\ 1733}#
                               (#{binding-value\ 1290}# #{b\ 1732}#)
                               #{e\ 1724}#
                               #{w\ 1726}#
                               #{s\ 1727}#
                               #{mod\ 1729}#)))))))
                 (if (pair? #{e\ 1724}#)
                   (let ((#{first\ 1734}# (car #{e\ 1724}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 1331}#
                           #{first\ 1734}#
                           #{r\ 1725}#
                           #{w\ 1726}#
                           #{s\ 1727}#
                           #{rib\ 1728}#
                           #{mod\ 1729}#
                           #t))
                       (lambda (#{ftype\ 1735}#
                                #{fval\ 1736}#
                                #{fe\ 1737}#
                                #{fw\ 1738}#
                                #{fs\ 1739}#
                                #{fmod\ 1740}#)
                         (if (memv #{ftype\ 1735}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 1736}#
                             #{e\ 1724}#
                             #{w\ 1726}#
                             #{s\ 1727}#
                             #{mod\ 1729}#)
                           (if (memv #{ftype\ 1735}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 1280}#
                                 #{fval\ 1736}#
                                 #{w\ 1726}#
                                 #{fmod\ 1740}#)
                               #{e\ 1724}#
                               #{w\ 1726}#
                               #{s\ 1727}#
                               #{mod\ 1729}#)
                             (if (memv #{ftype\ 1735}# (quote (macro)))
                               (#{syntax-type\ 1331}#
                                 (#{chi-macro\ 1336}#
                                   #{fval\ 1736}#
                                   #{e\ 1724}#
                                   #{r\ 1725}#
                                   #{w\ 1726}#
                                   #{rib\ 1728}#
                                   #{mod\ 1729}#)
                                 #{r\ 1725}#
                                 '(())
                                 #{s\ 1727}#
                                 #{rib\ 1728}#
                                 #{mod\ 1729}#
                                 #{for-car?\ 1730}#)
                               (if (memv #{ftype\ 1735}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 1736}# #{e\ 1724}#))
                                   (lambda (#{sym\ 1741}# #{mod\ 1742}#)
                                     (#{syntax-type\ 1331}#
                                       #{sym\ 1741}#
                                       #{r\ 1725}#
                                       #{w\ 1726}#
                                       #{s\ 1727}#
                                       #{rib\ 1728}#
                                       #{mod\ 1742}#
                                       #{for-car?\ 1730}#)))
                                 (if (memv #{ftype\ 1735}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 1736}#
                                     #{e\ 1724}#
                                     #{w\ 1726}#
                                     #{s\ 1727}#
                                     #{mod\ 1729}#)
                                   (if (memv #{ftype\ 1735}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 1736}#
                                       #{e\ 1724}#
                                       #{w\ 1726}#
                                       #{s\ 1727}#
                                       #{mod\ 1729}#)
                                     (if (memv #{ftype\ 1735}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 1724}#
                                         #{w\ 1726}#
                                         #{s\ 1727}#
                                         #{mod\ 1729}#)
                                       (if (memv #{ftype\ 1735}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 1724}#
                                           #{w\ 1726}#
                                           #{s\ 1727}#
                                           #{mod\ 1729}#)
                                         (if (memv #{ftype\ 1735}#
                                                   '(define))
                                           ((lambda (#{tmp\ 1743}#)
                                              ((lambda (#{tmp\ 1744}#)
                                                 (if (if #{tmp\ 1744}#
                                                       (apply (lambda (#{_\ 1745}#
                                                                       #{name\ 1746}#
                                                                       #{val\ 1747}#)
                                                                (#{id?\ 1297}#
                                                                  #{name\ 1746}#))
                                                              #{tmp\ 1744}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 1748}#
                                                                   #{name\ 1749}#
                                                                   #{val\ 1750}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 1749}#
                                                              #{val\ 1750}#
                                                              #{w\ 1726}#
                                                              #{s\ 1727}#
                                                              #{mod\ 1729}#))
                                                          #{tmp\ 1744}#)
                                                   ((lambda (#{tmp\ 1751}#)
                                                      (if (if #{tmp\ 1751}#
                                                            (apply (lambda (#{_\ 1752}#
                                                                            #{name\ 1753}#
                                                                            #{args\ 1754}#
                                                                            #{e1\ 1755}#
                                                                            #{e2\ 1756}#)
                                                                     (if (#{id?\ 1297}#
                                                                           #{name\ 1753}#)
                                                                       (#{valid-bound-ids?\ 1322}#
                                                                         (#{lambda-var-list\ 1345}#
                                                                           #{args\ 1754}#))
                                                                       #f))
                                                                   #{tmp\ 1751}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 1757}#
                                                                        #{name\ 1758}#
                                                                        #{args\ 1759}#
                                                                        #{e1\ 1760}#
                                                                        #{e2\ 1761}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 1325}#
                                                                     #{name\ 1758}#
                                                                     #{w\ 1726}#
                                                                     #{mod\ 1729}#)
                                                                   (#{decorate-source\ 1262}#
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
                                                                           (#{wrap\ 1325}#
                                                                             (cons #{args\ 1759}#
                                                                                   (cons #{e1\ 1760}#
                                                                                         #{e2\ 1761}#))
                                                                             #{w\ 1726}#
                                                                             #{mod\ 1729}#))
                                                                     #{s\ 1727}#)
                                                                   '(())
                                                                   #{s\ 1727}#
                                                                   #{mod\ 1729}#))
                                                               #{tmp\ 1751}#)
                                                        ((lambda (#{tmp\ 1763}#)
                                                           (if (if #{tmp\ 1763}#
                                                                 (apply (lambda (#{_\ 1764}#
                                                                                 #{name\ 1765}#)
                                                                          (#{id?\ 1297}#
                                                                            #{name\ 1765}#))
                                                                        #{tmp\ 1763}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 1766}#
                                                                             #{name\ 1767}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 1325}#
                                                                          #{name\ 1767}#
                                                                          #{w\ 1726}#
                                                                          #{mod\ 1729}#)
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
                                                                        #{s\ 1727}#
                                                                        #{mod\ 1729}#))
                                                                    #{tmp\ 1763}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 1743}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 1743}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 1743}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 1743}#
                                                 '(any any any))))
                                            #{e\ 1724}#)
                                           (if (memv #{ftype\ 1735}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 1768}#)
                                                ((lambda (#{tmp\ 1769}#)
                                                   (if (if #{tmp\ 1769}#
                                                         (apply (lambda (#{_\ 1770}#
                                                                         #{name\ 1771}#
                                                                         #{val\ 1772}#)
                                                                  (#{id?\ 1297}#
                                                                    #{name\ 1771}#))
                                                                #{tmp\ 1769}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 1773}#
                                                                     #{name\ 1774}#
                                                                     #{val\ 1775}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 1774}#
                                                                #{val\ 1775}#
                                                                #{w\ 1726}#
                                                                #{s\ 1727}#
                                                                #{mod\ 1729}#))
                                                            #{tmp\ 1769}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 1768}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 1768}#
                                                   '(any any any))))
                                              #{e\ 1724}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 1724}#
                                               #{w\ 1726}#
                                               #{s\ 1727}#
                                               #{mod\ 1729}#))))))))))))))
                   (if (#{syntax-object?\ 1281}# #{e\ 1724}#)
                     (#{syntax-type\ 1331}#
                       (#{syntax-object-expression\ 1282}# #{e\ 1724}#)
                       #{r\ 1725}#
                       (#{join-wraps\ 1316}#
                         #{w\ 1726}#
                         (#{syntax-object-wrap\ 1283}# #{e\ 1724}#))
                       #{s\ 1727}#
                       #{rib\ 1728}#
                       (let ((#{t\ 1776}#
                               (#{syntax-object-module\ 1284}# #{e\ 1724}#)))
                         (if #{t\ 1776}# #{t\ 1776}# #{mod\ 1729}#))
                       #{for-car?\ 1730}#)
                     (if (self-evaluating? #{e\ 1724}#)
                       (values
                         'constant
                         #f
                         #{e\ 1724}#
                         #{w\ 1726}#
                         #{s\ 1727}#
                         #{mod\ 1729}#)
                       (values
                         'other
                         #f
                         #{e\ 1724}#
                         #{w\ 1726}#
                         #{s\ 1727}#
                         #{mod\ 1729}#)))))))
           (#{chi-when-list\ 1330}#
             (lambda (#{e\ 1777}# #{when-list\ 1778}# #{w\ 1779}#)
               (letrec ((#{f\ 1780}#
                          (lambda (#{when-list\ 1781}# #{situations\ 1782}#)
                            (if (null? #{when-list\ 1781}#)
                              #{situations\ 1782}#
                              (#{f\ 1780}#
                                (cdr #{when-list\ 1781}#)
                                (cons (let ((#{x\ 1783}#
                                              (car #{when-list\ 1781}#)))
                                        (if (#{free-id=?\ 1320}#
                                              #{x\ 1783}#
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
                                          'compile
                                          (if (#{free-id=?\ 1320}#
                                                #{x\ 1783}#
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
                                                   (hygiene guile)))
                                            'load
                                            (if (#{free-id=?\ 1320}#
                                                  #{x\ 1783}#
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
                                                     (hygiene guile)))
                                              'eval
                                              (syntax-violation
                                                'eval-when
                                                "invalid situation"
                                                #{e\ 1777}#
                                                (#{wrap\ 1325}#
                                                  #{x\ 1783}#
                                                  #{w\ 1779}#
                                                  #f))))))
                                      #{situations\ 1782}#))))))
                 (#{f\ 1780}# #{when-list\ 1778}# (quote ())))))
           (#{chi-install-global\ 1329}#
             (lambda (#{name\ 1784}# #{e\ 1785}#)
               (#{build-global-definition\ 1272}#
                 #f
                 #{name\ 1784}#
                 (if (let ((#{v\ 1786}#
                             (module-variable
                               (current-module)
                               #{name\ 1784}#)))
                       (if #{v\ 1786}#
                         (if (variable-bound? #{v\ 1786}#)
                           (if (macro? (variable-ref #{v\ 1786}#))
                             (not (eq? (macro-type (variable-ref #{v\ 1786}#))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (#{build-application\ 1264}#
                     #f
                     (#{build-primref\ 1274}#
                       #f
                       'make-extended-syncase-macro)
                     (list (#{build-application\ 1264}#
                             #f
                             (#{build-primref\ 1274}# #f (quote module-ref))
                             (list (#{build-application\ 1264}#
                                     #f
                                     (#{build-primref\ 1274}#
                                       #f
                                       'current-module)
                                     '())
                                   (#{build-data\ 1275}# #f #{name\ 1784}#)))
                           (#{build-data\ 1275}# #f (quote macro))
                           #{e\ 1785}#))
                   (#{build-application\ 1264}#
                     #f
                     (#{build-primref\ 1274}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 1275}# #f (quote macro))
                           #{e\ 1785}#))))))
           (#{chi-top-sequence\ 1328}#
             (lambda (#{body\ 1787}#
                      #{r\ 1788}#
                      #{w\ 1789}#
                      #{s\ 1790}#
                      #{m\ 1791}#
                      #{esew\ 1792}#
                      #{mod\ 1793}#)
               (#{build-sequence\ 1276}#
                 #{s\ 1790}#
                 (letrec ((#{dobody\ 1794}#
                            (lambda (#{body\ 1795}#
                                     #{r\ 1796}#
                                     #{w\ 1797}#
                                     #{m\ 1798}#
                                     #{esew\ 1799}#
                                     #{mod\ 1800}#)
                              (if (null? #{body\ 1795}#)
                                '()
                                (let ((#{first\ 1801}#
                                        (#{chi-top\ 1332}#
                                          (car #{body\ 1795}#)
                                          #{r\ 1796}#
                                          #{w\ 1797}#
                                          #{m\ 1798}#
                                          #{esew\ 1799}#
                                          #{mod\ 1800}#)))
                                  (cons #{first\ 1801}#
                                        (#{dobody\ 1794}#
                                          (cdr #{body\ 1795}#)
                                          #{r\ 1796}#
                                          #{w\ 1797}#
                                          #{m\ 1798}#
                                          #{esew\ 1799}#
                                          #{mod\ 1800}#)))))))
                   (#{dobody\ 1794}#
                     #{body\ 1787}#
                     #{r\ 1788}#
                     #{w\ 1789}#
                     #{m\ 1791}#
                     #{esew\ 1792}#
                     #{mod\ 1793}#)))))
           (#{chi-sequence\ 1327}#
             (lambda (#{body\ 1802}#
                      #{r\ 1803}#
                      #{w\ 1804}#
                      #{s\ 1805}#
                      #{mod\ 1806}#)
               (#{build-sequence\ 1276}#
                 #{s\ 1805}#
                 (letrec ((#{dobody\ 1807}#
                            (lambda (#{body\ 1808}#
                                     #{r\ 1809}#
                                     #{w\ 1810}#
                                     #{mod\ 1811}#)
                              (if (null? #{body\ 1808}#)
                                '()
                                (let ((#{first\ 1812}#
                                        (#{chi\ 1333}#
                                          (car #{body\ 1808}#)
                                          #{r\ 1809}#
                                          #{w\ 1810}#
                                          #{mod\ 1811}#)))
                                  (cons #{first\ 1812}#
                                        (#{dobody\ 1807}#
                                          (cdr #{body\ 1808}#)
                                          #{r\ 1809}#
                                          #{w\ 1810}#
                                          #{mod\ 1811}#)))))))
                   (#{dobody\ 1807}#
                     #{body\ 1802}#
                     #{r\ 1803}#
                     #{w\ 1804}#
                     #{mod\ 1806}#)))))
           (#{source-wrap\ 1326}#
             (lambda (#{x\ 1813}#
                      #{w\ 1814}#
                      #{s\ 1815}#
                      #{defmod\ 1816}#)
               (#{wrap\ 1325}#
                 (#{decorate-source\ 1262}#
                   #{x\ 1813}#
                   #{s\ 1815}#)
                 #{w\ 1814}#
                 #{defmod\ 1816}#)))
           (#{wrap\ 1325}#
             (lambda (#{x\ 1817}# #{w\ 1818}# #{defmod\ 1819}#)
               (if (if (null? (#{wrap-marks\ 1300}# #{w\ 1818}#))
                     (null? (#{wrap-subst\ 1301}# #{w\ 1818}#))
                     #f)
                 #{x\ 1817}#
                 (if (#{syntax-object?\ 1281}# #{x\ 1817}#)
                   (#{make-syntax-object\ 1280}#
                     (#{syntax-object-expression\ 1282}# #{x\ 1817}#)
                     (#{join-wraps\ 1316}#
                       #{w\ 1818}#
                       (#{syntax-object-wrap\ 1283}# #{x\ 1817}#))
                     (#{syntax-object-module\ 1284}# #{x\ 1817}#))
                   (if (null? #{x\ 1817}#)
                     #{x\ 1817}#
                     (#{make-syntax-object\ 1280}#
                       #{x\ 1817}#
                       #{w\ 1818}#
                       #{defmod\ 1819}#))))))
           (#{bound-id-member?\ 1324}#
             (lambda (#{x\ 1820}# #{list\ 1821}#)
               (if (not (null? #{list\ 1821}#))
                 (let ((#{t\ 1822}#
                         (#{bound-id=?\ 1321}#
                           #{x\ 1820}#
                           (car #{list\ 1821}#))))
                   (if #{t\ 1822}#
                     #{t\ 1822}#
                     (#{bound-id-member?\ 1324}#
                       #{x\ 1820}#
                       (cdr #{list\ 1821}#))))
                 #f)))
           (#{distinct-bound-ids?\ 1323}#
             (lambda (#{ids\ 1823}#)
               (letrec ((#{distinct?\ 1824}#
                          (lambda (#{ids\ 1825}#)
                            (let ((#{t\ 1826}# (null? #{ids\ 1825}#)))
                              (if #{t\ 1826}#
                                #{t\ 1826}#
                                (if (not (#{bound-id-member?\ 1324}#
                                           (car #{ids\ 1825}#)
                                           (cdr #{ids\ 1825}#)))
                                  (#{distinct?\ 1824}# (cdr #{ids\ 1825}#))
                                  #f))))))
                 (#{distinct?\ 1824}# #{ids\ 1823}#))))
           (#{valid-bound-ids?\ 1322}#
             (lambda (#{ids\ 1827}#)
               (if (letrec ((#{all-ids?\ 1828}#
                              (lambda (#{ids\ 1829}#)
                                (let ((#{t\ 1830}# (null? #{ids\ 1829}#)))
                                  (if #{t\ 1830}#
                                    #{t\ 1830}#
                                    (if (#{id?\ 1297}# (car #{ids\ 1829}#))
                                      (#{all-ids?\ 1828}# (cdr #{ids\ 1829}#))
                                      #f))))))
                     (#{all-ids?\ 1828}# #{ids\ 1827}#))
                 (#{distinct-bound-ids?\ 1323}# #{ids\ 1827}#)
                 #f)))
           (#{bound-id=?\ 1321}#
             (lambda (#{i\ 1831}# #{j\ 1832}#)
               (if (if (#{syntax-object?\ 1281}# #{i\ 1831}#)
                     (#{syntax-object?\ 1281}# #{j\ 1832}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 1282}# #{i\ 1831}#)
                          (#{syntax-object-expression\ 1282}# #{j\ 1832}#))
                   (#{same-marks?\ 1318}#
                     (#{wrap-marks\ 1300}#
                       (#{syntax-object-wrap\ 1283}# #{i\ 1831}#))
                     (#{wrap-marks\ 1300}#
                       (#{syntax-object-wrap\ 1283}# #{j\ 1832}#)))
                   #f)
                 (eq? #{i\ 1831}# #{j\ 1832}#))))
           (#{free-id=?\ 1320}#
             (lambda (#{i\ 1833}# #{j\ 1834}#)
               (if (eq? (let ((#{x\ 1835}# #{i\ 1833}#))
                          (if (#{syntax-object?\ 1281}# #{x\ 1835}#)
                            (#{syntax-object-expression\ 1282}# #{x\ 1835}#)
                            #{x\ 1835}#))
                        (let ((#{x\ 1836}# #{j\ 1834}#))
                          (if (#{syntax-object?\ 1281}# #{x\ 1836}#)
                            (#{syntax-object-expression\ 1282}# #{x\ 1836}#)
                            #{x\ 1836}#)))
                 (eq? (#{id-var-name\ 1319}# #{i\ 1833}# (quote (())))
                      (#{id-var-name\ 1319}# #{j\ 1834}# (quote (()))))
                 #f)))
           (#{id-var-name\ 1319}#
             (lambda (#{id\ 1837}# #{w\ 1838}#)
               (letrec ((#{search-vector-rib\ 1841}#
                          (lambda (#{sym\ 1847}#
                                   #{subst\ 1848}#
                                   #{marks\ 1849}#
                                   #{symnames\ 1850}#
                                   #{ribcage\ 1851}#)
                            (let ((#{n\ 1852}#
                                    (vector-length #{symnames\ 1850}#)))
                              (letrec ((#{f\ 1853}#
                                         (lambda (#{i\ 1854}#)
                                           (if (#{fx=\ 1256}#
                                                 #{i\ 1854}#
                                                 #{n\ 1852}#)
                                             (#{search\ 1839}#
                                               #{sym\ 1847}#
                                               (cdr #{subst\ 1848}#)
                                               #{marks\ 1849}#)
                                             (if (if (eq? (vector-ref
                                                            #{symnames\ 1850}#
                                                            #{i\ 1854}#)
                                                          #{sym\ 1847}#)
                                                   (#{same-marks?\ 1318}#
                                                     #{marks\ 1849}#
                                                     (vector-ref
                                                       (#{ribcage-marks\ 1307}#
                                                         #{ribcage\ 1851}#)
                                                       #{i\ 1854}#))
                                                   #f)
                                               (values
                                                 (vector-ref
                                                   (#{ribcage-labels\ 1308}#
                                                     #{ribcage\ 1851}#)
                                                   #{i\ 1854}#)
                                                 #{marks\ 1849}#)
                                               (#{f\ 1853}#
                                                 (#{fx+\ 1254}#
                                                   #{i\ 1854}#
                                                   1)))))))
                                (#{f\ 1853}# 0)))))
                        (#{search-list-rib\ 1840}#
                          (lambda (#{sym\ 1855}#
                                   #{subst\ 1856}#
                                   #{marks\ 1857}#
                                   #{symnames\ 1858}#
                                   #{ribcage\ 1859}#)
                            (letrec ((#{f\ 1860}#
                                       (lambda (#{symnames\ 1861}# #{i\ 1862}#)
                                         (if (null? #{symnames\ 1861}#)
                                           (#{search\ 1839}#
                                             #{sym\ 1855}#
                                             (cdr #{subst\ 1856}#)
                                             #{marks\ 1857}#)
                                           (if (if (eq? (car #{symnames\ 1861}#)
                                                        #{sym\ 1855}#)
                                                 (#{same-marks?\ 1318}#
                                                   #{marks\ 1857}#
                                                   (list-ref
                                                     (#{ribcage-marks\ 1307}#
                                                       #{ribcage\ 1859}#)
                                                     #{i\ 1862}#))
                                                 #f)
                                             (values
                                               (list-ref
                                                 (#{ribcage-labels\ 1308}#
                                                   #{ribcage\ 1859}#)
                                                 #{i\ 1862}#)
                                               #{marks\ 1857}#)
                                             (#{f\ 1860}#
                                               (cdr #{symnames\ 1861}#)
                                               (#{fx+\ 1254}#
                                                 #{i\ 1862}#
                                                 1)))))))
                              (#{f\ 1860}# #{symnames\ 1858}# 0))))
                        (#{search\ 1839}#
                          (lambda (#{sym\ 1863}#
                                   #{subst\ 1864}#
                                   #{marks\ 1865}#)
                            (if (null? #{subst\ 1864}#)
                              (values #f #{marks\ 1865}#)
                              (let ((#{fst\ 1866}# (car #{subst\ 1864}#)))
                                (if (eq? #{fst\ 1866}# (quote shift))
                                  (#{search\ 1839}#
                                    #{sym\ 1863}#
                                    (cdr #{subst\ 1864}#)
                                    (cdr #{marks\ 1865}#))
                                  (let ((#{symnames\ 1867}#
                                          (#{ribcage-symnames\ 1306}#
                                            #{fst\ 1866}#)))
                                    (if (vector? #{symnames\ 1867}#)
                                      (#{search-vector-rib\ 1841}#
                                        #{sym\ 1863}#
                                        #{subst\ 1864}#
                                        #{marks\ 1865}#
                                        #{symnames\ 1867}#
                                        #{fst\ 1866}#)
                                      (#{search-list-rib\ 1840}#
                                        #{sym\ 1863}#
                                        #{subst\ 1864}#
                                        #{marks\ 1865}#
                                        #{symnames\ 1867}#
                                        #{fst\ 1866}#)))))))))
                 (if (symbol? #{id\ 1837}#)
                   (let ((#{t\ 1868}#
                           (call-with-values
                             (lambda ()
                               (#{search\ 1839}#
                                 #{id\ 1837}#
                                 (#{wrap-subst\ 1301}# #{w\ 1838}#)
                                 (#{wrap-marks\ 1300}# #{w\ 1838}#)))
                             (lambda (#{x\ 1870}# . #{ignore\ 1869}#)
                               #{x\ 1870}#))))
                     (if #{t\ 1868}# #{t\ 1868}# #{id\ 1837}#))
                   (if (#{syntax-object?\ 1281}# #{id\ 1837}#)
                     (let ((#{id\ 1871}#
                             (#{syntax-object-expression\ 1282}# #{id\ 1837}#))
                           (#{w1\ 1872}#
                             (#{syntax-object-wrap\ 1283}# #{id\ 1837}#)))
                       (let ((#{marks\ 1873}#
                               (#{join-marks\ 1317}#
                                 (#{wrap-marks\ 1300}# #{w\ 1838}#)
                                 (#{wrap-marks\ 1300}# #{w1\ 1872}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 1839}#
                               #{id\ 1871}#
                               (#{wrap-subst\ 1301}# #{w\ 1838}#)
                               #{marks\ 1873}#))
                           (lambda (#{new-id\ 1874}# #{marks\ 1875}#)
                             (let ((#{t\ 1876}# #{new-id\ 1874}#))
                               (if #{t\ 1876}#
                                 #{t\ 1876}#
                                 (let ((#{t\ 1877}#
                                         (call-with-values
                                           (lambda ()
                                             (#{search\ 1839}#
                                               #{id\ 1871}#
                                               (#{wrap-subst\ 1301}#
                                                 #{w1\ 1872}#)
                                               #{marks\ 1875}#))
                                           (lambda (#{x\ 1879}#
                                                    .
                                                    #{ignore\ 1878}#)
                                             #{x\ 1879}#))))
                                   (if #{t\ 1877}#
                                     #{t\ 1877}#
                                     #{id\ 1871}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 1837}#))))))
           (#{same-marks?\ 1318}#
             (lambda (#{x\ 1880}# #{y\ 1881}#)
               (let ((#{t\ 1882}# (eq? #{x\ 1880}# #{y\ 1881}#)))
                 (if #{t\ 1882}#
                   #{t\ 1882}#
                   (if (not (null? #{x\ 1880}#))
                     (if (not (null? #{y\ 1881}#))
                       (if (eq? (car #{x\ 1880}#) (car #{y\ 1881}#))
                         (#{same-marks?\ 1318}#
                           (cdr #{x\ 1880}#)
                           (cdr #{y\ 1881}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 1317}#
             (lambda (#{m1\ 1883}# #{m2\ 1884}#)
               (#{smart-append\ 1315}#
                 #{m1\ 1883}#
                 #{m2\ 1884}#)))
           (#{join-wraps\ 1316}#
             (lambda (#{w1\ 1885}# #{w2\ 1886}#)
               (let ((#{m1\ 1887}#
                       (#{wrap-marks\ 1300}# #{w1\ 1885}#))
                     (#{s1\ 1888}#
                       (#{wrap-subst\ 1301}# #{w1\ 1885}#)))
                 (if (null? #{m1\ 1887}#)
                   (if (null? #{s1\ 1888}#)
                     #{w2\ 1886}#
                     (#{make-wrap\ 1299}#
                       (#{wrap-marks\ 1300}# #{w2\ 1886}#)
                       (#{smart-append\ 1315}#
                         #{s1\ 1888}#
                         (#{wrap-subst\ 1301}# #{w2\ 1886}#))))
                   (#{make-wrap\ 1299}#
                     (#{smart-append\ 1315}#
                       #{m1\ 1887}#
                       (#{wrap-marks\ 1300}# #{w2\ 1886}#))
                     (#{smart-append\ 1315}#
                       #{s1\ 1888}#
                       (#{wrap-subst\ 1301}# #{w2\ 1886}#)))))))
           (#{smart-append\ 1315}#
             (lambda (#{m1\ 1889}# #{m2\ 1890}#)
               (if (null? #{m2\ 1890}#)
                 #{m1\ 1889}#
                 (append #{m1\ 1889}# #{m2\ 1890}#))))
           (#{make-binding-wrap\ 1314}#
             (lambda (#{ids\ 1891}# #{labels\ 1892}# #{w\ 1893}#)
               (if (null? #{ids\ 1891}#)
                 #{w\ 1893}#
                 (#{make-wrap\ 1299}#
                   (#{wrap-marks\ 1300}# #{w\ 1893}#)
                   (cons (let ((#{labelvec\ 1894}#
                                 (list->vector #{labels\ 1892}#)))
                           (let ((#{n\ 1895}#
                                   (vector-length #{labelvec\ 1894}#)))
                             (let ((#{symnamevec\ 1896}#
                                     (make-vector #{n\ 1895}#))
                                   (#{marksvec\ 1897}#
                                     (make-vector #{n\ 1895}#)))
                               (begin
                                 (letrec ((#{f\ 1898}#
                                            (lambda (#{ids\ 1899}# #{i\ 1900}#)
                                              (if (not (null? #{ids\ 1899}#))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{id-sym-name&marks\ 1298}#
                                                      (car #{ids\ 1899}#)
                                                      #{w\ 1893}#))
                                                  (lambda (#{symname\ 1901}#
                                                           #{marks\ 1902}#)
                                                    (begin
                                                      (vector-set!
                                                        #{symnamevec\ 1896}#
                                                        #{i\ 1900}#
                                                        #{symname\ 1901}#)
                                                      (vector-set!
                                                        #{marksvec\ 1897}#
                                                        #{i\ 1900}#
                                                        #{marks\ 1902}#)
                                                      (#{f\ 1898}#
                                                        (cdr #{ids\ 1899}#)
                                                        (#{fx+\ 1254}#
                                                          #{i\ 1900}#
                                                          1)))))))))
                                   (#{f\ 1898}# #{ids\ 1891}# 0))
                                 (#{make-ribcage\ 1304}#
                                   #{symnamevec\ 1896}#
                                   #{marksvec\ 1897}#
                                   #{labelvec\ 1894}#)))))
                         (#{wrap-subst\ 1301}# #{w\ 1893}#))))))
           (#{extend-ribcage!\ 1313}#
             (lambda (#{ribcage\ 1903}# #{id\ 1904}# #{label\ 1905}#)
               (begin
                 (#{set-ribcage-symnames!\ 1309}#
                   #{ribcage\ 1903}#
                   (cons (#{syntax-object-expression\ 1282}# #{id\ 1904}#)
                         (#{ribcage-symnames\ 1306}# #{ribcage\ 1903}#)))
                 (#{set-ribcage-marks!\ 1310}#
                   #{ribcage\ 1903}#
                   (cons (#{wrap-marks\ 1300}#
                           (#{syntax-object-wrap\ 1283}# #{id\ 1904}#))
                         (#{ribcage-marks\ 1307}# #{ribcage\ 1903}#)))
                 (#{set-ribcage-labels!\ 1311}#
                   #{ribcage\ 1903}#
                   (cons #{label\ 1905}#
                         (#{ribcage-labels\ 1308}# #{ribcage\ 1903}#))))))
           (#{anti-mark\ 1312}#
             (lambda (#{w\ 1906}#)
               (#{make-wrap\ 1299}#
                 (cons #f (#{wrap-marks\ 1300}# #{w\ 1906}#))
                 (cons 'shift
                       (#{wrap-subst\ 1301}# #{w\ 1906}#)))))
           (#{set-ribcage-labels!\ 1311}#
             (lambda (#{x\ 1907}# #{update\ 1908}#)
               (vector-set! #{x\ 1907}# 3 #{update\ 1908}#)))
           (#{set-ribcage-marks!\ 1310}#
             (lambda (#{x\ 1909}# #{update\ 1910}#)
               (vector-set! #{x\ 1909}# 2 #{update\ 1910}#)))
           (#{set-ribcage-symnames!\ 1309}#
             (lambda (#{x\ 1911}# #{update\ 1912}#)
               (vector-set! #{x\ 1911}# 1 #{update\ 1912}#)))
           (#{ribcage-labels\ 1308}#
             (lambda (#{x\ 1913}#) (vector-ref #{x\ 1913}# 3)))
           (#{ribcage-marks\ 1307}#
             (lambda (#{x\ 1914}#) (vector-ref #{x\ 1914}# 2)))
           (#{ribcage-symnames\ 1306}#
             (lambda (#{x\ 1915}#) (vector-ref #{x\ 1915}# 1)))
           (#{ribcage?\ 1305}#
             (lambda (#{x\ 1916}#)
               (if (vector? #{x\ 1916}#)
                 (if (= (vector-length #{x\ 1916}#) 4)
                   (eq? (vector-ref #{x\ 1916}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 1304}#
             (lambda (#{symnames\ 1917}#
                      #{marks\ 1918}#
                      #{labels\ 1919}#)
               (vector
                 'ribcage
                 #{symnames\ 1917}#
                 #{marks\ 1918}#
                 #{labels\ 1919}#)))
           (#{gen-labels\ 1303}#
             (lambda (#{ls\ 1920}#)
               (if (null? #{ls\ 1920}#)
                 '()
                 (cons (#{gen-label\ 1302}#)
                       (#{gen-labels\ 1303}# (cdr #{ls\ 1920}#))))))
           (#{gen-label\ 1302}# (lambda () (string #\i)))
           (#{wrap-subst\ 1301}# cdr)
           (#{wrap-marks\ 1300}# car)
           (#{make-wrap\ 1299}# cons)
           (#{id-sym-name&marks\ 1298}#
             (lambda (#{x\ 1921}# #{w\ 1922}#)
               (if (#{syntax-object?\ 1281}# #{x\ 1921}#)
                 (values
                   (#{syntax-object-expression\ 1282}# #{x\ 1921}#)
                   (#{join-marks\ 1317}#
                     (#{wrap-marks\ 1300}# #{w\ 1922}#)
                     (#{wrap-marks\ 1300}#
                       (#{syntax-object-wrap\ 1283}# #{x\ 1921}#))))
                 (values
                   #{x\ 1921}#
                   (#{wrap-marks\ 1300}# #{w\ 1922}#)))))
           (#{id?\ 1297}#
             (lambda (#{x\ 1923}#)
               (if (symbol? #{x\ 1923}#)
                 #t
                 (if (#{syntax-object?\ 1281}# #{x\ 1923}#)
                   (symbol?
                     (#{syntax-object-expression\ 1282}# #{x\ 1923}#))
                   #f))))
           (#{nonsymbol-id?\ 1296}#
             (lambda (#{x\ 1924}#)
               (if (#{syntax-object?\ 1281}# #{x\ 1924}#)
                 (symbol?
                   (#{syntax-object-expression\ 1282}# #{x\ 1924}#))
                 #f)))
           (#{global-extend\ 1295}#
             (lambda (#{type\ 1925}# #{sym\ 1926}# #{val\ 1927}#)
               (#{put-global-definition-hook\ 1260}#
                 #{sym\ 1926}#
                 #{type\ 1925}#
                 #{val\ 1927}#)))
           (#{lookup\ 1294}#
             (lambda (#{x\ 1928}# #{r\ 1929}# #{mod\ 1930}#)
               (let ((#{t\ 1931}# (assq #{x\ 1928}# #{r\ 1929}#)))
                 (if #{t\ 1931}#
                   (cdr #{t\ 1931}#)
                   (if (symbol? #{x\ 1928}#)
                     (let ((#{t\ 1932}#
                             (#{get-global-definition-hook\ 1261}#
                               #{x\ 1928}#
                               #{mod\ 1930}#)))
                       (if #{t\ 1932}# #{t\ 1932}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 1293}#
             (lambda (#{r\ 1933}#)
               (if (null? #{r\ 1933}#)
                 '()
                 (let ((#{a\ 1934}# (car #{r\ 1933}#)))
                   (if (eq? (cadr #{a\ 1934}#) (quote macro))
                     (cons #{a\ 1934}#
                           (#{macros-only-env\ 1293}# (cdr #{r\ 1933}#)))
                     (#{macros-only-env\ 1293}# (cdr #{r\ 1933}#)))))))
           (#{extend-var-env\ 1292}#
             (lambda (#{labels\ 1935}# #{vars\ 1936}# #{r\ 1937}#)
               (if (null? #{labels\ 1935}#)
                 #{r\ 1937}#
                 (#{extend-var-env\ 1292}#
                   (cdr #{labels\ 1935}#)
                   (cdr #{vars\ 1936}#)
                   (cons (cons (car #{labels\ 1935}#)
                               (cons (quote lexical) (car #{vars\ 1936}#)))
                         #{r\ 1937}#)))))
           (#{extend-env\ 1291}#
             (lambda (#{labels\ 1938}# #{bindings\ 1939}# #{r\ 1940}#)
               (if (null? #{labels\ 1938}#)
                 #{r\ 1940}#
                 (#{extend-env\ 1291}#
                   (cdr #{labels\ 1938}#)
                   (cdr #{bindings\ 1939}#)
                   (cons (cons (car #{labels\ 1938}#)
                               (car #{bindings\ 1939}#))
                         #{r\ 1940}#)))))
           (#{binding-value\ 1290}# cdr)
           (#{binding-type\ 1289}# car)
           (#{source-annotation\ 1288}#
             (lambda (#{x\ 1941}#)
               (if (#{syntax-object?\ 1281}# #{x\ 1941}#)
                 (#{source-annotation\ 1288}#
                   (#{syntax-object-expression\ 1282}# #{x\ 1941}#))
                 (if (pair? #{x\ 1941}#)
                   (let ((#{props\ 1942}# (source-properties #{x\ 1941}#)))
                     (if (pair? #{props\ 1942}#) #{props\ 1942}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 1287}#
             (lambda (#{x\ 1943}# #{update\ 1944}#)
               (vector-set! #{x\ 1943}# 3 #{update\ 1944}#)))
           (#{set-syntax-object-wrap!\ 1286}#
             (lambda (#{x\ 1945}# #{update\ 1946}#)
               (vector-set! #{x\ 1945}# 2 #{update\ 1946}#)))
           (#{set-syntax-object-expression!\ 1285}#
             (lambda (#{x\ 1947}# #{update\ 1948}#)
               (vector-set! #{x\ 1947}# 1 #{update\ 1948}#)))
           (#{syntax-object-module\ 1284}#
             (lambda (#{x\ 1949}#) (vector-ref #{x\ 1949}# 3)))
           (#{syntax-object-wrap\ 1283}#
             (lambda (#{x\ 1950}#) (vector-ref #{x\ 1950}# 2)))
           (#{syntax-object-expression\ 1282}#
             (lambda (#{x\ 1951}#) (vector-ref #{x\ 1951}# 1)))
           (#{syntax-object?\ 1281}#
             (lambda (#{x\ 1952}#)
               (if (vector? #{x\ 1952}#)
                 (if (= (vector-length #{x\ 1952}#) 4)
                   (eq? (vector-ref #{x\ 1952}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 1280}#
             (lambda (#{expression\ 1953}#
                      #{wrap\ 1954}#
                      #{module\ 1955}#)
               (vector
                 'syntax-object
                 #{expression\ 1953}#
                 #{wrap\ 1954}#
                 #{module\ 1955}#)))
           (#{build-letrec\ 1279}#
             (lambda (#{src\ 1956}#
                      #{ids\ 1957}#
                      #{vars\ 1958}#
                      #{val-exps\ 1959}#
                      #{body-exp\ 1960}#)
               (if (null? #{vars\ 1958}#)
                 #{body-exp\ 1960}#
                 (let ((#{atom-key\ 1961}# (fluid-ref #{*mode*\ 1253}#)))
                   (if (memv #{atom-key\ 1961}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 1271}#
                         #{ids\ 1957}#
                         #{val-exps\ 1959}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 1956}#
                        #{ids\ 1957}#
                        #{vars\ 1958}#
                        #{val-exps\ 1959}#
                        #{body-exp\ 1960}#))
                     (#{decorate-source\ 1262}#
                       (list 'letrec
                             (map list #{vars\ 1958}# #{val-exps\ 1959}#)
                             #{body-exp\ 1960}#)
                       #{src\ 1956}#))))))
           (#{build-named-let\ 1278}#
             (lambda (#{src\ 1962}#
                      #{ids\ 1963}#
                      #{vars\ 1964}#
                      #{val-exps\ 1965}#
                      #{body-exp\ 1966}#)
               (let ((#{f\ 1967}# (car #{vars\ 1964}#))
                     (#{f-name\ 1968}# (car #{ids\ 1963}#))
                     (#{vars\ 1969}# (cdr #{vars\ 1964}#))
                     (#{ids\ 1970}# (cdr #{ids\ 1963}#)))
                 (let ((#{atom-key\ 1971}# (fluid-ref #{*mode*\ 1253}#)))
                   (if (memv #{atom-key\ 1971}# (quote (c)))
                     (let ((#{proc\ 1972}#
                             (#{build-lambda\ 1273}#
                               #{src\ 1962}#
                               #{ids\ 1970}#
                               #{vars\ 1969}#
                               #f
                               #{body-exp\ 1966}#)))
                       (begin
                         (#{maybe-name-value!\ 1271}#
                           #{f-name\ 1968}#
                           #{proc\ 1972}#)
                         (for-each
                           #{maybe-name-value!\ 1271}#
                           #{ids\ 1970}#
                           #{val-exps\ 1965}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 1962}#
                          (list #{f-name\ 1968}#)
                          (list #{f\ 1967}#)
                          (list #{proc\ 1972}#)
                          (#{build-application\ 1264}#
                            #{src\ 1962}#
                            (#{build-lexical-reference\ 1266}#
                              'fun
                              #{src\ 1962}#
                              #{f-name\ 1968}#
                              #{f\ 1967}#)
                            #{val-exps\ 1965}#))))
                     (#{decorate-source\ 1262}#
                       (list 'let
                             #{f\ 1967}#
                             (map list #{vars\ 1969}# #{val-exps\ 1965}#)
                             #{body-exp\ 1966}#)
                       #{src\ 1962}#))))))
           (#{build-let\ 1277}#
             (lambda (#{src\ 1973}#
                      #{ids\ 1974}#
                      #{vars\ 1975}#
                      #{val-exps\ 1976}#
                      #{body-exp\ 1977}#)
               (if (null? #{vars\ 1975}#)
                 #{body-exp\ 1977}#
                 (let ((#{atom-key\ 1978}# (fluid-ref #{*mode*\ 1253}#)))
                   (if (memv #{atom-key\ 1978}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 1271}#
                         #{ids\ 1974}#
                         #{val-exps\ 1976}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 1973}#
                        #{ids\ 1974}#
                        #{vars\ 1975}#
                        #{val-exps\ 1976}#
                        #{body-exp\ 1977}#))
                     (#{decorate-source\ 1262}#
                       (list 'let
                             (map list #{vars\ 1975}# #{val-exps\ 1976}#)
                             #{body-exp\ 1977}#)
                       #{src\ 1973}#))))))
           (#{build-sequence\ 1276}#
             (lambda (#{src\ 1979}# #{exps\ 1980}#)
               (if (null? (cdr #{exps\ 1980}#))
                 (car #{exps\ 1980}#)
                 (let ((#{atom-key\ 1981}# (fluid-ref #{*mode*\ 1253}#)))
                   (if (memv #{atom-key\ 1981}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 1979}#
                      #{exps\ 1980}#)
                     (#{decorate-source\ 1262}#
                       (cons (quote begin) #{exps\ 1980}#)
                       #{src\ 1979}#))))))
           (#{build-data\ 1275}#
             (lambda (#{src\ 1982}# #{exp\ 1983}#)
               (let ((#{atom-key\ 1984}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 1984}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 1982}#
                    #{exp\ 1983}#)
                   (#{decorate-source\ 1262}#
                     (if (if (self-evaluating? #{exp\ 1983}#)
                           (not (vector? #{exp\ 1983}#))
                           #f)
                       #{exp\ 1983}#
                       (list (quote quote) #{exp\ 1983}#))
                     #{src\ 1982}#)))))
           (#{build-primref\ 1274}#
             (lambda (#{src\ 1985}# #{name\ 1986}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 1987}# (fluid-ref #{*mode*\ 1253}#)))
                   (if (memv #{atom-key\ 1987}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 1985}#
                      #{name\ 1986}#)
                     (#{decorate-source\ 1262}#
                       #{name\ 1986}#
                       #{src\ 1985}#)))
                 (let ((#{atom-key\ 1988}# (fluid-ref #{*mode*\ 1253}#)))
                   (if (memv #{atom-key\ 1988}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 1985}#
                      '(guile)
                      #{name\ 1986}#
                      #f)
                     (#{decorate-source\ 1262}#
                       (list (quote @@) (quote (guile)) #{name\ 1986}#)
                       #{src\ 1985}#))))))
           (#{build-lambda\ 1273}#
             (lambda (#{src\ 1989}#
                      #{ids\ 1990}#
                      #{vars\ 1991}#
                      #{docstring\ 1992}#
                      #{exp\ 1993}#)
               (let ((#{atom-key\ 1994}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 1994}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 1989}#
                    #{ids\ 1990}#
                    #{vars\ 1991}#
                    (if #{docstring\ 1992}#
                      (list (cons (quote documentation) #{docstring\ 1992}#))
                      '())
                    #{exp\ 1993}#)
                   (#{decorate-source\ 1262}#
                     (cons 'lambda
                           (cons #{vars\ 1991}#
                                 (append
                                   (if #{docstring\ 1992}#
                                     (list #{docstring\ 1992}#)
                                     '())
                                   (list #{exp\ 1993}#))))
                     #{src\ 1989}#)))))
           (#{build-global-definition\ 1272}#
             (lambda (#{source\ 1995}# #{var\ 1996}# #{exp\ 1997}#)
               (let ((#{atom-key\ 1998}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 1998}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 1271}#
                       #{var\ 1996}#
                       #{exp\ 1997}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 1995}#
                      #{var\ 1996}#
                      #{exp\ 1997}#))
                   (#{decorate-source\ 1262}#
                     (list (quote define) #{var\ 1996}# #{exp\ 1997}#)
                     #{source\ 1995}#)))))
           (#{maybe-name-value!\ 1271}#
             (lambda (#{name\ 1999}# #{val\ 2000}#)
               (if ((@ (language tree-il) lambda?) #{val\ 2000}#)
                 (let ((#{meta\ 2001}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 2000}#)))
                   (if (not (assq (quote name) #{meta\ 2001}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 2000}#
                      (acons 'name
                             #{name\ 1999}#
                             #{meta\ 2001}#)))))))
           (#{build-global-assignment\ 1270}#
             (lambda (#{source\ 2002}#
                      #{var\ 2003}#
                      #{exp\ 2004}#
                      #{mod\ 2005}#)
               (#{analyze-variable\ 1268}#
                 #{mod\ 2005}#
                 #{var\ 2003}#
                 (lambda (#{mod\ 2006}# #{var\ 2007}# #{public?\ 2008}#)
                   (let ((#{atom-key\ 2009}# (fluid-ref #{*mode*\ 1253}#)))
                     (if (memv #{atom-key\ 2009}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 2002}#
                        #{mod\ 2006}#
                        #{var\ 2007}#
                        #{public?\ 2008}#
                        #{exp\ 2004}#)
                       (#{decorate-source\ 1262}#
                         (list 'set!
                               (list (if #{public?\ 2008}#
                                       '@
                                       '@@)
                                     #{mod\ 2006}#
                                     #{var\ 2007}#)
                               #{exp\ 2004}#)
                         #{source\ 2002}#))))
                 (lambda (#{var\ 2010}#)
                   (let ((#{atom-key\ 2011}# (fluid-ref #{*mode*\ 1253}#)))
                     (if (memv #{atom-key\ 2011}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 2002}#
                        #{var\ 2010}#
                        #{exp\ 2004}#)
                       (#{decorate-source\ 1262}#
                         (list (quote set!) #{var\ 2010}# #{exp\ 2004}#)
                         #{source\ 2002}#)))))))
           (#{build-global-reference\ 1269}#
             (lambda (#{source\ 2012}# #{var\ 2013}# #{mod\ 2014}#)
               (#{analyze-variable\ 1268}#
                 #{mod\ 2014}#
                 #{var\ 2013}#
                 (lambda (#{mod\ 2015}# #{var\ 2016}# #{public?\ 2017}#)
                   (let ((#{atom-key\ 2018}# (fluid-ref #{*mode*\ 1253}#)))
                     (if (memv #{atom-key\ 2018}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 2012}#
                        #{mod\ 2015}#
                        #{var\ 2016}#
                        #{public?\ 2017}#)
                       (#{decorate-source\ 1262}#
                         (list (if #{public?\ 2017}# (quote @) (quote @@))
                               #{mod\ 2015}#
                               #{var\ 2016}#)
                         #{source\ 2012}#))))
                 (lambda (#{var\ 2019}#)
                   (let ((#{atom-key\ 2020}# (fluid-ref #{*mode*\ 1253}#)))
                     (if (memv #{atom-key\ 2020}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 2012}#
                        #{var\ 2019}#)
                       (#{decorate-source\ 1262}#
                         #{var\ 2019}#
                         #{source\ 2012}#)))))))
           (#{analyze-variable\ 1268}#
             (lambda (#{mod\ 2021}#
                      #{var\ 2022}#
                      #{modref-cont\ 2023}#
                      #{bare-cont\ 2024}#)
               (if (not #{mod\ 2021}#)
                 (#{bare-cont\ 2024}# #{var\ 2022}#)
                 (let ((#{kind\ 2025}# (car #{mod\ 2021}#))
                       (#{mod\ 2026}# (cdr #{mod\ 2021}#)))
                   (if (memv #{kind\ 2025}# (quote (public)))
                     (#{modref-cont\ 2023}#
                       #{mod\ 2026}#
                       #{var\ 2022}#
                       #t)
                     (if (memv #{kind\ 2025}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 2026}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 2023}#
                           #{mod\ 2026}#
                           #{var\ 2022}#
                           #f)
                         (#{bare-cont\ 2024}# #{var\ 2022}#))
                       (if (memv #{kind\ 2025}# (quote (bare)))
                         (#{bare-cont\ 2024}# #{var\ 2022}#)
                         (if (memv #{kind\ 2025}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 2026}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 2026}#)
                                   #{var\ 2022}#)
                                 #f)
                             (#{modref-cont\ 2023}#
                               #{mod\ 2026}#
                               #{var\ 2022}#
                               #f)
                             (#{bare-cont\ 2024}# #{var\ 2022}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 2022}#
                             #{mod\ 2026}#)))))))))
           (#{build-lexical-assignment\ 1267}#
             (lambda (#{source\ 2027}#
                      #{name\ 2028}#
                      #{var\ 2029}#
                      #{exp\ 2030}#)
               (let ((#{atom-key\ 2031}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 2031}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 2027}#
                    #{name\ 2028}#
                    #{var\ 2029}#
                    #{exp\ 2030}#)
                   (#{decorate-source\ 1262}#
                     (list (quote set!) #{var\ 2029}# #{exp\ 2030}#)
                     #{source\ 2027}#)))))
           (#{build-lexical-reference\ 1266}#
             (lambda (#{type\ 2032}#
                      #{source\ 2033}#
                      #{name\ 2034}#
                      #{var\ 2035}#)
               (let ((#{atom-key\ 2036}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 2036}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 2033}#
                    #{name\ 2034}#
                    #{var\ 2035}#)
                   (#{decorate-source\ 1262}#
                     #{var\ 2035}#
                     #{source\ 2033}#)))))
           (#{build-conditional\ 1265}#
             (lambda (#{source\ 2037}#
                      #{test-exp\ 2038}#
                      #{then-exp\ 2039}#
                      #{else-exp\ 2040}#)
               (let ((#{atom-key\ 2041}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 2041}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 2037}#
                    #{test-exp\ 2038}#
                    #{then-exp\ 2039}#
                    #{else-exp\ 2040}#)
                   (#{decorate-source\ 1262}#
                     (if (equal? #{else-exp\ 2040}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 2038}#
                             #{then-exp\ 2039}#)
                       (list 'if
                             #{test-exp\ 2038}#
                             #{then-exp\ 2039}#
                             #{else-exp\ 2040}#))
                     #{source\ 2037}#)))))
           (#{build-application\ 1264}#
             (lambda (#{source\ 2042}#
                      #{fun-exp\ 2043}#
                      #{arg-exps\ 2044}#)
               (let ((#{atom-key\ 2045}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 2045}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 2042}#
                    #{fun-exp\ 2043}#
                    #{arg-exps\ 2044}#)
                   (#{decorate-source\ 1262}#
                     (cons #{fun-exp\ 2043}# #{arg-exps\ 2044}#)
                     #{source\ 2042}#)))))
           (#{build-void\ 1263}#
             (lambda (#{source\ 2046}#)
               (let ((#{atom-key\ 2047}# (fluid-ref #{*mode*\ 1253}#)))
                 (if (memv #{atom-key\ 2047}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 2046}#)
                   (#{decorate-source\ 1262}#
                     '(if #f #f)
                     #{source\ 2046}#)))))
           (#{decorate-source\ 1262}#
             (lambda (#{e\ 2048}# #{s\ 2049}#)
               (begin
                 (if (if (pair? #{e\ 2048}#) #{s\ 2049}# #f)
                   (set-source-properties! #{e\ 2048}# #{s\ 2049}#))
                 #{e\ 2048}#)))
           (#{get-global-definition-hook\ 1261}#
             (lambda (#{symbol\ 2050}# #{module\ 2051}#)
               (begin
                 (if (if (not #{module\ 2051}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 2050}#))
                 (let ((#{v\ 2052}#
                         (module-variable
                           (if #{module\ 2051}#
                             (resolve-module (cdr #{module\ 2051}#))
                             (current-module))
                           #{symbol\ 2050}#)))
                   (if #{v\ 2052}#
                     (if (variable-bound? #{v\ 2052}#)
                       (let ((#{val\ 2053}# (variable-ref #{v\ 2052}#)))
                         (if (macro? #{val\ 2053}#)
                           (if (syncase-macro-type #{val\ 2053}#)
                             (cons (syncase-macro-type #{val\ 2053}#)
                                   (syncase-macro-binding #{val\ 2053}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 1260}#
             (lambda (#{symbol\ 2054}# #{type\ 2055}# #{val\ 2056}#)
               (let ((#{existing\ 2057}#
                       (let ((#{v\ 2058}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 2054}#)))
                         (if #{v\ 2058}#
                           (if (variable-bound? #{v\ 2058}#)
                             (let ((#{val\ 2059}# (variable-ref #{v\ 2058}#)))
                               (if (macro? #{val\ 2059}#)
                                 (if (not (syncase-macro-type #{val\ 2059}#))
                                   #{val\ 2059}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 2054}#
                   (if #{existing\ 2057}#
                     (make-extended-syncase-macro
                       #{existing\ 2057}#
                       #{type\ 2055}#
                       #{val\ 2056}#)
                     (make-syncase-macro #{type\ 2055}# #{val\ 2056}#))))))
           (#{local-eval-hook\ 1259}#
             (lambda (#{x\ 2060}# #{mod\ 2061}#)
               (primitive-eval
                 (list #{noexpand\ 1252}#
                       (let ((#{atom-key\ 2062}# (fluid-ref #{*mode*\ 1253}#)))
                         (if (memv #{atom-key\ 2062}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 2060}#)
                           #{x\ 2060}#))))))
           (#{top-level-eval-hook\ 1258}#
             (lambda (#{x\ 2063}# #{mod\ 2064}#)
               (primitive-eval
                 (list #{noexpand\ 1252}#
                       (let ((#{atom-key\ 2065}# (fluid-ref #{*mode*\ 1253}#)))
                         (if (memv #{atom-key\ 2065}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 2063}#)
                           #{x\ 2063}#))))))
           (#{fx<\ 1257}# <)
           (#{fx=\ 1256}# =)
           (#{fx-\ 1255}# -)
           (#{fx+\ 1254}# +)
           (#{*mode*\ 1253}# (make-fluid))
           (#{noexpand\ 1252}# "noexpand"))
    (begin
      (#{global-extend\ 1295}#
        'local-syntax
        'letrec-syntax
        #t)
      (#{global-extend\ 1295}#
        'local-syntax
        'let-syntax
        #f)
      (#{global-extend\ 1295}#
        'core
        'fluid-let-syntax
        (lambda (#{e\ 2066}#
                 #{r\ 2067}#
                 #{w\ 2068}#
                 #{s\ 2069}#
                 #{mod\ 2070}#)
          ((lambda (#{tmp\ 2071}#)
             ((lambda (#{tmp\ 2072}#)
                (if (if #{tmp\ 2072}#
                      (apply (lambda (#{_\ 2073}#
                                      #{var\ 2074}#
                                      #{val\ 2075}#
                                      #{e1\ 2076}#
                                      #{e2\ 2077}#)
                               (#{valid-bound-ids?\ 1322}# #{var\ 2074}#))
                             #{tmp\ 2072}#)
                      #f)
                  (apply (lambda (#{_\ 2079}#
                                  #{var\ 2080}#
                                  #{val\ 2081}#
                                  #{e1\ 2082}#
                                  #{e2\ 2083}#)
                           (let ((#{names\ 2084}#
                                   (map (lambda (#{x\ 2085}#)
                                          (#{id-var-name\ 1319}#
                                            #{x\ 2085}#
                                            #{w\ 2068}#))
                                        #{var\ 2080}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 2087}# #{n\ 2088}#)
                                   (let ((#{atom-key\ 2089}#
                                           (#{binding-type\ 1289}#
                                             (#{lookup\ 1294}#
                                               #{n\ 2088}#
                                               #{r\ 2067}#
                                               #{mod\ 2070}#))))
                                     (if (memv #{atom-key\ 2089}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 2066}#
                                         (#{source-wrap\ 1326}#
                                           #{id\ 2087}#
                                           #{w\ 2068}#
                                           #{s\ 2069}#
                                           #{mod\ 2070}#)))))
                                 #{var\ 2080}#
                                 #{names\ 2084}#)
                               (#{chi-body\ 1337}#
                                 (cons #{e1\ 2082}# #{e2\ 2083}#)
                                 (#{source-wrap\ 1326}#
                                   #{e\ 2066}#
                                   #{w\ 2068}#
                                   #{s\ 2069}#
                                   #{mod\ 2070}#)
                                 (#{extend-env\ 1291}#
                                   #{names\ 2084}#
                                   (let ((#{trans-r\ 2092}#
                                           (#{macros-only-env\ 1293}#
                                             #{r\ 2067}#)))
                                     (map (lambda (#{x\ 2093}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 1340}#
                                                    (#{chi\ 1333}#
                                                      #{x\ 2093}#
                                                      #{trans-r\ 2092}#
                                                      #{w\ 2068}#
                                                      #{mod\ 2070}#)
                                                    #{mod\ 2070}#)))
                                          #{val\ 2081}#))
                                   #{r\ 2067}#)
                                 #{w\ 2068}#
                                 #{mod\ 2070}#))))
                         #{tmp\ 2072}#)
                  ((lambda (#{_\ 2095}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 1326}#
                         #{e\ 2066}#
                         #{w\ 2068}#
                         #{s\ 2069}#
                         #{mod\ 2070}#)))
                   #{tmp\ 2071}#)))
              ($sc-dispatch
                #{tmp\ 2071}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 2066}#)))
      (#{global-extend\ 1295}#
        'core
        'quote
        (lambda (#{e\ 2096}#
                 #{r\ 2097}#
                 #{w\ 2098}#
                 #{s\ 2099}#
                 #{mod\ 2100}#)
          ((lambda (#{tmp\ 2101}#)
             ((lambda (#{tmp\ 2102}#)
                (if #{tmp\ 2102}#
                  (apply (lambda (#{_\ 2103}# #{e\ 2104}#)
                           (#{build-data\ 1275}#
                             #{s\ 2099}#
                             (#{strip\ 1343}# #{e\ 2104}# #{w\ 2098}#)))
                         #{tmp\ 2102}#)
                  ((lambda (#{_\ 2105}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 1326}#
                         #{e\ 2096}#
                         #{w\ 2098}#
                         #{s\ 2099}#
                         #{mod\ 2100}#)))
                   #{tmp\ 2101}#)))
              ($sc-dispatch #{tmp\ 2101}# (quote (any any)))))
           #{e\ 2096}#)))
      (#{global-extend\ 1295}#
        'core
        'syntax
        (letrec ((#{regen\ 2113}#
                   (lambda (#{x\ 2114}#)
                     (let ((#{atom-key\ 2115}# (car #{x\ 2114}#)))
                       (if (memv #{atom-key\ 2115}# (quote (ref)))
                         (#{build-lexical-reference\ 1266}#
                           'value
                           #f
                           (cadr #{x\ 2114}#)
                           (cadr #{x\ 2114}#))
                         (if (memv #{atom-key\ 2115}# (quote (primitive)))
                           (#{build-primref\ 1274}# #f (cadr #{x\ 2114}#))
                           (if (memv #{atom-key\ 2115}# (quote (quote)))
                             (#{build-data\ 1275}# #f (cadr #{x\ 2114}#))
                             (if (memv #{atom-key\ 2115}# (quote (lambda)))
                               (#{build-lambda\ 1273}#
                                 #f
                                 (cadr #{x\ 2114}#)
                                 (cadr #{x\ 2114}#)
                                 #f
                                 (#{regen\ 2113}# (caddr #{x\ 2114}#)))
                               (#{build-application\ 1264}#
                                 #f
                                 (#{build-primref\ 1274}# #f (car #{x\ 2114}#))
                                 (map #{regen\ 2113}#
                                      (cdr #{x\ 2114}#))))))))))
                 (#{gen-vector\ 2112}#
                   (lambda (#{x\ 2116}#)
                     (if (eq? (car #{x\ 2116}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 2116}#))
                       (if (eq? (car #{x\ 2116}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 2116}#)))
                         (list (quote list->vector) #{x\ 2116}#)))))
                 (#{gen-append\ 2111}#
                   (lambda (#{x\ 2117}# #{y\ 2118}#)
                     (if (equal? #{y\ 2118}# (quote (quote ())))
                       #{x\ 2117}#
                       (list (quote append) #{x\ 2117}# #{y\ 2118}#))))
                 (#{gen-cons\ 2110}#
                   (lambda (#{x\ 2119}# #{y\ 2120}#)
                     (let ((#{atom-key\ 2121}# (car #{y\ 2120}#)))
                       (if (memv #{atom-key\ 2121}# (quote (quote)))
                         (if (eq? (car #{x\ 2119}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 2119}#) (cadr #{y\ 2120}#)))
                           (if (eq? (cadr #{y\ 2120}#) (quote ()))
                             (list (quote list) #{x\ 2119}#)
                             (list (quote cons) #{x\ 2119}# #{y\ 2120}#)))
                         (if (memv #{atom-key\ 2121}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 2119}# (cdr #{y\ 2120}#)))
                           (list (quote cons) #{x\ 2119}# #{y\ 2120}#))))))
                 (#{gen-map\ 2109}#
                   (lambda (#{e\ 2122}# #{map-env\ 2123}#)
                     (let ((#{formals\ 2124}# (map cdr #{map-env\ 2123}#))
                           (#{actuals\ 2125}#
                             (map (lambda (#{x\ 2126}#)
                                    (list (quote ref) (car #{x\ 2126}#)))
                                  #{map-env\ 2123}#)))
                       (if (eq? (car #{e\ 2122}#) (quote ref))
                         (car #{actuals\ 2125}#)
                         (if (and-map
                               (lambda (#{x\ 2127}#)
                                 (if (eq? (car #{x\ 2127}#) (quote ref))
                                   (memq (cadr #{x\ 2127}#) #{formals\ 2124}#)
                                   #f))
                               (cdr #{e\ 2122}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 2122}#))
                                       (map (let ((#{r\ 2128}#
                                                    (map cons
                                                         #{formals\ 2124}#
                                                         #{actuals\ 2125}#)))
                                              (lambda (#{x\ 2129}#)
                                                (cdr (assq (cadr #{x\ 2129}#)
                                                           #{r\ 2128}#))))
                                            (cdr #{e\ 2122}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 2124}#
                                             #{e\ 2122}#)
                                       #{actuals\ 2125}#)))))))
                 (#{gen-mappend\ 2108}#
                   (lambda (#{e\ 2130}# #{map-env\ 2131}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 2109}# #{e\ 2130}# #{map-env\ 2131}#))))
                 (#{gen-ref\ 2107}#
                   (lambda (#{src\ 2132}#
                            #{var\ 2133}#
                            #{level\ 2134}#
                            #{maps\ 2135}#)
                     (if (#{fx=\ 1256}# #{level\ 2134}# 0)
                       (values #{var\ 2133}# #{maps\ 2135}#)
                       (if (null? #{maps\ 2135}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 2132}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 2107}#
                               #{src\ 2132}#
                               #{var\ 2133}#
                               (#{fx-\ 1255}# #{level\ 2134}# 1)
                               (cdr #{maps\ 2135}#)))
                           (lambda (#{outer-var\ 2136}# #{outer-maps\ 2137}#)
                             (let ((#{b\ 2138}#
                                     (assq #{outer-var\ 2136}#
                                           (car #{maps\ 2135}#))))
                               (if #{b\ 2138}#
                                 (values (cdr #{b\ 2138}#) #{maps\ 2135}#)
                                 (let ((#{inner-var\ 2139}#
                                         (#{gen-var\ 1344}# (quote tmp))))
                                   (values
                                     #{inner-var\ 2139}#
                                     (cons (cons (cons #{outer-var\ 2136}#
                                                       #{inner-var\ 2139}#)
                                                 (car #{maps\ 2135}#))
                                           #{outer-maps\ 2137}#)))))))))))
                 (#{gen-syntax\ 2106}#
                   (lambda (#{src\ 2140}#
                            #{e\ 2141}#
                            #{r\ 2142}#
                            #{maps\ 2143}#
                            #{ellipsis?\ 2144}#
                            #{mod\ 2145}#)
                     (if (#{id?\ 1297}# #{e\ 2141}#)
                       (let ((#{label\ 2146}#
                               (#{id-var-name\ 1319}#
                                 #{e\ 2141}#
                                 '(()))))
                         (let ((#{b\ 2147}#
                                 (#{lookup\ 1294}#
                                   #{label\ 2146}#
                                   #{r\ 2142}#
                                   #{mod\ 2145}#)))
                           (if (eq? (#{binding-type\ 1289}# #{b\ 2147}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 2148}#
                                         (#{binding-value\ 1290}#
                                           #{b\ 2147}#)))
                                   (#{gen-ref\ 2107}#
                                     #{src\ 2140}#
                                     (car #{var.lev\ 2148}#)
                                     (cdr #{var.lev\ 2148}#)
                                     #{maps\ 2143}#)))
                               (lambda (#{var\ 2149}# #{maps\ 2150}#)
                                 (values
                                   (list (quote ref) #{var\ 2149}#)
                                   #{maps\ 2150}#)))
                             (if (#{ellipsis?\ 2144}# #{e\ 2141}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 2140}#)
                               (values
                                 (list (quote quote) #{e\ 2141}#)
                                 #{maps\ 2143}#)))))
                       ((lambda (#{tmp\ 2151}#)
                          ((lambda (#{tmp\ 2152}#)
                             (if (if #{tmp\ 2152}#
                                   (apply (lambda (#{dots\ 2153}# #{e\ 2154}#)
                                            (#{ellipsis?\ 2144}#
                                              #{dots\ 2153}#))
                                          #{tmp\ 2152}#)
                                   #f)
                               (apply (lambda (#{dots\ 2155}# #{e\ 2156}#)
                                        (#{gen-syntax\ 2106}#
                                          #{src\ 2140}#
                                          #{e\ 2156}#
                                          #{r\ 2142}#
                                          #{maps\ 2143}#
                                          (lambda (#{x\ 2157}#) #f)
                                          #{mod\ 2145}#))
                                      #{tmp\ 2152}#)
                               ((lambda (#{tmp\ 2158}#)
                                  (if (if #{tmp\ 2158}#
                                        (apply (lambda (#{x\ 2159}#
                                                        #{dots\ 2160}#
                                                        #{y\ 2161}#)
                                                 (#{ellipsis?\ 2144}#
                                                   #{dots\ 2160}#))
                                               #{tmp\ 2158}#)
                                        #f)
                                    (apply (lambda (#{x\ 2162}#
                                                    #{dots\ 2163}#
                                                    #{y\ 2164}#)
                                             (letrec ((#{f\ 2165}#
                                                        (lambda (#{y\ 2166}#
                                                                 #{k\ 2167}#)
                                                          ((lambda (#{tmp\ 2171}#)
                                                             ((lambda (#{tmp\ 2172}#)
                                                                (if (if #{tmp\ 2172}#
                                                                      (apply (lambda (#{dots\ 2173}#
                                                                                      #{y\ 2174}#)
                                                                               (#{ellipsis?\ 2144}#
                                                                                 #{dots\ 2173}#))
                                                                             #{tmp\ 2172}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 2175}#
                                                                                  #{y\ 2176}#)
                                                                           (#{f\ 2165}#
                                                                             #{y\ 2176}#
                                                                             (lambda (#{maps\ 2177}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 2167}#
                                                                                     (cons '()
                                                                                           #{maps\ 2177}#)))
                                                                                 (lambda (#{x\ 2178}#
                                                                                          #{maps\ 2179}#)
                                                                                   (if (null? (car #{maps\ 2179}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 2140}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 2108}#
                                                                                         #{x\ 2178}#
                                                                                         (car #{maps\ 2179}#))
                                                                                       (cdr #{maps\ 2179}#))))))))
                                                                         #{tmp\ 2172}#)
                                                                  ((lambda (#{_\ 2180}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 2106}#
                                                                           #{src\ 2140}#
                                                                           #{y\ 2166}#
                                                                           #{r\ 2142}#
                                                                           #{maps\ 2143}#
                                                                           #{ellipsis?\ 2144}#
                                                                           #{mod\ 2145}#))
                                                                       (lambda (#{y\ 2181}#
                                                                                #{maps\ 2182}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 2167}#
                                                                               #{maps\ 2182}#))
                                                                           (lambda (#{x\ 2183}#
                                                                                    #{maps\ 2184}#)
                                                                             (values
                                                                               (#{gen-append\ 2111}#
                                                                                 #{x\ 2183}#
                                                                                 #{y\ 2181}#)
                                                                               #{maps\ 2184}#))))))
                                                                   #{tmp\ 2171}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 2171}#
                                                                '(any . any))))
                                                           #{y\ 2166}#))))
                                               (#{f\ 2165}#
                                                 #{y\ 2164}#
                                                 (lambda (#{maps\ 2168}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 2106}#
                                                         #{src\ 2140}#
                                                         #{x\ 2162}#
                                                         #{r\ 2142}#
                                                         (cons '()
                                                               #{maps\ 2168}#)
                                                         #{ellipsis?\ 2144}#
                                                         #{mod\ 2145}#))
                                                     (lambda (#{x\ 2169}#
                                                              #{maps\ 2170}#)
                                                       (if (null? (car #{maps\ 2170}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 2140}#)
                                                         (values
                                                           (#{gen-map\ 2109}#
                                                             #{x\ 2169}#
                                                             (car #{maps\ 2170}#))
                                                           (cdr #{maps\ 2170}#)))))))))
                                           #{tmp\ 2158}#)
                                    ((lambda (#{tmp\ 2185}#)
                                       (if #{tmp\ 2185}#
                                         (apply (lambda (#{x\ 2186}#
                                                         #{y\ 2187}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 2106}#
                                                        #{src\ 2140}#
                                                        #{x\ 2186}#
                                                        #{r\ 2142}#
                                                        #{maps\ 2143}#
                                                        #{ellipsis?\ 2144}#
                                                        #{mod\ 2145}#))
                                                    (lambda (#{x\ 2188}#
                                                             #{maps\ 2189}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 2106}#
                                                            #{src\ 2140}#
                                                            #{y\ 2187}#
                                                            #{r\ 2142}#
                                                            #{maps\ 2189}#
                                                            #{ellipsis?\ 2144}#
                                                            #{mod\ 2145}#))
                                                        (lambda (#{y\ 2190}#
                                                                 #{maps\ 2191}#)
                                                          (values
                                                            (#{gen-cons\ 2110}#
                                                              #{x\ 2188}#
                                                              #{y\ 2190}#)
                                                            #{maps\ 2191}#))))))
                                                #{tmp\ 2185}#)
                                         ((lambda (#{tmp\ 2192}#)
                                            (if #{tmp\ 2192}#
                                              (apply (lambda (#{e1\ 2193}#
                                                              #{e2\ 2194}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 2106}#
                                                             #{src\ 2140}#
                                                             (cons #{e1\ 2193}#
                                                                   #{e2\ 2194}#)
                                                             #{r\ 2142}#
                                                             #{maps\ 2143}#
                                                             #{ellipsis?\ 2144}#
                                                             #{mod\ 2145}#))
                                                         (lambda (#{e\ 2196}#
                                                                  #{maps\ 2197}#)
                                                           (values
                                                             (#{gen-vector\ 2112}#
                                                               #{e\ 2196}#)
                                                             #{maps\ 2197}#))))
                                                     #{tmp\ 2192}#)
                                              ((lambda (#{_\ 2198}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 2141}#)
                                                   #{maps\ 2143}#))
                                               #{tmp\ 2151}#)))
                                          ($sc-dispatch
                                            #{tmp\ 2151}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 2151}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 2151}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 2151}# (quote (any any)))))
                        #{e\ 2141}#)))))
          (lambda (#{e\ 2199}#
                   #{r\ 2200}#
                   #{w\ 2201}#
                   #{s\ 2202}#
                   #{mod\ 2203}#)
            (let ((#{e\ 2204}#
                    (#{source-wrap\ 1326}#
                      #{e\ 2199}#
                      #{w\ 2201}#
                      #{s\ 2202}#
                      #{mod\ 2203}#)))
              ((lambda (#{tmp\ 2205}#)
                 ((lambda (#{tmp\ 2206}#)
                    (if #{tmp\ 2206}#
                      (apply (lambda (#{_\ 2207}# #{x\ 2208}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 2106}#
                                     #{e\ 2204}#
                                     #{x\ 2208}#
                                     #{r\ 2200}#
                                     '()
                                     #{ellipsis?\ 1342}#
                                     #{mod\ 2203}#))
                                 (lambda (#{e\ 2209}# #{maps\ 2210}#)
                                   (#{regen\ 2113}# #{e\ 2209}#))))
                             #{tmp\ 2206}#)
                      ((lambda (#{_\ 2211}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 2204}#))
                       #{tmp\ 2205}#)))
                  ($sc-dispatch #{tmp\ 2205}# (quote (any any)))))
               #{e\ 2204}#)))))
      (#{global-extend\ 1295}#
        'core
        'lambda
        (lambda (#{e\ 2212}#
                 #{r\ 2213}#
                 #{w\ 2214}#
                 #{s\ 2215}#
                 #{mod\ 2216}#)
          ((lambda (#{tmp\ 2217}#)
             ((lambda (#{tmp\ 2218}#)
                (if #{tmp\ 2218}#
                  (apply (lambda (#{_\ 2219}# #{c\ 2220}#)
                           (#{chi-lambda-clause\ 1338}#
                             (#{source-wrap\ 1326}#
                               #{e\ 2212}#
                               #{w\ 2214}#
                               #{s\ 2215}#
                               #{mod\ 2216}#)
                             #f
                             #{c\ 2220}#
                             #{r\ 2213}#
                             #{w\ 2214}#
                             #{mod\ 2216}#
                             (lambda (#{names\ 2221}#
                                      #{vars\ 2222}#
                                      #{docstring\ 2223}#
                                      #{body\ 2224}#)
                               (#{build-lambda\ 1273}#
                                 #{s\ 2215}#
                                 #{names\ 2221}#
                                 #{vars\ 2222}#
                                 #{docstring\ 2223}#
                                 #{body\ 2224}#))))
                         #{tmp\ 2218}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 2217}#)))
              ($sc-dispatch #{tmp\ 2217}# (quote (any . any)))))
           #{e\ 2212}#)))
      (#{global-extend\ 1295}#
        'core
        'let
        (letrec ((#{chi-let\ 2225}#
                   (lambda (#{e\ 2226}#
                            #{r\ 2227}#
                            #{w\ 2228}#
                            #{s\ 2229}#
                            #{mod\ 2230}#
                            #{constructor\ 2231}#
                            #{ids\ 2232}#
                            #{vals\ 2233}#
                            #{exps\ 2234}#)
                     (if (not (#{valid-bound-ids?\ 1322}# #{ids\ 2232}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 2226}#)
                       (let ((#{labels\ 2235}#
                               (#{gen-labels\ 1303}# #{ids\ 2232}#))
                             (#{new-vars\ 2236}#
                               (map #{gen-var\ 1344}# #{ids\ 2232}#)))
                         (let ((#{nw\ 2237}#
                                 (#{make-binding-wrap\ 1314}#
                                   #{ids\ 2232}#
                                   #{labels\ 2235}#
                                   #{w\ 2228}#))
                               (#{nr\ 2238}#
                                 (#{extend-var-env\ 1292}#
                                   #{labels\ 2235}#
                                   #{new-vars\ 2236}#
                                   #{r\ 2227}#)))
                           (#{constructor\ 2231}#
                             #{s\ 2229}#
                             (map syntax->datum #{ids\ 2232}#)
                             #{new-vars\ 2236}#
                             (map (lambda (#{x\ 2239}#)
                                    (#{chi\ 1333}#
                                      #{x\ 2239}#
                                      #{r\ 2227}#
                                      #{w\ 2228}#
                                      #{mod\ 2230}#))
                                  #{vals\ 2233}#)
                             (#{chi-body\ 1337}#
                               #{exps\ 2234}#
                               (#{source-wrap\ 1326}#
                                 #{e\ 2226}#
                                 #{nw\ 2237}#
                                 #{s\ 2229}#
                                 #{mod\ 2230}#)
                               #{nr\ 2238}#
                               #{nw\ 2237}#
                               #{mod\ 2230}#))))))))
          (lambda (#{e\ 2240}#
                   #{r\ 2241}#
                   #{w\ 2242}#
                   #{s\ 2243}#
                   #{mod\ 2244}#)
            ((lambda (#{tmp\ 2245}#)
               ((lambda (#{tmp\ 2246}#)
                  (if (if #{tmp\ 2246}#
                        (apply (lambda (#{_\ 2247}#
                                        #{id\ 2248}#
                                        #{val\ 2249}#
                                        #{e1\ 2250}#
                                        #{e2\ 2251}#)
                                 (and-map #{id?\ 1297}# #{id\ 2248}#))
                               #{tmp\ 2246}#)
                        #f)
                    (apply (lambda (#{_\ 2253}#
                                    #{id\ 2254}#
                                    #{val\ 2255}#
                                    #{e1\ 2256}#
                                    #{e2\ 2257}#)
                             (#{chi-let\ 2225}#
                               #{e\ 2240}#
                               #{r\ 2241}#
                               #{w\ 2242}#
                               #{s\ 2243}#
                               #{mod\ 2244}#
                               #{build-let\ 1277}#
                               #{id\ 2254}#
                               #{val\ 2255}#
                               (cons #{e1\ 2256}# #{e2\ 2257}#)))
                           #{tmp\ 2246}#)
                    ((lambda (#{tmp\ 2261}#)
                       (if (if #{tmp\ 2261}#
                             (apply (lambda (#{_\ 2262}#
                                             #{f\ 2263}#
                                             #{id\ 2264}#
                                             #{val\ 2265}#
                                             #{e1\ 2266}#
                                             #{e2\ 2267}#)
                                      (if (#{id?\ 1297}# #{f\ 2263}#)
                                        (and-map #{id?\ 1297}# #{id\ 2264}#)
                                        #f))
                                    #{tmp\ 2261}#)
                             #f)
                         (apply (lambda (#{_\ 2269}#
                                         #{f\ 2270}#
                                         #{id\ 2271}#
                                         #{val\ 2272}#
                                         #{e1\ 2273}#
                                         #{e2\ 2274}#)
                                  (#{chi-let\ 2225}#
                                    #{e\ 2240}#
                                    #{r\ 2241}#
                                    #{w\ 2242}#
                                    #{s\ 2243}#
                                    #{mod\ 2244}#
                                    #{build-named-let\ 1278}#
                                    (cons #{f\ 2270}# #{id\ 2271}#)
                                    #{val\ 2272}#
                                    (cons #{e1\ 2273}# #{e2\ 2274}#)))
                                #{tmp\ 2261}#)
                         ((lambda (#{_\ 2278}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 1326}#
                                #{e\ 2240}#
                                #{w\ 2242}#
                                #{s\ 2243}#
                                #{mod\ 2244}#)))
                          #{tmp\ 2245}#)))
                     ($sc-dispatch
                       #{tmp\ 2245}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 2245}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 2240}#))))
      (#{global-extend\ 1295}#
        'core
        'letrec
        (lambda (#{e\ 2279}#
                 #{r\ 2280}#
                 #{w\ 2281}#
                 #{s\ 2282}#
                 #{mod\ 2283}#)
          ((lambda (#{tmp\ 2284}#)
             ((lambda (#{tmp\ 2285}#)
                (if (if #{tmp\ 2285}#
                      (apply (lambda (#{_\ 2286}#
                                      #{id\ 2287}#
                                      #{val\ 2288}#
                                      #{e1\ 2289}#
                                      #{e2\ 2290}#)
                               (and-map #{id?\ 1297}# #{id\ 2287}#))
                             #{tmp\ 2285}#)
                      #f)
                  (apply (lambda (#{_\ 2292}#
                                  #{id\ 2293}#
                                  #{val\ 2294}#
                                  #{e1\ 2295}#
                                  #{e2\ 2296}#)
                           (let ((#{ids\ 2297}# #{id\ 2293}#))
                             (if (not (#{valid-bound-ids?\ 1322}#
                                        #{ids\ 2297}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 2279}#)
                               (let ((#{labels\ 2299}#
                                       (#{gen-labels\ 1303}# #{ids\ 2297}#))
                                     (#{new-vars\ 2300}#
                                       (map #{gen-var\ 1344}# #{ids\ 2297}#)))
                                 (let ((#{w\ 2301}#
                                         (#{make-binding-wrap\ 1314}#
                                           #{ids\ 2297}#
                                           #{labels\ 2299}#
                                           #{w\ 2281}#))
                                       (#{r\ 2302}#
                                         (#{extend-var-env\ 1292}#
                                           #{labels\ 2299}#
                                           #{new-vars\ 2300}#
                                           #{r\ 2280}#)))
                                   (#{build-letrec\ 1279}#
                                     #{s\ 2282}#
                                     (map syntax->datum #{ids\ 2297}#)
                                     #{new-vars\ 2300}#
                                     (map (lambda (#{x\ 2303}#)
                                            (#{chi\ 1333}#
                                              #{x\ 2303}#
                                              #{r\ 2302}#
                                              #{w\ 2301}#
                                              #{mod\ 2283}#))
                                          #{val\ 2294}#)
                                     (#{chi-body\ 1337}#
                                       (cons #{e1\ 2295}# #{e2\ 2296}#)
                                       (#{source-wrap\ 1326}#
                                         #{e\ 2279}#
                                         #{w\ 2301}#
                                         #{s\ 2282}#
                                         #{mod\ 2283}#)
                                       #{r\ 2302}#
                                       #{w\ 2301}#
                                       #{mod\ 2283}#)))))))
                         #{tmp\ 2285}#)
                  ((lambda (#{_\ 2306}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 1326}#
                         #{e\ 2279}#
                         #{w\ 2281}#
                         #{s\ 2282}#
                         #{mod\ 2283}#)))
                   #{tmp\ 2284}#)))
              ($sc-dispatch
                #{tmp\ 2284}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 2279}#)))
      (#{global-extend\ 1295}#
        'core
        'set!
        (lambda (#{e\ 2307}#
                 #{r\ 2308}#
                 #{w\ 2309}#
                 #{s\ 2310}#
                 #{mod\ 2311}#)
          ((lambda (#{tmp\ 2312}#)
             ((lambda (#{tmp\ 2313}#)
                (if (if #{tmp\ 2313}#
                      (apply (lambda (#{_\ 2314}# #{id\ 2315}# #{val\ 2316}#)
                               (#{id?\ 1297}# #{id\ 2315}#))
                             #{tmp\ 2313}#)
                      #f)
                  (apply (lambda (#{_\ 2317}# #{id\ 2318}# #{val\ 2319}#)
                           (let ((#{val\ 2320}#
                                   (#{chi\ 1333}#
                                     #{val\ 2319}#
                                     #{r\ 2308}#
                                     #{w\ 2309}#
                                     #{mod\ 2311}#))
                                 (#{n\ 2321}#
                                   (#{id-var-name\ 1319}#
                                     #{id\ 2318}#
                                     #{w\ 2309}#)))
                             (let ((#{b\ 2322}#
                                     (#{lookup\ 1294}#
                                       #{n\ 2321}#
                                       #{r\ 2308}#
                                       #{mod\ 2311}#)))
                               (let ((#{atom-key\ 2323}#
                                       (#{binding-type\ 1289}# #{b\ 2322}#)))
                                 (if (memv #{atom-key\ 2323}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 1267}#
                                     #{s\ 2310}#
                                     (syntax->datum #{id\ 2318}#)
                                     (#{binding-value\ 1290}# #{b\ 2322}#)
                                     #{val\ 2320}#)
                                   (if (memv #{atom-key\ 2323}#
                                             '(global))
                                     (#{build-global-assignment\ 1270}#
                                       #{s\ 2310}#
                                       #{n\ 2321}#
                                       #{val\ 2320}#
                                       #{mod\ 2311}#)
                                     (if (memv #{atom-key\ 2323}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 1325}#
                                           #{id\ 2318}#
                                           #{w\ 2309}#
                                           #{mod\ 2311}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 1326}#
                                           #{e\ 2307}#
                                           #{w\ 2309}#
                                           #{s\ 2310}#
                                           #{mod\ 2311}#)))))))))
                         #{tmp\ 2313}#)
                  ((lambda (#{tmp\ 2324}#)
                     (if #{tmp\ 2324}#
                       (apply (lambda (#{_\ 2325}#
                                       #{head\ 2326}#
                                       #{tail\ 2327}#
                                       #{val\ 2328}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 1331}#
                                      #{head\ 2326}#
                                      #{r\ 2308}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 2311}#
                                      #t))
                                  (lambda (#{type\ 2329}#
                                           #{value\ 2330}#
                                           #{ee\ 2331}#
                                           #{ww\ 2332}#
                                           #{ss\ 2333}#
                                           #{modmod\ 2334}#)
                                    (if (memv #{type\ 2329}#
                                              '(module-ref))
                                      (let ((#{val\ 2335}#
                                              (#{chi\ 1333}#
                                                #{val\ 2328}#
                                                #{r\ 2308}#
                                                #{w\ 2309}#
                                                #{mod\ 2311}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 2330}#
                                              (cons #{head\ 2326}#
                                                    #{tail\ 2327}#)))
                                          (lambda (#{id\ 2337}# #{mod\ 2338}#)
                                            (#{build-global-assignment\ 1270}#
                                              #{s\ 2310}#
                                              #{id\ 2337}#
                                              #{val\ 2335}#
                                              #{mod\ 2338}#))))
                                      (#{build-application\ 1264}#
                                        #{s\ 2310}#
                                        (#{chi\ 1333}#
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
                                                #{head\ 2326}#)
                                          #{r\ 2308}#
                                          #{w\ 2309}#
                                          #{mod\ 2311}#)
                                        (map (lambda (#{e\ 2339}#)
                                               (#{chi\ 1333}#
                                                 #{e\ 2339}#
                                                 #{r\ 2308}#
                                                 #{w\ 2309}#
                                                 #{mod\ 2311}#))
                                             (append
                                               #{tail\ 2327}#
                                               (list #{val\ 2328}#))))))))
                              #{tmp\ 2324}#)
                       ((lambda (#{_\ 2341}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 1326}#
                              #{e\ 2307}#
                              #{w\ 2309}#
                              #{s\ 2310}#
                              #{mod\ 2311}#)))
                        #{tmp\ 2312}#)))
                   ($sc-dispatch
                     #{tmp\ 2312}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 2312}#
                '(any any any))))
           #{e\ 2307}#)))
      (#{global-extend\ 1295}#
        'module-ref
        '@
        (lambda (#{e\ 2342}#)
          ((lambda (#{tmp\ 2343}#)
             ((lambda (#{tmp\ 2344}#)
                (if (if #{tmp\ 2344}#
                      (apply (lambda (#{_\ 2345}# #{mod\ 2346}# #{id\ 2347}#)
                               (if (and-map #{id?\ 1297}# #{mod\ 2346}#)
                                 (#{id?\ 1297}# #{id\ 2347}#)
                                 #f))
                             #{tmp\ 2344}#)
                      #f)
                  (apply (lambda (#{_\ 2349}# #{mod\ 2350}# #{id\ 2351}#)
                           (values
                             (syntax->datum #{id\ 2351}#)
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
                                     #{mod\ 2350}#))))
                         #{tmp\ 2344}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 2343}#)))
              ($sc-dispatch
                #{tmp\ 2343}#
                '(any each-any any))))
           #{e\ 2342}#)))
      (#{global-extend\ 1295}#
        'module-ref
        '@@
        (lambda (#{e\ 2353}#)
          ((lambda (#{tmp\ 2354}#)
             ((lambda (#{tmp\ 2355}#)
                (if (if #{tmp\ 2355}#
                      (apply (lambda (#{_\ 2356}# #{mod\ 2357}# #{id\ 2358}#)
                               (if (and-map #{id?\ 1297}# #{mod\ 2357}#)
                                 (#{id?\ 1297}# #{id\ 2358}#)
                                 #f))
                             #{tmp\ 2355}#)
                      #f)
                  (apply (lambda (#{_\ 2360}# #{mod\ 2361}# #{id\ 2362}#)
                           (values
                             (syntax->datum #{id\ 2362}#)
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
                                     #{mod\ 2361}#))))
                         #{tmp\ 2355}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 2354}#)))
              ($sc-dispatch
                #{tmp\ 2354}#
                '(any each-any any))))
           #{e\ 2353}#)))
      (#{global-extend\ 1295}#
        'core
        'if
        (lambda (#{e\ 2364}#
                 #{r\ 2365}#
                 #{w\ 2366}#
                 #{s\ 2367}#
                 #{mod\ 2368}#)
          ((lambda (#{tmp\ 2369}#)
             ((lambda (#{tmp\ 2370}#)
                (if #{tmp\ 2370}#
                  (apply (lambda (#{_\ 2371}# #{test\ 2372}# #{then\ 2373}#)
                           (#{build-conditional\ 1265}#
                             #{s\ 2367}#
                             (#{chi\ 1333}#
                               #{test\ 2372}#
                               #{r\ 2365}#
                               #{w\ 2366}#
                               #{mod\ 2368}#)
                             (#{chi\ 1333}#
                               #{then\ 2373}#
                               #{r\ 2365}#
                               #{w\ 2366}#
                               #{mod\ 2368}#)
                             (#{build-void\ 1263}# #f)))
                         #{tmp\ 2370}#)
                  ((lambda (#{tmp\ 2374}#)
                     (if #{tmp\ 2374}#
                       (apply (lambda (#{_\ 2375}#
                                       #{test\ 2376}#
                                       #{then\ 2377}#
                                       #{else\ 2378}#)
                                (#{build-conditional\ 1265}#
                                  #{s\ 2367}#
                                  (#{chi\ 1333}#
                                    #{test\ 2376}#
                                    #{r\ 2365}#
                                    #{w\ 2366}#
                                    #{mod\ 2368}#)
                                  (#{chi\ 1333}#
                                    #{then\ 2377}#
                                    #{r\ 2365}#
                                    #{w\ 2366}#
                                    #{mod\ 2368}#)
                                  (#{chi\ 1333}#
                                    #{else\ 2378}#
                                    #{r\ 2365}#
                                    #{w\ 2366}#
                                    #{mod\ 2368}#)))
                              #{tmp\ 2374}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 2369}#)))
                   ($sc-dispatch
                     #{tmp\ 2369}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 2369}#
                '(any any any))))
           #{e\ 2364}#)))
      (#{global-extend\ 1295}#
        'begin
        'begin
        '())
      (#{global-extend\ 1295}#
        'define
        'define
        '())
      (#{global-extend\ 1295}#
        'define-syntax
        'define-syntax
        '())
      (#{global-extend\ 1295}#
        'eval-when
        'eval-when
        '())
      (#{global-extend\ 1295}#
        'core
        'syntax-case
        (letrec ((#{gen-syntax-case\ 2382}#
                   (lambda (#{x\ 2383}#
                            #{keys\ 2384}#
                            #{clauses\ 2385}#
                            #{r\ 2386}#
                            #{mod\ 2387}#)
                     (if (null? #{clauses\ 2385}#)
                       (#{build-application\ 1264}#
                         #f
                         (#{build-primref\ 1274}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 1275}# #f #f)
                               (#{build-data\ 1275}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 2383}#))
                       ((lambda (#{tmp\ 2388}#)
                          ((lambda (#{tmp\ 2389}#)
                             (if #{tmp\ 2389}#
                               (apply (lambda (#{pat\ 2390}# #{exp\ 2391}#)
                                        (if (if (#{id?\ 1297}# #{pat\ 2390}#)
                                              (and-map
                                                (lambda (#{x\ 2392}#)
                                                  (not (#{free-id=?\ 1320}#
                                                         #{pat\ 2390}#
                                                         #{x\ 2392}#)))
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
                                                      #{keys\ 2384}#))
                                              #f)
                                          (let ((#{labels\ 2393}#
                                                  (list (#{gen-label\ 1302}#)))
                                                (#{var\ 2394}#
                                                  (#{gen-var\ 1344}#
                                                    #{pat\ 2390}#)))
                                            (#{build-application\ 1264}#
                                              #f
                                              (#{build-lambda\ 1273}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 2390}#))
                                                (list #{var\ 2394}#)
                                                #f
                                                (#{chi\ 1333}#
                                                  #{exp\ 2391}#
                                                  (#{extend-env\ 1291}#
                                                    #{labels\ 2393}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 2394}#
                                                                      0)))
                                                    #{r\ 2386}#)
                                                  (#{make-binding-wrap\ 1314}#
                                                    (list #{pat\ 2390}#)
                                                    #{labels\ 2393}#
                                                    '(()))
                                                  #{mod\ 2387}#))
                                              (list #{x\ 2383}#)))
                                          (#{gen-clause\ 2381}#
                                            #{x\ 2383}#
                                            #{keys\ 2384}#
                                            (cdr #{clauses\ 2385}#)
                                            #{r\ 2386}#
                                            #{pat\ 2390}#
                                            #t
                                            #{exp\ 2391}#
                                            #{mod\ 2387}#)))
                                      #{tmp\ 2389}#)
                               ((lambda (#{tmp\ 2395}#)
                                  (if #{tmp\ 2395}#
                                    (apply (lambda (#{pat\ 2396}#
                                                    #{fender\ 2397}#
                                                    #{exp\ 2398}#)
                                             (#{gen-clause\ 2381}#
                                               #{x\ 2383}#
                                               #{keys\ 2384}#
                                               (cdr #{clauses\ 2385}#)
                                               #{r\ 2386}#
                                               #{pat\ 2396}#
                                               #{fender\ 2397}#
                                               #{exp\ 2398}#
                                               #{mod\ 2387}#))
                                           #{tmp\ 2395}#)
                                    ((lambda (#{_\ 2399}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 2385}#)))
                                     #{tmp\ 2388}#)))
                                ($sc-dispatch
                                  #{tmp\ 2388}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 2388}# (quote (any any)))))
                        (car #{clauses\ 2385}#)))))
                 (#{gen-clause\ 2381}#
                   (lambda (#{x\ 2400}#
                            #{keys\ 2401}#
                            #{clauses\ 2402}#
                            #{r\ 2403}#
                            #{pat\ 2404}#
                            #{fender\ 2405}#
                            #{exp\ 2406}#
                            #{mod\ 2407}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 2379}#
                           #{pat\ 2404}#
                           #{keys\ 2401}#))
                       (lambda (#{p\ 2408}# #{pvars\ 2409}#)
                         (if (not (#{distinct-bound-ids?\ 1323}#
                                    (map car #{pvars\ 2409}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 2404}#)
                           (if (not (and-map
                                      (lambda (#{x\ 2410}#)
                                        (not (#{ellipsis?\ 1342}#
                                               (car #{x\ 2410}#))))
                                      #{pvars\ 2409}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 2404}#)
                             (let ((#{y\ 2411}#
                                     (#{gen-var\ 1344}# (quote tmp))))
                               (#{build-application\ 1264}#
                                 #f
                                 (#{build-lambda\ 1273}#
                                   #f
                                   (list (quote tmp))
                                   (list #{y\ 2411}#)
                                   #f
                                   (let ((#{y\ 2412}#
                                           (#{build-lexical-reference\ 1266}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 2411}#)))
                                     (#{build-conditional\ 1265}#
                                       #f
                                       ((lambda (#{tmp\ 2413}#)
                                          ((lambda (#{tmp\ 2414}#)
                                             (if #{tmp\ 2414}#
                                               (apply (lambda () #{y\ 2412}#)
                                                      #{tmp\ 2414}#)
                                               ((lambda (#{_\ 2415}#)
                                                  (#{build-conditional\ 1265}#
                                                    #f
                                                    #{y\ 2412}#
                                                    (#{build-dispatch-call\ 2380}#
                                                      #{pvars\ 2409}#
                                                      #{fender\ 2405}#
                                                      #{y\ 2412}#
                                                      #{r\ 2403}#
                                                      #{mod\ 2407}#)
                                                    (#{build-data\ 1275}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 2413}#)))
                                           ($sc-dispatch
                                             #{tmp\ 2413}#
                                             '#(atom #t))))
                                        #{fender\ 2405}#)
                                       (#{build-dispatch-call\ 2380}#
                                         #{pvars\ 2409}#
                                         #{exp\ 2406}#
                                         #{y\ 2412}#
                                         #{r\ 2403}#
                                         #{mod\ 2407}#)
                                       (#{gen-syntax-case\ 2382}#
                                         #{x\ 2400}#
                                         #{keys\ 2401}#
                                         #{clauses\ 2402}#
                                         #{r\ 2403}#
                                         #{mod\ 2407}#))))
                                 (list (if (eq? #{p\ 2408}# (quote any))
                                         (#{build-application\ 1264}#
                                           #f
                                           (#{build-primref\ 1274}#
                                             #f
                                             'list)
                                           (list #{x\ 2400}#))
                                         (#{build-application\ 1264}#
                                           #f
                                           (#{build-primref\ 1274}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 2400}#
                                                 (#{build-data\ 1275}#
                                                   #f
                                                   #{p\ 2408}#)))))))))))))
                 (#{build-dispatch-call\ 2380}#
                   (lambda (#{pvars\ 2416}#
                            #{exp\ 2417}#
                            #{y\ 2418}#
                            #{r\ 2419}#
                            #{mod\ 2420}#)
                     (let ((#{ids\ 2421}# (map car #{pvars\ 2416}#))
                           (#{levels\ 2422}# (map cdr #{pvars\ 2416}#)))
                       (let ((#{labels\ 2423}#
                               (#{gen-labels\ 1303}# #{ids\ 2421}#))
                             (#{new-vars\ 2424}#
                               (map #{gen-var\ 1344}# #{ids\ 2421}#)))
                         (#{build-application\ 1264}#
                           #f
                           (#{build-primref\ 1274}# #f (quote apply))
                           (list (#{build-lambda\ 1273}#
                                   #f
                                   (map syntax->datum #{ids\ 2421}#)
                                   #{new-vars\ 2424}#
                                   #f
                                   (#{chi\ 1333}#
                                     #{exp\ 2417}#
                                     (#{extend-env\ 1291}#
                                       #{labels\ 2423}#
                                       (map (lambda (#{var\ 2425}#
                                                     #{level\ 2426}#)
                                              (cons 'syntax
                                                    (cons #{var\ 2425}#
                                                          #{level\ 2426}#)))
                                            #{new-vars\ 2424}#
                                            (map cdr #{pvars\ 2416}#))
                                       #{r\ 2419}#)
                                     (#{make-binding-wrap\ 1314}#
                                       #{ids\ 2421}#
                                       #{labels\ 2423}#
                                       '(()))
                                     #{mod\ 2420}#))
                                 #{y\ 2418}#))))))
                 (#{convert-pattern\ 2379}#
                   (lambda (#{pattern\ 2427}# #{keys\ 2428}#)
                     (letrec ((#{cvt\ 2429}#
                                (lambda (#{p\ 2430}# #{n\ 2431}# #{ids\ 2432}#)
                                  (if (#{id?\ 1297}# #{p\ 2430}#)
                                    (if (#{bound-id-member?\ 1324}#
                                          #{p\ 2430}#
                                          #{keys\ 2428}#)
                                      (values
                                        (vector (quote free-id) #{p\ 2430}#)
                                        #{ids\ 2432}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 2430}# #{n\ 2431}#)
                                              #{ids\ 2432}#)))
                                    ((lambda (#{tmp\ 2433}#)
                                       ((lambda (#{tmp\ 2434}#)
                                          (if (if #{tmp\ 2434}#
                                                (apply (lambda (#{x\ 2435}#
                                                                #{dots\ 2436}#)
                                                         (#{ellipsis?\ 1342}#
                                                           #{dots\ 2436}#))
                                                       #{tmp\ 2434}#)
                                                #f)
                                            (apply (lambda (#{x\ 2437}#
                                                            #{dots\ 2438}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 2429}#
                                                           #{x\ 2437}#
                                                           (#{fx+\ 1254}#
                                                             #{n\ 2431}#
                                                             1)
                                                           #{ids\ 2432}#))
                                                       (lambda (#{p\ 2439}#
                                                                #{ids\ 2440}#)
                                                         (values
                                                           (if (eq? #{p\ 2439}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 2439}#))
                                                           #{ids\ 2440}#))))
                                                   #{tmp\ 2434}#)
                                            ((lambda (#{tmp\ 2441}#)
                                               (if #{tmp\ 2441}#
                                                 (apply (lambda (#{x\ 2442}#
                                                                 #{y\ 2443}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 2429}#
                                                                #{y\ 2443}#
                                                                #{n\ 2431}#
                                                                #{ids\ 2432}#))
                                                            (lambda (#{y\ 2444}#
                                                                     #{ids\ 2445}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 2429}#
                                                                    #{x\ 2442}#
                                                                    #{n\ 2431}#
                                                                    #{ids\ 2445}#))
                                                                (lambda (#{x\ 2446}#
                                                                         #{ids\ 2447}#)
                                                                  (values
                                                                    (cons #{x\ 2446}#
                                                                          #{y\ 2444}#)
                                                                    #{ids\ 2447}#))))))
                                                        #{tmp\ 2441}#)
                                                 ((lambda (#{tmp\ 2448}#)
                                                    (if #{tmp\ 2448}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 2432}#))
                                                             #{tmp\ 2448}#)
                                                      ((lambda (#{tmp\ 2449}#)
                                                         (if #{tmp\ 2449}#
                                                           (apply (lambda (#{x\ 2450}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 2429}#
                                                                          #{x\ 2450}#
                                                                          #{n\ 2431}#
                                                                          #{ids\ 2432}#))
                                                                      (lambda (#{p\ 2452}#
                                                                               #{ids\ 2453}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 2452}#)
                                                                          #{ids\ 2453}#))))
                                                                  #{tmp\ 2449}#)
                                                           ((lambda (#{x\ 2454}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 1343}#
                                                                    #{p\ 2430}#
                                                                    '(())))
                                                                #{ids\ 2432}#))
                                                            #{tmp\ 2433}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 2433}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 2433}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 2433}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 2433}#
                                          '(any any))))
                                     #{p\ 2430}#)))))
                       (#{cvt\ 2429}# #{pattern\ 2427}# 0 (quote ()))))))
          (lambda (#{e\ 2455}#
                   #{r\ 2456}#
                   #{w\ 2457}#
                   #{s\ 2458}#
                   #{mod\ 2459}#)
            (let ((#{e\ 2460}#
                    (#{source-wrap\ 1326}#
                      #{e\ 2455}#
                      #{w\ 2457}#
                      #{s\ 2458}#
                      #{mod\ 2459}#)))
              ((lambda (#{tmp\ 2461}#)
                 ((lambda (#{tmp\ 2462}#)
                    (if #{tmp\ 2462}#
                      (apply (lambda (#{_\ 2463}#
                                      #{val\ 2464}#
                                      #{key\ 2465}#
                                      #{m\ 2466}#)
                               (if (and-map
                                     (lambda (#{x\ 2467}#)
                                       (if (#{id?\ 1297}# #{x\ 2467}#)
                                         (not (#{ellipsis?\ 1342}#
                                                #{x\ 2467}#))
                                         #f))
                                     #{key\ 2465}#)
                                 (let ((#{x\ 2469}#
                                         (#{gen-var\ 1344}# (quote tmp))))
                                   (#{build-application\ 1264}#
                                     #{s\ 2458}#
                                     (#{build-lambda\ 1273}#
                                       #f
                                       (list (quote tmp))
                                       (list #{x\ 2469}#)
                                       #f
                                       (#{gen-syntax-case\ 2382}#
                                         (#{build-lexical-reference\ 1266}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 2469}#)
                                         #{key\ 2465}#
                                         #{m\ 2466}#
                                         #{r\ 2456}#
                                         #{mod\ 2459}#))
                                     (list (#{chi\ 1333}#
                                             #{val\ 2464}#
                                             #{r\ 2456}#
                                             '(())
                                             #{mod\ 2459}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 2460}#)))
                             #{tmp\ 2462}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 2461}#)))
                  ($sc-dispatch
                    #{tmp\ 2461}#
                    '(any any each-any . each-any))))
               #{e\ 2460}#)))))
      (set! sc-expand
        (lambda (#{x\ 2473}# . #{rest\ 2472}#)
          (if (if (pair? #{x\ 2473}#)
                (equal? (car #{x\ 2473}#) #{noexpand\ 1252}#)
                #f)
            (cadr #{x\ 2473}#)
            (let ((#{m\ 2474}#
                    (if (null? #{rest\ 2472}#)
                      'e
                      (car #{rest\ 2472}#)))
                  (#{esew\ 2475}#
                    (if (let ((#{t\ 2476}# (null? #{rest\ 2472}#)))
                          (if #{t\ 2476}#
                            #{t\ 2476}#
                            (null? (cdr #{rest\ 2472}#))))
                      '(eval)
                      (cadr #{rest\ 2472}#))))
              (with-fluid*
                #{*mode*\ 1253}#
                #{m\ 2474}#
                (lambda ()
                  (#{chi-top\ 1332}#
                    #{x\ 2473}#
                    '()
                    '((top))
                    #{m\ 2474}#
                    #{esew\ 2475}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 2477}#)
          (#{nonsymbol-id?\ 1296}# #{x\ 2477}#)))
      (set! datum->syntax
        (lambda (#{id\ 2478}# #{datum\ 2479}#)
          (#{make-syntax-object\ 1280}#
            #{datum\ 2479}#
            (#{syntax-object-wrap\ 1283}# #{id\ 2478}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 2480}#)
          (#{strip\ 1343}# #{x\ 2480}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 2481}#)
          (begin
            (let ((#{x\ 2482}# #{ls\ 2481}#))
              (if (not (list? #{x\ 2482}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 2482}#)))
            (map (lambda (#{x\ 2483}#)
                   (#{wrap\ 1325}# (gensym) (quote ((top))) #f))
                 #{ls\ 2481}#))))
      (set! free-identifier=?
        (lambda (#{x\ 2484}# #{y\ 2485}#)
          (begin
            (let ((#{x\ 2486}# #{x\ 2484}#))
              (if (not (#{nonsymbol-id?\ 1296}# #{x\ 2486}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 2486}#)))
            (let ((#{x\ 2487}# #{y\ 2485}#))
              (if (not (#{nonsymbol-id?\ 1296}# #{x\ 2487}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 2487}#)))
            (#{free-id=?\ 1320}# #{x\ 2484}# #{y\ 2485}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 2488}# #{y\ 2489}#)
          (begin
            (let ((#{x\ 2490}# #{x\ 2488}#))
              (if (not (#{nonsymbol-id?\ 1296}# #{x\ 2490}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 2490}#)))
            (let ((#{x\ 2491}# #{y\ 2489}#))
              (if (not (#{nonsymbol-id?\ 1296}# #{x\ 2491}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 2491}#)))
            (#{bound-id=?\ 1321}# #{x\ 2488}# #{y\ 2489}#))))
      (set! syntax-violation
        (lambda (#{who\ 2495}#
                 #{message\ 2494}#
                 #{form\ 2493}#
                 .
                 #{subform\ 2492}#)
          (begin
            (let ((#{x\ 2496}# #{who\ 2495}#))
              (if (not ((lambda (#{x\ 2497}#)
                          (let ((#{t\ 2498}# (not #{x\ 2497}#)))
                            (if #{t\ 2498}#
                              #{t\ 2498}#
                              (let ((#{t\ 2499}# (string? #{x\ 2497}#)))
                                (if #{t\ 2499}#
                                  #{t\ 2499}#
                                  (symbol? #{x\ 2497}#))))))
                        #{x\ 2496}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 2496}#)))
            (let ((#{x\ 2500}# #{message\ 2494}#))
              (if (not (string? #{x\ 2500}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 2500}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 2495}# "~a: " "")
                "~a "
                (if (null? #{subform\ 2492}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 2501}#
                      (cons #{message\ 2494}#
                            (map (lambda (#{x\ 2502}#)
                                   (#{strip\ 1343}# #{x\ 2502}# (quote (()))))
                                 (append
                                   #{subform\ 2492}#
                                   (list #{form\ 2493}#))))))
                (if #{who\ 2495}#
                  (cons #{who\ 2495}# #{tail\ 2501}#)
                  #{tail\ 2501}#))
              #f))))
      (letrec ((#{match\ 2507}#
                 (lambda (#{e\ 2508}#
                          #{p\ 2509}#
                          #{w\ 2510}#
                          #{r\ 2511}#
                          #{mod\ 2512}#)
                   (if (not #{r\ 2511}#)
                     #f
                     (if (eq? #{p\ 2509}# (quote any))
                       (cons (#{wrap\ 1325}#
                               #{e\ 2508}#
                               #{w\ 2510}#
                               #{mod\ 2512}#)
                             #{r\ 2511}#)
                       (if (#{syntax-object?\ 1281}# #{e\ 2508}#)
                         (#{match*\ 2506}#
                           (#{syntax-object-expression\ 1282}# #{e\ 2508}#)
                           #{p\ 2509}#
                           (#{join-wraps\ 1316}#
                             #{w\ 2510}#
                             (#{syntax-object-wrap\ 1283}# #{e\ 2508}#))
                           #{r\ 2511}#
                           (#{syntax-object-module\ 1284}# #{e\ 2508}#))
                         (#{match*\ 2506}#
                           #{e\ 2508}#
                           #{p\ 2509}#
                           #{w\ 2510}#
                           #{r\ 2511}#
                           #{mod\ 2512}#))))))
               (#{match*\ 2506}#
                 (lambda (#{e\ 2513}#
                          #{p\ 2514}#
                          #{w\ 2515}#
                          #{r\ 2516}#
                          #{mod\ 2517}#)
                   (if (null? #{p\ 2514}#)
                     (if (null? #{e\ 2513}#) #{r\ 2516}# #f)
                     (if (pair? #{p\ 2514}#)
                       (if (pair? #{e\ 2513}#)
                         (#{match\ 2507}#
                           (car #{e\ 2513}#)
                           (car #{p\ 2514}#)
                           #{w\ 2515}#
                           (#{match\ 2507}#
                             (cdr #{e\ 2513}#)
                             (cdr #{p\ 2514}#)
                             #{w\ 2515}#
                             #{r\ 2516}#
                             #{mod\ 2517}#)
                           #{mod\ 2517}#)
                         #f)
                       (if (eq? #{p\ 2514}# (quote each-any))
                         (let ((#{l\ 2518}#
                                 (#{match-each-any\ 2504}#
                                   #{e\ 2513}#
                                   #{w\ 2515}#
                                   #{mod\ 2517}#)))
                           (if #{l\ 2518}#
                             (cons #{l\ 2518}# #{r\ 2516}#)
                             #f))
                         (let ((#{atom-key\ 2519}# (vector-ref #{p\ 2514}# 0)))
                           (if (memv #{atom-key\ 2519}# (quote (each)))
                             (if (null? #{e\ 2513}#)
                               (#{match-empty\ 2505}#
                                 (vector-ref #{p\ 2514}# 1)
                                 #{r\ 2516}#)
                               (let ((#{l\ 2520}#
                                       (#{match-each\ 2503}#
                                         #{e\ 2513}#
                                         (vector-ref #{p\ 2514}# 1)
                                         #{w\ 2515}#
                                         #{mod\ 2517}#)))
                                 (if #{l\ 2520}#
                                   (letrec ((#{collect\ 2521}#
                                              (lambda (#{l\ 2522}#)
                                                (if (null? (car #{l\ 2522}#))
                                                  #{r\ 2516}#
                                                  (cons (map car #{l\ 2522}#)
                                                        (#{collect\ 2521}#
                                                          (map cdr
                                                               #{l\ 2522}#)))))))
                                     (#{collect\ 2521}# #{l\ 2520}#))
                                   #f)))
                             (if (memv #{atom-key\ 2519}# (quote (free-id)))
                               (if (#{id?\ 1297}# #{e\ 2513}#)
                                 (if (#{free-id=?\ 1320}#
                                       (#{wrap\ 1325}#
                                         #{e\ 2513}#
                                         #{w\ 2515}#
                                         #{mod\ 2517}#)
                                       (vector-ref #{p\ 2514}# 1))
                                   #{r\ 2516}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 2519}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 2514}# 1)
                                       (#{strip\ 1343}#
                                         #{e\ 2513}#
                                         #{w\ 2515}#))
                                   #{r\ 2516}#
                                   #f)
                                 (if (memv #{atom-key\ 2519}# (quote (vector)))
                                   (if (vector? #{e\ 2513}#)
                                     (#{match\ 2507}#
                                       (vector->list #{e\ 2513}#)
                                       (vector-ref #{p\ 2514}# 1)
                                       #{w\ 2515}#
                                       #{r\ 2516}#
                                       #{mod\ 2517}#)
                                     #f)))))))))))
               (#{match-empty\ 2505}#
                 (lambda (#{p\ 2523}# #{r\ 2524}#)
                   (if (null? #{p\ 2523}#)
                     #{r\ 2524}#
                     (if (eq? #{p\ 2523}# (quote any))
                       (cons (quote ()) #{r\ 2524}#)
                       (if (pair? #{p\ 2523}#)
                         (#{match-empty\ 2505}#
                           (car #{p\ 2523}#)
                           (#{match-empty\ 2505}#
                             (cdr #{p\ 2523}#)
                             #{r\ 2524}#))
                         (if (eq? #{p\ 2523}# (quote each-any))
                           (cons (quote ()) #{r\ 2524}#)
                           (let ((#{atom-key\ 2525}#
                                   (vector-ref #{p\ 2523}# 0)))
                             (if (memv #{atom-key\ 2525}# (quote (each)))
                               (#{match-empty\ 2505}#
                                 (vector-ref #{p\ 2523}# 1)
                                 #{r\ 2524}#)
                               (if (memv #{atom-key\ 2525}#
                                         '(free-id atom))
                                 #{r\ 2524}#
                                 (if (memv #{atom-key\ 2525}# (quote (vector)))
                                   (#{match-empty\ 2505}#
                                     (vector-ref #{p\ 2523}# 1)
                                     #{r\ 2524}#)))))))))))
               (#{match-each-any\ 2504}#
                 (lambda (#{e\ 2526}# #{w\ 2527}# #{mod\ 2528}#)
                   (if (pair? #{e\ 2526}#)
                     (let ((#{l\ 2529}#
                             (#{match-each-any\ 2504}#
                               (cdr #{e\ 2526}#)
                               #{w\ 2527}#
                               #{mod\ 2528}#)))
                       (if #{l\ 2529}#
                         (cons (#{wrap\ 1325}#
                                 (car #{e\ 2526}#)
                                 #{w\ 2527}#
                                 #{mod\ 2528}#)
                               #{l\ 2529}#)
                         #f))
                     (if (null? #{e\ 2526}#)
                       '()
                       (if (#{syntax-object?\ 1281}# #{e\ 2526}#)
                         (#{match-each-any\ 2504}#
                           (#{syntax-object-expression\ 1282}# #{e\ 2526}#)
                           (#{join-wraps\ 1316}#
                             #{w\ 2527}#
                             (#{syntax-object-wrap\ 1283}# #{e\ 2526}#))
                           #{mod\ 2528}#)
                         #f)))))
               (#{match-each\ 2503}#
                 (lambda (#{e\ 2530}#
                          #{p\ 2531}#
                          #{w\ 2532}#
                          #{mod\ 2533}#)
                   (if (pair? #{e\ 2530}#)
                     (let ((#{first\ 2534}#
                             (#{match\ 2507}#
                               (car #{e\ 2530}#)
                               #{p\ 2531}#
                               #{w\ 2532}#
                               '()
                               #{mod\ 2533}#)))
                       (if #{first\ 2534}#
                         (let ((#{rest\ 2535}#
                                 (#{match-each\ 2503}#
                                   (cdr #{e\ 2530}#)
                                   #{p\ 2531}#
                                   #{w\ 2532}#
                                   #{mod\ 2533}#)))
                           (if #{rest\ 2535}#
                             (cons #{first\ 2534}# #{rest\ 2535}#)
                             #f))
                         #f))
                     (if (null? #{e\ 2530}#)
                       '()
                       (if (#{syntax-object?\ 1281}# #{e\ 2530}#)
                         (#{match-each\ 2503}#
                           (#{syntax-object-expression\ 1282}# #{e\ 2530}#)
                           #{p\ 2531}#
                           (#{join-wraps\ 1316}#
                             #{w\ 2532}#
                             (#{syntax-object-wrap\ 1283}# #{e\ 2530}#))
                           (#{syntax-object-module\ 1284}# #{e\ 2530}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 2536}# #{p\ 2537}#)
            (if (eq? #{p\ 2537}# (quote any))
              (list #{e\ 2536}#)
              (if (#{syntax-object?\ 1281}# #{e\ 2536}#)
                (#{match*\ 2506}#
                  (#{syntax-object-expression\ 1282}# #{e\ 2536}#)
                  #{p\ 2537}#
                  (#{syntax-object-wrap\ 1283}# #{e\ 2536}#)
                  '()
                  (#{syntax-object-module\ 1284}# #{e\ 2536}#))
                (#{match*\ 2506}#
                  #{e\ 2536}#
                  #{p\ 2537}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2538}#)
      ((lambda (#{tmp\ 2539}#)
         ((lambda (#{tmp\ 2540}#)
            (if #{tmp\ 2540}#
              (apply (lambda (#{_\ 2541}# #{e1\ 2542}# #{e2\ 2543}#)
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
                             (cons #{e1\ 2542}# #{e2\ 2543}#)))
                     #{tmp\ 2540}#)
              ((lambda (#{tmp\ 2545}#)
                 (if #{tmp\ 2545}#
                   (apply (lambda (#{_\ 2546}#
                                   #{out\ 2547}#
                                   #{in\ 2548}#
                                   #{e1\ 2549}#
                                   #{e2\ 2550}#)
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
                                  #{in\ 2548}#
                                  '()
                                  (list #{out\ 2547}#
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
                                              (cons #{e1\ 2549}#
                                                    #{e2\ 2550}#)))))
                          #{tmp\ 2545}#)
                   ((lambda (#{tmp\ 2552}#)
                      (if #{tmp\ 2552}#
                        (apply (lambda (#{_\ 2553}#
                                        #{out\ 2554}#
                                        #{in\ 2555}#
                                        #{e1\ 2556}#
                                        #{e2\ 2557}#)
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
                                             #{in\ 2555}#)
                                       '()
                                       (list #{out\ 2554}#
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
                                                   (cons #{e1\ 2556}#
                                                         #{e2\ 2557}#)))))
                               #{tmp\ 2552}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 2539}#)))
                    ($sc-dispatch
                      #{tmp\ 2539}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 2539}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 2539}#
            '(any () any . each-any))))
       #{x\ 2538}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2561}#)
      ((lambda (#{tmp\ 2562}#)
         ((lambda (#{tmp\ 2563}#)
            (if #{tmp\ 2563}#
              (apply (lambda (#{_\ 2564}#
                              #{k\ 2565}#
                              #{keyword\ 2566}#
                              #{pattern\ 2567}#
                              #{template\ 2568}#)
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
                                         (cons #{k\ 2565}#
                                               (map (lambda (#{tmp\ 2571}#
                                                             #{tmp\ 2570}#)
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
                                                                  #{tmp\ 2570}#)
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
                                                                  #{tmp\ 2571}#)))
                                                    #{template\ 2568}#
                                                    #{pattern\ 2567}#))))))
                     #{tmp\ 2563}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2562}#)))
          ($sc-dispatch
            #{tmp\ 2562}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 2561}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 2572}#)
      ((lambda (#{tmp\ 2573}#)
         ((lambda (#{tmp\ 2574}#)
            (if (if #{tmp\ 2574}#
                  (apply (lambda (#{let*\ 2575}#
                                  #{x\ 2576}#
                                  #{v\ 2577}#
                                  #{e1\ 2578}#
                                  #{e2\ 2579}#)
                           (and-map identifier? #{x\ 2576}#))
                         #{tmp\ 2574}#)
                  #f)
              (apply (lambda (#{let*\ 2581}#
                              #{x\ 2582}#
                              #{v\ 2583}#
                              #{e1\ 2584}#
                              #{e2\ 2585}#)
                       (letrec ((#{f\ 2586}#
                                  (lambda (#{bindings\ 2587}#)
                                    (if (null? #{bindings\ 2587}#)
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
                                                  (cons #{e1\ 2584}#
                                                        #{e2\ 2585}#)))
                                      ((lambda (#{tmp\ 2591}#)
                                         ((lambda (#{tmp\ 2592}#)
                                            (if #{tmp\ 2592}#
                                              (apply (lambda (#{body\ 2593}#
                                                              #{binding\ 2594}#)
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
                                                             (list #{binding\ 2594}#)
                                                             #{body\ 2593}#))
                                                     #{tmp\ 2592}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 2591}#)))
                                          ($sc-dispatch
                                            #{tmp\ 2591}#
                                            '(any any))))
                                       (list (#{f\ 2586}#
                                               (cdr #{bindings\ 2587}#))
                                             (car #{bindings\ 2587}#)))))))
                         (#{f\ 2586}# (map list #{x\ 2582}# #{v\ 2583}#))))
                     #{tmp\ 2574}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2573}#)))
          ($sc-dispatch
            #{tmp\ 2573}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 2572}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 2595}#)
      ((lambda (#{tmp\ 2596}#)
         ((lambda (#{tmp\ 2597}#)
            (if #{tmp\ 2597}#
              (apply (lambda (#{_\ 2598}#
                              #{var\ 2599}#
                              #{init\ 2600}#
                              #{step\ 2601}#
                              #{e0\ 2602}#
                              #{e1\ 2603}#
                              #{c\ 2604}#)
                       ((lambda (#{tmp\ 2605}#)
                          ((lambda (#{tmp\ 2606}#)
                             (if #{tmp\ 2606}#
                               (apply (lambda (#{step\ 2607}#)
                                        ((lambda (#{tmp\ 2608}#)
                                           ((lambda (#{tmp\ 2609}#)
                                              (if #{tmp\ 2609}#
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
                                                                    #{var\ 2599}#
                                                                    #{init\ 2600}#)
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
                                                                           #{e0\ 2602}#)
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
                                                                             #{c\ 2604}#
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
                                                                                         #{step\ 2607}#)))))))
                                                       #{tmp\ 2609}#)
                                                ((lambda (#{tmp\ 2614}#)
                                                   (if #{tmp\ 2614}#
                                                     (apply (lambda (#{e1\ 2615}#
                                                                     #{e2\ 2616}#)
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
                                                                         #{var\ 2599}#
                                                                         #{init\ 2600}#)
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
                                                                          #{e0\ 2602}#
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
                                                                                (cons #{e1\ 2615}#
                                                                                      #{e2\ 2616}#))
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
                                                                                  #{c\ 2604}#
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
                                                                                              #{step\ 2607}#)))))))
                                                            #{tmp\ 2614}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 2608}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 2608}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 2608}#
                                              '())))
                                         #{e1\ 2603}#))
                                      #{tmp\ 2606}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 2605}#)))
                           ($sc-dispatch #{tmp\ 2605}# (quote each-any))))
                        (map (lambda (#{v\ 2623}# #{s\ 2624}#)
                               ((lambda (#{tmp\ 2625}#)
                                  ((lambda (#{tmp\ 2626}#)
                                     (if #{tmp\ 2626}#
                                       (apply (lambda () #{v\ 2623}#)
                                              #{tmp\ 2626}#)
                                       ((lambda (#{tmp\ 2627}#)
                                          (if #{tmp\ 2627}#
                                            (apply (lambda (#{e\ 2628}#)
                                                     #{e\ 2628}#)
                                                   #{tmp\ 2627}#)
                                            ((lambda (#{_\ 2629}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 2595}#
                                                 #{s\ 2624}#))
                                             #{tmp\ 2625}#)))
                                        ($sc-dispatch
                                          #{tmp\ 2625}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 2625}# (quote ()))))
                                #{s\ 2624}#))
                             #{var\ 2599}#
                             #{step\ 2601}#)))
                     #{tmp\ 2597}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2596}#)))
          ($sc-dispatch
            #{tmp\ 2596}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 2595}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 2632}#
               (lambda (#{x\ 2636}# #{y\ 2637}#)
                 ((lambda (#{tmp\ 2638}#)
                    ((lambda (#{tmp\ 2639}#)
                       (if #{tmp\ 2639}#
                         (apply (lambda (#{x\ 2640}# #{y\ 2641}#)
                                  ((lambda (#{tmp\ 2642}#)
                                     ((lambda (#{tmp\ 2643}#)
                                        (if #{tmp\ 2643}#
                                          (apply (lambda (#{dy\ 2644}#)
                                                   ((lambda (#{tmp\ 2645}#)
                                                      ((lambda (#{tmp\ 2646}#)
                                                         (if #{tmp\ 2646}#
                                                           (apply (lambda (#{dx\ 2647}#)
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
                                                                          (cons #{dx\ 2647}#
                                                                                #{dy\ 2644}#)))
                                                                  #{tmp\ 2646}#)
                                                           ((lambda (#{_\ 2648}#)
                                                              (if (null? #{dy\ 2644}#)
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
                                                                      #{x\ 2640}#)
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
                                                                      #{x\ 2640}#
                                                                      #{y\ 2641}#)))
                                                            #{tmp\ 2645}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 2645}#
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
                                                    #{x\ 2640}#))
                                                 #{tmp\ 2643}#)
                                          ((lambda (#{tmp\ 2649}#)
                                             (if #{tmp\ 2649}#
                                               (apply (lambda (#{stuff\ 2650}#)
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
                                                              (cons #{x\ 2640}#
                                                                    #{stuff\ 2650}#)))
                                                      #{tmp\ 2649}#)
                                               ((lambda (#{else\ 2651}#)
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
                                                        #{x\ 2640}#
                                                        #{y\ 2641}#))
                                                #{tmp\ 2642}#)))
                                           ($sc-dispatch
                                             #{tmp\ 2642}#
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
                                        #{tmp\ 2642}#
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
                                   #{y\ 2641}#))
                                #{tmp\ 2639}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 2638}#)))
                     ($sc-dispatch #{tmp\ 2638}# (quote (any any)))))
                  (list #{x\ 2636}# #{y\ 2637}#))))
             (#{quasiappend\ 2633}#
               (lambda (#{x\ 2652}# #{y\ 2653}#)
                 ((lambda (#{tmp\ 2654}#)
                    ((lambda (#{tmp\ 2655}#)
                       (if #{tmp\ 2655}#
                         (apply (lambda (#{x\ 2656}# #{y\ 2657}#)
                                  ((lambda (#{tmp\ 2658}#)
                                     ((lambda (#{tmp\ 2659}#)
                                        (if #{tmp\ 2659}#
                                          (apply (lambda () #{x\ 2656}#)
                                                 #{tmp\ 2659}#)
                                          ((lambda (#{_\ 2660}#)
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
                                                   #{x\ 2656}#
                                                   #{y\ 2657}#))
                                           #{tmp\ 2658}#)))
                                      ($sc-dispatch
                                        #{tmp\ 2658}#
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
                                   #{y\ 2657}#))
                                #{tmp\ 2655}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 2654}#)))
                     ($sc-dispatch #{tmp\ 2654}# (quote (any any)))))
                  (list #{x\ 2652}# #{y\ 2653}#))))
             (#{quasivector\ 2634}#
               (lambda (#{x\ 2661}#)
                 ((lambda (#{tmp\ 2662}#)
                    ((lambda (#{x\ 2663}#)
                       ((lambda (#{tmp\ 2664}#)
                          ((lambda (#{tmp\ 2665}#)
                             (if #{tmp\ 2665}#
                               (apply (lambda (#{x\ 2666}#)
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
                                              (list->vector #{x\ 2666}#)))
                                      #{tmp\ 2665}#)
                               ((lambda (#{tmp\ 2668}#)
                                  (if #{tmp\ 2668}#
                                    (apply (lambda (#{x\ 2669}#)
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
                                                   #{x\ 2669}#))
                                           #{tmp\ 2668}#)
                                    ((lambda (#{_\ 2671}#)
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
                                             #{x\ 2663}#))
                                     #{tmp\ 2664}#)))
                                ($sc-dispatch
                                  #{tmp\ 2664}#
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
                             #{tmp\ 2664}#
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
                        #{x\ 2663}#))
                     #{tmp\ 2662}#))
                  #{x\ 2661}#)))
             (#{quasi\ 2635}#
               (lambda (#{p\ 2672}# #{lev\ 2673}#)
                 ((lambda (#{tmp\ 2674}#)
                    ((lambda (#{tmp\ 2675}#)
                       (if #{tmp\ 2675}#
                         (apply (lambda (#{p\ 2676}#)
                                  (if (= #{lev\ 2673}# 0)
                                    #{p\ 2676}#
                                    (#{quasicons\ 2632}#
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
                                      (#{quasi\ 2635}#
                                        (list #{p\ 2676}#)
                                        (- #{lev\ 2673}# 1)))))
                                #{tmp\ 2675}#)
                         ((lambda (#{tmp\ 2677}#)
                            (if (if #{tmp\ 2677}#
                                  (apply (lambda (#{args\ 2678}#)
                                           (= #{lev\ 2673}# 0))
                                         #{tmp\ 2677}#)
                                  #f)
                              (apply (lambda (#{args\ 2679}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 2672}#
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
                                               #{args\ 2679}#)))
                                     #{tmp\ 2677}#)
                              ((lambda (#{tmp\ 2680}#)
                                 (if #{tmp\ 2680}#
                                   (apply (lambda (#{p\ 2681}# #{q\ 2682}#)
                                            (if (= #{lev\ 2673}# 0)
                                              (#{quasiappend\ 2633}#
                                                #{p\ 2681}#
                                                (#{quasi\ 2635}#
                                                  #{q\ 2682}#
                                                  #{lev\ 2673}#))
                                              (#{quasicons\ 2632}#
                                                (#{quasicons\ 2632}#
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
                                                  (#{quasi\ 2635}#
                                                    (list #{p\ 2681}#)
                                                    (- #{lev\ 2673}# 1)))
                                                (#{quasi\ 2635}#
                                                  #{q\ 2682}#
                                                  #{lev\ 2673}#))))
                                          #{tmp\ 2680}#)
                                   ((lambda (#{tmp\ 2683}#)
                                      (if (if #{tmp\ 2683}#
                                            (apply (lambda (#{args\ 2684}#
                                                            #{q\ 2685}#)
                                                     (= #{lev\ 2673}# 0))
                                                   #{tmp\ 2683}#)
                                            #f)
                                        (apply (lambda (#{args\ 2686}#
                                                        #{q\ 2687}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 2672}#
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
                                                         #{args\ 2686}#)))
                                               #{tmp\ 2683}#)
                                        ((lambda (#{tmp\ 2688}#)
                                           (if #{tmp\ 2688}#
                                             (apply (lambda (#{p\ 2689}#)
                                                      (#{quasicons\ 2632}#
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
                                                        (#{quasi\ 2635}#
                                                          (list #{p\ 2689}#)
                                                          (+ #{lev\ 2673}#
                                                             1))))
                                                    #{tmp\ 2688}#)
                                             ((lambda (#{tmp\ 2690}#)
                                                (if #{tmp\ 2690}#
                                                  (apply (lambda (#{p\ 2691}#
                                                                  #{q\ 2692}#)
                                                           (#{quasicons\ 2632}#
                                                             (#{quasi\ 2635}#
                                                               #{p\ 2691}#
                                                               #{lev\ 2673}#)
                                                             (#{quasi\ 2635}#
                                                               #{q\ 2692}#
                                                               #{lev\ 2673}#)))
                                                         #{tmp\ 2690}#)
                                                  ((lambda (#{tmp\ 2693}#)
                                                     (if #{tmp\ 2693}#
                                                       (apply (lambda (#{x\ 2694}#)
                                                                (#{quasivector\ 2634}#
                                                                  (#{quasi\ 2635}#
                                                                    #{x\ 2694}#
                                                                    #{lev\ 2673}#)))
                                                              #{tmp\ 2693}#)
                                                       ((lambda (#{p\ 2696}#)
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
                                                                #{p\ 2696}#))
                                                        #{tmp\ 2674}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 2674}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 2674}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 2674}#
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
                                      #{tmp\ 2674}#
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
                                 #{tmp\ 2674}#
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
                            #{tmp\ 2674}#
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
                       #{tmp\ 2674}#
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
                  #{p\ 2672}#))))
      (lambda (#{x\ 2697}#)
        ((lambda (#{tmp\ 2698}#)
           ((lambda (#{tmp\ 2699}#)
              (if #{tmp\ 2699}#
                (apply (lambda (#{_\ 2700}# #{e\ 2701}#)
                         (#{quasi\ 2635}# #{e\ 2701}# 0))
                       #{tmp\ 2699}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 2698}#)))
            ($sc-dispatch #{tmp\ 2698}# (quote (any any)))))
         #{x\ 2697}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2702}#)
      (letrec ((#{read-file\ 2703}#
                 (lambda (#{fn\ 2704}# #{k\ 2705}#)
                   (let ((#{p\ 2706}# (open-input-file #{fn\ 2704}#)))
                     (letrec ((#{f\ 2707}#
                                (lambda (#{x\ 2708}#)
                                  (if (eof-object? #{x\ 2708}#)
                                    (begin
                                      (close-input-port #{p\ 2706}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 2705}#
                                            #{x\ 2708}#)
                                          (#{f\ 2707}# (read #{p\ 2706}#)))))))
                       (#{f\ 2707}# (read #{p\ 2706}#)))))))
        ((lambda (#{tmp\ 2709}#)
           ((lambda (#{tmp\ 2710}#)
              (if #{tmp\ 2710}#
                (apply (lambda (#{k\ 2711}# #{filename\ 2712}#)
                         (let ((#{fn\ 2713}#
                                 (syntax->datum #{filename\ 2712}#)))
                           ((lambda (#{tmp\ 2714}#)
                              ((lambda (#{tmp\ 2715}#)
                                 (if #{tmp\ 2715}#
                                   (apply (lambda (#{exp\ 2716}#)
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
                                                  #{exp\ 2716}#))
                                          #{tmp\ 2715}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 2714}#)))
                               ($sc-dispatch #{tmp\ 2714}# (quote each-any))))
                            (#{read-file\ 2703}# #{fn\ 2713}# #{k\ 2711}#))))
                       #{tmp\ 2710}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 2709}#)))
            ($sc-dispatch #{tmp\ 2709}# (quote (any any)))))
         #{x\ 2702}#)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2718}#)
      ((lambda (#{tmp\ 2719}#)
         ((lambda (#{tmp\ 2720}#)
            (if #{tmp\ 2720}#
              (apply (lambda (#{_\ 2721}# #{e\ 2722}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 2718}#))
                     #{tmp\ 2720}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2719}#)))
          ($sc-dispatch #{tmp\ 2719}# (quote (any any)))))
       #{x\ 2718}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2723}#)
      ((lambda (#{tmp\ 2724}#)
         ((lambda (#{tmp\ 2725}#)
            (if #{tmp\ 2725}#
              (apply (lambda (#{_\ 2726}# #{e\ 2727}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 2723}#))
                     #{tmp\ 2725}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2724}#)))
          ($sc-dispatch #{tmp\ 2724}# (quote (any any)))))
       #{x\ 2723}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 2728}#)
      ((lambda (#{tmp\ 2729}#)
         ((lambda (#{tmp\ 2730}#)
            (if #{tmp\ 2730}#
              (apply (lambda (#{_\ 2731}#
                              #{e\ 2732}#
                              #{m1\ 2733}#
                              #{m2\ 2734}#)
                       ((lambda (#{tmp\ 2735}#)
                          ((lambda (#{body\ 2736}#)
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
                                               #{e\ 2732}#))
                                   #{body\ 2736}#))
                           #{tmp\ 2735}#))
                        (letrec ((#{f\ 2737}#
                                   (lambda (#{clause\ 2738}# #{clauses\ 2739}#)
                                     (if (null? #{clauses\ 2739}#)
                                       ((lambda (#{tmp\ 2741}#)
                                          ((lambda (#{tmp\ 2742}#)
                                             (if #{tmp\ 2742}#
                                               (apply (lambda (#{e1\ 2743}#
                                                               #{e2\ 2744}#)
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
                                                              (cons #{e1\ 2743}#
                                                                    #{e2\ 2744}#)))
                                                      #{tmp\ 2742}#)
                                               ((lambda (#{tmp\ 2746}#)
                                                  (if #{tmp\ 2746}#
                                                    (apply (lambda (#{k\ 2747}#
                                                                    #{e1\ 2748}#
                                                                    #{e2\ 2749}#)
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
                                                                               #{k\ 2747}#))
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
                                                                         (cons #{e1\ 2748}#
                                                                               #{e2\ 2749}#))))
                                                           #{tmp\ 2746}#)
                                                    ((lambda (#{_\ 2752}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 2728}#
                                                         #{clause\ 2738}#))
                                                     #{tmp\ 2741}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 2741}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 2741}#
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
                                        #{clause\ 2738}#)
                                       ((lambda (#{tmp\ 2753}#)
                                          ((lambda (#{rest\ 2754}#)
                                             ((lambda (#{tmp\ 2755}#)
                                                ((lambda (#{tmp\ 2756}#)
                                                   (if #{tmp\ 2756}#
                                                     (apply (lambda (#{k\ 2757}#
                                                                     #{e1\ 2758}#
                                                                     #{e2\ 2759}#)
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
                                                                                #{k\ 2757}#))
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
                                                                          (cons #{e1\ 2758}#
                                                                                #{e2\ 2759}#))
                                                                    #{rest\ 2754}#))
                                                            #{tmp\ 2756}#)
                                                     ((lambda (#{_\ 2762}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 2728}#
                                                          #{clause\ 2738}#))
                                                      #{tmp\ 2755}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 2755}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 2738}#))
                                           #{tmp\ 2753}#))
                                        (#{f\ 2737}#
                                          (car #{clauses\ 2739}#)
                                          (cdr #{clauses\ 2739}#)))))))
                          (#{f\ 2737}# #{m1\ 2733}# #{m2\ 2734}#))))
                     #{tmp\ 2730}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2729}#)))
          ($sc-dispatch
            #{tmp\ 2729}#
            '(any any any . each-any))))
       #{x\ 2728}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 2763}#)
      ((lambda (#{tmp\ 2764}#)
         ((lambda (#{tmp\ 2765}#)
            (if #{tmp\ 2765}#
              (apply (lambda (#{_\ 2766}# #{e\ 2767}#)
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
                                               #{e\ 2767}#))
                                   (list (cons #{_\ 2766}#
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
                                               (cons #{e\ 2767}#
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
                     #{tmp\ 2765}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 2764}#)))
          ($sc-dispatch #{tmp\ 2764}# (quote (any any)))))
       #{x\ 2763}#))))

