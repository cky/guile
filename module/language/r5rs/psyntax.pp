;;; psyntax.pp
;;; automatically generated from psyntax.ss
;;; Wed Aug 30 12:24:52 EST 2000
;;; see copyright notice in psyntax.ss

((lambda ()
   (letrec ((g452
             (lambda (g1823)
               ((letrec ((g1824
                          (lambda (g1827 g1825 g1826)
                            (if (pair? g1827)
                                (g1824
                                  (cdr g1827)
                                  (cons (g393 (car g1827) g1826) g1825)
                                  g1826)
                                (if (g256 g1827)
                                    (cons (g393 g1827 g1826) g1825)
                                    (if (null? g1827)
                                        g1825
                                        (if (g204 g1827)
                                            (g1824
                                              (g205 g1827)
                                              g1825
                                              (g371 g1826 (g206 g1827)))
                                            (if (g90 g1827)
                                                (g1824
                                                  (annotation-expression
                                                    g1827)
                                                  g1825
                                                  g1826)
                                                (cons g1827 g1825)))))))))
                  g1824)
                g1823
                '()
                '(()))))
            (g451
             (lambda (g833)
               ((lambda (g834) (if (g90 g834) (gensym) (gensym)))
                (if (g204 g833) (g205 g833) g833))))
            (g450
             (lambda (g1820 g1819)
               (g449 g1820
                     g1819
                     (lambda (g1821)
                       (if ((lambda (g1822)
                              (if g1822
                                  g1822
                                  (if (pair? g1821)
                                      (g90 (car g1821))
                                      '#f)))
                            (g90 g1821))
                           (g448 g1821 '#f)
                           g1821)))))
            (g449
             (lambda (g837 g835 g836)
               (if (memq 'top (g264 g835))
                   (g836 g837)
                   ((letrec ((g838
                              (lambda (g839)
                                (if (g204 g839)
                                    (g449 (g205 g839) (g206 g839) g836)
                                    (if (pair? g839)
                                        ((lambda (g841 g840)
                                           (if (if (eq? g841 (car g839))
                                                   (eq? g840 (cdr g839))
                                                   '#f)
                                               g839
                                               (cons g841 g840)))
                                         (g838 (car g839))
                                         (g838 (cdr g839)))
                                        (if (vector? g839)
                                            ((lambda (g842)
                                               ((lambda (g843)
                                                  (if (andmap
                                                        eq?
                                                        g842
                                                        g843)
                                                      g839
                                                      (list->vector g843)))
                                                (map g838 g842)))
                                             (vector->list g839))
                                            g839))))))
                      g838)
                    g837))))
            (g448
             (lambda (g1813 g1812)
               (if (pair? g1813)
                   ((lambda (g1814)
                      (begin (if g1812
                                 (set-annotation-stripped! g1812 g1814)
                                 (void))
                             (set-car! g1814 (g448 (car g1813) '#f))
                             (set-cdr! g1814 (g448 (cdr g1813) '#f))
                             g1814))
                    (cons '#f '#f))
                   (if (g90 g1813)
                       ((lambda (g1815)
                          (if g1815
                              g1815
                              (g448 (annotation-expression g1813) g1813)))
                        (annotation-stripped g1813))
                       (if (vector? g1813)
                           ((lambda (g1816)
                              (begin (if g1812
                                         (set-annotation-stripped!
                                           g1812
                                           g1816)
                                         (void))
                                     ((letrec ((g1817
                                                (lambda (g1818)
                                                  (if (not (< g1818 '0))
                                                      (begin (vector-set!
                                                               g1816
                                                               g1818
                                                               (g448 (vector-ref
                                                                       g1813
                                                                       g1818)
                                                                     '#f))
                                                             (g1817
                                                               (- g1818
                                                                  '1)))
                                                      (void)))))
                                        g1817)
                                      (- (vector-length g1813) '1))
                                     g1816))
                            (make-vector (vector-length g1813)))
                           g1813)))))
            (g447
             (lambda (g844)
               (if (g255 g844)
                   (g378 g844
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
                                 strip*
                                 strip-annotation
                                 ellipsis?
                                 chi-void
                                 chi-local-syntax
                                 chi-lambda-clause
                                 parse-define-syntax
                                 parse-define
                                 parse-import
                                 parse-module
                                 do-import!
                                 chi-internal
                                 chi-body
                                 chi-macro
                                 chi-set!
                                 chi-application
                                 chi-expr
                                 chi
                                 ct-eval/residualize
                                 do-top-import
                                 vfor-each
                                 vmap
                                 chi-external
                                 check-defined-ids
                                 check-module-exports
                                 extend-store!
                                 id-set-diff
                                 chi-top-module
                                 set-module-binding-val!
                                 set-module-binding-imps!
                                 set-module-binding-label!
                                 set-module-binding-id!
                                 set-module-binding-type!
                                 module-binding-val
                                 module-binding-imps
                                 module-binding-label
                                 module-binding-id
                                 module-binding-type
                                 module-binding?
                                 make-module-binding
                                 make-resolved-interface
                                 make-trimmed-interface
                                 set-interface-token!
                                 set-interface-exports!
                                 interface-token
                                 interface-exports
                                 interface?
                                 make-interface
                                 flatten-exports
                                 chi-top
                                 chi-top-expr
                                 syntax-type
                                 chi-when-list
                                 chi-top-sequence
                                 chi-sequence
                                 source-wrap
                                 wrap
                                 bound-id-member?
                                 invalid-ids-error
                                 distinct-bound-ids?
                                 valid-bound-ids?
                                 bound-id=?
                                 literal-id=?
                                 free-id=?
                                 id-var-name
                                 id-var-name-loc
                                 id-var-name&marks
                                 id-var-name-loc&marks
                                 same-marks?
                                 join-marks
                                 join-wraps
                                 smart-append
                                 make-trimmed-syntax-object
                                 make-binding-wrap
                                 lookup-import-binding-name
                                 extend-ribcage-subst!
                                 extend-ribcage-barrier-help!
                                 extend-ribcage-barrier!
                                 extend-ribcage!
                                 make-empty-ribcage
                                 import-token-key
                                 import-token?
                                 make-import-token
                                 barrier-marker
                                 new-mark
                                 anti-mark
                                 the-anti-mark
                                 only-top-marked?
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
                                 set-indirect-label!
                                 get-indirect-label
                                 indirect-label?
                                 gen-indirect-label
                                 gen-labels
                                 label?
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
                                 sanitize-binding
                                 lookup*
                                 displaced-lexical-error
                                 transformer-env
                                 extend-var-env*
                                 extend-env*
                                 extend-env
                                 null-env
                                 binding?
                                 set-binding-value!
                                 set-binding-type!
                                 binding-value
                                 binding-type
                                 make-binding
                                 arg-check
                                 source-annotation
                                 no-source
                                 unannotate
                                 set-syntax-object-wrap!
                                 set-syntax-object-expression!
                                 syntax-object-wrap
                                 syntax-object-expression
                                 syntax-object?
                                 make-syntax-object
                                 self-evaluating?
                                 build-lexical-var
                                 build-letrec
                                 build-sequence
                                 build-data
                                 build-primref
                                 build-lambda
                                 build-cte-install
                                 build-module-definition
                                 build-global-definition
                                 build-global-assignment
                                 build-global-reference
                                 build-lexical-assignment
                                 build-lexical-reference
                                 build-conditional
                                 build-application
                                 generate-id
                                 get-import-binding
                                 get-global-definition-hook
                                 put-global-definition-hook
                                 gensym-hook
                                 error-hook
                                 local-eval-hook
                                 top-level-eval-hook
                                 annotation?
                                 fx<
                                 fx=
                                 fx-
                                 fx+
                                 noexpand
                                 define-structure
                                 unless
                                 when)
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
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
                                (top)
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
                               ("i" "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
                                    "i"
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
                             #(ribcage ((import-token . *top*)) () ())
                             #(ribcage ((import-token . *top*)) () ()))))
                   '#f)))
            (g446 (lambda () (list 'void)))
            (g445
             (lambda (g850 g845 g849 g846 g848 g847)
               ((lambda (g851)
                  ((lambda (g852)
                     (if g852
                         (apply
                           (lambda (g857 g853 g856 g854 g855)
                             ((lambda (g858)
                                (if (not (g389 g858))
                                    (g391 (map (lambda (g859)
                                                 (g393 g859 g846))
                                               g858)
                                          (g394 g845 g846 g848)
                                          '"keyword")
                                    ((lambda (g860)
                                       ((lambda (g861)
                                          (g847 (cons g854 g855)
                                                (g247 g860
                                                      ((lambda (g863 g862)
                                                         (map (lambda (g865)
                                                                (g231 'deferred
                                                                      (g432 g865
                                                                            g862
                                                                            g863)))
                                                              g856))
                                                       (if g850 g861 g846)
                                                       (g249 g849))
                                                      g849)
                                                g861
                                                g848))
                                        (g368 g858 g860 g846)))
                                     (g299 g858))))
                              g853))
                           g852)
                         ((lambda (g868)
                            (syntax-error (g394 g845 g846 g848)))
                          g851)))
                   ($syntax-dispatch
                     g851
                     '(any #(each (any any)) any . each-any))))
                g845)))
            (g444
             (lambda (g1789 g1785 g1788 g1786 g1787)
               ((lambda (g1790)
                  ((lambda (g1791)
                     (if g1791
                         (apply
                           (lambda (g1794 g1792 g1793)
                             ((lambda (g1795)
                                (if (not (g389 g1795))
                                    (syntax-error
                                      g1789
                                      '"invalid parameter list in")
                                    ((lambda (g1797 g1796)
                                       (g1787
                                         g1796
                                         (g437 (cons g1792 g1793)
                                               g1789
                                               (g248 g1797 g1796 g1788)
                                               (g368 g1795 g1797 g1786))))
                                     (g299 g1795)
                                     (map g451 g1795))))
                              g1794))
                           g1791)
                         ((lambda (g1800)
                            (if g1800
                                (apply
                                  (lambda (g1803 g1801 g1802)
                                    ((lambda (g1804)
                                       (if (not (g389 g1804))
                                           (syntax-error
                                             g1789
                                             '"invalid parameter list in")
                                           ((lambda (g1806 g1805)
                                              (g1787
                                                ((letrec ((g1808
                                                           (lambda (g1810
                                                                    g1809)
                                                             (if (null?
                                                                   g1810)
                                                                 g1809
                                                                 (g1808
                                                                   (cdr g1810)
                                                                   (cons (car g1810)
                                                                         g1809))))))
                                                   g1808)
                                                 (cdr g1805)
                                                 (car g1805))
                                                (g437 (cons g1801 g1802)
                                                      g1789
                                                      (g248 g1806
                                                            g1805
                                                            g1788)
                                                      (g368 g1804
                                                            g1806
                                                            g1786))))
                                            (g299 g1804)
                                            (map g451 g1804))))
                                     (g452 g1803)))
                                  g1800)
                                ((lambda (g1811) (syntax-error g1789))
                                 g1790)))
                          ($syntax-dispatch g1790 '(any any . each-any)))))
                   ($syntax-dispatch g1790 '(each-any any . each-any))))
                g1785)))
            (g443
             (lambda (g872 g869 g871 g870)
               ((lambda (g873)
                  ((lambda (g874)
                     (if (if g874
                             (apply
                               (lambda (g877 g875 g876) (g256 g875))
                               g874)
                             '#f)
                         (apply
                           (lambda (g880 g878 g879) (g870 g878 g879 g869))
                           g874)
                         ((lambda (g881)
                            (syntax-error (g394 g872 g869 g871)))
                          g873)))
                   ($syntax-dispatch g873 '(any any any))))
                g872)))
            (g442
             (lambda (g1758 g1755 g1757 g1756)
               ((lambda (g1759)
                  ((lambda (g1760)
                     (if (if g1760
                             (apply
                               (lambda (g1763 g1761 g1762) (g256 g1761))
                               g1760)
                             '#f)
                         (apply
                           (lambda (g1766 g1764 g1765)
                             (g1756 g1764 g1765 g1755))
                           g1760)
                         ((lambda (g1767)
                            (if (if g1767
                                    (apply
                                      (lambda (g1772
                                               g1768
                                               g1771
                                               g1769
                                               g1770)
                                        (if (g256 g1768)
                                            (g389 (g452 g1771))
                                            '#f))
                                      g1767)
                                    '#f)
                                (apply
                                  (lambda (g1777 g1773 g1776 g1774 g1775)
                                    (g1756
                                      (g393 g1773 g1755)
                                      (cons '#(syntax-object
                                               lambda
                                               ((top)
                                                #(ribcage
                                                  #(_ name args e1 e2)
                                                  #((top)
                                                    (top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                  #("i" "i" "i" "i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(e w s k)
                                                  #((top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                  #("i" "i" "i" "i"))
                                                #(ribcage
                                                  (lambda-var-list
                                                    gen-var
                                                    strip
                                                    strip*
                                                    strip-annotation
                                                    ellipsis?
                                                    chi-void
                                                    chi-local-syntax
                                                    chi-lambda-clause
                                                    parse-define-syntax
                                                    parse-define
                                                    parse-import
                                                    parse-module
                                                    do-import!
                                                    chi-internal
                                                    chi-body
                                                    chi-macro
                                                    chi-set!
                                                    chi-application
                                                    chi-expr
                                                    chi
                                                    ct-eval/residualize
                                                    do-top-import
                                                    vfor-each
                                                    vmap
                                                    chi-external
                                                    check-defined-ids
                                                    check-module-exports
                                                    extend-store!
                                                    id-set-diff
                                                    chi-top-module
                                                    set-module-binding-val!
                                                    set-module-binding-imps!
                                                    set-module-binding-label!
                                                    set-module-binding-id!
                                                    set-module-binding-type!
                                                    module-binding-val
                                                    module-binding-imps
                                                    module-binding-label
                                                    module-binding-id
                                                    module-binding-type
                                                    module-binding?
                                                    make-module-binding
                                                    make-resolved-interface
                                                    make-trimmed-interface
                                                    set-interface-token!
                                                    set-interface-exports!
                                                    interface-token
                                                    interface-exports
                                                    interface?
                                                    make-interface
                                                    flatten-exports
                                                    chi-top
                                                    chi-top-expr
                                                    syntax-type
                                                    chi-when-list
                                                    chi-top-sequence
                                                    chi-sequence
                                                    source-wrap
                                                    wrap
                                                    bound-id-member?
                                                    invalid-ids-error
                                                    distinct-bound-ids?
                                                    valid-bound-ids?
                                                    bound-id=?
                                                    literal-id=?
                                                    free-id=?
                                                    id-var-name
                                                    id-var-name-loc
                                                    id-var-name&marks
                                                    id-var-name-loc&marks
                                                    same-marks?
                                                    join-marks
                                                    join-wraps
                                                    smart-append
                                                    make-trimmed-syntax-object
                                                    make-binding-wrap
                                                    lookup-import-binding-name
                                                    extend-ribcage-subst!
                                                    extend-ribcage-barrier-help!
                                                    extend-ribcage-barrier!
                                                    extend-ribcage!
                                                    make-empty-ribcage
                                                    import-token-key
                                                    import-token?
                                                    make-import-token
                                                    barrier-marker
                                                    new-mark
                                                    anti-mark
                                                    the-anti-mark
                                                    only-top-marked?
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
                                                    set-indirect-label!
                                                    get-indirect-label
                                                    indirect-label?
                                                    gen-indirect-label
                                                    gen-labels
                                                    label?
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
                                                    sanitize-binding
                                                    lookup*
                                                    displaced-lexical-error
                                                    transformer-env
                                                    extend-var-env*
                                                    extend-env*
                                                    extend-env
                                                    null-env
                                                    binding?
                                                    set-binding-value!
                                                    set-binding-type!
                                                    binding-value
                                                    binding-type
                                                    make-binding
                                                    arg-check
                                                    source-annotation
                                                    no-source
                                                    unannotate
                                                    set-syntax-object-wrap!
                                                    set-syntax-object-expression!
                                                    syntax-object-wrap
                                                    syntax-object-expression
                                                    syntax-object?
                                                    make-syntax-object
                                                    self-evaluating?
                                                    build-lexical-var
                                                    build-letrec
                                                    build-sequence
                                                    build-data
                                                    build-primref
                                                    build-lambda
                                                    build-cte-install
                                                    build-module-definition
                                                    build-global-definition
                                                    build-global-assignment
                                                    build-global-reference
                                                    build-lexical-assignment
                                                    build-lexical-reference
                                                    build-conditional
                                                    build-application
                                                    generate-id
                                                    get-import-binding
                                                    get-global-definition-hook
                                                    put-global-definition-hook
                                                    gensym-hook
                                                    error-hook
                                                    local-eval-hook
                                                    top-level-eval-hook
                                                    annotation?
                                                    fx<
                                                    fx=
                                                    fx-
                                                    fx+
                                                    noexpand
                                                    define-structure
                                                    unless
                                                    when)
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
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
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
                                                  ("i" "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
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
                                                  ((import-token . *top*))
                                                  ()
                                                  ())
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ())))
                                            (g393 (cons g1776
                                                        (cons g1774 g1775))
                                                  g1755))
                                      '(())))
                                  g1767)
                                ((lambda (g1779)
                                   (if (if g1779
                                           (apply
                                             (lambda (g1781 g1780)
                                               (g256 g1780))
                                             g1779)
                                           '#f)
                                       (apply
                                         (lambda (g1783 g1782)
                                           (g1756
                                             (g393 g1782 g1755)
                                             '(#(syntax-object
                                                 void
                                                 ((top)
                                                  #(ribcage
                                                    #(_ name)
                                                    #((top) (top))
                                                    #("i" "i"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e w s k)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i" "i" "i" "i"))
                                                  #(ribcage
                                                    (lambda-var-list
                                                      gen-var
                                                      strip
                                                      strip*
                                                      strip-annotation
                                                      ellipsis?
                                                      chi-void
                                                      chi-local-syntax
                                                      chi-lambda-clause
                                                      parse-define-syntax
                                                      parse-define
                                                      parse-import
                                                      parse-module
                                                      do-import!
                                                      chi-internal
                                                      chi-body
                                                      chi-macro
                                                      chi-set!
                                                      chi-application
                                                      chi-expr
                                                      chi
                                                      ct-eval/residualize
                                                      do-top-import
                                                      vfor-each
                                                      vmap
                                                      chi-external
                                                      check-defined-ids
                                                      check-module-exports
                                                      extend-store!
                                                      id-set-diff
                                                      chi-top-module
                                                      set-module-binding-val!
                                                      set-module-binding-imps!
                                                      set-module-binding-label!
                                                      set-module-binding-id!
                                                      set-module-binding-type!
                                                      module-binding-val
                                                      module-binding-imps
                                                      module-binding-label
                                                      module-binding-id
                                                      module-binding-type
                                                      module-binding?
                                                      make-module-binding
                                                      make-resolved-interface
                                                      make-trimmed-interface
                                                      set-interface-token!
                                                      set-interface-exports!
                                                      interface-token
                                                      interface-exports
                                                      interface?
                                                      make-interface
                                                      flatten-exports
                                                      chi-top
                                                      chi-top-expr
                                                      syntax-type
                                                      chi-when-list
                                                      chi-top-sequence
                                                      chi-sequence
                                                      source-wrap
                                                      wrap
                                                      bound-id-member?
                                                      invalid-ids-error
                                                      distinct-bound-ids?
                                                      valid-bound-ids?
                                                      bound-id=?
                                                      literal-id=?
                                                      free-id=?
                                                      id-var-name
                                                      id-var-name-loc
                                                      id-var-name&marks
                                                      id-var-name-loc&marks
                                                      same-marks?
                                                      join-marks
                                                      join-wraps
                                                      smart-append
                                                      make-trimmed-syntax-object
                                                      make-binding-wrap
                                                      lookup-import-binding-name
                                                      extend-ribcage-subst!
                                                      extend-ribcage-barrier-help!
                                                      extend-ribcage-barrier!
                                                      extend-ribcage!
                                                      make-empty-ribcage
                                                      import-token-key
                                                      import-token?
                                                      make-import-token
                                                      barrier-marker
                                                      new-mark
                                                      anti-mark
                                                      the-anti-mark
                                                      only-top-marked?
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
                                                      set-indirect-label!
                                                      get-indirect-label
                                                      indirect-label?
                                                      gen-indirect-label
                                                      gen-labels
                                                      label?
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
                                                      sanitize-binding
                                                      lookup*
                                                      displaced-lexical-error
                                                      transformer-env
                                                      extend-var-env*
                                                      extend-env*
                                                      extend-env
                                                      null-env
                                                      binding?
                                                      set-binding-value!
                                                      set-binding-type!
                                                      binding-value
                                                      binding-type
                                                      make-binding
                                                      arg-check
                                                      source-annotation
                                                      no-source
                                                      unannotate
                                                      set-syntax-object-wrap!
                                                      set-syntax-object-expression!
                                                      syntax-object-wrap
                                                      syntax-object-expression
                                                      syntax-object?
                                                      make-syntax-object
                                                      self-evaluating?
                                                      build-lexical-var
                                                      build-letrec
                                                      build-sequence
                                                      build-data
                                                      build-primref
                                                      build-lambda
                                                      build-cte-install
                                                      build-module-definition
                                                      build-global-definition
                                                      build-global-assignment
                                                      build-global-reference
                                                      build-lexical-assignment
                                                      build-lexical-reference
                                                      build-conditional
                                                      build-application
                                                      generate-id
                                                      get-import-binding
                                                      get-global-definition-hook
                                                      put-global-definition-hook
                                                      gensym-hook
                                                      error-hook
                                                      local-eval-hook
                                                      top-level-eval-hook
                                                      annotation?
                                                      fx<
                                                      fx=
                                                      fx-
                                                      fx+
                                                      noexpand
                                                      define-structure
                                                      unless
                                                      when)
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
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
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
                                                    ("i" "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
                                                         "i"
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
                                                    ((import-token
                                                       .
                                                       *top*))
                                                    ()
                                                    ())
                                                  #(ribcage
                                                    ((import-token
                                                       .
                                                       *top*))
                                                    ()
                                                    ()))))
                                             '(())))
                                         g1779)
                                       ((lambda (g1784)
                                          (syntax-error
                                            (g394 g1758 g1755 g1757)))
                                        g1759)))
                                 ($syntax-dispatch g1759 '(any any)))))
                          ($syntax-dispatch
                            g1759
                            '(any (any . any) any . each-any)))))
                   ($syntax-dispatch g1759 '(any any any))))
                g1758)))
            (g441
             (lambda (g885 g882 g884 g883)
               ((lambda (g886)
                  ((lambda (g887)
                     (if (if g887
                             (apply (lambda (g889 g888) (g256 g888)) g887)
                             '#f)
                         (apply
                           (lambda (g891 g890) (g883 (g393 g890 g882)))
                           g887)
                         ((lambda (g892)
                            (syntax-error (g394 g885 g882 g884)))
                          g886)))
                   ($syntax-dispatch g886 '(any any))))
                g885)))
            (g440
             (lambda (g1723 g1719 g1722 g1720 g1721)
               (letrec ((g1725
                         (lambda (g1753 g1751 g1752)
                           (g1721
                             g1753
                             (g1724 g1751)
                             (map (lambda (g1754) (g393 g1754 g1720))
                                  g1752))))
                        (g1724
                         (lambda (g1745)
                           (if (null? g1745)
                               '()
                               (cons ((lambda (g1746)
                                        ((lambda (g1747)
                                           (if g1747
                                               (apply
                                                 (lambda (g1748)
                                                   (g1724 g1748))
                                                 g1747)
                                               ((lambda (g1750)
                                                  (if (g256 g1750)
                                                      (g393 g1750 g1720)
                                                      (syntax-error
                                                        (g394 g1723
                                                              g1719
                                                              g1722)
                                                        '"invalid exports list in")))
                                                g1746)))
                                         ($syntax-dispatch
                                           g1746
                                           'each-any)))
                                      (car g1745))
                                     (g1724 (cdr g1745)))))))
                 ((lambda (g1726)
                    ((lambda (g1727)
                       (if g1727
                           (apply
                             (lambda (g1730 g1728 g1729)
                               (g1725 '#f g1728 g1729))
                             g1727)
                           ((lambda (g1733)
                              (if (if g1733
                                      (apply
                                        (lambda (g1737 g1734 g1736 g1735)
                                          (g256 g1734))
                                        g1733)
                                      '#f)
                                  (apply
                                    (lambda (g1741 g1738 g1740 g1739)
                                      (g1725
                                        (g393 g1738 g1719)
                                        g1740
                                        g1739))
                                    g1733)
                                  ((lambda (g1744)
                                     (syntax-error
                                       (g394 g1723 g1719 g1722)))
                                   g1726)))
                            ($syntax-dispatch
                              g1726
                              '(any any each-any . each-any)))))
                     ($syntax-dispatch g1726 '(any each-any . each-any))))
                  g1723))))
            (g439
             (lambda (g894 g893)
               ((lambda (g895)
                  (if g895
                      (g366 g893 g895)
                      (g429 (lambda (g896)
                              ((lambda (g897)
                                 (begin (if (not g897)
                                            (syntax-error
                                              g896
                                              '"exported identifier not visible")
                                            (void))
                                        (g363 g893 g896 g897)))
                               (g376 g896 '(()))))
                            (g404 g894))))
                (g405 g894))))
            (g438
             (lambda (g1652 g1648 g1651 g1649 g1650)
               (letrec ((g1653
                         (lambda (g1718 g1714 g1717 g1715 g1716)
                           (begin (g426 g1648 g1714)
                                  (g1650 g1718 g1714 g1717 g1715 g1716)))))
                 ((letrec ((g1654
                            (lambda (g1659 g1655 g1658 g1656 g1657)
                              (if (null? g1659)
                                  (g1653 g1659 g1655 g1658 g1656 g1657)
                                  ((lambda (g1661 g1660)
                                     (call-with-values
                                       (lambda ()
                                         (g398 g1661
                                               g1660
                                               '(())
                                               '#f
                                               g1652))
                                       (lambda (g1666
                                                g1662
                                                g1665
                                                g1663
                                                g1664)
                                         ((lambda (g1667)
                                            (if (memv g1667 '(define-form))
                                                (g442 g1665
                                                      g1663
                                                      g1664
                                                      (lambda (g1670
                                                               g1668
                                                               g1669)
                                                        ((lambda (g1672
                                                                  g1671)
                                                           ((lambda (g1673)
                                                              (begin (g363 g1652
                                                                           g1672
                                                                           g1671)
                                                                     (g424 g1649
                                                                           g1671
                                                                           (g231 'lexical
                                                                                 g1673))
                                                                     (g1654
                                                                       (cdr g1659)
                                                                       (cons g1672
                                                                             g1655)
                                                                       (cons g1673
                                                                             g1658)
                                                                       (cons (cons g1660
                                                                                   (g393 g1668
                                                                                         g1669))
                                                                             g1656)
                                                                       g1657)))
                                                            (g451 g1672)))
                                                         (g393 g1670 g1669)
                                                         (g297))))
                                                (if (memv g1667
                                                          '(define-syntax-form))
                                                    (g443 g1665
                                                          g1663
                                                          g1664
                                                          (lambda (g1676
                                                                   g1674
                                                                   g1675)
                                                            ((lambda (g1679
                                                                      g1677
                                                                      g1678)
                                                               (begin (g363 g1652
                                                                            g1679
                                                                            g1677)
                                                                      (g424 g1649
                                                                            g1677
                                                                            (g231 'deferred
                                                                                  g1678))
                                                                      (g1654
                                                                        (cdr g1659)
                                                                        (cons g1679
                                                                              g1655)
                                                                        g1658
                                                                        g1656
                                                                        g1657)))
                                                             (g393 g1676
                                                                   g1675)
                                                             (g297)
                                                             (g432 g1674
                                                                   (g249 g1660)
                                                                   g1675))))
                                                    (if (memv g1667
                                                              '(module-form))
                                                        ((lambda (g1680)
                                                           ((lambda (g1681)
                                                              ((lambda ()
                                                                 (g440 g1665
                                                                       g1663
                                                                       g1664
                                                                       g1681
                                                                       (lambda (g1684
                                                                                g1682
                                                                                g1683)
                                                                         (g438 g1680
                                                                               (g394 g1665
                                                                                     g1663
                                                                                     g1664)
                                                                               (map (lambda (g1695)
                                                                                      (cons g1660
                                                                                            g1695))
                                                                                    g1683)
                                                                               g1649
                                                                               (lambda (g1689
                                                                                        g1685
                                                                                        g1688
                                                                                        g1686
                                                                                        g1687)
                                                                                 (begin (g425 g1648
                                                                                              (g401 g1682)
                                                                                              g1685)
                                                                                        ((lambda (g1693
                                                                                                  g1690
                                                                                                  g1692
                                                                                                  g1691)
                                                                                           (if g1684
                                                                                               ((lambda (g1694)
                                                                                                  (begin (g363 g1652
                                                                                                               g1684
                                                                                                               g1694)
                                                                                                         (g424 g1649
                                                                                                               g1694
                                                                                                               (g231 'module
                                                                                                                     g1693))
                                                                                                         (g1654
                                                                                                           (cdr g1659)
                                                                                                           (cons g1684
                                                                                                                 g1655)
                                                                                                           g1690
                                                                                                           g1692
                                                                                                           g1691)))
                                                                                                (g297))
                                                                                               ((lambda ()
                                                                                                  (begin (g439 g1693
                                                                                                               g1652)
                                                                                                         (g1654
                                                                                                           (cdr g1659)
                                                                                                           (cons g1693
                                                                                                                 g1655)
                                                                                                           g1690
                                                                                                           g1692
                                                                                                           g1691))))))
                                                                                         (g408 g1682)
                                                                                         (append
                                                                                           g1688
                                                                                           g1658)
                                                                                         (append
                                                                                           g1686
                                                                                           g1656)
                                                                                         (append
                                                                                           g1657
                                                                                           g1687
                                                                                           g1689))))))))))
                                                            (g263 (g264 g1663)
                                                                  (cons g1680
                                                                        (g265 g1663)))))
                                                         (g304 '()
                                                               '()
                                                               '()))
                                                        (if (memv g1667
                                                                  '(import-form))
                                                            (g441 g1665
                                                                  g1663
                                                                  g1664
                                                                  (lambda (g1696)
                                                                    ((lambda (g1697)
                                                                       ((lambda (g1698)
                                                                          ((lambda (g1699)
                                                                             (if (memv g1699
                                                                                       '(module))
                                                                                 ((lambda (g1700)
                                                                                    (begin (if g1662
                                                                                               (g364 g1652
                                                                                                     g1662)
                                                                                               (void))
                                                                                           (g439 g1700
                                                                                                 g1652)
                                                                                           (g1654
                                                                                             (cdr g1659)
                                                                                             (cons g1700
                                                                                                   g1655)
                                                                                             g1658
                                                                                             g1656
                                                                                             g1657)))
                                                                                  (cdr g1698))
                                                                                 (if (memv g1699
                                                                                           '(displaced-lexical))
                                                                                     (g250 g1696)
                                                                                     (syntax-error
                                                                                       g1696
                                                                                       '"import from unknown module"))))
                                                                           (car g1698)))
                                                                        (g253 g1697
                                                                              g1649)))
                                                                     (g377 g1696
                                                                           '(())))))
                                                            (if (memv g1667
                                                                      '(begin-form))
                                                                ((lambda (g1701)
                                                                   ((lambda (g1702)
                                                                      (if g1702
                                                                          (apply
                                                                            (lambda (g1704
                                                                                     g1703)
                                                                              (g1654
                                                                                ((letrec ((g1705
                                                                                           (lambda (g1706)
                                                                                             (if (null?
                                                                                                   g1706)
                                                                                                 (cdr g1659)
                                                                                                 (cons (cons g1660
                                                                                                             (g393 (car g1706)
                                                                                                                   g1663))
                                                                                                       (g1705
                                                                                                         (cdr g1706)))))))
                                                                                   g1705)
                                                                                 g1703)
                                                                                g1655
                                                                                g1658
                                                                                g1656
                                                                                g1657))
                                                                            g1702)
                                                                          (syntax-error
                                                                            g1701)))
                                                                    ($syntax-dispatch
                                                                      g1701
                                                                      '(any .
                                                                            each-any))))
                                                                 g1665)
                                                                (if (memv g1667
                                                                          '(local-syntax-form))
                                                                    (g445 g1662
                                                                          g1665
                                                                          g1660
                                                                          g1663
                                                                          g1664
                                                                          (lambda (g1711
                                                                                   g1708
                                                                                   g1710
                                                                                   g1709)
                                                                            (g1654
                                                                              ((letrec ((g1712
                                                                                         (lambda (g1713)
                                                                                           (if (null?
                                                                                                 g1713)
                                                                                               (cdr g1659)
                                                                                               (cons (cons g1708
                                                                                                           (g393 (car g1713)
                                                                                                                 g1710))
                                                                                                     (g1712
                                                                                                       (cdr g1713)))))))
                                                                                 g1712)
                                                                               g1711)
                                                                              g1655
                                                                              g1658
                                                                              g1656
                                                                              g1657)))
                                                                    (g1653
                                                                      (cons (cons g1660
                                                                                  (g394 g1665
                                                                                        g1663
                                                                                        g1664))
                                                                            (cdr g1659))
                                                                      g1655
                                                                      g1658
                                                                      g1656
                                                                      g1657))))))))
                                          g1666))))
                                   (cdar g1659)
                                   (caar g1659))))))
                    g1654)
                  g1651
                  '()
                  '()
                  '()
                  '()))))
            (g437
             (lambda (g901 g898 g900 g899)
               ((lambda (g902)
                  ((lambda (g903)
                     ((lambda (g904)
                        ((lambda (g905)
                           ((lambda ()
                              (g438 g903
                                    g898
                                    g905
                                    g902
                                    (lambda (g910 g906 g909 g907 g908)
                                      (begin (if (null? g910)
                                                 (syntax-error
                                                   g898
                                                   '"no expressions in body")
                                                 (void))
                                             (g191 '#f
                                                   g909
                                                   (map (lambda (g912)
                                                          (g432 (cdr g912)
                                                                (car g912)
                                                                '(())))
                                                        g907)
                                                   (g190 '#f
                                                         (map (lambda (g911)
                                                                (g432 (cdr g911)
                                                                      (car g911)
                                                                      '(())))
                                                              (append
                                                                g908
                                                                g910))))))))))
                         (map (lambda (g913) (cons g902 (g393 g913 g904)))
                              g901)))
                      (g263 (g264 g899) (cons g903 (g265 g899)))))
                   (g304 '() '() '())))
                (cons '("placeholder" placeholder) g900))))
            (g436
             (lambda (g1635 g1630 g1634 g1631 g1633 g1632)
               (letrec ((g1636
                         (lambda (g1640 g1639)
                           (if (pair? g1640)
                               (cons (g1636 (car g1640) g1639)
                                     (g1636 (cdr g1640) g1639))
                               (if (g204 g1640)
                                   ((lambda (g1641)
                                      ((lambda (g1643 g1642)
                                         (g203 (g205 g1640)
                                               (if (if (pair? g1643)
                                                       (eq? (car g1643)
                                                            '#f)
                                                       '#f)
                                                   (g263 (cdr g1643)
                                                         (if g1632
                                                             (cons g1632
                                                                   (cdr g1642))
                                                             (cdr g1642)))
                                                   (g263 (cons g1639 g1643)
                                                         (if g1632
                                                             (cons g1632
                                                                   (cons 'shift
                                                                         g1642))
                                                             (cons 'shift
                                                                   g1642))))))
                                       (g264 g1641)
                                       (g265 g1641)))
                                    (g206 g1640))
                                   (if (vector? g1640)
                                       ((lambda (g1644)
                                          ((lambda (g1645)
                                             ((lambda ()
                                                ((letrec ((g1646
                                                           (lambda (g1647)
                                                             (if (= g1647
                                                                    g1644)
                                                                 g1645
                                                                 (begin (vector-set!
                                                                          g1645
                                                                          g1647
                                                                          (g1636
                                                                            (vector-ref
                                                                              g1640
                                                                              g1647)
                                                                            g1639))
                                                                        (g1646
                                                                          (+ g1647
                                                                             '1)))))))
                                                   g1646)
                                                 '0))))
                                           (make-vector g1644)))
                                        (vector-length g1640))
                                       (if (symbol? g1640)
                                           (syntax-error
                                             (g394 g1630 g1631 g1633)
                                             '"encountered raw symbol "
                                             (format '"~s" g1640)
                                             '" in output of macro")
                                           g1640)))))))
                 (g1636
                   ((lambda (g1637)
                      (if (procedure? g1637)
                          (g1637
                            (lambda (g1638)
                              (begin (if (not (identifier? g1638))
                                         (syntax-error
                                           g1638
                                           '"environment argument is not an identifier")
                                         (void))
                                     (g253 (g377 g1638 '(())) g1634))))
                          g1637))
                    (g1635 (g394 g1630 (g349 g1631) g1633)))
                   (string '#\m)))))
            (g435
             (lambda (g918 g914 g917 g915 g916)
               ((lambda (g919)
                  ((lambda (g920)
                     (if (if g920
                             (apply
                               (lambda (g923 g921 g922) (g256 g921))
                               g920)
                             '#f)
                         (apply
                           (lambda (g926 g924 g925)
                             ((lambda (g927)
                                ((lambda (g928)
                                   ((lambda (g929)
                                      (if (memv g929 '(macro!))
                                          ((lambda (g931 g930)
                                             (g398 (g436 (g233 g928)
                                                         (list '#(syntax-object
                                                                  set!
                                                                  ((top)
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(id
                                                                       val)
                                                                     #((top)
                                                                       (top))
                                                                     #("i"
                                                                       "i"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(t)
                                                                     #(("m" top))
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
                                                                     #(b)
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
                                                                     #(n)
                                                                     #((top))
                                                                     #("i"))
                                                                   #(ribcage
                                                                     #(_
                                                                       id
                                                                       val)
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
                                                                       r
                                                                       w
                                                                       s
                                                                       rib)
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
                                                                     (lambda-var-list
                                                                       gen-var
                                                                       strip
                                                                       strip*
                                                                       strip-annotation
                                                                       ellipsis?
                                                                       chi-void
                                                                       chi-local-syntax
                                                                       chi-lambda-clause
                                                                       parse-define-syntax
                                                                       parse-define
                                                                       parse-import
                                                                       parse-module
                                                                       do-import!
                                                                       chi-internal
                                                                       chi-body
                                                                       chi-macro
                                                                       chi-set!
                                                                       chi-application
                                                                       chi-expr
                                                                       chi
                                                                       ct-eval/residualize
                                                                       do-top-import
                                                                       vfor-each
                                                                       vmap
                                                                       chi-external
                                                                       check-defined-ids
                                                                       check-module-exports
                                                                       extend-store!
                                                                       id-set-diff
                                                                       chi-top-module
                                                                       set-module-binding-val!
                                                                       set-module-binding-imps!
                                                                       set-module-binding-label!
                                                                       set-module-binding-id!
                                                                       set-module-binding-type!
                                                                       module-binding-val
                                                                       module-binding-imps
                                                                       module-binding-label
                                                                       module-binding-id
                                                                       module-binding-type
                                                                       module-binding?
                                                                       make-module-binding
                                                                       make-resolved-interface
                                                                       make-trimmed-interface
                                                                       set-interface-token!
                                                                       set-interface-exports!
                                                                       interface-token
                                                                       interface-exports
                                                                       interface?
                                                                       make-interface
                                                                       flatten-exports
                                                                       chi-top
                                                                       chi-top-expr
                                                                       syntax-type
                                                                       chi-when-list
                                                                       chi-top-sequence
                                                                       chi-sequence
                                                                       source-wrap
                                                                       wrap
                                                                       bound-id-member?
                                                                       invalid-ids-error
                                                                       distinct-bound-ids?
                                                                       valid-bound-ids?
                                                                       bound-id=?
                                                                       literal-id=?
                                                                       free-id=?
                                                                       id-var-name
                                                                       id-var-name-loc
                                                                       id-var-name&marks
                                                                       id-var-name-loc&marks
                                                                       same-marks?
                                                                       join-marks
                                                                       join-wraps
                                                                       smart-append
                                                                       make-trimmed-syntax-object
                                                                       make-binding-wrap
                                                                       lookup-import-binding-name
                                                                       extend-ribcage-subst!
                                                                       extend-ribcage-barrier-help!
                                                                       extend-ribcage-barrier!
                                                                       extend-ribcage!
                                                                       make-empty-ribcage
                                                                       import-token-key
                                                                       import-token?
                                                                       make-import-token
                                                                       barrier-marker
                                                                       new-mark
                                                                       anti-mark
                                                                       the-anti-mark
                                                                       only-top-marked?
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
                                                                       set-indirect-label!
                                                                       get-indirect-label
                                                                       indirect-label?
                                                                       gen-indirect-label
                                                                       gen-labels
                                                                       label?
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
                                                                       sanitize-binding
                                                                       lookup*
                                                                       displaced-lexical-error
                                                                       transformer-env
                                                                       extend-var-env*
                                                                       extend-env*
                                                                       extend-env
                                                                       null-env
                                                                       binding?
                                                                       set-binding-value!
                                                                       set-binding-type!
                                                                       binding-value
                                                                       binding-type
                                                                       make-binding
                                                                       arg-check
                                                                       source-annotation
                                                                       no-source
                                                                       unannotate
                                                                       set-syntax-object-wrap!
                                                                       set-syntax-object-expression!
                                                                       syntax-object-wrap
                                                                       syntax-object-expression
                                                                       syntax-object?
                                                                       make-syntax-object
                                                                       self-evaluating?
                                                                       build-lexical-var
                                                                       build-letrec
                                                                       build-sequence
                                                                       build-data
                                                                       build-primref
                                                                       build-lambda
                                                                       build-cte-install
                                                                       build-module-definition
                                                                       build-global-definition
                                                                       build-global-assignment
                                                                       build-global-reference
                                                                       build-lexical-assignment
                                                                       build-lexical-reference
                                                                       build-conditional
                                                                       build-application
                                                                       generate-id
                                                                       get-import-binding
                                                                       get-global-definition-hook
                                                                       put-global-definition-hook
                                                                       gensym-hook
                                                                       error-hook
                                                                       local-eval-hook
                                                                       top-level-eval-hook
                                                                       annotation?
                                                                       fx<
                                                                       fx=
                                                                       fx-
                                                                       fx+
                                                                       noexpand
                                                                       define-structure
                                                                       unless
                                                                       when)
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
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
                                                                      (top)
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
                                                                     ("i" "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
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
                                                                     ((import-token
                                                                        .
                                                                        *top*))
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     ((import-token
                                                                        .
                                                                        *top*))
                                                                     ()
                                                                     ())))
                                                               g931
                                                               g930)
                                                         g914
                                                         '(())
                                                         g915
                                                         g916)
                                                   g914
                                                   '(())
                                                   g915
                                                   g916))
                                           (g393 g924 g917)
                                           (g393 g925 g917))
                                          (values
                                            'core
                                            (lambda (g935 g932 g934 g933)
                                              ((lambda (g937 g936)
                                                 ((lambda (g938)
                                                    ((lambda (g939)
                                                       (if (memv g939
                                                                 '(lexical))
                                                           (list 'set!
                                                                 (g233 g938)
                                                                 g937)
                                                           (if (memv g939
                                                                     '(global))
                                                               (list 'set!
                                                                     (g233 g938)
                                                                     g937)
                                                               (if (memv g939
                                                                         '(displaced-lexical))
                                                                   (syntax-error
                                                                     (g393 g924
                                                                           g934)
                                                                     '"identifier out of context")
                                                                   (syntax-error
                                                                     (g394 g935
                                                                           g934
                                                                           g933))))))
                                                     (g232 g938)))
                                                  (g253 g936 g932)))
                                               (g432 g925 g932 g934)
                                               (g377 g924 g934)))
                                            g918
                                            g917
                                            g915)))
                                    (g232 g928)))
                                 (g253 g927 g914)))
                              (g377 g924 g917)))
                           g920)
                         ((lambda (g940)
                            (syntax-error (g394 g918 g917 g915)))
                          g919)))
                   ($syntax-dispatch g919 '(any any any))))
                g918)))
            (g434
             (lambda (g1622 g1618 g1621 g1619 g1620)
               ((lambda (g1623)
                  ((lambda (g1624)
                     (if g1624
                         (apply
                           (lambda (g1626 g1625)
                             (cons g1622
                                   (map (lambda (g1628)
                                          (g432 g1628 g1621 g1619))
                                        g1625)))
                           g1624)
                         ((lambda (g1629)
                            (syntax-error (g394 g1618 g1619 g1620)))
                          g1623)))
                   ($syntax-dispatch g1623 '(any . each-any))))
                g1618)))
            (g433
             (lambda (g946 g941 g945 g942 g944 g943)
               ((lambda (g947)
                  (if (memv g947 '(lexical))
                      g941
                      (if (memv g947 '(core))
                          (g941 g945 g942 g944 g943)
                          (if (memv g947 '(lexical-call))
                              (g434 g941 g945 g942 g944 g943)
                              (if (memv g947 '(constant))
                                  (list 'quote
                                        (g450 (g394 g945 g944 g943) '(())))
                                  (if (memv g947 '(global))
                                      g941
                                      (if (memv g947 '(call))
                                          (g434 (g432 (car g945) g942 g944)
                                                g945
                                                g942
                                                g944
                                                g943)
                                          (if (memv g947 '(begin-form))
                                              ((lambda (g948)
                                                 ((lambda (g949)
                                                    (if g949
                                                        (apply
                                                          (lambda (g952
                                                                   g950
                                                                   g951)
                                                            (g395 (cons g950
                                                                        g951)
                                                                  g942
                                                                  g944
                                                                  g943))
                                                          g949)
                                                        (syntax-error
                                                          g948)))
                                                  ($syntax-dispatch
                                                    g948
                                                    '(any any
                                                          .
                                                          each-any))))
                                               g945)
                                              (if (memv g947
                                                        '(local-syntax-form))
                                                  (g445 g941
                                                        g945
                                                        g942
                                                        g944
                                                        g943
                                                        g395)
                                                  (if (memv g947
                                                            '(eval-when-form))
                                                      ((lambda (g954)
                                                         ((lambda (g955)
                                                            (if g955
                                                                (apply
                                                                  (lambda (g959
                                                                           g956
                                                                           g958
                                                                           g957)
                                                                    ((lambda (g960)
                                                                       (if (memq 'eval
                                                                                 g960)
                                                                           (g395 (cons g958
                                                                                       g957)
                                                                                 g942
                                                                                 g944
                                                                                 g943)
                                                                           (g446)))
                                                                     (g397 g945
                                                                           g956
                                                                           g944)))
                                                                  g955)
                                                                (syntax-error
                                                                  g954)))
                                                          ($syntax-dispatch
                                                            g954
                                                            '(any each-any
                                                                  any
                                                                  .
                                                                  each-any))))
                                                       g945)
                                                      (if (memv g947
                                                                '(define-form
                                                                   define-syntax-form
                                                                   module-form
                                                                   import-form))
                                                          (syntax-error
                                                            (g394 g945
                                                                  g944
                                                                  g943)
                                                            '"invalid context for definition")
                                                          (if (memv g947
                                                                    '(syntax))
                                                              (syntax-error
                                                                (g394 g945
                                                                      g944
                                                                      g943)
                                                                '"reference to pattern variable outside syntax form")
                                                              (if (memv g947
                                                                        '(displaced-lexical))
                                                                  (g250 (g394 g945
                                                                              g944
                                                                              g943))
                                                                  (syntax-error
                                                                    (g394 g945
                                                                          g944
                                                                          g943)))))))))))))))
                g946)))
            (g432
             (lambda (g1612 g1610 g1611)
               (call-with-values
                 (lambda () (g398 g1612 g1610 g1611 '#f '#f))
                 (lambda (g1617 g1613 g1616 g1614 g1615)
                   (g433 g1617 g1613 g1616 g1610 g1614 g1615)))))
            (g431
             (lambda (g965 g963 g964)
               ((lambda (g966)
                  (if (memv g966 '(c))
                      (if (memq 'compile g963)
                          ((lambda (g967)
                             (begin (g91 g967)
                                    (if (memq 'load g963) g967 (g446))))
                           (g964))
                          (if (memq 'load g963) (g964) (g446)))
                      (if (memv g966 '(c&e))
                          ((lambda (g968) (begin (g91 g968) g968)) (g964))
                          (begin (if (memq 'eval g963) (g91 (g964)) (void))
                                 (g446)))))
                g965)))
            (g430
             (lambda (g1609 g1608)
               (list '$sc-put-cte
                     (list 'quote g1609)
                     (list 'quote (g231 'do-import g1608)))))
            (g429
             (lambda (g970 g969)
               ((lambda (g971)
                  ((letrec ((g972
                             (lambda (g973)
                               (if (not (= g973 g971))
                                   (begin (g970 (vector-ref g969 g973))
                                          (g972 (+ g973 '1)))
                                   (void)))))
                     g972)
                   '0))
                (vector-length g969))))
            (g428
             (lambda (g1604 g1603)
               ((letrec ((g1605
                          (lambda (g1607 g1606)
                            (if (< g1607 '0)
                                g1606
                                (g1605
                                  (- g1607 '1)
                                  (cons (g1604 (vector-ref g1603 g1607))
                                        g1606))))))
                  g1605)
                (- (vector-length g1603) '1)
                '())))
            (g427
             (lambda (g982 g974 g981 g975 g980 g976 g979 g977 g978)
               (letrec ((g985
                         (lambda (g1050 g1049)
                           ((lambda (g1051)
                              (map (lambda (g1052)
                                     ((lambda (g1053)
                                        (if (not (g392 g1053 g1051))
                                            g1052
                                            (g410 (g412 g1052)
                                                  g1053
                                                  (g414 g1052)
                                                  (append
                                                    (g984 g1053)
                                                    (g415 g1052))
                                                  (g416 g1052))))
                                      (g413 g1052)))
                                   g1050))
                            (map (lambda (g1054)
                                   (if (pair? g1054) (car g1054) g1054))
                                 g1049))))
                        (g984
                         (lambda (g1043)
                           ((letrec ((g1044
                                      (lambda (g1045)
                                        (if (null? g1045)
                                            '()
                                            (if (if (pair? (car g1045))
                                                    (g388 g1043
                                                          (caar g1045))
                                                    '#f)
                                                (g401 (cdar g1045))
                                                (g1044 (cdr g1045)))))))
                              g1044)
                            g980)))
                        (g983
                         (lambda (g1048 g1046 g1047)
                           (begin (g426 g974 g1046)
                                  (g425 g974 g976 g1046)
                                  (g978 g1048 g1047)))))
                 ((letrec ((g986
                            (lambda (g990 g987 g989 g988)
                              (if (null? g990)
                                  (g983 g989 g987 g988)
                                  ((lambda (g992 g991)
                                     (call-with-values
                                       (lambda ()
                                         (g398 g992 g991 '(()) '#f g982))
                                       (lambda (g997 g993 g996 g994 g995)
                                         ((lambda (g998)
                                            (if (memv g998 '(define-form))
                                                (g442 g996
                                                      g994
                                                      g995
                                                      (lambda (g1001
                                                               g999
                                                               g1000)
                                                        ((lambda (g1002)
                                                           ((lambda (g1003)
                                                              ((lambda (g1004)
                                                                 ((lambda ()
                                                                    (begin (g363 g982
                                                                                 g1002
                                                                                 g1003)
                                                                           (g986 (cdr g990)
                                                                                 (cons g1002
                                                                                       g987)
                                                                                 (cons (g410 g997
                                                                                             g1002
                                                                                             g1003
                                                                                             g1004
                                                                                             (cons g991
                                                                                                   (g393 g999
                                                                                                         g1000)))
                                                                                       g989)
                                                                                 g988)))))
                                                               (g984 g1002)))
                                                            (g300)))
                                                         (g393 g1001
                                                               g1000))))
                                                (if (memv g998
                                                          '(define-syntax-form))
                                                    (g443 g996
                                                          g994
                                                          g995
                                                          (lambda (g1007
                                                                   g1005
                                                                   g1006)
                                                            ((lambda (g1008)
                                                               ((lambda (g1009)
                                                                  ((lambda (g1010)
                                                                     ((lambda (g1011)
                                                                        ((lambda ()
                                                                           (begin (g424 g975
                                                                                        (g302 g1009)
                                                                                        (cons 'deferred
                                                                                              g1011))
                                                                                  (g363 g982
                                                                                        g1008
                                                                                        g1009)
                                                                                  (g986 (cdr g990)
                                                                                        (cons g1008
                                                                                              g987)
                                                                                        (cons (g410 g997
                                                                                                    g1008
                                                                                                    g1009
                                                                                                    g1010
                                                                                                    g1011)
                                                                                              g989)
                                                                                        g988)))))
                                                                      (g432 g1005
                                                                            (g249 g991)
                                                                            g1006)))
                                                                   (g984 g1008)))
                                                                (g300)))
                                                             (g393 g1007
                                                                   g1006))))
                                                    (if (memv g998
                                                              '(module-form))
                                                        ((lambda (g1012)
                                                           ((lambda (g1013)
                                                              ((lambda ()
                                                                 (g440 g996
                                                                       g994
                                                                       g995
                                                                       g1013
                                                                       (lambda (g1016
                                                                                g1014
                                                                                g1015)
                                                                         (g427 g1012
                                                                               (g394 g996
                                                                                     g994
                                                                                     g995)
                                                                               (map (lambda (g1024)
                                                                                      (cons g991
                                                                                            g1024))
                                                                                    g1015)
                                                                               g975
                                                                               g1014
                                                                               (g401 g1014)
                                                                               g979
                                                                               g977
                                                                               (lambda (g1018
                                                                                        g1017)
                                                                                 ((lambda (g1019)
                                                                                    ((lambda (g1020)
                                                                                       ((lambda (g1021)
                                                                                          ((lambda ()
                                                                                             (if g1016
                                                                                                 ((lambda (g1023
                                                                                                           g1022)
                                                                                                    (begin (g424 g975
                                                                                                                 (g302 g1023)
                                                                                                                 (g231 'module
                                                                                                                       g1019))
                                                                                                           (g363 g982
                                                                                                                 g1016
                                                                                                                 g1023)
                                                                                                           (g986 (cdr g990)
                                                                                                                 (cons g1016
                                                                                                                       g987)
                                                                                                                 (cons (g410 g997
                                                                                                                             g1016
                                                                                                                             g1023
                                                                                                                             g1022
                                                                                                                             g1014)
                                                                                                                       g1020)
                                                                                                                 g1021)))
                                                                                                  (g300)
                                                                                                  (g984 g1016))
                                                                                                 ((lambda ()
                                                                                                    (begin (g439 g1019
                                                                                                                 g982)
                                                                                                           (g986 (cdr g990)
                                                                                                                 (cons g1019
                                                                                                                       g987)
                                                                                                                 g1020
                                                                                                                 g1021))))))))
                                                                                        (append
                                                                                          g988
                                                                                          g1017)))
                                                                                     (append
                                                                                       (if g1016
                                                                                           g1018
                                                                                           (g985 g1018
                                                                                                 g1014))
                                                                                       g989)))
                                                                                  (g408 g1014)))))))))
                                                            (g263 (g264 g994)
                                                                  (cons g1012
                                                                        (g265 g994)))))
                                                         (g304 '()
                                                               '()
                                                               '()))
                                                        (if (memv g998
                                                                  '(import-form))
                                                            (g441 g996
                                                                  g994
                                                                  g995
                                                                  (lambda (g1025)
                                                                    ((lambda (g1026)
                                                                       ((lambda (g1027)
                                                                          ((lambda (g1028)
                                                                             (if (memv g1028
                                                                                       '(module))
                                                                                 ((lambda (g1029)
                                                                                    (begin (if g993
                                                                                               (g364 g982
                                                                                                     g993)
                                                                                               (void))
                                                                                           (g439 g1029
                                                                                                 g982)
                                                                                           (g986 (cdr g990)
                                                                                                 (cons g1029
                                                                                                       g987)
                                                                                                 (g985 g989
                                                                                                       (vector->list
                                                                                                         (g404 g1029)))
                                                                                                 g988)))
                                                                                  (g233 g1027))
                                                                                 (if (memv g1028
                                                                                           '(displaced-lexical))
                                                                                     (g250 g1025)
                                                                                     (syntax-error
                                                                                       g1025
                                                                                       '"import from unknown module"))))
                                                                           (g232 g1027)))
                                                                        (g253 g1026
                                                                              g975)))
                                                                     (g377 g1025
                                                                           '(())))))
                                                            (if (memv g998
                                                                      '(begin-form))
                                                                ((lambda (g1030)
                                                                   ((lambda (g1031)
                                                                      (if g1031
                                                                          (apply
                                                                            (lambda (g1033
                                                                                     g1032)
                                                                              (g986 ((letrec ((g1034
                                                                                               (lambda (g1035)
                                                                                                 (if (null?
                                                                                                       g1035)
                                                                                                     (cdr g990)
                                                                                                     (cons (cons g991
                                                                                                                 (g393 (car g1035)
                                                                                                                       g994))
                                                                                                           (g1034
                                                                                                             (cdr g1035)))))))
                                                                                       g1034)
                                                                                     g1032)
                                                                                    g987
                                                                                    g989
                                                                                    g988))
                                                                            g1031)
                                                                          (syntax-error
                                                                            g1030)))
                                                                    ($syntax-dispatch
                                                                      g1030
                                                                      '(any .
                                                                            each-any))))
                                                                 g996)
                                                                (if (memv g998
                                                                          '(local-syntax-form))
                                                                    (g445 g993
                                                                          g996
                                                                          g991
                                                                          g994
                                                                          g995
                                                                          (lambda (g1040
                                                                                   g1037
                                                                                   g1039
                                                                                   g1038)
                                                                            (g986 ((letrec ((g1041
                                                                                             (lambda (g1042)
                                                                                               (if (null?
                                                                                                     g1042)
                                                                                                   (cdr g990)
                                                                                                   (cons (cons g1037
                                                                                                               (g393 (car g1042)
                                                                                                                     g1039))
                                                                                                         (g1041
                                                                                                           (cdr g1042)))))))
                                                                                     g1041)
                                                                                   g1040)
                                                                                  g987
                                                                                  g989
                                                                                  g988)))
                                                                    (g983 g989
                                                                          g987
                                                                          (append
                                                                            g988
                                                                            (cons (cons g991
                                                                                        (g394 g996
                                                                                              g994
                                                                                              g995))
                                                                                  (cdr g990)))))))))))
                                          g997))))
                                   (cdar g990)
                                   (caar g990))))))
                    g986)
                  g981
                  '()
                  '()
                  '()))))
            (g426
             (lambda (g1560 g1559)
               (letrec ((g1564
                         (lambda (g1597 g1595 g1596)
                           ((lambda (g1598)
                              (if g1598
                                  (if (g367 ((lambda (g1599)
                                               ((lambda (g1600)
                                                  (if (g90 g1600)
                                                      (annotation-expression
                                                        g1600)
                                                      g1600))
                                                (if (g204 g1599)
                                                    (g205 g1599)
                                                    g1599)))
                                             g1597)
                                            g1598
                                            (if (symbol? g1597)
                                                (g264 '((top)))
                                                (g264 (g206 g1597))))
                                      (cons g1597 g1596)
                                      g1596)
                                  (g1562
                                    (g404 g1595)
                                    (lambda (g1602 g1601)
                                      (if (g1561 g1602 g1597)
                                          (cons g1602 g1601)
                                          g1601))
                                    g1596)))
                            (g405 g1595))))
                        (g1563
                         (lambda (g1575 g1573 g1574)
                           (if (g403 g1575)
                               (if (g403 g1573)
                                   (call-with-values
                                     (lambda ()
                                       ((lambda (g1581 g1580)
                                          (if (fx> (vector-length g1581)
                                                   (vector-length g1580))
                                              (values g1575 g1580)
                                              (values g1573 g1581)))
                                        (g404 g1575)
                                        (g404 g1573)))
                                     (lambda (g1577 g1576)
                                       (g1562
                                         g1576
                                         (lambda (g1579 g1578)
                                           (g1564 g1579 g1577 g1578))
                                         g1574)))
                                   (g1564 g1573 g1575 g1574))
                               (if (g403 g1573)
                                   (g1564 g1575 g1573 g1574)
                                   (if (g1561 g1575 g1573)
                                       (cons g1575 g1574)
                                       g1574)))))
                        (g1562
                         (lambda (g1590 g1588 g1589)
                           ((lambda (g1591)
                              ((letrec ((g1592
                                         (lambda (g1594 g1593)
                                           (if (= g1594 g1591)
                                               g1593
                                               (g1592
                                                 (+ g1594 '1)
                                                 (g1588
                                                   (vector-ref g1590 g1594)
                                                   g1593))))))
                                 g1592)
                               '0
                               g1589))
                            (vector-length g1590))))
                        (g1561
                         (lambda (g1583 g1582)
                           (if (symbol? g1583)
                               (if (symbol? g1582)
                                   (eq? g1583 g1582)
                                   (if (eq? g1583
                                            ((lambda (g1584)
                                               ((lambda (g1585)
                                                  (if (g90 g1585)
                                                      (annotation-expression
                                                        g1585)
                                                      g1585))
                                                (if (g204 g1584)
                                                    (g205 g1584)
                                                    g1584)))
                                             g1582))
                                       (g373 (g264 (g206 g1582))
                                             (g264 '((top))))
                                       '#f))
                               (if (symbol? g1582)
                                   (if (eq? g1582
                                            ((lambda (g1586)
                                               ((lambda (g1587)
                                                  (if (g90 g1587)
                                                      (annotation-expression
                                                        g1587)
                                                      g1587))
                                                (if (g204 g1586)
                                                    (g205 g1586)
                                                    g1586)))
                                             g1583))
                                       (g373 (g264 (g206 g1583))
                                             (g264 '((top))))
                                       '#f)
                                   (g388 g1583 g1582))))))
                 (if (not (null? g1559))
                     ((letrec ((g1565
                                (lambda (g1568 g1566 g1567)
                                  (if (null? g1566)
                                      (if (not (null? g1567))
                                          ((lambda (g1569)
                                             (syntax-error
                                               g1560
                                               '"duplicate definition for "
                                               (symbol->string (car g1569))
                                               '" in"))
                                           (syntax-object->datum g1567))
                                          (void))
                                      ((letrec ((g1570
                                                 (lambda (g1572 g1571)
                                                   (if (null? g1572)
                                                       (g1565
                                                         (car g1566)
                                                         (cdr g1566)
                                                         g1571)
                                                       (g1570
                                                         (cdr g1572)
                                                         (g1563
                                                           g1568
                                                           (car g1572)
                                                           g1571))))))
                                         g1570)
                                       g1566
                                       g1567)))))
                        g1565)
                      (car g1559)
                      (cdr g1559)
                      '())
                     (void)))))
            (g425
             (lambda (g1057 g1055 g1056)
               (letrec ((g1058
                         (lambda (g1065 g1064)
                           (ormap
                             (lambda (g1066)
                               (if (g403 g1066)
                                   ((lambda (g1067)
                                      (if g1067
                                          (g367 ((lambda (g1068)
                                                   ((lambda (g1069)
                                                      (if (g90 g1069)
                                                          (annotation-expression
                                                            g1069)
                                                          g1069))
                                                    (if (g204 g1068)
                                                        (g205 g1068)
                                                        g1068)))
                                                 g1065)
                                                g1067
                                                (g264 (g206 g1065)))
                                          ((lambda (g1070)
                                             ((letrec ((g1071
                                                        (lambda (g1072)
                                                          (if (fx>= g1072
                                                                    '0)
                                                              ((lambda (g1073)
                                                                 (if g1073
                                                                     g1073
                                                                     (g1071
                                                                       (- g1072
                                                                          '1))))
                                                               (g388 g1065
                                                                     (vector-ref
                                                                       g1070
                                                                       g1072)))
                                                              '#f))))
                                                g1071)
                                              (- (vector-length g1070)
                                                 '1)))
                                           (g404 g1066))))
                                    (g405 g1066))
                                   (g388 g1065 g1066)))
                             g1064))))
                 ((letrec ((g1059
                            (lambda (g1061 g1060)
                              (if (null? g1061)
                                  (if (not (null? g1060))
                                      (syntax-error
                                        g1060
                                        '"missing definition for export(s)")
                                      (void))
                                  ((lambda (g1063 g1062)
                                     (if (g1058 g1063 g1056)
                                         (g1059 g1062 g1060)
                                         (g1059 g1062 (cons g1063 g1060))))
                                   (car g1061)
                                   (cdr g1061))))))
                    g1059)
                  g1055
                  '()))))
            (g424
             (lambda (g1558 g1556 g1557)
               (set-cdr! g1558 (g246 g1556 g1557 (cdr g1558)))))
            (g423
             (lambda (g1075 g1074)
               (if (null? g1075)
                   '()
                   (if (g392 (car g1075) g1074)
                       (g423 (cdr g1075) g1074)
                       (cons (car g1075) (g423 (cdr g1075) g1074))))))
            (g422
             (lambda (g1491
                      g1482
                      g1490
                      g1483
                      g1489
                      g1484
                      g1488
                      g1485
                      g1487
                      g1486)
               ((lambda (g1492)
                  (g427 g1490
                        (g394 g1491 g1483 g1489)
                        (map (lambda (g1555) (cons g1482 g1555)) g1486)
                        g1482
                        g1487
                        g1492
                        g1484
                        g1488
                        (lambda (g1494 g1493)
                          ((letrec ((g1495
                                     (lambda (g1500
                                              g1496
                                              g1499
                                              g1497
                                              g1498)
                                       (if (null? g1500)
                                           ((letrec ((g1501
                                                      (lambda (g1504
                                                               g1502
                                                               g1503)
                                                        (if (null? g1504)
                                                            ((lambda (g1507
                                                                      g1505
                                                                      g1506)
                                                               (begin (for-each
                                                                        (lambda (g1523)
                                                                          (apply
                                                                            (lambda (g1527
                                                                                     g1524
                                                                                     g1526
                                                                                     g1525)
                                                                              (if g1524
                                                                                  (g303 g1524
                                                                                        g1526)
                                                                                  (void)))
                                                                            g1523))
                                                                        g1498)
                                                                      (g190 '#f
                                                                            (list (g431 g1484
                                                                                        g1488
                                                                                        (lambda ()
                                                                                          (if (null?
                                                                                                g1498)
                                                                                              (g446)
                                                                                              (g190 '#f
                                                                                                    (map (lambda (g1518)
                                                                                                           (apply
                                                                                                             (lambda (g1522
                                                                                                                      g1519
                                                                                                                      g1521
                                                                                                                      g1520)
                                                                                                               (list '$sc-put-cte
                                                                                                                     (list 'quote
                                                                                                                           g1521)
                                                                                                                     (if (eq? g1522
                                                                                                                              'define-syntax-form)
                                                                                                                         g1520
                                                                                                                         (list 'quote
                                                                                                                               (g231 'module
                                                                                                                                     (g409 g1520
                                                                                                                                           g1521))))))
                                                                                                             g1518))
                                                                                                         g1498)))))
                                                                                  (g431 g1484
                                                                                        g1488
                                                                                        (lambda ()
                                                                                          ((lambda (g1508)
                                                                                             ((lambda (g1509)
                                                                                                ((lambda (g1510)
                                                                                                   ((lambda ()
                                                                                                      (if g1508
                                                                                                          (list '$sc-put-cte
                                                                                                                (list 'quote
                                                                                                                      (if (g373 (g264 (g206 g1485))
                                                                                                                                (g264 '((top))))
                                                                                                                          g1508
                                                                                                                          ((lambda (g1511)
                                                                                                                             (g203 g1508
                                                                                                                                   (g263 g1511
                                                                                                                                         (list (g304 (vector
                                                                                                                                                       g1508)
                                                                                                                                                     (vector
                                                                                                                                                       g1511)
                                                                                                                                                     (vector
                                                                                                                                                       (g101 g1508)))))))
                                                                                                                           (g264 (g206 g1485)))))
                                                                                                                g1510)
                                                                                                          ((lambda (g1512)
                                                                                                             (g190 '#f
                                                                                                                   (list (list '$sc-put-cte
                                                                                                                               (list 'quote
                                                                                                                                     g1512)
                                                                                                                               g1510)
                                                                                                                         (g430 g1512
                                                                                                                               g1509))))
                                                                                                           (g101 'tmp))))))
                                                                                                 (list 'quote
                                                                                                       (g231 'module
                                                                                                             (g409 g1487
                                                                                                                   g1509)))))
                                                                                              (g101 g1508)))
                                                                                           (if g1485
                                                                                               ((lambda (g1513)
                                                                                                  ((lambda (g1514)
                                                                                                     (if (g90 g1514)
                                                                                                         (annotation-expression
                                                                                                           g1514)
                                                                                                         g1514))
                                                                                                   (if (g204 g1513)
                                                                                                       (g205 g1513)
                                                                                                       g1513)))
                                                                                                g1485)
                                                                                               '#f))))
                                                                                  (g190 '#f
                                                                                        (map (lambda (g1517)
                                                                                               (list 'define
                                                                                                     g1517
                                                                                                     (g446)))
                                                                                             g1499))
                                                                                  (g191 '#f
                                                                                        g1502
                                                                                        g1505
                                                                                        (g190 '#f
                                                                                              (list (if (null?
                                                                                                          g1499)
                                                                                                        (g446)
                                                                                                        (g190 '#f
                                                                                                              (map (lambda (g1516
                                                                                                                            g1515)
                                                                                                                     (list 'set!
                                                                                                                           g1516
                                                                                                                           g1515))
                                                                                                                   g1499
                                                                                                                   g1507)))
                                                                                                    (if (null?
                                                                                                          g1506)
                                                                                                        (g446)
                                                                                                        (g190 '#f
                                                                                                              g1506)))))
                                                                                  (g446)))))
                                                             (map (lambda (g1530)
                                                                    (g432 (cdr g1530)
                                                                          (car g1530)
                                                                          '(())))
                                                                  g1497)
                                                             (map (lambda (g1528)
                                                                    (g432 (cdr g1528)
                                                                          (car g1528)
                                                                          '(())))
                                                                  g1503)
                                                             (map (lambda (g1529)
                                                                    (g432 (cdr g1529)
                                                                          (car g1529)
                                                                          '(())))
                                                                  g1493))
                                                            ((lambda (g1531)
                                                               ((lambda (g1532)
                                                                  (if (memv g1532
                                                                            '(define-form))
                                                                      ((lambda (g1533)
                                                                         (begin (g424 g1482
                                                                                      (g302 (g414 g1531))
                                                                                      (g231 'lexical
                                                                                            g1533))
                                                                                (g1501
                                                                                  (cdr g1504)
                                                                                  (cons g1533
                                                                                        g1502)
                                                                                  (cons (g416 g1531)
                                                                                        g1503))))
                                                                       (g451 (g413 g1531)))
                                                                      (if (memv g1532
                                                                                '(define-syntax-form
                                                                                   module-form))
                                                                          (g1501
                                                                            (cdr g1504)
                                                                            g1502
                                                                            g1503)
                                                                          (error 'sc-expand-internal
                                                                            '"unexpected module binding type"))))
                                                                (g412 g1531)))
                                                             (car g1504))))))
                                              g1501)
                                            g1496
                                            '()
                                            '())
                                           ((lambda (g1535 g1534)
                                              (letrec ((g1536
                                                        (lambda (g1551
                                                                 g1548
                                                                 g1550
                                                                 g1549)
                                                          ((letrec ((g1552
                                                                     (lambda (g1554
                                                                              g1553)
                                                                       (if (null?
                                                                             g1554)
                                                                           (g1549)
                                                                           (if (g388 (g413 (car g1554))
                                                                                     g1551)
                                                                               (g1550
                                                                                 (car g1554)
                                                                                 (g370 (reverse
                                                                                         g1553)
                                                                                       (cdr g1554)))
                                                                               (g1552
                                                                                 (cdr g1554)
                                                                                 (cons (car g1554)
                                                                                       g1553)))))))
                                                             g1552)
                                                           g1548
                                                           '()))))
                                                (g1536
                                                  g1535
                                                  g1496
                                                  (lambda (g1538 g1537)
                                                    ((lambda (g1541
                                                              g1539
                                                              g1540)
                                                       ((lambda (g1543
                                                                 g1542)
                                                          ((lambda (g1544)
                                                             (if (memv g1544
                                                                       '(define-form))
                                                                 (begin (g303 g1539
                                                                              g1542)
                                                                        (g1495
                                                                          g1543
                                                                          g1537
                                                                          (cons g1542
                                                                                g1499)
                                                                          (cons (g416 g1538)
                                                                                g1497)
                                                                          g1498))
                                                                 (if (memv g1544
                                                                           '(define-syntax-form))
                                                                     (g1495
                                                                       g1543
                                                                       g1537
                                                                       g1499
                                                                       g1497
                                                                       (cons (list g1541
                                                                                   g1539
                                                                                   g1542
                                                                                   (g416 g1538))
                                                                             g1498))
                                                                     (if (memv g1544
                                                                               '(module-form))
                                                                         ((lambda (g1545)
                                                                            (g1495
                                                                              (append
                                                                                (g401 g1545)
                                                                                g1543)
                                                                              g1537
                                                                              g1499
                                                                              g1497
                                                                              (cons (list g1541
                                                                                          g1539
                                                                                          g1542
                                                                                          g1545)
                                                                                    g1498)))
                                                                          (g416 g1538))
                                                                         (error 'sc-expand-internal
                                                                           '"unexpected module binding type")))))
                                                           g1541))
                                                        (append
                                                          g1540
                                                          g1534)
                                                        (g101 ((lambda (g1546)
                                                                 ((lambda (g1547)
                                                                    (if (g90 g1547)
                                                                        (annotation-expression
                                                                          g1547)
                                                                        g1547))
                                                                  (if (g204 g1546)
                                                                      (g205 g1546)
                                                                      g1546)))
                                                               g1535))))
                                                     (g412 g1538)
                                                     (g414 g1538)
                                                     (g415 g1538)))
                                                  (lambda ()
                                                    (g1495
                                                      g1534
                                                      g1496
                                                      g1499
                                                      g1497
                                                      g1498)))))
                                            (car g1500)
                                            (cdr g1500))))))
                             g1495)
                           g1492
                           g1494
                           '()
                           '()
                           '()))))
                (g401 g1487))))
            (g421 (lambda (g1077 g1076) (vector-set! g1077 '5 g1076)))
            (g420 (lambda (g1481 g1480) (vector-set! g1481 '4 g1480)))
            (g419 (lambda (g1079 g1078) (vector-set! g1079 '3 g1078)))
            (g418 (lambda (g1479 g1478) (vector-set! g1479 '2 g1478)))
            (g417 (lambda (g1081 g1080) (vector-set! g1081 '1 g1080)))
            (g416 (lambda (g1477) (vector-ref g1477 '5)))
            (g415 (lambda (g1082) (vector-ref g1082 '4)))
            (g414 (lambda (g1476) (vector-ref g1476 '3)))
            (g413 (lambda (g1083) (vector-ref g1083 '2)))
            (g412 (lambda (g1475) (vector-ref g1475 '1)))
            (g411
             (lambda (g1084)
               (if (vector? g1084)
                   (if (= (vector-length g1084) '6)
                       (eq? (vector-ref g1084 '0) 'module-binding)
                       '#f)
                   '#f)))
            (g410
             (lambda (g1474 g1470 g1473 g1471 g1472)
               (vector 'module-binding g1474 g1470 g1473 g1471 g1472)))
            (g409
             (lambda (g1086 g1085)
               (g402 (list->vector
                       (map (lambda (g1087)
                              (g369 (if (pair? g1087) (car g1087) g1087)))
                            g1086))
                     g1085)))
            (g408
             (lambda (g1468)
               (g402 (list->vector
                       (map (lambda (g1469)
                              (if (pair? g1469) (car g1469) g1469))
                            g1468))
                     '#f)))
            (g407 (lambda (g1089 g1088) (vector-set! g1089 '2 g1088)))
            (g406 (lambda (g1467 g1466) (vector-set! g1467 '1 g1466)))
            (g405 (lambda (g1090) (vector-ref g1090 '2)))
            (g404 (lambda (g1465) (vector-ref g1465 '1)))
            (g403
             (lambda (g1091)
               (if (vector? g1091)
                   (if (= (vector-length g1091) '3)
                       (eq? (vector-ref g1091 '0) 'interface)
                       '#f)
                   '#f)))
            (g402
             (lambda (g1464 g1463) (vector 'interface g1464 g1463)))
            (g401
             (lambda (g1092)
               ((letrec ((g1093
                          (lambda (g1095 g1094)
                            (if (null? g1095)
                                g1094
                                (g1093
                                  (cdr g1095)
                                  (if (pair? (car g1095))
                                      (g1093 (car g1095) g1094)
                                      (cons (car g1095) g1094)))))))
                  g1093)
                g1092
                '())))
            (g400
             (lambda (g1390 g1385 g1389 g1386 g1388 g1387)
               (call-with-values
                 (lambda () (g398 g1390 g1385 g1389 '#f g1387))
                 (lambda (g1401 g1397 g1400 g1398 g1399)
                   ((lambda (g1402)
                      (if (memv g1402 '(begin-form))
                          ((lambda (g1403)
                             ((lambda (g1404)
                                (if g1404
                                    (apply (lambda (g1405) (g446)) g1404)
                                    ((lambda (g1406)
                                       (if g1406
                                           (apply
                                             (lambda (g1409 g1407 g1408)
                                               (g396 (cons g1407 g1408)
                                                     g1385
                                                     g1398
                                                     g1399
                                                     g1386
                                                     g1388
                                                     g1387))
                                             g1406)
                                           (syntax-error g1403)))
                                     ($syntax-dispatch
                                       g1403
                                       '(any any . each-any)))))
                              ($syntax-dispatch g1403 '(any))))
                           g1400)
                          (if (memv g1402 '(local-syntax-form))
                              (g445 g1397
                                    g1400
                                    g1385
                                    g1398
                                    g1399
                                    (lambda (g1414 g1411 g1413 g1412)
                                      (g396 g1414
                                            g1411
                                            g1413
                                            g1412
                                            g1386
                                            g1388
                                            g1387)))
                              (if (memv g1402 '(eval-when-form))
                                  ((lambda (g1415)
                                     ((lambda (g1416)
                                        (if g1416
                                            (apply
                                              (lambda (g1420
                                                       g1417
                                                       g1419
                                                       g1418)
                                                ((lambda (g1422 g1421)
                                                   (if (eq? g1386 'e)
                                                       (if (memq 'eval
                                                                 g1422)
                                                           (g396 g1421
                                                                 g1385
                                                                 g1398
                                                                 g1399
                                                                 'e
                                                                 '(eval)
                                                                 g1387)
                                                           (g446))
                                                       (if (memq 'load
                                                                 g1422)
                                                           (if ((lambda (g1423)
                                                                  (if g1423
                                                                      g1423
                                                                      (if (eq? g1386
                                                                               'c&e)
                                                                          (memq 'eval
                                                                                g1422)
                                                                          '#f)))
                                                                (memq 'compile
                                                                      g1422))
                                                               (g396 g1421
                                                                     g1385
                                                                     g1398
                                                                     g1399
                                                                     'c&e
                                                                     '(compile
                                                                        load)
                                                                     g1387)
                                                               (if (memq g1386
                                                                         '(c c&e))
                                                                   (g396 g1421
                                                                         g1385
                                                                         g1398
                                                                         g1399
                                                                         'c
                                                                         '(load)
                                                                         g1387)
                                                                   (g446)))
                                                           (if ((lambda (g1424)
                                                                  (if g1424
                                                                      g1424
                                                                      (if (eq? g1386
                                                                               'c&e)
                                                                          (memq 'eval
                                                                                g1422)
                                                                          '#f)))
                                                                (memq 'compile
                                                                      g1422))
                                                               (begin (g91 (g396 g1421
                                                                                 g1385
                                                                                 g1398
                                                                                 g1399
                                                                                 'e
                                                                                 '(eval)
                                                                                 g1387))
                                                                      (g446))
                                                               (g446)))))
                                                 (g397 g1400 g1417 g1398)
                                                 (cons g1419 g1418)))
                                              g1416)
                                            (syntax-error g1415)))
                                      ($syntax-dispatch
                                        g1415
                                        '(any each-any any . each-any))))
                                   g1400)
                                  (if (memv g1402 '(define-syntax-form))
                                      (g443 g1400
                                            g1398
                                            g1399
                                            (lambda (g1429 g1427 g1428)
                                              ((lambda (g1430)
                                                 (begin ((lambda (g1435)
                                                           ((lambda (g1436)
                                                              ((lambda (g1437)
                                                                 (if (memv g1437
                                                                           '(displaced-lexical))
                                                                     (g250 g1430)
                                                                     (void)))
                                                               (g232 g1436)))
                                                            (g253 g1435
                                                                  g1385)))
                                                         (g377 g1430
                                                               '(())))
                                                        (g431 g1386
                                                              g1388
                                                              (lambda ()
                                                                (list '$sc-put-cte
                                                                      (list 'quote
                                                                            ((lambda (g1431)
                                                                               (if (g373 (g264 (g206 g1430))
                                                                                         (g264 '((top))))
                                                                                   g1431
                                                                                   ((lambda (g1432)
                                                                                      (g203 g1431
                                                                                            (g263 g1432
                                                                                                  (list (g304 (vector
                                                                                                                g1431)
                                                                                                              (vector
                                                                                                                g1432)
                                                                                                              (vector
                                                                                                                (g101 g1431)))))))
                                                                                    (g264 (g206 g1430)))))
                                                                             ((lambda (g1433)
                                                                                ((lambda (g1434)
                                                                                   (if (g90 g1434)
                                                                                       (annotation-expression
                                                                                         g1434)
                                                                                       g1434))
                                                                                 (if (g204 g1433)
                                                                                     (g205 g1433)
                                                                                     g1433)))
                                                                              g1430)))
                                                                      (g432 g1427
                                                                            (g249 g1385)
                                                                            g1428))))))
                                               (g393 g1429 g1428))))
                                      (if (memv g1402 '(define-form))
                                          (g442 g1400
                                                g1398
                                                g1399
                                                (lambda (g1440 g1438 g1439)
                                                  ((lambda (g1441)
                                                     (begin ((lambda (g1448)
                                                               ((lambda (g1449)
                                                                  ((lambda (g1450)
                                                                     (if (memv g1450
                                                                               '(displaced-lexical))
                                                                         (g250 g1441)
                                                                         (void)))
                                                                   (g232 g1449)))
                                                                (g253 g1448
                                                                      g1385)))
                                                             (g377 g1441
                                                                   '(())))
                                                            ((lambda (g1442)
                                                               ((lambda (g1443)
                                                                  (g190 '#f
                                                                        (list (g431 g1386
                                                                                    g1388
                                                                                    (lambda ()
                                                                                      (list '$sc-put-cte
                                                                                            (list 'quote
                                                                                                  (if (eq? g1442
                                                                                                           g1443)
                                                                                                      g1442
                                                                                                      ((lambda (g1445)
                                                                                                         (g203 g1442
                                                                                                               (g263 g1445
                                                                                                                     (list (g304 (vector
                                                                                                                                   g1442)
                                                                                                                                 (vector
                                                                                                                                   g1445)
                                                                                                                                 (vector
                                                                                                                                   g1443))))))
                                                                                                       (g264 (g206 g1441)))))
                                                                                            (list 'quote
                                                                                                  (g231 'global
                                                                                                        g1443)))))
                                                                              ((lambda (g1444)
                                                                                 (begin (if (eq? g1386
                                                                                                 'c&e)
                                                                                            (g91 g1444)
                                                                                            (void))
                                                                                        g1444))
                                                                               (list 'define
                                                                                     g1443
                                                                                     (g432 g1438
                                                                                           g1385
                                                                                           g1439))))))
                                                                (if (g373 (g264 (g206 g1441))
                                                                          (g264 '((top))))
                                                                    g1442
                                                                    (g101 g1442))))
                                                             ((lambda (g1446)
                                                                ((lambda (g1447)
                                                                   (if (g90 g1447)
                                                                       (annotation-expression
                                                                         g1447)
                                                                       g1447))
                                                                 (if (g204 g1446)
                                                                     (g205 g1446)
                                                                     g1446)))
                                                              g1441))))
                                                   (g393 g1440 g1439))))
                                          (if (memv g1402 '(module-form))
                                              ((lambda (g1452 g1451)
                                                 (g440 g1400
                                                       g1398
                                                       g1399
                                                       (g263 (g264 g1398)
                                                             (cons g1451
                                                                   (g265 g1398)))
                                                       (lambda (g1455
                                                                g1453
                                                                g1454)
                                                         (if g1455
                                                             (begin ((lambda (g1456)
                                                                       ((lambda (g1457)
                                                                          ((lambda (g1458)
                                                                             (if (memv g1458
                                                                                       '(displaced-lexical))
                                                                                 (g250 (g393 g1455
                                                                                             g1398))
                                                                                 (void)))
                                                                           (g232 g1457)))
                                                                        (g253 g1456
                                                                              g1452)))
                                                                     (g377 g1455
                                                                           '(())))
                                                                    (g422 g1400
                                                                          g1452
                                                                          g1451
                                                                          g1398
                                                                          g1399
                                                                          g1386
                                                                          g1388
                                                                          g1455
                                                                          g1453
                                                                          g1454))
                                                             (g422 g1400
                                                                   g1452
                                                                   g1451
                                                                   g1398
                                                                   g1399
                                                                   g1386
                                                                   g1388
                                                                   '#f
                                                                   g1453
                                                                   g1454)))))
                                               (cons '("top-level module placeholder"
                                                        placeholder)
                                                     g1385)
                                               (g304 '() '() '()))
                                              (if (memv g1402
                                                        '(import-form))
                                                  (g441 g1400
                                                        g1398
                                                        g1399
                                                        (lambda (g1459)
                                                          (g431 g1386
                                                                g1388
                                                                (lambda ()
                                                                  (begin (if g1397
                                                                             (syntax-error
                                                                               (g394 g1400
                                                                                     g1398
                                                                                     g1399)
                                                                               '"not valid at top-level")
                                                                             (void))
                                                                         ((lambda (g1460)
                                                                            ((lambda (g1461)
                                                                               (if (memv g1461
                                                                                         '(module))
                                                                                   (g430 g1459
                                                                                         (g405 (g233 g1460)))
                                                                                   (if (memv g1461
                                                                                             '(displaced-lexical))
                                                                                       (g250 g1459)
                                                                                       (syntax-error
                                                                                         g1459
                                                                                         '"import from unknown module"))))
                                                                             (g232 g1460)))
                                                                          (g253 (g377 g1459
                                                                                      '(()))
                                                                                '())))))))
                                                  ((lambda (g1462)
                                                     (begin (if (eq? g1386
                                                                     'c&e)
                                                                (g91 g1462)
                                                                (void))
                                                            g1462))
                                                   (g433 g1401
                                                         g1397
                                                         g1400
                                                         g1385
                                                         g1398
                                                         g1399))))))))))
                    g1401)))))
            (g399
             (lambda (g1099 g1096 g1098 g1097)
               (call-with-values
                 (lambda () (g398 g1099 g1096 g1098 '#f g1097))
                 (lambda (g1104 g1100 g1103 g1101 g1102)
                   (g433 g1104 g1100 g1103 g1096 g1101 g1102)))))
            (g398
             (lambda (g1370 g1366 g1369 g1367 g1368)
               (if (symbol? g1370)
                   ((lambda (g1371)
                      ((lambda (g1372)
                         ((lambda (g1373)
                            ((lambda ()
                               ((lambda (g1374)
                                  (if (memv g1374 '(lexical))
                                      (values
                                        g1373
                                        (g233 g1372)
                                        g1370
                                        g1369
                                        g1367)
                                      (if (memv g1374 '(global))
                                          (values
                                            g1373
                                            (g233 g1372)
                                            g1370
                                            g1369
                                            g1367)
                                          (if (memv g1374 '(macro macro!))
                                              (g398 (g436 (g233 g1372)
                                                          g1370
                                                          g1366
                                                          g1369
                                                          g1367
                                                          g1368)
                                                    g1366
                                                    '(())
                                                    '#f
                                                    g1368)
                                              (values
                                                g1373
                                                (g233 g1372)
                                                g1370
                                                g1369
                                                g1367)))))
                                g1373))))
                          (g232 g1372)))
                       (g253 g1371 g1366)))
                    (g377 g1370 g1369))
                   (if (pair? g1370)
                       ((lambda (g1375)
                          (if (g256 g1375)
                              ((lambda (g1376)
                                 ((lambda (g1377)
                                    ((lambda (g1378)
                                       ((lambda ()
                                          ((lambda (g1379)
                                             (if (memv g1379 '(lexical))
                                                 (values
                                                   'lexical-call
                                                   (g233 g1377)
                                                   g1370
                                                   g1369
                                                   g1367)
                                                 (if (memv g1379
                                                           '(macro macro!))
                                                     (g398 (g436 (g233 g1377)
                                                                 g1370
                                                                 g1366
                                                                 g1369
                                                                 g1367
                                                                 g1368)
                                                           g1366
                                                           '(())
                                                           '#f
                                                           g1368)
                                                     (if (memv g1379
                                                               '(core))
                                                         (values
                                                           g1378
                                                           (g233 g1377)
                                                           g1370
                                                           g1369
                                                           g1367)
                                                         (if (memv g1379
                                                                   '(local-syntax))
                                                             (values
                                                               'local-syntax-form
                                                               (g233 g1377)
                                                               g1370
                                                               g1369
                                                               g1367)
                                                             (if (memv g1379
                                                                       '(begin))
                                                                 (values
                                                                   'begin-form
                                                                   '#f
                                                                   g1370
                                                                   g1369
                                                                   g1367)
                                                                 (if (memv g1379
                                                                           '(eval-when))
                                                                     (values
                                                                       'eval-when-form
                                                                       '#f
                                                                       g1370
                                                                       g1369
                                                                       g1367)
                                                                     (if (memv g1379
                                                                               '(define))
                                                                         (values
                                                                           'define-form
                                                                           '#f
                                                                           g1370
                                                                           g1369
                                                                           g1367)
                                                                         (if (memv g1379
                                                                                   '(define-syntax))
                                                                             (values
                                                                               'define-syntax-form
                                                                               '#f
                                                                               g1370
                                                                               g1369
                                                                               g1367)
                                                                             (if (memv g1379
                                                                                       '(module-key))
                                                                                 (values
                                                                                   'module-form
                                                                                   '#f
                                                                                   g1370
                                                                                   g1369
                                                                                   g1367)
                                                                                 (if (memv g1379
                                                                                           '(import))
                                                                                     (values
                                                                                       'import-form
                                                                                       (if (g233 g1377)
                                                                                           (g393 g1375
                                                                                                 g1369)
                                                                                           '#f)
                                                                                       g1370
                                                                                       g1369
                                                                                       g1367)
                                                                                     (if (memv g1379
                                                                                               '(set!))
                                                                                         (g435 g1370
                                                                                               g1366
                                                                                               g1369
                                                                                               g1367
                                                                                               g1368)
                                                                                         (values
                                                                                           'call
                                                                                           '#f
                                                                                           g1370
                                                                                           g1369
                                                                                           g1367)))))))))))))
                                           g1378))))
                                     (g232 g1377)))
                                  (g253 g1376 g1366)))
                               (g377 g1375 g1369))
                              (values 'call '#f g1370 g1369 g1367)))
                        (car g1370))
                       (if (g204 g1370)
                           (g398 (g205 g1370)
                                 g1366
                                 (g371 g1369 (g206 g1370))
                                 '#f
                                 g1368)
                           (if (g90 g1370)
                               (g398 (annotation-expression g1370)
                                     g1366
                                     g1369
                                     (annotation-source g1370)
                                     g1368)
                               (if ((lambda (g1380)
                                      ((lambda (g1381)
                                         (if g1381
                                             g1381
                                             ((lambda (g1382)
                                                (if g1382
                                                    g1382
                                                    ((lambda (g1383)
                                                       (if g1383
                                                           g1383
                                                           ((lambda (g1384)
                                                              (if g1384
                                                                  g1384
                                                                  (null?
                                                                    g1380)))
                                                            (char?
                                                              g1380))))
                                                     (string? g1380))))
                                              (number? g1380))))
                                       (boolean? g1380)))
                                    g1370)
                                   (values 'constant '#f g1370 g1369 g1367)
                                   (values
                                     'other
                                     '#f
                                     g1370
                                     g1369
                                     g1367))))))))
            (g397
             (lambda (g1107 g1105 g1106)
               ((letrec ((g1108
                          (lambda (g1110 g1109)
                            (if (null? g1110)
                                g1109
                                (g1108
                                  (cdr g1110)
                                  (cons ((lambda (g1111)
                                           (if (g378 g1111
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
                                                           #(x)
                                                           #((top))
                                                           #("i"))
                                                         #(ribcage
                                                           ()
                                                           ()
                                                           ())
                                                         #(ribcage
                                                           #(when-list
                                                             situations)
                                                           #((top) (top))
                                                           #("i" "i"))
                                                         #(ribcage
                                                           #(f)
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
                                                           #(e when-list w)
                                                           #((top)
                                                             (top)
                                                             (top))
                                                           #("i" "i" "i"))
                                                         #(ribcage
                                                           (lambda-var-list
                                                             gen-var
                                                             strip
                                                             strip*
                                                             strip-annotation
                                                             ellipsis?
                                                             chi-void
                                                             chi-local-syntax
                                                             chi-lambda-clause
                                                             parse-define-syntax
                                                             parse-define
                                                             parse-import
                                                             parse-module
                                                             do-import!
                                                             chi-internal
                                                             chi-body
                                                             chi-macro
                                                             chi-set!
                                                             chi-application
                                                             chi-expr
                                                             chi
                                                             ct-eval/residualize
                                                             do-top-import
                                                             vfor-each
                                                             vmap
                                                             chi-external
                                                             check-defined-ids
                                                             check-module-exports
                                                             extend-store!
                                                             id-set-diff
                                                             chi-top-module
                                                             set-module-binding-val!
                                                             set-module-binding-imps!
                                                             set-module-binding-label!
                                                             set-module-binding-id!
                                                             set-module-binding-type!
                                                             module-binding-val
                                                             module-binding-imps
                                                             module-binding-label
                                                             module-binding-id
                                                             module-binding-type
                                                             module-binding?
                                                             make-module-binding
                                                             make-resolved-interface
                                                             make-trimmed-interface
                                                             set-interface-token!
                                                             set-interface-exports!
                                                             interface-token
                                                             interface-exports
                                                             interface?
                                                             make-interface
                                                             flatten-exports
                                                             chi-top
                                                             chi-top-expr
                                                             syntax-type
                                                             chi-when-list
                                                             chi-top-sequence
                                                             chi-sequence
                                                             source-wrap
                                                             wrap
                                                             bound-id-member?
                                                             invalid-ids-error
                                                             distinct-bound-ids?
                                                             valid-bound-ids?
                                                             bound-id=?
                                                             literal-id=?
                                                             free-id=?
                                                             id-var-name
                                                             id-var-name-loc
                                                             id-var-name&marks
                                                             id-var-name-loc&marks
                                                             same-marks?
                                                             join-marks
                                                             join-wraps
                                                             smart-append
                                                             make-trimmed-syntax-object
                                                             make-binding-wrap
                                                             lookup-import-binding-name
                                                             extend-ribcage-subst!
                                                             extend-ribcage-barrier-help!
                                                             extend-ribcage-barrier!
                                                             extend-ribcage!
                                                             make-empty-ribcage
                                                             import-token-key
                                                             import-token?
                                                             make-import-token
                                                             barrier-marker
                                                             new-mark
                                                             anti-mark
                                                             the-anti-mark
                                                             only-top-marked?
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
                                                             set-indirect-label!
                                                             get-indirect-label
                                                             indirect-label?
                                                             gen-indirect-label
                                                             gen-labels
                                                             label?
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
                                                             sanitize-binding
                                                             lookup*
                                                             displaced-lexical-error
                                                             transformer-env
                                                             extend-var-env*
                                                             extend-env*
                                                             extend-env
                                                             null-env
                                                             binding?
                                                             set-binding-value!
                                                             set-binding-type!
                                                             binding-value
                                                             binding-type
                                                             make-binding
                                                             arg-check
                                                             source-annotation
                                                             no-source
                                                             unannotate
                                                             set-syntax-object-wrap!
                                                             set-syntax-object-expression!
                                                             syntax-object-wrap
                                                             syntax-object-expression
                                                             syntax-object?
                                                             make-syntax-object
                                                             self-evaluating?
                                                             build-lexical-var
                                                             build-letrec
                                                             build-sequence
                                                             build-data
                                                             build-primref
                                                             build-lambda
                                                             build-cte-install
                                                             build-module-definition
                                                             build-global-definition
                                                             build-global-assignment
                                                             build-global-reference
                                                             build-lexical-assignment
                                                             build-lexical-reference
                                                             build-conditional
                                                             build-application
                                                             generate-id
                                                             get-import-binding
                                                             get-global-definition-hook
                                                             put-global-definition-hook
                                                             gensym-hook
                                                             error-hook
                                                             local-eval-hook
                                                             top-level-eval-hook
                                                             annotation?
                                                             fx<
                                                             fx=
                                                             fx-
                                                             fx+
                                                             noexpand
                                                             define-structure
                                                             unless
                                                             when)
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
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
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
                                                           ("i" "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
                                                                "i"
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
                                                           ((import-token
                                                              .
                                                              *top*))
                                                           ()
                                                           ())
                                                         #(ribcage
                                                           ((import-token
                                                              .
                                                              *top*))
                                                           ()
                                                           ()))))
                                               'compile
                                               (if (g378 g1111
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
                                                               #(x)
                                                               #((top))
                                                               #("i"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(when-list
                                                                 situations)
                                                               #((top)
                                                                 (top))
                                                               #("i" "i"))
                                                             #(ribcage
                                                               #(f)
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
                                                                 strip*
                                                                 strip-annotation
                                                                 ellipsis?
                                                                 chi-void
                                                                 chi-local-syntax
                                                                 chi-lambda-clause
                                                                 parse-define-syntax
                                                                 parse-define
                                                                 parse-import
                                                                 parse-module
                                                                 do-import!
                                                                 chi-internal
                                                                 chi-body
                                                                 chi-macro
                                                                 chi-set!
                                                                 chi-application
                                                                 chi-expr
                                                                 chi
                                                                 ct-eval/residualize
                                                                 do-top-import
                                                                 vfor-each
                                                                 vmap
                                                                 chi-external
                                                                 check-defined-ids
                                                                 check-module-exports
                                                                 extend-store!
                                                                 id-set-diff
                                                                 chi-top-module
                                                                 set-module-binding-val!
                                                                 set-module-binding-imps!
                                                                 set-module-binding-label!
                                                                 set-module-binding-id!
                                                                 set-module-binding-type!
                                                                 module-binding-val
                                                                 module-binding-imps
                                                                 module-binding-label
                                                                 module-binding-id
                                                                 module-binding-type
                                                                 module-binding?
                                                                 make-module-binding
                                                                 make-resolved-interface
                                                                 make-trimmed-interface
                                                                 set-interface-token!
                                                                 set-interface-exports!
                                                                 interface-token
                                                                 interface-exports
                                                                 interface?
                                                                 make-interface
                                                                 flatten-exports
                                                                 chi-top
                                                                 chi-top-expr
                                                                 syntax-type
                                                                 chi-when-list
                                                                 chi-top-sequence
                                                                 chi-sequence
                                                                 source-wrap
                                                                 wrap
                                                                 bound-id-member?
                                                                 invalid-ids-error
                                                                 distinct-bound-ids?
                                                                 valid-bound-ids?
                                                                 bound-id=?
                                                                 literal-id=?
                                                                 free-id=?
                                                                 id-var-name
                                                                 id-var-name-loc
                                                                 id-var-name&marks
                                                                 id-var-name-loc&marks
                                                                 same-marks?
                                                                 join-marks
                                                                 join-wraps
                                                                 smart-append
                                                                 make-trimmed-syntax-object
                                                                 make-binding-wrap
                                                                 lookup-import-binding-name
                                                                 extend-ribcage-subst!
                                                                 extend-ribcage-barrier-help!
                                                                 extend-ribcage-barrier!
                                                                 extend-ribcage!
                                                                 make-empty-ribcage
                                                                 import-token-key
                                                                 import-token?
                                                                 make-import-token
                                                                 barrier-marker
                                                                 new-mark
                                                                 anti-mark
                                                                 the-anti-mark
                                                                 only-top-marked?
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
                                                                 set-indirect-label!
                                                                 get-indirect-label
                                                                 indirect-label?
                                                                 gen-indirect-label
                                                                 gen-labels
                                                                 label?
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
                                                                 sanitize-binding
                                                                 lookup*
                                                                 displaced-lexical-error
                                                                 transformer-env
                                                                 extend-var-env*
                                                                 extend-env*
                                                                 extend-env
                                                                 null-env
                                                                 binding?
                                                                 set-binding-value!
                                                                 set-binding-type!
                                                                 binding-value
                                                                 binding-type
                                                                 make-binding
                                                                 arg-check
                                                                 source-annotation
                                                                 no-source
                                                                 unannotate
                                                                 set-syntax-object-wrap!
                                                                 set-syntax-object-expression!
                                                                 syntax-object-wrap
                                                                 syntax-object-expression
                                                                 syntax-object?
                                                                 make-syntax-object
                                                                 self-evaluating?
                                                                 build-lexical-var
                                                                 build-letrec
                                                                 build-sequence
                                                                 build-data
                                                                 build-primref
                                                                 build-lambda
                                                                 build-cte-install
                                                                 build-module-definition
                                                                 build-global-definition
                                                                 build-global-assignment
                                                                 build-global-reference
                                                                 build-lexical-assignment
                                                                 build-lexical-reference
                                                                 build-conditional
                                                                 build-application
                                                                 generate-id
                                                                 get-import-binding
                                                                 get-global-definition-hook
                                                                 put-global-definition-hook
                                                                 gensym-hook
                                                                 error-hook
                                                                 local-eval-hook
                                                                 top-level-eval-hook
                                                                 annotation?
                                                                 fx<
                                                                 fx=
                                                                 fx-
                                                                 fx+
                                                                 noexpand
                                                                 define-structure
                                                                 unless
                                                                 when)
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
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
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
                                                               ("i" "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
                                                                    "i"
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
                                                               ((import-token
                                                                  .
                                                                  *top*))
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               ((import-token
                                                                  .
                                                                  *top*))
                                                               ()
                                                               ()))))
                                                   'load
                                                   (if (g378 g1111
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
                                                                   #(x)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(when-list
                                                                     situations)
                                                                   #((top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   #(f)
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
                                                                     strip*
                                                                     strip-annotation
                                                                     ellipsis?
                                                                     chi-void
                                                                     chi-local-syntax
                                                                     chi-lambda-clause
                                                                     parse-define-syntax
                                                                     parse-define
                                                                     parse-import
                                                                     parse-module
                                                                     do-import!
                                                                     chi-internal
                                                                     chi-body
                                                                     chi-macro
                                                                     chi-set!
                                                                     chi-application
                                                                     chi-expr
                                                                     chi
                                                                     ct-eval/residualize
                                                                     do-top-import
                                                                     vfor-each
                                                                     vmap
                                                                     chi-external
                                                                     check-defined-ids
                                                                     check-module-exports
                                                                     extend-store!
                                                                     id-set-diff
                                                                     chi-top-module
                                                                     set-module-binding-val!
                                                                     set-module-binding-imps!
                                                                     set-module-binding-label!
                                                                     set-module-binding-id!
                                                                     set-module-binding-type!
                                                                     module-binding-val
                                                                     module-binding-imps
                                                                     module-binding-label
                                                                     module-binding-id
                                                                     module-binding-type
                                                                     module-binding?
                                                                     make-module-binding
                                                                     make-resolved-interface
                                                                     make-trimmed-interface
                                                                     set-interface-token!
                                                                     set-interface-exports!
                                                                     interface-token
                                                                     interface-exports
                                                                     interface?
                                                                     make-interface
                                                                     flatten-exports
                                                                     chi-top
                                                                     chi-top-expr
                                                                     syntax-type
                                                                     chi-when-list
                                                                     chi-top-sequence
                                                                     chi-sequence
                                                                     source-wrap
                                                                     wrap
                                                                     bound-id-member?
                                                                     invalid-ids-error
                                                                     distinct-bound-ids?
                                                                     valid-bound-ids?
                                                                     bound-id=?
                                                                     literal-id=?
                                                                     free-id=?
                                                                     id-var-name
                                                                     id-var-name-loc
                                                                     id-var-name&marks
                                                                     id-var-name-loc&marks
                                                                     same-marks?
                                                                     join-marks
                                                                     join-wraps
                                                                     smart-append
                                                                     make-trimmed-syntax-object
                                                                     make-binding-wrap
                                                                     lookup-import-binding-name
                                                                     extend-ribcage-subst!
                                                                     extend-ribcage-barrier-help!
                                                                     extend-ribcage-barrier!
                                                                     extend-ribcage!
                                                                     make-empty-ribcage
                                                                     import-token-key
                                                                     import-token?
                                                                     make-import-token
                                                                     barrier-marker
                                                                     new-mark
                                                                     anti-mark
                                                                     the-anti-mark
                                                                     only-top-marked?
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
                                                                     set-indirect-label!
                                                                     get-indirect-label
                                                                     indirect-label?
                                                                     gen-indirect-label
                                                                     gen-labels
                                                                     label?
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
                                                                     sanitize-binding
                                                                     lookup*
                                                                     displaced-lexical-error
                                                                     transformer-env
                                                                     extend-var-env*
                                                                     extend-env*
                                                                     extend-env
                                                                     null-env
                                                                     binding?
                                                                     set-binding-value!
                                                                     set-binding-type!
                                                                     binding-value
                                                                     binding-type
                                                                     make-binding
                                                                     arg-check
                                                                     source-annotation
                                                                     no-source
                                                                     unannotate
                                                                     set-syntax-object-wrap!
                                                                     set-syntax-object-expression!
                                                                     syntax-object-wrap
                                                                     syntax-object-expression
                                                                     syntax-object?
                                                                     make-syntax-object
                                                                     self-evaluating?
                                                                     build-lexical-var
                                                                     build-letrec
                                                                     build-sequence
                                                                     build-data
                                                                     build-primref
                                                                     build-lambda
                                                                     build-cte-install
                                                                     build-module-definition
                                                                     build-global-definition
                                                                     build-global-assignment
                                                                     build-global-reference
                                                                     build-lexical-assignment
                                                                     build-lexical-reference
                                                                     build-conditional
                                                                     build-application
                                                                     generate-id
                                                                     get-import-binding
                                                                     get-global-definition-hook
                                                                     put-global-definition-hook
                                                                     gensym-hook
                                                                     error-hook
                                                                     local-eval-hook
                                                                     top-level-eval-hook
                                                                     annotation?
                                                                     fx<
                                                                     fx=
                                                                     fx-
                                                                     fx+
                                                                     noexpand
                                                                     define-structure
                                                                     unless
                                                                     when)
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
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
                                                                    (top)
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
                                                                   ("i" "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
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
                                                                   ((import-token
                                                                      .
                                                                      *top*))
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   ((import-token
                                                                      .
                                                                      *top*))
                                                                   ()
                                                                   ()))))
                                                       'eval
                                                       (syntax-error
                                                         (g393 g1111 g1106)
                                                         '"invalid eval-when situation")))))
                                         (car g1110))
                                        g1109))))))
                  g1108)
                g1105
                '())))
            (g396
             (lambda (g1358 g1352 g1357 g1353 g1356 g1354 g1355)
               (g190 g1353
                     ((letrec ((g1359
                                (lambda (g1364 g1360 g1363 g1361 g1362)
                                  (if (null? g1364)
                                      '()
                                      ((lambda (g1365)
                                         (cons g1365
                                               (g1359
                                                 (cdr g1364)
                                                 g1360
                                                 g1363
                                                 g1361
                                                 g1362)))
                                       (g400 (car g1364)
                                             g1360
                                             g1363
                                             g1361
                                             g1362
                                             g1355))))))
                        g1359)
                      g1358
                      g1352
                      g1357
                      g1356
                      g1354))))
            (g395
             (lambda (g1115 g1112 g1114 g1113)
               (g190 g1113
                     ((letrec ((g1116
                                (lambda (g1119 g1117 g1118)
                                  (if (null? g1119)
                                      '()
                                      ((lambda (g1120)
                                         (cons g1120
                                               (g1116
                                                 (cdr g1119)
                                                 g1117
                                                 g1118)))
                                       (g432 (car g1119) g1117 g1118))))))
                        g1116)
                      g1115
                      g1112
                      g1114))))
            (g394
             (lambda (g1351 g1349 g1350)
               (g393 (if g1350 (make-annotation g1351 g1350 '#f) g1351)
                     g1349)))
            (g393
             (lambda (g1122 g1121)
               (if (if (null? (g264 g1121)) (null? (g265 g1121)) '#f)
                   g1122
                   (if (g204 g1122)
                       (g203 (g205 g1122) (g371 g1121 (g206 g1122)))
                       (if (null? g1122) g1122 (g203 g1122 g1121))))))
            (g392
             (lambda (g1347 g1346)
               (if (not (null? g1346))
                   ((lambda (g1348)
                      (if g1348 g1348 (g392 g1347 (cdr g1346))))
                    (g388 g1347 (car g1346)))
                   '#f)))
            (g391
             (lambda (g1125 g1123 g1124)
               ((letrec ((g1126
                          (lambda (g1128 g1127)
                            (if (null? g1128)
                                (syntax-error g1123)
                                (if (g256 (car g1128))
                                    (if (g392 (car g1128) g1127)
                                        (syntax-error
                                          (car g1128)
                                          '"duplicate "
                                          g1124)
                                        (g1126
                                          (cdr g1128)
                                          (cons (car g1128) g1127)))
                                    (syntax-error
                                      (car g1128)
                                      '"invalid "
                                      g1124))))))
                  g1126)
                g1125
                '())))
            (g390
             (lambda (g1342)
               ((letrec ((g1343
                          (lambda (g1344)
                            ((lambda (g1345)
                               (if g1345
                                   g1345
                                   (if (not (g392 (car g1344) (cdr g1344)))
                                       (g1343 (cdr g1344))
                                       '#f)))
                             (null? g1344)))))
                  g1343)
                g1342)))
            (g389
             (lambda (g1129)
               (if ((letrec ((g1130
                              (lambda (g1131)
                                ((lambda (g1132)
                                   (if g1132
                                       g1132
                                       (if (g256 (car g1131))
                                           (g1130 (cdr g1131))
                                           '#f)))
                                 (null? g1131)))))
                      g1130)
                    g1129)
                   (g390 g1129)
                   '#f)))
            (g388
             (lambda (g1337 g1336)
               (if (if (g204 g1337) (g204 g1336) '#f)
                   (if (eq? ((lambda (g1339)
                               (if (g90 g1339)
                                   (annotation-expression g1339)
                                   g1339))
                             (g205 g1337))
                            ((lambda (g1338)
                               (if (g90 g1338)
                                   (annotation-expression g1338)
                                   g1338))
                             (g205 g1336)))
                       (g373 (g264 (g206 g1337)) (g264 (g206 g1336)))
                       '#f)
                   (eq? ((lambda (g1341)
                           (if (g90 g1341)
                               (annotation-expression g1341)
                               g1341))
                         g1337)
                        ((lambda (g1340)
                           (if (g90 g1340)
                               (annotation-expression g1340)
                               g1340))
                         g1336)))))
            (g378
             (lambda (g1134 g1133)
               (if (eq? ((lambda (g1137)
                           ((lambda (g1138)
                              (if (g90 g1138)
                                  (annotation-expression g1138)
                                  g1138))
                            (if (g204 g1137) (g205 g1137) g1137)))
                         g1134)
                        ((lambda (g1135)
                           ((lambda (g1136)
                              (if (g90 g1136)
                                  (annotation-expression g1136)
                                  g1136))
                            (if (g204 g1135) (g205 g1135) g1135)))
                         g1133))
                   (eq? (g377 g1134 '(())) (g377 g1133 '(())))
                   '#f)))
            (g377
             (lambda (g1333 g1332)
               (call-with-values
                 (lambda () (g374 g1333 g1332))
                 (lambda (g1335 g1334)
                   (if (g301 g1335) (g302 g1335) g1335)))))
            (g376
             (lambda (g1140 g1139)
               (call-with-values
                 (lambda () (g374 g1140 g1139))
                 (lambda (g1142 g1141) g1142))))
            (g375
             (lambda (g1329 g1328)
               (call-with-values
                 (lambda () (g374 g1329 g1328))
                 (lambda (g1331 g1330)
                   (values (if (g301 g1331) (g302 g1331) g1331) g1330)))))
            (g374
             (lambda (g1144 g1143)
               (letrec ((g1147
                         (lambda (g1174 g1170 g1173 g1171 g1172)
                           ((lambda (g1175)
                              ((letrec ((g1176
                                         (lambda (g1177)
                                           (if (= g1177 g1175)
                                               (g1145
                                                 g1174
                                                 (cdr g1170)
                                                 g1173)
                                               (if (if (eq? (vector-ref
                                                              g1171
                                                              g1177)
                                                            g1174)
                                                       (g373 g1173
                                                             (vector-ref
                                                               (g307 g1172)
                                                               g1177))
                                                       '#f)
                                                   (values
                                                     (vector-ref
                                                       (g308 g1172)
                                                       g1177)
                                                     g1173)
                                                   (g1176 (+ g1177 '1)))))))
                                 g1176)
                               '0))
                            (vector-length g1171))))
                        (g1146
                         (lambda (g1159 g1155 g1158 g1156 g1157)
                           ((letrec ((g1160
                                      (lambda (g1162 g1161)
                                        (if (null? g1162)
                                            (g1145 g1159 (cdr g1155) g1158)
                                            (if (if (eq? (car g1162) g1159)
                                                    (g373 g1158
                                                          (list-ref
                                                            (g307 g1157)
                                                            g1161))
                                                    '#f)
                                                (values
                                                  (list-ref
                                                    (g308 g1157)
                                                    g1161)
                                                  g1158)
                                                (if (g357 (car g1162))
                                                    ((lambda (g1163)
                                                       (if g1163
                                                           ((lambda (g1164)
                                                              (if (symbol?
                                                                    g1164)
                                                                  (values
                                                                    g1164
                                                                    g1158)
                                                                  (g375 g1164
                                                                        '(()))))
                                                            g1163)
                                                           (g1160
                                                             (cdr g1162)
                                                             g1161)))
                                                     (g367 g1159
                                                           (g358 (car g1162))
                                                           g1158))
                                                    (if (if (eq? (car g1162)
                                                                 g354)
                                                            (g373 g1158
                                                                  (list-ref
                                                                    (g307 g1157)
                                                                    g1161))
                                                            '#f)
                                                        (values '#f g1158)
                                                        (g1160
                                                          (cdr g1162)
                                                          (+ g1161
                                                             '1)))))))))
                              g1160)
                            g1156
                            '0)))
                        (g1145
                         (lambda (g1167 g1165 g1166)
                           (if (null? g1165)
                               (values g1167 g1166)
                               ((lambda (g1168)
                                  (if (eq? g1168 'shift)
                                      (g1145 g1167 (cdr g1165) (cdr g1166))
                                      ((lambda (g1169)
                                         (if (vector? g1169)
                                             (g1147
                                               g1167
                                               g1165
                                               g1166
                                               g1169
                                               g1168)
                                             (g1146
                                               g1167
                                               g1165
                                               g1166
                                               g1169
                                               g1168)))
                                       (g306 g1168))))
                                (car g1165))))))
                 (if (symbol? g1144)
                     (g1145 g1144 (g265 g1143) (g264 g1143))
                     (if (g204 g1144)
                         ((lambda (g1149 g1148)
                            ((lambda (g1150)
                               (call-with-values
                                 (lambda ()
                                   (g1145 g1149 (g265 g1143) g1150))
                                 (lambda (g1152 g1151)
                                   (if (eq? g1152 g1149)
                                       (g1145 g1149 (g265 g1148) g1151)
                                       (values g1152 g1151)))))
                             (g372 (g264 g1143) (g264 g1148))))
                          ((lambda (g1153)
                             (if (g90 g1153)
                                 (annotation-expression g1153)
                                 g1153))
                           (g205 g1144))
                          (g206 g1144))
                         (if (g90 g1144)
                             (g1145
                               ((lambda (g1154)
                                  (if (g90 g1154)
                                      (annotation-expression g1154)
                                      g1154))
                                g1144)
                               (g265 g1143)
                               (g264 g1143))
                             (g93 'id-var-name '"invalid id" g1144)))))))
            (g373
             (lambda (g1326 g1325)
               ((lambda (g1327)
                  (if g1327
                      g1327
                      (if (not (null? g1326))
                          (if (not (null? g1325))
                              (if (eq? (car g1326) (car g1325))
                                  (g373 (cdr g1326) (cdr g1325))
                                  '#f)
                              '#f)
                          '#f)))
                (eq? g1326 g1325))))
            (g372 (lambda (g1179 g1178) (g370 g1179 g1178)))
            (g371
             (lambda (g1322 g1321)
               ((lambda (g1324 g1323)
                  (if (null? g1324)
                      (if (null? g1323)
                          g1321
                          (g263 (g264 g1321) (g370 g1323 (g265 g1321))))
                      (g263 (g370 g1324 (g264 g1321))
                            (g370 g1323 (g265 g1321)))))
                (g264 g1322)
                (g265 g1322))))
            (g370
             (lambda (g1181 g1180)
               (if (null? g1180) g1181 (append g1181 g1180))))
            (g369
             (lambda (g1315)
               (call-with-values
                 (lambda () (g375 g1315 '(())))
                 (lambda (g1317 g1316)
                   (begin (if (not g1317)
                              (syntax-error
                                g1315
                                '"identifier not visible for export")
                              (void))
                          ((lambda (g1318)
                             (g203 g1318
                                   (g263 g1316
                                         (list (g304 (vector g1318)
                                                     (vector g1316)
                                                     (vector g1317))))))
                           ((lambda (g1319)
                              ((lambda (g1320)
                                 (if (g90 g1320)
                                     (annotation-expression g1320)
                                     g1320))
                               (if (g204 g1319) (g205 g1319) g1319)))
                            g1315)))))))
            (g368
             (lambda (g1184 g1182 g1183)
               (if (null? g1184)
                   g1183
                   (g263 (g264 g1183)
                         (cons ((lambda (g1185)
                                  ((lambda (g1186)
                                     ((lambda (g1188 g1187)
                                        (begin ((letrec ((g1189
                                                          (lambda (g1191
                                                                   g1190)
                                                            (if (not (null?
                                                                       g1191))
                                                                (call-with-values
                                                                  (lambda ()
                                                                    (g262 (car g1191)
                                                                          g1183))
                                                                  (lambda (g1193
                                                                           g1192)
                                                                    (begin (vector-set!
                                                                             g1188
                                                                             g1190
                                                                             g1193)
                                                                           (vector-set!
                                                                             g1187
                                                                             g1190
                                                                             g1192)
                                                                           (g1189
                                                                             (cdr g1191)
                                                                             (+ g1190
                                                                                '1)))))
                                                                (void)))))
                                                  g1189)
                                                g1184
                                                '0)
                                               (g304 g1188 g1187 g1185)))
                                      (make-vector g1186)
                                      (make-vector g1186)))
                                   (vector-length g1185)))
                                (list->vector g1182))
                               (g265 g1183))))))
            (g367
             (lambda (g1310 g1308 g1309)
               ((lambda (g1311)
                  (if g1311
                      ((letrec ((g1312
                                 (lambda (g1313)
                                   (if (pair? g1313)
                                       ((lambda (g1314)
                                          (if g1314
                                              g1314
                                              (g1312 (cdr g1313))))
                                        (g1312 (car g1313)))
                                       (if (g373 g1309 (g264 (g206 g1313)))
                                           g1313
                                           '#f)))))
                         g1312)
                       g1311)
                      '#f))
                (g100 g1310 g1308))))
            (g366
             (lambda (g1195 g1194)
               (g309 g1195 (cons (g356 g1194) (g306 g1195)))))
            (g365
             (lambda (g1307 g1306)
               (begin (g309 g1307 (cons g354 (g306 g1307)))
                      (g310 g1307 (cons (g264 g1306) (g307 g1307))))))
            (g364 (lambda (g1197 g1196) (g365 g1197 (g206 g1196))))
            (g363
             (lambda (g1304 g1302 g1303)
               (begin (g309 g1304
                            (cons ((lambda (g1305)
                                     (if (g90 g1305)
                                         (annotation-expression g1305)
                                         g1305))
                                   (g205 g1302))
                                  (g306 g1304)))
                      (g310 g1304 (cons (g264 (g206 g1302)) (g307 g1304)))
                      (g311 g1304 (cons g1303 (g308 g1304))))))
            (g358 cdr)
            (g357
             (lambda (g1301)
               (if (pair? g1301) (eq? (car g1301) g355) '#f)))
            (g356 (lambda (g1198) (cons g355 g1198)))
            (g355 'import-token)
            (g354 '#f)
            (g349
             (lambda (g1300)
               (g263 (cons '#f (g264 g1300)) (cons 'shift (g265 g1300)))))
            (g311 (lambda (g1200 g1199) (vector-set! g1200 '3 g1199)))
            (g310 (lambda (g1299 g1298) (vector-set! g1299 '2 g1298)))
            (g309 (lambda (g1202 g1201) (vector-set! g1202 '1 g1201)))
            (g308 (lambda (g1297) (vector-ref g1297 '3)))
            (g307 (lambda (g1203) (vector-ref g1203 '2)))
            (g306 (lambda (g1296) (vector-ref g1296 '1)))
            (g305
             (lambda (g1204)
               (if (vector? g1204)
                   (if (= (vector-length g1204) '4)
                       (eq? (vector-ref g1204 '0) 'ribcage)
                       '#f)
                   '#f)))
            (g304
             (lambda (g1295 g1293 g1294)
               (vector 'ribcage g1295 g1293 g1294)))
            (g303 set-car!)
            (g302 car)
            (g301 pair?)
            (g300 (lambda () (list (g297))))
            (g299
             (lambda (g1205)
               (if (null? g1205) '() (cons (g297) (g299 (cdr g1205))))))
            (g298
             (lambda (g1290)
               ((lambda (g1291)
                  (if g1291
                      g1291
                      ((lambda (g1292) (if g1292 g1292 (g301 g1290)))
                       (symbol? g1290))))
                (string? g1290))))
            (g297 (lambda () (string '#\i)))
            (g265 cdr)
            (g264 car)
            (g263 cons)
            (g262
             (lambda (g1207 g1206)
               (if (g204 g1207)
                   (values
                     ((lambda (g1208)
                        (if (g90 g1208)
                            (annotation-expression g1208)
                            g1208))
                      (g205 g1207))
                     (g372 (g264 g1206) (g264 (g206 g1207))))
                   (values
                     ((lambda (g1209)
                        (if (g90 g1209)
                            (annotation-expression g1209)
                            g1209))
                      g1207)
                     (g264 g1206)))))
            (g256
             (lambda (g1288)
               (if (symbol? g1288)
                   '#t
                   (if (g204 g1288)
                       (symbol?
                         ((lambda (g1289)
                            (if (g90 g1289)
                                (annotation-expression g1289)
                                g1289))
                          (g205 g1288)))
                       (if (g90 g1288)
                           (symbol? (annotation-expression g1288))
                           '#f)))))
            (g255
             (lambda (g1210)
               (if (g204 g1210)
                   (symbol?
                     ((lambda (g1211)
                        (if (g90 g1211)
                            (annotation-expression g1211)
                            g1211))
                      (g205 g1210)))
                   '#f)))
            (g254
             (lambda (g1287 g1285 g1286) (g98 g1285 (g231 g1287 g1286))))
            (g253
             (lambda (g1213 g1212)
               (letrec ((g1214
                         (lambda (g1221 g1220)
                           (begin (g234 g1221 (g232 g1220))
                                  (g235 g1221 (g233 g1220))))))
                 ((lambda (g1215)
                    ((lambda (g1216)
                       (if (memv g1216 '(deferred))
                           (begin (g1214
                                    g1215
                                    ((lambda (g1217)
                                       ((lambda (g1218)
                                          (if g1218
                                              g1218
                                              (syntax-error
                                                g1217
                                                '"invalid transformer")))
                                        (g252 g1217)))
                                     (g92 (g233 g1215))))
                                  ((lambda (g1219) g1215) (g232 g1215)))
                           g1215))
                     (g232 g1215)))
                  (g251 g1213 g1212)))))
            (g252
             (lambda (g1283)
               (if (procedure? g1283)
                   (g231 'macro g1283)
                   (if (g236 g1283)
                       ((lambda (g1284)
                          (if (memv g1284 '(core macro macro!))
                              (if (procedure? (g233 g1283)) g1283 '#f)
                              (if (memv g1284 '(module))
                                  (if (g403 (g233 g1283)) g1283 '#f)
                                  g1283)))
                        (g232 g1283))
                       '#f))))
            (g251
             (lambda (g1223 g1222)
               ((lambda (g1224)
                  (if g1224
                      (cdr g1224)
                      (if (symbol? g1223)
                          ((lambda (g1225)
                             (if g1225 g1225 (g231 'global g1223)))
                           (g99 g1223))
                          (g231 'displaced-lexical '#f))))
                (assq g1223 g1222))))
            (g250
             (lambda (g1282)
               (syntax-error
                 g1282
                 (if (g377 g1282 '(()))
                     '"identifier out of context"
                     '"identifier not visible"))))
            (g249
             (lambda (g1226)
               (if (null? g1226)
                   '()
                   ((lambda (g1227)
                      (if (eq? (cadr g1227) 'lexical)
                          (g249 (cdr g1226))
                          (cons g1227 (g249 (cdr g1226)))))
                    (car g1226)))))
            (g248
             (lambda (g1281 g1279 g1280)
               (if (null? g1281)
                   g1280
                   (g248 (cdr g1281)
                         (cdr g1279)
                         (g246 (car g1281)
                               (g231 'lexical (car g1279))
                               g1280)))))
            (g247
             (lambda (g1230 g1228 g1229)
               (if (null? g1230)
                   g1229
                   (g247 (cdr g1230)
                         (cdr g1228)
                         (g246 (car g1230) (car g1228) g1229)))))
            (g246
             (lambda (g1278 g1276 g1277)
               (cons (cons g1278 g1276) g1277)))
            (g236
             (lambda (g1231)
               (if (pair? g1231) (symbol? (car g1231)) '#f)))
            (g235 set-cdr!)
            (g234 set-car!)
            (g233 cdr)
            (g232 car)
            (g231 (lambda (g1275 g1274) (cons g1275 g1274)))
            (g223
             (lambda (g1232)
               (if (g90 g1232)
                   (annotation-source g1232)
                   (if (g204 g1232) (g223 (g205 g1232)) '#f))))
            (g208 (lambda (g1273 g1272) (vector-set! g1273 '2 g1272)))
            (g207 (lambda (g1234 g1233) (vector-set! g1234 '1 g1233)))
            (g206 (lambda (g1271) (vector-ref g1271 '2)))
            (g205 (lambda (g1235) (vector-ref g1235 '1)))
            (g204
             (lambda (g1270)
               (if (vector? g1270)
                   (if (= (vector-length g1270) '3)
                       (eq? (vector-ref g1270 '0) 'syntax-object)
                       '#f)
                   '#f)))
            (g203
             (lambda (g1237 g1236) (vector 'syntax-object g1237 g1236)))
            (g191
             (lambda (g1269 g1266 g1268 g1267)
               (if (null? g1266)
                   g1267
                   (list 'letrec (map list g1266 g1268) g1267))))
            (g190
             (lambda (g1239 g1238)
               (if (null? (cdr g1238)) (car g1238) (cons 'begin g1238))))
            (g101
             ((lambda (g1251)
                (letrec ((g1254
                          (lambda (g1260)
                            ((letrec ((g1261
                                       (lambda (g1263 g1262)
                                         (if (< g1263 g1251)
                                             (list->string
                                               (cons (g1253 g1263) g1262))
                                             ((lambda (g1265 g1264)
                                                (g1261
                                                  g1264
                                                  (cons (g1253 g1265)
                                                        g1262)))
                                              (modulo g1263 g1251)
                                              (quotient g1263 g1251))))))
                               g1261)
                             g1260
                             '())))
                         (g1253
                          (lambda (g1259) (integer->char (+ g1259 '33))))
                         (g1252 (lambda () '0)))
                  ((lambda (g1256 g1255)
                     (lambda (g1257)
                       (begin (set! g1255 (+ g1255 '1))
                              ((lambda (g1258) g1258)
                               (string->symbol
                                 (string-append
                                   '"#"
                                   g1256
                                   (g1254 g1255)))))))
                   (g1254 (g1252))
                   '-1)))
              (- '127 '32 '2)))
            (g100 (lambda (g1241 g1240) (getprop g1241 g1240)))
            (g99 (lambda (g1250) (getprop g1250 '*sc-expander*)))
            (g98 (lambda (g1243 g1242) ($sc-put-cte g1243 g1242)))
            (g93
             (lambda (g1249 g1247 g1248)
               (error g1249 '"~a ~s" g1247 g1248)))
            (g92 (lambda (g1244) (eval (list g53 g1244))))
            (g91 (lambda (g1246) (eval (list g53 g1246))))
            (g90 (lambda (g1245) '#f))
            (g53 '"noexpand"))
     (begin (set! $sc-put-cte
              (lambda (g802 g801)
                (letrec ((g805
                          (lambda (g831 g830)
                            ((lambda (g832)
                               (putprop g832 '*sc-expander* g830))
                             (if (symbol? g831) g831 (g377 g831 '(()))))))
                         (g804
                          (lambda (g815 g814)
                            (g429 (lambda (g816) (g803 g816 g814)) g815)))
                         (g803
                          (lambda (g818 g817)
                            (letrec ((g820
                                      (lambda (g828 g827)
                                        (if (pair? g827)
                                            (if (g388 (car g827) g828)
                                                (g820 g828 (cdr g827))
                                                (g819 (car g827)
                                                      (g820 g828
                                                            (cdr g827))))
                                            (if ((lambda (g829)
                                                   (if g829
                                                       g829
                                                       (g388 g827 g828)))
                                                 (not g827))
                                                '#f
                                                g827))))
                                     (g819
                                      (lambda (g826 g825)
                                        (if (not g825)
                                            g826
                                            (cons g826 g825)))))
                              ((lambda (g821)
                                 ((lambda (g822)
                                    (if (if (not g822) (symbol? g818) '#f)
                                        (remprop g821 g817)
                                        (putprop
                                          g821
                                          g817
                                          (g819 g818 g822))))
                                  (g820 g818 (getprop g821 g817))))
                               ((lambda (g823)
                                  ((lambda (g824)
                                     (if (g90 g824)
                                         (annotation-expression g824)
                                         g824))
                                   (if (g204 g823) (g205 g823) g823)))
                                g818))))))
                  ((lambda (g806)
                     ((lambda (g807)
                        (if (memv g807 '(module))
                            (begin ((lambda (g808)
                                      (g804 (g404 g808) (g405 g808)))
                                    (g233 g806))
                                   (g805 g802 g806))
                            (if (memv g807 '(do-import))
                                ((lambda (g809)
                                   ((lambda (g810)
                                      ((lambda (g811)
                                         (if (memv g811 '(module))
                                             ((lambda (g812)
                                                (begin (if (not (eq? (g405 g812)
                                                                     g809))
                                                           (syntax-error
                                                             g802
                                                             '"import mismatch for module")
                                                           (void))
                                                       (g804 (g404 g812)
                                                             '*top*)))
                                              (g233 g810))
                                             (syntax-error
                                               g802
                                               '"import from unknown module")))
                                       (g232 g810)))
                                    (g253 (g377 g802 '(())) '())))
                                 (g233 g801))
                                (g805 g802 g806))))
                      (g232 g806)))
                   ((lambda (g813)
                      (if g813
                          g813
                          (error 'define-syntax
                            '"invalid transformer ~s"
                            g801)))
                    (g252 g801))))))
            (g254 'local-syntax 'letrec-syntax '#t)
            (g254 'local-syntax 'let-syntax '#f)
            (g254 'core
                  'fluid-let-syntax
                  (lambda (g456 g453 g455 g454)
                    ((lambda (g457)
                       ((lambda (g458)
                          (if (if g458
                                  (apply
                                    (lambda (g463 g459 g462 g460 g461)
                                      (g389 g459))
                                    g458)
                                  '#f)
                              (apply
                                (lambda (g469 g465 g468 g466 g467)
                                  ((lambda (g470)
                                     (begin (for-each
                                              (lambda (g477 g476)
                                                ((lambda (g478)
                                                   (if (memv g478
                                                             '(displaced-lexical))
                                                       (g250 (g393 g477
                                                                   g455))
                                                       (void)))
                                                 (g232 (g253 g476 g453))))
                                              g465
                                              g470)
                                            (g437 (cons g466 g467)
                                                  (g394 g456 g455 g454)
                                                  (g247 g470
                                                        ((lambda (g471)
                                                           (map (lambda (g473)
                                                                  (g231 'deferred
                                                                        (g432 g473
                                                                              g471
                                                                              g455)))
                                                                g468))
                                                         (g249 g453))
                                                        g453)
                                                  g455)))
                                   (map (lambda (g480) (g377 g480 g455))
                                        g465)))
                                g458)
                              ((lambda (g481)
                                 (syntax-error (g394 g456 g455 g454)))
                               g457)))
                        ($syntax-dispatch
                          g457
                          '(any #(each (any any)) any . each-any))))
                     g456)))
            (g254 'core
                  'quote
                  (lambda (g795 g792 g794 g793)
                    ((lambda (g796)
                       ((lambda (g797)
                          (if g797
                              (apply
                                (lambda (g799 g798)
                                  (list 'quote (g450 g798 g794)))
                                g797)
                              ((lambda (g800)
                                 (syntax-error (g394 g795 g794 g793)))
                               g796)))
                        ($syntax-dispatch g796 '(any any))))
                     g795)))
            (g254 'core
                  'syntax
                  ((lambda ()
                     (letrec ((g489
                               (lambda (g584)
                                 ((lambda (g585)
                                    (if (memv g585 '(ref))
                                        (cadr g584)
                                        (if (memv g585 '(primitive))
                                            (cadr g584)
                                            (if (memv g585 '(quote))
                                                (list 'quote (cadr g584))
                                                (if (memv g585 '(lambda))
                                                    (list 'lambda
                                                          (cadr g584)
                                                          (g489 (caddr
                                                                  g584)))
                                                    (if (memv g585 '(map))
                                                        ((lambda (g586)
                                                           (cons (if (= (length
                                                                          g586)
                                                                        '2)
                                                                     'map
                                                                     'map)
                                                                 g586))
                                                         (map g489
                                                              (cdr g584)))
                                                        (cons (car g584)
                                                              (map g489
                                                                   (cdr g584)))))))))
                                  (car g584))))
                              (g488
                               (lambda (g502)
                                 (if (eq? (car g502) 'list)
                                     (cons 'vector (cdr g502))
                                     (if (eq? (car g502) 'quote)
                                         (list 'quote
                                               (list->vector (cadr g502)))
                                         (list 'list->vector g502)))))
                              (g487
                               (lambda (g583 g582)
                                 (if (equal? g582 ''())
                                     g583
                                     (list 'append g583 g582))))
                              (g486
                               (lambda (g504 g503)
                                 ((lambda (g505)
                                    (if (memv g505 '(quote))
                                        (if (eq? (car g504) 'quote)
                                            (list 'quote
                                                  (cons (cadr g504)
                                                        (cadr g503)))
                                            (if (eq? (cadr g503) '())
                                                (list 'list g504)
                                                (list 'cons g504 g503)))
                                        (if (memv g505 '(list))
                                            (cons 'list
                                                  (cons g504 (cdr g503)))
                                            (list 'cons g504 g503))))
                                  (car g503))))
                              (g485
                               (lambda (g575 g574)
                                 ((lambda (g577 g576)
                                    (if (eq? (car g575) 'ref)
                                        (car g576)
                                        (if (andmap
                                              (lambda (g578)
                                                (if (eq? (car g578) 'ref)
                                                    (memq (cadr g578) g577)
                                                    '#f))
                                              (cdr g575))
                                            (cons 'map
                                                  (cons (list 'primitive
                                                              (car g575))
                                                        (map ((lambda (g579)
                                                                (lambda (g580)
                                                                  (cdr (assq (cadr g580)
                                                                             g579))))
                                                              (map cons
                                                                   g577
                                                                   g576))
                                                             (cdr g575))))
                                            (cons 'map
                                                  (cons (list 'lambda
                                                              g577
                                                              g575)
                                                        g576)))))
                                  (map cdr g574)
                                  (map (lambda (g581)
                                         (list 'ref (car g581)))
                                       g574))))
                              (g484
                               (lambda (g507 g506)
                                 (list 'apply
                                       '(primitive append)
                                       (g485 g507 g506))))
                              (g483
                               (lambda (g569 g566 g568 g567)
                                 (if (= g568 '0)
                                     (values g566 g567)
                                     (if (null? g567)
                                         (syntax-error
                                           g569
                                           '"missing ellipsis in syntax form")
                                         (call-with-values
                                           (lambda ()
                                             (g483 g569
                                                   g566
                                                   (- g568 '1)
                                                   (cdr g567)))
                                           (lambda (g571 g570)
                                             ((lambda (g572)
                                                (if g572
                                                    (values
                                                      (cdr g572)
                                                      g567)
                                                    ((lambda (g573)
                                                       (values
                                                         g573
                                                         (cons (cons (cons g571
                                                                           g573)
                                                                     (car g567))
                                                               g570)))
                                                     (g451 'tmp))))
                                              (assq g571 (car g567)))))))))
                              (g482
                               (lambda (g512 g508 g511 g509 g510)
                                 (if (g256 g508)
                                     ((lambda (g513)
                                        ((lambda (g514)
                                           (if (eq? (g232 g514) 'syntax)
                                               (call-with-values
                                                 (lambda ()
                                                   ((lambda (g517)
                                                      (g483 g512
                                                            (car g517)
                                                            (cdr g517)
                                                            g509))
                                                    (g233 g514)))
                                                 (lambda (g516 g515)
                                                   (values
                                                     (list 'ref g516)
                                                     g515)))
                                               (if (g510 g508)
                                                   (syntax-error
                                                     g512
                                                     '"misplaced ellipsis in syntax form")
                                                   (values
                                                     (list 'quote g508)
                                                     g509))))
                                         (g253 g513 g511)))
                                      (g377 g508 '(())))
                                     ((lambda (g518)
                                        ((lambda (g519)
                                           (if (if g519
                                                   (apply
                                                     (lambda (g521 g520)
                                                       (g510 g521))
                                                     g519)
                                                   '#f)
                                               (apply
                                                 (lambda (g523 g522)
                                                   (g482 g512
                                                         g522
                                                         g511
                                                         g509
                                                         (lambda (g524)
                                                           '#f)))
                                                 g519)
                                               ((lambda (g525)
                                                  (if (if g525
                                                          (apply
                                                            (lambda (g528
                                                                     g526
                                                                     g527)
                                                              (g510 g526))
                                                            g525)
                                                          '#f)
                                                      (apply
                                                        (lambda (g531
                                                                 g529
                                                                 g530)
                                                          ((letrec ((g532
                                                                     (lambda (g534
                                                                              g533)
                                                                       ((lambda (g535)
                                                                          ((lambda (g536)
                                                                             (if (if g536
                                                                                     (apply
                                                                                       (lambda (g538
                                                                                                g537)
                                                                                         (g510 g538))
                                                                                       g536)
                                                                                     '#f)
                                                                                 (apply
                                                                                   (lambda (g540
                                                                                            g539)
                                                                                     (g532 g539
                                                                                           (lambda (g541)
                                                                                             (call-with-values
                                                                                               (lambda ()
                                                                                                 (g533 (cons '()
                                                                                                             g541)))
                                                                                               (lambda (g543
                                                                                                        g542)
                                                                                                 (if (null?
                                                                                                       (car g542))
                                                                                                     (syntax-error
                                                                                                       g512
                                                                                                       '"extra ellipsis in syntax form")
                                                                                                     (values
                                                                                                       (g484 g543
                                                                                                             (car g542))
                                                                                                       (cdr g542))))))))
                                                                                   g536)
                                                                                 ((lambda (g544)
                                                                                    (call-with-values
                                                                                      (lambda ()
                                                                                        (g482 g512
                                                                                              g534
                                                                                              g511
                                                                                              g509
                                                                                              g510))
                                                                                      (lambda (g546
                                                                                               g545)
                                                                                        (call-with-values
                                                                                          (lambda ()
                                                                                            (g533 g545))
                                                                                          (lambda (g548
                                                                                                   g547)
                                                                                            (values
                                                                                              (g487 g548
                                                                                                    g546)
                                                                                              g547))))))
                                                                                  g535)))
                                                                           ($syntax-dispatch
                                                                             g535
                                                                             '(any .
                                                                                   any))))
                                                                        g534))))
                                                             g532)
                                                           g530
                                                           (lambda (g549)
                                                             (call-with-values
                                                               (lambda ()
                                                                 (g482 g512
                                                                       g531
                                                                       g511
                                                                       (cons '()
                                                                             g549)
                                                                       g510))
                                                               (lambda (g551
                                                                        g550)
                                                                 (if (null?
                                                                       (car g550))
                                                                     (syntax-error
                                                                       g512
                                                                       '"extra ellipsis in syntax form")
                                                                     (values
                                                                       (g485 g551
                                                                             (car g550))
                                                                       (cdr g550))))))))
                                                        g525)
                                                      ((lambda (g552)
                                                         (if g552
                                                             (apply
                                                               (lambda (g554
                                                                        g553)
                                                                 (call-with-values
                                                                   (lambda ()
                                                                     (g482 g512
                                                                           g554
                                                                           g511
                                                                           g509
                                                                           g510))
                                                                   (lambda (g556
                                                                            g555)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (g482 g512
                                                                               g553
                                                                               g511
                                                                               g555
                                                                               g510))
                                                                       (lambda (g558
                                                                                g557)
                                                                         (values
                                                                           (g486 g556
                                                                                 g558)
                                                                           g557))))))
                                                               g552)
                                                             ((lambda (g559)
                                                                (if g559
                                                                    (apply
                                                                      (lambda (g561
                                                                               g560)
                                                                        (call-with-values
                                                                          (lambda ()
                                                                            (g482 g512
                                                                                  (cons g561
                                                                                        g560)
                                                                                  g511
                                                                                  g509
                                                                                  g510))
                                                                          (lambda (g563
                                                                                   g562)
                                                                            (values
                                                                              (g488 g563)
                                                                              g562))))
                                                                      g559)
                                                                    ((lambda (g565)
                                                                       (values
                                                                         (list 'quote
                                                                               g508)
                                                                         g509))
                                                                     g518)))
                                                              ($syntax-dispatch
                                                                g518
                                                                '#(vector
                                                                   (any .
                                                                        each-any))))))
                                                       ($syntax-dispatch
                                                         g518
                                                         '(any . any)))))
                                                ($syntax-dispatch
                                                  g518
                                                  '(any any . any)))))
                                         ($syntax-dispatch
                                           g518
                                           '(any any))))
                                      g508)))))
                       (lambda (g493 g490 g492 g491)
                         ((lambda (g494)
                            ((lambda (g495)
                               ((lambda (g496)
                                  (if g496
                                      (apply
                                        (lambda (g498 g497)
                                          (call-with-values
                                            (lambda ()
                                              (g482 g494
                                                    g497
                                                    g490
                                                    '()
                                                    g447))
                                            (lambda (g500 g499)
                                              (g489 g500))))
                                        g496)
                                      ((lambda (g501) (syntax-error g494))
                                       g495)))
                                ($syntax-dispatch g495 '(any any))))
                             g494))
                          (g394 g493 g492 g491)))))))
            (g254 'core
                  'lambda
                  (lambda (g785 g782 g784 g783)
                    ((lambda (g786)
                       ((lambda (g787)
                          (if g787
                              (apply
                                (lambda (g789 g788)
                                  (g444 (g394 g785 g784 g783)
                                        g788
                                        g782
                                        g784
                                        (lambda (g791 g790)
                                          (list 'lambda g791 g790))))
                                g787)
                              (syntax-error g786)))
                        ($syntax-dispatch g786 '(any . any))))
                     g785)))
            (g254 'core
                  'letrec
                  (lambda (g590 g587 g589 g588)
                    ((lambda (g591)
                       ((lambda (g592)
                          (if g592
                              (apply
                                (lambda (g597 g593 g596 g594 g595)
                                  ((lambda (g598)
                                     (if (not (g389 g598))
                                         (g391 (map (lambda (g599)
                                                      (g393 g599 g589))
                                                    g598)
                                               (g394 g590 g589 g588)
                                               '"bound variable")
                                         ((lambda (g601 g600)
                                            ((lambda (g603 g602)
                                               (g191 g588
                                                     g600
                                                     (map (lambda (g606)
                                                            (g432 g606
                                                                  g602
                                                                  g603))
                                                          g596)
                                                     (g437 (cons g594 g595)
                                                           (g394 g590
                                                                 g603
                                                                 g588)
                                                           g602
                                                           g603)))
                                             (g368 g598 g601 g589)
                                             (g248 g601 g600 g587)))
                                          (g299 g598)
                                          (map g451 g598))))
                                   g593))
                                g592)
                              ((lambda (g608)
                                 (syntax-error (g394 g590 g589 g588)))
                               g591)))
                        ($syntax-dispatch
                          g591
                          '(any #(each (any any)) any . each-any))))
                     g590)))
            (g254 'core
                  'if
                  (lambda (g770 g767 g769 g768)
                    ((lambda (g771)
                       ((lambda (g772)
                          (if g772
                              (apply
                                (lambda (g775 g773 g774)
                                  (list 'if
                                        (g432 g773 g767 g769)
                                        (g432 g774 g767 g769)
                                        (g446)))
                                g772)
                              ((lambda (g776)
                                 (if g776
                                     (apply
                                       (lambda (g780 g777 g779 g778)
                                         (list 'if
                                               (g432 g777 g767 g769)
                                               (g432 g779 g767 g769)
                                               (g432 g778 g767 g769)))
                                       g776)
                                     ((lambda (g781)
                                        (syntax-error
                                          (g394 g770 g769 g768)))
                                      g771)))
                               ($syntax-dispatch
                                 g771
                                 '(any any any any)))))
                        ($syntax-dispatch g771 '(any any any))))
                     g770)))
            (g254 'set! 'set! '())
            (g254 'begin 'begin '())
            (g254 'module-key 'module '())
            (g254 'import 'import '#f)
            (g254 'import 'import-only '#t)
            (g254 'define 'define '())
            (g254 'define-syntax 'define-syntax '())
            (g254 'eval-when 'eval-when '())
            (g254 'core
                  'syntax-case
                  ((lambda ()
                     (letrec ((g612
                               (lambda (g693 g690 g692 g691)
                                 (if (null? g692)
                                     (list 'syntax-error g693)
                                     ((lambda (g694)
                                        ((lambda (g695)
                                           (if g695
                                               (apply
                                                 (lambda (g697 g696)
                                                   (if (if (g256 g697)
                                                           (if (not (g392 g697
                                                                          g690))
                                                               (not (g447 g697))
                                                               '#f)
                                                           '#f)
                                                       ((lambda (g699 g698)
                                                          (list (list 'lambda
                                                                      (list g698)
                                                                      (g432 g696
                                                                            (g246 g699
                                                                                  (g231 'syntax
                                                                                        (cons g698
                                                                                              '0))
                                                                                  g691)
                                                                            (g368 (list g697)
                                                                                  (list g699)
                                                                                  '(()))))
                                                                g693))
                                                        (g297)
                                                        (g451 g697))
                                                       (g611 g693
                                                             g690
                                                             (cdr g692)
                                                             g691
                                                             g697
                                                             '#t
                                                             g696)))
                                                 g695)
                                               ((lambda (g700)
                                                  (if g700
                                                      (apply
                                                        (lambda (g703
                                                                 g701
                                                                 g702)
                                                          (g611 g693
                                                                g690
                                                                (cdr g692)
                                                                g691
                                                                g703
                                                                g701
                                                                g702))
                                                        g700)
                                                      ((lambda (g704)
                                                         (syntax-error
                                                           (car g692)
                                                           '"invalid syntax-case clause"))
                                                       g694)))
                                                ($syntax-dispatch
                                                  g694
                                                  '(any any any)))))
                                         ($syntax-dispatch
                                           g694
                                           '(any any))))
                                      (car g692)))))
                              (g611
                               (lambda (g635 g629 g634 g630 g633 g631 g632)
                                 (call-with-values
                                   (lambda () (g609 g633 g629))
                                   (lambda (g637 g636)
                                     (if (not (g390 (map car g636)))
                                         (g391 (map car g636)
                                               g633
                                               '"pattern variable")
                                         (if (not (andmap
                                                    (lambda (g638)
                                                      (not (g447 (car g638))))
                                                    g636))
                                             (syntax-error
                                               g633
                                               '"misplaced ellipsis in syntax-case pattern")
                                             ((lambda (g639)
                                                (list (list 'lambda
                                                            (list g639)
                                                            (list 'if
                                                                  ((lambda (g649)
                                                                     ((lambda (g650)
                                                                        (if g650
                                                                            (apply
                                                                              (lambda ()
                                                                                g639)
                                                                              g650)
                                                                            ((lambda (g651)
                                                                               (list 'if
                                                                                     g639
                                                                                     (g610 g636
                                                                                           g631
                                                                                           g639
                                                                                           g630)
                                                                                     (list 'quote
                                                                                           '#f)))
                                                                             g649)))
                                                                      ($syntax-dispatch
                                                                        g649
                                                                        '#(atom
                                                                           #t))))
                                                                   g631)
                                                                  (g610 g636
                                                                        g632
                                                                        g639
                                                                        g630)
                                                                  (g612 g635
                                                                        g629
                                                                        g634
                                                                        g630)))
                                                      (if (eq? g637 'any)
                                                          (list 'list g635)
                                                          (list '$syntax-dispatch
                                                                g635
                                                                (list 'quote
                                                                      g637)))))
                                              (g451 'tmp))))))))
                              (g610
                               (lambda (g683 g680 g682 g681)
                                 ((lambda (g685 g684)
                                    ((lambda (g687 g686)
                                       (list 'apply
                                             (list 'lambda
                                                   g686
                                                   (g432 g680
                                                         (g247 g687
                                                               (map (lambda (g689
                                                                             g688)
                                                                      (g231 'syntax
                                                                            (cons g689
                                                                                  g688)))
                                                                    g686
                                                                    (map cdr
                                                                         g683))
                                                               g681)
                                                         (g368 g685
                                                               g687
                                                               '(()))))
                                             g682))
                                     (g299 g685)
                                     (map g451 g685)))
                                  (map car g683)
                                  (map cdr g683))))
                              (g609
                               (lambda (g653 g652)
                                 ((letrec ((g654
                                            (lambda (g657 g655 g656)
                                              (if (g256 g657)
                                                  (if (g392 g657 g652)
                                                      (values
                                                        (vector
                                                          'free-id
                                                          g657)
                                                        g656)
                                                      (values
                                                        'any
                                                        (cons (cons g657
                                                                    g655)
                                                              g656)))
                                                  ((lambda (g658)
                                                     ((lambda (g659)
                                                        (if (if g659
                                                                (apply
                                                                  (lambda (g661
                                                                           g660)
                                                                    (g447 g660))
                                                                  g659)
                                                                '#f)
                                                            (apply
                                                              (lambda (g663
                                                                       g662)
                                                                (call-with-values
                                                                  (lambda ()
                                                                    (g654 g663
                                                                          (+ g655
                                                                             '1)
                                                                          g656))
                                                                  (lambda (g665
                                                                           g664)
                                                                    (values
                                                                      (if (eq? g665
                                                                               'any)
                                                                          'each-any
                                                                          (vector
                                                                            'each
                                                                            g665))
                                                                      g664))))
                                                              g659)
                                                            ((lambda (g666)
                                                               (if g666
                                                                   (apply
                                                                     (lambda (g668
                                                                              g667)
                                                                       (call-with-values
                                                                         (lambda ()
                                                                           (g654 g667
                                                                                 g655
                                                                                 g656))
                                                                         (lambda (g670
                                                                                  g669)
                                                                           (call-with-values
                                                                             (lambda ()
                                                                               (g654 g668
                                                                                     g655
                                                                                     g669))
                                                                             (lambda (g672
                                                                                      g671)
                                                                               (values
                                                                                 (cons g672
                                                                                       g670)
                                                                                 g671))))))
                                                                     g666)
                                                                   ((lambda (g673)
                                                                      (if g673
                                                                          (apply
                                                                            (lambda ()
                                                                              (values
                                                                                '()
                                                                                g656))
                                                                            g673)
                                                                          ((lambda (g674)
                                                                             (if g674
                                                                                 (apply
                                                                                   (lambda (g675)
                                                                                     (call-with-values
                                                                                       (lambda ()
                                                                                         (g654 g675
                                                                                               g655
                                                                                               g656))
                                                                                       (lambda (g677
                                                                                                g676)
                                                                                         (values
                                                                                           (vector
                                                                                             'vector
                                                                                             g677)
                                                                                           g676))))
                                                                                   g674)
                                                                                 ((lambda (g679)
                                                                                    (values
                                                                                      (vector
                                                                                        'atom
                                                                                        (g450 g657
                                                                                              '(())))
                                                                                      g656))
                                                                                  g658)))
                                                                           ($syntax-dispatch
                                                                             g658
                                                                             '#(vector
                                                                                each-any)))))
                                                                    ($syntax-dispatch
                                                                      g658
                                                                      '()))))
                                                             ($syntax-dispatch
                                                               g658
                                                               '(any .
                                                                     any)))))
                                                      ($syntax-dispatch
                                                        g658
                                                        '(any any))))
                                                   g657)))))
                                    g654)
                                  g653
                                  '0
                                  '()))))
                       (lambda (g616 g613 g615 g614)
                         ((lambda (g617)
                            ((lambda (g618)
                               ((lambda (g619)
                                  (if g619
                                      (apply
                                        (lambda (g623 g620 g622 g621)
                                          (if (andmap
                                                (lambda (g625)
                                                  (if (g256 g625)
                                                      (not (g447 g625))
                                                      '#f))
                                                g622)
                                              ((lambda (g626)
                                                 (list (list 'lambda
                                                             (list g626)
                                                             (g612 g626
                                                                   g622
                                                                   g621
                                                                   g613))
                                                       (g432 g620
                                                             g613
                                                             '(()))))
                                               (g451 'tmp))
                                              (syntax-error
                                                g617
                                                '"invalid literals list in")))
                                        g619)
                                      (syntax-error g618)))
                                ($syntax-dispatch
                                  g618
                                  '(any any each-any . each-any))))
                             g617))
                          (g394 g616 g615 g614)))))))
            (set! sc-expand
              ((lambda (g763 g761 g762)
                 ((lambda (g764)
                    (lambda (g765)
                      (if (if (pair? g765) (equal? (car g765) g53) '#f)
                          (cadr g765)
                          (g400 g765 '() g764 g763 g761 g762))))
                  (g263 (g264 '((top))) (cons g762 (g265 '((top)))))))
               'e
               '(eval)
               ((lambda (g766) (begin (g366 g766 '*top*) g766))
                (g304 '() '() '()))))
            (set! identifier? (lambda (g705) (g255 g705)))
            (set! datum->syntax-object
              (lambda (g759 g758)
                (begin ((lambda (g760)
                          (if (not (g255 g760))
                              (g93 'datum->syntax-object
                                   '"invalid argument"
                                   g760)
                              (void)))
                        g759)
                       (g203 g758 (g206 g759)))))
            (set! syntax-object->datum
              (lambda (g706) (g450 g706 '(()))))
            (set! generate-temporaries
              (lambda (g755)
                (begin ((lambda (g757)
                          (if (not (list? g757))
                              (g93 'generate-temporaries
                                   '"invalid argument"
                                   g757)
                              (void)))
                        g755)
                       (map (lambda (g756) (g393 (gensym) '((top))))
                            g755))))
            (set! free-identifier=?
              (lambda (g708 g707)
                (begin ((lambda (g710)
                          (if (not (g255 g710))
                              (g93 'free-identifier=?
                                   '"invalid argument"
                                   g710)
                              (void)))
                        g708)
                       ((lambda (g709)
                          (if (not (g255 g709))
                              (g93 'free-identifier=?
                                   '"invalid argument"
                                   g709)
                              (void)))
                        g707)
                       (g378 g708 g707))))
            (set! bound-identifier=?
              (lambda (g752 g751)
                (begin ((lambda (g754)
                          (if (not (g255 g754))
                              (g93 'bound-identifier=?
                                   '"invalid argument"
                                   g754)
                              (void)))
                        g752)
                       ((lambda (g753)
                          (if (not (g255 g753))
                              (g93 'bound-identifier=?
                                   '"invalid argument"
                                   g753)
                              (void)))
                        g751)
                       (g388 g752 g751))))
            (set! syntax-error
              (lambda (g711 . g712)
                (begin (for-each
                         (lambda (g714)
                           ((lambda (g715)
                              (if (not (string? g715))
                                  (g93 'syntax-error
                                       '"invalid argument"
                                       g715)
                                  (void)))
                            g714))
                         g712)
                       ((lambda (g713) (g93 '#f g713 (g450 g711 '(()))))
                        (if (null? g712)
                            '"invalid syntax"
                            (apply string-append g712))))))
            ((lambda ()
               (letrec ((g720
                         (lambda (g748 g745 g747 g746)
                           (if (not g746)
                               '#f
                               (if (eq? g745 'any)
                                   (cons (g393 g748 g747) g746)
                                   (if (g204 g748)
                                       (g719 ((lambda (g749)
                                                (if (g90 g749)
                                                    (annotation-expression
                                                      g749)
                                                    g749))
                                              (g205 g748))
                                             g745
                                             (g371 g747 (g206 g748))
                                             g746)
                                       (g719 ((lambda (g750)
                                                (if (g90 g750)
                                                    (annotation-expression
                                                      g750)
                                                    g750))
                                              g748)
                                             g745
                                             g747
                                             g746))))))
                        (g719
                         (lambda (g728 g725 g727 g726)
                           (if (null? g725)
                               (if (null? g728) g726 '#f)
                               (if (pair? g725)
                                   (if (pair? g728)
                                       (g720 (car g728)
                                             (car g725)
                                             g727
                                             (g720 (cdr g728)
                                                   (cdr g725)
                                                   g727
                                                   g726))
                                       '#f)
                                   (if (eq? g725 'each-any)
                                       ((lambda (g729)
                                          (if g729 (cons g729 g726) '#f))
                                        (g717 g728 g727))
                                       ((lambda (g730)
                                          (if (memv g730 '(each))
                                              (if (null? g728)
                                                  (g718 (vector-ref
                                                          g725
                                                          '1)
                                                        g726)
                                                  ((lambda (g731)
                                                     (if g731
                                                         ((letrec ((g732
                                                                    (lambda (g733)
                                                                      (if (null?
                                                                            (car g733))
                                                                          g726
                                                                          (cons (map car
                                                                                     g733)
                                                                                (g732 (map cdr
                                                                                           g733)))))))
                                                            g732)
                                                          g731)
                                                         '#f))
                                                   (g716 g728
                                                         (vector-ref
                                                           g725
                                                           '1)
                                                         g727)))
                                              (if (memv g730 '(free-id))
                                                  (if (g256 g728)
                                                      (if (g378 (g393 g728
                                                                      g727)
                                                                (vector-ref
                                                                  g725
                                                                  '1))
                                                          g726
                                                          '#f)
                                                      '#f)
                                                  (if (memv g730 '(atom))
                                                      (if (equal?
                                                            (vector-ref
                                                              g725
                                                              '1)
                                                            (g450 g728
                                                                  g727))
                                                          g726
                                                          '#f)
                                                      (if (memv g730
                                                                '(vector))
                                                          (if (vector?
                                                                g728)
                                                              (g720 (vector->list
                                                                      g728)
                                                                    (vector-ref
                                                                      g725
                                                                      '1)
                                                                    g727
                                                                    g726)
                                                              '#f)
                                                          (void))))))
                                        (vector-ref g725 '0)))))))
                        (g718
                         (lambda (g743 g742)
                           (if (null? g743)
                               g742
                               (if (eq? g743 'any)
                                   (cons '() g742)
                                   (if (pair? g743)
                                       (g718 (car g743)
                                             (g718 (cdr g743) g742))
                                       (if (eq? g743 'each-any)
                                           (cons '() g742)
                                           ((lambda (g744)
                                              (if (memv g744 '(each))
                                                  (g718 (vector-ref
                                                          g743
                                                          '1)
                                                        g742)
                                                  (if (memv g744
                                                            '(free-id
                                                               atom))
                                                      g742
                                                      (if (memv g744
                                                                '(vector))
                                                          (g718 (vector-ref
                                                                  g743
                                                                  '1)
                                                                g742)
                                                          (void)))))
                                            (vector-ref g743 '0))))))))
                        (g717
                         (lambda (g735 g734)
                           (if (g90 g735)
                               (g717 (annotation-expression g735) g734)
                               (if (pair? g735)
                                   ((lambda (g736)
                                      (if g736
                                          (cons (g393 (car g735) g734)
                                                g736)
                                          '#f))
                                    (g717 (cdr g735) g734))
                                   (if (null? g735)
                                       '()
                                       (if (g204 g735)
                                           (g717 (g205 g735)
                                                 (g371 g734 (g206 g735)))
                                           '#f))))))
                        (g716
                         (lambda (g739 g737 g738)
                           (if (g90 g739)
                               (g716 (annotation-expression g739)
                                     g737
                                     g738)
                               (if (pair? g739)
                                   ((lambda (g740)
                                      (if g740
                                          ((lambda (g741)
                                             (if g741
                                                 (cons g740 g741)
                                                 '#f))
                                           (g716 (cdr g739) g737 g738))
                                          '#f))
                                    (g720 (car g739) g737 g738 '()))
                                   (if (null? g739)
                                       '()
                                       (if (g204 g739)
                                           (g716 (g205 g739)
                                                 g737
                                                 (g371 g738 (g206 g739)))
                                           '#f)))))))
                 (set! $syntax-dispatch
                   (lambda (g722 g721)
                     (if (eq? g721 'any)
                         (list g722)
                         (if (g204 g722)
                             (g719 ((lambda (g723)
                                      (if (g90 g723)
                                          (annotation-expression g723)
                                          g723))
                                    (g205 g722))
                                   g721
                                   (g206 g722)
                                   '())
                             (g719 ((lambda (g724)
                                      (if (g90 g724)
                                          (annotation-expression g724)
                                          g724))
                                    g722)
                                   g721
                                   '(())
                                   '()))))))))))))
($sc-put-cte
  'with-syntax
  (lambda (g1828)
    ((lambda (g1829)
       ((lambda (g1830)
          (if g1830
              (apply
                (lambda (g1833 g1831 g1832)
                  (cons '#(syntax-object
                           begin
                           ((top)
                            #(ribcage
                              #(_ e1 e2)
                              #((top) (top) (top))
                              #("i" "i" "i"))
                            #(ribcage () () ())
                            #(ribcage #(x) #((top)) #("i"))
                            #(ribcage ((import-token . *top*)) () ())))
                        (cons g1831 g1832)))
                g1830)
              ((lambda (g1835)
                 (if g1835
                     (apply
                       (lambda (g1840 g1836 g1839 g1837 g1838)
                         (list '#(syntax-object
                                  syntax-case
                                  ((top)
                                   #(ribcage
                                     #(_ out in e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i" "i" "i" "i" "i"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i"))
                                   #(ribcage
                                     ((import-token . *top*))
                                     ()
                                     ())))
                               g1839
                               '()
                               (list g1836
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
                                                 #("i"))
                                               #(ribcage
                                                 ((import-token . *top*))
                                                 ()
                                                 ())))
                                           (cons g1837 g1838)))))
                       g1835)
                     ((lambda (g1842)
                        (if g1842
                            (apply
                              (lambda (g1847 g1843 g1846 g1844 g1845)
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
                                          #(ribcage #(x) #((top)) #("i"))
                                          #(ribcage
                                            ((import-token . *top*))
                                            ()
                                            ())))
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
                                                  #("i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ())))
                                            g1846)
                                      '()
                                      (list g1843
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
                                                        #("i"))
                                                      #(ribcage
                                                        ((import-token
                                                           .
                                                           *top*))
                                                        ()
                                                        ())))
                                                  (cons g1844 g1845)))))
                              g1842)
                            (syntax-error g1829)))
                      ($syntax-dispatch
                        g1829
                        '(any #(each (any any)) any . each-any)))))
               ($syntax-dispatch
                 g1829
                 '(any ((any any)) any . each-any)))))
        ($syntax-dispatch g1829 '(any () any . each-any))))
     g1828)))
($sc-put-cte
  'syntax-rules
  (lambda (g1851)
    ((lambda (g1852)
       ((lambda (g1853)
          (if g1853
              (apply
                (lambda (g1858 g1854 g1857 g1855 g1856)
                  (list '#(syntax-object
                           lambda
                           ((top)
                            #(ribcage
                              #(_ k keyword pattern template)
                              #((top) (top) (top) (top) (top))
                              #("i" "i" "i" "i" "i"))
                            #(ribcage () () ())
                            #(ribcage #(x) #((top)) #("i"))
                            #(ribcage ((import-token . *top*)) () ())))
                        '(#(syntax-object
                            x
                            ((top)
                             #(ribcage
                               #(_ k keyword pattern template)
                               #((top) (top) (top) (top) (top))
                               #("i" "i" "i" "i" "i"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i"))
                             #(ribcage ((import-token . *top*)) () ()))))
                        (cons '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(_ k keyword pattern template)
                                    #((top) (top) (top) (top) (top))
                                    #("i" "i" "i" "i" "i"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage
                                    ((import-token . *top*))
                                    ()
                                    ())))
                              (cons '#(syntax-object
                                       x
                                       ((top)
                                        #(ribcage
                                          #(_ k keyword pattern template)
                                          #((top) (top) (top) (top) (top))
                                          #("i" "i" "i" "i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage
                                          ((import-token . *top*))
                                          ()
                                          ())))
                                    (cons g1854
                                          (map (lambda (g1861 g1860)
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
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ((import-token
                                                                      .
                                                                      *top*))
                                                                   ()
                                                                   ())))
                                                             g1860)
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
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ((import-token
                                                                      .
                                                                      *top*))
                                                                   ()
                                                                   ())))
                                                             g1861)))
                                               g1856
                                               g1855))))))
                g1853)
              (syntax-error g1852)))
        ($syntax-dispatch
          g1852
          '(any each-any . #(each ((any . any) any))))))
     g1851)))
($sc-put-cte
  'or
  (lambda (g1862)
    ((lambda (g1863)
       ((lambda (g1864)
          (if g1864
              (apply
                (lambda (g1865)
                  '#(syntax-object
                     #f
                     ((top)
                      #(ribcage #(_) #((top)) #("i"))
                      #(ribcage () () ())
                      #(ribcage #(x) #((top)) #("i"))
                      #(ribcage ((import-token . *top*)) () ()))))
                g1864)
              ((lambda (g1866)
                 (if g1866
                     (apply (lambda (g1868 g1867) g1867) g1866)
                     ((lambda (g1869)
                        (if g1869
                            (apply
                              (lambda (g1873 g1870 g1872 g1871)
                                (list '#(syntax-object
                                         let
                                         ((top)
                                          #(ribcage
                                            #(_ e1 e2 e3)
                                            #((top) (top) (top) (top))
                                            #("i" "i" "i" "i"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i"))
                                          #(ribcage
                                            ((import-token . *top*))
                                            ()
                                            ())))
                                      (list (list '#(syntax-object
                                                     t
                                                     ((top)
                                                      #(ribcage
                                                        #(_ e1 e2 e3)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i" "i" "i" "i"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i"))
                                                      #(ribcage
                                                        ((import-token
                                                           .
                                                           *top*))
                                                        ()
                                                        ())))
                                                  g1870))
                                      (list '#(syntax-object
                                               if
                                               ((top)
                                                #(ribcage
                                                  #(_ e1 e2 e3)
                                                  #((top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                  #("i" "i" "i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ())))
                                            '#(syntax-object
                                               t
                                               ((top)
                                                #(ribcage
                                                  #(_ e1 e2 e3)
                                                  #((top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                  #("i" "i" "i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ())))
                                            '#(syntax-object
                                               t
                                               ((top)
                                                #(ribcage
                                                  #(_ e1 e2 e3)
                                                  #((top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                  #("i" "i" "i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ())))
                                            (cons '#(syntax-object
                                                     or
                                                     ((top)
                                                      #(ribcage
                                                        #(_ e1 e2 e3)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i" "i" "i" "i"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i"))
                                                      #(ribcage
                                                        ((import-token
                                                           .
                                                           *top*))
                                                        ()
                                                        ())))
                                                  (cons g1872 g1871)))))
                              g1869)
                            (syntax-error g1863)))
                      ($syntax-dispatch g1863 '(any any any . each-any)))))
               ($syntax-dispatch g1863 '(any any)))))
        ($syntax-dispatch g1863 '(any))))
     g1862)))
($sc-put-cte
  'and
  (lambda (g1875)
    ((lambda (g1876)
       ((lambda (g1877)
          (if g1877
              (apply
                (lambda (g1881 g1878 g1880 g1879)
                  (cons '#(syntax-object
                           if
                           ((top)
                            #(ribcage
                              #(_ e1 e2 e3)
                              #((top) (top) (top) (top))
                              #("i" "i" "i" "i"))
                            #(ribcage () () ())
                            #(ribcage #(x) #((top)) #("i"))
                            #(ribcage ((import-token . *top*)) () ())))
                        (cons g1878
                              (cons (cons '#(syntax-object
                                             and
                                             ((top)
                                              #(ribcage
                                                #(_ e1 e2 e3)
                                                #((top) (top) (top) (top))
                                                #("i" "i" "i" "i"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i"))
                                              #(ribcage
                                                ((import-token . *top*))
                                                ()
                                                ())))
                                          (cons g1880 g1879))
                                    '(#(syntax-object
                                        #f
                                        ((top)
                                         #(ribcage
                                           #(_ e1 e2 e3)
                                           #((top) (top) (top) (top))
                                           #("i" "i" "i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i"))
                                         #(ribcage
                                           ((import-token . *top*))
                                           ()
                                           ()))))))))
                g1877)
              ((lambda (g1883)
                 (if g1883
                     (apply (lambda (g1885 g1884) g1884) g1883)
                     ((lambda (g1886)
                        (if g1886
                            (apply
                              (lambda (g1887)
                                '#(syntax-object
                                   #t
                                   ((top)
                                    #(ribcage #(_) #((top)) #("i"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i"))
                                    #(ribcage
                                      ((import-token . *top*))
                                      ()
                                      ()))))
                              g1886)
                            (syntax-error g1876)))
                      ($syntax-dispatch g1876 '(any)))))
               ($syntax-dispatch g1876 '(any any)))))
        ($syntax-dispatch g1876 '(any any any . each-any))))
     g1875)))
($sc-put-cte
  'let
  (lambda (g1888)
    ((lambda (g1889)
       ((lambda (g1890)
          (if (if g1890
                  (apply
                    (lambda (g1895 g1891 g1894 g1892 g1893)
                      (andmap identifier? g1891))
                    g1890)
                  '#f)
              (apply
                (lambda (g1901 g1897 g1900 g1898 g1899)
                  (cons (cons '#(syntax-object
                                 lambda
                                 ((top)
                                  #(ribcage
                                    #(_ x v e1 e2)
                                    #((top) (top) (top) (top) (top))
                                    #("i" "i" "i" "i" "i"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage
                                    ((import-token . *top*))
                                    ()
                                    ())))
                              (cons g1897 (cons g1898 g1899)))
                        g1900))
                g1890)
              ((lambda (g1905)
                 (if (if g1905
                         (apply
                           (lambda (g1911 g1906 g1910 g1907 g1909 g1908)
                             (andmap identifier? (cons g1906 g1910)))
                           g1905)
                         '#f)
                     (apply
                       (lambda (g1918 g1913 g1917 g1914 g1916 g1915)
                         (cons (list '#(syntax-object
                                        letrec
                                        ((top)
                                         #(ribcage
                                           #(_ f x v e1 e2)
                                           #((top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top))
                                           #("i" "i" "i" "i" "i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i"))
                                         #(ribcage
                                           ((import-token . *top*))
                                           ()
                                           ())))
                                     (list (list g1913
                                                 (cons '#(syntax-object
                                                          lambda
                                                          ((top)
                                                           #(ribcage
                                                             #(_
                                                               f
                                                               x
                                                               v
                                                               e1
                                                               e2)
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
                                                             #(x)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       (cons g1917
                                                             (cons g1916
                                                                   g1915)))))
                                     g1913)
                               g1914))
                       g1905)
                     (syntax-error g1889)))
               ($syntax-dispatch
                 g1889
                 '(any any #(each (any any)) any . each-any)))))
        ($syntax-dispatch
          g1889
          '(any #(each (any any)) any . each-any))))
     g1888)))
($sc-put-cte
  'let*
  (lambda (g1922)
    ((lambda (g1923)
       ((lambda (g1924)
          (if (if g1924
                  (apply
                    (lambda (g1929 g1925 g1928 g1926 g1927)
                      (andmap identifier? g1925))
                    g1924)
                  '#f)
              (apply
                (lambda (g1935 g1931 g1934 g1932 g1933)
                  ((letrec ((g1936
                             (lambda (g1937)
                               (if (null? g1937)
                                   (cons '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(bindings)
                                               #((top))
                                               #("i"))
                                             #(ribcage
                                               #(f)
                                               #((top))
                                               #("i"))
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
                                               #("i"))
                                             #(ribcage
                                               ((import-token . *top*))
                                               ()
                                               ())))
                                         (cons '() (cons g1932 g1933)))
                                   ((lambda (g1939)
                                      ((lambda (g1940)
                                         (if g1940
                                             (apply
                                               (lambda (g1942 g1941)
                                                 (list '#(syntax-object
                                                          let
                                                          ((top)
                                                           #(ribcage
                                                             #(body
                                                               binding)
                                                             #((top) (top))
                                                             #("i" "i"))
                                                           #(ribcage
                                                             ()
                                                             ()
                                                             ())
                                                           #(ribcage
                                                             #(bindings)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             #(f)
                                                             #((top))
                                                             #("i"))
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
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       (list g1941)
                                                       g1942))
                                               g1940)
                                             (syntax-error g1939)))
                                       ($syntax-dispatch
                                         g1939
                                         '(any any))))
                                    (list (g1936 (cdr g1937))
                                          (car g1937)))))))
                     g1936)
                   (map list g1931 g1934)))
                g1924)
              (syntax-error g1923)))
        ($syntax-dispatch
          g1923
          '(any #(each (any any)) any . each-any))))
     g1922)))
($sc-put-cte
  'cond
  (lambda (g1945)
    ((lambda (g1946)
       ((lambda (g1947)
          (if g1947
              (apply
                (lambda (g1950 g1948 g1949)
                  ((letrec ((g1951
                             (lambda (g1953 g1952)
                               (if (null? g1952)
                                   ((lambda (g1954)
                                      ((lambda (g1955)
                                         (if g1955
                                             (apply
                                               (lambda (g1957 g1956)
                                                 (cons '#(syntax-object
                                                          begin
                                                          ((top)
                                                           #(ribcage
                                                             #(e1 e2)
                                                             #((top) (top))
                                                             #("i" "i"))
                                                           #(ribcage
                                                             ()
                                                             ()
                                                             ())
                                                           #(ribcage
                                                             #(clause
                                                               clauses)
                                                             #((top) (top))
                                                             #("i" "i"))
                                                           #(ribcage
                                                             #(f)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             #(_ m1 m2)
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
                                                             #(x)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       (cons g1957 g1956)))
                                               g1955)
                                             ((lambda (g1959)
                                                (if g1959
                                                    (apply
                                                      (lambda (g1960)
                                                        (cons '#(syntax-object
                                                                 let
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(e0)
                                                                    #((top))
                                                                    #("i"))
                                                                  #(ribcage
                                                                    ()
                                                                    ()
                                                                    ())
                                                                  #(ribcage
                                                                    #(clause
                                                                      clauses)
                                                                    #((top)
                                                                      (top))
                                                                    #("i"
                                                                      "i"))
                                                                  #(ribcage
                                                                    #(f)
                                                                    #((top))
                                                                    #("i"))
                                                                  #(ribcage
                                                                    #(_
                                                                      m1
                                                                      m2)
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
                                                                    #(x)
                                                                    #((top))
                                                                    #("i"))
                                                                  #(ribcage
                                                                    ((import-token
                                                                       .
                                                                       *top*))
                                                                    ()
                                                                    ())))
                                                              (cons (list (list '#(syntax-object
                                                                                   t
                                                                                   ((top)
                                                                                    #(ribcage
                                                                                      #(e0)
                                                                                      #((top))
                                                                                      #("i"))
                                                                                    #(ribcage
                                                                                      ()
                                                                                      ()
                                                                                      ())
                                                                                    #(ribcage
                                                                                      #(clause
                                                                                        clauses)
                                                                                      #((top)
                                                                                        (top))
                                                                                      #("i"
                                                                                        "i"))
                                                                                    #(ribcage
                                                                                      #(f)
                                                                                      #((top))
                                                                                      #("i"))
                                                                                    #(ribcage
                                                                                      #(_
                                                                                        m1
                                                                                        m2)
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
                                                                                      #(x)
                                                                                      #((top))
                                                                                      #("i"))
                                                                                    #(ribcage
                                                                                      ((import-token
                                                                                         .
                                                                                         *top*))
                                                                                      ()
                                                                                      ())))
                                                                                g1960))
                                                                    '((#(syntax-object
                                                                         if
                                                                         ((top)
                                                                          #(ribcage
                                                                            #(e0)
                                                                            #((top))
                                                                            #("i"))
                                                                          #(ribcage
                                                                            ()
                                                                            ()
                                                                            ())
                                                                          #(ribcage
                                                                            #(clause
                                                                              clauses)
                                                                            #((top)
                                                                              (top))
                                                                            #("i"
                                                                              "i"))
                                                                          #(ribcage
                                                                            #(f)
                                                                            #((top))
                                                                            #("i"))
                                                                          #(ribcage
                                                                            #(_
                                                                              m1
                                                                              m2)
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
                                                                            #(x)
                                                                            #((top))
                                                                            #("i"))
                                                                          #(ribcage
                                                                            ((import-token
                                                                               .
                                                                               *top*))
                                                                            ()
                                                                            ())))
                                                                        #(syntax-object
                                                                          t
                                                                          ((top)
                                                                           #(ribcage
                                                                             #(e0)
                                                                             #((top))
                                                                             #("i"))
                                                                           #(ribcage
                                                                             ()
                                                                             ()
                                                                             ())
                                                                           #(ribcage
                                                                             #(clause
                                                                               clauses)
                                                                             #((top)
                                                                               (top))
                                                                             #("i"
                                                                               "i"))
                                                                           #(ribcage
                                                                             #(f)
                                                                             #((top))
                                                                             #("i"))
                                                                           #(ribcage
                                                                             #(_
                                                                               m1
                                                                               m2)
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
                                                                             #(x)
                                                                             #((top))
                                                                             #("i"))
                                                                           #(ribcage
                                                                             ((import-token
                                                                                .
                                                                                *top*))
                                                                             ()
                                                                             ())))
                                                                        #(syntax-object
                                                                          t
                                                                          ((top)
                                                                           #(ribcage
                                                                             #(e0)
                                                                             #((top))
                                                                             #("i"))
                                                                           #(ribcage
                                                                             ()
                                                                             ()
                                                                             ())
                                                                           #(ribcage
                                                                             #(clause
                                                                               clauses)
                                                                             #((top)
                                                                               (top))
                                                                             #("i"
                                                                               "i"))
                                                                           #(ribcage
                                                                             #(f)
                                                                             #((top))
                                                                             #("i"))
                                                                           #(ribcage
                                                                             #(_
                                                                               m1
                                                                               m2)
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
                                                                             #(x)
                                                                             #((top))
                                                                             #("i"))
                                                                           #(ribcage
                                                                             ((import-token
                                                                                .
                                                                                *top*))
                                                                             ()
                                                                             ()))))))))
                                                      g1959)
                                                    ((lambda (g1961)
                                                       (if g1961
                                                           (apply
                                                             (lambda (g1963
                                                                      g1962)
                                                               (list '#(syntax-object
                                                                        let
                                                                        ((top)
                                                                         #(ribcage
                                                                           #(e0
                                                                             e1)
                                                                           #((top)
                                                                             (top))
                                                                           #("i"
                                                                             "i"))
                                                                         #(ribcage
                                                                           ()
                                                                           ()
                                                                           ())
                                                                         #(ribcage
                                                                           #(clause
                                                                             clauses)
                                                                           #((top)
                                                                             (top))
                                                                           #("i"
                                                                             "i"))
                                                                         #(ribcage
                                                                           #(f)
                                                                           #((top))
                                                                           #("i"))
                                                                         #(ribcage
                                                                           #(_
                                                                             m1
                                                                             m2)
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
                                                                           #(x)
                                                                           #((top))
                                                                           #("i"))
                                                                         #(ribcage
                                                                           ((import-token
                                                                              .
                                                                              *top*))
                                                                           ()
                                                                           ())))
                                                                     (list (list '#(syntax-object
                                                                                    t
                                                                                    ((top)
                                                                                     #(ribcage
                                                                                       #(e0
                                                                                         e1)
                                                                                       #((top)
                                                                                         (top))
                                                                                       #("i"
                                                                                         "i"))
                                                                                     #(ribcage
                                                                                       ()
                                                                                       ()
                                                                                       ())
                                                                                     #(ribcage
                                                                                       #(clause
                                                                                         clauses)
                                                                                       #((top)
                                                                                         (top))
                                                                                       #("i"
                                                                                         "i"))
                                                                                     #(ribcage
                                                                                       #(f)
                                                                                       #((top))
                                                                                       #("i"))
                                                                                     #(ribcage
                                                                                       #(_
                                                                                         m1
                                                                                         m2)
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
                                                                                       #(x)
                                                                                       #((top))
                                                                                       #("i"))
                                                                                     #(ribcage
                                                                                       ((import-token
                                                                                          .
                                                                                          *top*))
                                                                                       ()
                                                                                       ())))
                                                                                 g1963))
                                                                     (list '#(syntax-object
                                                                              if
                                                                              ((top)
                                                                               #(ribcage
                                                                                 #(e0
                                                                                   e1)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(clause
                                                                                   clauses)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 #(f)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 #(_
                                                                                   m1
                                                                                   m2)
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
                                                                                 #(x)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 ((import-token
                                                                                    .
                                                                                    *top*))
                                                                                 ()
                                                                                 ())))
                                                                           '#(syntax-object
                                                                              t
                                                                              ((top)
                                                                               #(ribcage
                                                                                 #(e0
                                                                                   e1)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(clause
                                                                                   clauses)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 #(f)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 #(_
                                                                                   m1
                                                                                   m2)
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
                                                                                 #(x)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 ((import-token
                                                                                    .
                                                                                    *top*))
                                                                                 ()
                                                                                 ())))
                                                                           (cons g1962
                                                                                 '(#(syntax-object
                                                                                     t
                                                                                     ((top)
                                                                                      #(ribcage
                                                                                        #(e0
                                                                                          e1)
                                                                                        #((top)
                                                                                          (top))
                                                                                        #("i"
                                                                                          "i"))
                                                                                      #(ribcage
                                                                                        ()
                                                                                        ()
                                                                                        ())
                                                                                      #(ribcage
                                                                                        #(clause
                                                                                          clauses)
                                                                                        #((top)
                                                                                          (top))
                                                                                        #("i"
                                                                                          "i"))
                                                                                      #(ribcage
                                                                                        #(f)
                                                                                        #((top))
                                                                                        #("i"))
                                                                                      #(ribcage
                                                                                        #(_
                                                                                          m1
                                                                                          m2)
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
                                                                                        #(x)
                                                                                        #((top))
                                                                                        #("i"))
                                                                                      #(ribcage
                                                                                        ((import-token
                                                                                           .
                                                                                           *top*))
                                                                                        ()
                                                                                        ()))))))))
                                                             g1961)
                                                           ((lambda (g1964)
                                                              (if g1964
                                                                  (apply
                                                                    (lambda (g1967
                                                                             g1965
                                                                             g1966)
                                                                      (list '#(syntax-object
                                                                               if
                                                                               ((top)
                                                                                #(ribcage
                                                                                  #(e0
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
                                                                                  #(clause
                                                                                    clauses)
                                                                                  #((top)
                                                                                    (top))
                                                                                  #("i"
                                                                                    "i"))
                                                                                #(ribcage
                                                                                  #(f)
                                                                                  #((top))
                                                                                  #("i"))
                                                                                #(ribcage
                                                                                  #(_
                                                                                    m1
                                                                                    m2)
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
                                                                                  #(x)
                                                                                  #((top))
                                                                                  #("i"))
                                                                                #(ribcage
                                                                                  ((import-token
                                                                                     .
                                                                                     *top*))
                                                                                  ()
                                                                                  ())))
                                                                            g1967
                                                                            (cons '#(syntax-object
                                                                                     begin
                                                                                     ((top)
                                                                                      #(ribcage
                                                                                        #(e0
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
                                                                                        #(clause
                                                                                          clauses)
                                                                                        #((top)
                                                                                          (top))
                                                                                        #("i"
                                                                                          "i"))
                                                                                      #(ribcage
                                                                                        #(f)
                                                                                        #((top))
                                                                                        #("i"))
                                                                                      #(ribcage
                                                                                        #(_
                                                                                          m1
                                                                                          m2)
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
                                                                                        #(x)
                                                                                        #((top))
                                                                                        #("i"))
                                                                                      #(ribcage
                                                                                        ((import-token
                                                                                           .
                                                                                           *top*))
                                                                                        ()
                                                                                        ())))
                                                                                  (cons g1965
                                                                                        g1966))))
                                                                    g1964)
                                                                  ((lambda (g1969)
                                                                     (syntax-error
                                                                       g1945))
                                                                   g1954)))
                                                            ($syntax-dispatch
                                                              g1954
                                                              '(any any
                                                                    .
                                                                    each-any)))))
                                                     ($syntax-dispatch
                                                       g1954
                                                       '(any #(free-id
                                                               #(syntax-object
                                                                 =>
                                                                 ((top)
                                                                  #(ribcage
                                                                    ()
                                                                    ()
                                                                    ())
                                                                  #(ribcage
                                                                    #(clause
                                                                      clauses)
                                                                    #((top)
                                                                      (top))
                                                                    #("i"
                                                                      "i"))
                                                                  #(ribcage
                                                                    #(f)
                                                                    #((top))
                                                                    #("i"))
                                                                  #(ribcage
                                                                    #(_
                                                                      m1
                                                                      m2)
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
                                                                    #(x)
                                                                    #((top))
                                                                    #("i"))
                                                                  #(ribcage
                                                                    ((import-token
                                                                       .
                                                                       *top*))
                                                                    ()
                                                                    ()))))
                                                             any)))))
                                              ($syntax-dispatch
                                                g1954
                                                '(any)))))
                                       ($syntax-dispatch
                                         g1954
                                         '(#(free-id
                                             #(syntax-object
                                               else
                                               ((top)
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(clause clauses)
                                                  #((top) (top))
                                                  #("i" "i"))
                                                #(ribcage
                                                  #(f)
                                                  #((top))
                                                  #("i"))
                                                #(ribcage
                                                  #(_ m1 m2)
                                                  #((top) (top) (top))
                                                  #("i" "i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ()))))
                                            any
                                            .
                                            each-any))))
                                    g1953)
                                   ((lambda (g1970)
                                      ((lambda (g1971)
                                         ((lambda (g1972)
                                            ((lambda (g1973)
                                               (if g1973
                                                   (apply
                                                     (lambda (g1974)
                                                       (list '#(syntax-object
                                                                let
                                                                ((top)
                                                                 #(ribcage
                                                                   #(e0)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   #(rest)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(clause
                                                                     clauses)
                                                                   #((top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   #(f)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   #(_
                                                                     m1
                                                                     m2)
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
                                                                   #(x)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ((import-token
                                                                      .
                                                                      *top*))
                                                                   ()
                                                                   ())))
                                                             (list (list '#(syntax-object
                                                                            t
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(e0)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               #(rest)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(clause
                                                                                 clauses)
                                                                               #((top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               #(f)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               #(_
                                                                                 m1
                                                                                 m2)
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
                                                                               #(x)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               ((import-token
                                                                                  .
                                                                                  *top*))
                                                                               ()
                                                                               ())))
                                                                         g1974))
                                                             (list '#(syntax-object
                                                                      if
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(e0)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         #(rest)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(clause
                                                                           clauses)
                                                                         #((top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         #(f)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         #(_
                                                                           m1
                                                                           m2)
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
                                                                         #(x)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         ((import-token
                                                                            .
                                                                            *top*))
                                                                         ()
                                                                         ())))
                                                                   '#(syntax-object
                                                                      t
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(e0)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         #(rest)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(clause
                                                                           clauses)
                                                                         #((top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         #(f)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         #(_
                                                                           m1
                                                                           m2)
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
                                                                         #(x)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         ((import-token
                                                                            .
                                                                            *top*))
                                                                         ()
                                                                         ())))
                                                                   '#(syntax-object
                                                                      t
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(e0)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         #(rest)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(clause
                                                                           clauses)
                                                                         #((top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         #(f)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         #(_
                                                                           m1
                                                                           m2)
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
                                                                         #(x)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         ((import-token
                                                                            .
                                                                            *top*))
                                                                         ()
                                                                         ())))
                                                                   g1971)))
                                                     g1973)
                                                   ((lambda (g1975)
                                                      (if g1975
                                                          (apply
                                                            (lambda (g1977
                                                                     g1976)
                                                              (list '#(syntax-object
                                                                       let
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(e0
                                                                            e1)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
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
                                                                          #(clause
                                                                            clauses)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(f)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          #(_
                                                                            m1
                                                                            m2)
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
                                                                          #(x)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          ((import-token
                                                                             .
                                                                             *top*))
                                                                          ()
                                                                          ())))
                                                                    (list (list '#(syntax-object
                                                                                   t
                                                                                   ((top)
                                                                                    #(ribcage
                                                                                      #(e0
                                                                                        e1)
                                                                                      #((top)
                                                                                        (top))
                                                                                      #("i"
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
                                                                                      #(clause
                                                                                        clauses)
                                                                                      #((top)
                                                                                        (top))
                                                                                      #("i"
                                                                                        "i"))
                                                                                    #(ribcage
                                                                                      #(f)
                                                                                      #((top))
                                                                                      #("i"))
                                                                                    #(ribcage
                                                                                      #(_
                                                                                        m1
                                                                                        m2)
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
                                                                                      #(x)
                                                                                      #((top))
                                                                                      #("i"))
                                                                                    #(ribcage
                                                                                      ((import-token
                                                                                         .
                                                                                         *top*))
                                                                                      ()
                                                                                      ())))
                                                                                g1977))
                                                                    (list '#(syntax-object
                                                                             if
                                                                             ((top)
                                                                              #(ribcage
                                                                                #(e0
                                                                                  e1)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
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
                                                                                #(clause
                                                                                  clauses)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"))
                                                                              #(ribcage
                                                                                #(f)
                                                                                #((top))
                                                                                #("i"))
                                                                              #(ribcage
                                                                                #(_
                                                                                  m1
                                                                                  m2)
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
                                                                                #(x)
                                                                                #((top))
                                                                                #("i"))
                                                                              #(ribcage
                                                                                ((import-token
                                                                                   .
                                                                                   *top*))
                                                                                ()
                                                                                ())))
                                                                          '#(syntax-object
                                                                             t
                                                                             ((top)
                                                                              #(ribcage
                                                                                #(e0
                                                                                  e1)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
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
                                                                                #(clause
                                                                                  clauses)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"))
                                                                              #(ribcage
                                                                                #(f)
                                                                                #((top))
                                                                                #("i"))
                                                                              #(ribcage
                                                                                #(_
                                                                                  m1
                                                                                  m2)
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
                                                                                #(x)
                                                                                #((top))
                                                                                #("i"))
                                                                              #(ribcage
                                                                                ((import-token
                                                                                   .
                                                                                   *top*))
                                                                                ()
                                                                                ())))
                                                                          (cons g1976
                                                                                '(#(syntax-object
                                                                                    t
                                                                                    ((top)
                                                                                     #(ribcage
                                                                                       #(e0
                                                                                         e1)
                                                                                       #((top)
                                                                                         (top))
                                                                                       #("i"
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
                                                                                       #(clause
                                                                                         clauses)
                                                                                       #((top)
                                                                                         (top))
                                                                                       #("i"
                                                                                         "i"))
                                                                                     #(ribcage
                                                                                       #(f)
                                                                                       #((top))
                                                                                       #("i"))
                                                                                     #(ribcage
                                                                                       #(_
                                                                                         m1
                                                                                         m2)
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
                                                                                       #(x)
                                                                                       #((top))
                                                                                       #("i"))
                                                                                     #(ribcage
                                                                                       ((import-token
                                                                                          .
                                                                                          *top*))
                                                                                       ()
                                                                                       ())))))
                                                                          g1971)))
                                                            g1975)
                                                          ((lambda (g1978)
                                                             (if g1978
                                                                 (apply
                                                                   (lambda (g1981
                                                                            g1979
                                                                            g1980)
                                                                     (list '#(syntax-object
                                                                              if
                                                                              ((top)
                                                                               #(ribcage
                                                                                 #(e0
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
                                                                                 #(clause
                                                                                   clauses)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 #(f)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 #(_
                                                                                   m1
                                                                                   m2)
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
                                                                                 #(x)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 ((import-token
                                                                                    .
                                                                                    *top*))
                                                                                 ()
                                                                                 ())))
                                                                           g1981
                                                                           (cons '#(syntax-object
                                                                                    begin
                                                                                    ((top)
                                                                                     #(ribcage
                                                                                       #(e0
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
                                                                                       #(clause
                                                                                         clauses)
                                                                                       #((top)
                                                                                         (top))
                                                                                       #("i"
                                                                                         "i"))
                                                                                     #(ribcage
                                                                                       #(f)
                                                                                       #((top))
                                                                                       #("i"))
                                                                                     #(ribcage
                                                                                       #(_
                                                                                         m1
                                                                                         m2)
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
                                                                                       #(x)
                                                                                       #((top))
                                                                                       #("i"))
                                                                                     #(ribcage
                                                                                       ((import-token
                                                                                          .
                                                                                          *top*))
                                                                                       ()
                                                                                       ())))
                                                                                 (cons g1979
                                                                                       g1980))
                                                                           g1971))
                                                                   g1978)
                                                                 ((lambda (g1983)
                                                                    (syntax-error
                                                                      g1945))
                                                                  g1972)))
                                                           ($syntax-dispatch
                                                             g1972
                                                             '(any any
                                                                   .
                                                                   each-any)))))
                                                    ($syntax-dispatch
                                                      g1972
                                                      '(any #(free-id
                                                              #(syntax-object
                                                                =>
                                                                ((top)
                                                                 #(ribcage
                                                                   #(rest)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(clause
                                                                     clauses)
                                                                   #((top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   #(f)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   #(_
                                                                     m1
                                                                     m2)
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
                                                                   #(x)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ((import-token
                                                                      .
                                                                      *top*))
                                                                   ()
                                                                   ()))))
                                                            any)))))
                                             ($syntax-dispatch
                                               g1972
                                               '(any))))
                                          g1953))
                                       g1970))
                                    (g1951 (car g1952) (cdr g1952)))))))
                     g1951)
                   g1948
                   g1949))
                g1947)
              (syntax-error g1946)))
        ($syntax-dispatch g1946 '(any any . each-any))))
     g1945)))
($sc-put-cte
  'do
  (lambda (g1985)
    ((lambda (g1986)
       ((lambda (g1987)
          (if g1987
              (apply
                (lambda (g1994 g1988 g1993 g1989 g1992 g1990 g1991)
                  ((lambda (g1995)
                     ((lambda (g2005)
                        (if g2005
                            (apply
                              (lambda (g2006)
                                ((lambda (g2007)
                                   ((lambda (g2009)
                                      (if g2009
                                          (apply
                                            (lambda ()
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
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i"))
                                                        #(ribcage
                                                          ((import-token
                                                             .
                                                             *top*))
                                                          ()
                                                          ())))
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
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i"))
                                                        #(ribcage
                                                          ((import-token
                                                             .
                                                             *top*))
                                                          ()
                                                          ())))
                                                    (map list g1988 g1993)
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
                                                                #("i"))
                                                              #(ribcage
                                                                ((import-token
                                                                   .
                                                                   *top*))
                                                                ()
                                                                ())))
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
                                                                      #("i"))
                                                                    #(ribcage
                                                                      ((import-token
                                                                         .
                                                                         *top*))
                                                                      ()
                                                                      ())))
                                                                g1992)
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
                                                                      #("i"))
                                                                    #(ribcage
                                                                      ((import-token
                                                                         .
                                                                         *top*))
                                                                      ()
                                                                      ())))
                                                                (append
                                                                  g1991
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
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ((import-token
                                                                                       .
                                                                                       *top*))
                                                                                    ()
                                                                                    ())))
                                                                              g2006)))))))
                                            g2009)
                                          ((lambda (g2014)
                                             (if g2014
                                                 (apply
                                                   (lambda (g2016 g2015)
                                                     (list '#(syntax-object
                                                              let
                                                              ((top)
                                                               #(ribcage
                                                                 #(e1 e2)
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
                                                                 #("i"))
                                                               #(ribcage
                                                                 ((import-token
                                                                    .
                                                                    *top*))
                                                                 ()
                                                                 ())))
                                                           '#(syntax-object
                                                              doloop
                                                              ((top)
                                                               #(ribcage
                                                                 #(e1 e2)
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
                                                                 #("i"))
                                                               #(ribcage
                                                                 ((import-token
                                                                    .
                                                                    *top*))
                                                                 ()
                                                                 ())))
                                                           (map list
                                                                g1988
                                                                g1993)
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
                                                                       #("i"))
                                                                     #(ribcage
                                                                       ((import-token
                                                                          .
                                                                          *top*))
                                                                       ()
                                                                       ())))
                                                                 g1992
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
                                                                             #("i"))
                                                                           #(ribcage
                                                                             ((import-token
                                                                                .
                                                                                *top*))
                                                                             ()
                                                                             ())))
                                                                       (cons g2016
                                                                             g2015))
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
                                                                             #("i"))
                                                                           #(ribcage
                                                                             ((import-token
                                                                                .
                                                                                *top*))
                                                                             ()
                                                                             ())))
                                                                       (append
                                                                         g1991
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
                                                                                           #("i"))
                                                                                         #(ribcage
                                                                                           ((import-token
                                                                                              .
                                                                                              *top*))
                                                                                           ()
                                                                                           ())))
                                                                                     g2006)))))))
                                                   g2014)
                                                 (syntax-error g2007)))
                                           ($syntax-dispatch
                                             g2007
                                             '(any . each-any)))))
                                    ($syntax-dispatch g2007 '())))
                                 g1990))
                              g2005)
                            (syntax-error g1995)))
                      ($syntax-dispatch g1995 'each-any)))
                   (map (lambda (g1999 g1998)
                          ((lambda (g2000)
                             ((lambda (g2001)
                                (if g2001
                                    (apply (lambda () g1999) g2001)
                                    ((lambda (g2002)
                                       (if g2002
                                           (apply
                                             (lambda (g2003) g2003)
                                             g2002)
                                           ((lambda (g2004)
                                              (syntax-error g1985))
                                            g2000)))
                                     ($syntax-dispatch g2000 '(any)))))
                              ($syntax-dispatch g2000 '())))
                           g1998))
                        g1988
                        g1989)))
                g1987)
              (syntax-error g1986)))
        ($syntax-dispatch
          g1986
          '(any #(each (any any . any))
                (any . each-any)
                .
                each-any))))
     g1985)))
($sc-put-cte
  'quasiquote
  (letrec ((g2030
            (lambda (g2142)
              (if (identifier? g2142)
                  (free-identifier=?
                    g2142
                    '#(syntax-object
                       quote
                       ((top)
                        #(ribcage () () ())
                        #(ribcage () () ())
                        #(ribcage #(x) #((top)) #("i"))
                        #(ribcage
                          #(isquote?
                            islist?
                            iscons?
                            quote-nil?
                            quasilist*
                            quasicons
                            quasiappend
                            quasivector
                            quasi)
                          #((top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top))
                          #("i" "i" "i" "i" "i" "i" "i" "i" "i"))
                        #(ribcage ((import-token . *top*)) () ()))))
                  '#f)))
           (g2022
            (lambda (g2036)
              (if (identifier? g2036)
                  (free-identifier=?
                    g2036
                    '#(syntax-object
                       list
                       ((top)
                        #(ribcage () () ())
                        #(ribcage () () ())
                        #(ribcage #(x) #((top)) #("i"))
                        #(ribcage
                          #(isquote?
                            islist?
                            iscons?
                            quote-nil?
                            quasilist*
                            quasicons
                            quasiappend
                            quasivector
                            quasi)
                          #((top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top))
                          #("i" "i" "i" "i" "i" "i" "i" "i" "i"))
                        #(ribcage ((import-token . *top*)) () ()))))
                  '#f)))
           (g2029
            (lambda (g2141)
              (if (identifier? g2141)
                  (free-identifier=?
                    g2141
                    '#(syntax-object
                       cons
                       ((top)
                        #(ribcage () () ())
                        #(ribcage () () ())
                        #(ribcage #(x) #((top)) #("i"))
                        #(ribcage
                          #(isquote?
                            islist?
                            iscons?
                            quote-nil?
                            quasilist*
                            quasicons
                            quasiappend
                            quasivector
                            quasi)
                          #((top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top)
                            (top))
                          #("i" "i" "i" "i" "i" "i" "i" "i" "i"))
                        #(ribcage ((import-token . *top*)) () ()))))
                  '#f)))
           (g2023
            (lambda (g2037)
              ((lambda (g2038)
                 ((lambda (g2039)
                    (if g2039
                        (apply (lambda (g2040) (g2030 g2040)) g2039)
                        ((lambda (g2041) '#f) g2038)))
                  ($syntax-dispatch g2038 '(any ()))))
               g2037)))
           (g2028
            (lambda (g2138 g2137)
              ((letrec ((g2139
                         (lambda (g2140)
                           (if (null? g2140)
                               g2137
                               (g2024 (car g2140) (g2139 (cdr g2140)))))))
                 g2139)
               g2138)))
           (g2024
            (lambda (g2043 g2042)
              ((lambda (g2044)
                 ((lambda (g2045)
                    (if g2045
                        (apply
                          (lambda (g2047 g2046)
                            ((lambda (g2048)
                               ((lambda (g2049)
                                  (if (if g2049
                                          (apply
                                            (lambda (g2051 g2050)
                                              (g2030 g2051))
                                            g2049)
                                          '#f)
                                      (apply
                                        (lambda (g2053 g2052)
                                          ((lambda (g2054)
                                             ((lambda (g2055)
                                                (if (if g2055
                                                        (apply
                                                          (lambda (g2057
                                                                   g2056)
                                                            (g2030 g2057))
                                                          g2055)
                                                        '#f)
                                                    (apply
                                                      (lambda (g2059 g2058)
                                                        (list '#(syntax-object
                                                                 quote
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(quote?
                                                                      dx)
                                                                    #((top)
                                                                      (top))
                                                                    #("i"
                                                                      "i"))
                                                                  #(ribcage
                                                                    #(quote?
                                                                      dy)
                                                                    #((top)
                                                                      (top))
                                                                    #("i"
                                                                      "i"))
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
                                                                    #(isquote?
                                                                      islist?
                                                                      iscons?
                                                                      quote-nil?
                                                                      quasilist*
                                                                      quasicons
                                                                      quasiappend
                                                                      quasivector
                                                                      quasi)
                                                                    #((top)
                                                                      (top)
                                                                      (top)
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
                                                                      "i"
                                                                      "i"
                                                                      "i"))
                                                                  #(ribcage
                                                                    ((import-token
                                                                       .
                                                                       *top*))
                                                                    ()
                                                                    ())))
                                                              (cons g2058
                                                                    g2052)))
                                                      g2055)
                                                    ((lambda (g2060)
                                                       (if (null? g2052)
                                                           (list '#(syntax-object
                                                                    list
                                                                    ((top)
                                                                     #(ribcage
                                                                       #(_)
                                                                       #((top))
                                                                       #("i"))
                                                                     #(ribcage
                                                                       #(quote?
                                                                         dy)
                                                                       #((top)
                                                                         (top))
                                                                       #("i"
                                                                         "i"))
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
                                                                       #(isquote?
                                                                         islist?
                                                                         iscons?
                                                                         quote-nil?
                                                                         quasilist*
                                                                         quasicons
                                                                         quasiappend
                                                                         quasivector
                                                                         quasi)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
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
                                                                         "i"
                                                                         "i"
                                                                         "i"))
                                                                     #(ribcage
                                                                       ((import-token
                                                                          .
                                                                          *top*))
                                                                       ()
                                                                       ())))
                                                                 g2047)
                                                           (list '#(syntax-object
                                                                    cons
                                                                    ((top)
                                                                     #(ribcage
                                                                       #(_)
                                                                       #((top))
                                                                       #("i"))
                                                                     #(ribcage
                                                                       #(quote?
                                                                         dy)
                                                                       #((top)
                                                                         (top))
                                                                       #("i"
                                                                         "i"))
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
                                                                       #(isquote?
                                                                         islist?
                                                                         iscons?
                                                                         quote-nil?
                                                                         quasilist*
                                                                         quasicons
                                                                         quasiappend
                                                                         quasivector
                                                                         quasi)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
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
                                                                         "i"
                                                                         "i"
                                                                         "i"))
                                                                     #(ribcage
                                                                       ((import-token
                                                                          .
                                                                          *top*))
                                                                       ()
                                                                       ())))
                                                                 g2047
                                                                 g2046)))
                                                     g2054)))
                                              ($syntax-dispatch
                                                g2054
                                                '(any any))))
                                           g2047))
                                        g2049)
                                      ((lambda (g2061)
                                         (if (if g2061
                                                 (apply
                                                   (lambda (g2063 g2062)
                                                     (g2022 g2063))
                                                   g2061)
                                                 '#f)
                                             (apply
                                               (lambda (g2065 g2064)
                                                 (cons '#(syntax-object
                                                          list
                                                          ((top)
                                                           #(ribcage
                                                             #(listp stuff)
                                                             #((top) (top))
                                                             #("i" "i"))
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
                                                             #(isquote?
                                                               islist?
                                                               iscons?
                                                               quote-nil?
                                                               quasilist*
                                                               quasicons
                                                               quasiappend
                                                               quasivector
                                                               quasi)
                                                             #((top)
                                                               (top)
                                                               (top)
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
                                                               "i"
                                                               "i"
                                                               "i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       (cons g2047 g2064)))
                                               g2061)
                                             ((lambda (g2066)
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
                                                            #(isquote?
                                                              islist?
                                                              iscons?
                                                              quote-nil?
                                                              quasilist*
                                                              quasicons
                                                              quasiappend
                                                              quasivector
                                                              quasi)
                                                            #((top)
                                                              (top)
                                                              (top)
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
                                                              "i"
                                                              "i"
                                                              "i"))
                                                          #(ribcage
                                                            ((import-token
                                                               .
                                                               *top*))
                                                            ()
                                                            ())))
                                                      g2047
                                                      g2046))
                                              g2048)))
                                       ($syntax-dispatch
                                         g2048
                                         '(any . any)))))
                                ($syntax-dispatch g2048 '(any any))))
                             g2046))
                          g2045)
                        (syntax-error g2044)))
                  ($syntax-dispatch g2044 '(any any))))
               (list g2043 g2042))))
           (g2027
            (lambda (g2129 g2128)
              ((lambda (g2130)
                 (if (null? g2130)
                     '(#(syntax-object
                         quote
                         ((top)
                          #(ribcage () () ())
                          #(ribcage () () ())
                          #(ribcage #(ls) #((top)) #("i"))
                          #(ribcage () () ())
                          #(ribcage () () ())
                          #(ribcage #(x y) #((top) (top)) #("i" "i"))
                          #(ribcage
                            #(isquote?
                              islist?
                              iscons?
                              quote-nil?
                              quasilist*
                              quasicons
                              quasiappend
                              quasivector
                              quasi)
                            #((top)
                              (top)
                              (top)
                              (top)
                              (top)
                              (top)
                              (top)
                              (top)
                              (top))
                            #("i" "i" "i" "i" "i" "i" "i" "i" "i"))
                          #(ribcage ((import-token . *top*)) () ())))
                        ())
                     (if (null? (cdr g2130))
                         (car g2130)
                         ((lambda (g2131)
                            ((lambda (g2132)
                               (if g2132
                                   (apply
                                     (lambda (g2133)
                                       (cons '#(syntax-object
                                                append
                                                ((top)
                                                 #(ribcage
                                                   #(p)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage () () ())
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(ls)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage () () ())
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i" "i"))
                                                 #(ribcage
                                                   #(isquote?
                                                     islist?
                                                     iscons?
                                                     quote-nil?
                                                     quasilist*
                                                     quasicons
                                                     quasiappend
                                                     quasivector
                                                     quasi)
                                                   #((top)
                                                     (top)
                                                     (top)
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
                                                     "i"
                                                     "i"
                                                     "i"))
                                                 #(ribcage
                                                   ((import-token . *top*))
                                                   ()
                                                   ())))
                                             g2133))
                                     g2132)
                                   (syntax-error g2131)))
                             ($syntax-dispatch g2131 'each-any)))
                          g2130))))
               ((letrec ((g2135
                          (lambda (g2136)
                            (if (null? g2136)
                                (if (g2023 g2128) '() (list g2128))
                                (if (g2023 (car g2136))
                                    (g2135 (cdr g2136))
                                    (cons (car g2136)
                                          (g2135 (cdr g2136))))))))
                  g2135)
                g2129))))
           (g2025
            (lambda (g2067)
              ((lambda (g2068)
                 ((lambda (g2069)
                    ((lambda (g2070)
                       ((lambda (g2071)
                          (if (if g2071
                                  (apply
                                    (lambda (g2073 g2072) (g2030 g2073))
                                    g2071)
                                  '#f)
                              (apply
                                (lambda (g2075 g2074)
                                  (list '#(syntax-object
                                           quote
                                           ((top)
                                            #(ribcage
                                              #(quote? x)
                                              #((top) (top))
                                              #("i" "i"))
                                            #(ribcage
                                              #(pat-x)
                                              #((top))
                                              #("i"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i"))
                                            #(ribcage
                                              #(isquote?
                                                islist?
                                                iscons?
                                                quote-nil?
                                                quasilist*
                                                quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top)
                                                (top)
                                                (top)
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
                                                "i"
                                                "i"
                                                "i"))
                                            #(ribcage
                                              ((import-token . *top*))
                                              ()
                                              ())))
                                        (list->vector g2074)))
                                g2071)
                              ((lambda (g2077)
                                 ((letrec ((g2078
                                            (lambda (g2080 g2079)
                                              ((lambda (g2081)
                                                 ((lambda (g2082)
                                                    (if (if g2082
                                                            (apply
                                                              (lambda (g2084
                                                                       g2083)
                                                                (g2030
                                                                  g2084))
                                                              g2082)
                                                            '#f)
                                                        (apply
                                                          (lambda (g2086
                                                                   g2085)
                                                            (g2079
                                                              (map (lambda (g2087)
                                                                     (list '#(syntax-object
                                                                              quote
                                                                              ((top)
                                                                               #(ribcage
                                                                                 #(quote?
                                                                                   x)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(x
                                                                                   k)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 #(f)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 #(_)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 #(pat-x)
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
                                                                                 #(isquote?
                                                                                   islist?
                                                                                   iscons?
                                                                                   quote-nil?
                                                                                   quasilist*
                                                                                   quasicons
                                                                                   quasiappend
                                                                                   quasivector
                                                                                   quasi)
                                                                                 #((top)
                                                                                   (top)
                                                                                   (top)
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
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ((import-token
                                                                                    .
                                                                                    *top*))
                                                                                 ()
                                                                                 ())))
                                                                           g2087))
                                                                   g2085)))
                                                          g2082)
                                                        ((lambda (g2088)
                                                           (if (if g2088
                                                                   (apply
                                                                     (lambda (g2090
                                                                              g2089)
                                                                       (g2022
                                                                         g2090))
                                                                     g2088)
                                                                   '#f)
                                                               (apply
                                                                 (lambda (g2092
                                                                          g2091)
                                                                   (g2079
                                                                     g2091))
                                                                 g2088)
                                                               ((lambda (g2094)
                                                                  (if (if g2094
                                                                          (apply
                                                                            (lambda (g2097
                                                                                     g2095
                                                                                     g2096)
                                                                              (g2029
                                                                                g2097))
                                                                            g2094)
                                                                          '#f)
                                                                      (apply
                                                                        (lambda (g2100
                                                                                 g2098
                                                                                 g2099)
                                                                          (g2078
                                                                            g2099
                                                                            (lambda (g2101)
                                                                              (g2079
                                                                                (cons g2098
                                                                                      g2101)))))
                                                                        g2094)
                                                                      ((lambda (g2102)
                                                                         (list '#(syntax-object
                                                                                  list->vector
                                                                                  ((top)
                                                                                   #(ribcage
                                                                                     #(else)
                                                                                     #((top))
                                                                                     #("i"))
                                                                                   #(ribcage
                                                                                     ()
                                                                                     ()
                                                                                     ())
                                                                                   #(ribcage
                                                                                     #(x
                                                                                       k)
                                                                                     #((top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     #(f)
                                                                                     #((top))
                                                                                     #("i"))
                                                                                   #(ribcage
                                                                                     #(_)
                                                                                     #((top))
                                                                                     #("i"))
                                                                                   #(ribcage
                                                                                     #(pat-x)
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
                                                                                     #(isquote?
                                                                                       islist?
                                                                                       iscons?
                                                                                       quote-nil?
                                                                                       quasilist*
                                                                                       quasicons
                                                                                       quasiappend
                                                                                       quasivector
                                                                                       quasi)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top)
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
                                                                                       "i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     ((import-token
                                                                                        .
                                                                                        *top*))
                                                                                     ()
                                                                                     ())))
                                                                               g2069))
                                                                       g2081)))
                                                                ($syntax-dispatch
                                                                  g2081
                                                                  '(any any
                                                                        any)))))
                                                         ($syntax-dispatch
                                                           g2081
                                                           '(any .
                                                                 each-any)))))
                                                  ($syntax-dispatch
                                                    g2081
                                                    '(any each-any))))
                                               g2080))))
                                    g2078)
                                  g2067
                                  (lambda (g2103)
                                    (cons '#(syntax-object
                                             vector
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(ls)
                                                #((top))
                                                #("i"))
                                              #(ribcage
                                                #(_)
                                                #((top))
                                                #("i"))
                                              #(ribcage
                                                #(pat-x)
                                                #((top))
                                                #("i"))
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i"))
                                              #(ribcage
                                                #(isquote?
                                                  islist?
                                                  iscons?
                                                  quote-nil?
                                                  quasilist*
                                                  quasicons
                                                  quasiappend
                                                  quasivector
                                                  quasi)
                                                #((top)
                                                  (top)
                                                  (top)
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
                                                  "i"
                                                  "i"
                                                  "i"))
                                              #(ribcage
                                                ((import-token . *top*))
                                                ()
                                                ())))
                                          g2103))))
                               g2070)))
                        ($syntax-dispatch g2070 '(any each-any))))
                     g2069))
                  g2068))
               g2067)))
           (g2026
            (lambda (g2105 g2104)
              ((lambda (g2106)
                 ((lambda (g2107)
                    (if g2107
                        (apply
                          (lambda (g2108)
                            (if (= g2104 '0)
                                g2108
                                (g2024
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
                                         #(isquote?
                                           islist?
                                           iscons?
                                           quote-nil?
                                           quasilist*
                                           quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top)
                                           (top)
                                           (top)
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
                                           "i"
                                           "i"
                                           "i"))
                                       #(ribcage
                                         ((import-token . *top*))
                                         ()
                                         ())))
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
                                          #(isquote?
                                            islist?
                                            iscons?
                                            quote-nil?
                                            quasilist*
                                            quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top)
                                            (top)
                                            (top)
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
                                            "i"
                                            "i"
                                            "i"))
                                        #(ribcage
                                          ((import-token . *top*))
                                          ()
                                          ()))))
                                  (g2026 (list g2108) (- g2104 '1)))))
                          g2107)
                        ((lambda (g2109)
                           (if g2109
                               (apply
                                 (lambda (g2111 g2110)
                                   (if (= g2104 '0)
                                       (g2028 g2111 (g2026 g2110 g2104))
                                       (g2024
                                         (g2024
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
                                                  #(isquote?
                                                    islist?
                                                    iscons?
                                                    quote-nil?
                                                    quasilist*
                                                    quasicons
                                                    quasiappend
                                                    quasivector
                                                    quasi)
                                                  #((top)
                                                    (top)
                                                    (top)
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
                                                    "i"
                                                    "i"
                                                    "i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ())))
                                              #(syntax-object
                                                unquote
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
                                                   #(isquote?
                                                     islist?
                                                     iscons?
                                                     quote-nil?
                                                     quasilist*
                                                     quasicons
                                                     quasiappend
                                                     quasivector
                                                     quasi)
                                                   #((top)
                                                     (top)
                                                     (top)
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
                                                     "i"
                                                     "i"
                                                     "i"))
                                                 #(ribcage
                                                   ((import-token . *top*))
                                                   ()
                                                   ()))))
                                           (g2026 g2111 (- g2104 '1)))
                                         (g2026 g2110 g2104))))
                                 g2109)
                               ((lambda (g2114)
                                  (if g2114
                                      (apply
                                        (lambda (g2116 g2115)
                                          (if (= g2104 '0)
                                              (g2027
                                                g2116
                                                (g2026 g2115 g2104))
                                              (g2024
                                                (g2024
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
                                                         #(isquote?
                                                           islist?
                                                           iscons?
                                                           quote-nil?
                                                           quasilist*
                                                           quasicons
                                                           quasiappend
                                                           quasivector
                                                           quasi)
                                                         #((top)
                                                           (top)
                                                           (top)
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
                                                           "i"
                                                           "i"
                                                           "i"))
                                                       #(ribcage
                                                         ((import-token
                                                            .
                                                            *top*))
                                                         ()
                                                         ())))
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
                                                          #(isquote?
                                                            islist?
                                                            iscons?
                                                            quote-nil?
                                                            quasilist*
                                                            quasicons
                                                            quasiappend
                                                            quasivector
                                                            quasi)
                                                          #((top)
                                                            (top)
                                                            (top)
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
                                                            "i"
                                                            "i"
                                                            "i"))
                                                        #(ribcage
                                                          ((import-token
                                                             .
                                                             *top*))
                                                          ()
                                                          ()))))
                                                  (g2026
                                                    g2116
                                                    (- g2104 '1)))
                                                (g2026 g2115 g2104))))
                                        g2114)
                                      ((lambda (g2119)
                                         (if g2119
                                             (apply
                                               (lambda (g2120)
                                                 (g2024
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
                                                          #(isquote?
                                                            islist?
                                                            iscons?
                                                            quote-nil?
                                                            quasilist*
                                                            quasicons
                                                            quasiappend
                                                            quasivector
                                                            quasi)
                                                          #((top)
                                                            (top)
                                                            (top)
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
                                                            "i"
                                                            "i"
                                                            "i"))
                                                        #(ribcage
                                                          ((import-token
                                                             .
                                                             *top*))
                                                          ()
                                                          ())))
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
                                                           #(isquote?
                                                             islist?
                                                             iscons?
                                                             quote-nil?
                                                             quasilist*
                                                             quasicons
                                                             quasiappend
                                                             quasivector
                                                             quasi)
                                                           #((top)
                                                             (top)
                                                             (top)
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
                                                             "i"
                                                             "i"
                                                             "i"))
                                                         #(ribcage
                                                           ((import-token
                                                              .
                                                              *top*))
                                                           ()
                                                           ()))))
                                                   (g2026
                                                     (list g2120)
                                                     (+ g2104 '1))))
                                               g2119)
                                             ((lambda (g2121)
                                                (if g2121
                                                    (apply
                                                      (lambda (g2123 g2122)
                                                        (g2024
                                                          (g2026
                                                            g2123
                                                            g2104)
                                                          (g2026
                                                            g2122
                                                            g2104)))
                                                      g2121)
                                                    ((lambda (g2124)
                                                       (if g2124
                                                           (apply
                                                             (lambda (g2125)
                                                               (g2025
                                                                 (g2026
                                                                   g2125
                                                                   g2104)))
                                                             g2124)
                                                           ((lambda (g2127)
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
                                                                          #(isquote?
                                                                            islist?
                                                                            iscons?
                                                                            quote-nil?
                                                                            quasilist*
                                                                            quasicons
                                                                            quasiappend
                                                                            quasivector
                                                                            quasi)
                                                                          #((top)
                                                                            (top)
                                                                            (top)
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
                                                                            "i"
                                                                            "i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          ((import-token
                                                                             .
                                                                             *top*))
                                                                          ()
                                                                          ())))
                                                                    g2127))
                                                            g2106)))
                                                     ($syntax-dispatch
                                                       g2106
                                                       '#(vector
                                                          each-any)))))
                                              ($syntax-dispatch
                                                g2106
                                                '(any . any)))))
                                       ($syntax-dispatch
                                         g2106
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
                                                  #(isquote?
                                                    islist?
                                                    iscons?
                                                    quote-nil?
                                                    quasilist*
                                                    quasicons
                                                    quasiappend
                                                    quasivector
                                                    quasi)
                                                  #((top)
                                                    (top)
                                                    (top)
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
                                                    "i"
                                                    "i"
                                                    "i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ()))))
                                            any)))))
                                ($syntax-dispatch
                                  g2106
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
                                            #(isquote?
                                              islist?
                                              iscons?
                                              quote-nil?
                                              quasilist*
                                              quasicons
                                              quasiappend
                                              quasivector
                                              quasi)
                                            #((top)
                                              (top)
                                              (top)
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
                                              "i"
                                              "i"
                                              "i"))
                                          #(ribcage
                                            ((import-token . *top*))
                                            ()
                                            ()))))
                                      .
                                      each-any)
                                    .
                                    any)))))
                         ($syntax-dispatch
                           g2106
                           '((#(free-id
                                #(syntax-object
                                  unquote
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(p lev)
                                     #((top) (top))
                                     #("i" "i"))
                                   #(ribcage
                                     #(isquote?
                                       islist?
                                       iscons?
                                       quote-nil?
                                       quasilist*
                                       quasicons
                                       quasiappend
                                       quasivector
                                       quasi)
                                     #((top)
                                       (top)
                                       (top)
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
                                       "i"
                                       "i"
                                       "i"))
                                   #(ribcage
                                     ((import-token . *top*))
                                     ()
                                     ()))))
                               .
                               each-any)
                             .
                             any)))))
                  ($syntax-dispatch
                    g2106
                    '(#(free-id
                        #(syntax-object
                          unquote
                          ((top)
                           #(ribcage () () ())
                           #(ribcage #(p lev) #((top) (top)) #("i" "i"))
                           #(ribcage
                             #(isquote?
                               islist?
                               iscons?
                               quote-nil?
                               quasilist*
                               quasicons
                               quasiappend
                               quasivector
                               quasi)
                             #((top)
                               (top)
                               (top)
                               (top)
                               (top)
                               (top)
                               (top)
                               (top)
                               (top))
                             #("i" "i" "i" "i" "i" "i" "i" "i" "i"))
                           #(ribcage ((import-token . *top*)) () ()))))
                       any))))
               g2105))))
    (lambda (g2031)
      ((lambda (g2032)
         ((lambda (g2033)
            (if g2033
                (apply (lambda (g2035 g2034) (g2026 g2034 '0)) g2033)
                (syntax-error g2032)))
          ($syntax-dispatch g2032 '(any any))))
       g2031))))
($sc-put-cte
  'include
  (lambda (g2143)
    (letrec ((g2144
              (lambda (g2155 g2154)
                ((lambda (g2156)
                   ((letrec ((g2157
                              (lambda ()
                                ((lambda (g2158)
                                   (if (eof-object? g2158)
                                       (begin (close-input-port g2156) '())
                                       (cons (datum->syntax-object
                                               g2154
                                               g2158)
                                             (g2157))))
                                 (read g2156)))))
                      g2157)))
                 (open-input-file g2155)))))
      ((lambda (g2145)
         ((lambda (g2146)
            (if g2146
                (apply
                  (lambda (g2148 g2147)
                    ((lambda (g2149)
                       ((lambda (g2150)
                          ((lambda (g2151)
                             (if g2151
                                 (apply
                                   (lambda (g2152)
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
                                                 #("i"))
                                               #(ribcage
                                                 ((import-token . *top*))
                                                 ()
                                                 ())))
                                           g2152))
                                   g2151)
                                 (syntax-error g2150)))
                           ($syntax-dispatch g2150 'each-any)))
                        (g2144 g2149 g2148)))
                     (syntax-object->datum g2147)))
                  g2146)
                (syntax-error g2145)))
          ($syntax-dispatch g2145 '(any any))))
       g2143))))
($sc-put-cte
  'unquote
  (lambda (g2159)
    ((lambda (g2160)
       ((lambda (g2161)
          (if g2161
              (apply
                (lambda (g2163 g2162)
                  (syntax-error
                    g2159
                    '"expression not valid outside of quasiquote"))
                g2161)
              (syntax-error g2160)))
        ($syntax-dispatch g2160 '(any . each-any))))
     g2159)))
($sc-put-cte
  'unquote-splicing
  (lambda (g2164)
    ((lambda (g2165)
       ((lambda (g2166)
          (if g2166
              (apply
                (lambda (g2168 g2167)
                  (syntax-error
                    g2164
                    '"expression not valid outside of quasiquote"))
                g2166)
              (syntax-error g2165)))
        ($syntax-dispatch g2165 '(any . each-any))))
     g2164)))
($sc-put-cte
  'case
  (lambda (g2169)
    ((lambda (g2170)
       ((lambda (g2171)
          (if g2171
              (apply
                (lambda (g2175 g2172 g2174 g2173)
                  ((lambda (g2176)
                     ((lambda (g2203)
                        (list '#(syntax-object
                                 let
                                 ((top)
                                  #(ribcage #(body) #((top)) #("i"))
                                  #(ribcage
                                    #(_ e m1 m2)
                                    #((top) (top) (top) (top))
                                    #("i" "i" "i" "i"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage
                                    ((import-token . *top*))
                                    ()
                                    ())))
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
                                                #("i"))
                                              #(ribcage
                                                ((import-token . *top*))
                                                ()
                                                ())))
                                          g2172))
                              g2203))
                      g2176))
                   ((letrec ((g2177
                              (lambda (g2179 g2178)
                                (if (null? g2178)
                                    ((lambda (g2180)
                                       ((lambda (g2181)
                                          (if g2181
                                              (apply
                                                (lambda (g2183 g2182)
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
                                                              #(clause
                                                                clauses)
                                                              #((top)
                                                                (top))
                                                              #("i" "i"))
                                                            #(ribcage
                                                              #(f)
                                                              #((top))
                                                              #("i"))
                                                            #(ribcage
                                                              #(_ e m1 m2)
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
                                                              #("i"))
                                                            #(ribcage
                                                              ((import-token
                                                                 .
                                                                 *top*))
                                                              ()
                                                              ())))
                                                        (cons g2183
                                                              g2182)))
                                                g2181)
                                              ((lambda (g2185)
                                                 (if g2185
                                                     (apply
                                                       (lambda (g2188
                                                                g2186
                                                                g2187)
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
                                                                     #(clause
                                                                       clauses)
                                                                     #((top)
                                                                       (top))
                                                                     #("i"
                                                                       "i"))
                                                                   #(ribcage
                                                                     #(f)
                                                                     #((top))
                                                                     #("i"))
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
                                                                     #("i"))
                                                                   #(ribcage
                                                                     ((import-token
                                                                        .
                                                                        *top*))
                                                                     ()
                                                                     ())))
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
                                                                           #(clause
                                                                             clauses)
                                                                           #((top)
                                                                             (top))
                                                                           #("i"
                                                                             "i"))
                                                                         #(ribcage
                                                                           #(f)
                                                                           #((top))
                                                                           #("i"))
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
                                                                           #("i"))
                                                                         #(ribcage
                                                                           ((import-token
                                                                              .
                                                                              *top*))
                                                                           ()
                                                                           ())))
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
                                                                           #(clause
                                                                             clauses)
                                                                           #((top)
                                                                             (top))
                                                                           #("i"
                                                                             "i"))
                                                                         #(ribcage
                                                                           #(f)
                                                                           #((top))
                                                                           #("i"))
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
                                                                           #("i"))
                                                                         #(ribcage
                                                                           ((import-token
                                                                              .
                                                                              *top*))
                                                                           ()
                                                                           ())))
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
                                                                                 #(clause
                                                                                   clauses)
                                                                                 #((top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 #(f)
                                                                                 #((top))
                                                                                 #("i"))
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
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 ((import-token
                                                                                    .
                                                                                    *top*))
                                                                                 ()
                                                                                 ())))
                                                                           g2188))
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
                                                                           #(clause
                                                                             clauses)
                                                                           #((top)
                                                                             (top))
                                                                           #("i"
                                                                             "i"))
                                                                         #(ribcage
                                                                           #(f)
                                                                           #((top))
                                                                           #("i"))
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
                                                                           #("i"))
                                                                         #(ribcage
                                                                           ((import-token
                                                                              .
                                                                              *top*))
                                                                           ()
                                                                           ())))
                                                                     (cons g2186
                                                                           g2187))))
                                                       g2185)
                                                     ((lambda (g2191)
                                                        (syntax-error
                                                          g2169))
                                                      g2180)))
                                               ($syntax-dispatch
                                                 g2180
                                                 '(each-any
                                                    any
                                                    .
                                                    each-any)))))
                                        ($syntax-dispatch
                                          g2180
                                          '(#(free-id
                                              #(syntax-object
                                                else
                                                ((top)
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(clause clauses)
                                                   #((top) (top))
                                                   #("i" "i"))
                                                 #(ribcage
                                                   #(f)
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
                                                   #("i"))
                                                 #(ribcage
                                                   ((import-token . *top*))
                                                   ()
                                                   ()))))
                                             any
                                             .
                                             each-any))))
                                     g2179)
                                    ((lambda (g2192)
                                       ((lambda (g2193)
                                          ((lambda (g2194)
                                             ((lambda (g2195)
                                                (if g2195
                                                    (apply
                                                      (lambda (g2198
                                                               g2196
                                                               g2197)
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
                                                                    #(clause
                                                                      clauses)
                                                                    #((top)
                                                                      (top))
                                                                    #("i"
                                                                      "i"))
                                                                  #(ribcage
                                                                    #(f)
                                                                    #((top))
                                                                    #("i"))
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
                                                                    #("i"))
                                                                  #(ribcage
                                                                    ((import-token
                                                                       .
                                                                       *top*))
                                                                    ()
                                                                    ())))
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
                                                                          #(clause
                                                                            clauses)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(f)
                                                                          #((top))
                                                                          #("i"))
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
                                                                          #("i"))
                                                                        #(ribcage
                                                                          ((import-token
                                                                             .
                                                                             *top*))
                                                                          ()
                                                                          ())))
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
                                                                          #(clause
                                                                            clauses)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(f)
                                                                          #((top))
                                                                          #("i"))
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
                                                                          #("i"))
                                                                        #(ribcage
                                                                          ((import-token
                                                                             .
                                                                             *top*))
                                                                          ()
                                                                          ())))
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
                                                                                #(clause
                                                                                  clauses)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"))
                                                                              #(ribcage
                                                                                #(f)
                                                                                #((top))
                                                                                #("i"))
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
                                                                                #("i"))
                                                                              #(ribcage
                                                                                ((import-token
                                                                                   .
                                                                                   *top*))
                                                                                ()
                                                                                ())))
                                                                          g2198))
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
                                                                          #(clause
                                                                            clauses)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(f)
                                                                          #((top))
                                                                          #("i"))
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
                                                                          #("i"))
                                                                        #(ribcage
                                                                          ((import-token
                                                                             .
                                                                             *top*))
                                                                          ()
                                                                          ())))
                                                                    (cons g2196
                                                                          g2197))
                                                              g2193))
                                                      g2195)
                                                    ((lambda (g2201)
                                                       (syntax-error
                                                         g2169))
                                                     g2194)))
                                              ($syntax-dispatch
                                                g2194
                                                '(each-any
                                                   any
                                                   .
                                                   each-any))))
                                           g2179))
                                        g2192))
                                     (g2177 (car g2178) (cdr g2178)))))))
                      g2177)
                    g2174
                    g2173)))
                g2171)
              (syntax-error g2170)))
        ($syntax-dispatch g2170 '(any any any . each-any))))
     g2169)))
($sc-put-cte
  'identifier-syntax
  (lambda (g2204)
    ((lambda (g2205)
       ((lambda (g2206)
          (if g2206
              (apply
                (lambda (g2208 g2207)
                  (list '#(syntax-object
                           lambda
                           ((top)
                            #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                            #(ribcage () () ())
                            #(ribcage #(x) #((top)) #("i"))
                            #(ribcage ((import-token . *top*)) () ())))
                        '(#(syntax-object
                            x
                            ((top)
                             #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i"))
                             #(ribcage ((import-token . *top*)) () ()))))
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(_ e)
                                    #((top) (top))
                                    #("i" "i"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage
                                    ((import-token . *top*))
                                    ()
                                    ())))
                              '#(syntax-object
                                 x
                                 ((top)
                                  #(ribcage
                                    #(_ e)
                                    #((top) (top))
                                    #("i" "i"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage
                                    ((import-token . *top*))
                                    ()
                                    ())))
                              '()
                              (list '#(syntax-object
                                       id
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage
                                          ((import-token . *top*))
                                          ()
                                          ())))
                                    '(#(syntax-object
                                        identifier?
                                        ((top)
                                         #(ribcage
                                           #(_ e)
                                           #((top) (top))
                                           #("i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i"))
                                         #(ribcage
                                           ((import-token . *top*))
                                           ()
                                           ())))
                                       (#(syntax-object
                                          syntax
                                          ((top)
                                           #(ribcage
                                             #(_ e)
                                             #((top) (top))
                                             #("i" "i"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i"))
                                           #(ribcage
                                             ((import-token . *top*))
                                             ()
                                             ())))
                                         #(syntax-object
                                           id
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i" "i"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i"))
                                            #(ribcage
                                              ((import-token . *top*))
                                              ()
                                              ())))))
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
                                                #("i"))
                                              #(ribcage
                                                ((import-token . *top*))
                                                ()
                                                ())))
                                          g2207))
                              (list (cons g2208
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
                                                 #("i"))
                                               #(ribcage
                                                 ((import-token . *top*))
                                                 ()
                                                 ())))
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
                                                  #("i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ())))))
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
                                                #("i"))
                                              #(ribcage
                                                ((import-token . *top*))
                                                ()
                                                ())))
                                          (cons g2207
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
                                                       #("i"))
                                                     #(ribcage
                                                       ((import-token
                                                          .
                                                          *top*))
                                                       ()
                                                       ())))
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
                                                        #("i"))
                                                      #(ribcage
                                                        ((import-token
                                                           .
                                                           *top*))
                                                        ()
                                                        ()))))))))))
                g2206)
              ((lambda (g2209)
                 (if (if g2209
                         (apply
                           (lambda (g2215 g2210 g2214 g2211 g2213 g2212)
                             (if (identifier? g2210)
                                 (identifier? g2211)
                                 '#f))
                           g2209)
                         '#f)
                     (apply
                       (lambda (g2221 g2216 g2220 g2217 g2219 g2218)
                         (list '#(syntax-object
                                  cons
                                  ((top)
                                   #(ribcage
                                     #(_ id exp1 var val exp2)
                                     #((top) (top) (top) (top) (top) (top))
                                     #("i" "i" "i" "i" "i" "i"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i"))
                                   #(ribcage
                                     ((import-token . *top*))
                                     ()
                                     ())))
                               '(#(syntax-object
                                   quote
                                   ((top)
                                    #(ribcage
                                      #(_ id exp1 var val exp2)
                                      #((top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top))
                                      #("i" "i" "i" "i" "i" "i"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i"))
                                    #(ribcage
                                      ((import-token . *top*))
                                      ()
                                      ())))
                                  #(syntax-object
                                    macro!
                                    ((top)
                                     #(ribcage
                                       #(_ id exp1 var val exp2)
                                       #((top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top))
                                       #("i" "i" "i" "i" "i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i"))
                                     #(ribcage
                                       ((import-token . *top*))
                                       ()
                                       ()))))
                               (list '#(syntax-object
                                        lambda
                                        ((top)
                                         #(ribcage
                                           #(_ id exp1 var val exp2)
                                           #((top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top))
                                           #("i" "i" "i" "i" "i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i"))
                                         #(ribcage
                                           ((import-token . *top*))
                                           ()
                                           ())))
                                     '(#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(_ id exp1 var val exp2)
                                            #((top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top))
                                            #("i" "i" "i" "i" "i" "i"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i"))
                                          #(ribcage
                                            ((import-token . *top*))
                                            ()
                                            ()))))
                                     (list '#(syntax-object
                                              syntax-case
                                              ((top)
                                               #(ribcage
                                                 #(_ id exp1 var val exp2)
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
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x)
                                                 #((top))
                                                 #("i"))
                                               #(ribcage
                                                 ((import-token . *top*))
                                                 ()
                                                 ())))
                                           '#(syntax-object
                                              x
                                              ((top)
                                               #(ribcage
                                                 #(_ id exp1 var val exp2)
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
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x)
                                                 #((top))
                                                 #("i"))
                                               #(ribcage
                                                 ((import-token . *top*))
                                                 ()
                                                 ())))
                                           '(#(syntax-object
                                               set!
                                               ((top)
                                                #(ribcage
                                                  #(_ id exp1 var val exp2)
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
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i"))
                                                #(ribcage
                                                  ((import-token . *top*))
                                                  ()
                                                  ()))))
                                           (list (list '#(syntax-object
                                                          set!
                                                          ((top)
                                                           #(ribcage
                                                             #(_
                                                               id
                                                               exp1
                                                               var
                                                               val
                                                               exp2)
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
                                                             #(x)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       g2217
                                                       g2219)
                                                 (list '#(syntax-object
                                                          syntax
                                                          ((top)
                                                           #(ribcage
                                                             #(_
                                                               id
                                                               exp1
                                                               var
                                                               val
                                                               exp2)
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
                                                             #(x)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       g2218))
                                           (list (cons g2216
                                                       '(#(syntax-object
                                                           x
                                                           ((top)
                                                            #(ribcage
                                                              #(_
                                                                id
                                                                exp1
                                                                var
                                                                val
                                                                exp2)
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
                                                              #(x)
                                                              #((top))
                                                              #("i"))
                                                            #(ribcage
                                                              ((import-token
                                                                 .
                                                                 *top*))
                                                              ()
                                                              ())))
                                                          #(syntax-object
                                                            ...
                                                            ((top)
                                                             #(ribcage
                                                               #(_
                                                                 id
                                                                 exp1
                                                                 var
                                                                 val
                                                                 exp2)
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
                                                               #(x)
                                                               #((top))
                                                               #("i"))
                                                             #(ribcage
                                                               ((import-token
                                                                  .
                                                                  *top*))
                                                               ()
                                                               ())))))
                                                 (list '#(syntax-object
                                                          syntax
                                                          ((top)
                                                           #(ribcage
                                                             #(_
                                                               id
                                                               exp1
                                                               var
                                                               val
                                                               exp2)
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
                                                             #(x)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       (cons g2220
                                                             '(#(syntax-object
                                                                 x
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(_
                                                                      id
                                                                      exp1
                                                                      var
                                                                      val
                                                                      exp2)
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
                                                                    #(x)
                                                                    #((top))
                                                                    #("i"))
                                                                  #(ribcage
                                                                    ((import-token
                                                                       .
                                                                       *top*))
                                                                    ()
                                                                    ())))
                                                                #(syntax-object
                                                                  ...
                                                                  ((top)
                                                                   #(ribcage
                                                                     #(_
                                                                       id
                                                                       exp1
                                                                       var
                                                                       val
                                                                       exp2)
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
                                                                     #(x)
                                                                     #((top))
                                                                     #("i"))
                                                                   #(ribcage
                                                                     ((import-token
                                                                        .
                                                                        *top*))
                                                                     ()
                                                                     ())))))))
                                           (list g2216
                                                 (list '#(syntax-object
                                                          identifier?
                                                          ((top)
                                                           #(ribcage
                                                             #(_
                                                               id
                                                               exp1
                                                               var
                                                               val
                                                               exp2)
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
                                                             #(x)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       (list '#(syntax-object
                                                                syntax
                                                                ((top)
                                                                 #(ribcage
                                                                   #(_
                                                                     id
                                                                     exp1
                                                                     var
                                                                     val
                                                                     exp2)
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
                                                                   #(x)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ((import-token
                                                                      .
                                                                      *top*))
                                                                   ()
                                                                   ())))
                                                             g2216))
                                                 (list '#(syntax-object
                                                          syntax
                                                          ((top)
                                                           #(ribcage
                                                             #(_
                                                               id
                                                               exp1
                                                               var
                                                               val
                                                               exp2)
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
                                                             #(x)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage
                                                             ((import-token
                                                                .
                                                                *top*))
                                                             ()
                                                             ())))
                                                       g2220))))))
                       g2209)
                     (syntax-error g2205)))
               ($syntax-dispatch
                 g2205
                 '(any (any any)
                       ((#(free-id
                           #(syntax-object
                             set!
                             ((top)
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i"))
                              #(ribcage ((import-token . *top*)) () ()))))
                          any
                          any)
                        any))))))
        ($syntax-dispatch g2205 '(any any))))
     g2204)))
