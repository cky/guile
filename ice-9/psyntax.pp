;;; psyntax.pp
;;; automatically generated from psyntax.ss
;;; Mon Aug 18 20:35:13 EST 1997
;;; see copyright notice in psyntax.ss

((lambda ()
((lambda ()
(letrec ((g115 (lambda (g800)
((letrec ((g801 (lambda (g804 g802 g803)
(if (pair? g804)
(g801 (cdr g804)
(cons (g94 (car g804)
g803)
g802)
g803)
(if (g66 g804)
(cons (g94 g804 g803)
g802)
(if (null? g804)
g802
(if (g52 g804)
(g801 (g53 g804)
g802
(g85 g803
(g54 g804)))
(if (g43 g804)
(g801 (annotation-expression
g804)
g802
g803)
(cons g804
g802)))))))))
g801)
g800
'()
'(()))))
(g114 (lambda (g322)
((lambda (g323) (if (g43 g323) (gensym) (gensym)))
(if (g52 g322) (g53 g322) g322))))
(g113 (lambda (g792 g791)
(if (memq 'top (g69 g791))
(if ((lambda (g793)
(if g793
g793
(if (pair? g792)
(g43 (car g792))
'#f)))
(g43 g792))
(g112 g792 '#f)
g792)
((letrec ((g794 (lambda (g795)
(if (g52 g795)
(g113 (g53 g795)
(g54 g795))
(if (pair? g795)
((lambda (g797 g796)
(if (if (eq? g797
(car g795))
(eq? g796
(cdr g795))
'#f)
g795
(cons g797
g796)))
(g794 (car g795))
(g794 (cdr g795)))
(if (vector? g795)
((lambda (g798)
((lambda (g799)
(if (andmap
eq?
g798
g799)
g795
(list->vector
g799)))
(map g794
g798)))
(vector->list
g795))
g795))))))
g794)
g792))))
(g112 (lambda (g325 g324)
(if (pair? g325)
((lambda (g326)
(begin (when g324
(set-annotation-stripped!
g324
g326))
(set-car! g326 (g112 (car g325) '#f))
(set-cdr! g326 (g112 (cdr g325) '#f))
g326))
(cons '#f '#f))
(if (g43 g325)
((lambda (g327)
(if g327
g327
(g112 (annotation-expression g325)
g325)))
(annotation-stripped g325))
(if (vector? g325)
((lambda (g328)
(begin (when g324
(set-annotation-stripped!
g324
g328))
((letrec ((g329 (lambda (g330)
(unless (g42 g330
'0)
(vector-set!
g328
g330
(g112 (vector-ref
g325
g330)
'#f))
(g329 (g40 g330
'1))))))
g329)
(- (vector-length g325) '1))
g328))
(make-vector (vector-length g325)))
g325)))))
(g111 (lambda (g790)
(if (g65 g790)
(g89 g790
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
strip-annotation
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
he-anti-mark
op-marked?
op-wrap
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
build-global-definition
build-global-assignment
build-global-reference
build-lexical-assignment
build-lexical-reference
build-conditional
build-application
get-global-definition-hook
put-global-definition-hook
gensym-hook
error-hook
local-eval-hook
op-level-eval-hook
annotation?
fx<
fx=
fx-
fx+
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
"i"))
#(ribcage
(define-structure)
((top))
("i"))
#(ribcage
(define-structure)
((top))
("i")))))
'#f)))
(g110 (lambda () (list 'void)))
(g109 (lambda (g788)
((lambda (g789)
(if (procedure? g789)
g789
(syntax-error
g789
'"nonprocedure transfomer")))
(g45 g788))))
(g108 (lambda (g336 g331 g335 g332 g334 g333)
((lambda (g337)
((lambda (g338)
(if g338
(apply
(lambda (g343 g339 g342 g340 g341)
((lambda (g344)
(if (not (g91 g344))
(syntax-error
g331
'"duplicate bound keyword in")
((lambda (g345)
((lambda (g346)
(g333 (cons g340 g341)
(g60 g345
((lambda (g348
g347)
(map (lambda (g350)
(cons 'macro
(g109 (g102 g350
g347
g348))))
g342))
(if g336
g346
g332)
(g62 g335))
g335)
g346
g334))
(g83 g344 g345 g332)))
(g72 g344))))
g339))
g338)
((lambda (g353)
(syntax-error (g95 g331 g332 g334)))
g337)))
(syntax-dispatch
g337
'(any #(each (any any)) any . each-any))))
g331)))
(g107 (lambda (g765 g761 g764 g762 g763)
((lambda (g766)
((lambda (g767)
(if g767
(apply
(lambda (g770 g768 g769)
((lambda (g771)
(if (not (g91 g771))
(syntax-error
g765
'"invalid parameter list in")
((lambda (g773 g772)
(g763 g772
(g106 (cons g768 g769)
g765
(g61 g773
g772
g764)
(g83 g771
g773
g762))))
(g72 g771)
(map g114 g771))))
g770))
g767)
((lambda (g776)
(if g776
(apply
(lambda (g779 g777 g778)
((lambda (g780)
(if (not (g91 g780))
(syntax-error
g765
'"invalid parameter list in")
((lambda (g782 g781)
(g763 ((letrec ((g784 (lambda (g786
g785)
(if (null?
g786)
g785
(g784 (cdr g786)
(cons (car g786)
g785))))))
g784)
(cdr g781)
(car g781))
(g106 (cons g777
g778)
g765
(g61 g782
g781
g764)
(g83 g780
g782
g762))))
(g72 g780)
(map g114 g780))))
(g115 g779)))
g776)
((lambda (g787)
(syntax-error g765))
g766)))
(syntax-dispatch
g766
'(any any . each-any)))))
(syntax-dispatch
g766
'(each-any any . each-any))))
g761)))
(g106 (lambda (g357 g354 g356 g355)
((lambda (g358)
((lambda (g359)
((lambda (g360)
((lambda ()
((letrec ((g361 (lambda (g367
g362
g366
g363
g365
g364)
(if (null? g367)
(syntax-error
g354
'"no expressions in body")
((lambda (g369
g368)
(call-with-values
(lambda ()
(g100 g369
g368
'(())
'#f
g359))
(lambda (g374
g370
g373
g371
g372)
((lambda (g375)
(if (memv g375
'(define-form))
((lambda (g377
g376)
((lambda (g378)
(begin (g82 g359
g377
g376)
(g361 (cdr g367)
(cons g377
g362)
(cons g376
g366)
(cons g378
g363)
(cons (cons g368
(g94 g373
g371))
g365)
(cons (cons 'lexical
g378)
g364))))
(g114 g377)))
(g94 g370
g371)
(g71))
(if (memv g375
'(define-syntax-form))
((lambda (g380
g379)
(begin (g82 g359
g380
g379)
(g361 (cdr g367)
(cons g380
g362)
(cons g379
g366)
g363
g365
(cons (cons 'macro
(cons g368
(g94 g373
g371)))
g364))))
(g94 g370
g371)
(g71))
(if (memv g375
'(begin-form))
((lambda (g381)
((lambda (g382)
(if g382
(apply
(lambda (g384
g383)
(g361 ((letrec ((g385 (lambda (g386)
(if (null?
g386)
(cdr g367)
(cons (cons g368
(g94 (car g386)
g371))
(g385 (cdr g386)))))))
g385)
g383)
g362
g366
g363
g365
g364))
g382)
(syntax-error
g381)))
(syntax-dispatch
g381
'(any .
each-any))))
g373)
(if (memv g375
'(local-syntax-form))
(g108 g370
g373
g368
g371
g372
(lambda (g391
g388
g390
g389)
(g361 ((letrec ((g392 (lambda (g393)
(if (null?
g393)
(cdr g367)
(cons (cons g388
(g94 (car g393)
g390))
(g392 (cdr g393)))))))
g392)
g391)
g362
g366
g363
g365
g364)))
(if (null?
g362)
(g49 '#f
(map (lambda (g394)
(g102 (cdr g394)
(car g394)
'(())))
(cons (cons g368
(g95 g373
g371
g372))
(cdr g367))))
(begin (if (not (g91 g362))
(syntax-error
g354
'"invalid or duplicate identifier in definition")
(void))
((letrec ((g395 (lambda (g398
g396
g397)
(if (not (null?
g398))
((lambda (g399)
((lambda ()
(if (eq? (car g399)
'macro)
((lambda (g400)
((lambda (g401)
((lambda ()
(begin (set-cdr!
g399
(g109 (g102 (cddr g399)
g401
'(()))))
(g395 (cdr g398)
g400
g401)))))
(if (eq? g400
g396)
g397
(g62 g400))))
(cadr g399))
(g395 (cdr g398)
g396
g397)))))
(car g398))
(void)))))
g395)
g364
'#f
'#f)
(set-cdr!
g358
(g60 g366
g364
(cdr g358)))
(g50 '#f
g363
(map (lambda (g403)
(g102 (cdr g403)
(car g403)
'(())))
g365)
(g49 '#f
(map (lambda (g402)
(g102 (cdr g402)
(car g402)
'(())))
(cons (cons g368
(g95 g373
g371
g372))
(cdr g367))))))))))))
g374))))
(cdar g367)
(caar g367))))))
g361)
(map (lambda (g404)
(cons g358 (g94 g404 g360)))
g357)
'()
'()
'()
'()
'()))))
(g68 (g69 g355) (cons g359 (g70 g355)))))
(g73 '() '() '())))
(cons '("placeholder" placeholder) g356))))
(g105 (lambda (g750 g746 g749 g747 g748)
(letrec ((g751 (lambda (g753 g752)
(if (pair? g753)
(cons (g751 (car g753) g752)
(g751 (cdr g753) g752))
(if (g52 g753)
((lambda (g754)
((lambda (g756 g755)
(g51 (g53 g753)
(if (if (pair?
g756)
(eq? (car g756)
'#f)
'#f)
(g68 (cdr g756)
(if g748
(cons g748
(cdr g755))
(cdr g755)))
(g68 (cons g752
g756)
(if g748
(cons g748
(cons 'shift
g755))
(cons 'shift
g755))))))
(g69 g754)
(g70 g754)))
(g54 g753))
(if (vector? g753)
((lambda (g757)
((lambda (g758)
((lambda ()
((letrec ((g759 (lambda (g760)
(if (g41 g760
g757)
g758
(begin (vector-set!
g758
g760
(g751 (vector-ref
g753
g760)
g752))
(g759 (g39 g760
'1)))))))
g759)
'0))))
(make-vector
g757)))
(vector-length g753))
(if (symbol? g753)
(syntax-error
g753
'"encountered raw symbol in macro output")
g753)))))))
(g751 (g750 (g94 g746 (g81 g747)))
(string '#\m)))))
(g104 (lambda (g409 g405 g408 g406 g407)
((lambda (g410)
((lambda (g411)
(if g411
(apply
(lambda (g413 g412)
(cons g409
(map (lambda (g415)
(g102 g415 g408 g406))
g412)))
g411)
(syntax-error g410)))
(syntax-dispatch g410 '(any . each-any))))
g405)))
(g103 (lambda (g729 g724 g728 g725 g727 g726)
((lambda (g730)
(if (memv g730 '(lexical))
g724
(if (memv g730 '(core))
(g724 g728 g725 g727 g726)
(if (memv g730 '(lexical-call))
(g104 g724 g728 g725 g727 g726)
(if (memv g730 '(global-call))
(g104 g724 g728 g725 g727 g726)
(if (memv g730 '(constant))
(list 'quote
(g113 (g95 g728
g727
g726)
'(())))
(if (memv g730 '(global))
g724
(if (memv g730 '(call))
(g104 (g102 (car g728)
g725
g727)
g728
g725
g727
g726)
(if (memv g730
'(begin-form))
((lambda (g731)
((lambda (g732)
(if g732
(apply
(lambda (g735
g733
g734)
(g96 (cons g733
g734)
g725
g727
g726))
g732)
(syntax-error
g731)))
(syntax-dispatch
g731
'(any any
.
each-any))))
g728)
(if (memv g730
'(local-syntax-form))
(g108 g724
g728
g725
g727
g726
g96)
(if (memv g730
'(eval-when-form))
((lambda (g737)
((lambda (g738)
(if g738
(apply
(lambda (g742
g739
g741
g740)
((lambda (g743)
(if (memq 'eval
g743)
(g96 (cons g741
g740)
g725
g727
g726)
(g110)))
(g99 g728
g739
g727)))
g738)
(syntax-error
g737)))
(syntax-dispatch
g737
'(any each-any
any
.
each-any))))
g728)
(if (memv g730
'(define-form
define-syntax-form))
(syntax-error
(g94 g724
g727)
'"invalid context for definition of")
(if (memv g730
'(syntax))
(syntax-error
(g95 g728
g727
g726)
'"reference to pattern variable outside syntax form")
(if (memv g730
'(displaced-lexical))
(syntax-error
(g95 g728
g727
g726)
'"reference to identifier outside its scope")
(syntax-error
(g95 g728
g727
g726))))))))))))))))
g729)))
(g102 (lambda (g418 g416 g417)
(call-with-values
(lambda () (g100 g418 g416 g417 '#f '#f))
(lambda (g423 g419 g422 g420 g421)
(g103 g423 g419 g422 g416 g420 g421)))))
(g101 (lambda (g678 g674 g677 g675 g676)
(call-with-values
(lambda () (g100 g678 g674 g677 '#f '#f))
(lambda (g689 g685 g688 g686 g687)
((lambda (g690)
(if (memv g690 '(begin-form))
((lambda (g691)
((lambda (g692)
(if g692
(apply
(lambda (g693) (g110))
g692)
((lambda (g694)
(if g694
(apply
(lambda (g697
g695
g696)
(g97 (cons g695
g696)
g674
g686
g687
g675
g676))
g694)
(syntax-error g691)))
(syntax-dispatch
g691
'(any any . each-any)))))
(syntax-dispatch g691 '(any))))
g688)
(if (memv g690 '(local-syntax-form))
(g108 g685
g688
g674
g686
g687
(lambda (g702 g699 g701 g700)
(g97 g702
g699
g701
g700
g675
g676)))
(if (memv g690 '(eval-when-form))
((lambda (g703)
((lambda (g704)
(if g704
(apply
(lambda (g708
g705
g707
g706)
((lambda (g710
g709)
(if (eq? g675
'e)
(if (memq 'eval
g710)
(g97 g709
g674
g686
g687
'e
'(eval))
(g110))
(if (memq 'load
g710)
(if ((lambda (g711)
(if g711
g711
(if (eq? g675
'c&e)
(memq 'eval
g710)
'#f)))
(memq 'compile
g710))
(g97 g709
g674
g686
g687
'c&e
'(compile
load))
(if (memq g675
'(c c&e))
(g97 g709
g674
g686
g687
'c
'(load))
(g110)))
(if ((lambda (g712)
(if g712
g712
(if (eq? g675
'c&e)
(memq 'eval
g710)
'#f)))
(memq 'compile
g710))
(begin (g44 (g97 g709
g674
g686
g687
'e
'(eval)))
(g110))
(g110)))))
(g99 g688
g705
g686)
(cons g707 g706)))
g704)
(syntax-error g703)))
(syntax-dispatch
g703
'(any each-any
any
.
each-any))))
g688)
(if (memv g690
'(define-syntax-form))
((lambda (g716 g715)
((lambda (g717)
(if (memv g717 '(c))
(if (memq 'compile
g676)
((lambda (g718)
(begin (g44 g718)
(if (memq 'load
g676)
g718
(g110))))
(g98 g716
(g102 g688
g715
g686)))
(if (memq 'load
g676)
(g98 g716
(g102 g688
g715
g686))
(g110)))
(if (memv g717
'(c&e))
((lambda (g719)
(begin (g44 g719)
g719))
(g98 g716
(g102 g688
g715
g686)))
(begin (if (memq 'eval
g676)
(g44 (g98 g716
(g102 g688
g715
g686)))
(void))
(g110)))))
g675))
(g88 g685 g686)
(g62 g674))
(if (memv g690
'(define-form))
((lambda (g720)
((lambda (g721)
(if (memv g721
'(global))
((lambda (g722)
(begin (if (eq? g675
'c&e)
(g44 g722)
(void))
g722))
(list 'define
g720
(g102 g688
g674
g686)))
(if (memv g721
'(displaced-lexical))
(syntax-error
(g94 g685
g686)
'"identifier out of context")
(syntax-error
(g94 g685
g686)
'"cannot define keyword at top level"))))
(g58 (g63 g720
g674))))
(g88 g685 g686))
((lambda (g723)
(begin (if (eq? g675
'c&e)
(g44 g723)
(void))
g723))
(g103 g689
g685
g688
g674
g686
g687))))))))
g689)))))
(g100 (lambda (g428 g424 g427 g425 g426)
(if (symbol? g428)
((lambda (g429)
((lambda (g430)
((lambda (g431)
((lambda ()
((lambda (g432)
(if (memv g432 '(lexical))
(values
g431
(g59 g430)
g428
g427
g425)
(if (memv g432 '(global))
(values
g431
g429
g428
g427
g425)
(if (memv g432 '(macro))
(g100 (g105 (g59 g430)
g428
g424
g427
g426)
g424
'(())
g425
g426)
(values
g431
(g59 g430)
g428
g427
g425)))))
g431))))
(g58 g430)))
(g63 g429 g424)))
(g88 g428 g427))
(if (pair? g428)
((lambda (g433)
(if (g66 g433)
((lambda (g434)
((lambda (g435)
((lambda (g436)
((lambda ()
((lambda (g437)
(if (memv g437
'(lexical))
(values
'lexical-call
(g59 g435)
g428
g427
g425)
(if (memv g437
'(global))
(values
'global-call
g434
g428
g427
g425)
(if (memv g437
'(macro))
(g100 (g105 (g59 g435)
g428
g424
g427
g426)
g424
'(())
g425
g426)
(if (memv g437
'(core))
(values
g436
(g59 g435)
g428
g427
g425)
(if (memv g437
'(local-syntax))
(values
'local-syntax-form
(g59 g435)
g428
g427
g425)
(if (memv g437
'(begin))
(values
'begin-form
'#f
g428
g427
g425)
(if (memv g437
'(eval-when))
(values
'eval-when-form
'#f
g428
g427
g425)
(if (memv g437
'(define))
((lambda (g438)
((lambda (g439)
(if (if g439
(apply
(lambda (g442
g440
g441)
(g66 g440))
g439)
'#f)
(apply
(lambda (g445
g443
g444)
(values
'define-form
g443
g444
g427
g425))
g439)
((lambda (g446)
(if (if g446
(apply
(lambda (g451
g447
g450
g448
g449)
(if (g66 g447)
(g91 (g115 g450))
'#f))
g446)
'#f)
(apply
(lambda (g456
g452
g455
g453
g454)
(values
'define-form
(g94 g452
g427)
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
()
()
())
#(ribcage
()
()
())
#(ribcage
#(type)
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
strip-annotation
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
he-anti-mark
op-marked?
op-wrap
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
build-global-definition
build-global-assignment
build-global-reference
build-lexical-assignment
build-lexical-reference
build-conditional
build-application
get-global-definition-hook
put-global-definition-hook
gensym-hook
error-hook
local-eval-hook
op-level-eval-hook
annotation?
fx<
fx=
fx-
fx+
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
"i"))
#(ribcage
(define-structure)
((top))
("i"))
#(ribcage
(define-structure)
((top))
("i"))))
(g94 (cons g455
(cons g453
g454))
g427))
'(())
g425))
g446)
((lambda (g458)
(if (if g458
(apply
(lambda (g460
g459)
(g66 g459))
g458)
'#f)
(apply
(lambda (g462
g461)
(values
'define-form
(g94 g461
g427)
'(#(syntax-object
void
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
()
()
())
#(ribcage
()
()
())
#(ribcage
#(type)
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
strip-annotation
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
he-anti-mark
op-marked?
op-wrap
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
build-global-definition
build-global-assignment
build-global-reference
build-lexical-assignment
build-lexical-reference
build-conditional
build-application
get-global-definition-hook
put-global-definition-hook
gensym-hook
error-hook
local-eval-hook
op-level-eval-hook
annotation?
fx<
fx=
fx-
fx+
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
"i"))
#(ribcage
(define-structure)
((top))
("i"))
#(ribcage
(define-structure)
((top))
("i")))))
'(())
g425))
g458)
(syntax-error
g438)))
(syntax-dispatch
g438
'(any any)))))
(syntax-dispatch
g438
'(any (any .
any)
any
.
each-any)))))
(syntax-dispatch
g438
'(any any
any))))
g428)
(if (memv g437
'(define-syntax))
((lambda (g463)
((lambda (g464)
(if (if g464
(apply
(lambda (g467
g465
g466)
(g66 g465))
g464)
'#f)
(apply
(lambda (g470
g468
g469)
(values
'define-syntax-form
g468
g469
g427
g425))
g464)
(syntax-error
g463)))
(syntax-dispatch
g463
'(any any
any))))
g428)
(values
'call
'#f
g428
g427
g425)))))))))))
g436))))
(g58 g435)))
(g63 g434 g424)))
(g88 g433 g427))
(values 'call '#f g428 g427 g425)))
(car g428))
(if (g52 g428)
(g100 (g53 g428)
g424
(g85 g427 (g54 g428))
'#f
g426)
(if (g43 g428)
(g100 (annotation-expression g428)
g424
g427
(annotation-source g428)
g426)
(if ((lambda (g471)
((lambda (g472)
(if g472
g472
((lambda (g473)
(if g473
g473
((lambda (g474)
(if g474
g474
((lambda (g475)
(if g475
g475
(null?
g471)))
(char?
g471))))
(string?
g471))))
(number? g471))))
(boolean? g471)))
g428)
(values
'constant
'#f
g428
g427
g425)
(values
'other
'#f
g428
g427
g425))))))))
(g99 (lambda (g669 g667 g668)
((letrec ((g670 (lambda (g672 g671)
(if (null? g672)
g671
(g670 (cdr g672)
(cons ((lambda (g673)
(if (g89 g673
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
strip-annotation
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
he-anti-mark
op-marked?
op-wrap
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
build-global-definition
build-global-assignment
build-global-reference
build-lexical-assignment
build-lexical-reference
build-conditional
build-application
get-global-definition-hook
put-global-definition-hook
gensym-hook
error-hook
local-eval-hook
op-level-eval-hook
annotation?
fx<
fx=
fx-
fx+
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
"i"))
#(ribcage
(define-structure)
((top))
("i"))
#(ribcage
(define-structure)
((top))
("i")))))
'compile
(if (g89 g673
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
strip-annotation
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
he-anti-mark
op-marked?
op-wrap
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
build-global-definition
build-global-assignment
build-global-reference
build-lexical-assignment
build-lexical-reference
build-conditional
build-application
get-global-definition-hook
put-global-definition-hook
gensym-hook
error-hook
local-eval-hook
op-level-eval-hook
annotation?
fx<
fx=
fx-
fx+
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
"i"))
#(ribcage
(define-structure)
((top))
("i"))
#(ribcage
(define-structure)
((top))
("i")))))
'load
(if (g89 g673
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
strip-annotation
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
he-anti-mark
op-marked?
op-wrap
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
build-global-definition
build-global-assignment
build-global-reference
build-lexical-assignment
build-lexical-reference
build-conditional
build-application
get-global-definition-hook
put-global-definition-hook
gensym-hook
error-hook
local-eval-hook
op-level-eval-hook
annotation?
fx<
fx=
fx-
fx+
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
"i"))
#(ribcage
(define-structure)
((top))
("i"))
#(ribcage
(define-structure)
((top))
("i")))))
'eval
(syntax-error
(g94 g673
g668)
'"invalid eval-when situation")))))
(car g672))
g671))))))
g670)
g667
'())))
(g98 (lambda (g477 g476)
(list 'install-global-transformer
(list 'quote g477)
g476)))
(g97 (lambda (g659 g654 g658 g655 g657 g656)
(g49 g655
((letrec ((g660 (lambda (g665
g661
g664
g662
g663)
(if (null? g665)
'()
((lambda (g666)
(cons g666
(g660 (cdr g665)
g661
g664
g662
g663)))
(g101 (car g665)
g661
g664
g662
g663))))))
g660)
g659
g654
g658
g657
g656))))
(g96 (lambda (g481 g478 g480 g479)
(g49 g479
((letrec ((g482 (lambda (g485 g483 g484)
(if (null? g485)
'()
((lambda (g486)
(cons g486
(g482 (cdr g485)
g483
g484)))
(g102 (car g485)
g483
g484))))))
g482)
g481
g478
g480))))
(g95 (lambda (g653 g651 g652)
(g94 (if g652 (make-annotation g653 g652 '#f) g653)
g651)))
(g94 (lambda (g488 g487)
(if (if (null? (g69 g487)) (null? (g70 g487)) '#f)
g488
(if (g52 g488)
(g51 (g53 g488) (g85 g487 (g54 g488)))
(if (null? g488) g488 (g51 g488 g487))))))
(g93 (lambda (g649 g648)
(if (not (null? g648))
((lambda (g650)
(if g650 g650 (g93 g649 (cdr g648))))
(g90 g649 (car g648)))
'#f)))
(g92 (lambda (g489)
((letrec ((g490 (lambda (g491)
((lambda (g492)
(if g492
g492
(if (not (g93 (car g491)
(cdr g491)))
(g490 (cdr g491))
'#f)))
(null? g491)))))
g490)
g489)))
(g91 (lambda (g644)
(if ((letrec ((g645 (lambda (g646)
((lambda (g647)
(if g647
g647
(if (g66 (car g646))
(g645 (cdr g646))
'#f)))
(null? g646)))))
g645)
g644)
(g92 g644)
'#f)))
(g90 (lambda (g494 g493)
(if (if (g52 g494) (g52 g493) '#f)
(if (eq? ((lambda (g496)
(if (g43 g496)
(annotation-expression g496)
g496))
(g53 g494))
((lambda (g495)
(if (g43 g495)
(annotation-expression g495)
g495))
(g53 g493)))
(g87 (g69 (g54 g494)) (g69 (g54 g493)))
'#f)
(eq? ((lambda (g498)
(if (g43 g498)
(annotation-expression g498)
g498))
g494)
((lambda (g497)
(if (g43 g497)
(annotation-expression g497)
g497))
g493)))))
(g89 (lambda (g639 g638)
(if (eq? ((lambda (g642)
((lambda (g643)
(if (g43 g643)
(annotation-expression g643)
g643))
(if (g52 g642) (g53 g642) g642)))
g639)
((lambda (g640)
((lambda (g641)
(if (g43 g641)
(annotation-expression g641)
g641))
(if (g52 g640) (g53 g640) g640)))
g638))
(eq? (g88 g639 '(())) (g88 g638 '(())))
'#f)))
(g88 (lambda (g500 g499)
(letrec ((g503 (lambda (g526 g522 g525 g523 g524)
((lambda (g527)
((letrec ((g528 (lambda (g529)
(if (g41 g529
g527)
(g501 g526
(cdr g522)
g525)
(if (if (eq? (vector-ref
g523
g529)
g526)
(g87 g525
(vector-ref
(g76 g524)
g529))
'#f)
(values
(vector-ref
(g77 g524)
g529)
g525)
(g528 (g39 g529
'1)))))))
g528)
'0))
(vector-length g523))))
(g502 (lambda (g513 g509 g512 g510 g511)
((letrec ((g514 (lambda (g516 g515)
(if (null? g516)
(g501 g513
(cdr g509)
g512)
(if (if (eq? (car g516)
g513)
(g87 g512
(list-ref
(g76 g511)
g515))
'#f)
(values
(list-ref
(g77 g511)
g515)
g512)
(g514 (cdr g516)
(g39 g515
'1)))))))
g514)
g510
'0)))
(g501 (lambda (g519 g517 g518)
(if (null? g517)
(values '#f g518)
((lambda (g520)
(if (eq? g520 'shift)
(g501 g519
(cdr g517)
(cdr g518))
((lambda (g521)
(if (vector? g521)
(g503 g519
g517
g518
g521
g520)
(g502 g519
g517
g518
g521
g520)))
(g75 g520))))
(car g517))))))
(if (symbol? g500)
((lambda (g530) (if g530 g530 g500))
(call-with-values
(lambda ()
(g501 g500 (g70 g499) (g69 g499)))
(lambda (g531 . g532) g531)))
(if (g52 g500)
((lambda (g534 g533)
((lambda (g535)
(call-with-values
(lambda ()
(g501 g534 (g70 g499) g535))
(lambda (g537 g536)
((lambda (g538)
(if g538
g538
((lambda (g539)
(if g539 g539 g534))
(call-with-values
(lambda ()
(g501 g534
(g70 g533)
g536))
(lambda (g540 . g541)
g540)))))
g537))))
(g86 (g69 g499) (g69 g533))))
((lambda (g542)
(if (g43 g542)
(annotation-expression g542)
g542))
(g53 g500))
(g54 g500))
(if (g43 g500)
((lambda (g543)
((lambda (g544) (if g544 g544 g543))
(call-with-values
(lambda ()
(g501 g543
(g70 g499)
(g69 g499)))
(lambda (g545 . g546) g545))))
((lambda (g547)
(if (g43 g547)
(annotation-expression g547)
g547))
g500))
(g46 'id-var-name
'"invalid id"
g500)))))))
(g87 (lambda (g636 g635)
((lambda (g637)
(if g637
g637
(if (not (null? g636))
(if (not (null? g635))
(if (eq? (car g636) (car g635))
(g87 (cdr g636) (cdr g635))
'#f)
'#f)
'#f)))
(eq? g636 g635))))
(g86 (lambda (g549 g548) (g84 g549 g548)))
(g85 (lambda (g632 g631)
((lambda (g634 g633)
(if (null? g634)
(if (null? g633)
g631
(g68 (g69 g631) (g84 g633 (g70 g631))))
(g68 (g84 g634 (g69 g631))
(g84 g633 (g70 g631)))))
(g69 g632)
(g70 g632))))
(g84 (lambda (g551 g550)
(if (null? g550) g551 (append g551 g550))))
(g83 (lambda (g621 g619 g620)
(if (null? g621)
g620
(g68 (g69 g620)
(cons ((lambda (g622)
((lambda (g623)
((lambda (g625 g624)
(begin ((letrec ((g626 (lambda (g628
g627)
(if (not (null?
g628))
(call-with-values
(lambda ()
(g67 (car g628)
g620))
(lambda (g630
g629)
(begin (vector-set!
g625
g627
g630)
(vector-set!
g624
g627
g629)
(g626 (cdr g628)
(g39 g627
'1)))))
(void)))))
g626)
g621
'0)
(g73 g625 g624 g622)))
(make-vector g623)
(make-vector g623)))
(vector-length g622)))
(list->vector g619))
(g70 g620))))))
(g82 (lambda (g554 g552 g553)
(begin (g78 g554
(cons ((lambda (g555)
(if (g43 g555)
(annotation-expression g555)
g555))
(g53 g552))
(g75 g554)))
(g79 g554 (cons (g69 (g54 g552)) (g76 g554)))
(g80 g554 (cons g553 (g77 g554))))))
(g81 (lambda (g618)
(g68 (cons '#f (g69 g618)) (cons 'shift (g70 g618)))))
(g80 (lambda (g557 g556) (vector-set! g557 '3 g556)))
(g79 (lambda (g617 g616) (vector-set! g617 '2 g616)))
(g78 (lambda (g559 g558) (vector-set! g559 '1 g558)))
(g77 (lambda (g615) (vector-ref g615 '3)))
(g76 (lambda (g560) (vector-ref g560 '2)))
(g75 (lambda (g614) (vector-ref g614 '1)))
(g74 (lambda (g561)
(if (vector? g561)
(if (= (vector-length g561) '4)
(eq? (vector-ref g561 '0) 'ribcage)
'#f)
'#f)))
(g73 (lambda (g613 g611 g612)
(vector 'ribcage g613 g611 g612)))
(g72 (lambda (g562)
(if (null? g562) '() (cons (g71) (g72 (cdr g562))))))
(g71 (lambda () (string '#\i)))
(g70 cdr)
(g69 car)
(g68 cons)
(g67 (lambda (g608 g607)
(if (g52 g608)
(values
((lambda (g609)
(if (g43 g609)
(annotation-expression g609)
g609))
(g53 g608))
(g86 (g69 g607) (g69 (g54 g608))))
(values
((lambda (g610)
(if (g43 g610)
(annotation-expression g610)
g610))
g608)
(g69 g607)))))
(g66 (lambda (g563)
(if (symbol? g563)
'#t
(if (g52 g563)
(symbol?
((lambda (g564)
(if (g43 g564)
(annotation-expression g564)
g564))
(g53 g563)))
(if (g43 g563)
(symbol? (annotation-expression g563))
'#f)))))
(g65 (lambda (g605)
(if (g52 g605)
(symbol?
((lambda (g606)
(if (g43 g606)
(annotation-expression g606)
g606))
(g53 g605)))
'#f)))
(g64 (lambda (g567 g565 g566) (g47 g565 (cons g567 g566))))
(g63 (lambda (g602 g601)
((lambda (g603)
(if g603
(cdr g603)
(if (symbol? g602)
((lambda (g604) (if g604 g604 '(global)))
(g48 g602))
'(displaced-lexical))))
(assq g602 g601))))
(g62 (lambda (g568)
(if (null? g568)
'()
((lambda (g569)
(if (eq? (cadr g569) 'macro)
(cons g569 (g62 (cdr g568)))
(g62 (cdr g568))))
(car g568)))))
(g61 (lambda (g600 g598 g599)
(if (null? g600)
g599
(g61 (cdr g600)
(cdr g598)
(cons (cons (car g600)
(cons 'lexical (car g598)))
g599)))))
(g60 (lambda (g572 g570 g571)
(if (null? g572)
g571
(g60 (cdr g572)
(cdr g570)
(cons (cons (car g572) (car g570)) g571)))))
(g59 cdr)
(g58 car)
(g57 (lambda (g597)
(if (g43 g597)
(annotation-source g597)
(if (g52 g597) (g57 (g53 g597)) '#f))))
(g56 (lambda (g574 g573) (vector-set! g574 '2 g573)))
(g55 (lambda (g596 g595) (vector-set! g596 '1 g595)))
(g54 (lambda (g575) (vector-ref g575 '2)))
(g53 (lambda (g594) (vector-ref g594 '1)))
(g52 (lambda (g576)
(if (vector? g576)
(if (= (vector-length g576) '3)
(eq? (vector-ref g576 '0) 'syntax-object)
'#f)
'#f)))
(g51 (lambda (g593 g592) (vector 'syntax-object g593 g592)))
(g50 (lambda (g580 g577 g579 g578)
(if (null? g577)
g578
(list 'letrec (map list g577 g579) g578))))
(g49 (lambda (g591 g590)
(if (null? (cdr g590))
(car g590)
(cons 'begin g590))))
(g48 (lambda (g581) (getprop g581 '*sc-expander*)))
(g47 (lambda (g589 g588) (putprop g589 '*sc-expander* g588)))
(g46 (lambda (g584 g582 g583)
(error g584 '"~a ~s" g582 g583)))
(g45 (lambda (g587) (eval (list g38 g587))))
(g44 (lambda (g585) (eval (list g38 g585))))
(g43 (lambda (g586) '#f))
(g42 <)
(g41 =)
(g40 -)
(g39 +)
(g38 '"noexpand"))
(begin (g64 'local-syntax 'letrec-syntax '#t)
(g64 'local-syntax 'let-syntax '#f)
(g64 'core
'fluid-let-syntax
(lambda (g1137 g1134 g1136 g1135)
((lambda (g1138)
((lambda (g1139)
(if (if g1139
(apply
(lambda (g1144
g1140
g1143
g1141
g1142)
(g91 g1140))
g1139)
'#f)
(apply
(lambda (g1150 g1146 g1149 g1147 g1148)
((lambda (g1151)
(begin (for-each
(lambda (g1158 g1157)
((lambda (g1159)
(if (memv g1159
'(displaced-lexical))
(syntax-error
(g95 g1158
g1136
g1135)
'"identifier out of context")
(void)))
(g58 (g63 g1157
g1134))))
g1146
g1151)
(g106 (cons g1147 g1148)
(g95 g1137 g1136 g1135)
(g60 g1151
((lambda (g1152)
(map (lambda (g1154)
(cons 'macro
(g109 (g102 g1154
g1152
g1136))))
g1149))
(g62 g1134))
g1134)
g1136)))
(map (lambda (g1161)
(g88 g1161 g1136))
g1146)))
g1139)
((lambda (g1162)
(syntax-error (g95 g1137 g1136 g1135)))
g1138)))
(syntax-dispatch
g1138
'(any #(each (any any)) any . each-any))))
g1137)))
(g64 'core
'quote
(lambda (g808 g805 g807 g806)
((lambda (g809)
((lambda (g810)
(if g810
(apply
(lambda (g812 g811)
(list 'quote (g113 g811 g807)))
g810)
((lambda (g813)
(syntax-error (g95 g808 g807 g806)))
g809)))
(syntax-dispatch g809 '(any any))))
g808)))
(g64 'core
'syntax
((lambda ()
(letrec ((g1036 (lambda (g1119)
((lambda (g1120)
(if (memv g1120 '(ref))
(cadr g1119)
(if (memv g1120
'(primitive))
(cadr g1119)
(if (memv g1120
'(quote))
(list 'quote
(cadr g1119))
(if (memv g1120
'(lambda))
(list 'lambda
(cadr g1119)
(g1036
(caddr
g1119)))
(if (memv g1120
'(map))
((lambda (g1121)
(cons (if (g41 (length
g1121)
'2)
'map
'map)
g1121))
(map g1036
(cdr g1119)))
(cons (car g1119)
(map g1036
(cdr g1119)))))))))
(car g1119))))
(g1035 (lambda (g1037)
(if (eq? (car g1037) 'list)
(cons 'vector (cdr g1037))
(if (eq? (car g1037) 'quote)
(list 'quote
(list->vector
(cadr g1037)))
(list 'list->vector
g1037)))))
(g1034 (lambda (g1118 g1117)
(if (equal? g1117 ''())
g1118
(list 'append g1118 g1117))))
(g1033 (lambda (g1039 g1038)
((lambda (g1040)
(if (memv g1040 '(quote))
(if (eq? (car g1039)
'quote)
(list 'quote
(cons (cadr g1039)
(cadr g1038)))
(if (eq? (cadr g1038)
'())
(list 'list g1039)
(list 'cons
g1039
g1038)))
(if (memv g1040 '(list))
(cons 'list
(cons g1039
(cdr g1038)))
(list 'cons
g1039
g1038))))
(car g1038))))
(g1032 (lambda (g1110 g1109)
((lambda (g1112 g1111)
(if (eq? (car g1110) 'ref)
(car g1111)
(if (andmap
(lambda (g1113)
(if (eq? (car g1113)
'ref)
(memq (cadr g1113)
g1112)
'#f))
(cdr g1110))
(cons 'map
(cons (list 'primitive
(car g1110))
(map ((lambda (g1114)
(lambda (g1115)
(cdr (assq (cadr g1115)
g1114))))
(map cons
g1112
g1111))
(cdr g1110))))
(cons 'map
(cons (list 'lambda
g1112
g1110)
g1111)))))
(map cdr g1109)
(map (lambda (g1116)
(list 'ref (car g1116)))
g1109))))
(g1031 (lambda (g1042 g1041)
(list 'apply
'(primitive append)
(g1032 g1042 g1041))))
(g1030 (lambda (g1104 g1101 g1103 g1102)
(if (g41 g1103 '0)
(values g1101 g1102)
(if (null? g1102)
(syntax-error
g1104
'"missing ellipsis in syntax form")
(call-with-values
(lambda ()
(g1030
g1104
g1101
(g40 g1103 '1)
(cdr g1102)))
(lambda (g1106 g1105)
((lambda (g1107)
(if g1107
(values
(cdr g1107)
g1102)
((lambda (g1108)
(values
g1108
(cons (cons (cons g1106
g1108)
(car g1102))
g1105)))
(g114 'tmp))))
(assq g1106
(car g1102)))))))))
(g1029 (lambda (g1047
g1043
g1046
g1044
g1045)
(if (g66 g1043)
((lambda (g1048)
((lambda (g1049)
(if (eq? (g58 g1049)
'syntax)
(call-with-values
(lambda ()
((lambda (g1052)
(g1030
g1047
(car g1052)
(cdr g1052)
g1044))
(g59 g1049)))
(lambda (g1051
g1050)
(values
(list 'ref
g1051)
g1050)))
(if (g1045 g1043)
(syntax-error
g1047
'"misplaced ellipsis in syntax form")
(values
(list 'quote
g1043)
g1044))))
(g63 g1048 g1046)))
(g88 g1043 '(())))
((lambda (g1053)
((lambda (g1054)
(if (if g1054
(apply
(lambda (g1056
g1055)
(g1045
g1056))
g1054)
'#f)
(apply
(lambda (g1058
g1057)
(g1029
g1047
g1057
g1046
g1044
(lambda (g1059)
'#f)))
g1054)
((lambda (g1060)
(if (if g1060
(apply
(lambda (g1063
g1061
g1062)
(g1045
g1061))
g1060)
'#f)
(apply
(lambda (g1066
g1064
g1065)
((letrec ((g1067 (lambda (g1069
g1068)
((lambda (g1070)
((lambda (g1071)
(if (if g1071
(apply
(lambda (g1073
g1072)
(g1045
g1073))
g1071)
'#f)
(apply
(lambda (g1075
g1074)
(g1067
g1074
(lambda (g1076)
(call-with-values
(lambda ()
(g1068
(cons '()
g1076)))
(lambda (g1078
g1077)
(if (null?
(car g1077))
(syntax-error
g1047
'"extra ellipsis in syntax form")
(values
(g1031
g1078
(car g1077))
(cdr g1077))))))))
g1071)
((lambda (g1079)
(call-with-values
(lambda ()
(g1029
g1047
g1069
g1046
g1044
g1045))
(lambda (g1081
g1080)
(call-with-values
(lambda ()
(g1068
g1080))
(lambda (g1083
g1082)
(values
(g1034
g1083
g1081)
g1082))))))
g1070)))
(syntax-dispatch
g1070
'(any .
any))))
g1069))))
g1067)
g1065
(lambda (g1084)
(call-with-values
(lambda ()
(g1029
g1047
g1066
g1046
(cons '()
g1084)
g1045))
(lambda (g1086
g1085)
(if (null?
(car g1085))
(syntax-error
g1047
'"extra ellipsis in syntax form")
(values
(g1032
g1086
(car g1085))
(cdr g1085))))))))
g1060)
((lambda (g1087)
(if g1087
(apply
(lambda (g1089
g1088)
(call-with-values
(lambda ()
(g1029
g1047
g1089
g1046
g1044
g1045))
(lambda (g1091
g1090)
(call-with-values
(lambda ()
(g1029
g1047
g1088
g1046
g1090
g1045))
(lambda (g1093
g1092)
(values
(g1033
g1091
g1093)
g1092))))))
g1087)
((lambda (g1094)
(if g1094
(apply
(lambda (g1096
g1095)
(call-with-values
(lambda ()
(g1029
g1047
(cons g1096
g1095)
g1046
g1044
g1045))
(lambda (g1098
g1097)
(values
(g1035
g1098)
g1097))))
g1094)
((lambda (g1100)
(values
(list 'quote
g1043)
g1044))
g1053)))
(syntax-dispatch
g1053
'#(vector
(any .
each-any))))))
(syntax-dispatch
g1053
'(any .
any)))))
(syntax-dispatch
g1053
'(any any
.
any)))))
(syntax-dispatch
g1053
'(any any))))
g1043)))))
(lambda (g1125 g1122 g1124 g1123)
((lambda (g1126)
((lambda (g1127)
((lambda (g1128)
(if g1128
(apply
(lambda (g1130 g1129)
(call-with-values
(lambda ()
(g1029
g1126
g1129
g1122
'()
g111))
(lambda (g1132 g1131)
(g1036 g1132))))
g1128)
((lambda (g1133)
(syntax-error g1126))
g1127)))
(syntax-dispatch g1127 '(any any))))
g1126))
(g95 g1125 g1124 g1123)))))))
(g64 'core
'lambda
(lambda (g817 g814 g816 g815)
((lambda (g818)
((lambda (g819)
(if g819
(apply
(lambda (g821 g820)
(g107 (g95 g817 g816 g815)
g820
g814
g816
(lambda (g823 g822)
(list 'lambda g823 g822))))
g819)
(syntax-error g818)))
(syntax-dispatch g818 '(any . any))))
g817)))
(g64 'core
'letrec
(lambda (g1011 g1008 g1010 g1009)
((lambda (g1012)
((lambda (g1013)
(if g1013
(apply
(lambda (g1018 g1014 g1017 g1015 g1016)
((lambda (g1019)
(if (not (g91 g1019))
(syntax-error
g1011
'"duplicate bound variable in")
((lambda (g1021 g1020)
((lambda (g1023 g1022)
(g50 g1009
g1020
(map (lambda (g1026)
(g102 g1026
g1022
g1023))
g1017)
(g106 (cons g1015
g1016)
(g95 g1011
g1023
g1009)
g1022
g1023)))
(g83 g1019 g1021 g1010)
(g61 g1021 g1020 g1008)))
(g72 g1019)
(map g114 g1019))))
g1014))
g1013)
((lambda (g1028)
(syntax-error (g95 g1011 g1010 g1009)))
g1012)))
(syntax-dispatch
g1012
'(any #(each (any any)) any . each-any))))
g1011)))
(g64 'core
'if
(lambda (g827 g824 g826 g825)
((lambda (g828)
((lambda (g829)
(if g829
(apply
(lambda (g832 g830 g831)
(list 'if
(g102 g830 g824 g826)
(g102 g831 g824 g826)
(g110)))
g829)
((lambda (g833)
(if g833
(apply
(lambda (g837 g834 g836 g835)
(list 'if
(g102 g834 g824 g826)
(g102 g836 g824 g826)
(g102 g835 g824 g826)))
g833)
((lambda (g838)
(syntax-error
(g95 g827 g826 g825)))
g828)))
(syntax-dispatch
g828
'(any any any any)))))
(syntax-dispatch g828 '(any any any))))
g827)))
(g64 'core
'set!
(lambda (g994 g991 g993 g992)
((lambda (g995)
((lambda (g996)
(if (if g996
(apply
(lambda (g999 g997 g998) (g66 g997))
g996)
'#f)
(apply
(lambda (g1002 g1000 g1001)
((lambda (g1004 g1003)
((lambda (g1005)
((lambda (g1006)
(if (memv g1006 '(lexical))
(list 'set!
(g59 g1005)
g1004)
(if (memv g1006 '(global))
(list 'set!
g1003
g1004)
(if (memv g1006
'(displaced-lexical))
(syntax-error
(g94 g1000 g993)
'"identifier out of context")
(syntax-error
(g95 g994
g993
g992))))))
(g58 g1005)))
(g63 g1003 g991)))
(g102 g1001 g991 g993)
(g88 g1000 g993)))
g996)
((lambda (g1007)
(syntax-error (g95 g994 g993 g992)))
g995)))
(syntax-dispatch g995 '(any any any))))
g994)))
(g64 'begin 'begin '())
(g64 'define 'define '())
(g64 'define-syntax 'define-syntax '())
(g64 'eval-when 'eval-when '())
(g64 'core
'syntax-case
((lambda ()
(letrec ((g842 (lambda (g899 g896 g898 g897)
(if (null? g898)
(list 'syntax-error g899)
((lambda (g900)
((lambda (g901)
(if g901
(apply
(lambda (g903 g902)
(if (if (g66 g903)
(andmap
(lambda (g904)
(not (g89 g903
g904)))
(cons '#(syntax-object
...
((top)
#(ribcage
#(pat
exp)
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
keys
clauses
r)
#((top)
(top)
(top)
(top))
#("i"
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
("i" "i"
"i"
"i"))
#(ribcage
(lambda-var-list
gen-var
strip
strip-annotation
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
he-anti-mark
op-marked?
op-wrap
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
build-global-definition
build-global-assignment
build-global-reference
build-lexical-assignment
build-lexical-reference
build-conditional
build-application
get-global-definition-hook
put-global-definition-hook
gensym-hook
error-hook
local-eval-hook
op-level-eval-hook
annotation?
fx<
fx=
fx-
fx+
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
"i"))
#(ribcage
(define-structure)
((top))
("i"))
#(ribcage
(define-structure)
((top))
("i"))))
g896))
'#f)
((lambda (g906
g905)
(list (list 'lambda
(list g905)
(g102 g902
(g60 g906
(list (cons 'syntax
(cons g905
'0)))
g897)
(g83 (list g903)
g906
'(()))))
g899))
(list (g71))
(g114 g903))
(g841 g899
g896
(cdr g898)
g897
g903
'#t
g902)))
g901)
((lambda (g907)
(if g907
(apply
(lambda (g910
g908
g909)
(g841 g899
g896
(cdr g898)
g897
g910
g908
g909))
g907)
((lambda (g911)
(syntax-error
(car g898)
'"invalid syntax-case clause"))
g900)))
(syntax-dispatch
g900
'(any any any)))))
(syntax-dispatch
g900
'(any any))))
(car g898)))))
(g841 (lambda (g849
g843
g848
g844
g847
g845
g846)
(call-with-values
(lambda () (g839 g847 g843))
(lambda (g851 g850)
(if (not (g92 (map car g850)))
(syntax-error
g847
'"duplicate pattern variable in syntax-case pattern")
(if (not (andmap
(lambda (g852)
(not (g111 (car g852))))
g850))
(syntax-error
g847
'"misplaced ellipsis in syntax-case pattern")
((lambda (g853)
(list (list 'lambda
(list g853)
((lambda (g854)
(list 'if
((lambda (g855)
((lambda (g856)
(if g856
(apply
(lambda ()
g854)
g856)
((lambda (g857)
(list 'if
g854
(g840 g850
g845
g854
g844)
(list 'quote
'#f)))
g855)))
(syntax-dispatch
g855
'#(atom
#t))))
g845)
(g840 g850
g846
g854
g844)
(g842 g849
g843
g848
g844)))
g853))
(if (eq? g851
'any)
(list 'list
g849)
(list 'syntax-dispatch
g849
(list 'quote
g851)))))
(g114 'tmp))))))))
(g840 (lambda (g889 g886 g888 g887)
((lambda (g891 g890)
((lambda (g893 g892)
(list 'apply
(list 'lambda
g892
(g102 g886
(g60 g893
(map (lambda (g895
g894)
(cons 'syntax
(cons g895
g894)))
g892
(map cdr
g889))
g887)
(g83 g891
g893
'(()))))
g888))
(g72 g891)
(map g114 g891)))
(map car g889)
(map cdr g889))))
(g839 (lambda (g859 g858)
((letrec ((g860 (lambda (g863
g861
g862)
(if (g66 g863)
(if (g93 g863
g858)
(values
(vector
'free-id
g863)
g862)
(values
'any
(cons (cons g863
g861)
g862)))
((lambda (g864)
((lambda (g865)
(if (if g865
(apply
(lambda (g867
g866)
(g111 g866))
g865)
'#f)
(apply
(lambda (g869
g868)
(call-with-values
(lambda ()
(g860 g869
(g39 g861
'1)
g862))
(lambda (g871
g870)
(values
(if (eq? g871
'any)
'each-any
(vector
'each
g871))
g870))))
g865)
((lambda (g872)
(if g872
(apply
(lambda (g874
g873)
(call-with-values
(lambda ()
(g860 g873
g861
g862))
(lambda (g876
g875)
(call-with-values
(lambda ()
(g860 g874
g861
g875))
(lambda (g878
g877)
(values
(cons g878
g876)
g877))))))
g872)
((lambda (g879)
(if g879
(apply
(lambda ()
(values
'()
g862))
g879)
((lambda (g880)
(if g880
(apply
(lambda (g881)
(call-with-values
(lambda ()
(g860 g881
g861
g862))
(lambda (g883
g882)
(values
(vector
'vector
g883)
g882))))
g880)
((lambda (g885)
(values
(vector
'atom
(g113 g863
'(())))
g862))
g864)))
(syntax-dispatch
g864
'#(vector
each-any)))))
(syntax-dispatch
g864
'()))))
(syntax-dispatch
g864
'(any .
any)))))
(syntax-dispatch
g864
'(any any))))
g863)))))
g860)
g859
'0
'()))))
(lambda (g915 g912 g914 g913)
((lambda (g916)
((lambda (g917)
((lambda (g918)
(if g918
(apply
(lambda (g922 g919 g921 g920)
(if (andmap
(lambda (g924)
(if (g66 g924)
(not (g111 g924))
'#f))
g921)
((lambda (g925)
(list (list 'lambda
(list g925)
(g842 g925
g921
g920
g912))
(g102 g919
g912
'(()))))
(g114 'tmp))
(syntax-error
g916
'"invalid literals list in")))
g918)
(syntax-error g917)))
(syntax-dispatch
g917
'(any any each-any . each-any))))
g916))
(g95 g915 g914 g913)))))))
(set! sc-expand
((lambda (g989 g988)
(lambda (g990)
(if (if (pair? g990) (equal? (car g990) g38) '#f)
(cadr g990)
(g101 g990 '() '((top)) g989 g988))))
'e
'(eval)))
(set! identifier? (lambda (g928) (g65 g928)))
(set! datum->syntax-object
(lambda (g986 g985)
(begin ((lambda (g987)
(if (not (g65 g987))
(g46 'datum->syntax-object
'"invalid argument"
g987)
(void)))
g986)
(g51 g985 (g54 g986)))))
(set! syntax-object->datum
(lambda (g929) (g113 g929 '(()))))
(set! generate-temporaries
(lambda (g982)
(begin ((lambda (g984)
(if (not (list? g984))
(g46 'generate-temporaries
'"invalid argument"
g984)
(void)))
g982)
(map (lambda (g983) (g94 (gensym) '((top))))
g982))))
(set! free-identifier=?
(lambda (g931 g930)
(begin ((lambda (g933)
(if (not (g65 g933))
(g46 'free-identifier=?
'"invalid argument"
g933)
(void)))
g931)
((lambda (g932)
(if (not (g65 g932))
(g46 'free-identifier=?
'"invalid argument"
g932)
(void)))
g930)
(g89 g931 g930))))
(set! bound-identifier=?
(lambda (g979 g978)
(begin ((lambda (g981)
(if (not (g65 g981))
(g46 'bound-identifier=?
'"invalid argument"
g981)
(void)))
g979)
((lambda (g980)
(if (not (g65 g980))
(g46 'bound-identifier=?
'"invalid argument"
g980)
(void)))
g978)
(g90 g979 g978))))
(set! syntax-error
(lambda (g934 . g935)
(begin (for-each
(lambda (g937)
((lambda (g938)
(if (not (string? g938))
(g46 'syntax-error
'"invalid argument"
g938)
(void)))
g937))
g935)
((lambda (g936) (g46 '#f g936 (g113 g934 '(()))))
(if (null? g935)
'"invalid syntax"
(apply string-append g935))))))
(set! install-global-transformer
(lambda (g975 g974)
(begin ((lambda (g977)
(if (not (symbol? g977))
(g46 'define-syntax
'"invalid argument"
g977)
(void)))
g975)
((lambda (g976)
(if (not (procedure? g976))
(g46 'define-syntax
'"invalid argument"
g976)
(void)))
g974)
(g64 'macro g975 g974))))
((lambda ()
(letrec ((g943 (lambda (g967 g964 g966 g965)
(if (not g965)
'#f
(if (eq? g964 'any)
(cons (g94 g967 g966) g965)
(if (g52 g967)
(g942 ((lambda (g968)
(if (g43 g968)
(annotation-expression
g968)
g968))
(g53 g967))
g964
(g85 g966 (g54 g967))
g965)
(g942 ((lambda (g969)
(if (g43 g969)
(annotation-expression
g969)
g969))
g967)
g964
g966
g965))))))
(g942 (lambda (g947 g944 g946 g945)
(if (null? g944)
(if (null? g947) g945 '#f)
(if (pair? g944)
(if (pair? g947)
(g943 (car g947)
(car g944)
g946
(g943 (cdr g947)
(cdr g944)
g946
g945))
'#f)
(if (eq? g944 'each-any)
((lambda (g948)
(if g948
(cons g948 g945)
'#f))
(g940 g947 g946))
((lambda (g949)
(if (memv g949 '(each))
(if (null? g947)
(g941 (vector-ref
g944
'1)
g945)
((lambda (g950)
(if g950
((letrec ((g951 (lambda (g952)
(if (null?
(car g952))
g945
(cons (map car
g952)
(g951 (map cdr
g952)))))))
g951)
g950)
'#f))
(g939 g947
(vector-ref
g944
'1)
g946)))
(if (memv g949
'(free-id))
(if (g66 g947)
(if (g89 (g94 g947
g946)
(vector-ref
g944
'1))
g945
'#f)
'#f)
(if (memv g949
'(atom))
(if (equal?
(vector-ref
g944
'1)
(g113 g947
g946))
g945
'#f)
(if (memv g949
'(vector))
(if (vector?
g947)
(g943 (vector->list
g947)
(vector-ref
g944
'1)
g946
g945)
'#f)
(void))))))
(vector-ref g944 '0)))))))
(g941 (lambda (g962 g961)
(if (null? g962)
g961
(if (eq? g962 'any)
(cons '() g961)
(if (pair? g962)
(g941 (car g962)
(g941 (cdr g962)
g961))
(if (eq? g962 'each-any)
(cons '() g961)
((lambda (g963)
(if (memv g963
'(each))
(g941 (vector-ref
g962
'1)
g961)
(if (memv g963
'(free-id
atom))
g961
(if (memv g963
'(vector))
(g941 (vector-ref
g962
'1)
g961)
(void)))))
(vector-ref
g962
'0))))))))
(g940 (lambda (g954 g953)
(if (g43 g954)
(g940 (annotation-expression g954)
g953)
(if (pair? g954)
((lambda (g955)
(if g955
(cons (g94 (car g954)
g953)
g955)
'#f))
(g940 (cdr g954) g953))
(if (null? g954)
'()
(if (g52 g954)
(g940 (g53 g954)
(g85 g953
(g54 g954)))
'#f))))))
(g939 (lambda (g958 g956 g957)
(if (g43 g958)
(g939 (annotation-expression g958)
g956
g957)
(if (pair? g958)
((lambda (g959)
(if g959
((lambda (g960)
(if g960
(cons g959 g960)
'#f))
(g939 (cdr g958)
g956
g957))
'#f))
(g943 (car g958)
g956
g957
'()))
(if (null? g958)
'()
(if (g52 g958)
(g939 (g53 g958)
g956
(g85 g957
(g54 g958)))
'#f)))))))
(set! syntax-dispatch
(lambda (g971 g970)
(if (eq? g970 'any)
(list g971)
(if (g52 g971)
(g942 ((lambda (g972)
(if (g43 g972)
(annotation-expression g972)
g972))
(g53 g971))
g970
(g54 g971)
'())
(g942 ((lambda (g973)
(if (g43 g973)
(annotation-expression g973)
g973))
g971)
g970
'(())
'()))))))))))))))
(install-global-transformer
'with-syntax
(lambda (g1163)
((lambda (g1164)
((lambda (g1165)
(if g1165
(apply
(lambda (g1168 g1166 g1167)
(cons '#(syntax-object
begin
((top)
#(ribcage
#(_ e1 e2)
#((top) (top) (top))
#("i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(cons g1166 g1167)))
g1165)
((lambda (g1170)
(if g1170
(apply
(lambda (g1175 g1171 g1174 g1172 g1173)
(list '#(syntax-object
syntax-case
((top)
#(ribcage
#(_ out in e1 e2)
#((top) (top) (top) (top) (top))
#("i" "i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
g1174
'()
(list g1171
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
#("i"))))
(cons g1172 g1173)))))
g1170)
((lambda (g1177)
(if g1177
(apply
(lambda (g1182 g1178 g1181 g1179 g1180)
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
#(ribcage #(x) #((top)) #("i"))))
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
#("i"))))
g1181)
'()
(list g1178
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
#("i"))))
(cons g1179 g1180)))))
g1177)
(syntax-error g1164)))
(syntax-dispatch
g1164
'(any #(each (any any)) any . each-any)))))
(syntax-dispatch g1164 '(any ((any any)) any . each-any)))))
(syntax-dispatch g1164 '(any () any . each-any))))
g1163)))
(install-global-transformer
'syntax-rules
(lambda (g1186)
((lambda (g1187)
((lambda (g1188)
(if g1188
(apply
(lambda (g1193 g1189 g1192 g1190 g1191)
(list '#(syntax-object
lambda
((top)
#(ribcage
#(_ k keyword pattern template)
#((top) (top) (top) (top) (top))
#("i" "i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
'(#(syntax-object
x
((top)
#(ribcage
#(_ k keyword pattern template)
#((top) (top) (top) (top) (top))
#("i" "i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i")))))
(cons '#(syntax-object
syntax-case
((top)
#(ribcage
#(_ k keyword pattern template)
#((top) (top) (top) (top) (top))
#("i" "i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(cons '#(syntax-object
x
((top)
#(ribcage
#(_ k keyword pattern template)
#((top) (top) (top) (top) (top))
#("i" "i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(cons g1189
(map (lambda (g1196 g1195)
(list (cons '#(syntax-object
dummy
((top)
#(ribcage
#(_
k
keyword
pattern
emplate)
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
#("i"))))
g1195)
(list '#(syntax-object
syntax
((top)
#(ribcage
#(_
k
keyword
pattern
emplate)
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
#("i"))))
g1196)))
g1191
g1190))))))
g1188)
(syntax-error g1187)))
(syntax-dispatch
g1187
'(any each-any . #(each ((any . any) any))))))
g1186)))
(install-global-transformer
'or
(lambda (g1197)
((lambda (g1198)
((lambda (g1199)
(if g1199
(apply
(lambda (g1200)
'#(syntax-object
#f
((top)
#(ribcage #(_) #((top)) #("i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i")))))
g1199)
((lambda (g1201)
(if g1201
(apply (lambda (g1203 g1202) g1202) g1201)
((lambda (g1204)
(if g1204
(apply
(lambda (g1208 g1205 g1207 g1206)
(list '#(syntax-object
let
((top)
#(ribcage
#(_ e1 e2 e3)
#((top) (top) (top) (top))
#("i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(list (list '#(syntax-object

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
#("i"))))
g1205))
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
#("i"))))
'#(syntax-object

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
#("i"))))
'#(syntax-object

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
#("i"))))
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
#("i"))))
(cons g1207 g1206)))))
g1204)
(syntax-error g1198)))
(syntax-dispatch g1198 '(any any any . each-any)))))
(syntax-dispatch g1198 '(any any)))))
(syntax-dispatch g1198 '(any))))
g1197)))
(install-global-transformer
'and
(lambda (g1210)
((lambda (g1211)
((lambda (g1212)
(if g1212
(apply
(lambda (g1216 g1213 g1215 g1214)
(cons '#(syntax-object
if
((top)
#(ribcage
#(_ e1 e2 e3)
#((top) (top) (top) (top))
#("i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(cons g1213
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
#("i"))))
(cons g1215 g1214))
'(#(syntax-object
#f
((top)
#(ribcage
#(_ e1 e2 e3)
#((top) (top) (top) (top))
#("i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage
#(x)
#((top))
#("i")))))))))
g1212)
((lambda (g1218)
(if g1218
(apply (lambda (g1220 g1219) g1219) g1218)
((lambda (g1221)
(if g1221
(apply
(lambda (g1222)
'#(syntax-object
#t
((top)
#(ribcage #(_) #((top)) #("i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i")))))
g1221)
(syntax-error g1211)))
(syntax-dispatch g1211 '(any)))))
(syntax-dispatch g1211 '(any any)))))
(syntax-dispatch g1211 '(any any any . each-any))))
g1210)))
(install-global-transformer
'let
(lambda (g1223)
((lambda (g1224)
((lambda (g1225)
(if (if g1225
(apply
(lambda (g1230 g1226 g1229 g1227 g1228)
(andmap identifier? g1226))
g1225)
'#f)
(apply
(lambda (g1236 g1232 g1235 g1233 g1234)
(cons (cons '#(syntax-object
lambda
((top)
#(ribcage
#(_ x v e1 e2)
#((top) (top) (top) (top) (top))
#("i" "i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(cons g1232 (cons g1233 g1234)))
g1235))
g1225)
((lambda (g1240)
(if (if g1240
(apply
(lambda (g1246 g1241 g1245 g1242 g1244 g1243)
(andmap identifier? (cons g1241 g1245)))
g1240)
'#f)
(apply
(lambda (g1253 g1248 g1252 g1249 g1251 g1250)
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
#(ribcage #(x) #((top)) #("i"))))
(list (list g1248
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
#("i"))))
(cons g1252
(cons g1251
g1250)))))
g1248)
g1249))
g1240)
(syntax-error g1224)))
(syntax-dispatch
g1224
'(any any #(each (any any)) any . each-any)))))
(syntax-dispatch
g1224
'(any #(each (any any)) any . each-any))))
g1223)))
(install-global-transformer
'let*
(lambda (g1257)
((lambda (g1258)
((lambda (g1259)
(if (if g1259
(apply
(lambda (g1264 g1260 g1263 g1261 g1262)
(andmap identifier? g1260))
g1259)
'#f)
(apply
(lambda (g1270 g1266 g1269 g1267 g1268)
((letrec ((g1271 (lambda (g1272)
(if (null? g1272)
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
#("i"
"i"
"i"
"i"
"i"))
#(ribcage () () ())
#(ribcage
#(x)
#((top))
#("i"))))
(cons '()
(cons g1267 g1268)))
((lambda (g1274)
((lambda (g1275)
(if g1275
(apply
(lambda (g1277 g1276)
(list '#(syntax-object
let
((top)
#(ribcage
#(body
binding)
#((top)
(top))
#("i"
"i"))
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
#("i"))))
(list g1276)
g1277))
g1275)
(syntax-error g1274)))
(syntax-dispatch
g1274
'(any any))))
(list (g1271 (cdr g1272))
(car g1272)))))))
g1271)
(map list g1266 g1269)))
g1259)
(syntax-error g1258)))
(syntax-dispatch
g1258
'(any #(each (any any)) any . each-any))))
g1257)))
(install-global-transformer
'cond
(lambda (g1280)
((lambda (g1281)
((lambda (g1282)
(if g1282
(apply
(lambda (g1285 g1283 g1284)
((letrec ((g1286 (lambda (g1288 g1287)
(if (null? g1287)
((lambda (g1289)
((lambda (g1290)
(if g1290
(apply
(lambda (g1292 g1291)
(cons '#(syntax-object
begin
((top)
#(ribcage
#(e1 e2)
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
#("i"))))
(cons g1292
g1291)))
g1290)
((lambda (g1294)
(if g1294
(apply
(lambda (g1295)
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
#("i"))))
(cons (list (list '#(syntax-object

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
#("i"))))
g1295))
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
#("i"))))
#(syntax-object

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
#("i"))))
#(syntax-object

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
#("i")))))))))
g1294)
((lambda (g1296)
(if g1296
(apply
(lambda (g1298
g1297)
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
#("i"))))
(list (list '#(syntax-object

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
#("i"))))
g1298))
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
#("i"))))
'#(syntax-object

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
#("i"))))
(cons g1297
'(#(syntax-object

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
#("i")))))))))
g1296)
((lambda (g1299)
(if g1299
(apply
(lambda (g1302
g1300
g1301)
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
#("i"))))
g1302
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
#("i"))))
(cons g1300
g1301))))
g1299)
((lambda (g1304)
(syntax-error
g1280))
g1289)))
(syntax-dispatch
g1289
'(any any
.
each-any)))))
(syntax-dispatch
g1289
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
#("i")))))
any)))))
(syntax-dispatch
g1289
'(any)))))
(syntax-dispatch
g1289
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
#((top)
(top)
(top))
#("i" "i" "i"))
#(ribcage () () ())
#(ribcage
#(x)
#((top))
#("i")))))
any
.
each-any))))
g1288)
((lambda (g1305)
((lambda (g1306)
((lambda (g1307)
((lambda (g1308)
(if g1308
(apply
(lambda (g1309)
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
#("i"))))
(list (list '#(syntax-object

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
#("i"))))
g1309))
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
#("i"))))
'#(syntax-object

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
#("i"))))
'#(syntax-object

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
#("i"))))
g1306)))
g1308)
((lambda (g1310)
(if g1310
(apply
(lambda (g1312
g1311)
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
#("i"))))
(list (list '#(syntax-object

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
#("i"))))
g1312))
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
#("i"))))
'#(syntax-object

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
#("i"))))
(cons g1311
'(#(syntax-object

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
#("i"))))))
g1306)))
g1310)
((lambda (g1313)
(if g1313
(apply
(lambda (g1316
g1314
g1315)
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
#("i"))))
g1316
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
#("i"))))
(cons g1314
g1315))
g1306))
g1313)
((lambda (g1318)
(syntax-error
g1280))
g1307)))
(syntax-dispatch
g1307
'(any any
.
each-any)))))
(syntax-dispatch
g1307
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
#("i")))))
any)))))
(syntax-dispatch
g1307
'(any))))
g1288))
g1305))
(g1286
(car g1287)
(cdr g1287)))))))
g1286)
g1283
g1284))
g1282)
(syntax-error g1281)))
(syntax-dispatch g1281 '(any any . each-any))))
g1280)))
(install-global-transformer
'do
(lambda (g1320)
((lambda (g1321)
((lambda (g1322)
(if g1322
(apply
(lambda (g1329 g1323 g1328 g1324 g1327 g1325 g1326)
((lambda (g1330)
((lambda (g1340)
(if g1340
(apply
(lambda (g1341)
((lambda (g1342)
((lambda (g1344)
(if g1344
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
#("i"))))
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
#("i"))))
(map list g1323 g1328)
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
#("i"))))
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
#("i"))))
g1327)
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
#("i"))))
(append
g1326
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
#("i"))))
g1341)))))))
g1344)
((lambda (g1349)
(if g1349
(apply
(lambda (g1351 g1350)
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
#("i"))))
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
#("i"))))
(map list
g1323
g1328)
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
#("i"))))
g1327
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
#("i"))))
(cons g1351
g1350))
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
#("i"))))
(append
g1326
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
#("i"))))
g1341)))))))
g1349)
(syntax-error g1342)))
(syntax-dispatch
g1342
'(any . each-any)))))
(syntax-dispatch g1342 '())))
g1325))
g1340)
(syntax-error g1330)))
(syntax-dispatch g1330 'each-any)))
(map (lambda (g1334 g1333)
((lambda (g1335)
((lambda (g1336)
(if g1336
(apply (lambda () g1334) g1336)
((lambda (g1337)
(if g1337
(apply
(lambda (g1338) g1338)
g1337)
((lambda (g1339)
(syntax-error g1320))
g1335)))
(syntax-dispatch g1335 '(any)))))
(syntax-dispatch g1335 '())))
g1333))
g1323
g1324)))
g1322)
(syntax-error g1321)))
(syntax-dispatch
g1321
'(any #(each (any any . any))
(any . each-any)
.
each-any))))
g1320)))
(install-global-transformer
'quasiquote
(letrec ((g1360 (lambda (g1399 g1398)
((lambda (g1400)
((lambda (g1401)
(if g1401
(apply
(lambda (g1403 g1402)
((lambda (g1404)
((lambda (g1405)
(if g1405
(apply
(lambda (g1406)
((lambda (g1407)
((lambda (g1408)
(if g1408
(apply
(lambda (g1409)
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
"i"))))
(cons g1409
g1406)))
g1408)
((lambda (g1410)
(if (null?
g1406)
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
"i"))))
g1403)
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
"i"))))
g1403
g1402)))
g1407)))
(syntax-dispatch
g1407
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
"i")))))
any))))
g1403))
g1405)
((lambda (g1411)
(if g1411
(apply
(lambda (g1412)
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
"i"))))
(cons g1403
g1412)))
g1411)
((lambda (g1413)
(list '#(syntax-object
cons
((top)
#(ribcage
#(else)
#((top))
#("i"))
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
"i"))))
g1403
g1402))
g1404)))
(syntax-dispatch
g1404
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
#("i"
"i"
"i"
"i")))))
.
any)))))
(syntax-dispatch
g1404
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
#("i" "i" "i" "i")))))
any))))
g1402))
g1401)
(syntax-error g1400)))
(syntax-dispatch g1400 '(any any))))
(list g1399 g1398))))
(g1357 (lambda (g1362 g1361)
((lambda (g1363)
((lambda (g1364)
(if g1364
(apply
(lambda (g1366 g1365)
((lambda (g1367)
((lambda (g1368)
(if g1368
(apply (lambda () g1366) g1368)
((lambda (g1369)
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
"i"))))
g1366
g1365))
g1367)))
(syntax-dispatch
g1367
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
#("i" "i" "i" "i")))))
()))))
g1365))
g1364)
(syntax-error g1363)))
(syntax-dispatch g1363 '(any any))))
(list g1362 g1361))))
(g1359 (lambda (g1387)
((lambda (g1388)
((lambda (g1389)
((lambda (g1390)
((lambda (g1391)
(if g1391
(apply
(lambda (g1392)
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
#((top)
(top)
(top)
(top))
#("i" "i" "i" "i"))))
(list->vector g1392)))
g1391)
((lambda (g1394)
(if g1394
(apply
(lambda (g1395)
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
"i"))))
g1395))
g1394)
((lambda (g1397)
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
#((top)
(top)
(top)
(top))
#("i"
"i"
"i"
"i"))))
g1389))
g1390)))
(syntax-dispatch
g1390
'(#(free-id
#(syntax-object
list
((top)
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
#("i" "i" "i" "i")))))
.
each-any)))))
(syntax-dispatch
g1390
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
#("i" "i" "i" "i")))))
each-any))))
g1389))
g1388))
g1387)))
(g1358 (lambda (g1371 g1370)
((lambda (g1372)
((lambda (g1373)
(if g1373
(apply
(lambda (g1374)
(if (= g1370 '0)
g1374
(g1360
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
#(quasicons
quasiappend
quasivector
quasi)
#((top) (top) (top) (top))
#("i" "i" "i" "i"))))
#(syntax-object
unquote
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
#(quasicons
quasiappend
quasivector
quasi)
#((top) (top) (top) (top))
#("i" "i" "i" "i")))))
(g1358
(list g1374)
(- g1370 '1)))))
g1373)
((lambda (g1375)
(if g1375
(apply
(lambda (g1377 g1376)
(if (= g1370 '0)
(g1357
g1377
(g1358 g1376 g1370))
(g1360
(g1360
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
#("i"
"i"
"i"
"i"))))
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
#("i"
"i"
"i"
"i")))))
(g1358
(list g1377)
(- g1370 '1)))
(g1358 g1376 g1370))))
g1375)
((lambda (g1378)
(if g1378
(apply
(lambda (g1379)
(g1360
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
"i"))))
#(syntax-object
quasiquote
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
"i")))))
(g1358
(list g1379)
(+ g1370 '1))))
g1378)
((lambda (g1380)
(if g1380
(apply
(lambda (g1382 g1381)
(g1360
(g1358
g1382
g1370)
(g1358
g1381
g1370)))
g1380)
((lambda (g1383)
(if g1383
(apply
(lambda (g1384)
(g1359
(g1358
g1384
g1370)))
g1383)
((lambda (g1386)
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
"i"))))
g1386))
g1372)))
(syntax-dispatch
g1372
'#(vector
each-any)))))
(syntax-dispatch
g1372
'(any . any)))))
(syntax-dispatch
g1372
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
#("i" "i" "i" "i")))))
any)))))
(syntax-dispatch
g1372
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
#("i" "i" "i" "i")))))
any)
.
any)))))
(syntax-dispatch
g1372
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
#(quasicons
quasiappend
quasivector
quasi)
#((top) (top) (top) (top))
#("i" "i" "i" "i")))))
any))))
g1371))))
(lambda (g1414)
((lambda (g1415)
((lambda (g1416)
(if g1416
(apply (lambda (g1418 g1417) (g1358 g1417 '0)) g1416)
(syntax-error g1415)))
(syntax-dispatch g1415 '(any any))))
g1414))))
(install-global-transformer
'include
(lambda (g1419)
(letrec ((g1420 (lambda (g1422 g1421)
((lambda (g1423)
((letrec ((g1424 (lambda (g1425)
(if (eof-object? g1425)
(begin (close-input-port
g1423)
'())
(cons (datum->syntax-object
g1421
g1425)
(g1424
(read g1423)))))))
g1424)
(read g1423)))
(open-input-file g1422)))))
((lambda (g1426)
((lambda (g1427)
(if g1427
(apply
(lambda (g1429 g1428)
((lambda (g1430)
((lambda (g1431)
((lambda (g1432)
(if g1432
(apply
(lambda (g1433)
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
#("i"))))
g1433))
g1432)
(syntax-error g1431)))
(syntax-dispatch g1431 'each-any)))
(g1420 g1430 g1429)))
(syntax-object->datum g1428)))
g1427)
(syntax-error g1426)))
(syntax-dispatch g1426 '(any any))))
g1419))))
(install-global-transformer
'unquote
(lambda (g1435)
((lambda (g1436)
((lambda (g1437)
(if g1437
(apply
(lambda (g1439 g1438)
(error 'unquote
'"expression ,~s not valid outside of quasiquote"
(syntax-object->datum g1438)))
g1437)
(syntax-error g1436)))
(syntax-dispatch g1436 '(any any))))
g1435)))
(install-global-transformer
'unquote-splicing
(lambda (g1440)
((lambda (g1441)
((lambda (g1442)
(if g1442
(apply
(lambda (g1444 g1443)
(error 'unquote-splicing
'"expression ,@~s not valid outside of quasiquote"
(syntax-object->datum g1443)))
g1442)
(syntax-error g1441)))
(syntax-dispatch g1441 '(any any))))
g1440)))
(install-global-transformer
'case
(lambda (g1445)
((lambda (g1446)
((lambda (g1447)
(if g1447
(apply
(lambda (g1451 g1448 g1450 g1449)
((lambda (g1452)
((lambda (g1479)
(list '#(syntax-object
let
((top)
#(ribcage #(body) #((top)) #("i"))
#(ribcage
#(_ e m1 m2)
#((top) (top) (top) (top))
#("i" "i" "i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(list (list '#(syntax-object

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
#("i"))))
g1448))
g1479))
g1452))
((letrec ((g1453 (lambda (g1455 g1454)
(if (null? g1454)
((lambda (g1456)
((lambda (g1457)
(if g1457
(apply
(lambda (g1459 g1458)
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
#("i"))))
(cons g1459
g1458)))
g1457)
((lambda (g1461)
(if g1461
(apply
(lambda (g1464
g1462
g1463)
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
#("i"))))
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
#("i"))))
'#(syntax-object

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
#("i"))))
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
#("i"))))
g1464))
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
#("i"))))
(cons g1462
g1463))))
g1461)
((lambda (g1467)
(syntax-error
g1445))
g1456)))
(syntax-dispatch
g1456
'(each-any
any
.
each-any)))))
(syntax-dispatch
g1456
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
#("i"
"i"
"i"
"i"))
#(ribcage () () ())
#(ribcage
#(x)
#((top))
#("i")))))
any
.
each-any))))
g1455)
((lambda (g1468)
((lambda (g1469)
((lambda (g1470)
((lambda (g1471)
(if g1471
(apply
(lambda (g1474
g1472
g1473)
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
#("i"))))
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
#("i"))))
'#(syntax-object

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
#("i"))))
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
#("i"))))
g1474))
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
#("i"))))
(cons g1472
g1473))
g1469))
g1471)
((lambda (g1477)
(syntax-error
g1445))
g1470)))
(syntax-dispatch
g1470
'(each-any
any
.
each-any))))
g1455))
g1468))
(g1453
(car g1454)
(cdr g1454)))))))
g1453)
g1450
g1449)))
g1447)
(syntax-error g1446)))
(syntax-dispatch g1446 '(any any any . each-any))))
g1445)))
(install-global-transformer
'identifier-syntax
(lambda (g1480)
((lambda (g1481)
((lambda (g1482)
(if g1482
(apply
(lambda (g1484 g1483)
(list '#(syntax-object
lambda
((top)
#(ribcage #(_ e) #((top) (top)) #("i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
'(#(syntax-object
x
((top)
#(ribcage #(_ e) #((top) (top)) #("i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i")))))
(list '#(syntax-object
syntax-case
((top)
#(ribcage
#(_ e)
#((top) (top))
#("i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
'#(syntax-object
x
((top)
#(ribcage
#(_ e)
#((top) (top))
#("i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
'()
(list '#(syntax-object
id
((top)
#(ribcage
#(_ e)
#((top) (top))
#("i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
'(#(syntax-object
identifier?
((top)
#(ribcage
#(_ e)
#((top) (top))
#("i" "i"))
#(ribcage () () ())
#(ribcage #(x) #((top)) #("i"))))
(#(syntax-object
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
#("i"))))
#(syntax-object
id
((top)
#(ribcage
#(_ e)
#((top) (top))
#("i" "i"))
#(ribcage () () ())
#(ribcage
#(x)
#((top))
#("i"))))))
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
#("i"))))
g1483))
(list (cons g1484
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
#("i"))))
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
#("i"))))))
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
#("i"))))
(cons g1483
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
#("i"))))
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
#("i")))))))))))
g1482)
(syntax-error g1481)))
(syntax-dispatch g1481 '(any any))))
g1480)))
