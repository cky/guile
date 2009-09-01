;; all the different permutations of or
(list
 ;; not in tail position, no args
 (or)
 ;; not in tail position, one arg
 (or 'what)
 (or #f)
 ;; not in tail position, two arg
 (or 'what 'where)
 (or #f 'where)
 (or #f #f)
 (or 'what #f)
 ;; not in tail position, value discarded
 (begin (or 'what (error "two")) 'two)
 ;; in tail position (within the lambdas)
 ((lambda ()
    (or)))
 ((lambda ()
    (or 'what)))
 ((lambda ()
    (or #f)))
 ((lambda ()
    (or 'what 'where)))
 ((lambda ()
    (or #f 'where)))
 ((lambda ()
    (or #f #f)))
 ((lambda ()
    (or 'what #f))))
