(set! %load-path (cons (string-append (getenv "HOME") "/src/guile")
                       %load-path))

(load "../test-suite/guile-test")

(main `("guile-test"
        "--test-suite" ,(string-append (getenv "HOME")
                                       "/src/guile/test-suite/tests")
        "--log-file" ",,test-suite.log"))
