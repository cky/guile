# guile-test.sh --
#

for item in \
    test-glr-basics-01.scm              \
    test-glr-basics-02.scm              \
    test-glr-basics-03.scm              \
    test-glr-basics-04.scm              \
    test-glr-basics-05.scm              \
    test-glr-associativity.scm          \
    test-glr-script-expression.scm      \
    test-glr-single-expressions.scm     \
    \
    test-lr-basics-01.scm               \
    test-lr-basics-02.scm               \
    test-lr-basics-03.scm               \
    test-lr-basics-04.scm               \
    test-lr-basics-05.scm               \
    test-lr-error-recovery-01.scm       \
    test-lr-error-recovery-02.scm       \
    test-lr-no-clause.scm               \
    test-lr-associativity-01.scm        \
    test-lr-script-expression.scm       \
    test-lr-single-expressions.scm
    do
printf "\n\n*** Running $item\n"
guile $item
done

### end of file
