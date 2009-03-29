TESTS_ENVIRONMENT = \
	$(top_builddir)/meta/guile \
	-l $(srcdir)/run-vm-tests.scm -e run-vm-tests

TESTS =						\
      t-basic-contructs.scm			\
      t-global-bindings.scm			\
      t-catch.scm				\
      t-call-cc.scm				\
      t-closure.scm				\
      t-closure2.scm				\
      t-closure3.scm				\
      t-closure4.scm				\
      t-do-loop.scm				\
      t-literal-integers.scm			\
      t-macros.scm				\
      t-macros2.scm				\
      t-map.scm					\
      t-or.scm					\
      t-proc-with-setter.scm			\
      t-quasiquote.scm				\
      t-values.scm				\
      t-records.scm				\
      t-match.scm				\
      t-mutual-toplevel-defines.scm

EXTRA_DIST = run-vm-tests.scm $(TESTS)
