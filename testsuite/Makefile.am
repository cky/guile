# The test programs.

# The Libtool executable.
GUILE_VM = $(top_builddir)/pre-inst-guile

vm_test_files =					\
      t-basic-contructs.scm			\
      t-global-bindings.scm			\
      t-catch.scm				\
      t-call-cc.scm				\
      t-closure.scm				\
      t-closure2.scm				\
      t-closure3.scm				\
      t-do-loop.scm				\
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

EXTRA_DIST = run-vm-tests.scm $(vm_test_files)


check:
	$(top_builddir)/pre-inst-guile-env $(GUILE_VM)  \
	            -L $(top_srcdir)/module		\
	            -l run-vm-tests.scm -e run-vm-tests	\
		    $(vm_test_files)
