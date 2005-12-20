# The test programs.

# The Libtool executable.
GUILE_VM = $(top_srcdir)/src/guile-vm

vm_test_files =					\
      t-global-bindings.scm			\
      t-closure.scm				\
      t-closure2.scm				\
      t-closure3.scm				\
      t-do-loop.scm				\
      t-macros.scm				\
      t-macros2.scm				\
      t-proc-with-setter.scm			\
      t-values.scm				\
      t-records.scm				\
      t-match.scm

EXTRA_DIST = run-vm-tests.scm $(vm_test_files)


check:
	$(GUILE_VM) -L $(top_srcdir)/module		\
	            -l run-vm-tests.scm -e run-vm-tests	\
		    $(vm_test_files)
