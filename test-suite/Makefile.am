SCM_TESTS = tests/alist.test			\
	    tests/bit-operations.test		\
	    tests/c-api.test			\
	    tests/chars.test			\
	    tests/common-list.test		\
	    tests/elisp.test			\
	    tests/environments.test		\
	    tests/eval.test			\
	    tests/exceptions.test		\
	    tests/format.test			\
	    tests/gc.test			\
	    tests/getopt-long.test		\
	    tests/goops.test			\
	    tests/guardians.test		\
	    tests/hooks.test			\
	    tests/import.test			\
	    tests/interp.test			\
	    tests/list.test			\
	    tests/load.test			\
	    tests/multilingual.nottest		\
	    tests/numbers.test			\
	    tests/optargs.test			\
	    tests/ports.test			\
	    tests/r4rs.test			\
	    tests/reader.test			\
	    tests/regexp.test			\
	    tests/srfi-10.test			\
	    tests/srfi-13.test			\
	    tests/srfi-14.test			\
	    tests/srfi-19.test			\
	    tests/srfi-4.test			\
	    tests/srfi-9.test			\
	    tests/strings.test			\
	    tests/symbols.test			\
	    tests/syncase.test			\
	    tests/syntax.test			\
	    tests/time.test			\
	    tests/version.test			\
	    tests/weaks.test

SCM_TESTS_DIRS = tests/asmobs \
                 tests/c-api

EXTRA_DIST = guile-test lib.scm $(SCM_TESTS)

## Automake should be able to handle the distribution of tests/asmobs
## etc without any help, but not all version can handle 'deep'
## directories.  So we do it on our own.
dist-hook:
	for d in $(SCM_TESTS_DIRS); do \
	  cp -pR $(srcdir)/$$d $(distdir)/$$d; \
          rm -rf $(distdir)/$$d/CVS; \
        done
