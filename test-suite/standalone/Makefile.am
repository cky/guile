
# initializations so we can use += below.
TESTS =
noinst_LTLIBRARIES =
check_PROGRAMS = 
check_SCRIPTS =
BUILT_SOURCES =

TESTS_ENVIRONMENT = "${top_builddir}/pre-inst-guile-env"

test_cflags := \
  -I$(top_srcdir)/test-suite/standalone \
  -I$(top_srcdir) \
  -I$(top_srcdir)/libguile-ltdl

snarfcppopts = $(DEFS) $(DEFAULT_INCLUDES) $(CPPFLAGS) $(CFLAGS)
%.x: %.c
	${top_builddir}/libguile/guile-snarf -o $@ $< $(snarfcppopts)

CLEANFILES = *.x

.DELETE_ON_ERROR:

# test-num2integral
test_num2integral_SOURCES = test-num2integral.c
test_num2integral_CFLAGS = ${test_cflags}
test_num2integral_LDADD = ${top_builddir}/libguile/libguile.la
check_PROGRAMS += test-num2integral
TESTS += test-num2integral

# test-gh
test_gh_SOURCES = test-gh.c
test_gh_CFLAGS = ${test_cflags}
test_gh_LDADD = ${top_builddir}/libguile/libguile.la
check_PROGRAMS += test-gh
TESTS += test-gh

# test-asmobs
noinst_LTLIBRARIES += libtest-asmobs.la
libtest_asmobs_la_SOURCES = test-asmobs-lib.c test-asmobs-lib.x
libtest_asmobs_la_CFLAGS = ${test_cflags}
libtest_asmobs_la_LDFLAGS = -rpath `pwd` # so libtool will really build an .so
libtest_asmobs_la_LIBADD = ${top_builddir}/libguile/libguile.la
BUILT_SOURCES += test-asmobs-lib.x
check_SCRIPTS += test-asmobs
TESTS += test-asmobs

all-local:
	cd ${srcdir} && chmod u+x ${check_SCRIPTS}

EXTRA_DIST = ${check_SCRIPTS}
