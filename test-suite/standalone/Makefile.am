## Process this file with automake to produce Makefile.in.
##
## Copyright 2003, 2004, 2005, 2006 Software Foundation, Inc.
##
## This file is part of GUILE.
##
## GUILE is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 2, or
## (at your option) any later version.
##
## GUILE is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with GUILE; see the file COPYING.  If not, write
## to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301 USA


# initializations so we can use += below.
TESTS =
noinst_LTLIBRARIES =
check_PROGRAMS = 
check_SCRIPTS =
BUILT_SOURCES =

TESTS_ENVIRONMENT = "${top_builddir}/pre-inst-guile-env"

test_cflags = \
  -I$(top_srcdir)/test-suite/standalone \
  -I$(top_srcdir) \
  -I$(top_srcdir)/libguile-ltdl $(EXTRA_DEFS) $(GUILE_CFLAGS)

AM_LDFLAGS = $(GUILE_CFLAGS)

snarfcppopts = \
  $(DEFS) $(DEFAULT_INCLUDES) $(CPPFLAGS) $(CFLAGS) -I$(top_srcdir)
SUFFIXES = .x
.c.x:
	${top_builddir}/libguile/guile-snarf -o $@ $< $(snarfcppopts)

CLEANFILES = *.x

.DELETE_ON_ERROR:

check_SCRIPTS += test-system-cmds
TESTS += test-system-cmds

check_SCRIPTS += test-require-extension
TESTS += test-require-extension

# test-num2integral
test_num2integral_SOURCES = test-num2integral.c
test_num2integral_CFLAGS = ${test_cflags}
test_num2integral_LDADD = ${top_builddir}/libguile/libguile.la
check_PROGRAMS += test-num2integral
TESTS += test-num2integral

# test-round
test_round_CFLAGS = ${test_cflags}
test_round_LDADD = ${top_builddir}/libguile/libguile.la
check_PROGRAMS += test-round
TESTS += test-round

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
libtest_asmobs_la_LDFLAGS = -no-undefined -rpath `pwd` # so libtool will really build an .so
libtest_asmobs_la_LIBADD = ${top_builddir}/libguile/libguile.la
BUILT_SOURCES += test-asmobs-lib.x
check_SCRIPTS += test-asmobs
TESTS += test-asmobs

# test-list
test_list_SOURCES = test-list.c
test_list_CFLAGS = ${test_cflags}
test_list_LDADD = ${top_builddir}/libguile/libguile.la
check_PROGRAMS += test-list
TESTS += test-list

# test-unwind
test_unwind_SOURCES = test-unwind.c
test_unwind_CFLAGS = ${test_cflags}
test_unwind_LDADD = ${top_builddir}/libguile/libguile.la
check_PROGRAMS += test-unwind
TESTS += test-unwind

# test-conversion
test_conversion_SOURCES = test-conversion.c
test_conversion_CFLAGS = ${test_cflags}
test_conversion_LDADD = ${top_builddir}/libguile/libguile.la
check_PROGRAMS += test-conversion
TESTS += test-conversion

# test-use-srfi
TESTS += test-use-srfi

all-local:
	cd ${srcdir} && chmod u+x ${check_SCRIPTS}

EXTRA_DIST = ${check_SCRIPTS}
