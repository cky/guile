## Process this file with automake to produce Makefile.in.
##
## Copyright 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
##   2011 Free Software Foundation, Inc.
##
## This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

include $(top_srcdir)/am/snarf

# initializations so we can use += below.
TESTS =
noinst_LTLIBRARIES =
check_PROGRAMS = 
check_SCRIPTS =
BUILT_SOURCES =
EXTRA_DIST =

TESTS_ENVIRONMENT =						\
  builddir="$(builddir)"					\
  GUILE_AUTO_COMPILE=0 "${top_builddir}/meta/uninstalled-env"

## Check for headers in $(srcdir) and bulid dir before $(CPPFLAGS), which
## may point us to an old, installed version of guile.
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir) \
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib

test_cflags =					\
  -I$(top_srcdir)/test-suite/standalone -I.	\
  $(EXTRA_DEFS) $(GUILE_CFLAGS) $(GCC_CFLAGS)

AM_LDFLAGS = $(GUILE_CFLAGS)

# How to link with libguile (same as `Libs' in `guile-2.0.pc'.)
LIBGUILE_LDADD =							\
  ${top_builddir}/libguile/libguile-@GUILE_EFFECTIVE_VERSION@.la	\
  $(BDW_GC_LIBS)


snarfcppopts =								      \
  -I$(top_srcdir) -I$(top_srcdir)/lib -I$(top_builddir)/lib -I$(top_builddir) \
  -I. $(DEFS) $(DEFAULT_INCLUDES) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x
.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x

.DELETE_ON_ERROR:

check_SCRIPTS += test-system-cmds
TESTS += test-system-cmds

check_SCRIPTS += test-bad-identifiers
TESTS += test-bad-identifiers

check_SCRIPTS += test-require-extension
TESTS += test-require-extension

check_SCRIPTS += test-guile-snarf
TESTS += test-guile-snarf

# test-num2integral
test_num2integral_SOURCES = test-num2integral.c
test_num2integral_CFLAGS = ${test_cflags}
test_num2integral_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-num2integral
TESTS += test-num2integral

# test-round
test_round_CFLAGS = ${test_cflags}
test_round_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-round
TESTS += test-round

if HAVE_SHARED_LIBRARIES

# test-asmobs
noinst_LTLIBRARIES += libtest-asmobs.la
libtest_asmobs_la_SOURCES = test-asmobs-lib.c
libtest_asmobs_la_CFLAGS = ${test_cflags}
libtest_asmobs_la_LDFLAGS = -module -no-undefined -rpath `pwd` # so libtool will really build an .so
libtest_asmobs_la_LIBADD = $(LIBGUILE_LDADD)
BUILT_SOURCES += test-asmobs-lib.x
check_SCRIPTS += test-asmobs
TESTS += test-asmobs

# test-ffi
noinst_LTLIBRARIES += libtest-ffi.la
libtest_ffi_la_SOURCES = test-ffi-lib.c
libtest_ffi_la_CFLAGS = ${test_cflags}
libtest_ffi_la_LDFLAGS = -module -no-undefined -rpath `pwd` # so libtool will really build an .so
libtest_ffi_la_LIBADD = $(LIBGUILE_LDADD)
check_SCRIPTS += test-ffi
TESTS += test-ffi

endif HAVE_SHARED_LIBRARIES

# test-list
test_list_SOURCES = test-list.c
test_list_CFLAGS = ${test_cflags}
test_list_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-list
TESTS += test-list

# test-unwind
test_unwind_SOURCES = test-unwind.c
test_unwind_CFLAGS = ${test_cflags}
test_unwind_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-unwind
TESTS += test-unwind

# test-conversion
test_conversion_SOURCES = test-conversion.c
test_conversion_CFLAGS = ${test_cflags}
test_conversion_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-conversion
TESTS += test-conversion

# test-loose-ends
test_loose_ends_SOURCES = test-loose-ends.c
test_loose_ends_CFLAGS = ${test_cflags}
test_loose_ends_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-loose-ends
TESTS += test-loose-ends

# test-fast-slot-ref
check_SCRIPTS += test-fast-slot-ref
TESTS += test-fast-slot-ref

# test-mb-regexp
check_SCRIPTS += test-mb-regexp
TESTS += test-mb-regexp

# test-use-srfi
check_SCRIPTS += test-use-srfi
TESTS += test-use-srfi

# test-scm-c-read
test_scm_c_read_SOURCES = test-scm-c-read.c
test_scm_c_read_CFLAGS = ${test_cflags}
test_scm_c_read_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-scm-c-read
TESTS += test-scm-c-read

# test-scm-take-locale-symbol
test_scm_take_locale_symbol_SOURCES = test-scm-take-locale-symbol.c
test_scm_take_locale_symbol_CFLAGS = ${test_cflags}
test_scm_take_locale_symbol_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-scm-take-locale-symbol
TESTS += test-scm-take-locale-symbol

# test-scm-take-u8vector
test_scm_take_u8vector_SOURCES = test-scm-take-u8vector.c
test_scm_take_u8vector_CFLAGS = ${test_cflags}
test_scm_take_u8vector_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-scm-take-u8vector
TESTS += test-scm-take-u8vector

if HAVE_SHARED_LIBRARIES

# test-extensions
noinst_LTLIBRARIES += libtest-extensions.la
libtest_extensions_la_SOURCES = test-extensions-lib.c
libtest_extensions_la_CFLAGS = ${test_cflags}
libtest_extensions_la_LDFLAGS = -module -no-undefined -rpath `pwd` # so libtool will really build an .so
libtest_extensions_la_LIBADD = $(LIBGUILE_LDADD)
check_SCRIPTS += test-extensions
TESTS += test-extensions

endif HAVE_SHARED_LIBRARIES

if BUILD_PTHREAD_SUPPORT

# test-with-guile-module
test_with_guile_module_CFLAGS = ${test_cflags}
test_with_guile_module_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-with-guile-module
TESTS += test-with-guile-module

test_scm_with_guile_CFLAGS = ${test_cflags}
test_scm_with_guile_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-scm-with-guile
TESTS += test-scm-with-guile

else

EXTRA_DIST += test-with-guile-module.c test-scm-with-guile.c

endif

EXTRA_DIST += ${check_SCRIPTS}
