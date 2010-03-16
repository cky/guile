## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 2001, 2002, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
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

AUTOMAKE_OPTIONS = gnu

## Prevent automake from adding extra -I options
DEFS = @DEFS@ @EXTRA_DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.  Also look for Gnulib headers in `lib'.
AM_CPPFLAGS = -I.. -I$(srcdir)/..				\
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib

AM_CFLAGS = $(GCC_CFLAGS)
AM_LDFLAGS = $(GNU_LD_FLAGS)

srfiincludedir = $(pkgincludedir)/srfi

# These headers are visible as <guile/srfi/mumble.h>
srfiinclude_HEADERS = srfi-1.h srfi-4.h srfi-13.h srfi-14.h srfi-60.h

lib_LTLIBRARIES = \
  libguile-srfi-srfi-1-v-@LIBGUILE_SRFI_SRFI_1_MAJOR@.la \
  libguile-srfi-srfi-4-v-@LIBGUILE_SRFI_SRFI_4_MAJOR@.la \
  libguile-srfi-srfi-13-14-v-@LIBGUILE_SRFI_SRFI_13_14_MAJOR@.la \
  libguile-srfi-srfi-60-v-@LIBGUILE_SRFI_SRFI_60_MAJOR@.la

BUILT_SOURCES = srfi-1.x srfi-4.x srfi-13.x srfi-14.x srfi-60.x

libguile_srfi_srfi_1_v_@LIBGUILE_SRFI_SRFI_1_MAJOR@_la_SOURCES = srfi-1.x srfi-1.c
libguile_srfi_srfi_1_v_@LIBGUILE_SRFI_SRFI_1_MAJOR@_la_LIBADD =		\
   $(top_builddir)/libguile/libguile-@GUILE_EFFECTIVE_VERSION@.la $(top_builddir)/lib/libgnu.la
libguile_srfi_srfi_1_v_@LIBGUILE_SRFI_SRFI_1_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_1_INTERFACE@

libguile_srfi_srfi_4_v_@LIBGUILE_SRFI_SRFI_4_MAJOR@_la_SOURCES = srfi-4.x srfi-4.c
libguile_srfi_srfi_4_v_@LIBGUILE_SRFI_SRFI_4_MAJOR@_la_LIBADD =		\
   $(top_builddir)/libguile/libguile-@GUILE_EFFECTIVE_VERSION@.la $(top_builddir)/lib/libgnu.la
libguile_srfi_srfi_4_v_@LIBGUILE_SRFI_SRFI_4_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_4_INTERFACE@

libguile_srfi_srfi_13_14_v_@LIBGUILE_SRFI_SRFI_13_14_MAJOR@_la_SOURCES = srfi-13.x srfi-13.c srfi-14.x srfi-14.c
libguile_srfi_srfi_13_14_v_@LIBGUILE_SRFI_SRFI_13_14_MAJOR@_la_LIBADD =	\
   $(top_builddir)/libguile/libguile-@GUILE_EFFECTIVE_VERSION@.la $(top_builddir)/lib/libgnu.la
libguile_srfi_srfi_13_14_v_@LIBGUILE_SRFI_SRFI_13_14_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_13_14_INTERFACE@

libguile_srfi_srfi_60_v_@LIBGUILE_SRFI_SRFI_60_MAJOR@_la_SOURCES = srfi-60.x srfi-60.c
libguile_srfi_srfi_60_v_@LIBGUILE_SRFI_SRFI_60_MAJOR@_la_LIBADD =	\
   $(top_builddir)/libguile/libguile-@GUILE_EFFECTIVE_VERSION@.la $(top_builddir)/lib/libgnu.la
libguile_srfi_srfi_60_v_@LIBGUILE_SRFI_SRFI_60_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_60_INTERFACE@

EXTRA_DIST = ChangeLog-2008

GUILE_SNARF = ../libguile/guile-snarf

MKDEP = gcc -M -MG $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x
