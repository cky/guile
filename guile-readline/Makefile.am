## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 1998, 1999, 2000, 2001, 2004, 2006, 2007, 2008,
##     2009, 2010, 2012 Free Software Foundation, Inc.
##
##   This file is part of guile-readline.
##
##   guile-readline is free software; you can redistribute it and/or
##   modify it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   guile-readline is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
##   General Public License for more details.
##
##   You should have received a copy of the GNU General Public License
##   along with guile-readline; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
##   Floor, Boston, MA 02110-1301 USA

include $(top_srcdir)/am/snarf
include $(top_srcdir)/am/guilec

## Prevent automake from adding extra -I options
DEFS = @DEFS@ @EXTRA_DEFS@

# We're at the root of the module hierarchy.
modpath =
SOURCES =

if HAVE_READLINE

SOURCES += ice-9/readline.scm

## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.  Also look for Gnulib headers in `lib'.
AM_CPPFLAGS = -I. -I.. -I$(srcdir)/..			\
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib

AM_CFLAGS = $(GCC_CFLAGS)

lib_LTLIBRARIES = libguilereadline-v-@LIBGUILEREADLINE_MAJOR@.la

libguilereadline_v_@LIBGUILEREADLINE_MAJOR@_la_SOURCES = readline.c
libguilereadline_v_@LIBGUILEREADLINE_MAJOR@_la_LIBADD =	\
  $(READLINE_LIBS)					\
  ../libguile/libguile-@GUILE_EFFECTIVE_VERSION@.la ../lib/libgnu.la

libguilereadline_v_@LIBGUILEREADLINE_MAJOR@_la_LDFLAGS =	\
  -version-info @LIBGUILEREADLINE_INTERFACE@ -export-dynamic	\
  -no-undefined


BUILT_SOURCES = readline.x

modincludedir = $(pkgincludedir)/$(GUILE_EFFECTIVE_VERSION)
modinclude_HEADERS = readline.h

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)
SUFFIXES += .x
.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

EXTRA_DIST += LIBGUILEREADLINE-VERSION ChangeLog-2008

ETAGS_ARGS +=							\
  $(libguilereadline_v_@LIBGUILEREADLINE_MAJOR@_la_SOURCES)

CLEANFILES += *.x *.go

endif HAVE_READLINE

dist-hook:
	(temp="/tmp/mangle-deps.$$$$"; \
	 trap "rm -f $$temp" 0 1 2 15; \
	 sed -e 's|\([ 	]\)\(\.\./\(\|libguile/\|guile-readline/\)[a-z_-]*\.h\)|\1$$(srcdir)/\2|g' $(distdir)/Makefile.in \
	 | sed -e 's|\$$(srcdir)/\(\.\./libguile/\(libpath\|scmconfig\|versiondat\)\)|\1|g' \
	 | sed -e 's|\.\./guile-readline/\([a-z_-]*\.x\)|\1|g' > $$temp \
	 && chmod u+w $(distdir)/Makefile.in && cp -p $$temp $(distdir)/Makefile.in)

