## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##   
##   GUILE is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 2, or
##   (at your option) any later version.
##   
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public
##   License along with GUILE; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 59 Temple Place, Suite
##   330, Boston, MA 02111-1307 USA

AUTOMAKE_OPTIONS = foreign

## Prevent automake from adding extra -I options
DEFS = @DEFS@ @EXTRA_DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
INCLUDES = -I.. -I$(srcdir)/..

GUILE_SNARF = ../libguile/guile-snarf

lib_LTLIBRARIES = libguilereadline.la

libguilereadline_la_SOURCES = readline.c
libguilereadline_la_LDFLAGS = -export-dynamic -no-undefined
libguilereadline_la_LIBADD = ../libguile/libguile.la

BUILT_SOURCES = readline.x

pkginclude_HEADERS = readline.h

ice9dir = $(datadir)/guile/$(VERSION)/ice-9
ice9_DATA = readline.scm

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

EXTRA_DIST = readline.scm

MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

CLEANFILES = *.x

dist-hook:
	(temp="/tmp/mangle-deps.$$$$"; \
	 trap "rm -f $$temp" 0 1 2 15; \
	 sed -e 's|\([ 	]\)\(\.\./\(\|libguile/\|guile-readline/\)[a-z_-]*\.h\)|\1$$(srcdir)/\2|g' $(distdir)/Makefile.in \
	 | sed -e 's|\$$(srcdir)/\(\.\./libguile/\(libpath\|scmconfig\|versiondat\)\)|\1|g' \
	 | sed -e 's|\.\./guile-readline/\([a-z_-]*\.x\)|\1|g' > $$temp \
	 && cp -p $$temp $(distdir)/Makefile.in)
