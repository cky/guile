## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 2001 Free Software Foundation, Inc.
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

AUTOMAKE_OPTIONS = gnu

## Prevent automake from adding extra -I options
DEFS = @DEFS@ @EXTRA_DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
INCLUDES = -I.. -I$(srcdir)/..


lib_LTLIBRARIES = libguile-srfi-srfi-13-14.la libguile-srfi-srfi-4.la

BUILT_SOURCES = srfi-13.x srfi-14.x srfi-4.x
libguile_srfi_srfi_13_14_la_SOURCES = srfi-13.x srfi-13.c srfi-14.x srfi-14.c\
 srfi-13.h srfi-14.h
libguile_srfi_srfi_13_14_la_LDFLAGS = -version-info 0:0 -export-dynamic -no-undefined
libguile_srfi_srfi_13_14_la_LIBADD = $(top_srcdir)/libguile/libguile.la

libguile_srfi_srfi_4_la_SOURCES = srfi-4.x srfi-4.c srfi-4.h
libguile_srfi_srfi_4_la_LDFLAGS = -version-info 0:0 -export-dynamic -no-undefined
libguile_srfi_srfi_4_la_LIBADD = $(top_srcdir)/libguile/libguile.la

srfidir = $(datadir)/guile/$(VERSION)/srfi
srfi_DATA = srfi-1.scm \
            srfi-2.scm \
	    srfi-4.scm \
            srfi-6.scm \
            srfi-8.scm \
            srfi-9.scm \
            srfi-10.scm \
            srfi-11.scm \
            srfi-13.scm \
            srfi-14.scm \
	    srfi-16.scm \
            srfi-17.scm \
            srfi-19.scm

EXTRA_DIST = $(srfi_DATA)

GUILE_SNARF = ../libguile/guile-snarf

MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

CLEANFILES = *.x
