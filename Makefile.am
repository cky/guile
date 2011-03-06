## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002, 2006, 2007,
##        2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

# want automake 1.10 or higher so that AM_GNU_GETTEXT can tell automake that
# config.rpath is needed
#
AUTOMAKE_OPTIONS = 1.10

SUBDIRS =					\
	lib					\
	meta					\
	libguile				\
	module					\
	guile-readline				\
	examples				\
	emacs					\
	test-suite				\
	benchmark-suite				\
	gc-benchmarks				\
	am					\
	doc

libguileincludedir = $(pkgincludedir)/$(GUILE_EFFECTIVE_VERSION)
libguileinclude_HEADERS = libguile.h

EXTRA_DIST = LICENSE HACKING GUILE-VERSION	\
	     m4/ChangeLog-2008			\
	     ChangeLog-2008			\
	     .version

TESTS = check-guile

ACLOCAL_AMFLAGS = -I m4

DISTCLEANFILES = check-guile.log

DISTCHECK_CONFIGURE_FLAGS = --enable-error-on-warning

dist-hook: gen-ChangeLog gen-tarball-version

clean-local:
	rm -rf cache/

CONFIG_STATUS_DEPENDENCIES = GUILE-VERSION

gen_start_rev = 61db429e251bfd2f75cb4632972e0238056eb24b
.PHONY: gen-ChangeLog
gen-ChangeLog:
	if test -d .git; then						\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
	    $(gen_start_rev)..HEAD > $(distdir)/cl-t;			\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;			\
	fi

BUILT_SOURCES = $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo $(VERSION) > $@-t && mv $@-t $@
gen-tarball-version:
	echo $(VERSION) > $(distdir)/.tarball-version

# Makefile.am ends here
