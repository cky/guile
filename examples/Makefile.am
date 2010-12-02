## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 2001, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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

EXTRA_DIST = README ChangeLog-2008 check.test				\
									\
 scripts/README scripts/simple-hello.scm scripts/hello			\
 scripts/fact								\
									\
 box/README box/box.c							\
									\
 box-module/README box-module/box.c					\
									\
 box-dynamic/README box-dynamic/box.c					\
									\
 box-dynamic-module/README box-dynamic-module/box.c			\
 box-dynamic-module/box-module.scm box-dynamic-module/box-mixed.scm	\
									\
 modules/README modules/module-0.scm modules/module-1.scm		\
 modules/module-2.scm modules/main					\
									\
 safe/README safe/safe safe/untrusted.scm safe/evil.scm			\
									\
 web/hello.scm web/debug-sxml.scm

AM_CFLAGS = `PATH=$(bindir)$(PATH_SEPARATOR)$$PATH PKG_CONFIG_PATH=$(libdir)/pkgconfig $(bindir)/guile-config compile`
AM_LIBS   = `PATH=$(bindir)$(PATH_SEPARATOR)$$PATH PKG_CONFIG_PATH=$(libdir)/pkgconfig $(bindir)/guile-config link`


box/box: box/box.o
	-$(MKDIR_P) box
	$(CC) $< $(AM_LIBS) -o $@

box/box.o: box/box.c
	-$(MKDIR_P) box
	$(CC) $(AM_CFLAGS) -c $< -o $@


box-module/box: box-module/box.o
	-$(MKDIR_P) box-module
	$(CC) $< $(AM_LIBS) -o $@

box-module/box.o: box-module/box.c
	-$(MKDIR_P) box-module
	$(CC) $(AM_CFLAGS) -c $< -o $@


libbox.la: box-dynamic/box.lo
	$(top_builddir)/libtool --mode=link $(CC) $< $(AM_LIBS) -rpath $(libdir) -o $@

box-dynamic/box.lo: box-dynamic/box.c
	-$(MKDIR_P) box-dynamic
	$(top_builddir)/libtool --mode=compile $(CC) $(AM_CFLAGS) -c $< -o $@


libbox-module.la: box-dynamic-module/box.lo
	$(top_builddir)/libtool --mode=link $(CC) $< $(AM_LIBS) -rpath $(libdir) -o $@

box-dynamic-module/box.lo: box-dynamic-module/box.c
	-$(MKDIR_P) box-dynamic-module
	$(top_builddir)/libtool --mode=compile $(CC) $(AM_CFLAGS) -c $< -o $@


installcheck: box/box box-module/box libbox.la libbox-module.la
	LD_LIBRARY_PATH="$(libdir)$(PATH_SEPARATOR)$$LD_LIBRARY_PATH"		\
	  LTDL_LIBRARY_PATH="$(builddir)$(PATH_SEPARATOR)$$LTDL_LIBRARY_PATH"	\
	  GUILE_LOAD_PATH="$(abs_top_srcdir)$(PATH_SEPARATOR)$$GUILE_LOAD_PATH"	\
	  PATH="$(bindir)$(PATH_SEPARATOR)$$PATH"		\
	  GUILE_AUTO_COMPILE=0					\
	  srcdir="$(srcdir)"					\
	  $(srcdir)/check.test

CLEANFILES =					\
  box/box box/box.o				\
  box-module/box box-module/box.o

clean-local:
	$(top_builddir)/libtool --mode=clean rm -f	\
	  box-dynamic/box.lo libbox.la			\
	  box-dynamic-module/box.lo libbox-module.la
