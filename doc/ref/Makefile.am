## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998 Free Software Foundation, Inc.
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

info_TEXINFOS = guile.texi

guile_TEXINFOS = preface.texi intro.texi program.texi scheme-intro.texi \
	scheme-ideas.texi scheme-data.texi scheme-procedures.texi \
	scheme-utility.texi scheme-binding.texi scheme-control.texi \
	scheme-io.texi scheme-evaluation.texi scheme-memory.texi \
	scheme-modules.texi scheme-scheduling.texi scheme-options.texi \
	scheme-translation.texi scheme-debug.texi deprecated.texi \
	scheme-reading.texi scheme-indices.texi slib.texi posix.texi \
	expect.texi scsh.texi tcltk.texi scripts.texi gh.texi scm.texi \
	debugging.texi indices.texi script-getopt.texi data-rep.texi \
	extend.texi repl-modules.texi srfi-modules.texi misc-modules.texi \
	scheme-compound.texi autoconf.texi autoconf-macros.texi

ETAGS_ARGS = $(info_TEXINFOS) $(guile_TEXINFOS)

GUILE = $(top_builddir)/libguile/guile

# Automated snarfing

autoconf.texi: autoconf-macros.texi
autoconf-macros.texi: $(top_srcdir)/guile-config/guile.m4
	GUILE=$(GUILE) $(top_srcdir)/scripts/snarf-guile-m4-docs $< > $@

# Optionally support building an HTML version of the reference manual.

if HTMLDOC

htmldir = $(prefix)/html/guile-$(VERSION)

html_DATA = guile_toc.html $(shell ls guile_*.html 2>/dev/null)

guile_toc.html: guile.texi $(guile_TEXINFOS)
	$(TEXI2HTML) -split_chapter guile.texi

endif
