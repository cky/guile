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

AUTOMAKE_OPTIONS = foreign

# pending the papers from Robert Merkel
# man_MANS = guile.1

info_TEXINFOS = guile.texi r5rs.texi goops.texi guile-tut.texi

guile_TEXINFOS = preface.texi intro.texi scheme-intro.texi \
	scheme-ideas.texi scheme-data.texi scheme-procedures.texi \
	scheme-utility.texi scheme-binding.texi scheme-control.texi \
	scheme-io.texi scheme-evaluation.texi scheme-memory.texi \
	scheme-modules.texi scheme-scheduling.texi scheme-options.texi \
	scheme-translation.texi scheme-debug.texi deprecated.texi \
	scheme-reading.texi scheme-indices.texi slib.texi posix.texi \
	expect.texi scsh.texi tcltk.texi scripts.texi gh.texi scm.texi \
	appendices.texi indices.texi script-getopt.texi data-rep.texi \
	extend.texi repl-modules.texi srfi-modules.texi \
	AUTHORS

guile_tut_TEXINFOS = guile-tut.texi AUTHORS

goops_TEXINFOS = goops-tutorial.texi hierarchy.eps hierarchy.txt AUTHORS

# Optionally support building an HTML version of the reference manual.

if HTMLDOC

htmldir = $(prefix)/html/guile-$(VERSION)

html_DATA = guile_toc.html $(shell ls guile_*.html 2>/dev/null) guile-tut_toc.html $(shell ls guile-tut_*.html 2>/dev/null)

guile_toc.html: guile.texi $(guile_TEXINFOS)
	$(TEXI2HTML) -split_chapter guile.texi

guile-tut_toc.html: guile-tut.texi
	$(TEXI2HTML) -split_chapter guile-tut.texi

endif

EXAMPLE_SMOB_FILES = \
    ChangeLog Makefile README COPYING image-type.c image-type.h myguile.c

OLDFMT = oldfmt.c

dist-hook:
	mkdir $(distdir)/example-smob
	(cd $(srcdir); \
	 cp $(OLDFMT) $(distdir); \
	 dest="`cd $(distdir)/example-smob; pwd`"; \
         cd example-smob; \
	 cp $(EXAMPLE_SMOB_FILES) $$dest)

## The following two kluges were added so that CVS checkouts would
## build when not configured with `--enable-maintainer-mode'.  They
## were removed again since they relied on internal automake features.

# This rule overrides automake's rule for version.texi.  It causes
# version.texi to be created even in non-maintainer-mode.
#$(srcdir)/version.texi: stamp-vti
#	@:

# And the same for version-tutorial.texi.
#$(srcdir)/version-tutorial.texi: stamp-vti1
#	@:

# pending the papers from Robert Merkel
# EXTRA_DIST = guile.1
