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

SUBDIRS = ref tutorial goops r5rs

# pending the papers from Robert Merkel
# man_MANS = guile.1

EXAMPLE_SMOB_FILES = \
    ChangeLog Makefile README COPYING image-type.c image-type.h myguile.c

OLDFMT = oldfmt.c

dist-hook:
	cp $(srcdir)/$(OLDFMT) $(distdir)/
	mkdir $(distdir)/example-smob
	for f in $(EXAMPLE_SMOB_FILES); do \
	  cp $(srcdir)/example-smob/$$f $(distdir)/example-smob/; \
        done

# pending the papers from Robert Merkel
# EXTRA_DIST = guile.1
