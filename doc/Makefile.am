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

# This rule overrides automake's rule for version.texi.  It causes
# version.texi to be created even in non-maintainer-mode.
#
$(srcdir)/version.texi: stamp-vti
	@:

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
