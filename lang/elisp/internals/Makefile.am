## Process this file with automake to produce Makefile.in.
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

AUTOMAKE_OPTIONS = gnu

# These should be installed and distributed.

elisp_sources = \
	evaluation.scm \
	format.scm \
	fset.scm \
	lambda.scm \
	load.scm \
	null.scm \
	set.scm \
	signal.scm \
	time.scm \
	trace.scm

subpkgdatadir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)/lang/elisp/internals
subpkgdata_DATA = $(elisp_sources)
ETAGS_ARGS = $(subpkgdata_DATA)

EXTRA_DIST = $(elisp_sources)
