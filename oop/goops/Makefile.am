## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2000, 2001 Free Software Foundation, Inc.
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

# These should be installed and distributed.
goops_sources =							\
    active-slot.scm compile.scm composite-slot.scm describe.scm	\
    dispatch.scm internal.scm save.scm stklos.scm util.scm      \
    old-define-method.scm

subpkgdatadir = $(pkgdatadir)/$(VERSION)/oop/goops
subpkgdata_DATA = $(goops_sources)
ETAGS_ARGS = $(subpkgdata_DATA)

EXTRA_DIST = $(goops_sources)
