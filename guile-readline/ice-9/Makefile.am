## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
##  	Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
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

guile_pdd = $(patsubst %/guile-readline,%/guile,$(pkgdatadir))
ice9dir = $(guile_pdd)/$(GUILE_EFFECTIVE_VERSION)/ice-9
ice9_DATA = readline.scm
ETAGS_ARGS = $(ice9_DATA)
EXTRA_DIST = $(ice9_DATA)
