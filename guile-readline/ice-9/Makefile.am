## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
##
##   This file is part of guile-readline.
##
##   guile-readline is free software; you can redistribute it and/or
##   modify it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   guile-readline is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
##   General Public License for more details.
##
##   You should have received a copy of the GNU General Public License
##   along with guile-readline; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
##   Floor, Boston, MA 02110-1301 USA

# Guile's `pkgdatadir'.
guile_pdd = $(datadir)/guile

ice9dir = $(guile_pdd)/$(GUILE_EFFECTIVE_VERSION)/ice-9
ice9_DATA = readline.scm
ETAGS_ARGS = $(ice9_DATA)
EXTRA_DIST = $(ice9_DATA)
