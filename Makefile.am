## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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

AUTOMAKE_OPTIONS = 1.5

SUBDIRS = oop libguile-ltdl libguile ice-9 guile-config guile-readline \
	  scripts srfi doc examples test-suite benchmark-suite lang am

bin_SCRIPTS = guile-tools

include_HEADERS = libguile.h

# automake sometimes forgets to distribute acconfig.h,
# apparently depending on the phase of the moon.
EXTRA_DIST = HACKING GUILE-VERSION ANON-CVS SNAPSHOTS BUGS

TESTS = check-guile

ACLOCAL_AMFLAGS = -I guile-config

DISTCLEANFILES = check-guile.log

# Makefile.am ends here
