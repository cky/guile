## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
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


SUBDIRS = ice-9 oop qt libltdl libguile guile-config guile-readline \
	  scripts srfi doc

bin_SCRIPTS = guile-tools

include_HEADERS = libguile.h

# automake sometimes forgets to distribute acconfig.h,
# apparently depending on the phase of the moon.
EXTRA_DIST = qthreads.m4 HACKING GUILE-VERSION ANON-CVS SNAPSHOTS \
	     test-suite $(ACLOCAL) acconfig.h

TESTS = check-guile

# The rule will cd to $(top_srcdir).
ACLOCAL = ./guile-aclocal.sh
