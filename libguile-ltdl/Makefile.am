## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 2002 Free Software Foundation, Inc.
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

SUBDIRS = upstream

AUTOMAKE_OPTIONS = gnu

## Prevent automake from adding extra -I options
DEFS = @DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
INCLUDES = -I.. -I$(srcdir)/..

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

# We don't install this header since no one should be using the lib directly.
EXTRA_HEADERS = guile-ltdl.h raw-ltdl.h
EXTRA_DIST = raw-ltdl.c

lib_LTLIBRARIES = libguile-ltdl.la
libguile_ltdl_la_SOURCES = guile-ltdl.c
#libguile_ltdl_la_DEPENDENCIES = 
libguile_ltdl_la_LIBADD = ${LIBADD_DL}
libguile_ltdl_la_LDFLAGS = -version-info 1:0:0 -export-dynamic -no-undefined
