## Process this file with automake to produce Makefile.in.
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

SUBDIRS = ice-9 qt libguile guile-config guile-readline doc

## FIXME: in the future there will be direct automake support for
## doing this.  When that happens, switch over.
aclocaldir = $(datadir)/aclocal
aclocal_DATA = guile.m4 qthreads.m4

EXTRA_DIST = $(aclocal_DATA) ltconfig ltmain.sh acconfig.h \
	HACKING GUILE-VERSION ANON-CVS SNAPSHOTS
