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

AUTOMAKE_OPTIONS = gnu

## Prevent automake from adding extra -I options
DEFS = @DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
INCLUDES = -I.. -I$(srcdir)/..

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

EXTRA_DIST := ltdl.h ltdl.c ltdl.h.diff ltdl.c.diff
BUILT_SOURCES := ltdl.h.diff ltdl.c.diff

ltdl.h.diff: ltdl.h $(srcdir)/../raw-ltdl.h
	cp $(srcdir)/../raw-ltdl.h raw-ltdl.guilemod.h.tmp
	perl -pi \
	  -e 's/SCMLTXT/extern/go;' \
	  -e 's/SCMLTSTATIC //go;' \
	  -e 's/ SCM_UNUSED//go;' \
	  -e 's/SCM_INSERTED_DLSYMLIST_STRUCT_DECL //go;' \
	  raw-ltdl.guilemod.h.tmp
	mv raw-ltdl.guilemod.h.tmp raw-ltdl.guilemod.h
	diff -ru $(srcdir)/ltdl.h raw-ltdl.guilemod.h > ltdl.h.diff; \
	  test "$$?" -eq 1

ltdl.c.diff: ltdl.c $(srcdir)/../raw-ltdl.c
	cp $(srcdir)/../raw-ltdl.c raw-ltdl.guilemod.c.tmp
	perl -pi \
	  -e 's/SCMLTXT/extern/go;' \
	  -e 's/SCMLTSTATIC //go;' \
	  -e 's/ SCM_UNUSED//go;' \
	  -e 's/SCM_INSERTED_DLSYMLIST_STRUCT_DECL //go;' \
	  raw-ltdl.guilemod.c.tmp
	mv raw-ltdl.guilemod.c.tmp raw-ltdl.guilemod.c
	diff -ru $(srcdir)/ltdl.c raw-ltdl.guilemod.c > ltdl.c.diff; \
	  test "$$?" -eq 1

CLEANFILES := \
  raw-ltdl.guilemod.h raw-ltdl.guilemod.c \
  raw-ltdl.guilemod.h.tmp raw-ltdl.guilemod.c.tmp
