## Process this file with automake to produce Makefile.in.
##
##   Copyright (C) 1998,1999,2000,2001,2003 Free Software Foundation, Inc.
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

SUBDIRS = debugger

# These should be installed and distributed.
ice9_sources =								   \
	and-let-star.scm arrays.scm boot-9.scm calling.scm common-list.scm \
	debug.scm debugger.scm documentation.scm emacs.scm expect.scm	   \
	format.scm getopt-long.scm hcons.scm lineio.scm ls.scm mapping.scm \
	match.scm networking.scm null.scm optargs.scm poe.scm popen.scm	   \
	posix.scm psyntax.pp psyntax.ss q.scm r4rs.scm r5rs.scm		   \
	rdelim.scm receive.scm regex.scm runq.scm rw.scm		   \
	safe-r5rs.scm safe.scm session.scm slib.scm stack-catch.scm	   \
	streams.scm string-fun.scm syncase.scm threads.scm	   	   \
	buffered-input.scm time.scm history.scm channel.scm		   \
        pretty-print.scm ftw.scm gap-buffer.scm occam-channel.scm	   \
	weak-vector.scm

subpkgdatadir = $(pkgdatadir)/${GUILE_EFFECTIVE_VERSION}/ice-9
subpkgdata_DATA = $(ice9_sources)
ETAGS_ARGS = $(subpkgdata_DATA)

## test.scm is not currently installed.
EXTRA_DIST = $(ice9_sources) test.scm compile-psyntax.scm

if MAINTAINER_MODE
# We expect this to never be invoked when there is not already
# ice-9/psyntax.pp in %load-path, since compile-psyntax.scm depends
# on ice-9/syncase.scm, which does `(load-from-path "ice-9/psyntax.pp")'.
# In other words, to bootstrap this file, you need to do something like:
#    GUILE_LOAD_PATH=/usr/local/share/guile/1.5.4 make psyntax.pp
include $(top_srcdir)/am/pre-inst-guile
psyntax.pp: psyntax.ss
	$(preinstguile) -s $(srcdir)/compile-psyntax.scm \
		$(srcdir)/psyntax.ss $(srcdir)/psyntax.pp
endif
