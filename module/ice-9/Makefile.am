## Process this file with automake to produce Makefile.in.
##
##   Copyright (C) 1998,1999,2000,2001,2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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
##   to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
##   Floor, Boston, MA 02110-1301 USA

AUTOMAKE_OPTIONS = gnu

SUBDIRS = debugger debugging

# These should be installed and distributed.
modpath = ice-9
# Compile psyntax and boot-9 first, so that we get the speed benefit in
# the rest of the compilation. Also, if there is too much switching back
# and forth between interpreted and compiled code, we end up using more
# of the C stack than the interpreter would have; so avoid that by
# putting these core modules first.
SOURCES = psyntax-pp.scm annotate.scm boot-9.scm \
	and-let-star.scm calling.scm common-list.scm			\
	debug.scm debugger.scm documentation.scm emacs.scm expect.scm	\
	format.scm getopt-long.scm hcons.scm i18n.scm			\
	lineio.scm ls.scm mapping.scm match.scm				\
	networking.scm null.scm occam-channel.scm optargs.scm poe.scm	\
	popen.scm posix.scm q.scm r4rs.scm r5rs.scm			\
	rdelim.scm receive.scm regex.scm runq.scm rw.scm		\
	safe-r5rs.scm safe.scm session.scm slib.scm stack-catch.scm	\
	streams.scm string-fun.scm syncase.scm threads.scm		\
	buffered-input.scm time.scm history.scm channel.scm		\
        pretty-print.scm ftw.scm gap-buffer.scm				\
	weak-vector.scm deprecated.scm list.scm serialize.scm		\
	gds-server.scm

# gds-client is tight with the memoizer, so punt on it until it can be
# made portable.
#
# psyntax.scm needs help. fortunately it's only needed when recompiling
# psyntax-pp.scm.
NOCOMP_SOURCES = gds-client.scm psyntax.scm

include $(top_srcdir)/am/guilec

## test.scm is not currently installed.
EXTRA_DIST += test.scm compile-psyntax.scm ChangeLog-2008

TAGS_FILES = $(SOURCES)

# We expect this to never be invoked when there is not already
# ice-9/psyntax-pp.scm in %load-path, since compile-psyntax.scm depends
# on ice-9/syncase.scm, which does `(load-from-path "ice-9/psyntax-pp.scm")'.
# In other words, to bootstrap this file, you need to do something like:
#    GUILE_LOAD_PATH=/usr/local/share/guile/1.5.4 make psyntax-pp.scm
include $(top_srcdir)/am/pre-inst-guile
psyntax-pp.scm: psyntax.scm
	$(preinstguile) -s $(srcdir)/compile-psyntax.scm \
		$(srcdir)/psyntax.scm $(srcdir)/psyntax-pp.scm
