## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 1998, 2000 Free Software Foundation, Inc.
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

## subdirs are for making distributions only.
SUBDIRS = md time

lib_LTLIBRARIES = @QTHREAD_LTLIBS@
EXTRA_LTLIBRARIES = libqthreads.la

## Prevent automake from adding extra -I options
DEFS = @DEFS@
INCLUDES = -I.. -I$(srcdir)/..

libqthreads_la_SOURCES = qt.c copyright.h
libqthreads_la_LIBADD = qtmds.lo qtmdc.lo
libqthreads_la_DEPENDENCIES = qtmds.lo qtmdc.lo
libqthreads_la_LDFLAGS = -rpath $(libdir) -version-info @LIBGUILEQTHREADS_INTERFACE_CURRENT@:@LIBGUILEQTHREADS_INTERFACE_REVISION@:@LIBGUILEQTHREADS_INTERFACE_AGE@ -export-dynamic -no-undefined

OMIT_DEPENDENCIES = axp.h hppa.h i386.h ksr.h m88k.h mips.h sparc.h vax.h

.s.lo:
	$(LIBTOOL) --mode=compile $(COMPILE) $(qthread_asflags) -c $<
qtmds.s:
	${LN_S} ${srcdir}/${qtmds_s} qtmds.s
qtmdc.c:
	${LN_S} ${srcdir}/${qtmdc_c} qtmdc.c
qtdmdb.s:
	${LN_S} ${srcdir}/${qtdmdb_s} qtdmdb.s

EXTRA_DIST = CHANGES README.MISC README.PORT b.h meas.c stp.c stp.h \
	Makefile.base config libqthreads.def
