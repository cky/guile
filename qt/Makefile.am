## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

## subdirs are for making distributions only.
SUBDIRS = md time

lib_LTLIBRARIES = @QTHREAD_LTLIBS@
EXTRA_LTLIBRARIES = libqthreads.la

libqthreads_la_SOURCES = qt.c copyright.h
libqthreads_la_LIBADD = qtmds.lo qtmdc.lo
libqthreads_la_DEPENDENCIES = qtmds.lo qtmdc.lo
libqthreads_la_LDFLAGS = -rpath $(libdir)

.s.lo:
	$(LIBTOOL) --mode=compile $(COMPILE) $(qthread_asflags) -c $<
qtmds.s:
	${LN_S} ${srcdir}/${qtmds_s} qtmds.s
qtmdc.c:
	${LN_S} ${srcdir}/${qtmdc_c} qtmdc.c
qtdmdb.s:
	${LN_S} ${srcdir}/${qtdmdb_s} qtdmdb.s

EXTRA_DIST = CHANGES README.MISC README.PORT b.h meas.c stp.c stp.h \
	Makefile.base config
