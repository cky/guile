## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

## subdirs are for making distributions only.
SUBDIRS = md time

lib_LIBRARIES = @target_libs@
EXTRA_LIBRARIES = libqt.a

libqt_a_SOURCES = qt.c copyright.h
libqt_a_LIBADD = qtmds.o qtmdc.o

qtmds.o: @qtmds_s@ 
## FIXME use of -c with -o not portable.
	$(COMPILE) -o qtmds.o @qtmds_s@

qtmdc.o: @qtmdc_c@ @qtmd_h@
## FIXME use of -c with -o not portable.
	$(COMPILE) -o qtmdc.o @qtmdc_c@

EXTRA_DIST = CHANGES README.MISC README.PORT b.h meas.c stp.c stp.h \
PLUGIN/OPT Makefile.base config
