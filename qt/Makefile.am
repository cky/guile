## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

## subdirs are for making distributions only.
SUBDIRS = md time

lib_LIBRARIES = @target_libs@
EXTRA_LIBRARIES = libqt.a

libqt_a_SOURCES = qt.c copyright.h
libqt_a_LIBADD = qtmds.o qtmdc.o

qtmds.o: @qtmds_s@ 
	$(COMPILE) -c @qtmds_s@
	mv @qtmds_o@ qtmds.o

qtmdc.o: @qtmdc_c@ @qtmd_h@
	$(COMPILE) -c @qtmdc_c@
	mv @qtmdc_o@ qtmdc.o

EXTRA_DIST = CHANGES README.MISC README.PORT b.h meas.c stp.c stp.h \
Makefile.base config
