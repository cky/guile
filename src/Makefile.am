bin_PROGRAMS = guile-vm
guile_vm_SOURCES = guile-vm.c
guile_vm_LDADD = libguilevm.la 
guile_vm_LDFLAGS = $(GUILE_LDFLAGS)

bin_SCRIPTS = guile-compile

lib_LTLIBRARIES = libguilevm.la
libguilevm_la_SOURCES = vm.c
libguilevm_la_LDFLAGS = -version-info 0:0:0 -export-dynamic
noinst_HEADERS = vm.h vm_engine.h vm_expand.h
EXTRA_DIST = vm_engine.c vm_system.c vm_scheme.c vm_number.c \
	     test.scm guile-compile.in
BUILT_SOURCES = vm_system.i vm_scheme.i vm_number.i vm.x

CFLAGS = -g -O2 -Wall
INCLUDES = $(GUILE_CFLAGS)
CLEANFILES = $(bin_SCRIPTS)
DISTCLEANFILES = $(BUILT_SOURCES)
MAINTAINERCLEANFILES = Makefile.in config.h.in stamp-h.in

SNARF = guile-snarf
SUFFIXES = .x .i
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.c.i:
	grep '^SCM_DEFINE' $< > $@

$(BUILT_SOURCES): config.h vm_expand.h

guile-compile: guile-compile.in
	sed -e 's!\@bindir\@!$(bindir)!' -e 's!\@PACKAGE\@!$(PACKAGE)!' \
	  $< > $@

test: all
	$(bin_PROGRAMS) -s test.scm

debug-test: all
	$(bin_PROGRAMS) -s test.scm debug
