bin_PROGRAMS = guile-vm
guile_vm_SOURCES = guile-vm.c
guile_vm_LDADD = libguilevm.la 
guile_vm_LDFLAGS = $(GUILE_LDFLAGS)

bin_SCRIPTS = guile-compile

lib_LTLIBRARIES = libguilevm.la
libguilevm_la_SOURCES = vm.c
libguilevm_la_LDFLAGS = -version-info 0:0:0 -export-dynamic
noinst_HEADERS = vm.h vm_engine.h vm-snarf.h
EXTRA_DIST = vm_engine.c vm_system.c vm_scheme.c vm_number.c \
	     test.scm guile-compile.in
BUILT_SOURCES = vm_system.inst vm_scheme.inst vm_number.inst \
		vm_system.label vm_scheme.label vm_number.label \
		vm_system.opcode vm_scheme.opcode vm_number.opcode vm.x

CFLAGS = -g -O2 -Wall
INCLUDES = $(GUILE_CFLAGS)
CLEANFILES = $(bin_SCRIPTS)
DISTCLEANFILES = $(BUILT_SOURCES)
MAINTAINERCLEANFILES = Makefile.in config.h.in stamp-h.in

SNARF = guile-snarf
SUFFIXES = .x .inst .label .opcode
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.c.inst:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.c.label:
	$(SNARF) -DSCM_SNARF_LABEL $(DEFS) $(INCLUDES) $(CPPFLAGS) \
	$(CFLAGS) $< > $@ || { rm $@; false; }

.c.opcode:
	$(SNARF) -DSCM_SNARF_OPCODE $(DEFS) $(INCLUDES) $(CPPFLAGS) \
	$(CFLAGS) $< > $@ || { rm $@; false; }

$(BUILT_SOURCES): config.h vm-snarf.h

guile-compile: guile-compile.in
	sed -e 's!\@bindir\@!$(bindir)!' -e 's!\@PACKAGE\@!$(PACKAGE)!' \
	  $< > $@

test: all
	$(bin_PROGRAMS) -s test.scm

debug-test: all
	$(bin_PROGRAMS) -s test.scm debug
