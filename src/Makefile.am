bin_PROGRAMS = guile-vm
bin_SCRIPTS = guilec
guile_vm_SOURCES = guile-vm.c
guile_vm_LDADD = libguilevm.la 
guile_vm_LDFLAGS = $(GUILE_LDFLAGS)

lib_LTLIBRARIES = libguilevm.la
libguilevm_la_SOURCES = envs.c instructions.c programs.c vm.c	\
	envs.h instructions.h programs.h vm.h vm_engine.h vm_expand.h
libguilevm_la_LDFLAGS = -version-info 0:0:0 -export-dynamic
EXTRA_DIST = vm_engine.c vm_system.c vm_scheme.c vm_loader.c
BUILT_SOURCES = vm_system.i vm_scheme.i vm_loader.i \
	envs.x instructions.x programs.x vm.x

INCLUDES = $(GUILE_CFLAGS)
DISTCLEANFILES = $(BUILT_SOURCES)
MAINTAINERCLEANFILES = Makefile.in config.h.in stamp-h.in

ETAGS_ARGS = --regex='/SCM_\(SYMBOL\|VCELL\).*\"\([^\"]\)*\"/\3/' \
   --regex='/SCM_DEFINE[ \t]*(\([^,]*\),[^,]*/\1/'

SNARF = guile-snarf
SUFFIXES = .i .x
.c.i:
	grep '^VM_DEFINE' $< > $@
.c.x:
	$(SNARF) $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

$(BUILT_SOURCES): config.h vm_expand.h
