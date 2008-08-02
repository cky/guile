bin_PROGRAMS = guile-vm
bin_SCRIPTS = guilec guile-disasm
guile_vm_SOURCES = guile-vm.c
guile_vm_LDADD = libguile-vm.la 
guile_vm_LDFLAGS = $(GUILE_LDFLAGS)

AM_CFLAGS = -Wall -g

lib_LTLIBRARIES = libguile-vm.la
libguile_vm_la_SOURCES =						\
	envs.c frames.c instructions.c objcodes.c programs.c vm.c	\
	envs.h frames.h instructions.h objcodes.h programs.h vm.h	\
	vm_engine.h vm_expand.h
libguile_vm_la_LDFLAGS = -version-info 0:0:0 -export-dynamic
EXTRA_DIST = vm_engine.c vm_system.c vm_scheme.c vm_loader.c \
             guilec.in guile-disasm.in
BUILT_SOURCES = vm_system.i vm_scheme.i vm_loader.i \
	envs.x frames.x instructions.x objcodes.x programs.x vm.x

INCLUDES = $(GUILE_CFLAGS)
CLEANFILES = guilec guile-disasm
DISTCLEANFILES = $(BUILT_SOURCES)
MAINTAINERCLEANFILES = Makefile.in config.h.in stamp-h.in

ETAGS_ARGS = --regex='/SCM_\(SYMBOL\|VCELL\).*\"\([^\"]\)*\"/\3/' \
   --regex='/SCM_DEFINE[ \t]*(\([^,]*\),[^,]*/\1/'

SNARF = guile-snarf
SUFFIXES = .i .x

.c.i:
	grep '^VM_DEFINE' $< > $@

.c.x:
	$(SNARF) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }


# Extra rules for debugging purposes.

%.I: %.c
	$(CPP) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $< > $@

%.s: %.c
	$(CC) -S -dA $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CFLAGS) $(CPPFLAGS) -o $@ $<


%: %.in
	sed "s!@guile@!$(GUILE)!" $^ > $@
	@chmod 755 $@

$(BUILT_SOURCES): config.h vm_expand.h
