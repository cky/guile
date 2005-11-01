SOURCES = assemble.scm conv.scm core.scm debug.scm \
	  disasm.scm frame.scm profile.scm trace.scm
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/system/vm
vm_DATA = $(SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<
