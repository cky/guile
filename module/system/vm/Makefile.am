SOURCES = assemble.scm conv.scm core.scm disasm.scm profile.scm trace.scm
OBJECTS = assemble.go  conv.go  core.go  disasm.go  profile.go  trace.go

vmdir = $(guiledir)/system/vm
vm_DATA = $(SOURCES) $(OBJECTS)

DISTCLEANFILES = $(OBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
.scm.go:
	$(GUILEC) $<
