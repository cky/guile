SOURCES = repl.scm common.scm command.scm
OBJECTS = repl.go  common.go  command.go

vmdir = $(guiledir)/system/repl
vm_DATA = $(SOURCES) $(OBJECTS)

DISTCLEANFILES = $(OBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
.scm.go:
	$(GUILEC) $<
