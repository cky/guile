SOURCES = syntax.scm language.scm compile.scm
OBJECTS = syntax.go  language.go  compile.go

vmdir = $(guiledir)/system/vm
vm_DATA = $(SOURCES) $(OBJECTS)

DISTCLEANFILES = $(OBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
.scm.go:
	$(GUILEC) $<
