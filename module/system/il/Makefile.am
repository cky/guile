SOURCES = glil.scm ghil.scm macros.scm compile.scm
OBJECTS = glil.go  ghil.go  macros.go  compile.go

vmdir = $(guiledir)/system/il
vm_DATA = $(SOURCES) $(OBJECTS)

DISTCLEANFILES = $(OBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
.scm.go:
	$(GUILEC) $<
