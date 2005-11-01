SOURCES = glil.scm macros.scm
## FIXME: There's a bug showing up when compiling `ghil.scm' and
## `compile.scm'!
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/system/il
vm_DATA = $(SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<
