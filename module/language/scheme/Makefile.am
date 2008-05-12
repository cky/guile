SOURCES = translate.scm spec.scm
## FIXME: There's a bug showing up when compiling `translate.scm'.
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/language/scheme
vm_DATA = $(SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<
