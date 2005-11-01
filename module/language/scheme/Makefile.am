SOURCES =
## FIXME: There's a bug showing up when compiling `translate.scm'.
##
## `spec.scm' cannot be compiled because it uses the `define-language'
## macro which introduces an unregular object, namely the first-class
## `<language>' procedure.
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/language/scheme
vm_DATA = $(SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<
