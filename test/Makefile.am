SOURCE_FILES = control.scm procedure.scm queens.scm
COMPILED_FILES = control.scc procedure.scc queens.scc
EXTRA_DIST = test.scm $(SOURCE_FILES)
CLEANFILES = $(COMPILED_FILES)
MAINTAINERCLEANFILES = Makefile.in

GUILE = $(top_srcdir)/src/$(PACKAGE)

test: $(COMPILED_FILES)
	@for file in $(COMPILED_FILES); do \
	  $(GUILE) -s test.scm $$file; \
	done

SUFFIXES = .scm .scc
.scm.scc:
	guile-compile $<
