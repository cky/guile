info_TEXINFOS = data-rep.texi
data_rep_TEXINFOS = data-rep.texi version.texi

EXAMPLE_SMOB_FILES = \
    ChangeLog Makefile README image-type.c image-type.h myguile.c

dist-hook:
	mkdir $(distdir)/example-smob
	(dest="`cd $(distdir)/example-smob; pwd`"; \
         cd $(srcdir)/example-smob; \
	 cp $(EXAMPLE_SMOB_FILES) $$dest)
