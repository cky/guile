vmdatadir = $(datadir)/guile/vm
vmdata_DATA = utils.scm types.scm bytecomp.scm compile.scm shell.scm
noinst_DATA = libvm.so

EXTRA_DIST = $(vmdata_DATA)
CLEANFILES = $(noinst_DATA)
MAINTAINERCLEANFILES = Makefile.in

libvm.so:
	$(LN_S) -f ../src/.libs/libguilevm.so ./libvm.so

install-data-local:
	rm -f $(vmdatadir)/libvm.so \
	&& $(LN_S) $(libdir)/libguilevm.so $(vmdatadir)/libvm.so
