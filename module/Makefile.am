guiledatadir = $(datadir)/guile

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(guiledatadir)
	cp -rp module/* $(DESTDIR)$(guiledatadir)
	rm -f $(guiledatadir)/system/vm/libcore.so \
	&& $(LN_S) $(libdir)/libguilevm.so $(guiledatadir)/system/vm/libcore.so

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(guiledatadir)
	cp -rp module/* $(DESTDIR)$(guiledatadir)
	rm -f $(guiledatadir)/system/vm/libcore.so \
	&& $(LN_S) $(libdir)/libguilevm.so $(guiledatadir)/system/vm/libcore.so
