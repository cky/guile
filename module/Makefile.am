dist-hook:
	mkdir $(distdir)/module
	$(TAR) cf - --exclude=CVS $(srcdir)/language $(srcdir)/system \
	| (cd $(distdir)/module; $(TAR) xf -)
