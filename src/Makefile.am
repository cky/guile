bin_SCRIPTS = guilec guile-disasm
%: %.in
	sed "s!@guile@!$(bindir)/guile!" $^ > $@
	@chmod 755 $@
