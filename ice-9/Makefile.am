## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

# These should be installed and distributed.
ice9_sources = boot-9.scm calling.scm common-list.scm debug.scm \
emacs.scm expect.scm getopt-gnu-style.scm hcons.scm lineio.scm ls.scm \
mapping.scm poe.scm q.scm readline.scm regex.scm runq.scm slib.scm \
string-fun.scm tags.scm threads.scm r4rs.scm session.scm

# These should be installed, but not distributed.
ice9_generated = version.scm

subpkgdatadir = $(pkgdatadir)/$(VERSION)/ice-9
subpkgdata_DATA = $(ice9_sources) $(ice9_generated)
ETAGS_ARGS = $(subpkgdata_DATA)

## test.scm is not currently installed.
EXTRA_DIST = $(ice9_sources) test.scm version.scm.in
