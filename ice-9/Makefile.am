## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

# These should be installed and distributed.
ice9_sources = boot-9.scm debug.scm expect.scm hcons.scm lineio.scm \
mapping.scm poe.scm regex.scm slib.scm tags.scm threads.scm r4rs.scm \
session.scm syncase.scm psyntax.pp psyntax.ss

# These should be installed, but not distributed.
ice9_generated = version.scm

subpkgdatadir = $(pkgdatadir)/$(VERSION)/ice-9
subpkgdata_DATA = $(ice9_sources) $(ice9_generated)
ETAGS_ARGS = $(subpkgdata_DATA)

## test.scm is not currently installed.
EXTRA_DIST = $(ice9_sources) test.scm version.scm.in
