## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

subpkgdatadir = $(pkgdatadir)/$(VERSION)/@module@
subpkgdata_DATA = boot-9.scm debug.scm expect.scm hcons.scm lineio.scm \
mapping.scm poe.scm slib.scm tags.scm threads.scm r4rs.scm

## test.scm is not currently installed.
EXTRA_DIST = PLUGIN/REQ $(subpkgdata_DATA) test.scm
