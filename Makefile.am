## Process this file with automake to produce Makefile.in.

SUBDIRS = ice-9 qt libguile build

## FIXME: in the future there will be direct automake support for
## doing this.  When that happens, switch over.
aclocaldir = $(datadir)/aclocal
aclocal_DATA = guile.m4 threads.m4

EXTRA_DIST = $(aclocal_DATA) ltconfig ltmain.sh acconfig.h \
	HACKING GUILE-VERSION
