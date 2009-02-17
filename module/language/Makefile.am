SUBDIRS=scheme ghil glil assembly bytecode objcode value
SOURCES=ghil.scm glil.scm assembly.scm \
	ecmascript/parse-lalr.scm \
	ecmascript/tokenize.scm
# unfortunately, the one that we want to compile can't yet be compiled
# -- too many local vars, or something.
NOCOMP_SOURCES = ecmascript/parse.scm
modpath = language
include $(top_srcdir)/am/guilec
