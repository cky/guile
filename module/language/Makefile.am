SUBDIRS=scheme ghil glil assembly bytecode objcode value
SOURCES=ghil.scm glil.scm assembly.scm \
	ecmascript/parse-lalr.scm \
	ecmascript/tokenize.scm \
	ecmascript/spec.scm \
	ecmascript/impl.scm \
	ecmascript/base.scm \
	ecmascript/function.scm \
	ecmascript/array.scm \
	ecmascript/compile-ghil.scm
# unfortunately, the one that we want to compile can't yet be compiled
# -- too many external vars, or something. seems like we need to do
# closure elimination or something.
NOCOMP_SOURCES = ecmascript/parse.scm
modpath = language
include $(top_srcdir)/am/guilec
