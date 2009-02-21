SUBDIRS=scheme ghil glil assembly bytecode objcode value
SOURCES=ghil.scm glil.scm assembly.scm \
	ecmascript/parse-lalr.scm \
	ecmascript/tokenize.scm \
	ecmascript/spec.scm \
	ecmascript/impl.scm \
	ecmascript/base.scm \
	ecmascript/function.scm \
	ecmascript/array.scm
# unfortunately, the ones that we want to compile can't yet be compiled
# -- too many local vars in the first case, and some non-tail-recursion
# in pmatch in the second.
NOCOMP_SOURCES = ecmascript/parse.scm \
	ecmascript/compile-ghil.scm
modpath = language
include $(top_srcdir)/am/guilec
