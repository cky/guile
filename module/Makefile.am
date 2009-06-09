## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2009 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 2, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   You should have received a copy of the GNU General Public
##   License along with GUILE; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
##   Floor, Boston, MA 02110-1301 USA

include $(top_srcdir)/am/guilec

# We're at the root of the module hierarchy.
modpath =

# Compile psyntax and boot-9 first, so that we get the speed benefit in
# the rest of the compilation. Also, if there is too much switching back
# and forth between interpreted and compiled code, we end up using more
# of the C stack than the interpreter would have; so avoid that by
# putting these core modules first.

SOURCES =								\
  ice-9/psyntax-pp.scm 							\
  system/base/pmatch.scm system/base/syntax.scm				\
  system/base/compile.scm system/base/language.scm			\
									\
  language/tree-il.scm							\
  language/ghil.scm language/glil.scm language/assembly.scm		\
									\
  $(SCHEME_LANG_SOURCES) 						\
  $(TREE_IL_LANG_SOURCES)						\
  $(GHIL_LANG_SOURCES) $(GLIL_LANG_SOURCES)				\
  $(ASSEMBLY_LANG_SOURCES) $(BYTECODE_LANG_SOURCES)			\
  $(OBJCODE_LANG_SOURCES) $(VALUE_LANG_SOURCES)				\
									\
  $(ICE_9_SOURCES)							\
  $(SRFI_SOURCES)							\
  $(RNRS_SOURCES)							\
  $(OOP_SOURCES)							\
  $(SYSTEM_SOURCES)                                                     \
  $(ECMASCRIPT_LANG_SOURCES)						\
  $(SCRIPTS_SOURCES)

## test.scm is not currently installed.
EXTRA_DIST += ice-9/test.scm ice-9/compile-psyntax.scm ice-9/ChangeLog-2008

# We expect this to never be invoked when there is not already
# ice-9/psyntax-pp.scm in %load-path, since compile-psyntax.scm depends
# on ice-9/syncase.scm, which does `(load-from-path "ice-9/psyntax-pp.scm")'.
# In other words, to bootstrap this file, you need to do something like:
#    GUILE_LOAD_PATH=/usr/local/share/guile/1.5.4 make psyntax-pp.scm
include $(top_srcdir)/am/pre-inst-guile
ice-9/psyntax-pp.scm: ice-9/psyntax.scm
	$(preinstguile) --no-autocompile -s $(srcdir)/ice-9/compile-psyntax.scm \
		$(srcdir)/ice-9/psyntax.scm $(srcdir)/ice-9/psyntax-pp.scm

SCHEME_LANG_SOURCES =						\
  language/scheme/compile-ghil.scm 				\
  language/scheme/spec.scm					\
  language/scheme/compile-tree-il.scm				\
  language/scheme/decompile-tree-il.scm				\
  language/scheme/inline.scm

TREE_IL_LANG_SOURCES =						\
  language/tree-il/primitives.scm				\
  language/tree-il/optimize.scm                                 \
  language/tree-il/analyze.scm					\
  language/tree-il/compile-glil.scm 				\
  language/tree-il/spec.scm

GHIL_LANG_SOURCES =					\
  language/ghil/spec.scm language/ghil/compile-glil.scm

GLIL_LANG_SOURCES =						\
  language/glil/spec.scm language/glil/compile-assembly.scm 	\
  language/glil/decompile-assembly.scm

ASSEMBLY_LANG_SOURCES =				\
  language/assembly/spec.scm			\
  language/assembly/compile-bytecode.scm	\
  language/assembly/decompile-bytecode.scm	\
  language/assembly/disassemble.scm

BYTECODE_LANG_SOURCES =				\
  language/bytecode/spec.scm

OBJCODE_LANG_SOURCES =				\
  language/objcode/spec.scm

VALUE_LANG_SOURCES =				\
  language/value/spec.scm

ECMASCRIPT_LANG_SOURCES =			\
  language/ecmascript/parse-lalr.scm		\
  language/ecmascript/tokenize.scm		\
  language/ecmascript/parse.scm			\
  language/ecmascript/impl.scm			\
  language/ecmascript/base.scm			\
  language/ecmascript/function.scm		\
  language/ecmascript/array.scm			\
  language/ecmascript/compile-ghil.scm		\
  language/ecmascript/spec.scm

SCRIPTS_SOURCES =				\
  scripts/PROGRAM.scm				\
  scripts/autofrisk.scm				\
  scripts/compile.scm				\
  scripts/disassemble.scm			\
  scripts/display-commentary.scm		\
  scripts/doc-snarf.scm				\
  scripts/frisk.scm				\
  scripts/generate-autoload.scm			\
  scripts/lint.scm				\
  scripts/punify.scm				\
  scripts/read-scheme-source.scm		\
  scripts/read-text-outline.scm			\
  scripts/use2dot.scm				\
  scripts/snarf-check-and-output-texi.scm	\
  scripts/summarize-guile-TODO.scm		\
  scripts/scan-api.scm				\
  scripts/api-diff.scm				\
  scripts/read-rfc822.scm			\
  scripts/snarf-guile-m4-docs.scm

ICE_9_SOURCES = \
  ice-9/boot-9.scm \
  ice-9/r4rs.scm \
  ice-9/r5rs.scm \
  ice-9/and-let-star.scm \
  ice-9/calling.scm \
  ice-9/common-list.scm \
  ice-9/debug.scm \
  ice-9/debugger.scm \
  ice-9/documentation.scm \
  ice-9/emacs.scm \
  ice-9/expect.scm \
  ice-9/format.scm \
  ice-9/getopt-long.scm \
  ice-9/hcons.scm \
  ice-9/i18n.scm \
  ice-9/lineio.scm \
  ice-9/ls.scm \
  ice-9/mapping.scm \
  ice-9/match.scm \
  ice-9/networking.scm \
  ice-9/null.scm \
  ice-9/occam-channel.scm \
  ice-9/optargs.scm \
  ice-9/poe.scm \
  ice-9/popen.scm \
  ice-9/posix.scm \
  ice-9/q.scm \
  ice-9/rdelim.scm \
  ice-9/receive.scm \
  ice-9/regex.scm \
  ice-9/runq.scm \
  ice-9/rw.scm \
  ice-9/safe-r5rs.scm \
  ice-9/safe.scm \
  ice-9/session.scm \
  ice-9/slib.scm \
  ice-9/stack-catch.scm \
  ice-9/streams.scm \
  ice-9/string-fun.scm \
  ice-9/syncase.scm \
  ice-9/threads.scm \
  ice-9/buffered-input.scm \
  ice-9/time.scm \
  ice-9/history.scm \
  ice-9/channel.scm \
  ice-9/pretty-print.scm \
  ice-9/ftw.scm \
  ice-9/gap-buffer.scm \
  ice-9/weak-vector.scm \
  ice-9/deprecated.scm \
  ice-9/list.scm \
  ice-9/serialize.scm \
  ice-9/gds-server.scm

SRFI_SOURCES = \
  srfi/srfi-1.scm \
  srfi/srfi-2.scm \
  srfi/srfi-4.scm \
  srfi/srfi-6.scm \
  srfi/srfi-8.scm \
  srfi/srfi-9.scm \
  srfi/srfi-10.scm \
  srfi/srfi-11.scm \
  srfi/srfi-13.scm \
  srfi/srfi-14.scm \
  srfi/srfi-16.scm \
  srfi/srfi-17.scm \
  srfi/srfi-18.scm \
  srfi/srfi-19.scm \
  srfi/srfi-26.scm \
  srfi/srfi-31.scm \
  srfi/srfi-34.scm \
  srfi/srfi-35.scm \
  srfi/srfi-37.scm \
  srfi/srfi-39.scm \
  srfi/srfi-60.scm \
  srfi/srfi-69.scm \
  srfi/srfi-88.scm \
  srfi/srfi-98.scm

RNRS_SOURCES =					\
  rnrs/bytevector.scm				\
  rnrs/io/ports.scm

EXTRA_DIST += scripts/ChangeLog-2008
EXTRA_DIST += scripts/README

OOP_SOURCES = \
  oop/goops.scm \
  oop/goops/active-slot.scm \
  oop/goops/compile.scm \
  oop/goops/composite-slot.scm \
  oop/goops/describe.scm \
  oop/goops/dispatch.scm \
  oop/goops/internal.scm \
  oop/goops/save.scm \
  oop/goops/stklos.scm \
  oop/goops/util.scm \
  oop/goops/accessors.scm \
  oop/goops/simple.scm

SYSTEM_SOURCES = \
  system/vm/debug.scm system/vm/frame.scm system/vm/instruction.scm	\
  system/vm/objcode.scm system/vm/profile.scm system/vm/program.scm	\
  system/vm/trace.scm system/vm/vm.scm					\
									\
  system/xref.scm							\
									\
  system/repl/repl.scm system/repl/common.scm				\
  system/repl/command.scm

EXTRA_DIST += oop/ChangeLog-2008

NOCOMP_SOURCES =				\
  ice-9/gds-client.scm \
  ice-9/psyntax.scm \
  system/repl/describe.scm \
  ice-9/debugger/command-loop.scm \
  ice-9/debugger/commands.scm \
  ice-9/debugger/state.scm \
  ice-9/debugger/trc.scm \
  ice-9/debugger/utils.scm \
  ice-9/debugging/example-fns.scm \
  ice-9/debugging/ice-9-debugger-extensions.scm \
  ice-9/debugging/steps.scm \
  ice-9/debugging/trace.scm \
  ice-9/debugging/traps.scm \
  ice-9/debugging/trc.scm
