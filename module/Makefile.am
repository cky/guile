## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2009, 2010, 2011, 2012, 2013,
##        2014 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

include $(top_srcdir)/am/guilec

# We're at the root of the module hierarchy.
modpath =

# Build eval.go first.
$(GOBJECTS): ice-9/eval.go
CLEANFILES += ice-9/eval.go
nobase_mod_DATA += ice-9/eval.scm
nobase_ccache_DATA += ice-9/eval.go
EXTRA_DIST += ice-9/eval.scm
ETAGS_ARGS += ice-9/eval.scm

ice-9/boot-9.go: ice-9/boot-9.scm ice-9/quasisyntax.scm ice-9/r6rs-libraries.scm
ice-9/match.go: ice-9/match.scm ice-9/match.upstream.scm

# We can compile these in any order, but it's fastest if we compile
# psyntax and boot-9 first, then the compiler itself, then the rest of
# the code.
SOURCES =					\
  ice-9/psyntax-pp.scm				\
  ice-9/boot-9.scm				\
  ice-9/vlist.scm                               \
  srfi/srfi-1.scm                               \
  language/tree-il/peval.scm                    \
  language/tree-il/cse.scm                      \
						\
  language/tree-il.scm				\
  language/glil.scm				\
  language/assembly.scm				\
  $(TREE_IL_LANG_SOURCES)			\
  $(GLIL_LANG_SOURCES)				\
  $(ASSEMBLY_LANG_SOURCES)			\
  $(BYTECODE_LANG_SOURCES)			\
  $(OBJCODE_LANG_SOURCES)			\
  $(VALUE_LANG_SOURCES)				\
  $(SCHEME_LANG_SOURCES)			\
  $(SYSTEM_BASE_SOURCES)			\
						\
  $(ICE_9_SOURCES)				\
  $(SRFI_SOURCES)				\
  $(RNRS_SOURCES)				\
  $(OOP_SOURCES)				\
  $(SYSTEM_SOURCES)				\
  $(SCRIPTS_SOURCES)				\
  $(ECMASCRIPT_LANG_SOURCES)			\
  $(ELISP_LANG_SOURCES)				\
  $(BRAINFUCK_LANG_SOURCES)			\
  $(LIB_SOURCES)				\
  $(WEB_SOURCES)

## test.scm is not currently installed.
EXTRA_DIST +=					\
  ice-9/test.scm				\
  ice-9/compile-psyntax.scm			\
  ice-9/ChangeLog-2008
ETAGS_ARGS +=					\
  ice-9/test.scm				\
  ice-9/compile-psyntax.scm			\
  ice-9/ChangeLog-2008

ice-9/psyntax-pp.scm.gen:
	GUILE_INSTALL_LOCALE=1 GUILE_AUTO_COMPILE=0 \
	$(top_builddir_absolute)/meta/guile -s $(srcdir)/ice-9/compile-psyntax.scm \
		$(srcdir)/ice-9/psyntax.scm $(srcdir)/ice-9/psyntax-pp.scm

.PHONY: ice-9/psyntax-pp.scm.gen

# Keep this rule in sync with that in `am/guilec'.
ice-9/psyntax-pp.go: ice-9/psyntax.scm ice-9/psyntax-pp.scm
	$(AM_V_GUILEC)GUILE_INSTALL_LOCALE=1 GUILE_AUTO_COMPILE=0	\
	$(top_builddir)/meta/uninstalled-env				\
	guild compile --target="$(host)" $(GUILE_WARNINGS)		\
	  -L "$(abs_srcdir)" -L "$(abs_builddir)"			\
	  -L "$(abs_top_srcdir)/guile-readline"				\
	  -o "ice-9/psyntax-pp.go" "$(srcdir)/ice-9/psyntax.scm"

SCHEME_LANG_SOURCES =						\
  language/scheme/spec.scm					\
  language/scheme/compile-tree-il.scm				\
  language/scheme/decompile-tree-il.scm

TREE_IL_LANG_SOURCES =						\
  language/tree-il/primitives.scm				\
  language/tree-il/effects.scm                                 	\
  language/tree-il/fix-letrec.scm                               \
  language/tree-il/optimize.scm                                 \
  language/tree-il/canonicalize.scm                             \
  language/tree-il/analyze.scm					\
  language/tree-il/inline.scm 					\
  language/tree-il/compile-glil.scm 				\
  language/tree-il/debug.scm 					\
  language/tree-il/spec.scm

GLIL_LANG_SOURCES =						\
  language/glil/spec.scm language/glil/compile-assembly.scm

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
  language/ecmascript/tokenize.scm		\
  language/ecmascript/parse.scm			\
  language/ecmascript/impl.scm			\
  language/ecmascript/base.scm			\
  language/ecmascript/function.scm		\
  language/ecmascript/array.scm			\
  language/ecmascript/compile-tree-il.scm	\
  language/ecmascript/spec.scm

ELISP_LANG_SOURCES =				\
  language/elisp/lexer.scm			\
  language/elisp/parser.scm			\
  language/elisp/bindings.scm			\
  language/elisp/compile-tree-il.scm		\
  language/elisp/runtime.scm			\
  language/elisp/runtime/function-slot.scm	\
  language/elisp/runtime/value-slot.scm		\
  language/elisp/runtime/macros.scm		\
  language/elisp/runtime/subrs.scm		\
  language/elisp/spec.scm

BRAINFUCK_LANG_SOURCES =			\
  language/brainfuck/parse.scm			\
  language/brainfuck/compile-scheme.scm		\
  language/brainfuck/compile-tree-il.scm	\
  language/brainfuck/spec.scm

SCRIPTS_SOURCES =				\
  scripts/compile.scm				\
  scripts/disassemble.scm			\
  scripts/display-commentary.scm		\
  scripts/doc-snarf.scm				\
  scripts/frisk.scm				\
  scripts/generate-autoload.scm			\
  scripts/help.scm				\
  scripts/lint.scm				\
  scripts/list.scm				\
  scripts/punify.scm				\
  scripts/read-scheme-source.scm		\
  scripts/read-text-outline.scm			\
  scripts/use2dot.scm				\
  scripts/snarf-check-and-output-texi.scm	\
  scripts/summarize-guile-TODO.scm		\
  scripts/api-diff.scm				\
  scripts/read-rfc822.scm			\
  scripts/snarf-guile-m4-docs.scm

SYSTEM_BASE_SOURCES =				\
  system/base/pmatch.scm			\
  system/base/syntax.scm			\
  system/base/compile.scm			\
  system/base/language.scm			\
  system/base/lalr.scm				\
  system/base/message.scm			\
  system/base/target.scm			\
  system/base/ck.scm

ICE_9_SOURCES = \
  ice-9/r4rs.scm \
  ice-9/r5rs.scm \
  ice-9/deprecated.scm \
  ice-9/and-let-star.scm \
  ice-9/binary-ports.scm \
  ice-9/calling.scm \
  ice-9/command-line.scm \
  ice-9/common-list.scm \
  ice-9/control.scm \
  ice-9/curried-definitions.scm \
  ice-9/debug.scm \
  ice-9/documentation.scm \
  ice-9/eval-string.scm \
  ice-9/expect.scm \
  ice-9/format.scm \
  ice-9/futures.scm \
  ice-9/getopt-long.scm \
  ice-9/hash-table.scm \
  ice-9/hcons.scm \
  ice-9/i18n.scm \
  ice-9/iconv.scm \
  ice-9/lineio.scm \
  ice-9/ls.scm \
  ice-9/mapping.scm \
  ice-9/match.scm \
  ice-9/networking.scm \
  ice-9/null.scm \
  ice-9/occam-channel.scm \
  ice-9/optargs.scm \
  ice-9/poe.scm \
  ice-9/poll.scm \
  ice-9/posix.scm \
  ice-9/q.scm \
  ice-9/rdelim.scm \
  ice-9/receive.scm \
  ice-9/regex.scm \
  ice-9/runq.scm \
  ice-9/rw.scm \
  ice-9/safe-r5rs.scm \
  ice-9/safe.scm \
  ice-9/save-stack.scm \
  ice-9/scm-style-repl.scm \
  ice-9/session.scm \
  ice-9/slib.scm \
  ice-9/stack-catch.scm \
  ice-9/streams.scm \
  ice-9/string-fun.scm \
  ice-9/syncase.scm \
  ice-9/threads.scm \
  ice-9/top-repl.scm \
  ice-9/buffered-input.scm \
  ice-9/time.scm \
  ice-9/history.scm \
  ice-9/channel.scm \
  ice-9/pretty-print.scm \
  ice-9/ftw.scm \
  ice-9/gap-buffer.scm \
  ice-9/weak-vector.scm \
  ice-9/list.scm \
  ice-9/serialize.scm \
  ice-9/local-eval.scm

if BUILD_ICE_9_POPEN

# This functionality is missing on systems without `fork'---i.e., Windows.
ICE_9_SOURCES += ice-9/popen.scm

# These modules rely on (ice-9 popen).
SCRIPTS_SOURCES +=				\
  scripts/autofrisk.scm				\
  scripts/scan-api.scm

endif BUILD_ICE_9_POPEN

srfi/srfi-64.go: srfi/srfi-64.scm srfi/srfi-64/testing.scm

SRFI_SOURCES = \
  srfi/srfi-2.scm \
  srfi/srfi-4.scm \
  srfi/srfi-4/gnu.scm \
  srfi/srfi-6.scm \
  srfi/srfi-8.scm \
  srfi/srfi-9.scm \
  srfi/srfi-9/gnu.scm \
  srfi/srfi-10.scm \
  srfi/srfi-11.scm \
  srfi/srfi-13.scm \
  srfi/srfi-14.scm \
  srfi/srfi-16.scm \
  srfi/srfi-17.scm \
  srfi/srfi-18.scm \
  srfi/srfi-19.scm \
  srfi/srfi-26.scm \
  srfi/srfi-27.scm \
  srfi/srfi-31.scm \
  srfi/srfi-34.scm \
  srfi/srfi-35.scm \
  srfi/srfi-37.scm \
  srfi/srfi-38.scm \
  srfi/srfi-41.scm \
  srfi/srfi-42.scm \
  srfi/srfi-43.scm \
  srfi/srfi-39.scm \
  srfi/srfi-45.scm \
  srfi/srfi-60.scm \
  srfi/srfi-64.scm \
  srfi/srfi-67.scm \
  srfi/srfi-69.scm \
  srfi/srfi-88.scm \
  srfi/srfi-98.scm \
  srfi/srfi-111.scm

RNRS_SOURCES =					\
  rnrs/base.scm					\
  rnrs/conditions.scm				\
  rnrs/control.scm				\
  rnrs/enums.scm				\
  rnrs/eval.scm					\
  rnrs/exceptions.scm				\
  rnrs/files.scm				\
  rnrs/hashtables.scm				\
  rnrs/lists.scm				\
  rnrs/mutable-pairs.scm			\
  rnrs/mutable-strings.scm			\
  rnrs/programs.scm				\
  rnrs/r5rs.scm					\
  rnrs/sorting.scm				\
  rnrs/syntax-case.scm				\
  rnrs/unicode.scm				\
  rnrs/arithmetic/bitwise.scm			\
  rnrs/arithmetic/fixnums.scm			\
  rnrs/arithmetic/flonums.scm			\
  rnrs/bytevectors.scm				\
  rnrs/io/simple.scm				\
  rnrs/io/ports.scm				\
  rnrs/records/inspection.scm			\
  rnrs/records/procedural.scm			\
  rnrs/records/syntactic.scm			\
  rnrs.scm

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

SYSTEM_SOURCES =				\
  system/vm/inspect.scm				\
  system/vm/coverage.scm			\
  system/vm/frame.scm				\
  system/vm/instruction.scm			\
  system/vm/objcode.scm				\
  system/vm/program.scm				\
  system/vm/trace.scm				\
  system/vm/traps.scm				\
  system/vm/trap-state.scm			\
  system/vm/vm.scm				\
  system/foreign.scm				\
  system/xref.scm				\
  system/repl/debug.scm				\
  system/repl/error-handling.scm		\
  system/repl/common.scm			\
  system/repl/command.scm			\
  system/repl/repl.scm				\
  system/repl/server.scm			\
  system/repl/coop-server.scm

LIB_SOURCES =					\
  statprof.scm					\
  sxml/apply-templates.scm			\
  sxml/fold.scm					\
  sxml/match.scm				\
  sxml/simple.scm				\
  sxml/ssax/input-parse.scm			\
  sxml/ssax.scm					\
  sxml/transform.scm				\
  sxml/xpath.scm				\
  texinfo.scm					\
  texinfo/docbook.scm				\
  texinfo/html.scm				\
  texinfo/indexing.scm				\
  texinfo/string-utils.scm			\
  texinfo/plain-text.scm			\
  texinfo/reflection.scm			\
  texinfo/serialize.scm

WEB_SOURCES =					\
  web/client.scm				\
  web/http.scm					\
  web/request.scm				\
  web/response.scm				\
  web/server.scm				\
  web/server/http.scm				\
  web/uri.scm

EXTRA_DIST += oop/ChangeLog-2008

NOCOMP_SOURCES =				\
  ice-9/match.upstream.scm			\
  ice-9/psyntax.scm				\
  ice-9/r6rs-libraries.scm			\
  ice-9/quasisyntax.scm				\
  srfi/srfi-42/ec.scm				\
  srfi/srfi-64/testing.scm			\
  srfi/srfi-67/compare.scm			\
  system/base/lalr.upstream.scm			\
  system/repl/describe.scm			\
  sxml/sxml-match.ss				\
  sxml/upstream/SSAX.scm			\
  sxml/upstream/SXML-tree-trans.scm		\
  sxml/upstream/SXPath-old.scm			\
  sxml/upstream/assert.scm			\
  sxml/upstream/input-parse.scm
