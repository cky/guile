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

# Build the compiler and VM support first to avoid stack overflows
# when building the rest.
SUBDIRS = . ice-9 srfi oop

# We're at the root of the module hierarchy.
modpath =

SOURCES =								\
  system/base/pmatch.scm system/base/syntax.scm				\
  system/base/compile.scm system/base/language.scm			\
									\
  system/vm/debug.scm system/vm/frame.scm system/vm/instruction.scm	\
  system/vm/objcode.scm system/vm/profile.scm system/vm/program.scm	\
  system/vm/trace.scm system/vm/vm.scm					\
									\
  system/repl/repl.scm system/repl/common.scm				\
  system/repl/command.scm						\
									\
  language/ghil.scm language/glil.scm language/assembly.scm		\
									\
  $(SCHEME_LANG_SOURCES) $(ECMASCRIPT_LANG_SOURCES)			\
  $(GHIL_LANG_SOURCES) $(GLIL_LANG_SOURCES)				\
  $(ASSEMBLY_LANG_SOURCES) $(BYTECODE_LANG_SOURCES)			\
  $(OBJCODE_LANG_SOURCES) $(VALUE_LANG_SOURCES)

SCHEME_LANG_SOURCES =						\
  language/scheme/amatch.scm language/scheme/expand.scm 	\
  language/scheme/compile-ghil.scm language/scheme/spec.scm	\
  language/scheme/inline.scm

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
  language/ecmascript/spec.scm			\
  language/ecmascript/impl.scm			\
  language/ecmascript/base.scm			\
  language/ecmascript/function.scm		\
  language/ecmascript/array.scm			\
  language/ecmascript/compile-ghil.scm

NOCOMP_SOURCES =				\
  system/repl/describe.scm

include $(top_srcdir)/am/guilec
