/* Copyright (C) 2000 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

#include "config.h"

#ifndef VM_LABEL
#define VM_LABEL(tag) l_##tag
#define VM_OPCODE(tag) scm_op_##tag

#ifdef HAVE_LABELS_AS_VALUES
#define VM_TAG(tag) VM_LABEL(tag):
#define VM_ADDR(tag) &&VM_LABEL(tag)
#else /* not HAVE_LABELS_AS_VALUES */
#define VM_TAG(tag) case VM_OPCODE(tag):
#define VM_ADDR(tag) NULL
#endif /* not HAVE_LABELS_AS_VALUES */
#endif /* VM_LABEL */

#undef VM_DEFINE_INSTRUCTION
#undef VM_DEFINE_FUNCTION
#undef VM_DEFINE_LOADER
#ifdef VM_INSTRUCTION_TO_TABLE
/*
 * These will go to scm_instruction_table in vm.c
 */
#define VM_DEFINE_INSTRUCTION(tag,name,len,npop,npush) \
  {VM_OPCODE (tag), name, len, npop, npush},
#define VM_DEFINE_FUNCTION(tag,name,nargs) \
  {VM_OPCODE (tag), name, (nargs < 0) ? 1 : 0, nargs, 1},
#define VM_DEFINE_LOADER(tag,name) \
  {VM_OPCODE (tag), name, -1, 0, 1},

#else
#ifdef VM_INSTRUCTION_TO_LABEL
/*
 * These will go to jump_table in vm_engine.c
 */
#define VM_DEFINE_INSTRUCTION(tag,name,len,npop,npush)	VM_ADDR (tag),
#define VM_DEFINE_FUNCTION(tag,name,nargs)		VM_ADDR (tag),
#define VM_DEFINE_LOADER(tag,name)			VM_ADDR (tag),

#else
#ifdef VM_INSTRUCTION_TO_OPCODE
/*
 * These will go to scm_opcode in vm.h
 */
#define VM_DEFINE_INSTRUCTION(tag,name,len,npop,npush)	VM_OPCODE (tag),
#define VM_DEFINE_FUNCTION(tag,name,nargs)		VM_OPCODE (tag),
#define VM_DEFINE_LOADER(tag,name)			VM_OPCODE (tag),

#else /* Otherwise */
/*
 * These are directly included in vm_engine.c
 */
#define VM_DEFINE_INSTRUCTION(tag,name,len,npop,npush)	VM_TAG (tag)
#define VM_DEFINE_FUNCTION(tag,name,nargs)		VM_TAG (tag)
#define VM_DEFINE_LOADER(tag,name)			VM_TAG (tag)

#endif /* VM_INSTRUCTION_TO_OPCODE */
#endif /* VM_INSTRUCTION_TO_LABEL */
#endif /* VM_INSTRUCTION_TO_TABLE */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
