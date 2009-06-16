/* Copyright (C) 2001 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

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

#undef VM_DEFINE_FUNCTION
#undef VM_DEFINE_LOADER
#define VM_DEFINE_FUNCTION(code,tag,name,nargs) \
  VM_DEFINE_INSTRUCTION(code,tag,name,0,nargs,1)
#define VM_DEFINE_LOADER(code,tag,name)         \
  VM_DEFINE_INSTRUCTION(code,tag,name,-1,0,1)

#undef VM_DEFINE_INSTRUCTION
/*
 * These will go to scm_instruction_table in instructions.c
 */
#ifdef VM_INSTRUCTION_TO_TABLE
#define VM_DEFINE_INSTRUCTION(code_,tag_,name_,len_,npop_,npush_) \
  table[VM_OPCODE (tag_)].opcode = code_;                          \
  table[VM_OPCODE (tag_)].name = name_;                            \
  table[VM_OPCODE (tag_)].len = len_;                              \
  table[VM_OPCODE (tag_)].npop = npop_;                            \
  table[VM_OPCODE (tag_)].npush = npush_;

#else
#ifdef VM_INSTRUCTION_TO_LABEL
/*
 * These will go to jump_table in vm_engine.c
 */
#define VM_DEFINE_INSTRUCTION(code,tag,name,len,npop,npush)	jump_table[code] = VM_ADDR (tag);

#else
#ifdef VM_INSTRUCTION_TO_OPCODE
/*
 * These will go to scm_opcode in instructions.h
 */
#define VM_DEFINE_INSTRUCTION(code,tag,name,len,npop,npush)	VM_OPCODE (tag) = code,

#else /* Otherwise */
/*
 * These are directly included in vm_engine.c
 */
#define VM_DEFINE_INSTRUCTION(code,tag,name,len,npop,npush)	VM_TAG (tag)

#endif /* VM_INSTRUCTION_TO_OPCODE */
#endif /* VM_INSTRUCTION_TO_LABEL */
#endif /* VM_INSTRUCTION_TO_TABLE */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
