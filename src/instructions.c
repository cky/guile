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

#include <string.h>
#include "instructions.h"

struct scm_instruction scm_instruction_table[] = {
#define VM_INSTRUCTION_TO_TABLE 1
#include "vm_expand.h"
#include "vm_system.i"
#include "vm_scheme.i"
#include "vm_loader.i"
#undef VM_INSTRUCTION_TO_TABLE
  {scm_op_last}
};

/* C interface */

struct scm_instruction *
scm_lookup_instruction (SCM name)
{
  struct scm_instruction *ip;
  if (SCM_SYMBOLP (name))
    for (ip = scm_instruction_table; ip->opcode != scm_op_last; ip++)
      if (strcmp (ip->name, SCM_SYMBOL_CHARS (name)) == 0)
	return ip;
  return 0;
}

/* Scheme interface */

SCM_DEFINE (scm_instruction_list, "instruction-list", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_instruction_list
{
  SCM list = SCM_EOL;
  struct scm_instruction *ip;
  for (ip = scm_instruction_table; ip->opcode != scm_op_last; ip++)
    list = scm_cons (scm_str2symbol (ip->name), list);
  return scm_reverse_x (list, SCM_EOL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_p, "instruction?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_instruction_p
{
  return SCM_BOOL (SCM_INSTRUCTION_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_length, "instruction-length", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_length
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  return SCM_MAKINUM (SCM_INSTRUCTION_LENGTH (inst));
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_pops, "instruction-pops", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_pops
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  return SCM_MAKINUM (SCM_INSTRUCTION_POPS (inst));
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_pushes, "instruction-pushes", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_pushes
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  return SCM_MAKINUM (SCM_INSTRUCTION_PUSHES (inst));
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_to_opcode, "instruction->opcode", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_to_opcode
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  return SCM_MAKINUM (SCM_INSTRUCTION_OPCODE (inst));
}
#undef FUNC_NAME

SCM_DEFINE (scm_opcode_to_instruction, "opcode->instruction", 1, 0, 0,
	    (SCM op),
	    "")
#define FUNC_NAME s_scm_opcode_to_instruction
{
  int i;
  SCM_VALIDATE_INUM (1, op);
  i = SCM_INUM (op);
  SCM_ASSERT_RANGE (1, op, 0 <= i && i < scm_op_last);
  return scm_str2symbol (scm_instruction_table[i].name);
}
#undef FUNC_NAME

void
scm_init_instructions (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "instructions.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
