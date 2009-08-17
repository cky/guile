/* Copyright (C) 2001, 2009 Free Software Foundation, Inc.
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

#ifndef _SCM_INSTRUCTIONS_H_
#define _SCM_INSTRUCTIONS_H_

#include <libguile.h>

#define SCM_VM_NUM_INSTRUCTIONS (1<<8)
#define SCM_VM_INSTRUCTION_MASK (SCM_VM_NUM_INSTRUCTIONS-1)

enum scm_opcode {
#define VM_INSTRUCTION_TO_OPCODE 1
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef VM_INSTRUCTION_TO_OPCODE
};

SCM_API SCM scm_instruction_list (void);
SCM_API SCM scm_instruction_p (SCM obj);
SCM_API SCM scm_instruction_length (SCM inst);
SCM_API SCM scm_instruction_pops (SCM inst);
SCM_API SCM scm_instruction_pushes (SCM inst);
SCM_API SCM scm_instruction_to_opcode (SCM inst);
SCM_API SCM scm_opcode_to_instruction (SCM op);

SCM_INTERNAL void scm_bootstrap_instructions (void);
SCM_INTERNAL void scm_init_instructions (void);

#endif /* _SCM_INSTRUCTIONS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
