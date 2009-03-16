/* Copyright (C) 2001 Free Software Foundation, Inc.
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

#ifndef _SCM_INSTRUCTIONS_H_
#define _SCM_INSTRUCTIONS_H_

#include <libguile.h>

#define SCM_VM_NUM_INSTRUCTIONS (1<<7)
#define SCM_VM_INSTRUCTION_MASK (SCM_VM_NUM_INSTRUCTIONS-1)

enum scm_opcode {
#define VM_INSTRUCTION_TO_OPCODE 1
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef VM_INSTRUCTION_TO_OPCODE
  scm_op_last = SCM_VM_NUM_INSTRUCTIONS
};

extern SCM scm_instruction_list (void);
extern SCM scm_instruction_p (SCM obj);
extern SCM scm_instruction_length (SCM inst);
extern SCM scm_instruction_pops (SCM inst);
extern SCM scm_instruction_pushes (SCM inst);
extern SCM scm_instruction_to_opcode (SCM inst);
extern SCM scm_opcode_to_instruction (SCM op);

extern void scm_bootstrap_instructions (void);
extern void scm_init_instructions (void);

#endif /* _SCM_INSTRUCTIONS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
