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

#ifndef _INSTRUCTIONS_H_
#define _INSTRUCTIONS_H_

#include <libguile.h>
#include "config.h"

enum scm_opcode {
#define VM_INSTRUCTION_TO_OPCODE 1
#include "vm_expand.h"
#include "vm_system.i"
#include "vm_scheme.i"
#include "vm_number.i"
#include "vm_loader.i"
#undef VM_INSTRUCTION_TO_OPCODE
  scm_op_last
};

struct scm_instruction {
  enum scm_opcode opcode;	/* opcode */
  char *name;			/* instruction name */
  char len;			/* byte length */
};

#define SCM_INSTRUCTION_P(x)		(scm_lookup_instruction (x))
#define SCM_INSTRUCTION_OPCODE(i)	(scm_lookup_instruction (i)->opcode)
#define SCM_INSTRUCTION_NAME(i)		(scm_lookup_instruction (i)->name)
#define SCM_INSTRUCTION_LEN(i)		(scm_lookup_instruction (i)->len)
#define SCM_VALIDATE_INSTRUCTION(p,x)	SCM_MAKE_VALIDATE (p, x, INSTRUCTION_P)

#define SCM_INSTRUCTION(i)		(&scm_instruction_table[i])

extern struct scm_instruction scm_instruction_table[];
extern struct scm_instruction *scm_lookup_instruction (SCM name);

extern void scm_init_instructions (void);

#endif /* _INSTRUCTIONS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
