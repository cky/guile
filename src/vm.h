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

#ifndef VM_H
#define VM_H

#include <libguile.h>


/*
 * Instruction
 */

/* Opcode */
enum scm_opcode {
#include "vm_system.op"
#include "vm_scheme.op"
#include "vm_number.op"
  op_last
};

/* Argument type */
/* Modify `mark_bytecode', `scm_make_bytecode', and `scm_bytecode_decode'! */
enum scm_inst_type {
  INST_NONE,			/* no argument */
  INST_INUM,			/* fixed integer */
  INST_SCM,			/* scheme object */
  INST_EXT,			/* external offset */
  INST_TOP,			/* top-level variable */
  INST_CODE,			/* program code */
  INST_ADDR			/* program address */
};

struct scm_instruction {
  enum scm_opcode opcode;	/* opcode */
  enum scm_inst_type type;	/* argument type */
  char *name;			/* instruction name */
  void *addr;			/* instruction address */
  SCM obj;			/* instruction object */
  /* fields for VM functions */
  char *sname;			/* Scheme procedure name */
  char nargs;			/* the number of arguments */
  char restp;			/* have a rest argument or not */
};

#define SCM_INSTRUCTION_P(OBJ)	SCM_SMOB_PREDICATE (scm_instruction_tag, OBJ)
#define SCM_INSTRUCTION_DATA(INST) ((struct scm_instruction *) SCM_SMOB_DATA (INST))
#define SCM_VALIDATE_INSTRUCTION(POS,OBJ) SCM_MAKE_VALIDATE (POS, OBJ, INSTRUCTION_P)

#define SCM_SYSTEM_INSTRUCTION_P(OBJ) \
  (SCM_INSTRUCTION_P (OBJ) && !SCM_INSTRUCTION_DATA(OBJ)->sname)
#define SCM_FUNCTIONAL_INSTRUCTION_P(OBJ) \
  (SCM_INSTRUCTION_P (OBJ) && SCM_INSTRUCTION_DATA(OBJ)->sname)

#define SCM_ADDR_TO_CODE(ADDR)	SCM_PACK (ADDR)
#define SCM_CODE_TO_ADDR(CODE)	((void *) SCM_UNPACK (CODE))
#define SCM_CODE_TO_DEBUG_ADDR(CODE) instruction_code_to_debug_addr (CODE)


/*
 * Bytecode
 */

struct scm_bytecode {
  int size;	/* the size of the bytecode  */
  char nreqs;	/* the number of required arguments */
  char restp;	/* have a rest argument or not */
  char nvars;	/* the number of local variables */
  char nexts;	/* the number of external variables */
  int *exts;	/* externalized arguments */
  SCM base[0];	/* base address (must be the last!) */
};

#define SCM_BYTECODE_P(OBJ)	SCM_SMOB_PREDICATE (scm_bytecode_tag, OBJ)
#define SCM_BYTECODE_DATA(BC)	((struct scm_bytecode *) SCM_SMOB_DATA (BC))
#define SCM_VALIDATE_BYTECODE(POS,OBJ) SCM_MAKE_VALIDATE (POS, OBJ, BYTECODE_P)

#define SCM_BYTECODE_SIZE(BC)	SCM_BYTECODE_DATA (BC)->size
#define SCM_BYTECODE_NREQS(BC)	SCM_BYTECODE_DATA (BC)->nreqs
#define SCM_BYTECODE_RESTP(BC)	SCM_BYTECODE_DATA (BC)->restp
#define SCM_BYTECODE_NVARS(BC)	SCM_BYTECODE_DATA (BC)->nvars
#define SCM_BYTECODE_NEXTS(BC)	SCM_BYTECODE_DATA (BC)->nexts
#define SCM_BYTECODE_EXTS(BC)	SCM_BYTECODE_DATA (BC)->exts
#define SCM_BYTECODE_BASE(BC)	SCM_BYTECODE_DATA (BC)->base

extern SCM scm_bytecode_p (SCM obj);
extern SCM scm_make_bytecode (SCM code);
extern SCM scm_bytecode_decode (SCM bytecode);


/*
 * Program
 */

#define SCM_MAKE_PROGRAM(CODE,ENV)    make_program (CODE, ENV)
#define SCM_PROGRAM_P(OBJ)	      SCM_SMOB_PREDICATE (scm_program_tag, OBJ)
#define SCM_PROGRAM_CODE(PROG)	      SCM_CELL_OBJECT_1 (PROG)
#define SCM_PROGRAM_ENV(PROG)         SCM_CELL_OBJECT_2 (PROG)
#define SCM_VALIDATE_PROGRAM(POS,PROG) SCM_MAKE_VALIDATE (POS, PROG, PROGRAM_P)

/* Abbreviations */
#define SCM_PROGRAM_SIZE(PROG)	SCM_BYTECODE_SIZE (SCM_PROGRAM_CODE (PROG))
#define SCM_PROGRAM_NREQS(PROG)	SCM_BYTECODE_NREQS (SCM_PROGRAM_CODE (PROG))
#define SCM_PROGRAM_RESTP(PROG)	SCM_BYTECODE_RESTP (SCM_PROGRAM_CODE (PROG))
#define SCM_PROGRAM_NVARS(PROG)	SCM_BYTECODE_NVARS (SCM_PROGRAM_CODE (PROG))
#define SCM_PROGRAM_NEXTS(PROG)	SCM_BYTECODE_NEXTS (SCM_PROGRAM_CODE (PROG))
#define SCM_PROGRAM_EXTS(PROG)	SCM_BYTECODE_EXTS (SCM_PROGRAM_CODE (PROG))
#define SCM_PROGRAM_BASE(PROG)	SCM_BYTECODE_BASE (SCM_PROGRAM_CODE (PROG))

extern SCM scm_program_p (SCM obj);
extern SCM scm_make_program (SCM bytecode, SCM env);
extern SCM scm_program_code (SCM program);
extern SCM scm_program_base (SCM program);


/*
 * VM Address
 */

#define SCM_VM_MAKE_ADDRESS(ADDR)	SCM_MAKINUM ((long) (ADDR))
#define SCM_VM_ADDRESS(OBJ)		((SCM *) SCM_INUM (OBJ))


/*
 * VM External
 */

/* VM external maintains a set of variables outside of the stack.
   This is used to implement external chain of the environment. */

#define SCM_VM_MAKE_EXTERNAL(SIZE)	scm_make_vector (SCM_MAKINUM ((SIZE) + 1), SCM_UNDEFINED)
#define SCM_VM_EXTERNAL_LINK(EXT)	(SCM_VELTS (EXT)[0])
#define SCM_VM_EXTERNAL_VARIABLE(EXT,N)	(SCM_VELTS (EXT)[(N) + 1])


/*
 * VM Continuation
 */

#define SCM_VM_CONT_P(OBJ)	SCM_SMOB_PREDICATE (scm_vm_cont_tag, OBJ)
#define SCM_VM_CONT_VMP(CONT)	((struct scm_vm *) SCM_CELL_WORD_1 (CONT))

#define SCM_VM_CAPTURE_CONT(VMP)	capture_vm_cont (VMP)
#define SCM_VM_REINSTATE_CONT(VMP,CONT) reinstate_vm_cont (VMP, CONT)


/*
 * VM Frame
 */

/* VM frame is allocated in the stack */
/* NOTE: Modify make_vm_frame and VM_NEW_FRAME too! */
#define SCM_VM_FRAME_DATA_SIZE		6
#define SCM_VM_FRAME_VARIABLE(FP,N)	(FP[N])
#define SCM_VM_FRAME_SIZE(FP)		(FP[-1])
#define SCM_VM_FRAME_PROGRAM(FP)	(FP[-2])
#define SCM_VM_FRAME_DYNAMIC_LINK(FP)	(FP[-3])
#define SCM_VM_FRAME_EXTERNAL_LINK(FP)	(FP[-4])
#define SCM_VM_FRAME_STACK_POINTER(FP)	(FP[-5])
#define SCM_VM_FRAME_RETURN_ADDRESS(FP)	(FP[-6])


/*
 * VM
 */

/* Modify make_vm, mark_vm, and SYNC, too! */
struct scm_vm {
  SCM ac;		/* Accumulator */
  SCM *pc;		/* Program counter */
  SCM *sp;		/* Stack pointer */
  SCM *fp;		/* Frame pointer */
  int stack_size;
  SCM *stack_base;
  SCM *stack_limit;
  SCM options;
  SCM boot_hook, halt_hook, next_hook;
  SCM call_hook, apply_hook, return_hook;
};

#define SCM_VM_P(OBJ) SCM_SMOB_PREDICATE (scm_vm_tag, OBJ)
#define SCM_VM_DATA(VM) ((struct scm_vm *) SCM_SMOB_DATA (VM))
#define SCM_VALIDATE_VM(POS,OBJ) SCM_MAKE_VALIDATE (POS, OBJ, VM_P)

/* Engine types */
#define SCM_VM_REGULAR_ENGINE	0	/* Fail safe and fast enough */
#define SCM_VM_DEBUG_ENGINE	1	/* Functional but very slow */

#endif /* not VM_H */
