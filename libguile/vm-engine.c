/* Copyright (C) 2001, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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

/* This file is included in vm.c multiple times */

#if (VM_ENGINE == SCM_VM_REGULAR_ENGINE)
#define VM_USE_HOOKS		0	/* Various hooks */
#define VM_CHECK_OBJECT         0       /* Check object table */
#define VM_CHECK_FREE_VARIABLES 0       /* Check free variable access */
#define VM_CHECK_UNDERFLOW      0       /* Check underflow when popping values */
#elif (VM_ENGINE == SCM_VM_DEBUG_ENGINE)
#define VM_USE_HOOKS		1
#define VM_CHECK_OBJECT         0
#define VM_CHECK_FREE_VARIABLES 0
#define VM_CHECK_UNDERFLOW      0       /* Check underflow when popping values */
#else
#error unknown debug engine VM_ENGINE
#endif

#include "vm-engine.h"


static SCM
VM_NAME (SCM vm, SCM program, SCM *argv, int nargs)
{
  /* VM registers */
  register scm_t_uint8 *ip IP_REG;	/* instruction pointer */
  register SCM *sp SP_REG;		/* stack pointer */
  register SCM *fp FP_REG;		/* frame pointer */
  struct scm_vm *vp = SCM_VM_DATA (vm);

  /* Cache variables */
  struct scm_objcode *bp = NULL;	/* program base pointer */
  SCM *objects = NULL;			/* constant objects */
#if VM_CHECK_OBJECT
  size_t object_count = 0;              /* length of OBJECTS */
#endif
  SCM *stack_limit = vp->stack_limit;	/* stack limit address */

  scm_i_thread *current_thread = SCM_I_CURRENT_THREAD;
  scm_t_int64 vm_cookie = vp->cookie++;

  /* Internal variables */
  int nvalues = 0;

#ifdef HAVE_LABELS_AS_VALUES
  static const void **jump_table_pointer = NULL;
#endif

#ifdef HAVE_LABELS_AS_VALUES
  register const void **jump_table JT_REG;

  if (SCM_UNLIKELY (!jump_table_pointer))
    {
      int i;
      jump_table_pointer = malloc (SCM_VM_NUM_INSTRUCTIONS * sizeof (void*));
      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        jump_table_pointer[i] = &&vm_error_bad_instruction;
#define VM_INSTRUCTION_TO_LABEL 1
#define jump_table jump_table_pointer
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef jump_table
#undef VM_INSTRUCTION_TO_LABEL
    }

  /* Attempt to keep JUMP_TABLE_POINTER in a register.  This saves one
     load instruction at each instruction dispatch.  */
  jump_table = jump_table_pointer;
#endif

  /* Initial frame */
  CACHE_REGISTER ();
  PUSH (SCM_PACK (fp)); /* dynamic link */
  PUSH (SCM_PACK (0)); /* mvra */
  PUSH (SCM_PACK (ip)); /* ra */
  PUSH (boot_continuation);
  fp = sp + 1;
  ip = SCM_C_OBJCODE_BASE (SCM_PROGRAM_DATA (boot_continuation));

  /* MV-call frame, function & arguments */
  PUSH (SCM_PACK (fp)); /* dynamic link */
  PUSH (SCM_PACK (ip + 1)); /* mvra */
  PUSH (SCM_PACK (ip)); /* ra */
  PUSH (program);
  fp = sp + 1;
  VM_ASSERT (sp + nargs < stack_limit, vm_error_too_many_args (nargs));
  while (nargs--)
    PUSH (*argv++);

  PUSH_CONTINUATION_HOOK ();

 apply:
  program = fp[-1];
  if (!SCM_PROGRAM_P (program))
    {
      if (SCM_STRUCTP (program) && SCM_STRUCT_APPLICABLE_P (program))
        fp[-1] = SCM_STRUCT_PROCEDURE (program);
      else if (SCM_NIMP (program) && SCM_TYP7 (program) == scm_tc7_smob
               && SCM_SMOB_APPLICABLE_P (program))
        {
          /* (smob arg0 ... argN) => (apply-smob smob arg0 ... argN) */
          int i;
          PUSH (SCM_BOOL_F);
          for (i = sp - fp; i >= 0; i--)
            fp[i] = fp[i - 1];
          fp[-1] = SCM_SMOB_DESCRIPTOR (program).apply_trampoline_objcode;
        }
      else
        {
          SYNC_ALL();
          vm_error_wrong_type_apply (program);
        }
      goto apply;
    }

  CACHE_PROGRAM ();
  ip = SCM_C_OBJCODE_BASE (bp);

  APPLY_HOOK ();

  /* Let's go! */
  NEXT;

#ifndef HAVE_LABELS_AS_VALUES
 vm_start:
  switch ((*ip++) & SCM_VM_INSTRUCTION_MASK) {
#endif

#include "vm-expand.h"
#include "vm-i-system.c"
#include "vm-i-scheme.c"
#include "vm-i-loader.c"

#ifndef HAVE_LABELS_AS_VALUES
  default:
    goto vm_error_bad_instruction;
  }
#endif

  abort (); /* never reached */

 vm_error_bad_instruction:
  vm_error_bad_instruction (ip[-1]);
  abort (); /* never reached */

 handle_overflow:
  SYNC_ALL ();
  vm_error_stack_overflow (vp);
  abort (); /* never reached */
}

#undef VM_USE_HOOKS
#undef VM_CHECK_OBJECT
#undef VM_CHECK_FREE_VARIABLE
#undef VM_CHECK_UNDERFLOW

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
