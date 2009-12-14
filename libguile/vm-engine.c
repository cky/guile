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

/* This file is included in vm.c multiple times */

#if (VM_ENGINE == SCM_VM_REGULAR_ENGINE)
#define VM_USE_HOOKS		0	/* Various hooks */
#define VM_CHECK_OBJECT         1       /* Check object table */
#define VM_CHECK_FREE_VARIABLES 1       /* Check free variable access */
#elif (VM_ENGINE == SCM_VM_DEBUG_ENGINE)
#define VM_USE_HOOKS		1
#define VM_CHECK_OBJECT         1
#define VM_CHECK_FREE_VARIABLES 1
#else
#error unknown debug engine VM_ENGINE
#endif

#include "vm-engine.h"


static SCM
VM_NAME (struct scm_vm *vp, SCM program, SCM *argv, int nargs)
{
  /* VM registers */
  register scm_t_uint8 *ip IP_REG;	/* instruction pointer */
  register SCM *sp SP_REG;		/* stack pointer */
  register SCM *fp FP_REG;		/* frame pointer */

  /* Cache variables */
  struct scm_objcode *bp = NULL;	/* program base pointer */
  SCM *free_vars = NULL;                /* free variables */
  size_t free_vars_count = 0;           /* length of FREE_VARS */
  SCM *objects = NULL;			/* constant objects */
  size_t object_count = 0;              /* length of OBJECTS */
  SCM *stack_limit = vp->stack_limit;	/* stack limit address */

  /* Internal variables */
  int nvalues = 0;
  SCM finish_args;                      /* used both for returns: both in error
                                           and normal situations */
#if VM_USE_HOOKS
  SCM hook_args = SCM_EOL;
#endif

#ifdef HAVE_LABELS_AS_VALUES
  static void **jump_table = NULL;
#endif
  
#ifdef HAVE_LABELS_AS_VALUES
  if (SCM_UNLIKELY (!jump_table))
    {
      int i;
      jump_table = malloc (SCM_VM_NUM_INSTRUCTIONS * sizeof(void*));
      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        jump_table[i] = &&vm_error_bad_instruction;
#define VM_INSTRUCTION_TO_LABEL 1
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef VM_INSTRUCTION_TO_LABEL
    }
#endif

  /* Initialization */
  {
    SCM prog = program;

    /* Boot program */
    program = vm_make_boot_program (nargs);

    /* Initial frame */
    CACHE_REGISTER ();
    PUSH ((SCM)fp); /* dynamic link */
    PUSH (0); /* mvra */
    PUSH ((SCM)ip); /* ra */
    CACHE_PROGRAM ();
    PUSH (program);
    fp = sp + 1;
    ip = SCM_C_OBJCODE_BASE (bp);
    /* MV-call frame, function & arguments */
    PUSH ((SCM)fp); /* dynamic link */
    PUSH (0); /* mvra */
    PUSH (0); /* ra */
    PUSH (prog);
    if (SCM_UNLIKELY (sp + nargs >= stack_limit))
      goto vm_error_too_many_args;
    while (nargs--)
      PUSH (*argv++);
  }

  /* Let's go! */
  BOOT_HOOK ();
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

  
 vm_done:
  SYNC_ALL ();
  return finish_args;

  /* Errors */
  {
    SCM err_msg;

  vm_error_bad_instruction:
    err_msg  = scm_from_locale_string ("VM: Bad instruction: ~s");
    finish_args = scm_list_1 (scm_from_uchar (ip[-1]));
    goto vm_error;

  vm_error_unbound:
    err_msg  = scm_from_locale_string ("VM: Unbound variable: ~s");
    goto vm_error;

  vm_error_wrong_type_arg:
    err_msg  = scm_from_locale_string ("VM: Wrong type argument");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_kwargs_length_not_even:
    err_msg  = scm_from_locale_string ("Bad keyword argument list: odd length");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_kwargs_invalid_keyword:
    err_msg  = scm_from_locale_string ("Bad keyword argument list: expected keyword");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_kwargs_unrecognized_keyword:
    err_msg  = scm_from_locale_string ("Bad keyword argument list: unrecognized keyword");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_too_many_args:
    err_msg  = scm_from_locale_string ("VM: Too many arguments");
    finish_args = scm_list_1 (scm_from_int (nargs));
    goto vm_error;

  vm_error_wrong_num_args:
    /* nargs and program are valid */
    SYNC_ALL ();
    scm_wrong_num_args (program);
    /* shouldn't get here */
    goto vm_error;

  vm_error_wrong_type_apply:
    SYNC_ALL ();
    scm_error (scm_arg_type_key, FUNC_NAME, "Wrong type to apply: ~S",
               scm_list_1 (program), scm_list_1 (program));
    goto vm_error;

  vm_error_stack_overflow:
    err_msg  = scm_from_locale_string ("VM: Stack overflow");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_stack_underflow:
    err_msg  = scm_from_locale_string ("VM: Stack underflow");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_improper_list:
    err_msg  = scm_from_locale_string ("Expected a proper list, but got object with tail ~s");
    goto vm_error;

  vm_error_not_a_pair:
    SYNC_ALL ();
    scm_wrong_type_arg_msg (FUNC_NAME, 1, finish_args, "pair");
    /* shouldn't get here */
    goto vm_error;

  vm_error_not_a_bytevector:
    SYNC_ALL ();
    scm_wrong_type_arg_msg (FUNC_NAME, 1, finish_args, "bytevector");
    /* shouldn't get here */
    goto vm_error;

  vm_error_not_a_struct:
    SYNC_ALL ();
    scm_wrong_type_arg_msg (FUNC_NAME, 1, finish_args, "struct");
    /* shouldn't get here */
    goto vm_error;

  vm_error_no_values:
    err_msg  = scm_from_locale_string ("Zero values returned to single-valued continuation");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_not_enough_values:
    err_msg  = scm_from_locale_string ("Too few values returned to continuation");
    finish_args = SCM_EOL;
    goto vm_error;

  vm_error_bad_wide_string_length:
    err_msg  = scm_from_locale_string ("VM: Bad wide string length: ~S");
    goto vm_error;

#ifdef VM_CHECK_IP
  vm_error_invalid_address:
    err_msg  = scm_from_locale_string ("VM: Invalid program address");
    finish_args = SCM_EOL;
    goto vm_error;
#endif

#if VM_CHECK_OBJECT
  vm_error_object:
    err_msg = scm_from_locale_string ("VM: Invalid object table access");
    finish_args = SCM_EOL;
    goto vm_error;
#endif

#if VM_CHECK_FREE_VARIABLES
  vm_error_free_variable:
    err_msg = scm_from_locale_string ("VM: Invalid free variable access");
    finish_args = SCM_EOL;
    goto vm_error;
#endif

  vm_error:
    SYNC_ALL ();

    scm_ithrow (sym_vm_error, scm_list_3 (sym_vm_run, err_msg, finish_args),
		1);
  }

  abort (); /* never reached */
}

#undef VM_USE_HOOKS
#undef VM_CHECK_OBJECT
#undef VM_CHECK_FREE_VARIABLE

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
