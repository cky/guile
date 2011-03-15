/* Copyright (C) 2001, 2009, 2010, 2011 Free Software Foundation, Inc.
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
  size_t object_count = 0;              /* length of OBJECTS */
  SCM *stack_limit = vp->stack_limit;	/* stack limit address */

  SCM dynstate = SCM_I_CURRENT_THREAD->dynamic_state;
  scm_t_int64 vm_cookie = vp->cookie++;

  /* Internal variables */
  int nvalues = 0;
  const char *func_name = NULL;         /* used for error reporting */
  SCM finish_args;                      /* used both for returns: both in error
                                           and normal situations */
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
    PUSH (0); /* dynamic link */
    PUSH (0); /* mvra */
    PUSH (0); /* ra */
    PUSH (prog);
    if (SCM_UNLIKELY (sp + nargs >= stack_limit))
      goto vm_error_too_many_args;
    while (nargs--)
      PUSH (*argv++);
  }

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

  
 vm_done:
  SYNC_ALL ();
  return finish_args;

  /* Errors */
  {
    SCM err_msg;

    /* FIXME: need to sync regs before allocating anything, in each case. */

  vm_error_bad_instruction:
    err_msg  = scm_from_locale_string ("VM: Bad instruction: ~s");
    finish_args = scm_list_1 (scm_from_uchar (ip[-1]));
    goto vm_error;

  vm_error_unbound:
    /* FINISH_ARGS should be the name of the unbound variable.  */
    SYNC_ALL ();
    err_msg = scm_from_locale_string ("Unbound variable: ~s");
    scm_error_scm (scm_misc_error_key, program, err_msg,
                   scm_list_1 (finish_args), SCM_BOOL_F);
    goto vm_error;

  vm_error_unbound_fluid:
    SYNC_ALL ();
    err_msg = scm_from_locale_string ("Unbound fluid: ~s");
    scm_error_scm (scm_misc_error_key, program, err_msg,
                   scm_list_1 (finish_args), SCM_BOOL_F);
    goto vm_error;

  vm_error_not_a_variable:
    SYNC_ALL ();
    scm_error (scm_arg_type_key, func_name, "Not a variable: ~S",
               scm_list_1 (finish_args), scm_list_1 (finish_args));
    goto vm_error;

  vm_error_apply_to_non_list:
    SYNC_ALL ();
    scm_error (scm_arg_type_key, "apply", "Apply to non-list: ~S",
               scm_list_1 (finish_args), scm_list_1 (finish_args));
    goto vm_error;

  vm_error_kwargs_length_not_even:
    SYNC_ALL ();
    err_msg = scm_from_locale_string ("Odd length of keyword argument list");
    scm_error_scm (sym_keyword_argument_error, program, err_msg,
                   SCM_EOL, SCM_BOOL_F);

  vm_error_kwargs_invalid_keyword:
    /* FIXME say which one it was */
    SYNC_ALL ();
    err_msg = scm_from_locale_string ("Invalid keyword");
    scm_error_scm (sym_keyword_argument_error, program, err_msg,
                   SCM_EOL, SCM_BOOL_F);

  vm_error_kwargs_unrecognized_keyword:
    /* FIXME say which one it was */
    SYNC_ALL ();
    err_msg = scm_from_locale_string ("Unrecognized keyword");
    scm_error_scm (sym_keyword_argument_error, program, err_msg,
                   SCM_EOL, SCM_BOOL_F);

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
    scm_error (scm_arg_type_key, NULL, "Wrong type to apply: ~S",
               scm_list_1 (program), scm_list_1 (program));
    goto vm_error;

  vm_error_stack_overflow:
    err_msg  = scm_from_locale_string ("VM: Stack overflow");
    finish_args = SCM_EOL;
    if (stack_limit < vp->stack_base + vp->stack_size)
      /* There are VM_STACK_RESERVE_SIZE bytes left.  Make them available so
	 that `throw' below can run on this VM.  */
      vp->stack_limit = vp->stack_base + vp->stack_size;
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
    scm_wrong_type_arg_msg (func_name, 1, finish_args, "pair");
    /* shouldn't get here */
    goto vm_error;

  vm_error_not_a_bytevector:
    SYNC_ALL ();
    scm_wrong_type_arg_msg (func_name, 1, finish_args, "bytevector");
    /* shouldn't get here */
    goto vm_error;

  vm_error_not_a_struct:
    SYNC_ALL ();
    scm_wrong_type_arg_msg (func_name, 1, finish_args, "struct");
    /* shouldn't get here */
    goto vm_error;

  vm_error_not_a_thunk:
    SYNC_ALL ();
    scm_wrong_type_arg_msg ("dynamic-wind", 1, finish_args, "thunk");
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

  vm_error_continuation_not_rewindable:
    err_msg  = scm_from_locale_string ("Unrewindable partial continuation");
    finish_args = scm_cons (finish_args, SCM_EOL);
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
