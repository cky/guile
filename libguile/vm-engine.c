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

/* This file is included in vm.c twice */

#include "vm-engine.h"


static SCM
vm_run (SCM vm, SCM program, SCM args)
#define FUNC_NAME "vm-engine"
{
  /* VM registers */
  register scm_byte_t *ip IP_REG;	/* instruction pointer */
  register SCM *sp SP_REG;		/* stack pointer */
  register SCM *fp FP_REG;		/* frame pointer */

  /* Cache variables */
  struct scm_vm *vp = SCM_VM_DATA (vm);	/* VM data pointer */
  struct scm_program *bp = NULL;	/* program base pointer */
  SCM external = SCM_EOL;		/* external environment */
  SCM *objects = NULL;			/* constant objects */
  scm_t_array_handle objects_handle;    /* handle of the OBJECTS array */
  size_t object_count;                  /* length of OBJECTS */
  SCM *stack_base = vp->stack_base;	/* stack base address */
  SCM *stack_limit = vp->stack_limit;	/* stack limit address */

  /* Internal variables */
  int nargs = 0;
  int nvalues = 0;
  long start_time = scm_c_get_internal_run_time ();
  // SCM dynwinds = SCM_EOL;
  SCM err_msg;
  SCM err_args;
#if VM_USE_HOOKS
  SCM hook_args = SCM_LIST1 (vm);
#endif
  struct vm_unwind_data wind_data;

  /* dynwind ended in the halt instruction */
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  wind_data.vp = vp;
  wind_data.sp = vp->sp;
  wind_data.fp = vp->fp;
  wind_data.this_frame = vp->this_frame;
  scm_dynwind_unwind_handler (vm_reset_stack, &wind_data, 0);

  /* could do this if we reified all vm stacks -- for now, don't bother changing
     *the-vm*
  if (scm_fluid_ref (scm_the_vm_fluid) != vm)
    scm_dynwind_fluid (scm_the_vm_fluid, vm);
   */

#ifdef HAVE_LABELS_AS_VALUES
  /* Jump table */
  static void *jump_table[] = {
#define VM_INSTRUCTION_TO_LABEL 1
#include "vm-expand.h"
#include "vm-i-system.i"
#include "vm-i-scheme.i"
#include "vm-i-loader.i"
#undef VM_INSTRUCTION_TO_LABEL
  };
#endif

  /* Initialization */
  {
    SCM prog = program;

    /* Boot program */
    scm_byte_t bytes[6] = {scm_op_mv_call, 0, 0, 1, scm_op_make_int8_1, scm_op_halt};
    bytes[1] = scm_ilength (args); /* FIXME: argument overflow */
    program = scm_c_make_program (bytes, 6, SCM_BOOL_F);

    /* Initial frame */
    CACHE_REGISTER ();
    CACHE_PROGRAM ();
    PUSH (program);
    NEW_FRAME ();

    /* Initial arguments */
    PUSH (prog);
    for (; !SCM_NULLP (args); args = SCM_CDR (args))
      PUSH (SCM_CAR (args));
  }

  /* Let's go! */
  BOOT_HOOK ();

#ifndef HAVE_LABELS_AS_VALUES
 vm_start:
  switch (*ip++) {
#endif

#include "vm-expand.h"
#include "vm-i-system.c"
#include "vm-i-scheme.c"
#include "vm-i-loader.c"

#ifndef HAVE_LABELS_AS_VALUES
  }
#endif

  /* Errors */
  {
  vm_error_unbound:
    err_msg  = scm_from_locale_string ("VM: Unbound variable: ~A");
    goto vm_error;

  vm_error_wrong_type_arg:
    err_msg  = scm_from_locale_string ("VM: Wrong type argument");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_wrong_num_args:
    err_msg  = scm_from_locale_string ("VM: Wrong number of arguments");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_wrong_type_apply:
    err_msg  = scm_from_locale_string ("VM: Wrong type to apply: ~S "
				       "[IP offset: ~a]");
    err_args = SCM_LIST2 (program,
			  SCM_I_MAKINUM (ip - bp->base));
    goto vm_error;

  vm_error_stack_overflow:
    err_msg  = scm_from_locale_string ("VM: Stack overflow");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_stack_underflow:
    err_msg  = scm_from_locale_string ("VM: Stack underflow");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_no_values:
    err_msg  = scm_from_locale_string ("VM: 0-valued return");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_not_enough_values:
    err_msg  = scm_from_locale_string ("VM: Not enough values for mv-bind");
    err_args = SCM_EOL;
    goto vm_error;

#if VM_CHECK_IP
  vm_error_invalid_address:
    err_msg  = scm_from_locale_string ("VM: Invalid program address");
    err_args = SCM_EOL;
    goto vm_error;
#endif

#if VM_CHECK_EXTERNAL
  vm_error_external:
    err_msg  = scm_from_locale_string ("VM: Invalid external access");
    err_args = SCM_EOL;
    goto vm_error;
#endif

#if VM_CHECK_OBJECT
  vm_error_object:
    err_msg = scm_from_locale_string ("VM: Invalid object table access");
    err_args = SCM_EOL;
    goto vm_error;
#endif

  vm_error:
    SYNC_ALL ();
    if (objects)
      scm_array_handle_release (&objects_handle);

    scm_ithrow (sym_vm_error, SCM_LIST3 (sym_vm_run, err_msg, err_args), 1);
  }

  abort (); /* never reached */
}
#undef FUNC_NAME

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
