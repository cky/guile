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

#include "vm_engine.h"

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
  SCM *stack_base = vp->stack_base;	/* stack base address */
  SCM *stack_limit = vp->stack_limit;	/* stack limit address */

  /* Internal variables */
  int nargs = 0;
  long start_time = scm_c_get_internal_run_time ();
  // SCM dynwinds = SCM_EOL;
  SCM err_msg;
  SCM err_args;
#if VM_USE_HOOKS
  SCM hook_args = SCM_LIST1 (vm);
#endif

#ifdef HAVE_LABELS_AS_VALUES
  /* Jump talbe */
  static void *jump_table[] = {
#define VM_INSTRUCTION_TO_LABEL 1
#include "vm_expand.h"
#include "vm_system.i"
#include "vm_scheme.i"
#include "vm_loader.i"
#undef VM_INSTRUCTION_TO_LABEL
  };
#endif

  /* Initialization */
  {
    SCM prog = program;

    /* Boot program */
    scm_byte_t bytes[3] = {scm_op_call, 0, scm_op_halt};
    bytes[1] = scm_ilength (args);
    program = scm_c_make_program (bytes, 3, SCM_BOOL_T);

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

#include "vm_expand.h"
#include "vm_system.c"
#include "vm_scheme.c"
#include "vm_loader.c"

#ifndef HAVE_LABELS_AS_VALUES
  }
#endif

  /* Errors */
  {
  vm_error_unbound:
    err_msg  = scm_makfrom0str ("VM: Unbound variable: ~A");
    goto vm_error;

  vm_error_wrong_type_arg:
    err_msg  = scm_makfrom0str ("VM: Wrong type argument");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_wrong_num_args:
    err_msg  = scm_makfrom0str ("VM: Wrong number of arguments");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_wrong_type_apply:
    err_msg  = scm_makfrom0str ("VM: Wrong type to apply: ~S");
    err_args = SCM_LIST1 (program);
    goto vm_error;

  vm_error_stack_overflow:
    err_msg  = scm_makfrom0str ("VM: Stack overflow");
    err_args = SCM_EOL;
    goto vm_error;

  vm_error_stack_underflow:
    err_msg  = scm_makfrom0str ("VM: Stack underflow");
    err_args = SCM_EOL;
    goto vm_error;

#if VM_CHECK_IP
  vm_error_invalid_address:
    err_msg  = scm_makfrom0str ("VM: Invalid program address");
    err_args = SCM_EOL;
    goto vm_error;
#endif

#if VM_CHECK_EXTERNAL
  vm_error_external:
    err_msg  = scm_makfrom0str ("VM: Invalid external access");
    err_args = SCM_EOL;
    goto vm_error;
#endif

  vm_error:
    SYNC_ALL ();
    scm_ithrow (sym_vm_error,
		SCM_LIST4 (sym_vm_run, err_msg, err_args,
			   scm_vm_current_frame (vm)),
		1);
  }

  abort (); /* never reached */
}
#undef FUNC_NAME

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
