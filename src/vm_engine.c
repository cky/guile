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

/* This file is included in vm.c two times! */

#include "vm_engine.h"

/* VM names */
#undef VM_NAME
#undef VM_TABLE
#if VM_ENGINE == SCM_VM_REGULAR_ENGINE
#define VM_NAME		scm_regular_vm
#define VM_TABLE	scm_regular_instruction_table
#else
#if VM_ENGINE == SCM_VM_DEBUG_ENGINE
#define VM_NAME		scm_debug_vm
#define VM_TABLE	scm_debug_instruction_table
#endif
#endif

static SCM
VM_NAME (SCM vm, SCM program)
#define FUNC_NAME "vm-engine"
{
  /* Copies of VM registers */
  SCM ac = SCM_PACK (0);	/* accumulator */
  SCM *pc = NULL;		/* program counter */
  SCM *sp = NULL;		/* stack pointer */
  SCM *fp = NULL;		/* frame pointer */

  /* Cache variables */
  struct scm_vm *vmp = NULL;	/* the VM data pointer */
  SCM ext = SCM_BOOL_F;		/* the current external frame */
  SCM *stack_base = NULL;	/* stack base address */
  SCM *stack_limit = NULL;	/* stack limit address */

  /* Internal variables */
  int nargs = 0;		/* the number of arguments */
  SCM dynwinds = SCM_EOL;
#if VM_USE_HOOK
  SCM hook_args = SCM_LIST1 (vm);
#endif

  /* Initialize the instruction table at the first time.
   * This code must be here because the following table contains
   * pointers to the labels defined in this function.  */
  if (!VM_TABLE)
    {
      static struct scm_instruction table[] = {
#include "vm_system.vi"
#include "vm_scheme.vi"
#include "vm_number.vi"
	{ op_last }
      };
      VM_TABLE = table;
      return SCM_UNSPECIFIED;
    }

  /* Initialize the VM */
  vmp     = SCM_VM_DATA (vm);
  vmp->pc = SCM_PROGRAM_BASE (program);
  vmp->sp = vmp->stack_limit;
  LOAD ();

  /* top frame */
  VM_NEW_FRAME (fp, program, SCM_BOOL_F,
		SCM_VM_MAKE_ADDRESS (0),
		SCM_VM_MAKE_ADDRESS (0));

  /* Let's go! */
  VM_BOOT_HOOK ();

#ifndef HAVE_LABELS_AS_VALUES
  vm_start: switch (*pc++) {
#endif

#include "vm_system.c"
#include "vm_scheme.c"
#include "vm_number.c"

#ifndef HAVE_LABELS_AS_VALUES
  }
#endif

  abort (); /* never reached */
}
#undef FUNC_NAME
