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

#ifndef _SCM_VM_H_
#define _SCM_VM_H_

#include <libguile.h>
#include <libguile/programs.h>

#define SCM_VM_BOOT_HOOK	0
#define SCM_VM_HALT_HOOK	1
#define SCM_VM_NEXT_HOOK	2
#define SCM_VM_BREAK_HOOK	3
#define SCM_VM_ENTER_HOOK	4
#define SCM_VM_APPLY_HOOK	5
#define SCM_VM_EXIT_HOOK	6
#define SCM_VM_RETURN_HOOK	7
#define SCM_VM_NUM_HOOKS	8

struct scm_vm;

typedef SCM (*scm_t_vm_engine) (struct scm_vm *vp, SCM program, SCM *argv, int nargs);

#define SCM_VM_REGULAR_ENGINE 0
#define SCM_VM_DEBUG_ENGINE 1
#define SCM_VM_NUM_ENGINES 2

struct scm_vm {
  scm_t_uint8 *ip;		/* instruction pointer */
  SCM *sp;			/* stack pointer */
  SCM *fp;			/* frame pointer */
  size_t stack_size;		/* stack size */
  SCM *stack_base;		/* stack base address */
  SCM *stack_limit;		/* stack limit address */
  int engine;                   /* which vm engine we're using */
  SCM hooks[SCM_VM_NUM_HOOKS];	/* hooks */
  SCM options;			/* options */
  unsigned long time;		/* time spent */
  unsigned long clock;		/* bogos clock */
  SCM trace_frame;              /* a frame being traced */
};

SCM_API SCM scm_the_vm_fluid;

#define SCM_VM_P(x)		SCM_SMOB_PREDICATE (scm_tc16_vm, x)
#define SCM_VM_DATA(vm)		((struct scm_vm *) SCM_SMOB_DATA (vm))
#define SCM_VALIDATE_VM(pos,x)	SCM_MAKE_VALIDATE (pos, x, VM_P)

SCM_API SCM scm_the_vm ();
SCM_API SCM scm_make_vm (void);
SCM_API SCM scm_vm_apply (SCM vm, SCM program, SCM args);
SCM_API SCM scm_c_vm_run (SCM vm, SCM program, SCM *argv, int nargs);
SCM_API SCM scm_vm_call_with_new_stack (SCM vm, SCM thunk, SCM id);
SCM_API SCM scm_vm_option_ref (SCM vm, SCM key);
SCM_API SCM scm_vm_option_set_x (SCM vm, SCM key, SCM val);

SCM_API SCM scm_vm_version (void);
SCM_API SCM scm_the_vm (void);
SCM_API SCM scm_vm_p (SCM obj);
SCM_API SCM scm_vm_ip (SCM vm);
SCM_API SCM scm_vm_sp (SCM vm);
SCM_API SCM scm_vm_fp (SCM vm);
SCM_API SCM scm_vm_boot_hook (SCM vm);
SCM_API SCM scm_vm_halt_hook (SCM vm);
SCM_API SCM scm_vm_next_hook (SCM vm);
SCM_API SCM scm_vm_break_hook (SCM vm);
SCM_API SCM scm_vm_enter_hook (SCM vm);
SCM_API SCM scm_vm_apply_hook (SCM vm);
SCM_API SCM scm_vm_exit_hook (SCM vm);
SCM_API SCM scm_vm_return_hook (SCM vm);
SCM_API SCM scm_vm_option (SCM vm, SCM key);
SCM_API SCM scm_set_vm_option_x (SCM vm, SCM key, SCM val);
SCM_API SCM scm_vm_stats (SCM vm);
SCM_API SCM scm_vm_trace_frame (SCM vm);

struct scm_vm_cont {
  scm_t_uint8 *ip;
  SCM *sp;
  SCM *fp;
  scm_t_ptrdiff stack_size;
  SCM *stack_base;
  scm_t_ptrdiff reloc;
};

SCM_API scm_t_bits scm_tc16_vm_cont;
#define SCM_VM_CONT_P(OBJ)	SCM_SMOB_PREDICATE (scm_tc16_vm_cont, OBJ)
#define SCM_VM_CONT_DATA(CONT)	((struct scm_vm_cont *) SCM_SMOB_DATA_1 (CONT))

SCM_API SCM scm_vm_capture_continuations (void);
SCM_API void scm_vm_reinstate_continuations (SCM conts);

SCM_API SCM scm_load_compiled_with_vm (SCM file);

SCM_INTERNAL void scm_init_vm (void);

#endif /* _SCM_VM_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
