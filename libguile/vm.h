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

#ifndef _SCM_VM_H_
#define _SCM_VM_H_

#include <libguile.h>
#include <libguile/programs.h>

enum {
  SCM_VM_APPLY_HOOK,
  SCM_VM_PUSH_CONTINUATION_HOOK,
  SCM_VM_POP_CONTINUATION_HOOK,
  SCM_VM_NEXT_HOOK,
  SCM_VM_ABORT_CONTINUATION_HOOK,
  SCM_VM_RESTORE_CONTINUATION_HOOK,
  SCM_VM_NUM_HOOKS,
};

struct scm_vm;

typedef SCM (*scm_t_vm_engine) (SCM vm, SCM program, SCM *argv, int nargs);

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
  int trace_level;              /* traces enabled if trace_level > 0 */
  scm_t_int64 cookie;           /* used to detect unrewindable continuations */
};

SCM_API SCM scm_the_vm_fluid;

#define SCM_VM_P(x)		(SCM_NIMP (x) && SCM_TYP7 (x) == scm_tc7_vm)
#define SCM_VM_DATA(vm)		((struct scm_vm *) SCM_CELL_WORD_1 (vm))
#define SCM_VALIDATE_VM(pos,x)	SCM_MAKE_VALIDATE (pos, x, VM_P)

SCM_API SCM scm_the_vm (void);
SCM_API SCM scm_make_vm (void);

SCM_API SCM scm_the_vm (void);
SCM_API SCM scm_call_with_vm (SCM vm, SCM proc, SCM args);

SCM_API SCM scm_vm_p (SCM obj);
SCM_API SCM scm_vm_ip (SCM vm);
SCM_API SCM scm_vm_sp (SCM vm);
SCM_API SCM scm_vm_fp (SCM vm);
SCM_API SCM scm_vm_apply_hook (SCM vm);
SCM_API SCM scm_vm_push_continuation_hook (SCM vm);
SCM_API SCM scm_vm_pop_continuation_hook (SCM vm);
SCM_API SCM scm_vm_abort_continuation_hook (SCM vm);
SCM_API SCM scm_vm_restore_continuation_hook (SCM vm);
SCM_API SCM scm_vm_next_hook (SCM vm);
SCM_API SCM scm_vm_trace_level (SCM vm);
SCM_API SCM scm_set_vm_trace_level_x (SCM vm, SCM level);
SCM_API SCM scm_vm_engine (SCM vm);
SCM_API SCM scm_set_vm_engine_x (SCM vm, SCM engine);
SCM_API SCM scm_set_default_vm_engine_x (SCM engine);
SCM_API void scm_c_set_vm_engine_x (SCM vm, int engine);
SCM_API void scm_c_set_default_vm_engine_x (int engine);

#define SCM_F_VM_CONT_PARTIAL 0x1
#define SCM_F_VM_CONT_REWINDABLE 0x2

struct scm_vm_cont {
  SCM *sp;
  SCM *fp;
  scm_t_uint8 *ra, *mvra;
  scm_t_ptrdiff stack_size;
  SCM *stack_base;
  scm_t_ptrdiff reloc;
  scm_t_uint32 flags;
};

#define SCM_VM_CONT_P(OBJ)	(SCM_NIMP (OBJ) && SCM_TYP7 (OBJ) == scm_tc7_vm_cont)
#define SCM_VM_CONT_DATA(CONT)	((struct scm_vm_cont *) SCM_CELL_WORD_1 (CONT))
#define SCM_VM_CONT_PARTIAL_P(CONT) (SCM_VM_CONT_DATA (CONT)->flags & SCM_F_VM_CONT_PARTIAL)
#define SCM_VM_CONT_REWINDABLE_P(CONT) (SCM_VM_CONT_DATA (CONT)->flags & SCM_F_VM_CONT_REWINDABLE)

SCM_API SCM scm_load_compiled_with_vm (SCM file);

SCM_INTERNAL SCM scm_c_vm_run (SCM vm, SCM program, SCM *argv, int nargs);

SCM_INTERNAL void scm_i_vm_print (SCM x, SCM port,
                                  scm_print_state *pstate);
SCM_INTERNAL SCM scm_i_vm_capture_continuation (SCM vm);
SCM_INTERNAL SCM scm_i_vm_capture_stack (SCM *stack_base, SCM *fp, SCM *sp,
                                         scm_t_uint8 *ra, scm_t_uint8 *mvra,
                                         scm_t_uint32 flags);
SCM_INTERNAL void scm_i_vm_cont_print (SCM x, SCM port,
                                       scm_print_state *pstate);
SCM_INTERNAL void scm_bootstrap_vm (void);
SCM_INTERNAL void scm_init_vm (void);

#endif /* _SCM_VM_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
