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

struct scm_vm {
  scm_byte_t *ip;		/* instruction pointer */
  SCM *sp;			/* stack pointer */
  SCM *fp;			/* frame pointer */
  size_t stack_size;		/* stack size */
  SCM *stack_base;		/* stack base address */
  SCM *stack_limit;		/* stack limit address */
  SCM hooks[SCM_VM_NUM_HOOKS];	/* hooks */
  SCM options;			/* options */
  unsigned long time;		/* time spent */
  unsigned long clock;		/* bogos clock */
  SCM trace_frame;              /* a frame being traced */
};

extern SCM scm_the_vm_fluid;

#define SCM_VM_P(x)		SCM_SMOB_PREDICATE (scm_tc16_vm, x)
#define SCM_VM_DATA(vm)		((struct scm_vm *) SCM_SMOB_DATA (vm))
#define SCM_VALIDATE_VM(pos,x)	SCM_MAKE_VALIDATE (pos, x, VM_P)

extern SCM scm_the_vm ();
extern SCM scm_make_vm (void);
extern SCM scm_vm_apply (SCM vm, SCM program, SCM args);
extern SCM scm_vm_option_ref (SCM vm, SCM key);
extern SCM scm_vm_option_set_x (SCM vm, SCM key, SCM val);

extern SCM scm_vm_version (void);
extern SCM scm_the_vm (void);
extern SCM scm_vm_p (SCM obj);
extern SCM scm_vm_ip (SCM vm);
extern SCM scm_vm_sp (SCM vm);
extern SCM scm_vm_fp (SCM vm);
extern SCM scm_vm_boot_hook (SCM vm);
extern SCM scm_vm_halt_hook (SCM vm);
extern SCM scm_vm_next_hook (SCM vm);
extern SCM scm_vm_break_hook (SCM vm);
extern SCM scm_vm_enter_hook (SCM vm);
extern SCM scm_vm_apply_hook (SCM vm);
extern SCM scm_vm_exit_hook (SCM vm);
extern SCM scm_vm_return_hook (SCM vm);
extern SCM scm_vm_option (SCM vm, SCM key);
extern SCM scm_set_vm_option_x (SCM vm, SCM key, SCM val);
extern SCM scm_vm_stats (SCM vm);
extern SCM scm_vm_trace_frame (SCM vm);

struct scm_vm_cont {
  scm_byte_t *ip;
  SCM *sp;
  SCM *fp;
  scm_t_ptrdiff stack_size;
  SCM *stack_base;
  scm_t_ptrdiff reloc;
};

extern scm_t_bits scm_tc16_vm_cont;
#define SCM_VM_CONT_P(OBJ)	SCM_SMOB_PREDICATE (scm_tc16_vm_cont, OBJ)
#define SCM_VM_CONT_DATA(CONT)	((struct scm_vm_cont *) SCM_CELL_WORD_1 (CONT))

extern SCM scm_vm_capture_continuations (void);
extern void scm_vm_reinstate_continuations (SCM conts);

extern SCM scm_load_compiled_with_vm (SCM file);

extern void scm_init_vm (void);

#endif /* _SCM_VM_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
