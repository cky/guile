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

#include <string.h>
#include "envs.h"
#include "frames.h"
#include "instructions.h"
#include "objcodes.h"
#include "programs.h"
#include "vm.h"

/* I sometimes use this for debugging. */
#define vm_puts(OBJ)				\
{						\
  scm_display (OBJ, scm_def_errp);		\
  scm_newline (scm_def_errp);			\
}


/*
 * VM Continuation
 */

scm_bits_t scm_tc16_vm_cont;


#define SCM_VM_CONT_P(OBJ)	SCM_SMOB_PREDICATE (scm_tc16_vm_cont, OBJ)
#define SCM_VM_CONT_VP(CONT)	((struct scm_vm *) SCM_CELL_WORD_1 (CONT))

static SCM
capture_vm_cont (struct scm_vm *vp)
{
  struct scm_vm *p = scm_must_malloc (sizeof (*p), "capture_vm_cont");
  p->stack_size = vp->stack_limit - vp->sp;
  p->stack_base = scm_must_malloc (p->stack_size * sizeof (SCM),
				   "capture_vm_cont");
  p->stack_limit = p->stack_base + p->stack_size - 2;
  p->ip = vp->ip;
  p->sp = (SCM *) (vp->stack_limit - vp->sp);
  p->fp = (SCM *) (vp->stack_limit - vp->fp);
  memcpy (p->stack_base, vp->sp + 1, vp->stack_size * sizeof (SCM));
  SCM_RETURN_NEWSMOB (scm_tc16_vm_cont, p);
}

static void
reinstate_vm_cont (struct scm_vm *vp, SCM cont)
{
  struct scm_vm *p = SCM_VM_CONT_VP (cont);
  if (vp->stack_size < p->stack_size)
    {
      /* puts ("FIXME: Need to expand"); */
      abort ();
    }
  vp->ip = p->ip;
  vp->sp = vp->stack_limit - (int) p->sp;
  vp->fp = vp->stack_limit - (int) p->fp;
  memcpy (vp->sp + 1, p->stack_base, p->stack_size * sizeof (SCM));
}

static SCM
vm_cont_mark (SCM obj)
{
  SCM *p;
  struct scm_vm *vp = SCM_VM_CONT_VP (obj);
  for (p = vp->stack_base; p <= vp->stack_limit; p++)
    if (SCM_NIMP (*p))
      scm_gc_mark (*p);
  return SCM_BOOL_F;
}

static scm_sizet
vm_cont_free (SCM obj)
{
  struct scm_vm *p = SCM_VM_CONT_VP (obj);
  int size = sizeof (struct scm_vm) + p->stack_size * sizeof (SCM);
  scm_must_free (p->stack_base);
  scm_must_free (p);
  return size;
}


/*
 * VM Internal functions
 */

SCM_SYMBOL (sym_vm_run, "vm-run");
SCM_SYMBOL (sym_vm_error, "vm-error");

static scm_byte_t *
vm_fetch_length (scm_byte_t *ip, size_t *lenp)
{
  /* NOTE: format defined in system/vm/conv.scm */
  *lenp = *ip++;
  if (*lenp < 254)
    return ip;
  else if (*lenp == 254)
    {
      int b1 = *ip++;
      int b2 = *ip++;
      *lenp = (b1 << 8) + b2;
    }
  else
    {
      int b1 = *ip++;
      int b2 = *ip++;
      int b3 = *ip++;
      int b4 = *ip++;
      *lenp = (b1 << 24) + (b2 << 16) + (b3 << 8) + b4;
    }
  return ip;
}

static SCM
vm_heapify_frames_1 (struct scm_vm *vp, SCM *fp, SCM *sp, SCM **destp)
{
  SCM frame;
  SCM *dl = SCM_FRAME_DYNAMIC_LINK (fp);
  SCM *src = SCM_FRAME_UPPER_ADDRESS (fp);
  SCM *dest = SCM_FRAME_LOWER_ADDRESS (fp);

  if (!dl)
    {
      /* The top frame */
      frame = scm_c_make_heap_frame (fp);
      fp = SCM_HEAP_FRAME_POINTER (frame);
      SCM_FRAME_HEAP_LINK (fp) = SCM_BOOL_T;
    }
  else
    {
      /* Child frames */
      SCM link = SCM_FRAME_HEAP_LINK (dl);
      if (!SCM_FALSEP (link))
	link = SCM_FRAME_LOWER_ADDRESS (dl)[-1]; /* self link */
      else
	link = vm_heapify_frames_1 (vp, dl, dest - 1, &dest);
      frame = scm_c_make_heap_frame (fp);
      fp = SCM_HEAP_FRAME_POINTER (frame);
      SCM_FRAME_HEAP_LINK (fp)    = link;
      SCM_FRAME_DYNAMIC_LINK (fp) = SCM_HEAP_FRAME_POINTER (link);
    }

  /* Move stack data */
  for (; src <= sp; src++, dest++)
    *dest = *src;
  *destp = dest;

  return frame;
}

static SCM
vm_heapify_frames (SCM vm)
{
  struct scm_vm *vp = SCM_VM_DATA (vm);
  if (SCM_FALSEP (SCM_FRAME_HEAP_LINK (vp->fp)))
    {
      SCM *dest;
      vp->this_frame = vm_heapify_frames_1 (vp, vp->fp, vp->sp, &dest);
      vp->fp = SCM_HEAP_FRAME_POINTER (vp->this_frame);
      vp->sp = dest - 1;
    }
  return vp->this_frame;
}


/*
 * VM
 */

#define VM_DEFAULT_STACK_SIZE	(16 * 1024)

#define VM_REGULAR_ENGINE	0
#define VM_DEBUG_ENGINE		1

#if 0
#define VM_NAME   vm_regular_engine
#define VM_ENGINE VM_REGULAR_ENGINE
#include "vm_engine.c"
#undef VM_NAME
#undef VM_ENGINE
#endif

#define VM_NAME	  vm_debug_engine
#define VM_ENGINE VM_DEBUG_ENGINE
#include "vm_engine.c"
#undef VM_NAME
#undef VM_ENGINE

scm_bits_t scm_tc16_vm;

static SCM the_vm;

static SCM
make_vm (void)
#define FUNC_NAME "make_vm"
{
  int i;
  struct scm_vm *vp = SCM_MUST_MALLOC (sizeof (struct scm_vm));
  vp->stack_size  = VM_DEFAULT_STACK_SIZE;
  vp->stack_base  = SCM_MUST_MALLOC (vp->stack_size * sizeof (SCM));
  vp->stack_limit = vp->stack_base + vp->stack_size - 3;
  vp->ip    	  = NULL;
  vp->sp    	  = vp->stack_base - 1;
  vp->fp    	  = NULL;
  vp->time        = 0;
  vp->clock       = 0;
  vp->options     = SCM_EOL;
  vp->this_frame  = SCM_BOOL_F;
  vp->last_frame  = SCM_BOOL_F;
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    vp->hooks[i] = SCM_BOOL_F;
  SCM_RETURN_NEWSMOB (scm_tc16_vm, vp);
}
#undef FUNC_NAME

static SCM
vm_mark (SCM obj)
{
  int i;
  struct scm_vm *vp = SCM_VM_DATA (obj);

  /* mark the stack conservatively */
  scm_mark_locations ((SCM_STACKITEM *) vp->stack_base,
		      sizeof (SCM) * (vp->sp - vp->stack_base + 1));

  /* mark other objects  */
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    scm_gc_mark (vp->hooks[i]);
  scm_gc_mark (vp->this_frame);
  scm_gc_mark (vp->last_frame);
  return vp->options;
}

static scm_sizet
vm_free (SCM obj)
{
  struct scm_vm *vp = SCM_VM_DATA (obj);
  int size = (sizeof (struct scm_vm) + vp->stack_size * sizeof (SCM));
  scm_must_free (vp->stack_base);
  scm_must_free (vp);
  return size;
}

SCM_SYMBOL (sym_debug, "debug");

SCM
scm_vm_apply (SCM vm, SCM program, SCM args)
#define FUNC_NAME "scm_vm_apply"
{
  SCM_VALIDATE_PROGRAM (1, program);
  return vm_run (vm, program, args);
}
#undef FUNC_NAME

/* Scheme interface */

SCM_DEFINE (scm_vm_version, "vm-version", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_version
{
  return scm_makfrom0str (VERSION);
}
#undef FUNC_NAME

SCM_DEFINE (scm_the_vm, "the-vm", 0, 0, 0,
	    (),
	    "")
#define FUNC_NAME s_scm_the_vm
{
  return the_vm;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vm_p, "vm?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_vm_p
{
  return SCM_BOOL (SCM_VM_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_vm, "make-vm", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_make_vm,
{
  return make_vm ();
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_ip, "vm:ip", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_ip
{
  SCM_VALIDATE_VM (1, vm);
  return scm_ulong2num ((unsigned long) SCM_VM_DATA (vm)->ip);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_sp, "vm:sp", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_sp
{
  SCM_VALIDATE_VM (1, vm);
  return scm_ulong2num ((unsigned long) SCM_VM_DATA (vm)->sp);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_fp, "vm:fp", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_fp
{
  SCM_VALIDATE_VM (1, vm);
  return scm_ulong2num ((unsigned long) SCM_VM_DATA (vm)->fp);
}
#undef FUNC_NAME

#define VM_DEFINE_HOOK(n)				\
{							\
  struct scm_vm *vp;					\
  SCM_VALIDATE_VM (1, vm);				\
  vp = SCM_VM_DATA (vm);				\
  if (SCM_FALSEP (vp->hooks[n]))			\
    vp->hooks[n] = scm_make_hook (SCM_MAKINUM (1));	\
  return vp->hooks[n];					\
}

SCM_DEFINE (scm_vm_boot_hook, "vm-boot-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_boot_hook
{
  VM_DEFINE_HOOK (SCM_VM_BOOT_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_halt_hook, "vm-halt-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_halt_hook
{
  VM_DEFINE_HOOK (SCM_VM_HALT_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_next_hook, "vm-next-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_next_hook
{
  VM_DEFINE_HOOK (SCM_VM_NEXT_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_break_hook, "vm-break-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_break_hook
{
  VM_DEFINE_HOOK (SCM_VM_BREAK_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_enter_hook, "vm-enter-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_enter_hook
{
  VM_DEFINE_HOOK (SCM_VM_ENTER_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_apply_hook, "vm-apply-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_apply_hook
{
  VM_DEFINE_HOOK (SCM_VM_APPLY_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_exit_hook, "vm-exit-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_exit_hook
{
  VM_DEFINE_HOOK (SCM_VM_EXIT_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_return_hook, "vm-return-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_return_hook
{
  VM_DEFINE_HOOK (SCM_VM_RETURN_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_option, "vm-option", 2, 0, 0,
	    (SCM vm, SCM key),
	    "")
#define FUNC_NAME s_scm_vm_option
{
  SCM_VALIDATE_VM (1, vm);
  return scm_assq_ref (SCM_VM_DATA (vm)->options, key);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_vm_option_x, "set-vm-option!", 3, 0, 0,
	    (SCM vm, SCM key, SCM val),
	    "")
#define FUNC_NAME s_scm_set_vm_option_x
{
  SCM_VALIDATE_VM (1, vm);
  SCM_VM_DATA (vm)->options
    = scm_assq_set_x (SCM_VM_DATA (vm)->options, key, val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_stats, "vm-stats", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_stats
{
  SCM stats;

  SCM_VALIDATE_VM (1, vm);

  stats = scm_c_make_vector (2, SCM_MAKINUM (0));
  SCM_VELTS (stats)[0] = scm_ulong2num (SCM_VM_DATA (vm)->time);
  SCM_VELTS (stats)[1] = scm_ulong2num (SCM_VM_DATA (vm)->clock);

  return stats;
}
#undef FUNC_NAME

#define VM_CHECK_RUNNING(vm) 				\
  if (!SCM_VM_DATA (vm)->ip)				\
    SCM_MISC_ERROR ("Not running", SCM_LIST1 (vm))

SCM_DEFINE (scm_vm_this_frame, "vm-this-frame", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_this_frame
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->this_frame;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_last_frame, "vm-last-frame", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_last_frame
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->last_frame;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_fetch_code, "vm-fetch-code", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_fetch_code
{
  int i;
  SCM list;
  scm_byte_t *ip;
  struct scm_instruction *p;

  SCM_VALIDATE_VM (1, vm);
  VM_CHECK_RUNNING (vm);

  ip = SCM_VM_DATA (vm)->ip;
  p = SCM_INSTRUCTION (*ip);

  list = SCM_LIST1 (scm_str2symbol (p->name));
  for (i = 1; i <= p->len; i++)
    list = scm_cons (SCM_MAKINUM (ip[i]), list);
  return scm_reverse_x (list, SCM_EOL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_fetch_stack, "vm-fetch-stack", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_fetch_stack
{
  SCM *sp;
  SCM ls = SCM_EOL;
  struct scm_vm *vp;

  SCM_VALIDATE_VM (1, vm);
  VM_CHECK_RUNNING (vm);

  vp = SCM_VM_DATA (vm);
  for (sp = vp->stack_base; sp <= vp->sp; sp++)
    ls = scm_cons (*sp, ls);
  return ls;
}
#undef FUNC_NAME


/*
 * Initialize
 */

void
scm_init_vm (void)
{
  scm_init_frames ();
  scm_init_instructions ();
  scm_init_objcodes ();
  scm_init_programs ();

  scm_tc16_vm_cont = scm_make_smob_type ("vm-cont", 0);
  scm_set_smob_mark (scm_tc16_vm_cont, vm_cont_mark);
  scm_set_smob_free (scm_tc16_vm_cont, vm_cont_free);

  scm_tc16_vm = scm_make_smob_type ("vm", 0);
  scm_set_smob_mark (scm_tc16_vm, vm_mark);
  scm_set_smob_free (scm_tc16_vm, vm_free);
  scm_set_smob_apply (scm_tc16_vm, scm_vm_apply, 1, 0, 1);

  the_vm = scm_permanent_object (make_vm ());

#ifndef SCM_MAGIC_SNARFER
#include "vm.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
