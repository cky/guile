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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <alloca.h>
#include <string.h>
#include "vm-bootstrap.h"
#include "frames.h"
#include "instructions.h"
#include "objcodes.h"
#include "programs.h"
#include "lang.h" /* NULL_OR_NIL_P */
#include "vm.h"

/* I sometimes use this for debugging. */
#define vm_puts(OBJ)				\
{						\
  scm_display (OBJ, scm_current_error_port ()); \
  scm_newline (scm_current_error_port ());      \
}

/* The VM has a number of internal assertions that shouldn't normally be
   necessary, but might be if you think you found a bug in the VM. */
#define VM_ENABLE_ASSERTIONS

/* We can add a mode that ensures that all stack items above the stack pointer
   are NULL. This is useful for checking the internal consistency of the VM's
   assumptions and its operators, but isn't necessary for normal operation. It
   will ensure that assertions are enabled. Slows down the VM by about 30%. */
/* NB! If you enable this, search for NULLING in throw.c */
/* #define VM_ENABLE_STACK_NULLING */

/* #define VM_ENABLE_PARANOID_ASSERTIONS */

#if defined (VM_ENABLE_STACK_NULLING) && !defined (VM_ENABLE_ASSERTIONS)
#define VM_ENABLE_ASSERTIONS
#endif


/*
 * VM Continuation
 */

scm_t_bits scm_tc16_vm_cont;

static void
vm_mark_stack (SCM *base, scm_t_ptrdiff size, SCM *fp, scm_t_ptrdiff reloc)
{
  SCM *sp, *upper, *lower;
  sp = base + size - 1;

  while (sp > base && fp) 
    {
      upper = SCM_FRAME_UPPER_ADDRESS (fp);
      lower = SCM_FRAME_LOWER_ADDRESS (fp);

      for (; sp >= upper; sp--)
        if (SCM_NIMP (*sp)) 
          {
            if (scm_in_heap_p (*sp))
              scm_gc_mark (*sp);
            else
              fprintf (stderr, "BADNESS: crap on the stack: %p\n", *sp);
          }
      

      /* skip ra, mvra */
      sp -= 2;

      /* update fp from the dynamic link */
      fp = (SCM*)*sp-- + reloc;

      /* mark from the el down to the lower address */
      for (; sp >= lower; sp--)
        if (*sp && SCM_NIMP (*sp))
          scm_gc_mark (*sp);
    }
}

static SCM
vm_cont_mark (SCM obj)
{
  struct scm_vm_cont *p = SCM_VM_CONT_DATA (obj);

  if (p->stack_size)
    vm_mark_stack (p->stack_base, p->stack_size, p->fp + p->reloc, p->reloc);

  return SCM_BOOL_F;
}

static scm_sizet
vm_cont_free (SCM obj)
{
  struct scm_vm_cont *p = SCM_VM_CONT_DATA (obj);

  scm_gc_free (p->stack_base, p->stack_size * sizeof (SCM), "stack-base");
  scm_gc_free (p, sizeof (struct scm_vm), "vm");

  return 0;
}

static SCM
capture_vm_cont (struct scm_vm *vp)
{
  struct scm_vm_cont *p = scm_gc_malloc (sizeof (*p), "capture_vm_cont");
  p->stack_size = vp->sp - vp->stack_base + 1;
  p->stack_base = scm_gc_malloc (p->stack_size * sizeof (SCM),
				 "capture_vm_cont");
#ifdef VM_ENABLE_STACK_NULLING
  if (vp->sp >= vp->stack_base)
    if (!vp->sp[0] || vp->sp[1])
      abort ();
  memset (p->stack_base, 0, p->stack_size * sizeof (SCM));
#endif
  p->ip = vp->ip;
  p->sp = vp->sp;
  p->fp = vp->fp;
  memcpy (p->stack_base, vp->stack_base, p->stack_size * sizeof (SCM));
  p->reloc = p->stack_base - vp->stack_base;
  SCM_RETURN_NEWSMOB (scm_tc16_vm_cont, p);
}

static void
reinstate_vm_cont (struct scm_vm *vp, SCM cont)
{
  struct scm_vm_cont *p = SCM_VM_CONT_DATA (cont);
  if (vp->stack_size < p->stack_size)
    {
      /* puts ("FIXME: Need to expand"); */
      abort ();
    }
#ifdef VM_ENABLE_STACK_NULLING
  {
    scm_t_ptrdiff nzero = (vp->sp - p->sp);
    if (nzero > 0)
      memset (vp->stack_base + p->stack_size, 0, nzero * sizeof (SCM));
    /* actually nzero should always be negative, because vm_reset_stack will
       unwind the stack to some point *below* this continuation */
  }
#endif
  vp->ip = p->ip;
  vp->sp = p->sp;
  vp->fp = p->fp;
  memcpy (vp->stack_base, p->stack_base, p->stack_size * sizeof (SCM));
}

/* In theory, a number of vm instances can be active in the call trace, and we
   only want to reify the continuations of those in the current continuation
   root. I don't see a nice way to do this -- ideally it would involve dynwinds,
   and previous values of the *the-vm* fluid within the current continuation
   root. But we don't have access to continuation roots in the dynwind stack.
   So, just punt for now -- take the current value of *the-vm*.

   While I'm on the topic, ideally we could avoid copying the C stack if the
   continuation root is inside VM code, and call/cc was invoked within that same
   call to vm_run; but that's currently not implemented.
 */
SCM
scm_vm_capture_continuations (void)
{
  SCM vm = scm_the_vm ();
  return scm_acons (vm, capture_vm_cont (SCM_VM_DATA (vm)), SCM_EOL);
}

void
scm_vm_reinstate_continuations (SCM conts)
{
  for (; conts != SCM_EOL; conts = SCM_CDR (conts))
    reinstate_vm_cont (SCM_VM_DATA (SCM_CAAR (conts)), SCM_CDAR (conts));
}

static void enfalsen_frame (void *p)
{ 
  struct scm_vm *vp = p;
  vp->trace_frame = SCM_BOOL_F;
}

static void
vm_dispatch_hook (struct scm_vm *vp, SCM hook, SCM hook_args)
{
  if (!SCM_FALSEP (vp->trace_frame))
    return;
  
  scm_dynwind_begin (0);
  // FIXME, stack holder should be the vm
  vp->trace_frame = scm_c_make_vm_frame (SCM_BOOL_F, vp->fp, vp->sp, vp->ip, 0);
  scm_dynwind_unwind_handler (enfalsen_frame, vp, SCM_F_WIND_EXPLICITLY);

  scm_c_run_hook (hook, hook_args);

  scm_dynwind_end ();
}


/*
 * VM Internal functions
 */

static SCM sym_vm_run;
static SCM sym_vm_error;
static SCM sym_debug;

static SCM make_u8vector (const scm_t_uint8 *bytes, size_t len)
{
  scm_t_uint8 *new_bytes = scm_gc_malloc (len, "make-u8vector");
  memcpy (new_bytes, bytes, len);
  return scm_take_u8vector (new_bytes, len);
}

static SCM
really_make_boot_program (long nargs)
{
  scm_byte_t bytes[] = {0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        scm_op_mv_call, 0, 0, 1, scm_op_make_int8_1, scm_op_halt};
  ((scm_t_uint32*)bytes)[1] = 6; /* set len in current endianness, no meta */
  if (SCM_UNLIKELY (nargs > 255 || nargs < 0))
    abort ();
  bytes[13] = (scm_byte_t)nargs;
  return scm_make_program (scm_bytecode_to_objcode (make_u8vector (bytes, sizeof(bytes))),
                           SCM_BOOL_F, SCM_EOL);
}
#define NUM_BOOT_PROGS 8
static SCM
vm_make_boot_program (long nargs)
{
  static SCM programs[NUM_BOOT_PROGS] = { 0, };

  if (SCM_UNLIKELY (!programs[0])) 
    {
      int i;
      for (i = 0; i < NUM_BOOT_PROGS; i++)
        programs[i] = scm_permanent_object (really_make_boot_program (i));
    }
  
  if (SCM_LIKELY (nargs < NUM_BOOT_PROGS))
    return programs[nargs];
  else
    return really_make_boot_program (nargs);
}


/*
 * VM
 */

#define VM_DEFAULT_STACK_SIZE	(16 * 1024)

#define VM_NAME   vm_regular_engine
#define FUNC_NAME "vm-regular-engine"
#define VM_ENGINE SCM_VM_REGULAR_ENGINE
#include "vm-engine.c"
#undef VM_NAME
#undef FUNC_NAME
#undef VM_ENGINE

#define VM_NAME	  vm_debug_engine
#define FUNC_NAME "vm-debug-engine"
#define VM_ENGINE SCM_VM_DEBUG_ENGINE
#include "vm-engine.c"
#undef VM_NAME
#undef FUNC_NAME
#undef VM_ENGINE

static const scm_t_vm_engine vm_engines[] = 
  { vm_regular_engine, vm_debug_engine };

scm_t_bits scm_tc16_vm;

static SCM
make_vm (void)
#define FUNC_NAME "make_vm"
{
  int i;

  if (!scm_tc16_vm)
    return SCM_BOOL_F; /* not booted yet */

  struct scm_vm *vp = scm_gc_malloc (sizeof (struct scm_vm), "vm");

  vp->stack_size  = VM_DEFAULT_STACK_SIZE;
  vp->stack_base  = scm_gc_malloc (vp->stack_size * sizeof (SCM),
				   "stack-base");
#ifdef VM_ENABLE_STACK_NULLING
  memset (vp->stack_base, 0, vp->stack_size * sizeof (SCM));
#endif
  vp->stack_limit = vp->stack_base + vp->stack_size - 3;
  vp->ip    	  = NULL;
  vp->sp    	  = vp->stack_base - 1;
  vp->fp    	  = NULL;
  vp->engine      = SCM_VM_DEBUG_ENGINE;
  vp->time        = 0;
  vp->clock       = 0;
  vp->options     = SCM_EOL;
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    vp->hooks[i] = SCM_BOOL_F;
  vp->trace_frame = SCM_BOOL_F;
  SCM_RETURN_NEWSMOB (scm_tc16_vm, vp);
}
#undef FUNC_NAME

static SCM
vm_mark (SCM obj)
{
  int i;
  struct scm_vm *vp = SCM_VM_DATA (obj);

#ifdef VM_ENABLE_STACK_NULLING
  if (vp->sp >= vp->stack_base)
    if (!vp->sp[0] || vp->sp[1])
      abort ();
#endif

  /* mark the stack, precisely */
  vm_mark_stack (vp->stack_base, vp->sp + 1 - vp->stack_base, vp->fp, 0);

  /* mark other objects  */
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    scm_gc_mark (vp->hooks[i]);

  scm_gc_mark (vp->trace_frame);

  return vp->options;
}

static scm_sizet
vm_free (SCM obj)
{
  struct scm_vm *vp = SCM_VM_DATA (obj);

  scm_gc_free (vp->stack_base, vp->stack_size * sizeof (SCM),
	       "stack-base");
  scm_gc_free (vp, sizeof (struct scm_vm), "vm");

  return 0;
}

SCM
scm_c_vm_run (SCM vm, SCM program, SCM *argv, int nargs)
{
  struct scm_vm *vp = SCM_VM_DATA (vm);
  return vm_engines[vp->engine](vp, program, argv, nargs);
}

SCM
scm_vm_apply (SCM vm, SCM program, SCM args)
#define FUNC_NAME "scm_vm_apply"
{
  SCM *argv;
  int i, nargs;
  
  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_PROGRAM (2, program);

  nargs = scm_ilength (args);
  if (SCM_UNLIKELY (nargs < 0))
    scm_wrong_type_arg_msg (FUNC_NAME, 3, args, "list");
  
  argv = alloca(nargs * sizeof(SCM));
  for (i = 0; i < nargs; i++)
    {
      argv[i] = SCM_CAR (args);
      args = SCM_CDR (args);
    }

  return scm_c_vm_run (vm, program, argv, nargs);
}
#undef FUNC_NAME

/* Scheme interface */

SCM_DEFINE (scm_vm_version, "vm-version", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_version
{
  return scm_from_locale_string (PACKAGE_VERSION);
}
#undef FUNC_NAME

SCM_DEFINE (scm_the_vm, "the-vm", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_the_vm
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (SCM_UNLIKELY (SCM_FALSEP ((t->vm))))
    t->vm = make_vm ();

  return t->vm;
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
  return scm_from_ulong ((unsigned long) SCM_VM_DATA (vm)->ip);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_sp, "vm:sp", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_sp
{
  SCM_VALIDATE_VM (1, vm);
  return scm_from_ulong ((unsigned long) SCM_VM_DATA (vm)->sp);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_fp, "vm:fp", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_fp
{
  SCM_VALIDATE_VM (1, vm);
  return scm_from_ulong ((unsigned long) SCM_VM_DATA (vm)->fp);
}
#undef FUNC_NAME

#define VM_DEFINE_HOOK(n)				\
{							\
  struct scm_vm *vp;					\
  SCM_VALIDATE_VM (1, vm);				\
  vp = SCM_VM_DATA (vm);				\
  if (SCM_FALSEP (vp->hooks[n]))			\
    vp->hooks[n] = scm_make_hook (SCM_I_MAKINUM (1));	\
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

  stats = scm_make_vector (SCM_I_MAKINUM (2), SCM_UNSPECIFIED);
  scm_vector_set_x (stats, SCM_I_MAKINUM (0),
		    scm_from_ulong (SCM_VM_DATA (vm)->time));
  scm_vector_set_x (stats, SCM_I_MAKINUM (1),
		    scm_from_ulong (SCM_VM_DATA (vm)->clock));

  return stats;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_trace_frame, "vm-trace-frame", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_trace_frame
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->trace_frame;
}
#undef FUNC_NAME


/*
 * Initialize
 */

SCM scm_load_compiled_with_vm (SCM file)
{
  SCM program = scm_make_program (scm_load_objcode (file),
                                  SCM_BOOL_F, SCM_EOL);
  
  return scm_c_vm_run (scm_the_vm (), program, NULL, 0);
}

void
scm_bootstrap_vm (void)
{
  static int strappage = 0;
  
  if (strappage)
    return;

  scm_bootstrap_frames ();
  scm_bootstrap_instructions ();
  scm_bootstrap_objcodes ();
  scm_bootstrap_programs ();

  scm_tc16_vm_cont = scm_make_smob_type ("vm-cont", 0);
  scm_set_smob_mark (scm_tc16_vm_cont, vm_cont_mark);
  scm_set_smob_free (scm_tc16_vm_cont, vm_cont_free);

  scm_tc16_vm = scm_make_smob_type ("vm", 0);
  scm_set_smob_mark (scm_tc16_vm, vm_mark);
  scm_set_smob_free (scm_tc16_vm, vm_free);
  scm_set_smob_apply (scm_tc16_vm, scm_vm_apply, 1, 0, 1);

  scm_c_define ("load-compiled",
                scm_c_make_gsubr ("load-compiled/vm", 1, 0, 0,
                                  scm_load_compiled_with_vm));

  sym_vm_run = scm_permanent_object (scm_from_locale_symbol ("vm-run"));
  sym_vm_error = scm_permanent_object (scm_from_locale_symbol ("vm-error"));
  sym_debug = scm_permanent_object (scm_from_locale_symbol ("debug"));

  strappage = 1;
}

void
scm_init_vm (void)
{
  scm_bootstrap_vm ();

#ifndef SCM_MAGIC_SNARFER
#include "vm.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
