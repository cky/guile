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

#include <string.h>
#include "instructions.h"
#include "programs.h"
#include "envs.h"
#include "vm.h"

/* I sometimes use this for debugging. */
#define vm_puts(OBJ)				\
{						\
  scm_display (OBJ, scm_def_errp);		\
  scm_newline (scm_def_errp);			\
}


/*
 * VM Debug frame
 */

scm_bits_t scm_tc16_vm_debug_frame;

static SCM
make_vm_debug_frame (SCM *fp)
{
  int i, size;
  struct scm_vm_debug_frame *p;

  if (!fp)
    return SCM_BOOL_F;

  p = scm_must_malloc (sizeof (struct scm_vm_debug_frame), "make_vm_debug_frame");
  p->program      = SCM_VM_FRAME_PROGRAM (fp);
  p->dynamic_link = make_vm_debug_frame (SCM_VM_FRAME_ADDRESS
					 (SCM_VM_FRAME_DYNAMIC_LINK (fp)));

  size = SCM_PROGRAM_NARGS (p->program) + SCM_PROGRAM_NLOCS (p->program);
  p->variables = scm_make_vector (SCM_MAKINUM (size), SCM_BOOL_F);
  for (i = 0; i < size; i++)
    SCM_VELTS (p->variables)[i] = SCM_VM_FRAME_VARIABLE (fp, i);

  SCM_RETURN_NEWSMOB (scm_tc16_vm_debug_frame, p);
}

static SCM
vm_debug_frame_mark (SCM obj)
{
  scm_gc_mark (SCM_VM_DEBUG_FRAME_PROGRAM (obj));
  scm_gc_mark (SCM_VM_DEBUG_FRAME_VARIABLES (obj));
  return SCM_VM_DEBUG_FRAME_DYNAMIC_LINK (obj);
}

/* Scheme interface */

SCM_DEFINE (scm_frame_p, "frame?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_frame_p
{
  return SCM_BOOL (SCM_VM_DEBUG_FRAME_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_program, "frame-program", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_program
{
  SCM_VALIDATE_VM_DEBUG_FRAME (1, frame);
  return SCM_VM_DEBUG_FRAME_PROGRAM (frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_variables, "frame-variables", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_variables
{
  SCM_VALIDATE_VM_DEBUG_FRAME (1, frame);
  return SCM_VM_DEBUG_FRAME_VARIABLES (frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_dynamic_link, "frame-dynamic-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_dynamic_link
{
  SCM_VALIDATE_VM_DEBUG_FRAME (1, frame);
  return SCM_VM_DEBUG_FRAME_DYNAMIC_LINK (frame);
}
#undef FUNC_NAME


/*
 * VM Continuation
 */

scm_bits_t scm_tc16_vm_cont;


#define SCM_VM_CONT_P(OBJ)	SCM_SMOB_PREDICATE (scm_tc16_vm_cont, OBJ)
#define SCM_VM_CONT_VMP(CONT)	((struct scm_vm *) SCM_CELL_WORD_1 (CONT))

static SCM
capture_vm_cont (struct scm_vm *vmp)
{
  struct scm_vm *p = scm_must_malloc (sizeof (*p), "capture_vm_cont");
  p->stack_size = vmp->stack_limit - vmp->sp;
  p->stack_base = scm_must_malloc (p->stack_size * sizeof (SCM),
				   "capture_vm_cont");
  p->stack_limit = p->stack_base + p->stack_size - 2;
  p->ip = vmp->ip;
  p->sp = (SCM *) (vmp->stack_limit - vmp->sp);
  p->fp = (SCM *) (vmp->stack_limit - vmp->fp);
  memcpy (p->stack_base, vmp->sp + 1, vmp->stack_size * sizeof (SCM));
  SCM_RETURN_NEWSMOB (scm_tc16_vm_cont, p);
}

static void
reinstate_vm_cont (struct scm_vm *vmp, SCM cont)
{
  struct scm_vm *p = SCM_VM_CONT_VMP (cont);
  if (vmp->stack_size < p->stack_size)
    {
      /* puts ("FIXME: Need to expand"); */
      abort ();
    }
  vmp->ip = p->ip;
  vmp->sp = vmp->stack_limit - (int) p->sp;
  vmp->fp = vmp->stack_limit - (int) p->fp;
  memcpy (vmp->sp + 1, p->stack_base, p->stack_size * sizeof (SCM));
}

static SCM
vm_cont_mark (SCM obj)
{
  SCM *p;
  struct scm_vm *vmp = SCM_VM_CONT_VMP (obj);
  for (p = vmp->stack_base; p <= vmp->stack_limit; p++)
    if (SCM_NIMP (*p))
      scm_gc_mark (*p);
  return SCM_BOOL_F;
}

static scm_sizet
vm_cont_free (SCM obj)
{
  struct scm_vm *p = SCM_VM_CONT_VMP (obj);
  int size = sizeof (struct scm_vm) + p->stack_size * sizeof (SCM);
  scm_must_free (p->stack_base);
  scm_must_free (p);
  return size;
}


/*
 * VM Internal functions
 */

SCM_SYMBOL (sym_vm_engine, "vm-engine");
SCM_SYMBOL (sym_vm_error, "vm-error");

static scm_byte_t *
vm_fetch_length (scm_byte_t *ip, size_t *lenp)
{
  *lenp = *ip++;
  if (*lenp < 254)
    return ip;
  else if (*lenp == 254)
    *lenp = (*ip++ << 8) + *ip++;
  else
    *lenp = (*ip++ << 24) + (*ip++ << 16) + (*ip++ << 8) + *ip++;
  return ip;
}


/*
 * VM
 */

#define VM_DEFAULT_STACK_SIZE	(16 * 1024)
#define VM_MAXIMUM_STACK_SIZE	(128 * 1024)

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

static SCM
make_vm (void)
#define FUNC_NAME "make_vm"
{
  int i;
  struct scm_vm *vmp = SCM_MUST_MALLOC (sizeof (struct scm_vm));
  vmp->stack_size  = VM_DEFAULT_STACK_SIZE;
  vmp->stack_base  = SCM_MUST_MALLOC (vmp->stack_size * sizeof (SCM));
  vmp->stack_limit = vmp->stack_base + vmp->stack_size - 1;
  vmp->ip    	   = NULL;
  vmp->sp    	   = vmp->stack_limit;
  vmp->fp    	   = NULL;
  vmp->cons        = 0;
  vmp->time        = 0;
  vmp->clock       = 0;
  vmp->options     = SCM_EOL;
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    vmp->hooks[i] = SCM_BOOL_F;
  SCM_RETURN_NEWSMOB (scm_tc16_vm, vmp);
}
#undef FUNC_NAME

static SCM
vm_mark (SCM obj)
{
  int i;
  SCM *sp, *fp;
  struct scm_vm *vmp = SCM_VM_DATA (obj);

  /* Mark the stack */
  sp = vmp->sp;
  fp = vmp->fp;
  while (fp)
    {
      SCM *upper = SCM_VM_FRAME_UPPER_ADDRESS (fp);
      SCM *lower = SCM_VM_FRAME_LOWER_ADDRESS (fp);
      /* Mark intermediate data */
      for (; sp < lower; sp++)
	if (SCM_NIMP (*sp))
	  scm_gc_mark (*sp);
      /* Mark frame data */
      scm_gc_mark (SCM_VM_FRAME_PROGRAM (fp));
      /* Mark frame variables */
      for (sp = fp; sp < upper; sp++)
	if (SCM_NIMP (*sp))
	  scm_gc_mark (*sp);
      fp = SCM_VM_FRAME_ADDRESS (SCM_VM_FRAME_DYNAMIC_LINK (fp));
    }

  /* Mark the options */
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    scm_gc_mark (vmp->hooks[i]);
  return vmp->options;
}

static scm_sizet
vm_free (SCM obj)
{
  struct scm_vm *vmp = SCM_VM_DATA (obj);
  int size = (sizeof (struct scm_vm) + vmp->stack_size * sizeof (SCM));
  scm_must_free (vmp->stack_base);
  scm_must_free (vmp);
  return size;
}

SCM_SYMBOL (sym_debug, "debug");

SCM
scm_vm_apply (SCM vm, SCM program, SCM args)
#define FUNC_NAME "scm_vm_apply"
{
  SCM_VALIDATE_PROGRAM (1, program);
  return vm_engine (vm, program, args);
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
  struct scm_vm *vmp;					\
  SCM_VALIDATE_VM (1, vm);				\
  vmp = SCM_VM_DATA (vm);				\
  if (SCM_FALSEP (vmp->hooks[n]))			\
    vmp->hooks[n] = scm_make_hook (SCM_MAKINUM (1));	\
  return vmp->hooks[n];					\
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

  stats = scm_c_make_vector (3, SCM_MAKINUM (0));
  SCM_VELTS (stats)[0] = scm_long2num (SCM_VM_DATA (vm)->cons);
  SCM_VELTS (stats)[1] = scm_long2num (SCM_VM_DATA (vm)->time);
  SCM_VELTS (stats)[2] = scm_long2num (SCM_VM_DATA (vm)->clock);

  return stats;
}
#undef FUNC_NAME

#define VM_CHECK_RUNNING(vm) 				\
  if (!SCM_VM_DATA (vm)->ip)				\
    SCM_MISC_ERROR ("Not running", SCM_LIST1 (vm))

SCM_DEFINE (scm_vm_current_frame, "vm-current-frame", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_current_frame
{
  SCM_VALIDATE_VM (1, vm);
  VM_CHECK_RUNNING (vm);
  return make_vm_debug_frame (SCM_VM_DATA (vm)->fp);
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
  SCM *p;
  SCM list = SCM_EOL;

  SCM_VALIDATE_VM (1, vm);
  VM_CHECK_RUNNING (vm);

  if (SCM_VM_DATA (vm)->fp)
    for (p = SCM_VM_FRAME_LOWER_ADDRESS (SCM_VM_DATA (vm)->fp) - 1;
	 p >= SCM_VM_DATA (vm)->sp;
	 p--)
      list = scm_cons (*p, list);
  return list;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_load, "vm-load", 2, 0, 0,
	    (SCM vm, SCM bytes),
	    "")
#define FUNC_NAME s_scm_vm_load
{
  SCM prog;

  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_STRING (2, bytes);

  prog = scm_c_make_program (SCM_STRING_CHARS (bytes),
			     SCM_STRING_LENGTH (bytes),
			     bytes);
  return scm_vm_apply (vm, prog, SCM_EOL);
}
#undef FUNC_NAME


/*
 * Initialize
 */

void
scm_init_vm (void)
{
  SCM mod = scm_resolve_module (scm_read_0str ("(system vm core)"));
  mod = scm_set_current_module (mod);

  scm_init_instructions ();
  scm_init_programs ();

  scm_tc16_vm_debug_frame = scm_make_smob_type ("vm_frame", 0);
  scm_set_smob_mark (scm_tc16_vm_debug_frame, vm_debug_frame_mark);

  scm_tc16_vm_cont = scm_make_smob_type ("vm-cont", 0);
  scm_set_smob_mark (scm_tc16_vm_cont, vm_cont_mark);
  scm_set_smob_free (scm_tc16_vm_cont, vm_cont_free);

  scm_tc16_vm = scm_make_smob_type ("vm", 0);
  scm_set_smob_mark (scm_tc16_vm, vm_mark);
  scm_set_smob_free (scm_tc16_vm, vm_free);
  scm_set_smob_apply (scm_tc16_vm, scm_vm_apply, 1, 0, 1);

#ifndef SCM_MAGIC_SNARFER
#include "vm.x"
#endif

  scm_set_current_module (mod);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
