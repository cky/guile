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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <alloca.h>
#include <alignof.h>
#include <string.h>
#include <stdint.h>

#include "libguile/bdw-gc.h"
#include <gc/gc_mark.h>

#include "_scm.h"
#include "control.h"
#include "frames.h"
#include "instructions.h"
#include "objcodes.h"
#include "programs.h"
#include "vm.h"

static int vm_default_engine = SCM_VM_REGULAR_ENGINE;

/* Unfortunately we can't snarf these: snarfed things are only loaded up from
   (system vm vm), which might not be loaded before an error happens. */
static SCM sym_vm_run;
static SCM sym_vm_error;
static SCM sym_keyword_argument_error;
static SCM sym_regular;
static SCM sym_debug;

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

/* When defined, arrange so that the GC doesn't scan the VM stack beyond its
   current SP.  This should help avoid excess data retention.  See
   http://thread.gmane.org/gmane.comp.programming.garbage-collection.boehmgc/3001
   for a discussion.  */
#define VM_ENABLE_PRECISE_STACK_GC_SCAN

/* Size in SCM objects of the stack reserve.  The reserve is used to run
   exception handling code in case of a VM stack overflow.  */
#define VM_STACK_RESERVE_SIZE  512



/*
 * VM Continuation
 */

void
scm_i_vm_cont_print (SCM x, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<vm-continuation ", port);
  scm_uintprint (SCM_UNPACK (x), 16, port);
  scm_puts (">", port);
}

/* In theory, a number of vm instances can be active in the call trace, and we
   only want to reify the continuations of those in the current continuation
   root. I don't see a nice way to do this -- ideally it would involve dynwinds,
   and previous values of the *the-vm* fluid within the current continuation
   root. But we don't have access to continuation roots in the dynwind stack.
   So, just punt for now, we just capture the continuation for the current VM.

   While I'm on the topic, ideally we could avoid copying the C stack if the
   continuation root is inside VM code, and call/cc was invoked within that same
   call to vm_run; but that's currently not implemented.
 */
SCM
scm_i_vm_capture_stack (SCM *stack_base, SCM *fp, SCM *sp, scm_t_uint8 *ra,
                        scm_t_uint8 *mvra, scm_t_uint32 flags)
{
  struct scm_vm_cont *p;

  p = scm_gc_malloc (sizeof (*p), "capture_vm_cont");
  p->stack_size = sp - stack_base + 1;
  p->stack_base = scm_gc_malloc (p->stack_size * sizeof (SCM),
				 "capture_vm_cont");
#if defined(VM_ENABLE_STACK_NULLING) && 0
  /* Tail continuations leave their frame on the stack for subsequent
     application, but don't capture the frame -- so there are some elements on
     the stack then, and this check doesn't work, so disable it for now. */
  if (sp >= vp->stack_base)
    if (!vp->sp[0] || vp->sp[1])
      abort ();
  memset (p->stack_base, 0, p->stack_size * sizeof (SCM));
#endif
  p->ra = ra;
  p->mvra = mvra;
  p->sp = sp;
  p->fp = fp;
  memcpy (p->stack_base, stack_base, (sp + 1 - stack_base) * sizeof (SCM));
  p->reloc = p->stack_base - stack_base;
  p->flags = flags;
  return scm_cell (scm_tc7_vm_cont, (scm_t_bits)p);
}

static void
vm_return_to_continuation (SCM vm, SCM cont, size_t n, SCM *argv)
{
  struct scm_vm *vp;
  struct scm_vm_cont *cp;
  SCM *argv_copy;

  argv_copy = alloca (n * sizeof(SCM));
  memcpy (argv_copy, argv, n * sizeof(SCM));

  vp = SCM_VM_DATA (vm);
  cp = SCM_VM_CONT_DATA (cont);

  if (n == 0 && !cp->mvra)
    scm_misc_error (NULL, "Too few values returned to continuation",
                    SCM_EOL);

  if (vp->stack_size < cp->stack_size + n + 1)
    scm_misc_error ("vm-engine", "not enough space to reinstate continuation",
                    scm_list_2 (vm, cont));

#ifdef VM_ENABLE_STACK_NULLING
  {
    scm_t_ptrdiff nzero = (vp->sp - cp->sp);
    if (nzero > 0)
      memset (vp->stack_base + cp->stack_size, 0, nzero * sizeof (SCM));
    /* actually nzero should always be negative, because vm_reset_stack will
       unwind the stack to some point *below* this continuation */
  }
#endif
  vp->sp = cp->sp;
  vp->fp = cp->fp;
  memcpy (vp->stack_base, cp->stack_base, cp->stack_size * sizeof (SCM));

  if (n == 1 || !cp->mvra)
    {
      vp->ip = cp->ra;
      vp->sp++;
      *vp->sp = argv_copy[0];
    }
  else
    {
      size_t i;
      for (i = 0; i < n; i++)
        {
          vp->sp++;
          *vp->sp = argv_copy[i];
        }
      vp->sp++;
      *vp->sp = scm_from_size_t (n);
      vp->ip = cp->mvra;
    }
}

SCM
scm_i_vm_capture_continuation (SCM vm)
{
  struct scm_vm *vp = SCM_VM_DATA (vm);
  return scm_i_vm_capture_stack (vp->stack_base, vp->fp, vp->sp, vp->ip, NULL, 0);
}

static void
vm_dispatch_hook (SCM vm, int hook_num)
{
  struct scm_vm *vp;
  SCM hook;
  struct scm_frame c_frame;
  scm_t_cell *frame;
  SCM args[1];
  int saved_trace_level;

  vp = SCM_VM_DATA (vm);
  hook = vp->hooks[hook_num];

  if (SCM_LIKELY (scm_is_false (hook))
      || scm_is_null (SCM_HOOK_PROCEDURES (hook)))
    return;

  saved_trace_level = vp->trace_level;
  vp->trace_level = 0;

  /* Allocate a frame object on the stack.  This is more efficient than calling
     `scm_c_make_frame ()' to allocate on the heap, but it forces hooks to not
     capture frame objects.

     At the same time, procedures such as `frame-procedure' make sense only
     while the stack frame represented by the frame object is visible, so it
     seems reasonable to limit the lifetime of frame objects.  */

  c_frame.stack_holder = vm;
  c_frame.fp = vp->fp;
  c_frame.sp = vp->sp;
  c_frame.ip = vp->ip;
  c_frame.offset = 0;

  /* Arrange for FRAME to be 8-byte aligned, like any other cell.  */
  frame = alloca (sizeof (*frame) + 8);
  frame = (scm_t_cell *) ROUND_UP ((scm_t_uintptr) frame, 8UL);

  frame->word_0 = SCM_PACK (scm_tc7_frame);
  frame->word_1 = PTR2SCM (&c_frame);
  args[0] = PTR2SCM (frame);

  scm_c_run_hookn (hook, args, 1);

  vp->trace_level = saved_trace_level;
}

static void vm_abort (SCM vm, size_t n, scm_t_int64 cookie) SCM_NORETURN;
static void
vm_abort (SCM vm, size_t n, scm_t_int64 vm_cookie)
{
  size_t i;
  ssize_t tail_len;
  SCM tag, tail, *argv;
  
  /* FIXME: VM_ENABLE_STACK_NULLING */
  tail = *(SCM_VM_DATA (vm)->sp--);
  /* NULLSTACK (1) */
  tail_len = scm_ilength (tail);
  if (tail_len < 0)
    scm_misc_error ("vm-engine", "tail values to abort should be a list",
                    scm_list_1 (tail));

  tag = SCM_VM_DATA (vm)->sp[-n];
  argv = alloca ((n + tail_len) * sizeof (SCM));
  for (i = 0; i < n; i++)
    argv[i] = SCM_VM_DATA (vm)->sp[-(n-1-i)];
  for (; i < n + tail_len; i++, tail = scm_cdr (tail))
    argv[i] = scm_car (tail);
  /* NULLSTACK (n + 1) */
  SCM_VM_DATA (vm)->sp -= n + 1;

  scm_c_abort (vm, tag, n + tail_len, argv, vm_cookie);
}

static void
vm_reinstate_partial_continuation (SCM vm, SCM cont, SCM intwinds,
                                   size_t n, SCM *argv, scm_t_int64 vm_cookie)
{
  struct scm_vm *vp;
  struct scm_vm_cont *cp;
  SCM *argv_copy, *base;
  size_t i;

  argv_copy = alloca (n * sizeof(SCM));
  memcpy (argv_copy, argv, n * sizeof(SCM));

  vp = SCM_VM_DATA (vm);
  cp = SCM_VM_CONT_DATA (cont);
  base = SCM_FRAME_UPPER_ADDRESS (vp->fp) + 1;

#define RELOC(scm_p) (scm_p + cp->reloc + (base - cp->stack_base))

  if ((base - vp->stack_base) + cp->stack_size + n + 1 > vp->stack_size)
    scm_misc_error ("vm-engine",
                    "not enough space to instate partial continuation",
                    scm_list_2 (vm, cont));

  memcpy (base, cp->stack_base, cp->stack_size * sizeof (SCM));

  /* now relocate frame pointers */
  {
    SCM *fp;
    for (fp = RELOC (cp->fp);
         SCM_FRAME_LOWER_ADDRESS (fp) > base;
         fp = SCM_FRAME_DYNAMIC_LINK (fp))
      SCM_FRAME_SET_DYNAMIC_LINK (fp, RELOC (SCM_FRAME_DYNAMIC_LINK (fp)));
  }

  vp->sp = base - 1 + cp->stack_size;
  vp->fp = RELOC (cp->fp);
  vp->ip = cp->mvra;

  /* now push args. ip is in a MV context. */
  for (i = 0; i < n; i++)
    {
      vp->sp++;
      *vp->sp = argv_copy[i];
    }
  vp->sp++;
  *vp->sp = scm_from_size_t (n);

  /* Finally, rewind the dynamic state.

     We have to treat prompts specially, because we could be rewinding the
     dynamic state from a different thread, or just a different position on the
     C and/or VM stack -- so we need to reset the jump buffers so that an abort
     comes back here, with appropriately adjusted sp and fp registers. */
  {
    long delta = 0;
    SCM newwinds = scm_i_dynwinds ();
    for (; scm_is_pair (intwinds); intwinds = scm_cdr (intwinds), delta--)
      {
        SCM x = scm_car (intwinds);
        if (SCM_PROMPT_P (x))
          /* the jmpbuf will be reset by our caller */
          x = scm_c_make_prompt (SCM_PROMPT_TAG (x),
                                 RELOC (SCM_PROMPT_REGISTERS (x)->fp),
                                 RELOC (SCM_PROMPT_REGISTERS (x)->sp),
                                 SCM_PROMPT_REGISTERS (x)->ip,
                                 SCM_PROMPT_ESCAPE_P (x),
                                 vm_cookie,
                                 newwinds);
        newwinds = scm_cons (x, newwinds);
      }
    scm_dowinds (newwinds, delta);
  }
#undef RELOC
}


/*
 * VM Internal functions
 */

void
scm_i_vm_print (SCM x, SCM port, scm_print_state *pstate)
{
  const struct scm_vm *vm;

  vm = SCM_VM_DATA (x);

  scm_puts ("#<vm ", port);
  switch (vm->engine)
    {
    case SCM_VM_REGULAR_ENGINE:
      scm_puts ("regular-engine ", port);
      break;

    case SCM_VM_DEBUG_ENGINE:
      scm_puts ("debug-engine ", port);
      break;

    default:
      scm_puts ("unknown-engine ", port);
    }
  scm_uintprint (SCM_UNPACK (x), 16, port);
  scm_puts (">", port);
}

static SCM
really_make_boot_program (long nargs)
{
  SCM u8vec;
  scm_t_uint8 text[] = { scm_op_mv_call, 0, 0, 0, 1,
                         scm_op_make_int8_1, scm_op_halt };
  struct scm_objcode *bp;
  SCM ret;

  if (SCM_UNLIKELY (nargs > 255 || nargs < 0))
    scm_misc_error ("vm-engine", "too many args when making boot procedure",
                    scm_list_1 (scm_from_long (nargs)));

  text[1] = (scm_t_uint8)nargs;

  bp = scm_malloc (sizeof (struct scm_objcode) + sizeof (text));
  memcpy (SCM_C_OBJCODE_BASE (bp), text, sizeof (text));
  bp->len = sizeof(text);
  bp->metalen = 0;

  u8vec = scm_c_take_bytevector ((scm_t_int8*)bp,
                                 sizeof (struct scm_objcode) + sizeof (text));
  ret = scm_make_program (scm_bytecode_to_objcode (u8vec),
                          SCM_BOOL_F, SCM_BOOL_F);
  SCM_SET_CELL_WORD_0 (ret, SCM_CELL_WORD_0 (ret) | SCM_F_PROGRAM_IS_BOOT);

  return ret;
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
        programs[i] = really_make_boot_program (i);
    }
  
  if (SCM_LIKELY (nargs < NUM_BOOT_PROGS))
    return programs[nargs];
  else
    return really_make_boot_program (nargs);
}


/*
 * VM
 */

static SCM
resolve_variable (SCM what, SCM program_module)
{
  if (SCM_LIKELY (scm_is_symbol (what)))
    {
      if (SCM_LIKELY (scm_module_system_booted_p
                      && scm_is_true (program_module)))
        /* might longjmp */
        return scm_module_lookup (program_module, what);
      else
        {
          SCM v = scm_sym2var (what, SCM_BOOL_F, SCM_BOOL_F);
          if (scm_is_false (v))
            scm_misc_error (NULL, "unbound variable: ~S", scm_list_1 (what));
          else
            return v;
        }
    }
  else
    {
      SCM mod;
      /* compilation of @ or @@
         `what' is a three-element list: (MODNAME SYM INTERFACE?)
         INTERFACE? is #t if we compiled @ or #f if we compiled @@
      */
      mod = scm_resolve_module (SCM_CAR (what));
      if (scm_is_true (SCM_CADDR (what)))
        mod = scm_module_public_interface (mod);
      if (scm_is_false (mod))
        scm_misc_error (NULL, "no such module: ~S",
                        scm_list_1 (SCM_CAR (what)));
      /* might longjmp */
      return scm_module_lookup (mod, SCM_CADR (what));
    }
}
  
#define VM_DEFAULT_STACK_SIZE	(64 * 1024)

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

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN

/* The GC "kind" for the VM stack.  */
static int vm_stack_gc_kind;

#endif

static SCM
make_vm (void)
#define FUNC_NAME "make_vm"
{
  int i;
  struct scm_vm *vp;

  vp = scm_gc_malloc (sizeof (struct scm_vm), "vm");

  vp->stack_size  = VM_DEFAULT_STACK_SIZE;

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN
  vp->stack_base = (SCM *)
    GC_generic_malloc (vp->stack_size * sizeof (SCM), vm_stack_gc_kind);

  /* Keep a pointer to VP so that `vm_stack_mark ()' can know what the stack
     top is.  */
  *vp->stack_base = PTR2SCM (vp);
  vp->stack_base++;
  vp->stack_size--;
#else
  vp->stack_base  = scm_gc_malloc (vp->stack_size * sizeof (SCM),
				   "stack-base");
#endif

#ifdef VM_ENABLE_STACK_NULLING
  memset (vp->stack_base, 0, vp->stack_size * sizeof (SCM));
#endif
  vp->stack_limit = vp->stack_base + vp->stack_size - VM_STACK_RESERVE_SIZE;
  vp->ip    	  = NULL;
  vp->sp    	  = vp->stack_base - 1;
  vp->fp    	  = NULL;
  vp->engine      = vm_default_engine;
  vp->trace_level = 0;
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    vp->hooks[i] = SCM_BOOL_F;
  vp->cookie = 0;
  return scm_cell (scm_tc7_vm, (scm_t_bits)vp);
}
#undef FUNC_NAME

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN

/* Mark the VM stack region between its base and its current top.  */
static struct GC_ms_entry *
vm_stack_mark (GC_word *addr, struct GC_ms_entry *mark_stack_ptr,
	       struct GC_ms_entry *mark_stack_limit, GC_word env)
{
  GC_word *word;
  const struct scm_vm *vm;

  /* The first word of the VM stack should contain a pointer to the
     corresponding VM.  */
  vm = * ((struct scm_vm **) addr);

  if (vm == NULL
      || (SCM *) addr != vm->stack_base - 1)
    /* ADDR must be a pointer to a free-list element, which we must ignore
       (see warning in <gc/gc_mark.h>).  */
    return mark_stack_ptr;

  for (word = (GC_word *) vm->stack_base; word <= (GC_word *) vm->sp; word++)
    mark_stack_ptr = GC_MARK_AND_PUSH ((* (GC_word **) word),
				       mark_stack_ptr, mark_stack_limit,
				       NULL);

  return mark_stack_ptr;
}

#endif /* VM_ENABLE_PRECISE_STACK_GC_SCAN */


SCM
scm_c_vm_run (SCM vm, SCM program, SCM *argv, int nargs)
{
  struct scm_vm *vp = SCM_VM_DATA (vm);
  SCM_CHECK_STACK;
  return vm_engines[vp->engine](vm, program, argv, nargs);
}

/* Scheme interface */

SCM_DEFINE (scm_the_vm, "the-vm", 0, 0, 0,
	    (void),
	    "Return the current thread's VM.")
#define FUNC_NAME s_scm_the_vm
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (SCM_UNLIKELY (scm_is_false (t->vm)))
    t->vm = make_vm ();

  return t->vm;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vm_p, "vm?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_vm_p
{
  return scm_from_bool (SCM_VM_P (obj));
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
  return scm_from_unsigned_integer ((scm_t_bits) SCM_VM_DATA (vm)->ip);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_sp, "vm:sp", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_sp
{
  SCM_VALIDATE_VM (1, vm);
  return scm_from_unsigned_integer ((scm_t_bits) SCM_VM_DATA (vm)->sp);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_fp, "vm:fp", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_fp
{
  SCM_VALIDATE_VM (1, vm);
  return scm_from_unsigned_integer ((scm_t_bits) SCM_VM_DATA (vm)->fp);
}
#undef FUNC_NAME

#define VM_DEFINE_HOOK(n)				\
{							\
  struct scm_vm *vp;					\
  SCM_VALIDATE_VM (1, vm);				\
  vp = SCM_VM_DATA (vm);				\
  if (scm_is_false (vp->hooks[n]))			\
    vp->hooks[n] = scm_make_hook (SCM_I_MAKINUM (1));	\
  return vp->hooks[n];					\
}

SCM_DEFINE (scm_vm_apply_hook, "vm-apply-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_apply_hook
{
  VM_DEFINE_HOOK (SCM_VM_APPLY_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_push_continuation_hook, "vm-push-continuation-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_push_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_PUSH_CONTINUATION_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_pop_continuation_hook, "vm-pop-continuation-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_pop_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_POP_CONTINUATION_HOOK);
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

SCM_DEFINE (scm_vm_abort_continuation_hook, "vm-abort-continuation-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_abort_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_ABORT_CONTINUATION_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_restore_continuation_hook, "vm-restore-continuation-hook", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_restore_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_RESTORE_CONTINUATION_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_trace_level, "vm-trace-level", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_trace_level
{
  SCM_VALIDATE_VM (1, vm);
  return scm_from_int (SCM_VM_DATA (vm)->trace_level);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_vm_trace_level_x, "set-vm-trace-level!", 2, 0, 0,
	    (SCM vm, SCM level),
	    "")
#define FUNC_NAME s_scm_set_vm_trace_level_x
{
  SCM_VALIDATE_VM (1, vm);
  SCM_VM_DATA (vm)->trace_level = scm_to_int (level);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*
 * VM engines
 */

static int
symbol_to_vm_engine (SCM engine, const char *FUNC_NAME)
{
  if (scm_is_eq (engine, sym_regular))
    return SCM_VM_REGULAR_ENGINE;
  else if (scm_is_eq (engine, sym_debug))
    return SCM_VM_DEBUG_ENGINE;
  else
    SCM_MISC_ERROR ("Unknown VM engine: ~a", scm_list_1 (engine));
}
  
static SCM
vm_engine_to_symbol (int engine, const char *FUNC_NAME)
{
  switch (engine)
    {
    case SCM_VM_REGULAR_ENGINE:
      return sym_regular;
    case SCM_VM_DEBUG_ENGINE:
      return sym_debug;
    default:
      /* ? */
      SCM_MISC_ERROR ("Unknown VM engine: ~a",
                      scm_list_1 (scm_from_int (engine)));
    }
}
  
SCM_DEFINE (scm_vm_engine, "vm-engine", 1, 0, 0,
	    (SCM vm),
	    "")
#define FUNC_NAME s_scm_vm_engine
{
  SCM_VALIDATE_VM (1, vm);
  return vm_engine_to_symbol (SCM_VM_DATA (vm)->engine, FUNC_NAME);
}
#undef FUNC_NAME

void
scm_c_set_vm_engine_x (SCM vm, int engine)
#define FUNC_NAME "set-vm-engine!"
{
  SCM_VALIDATE_VM (1, vm);

  if (engine < 0 || engine >= SCM_VM_NUM_ENGINES)
    SCM_MISC_ERROR ("Unknown VM engine: ~a",
                    scm_list_1 (scm_from_int (engine)));
    
  SCM_VM_DATA (vm)->engine = engine;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_vm_engine_x, "set-vm-engine!", 2, 0, 0,
	    (SCM vm, SCM engine),
	    "")
#define FUNC_NAME s_scm_set_vm_engine_x
{
  scm_c_set_vm_engine_x (vm, symbol_to_vm_engine (engine, FUNC_NAME));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_set_default_vm_engine_x (int engine)
#define FUNC_NAME "set-default-vm-engine!"
{
  if (engine < 0 || engine >= SCM_VM_NUM_ENGINES)
    SCM_MISC_ERROR ("Unknown VM engine: ~a",
                    scm_list_1 (scm_from_int (engine)));
    
  vm_default_engine = engine;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_default_vm_engine_x, "set-default-vm-engine!", 1, 0, 0,
	    (SCM engine),
	    "")
#define FUNC_NAME s_scm_set_default_vm_engine_x
{
  scm_c_set_default_vm_engine_x (symbol_to_vm_engine (engine, FUNC_NAME));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void reinstate_vm (SCM vm)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  t->vm = vm;
}

SCM_DEFINE (scm_call_with_vm, "call-with-vm", 2, 0, 1,
	    (SCM vm, SCM proc, SCM args),
	    "Apply @var{proc} to @var{args} in a dynamic extent in which\n"
            "@var{vm} is the current VM.\n\n"
            "As an implementation restriction, if @var{vm} is not the same\n"
            "as the current thread's VM, continuations captured within the\n"
            "call to @var{proc} may not be reinstated once control leaves\n"
            "@var{proc}.")
#define FUNC_NAME s_scm_call_with_vm
{
  SCM prev_vm, ret;
  SCM *argv;
  int i, nargs;
  scm_t_wind_flags flags;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_PROC (2, proc);

  nargs = scm_ilength (args);
  if (SCM_UNLIKELY (nargs < 0))
    scm_wrong_type_arg_msg (FUNC_NAME, 3, args, "list");
  
  argv = alloca (nargs * sizeof(SCM));
  for (i = 0; i < nargs; i++)
    {
      argv[i] = SCM_CAR (args);
      args = SCM_CDR (args);
    }

  prev_vm = t->vm;

  /* Reentry can happen via invokation of a saved continuation, but
     continuations only save the state of the VM that they are in at
     capture-time, which might be different from this one.  So, in the
     case that the VMs are different, set up a non-rewindable frame to
     prevent reinstating an incomplete continuation.  */
  flags = scm_is_eq (prev_vm, vm) ? 0 : SCM_F_WIND_EXPLICITLY;
  if (flags)
    {
      scm_dynwind_begin (0);
      scm_dynwind_unwind_handler_with_scm (reinstate_vm, prev_vm, flags);
      t->vm = vm;
    }

  ret = scm_c_vm_run (vm, proc, argv, nargs);

  if (flags)
    scm_dynwind_end ();
  
  return ret;
}
#undef FUNC_NAME


/*
 * Initialize
 */

SCM scm_load_compiled_with_vm (SCM file)
{
  SCM program = scm_make_program (scm_load_objcode (file),
                                  SCM_BOOL_F, SCM_BOOL_F);
  
  return scm_c_vm_run (scm_the_vm (), program, NULL, 0);
}

void
scm_bootstrap_vm (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_vm",
                            (scm_t_extension_init_func)scm_init_vm, NULL);

  sym_vm_run = scm_from_latin1_symbol ("vm-run");
  sym_vm_error = scm_from_latin1_symbol ("vm-error");
  sym_keyword_argument_error = scm_from_latin1_symbol ("keyword-argument-error");
  sym_regular = scm_from_latin1_symbol ("regular");
  sym_debug = scm_from_latin1_symbol ("debug");

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN
  vm_stack_gc_kind =
    GC_new_kind (GC_new_free_list (),
		 GC_MAKE_PROC (GC_new_proc (vm_stack_mark), 0),
		 0, 1);

#endif
}

void
scm_init_vm (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/vm.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
