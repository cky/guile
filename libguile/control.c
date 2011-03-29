/* Copyright (C) 2010, 2011  Free Software Foundation, Inc.
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

#include <alloca.h>

#include "libguile/_scm.h"
#include "libguile/control.h"
#include "libguile/objcodes.h"
#include "libguile/instructions.h"
#include "libguile/vm.h"




SCM
scm_c_make_prompt (SCM k, SCM *fp, SCM *sp, scm_t_uint8 *abort_ip,
                   scm_t_uint8 escape_only_p, scm_t_int64 vm_cookie,
                   SCM winds)
{
  scm_t_bits tag;
  struct scm_prompt_registers *regs;

  tag = scm_tc7_prompt;
  if (escape_only_p)
    tag |= (SCM_F_PROMPT_ESCAPE<<8);

  regs = scm_gc_malloc_pointerless (sizeof (*regs), "prompt registers");
  regs->fp = fp;
  regs->sp = sp;
  regs->ip = abort_ip;
  regs->cookie = vm_cookie;

  return scm_double_cell (tag, SCM_UNPACK (k), (scm_t_bits)regs, 
                          SCM_UNPACK (winds));
}

/* Only to be called if the SCM_PROMPT_SETJMP returns 1 */
SCM
scm_i_prompt_pop_abort_args_x (SCM vm)
{
  size_t i, n;
  SCM vals = SCM_EOL;

  n = scm_to_size_t (SCM_VM_DATA (vm)->sp[0]);
  for (i = 0; i < n; i++)
    vals = scm_cons (SCM_VM_DATA (vm)->sp[-(i + 1)], vals);

  /* The abort did reset the VM's registers, but then these values
     were pushed on; so we need to pop them ourselves. */
  SCM_VM_DATA (vm)->sp -= n + 1;
  /* FIXME NULLSTACK */

  return vals;
}


#ifdef WORDS_BIGENDIAN
#define OBJCODE_HEADER(main,meta) 0, 0, 0, main, 0, 0, 0, meta+8
#define META_HEADER(meta)         0, 0, 0, meta, 0, 0, 0, 0
#else
#define OBJCODE_HEADER(main,meta) main, 0, 0, 0, meta+8, 0, 0, 0
#define META_HEADER(meta)         meta, 0, 0, 0, 0,      0, 0, 0
#endif

#define ALIGN_PTR(type,p,align) (type*)(ROUND_UP (((scm_t_bits)p), align))

#ifdef SCM_ALIGNED
#define SCM_DECLARE_STATIC_ALIGNED_ARRAY(type, sym)     \
static const type sym[]
#define SCM_STATIC_ALIGNED_ARRAY(alignment, type, sym)  \
static SCM_ALIGNED (alignment) const type sym[]
#else
#define SCM_DECLARE_STATIC_ALIGNED_ARRAY(type, sym)     \
static type *sym
#define SCM_STATIC_ALIGNED_ARRAY(alignment, type, sym)                  \
SCM_SNARF_INIT(sym = scm_malloc_pointerless (sizeof(sym##__unaligned); \
               memcpy (sym, sym##__unaligned, sizeof(sym##__unaligned));) \
static type *sym = NULL;                                                \
static const type sym##__unaligned[]
#endif

#define STATIC_OBJCODE_TAG                                      \
  SCM_PACK (SCM_MAKE_OBJCODE_TAG (SCM_OBJCODE_TYPE_STATIC, 0))

#define SCM_STATIC_OBJCODE(sym)                                         \
  SCM_DECLARE_STATIC_ALIGNED_ARRAY (scm_t_uint8, sym##__bytecode);      \
  SCM_STATIC_ALIGNED_ARRAY (8, scm_t_cell, sym##__cells) = {            \
    { STATIC_OBJCODE_TAG, SCM_PACK (sym##__bytecode) },                 \
    { SCM_BOOL_F, SCM_PACK (0) }                                        \
  };                                                                    \
  static const SCM sym = SCM_PACK (sym##__cells);                       \
  SCM_STATIC_ALIGNED_ARRAY (8, scm_t_uint8, sym##__bytecode)

  
SCM_STATIC_OBJCODE (cont_objcode) = {
  /* Like in continuations.c, but with partial-cont-call. */
  OBJCODE_HEADER (8, 19),
  /* leave args on the stack */
  /* 0 */ scm_op_object_ref, 0, /* push scm_vm_cont object */
  /* 2 */ scm_op_object_ref, 1, /* push internal winds */
  /* 4 */ scm_op_partial_cont_call, /* and go! */
  /* 5 */ scm_op_nop, scm_op_nop, scm_op_nop, /* pad to 8 bytes */
  /* 8 */

  /* We could put some meta-info to say that this proc is a continuation. Not sure
     how to do that, though. */
  META_HEADER (19),
  /* 0 */ scm_op_make_eol, /* bindings */
  /* 1 */ scm_op_make_eol, /* sources */
  /* 2 */ scm_op_make_int8, 0, scm_op_make_int8, 5, /* arity: from ip 0 to ip 7 */
  /* 6 */ scm_op_make_int8_0, /* the arity is 0 required args */
  /* 7 */ scm_op_make_int8_0, /* 0 optionals */
  /* 8 */ scm_op_make_true, /* and a rest arg */
  /* 9 */ scm_op_list, 0, 5, /* make a list of those 5 vals */
  /* 12 */ scm_op_list, 0, 1, /* and the arities will be a list of that one list */
  /* 15 */ scm_op_list, 0, 3, /* pack bindings, sources, and arities into list */
  /* 18 */ scm_op_return /* and return */
  /* 19 */
};


static SCM
reify_partial_continuation (SCM vm, SCM prompt, SCM extwinds,
                            scm_t_int64 cookie)
{
  SCM vm_cont, dynwinds, intwinds = SCM_EOL, ret;
  scm_t_uint32 flags;

  /* No need to reify if the continuation is never referenced in the handler. */
  if (SCM_PROMPT_ESCAPE_P (prompt))
    return SCM_BOOL_F;

  dynwinds = scm_i_dynwinds ();
  while (!scm_is_eq (dynwinds, extwinds))
    {
      intwinds = scm_cons (scm_car (dynwinds), intwinds);
      dynwinds = scm_cdr (dynwinds);
    }

  flags = SCM_F_VM_CONT_PARTIAL;
  if (cookie >= 0 && SCM_PROMPT_REGISTERS (prompt)->cookie == cookie)
    flags |= SCM_F_VM_CONT_REWINDABLE;

  /* Since non-escape continuations should begin with a thunk application, the
     first bit of the stack should be a frame, with the saved fp equal to the fp
     that was current when the prompt was made. */
  if ((SCM*)(SCM_PROMPT_REGISTERS (prompt)->sp[1])
      != SCM_PROMPT_REGISTERS (prompt)->fp)
    abort ();

  /* Capture from the top of the thunk application frame up to the end. Set an
     MVRA only, as the post-abort code is in an MV context. */
  vm_cont = scm_i_vm_capture_stack (SCM_PROMPT_REGISTERS (prompt)->sp + 4,
                                    SCM_VM_DATA (vm)->fp,
                                    SCM_VM_DATA (vm)->sp,
                                    NULL,
                                    SCM_VM_DATA (vm)->ip,
                                    flags);

  ret = scm_make_program (cont_objcode,
                          scm_vector (scm_list_2 (vm_cont, intwinds)),
                          SCM_BOOL_F);
  SCM_SET_CELL_WORD_0 (ret,
                       SCM_CELL_WORD_0 (ret) | SCM_F_PROGRAM_IS_PARTIAL_CONTINUATION);
  return ret;
}

void
scm_c_abort (SCM vm, SCM tag, size_t n, SCM *argv, scm_t_int64 cookie)
{
  SCM cont, winds, prompt = SCM_BOOL_F;
  long delta;
  size_t i;

  /* Search the wind list for an appropriate prompt.
     "Waiter, please bring us the wind list." */
  for (winds = scm_i_dynwinds (), delta = 0;
       scm_is_pair (winds);
       winds = SCM_CDR (winds), delta++)
    {
      SCM elt = SCM_CAR (winds);
      if (SCM_PROMPT_P (elt) && scm_is_eq (SCM_PROMPT_TAG (elt), tag))
        {
          prompt = elt;
          break;
        }
    }
  
  /* If we didn't find anything, raise an error. */
  if (scm_is_false (prompt))
    scm_misc_error ("abort", "Abort to unknown prompt", scm_list_1 (tag));

  cont = reify_partial_continuation (vm, prompt, winds, cookie);

  /* Unwind once more, beyond the prompt. */
  winds = SCM_CDR (winds), delta++;

  /* Unwind */
  scm_dowinds (winds, delta);

  /* Unwinding may have changed the current thread's VM, so use the
     new one.  */
  vm = scm_the_vm ();

  /* Restore VM regs */
  SCM_VM_DATA (vm)->fp = SCM_PROMPT_REGISTERS (prompt)->fp;
  SCM_VM_DATA (vm)->sp = SCM_PROMPT_REGISTERS (prompt)->sp;
  SCM_VM_DATA (vm)->ip = SCM_PROMPT_REGISTERS (prompt)->ip;

  /* Since we're jumping down, we should always have enough space */
  if (SCM_VM_DATA (vm)->sp + n + 1 >= SCM_VM_DATA (vm)->stack_limit)
    abort ();

  /* Push vals */
  *(++(SCM_VM_DATA (vm)->sp)) = cont;
  for (i = 0; i < n; i++)
    *(++(SCM_VM_DATA (vm)->sp)) = argv[i];
  *(++(SCM_VM_DATA (vm)->sp)) = scm_from_size_t (n+1); /* +1 for continuation */

  /* Jump! */
  SCM_I_LONGJMP (SCM_PROMPT_REGISTERS (prompt)->regs, 1);

  /* Shouldn't get here */
  abort ();
}

SCM_DEFINE (scm_at_abort, "@abort", 2, 0, 0, (SCM tag, SCM args),
            "Abort to the nearest prompt with tag @var{tag}.")
#define FUNC_NAME s_scm_at_abort
{
  SCM *argv;
  size_t i, n;

  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG2, args, n);
  argv = alloca (sizeof (SCM)*n);
  for (i = 0; i < n; i++, args = scm_cdr (args))
    argv[i] = scm_car (args);

  scm_c_abort (scm_the_vm (), tag, n, argv, -1);

  /* Oh, what, you're still here? The abort must have been reinstated. Actually,
     that's quite impossible, given that we're already in C-land here, so...
     abort! */

  abort ();
}
#undef FUNC_NAME

void
scm_i_prompt_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<prompt ", port);
  scm_intprint (SCM_UNPACK (exp), 16, port);
  scm_putc ('>', port);
}

void
scm_init_control (void)
{
#include "libguile/control.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
