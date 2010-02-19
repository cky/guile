/* Copyright (C) 2010  Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/control.h"
#include "libguile/vm.h"



SCM scm_atcontrol (SCM, SCM, SCM);
SCM_DEFINE (scm_atcontrol, "@control", 3, 0, 0,
            (SCM tag, SCM type, SCM args),
            "Transfer control to the handler of a delimited continuation.")
#define FUNC_NAME s_scm_atcontrol
{
  abort ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM scm_atprompt (SCM, SCM, SCM, SCM);
SCM_DEFINE (scm_atprompt, "@prompt", 4, 0, 0,
            (SCM tag, SCM thunk, SCM handler, SCM pre_unwind_handler),
            "Begin a delimited continuation.")
#define FUNC_NAME s_scm_atprompt
{
  abort ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_c_make_prompt (SCM vm, SCM k, SCM handler, scm_t_uint8 escape_only_p)
{
  scm_t_bits tag;
  SCM ret;
  struct scm_prompt_registers *regs;

  tag = scm_tc7_prompt;
  if (escape_only_p)
    tag |= SCM_F_PROMPT_ESCAPE;
  ret = scm_words (tag, 5);

  regs = scm_gc_malloc_pointerless (sizeof (*regs), "prompt registers");
  regs->fp = SCM_VM_DATA (vm)->fp;
  regs->sp = SCM_VM_DATA (vm)->sp;
  regs->ip = SCM_VM_DATA (vm)->ip;

  SCM_SET_CELL_OBJECT (ret, 1, k);
  SCM_SET_CELL_WORD (ret, 2, (scm_t_bits)regs);
  SCM_SET_CELL_OBJECT (ret, 3, scm_i_dynwinds ());
  SCM_SET_CELL_OBJECT (ret, 4, handler);

  return ret;
}

SCM
scm_c_abort (SCM vm, SCM tag, size_t n, SCM *argv)
{
  SCM winds, prompt = SCM_BOOL_F;
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
  
  /* If we didn't find anything, print a message and abort the process
     right here.  If you don't want this, establish a catch-all around
     any code that might throw up. */
  if (scm_is_false (prompt))
    {
      /* FIXME: jump to default */
      /* scm_handle_by_message (NULL, key, args); */
      abort ();
    }

  /* Unwind once more, beyond the prompt. */
  winds = SCM_CDR (winds), delta++;
  
  /* Unwind */
  scm_dowinds (winds, delta);

  /* Restore VM regs */
  SCM_VM_DATA (vm)->fp = SCM_PROMPT_REGISTERS (prompt)->fp;
  SCM_VM_DATA (vm)->sp = SCM_PROMPT_REGISTERS (prompt)->sp;
  SCM_VM_DATA (vm)->ip = SCM_PROMPT_REGISTERS (prompt)->ip;

  /* Since we're jumping down, we should always have enough space */
  if (SCM_VM_DATA (vm)->sp + n + 1 >= SCM_VM_DATA (vm)->stack_limit)
    abort ();

  /* Push vals */
  *(++(SCM_VM_DATA (vm)->sp)) = SCM_BOOL_F; /* the continuation */
  for (i = 0; i < n; i++)
    *(++(SCM_VM_DATA (vm)->sp)) = argv[i];
  *(++(SCM_VM_DATA (vm)->sp)) = scm_from_size_t (n+1); /* +1 for continuation */

  /* Jump! */
  SCM_I_LONGJMP (SCM_PROMPT_REGISTERS (prompt)->regs, 1);

  /* Shouldn't get here */
  abort ();
}



static void
scm_init_control (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/control.x"
#endif
}

void
scm_register_control (void)
{
  scm_c_register_extension ("libguile", "scm_init_control",
                            (scm_t_extension_init_func)scm_init_control,
                            NULL);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
