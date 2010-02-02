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
scm_c_make_prompt (SCM vm, SCM k, SCM handler, SCM pre_unwind,
                   scm_t_uint8 inline_p, scm_t_uint8 escape_only_p)
{
  scm_t_bits tag;
  SCM ret;
  struct scm_prompt_registers *regs;

  tag = scm_tc7_prompt;
  if (inline_p)
    tag |= SCM_F_PROMPT_INLINE;
  if (escape_only_p)
    tag |= SCM_F_PROMPT_ESCAPE;
  ret = scm_words (tag, 6);

  regs = scm_gc_malloc_pointerless (sizeof (*regs), "prompt registers");
  regs->fp = SCM_VM_DATA (vm)->fp;
  regs->sp = SCM_VM_DATA (vm)->sp;
  regs->ip = SCM_VM_DATA (vm)->ip;

  SCM_SET_CELL_OBJECT (ret, 1, k);
  SCM_SET_CELL_WORD (ret, 2, (scm_t_bits)regs);
  SCM_SET_CELL_OBJECT (ret, 3, scm_i_dynwinds ());
  SCM_SET_CELL_OBJECT (ret, 4, handler);
  SCM_SET_CELL_OBJECT (ret, 5, pre_unwind);

  return ret;
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
