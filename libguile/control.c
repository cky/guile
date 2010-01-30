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
