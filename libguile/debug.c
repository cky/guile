/* Debugging extensions for Guile
 * Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2003, 2006, 2008, 2009 Free Software Foundation
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


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef HAVE_GETRLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/eval.h"
#include "libguile/list.h"
#include "libguile/stackchk.h"
#include "libguile/throw.h"
#include "libguile/macros.h"
#include "libguile/smob.h"
#include "libguile/procprop.h"
#include "libguile/srcprop.h"
#include "libguile/alist.h"
#include "libguile/continuations.h"
#include "libguile/strports.h"
#include "libguile/read.h"
#include "libguile/feature.h"
#include "libguile/dynwind.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/fluids.h"
#include "libguile/programs.h"
#include "libguile/memoize.h"
#include "libguile/vm.h"

#include "libguile/validate.h"
#include "libguile/debug.h"

#include "libguile/private-options.h"



/* {Run time control of the debugging evaluator}
 */

SCM_DEFINE (scm_debug_options, "debug-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the debug options. Instead of using\n"
	    "this procedure directly, use the procedures @code{debug-enable},\n"
	    "@code{debug-disable}, @code{debug-set!} and @code{debug-options}.")
#define FUNC_NAME s_scm_debug_options
{
  SCM ans;

  scm_dynwind_begin (0);
  scm_dynwind_critical_section (SCM_BOOL_F);

  ans = scm_options (setting, scm_debug_opts, FUNC_NAME);
  if (SCM_N_FRAMES < 1)
    {
      scm_options (ans, scm_debug_opts, FUNC_NAME);
      SCM_OUT_OF_RANGE (1, setting);
    }
#ifdef STACK_CHECKING
  scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
#endif

  scm_dynwind_end ();
  return ans;
}
#undef FUNC_NAME


static void
with_traps_before (void *data)
{
  int *trap_flag = data;
  *trap_flag = SCM_TRAPS_P;
  SCM_TRAPS_P = 1;
}

static void
with_traps_after (void *data)
{
  int *trap_flag = data;
  SCM_TRAPS_P = *trap_flag;
}

static SCM
with_traps_inner (void *data)
{
  SCM thunk = SCM_PACK ((scm_t_bits) data);
  return scm_call_0 (thunk);
}

SCM_DEFINE (scm_with_traps, "with-traps", 1, 0, 0, 
            (SCM thunk),
	    "Call @var{thunk} with traps enabled.")
#define FUNC_NAME s_scm_with_traps
{
  int trap_flag;
  SCM_VALIDATE_THUNK (1, thunk);
  return scm_internal_dynamic_wind (with_traps_before,
				    with_traps_inner,
				    with_traps_after,
				    (void *) SCM_UNPACK (thunk),
				    &trap_flag);
}
#undef FUNC_NAME


SCM_SYMBOL (scm_sym_procname, "procname");
SCM_SYMBOL (scm_sym_dots, "...");
SCM_SYMBOL (scm_sym_source, "source");

SCM_DEFINE (scm_procedure_name, "procedure-name", 1, 0, 0, 
            (SCM proc),
	    "Return the name of the procedure @var{proc}")
#define FUNC_NAME s_scm_procedure_name
{
  SCM_VALIDATE_PROC (1, proc);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_subrs:
    return SCM_SUBR_NAME (proc);
  default:
    {
      SCM name = scm_procedure_property (proc, scm_sym_name);
#if 0
      /* Source property scm_sym_procname not implemented yet... */
      SCM name = scm_source_property (SCM_CAR (SCM_CLOSURE_BODY (proc)), scm_sym_procname);
      if (scm_is_false (name))
	name = scm_procedure_property (proc, scm_sym_name);
#endif
      if (scm_is_false (name) && SCM_CLOSUREP (proc))
	name = scm_reverse_lookup (SCM_ENV (proc), proc);
      if (scm_is_false (name) && SCM_PROGRAM_P (proc))
        name = scm_program_name (proc);
      return name;
    }
  }
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_source, "procedure-source", 1, 0, 0, 
            (SCM proc),
	    "Return the source of the procedure @var{proc}.")
#define FUNC_NAME s_scm_procedure_source
{
  SCM src;
  SCM_VALIDATE_PROC (1, proc);

  do 
    {
      src = scm_procedure_property (proc, scm_sym_source);
      if (scm_is_true (src))
        return src;

      switch (SCM_TYP7 (proc)) {
      case scm_tcs_struct:
        if (!SCM_STRUCT_APPLICABLE_P (proc)
            || SCM_IMP (SCM_STRUCT_PROCEDURE (proc)))
          break;
        proc = SCM_STRUCT_PROCEDURE (proc);
        continue;
      case scm_tc7_pws:
        proc = SCM_PROCEDURE (proc);
        continue;
      default:
        break;
      }
    }
  while (0);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_module, "procedure-module", 1, 0, 0, 
           (SCM proc),
	    "Return the module that was current when @var{proc} was defined.")
#define FUNC_NAME s_scm_procedure_module
{
  SCM_VALIDATE_PROC (SCM_ARG1, proc);

  if (scm_is_true (scm_program_p (proc)))
    return scm_program_module (proc);
  else if (SCM_CLOSUREP (proc))
    {
      SCM env = SCM_ENV (proc);
      while (scm_is_pair (env))
        env = scm_cdr (env);
      return env;
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME




#if 0
SCM_REGISTER_PROC (s_reverse_lookup, "reverse-lookup", 2, 0, 0, scm_reverse_lookup);
#endif

SCM
scm_reverse_lookup (SCM env, SCM data)
{
  while (scm_is_pair (env) && scm_is_pair (SCM_CAR (env)))
    {
      SCM names = SCM_CAAR (env);
      SCM values = SCM_CDAR (env);
      while (scm_is_pair (names))
	{
	  if (scm_is_eq (SCM_CAR (values), data))
	    return SCM_CAR (names);
	  names = SCM_CDR (names);
	  values = SCM_CDR (values);
	}
      if (!scm_is_null (names) && scm_is_eq (values, data))
	return names;
      env = SCM_CDR (env);
    }
  return SCM_BOOL_F;
}

SCM_DEFINE (scm_sys_start_stack, "%start-stack", 2, 0, 0,
            (SCM id, SCM thunk),
	    "Call @var{thunk} on an evaluator stack tagged with @var{id}.")
#define FUNC_NAME s_scm_sys_start_stack
{
  return scm_vm_call_with_new_stack (scm_the_vm (), thunk, id);
}
#undef FUNC_NAME



/* Undocumented debugging procedure */
#ifdef GUILE_DEBUG
SCM_DEFINE (scm_debug_hang, "debug-hang", 0, 1, 0, 
            (SCM obj),
	    "Go into an endless loop, which can be only terminated with\n"
	    "a debugger.")
#define FUNC_NAME s_scm_debug_hang
{
  int go = 0;
  while (!go) ;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

static void
init_stack_limit (void)
{
#ifdef HAVE_GETRLIMIT
  struct rlimit lim;
  if (getrlimit (RLIMIT_STACK, &lim) == 0)
      {
        rlim_t bytes = lim.rlim_cur;

        /* set our internal stack limit to 80% of the rlimit. */
        if (bytes == RLIM_INFINITY)
          bytes = lim.rlim_max;

        if (bytes != RLIM_INFINITY)
          SCM_STACK_LIMIT = bytes * 8 / 10 / sizeof (scm_t_bits);
      }
  errno = 0;
#endif
}



void
scm_init_debug ()
{
  init_stack_limit ();
  scm_init_opts (scm_debug_options, scm_debug_opts);

  scm_add_feature ("debug-extensions");

#include "libguile/debug.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
