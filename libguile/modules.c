/*	Copyright (C) 1998, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include "libguile/_scm.h"

#include "libguile/eval.h"
#include "libguile/smob.h"
#include "libguile/procprop.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"
#include "libguile/struct.h"
#include "libguile/variable.h"
#include "libguile/fluids.h"

#include "libguile/modules.h"

SCM scm_module_system_booted_p = 0;

SCM scm_module_tag;

static SCM the_root_module;
static SCM root_module_lookup_closure;

SCM
scm_the_root_module ()
{
  return SCM_CDR (the_root_module);
}

static SCM the_module;

SCM
scm_selected_module ()
{
  return scm_fluid_ref (SCM_CDR (the_module));
}

static SCM set_current_module;

/* This is the module selected during loading of code.  Currently,
 * this is the same as (interaction-environment), but need not be in
 * the future.
 */

SCM
scm_select_module (SCM module)
{
  SCM old = scm_selected_module ();
  scm_apply (SCM_CDR (set_current_module), SCM_LIST1 (module), SCM_EOL);
  return old;
}

SCM_DEFINE (scm_interaction_environment, "interaction-environment", 0, 0, 0,
	    (),
	    "This procedure returns a specifier for the environment that contains\n"
	    "implementation-defined bindings, typically a superset of those listed in\n"
	    "the report.  The intent is that this procedure will return the\n"
	    "environment in which the implementation would evaluate expressions\n"
	    "dynamically typed by the user.")
#define FUNC_NAME s_scm_interaction_environment
{
  return scm_selected_module ();
}
#undef FUNC_NAME

SCM_SYMBOL (scm_sym_app, "app");
SCM_SYMBOL (scm_sym_modules, "modules");
static SCM module_prefix;

static SCM
scm_module_full_name (SCM name)
{
  if (SCM_EQ_P (SCM_CAR (name), scm_sym_app))
    return name;
  else
    return scm_append (SCM_LIST2 (module_prefix, name));
}

static SCM make_modules_in;
static SCM beautify_user_module_x;

SCM
scm_make_module (SCM name)
{
  return scm_apply (SCM_CDR (make_modules_in),
		    SCM_LIST2 (scm_the_root_module (),
			       scm_module_full_name (name)),
		    SCM_EOL);
}

SCM
scm_ensure_user_module (SCM module)
{
  scm_apply (SCM_CDR (beautify_user_module_x), SCM_LIST1 (module), SCM_EOL);
  return SCM_UNSPECIFIED;
}

SCM
scm_module_lookup_closure (SCM module)
{
  return SCM_MODULE_EVAL_CLOSURE (module);
}

static SCM resolve_module;

SCM
scm_resolve_module (SCM name)
{
  return scm_apply (SCM_CDR (resolve_module), SCM_LIST1 (name), SCM_EOL);
}

static SCM try_module_autoload;

SCM
scm_load_scheme_module (SCM name)
{
  return scm_apply (SCM_CDR (try_module_autoload), SCM_LIST1 (name), SCM_EOL);
}

/* Environments */

SCM
scm_top_level_env (SCM thunk)
{
  if (SCM_IMP (thunk))
    return SCM_EOL;
  else
    return scm_cons (thunk, SCM_EOL);
}

SCM
scm_env_top_level (SCM env)
{
  while (SCM_NIMP (env))
    {
      if (!SCM_CONSP (SCM_CAR (env))
	  && SCM_NFALSEP (scm_procedure_p (SCM_CAR (env))))
	return SCM_CAR (env);
      env = SCM_CDR (env);
    }
  return SCM_BOOL_F;
}


SCM_SYMBOL (scm_sym_system_module, "system-module");

SCM
scm_system_module_env_p (SCM env)
{
  SCM proc = scm_env_top_level (env);
  if (SCM_FALSEP (proc))
    proc = root_module_lookup_closure;
  return ((SCM_NFALSEP (scm_procedure_property (proc,
						scm_sym_system_module)))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

/*
 * C level implementation of the standard eval closure
 *
 * This increases loading speed substantially.
 * The code will be replaced by the low-level environments in next release.
 */

static SCM module_make_local_var_x;

static SCM
module_variable (SCM module, SCM sym)
{
  /* 1. Check module obarray */
  SCM b = scm_hashq_ref (SCM_MODULE_OBARRAY (module), sym, SCM_UNDEFINED);
  if (SCM_VARIABLEP (b))
    return b;
  {
    SCM binder = SCM_MODULE_BINDER (module);
    if (SCM_NFALSEP (binder))
      /* 2. Custom binder */
      {
	b = scm_apply (binder,
		       SCM_LIST3 (module, sym, SCM_BOOL_F),
		       SCM_EOL);
	if (SCM_NFALSEP (b))
	  return b;
      }
  }
  {
    /* 3. Search the use list */
    SCM uses = SCM_MODULE_USES (module);
    while (SCM_CONSP (uses))
      {
	b = module_variable (SCM_CAR (uses), sym);
	if (SCM_NFALSEP (b))
	  return b;
	uses = SCM_CDR (uses);
      }
    return SCM_BOOL_F;
  }
}

SCM scm_eval_closure_tag;

/* NOTE: This function may be called by a smob application
   or from another C function directly. */
SCM
scm_eval_closure_lookup (SCM eclo, SCM sym, SCM definep)
{
  SCM module = SCM_PACK (SCM_SMOB_DATA (eclo));
  if (SCM_NFALSEP (definep))
    return scm_apply (SCM_CDR (module_make_local_var_x),
		      SCM_LIST2 (module, sym),
		      SCM_EOL);
  else
    return module_variable (module, sym);
}

SCM_DEFINE (scm_standard_eval_closure, "standard-eval-closure", 1, 0, 0,
	    (SCM module),
	    "")
#define FUNC_NAME s_scm_standard_eval_closure
{
  SCM_RETURN_NEWSMOB (scm_eval_closure_tag, SCM_UNPACK (module));
}
#undef FUNC_NAME

void
scm_init_modules ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/modules.x"
#endif
  module_make_local_var_x = scm_sysintern ("module-make-local-var!",
					   SCM_UNDEFINED);
  scm_eval_closure_tag = scm_make_smob_type ("eval-closure", 0);
  scm_set_smob_mark (scm_eval_closure_tag, scm_markcdr);
  scm_set_smob_apply (scm_eval_closure_tag, scm_eval_closure_lookup, 2, 0, 0);
}

void
scm_post_boot_init_modules ()
{
  scm_module_tag = (SCM_CELL_WORD_1 (SCM_CDR (scm_intern0 ("module-type")))
		    + scm_tc3_cons_gloc);
  the_root_module = scm_intern0 ("the-root-module");
  the_module = scm_intern0 ("the-module");
  set_current_module = scm_intern0 ("set-current-module");
  module_prefix = scm_permanent_object (SCM_LIST2 (scm_sym_app,
						   scm_sym_modules));
  make_modules_in = scm_intern0 ("make-modules-in");
  beautify_user_module_x = scm_intern0 ("beautify-user-module!");
  root_module_lookup_closure = scm_permanent_object
    (scm_module_lookup_closure (SCM_CDR (the_root_module)));
  resolve_module = scm_intern0 ("resolve-module");
  try_module_autoload = scm_intern0 ("try-module-autoload");
  scm_module_system_booted_p = 1;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
