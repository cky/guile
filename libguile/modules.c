/* Copyright (C) 1998,2000,2001 Free Software Foundation, Inc.
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




#include <stdarg.h>

#include "libguile/_scm.h"

#include "libguile/eval.h"
#include "libguile/smob.h"
#include "libguile/procprop.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"
#include "libguile/struct.h"
#include "libguile/variable.h"
#include "libguile/fluids.h"
#include "libguile/deprecation.h"

#include "libguile/modules.h"

int scm_module_system_booted_p = 0;

scm_t_bits scm_module_tag;

static SCM the_module;

SCM_DEFINE (scm_current_module, "current-module", 0, 0, 0,
	    (),
	    "Return the current module.")
#define FUNC_NAME s_scm_current_module
{
  return scm_fluid_ref (the_module);
}
#undef FUNC_NAME

static void scm_post_boot_init_modules (void);

SCM_DEFINE (scm_set_current_module, "set-current-module", 1, 0, 0,
	    (SCM module),
	    "Set the current module to @var{module} and return"
	    "the previous current module.")
#define FUNC_NAME s_scm_set_current_module
{
  SCM old;

  if (!scm_module_system_booted_p)
    scm_post_boot_init_modules ();

  SCM_VALIDATE_MODULE (SCM_ARG1, module);

  old = scm_current_module ();
  scm_fluid_set_x (the_module, module);

  return old;
}
#undef FUNC_NAME

SCM_DEFINE (scm_interaction_environment, "interaction-environment", 0, 0, 0,
	    (),
	    "Return a specifier for the environment that contains\n"
	    "implementation--defined bindings, typically a superset of those\n"
	    "listed in the report.  The intent is that this procedure will\n"
	    "return the environment in which the implementation would\n"
	    "evaluate expressions dynamically typed by the user.")
#define FUNC_NAME s_scm_interaction_environment
{
  return scm_current_module ();
}
#undef FUNC_NAME

SCM
scm_c_call_with_current_module (SCM module,
				SCM (*func)(void *), void *data)
{
  return scm_c_with_fluid (the_module, module, func, data);
}

static SCM
convert_module_name (const char *name)
{
  SCM list = SCM_EOL;
  SCM *tail = &list;

  const char *ptr;
  while (*name)
    {
      while (*name == ' ')
	name++;
      ptr = name;
      while (*ptr && *ptr != ' ')
	ptr++;
      if (ptr > name)
	{
	  *tail = scm_cons (scm_mem2symbol (name, ptr-name), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	}
      name = ptr;
    }

  return list;
}

static SCM process_define_module_var;
static SCM process_use_modules_var;
static SCM resolve_module_var;

SCM
scm_c_resolve_module (const char *name)
{
  return scm_resolve_module (convert_module_name (name));
}

SCM
scm_resolve_module (SCM name)
{
  return scm_call_1 (SCM_VARIABLE_REF (resolve_module_var), name);
}

SCM
scm_c_define_module (const char *name,
		     void (*init)(void *), void *data)
{
  SCM module = scm_call_1 (SCM_VARIABLE_REF (process_define_module_var),
			   scm_list_1 (convert_module_name (name)));
  if (init)
    scm_c_call_with_current_module (module, (SCM (*)(void*))init, data);
  return module;
}

void
scm_c_use_module (const char *name)
{
  scm_call_1 (SCM_VARIABLE_REF (process_use_modules_var),
	      scm_list_1 (convert_module_name (name)));
}

static SCM module_export_x_var;

void
scm_c_export (const char *name, ...)
{
  va_list ap;
  SCM names = scm_cons (scm_str2symbol (name), SCM_EOL);
  SCM *tail = SCM_CDRLOC (names);
  va_start (ap, name);
  while (1)
    {
      const char *n = va_arg (ap, const char *);
      if (n == NULL)
	break;
      *tail = scm_cons (scm_str2symbol (n), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  scm_call_2 (SCM_VARIABLE_REF (module_export_x_var),
	      scm_current_module (), names);
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

SCM_SYMBOL (sym_module, "module");

static SCM the_root_module_var;

static SCM
the_root_module ()
{
  if (scm_module_system_booted_p)
    return SCM_VARIABLE_REF (the_root_module_var);
  else
    return SCM_BOOL_F;
}

SCM
scm_lookup_closure_module (SCM proc)
{
  if (SCM_FALSEP (proc))
    return the_root_module ();
  else if (SCM_EVAL_CLOSURE_P (proc))
    return SCM_PACK (SCM_SMOB_DATA (proc));
  else
    {
      SCM mod = scm_procedure_property (proc, sym_module);
      if (mod == SCM_BOOL_F)
	mod = the_root_module ();
      return mod;
    }
}

SCM_DEFINE (scm_env_module, "env-module", 1, 0, 0,
	    (SCM env),
	    "Return the module of @var{ENV}, a lexical environment.")
#define FUNC_NAME s_scm_env_module
{
  return scm_lookup_closure_module (scm_env_top_level (env));
}
#undef FUNC_NAME

/*
 * C level implementation of the standard eval closure
 *
 * This increases loading speed substantially.
 * The code will be replaced by the low-level environments in next release.
 */

static SCM module_make_local_var_x_var;

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
	b = scm_call_3 (binder, module, sym, SCM_BOOL_F);
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

scm_t_bits scm_tc16_eval_closure;

#define SCM_F_EVAL_CLOSURE_INTERFACE (1<<16)
#define SCM_EVAL_CLOSURE_INTERFACE_P(e) \
  (SCM_CELL_WORD_0 (e) & SCM_F_EVAL_CLOSURE_INTERFACE)

/* NOTE: This function may be called by a smob application
   or from another C function directly. */
SCM
scm_eval_closure_lookup (SCM eclo, SCM sym, SCM definep)
{
  SCM module = SCM_PACK (SCM_SMOB_DATA (eclo));
  if (SCM_NFALSEP (definep))
    {
      if (SCM_EVAL_CLOSURE_INTERFACE_P (eclo))
	return SCM_BOOL_F;
      return scm_call_2 (SCM_VARIABLE_REF (module_make_local_var_x_var),
			 module, sym);
    }
  else
    return module_variable (module, sym);
}

SCM_DEFINE (scm_standard_eval_closure, "standard-eval-closure", 1, 0, 0,
	    (SCM module),
	    "Return an eval closure for the module @var{module}.")
#define FUNC_NAME s_scm_standard_eval_closure
{
  SCM_RETURN_NEWSMOB (scm_tc16_eval_closure, SCM_UNPACK (module));
}
#undef FUNC_NAME

SCM_DEFINE (scm_standard_interface_eval_closure,
	    "standard-interface-eval-closure", 1, 0, 0,
	    (SCM module),
	    "Return a interface eval closure for the module @var{module}. "
	    "Such a closure does not allow new bindings to be added.")
#define FUNC_NAME s_scm_standard_interface_eval_closure
{
  SCM_RETURN_NEWSMOB (scm_tc16_eval_closure | SCM_F_EVAL_CLOSURE_INTERFACE,
		      SCM_UNPACK (module));
}
#undef FUNC_NAME

SCM
scm_module_lookup_closure (SCM module)
{
  if (module == SCM_BOOL_F)
    return SCM_BOOL_F;
  else
    return SCM_MODULE_EVAL_CLOSURE (module);
}

SCM
scm_current_module_lookup_closure ()
{
  if (scm_module_system_booted_p)
    return scm_module_lookup_closure (scm_current_module ());
  else
    return SCM_BOOL_F;
}

SCM
scm_module_transformer (SCM module)
{
  if (module == SCM_BOOL_F)
    return SCM_BOOL_F;
  else
    return SCM_MODULE_TRANSFORMER (module);
}

SCM
scm_current_module_transformer ()
{
  if (scm_module_system_booted_p)
    return scm_module_transformer (scm_current_module ());
  else
    return SCM_BOOL_F;
}

/* scm_sym2var
 *
 * looks up the variable bound to SYM according to PROC.  PROC should be
 * a `eval closure' of some module.
 *
 * When no binding exists, and DEFINEP is true, create a new binding
 * with a initial value of SCM_UNDEFINED.  Return `#f' when DEFINEP as
 * false and no binding exists.
 *
 * When PROC is `#f', it is ignored and the binding is searched for in
 * the scm_pre_modules_obarray (a `eq' hash table).
 */

SCM scm_pre_modules_obarray;

SCM 
scm_sym2var (SCM sym, SCM proc, SCM definep)
#define FUNC_NAME "scm_sym2var"
{
  SCM var;

  if (SCM_NIMP (proc))
    {
      if (SCM_EVAL_CLOSURE_P (proc))
	{
	  /* Bypass evaluator in the standard case. */
	  var = scm_eval_closure_lookup (proc, sym, definep);
	}
      else
	var = scm_call_2 (proc, sym, definep);
    }
  else
    {
      SCM handle;

      if (definep == SCM_BOOL_F)
	var = scm_hashq_ref (scm_pre_modules_obarray, sym, SCM_BOOL_F);
      else
	{
	  handle = scm_hashq_create_handle_x (scm_pre_modules_obarray,
					      sym, SCM_BOOL_F);
	  var = SCM_CDR (handle);
	  if (var == SCM_BOOL_F)
	    {
	      var = scm_make_variable (SCM_UNDEFINED);
	      SCM_SETCDR (handle, var);
	    }
	}
    }

  if (var != SCM_BOOL_F && !SCM_VARIABLEP (var))
    SCM_MISC_ERROR ("~S is not bound to a variable", scm_list_1 (sym));

  return var;
}
#undef FUNC_NAME

SCM
scm_c_module_lookup (SCM module, const char *name)
{
  return scm_module_lookup (module, scm_str2symbol (name));
}

SCM
scm_module_lookup (SCM module, SCM sym)
#define FUNC_NAME "module-lookup"
{
  SCM var;
  SCM_VALIDATE_MODULE (1, module);

  var = scm_sym2var (sym, scm_module_lookup_closure (module), SCM_BOOL_F);
  if (SCM_FALSEP (var))
    SCM_MISC_ERROR ("unbound variable: ~S", scm_list_1 (sym));
  return var;
}
#undef FUNC_NAME

SCM
scm_c_lookup (const char *name)
{
  return scm_lookup (scm_str2symbol (name));
}

SCM
scm_lookup (SCM sym)
{
  SCM var = 
    scm_sym2var (sym, scm_current_module_lookup_closure (), SCM_BOOL_F);
  if (SCM_FALSEP (var))
    scm_misc_error ("scm_lookup", "unbound variable: ~S", scm_list_1 (sym));
  return var;
}

SCM
scm_c_module_define (SCM module, const char *name, SCM value)
{
  return scm_module_define (module, scm_str2symbol (name), value);
}

SCM
scm_module_define (SCM module, SCM sym, SCM value)
#define FUNC_NAME "module-define"
{
  SCM var;
  SCM_VALIDATE_MODULE (1, module);

  var = scm_sym2var (sym, scm_module_lookup_closure (module), SCM_BOOL_T);
  SCM_VARIABLE_SET (var, value);
  return var;
}
#undef FUNC_NAME

SCM
scm_c_define (const char *name, SCM value)
{
  return scm_define (scm_str2symbol (name), value);
}

SCM
scm_define (SCM sym, SCM value)
{
  SCM var =
    scm_sym2var (sym, scm_current_module_lookup_closure (), SCM_BOOL_T);
  SCM_VARIABLE_SET (var, value);
  return var;
}

SCM
scm_module_reverse_lookup (SCM module, SCM variable)
#define FUNC_NAME "module-reverse-lookup"
{
  SCM obarray;
  long i, n;

  if (module == SCM_BOOL_F)
    obarray = scm_pre_modules_obarray;
  else
    {
      SCM_VALIDATE_MODULE (1, module);
      obarray = SCM_MODULE_OBARRAY (module);
    }

  /* XXX - We do not use scm_hash_fold here to avoid searching the
     whole obarray.  We should have a scm_hash_find procedure. */

  n = SCM_VECTOR_LENGTH (obarray);
  for (i = 0; i < n; ++i)
    {
      SCM ls = SCM_VELTS (obarray)[i], handle;
      while (!SCM_NULLP (ls))
	{
	  handle = SCM_CAR (ls);
	  if (SCM_CDR (handle) == variable)
	    return SCM_CAR (handle);
	  ls = SCM_CDR (ls);
	}
    }

  /* Try the `uses' list. 
   */
  {
    SCM uses = SCM_MODULE_USES (module);
    while (SCM_CONSP (uses))
      {
	SCM sym = scm_module_reverse_lookup (SCM_CAR (uses), variable);
	if (sym != SCM_BOOL_F)
	  return sym;
	uses = SCM_CDR (uses);
      }
  }

  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_pre_modules_obarray, "%get-pre-modules-obarray", 0, 0, 0,
	    (),
	    "Return the obarray that is used for all new bindings before "
	    "the module system is booted.  The first call to "
	    "@code{set-current-module} will boot the module system.")
#define FUNC_NAME s_scm_get_pre_modules_obarray
{
  return scm_pre_modules_obarray;
}
#undef FUNC_NAME

SCM_SYMBOL (scm_sym_system_module, "system-module");

SCM
scm_system_module_env_p (SCM env)
{
  SCM proc = scm_env_top_level (env);
  if (SCM_FALSEP (proc))
    return SCM_BOOL_T;
  return ((SCM_NFALSEP (scm_procedure_property (proc,
						scm_sym_system_module)))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

void
scm_modules_prehistory ()
{
  scm_pre_modules_obarray 
    = scm_permanent_object (scm_c_make_hash_table (2001));
}

void
scm_init_modules ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/modules.x"
#endif
  module_make_local_var_x_var = scm_c_define ("module-make-local-var!",
					    SCM_UNDEFINED);
  scm_tc16_eval_closure = scm_make_smob_type ("eval-closure", 0);
  scm_set_smob_mark (scm_tc16_eval_closure, scm_markcdr);
  scm_set_smob_apply (scm_tc16_eval_closure, scm_eval_closure_lookup, 2, 0, 0);

  the_module = scm_permanent_object (scm_make_fluid ());
}

static void
scm_post_boot_init_modules ()
{
#define PERM(x) scm_permanent_object(x)

  SCM module_type = SCM_VARIABLE_REF (scm_c_lookup ("module-type"));
  scm_module_tag = (SCM_CELL_WORD_1 (module_type) + scm_tc3_struct);

  resolve_module_var = PERM (scm_c_lookup ("resolve-module"));
  process_define_module_var = PERM (scm_c_lookup ("process-define-module"));
  process_use_modules_var = PERM (scm_c_lookup ("process-use-modules"));
  module_export_x_var = PERM (scm_c_lookup ("module-export!"));
  the_root_module_var = PERM (scm_c_lookup ("the-root-module"));

  scm_module_system_booted_p = 1;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
