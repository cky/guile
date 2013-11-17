/* Copyright (C) 1995, 1996, 1998, 2000, 2001, 2003, 2004, 2006,
 *   2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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

#define SCM_BUILDING_DEPRECATED_CODE

#include "libguile/_scm.h"

#include "libguile/alist.h"
#include "libguile/deprecation.h"
#include "libguile/deprecated.h"
#include "libguile/eval.h"
#include "libguile/procs.h"
#include "libguile/gsubr.h"
#include "libguile/smob.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"
#include "libguile/programs.h"

#include "libguile/validate.h"
#include "libguile/procprop.h"


SCM_GLOBAL_SYMBOL (scm_sym_system_procedure, "system-procedure");
#if (SCM_ENABLE_DEPRECATED == 1)
SCM_GLOBAL_SYMBOL (scm_sym_arity, "arity");
#endif
SCM_GLOBAL_SYMBOL (scm_sym_name, "name");

static SCM overrides;
static scm_i_pthread_mutex_t overrides_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

static SCM arity_overrides;

int
scm_i_procedure_arity (SCM proc, int *req, int *opt, int *rest)
{
  SCM o;

  scm_i_pthread_mutex_lock (&overrides_lock);
  o = scm_hashq_ref (arity_overrides, proc, SCM_BOOL_F);
  scm_i_pthread_mutex_unlock (&overrides_lock);

  if (scm_is_true (o))
    {
      *req = scm_to_int (scm_car (o));
      *opt = scm_to_int (scm_cadr (o));
      *rest = scm_is_true (scm_caddr (o));
      return 1;
    }

  while (!SCM_PROGRAM_P (proc))
    {
      if (SCM_IMP (proc))
        return 0;
      switch (SCM_TYP7 (proc))
        {
        case scm_tc7_smob:
          if (!SCM_SMOB_APPLICABLE_P (proc))
            return 0;
          if (!scm_i_program_arity
              (SCM_SMOB_DESCRIPTOR (proc).apply_trampoline_objcode,
               req, opt, rest))
            return 0;

          /* The trampoline gets the smob too, which users don't
             see.  */
          *req -= 1;

          return 1;
        case scm_tcs_struct:
          if (!SCM_STRUCT_APPLICABLE_P (proc))
            return 0;
          proc = SCM_STRUCT_PROCEDURE (proc);
          break;
        default:
          return 0;
        }
    }

  return scm_i_program_arity (proc, req, opt, rest);
}

SCM_DEFINE (scm_set_procedure_minimum_arity_x, "set-procedure-minimum-arity!",
            4, 0, 0, (SCM proc, SCM req, SCM opt, SCM rest),
            "")
#define FUNC_NAME s_scm_set_procedure_minimum_arity_x
{
  int t SCM_UNUSED;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_INT_COPY (2, req, t);
  SCM_VALIDATE_INT_COPY (3, opt, t);
  SCM_VALIDATE_BOOL (4, rest);

  scm_i_pthread_mutex_lock (&overrides_lock);
  scm_hashq_set_x (arity_overrides, proc, scm_list_3 (req, opt, rest));
  scm_i_pthread_mutex_unlock (&overrides_lock);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_minimum_arity, "procedure-minimum-arity", 1, 0, 0, 
           (SCM proc),
	    "Return the \"minimum arity\" of a procedure.\n\n"
            "If the procedure has only one arity, that arity is returned\n"
            "as a list of three values: the number of required arguments,\n"
            "the number of optional arguments, and a boolean indicating\n"
            "whether or not the procedure takes rest arguments.\n\n"
            "For a case-lambda procedure, the arity returned is the one\n"
            "with the lowest minimum number of arguments, and the highest\n"
            "maximum number of arguments.\n\n"
            "If it was not possible to determine the arity of the procedure,\n"
            "@code{#f} is returned.")
#define FUNC_NAME s_scm_procedure_minimum_arity
{
  int req, opt, rest;
  
  if (scm_i_procedure_arity (proc, &req, &opt, &rest))
    return scm_list_3 (scm_from_int (req),
                       scm_from_int (opt),
                       scm_from_bool (rest));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_properties, "procedure-properties", 1, 0, 0, 
           (SCM proc),
	    "Return @var{proc}'s property list.")
#define FUNC_NAME s_scm_procedure_properties
{
  SCM ret;
  
  SCM_VALIDATE_PROC (1, proc);

  scm_i_pthread_mutex_lock (&overrides_lock);
  ret = scm_hashq_ref (overrides, proc, SCM_BOOL_F);
  scm_i_pthread_mutex_unlock (&overrides_lock);

  if (scm_is_false (ret))
    {
      if (SCM_PROGRAM_P (proc))
        ret = scm_i_program_properties (proc);
      else
        ret = SCM_EOL;
    }
  
#if (SCM_ENABLE_DEPRECATED == 1)
  ret = scm_acons (scm_sym_arity, scm_procedure_minimum_arity (proc), ret);
#endif

  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_properties_x, "set-procedure-properties!", 2, 0, 0,
           (SCM proc, SCM alist),
	    "Set @var{proc}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_procedure_properties_x
{
  SCM_VALIDATE_PROC (1, proc);

#if (SCM_ENABLE_DEPRECATED == 1)
  if (scm_is_true (scm_assq (scm_sym_arity, alist)))
    SCM_MISC_ERROR ("arity is a read-only property", SCM_EOL);
#endif

  scm_i_pthread_mutex_lock (&overrides_lock);
  scm_hashq_set_x (overrides, proc, alist);
  scm_i_pthread_mutex_unlock (&overrides_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_property, "procedure-property", 2, 0, 0,
           (SCM proc, SCM key),
	    "Return the property of @var{proc} with name @var{key}.")
#define FUNC_NAME s_scm_procedure_property
{
  SCM_VALIDATE_PROC (1, proc);

#if (SCM_ENABLE_DEPRECATED == 1)
  if (scm_is_eq (key, scm_sym_arity))
    scm_c_issue_deprecation_warning
      ("Accessing a procedure's arity via `procedure-property' is deprecated.\n"
       "Use `procedure-minimum-arity instead.");
#endif

  return scm_assq_ref (scm_procedure_properties (proc), key);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_property_x, "set-procedure-property!", 3, 0, 0,
           (SCM proc, SCM key, SCM val),
	    "In @var{proc}'s property list, set the property named @var{key} to\n"
	    "@var{val}.")
#define FUNC_NAME s_scm_set_procedure_property_x
{
  SCM props;

  SCM_VALIDATE_PROC (1, proc);

#if (SCM_ENABLE_DEPRECATED == 1)
  if (scm_is_eq (key, scm_sym_arity))
    SCM_MISC_ERROR ("arity is a deprecated read-only property", SCM_EOL);
#endif

  scm_dynwind_begin (0);
  /* Here we must block asyncs while overrides_lock is held, to avoid
     deadlocks which can happen as follows: scm_i_program_properties
     calls out to the VM, which will run asyncs.  Asyncs are permitted
     to run VM code, which sometimes checks procedure properties, which
     locks overrides_lock. */
  scm_i_dynwind_pthread_mutex_lock_block_asyncs (&overrides_lock);
  props = scm_hashq_ref (overrides, proc, SCM_BOOL_F);
  if (scm_is_false (props))
    {
      if (SCM_PROGRAM_P (proc))
        props = scm_i_program_properties (proc);
      else
        props = SCM_EOL;
    }
  scm_hashq_set_x (overrides, proc, scm_assq_set_x (props, key, val));
  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_init_procprop ()
{
  overrides = scm_make_weak_key_hash_table (SCM_UNDEFINED);
  arity_overrides = scm_make_weak_key_hash_table (SCM_UNDEFINED);
#include "libguile/procprop.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
