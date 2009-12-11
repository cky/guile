/* Copyright (C) 1995,1996,1998,2000,2001,2003,2004, 2006, 2008, 2009 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"

#include "libguile/alist.h"
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
SCM_GLOBAL_SYMBOL (scm_sym_arity, "arity");

static SCM props;
static scm_i_pthread_mutex_t props_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

int
scm_i_procedure_arity (SCM proc, int *req, int *opt, int *rest)
{
  if (SCM_IMP (proc))
    return 0;
 loop:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_program:
      return scm_i_program_arity (proc, req, opt, rest);
    case scm_tc7_smob:
      if (SCM_SMOB_APPLICABLE_P (proc))
	{
	  int type = SCM_SMOB_DESCRIPTOR (proc).gsubr_type;
	  *req = SCM_GSUBR_REQ (type);
	  *opt = SCM_GSUBR_OPT (type);
	  *rest = SCM_GSUBR_REST (type);
          return 1;
	}
      else
        return 0;
    case scm_tc7_gsubr:
      {
	unsigned int type = SCM_GSUBR_TYPE (proc);
	*req = SCM_GSUBR_REQ (type);
	*opt = SCM_GSUBR_OPT (type);
	*rest = SCM_GSUBR_REST (type);
        return 1;
      }
    case scm_tcs_struct:
      if (!SCM_STRUCT_APPLICABLE_P (proc))
        return 0;
      proc = SCM_STRUCT_PROCEDURE (proc);
      goto loop;
    default:
      return 0;
    }
}

/* FIXME: instead of the weak hash, perhaps for some kinds of procedures, use
   other means; for example subrs have their own property slot, which is unused
   at present. */

SCM_DEFINE (scm_procedure_properties, "procedure-properties", 1, 0, 0, 
           (SCM proc),
	    "Return @var{obj}'s property list.")
#define FUNC_NAME s_scm_procedure_properties
{
  SCM ret;
  int req, opt, rest;
  
  SCM_VALIDATE_PROC (1, proc);

  scm_i_pthread_mutex_lock (&props_lock);
  ret = scm_hashq_ref (props, proc, SCM_EOL);
  scm_i_pthread_mutex_unlock (&props_lock);

  scm_i_procedure_arity (proc, &req, &opt, &rest);

  return scm_acons (scm_sym_arity,
                    scm_list_3 (scm_from_int (req),
                                scm_from_int (opt),
                                scm_from_bool (rest)),
                    ret);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_properties_x, "set-procedure-properties!", 2, 0, 0,
           (SCM proc, SCM alist),
	    "Set @var{proc}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_procedure_properties_x
{
  SCM_VALIDATE_PROC (1, proc);

  scm_i_pthread_mutex_lock (&props_lock);
  scm_hashq_set_x (props, proc, alist);
  scm_i_pthread_mutex_unlock (&props_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_property, "procedure-property", 2, 0, 0,
           (SCM proc, SCM key),
	    "Return the property of @var{proc} with name @var{key}.")
#define FUNC_NAME s_scm_procedure_property
{
  SCM_VALIDATE_PROC (1, proc);

  if (scm_is_eq (key, scm_sym_arity))
    /* avoid a cons in this case */
    {
      int req, opt, rest;
      scm_i_procedure_arity (proc, &req, &opt, &rest);
      return scm_list_3 (scm_from_int (req),
                         scm_from_int (opt),
                         scm_from_bool (rest));
    }
  else
    {
      SCM ret;

      scm_i_pthread_mutex_lock (&props_lock);
      ret = scm_hashq_ref (props, proc, SCM_EOL);
      scm_i_pthread_mutex_unlock (&props_lock);

      return scm_assq_ref (ret, key);
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_property_x, "set-procedure-property!", 3, 0, 0,
           (SCM proc, SCM key, SCM val),
	    "In @var{proc}'s property list, set the property named @var{key} to\n"
	    "@var{val}.")
#define FUNC_NAME s_scm_set_procedure_property_x
{
  SCM_VALIDATE_PROC (1, proc);

  if (scm_is_eq (key, scm_sym_arity))
    SCM_MISC_ERROR ("arity is a read-only property", SCM_EOL);

  scm_i_pthread_mutex_lock (&props_lock);
  scm_hashq_set_x (props, proc,
                   scm_assq_set_x (scm_hashq_ref (props, proc,
                                                  SCM_EOL),
                                   key, val));
  scm_i_pthread_mutex_unlock (&props_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_init_procprop ()
{
  props = scm_make_weak_key_hash_table (SCM_UNDEFINED);
#include "libguile/procprop.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
