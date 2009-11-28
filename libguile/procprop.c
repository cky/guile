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

static SCM non_closure_props;
static scm_i_pthread_mutex_t non_closure_props_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

SCM
scm_i_procedure_arity (SCM proc)
{
  int a = 0, o = 0, r = 0;
  if (SCM_IMP (proc))
    return SCM_BOOL_F;
 loop:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_1o:
      o = 1;
    case scm_tc7_subr_0:
      break;
    case scm_tc7_subr_2o:
      o = 1;
    case scm_tc7_subr_1:
    case scm_tc7_dsubr:
    case scm_tc7_cxr:
      a += 1;
      break;
    case scm_tc7_subr_2:
      a += 2;
      break;
    case scm_tc7_subr_3:
      a += 3;
      break;
    case scm_tc7_asubr:
    case scm_tc7_rpsubr:
    case scm_tc7_lsubr:
      r = 1;
      break;
    case scm_tc7_program:
      if (scm_i_program_arity (proc, &a, &o, &r))
        break;
      else
        return SCM_BOOL_F;
    case scm_tc7_lsubr_2:
      a += 2;
      r = 1;
      break;
    case scm_tc7_smob:
      if (SCM_SMOB_APPLICABLE_P (proc))
	{
	  int type = SCM_SMOB_DESCRIPTOR (proc).gsubr_type;
	  a += SCM_GSUBR_REQ (type);
	  o = SCM_GSUBR_OPT (type);
	  r = SCM_GSUBR_REST (type);
	  break;
	}
      else
	{
	  return SCM_BOOL_F;
	}
    case scm_tc7_gsubr:
      {
	unsigned int type = SCM_GSUBR_TYPE (proc);
	a = SCM_GSUBR_REQ (type);
	o = SCM_GSUBR_OPT (type);
	r = SCM_GSUBR_REST (type);
	break;
      }
    case scm_tc7_pws:
      proc = SCM_PROCEDURE (proc);
      goto loop;
    case scm_tcs_closures:
      a = SCM_CLOSURE_NUM_REQUIRED_ARGS (proc);
      r = SCM_CLOSURE_HAS_REST_ARGS (proc) ? 1 : 0;
      break;
    case scm_tcs_struct:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	{
	  r = 1;
	  break;
	}
      else if (!SCM_STRUCT_APPLICABLE_P (proc))
        return SCM_BOOL_F;
      proc = SCM_STRUCT_PROCEDURE (proc);
      goto loop;
    default:
      return SCM_BOOL_F;
    }
  return scm_list_3 (scm_from_int (a), scm_from_int (o), scm_from_bool(r));
}

/* FIXME: instead of the weak hash, perhaps for some kinds of procedures, use
   other means; for example subrs have their own property slot, which is unused
   at present. */

SCM_DEFINE (scm_procedure_properties, "procedure-properties", 1, 0, 0, 
           (SCM proc),
	    "Return @var{obj}'s property list.")
#define FUNC_NAME s_scm_procedure_properties
{
  SCM props;
  
  SCM_VALIDATE_PROC (1, proc);
  if (SCM_CLOSUREP (proc))
    props = SCM_PROCPROPS (proc);
  else
    {
      scm_i_pthread_mutex_lock (&non_closure_props_lock);
      props = scm_hashq_ref (non_closure_props, proc, SCM_EOL);
      scm_i_pthread_mutex_unlock (&non_closure_props_lock);
    }
  return scm_acons (scm_sym_arity, scm_i_procedure_arity (proc), props);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_properties_x, "set-procedure-properties!", 2, 0, 0,
           (SCM proc, SCM alist),
	    "Set @var{proc}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_procedure_properties_x
{
  SCM_VALIDATE_PROC (1, proc);

  if (SCM_CLOSUREP (proc))
    SCM_SETPROCPROPS (proc, alist);
  else
    {
      scm_i_pthread_mutex_lock (&non_closure_props_lock);
      scm_hashq_set_x (non_closure_props, proc, alist);
      scm_i_pthread_mutex_unlock (&non_closure_props_lock);
    }
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
    return scm_i_procedure_arity (proc);
  else
    {
      SCM props;
      if (SCM_CLOSUREP (proc))
        props = SCM_PROCPROPS (proc);
      else
        {
          scm_i_pthread_mutex_lock (&non_closure_props_lock);
          props = scm_hashq_ref (non_closure_props, proc, SCM_EOL);
          scm_i_pthread_mutex_unlock (&non_closure_props_lock);
        }
      return scm_assq_ref (props, key);
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

  if (SCM_CLOSUREP (proc))
    SCM_SETPROCPROPS (proc,
                      scm_assq_set_x (SCM_PROCPROPS (proc), key, val));
  else
    {
      scm_i_pthread_mutex_lock (&non_closure_props_lock);
      scm_hashq_set_x (non_closure_props, proc,
                       scm_assq_set_x (scm_hashq_ref (non_closure_props, proc,
                                                      SCM_EOL),
                                       key, val));
      scm_i_pthread_mutex_unlock (&non_closure_props_lock);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_init_procprop ()
{
  non_closure_props = scm_make_weak_key_hash_table (SCM_UNDEFINED);
#include "libguile/procprop.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
