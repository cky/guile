/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2006, 2008, 2009 Free Software Foundation, Inc.
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

#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/smob.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/procs.h"
#include "libguile/procprop.h"
#include "libguile/objcodes.h"
#include "libguile/programs.h"



/* {Procedures}
 */


SCM
scm_c_make_subr (const char *name, long type, SCM (*fcn) ())
{
  register SCM z;
  SCM sname;
  SCM *meta_info;

  meta_info = scm_gc_malloc (2 * sizeof (*meta_info), "subr meta-info");
  sname = scm_from_locale_symbol (name);
  meta_info[0] = sname;
  meta_info[1] = SCM_EOL;  /* properties */

  z = scm_double_cell ((scm_t_bits) type, (scm_t_bits) fcn,
		       0 /* generic */, (scm_t_bits) meta_info);

  scm_remember_upto_here_1 (sname);

  return z;
}

SCM
scm_c_define_subr (const char *name, long type, SCM (*fcn) ())
{
  SCM subr = scm_c_make_subr (name, type, fcn);
  scm_define (SCM_SUBR_NAME (subr), subr);
  return subr;
}

SCM
scm_c_make_subr_with_generic (const char *name, 
			      long type, SCM (*fcn) (), SCM *gf)
{
  SCM subr = scm_c_make_subr (name, type, fcn);
  SCM_SET_SUBR_GENERIC_LOC (subr, gf);
  return subr;
}

SCM
scm_c_define_subr_with_generic (const char *name, 
				long type, SCM (*fcn) (), SCM *gf)
{
  SCM subr = scm_c_make_subr_with_generic (name, type, fcn, gf);
  scm_define (SCM_SUBR_NAME (subr), subr);
  return subr;
}


SCM_DEFINE (scm_procedure_p, "procedure?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a procedure.")
#define FUNC_NAME s_scm_procedure_p
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_struct:
	if (!((SCM_OBJ_CLASS_FLAGS (obj) & SCM_CLASSF_PURE_GENERIC)
              || SCM_STRUCT_APPLICABLE_P (obj)))
	  break;
      case scm_tcs_closures:
      case scm_tcs_subrs:
      case scm_tc7_pws:
      case scm_tc7_program:
	return SCM_BOOL_T;
      case scm_tc7_smob:
	return scm_from_bool (SCM_SMOB_DESCRIPTOR (obj).apply);
      default:
	return SCM_BOOL_F;
      }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_closure_p, "closure?", 1, 0, 0, 
           (SCM obj),
	    "Return @code{#t} if @var{obj} is a closure.")
#define FUNC_NAME s_scm_closure_p
{
  return scm_from_bool (SCM_CLOSUREP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_thunk_p, "thunk?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a thunk.")
#define FUNC_NAME s_scm_thunk_p
{
  if (SCM_NIMP (obj))
    {
    again:
      switch (SCM_TYP7 (obj))
	{
	case scm_tcs_closures:
	  return scm_from_bool (SCM_CLOSURE_NUM_REQUIRED_ARGS (obj) == 0);
	case scm_tc7_rpsubr:
	case scm_tc7_asubr:
	  return SCM_BOOL_T;
	case scm_tc7_gsubr:
	  return scm_from_bool (SCM_GSUBR_REQ (SCM_GSUBR_TYPE (obj)) == 0);
	case scm_tc7_program:
          {
            int a, o, r;
            if (scm_i_program_arity (obj, &a, &o, &r))
              return scm_from_bool (a == 0);
            else
              return SCM_BOOL_F;
          }
	case scm_tc7_pws:
	  obj = SCM_PROCEDURE (obj);
	  goto again;
	default:
          return SCM_BOOL_F;
	}
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Only used internally. */
int
scm_subr_p (SCM obj)
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_subrs:
	return 1;
      default:
	;
      }
  return 0;
}

SCM_SYMBOL (sym_documentation, "documentation");

SCM_DEFINE (scm_procedure_documentation, "procedure-documentation", 1, 0, 0, 
           (SCM proc),
	    "Return the documentation string associated with @code{proc}.  By\n"
	    "convention, if a procedure contains more than one expression and the\n"
	    "first expression is a string constant, that string is assumed to contain\n"
	    "documentation for that procedure.")
#define FUNC_NAME s_scm_procedure_documentation
{
  SCM code;
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
	      proc, SCM_ARG1, FUNC_NAME);
  if (SCM_PROGRAM_P (proc))
    return scm_assq_ref (scm_program_properties (proc), sym_documentation);
  switch (SCM_TYP7 (proc))
    {
    case scm_tcs_closures:
      code = SCM_CLOSURE_BODY (proc);
      if (scm_is_null (SCM_CDR (code)))
	return SCM_BOOL_F;
      code = SCM_CAR (code);
      if (scm_is_string (code))
	return code;
      else
	return SCM_BOOL_F;
    default:
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME


/* Procedure-with-setter
 */

SCM_DEFINE (scm_procedure_with_setter_p, "procedure-with-setter?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a procedure with an\n"
	    "associated setter procedure.")
#define FUNC_NAME s_scm_procedure_with_setter_p
{
  return scm_from_bool(SCM_PROCEDURE_WITH_SETTER_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_procedure_with_setter, "make-procedure-with-setter", 2, 0, 0, 
            (SCM procedure, SCM setter),
	    "Create a new procedure which behaves like @var{procedure}, but\n"
	    "with the associated setter @var{setter}.")
#define FUNC_NAME s_scm_make_procedure_with_setter
{
  SCM name, ret;
  SCM_VALIDATE_PROC (1, procedure);
  SCM_VALIDATE_PROC (2, setter);
  ret = scm_double_cell (scm_tc7_pws,
                         SCM_UNPACK (procedure),
                         SCM_UNPACK (setter), 0);
  /* don't use procedure_name, because don't care enough to do a reverse
     lookup */
  switch (SCM_TYP7 (procedure)) {
  case scm_tcs_subrs:
    name = SCM_SUBR_NAME (procedure);
    break;
  default:
    name = scm_procedure_property (procedure, scm_sym_name);
    break;
  }
  if (scm_is_true (name))
    scm_set_procedure_property_x (ret, scm_sym_name, name);
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure, "procedure", 1, 0, 0, 
            (SCM proc),
	    "Return the procedure of @var{proc}, which must be either a\n"
	    "procedure with setter, or an applicable struct.")
#define FUNC_NAME s_scm_procedure
{
  SCM_VALIDATE_NIM (1, proc);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_PROCEDURE (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM_ASSERT (SCM_PUREGENERICP (proc) || SCM_STRUCT_APPLICABLE_P (proc),
                  proc, SCM_ARG1, FUNC_NAME);
      return proc;
    }
  SCM_WRONG_TYPE_ARG (1, proc);
  return SCM_BOOL_F; /* not reached */
}
#undef FUNC_NAME

SCM_GPROC (s_setter, "setter", 1, 0, 0, scm_setter, g_setter);

SCM
scm_setter (SCM proc)
{
  SCM_GASSERT1 (SCM_NIMP (proc), g_setter, proc, SCM_ARG1, s_setter);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_SETTER (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM setter = SCM_BOOL_F;
      if (SCM_PUREGENERICP (proc))
        setter = SCM_GENERIC_SETTER (proc);
      else if (SCM_STRUCT_SETTER_P (proc))
        setter = SCM_STRUCT_SETTER (proc);
      if (SCM_NIMP (setter))
	return setter;
      /* fall through */
    }
  SCM_WTA_DISPATCH_1 (g_setter, proc, SCM_ARG1, s_setter);
  return SCM_BOOL_F; /* not reached */
}


void
scm_init_procs ()
{
#include "libguile/procs.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
