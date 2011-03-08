/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

SCM_DEFINE (scm_thunk_p, "thunk?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a thunk.")
#define FUNC_NAME s_scm_thunk_p
{
  int req, opt, rest;
  return scm_from_bool (scm_i_procedure_arity (obj, &req, &opt, &rest)
                        && req == 0);
}
#undef FUNC_NAME

SCM_SYMBOL (sym_documentation, "documentation");

SCM_DEFINE (scm_procedure_documentation, "procedure-documentation", 1, 0, 0, 
           (SCM proc),
	    "Return the documentation string associated with @code{proc}.  By\n"
	    "convention, if a procedure contains more than one expression and the\n"
	    "first expression is a string constant, that string is assumed to contain\n"
	    "documentation for that procedure.")
#define FUNC_NAME s_scm_procedure_documentation
{
  SCM_VALIDATE_PROC (SCM_ARG1, proc);
  return scm_procedure_property (proc, sym_documentation);
}
#undef FUNC_NAME


/* Procedure-with-setter
 */

static SCM pws_vtable;


SCM_DEFINE (scm_procedure_with_setter_p, "procedure-with-setter?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a procedure with an\n"
	    "associated setter procedure.")
#define FUNC_NAME s_scm_procedure_with_setter_p
{
  return scm_from_bool (SCM_STRUCTP (obj) && SCM_STRUCT_SETTER_P (obj));
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
  ret = scm_make_struct (pws_vtable, SCM_INUM0,
                         scm_list_2 (procedure, setter));

  /* don't use procedure_name, because don't care enough to do a reverse
     lookup */
  name = scm_procedure_property (procedure, scm_sym_name);
  if (scm_is_true (name))
    scm_set_procedure_property_x (ret, scm_sym_name, name);
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure, "procedure", 1, 0, 0, 
            (SCM proc),
	    "Return the procedure of @var{proc}, which must be an\n"
	    "applicable struct.")
#define FUNC_NAME s_scm_procedure
{
  SCM_ASSERT (SCM_STRUCTP (proc) && SCM_STRUCT_APPLICABLE_P (proc),
              proc, SCM_ARG1, FUNC_NAME);
  return SCM_STRUCT_PROCEDURE (proc);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_setter, "setter", 1, 0, 0,
                       (SCM proc),
                       "Return the setter of @var{proc}, which must be an\n"
                       "applicable struct with a setter.")
#define FUNC_NAME s_scm_setter
{
  SCM_GASSERT1 (SCM_STRUCTP (proc), g_scm_setter, proc, SCM_ARG1, FUNC_NAME);
  if (SCM_STRUCT_SETTER_P (proc))
    return SCM_STRUCT_SETTER (proc);
  if (SCM_PUREGENERICP (proc)
      && SCM_IS_A_P (proc, scm_class_generic_with_setter))
    /* FIXME: might not be an accessor */
    return SCM_GENERIC_SETTER (proc);
  SCM_WTA_DISPATCH_1 (g_scm_setter, proc, SCM_ARG1, FUNC_NAME);
  return SCM_BOOL_F; /* not reached */
}
#undef FUNC_NAME


void
scm_init_procs ()
{
  pws_vtable =
    scm_c_make_struct (scm_applicable_struct_with_setter_vtable_vtable,
                       0,
                       1,
                       SCM_UNPACK (scm_from_latin1_symbol ("pwpw")));

#include "libguile/procs.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
