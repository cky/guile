/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

/* Copyright (C) 2003 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "libguile/_scm.h"
#include "libguile/deprecated.h"
#include "libguile/deprecation.h"
#include "libguile/snarf.h"
#include "libguile/validate.h"
#include "libguile/strings.h"
#include "libguile/strop.h"
#include "libguile/modules.h"
#include "libguile/eval.h"
#include "libguile/smob.h"
#include "libguile/procprop.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"
#include "libguile/struct.h"
#include "libguile/variable.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"

#include <stdio.h>
#include <string.h>

#if (SCM_ENABLE_DEPRECATED == 1)

SCM_REGISTER_PROC(s_substring_move_left_x, "substring-move-left!", 5, 0, 0, scm_substring_move_x);

SCM_REGISTER_PROC(s_substring_move_right_x, "substring-move-right!", 5, 0, 0, scm_substring_move_x);

SCM
scm_wta (SCM arg, const char *pos, const char *s_subr)
{
  if (!s_subr || !*s_subr)
    s_subr = NULL;
  if ((~0x1fL) & (long) pos)
    {
      /* error string supplied.  */
      scm_misc_error (s_subr, pos, scm_list_1 (arg));
    }
  else
    {
      /* numerical error code.  */
      scm_t_bits error = (scm_t_bits) pos;

      switch (error)
	{
	case SCM_ARGn:
	  scm_wrong_type_arg (s_subr, 0, arg);
	case SCM_ARG1:
	  scm_wrong_type_arg (s_subr, 1, arg);
	case SCM_ARG2:
	  scm_wrong_type_arg (s_subr, 2, arg);
	case SCM_ARG3:
	  scm_wrong_type_arg (s_subr, 3, arg);
	case SCM_ARG4:
	  scm_wrong_type_arg (s_subr, 4, arg);
	case SCM_ARG5:
	  scm_wrong_type_arg (s_subr, 5, arg);
	case SCM_ARG6:
	  scm_wrong_type_arg (s_subr, 6, arg);
	case SCM_ARG7:
	  scm_wrong_type_arg (s_subr, 7, arg);
	case SCM_WNA:
	  scm_wrong_num_args (arg);
	case SCM_OUTOFRANGE:
	  scm_out_of_range (s_subr, arg);
	case SCM_NALLOC:
	  scm_memory_error (s_subr);
	default:
	  /* this shouldn't happen.  */
	  scm_misc_error (s_subr, "Unknown error", SCM_EOL);
	}
    }
  return SCM_UNSPECIFIED;
}

/* Module registry
 */

/* We can't use SCM objects here. One should be able to call
   SCM_REGISTER_MODULE from a C++ constructor for a static
   object. This happens before main and thus before libguile is
   initialized. */

struct moddata {
  struct moddata *link;
  char *module_name;
  void *init_func;
};

static struct moddata *registered_mods = NULL;

void
scm_register_module_xxx (char *module_name, void *init_func)
{
  struct moddata *md;

  scm_c_issue_deprecation_warning 
    ("`scm_register_module_xxx' is deprecated.  Use extensions instead.");

  /* XXX - should we (and can we) DEFER_INTS here? */

  for (md = registered_mods; md; md = md->link)
    if (!strcmp (md->module_name, module_name))
      {
	md->init_func = init_func;
	return;
      }

  md = (struct moddata *) malloc (sizeof (struct moddata));
  if (md == NULL)
    {
      fprintf (stderr,
	       "guile: can't register module (%s): not enough memory",
	       module_name);
      return;
    }

  md->module_name = module_name;
  md->init_func = init_func;
  md->link = registered_mods;
  registered_mods = md;
}

SCM_DEFINE (scm_registered_modules, "c-registered-modules", 0, 0, 0, 
            (),
	    "Return a list of the object code modules that have been imported into\n"
	    "the current Guile process.  Each element of the list is a pair whose\n"
	    "car is the name of the module, and whose cdr is the function handle\n"
	    "for that module's initializer function.  The name is the string that\n"
	    "has been passed to scm_register_module_xxx.")
#define FUNC_NAME s_scm_registered_modules
{
  SCM res;
  struct moddata *md;

  res = SCM_EOL;
  for (md = registered_mods; md; md = md->link)
    res = scm_cons (scm_cons (scm_makfrom0str (md->module_name),
			      scm_ulong2num ((unsigned long) md->init_func)),
		    res);
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_clear_registered_modules, "c-clear-registered-modules", 0, 0, 0, 
            (),
	    "Destroy the list of modules registered with the current Guile process.\n"
	    "The return value is unspecified.  @strong{Warning:} this function does\n"
	    "not actually unlink or deallocate these modules, but only destroys the\n"
	    "records of which modules have been loaded.  It should therefore be used\n"
	    "only by module bookkeeping operations.")
#define FUNC_NAME s_scm_clear_registered_modules
{
  struct moddata *md1, *md2;

  SCM_DEFER_INTS;

  for (md1 = registered_mods; md1; md1 = md2)
    {
      md2 = md1->link;
      free (md1);
    }
  registered_mods = NULL;

  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_remember (SCM *ptr)
{
  scm_c_issue_deprecation_warning ("`scm_remember' is deprecated. "
                                   "Use the `scm_remember_upto_here*' family of functions instead.");
}

SCM
scm_protect_object (SCM obj)
{
  scm_c_issue_deprecation_warning ("`scm_protect_object' is deprecated. "
                                   "Use `scm_gc_protect_object' instead.");
  return scm_gc_protect_object (obj);
}

SCM
scm_unprotect_object (SCM obj)
{
  scm_c_issue_deprecation_warning ("`scm_unprotect_object' is deprecated. "
                                   "Use `scm_gc_unprotect_object' instead.");
  return scm_gc_unprotect_object (obj);
}

SCM_SYMBOL (scm_sym_app, "app");
SCM_SYMBOL (scm_sym_modules, "modules");
static SCM module_prefix = SCM_BOOL_F;
static SCM make_modules_in_var;
static SCM beautify_user_module_x_var;
static SCM try_module_autoload_var;

static void
init_module_stuff ()
{
#define PERM(x) scm_permanent_object(x)

  if (module_prefix == SCM_BOOL_F)
    {
      module_prefix = PERM (scm_list_2 (scm_sym_app, scm_sym_modules));
      make_modules_in_var = PERM (scm_c_lookup ("make-modules-in"));
      beautify_user_module_x_var =
	PERM (scm_c_lookup ("beautify-user-module!"));
      try_module_autoload_var = PERM (scm_c_lookup ("try-module-autoload"));
    }
}

SCM
scm_the_root_module ()
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_the_root_module' is deprecated. "
				   "Use `scm_c_resolve_module (\"guile\")' "
				   "instead.");

  return scm_c_resolve_module ("guile");
}

static SCM
scm_module_full_name (SCM name)
{
  init_module_stuff ();
  if (SCM_EQ_P (SCM_CAR (name), scm_sym_app))
    return name;
  else
    return scm_append (scm_list_2 (module_prefix, name));
}

SCM
scm_make_module (SCM name)
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_make_module' is deprecated. "
				   "Use `scm_c_define_module instead.");

  return scm_call_2 (SCM_VARIABLE_REF (make_modules_in_var),
		     scm_the_root_module (),
		     scm_module_full_name (name));
}

SCM
scm_ensure_user_module (SCM module)
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_ensure_user_module' is deprecated. "
				   "Use `scm_c_define_module instead.");

  scm_call_1 (SCM_VARIABLE_REF (beautify_user_module_x_var), module);
  return SCM_UNSPECIFIED;
}

SCM
scm_load_scheme_module (SCM name)
{
  init_module_stuff ();
  scm_c_issue_deprecation_warning ("`scm_load_scheme_module' is deprecated. "
				   "Use `scm_c_resolve_module instead.");

  return scm_call_1 (SCM_VARIABLE_REF (try_module_autoload_var), name);
}

/* This is implemented in C solely for SCM_COERCE_OUTPORT ... */

static void
maybe_close_port (void *data, SCM port)
{
  SCM except = (SCM)data;

  while (!SCM_NULLP (except))
    {
      SCM p = SCM_COERCE_OUTPORT (SCM_CAR (except));
      if (SCM_EQ_P (p, port))
	return;
      except = SCM_CDR (except);
    }

  scm_close_port (port);
}

SCM_DEFINE (scm_close_all_ports_except, "close-all-ports-except", 0, 0, 1,
           (SCM ports),
	    "[DEPRECATED] Close all open file ports used by the interpreter\n"
	    "except for those supplied as arguments.  This procedure\n"
	    "was intended to be used before an exec call to close file descriptors\n"
	    "which are not needed in the new process.  However it has the\n"
	    "undesirable side effect of flushing buffers, so it's deprecated.\n"
	    "Use port-for-each instead.")
#define FUNC_NAME s_scm_close_all_ports_except
{
  SCM p;
  SCM_VALIDATE_REST_ARGUMENT (ports);
  
  for (p = ports; !SCM_NULLP (p); p = SCM_CDR (p))
    SCM_VALIDATE_OPPORT (SCM_ARG1, SCM_COERCE_OUTPORT (SCM_CAR (p)));

  scm_c_port_for_each (maybe_close_port, ports);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_i_init_deprecated ()
{
#include "libguile/deprecated.x"
}

#endif
