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
#include "libguile/eq.h"
#include "libguile/read.h"
#include "libguile/strports.h"
#include "libguile/smob.h"

#include <stdio.h>
#include <string.h>

#if (SCM_ENABLE_DEPRECATED == 1)

/* From eval.c: Error messages of the evaluator.  These were deprecated in
 * guile 1.7.0 on 2003-06-02.  */
const char scm_s_expression[] = "missing or extra expression";
const char scm_s_test[] = "bad test";
const char scm_s_body[] = "bad body";
const char scm_s_bindings[] = "bad bindings";
const char scm_s_variable[] = "bad variable";
const char scm_s_clauses[] = "bad or missing clauses";
const char scm_s_formals[] = "bad formals";


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

SCM_DEFINE (scm_variable_set_name_hint, "variable-set-name-hint!", 2, 0, 0,
	    (SCM var, SCM hint),
	    "Do not use this function.")
#define FUNC_NAME s_scm_variable_set_name_hint
{
  SCM_VALIDATE_VARIABLE (1, var);
  SCM_VALIDATE_SYMBOL (2, hint);
  scm_c_issue_deprecation_warning
    ("'variable-set-name-hint!' is deprecated.  Do not use it.");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_builtin_variable, "builtin-variable", 1, 0, 0, 
            (SCM name),
	    "Do not use this function.")
#define FUNC_NAME s_scm_builtin_variable
{
  SCM_VALIDATE_SYMBOL (1,name);
  scm_c_issue_deprecation_warning ("`builtin-variable' is deprecated. "
				   "Use module system operations instead.");
  return scm_sym2var (name, SCM_BOOL_F, SCM_BOOL_T);
}
#undef FUNC_NAME

SCM 
scm_makstr (size_t len, int dummy)
{
  scm_c_issue_deprecation_warning
    ("'scm_makstr' is deprecated.  Use 'scm_allocate_string' instead.");
  return scm_allocate_string (len);
}

SCM 
scm_makfromstr (const char *src, size_t len, int dummy SCM_UNUSED)
{
  scm_c_issue_deprecation_warning ("`scm_makfromstr' is deprecated. "
				   "Use `scm_mem2string' instead.");

  return scm_mem2string (src, len);
}

SCM
scm_internal_with_fluids (SCM fluids, SCM values, SCM (*cproc) (), void *cdata)
{
  scm_c_issue_deprecation_warning ("`scm_internal_with_fluids' is deprecated. "
				   "Use `scm_c_with_fluids' instead.");

  return scm_c_with_fluids (fluids, values, cproc, cdata);
}

SCM
scm_make_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  scm_c_issue_deprecation_warning
    ("`scm_make_gsubr' is deprecated.  Use `scm_c_define_gsubr' instead.");

  return scm_c_define_gsubr (name, req, opt, rst, fcn);
}

SCM
scm_make_gsubr_with_generic (const char *name,
			     int req, int opt, int rst,
			     SCM (*fcn)(), SCM *gf)
{
  scm_c_issue_deprecation_warning
    ("`scm_make_gsubr_with_generic' is deprecated.  "
     "Use `scm_c_define_gsubr_with_generic' instead.");

  return scm_c_define_gsubr_with_generic (name, req, opt, rst, fcn, gf);
}

SCM
scm_create_hook (const char *name, int n_args)
{
  scm_c_issue_deprecation_warning
    ("'scm_create_hook' is deprecated.  "
     "Use 'scm_make_hook' and 'scm_c_define' instead.");
  {
    SCM hook = scm_make_hook (SCM_MAKINUM (n_args));
    scm_c_define (name, hook);
    return scm_permanent_object (hook);
  }
}

SCM_DEFINE (scm_sloppy_memq, "sloppy-memq", 2, 0, 0,
            (SCM x, SCM lst),
	    "This procedure behaves like @code{memq}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_memq
{
  scm_c_issue_deprecation_warning
    ("'sloppy-memq' is deprecated.  Use 'memq' instead.");

  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_EQ_P (SCM_CAR (lst), x))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_memv, "sloppy-memv", 2, 0, 0,
            (SCM x, SCM lst),
 	    "This procedure behaves like @code{memv}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_memv
{
  scm_c_issue_deprecation_warning
    ("'sloppy-memv' is deprecated.  Use 'memv' instead.");

  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (! SCM_FALSEP (scm_eqv_p (SCM_CAR (lst), x)))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_member, "sloppy-member", 2, 0, 0,
            (SCM x, SCM lst),
 	    "This procedure behaves like @code{member}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_member
{
  scm_c_issue_deprecation_warning
    ("'sloppy-member' is deprecated.  Use 'member' instead.");

  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (! SCM_FALSEP (scm_equal_p (SCM_CAR (lst), x)))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME

SCM_SYMBOL (scm_end_of_file_key, "end-of-file");

SCM_DEFINE (scm_read_and_eval_x, "read-and-eval!", 0, 1, 0, 
            (SCM port),
	    "Read a form from @var{port} (standard input by default), and evaluate it\n"
	    "(memoizing it in the process) in the top-level environment.  If no data\n"
	    "is left to be read from @var{port}, an @code{end-of-file} error is\n"
	    "signalled.")
#define FUNC_NAME s_scm_read_and_eval_x
{
  SCM form;

  scm_c_issue_deprecation_warning
    ("'read-and-eval!' is deprecated.  Use 'read' and 'eval' instead.");

  form = scm_read (port);
  if (SCM_EOF_OBJECT_P (form))
    scm_ithrow (scm_end_of_file_key, SCM_EOL, 1);
  return scm_eval_x (form, scm_current_module ());
}
#undef FUNC_NAME

SCM
scm_make_subr_opt (const char *name, int type, SCM (*fcn) (), int set)
{
  scm_c_issue_deprecation_warning 
    ("`scm_make_subr_opt' is deprecated.  Use `scm_c_make_subr' or "
     "`scm_c_define_subr' instead.");

  if (set)
    return scm_c_define_subr (name, type, fcn);
  else
    return scm_c_make_subr (name, type, fcn);
}

SCM 
scm_make_subr (const char *name, int type, SCM (*fcn) ())
{
  scm_c_issue_deprecation_warning 
    ("`scm_make_subr' is deprecated.  Use `scm_c_define_subr' instead.");

  return scm_c_define_subr (name, type, fcn);
}

SCM
scm_make_subr_with_generic (const char *name, int type, SCM (*fcn) (), SCM *gf)
{
  scm_c_issue_deprecation_warning 
    ("`scm_make_subr_with_generic' is deprecated.  Use "
     "`scm_c_define_subr_with_generic' instead.");
  
  return scm_c_define_subr_with_generic (name, type, fcn, gf);
}

/* Call thunk(closure) underneath a top-level error handler.
 * If an error occurs, pass the exitval through err_filter and return it.
 * If no error occurs, return the value of thunk.
 */

#ifdef _UNICOS
typedef int setjmp_type;
#else
typedef long setjmp_type;
#endif

struct cce_handler_data {
  SCM (*err_filter) ();
  void *closure;
};

static SCM
invoke_err_filter (void *d, SCM tag, SCM args)
{
  struct cce_handler_data *data = (struct cce_handler_data *)d;
  return data->err_filter (SCM_BOOL_F, data->closure);
}

SCM
scm_call_catching_errors (SCM (*thunk)(), SCM (*err_filter)(), void *closure)
{
  scm_c_issue_deprecation_warning
    ("'scm_call_catching_errors' is deprecated.  "
     "Use 'scm_internal_catch' instead.");
  
  {
    struct cce_handler_data data;
    data.err_filter = err_filter;
    data.closure = closure;
    return scm_internal_catch (SCM_BOOL_T,
			       (scm_t_catch_body)thunk, closure,
			       (scm_t_catch_handler)invoke_err_filter, &data);
  }
}

long
scm_make_smob_type_mfpe (char *name, size_t size,
                        SCM (*mark) (SCM),
                        size_t (*free) (SCM),
                        int (*print) (SCM, SCM, scm_print_state *),
                        SCM (*equalp) (SCM, SCM))
{
  scm_c_issue_deprecation_warning
    ("'scm_make_smob_type_mfpe' is deprecated.  "
     "Use 'scm_make_smob_type' plus 'scm_set_smob_*' instead.");

  {
    long answer = scm_make_smob_type (name, size);
    scm_set_smob_mfpe (answer, mark, free, print, equalp);
    return answer;
  }
}

void
scm_set_smob_mfpe (long tc, 
		   SCM (*mark) (SCM),
		   size_t (*free) (SCM),
		   int (*print) (SCM, SCM, scm_print_state *),
		   SCM (*equalp) (SCM, SCM))
{
  scm_c_issue_deprecation_warning
    ("'scm_set_smob_mfpe' is deprecated.  "
     "Use 'scm_set_smob_mark' instead, for example.");

  if (mark) scm_set_smob_mark (tc, mark);
  if (free) scm_set_smob_free (tc, free);
  if (print) scm_set_smob_print (tc, print);
  if (equalp) scm_set_smob_equalp (tc, equalp);
}

SCM
scm_read_0str (char *expr)
{
  scm_c_issue_deprecation_warning 
    ("scm_read_0str is deprecated.  Use scm_c_read_string instead.");

  return scm_c_read_string (expr);
}

SCM
scm_eval_0str (const char *expr)
{
  scm_c_issue_deprecation_warning 
    ("scm_eval_0str is deprecated.  Use scm_c_eval_string instead.");

  return scm_c_eval_string (expr);
}

SCM
scm_strprint_obj (SCM obj)
{
  scm_c_issue_deprecation_warning 
    ("scm_strprint_obj is deprecated.  Use scm_object_to_string instead.");
  return scm_object_to_string (obj, SCM_UNDEFINED);
}

char *
scm_i_object_chars (SCM obj)
{
  scm_c_issue_deprecation_warning 
    ("SCM_CHARS is deprecated.  Use SCM_STRING_CHARS or "
     "SCM_SYMBOL_CHARS instead.");
  if (SCM_STRINGP (obj))
    return SCM_STRING_CHARS (obj);
  if (SCM_SYMBOLP (obj))
    return SCM_SYMBOL_CHARS (obj);
  abort ();
}

long
scm_i_object_length (SCM obj)
{
  scm_c_issue_deprecation_warning 
    ("SCM_LENGTH is deprecated.  Use SCM_STRING_LENGTH instead, for example.");
  if (SCM_STRINGP (obj))
    return SCM_STRING_LENGTH (obj);
  if (SCM_SYMBOLP (obj))
    return SCM_SYMBOL_LENGTH (obj);
  if (SCM_VECTORP (obj))
    return SCM_VECTOR_LENGTH (obj);
  abort ();
}

void
scm_i_init_deprecated ()
{
#include "libguile/deprecated.x"
}

#endif
