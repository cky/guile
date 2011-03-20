/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

/* Copyright (C) 2003, 2004, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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
#include "libguile/async.h"
#include "libguile/arrays.h"
#include "libguile/array-map.h"
#include "libguile/generalized-arrays.h"
#include "libguile/bytevectors.h"
#include "libguile/bitvectors.h"
#include "libguile/deprecated.h"
#include "libguile/deprecation.h"
#include "libguile/snarf.h"
#include "libguile/validate.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
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
#include "libguile/r6rs-ports.h"
#include "libguile/strports.h"
#include "libguile/smob.h"
#include "libguile/alist.h"
#include "libguile/keywords.h"
#include "libguile/socket.h"
#include "libguile/feature.h"
#include "libguile/uniform.h"

#include <math.h>
#include <stdio.h>
#include <string.h>

#include <arpa/inet.h>

#if (SCM_ENABLE_DEPRECATED == 1)

/* From print.c: Internal symbol names of isyms.  Deprecated in guile 1.7.0 on
 * 2004-04-22.  */
char *scm_isymnames[] =
{
  "#@<deprecated>"
};


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
    res = scm_cons (scm_cons (scm_from_locale_string (md->module_name),
			      scm_from_ulong ((unsigned long) md->init_func)),
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

  SCM_CRITICAL_SECTION_START;

  for (md1 = registered_mods; md1; md1 = md2)
    {
      md2 = md1->link;
      free (md1);
    }
  registered_mods = NULL;

  SCM_CRITICAL_SECTION_END;
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
  if (module_prefix == SCM_BOOL_F)
    {
      module_prefix = scm_list_2 (scm_sym_app, scm_sym_modules);
      make_modules_in_var = scm_c_lookup ("make-modules-in");
      beautify_user_module_x_var =
	scm_c_lookup ("beautify-user-module!");
      try_module_autoload_var = scm_c_lookup ("try-module-autoload");
    }
}

static SCM
scm_module_full_name (SCM name)
{
  init_module_stuff ();
  if (scm_is_eq (SCM_CAR (name), scm_sym_app))
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
  SCM except_set = (SCM) data;

  while (!scm_is_null (except_set))
    {
      SCM p = SCM_COERCE_OUTPORT (SCM_CAR (except_set));
      if (scm_is_eq (p, port))
	return;
      except_set = SCM_CDR (except_set);
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
  
  for (p = ports; !scm_is_null (p); p = SCM_CDR (p))
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
    ("'scm_makstr' is deprecated.  Use 'scm_c_make_string' instead.");
  return scm_c_make_string (len, SCM_UNDEFINED);
}

SCM 
scm_makfromstr (const char *src, size_t len, int dummy SCM_UNUSED)
{
  scm_c_issue_deprecation_warning ("`scm_makfromstr' is deprecated. "
				   "Use `scm_from_locale_stringn' instead.");

  return scm_from_locale_stringn (src, len);
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
    SCM hook = scm_make_hook (scm_from_int (n_args));
    scm_c_define (name, hook);
    return hook;
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

  for(;  scm_is_pair (lst);  lst = SCM_CDR(lst))
    {
      if (scm_is_eq (SCM_CAR (lst), x))
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

  for(;  scm_is_pair (lst);  lst = SCM_CDR(lst))
    {
      if (! scm_is_false (scm_eqv_p (SCM_CAR (lst), x)))
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

  for(;  scm_is_pair (lst);  lst = SCM_CDR(lst))
    {
      if (! scm_is_false (scm_equal_p (SCM_CAR (lst), x)))
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

size_t
scm_smob_free (SCM obj)
{
  long n = SCM_SMOBNUM (obj);

  scm_c_issue_deprecation_warning
    ("`scm_smob_free' is deprecated.  "
     "It is no longer needed.");

  if (scm_smobs[n].size > 0)
    scm_gc_free ((void *) SCM_SMOB_DATA_1 (obj), 
		 scm_smobs[n].size, SCM_SMOBNAME (n));
  return 0;
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
    ("SCM_CHARS is deprecated.  See the manual for alternatives.");
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
    ("SCM_LENGTH is deprecated.  "
     "Use scm_c_string_length instead, for example, or see the manual.");
  if (SCM_STRINGP (obj))
    return SCM_STRING_LENGTH (obj);
  if (SCM_SYMBOLP (obj))
    return SCM_SYMBOL_LENGTH (obj);
  if (SCM_VECTORP (obj))
    return SCM_VECTOR_LENGTH (obj);
  abort ();
}

SCM 
scm_sym2ovcell_soft (SCM sym, SCM obarray)
{
  SCM lsym, z;
  size_t hash = scm_i_symbol_hash (sym) % SCM_VECTOR_LENGTH (obarray);

  scm_c_issue_deprecation_warning ("`scm_sym2ovcell_soft' is deprecated. "
				   "Use hashtables instead.");

  SCM_CRITICAL_SECTION_START;
  for (lsym = SCM_VECTOR_REF (obarray, hash);
       SCM_NIMP (lsym);
       lsym = SCM_CDR (lsym))
    {
      z = SCM_CAR (lsym);
      if (scm_is_eq (SCM_CAR (z), sym))
	{
	  SCM_CRITICAL_SECTION_END;
	  return z;
	}
    }
  SCM_CRITICAL_SECTION_END;
  return SCM_BOOL_F;
}


SCM 
scm_sym2ovcell (SCM sym, SCM obarray)
#define FUNC_NAME "scm_sym2ovcell"
{
  SCM answer;

  scm_c_issue_deprecation_warning ("`scm_sym2ovcell' is deprecated. "
				   "Use hashtables instead.");

  answer = scm_sym2ovcell_soft (sym, obarray);
  if (scm_is_true (answer))
    return answer;
  SCM_MISC_ERROR ("uninterned symbol: ~S", scm_list_1 (sym));
  return SCM_UNSPECIFIED;		/* not reached */
}
#undef FUNC_NAME


/* Intern a symbol whose name is the LEN characters at NAME in OBARRAY.

   OBARRAY should be a vector of lists, indexed by the name's hash
   value, modulo OBARRAY's length.  Each list has the form 
   ((SYMBOL . VALUE) ...), where SYMBOL is a symbol, and VALUE is the
   value associated with that symbol (in the current module?  in the
   system module?)

   To "intern" a symbol means: if OBARRAY already contains a symbol by
   that name, return its (SYMBOL . VALUE) pair; otherwise, create a
   new symbol, add the pair (SYMBOL . SCM_UNDEFINED) to the
   appropriate list of the OBARRAY, and return the pair.

   If softness is non-zero, don't create a symbol if it isn't already
   in OBARRAY; instead, just return #f.

   If OBARRAY is SCM_BOOL_F, create a symbol listed in no obarray and
   return (SYMBOL . SCM_UNDEFINED).  */


static SCM 
intern_obarray_soft (SCM symbol, SCM obarray, unsigned int softness)
{
  size_t raw_hash = scm_i_symbol_hash (symbol);
  size_t hash;
  SCM lsym;

  if (scm_is_false (obarray))
    {
      if (softness)
	return SCM_BOOL_F;
      else
	return scm_cons (symbol, SCM_UNDEFINED);
    }

  hash = raw_hash % SCM_VECTOR_LENGTH (obarray);

  for (lsym = SCM_VECTOR_REF(obarray, hash);
       SCM_NIMP (lsym); lsym = SCM_CDR (lsym))
    {
      SCM a = SCM_CAR (lsym);
      SCM z = SCM_CAR (a);
      if (scm_is_eq (z, symbol))
	return a;
    }
  
  if (softness)
    {
      return SCM_BOOL_F;
    }
  else
    {
      SCM cell = scm_cons (symbol, SCM_UNDEFINED);
      SCM slot = SCM_VECTOR_REF (obarray, hash);

      SCM_VECTOR_SET (obarray, hash, scm_cons (cell, slot));

      return cell;
    }
}


SCM 
scm_intern_obarray_soft (const char *name, size_t len, SCM obarray,
                         unsigned int softness)
{
  SCM symbol = scm_from_locale_symboln (name, len);

  scm_c_issue_deprecation_warning ("`scm_intern_obarray_soft' is deprecated. "
				   "Use hashtables instead.");

  return intern_obarray_soft (symbol, obarray, softness);
}
  
SCM
scm_intern_obarray (const char *name,size_t len,SCM obarray)
{
  scm_c_issue_deprecation_warning ("`scm_intern_obarray' is deprecated. "
				   "Use hashtables instead.");

  return scm_intern_obarray_soft (name, len, obarray, 0);
}

/* Lookup the value of the symbol named by the nul-terminated string
   NAME in the current module.  */
SCM
scm_symbol_value0 (const char *name)
{
  scm_c_issue_deprecation_warning ("`scm_symbol_value0' is deprecated. "
				   "Use `scm_lookup' instead.");

  return scm_variable_ref (scm_c_lookup (name));
}

SCM_DEFINE (scm_string_to_obarray_symbol, "string->obarray-symbol", 2, 1, 0,
           (SCM o, SCM s, SCM softp),
	    "Intern a new symbol in @var{obarray}, a symbol table, with name\n"
	    "@var{string}.\n\n"
	    "If @var{obarray} is @code{#f}, use the default system symbol table.  If\n"
	    "@var{obarray} is @code{#t}, the symbol should not be interned in any\n"
	    "symbol table; merely return the pair (@var{symbol}\n"
	    ". @var{#<undefined>}).\n\n"
	    "The @var{soft?} argument determines whether new symbol table entries\n"
	    "should be created when the specified symbol is not already present in\n"
	    "@var{obarray}.  If @var{soft?} is specified and is a true value, then\n"
	    "new entries should not be added for symbols not already present in the\n"
	    "table; instead, simply return @code{#f}.")
#define FUNC_NAME s_scm_string_to_obarray_symbol
{
  SCM vcell;
  SCM answer;
  int softness;

  SCM_VALIDATE_STRING (2, s);
  SCM_ASSERT (scm_is_bool (o) || SCM_VECTORP (o), o, SCM_ARG1, FUNC_NAME);

  scm_c_issue_deprecation_warning ("`string->obarray-symbol' is deprecated. "
				   "Use hashtables instead.");

  softness = (!SCM_UNBNDP (softp) && scm_is_true(softp));
  /* iron out some screwy calling conventions */
  if (scm_is_false (o))
    {
      /* nothing interesting to do here. */
      return scm_string_to_symbol (s);
    }
  else if (scm_is_eq (o, SCM_BOOL_T))
    o = SCM_BOOL_F;
    
  vcell = intern_obarray_soft (scm_string_to_symbol (s), o, softness);
  if (scm_is_false (vcell))
    return vcell;
  answer = SCM_CAR (vcell);
  return answer;
}
#undef FUNC_NAME

SCM_DEFINE (scm_intern_symbol, "intern-symbol", 2, 0, 0,
           (SCM o, SCM s),
	    "Add a new symbol to @var{obarray} with name @var{string}, bound to an\n"
	    "unspecified initial value.  The symbol table is not modified if a symbol\n"
	    "with this name is already present.")
#define FUNC_NAME s_scm_intern_symbol
{
  size_t hval;
  SCM_VALIDATE_SYMBOL (2,s);
  if (scm_is_false (o))
    return SCM_UNSPECIFIED;

  scm_c_issue_deprecation_warning ("`intern-symbol' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_VECTOR (1,o);
  hval = scm_i_symbol_hash (s) % SCM_VECTOR_LENGTH (o);
  /* If the symbol is already interned, simply return. */
  SCM_CRITICAL_SECTION_START;
  {
    SCM lsym;
    SCM sym;
    for (lsym = SCM_VECTOR_REF (o, hval);
	 SCM_NIMP (lsym);
	 lsym = SCM_CDR (lsym))
      {
	sym = SCM_CAR (lsym);
	if (scm_is_eq (SCM_CAR (sym), s))
	  {
	    SCM_CRITICAL_SECTION_END;
	    return SCM_UNSPECIFIED;
	  }
      }
    SCM_VECTOR_SET (o, hval, 
		    scm_acons (s, SCM_UNDEFINED,
			       SCM_VECTOR_REF (o, hval)));
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unintern_symbol, "unintern-symbol", 2, 0, 0,
           (SCM o, SCM s),
	    "Remove the symbol with name @var{string} from @var{obarray}.  This\n"
	    "function returns @code{#t} if the symbol was present and @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_unintern_symbol
{
  size_t hval;

  scm_c_issue_deprecation_warning ("`unintern-symbol' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (scm_is_false (o))
    return SCM_BOOL_F;
  SCM_VALIDATE_VECTOR (1,o);
  hval = scm_i_symbol_hash (s) % SCM_VECTOR_LENGTH (o);
  SCM_CRITICAL_SECTION_START;
  {
    SCM lsym_follow;
    SCM lsym;
    SCM sym;
    for (lsym = SCM_VECTOR_REF (o, hval), lsym_follow = SCM_BOOL_F;
	 SCM_NIMP (lsym);
	 lsym_follow = lsym, lsym = SCM_CDR (lsym))
      {
	sym = SCM_CAR (lsym);
	if (scm_is_eq (SCM_CAR (sym), s))
	  {
	    /* Found the symbol to unintern. */
	    if (scm_is_false (lsym_follow))
	      SCM_VECTOR_SET (o, hval, lsym);
	    else
	      SCM_SETCDR (lsym_follow, SCM_CDR(lsym));
	    SCM_CRITICAL_SECTION_END;
	    return SCM_BOOL_T;
	  }
      }
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_binding, "symbol-binding", 2, 0, 0,
           (SCM o, SCM s),
	    "Look up in @var{obarray} the symbol whose name is @var{string}, and\n"
	    "return the value to which it is bound.  If @var{obarray} is @code{#f},\n"
	    "use the global symbol table.  If @var{string} is not interned in\n"
	    "@var{obarray}, an error is signalled.")
#define FUNC_NAME s_scm_symbol_binding
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-binding' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (scm_is_false (o))
    return scm_variable_ref (scm_lookup (s));
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell (s, o);
  return SCM_CDR(vcell);
}
#undef FUNC_NAME

#if 0
SCM_DEFINE (scm_symbol_interned_p, "symbol-interned?", 2, 0, 0,
	    (SCM o, SCM s),
	    "Return @code{#t} if @var{obarray} contains a symbol with name\n"
	    "@var{string}, and @code{#f} otherwise.")
#define FUNC_NAME s_scm_symbol_interned_p
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-interned?' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (scm_is_false (o))
    {
      SCM var = scm_sym2var (s, SCM_BOOL_F, SCM_BOOL_F);
      if (var != SCM_BOOL_F)
	return SCM_BOOL_T;
      return SCM_BOOL_F;
    }
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell_soft (s, o);
  return (SCM_NIMP(vcell)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}
#undef FUNC_NAME
#endif

SCM_DEFINE (scm_symbol_bound_p, "symbol-bound?", 2, 0, 0,
	    (SCM o, SCM s),
	    "Return @code{#t} if @var{obarray} contains a symbol with name\n"
	    "@var{string} bound to a defined value.  This differs from\n"
	    "@var{symbol-interned?} in that the mere mention of a symbol\n"
	    "usually causes it to be interned; @code{symbol-bound?}\n"
	    "determines whether a symbol has been given any meaningful\n"
	    "value.")
#define FUNC_NAME s_scm_symbol_bound_p
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-bound?' is deprecated. "
				   "Use hashtables instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (scm_is_false (o))
    {
      SCM var = scm_sym2var (s, SCM_BOOL_F, SCM_BOOL_F);
      if (SCM_VARIABLEP(var) && !SCM_UNBNDP(SCM_VARIABLE_REF(var)))
	return SCM_BOOL_T;
      return SCM_BOOL_F;
    }
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell_soft (s, o);
  return scm_from_bool (SCM_NIMP (vcell) && !SCM_UNBNDP (SCM_CDR (vcell)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_set_x, "symbol-set!", 3, 0, 0,
           (SCM o, SCM s, SCM v),
	    "Find the symbol in @var{obarray} whose name is @var{string}, and rebind\n"
	    "it to @var{value}.  An error is signalled if @var{string} is not present\n"
	    "in @var{obarray}.")
#define FUNC_NAME s_scm_symbol_set_x
{
  SCM vcell;

  scm_c_issue_deprecation_warning ("`symbol-set!' is deprecated. "
				   "Use the module system instead.");

  SCM_VALIDATE_SYMBOL (2,s);
  if (scm_is_false (o))
    {
      scm_define (s, v);
      return SCM_UNSPECIFIED;
    }
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell (s, o);
  SCM_SETCDR (vcell, v);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define MAX_PREFIX_LENGTH 30

static int gentemp_counter;

SCM_DEFINE (scm_gentemp, "gentemp", 0, 2, 0,
            (SCM prefix, SCM obarray),
	    "Create a new symbol with a name unique in an obarray.\n"
	    "The name is constructed from an optional string @var{prefix}\n"
	    "and a counter value.  The default prefix is @code{t}.  The\n"
	    "@var{obarray} is specified as a second optional argument.\n"
	    "Default is the system obarray where all normal symbols are\n"
	    "interned.  The counter is increased by 1 at each\n"
	    "call.  There is no provision for resetting the counter.")
#define FUNC_NAME s_scm_gentemp
{
  char buf[MAX_PREFIX_LENGTH + SCM_INTBUFLEN];
  char *name = buf;
  int n_digits;
  size_t len;

  scm_c_issue_deprecation_warning ("`gentemp' is deprecated. "
				   "Use `gensym' instead.");

  if (SCM_UNBNDP (prefix))
    {
      name[0] = 't';
      len = 1;
    }
  else
    {
      SCM_VALIDATE_STRING (1, prefix);
      len = scm_i_string_length (prefix);
      name = scm_to_locale_stringn (prefix, &len);
      name = scm_realloc (name, len + SCM_INTBUFLEN);
    }

  if (SCM_UNBNDP (obarray))
    return scm_gensym (prefix);
  else
    SCM_ASSERT ((scm_is_vector (obarray) || SCM_I_WVECTP (obarray)),
		obarray,
		SCM_ARG2,
		FUNC_NAME);
  do
    n_digits = scm_iint2str (gentemp_counter++, 10, &name[len]);
  while (scm_is_true (scm_intern_obarray_soft (name,
					       len + n_digits,
					       obarray,
					       1)));
  {
    SCM vcell = scm_intern_obarray_soft (name,
					 len + n_digits,
					 obarray,
					 0);
    if (name != buf)
      free (name);
    return SCM_CAR (vcell);
  }
}
#undef FUNC_NAME

SCM
scm_i_makinum (scm_t_signed_bits val)
{
  scm_c_issue_deprecation_warning
    ("SCM_MAKINUM is deprecated.  Use scm_from_int or similar instead.");
  return SCM_I_MAKINUM (val);
}

int
scm_i_inump (SCM obj)
{
  scm_c_issue_deprecation_warning
    ("SCM_INUMP is deprecated.  Use scm_is_integer or similar instead.");
  return SCM_I_INUMP (obj);
}

scm_t_signed_bits
scm_i_inum (SCM obj)
{
  scm_c_issue_deprecation_warning
    ("SCM_INUM is deprecated.  Use scm_to_int or similar instead.");
  return scm_to_intmax (obj);
}

char *
scm_c_string2str (SCM obj, char *str, size_t *lenp)
{
  scm_c_issue_deprecation_warning
    ("scm_c_string2str is deprecated.  Use scm_to_locale_stringbuf or similar instead.");
  
  if (str == NULL)
    {
      char *result = scm_to_locale_string (obj);
      if (lenp)
	*lenp = scm_i_string_length (obj);
      return result;
    }
  else
    {
      /* Pray that STR is large enough.
       */
      size_t len = scm_to_locale_stringbuf (obj, str, SCM_I_SIZE_MAX);
      str[len] = '\0';
      if (lenp)
	*lenp = len;
      return str;
    }
}

char *
scm_c_substring2str (SCM obj, char *str, size_t start, size_t len)
{
  scm_c_issue_deprecation_warning
    ("scm_c_substring2str is deprecated.  Use scm_substring plus scm_to_locale_stringbuf instead.");

  if (start)
    obj = scm_substring (obj, scm_from_size_t (start), SCM_UNDEFINED);

  scm_to_locale_stringbuf (obj, str, len);
  return str;
}

/* Converts the given Scheme symbol OBJ into a C string, containing a copy
   of OBJ's content with a trailing null byte.  If LENP is non-NULL, set
   *LENP to the string's length.

   When STR is non-NULL it receives the copy and is returned by the function,
   otherwise new memory is allocated and the caller is responsible for 
   freeing it via free().  If out of memory, NULL is returned.

   Note that Scheme symbols may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way to 
   determine the length of the returned value.  However, the function always 
   copies the complete contents of OBJ, and sets *LENP to the length of the
   scheme symbol (if LENP is non-null).  */
char *
scm_c_symbol2str (SCM obj, char *str, size_t *lenp)
{
  return scm_c_string2str (scm_symbol_to_string (obj), str, lenp);
}

double
scm_truncate (double x)
{
  scm_c_issue_deprecation_warning
    ("scm_truncate is deprecated.  Use scm_c_truncate instead.");
  return scm_c_truncate (x);
}

double
scm_round (double x)
{
  scm_c_issue_deprecation_warning
    ("scm_round is deprecated.  Use scm_c_round instead.");
  return scm_c_round (x);
}

SCM
scm_sys_expt (SCM x, SCM y)
{
  scm_c_issue_deprecation_warning
    ("scm_sys_expt is deprecated.  Use scm_expt instead.");
  return scm_expt (x, y);
}

double
scm_asinh (double x)
{
  scm_c_issue_deprecation_warning
    ("scm_asinh is deprecated.  Use asinh instead.");
#if HAVE_ASINH
  return asinh (x);
#else
  return log (x + sqrt (x * x + 1));
#endif
}

double
scm_acosh (double x)
{
  scm_c_issue_deprecation_warning
    ("scm_acosh is deprecated.  Use acosh instead.");
#if HAVE_ACOSH
  return acosh (x);
#else
  return log (x + sqrt (x * x - 1));
#endif
}

double
scm_atanh (double x)
{
  scm_c_issue_deprecation_warning
    ("scm_atanh is deprecated.  Use atanh instead.");
#if HAVE_ATANH
  return atanh (x);
#else
  return 0.5 * log ((1 + x) / (1 - x));
#endif
}

SCM
scm_sys_atan2 (SCM z1, SCM z2)
{
  scm_c_issue_deprecation_warning
    ("scm_sys_atan2 is deprecated.  Use scm_atan instead.");
  return scm_atan (z1, z2);
}

char *
scm_i_deprecated_symbol_chars (SCM sym)
{
  scm_c_issue_deprecation_warning
    ("SCM_SYMBOL_CHARS is deprecated.  Use scm_symbol_to_string.");

  return (char *)scm_i_symbol_chars (sym);
}

size_t
scm_i_deprecated_symbol_length (SCM sym)
{
  scm_c_issue_deprecation_warning
    ("SCM_SYMBOL_LENGTH is deprecated.  Use scm_symbol_to_string.");
  return scm_i_symbol_length (sym);
}

int
scm_i_keywordp (SCM obj)
{
  scm_c_issue_deprecation_warning
    ("SCM_KEYWORDP is deprecated.  Use scm_is_keyword instead.");
  return scm_is_keyword (obj);
}

SCM
scm_i_keywordsym (SCM keyword)
{
  scm_c_issue_deprecation_warning
    ("SCM_KEYWORDSYM is deprecated.  See scm_keyword_to_symbol instead.");
  return scm_keyword_dash_symbol (keyword);
}

int
scm_i_vectorp (SCM x)
{
  scm_c_issue_deprecation_warning
    ("SCM_VECTORP is deprecated.  Use scm_is_vector instead.");
  return SCM_I_IS_VECTOR (x);
}

unsigned long
scm_i_vector_length (SCM x)
{
  scm_c_issue_deprecation_warning
    ("SCM_VECTOR_LENGTH is deprecated.  Use scm_c_vector_length instead.");
  return SCM_I_VECTOR_LENGTH (x);
}

const SCM *
scm_i_velts (SCM x)
{
  scm_c_issue_deprecation_warning
    ("SCM_VELTS is deprecated.  Use scm_vector_elements instead.");
  return SCM_I_VECTOR_ELTS (x);
}

SCM *
scm_i_writable_velts (SCM x)
{
  scm_c_issue_deprecation_warning
    ("SCM_WRITABLE_VELTS is deprecated.  "
     "Use scm_vector_writable_elements instead.");
  return SCM_I_VECTOR_WELTS (x);
}

SCM
scm_i_vector_ref (SCM x, size_t idx)
{
  scm_c_issue_deprecation_warning
    ("SCM_VECTOR_REF is deprecated.  "
     "Use scm_c_vector_ref or scm_vector_elements instead.");
  return scm_c_vector_ref (x, idx);
}

void
scm_i_vector_set (SCM x, size_t idx, SCM val)
{
  scm_c_issue_deprecation_warning
    ("SCM_VECTOR_SET is deprecated.  "
     "Use scm_c_vector_set_x or scm_vector_writable_elements instead.");
  scm_c_vector_set_x (x, idx, val);
}

SCM
scm_vector_equal_p (SCM x, SCM y)
{
  scm_c_issue_deprecation_warning
    ("scm_vector_euqal_p is deprecated.  "
     "Use scm_equal_p instead.");
  return scm_equal_p (x, y);
}

SCM_DEFINE (scm_uniform_vector_read_x, "uniform-vector-read!", 1, 3, 0,
           (SCM uvec, SCM port_or_fd, SCM start, SCM end),
	    "Fill the elements of @var{uvec} by reading\n"
	    "raw bytes from @var{port-or-fdes}, using host byte order.\n\n"
	    "The optional arguments @var{start} (inclusive) and @var{end}\n"
	    "(exclusive) allow a specified region to be read,\n"
	    "leaving the remainder of the vector unchanged.\n\n"
	    "When @var{port-or-fdes} is a port, all specified elements\n"
	    "of @var{uvec} are attempted to be read, potentially blocking\n"
	    "while waiting for more input or end-of-file.\n"
	    "When @var{port-or-fd} is an integer, a single call to\n"
	    "read(2) is made.\n\n"
	    "An error is signalled when the last element has only\n"
	    "been partially filled before reaching end-of-file or in\n"
	    "the single call to read(2).\n\n"
	    "@code{uniform-vector-read!} returns the number of elements\n"
	    "read.\n\n"
	    "@var{port-or-fdes} may be omitted, in which case it defaults\n"
	    "to the value returned by @code{(current-input-port)}.")
#define FUNC_NAME s_scm_uniform_vector_read_x
{
  SCM result;
  size_t c_width, c_start, c_end;

  SCM_VALIDATE_BYTEVECTOR (SCM_ARG1, uvec);

  scm_c_issue_deprecation_warning
    ("`uniform-vector-read!' is deprecated. Use `get-bytevector-n!' from\n"
     "`(rnrs io ports)' instead.");

  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_input_port ();

  c_width = scm_to_size_t (scm_uniform_vector_element_size (uvec));

  c_start = SCM_UNBNDP (start) ? 0 : scm_to_size_t (start);
  c_start *= c_width;

  c_end = SCM_UNBNDP (end) ? SCM_BYTEVECTOR_LENGTH (uvec) : scm_to_size_t (end);
  c_end *= c_width;

  result = scm_get_bytevector_n_x (port_or_fd, uvec,
				   scm_from_size_t (c_start),
				   scm_from_size_t (c_end - c_start));

  if (SCM_EOF_OBJECT_P (result))
    result = SCM_INUM0;

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_write, "uniform-vector-write", 1, 3, 0,
           (SCM uvec, SCM port_or_fd, SCM start, SCM end),
	    "Write the elements of @var{uvec} as raw bytes to\n"
	    "@var{port-or-fdes}, in the host byte order.\n\n"
	    "The optional arguments @var{start} (inclusive)\n"
	    "and @var{end} (exclusive) allow\n"
	    "a specified region to be written.\n\n"
	    "When @var{port-or-fdes} is a port, all specified elements\n"
	    "of @var{uvec} are attempted to be written, potentially blocking\n"
	    "while waiting for more room.\n"
	    "When @var{port-or-fd} is an integer, a single call to\n"
	    "write(2) is made.\n\n"
	    "An error is signalled when the last element has only\n"
	    "been partially written in the single call to write(2).\n\n"
	    "The number of objects actually written is returned.\n"
	    "@var{port-or-fdes} may be\n"
	    "omitted, in which case it defaults to the value returned by\n"
	    "@code{(current-output-port)}.")
#define FUNC_NAME s_scm_uniform_vector_write
{
  size_t c_width, c_start, c_end;

  SCM_VALIDATE_BYTEVECTOR (SCM_ARG1, uvec);

  scm_c_issue_deprecation_warning
    ("`uniform-vector-write' is deprecated. Use `put-bytevector' from\n"
     "`(rnrs io ports)' instead.");

  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_output_port ();

  port_or_fd = SCM_COERCE_OUTPORT (port_or_fd);

  c_width = scm_to_size_t (scm_uniform_vector_element_size (uvec));

  c_start = SCM_UNBNDP (start) ? 0 : scm_to_size_t (start);
  c_start *= c_width;

  c_end = SCM_UNBNDP (end) ? SCM_BYTEVECTOR_LENGTH (uvec) : scm_to_size_t (end);
  c_end *= c_width;

  return scm_put_bytevector (port_or_fd, uvec,
                             scm_from_size_t (c_start),
                             scm_from_size_t (c_end - c_start));
}
#undef FUNC_NAME

static SCM 
scm_ra2contig (SCM ra, int copy)
{
  SCM ret;
  long inc = 1;
  size_t k, len = 1;
  for (k = SCM_I_ARRAY_NDIM (ra); k--;)
    len *= SCM_I_ARRAY_DIMS (ra)[k].ubnd - SCM_I_ARRAY_DIMS (ra)[k].lbnd + 1;
  k = SCM_I_ARRAY_NDIM (ra);
  if (SCM_I_ARRAY_CONTP (ra) && ((0 == k) || (1 == SCM_I_ARRAY_DIMS (ra)[k - 1].inc)))
    {
      if (!scm_is_bitvector (SCM_I_ARRAY_V (ra)))
	return ra;
      if ((len == scm_c_bitvector_length (SCM_I_ARRAY_V (ra)) &&
	   0 == SCM_I_ARRAY_BASE (ra) % SCM_LONG_BIT &&
	   0 == len % SCM_LONG_BIT))
	return ra;
    }
  ret = scm_i_make_array (k);
  SCM_I_ARRAY_BASE (ret) = 0;
  while (k--)
    {
      SCM_I_ARRAY_DIMS (ret)[k].lbnd = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
      SCM_I_ARRAY_DIMS (ret)[k].ubnd = SCM_I_ARRAY_DIMS (ra)[k].ubnd;
      SCM_I_ARRAY_DIMS (ret)[k].inc = inc;
      inc *= SCM_I_ARRAY_DIMS (ra)[k].ubnd - SCM_I_ARRAY_DIMS (ra)[k].lbnd + 1;
    }
  SCM_I_ARRAY_V (ret) =
    scm_make_generalized_vector (scm_array_type (ra), scm_from_size_t (inc),
                                 SCM_UNDEFINED);
  if (copy)
    scm_array_copy_x (ra, ret);
  return ret;
}

SCM_DEFINE (scm_uniform_array_read_x, "uniform-array-read!", 1, 3, 0,
           (SCM ura, SCM port_or_fd, SCM start, SCM end),
	    "@deffnx {Scheme Procedure} uniform-vector-read! uve [port-or-fdes] [start] [end]\n"
	    "Attempt to read all elements of @var{ura}, in lexicographic order, as\n"
	    "binary objects from @var{port-or-fdes}.\n"
	    "If an end of file is encountered,\n"
	    "the objects up to that point are put into @var{ura}\n"
	    "(starting at the beginning) and the remainder of the array is\n"
	    "unchanged.\n\n"
	    "The optional arguments @var{start} and @var{end} allow\n"
	    "a specified region of a vector (or linearized array) to be read,\n"
	    "leaving the remainder of the vector unchanged.\n\n"
	    "@code{uniform-array-read!} returns the number of objects read.\n"
	    "@var{port-or-fdes} may be omitted, in which case it defaults to the value\n"
	    "returned by @code{(current-input-port)}.")
#define FUNC_NAME s_scm_uniform_array_read_x
{
  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_input_port ();

  if (scm_is_uniform_vector (ura))
    {
      return scm_uniform_vector_read_x (ura, port_or_fd, start, end);
    }
  else if (SCM_I_ARRAYP (ura))
    {
      size_t base, vlen, cstart, cend;
      SCM cra, ans;
      
      cra = scm_ra2contig (ura, 0);
      base = SCM_I_ARRAY_BASE (cra);
      vlen = SCM_I_ARRAY_DIMS (cra)->inc *
	(SCM_I_ARRAY_DIMS (cra)->ubnd - SCM_I_ARRAY_DIMS (cra)->lbnd + 1);

      cstart = 0;
      cend = vlen;
      if (!SCM_UNBNDP (start))
	{
	  cstart = scm_to_unsigned_integer (start, 0, vlen);
	  if (!SCM_UNBNDP (end))
	    cend = scm_to_unsigned_integer (end, cstart, vlen);
	}

      ans = scm_uniform_vector_read_x (SCM_I_ARRAY_V (cra), port_or_fd,
				       scm_from_size_t (base + cstart),
				       scm_from_size_t (base + cend));

      if (!scm_is_eq (cra, ura))
	scm_array_copy_x (cra, ura);
      return ans;
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, ura, "array");
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_array_write, "uniform-array-write", 1, 3, 0,
           (SCM ura, SCM port_or_fd, SCM start, SCM end),
	    "Writes all elements of @var{ura} as binary objects to\n"
	    "@var{port-or-fdes}.\n\n"
	    "The optional arguments @var{start}\n"
	    "and @var{end} allow\n"
	    "a specified region of a vector (or linearized array) to be written.\n\n"
	    "The number of objects actually written is returned.\n"
	    "@var{port-or-fdes} may be\n"
	    "omitted, in which case it defaults to the value returned by\n"
	    "@code{(current-output-port)}.")
#define FUNC_NAME s_scm_uniform_array_write
{
  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_output_port ();

  if (scm_is_uniform_vector (ura))
    {
      return scm_uniform_vector_write (ura, port_or_fd, start, end);
    }
  else if (SCM_I_ARRAYP (ura))
    {
      size_t base, vlen, cstart, cend;
      SCM cra, ans;
      
      cra = scm_ra2contig (ura, 1);
      base = SCM_I_ARRAY_BASE (cra);
      vlen = SCM_I_ARRAY_DIMS (cra)->inc *
	(SCM_I_ARRAY_DIMS (cra)->ubnd - SCM_I_ARRAY_DIMS (cra)->lbnd + 1);

      cstart = 0;
      cend = vlen;
      if (!SCM_UNBNDP (start))
	{
	  cstart = scm_to_unsigned_integer (start, 0, vlen);
	  if (!SCM_UNBNDP (end))
	    cend = scm_to_unsigned_integer (end, cstart, vlen);
	}

      ans = scm_uniform_vector_write (SCM_I_ARRAY_V (cra), port_or_fd,
				      scm_from_size_t (base + cstart),
				      scm_from_size_t (base + cend));

      return ans;
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, ura, "array");
}
#undef FUNC_NAME

SCM
scm_i_cur_inp (void)
{
  scm_c_issue_deprecation_warning
    ("scm_cur_inp is deprecated.  Use scm_current_input_port instead.");
  return scm_current_input_port ();
}

SCM
scm_i_cur_outp (void)
{
  scm_c_issue_deprecation_warning
    ("scm_cur_outp is deprecated.  Use scm_current_output_port instead.");
  return scm_current_output_port ();
}

SCM
scm_i_cur_errp (void)
{
  scm_c_issue_deprecation_warning
    ("scm_cur_errp is deprecated.  Use scm_current_error_port instead.");
  return scm_current_error_port ();
}

SCM
scm_i_cur_loadp (void)
{
  scm_c_issue_deprecation_warning
    ("scm_cur_loadp is deprecated.  Use scm_current_load_port instead.");
  return scm_current_load_port ();
}

SCM
scm_i_progargs (void)
{
  scm_c_issue_deprecation_warning
    ("scm_progargs is deprecated.  Use scm_program_arguments instead.");
  return scm_program_arguments ();
}

SCM
scm_i_deprecated_dynwinds (void)
{
  scm_c_issue_deprecation_warning
    ("scm_dynwinds is deprecated.  Do not use it.");
  return scm_i_dynwinds ();
}

SCM_STACKITEM *
scm_i_stack_base (void)
{
  scm_c_issue_deprecation_warning
    ("scm_stack_base is deprecated.  Do not use it.");
  return SCM_I_CURRENT_THREAD->base;
}

int
scm_i_fluidp (SCM x)
{
  scm_c_issue_deprecation_warning
    ("SCM_FLUIDP is deprecated.  Use scm_is_fluid instead.");
  return scm_is_fluid (x);
}


/* Networking.  */

#ifdef HAVE_NETWORKING

SCM_DEFINE (scm_inet_aton, "inet-aton", 1, 0, 0,
            (SCM address),
	    "Convert an IPv4 Internet address from printable string\n"
	    "(dotted decimal notation) to an integer.  E.g.,\n\n"
	    "@lisp\n"
	    "(inet-aton \"127.0.0.1\") @result{} 2130706433\n"
	    "@end lisp")
#define FUNC_NAME s_scm_inet_aton
{
  scm_c_issue_deprecation_warning
    ("`inet-aton' is deprecated.  Use `inet-pton' instead.");

  return scm_inet_pton (scm_from_int (AF_INET), address);
}
#undef FUNC_NAME


SCM_DEFINE (scm_inet_ntoa, "inet-ntoa", 1, 0, 0,
            (SCM inetid),
	    "Convert an IPv4 Internet address to a printable\n"
	    "(dotted decimal notation) string.  E.g.,\n\n"
	    "@lisp\n"
	    "(inet-ntoa 2130706433) @result{} \"127.0.0.1\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_inet_ntoa
{
  scm_c_issue_deprecation_warning
    ("`inet-ntoa' is deprecated.  Use `inet-ntop' instead.");

  return scm_inet_ntop (scm_from_int (AF_INET), inetid);
}
#undef FUNC_NAME

#endif /* HAVE_NETWORKING */


void
scm_i_defer_ints_etc ()
{
  scm_c_issue_deprecation_warning
    ("SCM_DEFER_INTS etc are deprecated.  "
     "Use a mutex instead if appropriate.");
}

int
scm_i_mask_ints (void)
{
  scm_c_issue_deprecation_warning ("`scm_mask_ints' is deprecated.");
  return (SCM_I_CURRENT_THREAD->block_asyncs != 0);
}


SCM
scm_guard (SCM guardian, SCM obj, int throw_p)
{
  scm_c_issue_deprecation_warning
    ("scm_guard is deprecated.  Use scm_call_1 instead.");

  return scm_call_1 (guardian, obj);
}

SCM
scm_get_one_zombie (SCM guardian)
{
  scm_c_issue_deprecation_warning
    ("scm_guard is deprecated.  Use scm_call_0 instead.");

  return scm_call_0 (guardian);
}

SCM_DEFINE (scm_guardian_destroyed_p, "guardian-destroyed?", 1, 0, 0, 
            (SCM guardian),
            "Return @code{#t} if @var{guardian} has been destroyed, otherwise @code{#f}.")
#define FUNC_NAME s_scm_guardian_destroyed_p       
{
  scm_c_issue_deprecation_warning
    ("'guardian-destroyed?' is deprecated.");
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_guardian_greedy_p, "guardian-greedy?", 1, 0, 0,
            (SCM guardian),
            "Return @code{#t} if @var{guardian} is a greedy guardian, otherwise @code{#f}.")
#define FUNC_NAME s_scm_guardian_greedy_p  
{
  scm_c_issue_deprecation_warning
    ("'guardian-greedy?' is deprecated.");
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_destroy_guardian_x, "destroy-guardian!", 1, 0, 0, 
            (SCM guardian),
            "Destroys @var{guardian}, by making it impossible to put any more\n"
            "objects in it or get any objects from it.  It also unguards any\n"
            "objects guarded by @var{guardian}.")
#define FUNC_NAME s_scm_destroy_guardian_x
{
  scm_c_issue_deprecation_warning
    ("'destroy-guardian!' is deprecated and ineffective.");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* GC-related things.  */

unsigned long scm_mallocated, scm_mtrigger;
size_t scm_max_segment_size;

#if defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST)
SCM
scm_map_free_list (void)
{
  return SCM_EOL;
}
#endif

#if defined (GUILE_DEBUG_FREELIST)
SCM
scm_gc_set_debug_check_freelist_x (SCM flag)
{
  return SCM_UNSPECIFIED;
}
#endif


/* Trampolines
 *  
 * Trampolines were an intent to speed up calling the same Scheme procedure many
 * times from C.
 *
 * However, this was the wrong thing to optimize; if you really know what you're
 * calling, call its function directly, otherwise you're in Scheme-land, and we
 * have many better tricks there (inlining, for example, which can remove the
 * need for closures and free variables).
 *
 * Also, in the normal debugging case, trampolines were being computed but not
 * used. Silliness.
 */

scm_t_trampoline_0
scm_trampoline_0 (SCM proc)
{
  scm_c_issue_deprecation_warning
    ("`scm_trampoline_0' is deprecated. Just use `scm_call_0' instead.");
  return scm_call_0;
}

scm_t_trampoline_1
scm_trampoline_1 (SCM proc)
{
  scm_c_issue_deprecation_warning
    ("`scm_trampoline_1' is deprecated. Just use `scm_call_1' instead.");
  return scm_call_1;
}

scm_t_trampoline_2
scm_trampoline_2 (SCM proc)
{
  scm_c_issue_deprecation_warning
    ("`scm_trampoline_2' is deprecated. Just use `scm_call_2' instead.");
  return scm_call_2;
}

int
scm_i_subr_p (SCM x)
{
  scm_c_issue_deprecation_warning ("`scm_subr_p' is deprecated. Use SCM_PRIMITIVE_P instead.");
  return SCM_PRIMITIVE_P (x);
}



SCM
scm_internal_lazy_catch (SCM tag, scm_t_catch_body body, void *body_data, scm_t_catch_handler handler, void *handler_data)
{
  scm_c_issue_deprecation_warning
    ("`scm_internal_lazy_catch' is no longer supported. Instead this call will\n"
     "dispatch to `scm_c_with_throw_handler'. Your handler will be invoked from\n"
     "within the dynamic context of the corresponding `throw'.\n"
     "\nTHIS COULD CHANGE YOUR PROGRAM'S BEHAVIOR.\n\n"
     "Please modify your program to use `scm_c_with_throw_handler' directly,\n"
     "and adapt it (if necessary) to expect to be within the dynamic context\n"
     "of the throw.");
  return scm_c_with_throw_handler (tag, body, body_data, handler, handler_data, 0);
}

SCM_DEFINE (scm_lazy_catch, "lazy-catch", 3, 0, 0,
	    (SCM key, SCM thunk, SCM handler),
	    "This behaves exactly like @code{catch}, except that it does\n"
	    "not unwind the stack before invoking @var{handler}.\n"
	    "If the @var{handler} procedure returns normally, Guile\n"
	    "rethrows the same exception again to the next innermost catch,\n"
	    "lazy-catch or throw handler.  If the @var{handler} exits\n"
	    "non-locally, that exit determines the continuation.")
#define FUNC_NAME s_scm_lazy_catch
{
  struct scm_body_thunk_data c;

  SCM_ASSERT (scm_is_symbol (key) || scm_is_eq (key, SCM_BOOL_T),
	      key, SCM_ARG1, FUNC_NAME);

  c.tag = key;
  c.body_proc = thunk;

  scm_c_issue_deprecation_warning
    ("`lazy-catch' is no longer supported. Instead this call will dispatch\n"
     "to `with-throw-handler'. Your handler will be invoked from within the\n"
     "dynamic context of the corresponding `throw'.\n"
     "\nTHIS COULD CHANGE YOUR PROGRAM'S BEHAVIOR.\n\n"
     "Please modify your program to use `with-throw-handler' directly, and\n"
     "adapt it (if necessary) to expect to be within the dynamic context of\n"
     "the throw.");

  return scm_c_with_throw_handler (key,
                                   scm_body_thunk, &c, 
                                   scm_handle_by_proc, &handler, 0);
}
#undef FUNC_NAME





SCM
scm_raequal (SCM ra0, SCM ra1)
{
  return scm_array_equal_p (ra0, ra1);
}





SCM_DEFINE (scm_dynamic_args_call, "dynamic-args-call", 3, 0, 0, 
            (SCM func, SCM dobj, SCM args),
	    "Call the C function indicated by @var{func} and @var{dobj},\n"
	    "just like @code{dynamic-call}, but pass it some arguments and\n"
	    "return its return value.  The C function is expected to take\n"
	    "two arguments and return an @code{int}, just like @code{main}:\n"
	    "@smallexample\n"
	    "int c_func (int argc, char **argv);\n"
	    "@end smallexample\n\n"
	    "The parameter @var{args} must be a list of strings and is\n"
	    "converted into an array of @code{char *}.  The array is passed\n"
	    "in @var{argv} and its size in @var{argc}.  The return value is\n"
	    "converted to a Scheme number and returned from the call to\n"
	    "@code{dynamic-args-call}.")
#define FUNC_NAME s_scm_dynamic_args_call
{
  int (*fptr) (int argc, char **argv);
  int result, argc;
  char **argv;

  if (scm_is_string (func))
    func = scm_dynamic_func (func, dobj);
  SCM_VALIDATE_POINTER (SCM_ARG1, func);

  fptr = SCM_POINTER_VALUE (func);

  argv = scm_i_allocate_string_pointers (args);
  for (argc = 0; argv[argc]; argc++)
    ;
  result = (*fptr) (argc, argv);

  return scm_from_int (result);
}
#undef FUNC_NAME





int
scm_badargsp (SCM formals, SCM args)
{
  scm_c_issue_deprecation_warning
    ("`scm_badargsp' is deprecated. Copy it into your project if you need it.");

  while (!scm_is_null (formals))
    {
      if (!scm_is_pair (formals)) 
        return 0;
      if (scm_is_null (args)) 
        return 1;
      formals = scm_cdr (formals);
      args = scm_cdr (args);
    }
  return !scm_is_null (args) ? 1 : 0;
}



/* scm_internal_stack_catch
   Use this one if you want debugging information to be stored in
   the-last-stack on error. */

static SCM
ss_handler (void *data SCM_UNUSED, SCM tag, SCM throw_args)
{
  /* In the stack */
  scm_fluid_set_x (scm_variable_ref
                   (scm_c_module_lookup
                    (scm_c_resolve_module ("ice-9 save-stack"),
                     "the-last-stack")),
		   scm_make_stack (SCM_BOOL_T, SCM_EOL));
  /* Throw the error */
  return scm_throw (tag, throw_args);
}

struct cwss_data
{
  SCM tag;
  scm_t_catch_body body;
  void *data;
};

static SCM
cwss_body (void *data)
{
  struct cwss_data *d = data;
  return scm_c_with_throw_handler (d->tag, d->body, d->data, ss_handler, NULL, 0);
}

SCM
scm_internal_stack_catch (SCM tag,
			  scm_t_catch_body body,
			  void *body_data,
			  scm_t_catch_handler handler,
			  void *handler_data)
{
  struct cwss_data d;
  d.tag = tag;
  d.body = body;
  d.data = body_data;
  scm_c_issue_deprecation_warning
    ("`scm_internal_stack_catch' is deprecated. Talk to guile-devel if you see this message.");
  return scm_internal_catch (tag, cwss_body, &d, handler, handler_data);
}



SCM
scm_short2num (short x)
{
  scm_c_issue_deprecation_warning
    ("`scm_short2num' is deprecated. Use scm_from_short instead.");
  return scm_from_short (x);
}

SCM
scm_ushort2num (unsigned short x)
{
  scm_c_issue_deprecation_warning
    ("`scm_ushort2num' is deprecated. Use scm_from_ushort instead.");
  return scm_from_ushort (x);
}

SCM
scm_int2num (int x)
{
  scm_c_issue_deprecation_warning
    ("`scm_int2num' is deprecated. Use scm_from_int instead.");
  return scm_from_int (x);
}

SCM
scm_uint2num (unsigned int x)
{
  scm_c_issue_deprecation_warning
    ("`scm_uint2num' is deprecated. Use scm_from_uint instead.");
  return scm_from_uint (x);
}

SCM
scm_long2num (long x)
{
  scm_c_issue_deprecation_warning
    ("`scm_long2num' is deprecated. Use scm_from_long instead.");
  return scm_from_long (x);
}

SCM
scm_ulong2num (unsigned long x)
{
  scm_c_issue_deprecation_warning
    ("`scm_ulong2num' is deprecated. Use scm_from_ulong instead.");
  return scm_from_ulong (x);
}

SCM
scm_size2num (size_t x)
{
  scm_c_issue_deprecation_warning
    ("`scm_size2num' is deprecated. Use scm_from_size_t instead.");
  return scm_from_size_t (x);
}

SCM
scm_ptrdiff2num (ptrdiff_t x)
{
  scm_c_issue_deprecation_warning
    ("`scm_ptrdiff2num' is deprecated. Use scm_from_ssize_t instead.");
  return scm_from_ssize_t (x);
}

short
scm_num2short (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2short' is deprecated. Use scm_to_short instead.");
  return scm_to_short (x);
}

unsigned short
scm_num2ushort (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2ushort' is deprecated. Use scm_to_ushort instead.");
  return scm_to_ushort (x);
}

int
scm_num2int (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2int' is deprecated. Use scm_to_int instead.");
  return scm_to_int (x);
}

unsigned int
scm_num2uint (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2uint' is deprecated. Use scm_to_uint instead.");
  return scm_to_uint (x);
}

long
scm_num2long (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2long' is deprecated. Use scm_to_long instead.");
  return scm_to_long (x);
}

unsigned long
scm_num2ulong (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2ulong' is deprecated. Use scm_to_ulong instead.");
  return scm_to_ulong (x);
}

size_t
scm_num2size (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2size' is deprecated. Use scm_to_size_t instead.");
  return scm_to_size_t (x);
}

ptrdiff_t
scm_num2ptrdiff (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2ptrdiff' is deprecated. Use scm_to_ssize_t instead.");
  return scm_to_ssize_t (x);
}

#if SCM_SIZEOF_LONG_LONG != 0

SCM
scm_long_long2num (long long x)
{
  scm_c_issue_deprecation_warning
    ("`scm_long_long2num' is deprecated. Use scm_from_long_long instead.");
  return scm_from_long_long (x);
}

SCM
scm_ulong_long2num (unsigned long long x)
{
  scm_c_issue_deprecation_warning
    ("`scm_ulong_long2num' is deprecated. Use scm_from_ulong_long instead.");
  return scm_from_ulong_long (x);
}

long long
scm_num2long_long (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2long_long' is deprecated. Use scm_to_long_long instead.");
  return scm_to_long_long (x);
}

unsigned long long
scm_num2ulong_long (SCM x, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2ulong_long' is deprecated. Use scm_from_ulong_long instead.");
  return scm_to_ulong_long (x);
}

#endif

SCM
scm_make_real (double x)
{
  scm_c_issue_deprecation_warning
    ("`scm_make_real' is deprecated. Use scm_from_double instead.");
  return scm_from_double (x);
}

double
scm_num2dbl (SCM a, const char *why)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2dbl' is deprecated. Use scm_to_double instead.");
  return scm_to_double (a);
}

SCM
scm_float2num (float n)
{
  scm_c_issue_deprecation_warning
    ("`scm_float2num' is deprecated. Use scm_from_double instead.");
  return scm_from_double ((double) n);
}

SCM
scm_double2num (double n)
{
  scm_c_issue_deprecation_warning
    ("`scm_double2num' is deprecated. Use scm_from_double instead.");
  return scm_from_double (n);
}

SCM
scm_make_complex (double x, double y)
{
  scm_c_issue_deprecation_warning
    ("`scm_make_complex' is deprecated. Use scm_c_make_rectangular instead.");
  return scm_c_make_rectangular (x, y);
}

SCM
scm_mem2symbol (const char *mem, size_t len)
{
  scm_c_issue_deprecation_warning
    ("`scm_mem2symbol' is deprecated. Use scm_from_locale_symboln instead.");
  return scm_from_locale_symboln (mem, len);
}

SCM
scm_mem2uninterned_symbol (const char *mem, size_t len)
{
  scm_c_issue_deprecation_warning
    ("`scm_mem2uninterned_symbol' is deprecated. "
     "Use scm_make_symbol and scm_from_locale_symboln instead.");
  return scm_make_symbol (scm_from_locale_stringn (mem, len));
}

SCM
scm_str2symbol (const char *str)
{
  scm_c_issue_deprecation_warning
    ("`scm_str2symbol' is deprecated. Use scm_from_locale_symbol instead.");
  return scm_from_locale_symbol (str);
}


/* This function must only be applied to memory obtained via malloc,
   since the GC is going to apply `free' to it when the string is
   dropped.

   Also, s[len] must be `\0', since we promise that strings are
   null-terminated.  Perhaps we could handle non-null-terminated
   strings by claiming they're shared substrings of a string we just
   made up.  */
SCM
scm_take_str (char *s, size_t len)
{
  scm_c_issue_deprecation_warning
    ("`scm_take_str' is deprecated. Use scm_take_locale_stringn instead.");
  return scm_take_locale_stringn (s, len);
}

/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  scm_c_issue_deprecation_warning
    ("`scm_take0str' is deprecated. Use scm_take_locale_string instead.");
  return scm_take_locale_string (s);
}

SCM 
scm_mem2string (const char *src, size_t len)
{
  scm_c_issue_deprecation_warning
    ("`scm_mem2string' is deprecated. Use scm_from_locale_stringn instead.");
  return scm_from_locale_stringn (src, len);
}

SCM
scm_str2string (const char *src)
{
  scm_c_issue_deprecation_warning
    ("`scm_str2string' is deprecated. Use scm_from_locale_string instead.");
  return scm_from_locale_string (src);
}

SCM 
scm_makfrom0str (const char *src)
{
  scm_c_issue_deprecation_warning
    ("`scm_makfrom0str' is deprecated."
     "Use scm_from_locale_string instead, but check for NULL first.");
  if (!src) return SCM_BOOL_F;
  return scm_from_locale_string (src);
}

SCM 
scm_makfrom0str_opt (const char *src)
{
  scm_c_issue_deprecation_warning
    ("`scm_makfrom0str_opt' is deprecated."
     "Use scm_from_locale_string instead, but check for NULL first.");
  return scm_makfrom0str (src);
}


SCM
scm_allocate_string (size_t len)
{
  scm_c_issue_deprecation_warning
    ("`scm_allocate_string' is deprecated. Use scm_c_make_string instead.");
  return scm_i_make_string (len, NULL, 0);
}

SCM_DEFINE (scm_make_keyword_from_dash_symbol, "make-keyword-from-dash-symbol", 1, 0, 0, 
            (SCM symbol),
            "Make a keyword object from a @var{symbol} that starts with a dash.")
#define FUNC_NAME s_scm_make_keyword_from_dash_symbol
{
  SCM dash_string, non_dash_symbol;

  scm_c_issue_deprecation_warning
    ("`scm_make_keyword_from_dash_symbol' is deprecated. Don't use dash symbols.");

  SCM_ASSERT (scm_is_symbol (symbol)
	      && (scm_i_symbol_ref (symbol, 0) == '-'),
	      symbol, SCM_ARG1, FUNC_NAME);

  dash_string = scm_symbol_to_string (symbol);
  non_dash_symbol =
    scm_string_to_symbol (scm_c_substring (dash_string,
					   1,
					   scm_c_string_length (dash_string)));

  return scm_symbol_to_keyword (non_dash_symbol);
}
#undef FUNC_NAME

SCM_DEFINE (scm_keyword_dash_symbol, "keyword-dash-symbol", 1, 0, 0, 
            (SCM keyword),
	    "Return the dash symbol for @var{keyword}.\n"
	    "This is the inverse of @code{make-keyword-from-dash-symbol}.")
#define FUNC_NAME s_scm_keyword_dash_symbol
{
  SCM symbol = scm_keyword_to_symbol (keyword);
  SCM parts = scm_list_2 (scm_from_locale_string ("-"),
			  scm_symbol_to_string (symbol));
  scm_c_issue_deprecation_warning
    ("`scm_keyword_dash_symbol' is deprecated. Don't use dash symbols.");

  return scm_string_to_symbol (scm_string_append (parts));
}
#undef FUNC_NAME

SCM
scm_c_make_keyword (const char *s)
{
  scm_c_issue_deprecation_warning
    ("`scm_c_make_keyword' is deprecated. Use scm_from_locale_keyword instead.");
  return scm_from_locale_keyword (s);
}

unsigned int
scm_thread_sleep (unsigned int t)
{
  scm_c_issue_deprecation_warning
    ("`scm_thread_sleep' is deprecated. Use scm_std_sleep instead.");
  return scm_std_sleep (t);
}

unsigned long
scm_thread_usleep (unsigned long t)
{
  scm_c_issue_deprecation_warning
    ("`scm_thread_usleep' is deprecated. Use scm_std_usleep instead.");
  return scm_std_usleep (t);
}

int scm_internal_select (int fds,
                         SELECT_TYPE *rfds,
                         SELECT_TYPE *wfds,
                         SELECT_TYPE *efds,
                         struct timeval *timeout)
{
  scm_c_issue_deprecation_warning
    ("`scm_internal_select' is deprecated. Use scm_std_select instead.");
  return scm_std_select (fds, rfds, wfds, efds, timeout);
}



#ifdef HAVE_CUSERID

# if !HAVE_DECL_CUSERID
extern char *cuserid (char *);
# endif

SCM_DEFINE (scm_cuserid, "cuserid", 0, 0, 0, 
            (void),
	    "Return a string containing a user name associated with the\n"
	    "effective user id of the process.  Return @code{#f} if this\n"
	    "information cannot be obtained.")
#define FUNC_NAME s_scm_cuserid
{
  char buf[L_cuserid];
  char * p;

  scm_c_issue_deprecation_warning
    ("`cuserid' is deprecated. Use `(passwd:name (getpwuid (geteuid)))' instead.");

  p = cuserid (buf);
  if (!p || !*p)
    return SCM_BOOL_F;
  return scm_from_locale_string (p);
}
#undef FUNC_NAME
#endif /* HAVE_CUSERID */



/* {Properties}
 */

static SCM properties_whash;

SCM_DEFINE (scm_primitive_make_property, "primitive-make-property", 1, 0, 0,
	    (SCM not_found_proc),
	    "Create a @dfn{property token} that can be used with\n"
	    "@code{primitive-property-ref} and @code{primitive-property-set!}.\n"
	    "See @code{primitive-property-ref} for the significance of\n"
	    "@var{not_found_proc}.")
#define FUNC_NAME s_scm_primitive_make_property
{
  scm_c_issue_deprecation_warning
    ("`primitive-make-property' is deprecated.  Use object properties.");

  if (not_found_proc != SCM_BOOL_F)
    SCM_VALIDATE_PROC (SCM_ARG1, not_found_proc);
  return scm_cons (not_found_proc, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_primitive_property_ref, "primitive-property-ref", 2, 0, 0,
	    (SCM prop, SCM obj),
	    "Return the property @var{prop} of @var{obj}.\n"
	    "\n"
	    "When no value has yet been associated with @var{prop} and\n"
	    "@var{obj}, the @var{not-found-proc} from @var{prop} is used.  A\n"
	    "call @code{(@var{not-found-proc} @var{prop} @var{obj})} is made\n"
	    "and the result set as the property value.  If\n"
	    "@var{not-found-proc} is @code{#f} then @code{#f} is the\n"
	    "property value.")
#define FUNC_NAME s_scm_primitive_property_ref
{
  SCM h;

  scm_c_issue_deprecation_warning
    ("`primitive-property-ref' is deprecated.  Use object properties.");

  SCM_VALIDATE_CONS (SCM_ARG1, prop);

  h = scm_hashq_get_handle (properties_whash, obj);
  if (scm_is_true (h))
    {
      SCM assoc = scm_assq (prop, SCM_CDR (h));
      if (scm_is_true (assoc))
	return SCM_CDR (assoc);
    }

  if (scm_is_false (SCM_CAR (prop)))
    return SCM_BOOL_F;
  else
    {
      SCM val = scm_call_2 (SCM_CAR (prop), prop, obj);
      if (scm_is_false (h))
	h = scm_hashq_create_handle_x (properties_whash, obj, SCM_EOL);
      SCM_SETCDR (h, scm_acons (prop, val, SCM_CDR (h)));
      return val;
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_primitive_property_set_x, "primitive-property-set!", 3, 0, 0,
	    (SCM prop, SCM obj, SCM val),
	    "Set the property @var{prop} of @var{obj} to @var{val}.")
#define FUNC_NAME s_scm_primitive_property_set_x
{
  SCM h, assoc;

  scm_c_issue_deprecation_warning
    ("`primitive-property-set!' is deprecated.  Use object properties.");

  SCM_VALIDATE_CONS (SCM_ARG1, prop);
  h = scm_hashq_create_handle_x (properties_whash, obj, SCM_EOL);
  assoc = scm_assq (prop, SCM_CDR (h));
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, val);
  else
    {
      assoc = scm_acons (prop, val, SCM_CDR (h));
      SCM_SETCDR (h, assoc);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_primitive_property_del_x, "primitive-property-del!", 2, 0, 0,
	    (SCM prop, SCM obj),
	    "Remove any value associated with @var{prop} and @var{obj}.")
#define FUNC_NAME s_scm_primitive_property_del_x
{
  SCM h;

  scm_c_issue_deprecation_warning
    ("`primitive-property-del!' is deprecated.  Use object properties.");

  SCM_VALIDATE_CONS (SCM_ARG1, prop);
  h = scm_hashq_get_handle (properties_whash, obj);
  if (scm_is_true (h))
    SCM_SETCDR (h, scm_assq_remove_x (SCM_CDR (h), prop));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_i_init_deprecated ()
{
  properties_whash = scm_make_weak_key_hash_table (SCM_UNDEFINED);
#include "libguile/deprecated.x"
}

#endif
