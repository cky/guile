/*	Copyright (C) 1995, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "libguile/_scm.h"

#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/objprop.h"
#include "libguile/procprop.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/hooks.h"


/* C level hooks
 *
 * Currently, this implementation is separate from the Scheme level
 * hooks.  The possibility exists to implement the Scheme level hooks
 * using C level hooks.
 */

void
scm_c_hook_init (scm_c_hook_t *hook, void *hook_data, scm_c_hook_type_t type)
{
  hook->first = 0;
  hook->type = type;
  hook->data = hook_data;
}

void
scm_c_hook_add (scm_c_hook_t *hook,
		scm_c_hook_function_t func,
		void *func_data, 
		int appendp)
{
  scm_c_hook_entry_t *entry = scm_must_malloc (sizeof (scm_c_hook_entry_t),
					     "C level hook entry");
  scm_c_hook_entry_t **loc = &hook->first;
  if (appendp)
    while (*loc)
      *loc = (*loc)->next;
  entry->next = *loc;
  entry->func = func;
  entry->data = func_data;
  *loc = entry;
}

void
scm_c_hook_remove (scm_c_hook_t *hook,
		   scm_c_hook_function_t func,
		   void *func_data)
{
  scm_c_hook_entry_t **loc = &hook->first;
  while (*loc)
    {
      if ((*loc)->func == func && (*loc)->data == func_data)
	{
	  scm_c_hook_entry_t *entry = *loc;
	  *loc = (*loc)->next;
	  scm_must_free (entry);
	  return;
	}
      loc = &(*loc)->next;
    }
  fprintf (stderr, "Attempt to remove non-existent hook function\n");
  abort ();
}

void *
scm_c_hook_run (scm_c_hook_t *hook, void *data)
{
  scm_c_hook_entry_t *entry = hook->first;
  scm_c_hook_type_t type = hook->type;
  void *res = 0;
  while (entry)
    {
      res = (entry->func) (hook->data, entry->data, data);
      if (res)
	{
	  if (type == SCM_C_HOOK_OR)
	    break;
	}
      else
	{
	  if (type == SCM_C_HOOK_AND)
	    break;
	}
      entry = entry->next;
    }
  return res;
}


/* Scheme level hooks
 *
 * A hook is basically a list of procedures to be called at well defined
 * points in time.
 *
 * Hook arity is not a full member of this type and therefore lacks an
 * accessor.  It exists to aid debugging and is not intended to be used in
 * programs.
 */

long scm_tc16_hook;


static SCM
make_hook (SCM n_args, const char *subr)
{
  int n;

  if (SCM_UNBNDP (n_args))
    {
      n = 0;
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (n_args), n_args, SCM_ARGn, subr);
      n = SCM_INUM (n_args);
      if (n < 0 || n > 16)
	scm_out_of_range (subr, n_args);
    }
  SCM_RETURN_NEWSMOB (scm_tc16_hook + (n << 16), SCM_UNPACK (SCM_EOL));
}


static int
print_hook (SCM hook, SCM port, scm_print_state *pstate)
{
  SCM ls, name;
  scm_puts ("#<hook ", port);
  scm_intprint (SCM_HOOK_ARITY (hook), 10, port);
  scm_putc (' ', port);
  scm_intprint (SCM_UNPACK (hook), 16, port);
  ls = SCM_HOOK_PROCEDURES (hook);
  while (SCM_NIMP (ls))
    {
      scm_putc (' ', port);
      name = scm_procedure_name (SCM_CAR (ls));
      if (SCM_NFALSEP (name))
	scm_iprin1 (name, port, pstate);
      else
	scm_putc ('?', port);
      ls = SCM_CDR (ls);
    }
  scm_putc ('>', port);
  return 1;
}


SCM_SYMBOL (symbol_name, "name");

SCM
scm_create_hook (const char* name, int n_args)
{
  SCM vcell = scm_sysintern0 (name);
  SCM hook = make_hook (SCM_MAKINUM (n_args), "scm_create_hook");
  SCM_SETCDR (vcell, hook);
  scm_set_object_property_x (hook, symbol_name, scm_makfrom0str (name));
  scm_protect_object (hook);
  return hook;
}


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_DEFINE (scm_make_hook_with_name, "make-hook-with-name", 1, 1, 0, 
            (SCM name, SCM n_args),
"")
#define FUNC_NAME s_scm_make_hook_with_name
{
  SCM hook = make_hook (n_args, FUNC_NAME);
  scm_set_object_property_x (hook, scm_makfrom0str ("name"), name);
  return hook;
}
#undef FUNC_NAME

#endif  /* SCM_DEBUG_DEPRECATED == 0 */


SCM_DEFINE (scm_make_hook, "make-hook", 0, 1, 0, 
            (SCM n_args),
"")
#define FUNC_NAME s_scm_make_hook
{
  return make_hook (n_args, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hook_p, "hook?", 1, 0, 0, 
            (SCM x),
"")
#define FUNC_NAME s_scm_hook_p
{
  return SCM_BOOL (SCM_HOOKP (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_hook_empty_p, "hook-empty?", 1, 0, 0, 
            (SCM hook),
"")
#define FUNC_NAME s_scm_hook_empty_p
{
  SCM_VALIDATE_HOOK (1, hook);
  return SCM_BOOL (SCM_NULLP (SCM_HOOK_PROCEDURES (hook)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_add_hook_x, "add-hook!", 2, 1, 0, 
            (SCM hook, SCM proc, SCM append_p),
"")
#define FUNC_NAME s_scm_add_hook_x
{
  SCM arity, rest;
  int n_args;
  SCM_VALIDATE_HOOK (1,hook);
  SCM_ASSERT (SCM_NFALSEP (arity = scm_i_procedure_arity (proc)),
	      proc, SCM_ARG2, FUNC_NAME);
  n_args = SCM_HOOK_ARITY (hook);
  if (SCM_INUM (SCM_CAR (arity)) > n_args
      || (SCM_FALSEP (SCM_CADDR (arity))
	  && (SCM_INUM (SCM_CAR (arity)) + SCM_INUM (SCM_CADR (arity))
	      < n_args)))
    scm_wrong_type_arg (FUNC_NAME, 2, proc);
  rest = scm_delq_x (proc, SCM_HOOK_PROCEDURES (hook));
  SCM_SET_HOOK_PROCEDURES (hook,
			   (!SCM_UNBNDP (append_p) && !SCM_FALSEP (append_p)
			    ? scm_append_x (SCM_LIST2 (rest, SCM_LIST1 (proc)))
			    : scm_cons (proc, rest)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_remove_hook_x, "remove-hook!", 2, 0, 0, 
            (SCM hook, SCM proc),
"")
#define FUNC_NAME s_scm_remove_hook_x
{
  SCM_VALIDATE_HOOK (1, hook);
  SCM_SET_HOOK_PROCEDURES (hook,
			   scm_delq_x (proc, SCM_HOOK_PROCEDURES (hook)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_reset_hook_x, "reset-hook!", 1, 0, 0, 
            (SCM hook),
"")
#define FUNC_NAME s_scm_reset_hook_x
{
  SCM_VALIDATE_HOOK (1,hook);
  SCM_SET_HOOK_PROCEDURES (hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_run_hook, "run-hook", 1, 0, 1, 
            (SCM hook, SCM args),
"")
#define FUNC_NAME s_scm_run_hook
{
  SCM_VALIDATE_HOOK (1,hook);
  if (scm_ilength (args) != SCM_HOOK_ARITY (hook))
    SCM_MISC_ERROR ("Hook ~S requires ~A arguments",
		    SCM_LIST2 (hook,SCM_MAKINUM (SCM_HOOK_ARITY (hook))));
  scm_c_run_hook (hook, args);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
scm_c_run_hook (SCM hook, SCM args)
{
  SCM procs = SCM_HOOK_PROCEDURES (hook);
  while (SCM_NIMP (procs))
    {
      scm_apply (SCM_CAR (procs), args, SCM_EOL);
      procs = SCM_CDR (procs);
    }
}


SCM_DEFINE (scm_hook_to_list, "hook->list", 1, 0, 0, 
            (SCM hook),
"")
#define FUNC_NAME s_scm_hook_to_list
{
  SCM_VALIDATE_HOOK (1, hook);
  return scm_list_copy (SCM_HOOK_PROCEDURES (hook));
}
#undef FUNC_NAME




void
scm_init_hooks ()
{
  scm_tc16_hook = scm_make_smob_type ("hook", 0);
  scm_set_smob_mark (scm_tc16_hook, scm_markcdr);
  scm_set_smob_print (scm_tc16_hook, print_hook);

#include "libguile/hooks.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
