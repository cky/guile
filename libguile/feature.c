/*	Copyright (C) 1995, 1996, 1998, 1999 Free Software Foundation, Inc.
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
#include "_scm.h"

#include "eval.h"
#include "ports.h"
#include "procprop.h"
#include "root.h"
#include "smob.h"
#include "strings.h"

#include "validate.h"
#include "feature.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif


static SCM *scm_loc_features;

void
scm_add_feature (const char *str)
{
  *scm_loc_features = scm_cons (SCM_CAR (scm_intern (str, strlen (str))),
				*scm_loc_features);
}




SCM_DEFINE (scm_program_arguments, "program-arguments", 0, 0, 0, 
           (),
"")
#define FUNC_NAME s_scm_program_arguments
{
  return scm_progargs;
}
#undef FUNC_NAME

/* Set the value returned by program-arguments, given ARGC and ARGV.

   If FIRST is non-zero, make it the first element; we do this in
   situations where other code (like getopt) has parsed out a few
   arguments, but we still want the script name to be the first
   element.  */
void
scm_set_program_arguments (int argc, char **argv, char *first)
{
  scm_progargs = scm_makfromstrs (argc, argv);
  if (first)
    scm_progargs = scm_cons (scm_makfrom0str (first), scm_progargs);
}



/* Hooks
 *
 * A hook is basically a list of procedures to be called at well defined
 * points in time.
 *
 * Hook name and arity are not full members of this type and therefore
 * lack accessors.  They are added to aid debugging and are not
 * intended to be used in programs.
 *
 */

long scm_tc16_hook;


static SCM
make_hook (SCM name, SCM n_args, const char *subr)
{
  int n;
  SCM_ASSERT (SCM_FALSEP (name) || (SCM_SYMBOLP (name)),
	      name,
	      SCM_ARG1,
	      subr);
  if (SCM_UNBNDP (n_args))
    n = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (n_args), n_args, SCM_ARGn, subr);
      n = SCM_INUM (n_args);
    }
  SCM_ASSERT (n >= 0 && n <= 16, n_args, SCM_OUTOFRANGE, subr);
  SCM_RETURN_NEWSMOB (scm_tc16_hook + (n << 16), SCM_UNPACK (SCM_LIST1 (name)));
}


static int
print_hook (SCM hook, SCM port, scm_print_state *pstate)
{
  SCM ls, name;
  scm_puts ("#<hook ", port);
  if (SCM_NFALSEP (SCM_HOOK_NAME (hook)))
    {
      scm_iprin1 (SCM_HOOK_NAME (hook), port, pstate);
      scm_putc (' ', port);
    }
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


SCM
scm_create_hook (const char* name, int n_args)
{
  SCM vcell = scm_sysintern0 (name);
  SCM hook = make_hook (SCM_CAR (vcell), SCM_MAKINUM (n_args),
			"scm_create_hook");
  SCM_SETCDR (vcell, hook);
  scm_protect_object (vcell);
  return hook;
}


/* This function is deprecated.  It will be removed in next release. */
SCM
scm_make_named_hook (const char* name, int n_args)
{
  return scm_create_hook (name, n_args);
}


SCM_DEFINE (scm_make_hook_with_name, "make-hook-with-name", 1, 1, 0, 
            (SCM name, SCM n_args),
"")
#define FUNC_NAME s_scm_make_hook_with_name
{
  return make_hook (name, n_args, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_hook, "make-hook", 0, 1, 0, 
            (SCM n_args),
"")
#define FUNC_NAME s_scm_make_hook
{
  return make_hook (SCM_BOOL_F, n_args, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hook_p, "hook?", 1, 0, 0, 
            (SCM x),
"")
#define FUNC_NAME s_scm_hook_p
{
  return SCM_BOOL(SCM_HOOKP (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_hook_empty_p, "hook-empty?", 1, 0, 0, 
            (SCM hook),
"")
#define FUNC_NAME s_scm_hook_empty_p
{
  SCM_VALIDATE_HOOK (1,hook);
  return SCM_BOOL(SCM_NULLP (SCM_HOOK_PROCEDURES (hook)));
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
			   (!SCM_UNBNDP (append_p) && SCM_NFALSEP (append_p)
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
  SCM_VALIDATE_HOOK (1,hook);
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
  if (SCM_UNBNDP (args))
    args = SCM_EOL;
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
  SCM_VALIDATE_HOOK (1,hook);
  return scm_list_copy (SCM_HOOK_PROCEDURES (hook));
}
#undef FUNC_NAME




void
scm_init_feature()
{
  scm_loc_features = SCM_CDRLOC (scm_sysintern ("*features*", SCM_EOL));
#ifdef SCM_RECKLESS
  scm_add_feature("reckless");
#endif
#ifndef _Windows
  scm_add_feature("system");
#endif
#ifdef vms
  scm_add_feature(s_ed);
#endif
#ifdef SICP
  scm_add_feature("sicp");
#endif
#ifndef GO32
  scm_add_feature("char-ready?");
#endif
#ifndef CHEAP_CONTINUATIONS
  scm_add_feature ("full-continuation");
#endif
#ifdef USE_THREADS
  scm_add_feature ("threads");
#endif
  
  scm_sysintern ("char-code-limit", SCM_MAKINUM (SCM_CHAR_CODE_LIMIT));

  scm_tc16_hook = scm_make_smob_type ("hook", 0);
  scm_set_smob_mark (scm_tc16_hook, scm_markcdr);
  scm_set_smob_print (scm_tc16_hook, print_hook);

#include "feature.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
