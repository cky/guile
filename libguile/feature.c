/*	Copyright (C) 1995,1996,1998 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "_scm.h"

#include "eval.h"

#include "feature.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif


static SCM *scm_loc_features;

void
scm_add_feature(str)
     char* str;
{
  *scm_loc_features = scm_cons(SCM_CAR(scm_intern(str, strlen(str))),
			       *scm_loc_features);
}



SCM_PROC(s_program_arguments, "program-arguments", 0, 0, 0, scm_program_arguments);

SCM 
scm_program_arguments ()
{
  return scm_progargs;
}

/* Set the value returned by program-arguments, given ARGC and ARGV.

   If FIRST is non-zero, make it the first element; we do this in
   situations where other code (like getopt) has parsed out a few
   arguments, but we still want the script name to be the first
   element.  */
void
scm_set_program_arguments (argc, argv, first)
     int argc;
     char **argv;
     char *first;
{
  scm_progargs = scm_makfromstrs (argc, argv);
  if (first)
    scm_progargs = scm_cons (scm_makfrom0str (first), scm_progargs);
}



/* Hooks */

SCM_SYMBOL (scm_sym_hook, "hook");

SCM_PROC (s_make_hook, "make-hook", 0, 0, 0, scm_make_hook);

SCM
scm_make_hook ()
{
  return scm_cons (scm_sym_hook, SCM_EOL);
}

SCM
scm_make_named_hook (char* name)
{
  SCM hook = scm_make_hook ();
  scm_permanent_object (scm_sysintern (name, hook));
  return hook;
}

SCM_PROC (s_add_hook_x, "add-hook!", 2, 1, 0, scm_add_hook_x);

SCM
scm_add_hook_x (SCM hook, SCM thunk, SCM append_p)
{
  SCM rest;
  SCM_ASSERT (SCM_NIMP (hook)
	      && SCM_CONSP (hook)
	      && SCM_CAR (hook) == scm_sym_hook
	      && scm_ilength (SCM_CDR (hook)) >= 0,
	      hook, SCM_ARG1, s_add_hook_x);
  SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)),
	      thunk, SCM_ARG2, s_add_hook_x);
  rest = scm_delq_x (thunk, SCM_CDR (hook));
  SCM_SETCDR (hook, (!SCM_UNBNDP (append_p) && SCM_NFALSEP (append_p)
		     ? scm_append_x (SCM_LIST2 (rest, SCM_LIST1 (thunk)))
		     : scm_cons (thunk, rest)));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_remove_hook_x, "remove-hook!", 2, 0, 0, scm_remove_hook_x);

SCM
scm_remove_hook_x (SCM hook, SCM thunk)
{
  SCM_ASSERT (SCM_NIMP (hook)
	      && SCM_CONSP (hook)
	      && SCM_CAR (hook) == scm_sym_hook
	      && scm_ilength (SCM_CDR (hook)) >= 0,
	      hook, SCM_ARG1, s_add_hook_x);
  SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)),
	      thunk, SCM_ARG2, s_add_hook_x);
  SCM_SETCDR (hook, scm_delq_x (thunk, SCM_CDR (hook)));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_run_hooks, "run-hooks", 1, 0, 0, scm_run_hooks);

SCM
scm_run_hooks (SCM hook)
{
  SCM_ASSERT (SCM_NIMP (hook)
	      && SCM_CONSP (hook)
	      && SCM_CAR (hook) == scm_sym_hook
	      && scm_ilength (SCM_CDR (hook)) >= 0,
	      hook, SCM_ARG1, s_add_hook_x);
  while (SCM_NIMP (hook = SCM_CDR (hook)))
    scm_apply (SCM_CAR (hook), SCM_EOL, SCM_EOL);
  return SCM_UNSPECIFIED;
}



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
#include "feature.x"
}
