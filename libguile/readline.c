/* readline.c --- line editing support for Guile */

/*	Copyright (C) 1997 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include "_scm.h"
#ifdef HAVE_LIBREADLINE
#include <libguile.h>
#include <readline.h>
#include <gh.h>
#include <readline/readline.h>
#include <readline/history.h>

static int
current_input_getc ()
{
  return scm_getc (scm_cur_inp);
}

SCM_PROC (s_readline, "readline", 0, 1, 0, scm_readline);

SCM
scm_readline (SCM text)
{
  SCM ret;
  char* s;
  char* prompt;

  if (! SCM_UNBNDP (text))
    {
      SCM_ASSERT ((SCM_NIMP(text) && SCM_STRINGP(text)), text, SCM_ARG1,
		  s_readline);
      SCM_COERCE_SUBSTR (text);
    }

  prompt = SCM_UNBNDP (text) ? "" : SCM_CHARS (text);

  s = readline (prompt);
  if (s)
    ret = scm_makfrom0str (s);
  else 
    ret = SCM_EOF_VAL;

  free (s);

  return ret;
}

SCM_PROC (s_add_history, "add-history", 1, 0, 0, scm_add_history);

SCM
scm_add_history (SCM text)
{
  char* s;
  SCM_ASSERT ((SCM_NIMP(text) && SCM_STRINGP(text)), text, SCM_ARG1,
	      s_add_history);
  SCM_COERCE_SUBSTR (text);

  SCM_DEFER_INTS;
  s = SCM_CHARS (text);
  add_history (strdup (s));
  SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

static SCM subr_filename_completion_function;
static char s_filename_completion_function[] = "filename-completion-function";

SCM
scm_filename_completion_function (SCM text, SCM continuep)
{
  char *s;
  SCM ans;
  SCM_ASSERT (SCM_NIMP (text) && SCM_STRINGP (text),
	      text,
	      SCM_ARG1,
	      s_filename_completion_function);
  SCM_COERCE_SUBSTR (text);
  s = filename_completion_function (SCM_CHARS (text), SCM_NFALSEP (continuep));
  ans = scm_makfrom0str (s);
  free (s);
  return ans;
}

/*
 * The following has been modified from code contributed by
 * Andrew Archibald <aarchiba@undergrad.math.uwaterloo.ca>
 */

SCM scm_readline_completion_function_var;

static SCM
apply (SCM a)
{
  return scm_apply (SCM_CAR (a), SCM_CDR (a), SCM_EOL);
}

static char *
completion_function (char *text, int continuep)
{
  SCM_STACKITEM mark;
  SCM compfunc = SCM_CDR (scm_readline_completion_function_var);
  SCM res;

  if (SCM_FALSEP (compfunc))
    return NULL; /* #f => completion disabled */
  else
    {
      SCM t = scm_makfrom0str (text);
      SCM c = continuep ? SCM_BOOL_T : SCM_BOOL_F;
      res = scm_internal_cwdr ((scm_catch_body_t) apply,
			       (void *) SCM_LIST3 (compfunc, t, c),
			       scm_handle_by_throw,
			       0,
			       &mark);
  
      if (SCM_FALSEP (res))
	return NULL;
  
      if (!(SCM_NIMP (res) && SCM_STRINGP (res)))
	scm_misc_error (s_readline,
			"Completion function returned bogus value: %S",
			SCM_LIST1 (res));
      SCM_COERCE_SUBSTR (res);
      return strdup (SCM_CHARS (res));
    }
}


void
scm_init_readline ()
{
#include "readline.x"
  subr_filename_completion_function
    = scm_make_subr (s_filename_completion_function,
		     scm_tc7_subr_2,
		     scm_filename_completion_function);
  scm_readline_completion_function_var
    = scm_sysintern ("*readline-completion-function*", SCM_BOOL_F);
  rl_getc_function = current_input_getc;
  rl_completion_entry_function = (Function*) completion_function;
  scm_add_feature ("readline");
}

#endif 
