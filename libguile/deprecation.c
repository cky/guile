/* Copyright (C) 2001 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"

#include "libguile/deprecation.h"
#include "libguile/hashtab.h"
#include "libguile/strings.h"
#include "libguile/ports.h"



#if (SCM_DEBUG_DEPRECATED == 0)

/* This is either a boolean (when a summary should be printed) or a
   hashtab (when detailed warnings shouold be printed).
*/
SCM issued_msgs;

void
scm_c_issue_deprecation_warning (const char *msg)
{
  if (SCM_BOOLP (issued_msgs))
    issued_msgs = SCM_BOOL_T;
  else
    scm_issue_deprecation_warning (SCM_LIST1 (scm_makfrom0str (msg)));
}

SCM_DEFINE(scm_issue_deprecation_warning,
	   "issue-deprecation-warning", 0, 0, 1, 
	   (SCM msgs),
	   "Output @var{msgs} to @code{(current-error-port)} when this "
	   "is the first call to @code{issue-deprecation-warning} with "
	   "this specific @var{msg}.  Do nothing otherwise. "
	   "The argument @var{msgs} should be a list of strings; "
	   "they are printed in turn, each one followed by a newline.")
#define FUNC_NAME s_scm_issue_deprecation_warning
{
  if (SCM_BOOLP (issued_msgs))
    issued_msgs = SCM_BOOL_T;
  else
    {
      SCM handle = scm_hash_create_handle_x (issued_msgs, msgs, SCM_BOOL_F);
      if (SCM_CDR (handle) == SCM_BOOL_F)
	{
	  while (SCM_CONSP (msgs))
	    {
	      scm_display (SCM_CAR (msgs), scm_current_error_port ());
	      scm_newline (scm_current_error_port ());
	      msgs = SCM_CDR (msgs);
	    }
	  SCM_SETCDR (handle, SCM_BOOL_T);
	}
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
print_deprecation_summary (void)
{
  if (issued_msgs == SCM_BOOL_T)
    {
      fputs ("\n"
	     "Some deprecated features have been used.  Set the environment\n"
             "variable GUILE_WARN_DEPRECATED to \"detailed\" and rerun the\n"
	     "program to get more information.  Set it to \"no\" to suppress\n"
	     "this message.\n", stderr);
    }
}

#endif

SCM_DEFINE(scm_include_deprecated_features,
	   "include-deprecated-features", 0, 0, 0,
	   (),
	   "Return @code{#t} iff deprecated features should be included "
           "in public interfaces.")
#define FUNC_NAME s_scm_include_deprecated_features
{
#if SCM_DEBUG_DEPRECATED == 0
  return SCM_BOOL_T;
#else
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME




void
scm_init_deprecation ()
{
#if SCM_DEBUG_DEPRECATED == 0
  const char *level = getenv ("GUILE_WARN_DEPRECATED");
  if (level == NULL)
    level = SCM_WARN_DEPRECATED_DEFAULT;
  if (!strcmp (level, "detailed"))
    issued_msgs = scm_permanent_object (scm_c_make_hash_table (17));
  else if (!strcmp (level, "no"))
    issued_msgs = SCM_BOOL_F;
  else
    {
      issued_msgs = SCM_BOOL_F;
      atexit (print_deprecation_summary);
    }
#endif
#ifndef SCM_MAGIC_SNARFER
#include "libguile/deprecation.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End: */
