/*      Copyright (C) 1995,1996,1997,1998, 2000 Free Software Foundation, Inc.

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



/* Defining Scheme functions implemented by C functions --- subrs.  */

#include "libguile/gh.h"

/* allows you to define new scheme primitives written in C */
SCM
gh_new_procedure (const char *proc_name, SCM (*fn) (),
		  int n_required_args, int n_optional_args, int varp)
{
  return scm_c_define_gsubr (proc_name, n_required_args, n_optional_args,
			     varp, fn);
}

SCM
gh_new_procedure0_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 0, 0, 0);
}

SCM
gh_new_procedure0_1 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 0, 1, 0);
}

SCM
gh_new_procedure0_2 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 0, 2, 0);
}

SCM
gh_new_procedure1_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 1, 0, 0);
}

SCM
gh_new_procedure1_1 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 1, 1, 0);
}

SCM
gh_new_procedure1_2 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 1, 2, 0);
}

SCM
gh_new_procedure2_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 2, 0, 0);
}

SCM
gh_new_procedure2_1 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 2, 1, 0);
}

SCM
gh_new_procedure2_2 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 2, 2, 0);
}

SCM
gh_new_procedure3_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 3, 0, 0);
}

SCM
gh_new_procedure4_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 4, 0, 0);
}

SCM
gh_new_procedure5_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 5, 0, 0);
}

/* some (possibly most) Scheme functions available from C */
SCM
gh_define (const char *name, SCM val)
{
  scm_c_define (name, val);
  return SCM_UNSPECIFIED;
}


/* Calling Scheme functions from C.  */

SCM
gh_apply (SCM proc, SCM args)
{
  return scm_apply (proc, args, SCM_EOL);
}

SCM
gh_call0 (SCM proc)
{
  return scm_apply (proc, SCM_EOL, SCM_EOL);
}

SCM
gh_call1 (SCM proc, SCM arg)
{
  return scm_apply (proc, arg, scm_listofnull);
}

SCM
gh_call2 (SCM proc, SCM arg1, SCM arg2)
{
  return scm_apply (proc, arg1, scm_cons (arg2, scm_listofnull));
}

SCM
gh_call3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  return scm_apply (proc, arg1, scm_cons2 (arg2, arg3, scm_listofnull));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
