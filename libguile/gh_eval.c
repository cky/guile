/*      Copyright (C) 1995,1996,1997,1998, 2000, 2001 Free Software Foundation, Inc.

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


/* routines to evaluate Scheme code */

#include "libguile/gh.h"

typedef SCM (*gh_eval_t) (void *data, SCM jmpbuf);

/* Evaluate the string; toss the value.  */
SCM
gh_eval_str (const char *scheme_code)
{
  return scm_c_eval_string (scheme_code);
}

/* evaluate the file by passing it to the lower level scm_primitive_load() */
SCM
gh_eval_file (const char *fname)
{
  return scm_primitive_load (gh_str02scm (fname));
}

static SCM
eval_str_wrapper (void *data)
{
/*   gh_eval_t real_eval_proc = (gh_eval_t) (* ((gh_eval_t *) data)); */

  char *scheme_code = (char *) data;
  return gh_eval_str (scheme_code);
}

SCM
gh_eval_str_with_catch (const char *scheme_code, scm_t_catch_handler handler)
{
  /* FIXME: not there yet */
  return gh_catch (SCM_BOOL_T, (scm_t_catch_body) eval_str_wrapper, (void *) scheme_code,
		   (scm_t_catch_handler) handler, (void *) scheme_code);
}

SCM
gh_eval_str_with_standard_handler (const char *scheme_code)
{
  return gh_eval_str_with_catch (scheme_code, gh_standard_handler);
}

SCM
gh_eval_str_with_stack_saving_handler (const char *scheme_code)
{
  return scm_internal_stack_catch (SCM_BOOL_T,
				   (scm_t_catch_body) eval_str_wrapper,
				   (void *) scheme_code,
				   (scm_t_catch_handler)
				   gh_standard_handler,
				   (void *) scheme_code);
}

static SCM
eval_file_wrapper (void *data)
{
/*   gh_eval_t real_eval_proc = (gh_eval_t) (* ((gh_eval_t *) data)); */

  char *scheme_code = (char *) data;
  return gh_eval_file (scheme_code);
}

SCM
gh_eval_file_with_catch (const char *scheme_code, scm_t_catch_handler handler)
{
  /* FIXME: not there yet */
  return gh_catch (SCM_BOOL_T, (scm_t_catch_body) eval_file_wrapper,
		   (void *) scheme_code, (scm_t_catch_handler) handler,
		   (void *) scheme_code);
}

SCM
gh_eval_file_with_standard_handler (const char *scheme_code)
{
  return gh_eval_file_with_catch (scheme_code, gh_standard_handler);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
