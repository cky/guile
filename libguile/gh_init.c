/* Copyright (C) 1995,1996,1997,2000,2001 Free Software Foundation, Inc.
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


/* Guile high level (gh_) interface, initialization-related stuff */

#include <stdio.h>

#include "libguile/gh.h"

typedef void (*main_prog_t) (int argc, char **argv);
typedef void (*repl_prog_t) (int argc, char **argv);

/* This function takes care of all real GH initialization.  Since it's
   called by scm_boot_guile, it can safely work with heap objects, or
   call functions that do so.  */
static void 
gh_launch_pad (void *closure, int argc, char **argv)
{
  main_prog_t c_main_prog = (main_prog_t) closure;

  c_main_prog (argc, argv);
  exit (0);
}

/* starts up the Scheme interpreter, and stays in it.  c_main_prog()
   is the address of the user's main program, since gh_enter() never
   returns. */
void 
gh_enter (int argc, char *argv[], main_prog_t c_main_prog)
{
  scm_boot_guile (argc, argv, gh_launch_pad, (void *) c_main_prog);
  /* never returns */
}

/* offer a REPL to the C programmer; for now I just invoke the ice-9
   REPL that is written in Scheme */
void 
gh_repl (int argc, char *argv[])
{
/*   gh_eval_str ("(top-repl)"); */
  scm_shell (argc, argv);
}

/* libguile programmers need exception handling mechanisms; here is
   the recommended way of doing it with the gh_ interface */

/* gh_catch() -- set up an exception handler for a particular type of
   error (or any thrown error if tag is SCM_BOOL_T); see
   ../libguile/throw.c for the comments explaining scm_internal_catch */
SCM 
gh_catch (SCM tag, scm_catch_body_t body, void *body_data,
	  scm_catch_handler_t handler, void *handler_data)
{
  return scm_internal_catch (tag, body, body_data, handler, handler_data);
}

SCM 
gh_standard_handler (void *data SCM_UNUSED, SCM tag, SCM throw_args SCM_UNUSED)
{
  fprintf (stderr, "\nJust got an error; tag is\n        ");
  scm_display (tag, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_newline (scm_current_output_port ());

  return SCM_BOOL_F;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
