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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libguile/_scm.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/validate.h"

#include "libguile/feature.h"



static SCM features;


void
scm_add_feature (const char *str)
{
  SCM old = SCM_CDR (features);
  SCM new = scm_cons (SCM_CAR (scm_intern (str, strlen (str))), old);
  SCM_SETCDR (features, new);
}



SCM_DEFINE (scm_program_arguments, "program-arguments", 0, 0, 0, 
	    (),
	    "@deffnx procedure command-line\n"
	    "Return the list of command line arguments passed to Guile, as a list of\n"
	    "strings.  The list includes the invoked program name, which is usually\n"
	    "@code{\"guile\"}, but excludes switches and parameters for command line\n"
	    "options like @code{-e} and @code{-l}.")
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




void
scm_init_feature()
{
  features = scm_sysintern ("*features*", SCM_EOL);
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

#include "libguile/feature.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
