/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



#if HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libguile/_scm.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/validate.h"

#include "libguile/feature.h"



static SCM features_var;


void
scm_add_feature (const char *str)
{
  SCM old = SCM_VARIABLE_REF (features_var);
  SCM new = scm_cons (scm_str2symbol (str), old);
  SCM_VARIABLE_SET (features_var, new);
}



SCM_DEFINE (scm_program_arguments, "program-arguments", 0, 0, 0, 
	    (),
	    "@deffnx {Scheme Procedure} command-line\n"
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
  features_var = scm_c_define ("*features*", SCM_EOL);
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
#if SCM_USE_NULL_THREADS
  scm_add_feature ("threads");
#endif
  
  scm_c_define ("char-code-limit", SCM_MAKINUM (SCM_CHAR_CODE_LIMIT));

#include "libguile/feature.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
