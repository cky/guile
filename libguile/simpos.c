/* Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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

#include <errno.h>
#include <stdlib.h>  /* for getenv */

#include "libguile/_scm.h"

#include "libguile/scmsigs.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/simpos.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


extern int system();


#ifdef HAVE_SYSTEM
SCM_DEFINE (scm_system, "system", 0, 1, 0, 
           (SCM cmd),
	    "Execute @var{cmd} using the operating system's \"command\n"
	    "processor\".  Under Unix this is usually the default shell\n"
	    "@code{sh}.  The value returned is @var{cmd}'s exit status as\n"
	    "returned by @code{waitpid}, which can be interpreted using\n"
	    "@code{status:exit-val} and friends.\n"
	    "\n"
	    "If @code{system} is called without arguments, return a boolean\n"
	    "indicating whether the command processor is available.")
#define FUNC_NAME s_scm_system
{
  int rv;

  if (SCM_UNBNDP (cmd))
    {
      rv = system (NULL);
      return SCM_BOOL(rv);
    }
  SCM_VALIDATE_STRING (1, cmd);
  SCM_DEFER_INTS;
  errno = 0;
  rv = system (SCM_STRING_CHARS (cmd));
  if (rv == -1 || (rv == 127 && errno != 0))
    SCM_SYSERROR;
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME
#endif /* HAVE_SYSTEM */

SCM_DEFINE (scm_getenv, "getenv", 1, 0, 0, 
            (SCM nam),
	    "Looks up the string @var{name} in the current environment.  The return\n"
	    "value is @code{#f} unless a string of the form @code{NAME=VALUE} is\n"
	    "found, in which case the string @code{VALUE} is returned.")
#define FUNC_NAME s_scm_getenv
{
  char *val;
  SCM_VALIDATE_STRING (1, nam);
  val = getenv (SCM_STRING_CHARS (nam));
  return val ? scm_mem2string (val, strlen (val)) : SCM_BOOL_F;
}
#undef FUNC_NAME

/* simple exit, without unwinding the scheme stack or flushing ports.  */
SCM_DEFINE (scm_primitive_exit, "primitive-exit", 0, 1, 0, 
            (SCM status),
	    "Terminate the current process without unwinding the Scheme stack.\n"
	    "This is would typically be useful after a fork.  The exit status\n"
	    "is @var{status} if supplied, otherwise zero.")
#define FUNC_NAME s_scm_primitive_exit
{
  int cstatus = 0;
  if (!SCM_UNBNDP (status))
    {
      SCM_VALIDATE_INUM (1, status);
      cstatus = SCM_INUM (status);
    }
  exit (cstatus);
}
#undef FUNC_NAME


void
scm_init_simpos ()
{
#include "libguile/simpos.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
