/*	Copyright (C) 1995,1996,1997,1998, 2000 Free Software Foundation, Inc.
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
	    "Executes @var{cmd} using the operating system's \"command processor\".\n"
	    "Under Unix this is usually the default shell @code{sh}.  The value\n"
	    "returned is @var{cmd}'s exit status as returned by @code{waitpid}, which\n"
	    "can be interpreted using the functions above.\n\n"
	    "If @code{system} is called without arguments, it returns a boolean\n"
	    "indicating whether the command processor is available.")
#define FUNC_NAME s_scm_system
{
  int rv;

  if (SCM_UNBNDP (cmd))
    {
      rv = system (NULL);
      return SCM_BOOL(rv);
    }
  SCM_VALIDATE_ROSTRING (1,cmd);
  SCM_DEFER_INTS;
  errno = 0;
  if (SCM_ROSTRINGP (cmd))
    cmd = scm_makfromstr (SCM_ROCHARS (cmd), SCM_ROLENGTH (cmd), 0);
  rv = system(SCM_ROCHARS(cmd));
  if (rv == -1 || (rv == 127 && errno != 0))
    SCM_SYSERROR;
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME
#endif /* HAVE_SYSTEM */

extern char *getenv();
SCM_DEFINE (scm_getenv, "getenv", 1, 0, 0, 
            (SCM nam),
	    "Looks up the string @var{name} in the current environment.  The return\n"
	    "value is @code{#f} unless a string of the form @code{NAME=VALUE} is\n"
	    "found, in which case the string @code{VALUE} is returned.")
#define FUNC_NAME s_scm_getenv
{
  char *val;
  SCM_VALIDATE_ROSTRING (1,nam);
  nam = scm_makfromstr (SCM_ROCHARS (nam), SCM_ROLENGTH (nam), 0);
  val = getenv(SCM_CHARS(nam));
  return (val) ? scm_makfromstr(val, (scm_sizet)strlen(val), 0) : SCM_BOOL_F;
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
      SCM_VALIDATE_INUM (1,status);
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
