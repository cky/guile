/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2003 Free Software
 * Foundation, Inc.
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
#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#include "posix.h"


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
  errno = 0;
  rv = system (SCM_STRING_CHARS (cmd));
  if (rv == -1 || (rv == 127 && errno != 0))
    SCM_SYSERROR;
  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME
#endif /* HAVE_SYSTEM */


#ifdef HAVE_SYSTEM
#ifdef HAVE_WAITPID

/* return a newly allocated array of char pointers to each of the strings
   in args, with a terminating NULL pointer.  */
/* Note: a similar function is defined in dynl.c, but we don't necessarily
   want to export it.  */
static char **
allocate_string_pointers (SCM args)
{
  char **result;
  int n_args = scm_ilength (args);
  int i;

  SCM_ASSERT (n_args >= 0, args, SCM_ARGn, "allocate_string_pointers");
  result = (char **) scm_malloc ((n_args + 1) * sizeof (char *));
  result[n_args] = NULL;
  for (i = 0; i < n_args; i++)
    {
      SCM car = SCM_CAR (args);

      if (!SCM_STRINGP (car))
	{
	  free (result);
	  scm_wrong_type_arg ("allocate_string_pointers", SCM_ARGn, car);
	}
      result[i] = SCM_STRING_CHARS (SCM_CAR (args));
      args = SCM_CDR (args);
    }
  return result;
}

SCM_DEFINE (scm_system_star, "system*", 0, 0, 1,
           (SCM args),
"Execute the command indicated by @var{args}.  The first element must\n"
"be a string indicating the command to be executed, and the remaining\n"
"items must be strings representing each of the arguments to that\n"
"command.\n"
"\n"
"This function returns the exit status of the command as provided by\n"
"@code{waitpid}.  This value can be handled with @code{status:exit-val}\n"
"and the related functions.\n"
"\n"
"@code{system*} is similar to @code{system}, but accepts only one\n"
"string per-argument, and performs no shell interpretation.  The\n"
"command is executed using fork and execlp.  Accordingly this function\n"
"may be safer than @code{system} in situations where shell\n"
"interpretation is not required.\n"
"\n"
"Example: (system* \"echo\" \"foo\" \"bar\")")
#define FUNC_NAME s_scm_system_star
{
  if (SCM_NULLP (args))
    SCM_WRONG_NUM_ARGS ();

  if (SCM_CONSP (args))
    {
      SCM oldint;
      SCM oldquit;
      SCM sig_ign;
      SCM sigint;
      SCM sigquit;
      int pid;
      char **execargv;

      SCM_VALIDATE_STRING (1, SCM_CAR (args));
      /* allocate before fork */
      execargv = allocate_string_pointers (args);

      /* make sure the child can't kill us (as per normal system call) */
      sig_ign = scm_long2num ((long) SIG_IGN);
      sigint = scm_long2num (SIGINT);
      sigquit = scm_long2num (SIGQUIT);
      oldint = scm_sigaction (sigint, sig_ign, SCM_UNDEFINED);
      oldquit = scm_sigaction (sigquit, sig_ign, SCM_UNDEFINED);
      
      pid = fork ();
      if (pid == -1)
        SCM_SYSERROR;
      else if (pid)
        {
          int wait_result;
          int status;
          SCM_SYSCALL (wait_result = waitpid (pid, &status, 0));
          if (wait_result == -1) SCM_SYSERROR;
          scm_sigaction (sigint, SCM_CAR (oldint), SCM_CDR (oldint));
          scm_sigaction (sigquit, SCM_CAR (oldquit), SCM_CDR (oldquit));
          scm_remember_upto_here_2 (oldint, oldquit);
          return SCM_MAKINUM (0L + status);
        }
      else
        {
          execvp (SCM_STRING_CHARS (SCM_CAR (args)), execargv);
          scm_remember_upto_here_1 (args);
          SCM_SYSERROR;
          /* not reached.  */
          return SCM_BOOL_F;
        }
    }
  else
    SCM_WRONG_TYPE_ARG (1, SCM_CAR (args));
}
#undef FUNC_NAME
#endif /* HAVE_WAITPID */
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
