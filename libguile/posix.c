/*	Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
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
#include "_scm.h"
#include "fports.h"
#include "scmsigs.h"
#include "feature.h"

#include "scm_validate.h"
#include "posix.h"


#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
#ifndef ttyname
extern char *ttyname();
#endif
#endif

#ifdef LIBC_H_WITH_UNISTD_H
#include <libc.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <pwd.h>

#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#include <signal.h>

extern FILE *popen ();
extern char ** environ;

#include <grp.h>
#include <sys/utsname.h>

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif

/* Some Unix systems don't define these.  CPP hair is dangerous, but
   this seems safe enough... */
#ifndef R_OK
#define R_OK 4
#endif

#ifndef W_OK
#define W_OK 2
#endif

#ifndef X_OK
#define X_OK 1
#endif

#ifndef F_OK
#define F_OK 0
#endif

/* On NextStep, <utime.h> doesn't define struct utime, unless we
   #define _POSIX_SOURCE before #including it.  I think this is less
   of a kludge than defining struct utimbuf ourselves.  */
#ifdef UTIMBUF_NEEDS_POSIX
#define _POSIX_SOURCE
#endif

#ifdef HAVE_SYS_UTIME_H
#include <sys/utime.h>
#endif

#ifdef HAVE_UTIME_H
#include <utime.h>
#endif

/* Please don't add any more #includes or #defines here.  The hack
   above means that _POSIX_SOURCE may be #defined, which will
   encourage header files to do strange things.  */


SCM_SYMBOL (sym_read_pipe, "read pipe");
SCM_SYMBOL (sym_write_pipe, "write pipe");

SCM_DEFINE (scm_pipe, "pipe", 0, 0, 0,
            (),
"Creates a pipe which can be used for communication.  The return value
is a pair in which the CAR contains an input port and the CDR an
output port.  Data written to the output port can be read from the
input port.  Note that both ports are buffered so it may be necessary
to flush the output port before data will actually be sent across the pipe.
Alternatively a buffer can be added to the port using @code{setvbuf}
(see below).")
#define FUNC_NAME s_scm_pipe
{
  int fd[2], rv;
  SCM p_rd, p_wt;

  rv = pipe (fd);
  if (rv)
    SCM_SYSERROR;
  
  p_rd = scm_fdes_to_port (fd[0], "r", sym_read_pipe);
  p_wt = scm_fdes_to_port (fd[1], "w", sym_write_pipe);
  return scm_cons (p_rd, p_wt);
}
#undef FUNC_NAME


#ifdef HAVE_GETGROUPS
SCM_DEFINE (scm_getgroups, "getgroups", 0, 0, 0,
            (),
"Returns a vector of integers representing the current supplimentary group IDs.")
#define FUNC_NAME s_scm_getgroups
{
  SCM grps, ans;
  int ngroups = getgroups (0, NULL);
  if (!ngroups)
    SCM_SYSERROR;
  SCM_NEWCELL(grps);
  SCM_DEFER_INTS;
  {
    GETGROUPS_T *groups;
    int val;

    groups = SCM_MUST_MALLOC_TYPE_NUM(GETGROUPS_T,ngroups);					    
    val = getgroups(ngroups, groups);
    if (val < 0)
      {
	int en = errno;
	scm_must_free((char *)groups);
	errno = en;
	SCM_SYSERROR;
      }
    SCM_SETCHARS(grps, groups);	/* set up grps as a GC protect */
    SCM_SETLENGTH(grps, 0L + ngroups * sizeof(GETGROUPS_T), scm_tc7_string);
    ans = scm_make_vector (SCM_MAKINUM(ngroups), SCM_UNDEFINED);
    while (--ngroups >= 0) SCM_VELTS(ans)[ngroups] = SCM_MAKINUM(groups[ngroups]);
    SCM_SETCHARS(grps, groups);	/* to make sure grps stays around. */
    SCM_ALLOW_INTS;
    return ans;
  }
}
#undef FUNC_NAME  
#endif


SCM_DEFINE (scm_getpwuid, "getpw", 0, 1, 0,
            (SCM user),
"Look up an entry in the user database.  @var{obj} can be an integer,
a string, or omitted, giving the behaviour of getpwuid, getpwnam
or getpwent respectively.")
#define FUNC_NAME s_scm_getpwuid
{
  SCM result;
  struct passwd *entry;
  SCM *ve;

  result = scm_make_vector (SCM_MAKINUM (7), SCM_UNSPECIFIED);
  ve = SCM_VELTS (result);
  if (SCM_UNBNDP (user) || SCM_FALSEP (user))
    {
      SCM_SYSCALL (entry = getpwent ());
      if (! entry)
	{
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_INUMP (user))
    {
      entry = getpwuid (SCM_INUM (user));
    }
  else
    {
      SCM_VALIDATE_ROSTRING (1,user);
      if (SCM_SUBSTRP (user))
	user = scm_makfromstr (SCM_ROCHARS (user), SCM_ROLENGTH (user), 0);
      entry = getpwnam (SCM_ROCHARS (user));
    }
  if (!entry)
    SCM_MISC_ERROR ("entry not found", SCM_EOL);

  ve[0] = scm_makfrom0str (entry->pw_name);
  ve[1] = scm_makfrom0str (entry->pw_passwd);
  ve[2] = scm_ulong2num ((unsigned long) entry->pw_uid);
  ve[3] = scm_ulong2num ((unsigned long) entry->pw_gid);
  ve[4] = scm_makfrom0str (entry->pw_gecos);
  if (!entry->pw_dir)
    ve[5] = scm_makfrom0str ("");
  else
    ve[5] = scm_makfrom0str (entry->pw_dir);
  if (!entry->pw_shell)
    ve[6] = scm_makfrom0str ("");
  else
    ve[6] = scm_makfrom0str (entry->pw_shell);
  return result;
}
#undef FUNC_NAME


#ifdef HAVE_SETPWENT
SCM_DEFINE (scm_setpwent, "setpw", 0, 1, 0,
            (SCM arg),
"If called with a true argument, initialize or reset the password data
stream.  Otherwise, close the stream.  The @code{setpwent} and
@code{endpwent} procedures are implemented on top of this.")
#define FUNC_NAME s_scm_setpwent
{
  if (SCM_UNBNDP (arg) || SCM_FALSEP (arg))
    endpwent ();
  else
    setpwent ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif



/* Combines getgrgid and getgrnam.  */
SCM_DEFINE (scm_getgrgid, "getgr", 0, 1, 0,
            (SCM name),
"Look up an entry in the group database.  @var{obj} can be an integer,
a string, or omitted, giving the behaviour of getgrgid, getgrnam
or getgrent respectively.")
#define FUNC_NAME s_scm_getgrgid
{
  SCM result;
  struct group *entry;
  SCM *ve;
  result = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED);
  ve = SCM_VELTS (result);
  if (SCM_UNBNDP (name) || (name == SCM_BOOL_F))
    {
      SCM_SYSCALL (entry = getgrent ());
      if (! entry)
	{
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_INUMP (name))
    SCM_SYSCALL (entry = getgrgid (SCM_INUM (name)));
  else
    {
      SCM_VALIDATE_ROSTRING (1,name);
      SCM_COERCE_SUBSTR (name);
      SCM_SYSCALL (entry = getgrnam (SCM_ROCHARS (name)));
    }
  if (!entry)
    SCM_SYSERROR;

  ve[0] = scm_makfrom0str (entry->gr_name);
  ve[1] = scm_makfrom0str (entry->gr_passwd);
  ve[2] = scm_ulong2num ((unsigned long) entry->gr_gid);
  ve[3] = scm_makfromstrs (-1, entry->gr_mem);
  return result;
}
#undef FUNC_NAME



SCM_DEFINE (scm_setgrent, "setgr", 0, 1, 0, 
            (SCM arg),
"If called with a true argument, initialize or reset the group data
stream.  Otherwise, close the stream.  The @code{setgrent} and
@code{endgrent} procedures are implemented on top of this.")
#define FUNC_NAME s_scm_setgrent
{
  if (SCM_UNBNDP (arg) || SCM_FALSEP (arg))
    endgrent ();
  else
    setgrent ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_kill, "kill", 2, 0, 0,
            (SCM pid, SCM sig),
"Sends a signal to the specified process or group of processes.

@var{pid} specifies the processes to which the signal is sent:

@table @r
@item @var{pid} greater than 0
The process whose identifier is @var{pid}.
@item @var{pid} equal to 0
All processes in the current process group.
@item @var{pid} less than -1
The process group whose identifier is -@var{pid}
@item @var{pid} equal to -1
If the process is privileged, all processes except for some special
system processes.  Otherwise, all processes with the current effective
user ID.
@end table

@var{sig} should be specified using a variable corresponding to
the Unix symbolic name, e.g.,

@defvar SIGHUP
Hang-up signal.
@end defvar

@defvar SIGINT
Interrupt signal.
@end defvar")
#define FUNC_NAME s_scm_kill
{
  SCM_VALIDATE_INUM (1,pid);
  SCM_VALIDATE_INUM (2,sig);
  /* Signal values are interned in scm_init_posix().  */
  if (kill ((int) SCM_INUM (pid), (int) SCM_INUM (sig)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_WAITPID
SCM_DEFINE (scm_waitpid, "waitpid", 1, 1, 0,
            (SCM pid, SCM options),
"This procedure collects status information from a child process which
has terminated or (optionally) stopped.  Normally it will
suspend the calling process until this can be done.  If more than one
child process is eligible then one will be chosen by the operating system.

The value of @var{pid} determines the behaviour:

@table @r
@item @var{pid} greater than 0
Request status information from the specified child process.
@item @var{pid} equal to -1 or WAIT_ANY
Request status information for any child process.
@item @var{pid} equal to 0 or WAIT_MYPGRP
Request status information for any child process in the current process
group.
@item @var{pid} less than -1
Request status information for any child process whose process group ID
is -@var{PID}.
@end table

The @var{options} argument, if supplied, should be the bitwise OR of the
values of zero or more of the following variables:

@defvar WNOHANG
Return immediately even if there are no child processes to be collected.
@end defvar

@defvar WUNTRACED
Report status information for stopped processes as well as terminated
processes.
@end defvar

The return value is a pair containing:

@enumerate
@item
The process ID of the child process, or 0 if @code{WNOHANG} was
specified and no process was collected.
@item
The integer status value.
@end enumerate")
#define FUNC_NAME s_scm_waitpid
{
  int i;
  int status;
  int ioptions;
  SCM_VALIDATE_INUM (1,pid);
  if (SCM_UNBNDP (options))
    ioptions = 0;
  else
    {
      SCM_VALIDATE_INUM (2,options);
      /* Flags are interned in scm_init_posix.  */
      ioptions = SCM_INUM (options);
    }
  SCM_SYSCALL (i = waitpid (SCM_INUM (pid), &status, ioptions));
  if (i == -1)
    SCM_SYSERROR;
  return scm_cons (SCM_MAKINUM (0L + i), SCM_MAKINUM (0L + status));
}
#undef FUNC_NAME
#endif /* HAVE_WAITPID */

SCM_DEFINE (scm_status_exit_val, "status:exit-val", 1, 0, 0, 
            (SCM status),
"Returns the exit status value, as would be
set if a process ended normally through a
call to @code{exit} or @code{_exit}, if any, otherwise @code{#f}.")
#define FUNC_NAME s_scm_status_exit_val
{
  int lstatus;

  SCM_VALIDATE_INUM (1,status);

  /* On Ultrix, the WIF... macros assume their argument is an lvalue;
     go figure.  SCM_INUM does not yield an lvalue.  */
  lstatus = SCM_INUM (status);
  if (WIFEXITED (lstatus))
    return (SCM_MAKINUM (WEXITSTATUS (lstatus)));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_status_term_sig, "status:term-sig", 1, 0, 0, 
            (SCM status),
"Returns the signal number which terminated the
process, if any, otherwise @code{#f}.")
#define FUNC_NAME s_scm_status_term_sig
{
  int lstatus;

  SCM_VALIDATE_INUM (1,status);

  lstatus = SCM_INUM (status);
  if (WIFSIGNALED (lstatus))
    return SCM_MAKINUM (WTERMSIG (lstatus));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_status_stop_sig, "status:stop-sig", 1, 0, 0, 
            (SCM status),
"Returns the signal number which stopped the
process, if any, otherwise @code{#f}.")
#define FUNC_NAME s_scm_status_stop_sig
{
  int lstatus;

  SCM_VALIDATE_INUM (1,status);

  lstatus = SCM_INUM (status);
  if (WIFSTOPPED (lstatus))
    return SCM_MAKINUM (WSTOPSIG (lstatus));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_getppid, "getppid", 0, 0, 0,
            (),
"Returns an integer representing the process ID of the parent process.")
#define FUNC_NAME s_scm_getppid
{
  return SCM_MAKINUM (0L + getppid ());
}
#undef FUNC_NAME



SCM_DEFINE (scm_getuid, "getuid", 0, 0, 0,
            (),
"Returns an integer representing the current real user ID.")
#define FUNC_NAME s_scm_getuid
{
  return SCM_MAKINUM (0L + getuid ());
}
#undef FUNC_NAME



SCM_DEFINE (scm_getgid, "getgid", 0, 0, 0,
            (),
"Returns an integer representing the current real group ID.")
#define FUNC_NAME s_scm_getgid
{
  return SCM_MAKINUM (0L + getgid ());
}
#undef FUNC_NAME



SCM_DEFINE (scm_geteuid, "geteuid", 0, 0, 0,
            (),
"Returns an integer representing the current effective user ID.
If the system does not support effective IDs, then the real ID
is returned.  @code{(feature? 'EIDs)} reports whether the system
supports effective IDs.")
#define FUNC_NAME s_scm_geteuid
{
#ifdef HAVE_GETEUID
  return SCM_MAKINUM (0L + geteuid ());
#else
  return SCM_MAKINUM (0L + getuid ());
#endif
}
#undef FUNC_NAME



SCM_DEFINE (scm_getegid, "getegid", 0, 0, 0,
            (),
"Returns an integer representing the current effective group ID.
If the system does not support effective IDs, then the real ID
is returned.  @code{(feature? 'EIDs)} reports whether the system
supports effective IDs.")
#define FUNC_NAME s_scm_getegid
{
#ifdef HAVE_GETEUID
  return SCM_MAKINUM (0L + getegid ());
#else
  return SCM_MAKINUM (0L + getgid ());
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_setuid, "setuid", 1, 0, 0, 
            (SCM id),
"Sets both the real and effective user IDs to the integer @var{id}, provided
the process has appropriate privileges.
The return value is unspecified.")
#define FUNC_NAME s_scm_setuid
{
  SCM_VALIDATE_INUM (1,id);
  if (setuid (SCM_INUM (id)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_setgid, "setgid", 1, 0, 0, 
            (SCM id),
"Sets both the real and effective group IDs to the integer @var{id}, provided
the process has appropriate privileges.
The return value is unspecified.")
#define FUNC_NAME s_scm_setgid
{
  SCM_VALIDATE_INUM (1,id);
  if (setgid (SCM_INUM (id)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_seteuid, "seteuid", 1, 0, 0, 
            (SCM id),
"Sets the effective user ID to the integer @var{id}, provided the process
has appropriate privileges.  If effective IDs are not supported, the
real ID is set instead -- @code{(feature? 'EIDs)} reports whether the
system supports effective IDs.
The return value is unspecified.")
#define FUNC_NAME s_scm_seteuid
{
  int rv;

  SCM_VALIDATE_INUM (1,id);
#ifdef HAVE_SETEUID
  rv = seteuid (SCM_INUM (id));
#else
  rv = setuid (SCM_INUM (id));
#endif
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_SETEGID
SCM_DEFINE (scm_setegid, "setegid", 1, 0, 0, 
            (SCM id),
"Sets the effective group ID to the integer @var{id}, provided the process
has appropriate privileges.  If effective IDs are not supported, the
real ID is set instead -- @code{(feature? 'EIDs)} reports whether the
system supports effective IDs.
The return value is unspecified.")
#define FUNC_NAME s_scm_setegid
{
  int rv;

  SCM_VALIDATE_INUM (1,id);
#ifdef HAVE_SETEUID
  rv = setegid (SCM_INUM (id));
#else
  rv = setgid (SCM_INUM (id));
#endif
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
    
}
#undef FUNC_NAME
#endif

SCM_DEFINE (scm_getpgrp, "getpgrp", 0, 0, 0,
            (),
"Returns an integer representing the current process group ID.
This is the POSIX definition, not BSD.")
#define FUNC_NAME s_scm_getpgrp
{
  int (*fn)();
  fn = (int (*) ()) getpgrp;
  return SCM_MAKINUM (fn (0));
}
#undef FUNC_NAME

SCM_DEFINE (scm_setpgid, "setpgid", 2, 0, 0, 
            (SCM pid, SCM pgid),
"Move the process @var{pid} into the process group @var{pgid}.  @var{pid} or
@var{pgid} must be integers: they can be zero to indicate the ID of the
current process.
Fails on systems that do not support job control.
The return value is unspecified.")
#define FUNC_NAME s_scm_setpgid
{
#ifdef HAVE_SETPGID
  SCM_VALIDATE_INUM (1,pid);
  SCM_VALIDATE_INUM (2,pgid);
  /* FIXME(?): may be known as setpgrp.  */
  if (setpgid (SCM_INUM (pid), SCM_INUM (pgid)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_setsid, "setsid", 0, 0, 0,
            (),
"Creates a new session.  The current process becomes the session leader
and is put in a new process group.  The process will be detached
from its controlling terminal if it has one.
The return value is an integer representing the new process group ID.")
#define FUNC_NAME s_scm_setsid
{
#ifdef HAVE_SETSID
  pid_t sid = setsid ();
  if (sid == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_ttyname, "ttyname", 1, 0, 0, 
            (SCM port),
"Returns a string with the name of the serial terminal device underlying
@var{port}.")
#define FUNC_NAME s_scm_ttyname
{
  char *ans;
  int fd;

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPPORT (1,port);
  if (scm_tc16_fport != SCM_TYP16 (port))
    return SCM_BOOL_F;
  fd = SCM_FPORT_FDES (port);
  SCM_SYSCALL (ans = ttyname (fd));
  if (!ans)
    SCM_SYSERROR;
  /* ans could be overwritten by another call to ttyname */
  return (scm_makfrom0str (ans));
}
#undef FUNC_NAME


SCM_DEFINE (scm_ctermid, "ctermid", 0, 0, 0,
            (),
"Returns a string containing the file name of the controlling terminal
for the current process.")
#define FUNC_NAME s_scm_ctermid
{
#ifdef HAVE_CTERMID
  char *result = ctermid (NULL);
  if (*result == '\0')
    SCM_SYSERROR;
  return scm_makfrom0str (result);
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_tcgetpgrp, "tcgetpgrp", 1, 0, 0, 
            (SCM port),
"Returns the process group ID of the foreground
process group associated with the terminal open on the file descriptor
underlying @var{port}.

If there is no foreground process group, the return value is a
number greater than 1 that does not match the process group ID
of any existing process group.  This can happen if all of the
processes in the job that was formerly the foreground job have
terminated, and no other job has yet been moved into the
foreground.")
#define FUNC_NAME s_scm_tcgetpgrp
{
#ifdef HAVE_TCGETPGRP
  int fd;
  pid_t pgid;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPFPORT (1,port);
  fd = SCM_FPORT_FDES (port);
  if ((pgid = tcgetpgrp (fd)) == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (pgid);
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME    

SCM_DEFINE (scm_tcsetpgrp, "tcsetpgrp", 2, 0, 0,
            (SCM port, SCM pgid),
"Set the foreground process group ID for the terminal used by the file
descriptor underlying @var{port} to the integer @var{pgid}.
The calling process
must be a member of the same session as @var{pgid} and must have the same
controlling terminal.  The return value is unspecified.")
#define FUNC_NAME s_scm_tcsetpgrp
{
#ifdef HAVE_TCSETPGRP
  int fd;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPFPORT (1,port);
  SCM_VALIDATE_INUM (2,pgid);
  fd = SCM_FPORT_FDES (port);
  if (tcsetpgrp (fd, SCM_INUM (pgid)) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME
   

/* Copy exec args from an SCM vector into a new C array.  */

static char **
scm_convert_exec_args (SCM args, int pos, const char *subr)
{
  char **execargv;
  int num_args;
  int i;

  SCM_ASSERT (SCM_NULLP (args)
	      || (SCM_CONSP (args)),
	      args, pos, subr);
  num_args = scm_ilength (args);
  execargv = (char **) 
    scm_must_malloc ((num_args + 1) * sizeof (char *), subr);
  for (i = 0; SCM_NNULLP (args); args = SCM_CDR (args), ++i)
    {
      scm_sizet len;
      char *dst;
      char *src;
      SCM_ASSERT (SCM_ROSTRINGP (SCM_CAR (args)),
		  SCM_CAR (args), SCM_ARGn, subr);
      len = 1 + SCM_ROLENGTH (SCM_CAR (args));
      dst = (char *) scm_must_malloc ((long) len, subr);
      src = SCM_ROCHARS (SCM_CAR (args));
      while (len--) 
	dst[len] = src[len];
      execargv[i] = dst;
    }
  execargv[i] = 0;
  return execargv;
}

SCM_DEFINE (scm_execl, "execl", 1, 0, 1, 
            (SCM filename, SCM args),
"Executes the file named by @var{path} as a new process image.
The remaining arguments are supplied to the process; from a C program
they are accessable as the @code{argv} argument to @code{main}.
Conventionally the first @var{arg} is the same as @var{path}.
All arguments must be strings.  

If @var{arg} is missing, @var{path} is executed with a null
argument list, which may have system-dependent side-effects.

This procedure is currently implemented using the @code{execv} system
call, but we call it @code{execl} because of its Scheme calling interface.")
#define FUNC_NAME s_scm_execl
{
  char **execargv;
  SCM_VALIDATE_ROSTRING (1,filename);
  SCM_COERCE_SUBSTR (filename);
  execargv = scm_convert_exec_args (args, SCM_ARG2, FUNC_NAME);
  execv (SCM_ROCHARS (filename), execargv);
  SCM_SYSERROR;
  /* not reached.  */
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_execlp, "execlp", 1, 0, 1, 
            (SCM filename, SCM args),
"Similar to @code{execl}, however if
@var{filename} does not contain a slash
then the file to execute will be located by searching the
directories listed in the @code{PATH} environment variable.

This procedure is currently implemented using the @code{execlv} system
call, but we call it @code{execlp} because of its Scheme calling interface.")
#define FUNC_NAME s_scm_execlp
{
  char **execargv;
  SCM_VALIDATE_ROSTRING (1,filename);
  SCM_COERCE_SUBSTR (filename);
  execargv = scm_convert_exec_args (args, SCM_ARG2, FUNC_NAME);
  execvp (SCM_ROCHARS (filename), execargv);
  SCM_SYSERROR;
  /* not reached.  */
  return SCM_BOOL_F;
}
#undef FUNC_NAME

static char **
environ_list_to_c (SCM envlist, int arg, const char *proc)
{
  int num_strings;
  char **result;
  int i = 0;

  SCM_ASSERT (SCM_NULLP (envlist) || SCM_CONSP (envlist),
	      envlist, arg, proc);
  num_strings = scm_ilength (envlist);
  result = (char **) malloc ((num_strings + 1) * sizeof (char *));
  if (result == NULL)
    scm_memory_error (proc);
  while (SCM_NNULLP (envlist))
    {
      int len;
      char *src;

      SCM_ASSERT (SCM_ROSTRINGP (SCM_CAR (envlist)),
		  envlist, arg, proc);
      len = 1 + SCM_ROLENGTH (SCM_CAR (envlist));
      result[i] = malloc ((long) len);
      if (result[i] == NULL)
	scm_memory_error (proc);
      src = SCM_ROCHARS (SCM_CAR (envlist));
      while (len--) 
	result[i][len] = src[len];
      envlist = SCM_CDR (envlist);
      i++;
    }
  result[i] = 0;
  return result;
}

SCM_DEFINE (scm_execle, "execle", 2, 0, 1, 
            (SCM filename, SCM env, SCM args),
"Similar to @code{execl}, but the environment of the new process is
specified by @var{env}, which must be a list of strings as returned by the
@code{environ} procedure.

This procedure is currently implemented using the @code{execve} system
call, but we call it @code{execle} because of its Scheme calling interface.")
#define FUNC_NAME s_scm_execle
{
  char **execargv;
  char **exec_env;

  SCM_VALIDATE_ROSTRING (1,filename);
  SCM_COERCE_SUBSTR (filename);
  
  execargv = scm_convert_exec_args (args, SCM_ARG1, FUNC_NAME);
  exec_env = environ_list_to_c (env, SCM_ARG2, FUNC_NAME);
  execve (SCM_ROCHARS (filename), execargv, exec_env);
  SCM_SYSERROR;
  /* not reached.  */
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fork, "primitive-fork", 0, 0, 0,
            (),
"Creates a new \"child\" process by duplicating the current \"parent\" process.
In the child the return value is 0.  In the parent the return value is
the integer process ID of the child.

This procedure has been renamed from @code{fork} to avoid a naming conflict
with the scsh fork.")
#define FUNC_NAME s_scm_fork
{
  int pid;
  pid = fork ();
  if (pid == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (0L+pid);
}
#undef FUNC_NAME


SCM_DEFINE (scm_uname, "uname", 0, 0, 0,
            (),
"Returns an object with some information about the computer system the
program is running on.")
#define FUNC_NAME s_scm_uname
{
#ifdef HAVE_UNAME
  struct utsname buf;
  SCM ans = scm_make_vector (SCM_MAKINUM(5), SCM_UNSPECIFIED);
  SCM *ve = SCM_VELTS (ans);
  if (uname (&buf) < 0)
    SCM_SYSERROR;
  ve[0] = scm_makfrom0str (buf.sysname);
  ve[1] = scm_makfrom0str (buf.nodename);
  ve[2] = scm_makfrom0str (buf.release);
  ve[3] = scm_makfrom0str (buf.version);
  ve[4] = scm_makfrom0str (buf.machine);
/* 
   a linux special?
  ve[5] = scm_makfrom0str (buf.domainname);
*/
  return ans;
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_environ, "environ", 0, 1, 0, 
            (SCM env),
"If @var{env} is omitted, returns the current environment as a list of strings.
Otherwise it sets the current environment, which is also the
default environment for child processes, to the supplied list of strings.
Each member of @var{env} should be of the form
@code{NAME=VALUE} and values of @code{NAME} should not be duplicated.
If @var{env} is supplied then the return value is unspecified.")
#define FUNC_NAME s_scm_environ
{
  if (SCM_UNBNDP (env))
    return scm_makfromstrs (-1, environ);
  else
    {
      char **new_environ;

      new_environ = environ_list_to_c (env, SCM_ARG1, FUNC_NAME);
      /* Free the old environment, except when called for the first
       * time.
       */
      {
	char **ep;
	static int first = 1;
	if (!first)
	  {
	    for (ep = environ; *ep != NULL; ep++)
	      free (*ep);
	    free ((char *) environ);
	  }
	first = 0;
      }
      environ = new_environ;
      return SCM_UNSPECIFIED;
    }
}
#undef FUNC_NAME

#ifdef L_tmpnam

SCM_DEFINE (scm_tmpnam, "tmpnam", 0, 0, 0,
            (),
"Create a new file in the file system with a unique name.  The return
value is the name of the new file.  This function is implemented with
the @code{tmpnam} function in the system libraries.")
#define FUNC_NAME s_scm_tmpnam
{
  char name[L_tmpnam];
  SCM_SYSCALL (tmpnam (name););
  return scm_makfrom0str (name);
}
#undef FUNC_NAME;

#endif

SCM_DEFINE (scm_utime, "utime", 1, 2, 0,
            (SCM pathname, SCM actime, SCM modtime),
"@code{utime} sets the access and modification times for
the file named by @var{path}.  If @var{actime} or @var{modtime}
is not supplied, then the current time is used.
@var{actime} and @var{modtime}
must be integer time values as returned by the @code{current-time}
procedure.

E.g.,

@smalllisp
(utime \"foo\" (- (current-time) 3600))
@end smalllisp

will set the access time to one hour in the past and the modification
time to the current time.")
#define FUNC_NAME s_scm_utime
{
  int rv;
  struct utimbuf utm_tmp;

  SCM_VALIDATE_ROSTRING (1,pathname);
  SCM_COERCE_SUBSTR (pathname);
  if (SCM_UNBNDP (actime))
    SCM_SYSCALL (time (&utm_tmp.actime));
  else
    utm_tmp.actime = SCM_NUM2ULONG (2,actime);

  if (SCM_UNBNDP (modtime))
    SCM_SYSCALL (time (&utm_tmp.modtime));
  else
    utm_tmp.modtime = SCM_NUM2ULONG (3,modtime);

  SCM_SYSCALL (rv = utime (SCM_ROCHARS (pathname), &utm_tmp));
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_access, "access?", 2, 0, 0,
            (SCM path, SCM how),
"Returns @code{#t} if @var{path} corresponds to an existing
file and the current process
has the type of access specified by @var{how}, otherwise 
@code{#f}.
@var{how} should be specified
using the values of the variables listed below.  Multiple values can
be combined using a bitwise or, in which case @code{#t} will only
be returned if all accesses are granted.

Permissions are checked using the real id of the current process,
not the effective id, although it's the effective id which determines
whether the access would actually be granted.

@defvar R_OK
test for read permission.
@end defvar
@defvar W_OK
test for write permission.
@end defvar
@defvar X_OK
test for execute permission.
@end defvar
@defvar F_OK
test for existence of the file.
@end defvar")
#define FUNC_NAME s_scm_access
{
  int rv;

  SCM_VALIDATE_ROSTRING (1,path);
  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);
  SCM_VALIDATE_INUM (2,how);
  rv = access (SCM_ROCHARS (path), SCM_INUM (how));
  return SCM_NEGATE_BOOL(rv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_getpid, "getpid", 0, 0, 0,
            (),
"Returns an integer representing the current process ID.")
#define FUNC_NAME s_scm_getpid
{
  return SCM_MAKINUM ((unsigned long) getpid ());
}
#undef FUNC_NAME

SCM_DEFINE (scm_putenv, "putenv", 1, 0, 0, 
            (SCM str),
"Modifies the environment of the current process, which is
also the default environment inherited by child processes.

If @var{string} is of the form @code{NAME=VALUE} then it will be written
directly into the environment, replacing any existing environment string
with
name matching @code{NAME}.  If @var{string} does not contain an equal
sign, then any existing string with name matching @var{string} will
be removed.

The return value is unspecified.")
#define FUNC_NAME s_scm_putenv
{
  int rv;
  char *ptr;

  SCM_VALIDATE_ROSTRING (1,str);
  /* must make a new copy to be left in the environment, safe from gc.  */
  ptr = malloc (SCM_LENGTH (str) + 1);
  if (ptr == NULL)
    SCM_MEMORY_ERROR;
  strncpy (ptr, SCM_ROCHARS (str), SCM_LENGTH (str));
  ptr[SCM_LENGTH(str)] = 0;
  rv = putenv (ptr);
  if (rv < 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_setlocale, "setlocale", 1, 1, 0,
            (SCM category, SCM locale),
"If @var{locale} is omitted, returns the current value of the specified
locale category 
as a system-dependent string.
@var{category} should be specified using the values @code{LC_COLLATE},
@code{LC_ALL} etc.

Otherwise the specified locale category is set to
the string @var{locale}
and the new value is returned as a system-dependent string.  If @var{locale}
is an empty string, the locale will be set using envirionment variables.")
#define FUNC_NAME s_scm_setlocale
{
#ifdef HAVE_SETLOCALE
  char *clocale;
  char *rv;

  SCM_VALIDATE_INUM (1,category);
  if (SCM_UNBNDP (locale))
    {
      clocale = NULL;
    }
  else
    {
      SCM_VALIDATE_ROSTRING (2,locale);
      SCM_COERCE_SUBSTR (locale);
      clocale = SCM_ROCHARS (locale);
    }

  rv = setlocale (SCM_INUM (category), clocale);
  if (rv == NULL)
    SCM_SYSERROR;
  return scm_makfrom0str (rv);
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_mknod, "mknod", 4, 0, 0,
            (SCM path, SCM type, SCM perms, SCM dev),
"Creates a new special file, such as a file corresponding to a device.
@var{path} specifies the name of the file.  @var{type} should
be one of the following symbols:
regular, directory, symlink, block-special, char-special,
fifo, or socket.  @var{perms} (an integer) specifies the file permissions.
@var{dev} (an integer) specifies which device the special file refers
to.  Its exact interpretation depends on the kind of special file
being created.

E.g.,
@example
(mknod "/dev/fd0" 'block-special #o660 (+ (* 2 256) 2))
@end example

The return value is unspecified.")
#define FUNC_NAME s_scm_mknod
{
#ifdef HAVE_MKNOD
  int val;
  char *p;
  int ctype = 0;

  SCM_VALIDATE_ROSTRING (1,path);
  SCM_VALIDATE_SYMBOL (2,type);
  SCM_VALIDATE_INUM (3,perms);
  SCM_VALIDATE_INUM (4,dev);
  SCM_COERCE_SUBSTR (path);

  p = SCM_CHARS (type);
  if (strcmp (p, "regular") == 0)
    ctype = S_IFREG;
  else if (strcmp (p, "directory") == 0)
    ctype = S_IFDIR;
  else if (strcmp (p, "symlink") == 0)
    ctype = S_IFLNK;
  else if (strcmp (p, "block-special") == 0)
    ctype = S_IFBLK;
  else if (strcmp (p, "char-special") == 0)
    ctype = S_IFCHR;
  else if (strcmp (p, "fifo") == 0)
    ctype = S_IFIFO;
  else if (strcmp (p, "socket") == 0)
    ctype = S_IFSOCK;
  else
    SCM_OUT_OF_RANGE (2,type);

  SCM_SYSCALL (val = mknod(SCM_ROCHARS(path), ctype | SCM_INUM (perms),
			   SCM_INUM (dev)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_nice, "nice", 1, 0, 0, 
            (SCM incr),
"Increment the priority of the current process by @var{incr}.  A higher
priority value means that the process runs less often.
The return value is unspecified.")
#define FUNC_NAME s_scm_nice
{
#ifdef HAVE_NICE
  SCM_VALIDATE_INUM (1,incr);
  if (nice(SCM_INUM(incr)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
#else
  SCM_SYSMISSING;
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_sync, "sync", 0, 0, 0,
            (),
"Flush the operating system disk buffers.
The return value is unspecified.")
#define FUNC_NAME s_scm_sync
{
#ifdef HAVE_SYNC
  sync();
#else
  SCM_SYSMISSING;
  /* not reached.  */
#endif
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void 
scm_init_posix ()
{
  scm_add_feature ("posix");
#ifdef HAVE_GETEUID
  scm_add_feature ("EIDs");
#endif
#ifdef WAIT_ANY
  scm_sysintern ("WAIT_ANY", SCM_MAKINUM (WAIT_ANY));
#endif
#ifdef WAIT_MYPGRP
  scm_sysintern ("WAIT_MYPGRP", SCM_MAKINUM (WAIT_MYPGRP));
#endif
#ifdef WNOHANG
  scm_sysintern ("WNOHANG", SCM_MAKINUM (WNOHANG));
#endif
#ifdef WUNTRACED
  scm_sysintern ("WUNTRACED", SCM_MAKINUM (WUNTRACED));
#endif

  /* access() symbols.  */
  scm_sysintern ("R_OK", SCM_MAKINUM (R_OK));
  scm_sysintern ("W_OK", SCM_MAKINUM (W_OK));
  scm_sysintern ("X_OK", SCM_MAKINUM (X_OK));
  scm_sysintern ("F_OK", SCM_MAKINUM (F_OK));

#ifdef LC_COLLATE
  scm_sysintern ("LC_COLLATE", SCM_MAKINUM (LC_COLLATE));
#endif
#ifdef LC_CTYPE
  scm_sysintern ("LC_CTYPE", SCM_MAKINUM (LC_CTYPE));
#endif
#ifdef LC_MONETARY
  scm_sysintern ("LC_MONETARY", SCM_MAKINUM (LC_MONETARY));
#endif
#ifdef LC_NUMERIC
  scm_sysintern ("LC_NUMERIC", SCM_MAKINUM (LC_NUMERIC));
#endif
#ifdef LC_TIME
  scm_sysintern ("LC_TIME", SCM_MAKINUM (LC_TIME));
#endif
#ifdef LC_MESSAGES
  scm_sysintern ("LC_MESSAGES", SCM_MAKINUM (LC_MESSAGES));
#endif
#ifdef LC_ALL
  scm_sysintern ("LC_ALL", SCM_MAKINUM (LC_ALL));
#endif
#include "cpp_sig_symbols.c"
#include "posix.x"
}
