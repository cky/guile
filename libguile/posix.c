/*	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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
#include "_scm.h"
#include "fports.h"
#include "scmsigs.h"
#include "feature.h"

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




SCM_PROC (s_pipe, "pipe", 0, 0, 0, scm_pipe);

SCM 
scm_pipe ()
{
  int fd[2], rv;
  FILE *f_rd, *f_wt;
  SCM p_rd, p_wt;
  struct scm_port_table * ptr;
  struct scm_port_table * ptw;

  SCM_NEWCELL (p_rd);
  SCM_NEWCELL (p_wt);
  rv = pipe (fd);
  if (rv)
    scm_syserror (s_pipe);
  f_rd = fdopen (fd[0], "r");
  if (!f_rd)
    {
      SCM_SYSCALL (close (fd[0]));
      SCM_SYSCALL (close (fd[1]));
      scm_syserror (s_pipe);
    }
  f_wt = fdopen (fd[1], "w");
  if (!f_wt)
    {
      int en;
      en = errno;
      fclose (f_rd);
      SCM_SYSCALL (close (fd[1]));
      errno = en;
      scm_syserror (s_pipe);
    }
  ptr = scm_add_to_port_table (p_rd);
  ptw = scm_add_to_port_table (p_wt);
  SCM_SETPTAB_ENTRY (p_rd, ptr);
  SCM_SETPTAB_ENTRY (p_wt, ptw);
  SCM_SETCAR (p_rd, scm_tc16_fport | scm_mode_bits ("r"));
  SCM_SETCAR (p_wt, scm_tc16_fport | scm_mode_bits ("w"));
  SCM_SETSTREAM (p_rd, (SCM)f_rd);
  SCM_SETSTREAM (p_wt, (SCM)f_wt);

  SCM_ALLOW_INTS;
  return scm_cons (p_rd, p_wt);
}


#ifdef HAVE_GETGROUPS
SCM_PROC (s_getgroups, "getgroups", 0, 0, 0, scm_getgroups);

SCM
scm_getgroups()
{
  SCM grps, ans;
  int ngroups = getgroups (0, NULL);
  if (!ngroups)
    scm_syserror (s_getgroups);
  SCM_NEWCELL(grps);
  SCM_DEFER_INTS;
  {
    GETGROUPS_T *groups;
    int val;

    groups = (GETGROUPS_T *) scm_must_malloc(ngroups * sizeof(GETGROUPS_T),
					     s_getgroups);
    val = getgroups(ngroups, groups);
    if (val < 0)
      {
	scm_must_free((char *)groups);
	scm_syserror (s_getgroups);
      }
    SCM_SETCHARS(grps, groups);	/* set up grps as a GC protect */
    SCM_SETLENGTH(grps, 0L + ngroups * sizeof(GETGROUPS_T), scm_tc7_string);
    SCM_ALLOW_INTS;
    ans = scm_make_vector(SCM_MAKINUM(ngroups), SCM_UNDEFINED, SCM_BOOL_F);
    while (--ngroups >= 0) SCM_VELTS(ans)[ngroups] = SCM_MAKINUM(groups[ngroups]);
    SCM_SETCHARS(grps, groups);	/* to make sure grps stays around. */
    return ans;
  }
}  
#endif


SCM_PROC (s_getpwuid, "getpw", 0, 1, 0, scm_getpwuid);

SCM 
scm_getpwuid (user)
     SCM user;
{
  SCM result;
  struct passwd *entry;
  SCM *ve;

  result = scm_make_vector (SCM_MAKINUM (7), SCM_UNSPECIFIED, SCM_BOOL_F);
  ve = SCM_VELTS (result);
  if (SCM_UNBNDP (user) || SCM_FALSEP (user))
    {
      SCM_DEFER_INTS;
      SCM_SYSCALL (entry = getpwent ());
      if (! entry)
	{
	  SCM_ALLOW_INTS;
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_INUMP (user))
    {
      SCM_DEFER_INTS;
      entry = getpwuid (SCM_INUM (user));
    }
  else
    {
      SCM_ASSERT (SCM_NIMP (user) && SCM_ROSTRINGP (user), user, SCM_ARG1, s_getpwuid);
      if (SCM_SUBSTRP (user))
	user = scm_makfromstr (SCM_ROCHARS (user), SCM_ROLENGTH (user), 0);
      SCM_DEFER_INTS;
      entry = getpwnam (SCM_ROCHARS (user));
    }
  if (!entry)
    scm_syserror (s_getpwuid);

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
  SCM_ALLOW_INTS;
  return result;
}


#ifdef HAVE_SETPWENT
SCM_PROC (s_setpwent, "setpw", 0, 1, 0, scm_setpwent);

SCM 
scm_setpwent (arg)
     SCM arg;
{
  if (SCM_UNBNDP (arg) || SCM_FALSEP (arg))
    endpwent ();
  else
    setpwent ();
  return SCM_UNSPECIFIED;
}
#endif



/* Combines getgrgid and getgrnam.  */
SCM_PROC (s_getgrgid, "getgr", 0, 1, 0, scm_getgrgid);

SCM 
scm_getgrgid (name)
     SCM name;
{
  SCM result;
  struct group *entry;
  SCM *ve;
  result = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED, SCM_BOOL_F);
  ve = SCM_VELTS (result);
  SCM_DEFER_INTS;
  if (SCM_UNBNDP (name) || (name == SCM_BOOL_F))
    {
      SCM_SYSCALL (entry = getgrent ());
      if (! entry)
	{
	  SCM_ALLOW_INTS;
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_INUMP (name))
    SCM_SYSCALL (entry = getgrgid (SCM_INUM (name)));
  else
    {
      SCM_ASSERT (SCM_NIMP (name) && SCM_ROSTRINGP (name), name, SCM_ARG1,
		  s_getgrgid);
      SCM_COERCE_SUBSTR (name);
      SCM_SYSCALL (entry = getgrnam (SCM_ROCHARS (name)));
    }
  if (!entry)
    scm_syserror (s_getgrgid);

  ve[0] = scm_makfrom0str (entry->gr_name);
  ve[1] = scm_makfrom0str (entry->gr_passwd);
  ve[2] = scm_ulong2num ((unsigned long) entry->gr_gid);
  ve[3] = scm_makfromstrs (-1, entry->gr_mem);
  SCM_ALLOW_INTS;
  return result;
}



SCM_PROC (s_setgrent, "setgr", 0, 1, 0, scm_setgrent);

SCM 
scm_setgrent (arg)
     SCM arg;
{
  if (SCM_UNBNDP (arg) || SCM_FALSEP (arg))
    endgrent ();
  else
    setgrent ();
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_kill, "kill", 2, 0, 0, scm_kill);

SCM 
scm_kill (pid, sig)
     SCM pid;
     SCM sig;
{
  SCM_ASSERT (SCM_INUMP (pid), pid, SCM_ARG1, s_kill);
  SCM_ASSERT (SCM_INUMP (sig), sig, SCM_ARG2, s_kill);
  /* Signal values are interned in scm_init_posix().  */
  if (kill ((int) SCM_INUM (pid), (int) SCM_INUM (sig)) != 0)
    scm_syserror (s_kill);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_waitpid, "waitpid", 1, 1, 0, scm_waitpid);

SCM 
scm_waitpid (pid, options)
     SCM pid;
     SCM options;
{
#ifdef HAVE_WAITPID
  int i;
  int status;
  int ioptions;
  SCM_ASSERT (SCM_INUMP (pid), pid, SCM_ARG1, s_waitpid);
  if (SCM_UNBNDP (options))
    ioptions = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (options), options, SCM_ARG2, s_waitpid);
      /* Flags are interned in scm_init_posix.  */
      ioptions = SCM_INUM (options);
    }
  SCM_SYSCALL (i = waitpid (SCM_INUM (pid), &status, ioptions));
  if (i == -1)
    scm_syserror (s_waitpid);
  return scm_cons (SCM_MAKINUM (0L + i), SCM_MAKINUM (0L + status));
#else
  scm_sysmissing (s_waitpid);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}

SCM_PROC (s_status_exit_val, "status:exit-val", 1, 0, 0, scm_status_exit_val);
SCM
scm_status_exit_val (status)
     SCM status;
{
  int lstatus;

  SCM_ASSERT (SCM_INUMP (status), status, SCM_ARG1,s_status_exit_val);

  /* On Ultrix, the WIF... macros assume their argument is an lvalue;
     go figure.  SCM_INUM does not yield an lvalue.  */
  lstatus = SCM_INUM (status);
  if (WIFEXITED (lstatus))
    return (SCM_MAKINUM (WEXITSTATUS (lstatus)));
  else
    return SCM_BOOL_F;
}

SCM_PROC (s_status_term_sig, "status:term-sig", 1, 0, 0, scm_status_term_sig);
SCM
scm_status_term_sig (status)
     SCM status;
{
  int lstatus;

  SCM_ASSERT (SCM_INUMP (status), status, SCM_ARG1,s_status_term_sig);

  lstatus = SCM_INUM (status);
  if (WIFSIGNALED (lstatus))
    return SCM_MAKINUM (WTERMSIG (lstatus));
  else
    return SCM_BOOL_F;
}

SCM_PROC (s_status_stop_sig, "status:stop-sig", 1, 0, 0, scm_status_stop_sig);
SCM
scm_status_stop_sig (status)
     SCM status;
{
  int lstatus;

  SCM_ASSERT (SCM_INUMP (status), status, SCM_ARG1,s_status_stop_sig);

  lstatus = SCM_INUM (status);
  if (WIFSTOPPED (lstatus))
    return SCM_MAKINUM (WSTOPSIG (lstatus));
  else
    return SCM_BOOL_F;
}

SCM_PROC (s_getppid, "getppid", 0, 0, 0, scm_getppid);

SCM 
scm_getppid ()
{
  return SCM_MAKINUM (0L + getppid ());
}



SCM_PROC (s_getuid, "getuid", 0, 0, 0, scm_getuid);

SCM 
scm_getuid ()
{
  return SCM_MAKINUM (0L + getuid ());
}



SCM_PROC (s_getgid, "getgid", 0, 0, 0, scm_getgid);

SCM 
scm_getgid ()
{
  return SCM_MAKINUM (0L + getgid ());
}



SCM_PROC (s_geteuid, "geteuid", 0, 0, 0, scm_geteuid);

SCM 
scm_geteuid ()
{
#ifdef HAVE_GETEUID
  return SCM_MAKINUM (0L + geteuid ());
#else
  return SCM_MAKINUM (0L + getuid ());
#endif
}



SCM_PROC (s_getegid, "getegid", 0, 0, 0, scm_getegid);

SCM 
scm_getegid ()
{
#ifdef HAVE_GETEUID
  return SCM_MAKINUM (0L + getegid ());
#else
  return SCM_MAKINUM (0L + getgid ());
#endif
}


SCM_PROC (s_setuid, "setuid", 1, 0, 0, scm_setuid);

SCM 
scm_setuid (id)
     SCM id;
{
  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_setuid);
  if (setuid (SCM_INUM (id)) != 0)
    scm_syserror (s_setuid);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_setgid, "setgid", 1, 0, 0, scm_setgid);

SCM 
scm_setgid (id)
     SCM id;
{
  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_setgid);
  if (setgid (SCM_INUM (id)) != 0)
    scm_syserror (s_setgid);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_seteuid, "seteuid", 1, 0, 0, scm_seteuid);

SCM 
scm_seteuid (id)
     SCM id;
{
  int rv;

  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_seteuid);
#ifdef HAVE_SETEUID
  rv = seteuid (SCM_INUM (id));
#else
  rv = setuid (SCM_INUM (id));
#endif
  if (rv != 0)
    scm_syserror (s_seteuid);
  return SCM_UNSPECIFIED;
}

#ifdef HAVE_SETEGID
SCM_PROC (s_setegid, "setegid", 1, 0, 0, scm_setegid);

SCM 
scm_setegid (id)
     SCM id;
{
  int rv;

  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_setegid);
#ifdef HAVE_SETEUID
  rv = setegid (SCM_INUM (id));
#else
  rv = setgid (SCM_INUM (id));
#endif
  if (rv != 0)
    scm_syserror (s_setegid);
  return SCM_UNSPECIFIED;
    
}
#endif

SCM_PROC (s_getpgrp, "getpgrp", 0, 0, 0, scm_getpgrp);
SCM 
scm_getpgrp ()
{
  int (*fn)();
  fn = (int (*) ()) getpgrp;
  return SCM_MAKINUM (fn (0));
}

SCM_PROC (s_setpgid, "setpgid", 2, 0, 0, scm_setpgid);
SCM 
scm_setpgid (pid, pgid)
     SCM pid, pgid;
{
#ifdef HAVE_SETPGID
  SCM_ASSERT (SCM_INUMP (pid), pid, SCM_ARG1, s_setpgid);
  SCM_ASSERT (SCM_INUMP (pgid), pgid, SCM_ARG2, s_setpgid);
  /* FIXME(?): may be known as setpgrp.  */
  if (setpgid (SCM_INUM (pid), SCM_INUM (pgid)) != 0)
    scm_syserror (s_setpgid);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_setpgid);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}

SCM_PROC (s_setsid, "setsid", 0, 0, 0, scm_setsid);
SCM 
scm_setsid ()
{
#ifdef HAVE_SETSID
  pid_t sid = setsid ();
  if (sid == -1)
    scm_syserror (s_setsid);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_setsid);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}

SCM_PROC (s_ttyname, "ttyname", 1, 0, 0, scm_ttyname);

SCM 
scm_ttyname (port)
     SCM port;
{
  char *ans;
  int fd;

  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPPORTP (port), port, SCM_ARG1, s_ttyname);
  if (scm_tc16_fport != SCM_TYP16 (port))
    return SCM_BOOL_F;
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1)
    scm_syserror (s_ttyname);
  SCM_SYSCALL (ans = ttyname (fd));
  if (!ans)
    scm_syserror (s_ttyname);
  /* ans could be overwritten by another call to ttyname */
  return (scm_makfrom0str (ans));
}


SCM_PROC (s_ctermid, "ctermid", 0, 0, 0, scm_ctermid);
SCM 
scm_ctermid ()
{
#ifdef HAVE_CTERMID
  char *result = ctermid (NULL);
  if (*result == '\0')
    scm_syserror (s_ctermid);
  return scm_makfrom0str (result);
#else
  scm_sysmissing (s_ctermid);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}

SCM_PROC (s_tcgetpgrp, "tcgetpgrp", 1, 0, 0, scm_tcgetpgrp);
SCM 
scm_tcgetpgrp (port)
     SCM port;
{
#ifdef HAVE_TCGETPGRP
  int fd;
  pid_t pgid;

  port = SCM_COERCE_OUTPORT (port);

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_tcgetpgrp);
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1 || (pgid = tcgetpgrp (fd)) == -1)
    scm_syserror (s_tcgetpgrp);
  return SCM_MAKINUM (pgid);
#else
  scm_sysmissing (s_tcgetpgrp);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}    

SCM_PROC (s_tcsetpgrp, "tcsetpgrp", 2, 0, 0, scm_tcsetpgrp);
SCM 
scm_tcsetpgrp (port, pgid)
     SCM port, pgid;
{
#ifdef HAVE_TCSETPGRP
  int fd;

  port = SCM_COERCE_OUTPORT (port);

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_tcsetpgrp);
  SCM_ASSERT (SCM_INUMP (pgid), pgid, SCM_ARG2, s_tcsetpgrp);
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1 || tcsetpgrp (fd, SCM_INUM (pgid)) == -1)
    scm_syserror (s_tcsetpgrp);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_tcsetpgrp);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}    

/* Copy exec args from an SCM vector into a new C array.  */

static char **
scm_convert_exec_args (SCM args, int pos, char *subr)
{
  char **execargv;
  int num_args;
  int i;

  SCM_ASSERT (SCM_NULLP (args)
	      || (SCM_NIMP (args) && SCM_CONSP (args)),
	      args, pos, subr);
  SCM_DEFER_INTS;
  num_args = scm_ilength (args);
  execargv = (char **) 
    scm_must_malloc ((num_args + 1) * sizeof (char *), subr);
  for (i = 0; SCM_NNULLP (args); args = SCM_CDR (args), ++i)
    {
      scm_sizet len;
      char *dst;
      char *src;
      SCM_ASSERT (SCM_NIMP (SCM_CAR (args)) && SCM_ROSTRINGP (SCM_CAR (args)),
		  SCM_CAR (args), SCM_ARGn, subr);
      len = 1 + SCM_ROLENGTH (SCM_CAR (args));
      dst = (char *) scm_must_malloc ((long) len, subr);
      src = SCM_ROCHARS (SCM_CAR (args));
      while (len--) 
	dst[len] = src[len];
      execargv[i] = dst;
    }
  execargv[i] = 0;
  SCM_ALLOW_INTS;
  return execargv;
}

SCM_PROC (s_execl, "execl", 1, 0, 1, scm_execl);

SCM
scm_execl (filename, args)
     SCM filename, args;
{
  char **execargv;
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_execl);
  SCM_COERCE_SUBSTR (filename);
  execargv = scm_convert_exec_args (args, SCM_ARG2, s_execl);
  execv (SCM_ROCHARS (filename), execargv);
  scm_syserror (s_execl);
  /* not reached.  */
  return SCM_BOOL_F;
}

SCM_PROC (s_execlp, "execlp", 1, 0, 1, scm_execlp);

SCM
scm_execlp (filename, args)
     SCM filename, args;
{
  char **execargv;
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_execlp);
  SCM_COERCE_SUBSTR (filename);
  execargv = scm_convert_exec_args (args, SCM_ARG2, s_execlp);
  execvp (SCM_ROCHARS (filename), execargv);
  scm_syserror (s_execlp);
  /* not reached.  */
  return SCM_BOOL_F;
}

static char **
environ_list_to_c (SCM envlist, int arg, char *proc)
{
  int num_strings;
  char **result;
  int i = 0;

  SCM_REDEFER_INTS;
  SCM_ASSERT (SCM_NULLP (envlist)
	      || (SCM_NIMP (envlist) && SCM_CONSP (envlist)),
	      envlist, arg, proc);
  num_strings = scm_ilength (envlist);
  result = (char **) malloc ((num_strings + 1) * sizeof (char *));
  if (result == NULL)
    scm_memory_error (proc);
  while (SCM_NNULLP (envlist))
    {
      int len;
      char *src;

      SCM_ASSERT (SCM_NIMP (SCM_CAR (envlist))
		  && SCM_ROSTRINGP (SCM_CAR (envlist)),
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
  SCM_REALLOW_INTS;
  return result;
}

SCM_PROC (s_execle, "execle", 2, 0, 1, scm_execle);

SCM
scm_execle (filename, env, args)
     SCM filename, env, args;
{
  char **execargv;
  char **exec_env;

  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_execle);
  SCM_COERCE_SUBSTR (filename);
  
  execargv = scm_convert_exec_args (args, SCM_ARG1, s_execle);
  exec_env = environ_list_to_c (env, SCM_ARG2, s_execle);
  execve (SCM_ROCHARS (filename), execargv, exec_env);
  scm_syserror (s_execle);
  /* not reached.  */
  return SCM_BOOL_F;
}

SCM_PROC (s_fork, "primitive-fork", 0, 0, 0, scm_fork);

SCM
scm_fork()
{
  int pid;
  pid = fork ();
  if (pid == -1)
    scm_syserror (s_fork);
  return SCM_MAKINUM (0L+pid);
}


SCM_PROC (s_uname, "uname", 0, 0, 0, scm_uname);

SCM 
scm_uname ()
{
#ifdef HAVE_UNAME
  struct utsname buf;
  SCM ans = scm_make_vector(SCM_MAKINUM(5), SCM_UNSPECIFIED, SCM_BOOL_F);
  SCM *ve = SCM_VELTS (ans);
  SCM_DEFER_INTS;
  if (uname (&buf) < 0)
    scm_syserror (s_uname);
  ve[0] = scm_makfrom0str (buf.sysname);
  ve[1] = scm_makfrom0str (buf.nodename);
  ve[2] = scm_makfrom0str (buf.release);
  ve[3] = scm_makfrom0str (buf.version);
  ve[4] = scm_makfrom0str (buf.machine);
/* 
   a linux special?
  ve[5] = scm_makfrom0str (buf.domainname);
*/
  SCM_ALLOW_INTS;
  return ans;
#else
  scm_sysmissing (s_uname);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}

SCM_PROC (s_environ, "environ", 0, 1, 0, scm_environ);

SCM
scm_environ (env)
     SCM env;
{
  if (SCM_UNBNDP (env))
    return scm_makfromstrs (-1, environ);
  else
    {
      char **new_environ;

      SCM_DEFER_INTS;
      new_environ = environ_list_to_c (env, SCM_ARG1, s_environ);
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
      SCM_ALLOW_INTS;
      return SCM_UNSPECIFIED;
    }
}

#ifdef L_tmpnam

SCM_PROC (s_tmpnam, "tmpnam", 0, 0, 0, scm_tmpnam);

SCM scm_tmpnam()
{
  char name[L_tmpnam];
  SCM_SYSCALL (tmpnam (name););
  return scm_makfrom0str (name);
}
#endif

SCM_PROC (s_open_pipe, "open-pipe", 2, 0, 0, scm_open_pipe);

SCM 
scm_open_pipe (pipestr, modes)
     SCM pipestr;
     SCM modes;
{
  FILE *f;
  register SCM z;
  struct scm_port_table * pt;

  SCM_ASSERT (SCM_NIMP (pipestr) && SCM_ROSTRINGP (pipestr), pipestr,
	      SCM_ARG1, s_open_pipe);
  if (SCM_SUBSTRP (pipestr))
    pipestr = scm_makfromstr (SCM_ROCHARS (pipestr),
			      SCM_ROLENGTH (pipestr), 0);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2,
	      s_open_pipe);
  if (SCM_SUBSTRP (modes))
    modes = scm_makfromstr (SCM_ROCHARS (modes), SCM_ROLENGTH (modes), 0);
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  SCM_SYSCALL (f = popen (SCM_ROCHARS (pipestr), SCM_ROCHARS (modes)));
  if (!f)
    scm_syserror (s_open_pipe);
  pt = scm_add_to_port_table (z);
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETCAR (z, scm_tc16_pipe | SCM_OPN 
    | (strchr (SCM_ROCHARS (modes), 'r') ? SCM_RDNG : SCM_WRTNG));
  SCM_SETSTREAM (z, (SCM)f);
  SCM_ALLOW_INTS;
  return z;
}

SCM_PROC (s_close_pipe, "close-pipe", 1, 0, 0, scm_close_pipe);

SCM 
scm_close_pipe (port)
     SCM port;
{
  int rv;

  SCM_ASSERT (SCM_NIMP (port) && SCM_TYP16(port) == scm_tc16_pipe 
	      && SCM_OPENP (port), port, SCM_ARG1, s_close_pipe);
  SCM_DEFER_INTS;
  rv = pclose ((FILE *) SCM_STREAM (port));
  if (rv == -1)
    scm_syserror (s_close_pipe);
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (rv);
}

SCM_PROC (s_utime, "utime", 1, 2, 0, scm_utime);

SCM 
scm_utime (pathname, actime, modtime)
     SCM pathname;
     SCM actime;
     SCM modtime;
{
  int rv;
  struct utimbuf utm_tmp;

  SCM_ASSERT (SCM_NIMP (pathname) && SCM_ROSTRINGP (pathname), pathname,
	      SCM_ARG1, s_utime);

  SCM_COERCE_SUBSTR (pathname);
  if (SCM_UNBNDP (actime))
    SCM_SYSCALL (time (&utm_tmp.actime));
  else
    utm_tmp.actime = scm_num2ulong (actime, (char *) SCM_ARG2, s_utime);

  if (SCM_UNBNDP (modtime))
    SCM_SYSCALL (time (&utm_tmp.modtime));
  else
    utm_tmp.modtime = scm_num2ulong (modtime, (char *) SCM_ARG3, s_utime);

  SCM_SYSCALL (rv = utime (SCM_ROCHARS (pathname), &utm_tmp));
  if (rv != 0)
    scm_syserror (s_utime);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_access, "access?", 2, 0, 0, scm_access);

SCM 
scm_access (path, how)
     SCM path;
     SCM how;
{
  int rv;

  SCM_ASSERT (SCM_NIMP (path) && SCM_ROSTRINGP (path), path, SCM_ARG1,
	      s_access);
  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);
  SCM_ASSERT (SCM_INUMP (how), how, SCM_ARG2, s_access);
  rv = access (SCM_ROCHARS (path), SCM_INUM (how));
  return rv ? SCM_BOOL_F : SCM_BOOL_T;
}

SCM_PROC (s_getpid, "getpid", 0, 0, 0, scm_getpid);

SCM 
scm_getpid ()
{
  return SCM_MAKINUM ((unsigned long) getpid ());
}

SCM_PROC (s_putenv, "putenv", 1, 0, 0, scm_putenv);

SCM
scm_putenv (str)
     SCM str;
{
  int rv;
  char *ptr;

  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_putenv);
  /* must make a new copy to be left in the environment, safe from gc.  */
  ptr = malloc (SCM_LENGTH (str) + 1);
  if (ptr == NULL)
    scm_memory_error (s_putenv);
  strncpy (ptr, SCM_ROCHARS (str), SCM_LENGTH (str));
  ptr[SCM_LENGTH(str)] = 0;
  rv = putenv (ptr);
  if (rv < 0)
    scm_syserror (s_putenv);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_setlocale, "setlocale", 1, 1, 0, scm_setlocale);

SCM
scm_setlocale (category, locale)
     SCM category;
     SCM locale;
{
#ifdef HAVE_SETLOCALE
  char *clocale;
  char *rv;

  SCM_ASSERT (SCM_INUMP (category), category, SCM_ARG1, s_setlocale);
  if (SCM_UNBNDP (locale))
    {
      clocale = NULL;
    }
  else
    {
      SCM_ASSERT (SCM_NIMP (locale) && SCM_ROSTRINGP (locale), locale,
		  SCM_ARG2, s_setlocale);
      SCM_COERCE_SUBSTR (locale);
      clocale = SCM_ROCHARS (locale);
    }

  rv = setlocale (SCM_INUM (category), clocale);
  if (rv == NULL)
    scm_syserror (s_setlocale);
  return scm_makfrom0str (rv);
#else
  scm_sysmissing (s_setlocale);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}

SCM_PROC (s_mknod, "mknod", 4, 0, 0, scm_mknod);

SCM
scm_mknod(path, type, perms, dev)
     SCM path;
     SCM type;
     SCM perms;
     SCM dev;
{
#ifdef HAVE_MKNOD
  int val;
  char *p;
  int ctype;

  SCM_ASSERT (SCM_NIMP(path) && SCM_ROSTRINGP(path), path, SCM_ARG1, s_mknod);
  SCM_ASSERT (SCM_NIMP(type) && SCM_SYMBOLP (type), type, SCM_ARG2, s_mknod);
  SCM_ASSERT (SCM_INUMP (perms), perms, SCM_ARG3, s_mknod);
  SCM_ASSERT (SCM_INUMP(dev), dev, SCM_ARG4, s_mknod);
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
    scm_out_of_range (s_mknod, type);

  SCM_DEFER_INTS;
  SCM_SYSCALL (val = mknod(SCM_ROCHARS(path), ctype | SCM_INUM (perms),
			   SCM_INUM (dev)));
  if (val != 0)
    scm_syserror (s_mknod);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_mknod);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_nice, "nice", 1, 0, 0, scm_nice);

SCM
scm_nice(incr)
     SCM incr;
{
#ifdef HAVE_NICE
  SCM_ASSERT(SCM_INUMP(incr), incr, SCM_ARG1, s_nice);
  if (nice(SCM_INUM(incr)) != 0)
    scm_syserror (s_nice);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_nice);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_sync, "sync", 0, 0, 0, scm_sync);

SCM
scm_sync()
{
#ifdef HAVE_SYNC
  sync();
#else
  scm_sysmissing (s_sync);
  /* not reached.  */
#endif
  return SCM_UNSPECIFIED;
}

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
