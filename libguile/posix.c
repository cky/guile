/*	Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"



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
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

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

#ifdef FD_SET

#define SELECT_TYPE fd_set
#define SELECT_SET_SIZE FD_SETSIZE

#else /* no FD_SET */

/* Define the macros to access a single-int bitmap of descriptors.  */
#define SELECT_SET_SIZE 32
#define SELECT_TYPE int
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)

#endif /* no FD_SET */

extern char *ttyname ();
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

char *strptime ();

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif





SCM_PROC (s_sys_pipe, "%pipe", 0, 0, 0, scm_sys_pipe);
#ifdef __STDC__
SCM 
scm_sys_pipe (void)
#else
SCM 
scm_sys_pipe ()
#endif
{
  int fd[2], rv;
  FILE *f_rd, *f_wt;
  SCM p_rd, p_wt;
  SCM_NEWCELL (p_rd);
  SCM_NEWCELL (p_wt);
  rv = pipe (fd);
  if (rv)
    {
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
  f_rd = fdopen (fd[0], "r");
  if (!f_rd)
    {
      SCM_SYSCALL (close (fd[0]));
      SCM_SYSCALL (close (fd[1]));
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
  f_wt = fdopen (fd[1], "w");
  if (!f_wt)
    {
      int en;
      en = errno;
      fclose (f_rd);
      SCM_SYSCALL (close (fd[1]));
      SCM_ALLOW_INTS;
      return SCM_MAKINUM (en);
    }
    {
      struct scm_port_table * ptr;
      struct scm_port_table * ptw;

      ptr = scm_add_to_port_table (p_rd);
      ptw = scm_add_to_port_table (p_wt);
      SCM_SETPTAB_ENTRY (p_rd, ptr);
      SCM_SETPTAB_ENTRY (p_wt, ptw);
      SCM_CAR (p_rd) = scm_tc16_fport | scm_mode_bits ("r");
      SCM_CAR (p_wt) = scm_tc16_fport | scm_mode_bits ("w");
      SCM_SETSTREAM (p_rd, (SCM)f_rd);
      SCM_SETSTREAM (p_wt, (SCM)f_wt);
    }
  SCM_ALLOW_INTS;
  return scm_cons (p_rd, p_wt);
}



SCM_PROC (s_sys_getgroups, "%getgroups", 0, 0, 0, scm_sys_getgroups);
#ifdef __STDC__
SCM
scm_sys_getgroups(void)
#else
SCM
scm_sys_getgroups()
#endif
{
  SCM grps, ans;
  int ngroups = getgroups (0, NULL);
  if (!ngroups) return SCM_BOOL_F;
  SCM_NEWCELL(grps);
  SCM_DEFER_INTS;
  {
    GETGROUPS_T *groups;
    int val;

    groups = (gid_t *)scm_must_malloc(ngroups * sizeof(GETGROUPS_T),
				      s_sys_getgroups);
    val = getgroups(ngroups, groups);
    if (val < 0)
      {
	scm_must_free((char *)groups);
	SCM_ALLOW_INTS;
	return SCM_MAKINUM (errno);
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



SCM_PROC (s_sys_getpwuid, "%getpw", 0, 1, 0, scm_sys_getpwuid);
#ifdef __STDC__
SCM 
scm_sys_getpwuid (SCM user)
#else
SCM 
scm_sys_getpwuid (user)
     SCM user;
#endif
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
    }
  else if (SCM_INUMP (user))
    {
      SCM_DEFER_INTS;
      entry = getpwuid (SCM_INUM (user));
    }
  else
    {
      SCM_ASSERT (SCM_NIMP (user) && SCM_ROSTRINGP (user), user, SCM_ARG1, s_sys_getpwuid);
      if (SCM_SUBSTRP (user))
	user = scm_makfromstr (SCM_ROCHARS (user), SCM_ROLENGTH (user), 0);
      SCM_DEFER_INTS;
      entry = getpwnam (SCM_ROCHARS (user));
    }
  if (!entry)
    {
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
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



SCM_PROC (s_setpwent, "setpw", 0, 1, 0, scm_setpwent);
#ifdef __STDC__
SCM 
scm_setpwent (SCM arg)
#else
SCM 
scm_setpwent (arg)
     SCM arg;
#endif
{
  if (SCM_UNBNDP (arg) || SCM_FALSEP (arg))
    endpwent ();
  else
    setpwent ();
  return SCM_UNSPECIFIED;
}



/* Combines getgrgid and getgrnam.  */
SCM_PROC (s_sys_getgrgid, "%getgr", 0, 1, 0, scm_sys_getgrgid);
#ifdef __STDC__
SCM 
scm_sys_getgrgid (SCM name)
#else
SCM 
scm_sys_getgrgid (name)
     SCM name;
#endif
{
  SCM result;
  struct group *entry;
  SCM *ve;
  result = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED, SCM_BOOL_F);
  ve = SCM_VELTS (result);
  SCM_DEFER_INTS;
  if (SCM_UNBNDP (name) || (name == SCM_BOOL_F))
    SCM_SYSCALL (entry = getgrent ());
  else if (SCM_INUMP (name))
    SCM_SYSCALL (entry = getgrgid (SCM_INUM (name)));
  else
    {
      SCM_ASSERT (SCM_NIMP (name) && SCM_STRINGP (name), name, SCM_ARG1, s_sys_getgrgid);
      if (SCM_SUBSTRP (name))
	name = scm_makfromstr (SCM_ROCHARS (name), SCM_ROLENGTH (name), 0);
      SCM_SYSCALL (entry = getgrnam (SCM_CHARS (name)));
    }
  if (!entry)
    {
      SCM_ALLOW_INTS;
      return SCM_MAKINUM (errno);
    }
  ve[0] = scm_makfrom0str (entry->gr_name);
  ve[1] = scm_makfrom0str (entry->gr_passwd);
  ve[2] = scm_ulong2num ((unsigned long) entry->gr_gid);
  ve[3] = scm_makfromstrs (-1, entry->gr_mem);
  SCM_ALLOW_INTS;
  return result;
}



SCM_PROC (s_setgrent, "setgr", 0, 1, 0, scm_setgrent);
#ifdef __STDC__
SCM 
scm_setgrent (SCM arg)
#else
SCM 
scm_setgrent (arg)
     SCM arg;
#endif
{
  if (SCM_UNBNDP (arg) || SCM_FALSEP (arg))
    endgrent ();
  else
    setgrent ();
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_sys_kill, "%kill", 2, 0, 0, scm_sys_kill);
#ifdef __STDC__
SCM 
scm_sys_kill (SCM pid, SCM sig)
#else
SCM 
scm_sys_kill (pid, sig)
     SCM pid;
     SCM sig;
#endif
{
  int i;
  SCM_ASSERT (SCM_INUMP (pid), pid, SCM_ARG1, s_sys_kill);
  SCM_ASSERT (SCM_INUMP (sig), sig, SCM_ARG2, s_sys_kill);
  /* Signal values are interned in scm_init_posix().  */
  SCM_SYSCALL (i = kill ((int) SCM_INUM (pid), (int) SCM_INUM (sig)));
  return i ? SCM_MAKINUM (errno) : SCM_BOOL_T;
}



SCM_PROC (s_sys_waitpid, "%waitpid", 1, 1, 0, scm_sys_waitpid);
#ifdef __STDC__
SCM 
scm_sys_waitpid (SCM pid, SCM options)
#else
SCM 
scm_sys_waitpid (pid, options)
     SCM pid;
     SCM options;
#endif
{
  int i;
  int status;
  int ioptions;
  SCM_ASSERT (SCM_INUMP (pid), pid, SCM_ARG1, s_sys_waitpid);
  if (SCM_UNBNDP (options))
    ioptions = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (options), options, SCM_ARG2, s_sys_waitpid);
      /* Flags are interned in scm_init_posix.  */
      ioptions = SCM_INUM (options);
    }
  SCM_SYSCALL (i = waitpid (SCM_INUM (pid), &status, ioptions));
  return ((i == -1)
	  ? SCM_MAKINUM (errno)
	  : scm_cons (SCM_MAKINUM (0L + i), SCM_MAKINUM (0L + status)));
}



SCM_PROC (s_getppid, "getppid", 0, 0, 0, scm_getppid);
#ifdef __STDC__
SCM 
scm_getppid (void)
#else
SCM 
scm_getppid ()
#endif
{
  return SCM_MAKINUM (0L + getppid ());
}



SCM_PROC (s_getuid, "getuid", 0, 0, 0, scm_getuid);
#ifdef __STDC__
SCM 
scm_getuid (void)
#else
SCM 
scm_getuid ()
#endif
{
  return SCM_MAKINUM (0L + getuid ());
}



SCM_PROC (s_getgid, "getgid", 0, 0, 0, scm_getgid);
#ifdef __STDC__
SCM 
scm_getgid (void)
#else
SCM 
scm_getgid ()
#endif
{
  return SCM_MAKINUM (0L + getgid ());
}



SCM_PROC (s_geteuid, "geteuid", 0, 0, 0, scm_geteuid);
#ifdef __STDC__
SCM 
scm_geteuid (void)
#else
SCM 
scm_geteuid ()
#endif
{
#ifdef HAVE_GETEUID
  return SCM_MAKINUM (0L + geteuid ());
#else
  return SCM_MAKINUM (0L + getuid ());
#endif
}



SCM_PROC (s_getegid, "getegid", 0, 0, 0, scm_getegid);
#ifdef __STDC__
SCM 
scm_getegid (void)
#else
SCM 
scm_getegid ()
#endif
{
#ifdef HAVE_GETEUID
  return SCM_MAKINUM (0L + getegid ());
#else
  return SCM_MAKINUM (0L + getgid ());
#endif
}


SCM_PROC (s_sys_setuid, "%setuid", 1, 0, 0, scm_sys_setuid);
#ifdef __STDC__
SCM 
scm_sys_setuid (SCM id)
#else
SCM 
scm_sys_setuid (id)
     SCM id;
#endif
{
  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_sys_setuid);
  return setuid (SCM_INUM (id)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
}

SCM_PROC (s_sys_setgid, "%setgid", 1, 0, 0, scm_sys_setgid);
#ifdef __STDC__
SCM 
scm_sys_setgid (SCM id)
#else
SCM 
scm_sys_setgid (id)
     SCM id;
#endif
{
  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_sys_setgid);
  return setgid (SCM_INUM (id)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
}

SCM_PROC (s_sys_seteuid, "%seteuid", 1, 0, 0, scm_sys_seteuid);
#ifdef __STDC__
SCM 
scm_sys_seteuid (SCM id)
#else
SCM 
scm_sys_seteuid (id)
     SCM id;
#endif
{
  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_sys_seteuid);
#ifdef HAVE_SETEUID
  return seteuid (SCM_INUM (id)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
#else
  return setuid (SCM_INUM (id)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
#endif
}

SCM_PROC (s_sys_setegid, "%setegid", 1, 0, 0, scm_sys_setegid);
#ifdef __STDC__
SCM 
scm_sys_setegid (SCM id)
#else
SCM 
scm_sys_setegid (id)
     SCM id;
#endif
{
  SCM_ASSERT (SCM_INUMP (id), id, SCM_ARG1, s_sys_setegid);
#ifdef HAVE_SETEUID
  return setegid (SCM_INUM (id)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
#else
  return setgid (SCM_INUM (id)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
#endif
}

SCM_PROC (s_getpgrp, "getpgrp", 0, 0, 0, scm_getpgrp);
SCM 
scm_getpgrp ()
{
  int (*fn)();
  fn = getpgrp;
  return SCM_MAKINUM (fn (0));
}

SCM_PROC (s_setpgid, "%setpgid", 2, 0, 0, scm_setpgid);
SCM 
scm_setpgid (pid, pgid)
     SCM pid, pgid;
{
  SCM_ASSERT (SCM_INUMP (pid), pid, SCM_ARG1, s_setpgid);
  SCM_ASSERT (SCM_INUMP (pgid), pgid, SCM_ARG2, s_setpgid);
  /* This may be known as setpgrp, from BSD.  */
  return setpgid (SCM_INUM (pid), SCM_INUM (pgid)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
}

SCM_PROC (s_setsid, "%setsid", 0, 0, 0, scm_setsid);
SCM 
scm_setsid ()
{
  pid_t sid = setsid ();
  return (sid == -1) ? SCM_BOOL_F : SCM_MAKINUM (sid);
}

#ifndef ttyname
extern char * ttyname();
#endif

SCM_PROC (s_ttyname, "%ttyname", 1, 0, 0, scm_ttyname);
#ifdef __STDC__
SCM 
scm_ttyname (SCM port)
#else
SCM 
scm_ttyname (port)
     SCM port;
#endif
{
  char *ans;
  int fd;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPPORTP (port), port, SCM_ARG1, s_ttyname);
  if (scm_tc16_fport != SCM_TYP16 (port))
    return SCM_BOOL_F;
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd != -1)
    SCM_SYSCALL (ans = ttyname (fd));
  /* ans could be overwritten by another call to ttyname */
  return (((fd != -1) && ans)
	  ? scm_makfrom0str (ans)
	  : SCM_MAKINUM (errno));
}


SCM_PROC (s_ctermid, "%ctermid", 0, 0, 0, scm_ctermid);
SCM 
scm_ctermid ()
{
  char *result = ctermid (NULL);
  return *result == '\0' ? SCM_BOOL_F : scm_makfrom0str (result);
}

SCM_PROC (s_tcgetpgrp, "%tcgetpgrp", 1, 0, 0, scm_tcgetpgrp);
SCM 
scm_tcgetpgrp (port)
     SCM port;
{
  int fd;
  pid_t pgid;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_tcgetpgrp);
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1 || (pgid = tcgetpgrp (fd)) == -1)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (pgid);
}    

SCM_PROC (s_tcsetpgrp, "%tcsetpgrp", 2, 0, 0, scm_tcsetpgrp);
SCM 
scm_tcsetpgrp (port, pgid)
     SCM port, pgid;
{
  int fd;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_tcsetpgrp);
  SCM_ASSERT (SCM_INUMP (pgid), pgid, SCM_ARG2, s_tcsetpgrp);
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1 || tcsetpgrp (fd, SCM_INUM (pgid)) == -1)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}    

/* Copy exec args from an SCM vector into a new C array.  */
#ifdef __STDC__
static char **
scm_convert_exec_args (SCM args)
#else
static char **
scm_convert_exec_args (args)
     SCM args;
#endif
{
  char **execargv;
  int num_args;
  int i;
  SCM_DEFER_INTS;
  num_args = scm_ilength (args);
  execargv = (char **) 
    scm_must_malloc ((num_args + 1) * sizeof (char *), s_ttyname);
  for (i = 0; SCM_NNULLP (args); args = SCM_CDR (args), ++i)
    {
      scm_sizet len;
      char *dst;
      char *src;
      SCM_ASSERT (SCM_NIMP (SCM_CAR (args)) && SCM_ROSTRINGP (SCM_CAR (args)), SCM_CAR (args),
	      "wrong type in SCM_ARG", "exec arg");
      len = 1 + SCM_ROLENGTH (SCM_CAR (args));
      dst = (char *) scm_must_malloc ((long) len, s_ttyname);
      src = SCM_ROCHARS (SCM_CAR (args));
      while (len--) 
	dst[len] = src[len];
      execargv[i] = dst;
    }
  execargv[i] = 0;
  SCM_ALLOW_INTS;
  return execargv;
}

SCM_PROC (s_sys_execl, "%execl", 0, 0, 1, scm_sys_execl);
#ifdef __STDC__
SCM
scm_sys_execl (SCM args)
#else
SCM
scm_sys_execl (args)
     SCM args;
#endif
{
  char **execargv;
  SCM filename = SCM_CAR (args);
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename, SCM_ARG1, s_sys_execl);
  if (SCM_SUBSTRP (filename))
    filename = scm_makfromstr (SCM_ROCHARS (filename), SCM_ROLENGTH (filename), 0);
  args = SCM_CDR (args);
  execargv = scm_convert_exec_args (args);
  execv (SCM_ROCHARS (filename), execargv);
  return SCM_MAKINUM (errno);
}

SCM_PROC (s_sys_execlp, "%execlp", 0, 0, 1, scm_sys_execlp);
#ifdef __STDC__
SCM
scm_sys_execlp (SCM args)
#else
SCM
scm_sys_execlp (args)
     SCM args;
#endif
{
  char **execargv;
  SCM filename = SCM_CAR (args);
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename, SCM_ARG1, s_sys_execlp);
  if (SCM_SUBSTRP (filename))
    filename = scm_makfromstr (SCM_ROCHARS (filename), SCM_ROLENGTH (filename), 0);
  args = SCM_CDR (args);
  execargv = scm_convert_exec_args (args);
  execvp (SCM_ROCHARS (filename), execargv);
  return SCM_MAKINUM (errno);
}

/* Flushing streams etc., is not done here.  */
SCM_PROC (s_sys_fork, "%fork", 0, 0, 0, scm_sys_fork);
#ifdef __STDC__
SCM
scm_sys_fork(void)
#else
SCM
scm_sys_fork()
#endif
{
  pid_t pid;
  pid = fork ();
  if (pid == -1)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (0L+pid);
}


SCM_PROC (s_sys_uname, "%uname", 0, 0, 0, scm_sys_uname);
#ifdef __STDC__
SCM 
scm_sys_uname (void)
#else
SCM 
scm_sys_uname ()
#endif
{
#ifdef HAVE_UNAME
  struct utsname buf;
  SCM ans = scm_make_vector(SCM_MAKINUM(5), SCM_UNSPECIFIED, SCM_BOOL_F);
  SCM *ve = SCM_VELTS (ans);
  if (uname (&buf))
    return SCM_MAKINUM (errno);
  ve[0] = scm_makfrom0str (buf.sysname);
  ve[1] = scm_makfrom0str (buf.nodename);
  ve[2] = scm_makfrom0str (buf.release);
  ve[3] = scm_makfrom0str (buf.version);
  ve[4] = scm_makfrom0str (buf.machine);
/* 
  FIXME
  ve[5] = scm_makfrom0str (buf.domainname);
*/
  return ans;
#else
  return SCM_MAKINUM (ENOSYS);
#endif
}

SCM_PROC (s_environ, "environ", 0, 1, 0, scm_environ);
#ifdef __STDC__
SCM
scm_environ (SCM env)
#else
SCM
scm_environ (env)
     SCM env;
#endif
{
  if (SCM_UNBNDP (env))
    return scm_makfromstrs (-1, environ);
  else
    {
      int num_strings;
      char **new_environ;
      int i = 0;
      SCM_ASSERT (SCM_NULLP (env) || (SCM_NIMP (env) && SCM_CONSP (env)),
	      env, SCM_ARG1, s_environ);
      num_strings = scm_ilength (env);
      new_environ = (char **) scm_must_malloc ((num_strings + 1)
					       * sizeof (char *),
					       s_environ);
      while (SCM_NNULLP (env))
	{
	  int len;
	  char *src;
	  SCM_ASSERT (SCM_NIMP (SCM_CAR (env)) && SCM_ROSTRINGP (SCM_CAR (env)), env, SCM_ARG1,
		  s_environ);
	  len = 1 + SCM_ROLENGTH (SCM_CAR (env));
	  new_environ[i] = scm_must_malloc ((long) len, s_environ);
	  src = SCM_ROCHARS (SCM_CAR (env));
	  while (len--) 
	    new_environ[i][len] = src[len];
	  env = SCM_CDR (env);
	  i++;
	}
      new_environ[i] = 0;
      /* Free the old environment, except when called for the first
       * time.
       */
      {
	char **ep;
	static int first = 1;
	if (!first)
	  {
	    for (ep = environ; *ep != NULL; ep++)
	      scm_must_free (*ep);
	    scm_must_free ((char *) environ);
	  }
	first = 0;
      }
      environ = new_environ;
      return SCM_UNSPECIFIED;
    }
}


SCM_PROC (s_open_pipe, "open-pipe", 2, 0, 0, scm_open_pipe);
#ifdef __STDC__
SCM 
scm_open_pipe (SCM pipestr, SCM modes)
#else
SCM 
scm_open_pipe (pipestr, modes)
     SCM pipestr;
     SCM modes;
#endif
{
  FILE *f;
  register SCM z;
  SCM_ASSERT (SCM_NIMP (pipestr) && SCM_ROSTRINGP (pipestr), pipestr, SCM_ARG1, s_open_pipe);
  if (SCM_SUBSTRP (pipestr))
    pipestr = scm_makfromstr (SCM_ROCHARS (pipestr), SCM_ROLENGTH (pipestr), 0);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2, s_open_pipe);
  if (SCM_SUBSTRP (modes))
    modes = scm_makfromstr (SCM_ROCHARS (modes), SCM_ROLENGTH (modes), 0);
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  scm_ignore_signals ();
  SCM_SYSCALL (f = popen (SCM_ROCHARS (pipestr), SCM_ROCHARS (modes)));
  scm_unignore_signals ();
  if (!f)
    z = SCM_BOOL_F;
  else
    {
      struct scm_port_table * pt;
      pt = scm_add_to_port_table (z);
      SCM_SETPTAB_ENTRY (z, pt);
      SCM_CAR (z) = scm_tc16_pipe | SCM_OPN | (strchr (SCM_ROCHARS (modes), 'r') ? SCM_RDNG : SCM_WRTNG);
      SCM_SETSTREAM (z, (SCM)f);
    }
  SCM_ALLOW_INTS;
  return z;
}


SCM_PROC (s_open_input_pipe, "open-input-pipe", 1, 0, 0, scm_open_input_pipe);
#ifdef __STDC__
SCM
scm_open_input_pipe(SCM pipestr)
#else
SCM
scm_open_input_pipe(pipestr)
     SCM pipestr;
#endif
{
  return scm_open_pipe(pipestr, scm_makfromstr("r", (sizeof "r")-1, 0));
}

SCM_PROC (s_open_output_pipe, "open-output-pipe", 1, 0, 0, scm_open_output_pipe);
#ifdef __STDC__
SCM
scm_open_output_pipe(SCM pipestr)
#else
SCM
scm_open_output_pipe(pipestr)
     SCM pipestr;
#endif
{
  return scm_open_pipe(pipestr, scm_makfromstr("w", (sizeof "w")-1, 0));
}


#ifdef __EMX__
#include <sys/utime.h>
#else
#include <utime.h>
#endif

SCM_PROC (s_sys_utime, "%utime", 1, 2, 0, scm_sys_utime);
#ifdef __STDC__
SCM 
scm_sys_utime (SCM pathname, SCM actime, SCM modtime)
#else
SCM 
scm_sys_utime (pathname, actime, modtime)
     SCM pathname;
     SCM actime;
     SCM modtime;
#endif
{
  int rv;
  struct utimbuf utm_tmp;

  SCM_ASSERT (SCM_NIMP (pathname) && SCM_STRINGP (pathname), pathname, SCM_ARG1, s_sys_utime);

  if (SCM_UNBNDP (actime))
    SCM_SYSCALL (time (&utm_tmp.actime));
  else
    utm_tmp.actime = scm_num2ulong (actime, (char *) SCM_ARG2, s_sys_utime);

  if (SCM_UNBNDP (modtime))
    SCM_SYSCALL (time (&utm_tmp.modtime));
  else
    utm_tmp.modtime = scm_num2ulong (modtime, (char *) SCM_ARG3, s_sys_utime);

  SCM_SYSCALL (rv = utime (SCM_CHARS (pathname), &utm_tmp));
  return rv ? SCM_MAKINUM (errno) : SCM_BOOL_T;
}





SCM_PROC (s_sys_access, "access?", 2, 0, 0, scm_sys_access);
#ifdef __STDC__
SCM 
scm_sys_access (SCM path, SCM how)
#else
SCM 
scm_sys_access (path, how)
     SCM path;
     SCM how;
#endif
{
  int rv;

  SCM_ASSERT (SCM_NIMP (path) && SCM_ROSTRINGP (path), path, SCM_ARG1, s_sys_access);
  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);
  SCM_ASSERT (SCM_INUMP (how), how, SCM_ARG2, s_sys_access);
  rv = access (SCM_ROCHARS (path), SCM_INUM (how));
  return rv ? SCM_BOOL_F : SCM_BOOL_T;
}



SCM_PROC (s_getpid, "getpid", 0, 0, 0, scm_getpid);
#ifdef __STDC__
SCM 
scm_getpid (void)
#else
SCM 
scm_getpid ()
#endif
{
  return SCM_MAKINUM ((unsigned long) getpid ());
}


SCM_PROC (s_sys_putenv, "%putenv", 1, 0, 0, scm_sys_putenv);
#ifdef __STDC__
SCM
scm_sys_putenv (SCM str)
#else
SCM
scm_sys_putenv (str)
     SCM str;
#endif
{
#ifdef HAVE_PUTENV
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_sys_putenv);
  return putenv (SCM_CHARS (str)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
#else
  return SCM_MAKINUM (ENOSYS);
#endif
}


SCM_PROC (s_read_line, "read-line", 0, 2, 0, scm_read_line);
#ifdef __STDC__
SCM 
scm_read_line (SCM port, SCM include_terminator)
#else
SCM 
scm_read_line (port, include_terminator)
     SCM port;
     SCM include_terminator;
#endif
{
  register int c;
  register int j = 0;
  scm_sizet len = 30;
  SCM tok_buf;
  register char *p;
  int include;

  tok_buf = scm_makstr ((long) len, 0);
  p = SCM_CHARS (tok_buf);
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_read_line);

  if (SCM_UNBNDP (include_terminator))
    include = 0;
  else
    include = SCM_NFALSEP (include_terminator);

  if (EOF == (c = scm_gen_getc (port)))
    return SCM_EOF_VAL;
  while (1)
    {
      switch (c)
	{
	case SCM_LINE_INCREMENTORS:
	  if (j >= len)
	    {
	      p = scm_grow_tok_buf (&tok_buf);
	      len = SCM_LENGTH (tok_buf);
	    }
	  p[j++] = c;
	  /* fallthrough */
	case EOF:
	  if (len == j)
	    return tok_buf;
	  return scm_vector_set_length_x (tok_buf, (SCM) SCM_MAKINUM (j));

	default:
	  if (j >= len)
	    {
	      p = scm_grow_tok_buf (&tok_buf);
	      len = SCM_LENGTH (tok_buf);
	    }
	  p[j++] = c;
	  c = scm_gen_getc (port);
	  break;
	}
    }
}



SCM_PROC (s_read_line_x, "read-line!", 1, 1, 0, scm_read_line_x);
#ifdef __STDC__
SCM 
scm_read_line_x (SCM str, SCM port)
#else
SCM 
scm_read_line_x (str, port)
     SCM str;
     SCM port;
#endif
{
  register int c;
  register int j = 0;
  register char *p;
  scm_sizet len;
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_read_line_x);
  p = SCM_CHARS (str);
  len = SCM_LENGTH (str);
  if SCM_UNBNDP
    (port) port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG2, s_read_line_x);
  c = scm_gen_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  while (1)
    {
      switch (c)
	{
	case SCM_LINE_INCREMENTORS:
	case EOF:
	  return SCM_MAKINUM (j);
	default:
	  if (j >= len)
	    {
	      scm_gen_ungetc (c, port);
	      return SCM_BOOL_F;
	    }
	  p[j++] = c;
	  c = scm_gen_getc (port);
	}
    }
}



SCM_PROC (s_write_line, "write-line", 1, 1, 0, scm_write_line);
#ifdef __STDC__
SCM 
scm_write_line (SCM obj, SCM port)
#else
SCM 
scm_write_line (obj, port)
     SCM obj;
     SCM port;
#endif
{
  scm_display (obj, port);
  return scm_newline (port);
}



SCM_PROC (s_setlocale, "%setlocale", 1, 1, 0, scm_setlocale);
#ifdef __STDC__
SCM
scm_setlocale (SCM category, SCM locale)
#else
SCM
scm_setlocale (category, locale)
     SCM category;
     SCM locale;
#endif
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
      SCM_ASSERT (SCM_NIMP (locale) && SCM_STRINGP (locale), locale, SCM_ARG2, s_setlocale);
      clocale = SCM_CHARS (locale);
    }

  rv = setlocale (SCM_INUM (category), clocale);
  return rv ? scm_makfrom0str (rv) : SCM_MAKINUM (errno);
#else
  /* setlocale not available.  */
  return SCM_MAKINUM (errno);
#endif
}

SCM_PROC (s_strftime, "strftime", 2, 0, 0, scm_strftime);
#ifdef __STDC__
SCM
scm_strftime (SCM format, SCM stime)
#else
SCM
scm_strftime (format, stime)
     SCM format;
     SCM stime;
#endif
{
  struct tm t;

  char *tbuf;
  int n;
  int size = 50;
  char *fmt;
  int len;

  SCM_ASSERT (SCM_NIMP (format) && SCM_STRINGP (format), format, SCM_ARG1, s_strftime);
  SCM_ASSERT (SCM_NIMP (stime) && SCM_VECTORP (stime) && scm_obj_length (stime) == 9,
	  stime, SCM_ARG2, s_strftime);

  fmt = SCM_ROCHARS (format);
  len = SCM_ROLENGTH (format);

#define tm_deref scm_num2long (SCM_VELTS (stime)[n++], (char *)SCM_ARG2, s_strftime)
  n = 0;
  t.tm_sec = tm_deref;
  t.tm_min = tm_deref;
  t.tm_hour = tm_deref;
  t.tm_mday = tm_deref;
  t.tm_mon = tm_deref;
  t.tm_year = tm_deref;
  /* not used by mktime.
     t.tm_wday = tm_deref;
     t.tm_yday = tm_deref; */
  t.tm_isdst = tm_deref;
#undef tm_deref

  /* fill in missing fields and set the timezone.  */
  mktime (&t);

  tbuf = scm_must_malloc (size, s_strftime);
  while ((len = strftime (tbuf, size, fmt, &t)) == size)
    {
      scm_must_free (tbuf);
      size *= 2;
      tbuf = scm_must_malloc (size, s_strftime);
    }
  return scm_makfromstr (tbuf, len, 0);
}



SCM_PROC (s_sys_strptime, "%strptime", 2, 0, 0, scm_sys_strptime);
#ifdef __STDC__
SCM
scm_sys_strptime (SCM format, SCM string)
#else
SCM
scm_sys_strptime (format, string)
     SCM format;
     SCM string;
#endif
{
#ifdef HAVE_STRPTIME
  SCM stime;
  struct tm t;

  char *fmt, *str, *rest;
  int len;
  int n;

  SCM_ASSERT (SCM_NIMP (format) && SCM_ROSTRINGP (format), format, SCM_ARG1, s_sys_strptime);
  if (SCM_SUBSTRP (format))
    format =  scm_makfromstr (SCM_ROCHARS (format), SCM_ROLENGTH (format), 0);
  SCM_ASSERT (SCM_NIMP (string) && SCM_ROSTRINGP (string), string, SCM_ARG2, s_sys_strptime);
  if (SCM_SUBSTRP (string))
    string =  scm_makfromstr (SCM_ROCHARS (string), SCM_ROLENGTH (string), 0);

  fmt = SCM_CHARS (format);
  str = SCM_CHARS (string);

  /* initialize the struct tm */
#define tm_init(field) t.field = 0
  tm_init (tm_sec);
  tm_init (tm_min);
  tm_init (tm_hour);
  tm_init (tm_mday);
  tm_init (tm_mon);
  tm_init (tm_year);
  tm_init (tm_wday);
  tm_init (tm_yday);
  tm_init (tm_isdst);
#undef tm_init

  SCM_DEFER_INTS;
  rest = strptime (str, fmt, &t);
  SCM_ALLOW_INTS;

  if (rest == NULL) {
    return SCM_BOOL_F;
  }

  stime = scm_make_vector (SCM_MAKINUM (9), scm_long2num (0), SCM_UNDEFINED);

#define stime_set(val) scm_vector_set_x (stime, SCM_MAKINUM (n++), scm_long2num (t.val));
  n = 0;
  stime_set (tm_sec);
  stime_set (tm_min);
  stime_set (tm_hour);
  stime_set (tm_mday);
  stime_set (tm_mon);
  stime_set (tm_year);
  stime_set (tm_wday);
  stime_set (tm_yday);
  stime_set (tm_isdst);
#undef stime_set

  return scm_cons (stime, scm_makfrom0str (rest));
#else
  scm_wta (SCM_UNSPECIFIED, "strptime is not available and no replacement has (yet) been supplied", "strptime");
  return SCM_BOOL_F;
#endif
}

SCM_PROC (s_sys_mknod, "%mknod", 3, 0, 0, scm_sys_mknod);
#ifdef __STDC__
SCM
scm_sys_mknod(SCM path, SCM mode, SCM dev)
#else
SCM
scm_sys_mknod(path, mode, dev)
     SCM path;
     SCM mode;
     SCM dev;
#endif
{
#ifdef HAVE_MKNOD
  int val;
  SCM_ASSERT(SCM_NIMP(path) && SCM_STRINGP(path), path, SCM_ARG1, s_sys_mknod);
  SCM_ASSERT(SCM_INUMP(mode), mode, SCM_ARG2, s_sys_mknod);
  SCM_ASSERT(SCM_INUMP(dev), dev, SCM_ARG3, s_sys_mknod);
  SCM_SYSCALL(val = mknod(SCM_CHARS(path), SCM_INUM(mode), SCM_INUM(dev)));
  return val ? SCM_BOOL_F : SCM_BOOL_T;
#else
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_sys_nice, "%nice", 1, 0, 0, scm_sys_nice);
#ifdef __STDC__
SCM
scm_sys_nice(SCM incr)
#else
SCM
scm_sys_nice(incr)
     SCM incr;
#endif
{
#ifdef HAVE_NICE
  SCM_ASSERT(SCM_INUMP(incr), incr, SCM_ARG1, s_sys_nice);
  return nice(SCM_INUM(incr)) ? SCM_MAKINUM (errno) : SCM_BOOL_T;
#else
  return SCM_MAKINUM (ENOSYS);
#endif
}


SCM_PROC (s_sync, "sync", 0, 0, 0, scm_sync);
#ifdef __STDC__
SCM
scm_sync(void)
#else
SCM
scm_sync()
#endif
{
#ifdef HAVE_SYNC
  sync();
#endif
  return SCM_UNSPECIFIED;
}



#ifdef __STDC__
void 
scm_init_posix (void)
#else
void 
scm_init_posix ()
#endif
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

#ifdef EINTR
  scm_sysintern ("EINTR", SCM_MAKINUM (EINTR));
#endif

#ifdef SIGHUP
  scm_sysintern ("SIGHUP", SCM_MAKINUM (SIGHUP));
#endif
#ifdef SIGINT
  scm_sysintern ("SIGINT", SCM_MAKINUM (SIGINT));
#endif
#ifdef SIGQUIT
  scm_sysintern ("SIGQUIT", SCM_MAKINUM (SIGQUIT));
#endif
#ifdef SIGILL
  scm_sysintern ("SIGILL", SCM_MAKINUM (SIGILL));
#endif
#ifdef SIGTRAP
  scm_sysintern ("SIGTRAP", SCM_MAKINUM (SIGTRAP));
#endif
#ifdef SIGABRT
  scm_sysintern ("SIGABRT", SCM_MAKINUM (SIGABRT));
#endif
#ifdef SIGIOT
  scm_sysintern ("SIGIOT", SCM_MAKINUM (SIGIOT));
#endif
#ifdef SIGBUS
  scm_sysintern ("SIGBUS", SCM_MAKINUM (SIGBUS));
#endif
#ifdef SIGFPE
  scm_sysintern ("SIGFPE", SCM_MAKINUM (SIGFPE));
#endif
#ifdef SIGKILL
  scm_sysintern ("SIGKILL", SCM_MAKINUM (SIGKILL));
#endif
#ifdef SIGUSR1
  scm_sysintern ("SIGUSR1", SCM_MAKINUM (SIGUSR1));
#endif
#ifdef SIGSEGV
  scm_sysintern ("SIGSEGV", SCM_MAKINUM (SIGSEGV));
#endif
#ifdef SIGUSR2
  scm_sysintern ("SIGUSR2", SCM_MAKINUM (SIGUSR2));
#endif
#ifdef SIGPIPE
  scm_sysintern ("SIGPIPE", SCM_MAKINUM (SIGPIPE));
#endif
#ifdef SIGALRM
  scm_sysintern ("SIGALRM", SCM_MAKINUM (SIGALRM));
#endif
#ifdef SIGTERM
  scm_sysintern ("SIGTERM", SCM_MAKINUM (SIGTERM));
#endif
#ifdef SIGSTKFLT
  scm_sysintern ("SIGSTKFLT", SCM_MAKINUM (SIGSTKFLT));
#endif
#ifdef SIGCHLD
  scm_sysintern ("SIGCHLD", SCM_MAKINUM (SIGCHLD));
#endif
#ifdef SIGCONT
  scm_sysintern ("SIGCONT", SCM_MAKINUM (SIGCONT));
#endif
#ifdef SIGSTOP
  scm_sysintern ("SIGSTOP", SCM_MAKINUM (SIGSTOP));
#endif
#ifdef SIGTSTP
  scm_sysintern ("SIGTSTP", SCM_MAKINUM (SIGTSTP));
#endif
#ifdef SIGTTIN
  scm_sysintern ("SIGTTIN", SCM_MAKINUM (SIGTTIN));
#endif
#ifdef SIGTTOU
  scm_sysintern ("SIGTTOU", SCM_MAKINUM (SIGTTOU));
#endif
#ifdef SIGIO
  scm_sysintern ("SIGIO", SCM_MAKINUM (SIGIO));
#endif
#ifdef SIGPOLL
  scm_sysintern ("SIGPOLL", SCM_MAKINUM (SIGPOLL));
#endif
#ifdef SIGURG
  scm_sysintern ("SIGURG", SCM_MAKINUM (SIGURG));
#endif
#ifdef SIGXCPU
  scm_sysintern ("SIGXCPU", SCM_MAKINUM (SIGXCPU));
#endif
#ifdef SIGXFSZ
  scm_sysintern ("SIGXFSZ", SCM_MAKINUM (SIGXFSZ));
#endif
#ifdef SIGVTALRM
  scm_sysintern ("SIGVTALRM", SCM_MAKINUM (SIGVTALRM));
#endif
#ifdef SIGPROF
  scm_sysintern ("SIGPROF", SCM_MAKINUM (SIGPROF));
#endif
#ifdef SIGWINCH
  scm_sysintern ("SIGWINCH", SCM_MAKINUM (SIGWINCH));
#endif
#ifdef SIGLOST
  scm_sysintern ("SIGLOST", SCM_MAKINUM (SIGLOST));
#endif
#ifdef SIGPWR
  scm_sysintern ("SIGPWR", SCM_MAKINUM (SIGPWR));
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
#include "posix.x"
}
