/* classes: h_files */

#ifndef SCM_POSIX_H
#define SCM_POSIX_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"






SCM_API SCM scm_tcsetpgrp (SCM port, SCM pgid);
SCM_API SCM scm_tcgetpgrp (SCM port);
SCM_API SCM scm_ctermid (void);
SCM_API SCM scm_setsid (void);
SCM_API SCM scm_setpgid (SCM pid, SCM pgid);
SCM_API SCM scm_pipe (void);
SCM_API SCM scm_getgroups (void);
SCM_API SCM scm_getpgrp (void);
SCM_API SCM scm_getpwuid (SCM user);
SCM_API SCM scm_setpwent (SCM arg);
SCM_API SCM scm_getgrgid (SCM name);
SCM_API SCM scm_setgrent (SCM arg);
SCM_API SCM scm_kill (SCM pid, SCM sig);
SCM_API SCM scm_waitpid (SCM pid, SCM options);
SCM_API SCM scm_status_exit_val (SCM status);
SCM_API SCM scm_status_term_sig (SCM status);
SCM_API SCM scm_status_stop_sig (SCM status);
SCM_API SCM scm_getppid (void);
SCM_API SCM scm_getuid (void);
SCM_API SCM scm_getgid (void);
SCM_API SCM scm_geteuid (void);
SCM_API SCM scm_getegid (void);
SCM_API SCM scm_setuid (SCM id);
SCM_API SCM scm_setgid (SCM id);
SCM_API SCM scm_seteuid (SCM id);
SCM_API SCM scm_setegid (SCM id);
SCM_API SCM scm_ttyname (SCM port);
SCM_API SCM scm_execl (SCM filename, SCM args);
SCM_API SCM scm_execlp (SCM filename, SCM args);
SCM_API SCM scm_execle (SCM filename, SCM env, SCM args);
SCM_API SCM scm_fork (void);
SCM_API SCM scm_uname (void);
SCM_API SCM scm_environ (SCM env);
SCM_API SCM scm_tmpnam (void);
SCM_API SCM scm_mkstemp (SCM tmpl);
SCM_API SCM scm_open_pipe (SCM pipestr, SCM modes);
SCM_API SCM scm_close_pipe (SCM port);
SCM_API SCM scm_utime (SCM pathname, SCM actime, SCM modtime);
SCM_API SCM scm_access (SCM path, SCM how);
SCM_API SCM scm_getpid (void);
SCM_API SCM scm_putenv (SCM str);
SCM_API SCM scm_setlocale (SCM category, SCM locale);
SCM_API SCM scm_mknod (SCM path, SCM type, SCM perms, SCM dev);
SCM_API SCM scm_nice (SCM incr);
SCM_API SCM scm_sync (void);
SCM_API SCM scm_crypt (SCM key, SCM salt);
SCM_API SCM scm_chroot (SCM path);
SCM_API SCM scm_getlogin (void);
SCM_API SCM scm_cuserid (void);
SCM_API SCM scm_getpriority (SCM which, SCM who);
SCM_API SCM scm_setpriority (SCM which, SCM who, SCM prio);
SCM_API SCM scm_getpass (SCM prompt);
SCM_API SCM scm_flock (SCM file, SCM operation);
SCM_API SCM scm_sethostname (SCM name);
SCM_API SCM scm_gethostname (void);
SCM_API void scm_init_posix (void);

#endif  /* SCM_POSIX_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
