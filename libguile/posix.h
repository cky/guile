/* classes: h_files */

#ifndef POSIXH
#define POSIXH
/*	Copyright (C) 1995 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"





#ifdef __STDC__
extern SCM scm_tcsetpgrp (SCM port, SCM pgid);
extern SCM scm_tcgetpgrp (SCM port);
extern SCM scm_ctermid (void);
extern SCM scm_setsid (void);
extern SCM scm_setpgid (SCM pid, SCM pgid);
extern SCM scm_sys_pipe (void);
extern SCM scm_sys_getgroups(void);
extern SCM scm_getpgrp (void);
extern SCM scm_sys_getpwuid (SCM user);
extern SCM scm_setpwent (SCM arg);
extern SCM scm_sys_getgrgid (SCM name);
extern SCM scm_setgrent (SCM arg);
extern SCM scm_sys_kill (SCM pid, SCM sig);
extern SCM scm_sys_waitpid (SCM pid, SCM options);
extern SCM scm_getppid (void);
extern SCM scm_getuid (void);
extern SCM scm_getgid (void);
extern SCM scm_geteuid (void);
extern SCM scm_getegid (void);
extern SCM scm_sys_setuid (SCM id);
extern SCM scm_sys_setgid (SCM id);
extern SCM scm_sys_seteuid (SCM id);
extern SCM scm_sys_setegid (SCM id);
extern SCM scm_ttyname (SCM port);
extern SCM scm_sys_execl (SCM args);
extern SCM scm_sys_execlp (SCM args);
extern SCM scm_sys_fork(void);
extern SCM scm_sys_uname (void);
extern SCM scm_environ (SCM env);
extern SCM scm_open_pipe (SCM pipestr, SCM modes);
extern SCM scm_open_input_pipe(SCM pipestr);
extern SCM scm_open_output_pipe(SCM pipestr);
extern SCM scm_sys_utime (SCM pathname, SCM actime, SCM modtime);
extern SCM scm_sys_access (SCM path, SCM how);
extern SCM scm_getpid (void);
extern SCM scm_sys_putenv (SCM str);
extern SCM scm_read_line (SCM port, SCM include_terminator);
extern SCM scm_read_line_x (SCM str, SCM port);
extern SCM scm_write_line (SCM obj, SCM port);
extern SCM scm_setlocale (SCM category, SCM locale);
extern SCM scm_strftime (SCM format, SCM stime);
extern SCM scm_sys_strptime (SCM format, SCM string);
extern SCM scm_sys_mknod(SCM path, SCM mode, SCM dev);
extern SCM scm_sys_nice(SCM incr);
extern SCM scm_sync(void);
extern void scm_init_posix (void);

#else /* STDC */
extern SCM scm_tcsetpgrp ();
extern SCM scm_tcgetpgrp ();
extern SCM scm_ctermid ();
extern SCM scm_setsid ();
extern SCM scm_setpgid ();
extern SCM scm_sys_pipe ();
extern SCM scm_sys_getgroups();
extern SCM scm_getpgrp ();
extern SCM scm_sys_getpwuid ();
extern SCM scm_setpwent ();
extern SCM scm_sys_getgrgid ();
extern SCM scm_setgrent ();
extern SCM scm_sys_kill ();
extern SCM scm_sys_waitpid ();
extern SCM scm_getppid ();
extern SCM scm_getuid ();
extern SCM scm_getgid ();
extern SCM scm_geteuid ();
extern SCM scm_getegid ();
extern SCM scm_sys_setuid ();
extern SCM scm_sys_setgid ();
extern SCM scm_sys_seteuid ();
extern SCM scm_sys_setegid ();
extern SCM scm_ttyname ();
extern SCM scm_sys_execl ();
extern SCM scm_sys_execlp ();
extern SCM scm_sys_fork();
extern SCM scm_sys_uname ();
extern SCM scm_environ ();
extern SCM scm_open_pipe ();
extern SCM scm_open_input_pipe();
extern SCM scm_open_output_pipe();
extern SCM scm_sys_utime ();
extern SCM scm_sys_access ();
extern SCM scm_getpid ();
extern SCM scm_sys_putenv ();
extern SCM scm_read_line ();
extern SCM scm_read_line_x ();
extern SCM scm_write_line ();
extern SCM scm_setlocale ();
extern SCM scm_strftime ();
extern SCM scm_sys_strptime ();
extern SCM scm_sys_mknod();
extern SCM scm_sys_nice();
extern SCM scm_sync();
extern void scm_init_posix ();

#endif /* STDC */







#endif  /* POSIXH */
