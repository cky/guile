/* classes: h_files */

#ifndef POSIXH
#define POSIXH
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


#include "libguile/__scm.h"






extern SCM scm_tcsetpgrp SCM_P ((SCM port, SCM pgid));
extern SCM scm_tcgetpgrp SCM_P ((SCM port));
extern SCM scm_ctermid SCM_P ((void));
extern SCM scm_setsid SCM_P ((void));
extern SCM scm_setpgid SCM_P ((SCM pid, SCM pgid));
extern SCM scm_pipe SCM_P ((void));
extern SCM scm_getgroups SCM_P ((void));
extern SCM scm_getpgrp SCM_P ((void));
extern SCM scm_getpwuid SCM_P ((SCM user));
extern SCM scm_setpwent SCM_P ((SCM arg));
extern SCM scm_getgrgid SCM_P ((SCM name));
extern SCM scm_setgrent SCM_P ((SCM arg));
extern SCM scm_kill SCM_P ((SCM pid, SCM sig));
extern SCM scm_waitpid SCM_P ((SCM pid, SCM options));
extern SCM scm_status_exit_val SCM_P ((SCM status));
extern SCM scm_status_term_sig SCM_P ((SCM status));
extern SCM scm_status_stop_sig SCM_P ((SCM status));
extern SCM scm_getppid SCM_P ((void));
extern SCM scm_getuid SCM_P ((void));
extern SCM scm_getgid SCM_P ((void));
extern SCM scm_geteuid SCM_P ((void));
extern SCM scm_getegid SCM_P ((void));
extern SCM scm_setuid SCM_P ((SCM id));
extern SCM scm_setgid SCM_P ((SCM id));
extern SCM scm_seteuid SCM_P ((SCM id));
extern SCM scm_setegid SCM_P ((SCM id));
extern SCM scm_ttyname SCM_P ((SCM port));
extern SCM scm_execl SCM_P ((SCM filename, SCM args));
extern SCM scm_execlp SCM_P ((SCM filename, SCM args));
extern SCM scm_execle SCM_P ((SCM filename, SCM env, SCM args));
extern SCM scm_fork SCM_P ((void));
extern SCM scm_uname SCM_P ((void));
extern SCM scm_environ SCM_P ((SCM env));
extern SCM scm_tmpnam SCM_P ((void));
extern SCM scm_open_pipe SCM_P ((SCM pipestr, SCM modes));
extern SCM scm_close_pipe SCM_P ((SCM port));
extern SCM scm_utime SCM_P ((SCM pathname, SCM actime, SCM modtime));
extern SCM scm_access SCM_P ((SCM path, SCM how));
extern SCM scm_getpid SCM_P ((void));
extern SCM scm_putenv SCM_P ((SCM str));
extern SCM scm_setlocale SCM_P ((SCM category, SCM locale));
extern SCM scm_mknod SCM_P ((SCM path, SCM type, SCM perms, SCM dev));
extern SCM scm_nice SCM_P ((SCM incr));
extern SCM scm_sync SCM_P ((void));
extern void scm_init_posix SCM_P ((void));

#endif  /* POSIXH */
