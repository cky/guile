/* classes: h_files */

#ifndef FILESYSH
#define FILESYSH
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


#include <stdio.h>
#include "libguile/__scm.h"



extern long scm_tc16_fd;

#define SCM_FD_P(x)     (SCM_TYP16(x)==(scm_tc16_fd))
#define SCM_FD_FLAGS(x)	(SCM_CAR(x) >> 16)
#define SCM_FD(x)	((int)SCM_CDR (x))

enum scm_fd_flags
{
  scm_fd_is_open = 1,
  scm_close_fd_on_gc = 2
};




extern long scm_tc16_dir;
#define SCM_DIRP(x) (SCM_TYP16(x)==(scm_tc16_dir))
#define SCM_OPDIRP(x) (SCM_CAR(x)==(scm_tc16_dir | SCM_OPN))




extern SCM scm_sys_chown SCM_P ((SCM path, SCM owner, SCM group));
extern SCM scm_sys_chmod SCM_P ((SCM port_or_path, SCM mode));
extern SCM scm_umask SCM_P ((SCM mode));
extern SCM scm_intern_fd SCM_P ((int fd, int flags));
extern SCM scm_sys_open SCM_P ((SCM path, SCM flags, SCM mode));
extern SCM scm_sys_create SCM_P ((SCM path, SCM mode));
extern SCM scm_sys_close SCM_P ((SCM sfd));
extern SCM scm_sys_write_fd SCM_P ((SCM sfd, SCM buf));
extern SCM scm_sys_read_fd SCM_P ((SCM sfd, SCM buf, SCM offset, SCM length));
extern SCM scm_sys_lseek SCM_P ((SCM sfd, SCM offset, SCM whence));
extern SCM scm_sys_dup SCM_P ((SCM oldfd, SCM newfd));
extern SCM scm_sys_stat SCM_P ((SCM fd_or_path));
extern SCM scm_sys_link SCM_P ((SCM oldpath, SCM newpath));
extern SCM scm_sys_rename SCM_P ((SCM oldname, SCM newname));
extern SCM scm_sys_delete_file SCM_P ((SCM str));
extern SCM scm_sys_mkdir SCM_P ((SCM path, SCM mode));
extern SCM scm_sys_rmdir SCM_P ((SCM path));
extern SCM scm_sys_opendir SCM_P ((SCM dirname));
extern SCM scm_sys_readdir SCM_P ((SCM port));
extern SCM scm_rewinddir SCM_P ((SCM port));
extern SCM scm_sys_closedir SCM_P ((SCM port));
extern SCM scm_sys_chdir SCM_P ((SCM str));
extern SCM scm_sys_getcwd SCM_P ((void));
extern SCM scm_sys_select SCM_P ((SCM reads, SCM writes, SCM excepts, SCM secs, SCM msecs));
extern int scm_input_waiting_p SCM_P ((FILE *file, char *caller));
extern SCM scm_sys_symlink SCM_P ((SCM oldpath, SCM newpath));
extern SCM scm_sys_readlink SCM_P ((SCM path));
extern SCM scm_sys_lstat SCM_P ((SCM str));
extern SCM scm_sys_copy_file SCM_P ((SCM oldfile, SCM newfile));
extern void scm_init_filesys SCM_P ((void));

#endif  /* FILESYSH */
