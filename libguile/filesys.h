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
#include "_scm.h"



extern long scm_tc16_fd;

#define SCM_FD_P(x) (SCM_TYP16(x)==(scm_tc16_fd))
#define SCM_FD_FLAGS(x)			(SCM_CAR(x) >> 16)
#define SCM_FD(x)			((int)SCM_CDR (x))

enum scm_fd_flags
{
  scm_fd_is_open = 1,
  scm_close_fd_on_gc = 2
};




extern long scm_tc16_dir;
#define SCM_DIRP(x) (SCM_TYP16(x)==(scm_tc16_dir))
#define SCM_OPDIRP(x) (SCM_CAR(x)==(scm_tc16_dir | SCM_OPN))



#ifdef __STDC__
extern SCM scm_sys_chown (SCM path, SCM owner, SCM group);
extern SCM scm_sys_chmod (SCM port_or_path, SCM mode);
extern SCM scm_umask (SCM mode);
extern SCM scm_intern_fd (int fd, int flags);
extern SCM scm_sys_open (SCM path, SCM flags, SCM mode);
extern SCM scm_sys_create (SCM path, SCM mode);
extern SCM scm_sys_close (SCM sfd);
extern SCM scm_sys_write_fd (SCM sfd, SCM buf);
extern SCM scm_sys_read_fd (SCM sfd, SCM buf, SCM offset, SCM length);
extern SCM scm_sys_lseek (SCM sfd, SCM offset, SCM whence);
extern SCM scm_sys_dup (SCM oldfd, SCM newfd);
extern SCM scm_sys_stat (SCM fd_or_path);
extern SCM scm_sys_link (SCM oldpath, SCM newpath);
extern SCM scm_sys_rename (SCM oldname, SCM newname);
extern SCM scm_sys_mkdir (SCM path, SCM mode);
extern SCM scm_sys_rmdir (SCM path);
extern SCM scm_sys_opendir (SCM dirname);
extern SCM scm_sys_readdir (SCM port);
extern SCM scm_rewinddir (SCM port);
extern SCM scm_sys_closedir (SCM port);
extern SCM scm_sys_chdir (SCM str);
extern SCM scm_sys_getcwd (void);
extern SCM scm_sys_select (SCM reads, SCM writes, SCM excepts, SCM secs, SCM msecs);
extern SCM scm_sys_symlink(SCM oldpath, SCM newpath);
extern SCM scm_sys_readlink(SCM path);
extern SCM scm_sys_lstat(SCM str);
extern SCM scm_sys_copy_file (SCM oldfile, SCM newfile);
extern void scm_init_filesys (void);

#else /* STDC */
extern SCM scm_sys_chown ();
extern SCM scm_sys_chmod ();
extern SCM scm_umask ();
extern SCM scm_intern_fd ();
extern SCM scm_sys_open ();
extern SCM scm_sys_create ();
extern SCM scm_sys_close ();
extern SCM scm_sys_write_fd ();
extern SCM scm_sys_read_fd ();
extern SCM scm_sys_lseek ();
extern SCM scm_sys_dup ();
extern SCM scm_sys_stat ();
extern SCM scm_sys_link ();
extern SCM scm_sys_rename ();
extern SCM scm_sys_mkdir ();
extern SCM scm_sys_rmdir ();
extern SCM scm_sys_opendir ();
extern SCM scm_sys_readdir ();
extern SCM scm_rewinddir ();
extern SCM scm_sys_closedir ();
extern SCM scm_sys_chdir ();
extern SCM scm_sys_getcwd ();
extern SCM scm_sys_select ();
extern SCM scm_sys_symlink();
extern SCM scm_sys_readlink();
extern SCM scm_sys_lstat();
extern SCM scm_sys_copy_file ();
extern void scm_init_filesys ();

#endif /* STDC */

#endif  /* FILESYSH */
