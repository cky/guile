/* classes: h_files */

#ifndef FILESYSH
#define FILESYSH
/*	Copyright (C) 1995,1997 Free Software Foundation, Inc.
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
#include "libguile/__scm.h"



extern long scm_tc16_dir;
#define SCM_DIRP(x) (SCM_TYP16(x)==(scm_tc16_dir))
#define SCM_OPDIRP(x) (SCM_CAR(x)==(scm_tc16_dir | SCM_OPN))


extern SCM scm_chown SCM_P ((SCM object, SCM owner, SCM group));
extern SCM scm_chmod SCM_P ((SCM object, SCM mode));
extern SCM scm_umask SCM_P ((SCM mode));
extern SCM scm_open_fdes (SCM path, SCM flags, SCM mode);
extern SCM scm_open (SCM path, SCM flags, SCM mode);
extern SCM scm_close (SCM fd_or_port);
extern SCM scm_stat SCM_P ((SCM object));
extern SCM scm_link SCM_P ((SCM oldpath, SCM newpath));
extern SCM scm_rename SCM_P ((SCM oldname, SCM newname));
extern SCM scm_delete_file SCM_P ((SCM str));
extern SCM scm_truncate_file (SCM object, SCM size);
extern SCM scm_mkdir SCM_P ((SCM path, SCM mode));
extern SCM scm_rmdir SCM_P ((SCM path));
extern SCM scm_opendir SCM_P ((SCM dirname));
extern SCM scm_readdir SCM_P ((SCM port));
extern SCM scm_rewinddir SCM_P ((SCM port));
extern SCM scm_closedir SCM_P ((SCM port));
extern SCM scm_chdir SCM_P ((SCM str));
extern SCM scm_getcwd SCM_P ((void));
extern SCM scm_select SCM_P ((SCM reads, SCM writes, SCM excepts, SCM secs, SCM msecs));
extern int scm_input_waiting_p SCM_P ((FILE *file, char *caller));
extern SCM scm_fcntl (SCM object, SCM cmd, SCM value);
extern SCM scm_fsync (SCM object);
extern SCM scm_symlink SCM_P ((SCM oldpath, SCM newpath));
extern SCM scm_readlink SCM_P ((SCM path));
extern SCM scm_lstat SCM_P ((SCM str));
extern SCM scm_copy_file SCM_P ((SCM oldfile, SCM newfile));
extern void scm_init_filesys SCM_P ((void));

#endif  /* FILESYSH */
