/*	Copyright (C) 1996 Free Software Foundation, Inc.
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

#include "_scm.h"
#include "genio.h"
#include "smob.h"
#include "feature.h"

#include "filesys.h"

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

#ifdef HAVE_LIBC_H
#include <libc.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <pwd.h>


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



#ifdef O_CREAT
SCM_CONST_LONG (scm_O_CREAT, "O_CREAT", O_CREAT);
#endif 

#ifdef O_EXCL
SCM_CONST_LONG (scm_O_EXCL, "O_EXCL", O_EXCL);
#endif 

#ifdef O_NOCTTY
SCM_CONST_LONG (scm_O_NOCTTY, "O_NOCTTY", O_NOCTTY);
#endif 

#ifdef O_TRUNC
SCM_CONST_LONG (scm_O_TRUNC, "O_TRUNC", O_TRUNC);
#endif 

#ifdef O_APPEND
SCM_CONST_LONG (scm_O_APPEND, "O_APPEND", O_APPEND);
#endif 

#ifdef O_NONBLOCK
SCM_CONST_LONG (scm_O_NONBLOCK, "O_NONBLOCK", O_NONBLOCK);
#endif 

#ifdef O_NDELAY
SCM_CONST_LONG (scm_O_NDELAY, "O_NDELAY", O_NDELAY);
#endif 

#ifdef O_SYNC
SCM_CONST_LONG (scm_O_SYNC, "O_SYNC", O_SYNC);
#endif 





/* {Permissions}
 */

SCM_PROC (s_sys_chown, "chown", 3, 0, 0, scm_sys_chown);

SCM 
scm_sys_chown (path, owner, group)
     SCM path;
     SCM owner;
     SCM group;
{
  int val;

  SCM_ASSERT (SCM_NIMP (path) && SCM_ROSTRINGP (path), path, SCM_ARG1, s_sys_chown);
  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);
  SCM_ASSERT (SCM_INUMP (owner), owner, SCM_ARG2, s_sys_chown);
  SCM_ASSERT (SCM_INUMP (group), group, SCM_ARG3, s_sys_chown);
  SCM_SYSCALL (val = chown (SCM_ROCHARS (path),
			    SCM_INUM (owner), SCM_INUM (group)));
  if (val != 0)
    scm_syserror (s_sys_chown);
  return SCM_UNSPECIFIED;
}


SCM_PROC (s_sys_chmod, "chmod", 2, 0, 0, scm_sys_chmod);

SCM 
scm_sys_chmod (port_or_path, mode)
     SCM port_or_path;
     SCM mode;
{
  int rv;
  SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG2, s_sys_chmod);
  SCM_ASSERT (SCM_NIMP (port_or_path), port_or_path, SCM_ARG1, s_sys_chmod);
  if (SCM_STRINGP (port_or_path))
    SCM_SYSCALL (rv = chmod (SCM_CHARS (port_or_path), SCM_INUM (mode)));
  else
    {
      SCM_ASSERT (SCM_OPFPORTP (port_or_path), port_or_path, SCM_ARG1, s_sys_chmod);
      rv = fileno ((FILE *)SCM_STREAM (port_or_path));
      if (rv != -1)
	SCM_SYSCALL (rv = fchmod (rv, SCM_INUM (mode)));
    }
  if (rv != 0)
    scm_syserror (s_sys_chmod);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_umask, "umask", 0, 1, 0, scm_umask);

SCM 
scm_umask (mode)
     SCM mode;
{
  mode_t mask;
  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG1, s_umask);
      mask = umask (SCM_INUM (mode));
    }
  return SCM_MAKINUM (mask);
}


/* {File Descriptors} 
 */
long scm_tc16_fd;


static int scm_fd_print SCM_P ((SCM sexp, SCM port, scm_print_state *pstate));

static int 
scm_fd_print (sexp, port, pstate)
     SCM sexp;
     SCM port;
     scm_print_state *pstate;
{
  scm_gen_puts (scm_regular_string, "#<fd ", port);
  scm_intprint (SCM_CDR (sexp), 10, port);
  scm_gen_puts (scm_regular_string, ">", port);
  return 1;
}


static scm_sizet scm_fd_free SCM_P ((SCM p));

static scm_sizet 
scm_fd_free (p)
     SCM p;
{
  SCM flags;

  flags = SCM_FD_FLAGS (p);
  if ((scm_close_fd_on_gc & flags) && (scm_fd_is_open & flags))
    {
      SCM_SYSCALL( close (SCM_FD (p)) );
    }
  return 0;
}

static scm_smobfuns fd_smob = {scm_mark0, scm_fd_free, scm_fd_print, 0};


SCM
scm_intern_fd (fd, flags)
     int fd;
     int flags;
{
  SCM it;
  SCM_NEWCELL (it);
  SCM_REDEFER_INTS;
  SCM_SETCAR (it, (scm_tc16_fd | (flags << 16)));
  SCM_SETCDR (it, (SCM)fd);
  SCM_REALLOW_INTS;
  return it;
}



SCM_PROC (s_sys_open, "open", 3, 0, 0, scm_sys_open);

SCM
scm_sys_open (path, flags, mode)
     SCM path;
     SCM flags;
     SCM mode;
{
  int fd;
  SCM sfd;

  SCM_ASSERT (SCM_NIMP (path) && SCM_ROSTRINGP (path), path, SCM_ARG1, s_sys_open);
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG2, s_sys_open);
  SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG3, s_sys_open);

  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);

  SCM_DEFER_INTS;
  SCM_SYSCALL ( fd = open (SCM_ROCHARS (path), SCM_INUM (flags), SCM_INUM (mode)) );
  if (fd == -1)
    scm_syserror (s_sys_open);
  sfd = scm_intern_fd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;

  return scm_return_first (sfd, path);
}


SCM_PROC (s_sys_create, "create", 2, 0, 0, scm_sys_create);

SCM
scm_sys_create (path, mode)
     SCM path;
     SCM mode;
{
  int fd;
  SCM sfd;

  SCM_ASSERT (SCM_NIMP (path) && SCM_ROSTRINGP (path), path, SCM_ARG1, s_sys_create);
  SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG2, s_sys_create);

  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);

  SCM_DEFER_INTS;
  SCM_SYSCALL ( fd = creat (SCM_ROCHARS (path), SCM_INUM (mode)) );
  if (fd == -1)
    scm_syserror (s_sys_create);
  sfd = scm_intern_fd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;

  return scm_return_first (sfd, path);
}


SCM_PROC (s_sys_close, "close", 1, 0, 0, scm_sys_close);

SCM
scm_sys_close (sfd)
     SCM sfd;
{
  int fd;
  int got;
  SCM_ASSERT (SCM_NIMP (sfd) && SCM_FD_P (sfd), sfd, SCM_ARG1,  s_sys_close);
  fd = SCM_FD (sfd);

  SCM_DEFER_INTS;
  got = close (fd);
  SCM_SETCAR (sfd, scm_tc16_fd);
  SCM_ALLOW_INTS;
  if (got == -1)
    scm_syserror (s_sys_close);
  return SCM_UNSPECIFIED;
}


SCM_PROC (s_sys_write_fd, "write-fd", 2, 0, 0, scm_sys_write_fd);

SCM
scm_sys_write_fd (sfd, buf)
     SCM sfd;
     SCM buf;
{
  SCM answer;
  int fd;
  size_t written;
  SCM_ASSERT (SCM_NIMP (sfd) && SCM_FD_P (sfd), sfd, SCM_ARG1,  s_sys_write_fd);
  SCM_ASSERT (SCM_NIMP (buf) && SCM_ROSTRINGP (buf), buf, SCM_ARG2,  s_sys_write_fd);
  fd = SCM_FD (sfd);
  SCM_DEFER_INTS;
  written = write (fd, SCM_ROCHARS (buf), SCM_ROLENGTH (buf));
  if (written == -1)
    scm_syserror (s_sys_write_fd);
  answer = scm_long2num (written);
  SCM_ALLOW_INTS;
  return scm_return_first (answer, buf);
}


SCM_PROC (s_sys_read_fd, "read-fd", 2, 2, 0, scm_sys_read_fd);

SCM
scm_sys_read_fd (sfd, buf, offset, length)
     SCM sfd;
     SCM buf;
     SCM offset;
     SCM length;
{
  SCM answer;
  int fd;
  char * bytes;
  int off;
  int len;
  size_t got;

  SCM_ASSERT (SCM_NIMP (sfd) && SCM_FD_P (sfd), sfd, SCM_ARG1,  s_sys_read_fd);
  fd = SCM_FD (sfd);

  SCM_ASSERT (SCM_NIMP (buf) && SCM_STRINGP (buf), buf, SCM_ARG2,  s_sys_read_fd);
  bytes = SCM_CHARS (buf);

  if (SCM_UNBNDP (offset))
    off = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (offset), offset, SCM_ARG3, s_sys_read_fd);
      off = SCM_INUM (offset);
    }

  if (SCM_UNBNDP (length))
    len = SCM_LENGTH (buf);
  else
    {
      SCM_ASSERT (SCM_INUMP (length), length, SCM_ARG3, s_sys_read_fd);
      len = SCM_INUM (length);
    }

  SCM_DEFER_INTS;
  got = read (fd, bytes + off, len);
  if (got == -1)
    scm_syserror (s_sys_read_fd);
  answer = scm_long2num (got);
  SCM_ALLOW_INTS;
  return scm_return_first (answer, buf);
}

SCM_PROC (s_sys_lseek, "lseek", 2, 1, 0, scm_sys_lseek);

SCM
scm_sys_lseek (sfd, offset, whence)
     SCM sfd;
     SCM offset;
     SCM whence;
{
  SCM answer;
  int fd;
  long off;
  int wh;
  long got;

  SCM_ASSERT (SCM_NIMP (sfd) && SCM_FD_P (sfd), sfd, SCM_ARG1,  s_sys_lseek);
  fd = SCM_FD (sfd);

  off = scm_num2long (offset, (char *)SCM_ARG2, s_sys_lseek);
  if (SCM_UNBNDP (whence))
    wh = SEEK_SET;
  else
    {
      SCM_ASSERT (SCM_INUMP (whence), whence, SCM_ARG3, s_sys_lseek);
      wh = SCM_INUM (whence);
    }

  SCM_DEFER_INTS;
  SCM_SYSCALL (got = lseek (fd, off, wh));
  if (got == -1)
    scm_syserror (s_sys_lseek);
  answer = scm_long2num (got);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_sys_dup, "dup", 1, 1, 0, scm_sys_dup);

SCM
scm_sys_dup (oldfd, newfd)
     SCM oldfd;
     SCM newfd;
{
  SCM answer;
  int fd;
  int nfd;
  int (*fn)();

  SCM_ASSERT (SCM_NIMP (oldfd) && SCM_FD_P (oldfd), oldfd, SCM_ARG1,  s_sys_dup);
  SCM_ASSERT (SCM_UNBNDP (newfd) || SCM_INUMP (newfd), newfd, SCM_ARG2,  s_sys_dup);
  fd = SCM_FD (oldfd);
  nfd = (SCM_INUMP (newfd) ? SCM_INUM (newfd) : -1);

  SCM_DEFER_INTS;
  fn = ((nfd == -1) ? (int (*)())dup : (int (*)())dup2);
  nfd = fn (fd, nfd);
  if (nfd == -1)
    scm_syserror (s_sys_dup);
  answer = SCM_MAKINUM (nfd);
  SCM_ALLOW_INTS;
  return answer;
}



/* {Files}
 */

static SCM scm_stat2scm SCM_P ((struct stat *stat_temp));

static SCM 
scm_stat2scm (stat_temp)
     struct stat *stat_temp;
{
  SCM ans = scm_make_vector (SCM_MAKINUM (13), SCM_UNSPECIFIED, SCM_BOOL_F);
  SCM *ve = SCM_VELTS (ans);
  ve[0] = scm_ulong2num ((unsigned long) stat_temp->st_dev);
  ve[1] = scm_ulong2num ((unsigned long) stat_temp->st_ino);
  ve[2] = scm_ulong2num ((unsigned long) stat_temp->st_mode);
  ve[3] = scm_ulong2num ((unsigned long) stat_temp->st_nlink);
  ve[4] = scm_ulong2num ((unsigned long) stat_temp->st_uid);
  ve[5] = scm_ulong2num ((unsigned long) stat_temp->st_gid);
#ifdef HAVE_ST_RDEV
  ve[6] = scm_ulong2num ((unsigned long) stat_temp->st_rdev);
#else
  ve[6] = SCM_BOOL_F;
#endif
  ve[7] = scm_ulong2num ((unsigned long) stat_temp->st_size);
  ve[8] = scm_ulong2num ((unsigned long) stat_temp->st_atime);
  ve[9] = scm_ulong2num ((unsigned long) stat_temp->st_mtime);
  ve[10] = scm_ulong2num ((unsigned long) stat_temp->st_ctime);
#ifdef HAVE_ST_BLKSIZE
  ve[11] = scm_ulong2num ((unsigned long) stat_temp->st_blksize);
#else
  ve[11] = scm_ulong2num (4096L);
#endif
#ifdef HAVE_ST_BLOCKS
  ve[12] = scm_ulong2num ((unsigned long) stat_temp->st_blocks);
#else
  ve[12] = SCM_BOOL_F;
#endif

  return ans;
}

SCM_PROC (s_sys_stat, "stat", 1, 0, 0, scm_sys_stat);

SCM 
scm_sys_stat (fd_or_path)
     SCM fd_or_path;
{
  int rv = 1;
  struct stat stat_temp;

  if (SCM_INUMP (fd_or_path))
    {
      rv = SCM_INUM (fd_or_path);
      SCM_SYSCALL (rv = fstat (rv, &stat_temp));
    }
  else if (SCM_NIMP (fd_or_path) && SCM_FD_P (fd_or_path))
    {
      rv = SCM_FD (fd_or_path);
      SCM_SYSCALL (rv = fstat (rv, &stat_temp));
    }
  else
    {
      SCM_ASSERT (SCM_NIMP (fd_or_path), fd_or_path, SCM_ARG1, s_sys_stat);
      SCM_ASSERT (SCM_ROSTRINGP (fd_or_path), fd_or_path, SCM_ARG1, s_sys_stat);
      if (SCM_ROSTRINGP (fd_or_path))
	{
	  if (SCM_SUBSTRP (fd_or_path))
	    fd_or_path = scm_makfromstr (SCM_ROCHARS (fd_or_path), SCM_ROLENGTH (fd_or_path), 0);
	  SCM_SYSCALL (rv = stat (SCM_CHARS (fd_or_path), &stat_temp));
	}

    }
  if (rv != 0)
    scm_syserror_msg (s_sys_stat, "%s: %S",
		      scm_listify (scm_makfrom0str (strerror (errno)),
				   fd_or_path,
				   SCM_UNDEFINED));
  return scm_stat2scm (&stat_temp);
}



/* {Modifying Directories}
 */

SCM_PROC (s_sys_link, "link", 2, 0, 0, scm_sys_link);

SCM 
scm_sys_link (oldpath, newpath)
     SCM oldpath;
     SCM newpath;
{
  int val;

  SCM_ASSERT (SCM_NIMP (oldpath) && SCM_ROSTRINGP (oldpath), oldpath, SCM_ARG1, s_sys_link);
  if (SCM_SUBSTRP (oldpath))
    oldpath = scm_makfromstr (SCM_ROCHARS (oldpath), SCM_ROLENGTH (oldpath), 0);
  SCM_ASSERT (SCM_NIMP (newpath) && SCM_ROSTRINGP (newpath), newpath, SCM_ARG2, s_sys_link);
  if (SCM_SUBSTRP (newpath))
    newpath = scm_makfromstr (SCM_ROCHARS (newpath), SCM_ROLENGTH (newpath), 0);
  SCM_SYSCALL (val = link (SCM_ROCHARS (oldpath), SCM_ROCHARS (newpath)));
  if (val != 0)
    scm_syserror (s_sys_link);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_sys_rename, "rename-file", 2, 0, 0, scm_sys_rename);

SCM 
scm_sys_rename (oldname, newname)
     SCM oldname;
     SCM newname;
{
  int rv;
  SCM_ASSERT (SCM_NIMP (oldname) && SCM_STRINGP (oldname), oldname, SCM_ARG1, s_sys_rename);
  SCM_ASSERT (SCM_NIMP (newname) && SCM_STRINGP (newname), newname, SCM_ARG2, s_sys_rename);
#ifdef HAVE_RENAME
  SCM_SYSCALL (rv = rename (SCM_CHARS (oldname), SCM_CHARS (newname)));
  if (rv != 0)
    scm_syserror (s_sys_rename);
  return SCM_UNSPECIFIED;
#else
  SCM_DEFER_INTS;
  SCM_SYSCALL (rv = link (SCM_CHARS (oldname), SCM_CHARS (newname)));
  if (rv == 0)
    {
      SCM_SYSCALL (rv = unlink (SCM_CHARS (oldname)));;
      if (rv != 0)
	/* unlink failed.  remove new name */
	SCM_SYSCALL (unlink (SCM_CHARS (newname))); 
    }
  SCM_ALLOW_INTS;
  if (rv != 0)
    scm_syserror (s_sys_rename);
  return SCM_UNSPECIFIED;
#endif
}


SCM_PROC(s_sys_delete_file, "delete-file", 1, 0, 0, scm_sys_delete_file);

SCM 
scm_sys_delete_file (str)
     SCM str;
{
  int ans;
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_sys_delete_file);
  SCM_SYSCALL (ans = unlink (SCM_CHARS (str)));
  if (ans != 0)
    scm_syserror (s_sys_delete_file);
  return SCM_UNSPECIFIED;
}


SCM_PROC (s_sys_mkdir, "mkdir", 1, 1, 0, scm_sys_mkdir);

SCM 
scm_sys_mkdir (path, mode)
     SCM path;
     SCM mode;
{
#ifdef HAVE_MKDIR
  int rv;
  mode_t mask;
  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path, SCM_ARG1, s_sys_mkdir);
  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
      SCM_SYSCALL (rv = mkdir (SCM_CHARS (path), 0777 ^ mask));
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG2, s_sys_mkdir);
      SCM_SYSCALL (rv = mkdir (SCM_CHARS (path), SCM_INUM (mode)));
    }
  if (rv != 0)
    scm_syserror (s_sys_mkdir);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_sys_mkdir);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_sys_rmdir, "rmdir", 1, 0, 0, scm_sys_rmdir);

SCM 
scm_sys_rmdir (path)
     SCM path;
{
#ifdef HAVE_RMDIR
  int val;

  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path, SCM_ARG1, s_sys_rmdir);
  SCM_SYSCALL (val = rmdir (SCM_CHARS (path)));
  if (val != 0)
    scm_syserror (s_sys_rmdir);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_sys_rmdir);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


/* {Examining Directories}
 */

long scm_tc16_dir;

SCM_PROC (s_sys_opendir, "opendir", 1, 0, 0, scm_sys_opendir);

SCM 
scm_sys_opendir (dirname)
     SCM dirname;
{
  DIR *ds;
  SCM dir;
  SCM_ASSERT (SCM_NIMP (dirname) && SCM_STRINGP (dirname), dirname, SCM_ARG1, s_sys_opendir);
  SCM_NEWCELL (dir);
  SCM_DEFER_INTS;
  SCM_SYSCALL (ds = opendir (SCM_CHARS (dirname)));
  if (ds == NULL)
    scm_syserror (s_sys_opendir);
  SCM_SETCAR (dir, scm_tc16_dir | SCM_OPN);
  SCM_SETCDR (dir, ds);
  SCM_ALLOW_INTS;
  return dir;
}


SCM_PROC (s_sys_readdir, "readdir", 1, 0, 0, scm_sys_readdir);

SCM 
scm_sys_readdir (port)
     SCM port;
{
  struct dirent *rdent;
  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPDIRP (port), port, SCM_ARG1, s_sys_readdir);
  errno = 0;
  SCM_SYSCALL (rdent = readdir ((DIR *) SCM_CDR (port)));
  SCM_ALLOW_INTS;
  if (errno != 0)
    scm_syserror (s_sys_readdir);
  return (rdent ? scm_makfromstr (rdent->d_name, NAMLEN (rdent), 0)
	  : SCM_EOF_VAL);
}



SCM_PROC (s_rewinddir, "rewinddir", 1, 0, 0, scm_rewinddir);

SCM 
scm_rewinddir (port)
     SCM port;
{
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPDIRP (port), port, SCM_ARG1, s_rewinddir);
  rewinddir ((DIR *) SCM_CDR (port));
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_sys_closedir, "closedir", 1, 0, 0, scm_sys_closedir);

SCM 
scm_sys_closedir (port)
     SCM port;
{
  int sts;

  SCM_ASSERT (SCM_NIMP (port) && SCM_DIRP (port), port, SCM_ARG1, s_sys_closedir);
  SCM_DEFER_INTS;
  if (SCM_CLOSEDP (port))
    {
      SCM_ALLOW_INTS;
      return SCM_UNSPECIFIED;
    }
  SCM_SYSCALL (sts = closedir ((DIR *) SCM_CDR (port)));
  if (sts != 0)
    scm_syserror (s_sys_closedir);
  SCM_SETCAR (port, scm_tc16_dir);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}




static int scm_dir_print SCM_P ((SCM sexp, SCM port, scm_print_state *pstate));

static int 
scm_dir_print (sexp, port, pstate)
     SCM sexp;
     SCM port;
     scm_print_state *pstate;
{
  scm_prinport (sexp, port, "directory");
  return 1;
}


static scm_sizet scm_dir_free SCM_P ((SCM p));

static scm_sizet 
scm_dir_free (p)
     SCM p;
{
  if (SCM_OPENP (p))
    closedir ((DIR *) SCM_CDR (p));
  return 0;
}

static scm_smobfuns dir_smob = {scm_mark0, scm_dir_free, scm_dir_print, 0};


/* {Navigating Directories}
 */


SCM_PROC (s_sys_chdir, "chdir", 1, 0, 0, scm_sys_chdir);

SCM 
scm_sys_chdir (str)
     SCM str;
{
  int ans;

  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_sys_chdir);
  SCM_SYSCALL (ans = chdir (SCM_CHARS (str)));
  if (ans != 0)
    scm_syserror (s_sys_chdir);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_sys_getcwd, "getcwd", 0, 0, 0, scm_sys_getcwd);

SCM 
scm_sys_getcwd ()
{
#ifdef HAVE_GETCWD
  char *rv;

  scm_sizet size = 100;
  char *wd;
  SCM result;

  SCM_DEFER_INTS;
  wd = scm_must_malloc (size, s_sys_getcwd);
  while ((rv = getcwd (wd, size)) == 0 && errno == ERANGE)
    {
      scm_must_free (wd);
      size *= 2;
      wd = scm_must_malloc (size, s_sys_getcwd);
    }
  if (rv == 0)
    scm_syserror (s_sys_getcwd);
  result = scm_makfromstr (wd, strlen (wd), 0);
  scm_must_free (wd);
  SCM_ALLOW_INTS;
  return result;
#else
  scm_sysmissing (s_sys_getcwd);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}




static void fill_select_type SCM_P ((SELECT_TYPE * set, SCM list));

static void
fill_select_type (set, list)
     SELECT_TYPE * set;
     SCM list;
{
  while (list != SCM_EOL)
    {
      if (   SCM_NIMP (SCM_CAR (list))
	  && (scm_tc16_fport == SCM_TYP16 (SCM_CAR (list)))
	  && SCM_OPPORTP (SCM_CAR (list)))
	FD_SET (fileno ((FILE *)SCM_STREAM (SCM_CAR (list))), set);
      else if (SCM_INUMP (SCM_CAR (list)))
	FD_SET (SCM_INUM (SCM_CAR (list)), set);
      else if (SCM_NIMP (SCM_CAR (list)) && SCM_FD_P (SCM_CAR (list)))
	FD_SET (SCM_FD (SCM_CAR (list)), set);
      list = SCM_CDR (list);
    }
}


static SCM retrieve_select_type SCM_P ((SELECT_TYPE * set, SCM list));

static SCM 
retrieve_select_type (set, list)
     SELECT_TYPE * set;
     SCM list;
{
  SCM answer;
  answer = SCM_EOL;
  while (list != SCM_EOL)
    {
      if (   SCM_NIMP (SCM_CAR (list))
	  && (scm_tc16_fport == SCM_TYP16 (SCM_CAR (list)))
	  && SCM_OPPORTP (SCM_CAR (list)))
	{
	  if (FD_ISSET (fileno ((FILE *)SCM_STREAM (SCM_CAR (list))), set))
	    answer = scm_cons (SCM_CAR (list), answer);
	}
      else if (SCM_INUMP (SCM_CAR (list)))
	{
	  if (FD_ISSET (SCM_INUM (SCM_CAR (list)), set))
	    answer = scm_cons (SCM_CAR (list), answer);
	}
      else if (SCM_NIMP (SCM_CAR (list)) && SCM_FD_P (SCM_CAR (list)))
	{
	  if (FD_ISSET (SCM_FD (SCM_CAR (list)), set))
	    answer = scm_cons (SCM_CAR (list), answer);
	}
      list = SCM_CDR (list);
    }
  return answer;
}


SCM_PROC (s_sys_select, "select", 3, 2, 0, scm_sys_select);

SCM
scm_sys_select (reads, writes, excepts, secs, msecs)
     SCM reads;
     SCM writes;
     SCM excepts;
     SCM secs;
     SCM msecs;
{
#ifdef HAVE_SELECT
  struct timeval timeout;
  struct timeval * time_p;
  SELECT_TYPE read_set;
  SELECT_TYPE write_set;
  SELECT_TYPE except_set;
  int sreturn;

  SCM_ASSERT (-1 < scm_ilength (reads), reads, SCM_ARG1, s_sys_select);
  SCM_ASSERT (-1 < scm_ilength (writes), reads, SCM_ARG1, s_sys_select);
  SCM_ASSERT (-1 < scm_ilength (excepts), reads, SCM_ARG1, s_sys_select);

  FD_ZERO (&read_set);
  FD_ZERO (&write_set);
  FD_ZERO (&except_set);

  fill_select_type (&read_set, reads);
  fill_select_type (&write_set, writes);
  fill_select_type (&except_set, excepts);

  if (SCM_UNBNDP (secs))
    time_p = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (secs), secs, SCM_ARG4, s_sys_select);
      if (SCM_UNBNDP (msecs))
	msecs = SCM_INUM0;
      else
	SCM_ASSERT (SCM_INUMP (msecs), msecs, SCM_ARG5, s_sys_select);

      timeout.tv_sec = SCM_INUM (secs);
      timeout.tv_usec = 1000 * SCM_INUM (msecs);
      time_p = &timeout;
    }

  SCM_DEFER_INTS;
  sreturn = select (SELECT_SET_SIZE,
		    &read_set, &write_set, &except_set, time_p);
  if (sreturn < 0)
    scm_syserror (s_sys_select);
  SCM_ALLOW_INTS;
  return scm_listify (retrieve_select_type (&read_set, reads),
		      retrieve_select_type (&write_set, writes),
		      retrieve_select_type (&except_set, excepts),
		      SCM_UNDEFINED);
#else
  scm_sysmissing (s_sys_select);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


/* {Symbolic Links} 
 */

SCM_PROC (s_sys_symlink, "symlink", 2, 0, 0, scm_sys_symlink);

SCM
scm_sys_symlink(oldpath, newpath)
     SCM oldpath;
     SCM newpath;
{
#ifdef HAVE_SYMLINK
  int val;

  SCM_ASSERT(SCM_NIMP(oldpath) && SCM_STRINGP(oldpath), oldpath, SCM_ARG1, s_sys_symlink);
  SCM_ASSERT(SCM_NIMP(newpath) && SCM_STRINGP(newpath), newpath, SCM_ARG2, s_sys_symlink);
  SCM_SYSCALL (val = symlink(SCM_CHARS(oldpath), SCM_CHARS(newpath)));
  if (val != 0)
    scm_syserror (s_sys_symlink);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_sys_symlink);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_sys_readlink, "readlink", 1, 0, 0, scm_sys_readlink);

SCM
scm_sys_readlink(path)
  SCM path;
{
#ifdef HAVE_READLINK
  scm_sizet rv;
  scm_sizet size = 100;
  char *buf;
  SCM result;
  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path),  path, (char *) SCM_ARG1, s_sys_readlink);
  SCM_DEFER_INTS;
  buf = scm_must_malloc (size, s_sys_readlink);
  while ((rv = readlink (SCM_CHARS (path), buf, (scm_sizet) size)) == size)
    {
      scm_must_free (buf);
      size *= 2;
      buf = scm_must_malloc (size, s_sys_readlink);
    }
  if (rv == -1)
    scm_syserror (s_sys_readlink);
  result = scm_makfromstr (buf, rv, 0);
  scm_must_free (buf);
  SCM_ALLOW_INTS;
  return result;
#else
  scm_sysmissing (s_sys_readlink);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_sys_lstat, "lstat", 1, 0, 0, scm_sys_lstat);

SCM
scm_sys_lstat(str)
  SCM str;
{
#ifdef HAVE_LSTAT
  int rv;
  struct stat stat_temp;

  SCM_ASSERT(SCM_NIMP(str) && SCM_STRINGP(str), str, (char *)SCM_ARG1, s_sys_lstat);
  SCM_SYSCALL(rv = lstat(SCM_CHARS(str), &stat_temp));
  if (rv != 0)
    scm_syserror_msg (s_sys_lstat, "%s: %S",
		      scm_listify (scm_makfrom0str (strerror (errno)),
				   str,
				   SCM_UNDEFINED));
  return scm_stat2scm(&stat_temp);
#else 
  scm_sysmissing (s_sys_lstat);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_sys_copy_file, "copy-file", 2, 0, 0, scm_sys_copy_file);

SCM
scm_sys_copy_file (oldfile, newfile)
     SCM oldfile;
     SCM newfile;
{
  int oldfd, newfd;
  int n;
  char buf[BUFSIZ];		/* this space could be shared.  */
  struct stat oldstat;

  SCM_ASSERT (SCM_NIMP (oldfile) && SCM_ROSTRINGP (oldfile), oldfile, SCM_ARG1, s_sys_copy_file);
  if (SCM_SUBSTRP (oldfile))
    oldfile = scm_makfromstr (SCM_ROCHARS (oldfile), SCM_ROLENGTH (oldfile), 0);
  SCM_ASSERT (SCM_NIMP (newfile) && SCM_ROSTRINGP (newfile), newfile, SCM_ARG2, s_sys_copy_file);
  if (SCM_SUBSTRP (newfile))
    newfile = scm_makfromstr (SCM_ROCHARS (newfile), SCM_ROLENGTH (newfile), 0);
  if (stat (SCM_ROCHARS (oldfile), &oldstat) == -1)
    scm_syserror (s_sys_copy_file);
  SCM_DEFER_INTS;
  oldfd = open (SCM_ROCHARS (oldfile), O_RDONLY);
  if (oldfd == -1)
    scm_syserror (s_sys_copy_file);

  /* use POSIX flags instead of 07777?.  */
  newfd = open (SCM_ROCHARS (newfile), O_WRONLY | O_CREAT | O_TRUNC,
		oldstat.st_mode & 07777);
  if (newfd == -1)
    scm_syserror (s_sys_copy_file);

  while ((n = read (oldfd, buf, sizeof buf)) > 0)
    if (write (newfd, buf, n) != n)
      {
	close (oldfd);
	close (newfd);
	scm_syserror (s_sys_copy_file);
      }
  close (oldfd);
  if (close (newfd) == -1)
    scm_syserror (s_sys_copy_file);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}



void
scm_init_filesys ()
{
  scm_add_feature ("i/o-extensions");
  /* File type/permission bits.  */
#ifdef S_IRUSR
  scm_sysintern ("S_IRUSR", SCM_MAKINUM (S_IRUSR));
#endif
#ifdef S_IWUSR
  scm_sysintern ("S_IWUSR", SCM_MAKINUM (S_IWUSR));
#endif
#ifdef S_IXUSR
  scm_sysintern ("S_IXUSR", SCM_MAKINUM (S_IXUSR));
#endif
#ifdef S_IRWXU
  scm_sysintern ("S_IRWXU", SCM_MAKINUM (S_IRWXU));
#endif

#ifdef S_IRGRP
  scm_sysintern ("S_IRGRP", SCM_MAKINUM (S_IRGRP));
#endif
#ifdef S_IWGRP
  scm_sysintern ("S_IWGRP", SCM_MAKINUM (S_IWGRP));
#endif
#ifdef S_IXGRP
  scm_sysintern ("S_IXGRP", SCM_MAKINUM (S_IXGRP));
#endif
#ifdef S_IRWXG
  scm_sysintern ("S_IRWXG", SCM_MAKINUM (S_IRWXG));
#endif

#ifdef S_IROTH
  scm_sysintern ("S_IROTH", SCM_MAKINUM (S_IROTH));
#endif
#ifdef S_IWOTH
  scm_sysintern ("S_IWOTH", SCM_MAKINUM (S_IWOTH));
#endif
#ifdef S_IXOTH
  scm_sysintern ("S_IXOTH", SCM_MAKINUM (S_IXOTH));
#endif
#ifdef S_IRWXO
  scm_sysintern ("S_IRWXO", SCM_MAKINUM (S_IRWXO));
#endif

#ifdef S_ISUID
  scm_sysintern ("S_ISUID", SCM_MAKINUM (S_ISUID));
#endif
#ifdef S_ISGID
  scm_sysintern ("S_ISGID", SCM_MAKINUM (S_ISGID));
#endif
#ifdef S_ISVTX
  scm_sysintern ("S_ISVTX", SCM_MAKINUM (S_ISVTX));
#endif

#ifdef S_IFMT
  scm_sysintern ("S_IFMT", SCM_MAKINUM (S_IFMT));
#endif
#ifdef S_IFDIR
  scm_sysintern ("S_IFDIR", SCM_MAKINUM (S_IFDIR));
#endif
#ifdef S_IFCHR
  scm_sysintern ("S_IFCHR", SCM_MAKINUM (S_IFCHR));
#endif
#ifdef S_IFBLK
  scm_sysintern ("S_IFBLK", SCM_MAKINUM (S_IFBLK));
#endif
#ifdef S_IFREG
  scm_sysintern ("S_IFREG", SCM_MAKINUM (S_IFREG));
#endif
#ifdef S_IFLNK
  scm_sysintern ("S_IFLNK", SCM_MAKINUM (S_IFLNK));
#endif
#ifdef S_IFSOCK
  scm_sysintern ("S_IFSOCK", SCM_MAKINUM (S_IFSOCK));
#endif
#ifdef S_IFIFO
  scm_sysintern ("S_IFIFO", SCM_MAKINUM (S_IFIFO));
#endif


  scm_tc16_fd = scm_newsmob (&fd_smob);
  scm_tc16_dir = scm_newsmob (&dir_smob);

#include "filesys.x"
}
