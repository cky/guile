/*	Copyright (C) 1996, 1997 Free Software Foundation, Inc.
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
#include "genio.h"
#include "smob.h"
#include "feature.h"
#include "fports.h"

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

#ifdef LIBC_H_WITH_UNISTD_H
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






/* {Permissions}
 */

SCM_PROC (s_chown, "chown", 3, 0, 0, scm_chown);

SCM 
scm_chown (path, owner, group)
     SCM path;
     SCM owner;
     SCM group;
{
  int val;

  SCM_ASSERT (SCM_NIMP (path) && SCM_ROSTRINGP (path), path, SCM_ARG1, s_chown);
  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);
  SCM_ASSERT (SCM_INUMP (owner), owner, SCM_ARG2, s_chown);
  SCM_ASSERT (SCM_INUMP (group), group, SCM_ARG3, s_chown);
  SCM_SYSCALL (val = chown (SCM_ROCHARS (path),
			    SCM_INUM (owner), SCM_INUM (group)));
  if (val != 0)
    scm_syserror (s_chown);
  return SCM_UNSPECIFIED;
}


SCM_PROC (s_chmod, "chmod", 2, 0, 0, scm_chmod);

SCM 
scm_chmod (port_or_path, mode)
     SCM port_or_path;
     SCM mode;
{
  int rv;
  SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG2, s_chmod);
  SCM_ASSERT (SCM_NIMP (port_or_path), port_or_path, SCM_ARG1, s_chmod);
  if (SCM_STRINGP (port_or_path))
    SCM_SYSCALL (rv = chmod (SCM_CHARS (port_or_path), SCM_INUM (mode)));
  else
    {
      SCM_ASSERT (SCM_OPFPORTP (port_or_path), port_or_path, SCM_ARG1, s_chmod);
      rv = fileno ((FILE *)SCM_STREAM (port_or_path));
      if (rv != -1)
	SCM_SYSCALL (rv = fchmod (rv, SCM_INUM (mode)));
    }
  if (rv != 0)
    scm_syserror (s_chmod);
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



SCM_PROC (s_open, "open", 2, 1, 0, scm_open);

SCM
scm_open (path, flags, mode)
     SCM path;
     SCM flags;
     SCM mode;
{
  int fd;
  SCM newpt;
  FILE *f;
  char *port_mode;
  int iflags;

  SCM_ASSERT (SCM_NIMP (path) && SCM_ROSTRINGP (path), path, SCM_ARG1, s_open);
  iflags = scm_num2long (flags, (char *) SCM_ARG2, s_open);

  if (SCM_SUBSTRP (path))
    path = scm_makfromstr (SCM_ROCHARS (path), SCM_ROLENGTH (path), 0);

  SCM_DEFER_INTS;
  if (SCM_UNBNDP (mode))
    SCM_SYSCALL (fd = open (SCM_ROCHARS (path), iflags));
  else
    {
      SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG3, s_open);
      SCM_SYSCALL (fd = open (SCM_ROCHARS (path), iflags, SCM_INUM (mode)));
    }
  if (fd == -1)
    scm_syserror (s_open);
  SCM_NEWCELL (newpt);
  if (iflags & O_RDWR)
    port_mode = "r+";
  else {
    if (iflags & O_WRONLY)
      port_mode = "w";
    else
      port_mode = "r";
  }
  f = fdopen (fd, port_mode);
  if (!f)
    {
      SCM_SYSCALL (close (fd));
      scm_syserror (s_open);
    }
  {
    struct scm_port_table * pt;

    pt = scm_add_to_port_table (newpt);
    SCM_SETPTAB_ENTRY (newpt, pt);
    SCM_SETCAR (newpt, scm_tc16_fport | scm_mode_bits (port_mode));
    /* if (SCM_BUF0 & SCM_CAR (newpt))
       scm_setbuf0 (newpt); */
    SCM_SETSTREAM (newpt, (SCM)f);
    SCM_PTAB_ENTRY (newpt)->file_name = path;
  }
  SCM_ALLOW_INTS;

  return newpt;
}


/* {Files}
 */

SCM_SYMBOL (scm_sym_regular, "regular");
SCM_SYMBOL (scm_sym_directory, "directory");
SCM_SYMBOL (scm_sym_symlink, "symlink");
SCM_SYMBOL (scm_sym_block_special, "block-special");
SCM_SYMBOL (scm_sym_char_special, "char-special");
SCM_SYMBOL (scm_sym_fifo, "fifo");
SCM_SYMBOL (scm_sym_sock, "socket");
SCM_SYMBOL (scm_sym_unknown, "unknown");

static SCM scm_stat2scm SCM_P ((struct stat *stat_temp));

static SCM 
scm_stat2scm (stat_temp)
     struct stat *stat_temp;
{
  SCM ans = scm_make_vector (SCM_MAKINUM (15), SCM_UNSPECIFIED, SCM_BOOL_F);
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
  {
    int mode = stat_temp->st_mode;
    
    if (S_ISREG (mode))
      ve[13] = scm_sym_regular;
    else if (S_ISDIR (mode))
      ve[13] = scm_sym_directory;
    else if (S_ISLNK (mode))
      ve[13] = scm_sym_symlink;
    else if (S_ISBLK (mode))
      ve[13] = scm_sym_block_special;
    else if (S_ISCHR (mode))
      ve[13] = scm_sym_char_special;
    else if (S_ISFIFO (mode))
      ve[13] = scm_sym_fifo;
    else if (S_ISSOCK (mode))
      ve[13] = scm_sym_sock;
    else
      ve[13] = scm_sym_unknown;

    ve[14] = SCM_MAKINUM ((~S_IFMT) & mode);

    /* the layout of the bits in ve[14] is intended to be portable.
       If there are systems that don't follow the usual convention,
       the following could be used:

       tmp = 0;
       if (S_ISUID & mode) tmp += 1;
       tmp <<= 1;
       if (S_IRGRP & mode) tmp += 1;
       tmp <<= 1;
       if (S_ISVTX & mode) tmp += 1;
       tmp <<= 1;
       if (S_IRUSR & mode) tmp += 1;
       tmp <<= 1;
       if (S_IWUSR & mode) tmp += 1;
       tmp <<= 1;
       if (S_IXUSR & mode) tmp += 1;
       tmp <<= 1;
       if (S_IWGRP & mode) tmp += 1;
       tmp <<= 1;
       if (S_IXGRP & mode) tmp += 1;
       tmp <<= 1;
       if (S_IROTH & mode) tmp += 1;
       tmp <<= 1;
       if (S_IWOTH & mode) tmp += 1;
       tmp <<= 1;
       if (S_IXOTH & mode) tmp += 1; 

       ve[14] = SCM_MAKINUM (tmp);
       
       */
  }  

  return ans;
}

SCM_PROC (s_stat, "stat", 1, 0, 0, scm_stat);

SCM 
scm_stat (file)
     SCM file;
{
  int rv = 1;
  struct stat stat_temp;

  if (SCM_INUMP (file))
    SCM_SYSCALL (rv = fstat (SCM_INUM (file), &stat_temp));
  else
    {
      SCM_ASSERT (SCM_NIMP (file), file, SCM_ARG1, s_stat);
      if (SCM_FPORTP (file))
	SCM_SYSCALL (rv = fstat (fileno ((FILE *) SCM_STREAM (file)),
				 &stat_temp));
      else
	{
	  SCM_ASSERT (SCM_ROSTRINGP (file), file, SCM_ARG1, s_stat);
	  if (SCM_SUBSTRP (file))
	    file = scm_makfromstr (SCM_ROCHARS (file),
				   SCM_ROLENGTH (file),
				   0);
	  SCM_SYSCALL (rv = stat (SCM_CHARS (file), &stat_temp));
	}
    }
  if (rv != 0)
    {
      int en = errno;

      scm_syserror_msg (s_stat, "%s: %S",
			scm_listify (scm_makfrom0str (strerror (errno)),
				     file,
				     SCM_UNDEFINED),
			en);
    }
  return scm_stat2scm (&stat_temp);
}



/* {Modifying Directories}
 */

SCM_PROC (s_link, "link", 2, 0, 0, scm_link);

SCM 
scm_link (oldpath, newpath)
     SCM oldpath;
     SCM newpath;
{
  int val;

  SCM_ASSERT (SCM_NIMP (oldpath) && SCM_ROSTRINGP (oldpath), oldpath, SCM_ARG1, s_link);
  if (SCM_SUBSTRP (oldpath))
    oldpath = scm_makfromstr (SCM_ROCHARS (oldpath), SCM_ROLENGTH (oldpath), 0);
  SCM_ASSERT (SCM_NIMP (newpath) && SCM_ROSTRINGP (newpath), newpath, SCM_ARG2, s_link);
  if (SCM_SUBSTRP (newpath))
    newpath = scm_makfromstr (SCM_ROCHARS (newpath), SCM_ROLENGTH (newpath), 0);
  SCM_SYSCALL (val = link (SCM_ROCHARS (oldpath), SCM_ROCHARS (newpath)));
  if (val != 0)
    scm_syserror (s_link);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_rename, "rename-file", 2, 0, 0, scm_rename);

SCM 
scm_rename (oldname, newname)
     SCM oldname;
     SCM newname;
{
  int rv;
  SCM_ASSERT (SCM_NIMP (oldname) && SCM_STRINGP (oldname), oldname, SCM_ARG1, s_rename);
  SCM_ASSERT (SCM_NIMP (newname) && SCM_STRINGP (newname), newname, SCM_ARG2, s_rename);
#ifdef HAVE_RENAME
  SCM_SYSCALL (rv = rename (SCM_CHARS (oldname), SCM_CHARS (newname)));
  if (rv != 0)
    scm_syserror (s_rename);
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
    scm_syserror (s_rename);
  return SCM_UNSPECIFIED;
#endif
}


SCM_PROC(s_delete_file, "delete-file", 1, 0, 0, scm_delete_file);

SCM 
scm_delete_file (str)
     SCM str;
{
  int ans;
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_delete_file);
  SCM_SYSCALL (ans = unlink (SCM_CHARS (str)));
  if (ans != 0)
    scm_syserror (s_delete_file);
  return SCM_UNSPECIFIED;
}


SCM_PROC (s_mkdir, "mkdir", 1, 1, 0, scm_mkdir);

SCM 
scm_mkdir (path, mode)
     SCM path;
     SCM mode;
{
#ifdef HAVE_MKDIR
  int rv;
  mode_t mask;
  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path, SCM_ARG1, s_mkdir);
  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
      SCM_SYSCALL (rv = mkdir (SCM_CHARS (path), 0777 ^ mask));
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG2, s_mkdir);
      SCM_SYSCALL (rv = mkdir (SCM_CHARS (path), SCM_INUM (mode)));
    }
  if (rv != 0)
    scm_syserror (s_mkdir);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_mkdir);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_rmdir, "rmdir", 1, 0, 0, scm_rmdir);

SCM 
scm_rmdir (path)
     SCM path;
{
#ifdef HAVE_RMDIR
  int val;

  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path, SCM_ARG1, s_rmdir);
  SCM_SYSCALL (val = rmdir (SCM_CHARS (path)));
  if (val != 0)
    scm_syserror (s_rmdir);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_rmdir);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


/* {Examining Directories}
 */

long scm_tc16_dir;

SCM_PROC (s_opendir, "opendir", 1, 0, 0, scm_opendir);

SCM 
scm_opendir (dirname)
     SCM dirname;
{
  DIR *ds;
  SCM dir;
  SCM_ASSERT (SCM_NIMP (dirname) && SCM_STRINGP (dirname), dirname, SCM_ARG1, s_opendir);
  SCM_NEWCELL (dir);
  SCM_DEFER_INTS;
  SCM_SYSCALL (ds = opendir (SCM_CHARS (dirname)));
  if (ds == NULL)
    scm_syserror (s_opendir);
  SCM_SETCAR (dir, scm_tc16_dir | SCM_OPN);
  SCM_SETCDR (dir, ds);
  SCM_ALLOW_INTS;
  return dir;
}


SCM_PROC (s_readdir, "readdir", 1, 0, 0, scm_readdir);

SCM 
scm_readdir (port)
     SCM port;
{
  struct dirent *rdent;
  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPDIRP (port), port, SCM_ARG1, s_readdir);
  errno = 0;
  SCM_SYSCALL (rdent = readdir ((DIR *) SCM_CDR (port)));
  SCM_ALLOW_INTS;
  if (errno != 0)
    scm_syserror (s_readdir);
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



SCM_PROC (s_closedir, "closedir", 1, 0, 0, scm_closedir);

SCM 
scm_closedir (port)
     SCM port;
{
  int sts;

  SCM_ASSERT (SCM_NIMP (port) && SCM_DIRP (port), port, SCM_ARG1, s_closedir);
  SCM_DEFER_INTS;
  if (SCM_CLOSEDP (port))
    {
      SCM_ALLOW_INTS;
      return SCM_UNSPECIFIED;
    }
  SCM_SYSCALL (sts = closedir ((DIR *) SCM_CDR (port)));
  if (sts != 0)
    scm_syserror (s_closedir);
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


SCM_PROC (s_chdir, "chdir", 1, 0, 0, scm_chdir);

SCM 
scm_chdir (str)
     SCM str;
{
  int ans;

  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_chdir);
  SCM_SYSCALL (ans = chdir (SCM_CHARS (str)));
  if (ans != 0)
    scm_syserror (s_chdir);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_getcwd, "getcwd", 0, 0, 0, scm_getcwd);

SCM 
scm_getcwd ()
{
#ifdef HAVE_GETCWD
  char *rv;

  scm_sizet size = 100;
  char *wd;
  SCM result;

  SCM_DEFER_INTS;
  wd = scm_must_malloc (size, s_getcwd);
  while ((rv = getcwd (wd, size)) == 0 && errno == ERANGE)
    {
      scm_must_free (wd);
      size *= 2;
      wd = scm_must_malloc (size, s_getcwd);
    }
  if (rv == 0)
    scm_syserror (s_getcwd);
  result = scm_makfromstr (wd, strlen (wd), 0);
  scm_must_free (wd);
  SCM_ALLOW_INTS;
  return result;
#else
  scm_sysmissing (s_getcwd);
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
      list = SCM_CDR (list);
    }
  return answer;
}


/* {Checking for events}
 */

SCM_PROC (s_select, "select", 3, 2, 0, scm_select);

SCM
scm_select (reads, writes, excepts, secs, msecs)
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

  SCM_ASSERT (-1 < scm_ilength (reads), reads, SCM_ARG1, s_select);
  SCM_ASSERT (-1 < scm_ilength (writes), reads, SCM_ARG1, s_select);
  SCM_ASSERT (-1 < scm_ilength (excepts), reads, SCM_ARG1, s_select);

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
      SCM_ASSERT (SCM_INUMP (secs), secs, SCM_ARG4, s_select);
      if (SCM_UNBNDP (msecs))
	msecs = SCM_INUM0;
      else
	SCM_ASSERT (SCM_INUMP (msecs), msecs, SCM_ARG5, s_select);

      timeout.tv_sec = SCM_INUM (secs);
      timeout.tv_usec = 1000 * SCM_INUM (msecs);
      time_p = &timeout;
    }

  SCM_DEFER_INTS;
  sreturn = select (SELECT_SET_SIZE,
		    &read_set, &write_set, &except_set, time_p);
  if (sreturn < 0)
    scm_syserror (s_select);
  SCM_ALLOW_INTS;
  return scm_listify (retrieve_select_type (&read_set, reads),
		      retrieve_select_type (&write_set, writes),
		      retrieve_select_type (&except_set, excepts),
		      SCM_UNDEFINED);
#else
  scm_sysmissing (s_select);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}

/* Check if FILE has characters waiting to be read.  */

#ifdef __IBMC__
# define MSDOS
#endif
#ifdef MSDOS
# ifndef GO32
#  include <io.h>
#  include <conio.h>

int 
scm_input_waiting_p (f, caller)
     FILE *f;
     char *caller;
{
  if (feof (f))
    return 1;
  if (fileno (f) == fileno (stdin) && (isatty (fileno (stdin))))
    return kbhit ();
  return -1;
}

# endif
#else
# ifdef _DCC
#  include <ioctl.h>
# else
#  ifndef AMIGA
#   ifndef vms
#    ifdef MWC
#     include <sys/io.h>
#    else
#     ifndef THINK_C
#      ifndef ARM_ULIB
#       include <sys/ioctl.h>
#      endif
#     endif
#    endif
#   endif
#  endif
# endif

int
scm_input_waiting_p (f, caller)
     FILE *f;
     char *caller;
{
  /* Can we return an end-of-file character? */
  if (feof (f))
    return 1;

  /* Do we have characters in the stdio buffer? */
# ifdef FILE_CNT_FIELD
  if (f->FILE_CNT_FIELD > 0)
    return 1;
# else
#  ifdef FILE_CNT_GPTR
  if (f->_gptr != f->_egptr)
    return 1;
# else
#   ifdef FILE_CNT_READPTR
  if (f->_IO_read_end != f->_IO_read_ptr)
    return 1;
#   else
  Configure.in could not guess the name of the correct field in a FILE *.
  This function needs to be ported to your system.
  It should return zero iff no characters are waiting to be read.;
#   endif
#  endif
# endif

  /* Is the file prepared to deliver input? */
# ifdef HAVE_SELECT
  {
    struct timeval timeout;
    SELECT_TYPE read_set;
    SELECT_TYPE write_set;
    SELECT_TYPE except_set;
    int fno = fileno ((FILE *)f);

    FD_ZERO (&read_set);
    FD_ZERO (&write_set);
    FD_ZERO (&except_set);

    FD_SET (fno, &read_set);

    timeout.tv_sec = 0;
    timeout.tv_usec = 0;

    SCM_DEFER_INTS;
    if (select (SELECT_SET_SIZE,
		&read_set, &write_set, &except_set, &timeout)
	< 0)
      scm_syserror (caller);
    SCM_ALLOW_INTS;
    return FD_ISSET (fno, &read_set);
  }
# else
# ifdef FIONREAD
  {
    long remir;
    ioctl(fileno(f), FIONREAD, &remir);
    return remir;
  }
#  else    
  scm_misc_error ("char-ready?", "Not fully implemented\n");
#  endif
# endif
}
#endif


/* {Symbolic Links} 
 */

SCM_PROC (s_symlink, "symlink", 2, 0, 0, scm_symlink);

SCM
scm_symlink(oldpath, newpath)
     SCM oldpath;
     SCM newpath;
{
#ifdef HAVE_SYMLINK
  int val;

  SCM_ASSERT(SCM_NIMP(oldpath) && SCM_STRINGP(oldpath), oldpath, SCM_ARG1, s_symlink);
  SCM_ASSERT(SCM_NIMP(newpath) && SCM_STRINGP(newpath), newpath, SCM_ARG2, s_symlink);
  SCM_SYSCALL (val = symlink(SCM_CHARS(oldpath), SCM_CHARS(newpath)));
  if (val != 0)
    scm_syserror (s_symlink);
  return SCM_UNSPECIFIED;
#else
  scm_sysmissing (s_symlink);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_readlink, "readlink", 1, 0, 0, scm_readlink);

SCM
scm_readlink(path)
  SCM path;
{
#ifdef HAVE_READLINK
  scm_sizet rv;
  scm_sizet size = 100;
  char *buf;
  SCM result;
  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path),  path, (char *) SCM_ARG1, s_readlink);
  SCM_DEFER_INTS;
  buf = scm_must_malloc (size, s_readlink);
  while ((rv = readlink (SCM_CHARS (path), buf, (scm_sizet) size)) == size)
    {
      scm_must_free (buf);
      size *= 2;
      buf = scm_must_malloc (size, s_readlink);
    }
  if (rv == -1)
    scm_syserror (s_readlink);
  result = scm_makfromstr (buf, rv, 0);
  scm_must_free (buf);
  SCM_ALLOW_INTS;
  return result;
#else
  scm_sysmissing (s_readlink);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_lstat, "lstat", 1, 0, 0, scm_lstat);

SCM
scm_lstat(str)
  SCM str;
{
#ifdef HAVE_LSTAT
  int rv;
  struct stat stat_temp;

  SCM_ASSERT(SCM_NIMP(str) && SCM_STRINGP(str), str, (char *)SCM_ARG1, s_lstat);
  SCM_SYSCALL(rv = lstat(SCM_CHARS(str), &stat_temp));
  if (rv != 0)
    {
      int en = errno;

      scm_syserror_msg (s_lstat, "%s: %S",
			scm_listify (scm_makfrom0str (strerror (errno)),
				     str,
				     SCM_UNDEFINED),
			en);
    }
  return scm_stat2scm(&stat_temp);
#else 
  scm_sysmissing (s_lstat);
  /* not reached.  */
  return SCM_BOOL_F;
#endif
}


SCM_PROC (s_copy_file, "copy-file", 2, 0, 0, scm_copy_file);

SCM
scm_copy_file (oldfile, newfile)
     SCM oldfile;
     SCM newfile;
{
  int oldfd, newfd;
  int n;
  char buf[BUFSIZ];		/* this space could be shared.  */
  struct stat oldstat;

  SCM_ASSERT (SCM_NIMP (oldfile) && SCM_ROSTRINGP (oldfile), oldfile, SCM_ARG1, s_copy_file);
  if (SCM_SUBSTRP (oldfile))
    oldfile = scm_makfromstr (SCM_ROCHARS (oldfile), SCM_ROLENGTH (oldfile), 0);
  SCM_ASSERT (SCM_NIMP (newfile) && SCM_ROSTRINGP (newfile), newfile, SCM_ARG2, s_copy_file);
  if (SCM_SUBSTRP (newfile))
    newfile = scm_makfromstr (SCM_ROCHARS (newfile), SCM_ROLENGTH (newfile), 0);
  if (stat (SCM_ROCHARS (oldfile), &oldstat) == -1)
    scm_syserror (s_copy_file);
  SCM_DEFER_INTS;
  oldfd = open (SCM_ROCHARS (oldfile), O_RDONLY);
  if (oldfd == -1)
    scm_syserror (s_copy_file);

  /* use POSIX flags instead of 07777?.  */
  newfd = open (SCM_ROCHARS (newfile), O_WRONLY | O_CREAT | O_TRUNC,
		oldstat.st_mode & 07777);
  if (newfd == -1)
    scm_syserror (s_copy_file);

  while ((n = read (oldfd, buf, sizeof buf)) > 0)
    if (write (newfd, buf, n) != n)
      {
	close (oldfd);
	close (newfd);
	scm_syserror (s_copy_file);
      }
  close (oldfd);
  if (close (newfd) == -1)
    scm_syserror (s_copy_file);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}



void
scm_init_filesys ()
{
  scm_add_feature ("i/o-extensions");

  scm_tc16_dir = scm_newsmob (&dir_smob);

#ifdef O_RDONLY
scm_sysintern ("O_RDONLY", scm_long2num (O_RDONLY));
#endif 	       
#ifdef O_WRONLY
scm_sysintern ("O_WRONLY", scm_long2num (O_WRONLY));
#endif 	       
#ifdef O_RDWR
scm_sysintern ("O_RDWR", scm_long2num (O_RDWR));
#endif 	       
#ifdef O_CREAT
scm_sysintern ("O_CREAT", scm_long2num (O_CREAT));
#endif 	       
#ifdef O_EXCL  
scm_sysintern ("O_EXCL", scm_long2num (O_EXCL));
#endif 	       
#ifdef O_NOCTTY
scm_sysintern ("O_NOCTTY", scm_long2num (O_NOCTTY));
#endif 	       
#ifdef O_TRUNC 
scm_sysintern ("O_TRUNC", scm_long2num (O_TRUNC));
#endif 	       
#ifdef O_APPEND
scm_sysintern ("O_APPEND", scm_long2num (O_APPEND));
#endif 	       
#ifdef O_NONBLO
scm_sysintern ("O_NONBLOCK", scm_long2num (O_NONBLOCK));
#endif 	       
#ifdef O_NDELAY
scm_sysintern ("O_NDELAY", scm_long2num (O_NDELAY));
#endif 	       
#ifdef O_SYNC  
scm_sysintern ("O_SYNC", scm_long2num (O_SYNC));
#endif 



#include "filesys.x"
}
