/* Copyright (C) 1996,1997,1998,1999,2000,2001 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/smob.h"
#include "libguile/feature.h"
#include "libguile/fports.h"
#include "libguile/iselect.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/filesys.h"


#ifdef HAVE_IO_H
#include <io.h>
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

/* Ultrix has S_IFSOCK, but no S_ISSOCK.  Ipe!  */
#if defined (S_IFSOCK) && ! defined (S_ISSOCK)
#define S_ISSOCK(mode) (((mode) & S_IFMT) == S_IFSOCK)
#endif





/* {Permissions}
 */

SCM_DEFINE (scm_chown, "chown", 3, 0, 0, 
            (SCM object, SCM owner, SCM group),
	    "Change the ownership and group of the file referred to by @var{object} to\n"
	    "the integer values @var{owner} and @var{group}.  @var{object} can be\n"
	    "a string containing a file name or, if the platform\n"
	    "supports fchown, a port or integer file descriptor\n"
	    "which is open on the file.  The return value\n"
	    "is unspecified.\n\n"
	    "If @var{object} is a symbolic link, either the\n"
	    "ownership of the link or the ownership of the referenced file will be\n"
	    "changed depending on the operating system (lchown is\n"
	    "unsupported at present).  If @var{owner} or @var{group} is specified\n"
	    "as @code{-1}, then that ID is not changed.")
#define FUNC_NAME s_scm_chown
{
  int rv;

  object = SCM_COERCE_OUTPORT (object);

  SCM_VALIDATE_INUM (2,owner);
  SCM_VALIDATE_INUM (3,group);
#ifdef HAVE_FCHOWN
  if (SCM_INUMP (object) || (SCM_OPFPORTP (object)))
    {
      int fdes = SCM_INUMP (object) ? SCM_INUM (object)
	: SCM_FPORT_FDES (object);

      SCM_SYSCALL (rv = fchown (fdes, SCM_INUM (owner), SCM_INUM (group)));
    }
  else
#endif
    {
      SCM_VALIDATE_STRING (1, object);
      SCM_STRING_COERCE_0TERMINATION_X (object);
      SCM_SYSCALL (rv = chown (SCM_STRING_CHARS (object),
			       SCM_INUM (owner), SCM_INUM (group)));
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_chmod, "chmod", 2, 0, 0,
            (SCM object, SCM mode),
	    "Changes the permissions of the file referred to by @var{obj}.\n"
	    "@var{obj} can be a string containing a file name or a port or integer file\n"
	    "descriptor which is open on a file (in which case @code{fchmod} is used\n"
	    "as the underlying system call).\n"
	    "@var{mode} specifies\n"
	    "the new permissions as a decimal number, e.g., @code{(chmod \"foo\" #o755)}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_chmod
{
  int rv;
  int fdes;

  object = SCM_COERCE_OUTPORT (object);

  SCM_VALIDATE_INUM (2,mode);
  if (SCM_INUMP (object) || SCM_OPFPORTP (object))
    {
      if (SCM_INUMP (object))
	fdes = SCM_INUM (object);
      else
	fdes = SCM_FPORT_FDES (object);
      SCM_SYSCALL (rv = fchmod (fdes, SCM_INUM (mode)));
    }
  else
    {
      SCM_VALIDATE_STRING (1, object);
      SCM_STRING_COERCE_0TERMINATION_X (object);
      SCM_SYSCALL (rv = chmod (SCM_STRING_CHARS (object), SCM_INUM (mode)));
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_umask, "umask", 0, 1, 0, 
            (SCM mode),
	    "If @var{mode} is omitted, retuns a decimal number representing the current\n"
	    "file creation mask.  Otherwise the file creation mask is set to\n"
	    "@var{mode} and the previous value is returned.\n\n"
	    "E.g., @code{(umask #o022)} sets the mask to octal 22, decimal 18.")
#define FUNC_NAME s_scm_umask
{
  mode_t mask;
  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
    }
  else
    {
      SCM_VALIDATE_INUM (1,mode);
      mask = umask (SCM_INUM (mode));
    }
  return SCM_MAKINUM (mask);
}
#undef FUNC_NAME



SCM_DEFINE (scm_open_fdes, "open-fdes", 2, 1, 0, 
            (SCM path, SCM flags, SCM mode),
	    "Similar to @code{open} but return a file descriptor instead of\n"
	    "a port.")
#define FUNC_NAME s_scm_open_fdes
{
  int fd;
  int iflags;
  int imode;

  SCM_VALIDATE_STRING (1, path);
  SCM_STRING_COERCE_0TERMINATION_X (path);
  iflags = SCM_NUM2INT (2, flags);
  imode = SCM_NUM2INT_DEF (3, mode, 0666);
  SCM_SYSCALL (fd = open (SCM_STRING_CHARS (path), iflags, imode));
  if (fd == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (fd);
}
#undef FUNC_NAME

SCM_DEFINE (scm_open, "open", 2, 1, 0, 
            (SCM path, SCM flags, SCM mode),
	    "Open the file named by @var{path} for reading and/or writing.\n"
	    "@var{flags} is an integer specifying how the file should be opened.\n"
	    "@var{mode} is an integer specifying the permission bits of the file, if\n"
	    "it needs to be created, before the umask is applied.  The default is 666\n"
	    "(Unix itself has no default).\n\n"
	    "@var{flags} can be constructed by combining variables using @code{logior}.\n"
	    "Basic flags are:\n\n"
	    "@defvar O_RDONLY\n"
	    "Open the file read-only.\n"
	    "@end defvar\n"
	    "@defvar O_WRONLY\n"
	    "Open the file write-only. \n"
	    "@end defvar\n"
	    "@defvar O_RDWR\n"
	    "Open the file read/write.\n"
	    "@end defvar\n"
	    "@defvar O_APPEND\n"
	    "Append to the file instead of truncating.\n"
	    "@end defvar\n"
	    "@defvar O_CREAT\n"
	    "Create the file if it does not already exist.\n"
	    "@end defvar\n\n"
	    "See the Unix documentation of the @code{open} system call\n"
	    "for additional flags.")
#define FUNC_NAME s_scm_open
{
  SCM newpt;
  char *port_mode;
  int fd;
  int iflags;

  fd = SCM_INUM (scm_open_fdes (path, flags, mode));
  iflags = SCM_NUM2INT (2, flags);
  if (iflags & O_RDWR)
    {
      if (iflags & O_APPEND)
	port_mode = "a+";
      else if (iflags & O_CREAT)
	port_mode = "w+";
      else
	port_mode = "r+";
    }
  else {
    if (iflags & O_APPEND)
      port_mode = "a";
    else if (iflags & O_WRONLY)
      port_mode = "w";
    else
      port_mode = "r";
  }
  newpt = scm_fdes_to_port (fd, port_mode, path);
  return newpt;
}
#undef FUNC_NAME

SCM_DEFINE (scm_close, "close", 1, 0, 0, 
            (SCM fd_or_port),
	    "Similar to close-port (@pxref{Generic Port Operations, close-port}),\n"
	    "but also works on file descriptors.  A side\n"
	    "effect of closing a file descriptor is that any ports using that file\n"
	    "descriptor are moved to a different file descriptor and have\n"
	    "their revealed counts set to zero.")
#define FUNC_NAME s_scm_close
{
  int rv;
  int fd;

  fd_or_port = SCM_COERCE_OUTPORT (fd_or_port);

  if (SCM_PORTP (fd_or_port))
    return scm_close_port (fd_or_port);
  SCM_VALIDATE_INUM (1,fd_or_port);
  fd = SCM_INUM (fd_or_port);
  scm_evict_ports (fd);		/* see scsh manual.  */
  SCM_SYSCALL (rv = close (fd));
  /* following scsh, closing an already closed file descriptor is
     not an error.  */
  if (rv < 0 && errno != EBADF)
    SCM_SYSERROR;
  return SCM_NEGATE_BOOL(rv < 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_close_fdes, "close-fdes", 1, 0, 0, 
            (SCM fd),
	    "A simple wrapper for the @code{close} system call.\n"
	    "Close file descriptor @var{fd}, which must be an integer.\n"
	    "Unlike close (@pxref{Ports and File Descriptors, close}),\n"
	    "the file descriptor will be closed even if a port is using it.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_close_fdes
{
  int c_fd;
  int rv;

  SCM_VALIDATE_INUM_COPY (1, fd, c_fd);
  SCM_SYSCALL (rv = close (c_fd));
  if (rv < 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* {Files}
 */

SCM_SYMBOL (scm_sym_regular, "regular");
SCM_SYMBOL (scm_sym_directory, "directory");
#ifdef HAVE_S_ISLNK
SCM_SYMBOL (scm_sym_symlink, "symlink");
#endif
SCM_SYMBOL (scm_sym_block_special, "block-special");
SCM_SYMBOL (scm_sym_char_special, "char-special");
SCM_SYMBOL (scm_sym_fifo, "fifo");
SCM_SYMBOL (scm_sym_sock, "socket");
SCM_SYMBOL (scm_sym_unknown, "unknown");

static SCM 
scm_stat2scm (struct stat *stat_temp)
{
  SCM ans = scm_c_make_vector (15, SCM_UNSPECIFIED);
  SCM *ve = SCM_VELTS (ans);
  
  ve[0] = scm_ulong2num ((unsigned long) stat_temp->st_dev);
  ve[1] = scm_ulong2num ((unsigned long) stat_temp->st_ino);
  ve[2] = scm_ulong2num ((unsigned long) stat_temp->st_mode);
  ve[3] = scm_ulong2num ((unsigned long) stat_temp->st_nlink);
  ve[4] = scm_ulong2num ((unsigned long) stat_temp->st_uid);
  ve[5] = scm_ulong2num ((unsigned long) stat_temp->st_gid);
#ifdef HAVE_STRUCT_STAT_ST_RDEV
  ve[6] = scm_ulong2num ((unsigned long) stat_temp->st_rdev);
#else
  ve[6] = SCM_BOOL_F;
#endif
  ve[7] = scm_ulong2num ((unsigned long) stat_temp->st_size);
  ve[8] = scm_ulong2num ((unsigned long) stat_temp->st_atime);
  ve[9] = scm_ulong2num ((unsigned long) stat_temp->st_mtime);
  ve[10] = scm_ulong2num ((unsigned long) stat_temp->st_ctime);
#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
  ve[11] = scm_ulong2num ((unsigned long) stat_temp->st_blksize);
#else
  ve[11] = scm_ulong2num (4096L);
#endif
#ifdef HAVE_STRUCT_STAT_ST_BLOCKS
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
#ifdef HAVE_S_ISLNK
    else if (S_ISLNK (mode))
      ve[13] = scm_sym_symlink;
#endif
    else if (S_ISBLK (mode))
      ve[13] = scm_sym_block_special;
    else if (S_ISCHR (mode))
      ve[13] = scm_sym_char_special;
    else if (S_ISFIFO (mode))
      ve[13] = scm_sym_fifo;
#ifdef S_ISSOCK
    else if (S_ISSOCK (mode))
      ve[13] = scm_sym_sock;
#endif
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

SCM_DEFINE (scm_stat, "stat", 1, 0, 0, 
            (SCM object),
	    "Return an object containing various information about the file\n"
	    "determined by @var{obj}.  @var{obj} can be a string containing\n"
	    "a file name or a port or integer file descriptor which is open\n"
	    "on a file (in which case @code{fstat} is used as the underlying\n"
	    "system call).\n"
	    "\n"
	    "The object returned by @code{stat} can be passed as a single\n"
	    "parameter to the following procedures, all of which return\n"
	    "integers:\n"
	    "\n"
	    "@table @code\n"
	    "@item stat:dev\n"
	    "The device containing the file.\n"
	    "@item stat:ino\n"
	    "The file serial number, which distinguishes this file from all\n"
	    "other files on the same device.\n"
	    "@item stat:mode\n"
	    "The mode of the file.  This includes file type information and\n"
	    "the file permission bits.  See @code{stat:type} and\n"
	    "@code{stat:perms} below.\n"
	    "@item stat:nlink\n"
	    "The number of hard links to the file.\n"
	    "@item stat:uid\n"
	    "The user ID of the file's owner.\n"
	    "@item stat:gid\n"
	    "The group ID of the file.\n"
	    "@item stat:rdev\n"
	    "Device ID; this entry is defined only for character or block\n"
	    "special files.\n"
	    "@item stat:size\n"
	    "The size of a regular file in bytes.\n"
	    "@item stat:atime\n"
	    "The last access time for the file.\n"
	    "@item stat:mtime\n"
	    "The last modification time for the file.\n"
	    "@item stat:ctime\n"
	    "The last modification time for the attributes of the file.\n"
	    "@item stat:blksize\n"
	    "The optimal block size for reading or writing the file, in\n"
	    "bytes.\n"
	    "@item stat:blocks\n"
	    "The amount of disk space that the file occupies measured in\n"
	    "units of 512 byte blocks.\n"
	    "@end table\n"
	    "\n"
	    "In addition, the following procedures return the information\n"
	    "from stat:mode in a more convenient form:\n"
	    "\n"
	    "@table @code\n"
	    "@item stat:type\n"
	    "A symbol representing the type of file.  Possible values are\n"
	    "regular, directory, symlink, block-special, char-special, fifo,\n"
	    "socket and unknown\n"
	    "@item stat:perms\n"
	    "An integer representing the access permission bits.\n"
	    "@end table")
#define FUNC_NAME s_scm_stat
{
  int rv;
  int fdes;
  struct stat stat_temp;

  if (SCM_INUMP (object))
    SCM_SYSCALL (rv = fstat (SCM_INUM (object), &stat_temp));
  else
    {
      SCM_VALIDATE_NIM (1,object);
      if (SCM_STRINGP (object))
	{
	  SCM_STRING_COERCE_0TERMINATION_X (object);
	  SCM_SYSCALL (rv = stat (SCM_STRING_CHARS (object), &stat_temp));
	}
      else
	{
	  object = SCM_COERCE_OUTPORT (object);
          SCM_VALIDATE_OPFPORT(1,object);
	  fdes = SCM_FPORT_FDES (object);
	  SCM_SYSCALL (rv = fstat (fdes, &stat_temp));
	}
    }
  if (rv == -1)
    {
      int en = errno;

      SCM_SYSERROR_MSG ("~A: ~S",
			SCM_LIST2 (scm_makfrom0str (strerror (errno)), object),
			en);
    }
  return scm_stat2scm (&stat_temp);
}
#undef FUNC_NAME


/* {Modifying Directories}
 */

SCM_DEFINE (scm_link, "link", 2, 0, 0,
            (SCM oldpath, SCM newpath),
	    "Creates a new name @var{newpath} in the file system for the\n"
	    "file named by @var{oldpath}.  If @var{oldpath} is a symbolic\n"
	    "link, the link may or may not be followed depending on the\n"
	    "system.")
#define FUNC_NAME s_scm_link
{
  int val;

  SCM_VALIDATE_STRING (1, oldpath);
  SCM_STRING_COERCE_0TERMINATION_X (oldpath);
  SCM_VALIDATE_STRING (2, newpath);
  SCM_STRING_COERCE_0TERMINATION_X (newpath);
  SCM_SYSCALL (val = link (SCM_STRING_CHARS (oldpath),
			   SCM_STRING_CHARS (newpath)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_rename, "rename-file", 2, 0, 0,
            (SCM oldname, SCM newname),
	    "Renames the file specified by @var{oldname} to @var{newname}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_rename
{
  int rv;
  SCM_VALIDATE_STRING (1, oldname);
  SCM_VALIDATE_STRING (2, newname);
  SCM_STRING_COERCE_0TERMINATION_X (oldname);
  SCM_STRING_COERCE_0TERMINATION_X (newname);
#ifdef HAVE_RENAME
  SCM_SYSCALL (rv = rename (SCM_STRING_CHARS (oldname), SCM_STRING_CHARS (newname)));
#else
  SCM_SYSCALL (rv = link (SCM_STRING_CHARS (oldname), SCM_STRING_CHARS (newname)));
  if (rv == 0)
    {
      SCM_SYSCALL (rv = unlink (SCM_STRING_CHARS (oldname)));;
      if (rv != 0)
	/* unlink failed.  remove new name */
	SCM_SYSCALL (unlink (SCM_STRING_CHARS (newname))); 
    }
#endif
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delete_file, "delete-file", 1, 0, 0, 
           (SCM str),
	    "Deletes (or \"unlinks\") the file specified by @var{path}.")
#define FUNC_NAME s_scm_delete_file
{
  int ans;
  SCM_VALIDATE_STRING (1, str);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  SCM_SYSCALL (ans = unlink (SCM_STRING_CHARS (str)));
  if (ans != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_MKDIR
SCM_DEFINE (scm_mkdir, "mkdir", 1, 1, 0,
            (SCM path, SCM mode),
	    "Create a new directory named by @var{path}.  If @var{mode} is omitted\n"
	    "then the permissions of the directory file are set using the current\n"
	    "umask.  Otherwise they are set to the decimal value specified with\n"
	    "@var{mode}.  The return value is unspecified.")
#define FUNC_NAME s_scm_mkdir
{
  int rv;
  mode_t mask;
  SCM_VALIDATE_STRING (1, path);
  SCM_STRING_COERCE_0TERMINATION_X (path);
  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
      SCM_SYSCALL (rv = mkdir (SCM_STRING_CHARS (path), 0777 ^ mask));
    }
  else
    {
      SCM_VALIDATE_INUM (2,mode);
      SCM_SYSCALL (rv = mkdir (SCM_STRING_CHARS (path), SCM_INUM (mode)));
    }
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_MKDIR */

#ifdef HAVE_RMDIR
SCM_DEFINE (scm_rmdir, "rmdir", 1, 0, 0, 
            (SCM path),
	    "Remove the existing directory named by @var{path}.  The directory must\n"
	    "be empty for this to succeed.  The return value is unspecified.")
#define FUNC_NAME s_scm_rmdir
{
  int val;

  SCM_VALIDATE_STRING (1, path);
  SCM_STRING_COERCE_0TERMINATION_X (path);
  SCM_SYSCALL (val = rmdir (SCM_STRING_CHARS (path)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif



/* {Examining Directories}
 */

scm_bits_t scm_tc16_dir;


SCM_DEFINE (scm_directory_stream_p, "directory-stream?", 1, 0, 0, 
            (SCM obj),
	    "Return a boolean indicating whether @var{object} is a directory\n"
	    "stream as returned by @code{opendir}.")
#define FUNC_NAME s_scm_directory_stream_p
{
  return SCM_BOOL (SCM_DIRP (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_opendir, "opendir", 1, 0, 0, 
            (SCM dirname),
	    "Open the directory specified by @var{path} and return a directory\n"
	    "stream.")
#define FUNC_NAME s_scm_opendir
{
  DIR *ds;
  SCM_VALIDATE_STRING (1, dirname);
  SCM_STRING_COERCE_0TERMINATION_X (dirname);
  SCM_SYSCALL (ds = opendir (SCM_STRING_CHARS (dirname)));
  if (ds == NULL)
    SCM_SYSERROR;
  SCM_RETURN_NEWSMOB (scm_tc16_dir | SCM_DIR_FLAG_OPEN, ds);
}
#undef FUNC_NAME


SCM_DEFINE (scm_readdir, "readdir", 1, 0, 0, 
            (SCM port),
	    "Return (as a string) the next directory entry from the directory stream\n"
	    "@var{stream}.  If there is no remaining entry to be read then the\n"
	    "end of file object is returned.")
#define FUNC_NAME s_scm_readdir
{
  struct dirent *rdent;

  SCM_VALIDATE_DIR (1, port);
  if (!SCM_DIR_OPEN_P (port))
    SCM_MISC_ERROR ("Directory ~S is not open.", SCM_LIST1 (port));

  errno = 0;
  SCM_SYSCALL (rdent = readdir ((DIR *) SCM_CELL_WORD_1 (port)));
  if (errno != 0)
    SCM_SYSERROR;

  return (rdent ? scm_makfromstr (rdent->d_name, NAMLEN (rdent), 0)
	  : SCM_EOF_VAL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_rewinddir, "rewinddir", 1, 0, 0, 
            (SCM port),
	    "Reset the directory port @var{stream} so that the next call to\n"
	    "@code{readdir} will return the first directory entry.")
#define FUNC_NAME s_scm_rewinddir
{
  SCM_VALIDATE_DIR (1, port);
  if (!SCM_DIR_OPEN_P (port))
    SCM_MISC_ERROR ("Directory ~S is not open.", SCM_LIST1 (port));

  rewinddir ((DIR *) SCM_CELL_WORD_1 (port));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_closedir, "closedir", 1, 0, 0, 
            (SCM port),
	    "Close the directory stream @var{stream}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_closedir
{
  SCM_VALIDATE_DIR (1, port);

  if (SCM_DIR_OPEN_P (port))
    {
      int sts;

      SCM_SYSCALL (sts = closedir ((DIR *) SCM_CELL_WORD_1 (port)));
      if (sts != 0)
	SCM_SYSERROR;

      SCM_SET_CELL_WORD_0 (port, scm_tc16_dir);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int 
scm_dir_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<", port);
  if (!SCM_DIR_OPEN_P (exp))
    scm_puts ("closed: ", port);
  scm_puts ("directory stream ", port);
  scm_intprint (SCM_CELL_WORD_1 (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}


static size_t 
scm_dir_free (SCM p)
{
  if (SCM_DIR_OPEN_P (p))
    closedir ((DIR *) SCM_CELL_WORD_1 (p));
  return 0;
}


/* {Navigating Directories}
 */


SCM_DEFINE (scm_chdir, "chdir", 1, 0, 0, 
            (SCM str),
	    "Change the current working directory to @var{path}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_chdir
{
  int ans;

  SCM_VALIDATE_STRING (1, str);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  SCM_SYSCALL (ans = chdir (SCM_STRING_CHARS (str)));
  if (ans != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_GETCWD
SCM_DEFINE (scm_getcwd, "getcwd", 0, 0, 0,
            (),
	    "Return the name of the current working directory.")
#define FUNC_NAME s_scm_getcwd
{
  char *rv;
  size_t size = 100;
  char *wd;
  SCM result;

  wd = scm_must_malloc (size, FUNC_NAME);
  while ((rv = getcwd (wd, size)) == 0 && errno == ERANGE)
    {
      scm_must_free (wd);
      size *= 2;
      wd = scm_must_malloc (size, FUNC_NAME);
    }
  if (rv == 0)
    SCM_SYSERROR;
  result = scm_makfromstr (wd, strlen (wd), 0);
  scm_must_free (wd);
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_GETCWD */



#ifdef HAVE_SELECT

/* check that element is a port or file descriptor.  if it's a port
   and its buffer is ready for use, add it to the ports_ready list.
   otherwise add its file descriptor to *set.  the type of list can be
   determined from pos: SCM_ARG1 for reads, SCM_ARG2 for writes,
   SCM_ARG3 for excepts.  */
static int
set_element (SELECT_TYPE *set, SCM *ports_ready, SCM element, int pos)
{
  int fd;

  if (SCM_INUMP (element))
    {
      fd = SCM_INUM (element);
    }
  else
    {
      int use_buf = 0;

      element = SCM_COERCE_OUTPORT (element);
      SCM_ASSERT (SCM_OPFPORTP (element), element, pos, "select");
      if (pos == SCM_ARG1)
	{
	  /* check whether port has buffered input.  */
	  scm_port_t *pt = SCM_PTAB_ENTRY (element);
      
	  if (pt->read_pos < pt->read_end)
	    use_buf = 1;
	}
      else if (pos == SCM_ARG2)
	{
	  /* check whether port's output buffer has room.  */
	  scm_port_t *pt = SCM_PTAB_ENTRY (element);

	  /* > 1 since writing the last byte in the buffer causes flush.  */
	  if (pt->write_end - pt->write_pos > 1)
	    use_buf = 1;
	}
      fd = use_buf ? -1 : SCM_FPORT_FDES (element);
    }
  if (fd == -1)
    *ports_ready = scm_cons (element, *ports_ready);
  else
    FD_SET (fd, set);
  return fd;
}

/* check list_or_vec, a list or vector of ports or file descriptors,
   adding each member to either the ports_ready list (if it's a port
   with a usable buffer) or to *set.  the kind of list_or_vec can be
   determined from pos: SCM_ARG1 for reads, SCM_ARG2 for writes,
   SCM_ARG3 for excepts.  */
static int
fill_select_type (SELECT_TYPE *set, SCM *ports_ready, SCM list_or_vec, int pos)
{
  int max_fd = 0;

  if (SCM_VECTORP (list_or_vec))
    {
      int i = SCM_VECTOR_LENGTH (list_or_vec);
      SCM *ve = SCM_VELTS (list_or_vec);
      
      while (--i >= 0)
	{
	  int fd = set_element (set, ports_ready, ve[i], pos);

	  if (fd > max_fd)
	    max_fd = fd;
	}
    }
  else
    {
      while (!SCM_NULLP (list_or_vec))
	{
	  int fd = set_element (set, ports_ready, SCM_CAR (list_or_vec), pos);

	  if (fd > max_fd)
	    max_fd = fd;
	  list_or_vec = SCM_CDR (list_or_vec);
	}
    }

  return max_fd;
}

/* if element (a file descriptor or port) appears in *set, cons it to
   list.  return list.  */
static SCM
get_element (SELECT_TYPE *set, SCM element, SCM list)
{
  int fd;

  if (SCM_INUMP (element))
    {
      fd = SCM_INUM (element);
    }
  else
    {
      fd = SCM_FPORT_FDES (SCM_COERCE_OUTPORT (element));
    }
  if (FD_ISSET (fd, set))
    list = scm_cons (element, list);
  return list;
}

/* construct component of scm_select return value.
   set: pointer to set of file descriptors found by select to be ready
   ports_ready: ports ready due to buffering
   list_or_vec: original list/vector handed to scm_select.
   the return value is a list/vector of ready ports/file descriptors. 
   works by finding the objects in list which correspond to members of
   *set and appending them to ports_ready.  result is converted to a
   vector if list_or_vec is a vector.  */
static SCM 
retrieve_select_type (SELECT_TYPE *set, SCM ports_ready, SCM list_or_vec)
{
  SCM answer_list = ports_ready;

  if (SCM_VECTORP (list_or_vec))
    {
      int i = SCM_VECTOR_LENGTH (list_or_vec);
      SCM *ve = SCM_VELTS (list_or_vec);

      while (--i >= 0)
	{
	  answer_list = get_element (set, ve[i], answer_list);
	}
      return scm_vector (answer_list);
    }
  else
    {
      /* list_or_vec must be a list.  */
      while (!SCM_NULLP (list_or_vec))
	{
	  answer_list = get_element (set, SCM_CAR (list_or_vec), answer_list);
	  list_or_vec = SCM_CDR (list_or_vec);
	}
      return answer_list;
    }
}

/* Static helper functions above refer to s_scm_select directly as s_select */
SCM_DEFINE (scm_select, "select", 3, 2, 0, 
            (SCM reads, SCM writes, SCM excepts, SCM secs, SCM usecs),
	    "This procedure has a variety of uses: waiting for the ability\n"
	    "to provide input, accept output, or the existance of\n"
	    "exceptional conditions on a collection of ports or file\n"
	    "descriptors, or waiting for a timeout to occur.\n"
	    "It also returns if interrupted by a signal.\n\n"
	    "@var{reads}, @var{writes} and @var{excepts} can be lists or\n"
	    "vectors, with each member a port or a file descriptor.\n"
	    "The value returned is a list of three corresponding\n"
	    "lists or vectors containing only the members which meet the\n"
	    "specified requirement.  The ability of port buffers to\n"
	    "provide input or accept output is taken into account.\n"
	    "Ordering of the input lists or vectors is not preserved.\n\n"
	    "The optional arguments @var{secs} and @var{usecs} specify the\n"
	    "timeout.  Either @var{secs} can be specified alone, as\n"
	    "either an integer or a real number, or both @var{secs} and\n"
	    "@var{usecs} can be specified as integers, in which case\n"
	    "@var{usecs} is an additional timeout expressed in\n"
	    "microseconds.  If @var{secs} is omitted or is @code{#f} then\n"
	    "select will wait for as long as it takes for one of the other\n"
	    "conditions to be satisfied.\n\n"
	    "The scsh version of @code{select} differs as follows:\n"
	    "Only vectors are accepted for the first three arguments.\n"
	    "The @var{usecs} argument is not supported.\n"
	    "Multiple values are returned instead of a list.\n"
	    "Duplicates in the input vectors appear only once in output.\n"
	    "An additional @code{select!} interface is provided.\n"
	    )
#define FUNC_NAME s_scm_select
{
  struct timeval timeout;
  struct timeval * time_ptr;
  SELECT_TYPE read_set;
  SELECT_TYPE write_set;
  SELECT_TYPE except_set;
  int read_count;
  int write_count;
  int except_count;
  /* these lists accumulate ports which are ready due to buffering.
     their file descriptors don't need to be added to the select sets.  */
  SCM read_ports_ready = SCM_EOL;
  SCM write_ports_ready = SCM_EOL;
  int max_fd;

  if (SCM_VECTORP (reads))
    {
      read_count = SCM_VECTOR_LENGTH (reads);
    }
  else
    {
      read_count = scm_ilength (reads);
      SCM_ASSERT (read_count >= 0, reads, SCM_ARG1, FUNC_NAME);
    }
  if (SCM_VECTORP (writes))
    {
      write_count = SCM_VECTOR_LENGTH (writes);
    }
  else
    {
      write_count = scm_ilength (writes);
      SCM_ASSERT (write_count >= 0, writes, SCM_ARG2, FUNC_NAME);
    }
  if (SCM_VECTORP (excepts))
    {
      except_count = SCM_VECTOR_LENGTH (excepts);
    }
  else
    {
      except_count = scm_ilength (excepts);
      SCM_ASSERT (except_count >= 0, excepts, SCM_ARG3, FUNC_NAME);
    }

  FD_ZERO (&read_set);
  FD_ZERO (&write_set);
  FD_ZERO (&except_set);

  max_fd = fill_select_type (&read_set, &read_ports_ready, reads, SCM_ARG1);

  {
    int write_max = fill_select_type (&write_set, &write_ports_ready, 
				      writes, SCM_ARG2);
    int except_max = fill_select_type (&except_set, NULL,
				       excepts, SCM_ARG3);

    if (write_max > max_fd)
      max_fd = write_max;
    if (except_max > max_fd)
      max_fd = except_max;
  }

  /* if there's a port with a ready buffer, don't block, just
     check for ready file descriptors.  */
  if (!SCM_NULLP (read_ports_ready) || !SCM_NULLP (write_ports_ready))
    {
      timeout.tv_sec = 0;
      timeout.tv_usec = 0;
      time_ptr = &timeout;
    }
  else if (SCM_UNBNDP (secs) || SCM_FALSEP (secs))
    time_ptr = 0;
  else
    {
      if (SCM_INUMP (secs))
	{
	  timeout.tv_sec = SCM_INUM (secs);
	  if (SCM_UNBNDP (usecs))
	    timeout.tv_usec = 0;
	  else
	    {
              SCM_VALIDATE_INUM (5,usecs);
	      timeout.tv_usec = SCM_INUM (usecs);
	    }
	}
      else
	{
	  double fl = scm_num2dbl (secs, FUNC_NAME);

	  if (!SCM_UNBNDP (usecs))
	    SCM_WRONG_TYPE_ARG (4, secs);
	  if (fl > LONG_MAX)
	    SCM_OUT_OF_RANGE (4, secs);
	  timeout.tv_sec = (long) fl;
	  timeout.tv_usec = (long) ((fl - timeout.tv_sec) * 1000000);
	}
      time_ptr = &timeout;
    }

  {
#ifdef GUILE_ISELECT
    int rv = scm_internal_select (max_fd + 1,
				  &read_set, &write_set, &except_set,
				  time_ptr);
#else
    int rv = select (max_fd + 1,
		     &read_set, &write_set, &except_set, time_ptr);
#endif
    if (rv < 0)
      SCM_SYSERROR;
  }
  return SCM_LIST3 (retrieve_select_type (&read_set, read_ports_ready, reads),
		    retrieve_select_type (&write_set, write_ports_ready, writes),
		    retrieve_select_type (&except_set, SCM_EOL, excepts));
}
#undef FUNC_NAME
#endif /* HAVE_SELECT */



SCM_DEFINE (scm_fcntl, "fcntl", 2, 1, 0,
            (SCM object, SCM cmd, SCM value),
	    "Apply @var{command} to the specified file descriptor or the underlying\n"
	    "file descriptor of the specified port.  @var{value} is an optional\n"
	    "integer argument.\n\n"
	    "Values for @var{command} are:\n\n"
	    "@table @code\n"
	    "@item F_DUPFD\n"
	    "Duplicate a file descriptor\n"
	    "@item F_GETFD\n"
	    "Get flags associated with the file descriptor.\n"
	    "@item F_SETFD\n"
	    "Set flags associated with the file descriptor to @var{value}.\n"
	    "@item F_GETFL\n"
	    "Get flags associated with the open file.\n"
	    "@item F_SETFL\n"
	    "Set flags associated with the open file to @var{value}\n"
	    "@item F_GETOWN\n"
	    "Get the process ID of a socket's owner, for @code{SIGIO} signals.\n"
	    "@item F_SETOWN\n"
	    "Set the process that owns a socket to @var{value}, for @code{SIGIO} signals.\n"
	    "@item FD_CLOEXEC\n"
	    "The value used to indicate the \"close on exec\" flag with @code{F_GETFL} or\n"
	    "@code{F_SETFL}.\n"
	    "@end table")
#define FUNC_NAME s_scm_fcntl
{
  int rv;
  int fdes;
  int ivalue;

  object = SCM_COERCE_OUTPORT (object);

  SCM_VALIDATE_INUM (2,cmd);
  if (SCM_OPFPORTP (object))
    fdes = SCM_FPORT_FDES (object);
  else
    {
      SCM_VALIDATE_INUM (1,object);
      fdes = SCM_INUM (object);
    }

  if (SCM_UNBNDP (value)) {
    ivalue = 0;
  } else {
    SCM_VALIDATE_INUM_COPY (SCM_ARG3, value, ivalue);
  }

  SCM_SYSCALL (rv = fcntl (fdes, SCM_INUM (cmd), ivalue));
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fsync, "fsync", 1, 0, 0, 
            (SCM object),
	    "Copies any unwritten data for the specified output file descriptor to disk.\n"
	    "If @var{port/fd} is a port, its buffer is flushed before the underlying\n"
	    "file descriptor is fsync'd.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_fsync
{
  int fdes;

  object = SCM_COERCE_OUTPORT (object);

  if (SCM_OPFPORTP (object))
    {
      scm_flush (object);
      fdes = SCM_FPORT_FDES (object);
    }
  else
    {
      SCM_VALIDATE_INUM (1,object);
      fdes = SCM_INUM (object);
    }
  if (fsync (fdes) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_SYMLINK
SCM_DEFINE (scm_symlink, "symlink", 2, 0, 0,
            (SCM oldpath, SCM newpath),
	    "Create a symbolic link named @var{path-to} with the value (i.e., pointing to)\n"
	    "@var{path-from}.  The return value is unspecified.")
#define FUNC_NAME s_scm_symlink
{
  int val;

  SCM_VALIDATE_STRING (1, oldpath);
  SCM_VALIDATE_STRING (2, newpath);
  SCM_STRING_COERCE_0TERMINATION_X (oldpath);
  SCM_STRING_COERCE_0TERMINATION_X (newpath);
  SCM_SYSCALL (val = symlink (SCM_STRING_CHARS (oldpath), SCM_STRING_CHARS (newpath)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SYMLINK */

#ifdef HAVE_READLINK
SCM_DEFINE (scm_readlink, "readlink", 1, 0, 0, 
            (SCM path),
	    "Return the value of the symbolic link named by @var{path} (a\n"
	    "string), i.e., the file that the link points to.")
#define FUNC_NAME s_scm_readlink
{
  int rv;
  int size = 100;
  char *buf;
  SCM result;
  SCM_VALIDATE_STRING (1, path);
  SCM_STRING_COERCE_0TERMINATION_X (path);
  buf = scm_must_malloc (size, FUNC_NAME);
  while ((rv = readlink (SCM_STRING_CHARS (path), buf, size)) == size)
    {
      scm_must_free (buf);
      size *= 2;
      buf = scm_must_malloc (size, FUNC_NAME);
    }
  if (rv == -1)
    SCM_SYSERROR;
  result = scm_makfromstr (buf, rv, 0);
  scm_must_free (buf);
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_READLINK */

#ifdef HAVE_LSTAT
SCM_DEFINE (scm_lstat, "lstat", 1, 0, 0, 
            (SCM str),
	    "Similar to @code{stat}, but does not follow symbolic links, i.e.,\n"
	    "it will return information about a symbolic link itself, not the \n"
	    "file it points to.  @var{path} must be a string.")
#define FUNC_NAME s_scm_lstat
{
  int rv;
  struct stat stat_temp;

  SCM_VALIDATE_STRING (1, str);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  SCM_SYSCALL (rv = lstat (SCM_STRING_CHARS (str), &stat_temp));
  if (rv != 0)
    {
      int en = errno;

      SCM_SYSERROR_MSG ("~A: ~S",
			SCM_LIST2 (scm_makfrom0str (strerror (errno)), str),
			en);
    }
  return scm_stat2scm(&stat_temp);
}
#undef FUNC_NAME
#endif /* HAVE_LSTAT */

SCM_DEFINE (scm_copy_file, "copy-file", 2, 0, 0,
            (SCM oldfile, SCM newfile),
	    "Copy the file specified by @var{path-from} to @var{path-to}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_copy_file
{
  int oldfd, newfd;
  int n;
  char buf[BUFSIZ];
  struct stat oldstat;

  SCM_VALIDATE_STRING (1, oldfile);
  SCM_STRING_COERCE_0TERMINATION_X (oldfile);
  SCM_VALIDATE_STRING (2, newfile);
  SCM_STRING_COERCE_0TERMINATION_X (newfile);
  if (stat (SCM_STRING_CHARS (oldfile), &oldstat) == -1)
    SCM_SYSERROR;
  oldfd = open (SCM_STRING_CHARS (oldfile), O_RDONLY);
  if (oldfd == -1)
    SCM_SYSERROR;

  /* use POSIX flags instead of 07777?.  */
  newfd = open (SCM_STRING_CHARS (newfile), O_WRONLY | O_CREAT | O_TRUNC,
		oldstat.st_mode & 07777);
  if (newfd == -1)
    SCM_SYSERROR;

  while ((n = read (oldfd, buf, sizeof buf)) > 0)
    if (write (newfd, buf, n) != n)
      {
	close (oldfd);
	close (newfd);
	SCM_SYSERROR;
      }
  close (oldfd);
  if (close (newfd) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* Filename manipulation */

SCM scm_dot_string;

SCM_DEFINE (scm_dirname, "dirname", 1, 0, 0, 
            (SCM filename),
	    "Return the directory name component of the file name\n"
	    "@var{filename}. If @var{filename} does not contain a directory\n"
	    "component, @code{.} is returned.")
#define FUNC_NAME s_scm_dirname
{
  char *s;
  long int i;
  unsigned long int len;

  SCM_VALIDATE_STRING (1,filename);

  s = SCM_STRING_CHARS (filename);
  len = SCM_STRING_LENGTH (filename);

  i = len - 1;
  while (i >= 0 && s[i] == '/') --i;
  while (i >= 0 && s[i] != '/') --i;
  while (i >= 0 && s[i] == '/') --i;
  if (i < 0)
    {
      if (len > 0 && s[0] == '/')
	return scm_substring (filename, SCM_INUM0, SCM_MAKINUM (1));
      else
	return scm_dot_string;
    }
  else
    return scm_substring (filename, SCM_INUM0, SCM_MAKINUM (i + 1));
}
#undef FUNC_NAME

SCM_DEFINE (scm_basename, "basename", 1, 1, 0, 
            (SCM filename, SCM suffix),
	    "Return the base name of the file name @var{filename}. The\n"
	    "base name is the file name without any directory components.\n"
	    "If @var{suffix} is privided, and is equal to the end of\n"
	    "@var{basename}, it is removed also.")
#define FUNC_NAME s_scm_basename
{
  char *f, *s = 0;
  int i, j, len, end;

  SCM_VALIDATE_STRING (1,filename);
  f = SCM_STRING_CHARS (filename);
  len = SCM_STRING_LENGTH (filename);

  if (SCM_UNBNDP (suffix))
    j = -1;
  else
    {
      SCM_VALIDATE_STRING (2, suffix);
      s = SCM_STRING_CHARS (suffix);
      j = SCM_STRING_LENGTH (suffix) - 1;
    }
  i = len - 1;
  while (i >= 0 && f[i] == '/') --i;
  end = i;
  while (i >= 0 && j >= 0 && f[i] == s[j]) --i, --j;
  if (j == -1)
    end = i;
  while (i >= 0 && f[i] != '/') --i;
  if (i == end)
    {
      if (len > 0 && f[0] == '/')
	return scm_substring (filename, SCM_INUM0, SCM_MAKINUM (1));
      else
	return scm_dot_string;
    }
  else
    return scm_substring (filename, SCM_MAKINUM (i + 1), SCM_MAKINUM (end + 1));
}
#undef FUNC_NAME





void
scm_init_filesys ()
{
  scm_tc16_dir = scm_make_smob_type ("directory", 0);
  scm_set_smob_free (scm_tc16_dir, scm_dir_free);
  scm_set_smob_print (scm_tc16_dir, scm_dir_print);

  scm_dot_string = scm_permanent_object (scm_makfrom0str ("."));
  
#ifdef O_RDONLY
  scm_c_define ("O_RDONLY", scm_long2num (O_RDONLY));
#endif 	       
#ifdef O_WRONLY
  scm_c_define ("O_WRONLY", scm_long2num (O_WRONLY));
#endif 	       
#ifdef O_RDWR
  scm_c_define ("O_RDWR", scm_long2num (O_RDWR));
#endif 	       
#ifdef O_CREAT
  scm_c_define ("O_CREAT", scm_long2num (O_CREAT));
#endif 	       
#ifdef O_EXCL  
  scm_c_define ("O_EXCL", scm_long2num (O_EXCL));
#endif 	       
#ifdef O_NOCTTY
  scm_c_define ("O_NOCTTY", scm_long2num (O_NOCTTY));
#endif 	       
#ifdef O_TRUNC 
  scm_c_define ("O_TRUNC", scm_long2num (O_TRUNC));
#endif 	       
#ifdef O_APPEND
  scm_c_define ("O_APPEND", scm_long2num (O_APPEND));
#endif 	       
#ifdef O_NONBLOCK
  scm_c_define ("O_NONBLOCK", scm_long2num (O_NONBLOCK));
#endif 	       
#ifdef O_NDELAY
  scm_c_define ("O_NDELAY", scm_long2num (O_NDELAY));
#endif 	       
#ifdef O_SYNC  
  scm_c_define ("O_SYNC", scm_long2num (O_SYNC));
#endif 

#ifdef F_DUPFD  
  scm_c_define ("F_DUPFD", scm_long2num (F_DUPFD));
#endif 
#ifdef F_GETFD  
  scm_c_define ("F_GETFD", scm_long2num (F_GETFD));
#endif 
#ifdef F_SETFD  
  scm_c_define ("F_SETFD", scm_long2num (F_SETFD));
#endif 
#ifdef F_GETFL  
  scm_c_define ("F_GETFL", scm_long2num (F_GETFL));
#endif 
#ifdef F_SETFL  
  scm_c_define ("F_SETFL", scm_long2num (F_SETFL));
#endif 
#ifdef F_GETOWN  
  scm_c_define ("F_GETOWN", scm_long2num (F_GETOWN));
#endif 
#ifdef F_SETOWN  
  scm_c_define ("F_SETOWN", scm_long2num (F_SETOWN));
#endif 
#ifdef FD_CLOEXEC  
  scm_c_define ("FD_CLOEXEC", scm_long2num (FD_CLOEXEC));
#endif

#ifndef SCM_MAGIC_SNARFER
#include "libguile/filesys.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
