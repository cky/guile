/*	Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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
#include "_scm.h"
#include "genio.h"
#include "smob.h"
#include "feature.h"
#include "fports.h"
#include "iselect.h"

#include "scm_validate.h"
#include "filesys.h"


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
"Change the ownership and group of the file referred to by @var{obj} to
the integer userid values @var{owner} and @var{group}.  @var{obj} can be
a string containing a file name or a port or integer file descriptor
which is open on the file (in which case fchown is used as the underlying
system call).  The return value
is unspecified.

If @var{obj} is a symbolic link, either the
ownership of the link or the ownership of the referenced file will be
changed depending on the operating system (lchown is
unsupported at present).  If @var{owner} or @var{group} is specified
as @code{-1}, then that ID is not changed.")
#define FUNC_NAME s_scm_chown
{
  int rv;
  int fdes;

  object = SCM_COERCE_OUTPORT (object);

  SCM_VALIDATE_INUM (2,owner);
  SCM_VALIDATE_INUM (3,group);
  if (SCM_INUMP (object) || (SCM_OPFPORTP (object)))
    {
      if (SCM_INUMP (object))
	fdes = SCM_INUM (object);
      else
	fdes = SCM_FPORT_FDES (object);
      SCM_SYSCALL (rv = fchown (fdes, SCM_INUM (owner), SCM_INUM (group)));
    }
  else
    {
      SCM_ASSERT (SCM_ROSTRINGP (object),
		  object, SCM_ARG1, FUNC_NAME);
      SCM_COERCE_SUBSTR (object);
      SCM_SYSCALL (rv = chown (SCM_ROCHARS (object),
			       SCM_INUM (owner), SCM_INUM (group)));
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_chmod, "chmod", 2, 0, 0,
            (SCM object, SCM mode),
"Changes the permissions of the file referred to by @var{obj}.
@var{obj} can be a string containing a file name or a port or integer file
descriptor which is open on a file (in which case @code{fchmod} is used
as the underlying system call).
@var{mode} specifies
the new permissions as a decimal number, e.g., @code{(chmod "foo" #o755)}.
The return value is unspecified.")
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
      SCM_VALIDATE_ROSTRING (1,object);
      SCM_COERCE_SUBSTR (object);
      SCM_SYSCALL (rv = chmod (SCM_ROCHARS (object), SCM_INUM (mode)));
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_umask, "umask", 0, 1, 0, 
            (SCM mode),
"If @var{mode} is omitted, retuns a decimal number representing the current
file creation mask.  Otherwise the file creation mask is set to
@var{mode} and the previous value is returned.

E.g., @code{(umask #o022)} sets the mask to octal 22, decimal 18.")
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
"Similar to @code{open} but returns a file descriptor instead of a
port.")
#define FUNC_NAME s_scm_open_fdes
{
  int fd;
  int iflags;
  int imode;

  SCM_VALIDATE_ROSTRING (1,path);
  SCM_COERCE_SUBSTR (path);
  SCM_VALIDATE_INUM_COPY (2,flags,iflags);
  SCM_VALIDATE_INUM_DEF_COPY (3,mode,0666,imode);
  SCM_SYSCALL (fd = open (SCM_ROCHARS (path), iflags, imode));
  if (fd == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (fd);
}
#undef FUNC_NAME

SCM_DEFINE (scm_open, "open", 2, 1, 0, 
            (SCM path, SCM flags, SCM mode),
"Open the file named by @var{path} for reading and/or writing.
@var{flags} is an integer specifying how the file should be opened.
@var{mode} is an integer specifying the permission bits of the file, if
it needs to be created, before the umask is applied.  The default is 666
(Unix itself has no default).

@var{flags} can be constructed by combining variables using @code{logior}.
Basic flags are:

@defvar O_RDONLY
Open the file read-only.
@end defvar
@defvar O_WRONLY
Open the file write-only. 
@end defvar
@defvar O_RDWR
Open the file read/write.
@end defvar
@defvar O_APPEND
Append to the file instead of truncating.
@end defvar
@defvar O_CREAT
Create the file if it does not already exist.
@end defvar

See the Unix documentation of the @code{open} system call
for additional flags.")
#define FUNC_NAME s_scm_open
{
  SCM newpt;
  char *port_mode;
  int fd;
  int iflags;

  fd = SCM_INUM (scm_open_fdes (path, flags, mode));
  SCM_VALIDATE_INUM_COPY (2,flags,iflags);
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
"Similar to close-port (@pxref{Generic Port Operations, close-port}),
but also works on file descriptors.  A side
effect of closing a file descriptor is that any ports using that file
descriptor are moved to a different file descriptor and have
their revealed counts set to zero.")
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
  SCM ans = scm_make_vector (SCM_MAKINUM (15), SCM_UNSPECIFIED);
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

SCM_DEFINE (scm_stat, "stat", 1, 0, 0, 
            (SCM object),
"Returns an object containing various information
about the file determined by @var{obj}.
@var{obj} can be a string containing a file name or a port or integer file
descriptor which is open on a file (in which case @code{fstat} is used
as the underlying system call).

The object returned by @code{stat} can be passed as a single parameter
to the following procedures, all of which return integers:

@table @code
@item stat:dev
The device containing the file.
@item stat:ino
The file serial number, which distinguishes this file from all other
files on the same device.
@item stat:mode
The mode of the file.  This includes file type information
and the file permission bits.  See @code{stat:type} and @code{stat:perms}
below.
@item stat:nlink
The number of hard links to the file.
@item stat:uid
The user ID of the file's owner.
@item stat:gid
The group ID of the file.
@item stat:rdev
Device ID; this entry is defined only for character or block
special files.
@item stat:size
The size of a regular file in bytes.
@item stat:atime
The last access time for the file.
@item stat:mtime
The last modification time for the file.
@item stat:ctime
The last modification time for the attributes of the file.
@item stat:blksize
The optimal block size for reading or writing the file, in bytes.
@item stat:blocks
The amount of disk space that the file occupies measured in units of
512 byte blocks.
@end table

In addition, the following procedures return the information
from stat:mode in a more convenient form:

@table @code
@item stat:type
A symbol representing the type of file.  Possible values are
regular, directory, symlink, block-special, char-special,
fifo, socket and unknown
@item stat:perms
An integer representing the access permission bits.
@end table")
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
      if (SCM_ROSTRINGP (object))
	{
	  SCM_COERCE_SUBSTR (object);
	  SCM_SYSCALL (rv = stat (SCM_ROCHARS (object), &stat_temp));
	}
      else
	{
	  object = SCM_COERCE_OUTPORT (object);
	  SCM_ASSERT (SCM_OPFPORTP (object), object, SCM_ARG1, FUNC_NAME);
	  fdes = SCM_FPORT_FDES (object);
	  SCM_SYSCALL (rv = fstat (fdes, &stat_temp));
	}
    }
  if (rv == -1)
    {
      int en = errno;

      scm_syserror_msg (FUNC_NAME, "%s: %S",
			scm_listify (scm_makfrom0str (strerror (errno)),
				     object,
				     SCM_UNDEFINED),
			en);
    }
  return scm_stat2scm (&stat_temp);
}
#undef FUNC_NAME


/* {Modifying Directories}
 */

SCM_DEFINE (scm_link, "link", 2, 0, 0,
            (SCM oldpath, SCM newpath),
"Creates a new name @var{path-to} in the file system for the file
named by @var{path-from}.  If @var{path-from} is a symbolic link, the
link may or may not be followed depending on the system.")
#define FUNC_NAME s_scm_link
{
  int val;

  SCM_VALIDATE_ROSTRING (1,oldpath);
  if (SCM_SUBSTRP (oldpath))
    oldpath = scm_makfromstr (SCM_ROCHARS (oldpath),
			      SCM_ROLENGTH (oldpath), 0);
  SCM_VALIDATE_ROSTRING (2,newpath);
  if (SCM_SUBSTRP (newpath))
    newpath = scm_makfromstr (SCM_ROCHARS (newpath),
			      SCM_ROLENGTH (newpath), 0);
  SCM_SYSCALL (val = link (SCM_ROCHARS (oldpath), SCM_ROCHARS (newpath)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_rename, "rename-file", 2, 0, 0,
            (SCM oldname, SCM newname),
"Renames the file specified by @var{path-from} to @var{path-to}.
The return value is unspecified.")
#define FUNC_NAME s_scm_rename
{
  int rv;
  SCM_VALIDATE_ROSTRING (1,oldname);
  SCM_VALIDATE_ROSTRING (2,newname);
  SCM_COERCE_SUBSTR (oldname);
  SCM_COERCE_SUBSTR (newname);
#ifdef HAVE_RENAME
  SCM_SYSCALL (rv = rename (SCM_ROCHARS (oldname), SCM_ROCHARS (newname)));
#else
  SCM_SYSCALL (rv = link (SCM_ROCHARS (oldname), SCM_ROCHARS (newname)));
  if (rv == 0)
    {
      SCM_SYSCALL (rv = unlink (SCM_ROCHARS (oldname)));;
      if (rv != 0)
	/* unlink failed.  remove new name */
	SCM_SYSCALL (unlink (SCM_ROCHARS (newname))); 
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
  SCM_VALIDATE_ROSTRING (1,str);
  SCM_COERCE_SUBSTR (str);
  SCM_SYSCALL (ans = unlink (SCM_ROCHARS (str)));
  if (ans != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_MKDIR
SCM_DEFINE (scm_mkdir, "mkdir", 1, 1, 0,
            (SCM path, SCM mode),
"Create a new directory named by @var{path}.  If @var{mode} is omitted
then the permissions of the directory file are set using the current
umask.  Otherwise they are set to the decimal value specified with
@var{mode}.  The return value is unspecified.")
#define FUNC_NAME s_scm_mkdir
{
  int rv;
  mode_t mask;
  SCM_VALIDATE_ROSTRING (1,path);
  SCM_COERCE_SUBSTR (path);
  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
      SCM_SYSCALL (rv = mkdir (SCM_ROCHARS (path), 0777 ^ mask));
    }
  else
    {
      SCM_VALIDATE_INUM (2,mode);
      SCM_SYSCALL (rv = mkdir (SCM_ROCHARS (path), SCM_INUM (mode)));
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
"Remove the existing directory named by @var{path}.  The directory must
be empty for this to succeed.  The return value is unspecified.")
#define FUNC_NAME s_scm_rmdir
{
  int val;

  SCM_VALIDATE_ROSTRING (1,path);
  SCM_COERCE_SUBSTR (path);
  SCM_SYSCALL (val = rmdir (SCM_ROCHARS (path)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif


/* {Examining Directories}
 */

long scm_tc16_dir;

SCM_DEFINE (scm_directory_stream_p, "directory-stream?", 1, 0, 0, 
            (SCM obj),
"Returns a boolean indicating whether @var{object} is a directory stream
as returned by @code{opendir}.")
#define FUNC_NAME s_scm_directory_stream_p
{
  return SCM_BOOL(SCM_DIRP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_opendir, "opendir", 1, 0, 0, 
            (SCM dirname),
"Open the directory specified by @var{path} and return a directory
stream.")
#define FUNC_NAME s_scm_opendir
{
  DIR *ds;
  SCM_VALIDATE_ROSTRING (1,dirname);
  SCM_COERCE_SUBSTR (dirname);
  SCM_SYSCALL (ds = opendir (SCM_ROCHARS (dirname)));
  if (ds == NULL)
    SCM_SYSERROR;
  SCM_RETURN_NEWSMOB (scm_tc16_dir | SCM_OPN, ds);
}
#undef FUNC_NAME


SCM_DEFINE (scm_readdir, "readdir", 1, 0, 0, 
            (SCM port),
"Return (as a string) the next directory entry from the directory stream
@var{stream}.  If there is no remaining entry to be read then the
end of file object is returned.")
#define FUNC_NAME s_scm_readdir
{
  struct dirent *rdent;
  SCM_VALIDATE_OPDIR (1,port);
  errno = 0;
  SCM_SYSCALL (rdent = readdir ((DIR *) SCM_CDR (port)));
  if (errno != 0)
    SCM_SYSERROR;
  return (rdent ? scm_makfromstr (rdent->d_name, NAMLEN (rdent), 0)
	  : SCM_EOF_VAL);
}
#undef FUNC_NAME



SCM_DEFINE (scm_rewinddir, "rewinddir", 1, 0, 0, 
            (SCM port),
"Reset the directory port @var{stream} so that the next call to
@code{readdir} will return the first directory entry.")
#define FUNC_NAME s_scm_rewinddir
{
  SCM_VALIDATE_OPDIR (1,port);
  rewinddir ((DIR *) SCM_CDR (port));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_closedir, "closedir", 1, 0, 0, 
            (SCM port),
"Close the directory stream @var{stream}.
The return value is unspecified.")
#define FUNC_NAME s_scm_closedir
{
  int sts;

  SCM_VALIDATE_DIR (1,port);
  if (SCM_CLOSEDP (port))
    {
      return SCM_UNSPECIFIED;
    }
  SCM_SYSCALL (sts = closedir ((DIR *) SCM_CDR (port)));
  if (sts != 0)
    SCM_SYSERROR;
  SCM_SETCAR (port, scm_tc16_dir);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




static int 
scm_dir_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<", port);
  if (SCM_CLOSEDP (exp))
    scm_puts ("closed: ", port);
  scm_puts ("directory stream ", port);
  scm_intprint (SCM_CDR (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}


static scm_sizet 
scm_dir_free (SCM p)
{
  if (SCM_OPENP (p))
    closedir ((DIR *) SCM_CDR (p));
  return 0;
}


/* {Navigating Directories}
 */


SCM_DEFINE (scm_chdir, "chdir", 1, 0, 0, 
            (SCM str),
"Change the current working directory to @var{path}.
The return value is unspecified.")
#define FUNC_NAME s_scm_chdir
{
  int ans;

  SCM_VALIDATE_ROSTRING (1,str);
  SCM_COERCE_SUBSTR (str);
  SCM_SYSCALL (ans = chdir (SCM_ROCHARS (str)));
  if (ans != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_GETCWD
SCM_DEFINE (scm_getcwd, "getcwd", 0, 0, 0,
            (),
"Returns the name of the current working directory.")
#define FUNC_NAME s_scm_getcwd
{
  char *rv;

  scm_sizet size = 100;
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



static int
set_element (SELECT_TYPE *set, SCM element, int arg)
{
  int fd;
  element = SCM_COERCE_OUTPORT (element);
  if (SCM_OPFPORTP (element))
    fd = SCM_FPORT_FDES (element);
  else {
    SCM_ASSERT (SCM_INUMP (element), element, arg, "select");
    fd = SCM_INUM (element);
  }
  FD_SET (fd, set);
  return fd;
}

static int
fill_select_type (SELECT_TYPE *set, SCM list, int arg)
{
  int max_fd = 0, fd;
  if (SCM_VECTORP (list))
    {
      int len = SCM_LENGTH (list);
      SCM *ve = SCM_VELTS (list);
      
      while (len > 0)
	{
	  fd = set_element (set, ve[len - 1], arg);
	  if (fd > max_fd)
	    max_fd = fd;
	  len--;
	}
    }
  else
    {
      while (list != SCM_EOL)
	{
	  fd = set_element (set, SCM_CAR (list), arg);
	  if (fd > max_fd)
	    max_fd = fd;
	  list = SCM_CDR (list);
	}
    }

  return max_fd;
}

static SCM
get_element (SELECT_TYPE *set, SCM element, SCM list)
{
  element = SCM_COERCE_OUTPORT (element);
  if (SCM_OPFPORTP (element))
    {
      if (FD_ISSET (SCM_FPORT_FDES (element), set))
	list = scm_cons (element, list);
    }
  else if (SCM_INUMP (element))
    {
      if (FD_ISSET (SCM_INUM (element), set))
	list = scm_cons (element, list);
    }
  return list;
}

static SCM 
retrieve_select_type (SELECT_TYPE *set, SCM list)
{
  SCM answer_list = SCM_EOL;

  if (SCM_VECTORP (list))
    {
      int len = SCM_LENGTH (list);
      SCM *ve = SCM_VELTS (list);

      while (len > 0)
	{
	  answer_list = get_element (set, ve[len - 1], answer_list);
	  len--;
	}
      return scm_vector (answer_list);
    }
  else
    {
      /* list is a list.  */
      while (list != SCM_EOL)
	{
	  answer_list = get_element (set, SCM_CAR (list), answer_list);
	  list = SCM_CDR (list);
	}
      return answer_list;
    }
}

#ifdef HAVE_SELECT
/* Static helper functions above refer to s_scm_select directly as s_select */
SCM_DEFINE (scm_select, "select", 3, 2, 0, 
            (SCM reads, SCM writes, SCM excepts, SCM secs, SCM usecs),
"@var{reads}, @var{writes} and @var{excepts} can be lists or vectors: it
doesn't matter which, but the corresponding object returned will be
of the same type.
Each element is a port or file descriptor on which to wait for
readability, writeability
or exceptional conditions respectively.  @var{secs} and @var{usecs}
optionally specify a timeout: @var{secs} can be specified alone, as
either an integer or a real number, or both @var{secs} and @var{usecs}
can be specified as integers, in which case @var{usecs} is an additional
timeout expressed in microseconds.

Buffered input or output data is (currently, but this may change)
ignored: select uses the underlying file descriptor of a port
(@code{char-ready?} will check input buffers, output buffers are
problematic).

The return value is a list of subsets of the input lists or vectors for
which the requested condition has been met.

It is not quite compatible with scsh's select: scsh checks port buffers,
doesn't accept input lists or a microsecond timeout, returns multiple
values instead of a list and has an additional select! interface.
")
#define FUNC_NAME s_scm_select
{
  struct timeval timeout;
  struct timeval * time_p;
  SELECT_TYPE read_set;
  SELECT_TYPE write_set;
  SELECT_TYPE except_set;
  int max_fd, fd;
  int sreturn;

#define assert_set(x, arg) \
  SCM_ASSERT (scm_ilength (x) >= 0 || (SCM_VECTORP (x)), \
	      x, arg, FUNC_NAME)
  assert_set (reads, SCM_ARG1);
  assert_set (writes, SCM_ARG2);
  assert_set (excepts, SCM_ARG3);
#undef assert_set

  FD_ZERO (&read_set);
  FD_ZERO (&write_set);
  FD_ZERO (&except_set);

  max_fd = fill_select_type (&read_set, reads, SCM_ARG1);
  fd = fill_select_type (&write_set, writes, SCM_ARG2);
  if (fd > max_fd)
    max_fd = fd;
  fd = fill_select_type (&except_set, excepts, SCM_ARG3);
  if (fd > max_fd)
    max_fd = fd;

  if (SCM_UNBNDP (secs) || SCM_FALSEP (secs))
    time_p = 0;
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
	    scm_wrong_type_arg (FUNC_NAME, 4, secs);
	  if (fl > LONG_MAX)
	    scm_out_of_range (FUNC_NAME, secs);
	  timeout.tv_sec = (long) fl;
	  timeout.tv_usec = (long) ((fl - timeout.tv_sec) * 1000000);
	}
      time_p = &timeout;
    }

#ifdef GUILE_ISELECT
  sreturn = scm_internal_select (max_fd + 1,
				 &read_set, &write_set, &except_set, time_p);
#else
  sreturn = select (max_fd + 1,
		    &read_set, &write_set, &except_set, time_p);
#endif
  if (sreturn < 0)
    SCM_SYSERROR;
  return scm_listify (retrieve_select_type (&read_set, reads),
		      retrieve_select_type (&write_set, writes),
		      retrieve_select_type (&except_set, excepts),
		      SCM_UNDEFINED);
}
#undef FUNC_NAME
#endif /* HAVE_SELECT */



SCM_DEFINE (scm_fcntl, "fcntl", 2, 0, 1,
            (SCM object, SCM cmd, SCM value),
"Apply @var{command} to the specified file descriptor or the underlying
file descriptor of the specified port.  @var{value} is an optional
integer argument.

Values for @var{command} are:

@table @code
@item F_DUPFD
Duplicate a file descriptor
@item F_GETFD
Get flags associated with the file descriptor.
@item F_SETFD
Set flags associated with the file descriptor to @var{value}.
@item F_GETFL
Get flags associated with the open file.
@item F_SETFL
Set flags associated with the open file to @var{value}
@item F_GETOWN
Get the process ID of a socket's owner, for @code{SIGIO} signals.
@item F_SETOWN
Set the process that owns a socket to @var{value}, for @code{SIGIO} signals.
@item FD_CLOEXEC
The value used to indicate the "close on exec" flag with @code{F_GETFL} or
@code{F_SETFL}.
@end table")
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
  if (SCM_NULLP (value))
    ivalue = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (SCM_CAR (value)), value, SCM_ARG3, FUNC_NAME);
      ivalue = SCM_INUM (SCM_CAR (value));
    }
  SCM_SYSCALL (rv = fcntl (fdes, SCM_INUM (cmd), ivalue));
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fsync, "fsync", 1, 0, 0, 
            (SCM object),
"Copies any unwritten data for the specified output file descriptor to disk.
If @var{port/fd} is a port, its buffer is flushed before the underlying
file descriptor is fsync'd.
The return value is unspecified.")
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
"Create a symbolic link named @var{path-to} with the value (i.e., pointing to)
@var{path-from}.  The return value is unspecified.")
#define FUNC_NAME s_scm_symlink
{
  int val;

  SCM_VALIDATE_ROSTRING (1,oldpath);
  SCM_VALIDATE_ROSTRING (2,newpath);
  SCM_COERCE_SUBSTR (oldpath);
  SCM_COERCE_SUBSTR (newpath);
  SCM_SYSCALL (val = symlink(SCM_ROCHARS(oldpath), SCM_ROCHARS(newpath)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SYMLINK */

#ifdef HAVE_READLINK
SCM_DEFINE (scm_readlink, "readlink", 1, 0, 0, 
            (SCM path),
"Returns the value of the symbolic link named by
@var{path} (a string), i.e., the
file that the link points to.")
#define FUNC_NAME s_scm_readlink
{
  int rv;
  int size = 100;
  char *buf;
  SCM result;
  SCM_VALIDATE_ROSTRING (1,path);
  SCM_COERCE_SUBSTR (path);
  buf = scm_must_malloc (size, FUNC_NAME);
  while ((rv = readlink (SCM_ROCHARS (path), buf, size)) == size)
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
"Similar to @code{stat}, but does not follow symbolic links, i.e.,
it will return information about a symbolic link itself, not the 
file it points to.  @var{path} must be a string.")
#define FUNC_NAME s_scm_lstat
{
  int rv;
  struct stat stat_temp;

  SCM_VALIDATE_ROSTRING (1,str);
  SCM_COERCE_SUBSTR (str);
  SCM_SYSCALL(rv = lstat(SCM_ROCHARS(str), &stat_temp));
  if (rv != 0)
    {
      int en = errno;

      scm_syserror_msg (FUNC_NAME, "%s: %S",
			scm_listify (scm_makfrom0str (strerror (errno)),
				     str,
				     SCM_UNDEFINED),
			en);
    }
  return scm_stat2scm(&stat_temp);
}
#undef FUNC_NAME
#endif /* HAVE_LSTAT */

SCM_DEFINE (scm_copy_file, "copy-file", 2, 0, 0,
            (SCM oldfile, SCM newfile),
"Copy the file specified by @var{path-from} to @var{path-to}.
The return value is unspecified.")
#define FUNC_NAME s_scm_copy_file
{
  int oldfd, newfd;
  int n;
  char buf[BUFSIZ];
  struct stat oldstat;

  SCM_VALIDATE_ROSTRING (1,oldfile);
  if (SCM_SUBSTRP (oldfile))
    oldfile = scm_makfromstr (SCM_ROCHARS (oldfile), SCM_ROLENGTH (oldfile), 0);
  SCM_VALIDATE_ROSTRING (2,newfile);
  if (SCM_SUBSTRP (newfile))
    newfile = scm_makfromstr (SCM_ROCHARS (newfile), SCM_ROLENGTH (newfile), 0);
  if (stat (SCM_ROCHARS (oldfile), &oldstat) == -1)
    SCM_SYSERROR;
  oldfd = open (SCM_ROCHARS (oldfile), O_RDONLY);
  if (oldfd == -1)
    SCM_SYSERROR;

  /* use POSIX flags instead of 07777?.  */
  newfd = open (SCM_ROCHARS (newfile), O_WRONLY | O_CREAT | O_TRUNC,
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
"")
#define FUNC_NAME s_scm_dirname
{
  char *s;
  int i, len;
  SCM_VALIDATE_ROSTRING (1,filename);
  s = SCM_ROCHARS (filename);
  len = SCM_LENGTH (filename);
  i = len - 1;
  while (i >= 0 && s[i] == '/') --i;
  while (i >= 0 && s[i] != '/') --i;
  while (i >= 0 && s[i] == '/') --i;
  if (i < 0)
    {
      if (len > 0 && s[0] == '/')
	return scm_make_shared_substring (filename, SCM_INUM0, SCM_MAKINUM (1));
      else
	return scm_dot_string;
    }
  else
    return scm_make_shared_substring (filename, SCM_INUM0, SCM_MAKINUM (i + 1));
}
#undef FUNC_NAME

SCM_DEFINE (scm_basename, "basename", 1, 1, 0, 
            (SCM filename, SCM suffix),
"")
#define FUNC_NAME s_scm_basename
{
  char *f, *s = 0;
  int i, j, len, end;
  SCM_VALIDATE_ROSTRING (1,filename);
  SCM_ASSERT (SCM_UNBNDP (suffix)
	      || (SCM_ROSTRINGP (suffix)),
	      suffix,
	      SCM_ARG2,
	      FUNC_NAME);
  f = SCM_ROCHARS (filename);
  if (SCM_UNBNDP (suffix))
    j = -1;
  else
    {
      s = SCM_ROCHARS (suffix);
      j = SCM_LENGTH (suffix) - 1;
    }
  len = SCM_LENGTH (filename);
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
	return scm_make_shared_substring (filename, SCM_INUM0, SCM_MAKINUM (1));
      else
	return scm_dot_string;
    }
  else
    return scm_make_shared_substring (filename,
				      SCM_MAKINUM (i + 1),
				      SCM_MAKINUM (end + 1));
}
#undef FUNC_NAME





void
scm_init_filesys ()
{
  scm_tc16_dir = scm_make_smob_type_mfpe ("directory", 0,
                                         NULL, scm_dir_free,scm_dir_print, NULL);

  scm_dot_string = scm_permanent_object (scm_makfrom0str ("."));
  
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
#ifdef O_NONBLOCK
scm_sysintern ("O_NONBLOCK", scm_long2num (O_NONBLOCK));
#endif 	       
#ifdef O_NDELAY
scm_sysintern ("O_NDELAY", scm_long2num (O_NDELAY));
#endif 	       
#ifdef O_SYNC  
scm_sysintern ("O_SYNC", scm_long2num (O_SYNC));
#endif 

#ifdef F_DUPFD  
scm_sysintern ("F_DUPFD", scm_long2num (F_DUPFD));
#endif 
#ifdef F_GETFD  
scm_sysintern ("F_GETFD", scm_long2num (F_GETFD));
#endif 
#ifdef F_SETFD  
scm_sysintern ("F_SETFD", scm_long2num (F_SETFD));
#endif 
#ifdef F_GETFL  
scm_sysintern ("F_GETFL", scm_long2num (F_GETFL));
#endif 
#ifdef F_SETFL  
scm_sysintern ("F_SETFL", scm_long2num (F_SETFL));
#endif 
#ifdef F_GETOWN  
scm_sysintern ("F_GETOWN", scm_long2num (F_GETOWN));
#endif 
#ifdef F_SETOWN  
scm_sysintern ("F_SETOWN", scm_long2num (F_SETOWN));
#endif 
#ifdef FD_CLOEXEC  
scm_sysintern ("FD_CLOEXEC", scm_long2num (FD_CLOEXEC));
#endif 

#include "filesys.x"
}
