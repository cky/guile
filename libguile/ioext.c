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
#include <unistd.h>
#include "fd.h"
#include "_scm.h"



SCM_PROC (s_sys_ftell, "ftell", 1, 0, 0, scm_sys_ftell);
#ifdef __STDC__
SCM 
scm_sys_ftell (SCM port)
#else
SCM 
scm_sys_ftell (port)
     SCM port;
#endif
{
  long pos;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_sys_ftell);
  SCM_SYSCALL (pos = ftell ((FILE *)SCM_STREAM (port)));
  if (pos < 0)
    SCM_SYSERROR (s_sys_ftell);
  if (pos > 0 && SCM_CRDYP (port))
    pos--;
  return SCM_MAKINUM (pos);
}



SCM_PROC (s_sys_fseek, "fseek", 3, 0, 0, scm_sys_fseek);
#ifdef __STDC__
SCM 
scm_sys_fseek (SCM port, SCM offset, SCM whence)
#else
SCM 
scm_sys_fseek (port, offset, whence)
     SCM port;
     SCM offset;
     SCM whence;
#endif
{
  int rv;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_sys_fseek);
  SCM_ASSERT (SCM_INUMP (offset), offset, SCM_ARG2, s_sys_fseek);
  SCM_ASSERT (SCM_INUMP (whence) && (SCM_INUM (whence) < 3) && (SCM_INUM (whence) >= 0),
	  whence, SCM_ARG3, s_sys_fseek);
  SCM_CLRDY (port);			/* Clear ungetted char */
  /* Values of whence are interned in scm_init_ioext.  */
  rv = fseek ((FILE *)SCM_STREAM (port), SCM_INUM (offset), SCM_INUM (whence));
  if (rv != 0)
    SCM_SYSERROR (s_sys_fseek);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_sys_freopen, "freopen", 3, 0, 0, scm_sys_freopen);
#ifdef __STDC__
SCM 
scm_sys_freopen (SCM filename, SCM modes, SCM port)
#else
SCM 
scm_sys_freopen (filename, modes, port)
     SCM filename;
     SCM modes;
     SCM port;
#endif
{
  FILE *f;
  SCM_ASSERT (SCM_NIMP (filename) && SCM_STRINGP (filename), filename, SCM_ARG1, s_sys_freopen);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_STRINGP (modes), modes, SCM_ARG2, s_sys_freopen);
  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (port) && SCM_FPORTP (port), port, SCM_ARG3, s_sys_freopen);
  SCM_SYSCALL (f = freopen (SCM_CHARS (filename), SCM_CHARS (modes), (FILE *)SCM_STREAM (port)));
  if (!f)
    {
      SCM p;
      p = port;
      port = SCM_MAKINUM (errno);
      SCM_CAR (p) &= ~SCM_OPN;
      scm_remove_from_port_table (p);
    }
  else
    {
      SCM_CAR (port) = scm_tc16_fport | scm_mode_bits (SCM_CHARS (modes));
      SCM_SETSTREAM (port, (SCM)f);
      if (SCM_BUF0 & (SCM_CAR (port) = scm_tc16_fport | scm_mode_bits (SCM_CHARS (modes))))
	scm_setbuf0 (port);
    }
  SCM_ALLOW_INTS;
  return port;
}



SCM_PROC (s_sys_duplicate_port, "duplicate-port", 2, 0, 0, scm_sys_duplicate_port);
#ifdef __STDC__
SCM 
scm_sys_duplicate_port (SCM oldpt, SCM modes)
#else
SCM 
scm_sys_duplicate_port (oldpt, modes)
     SCM oldpt;
     SCM modes;
#endif
{
  int oldfd;
  int newfd;
  FILE *f;
  SCM newpt;
  SCM_ASSERT (SCM_NIMP (oldpt) && SCM_OPPORTP (oldpt), oldpt, SCM_ARG1, s_sys_duplicate_port);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_STRINGP (modes), modes, SCM_ARG2, s_sys_duplicate_port);
  SCM_NEWCELL (newpt);
  SCM_DEFER_INTS;
  oldfd = fileno ((FILE *)SCM_STREAM (oldpt));
  if (oldfd == -1)
    SCM_SYSERROR (s_sys_duplicate_port);
  SCM_SYSCALL (newfd = dup (oldfd));
  if (newfd == -1)
    SCM_SYSERROR (s_sys_duplicate_port);
  f = fdopen (newfd, SCM_CHARS (modes));
  if (!f)
    {
      SCM_SYSCALL (close (newfd));
      SCM_SYSERROR (s_sys_duplicate_port);
    }
  {
    struct scm_port_table * pt;
    pt = scm_add_to_port_table (newpt);
    SCM_SETPTAB_ENTRY (newpt, pt);
    if (SCM_BUF0 & (SCM_CAR (newpt) = scm_tc16_fport | scm_mode_bits (SCM_CHARS (modes))))
      scm_setbuf0 (newpt);
    SCM_SETSTREAM (newpt, (SCM)f);
    SCM_PTAB_ENTRY (newpt)->file_name = SCM_PTAB_ENTRY (oldpt)->file_name;
  }
  SCM_ALLOW_INTS;
  return newpt;
}



SCM_PROC (s_sys_redirect_port, "redirect-port", 2, 0, 0, scm_sys_redirect_port);
#ifdef __STDC__
SCM 
scm_sys_redirect_port (SCM into_pt, SCM from_pt)
#else
SCM 
scm_sys_redirect_port (into_pt, from_pt)
     SCM into_pt;
     SCM from_pt;
#endif
{
  int ans, oldfd, newfd;
  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (into_pt) && SCM_OPPORTP (into_pt), into_pt, SCM_ARG1, s_sys_redirect_port);
  SCM_ASSERT (SCM_NIMP (from_pt) && SCM_OPPORTP (from_pt), from_pt, SCM_ARG2, s_sys_redirect_port);
  oldfd = fileno ((FILE *)SCM_STREAM (into_pt));
  if (oldfd == -1)
    SCM_SYSERROR (s_sys_redirect_port);
  newfd = fileno ((FILE *)SCM_STREAM (from_pt));
  if (newfd == -1)
    SCM_SYSERROR (s_sys_redirect_port);
  SCM_SYSCALL (ans = dup2 (oldfd, newfd));
  if (ans == -1)
    SCM_SYSERROR (s_sys_redirect_port);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_sys_fileno, "fileno", 1, 0, 0, scm_sys_fileno);
#ifdef __STDC__
SCM 
scm_sys_fileno (SCM port)
#else
SCM 
scm_sys_fileno (port)
     SCM port;
#endif
{
  int fd;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_sys_fileno);
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1)
    SCM_SYSERROR (s_sys_fileno);
  return SCM_MAKINUM (fd);
}

SCM_PROC (s_sys_isatty, "isatty?", 1, 0, 0, scm_sys_isatty_p);
#ifdef __STDC__
SCM 
scm_sys_isatty_p (SCM port)
#else
SCM 
scm_sys_isatty_p (port)
     SCM port;
#endif
{
  int rv;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_sys_isatty);
  rv = fileno ((FILE *)SCM_STREAM (port));
  if (rv == -1)
    SCM_SYSERROR (s_sys_isatty);
  rv = isatty (rv);
  return  rv ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC (s_sys_fdopen, "fdopen", 2, 0, 0, scm_sys_fdopen);
#ifdef __STDC__
SCM
scm_sys_fdopen (SCM fdes, SCM modes)
#else
SCM
scm_sys_fdopen (fdes, modes)
     SCM fdes;
     SCM modes;
#endif
{
  FILE *f;
  SCM port;
  struct scm_port_table * pt;

  SCM_ASSERT (SCM_INUMP (fdes), fdes, SCM_ARG1, s_sys_fdopen);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_STRINGP (modes), modes, SCM_ARG2, s_sys_fdopen);
  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  f = fdopen (SCM_INUM (fdes), SCM_CHARS (modes));
  if (f == NULL)
    SCM_SYSERROR (s_sys_fdopen);
  pt = scm_add_to_port_table (port);
  SCM_SETPTAB_ENTRY (port, pt);
  if (SCM_BUF0 & (SCM_CAR (port) = scm_tc16_fport
		  | scm_mode_bits (SCM_CHARS (modes))))
    scm_setbuf0 (port);
  SCM_SETSTREAM (port, (SCM)f);
  SCM_ALLOW_INTS;
  return port;
}



/* Move a port's underlying file descriptor to a given value.
 * Returns  #f if fdes is already the given value.
 *          #t if fdes moved. 
 * MOVE->FDES is implemented in Scheme and calls this primitive.
 */
SCM_PROC (s_sys_primitive_move_to_fdes, "primitive-move->fdes", 2, 0, 0, scm_sys_primitive_move_to_fdes);
#ifdef __STDC__
SCM
scm_sys_primitive_move_to_fdes (SCM port, SCM fd)
#else
SCM
scm_sys_primitive_move_to_fdes (port, fd)
     SCM port;
     SCM fd;
#endif
{
  FILE *stream;
  int old_fd;
  int new_fd;
  int rv;

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_sys_primitive_move_to_fdes);
  SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG2, s_sys_primitive_move_to_fdes);
  SCM_DEFER_INTS;
  stream = (FILE *)SCM_STREAM (port);
  old_fd = fileno (stream);
  new_fd = SCM_INUM (fd);
  if  (old_fd == new_fd)
    {
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
  scm_evict_ports (new_fd);
  rv = dup2 (old_fd, new_fd);
  if (rv == -1)
    SCM_SYSERROR (s_sys_primitive_move_to_fdes);
  scm_setfileno (stream, new_fd);
  SCM_SYSCALL (close (old_fd));  
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}

#ifdef __STDC__
void
scm_setfileno (FILE *fs, int fd)
#else
void
scm_setfileno (fs, fd)
     FILE *fs;
     int fd;
#endif
{
#ifdef SET_FILE_FD_FIELD
  SET_FILE_FD_FIELD(fs, fd);
#else
  Configure could not guess the name of the correct field in a FILE *.

  This function needs to be ported to your system.

  SET_FILE_FD_FIELD should change the descriptor refered to by a stdio
  stream, and nothing else.

  The way to port this file is to add cases to configure.in.  Search
  that file for "SET_FILE_FD_FIELD" and follow the examples there.
#endif
}

/* Move ports with the specified file descriptor to new descriptors,
 * reseting the revealed count to 0.
 * Should be called with SCM_DEFER_INTS active.
 */
#ifdef __STDC__
void
scm_evict_ports (int fd)
#else
void
scm_evict_ports (fd)
     int fd;
#endif
{
  int i;

  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_FPORTP (scm_port_table[i]->port)
	  && fileno ((FILE *)SCM_STREAM (scm_port_table[i]->port)) == fd)
	{
	  scm_setfileno ((FILE *)SCM_STREAM (scm_port_table[i]->port), dup (fd));
	  scm_set_port_revealed_x (scm_port_table[i]->port, SCM_MAKINUM (0));
	}
    }
}

/* Return a list of ports using a given file descriptor.  */
SCM_PROC(s_fdes_to_ports, "fdes->ports", 1, 0, 0, scm_fdes_to_ports);
#ifdef __STDC__
SCM
scm_fdes_to_ports (SCM fd)
#else
SCM
scm_fdes_to_ports (fd)
     SCM fd;
#endif
{
  SCM result = SCM_EOL;
  int int_fd;
  int i;
  
  SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG1, s_fdes_to_ports);
  int_fd = SCM_INUM (fd);

  SCM_DEFER_INTS;
  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_FPORTP (scm_port_table[i]->port)
	  && fileno ((FILE *)SCM_STREAM (scm_port_table[i]->port)) == int_fd)
	result = scm_cons (scm_port_table[i]->port, result);
    }
  SCM_ALLOW_INTS;
  return result;
}    

#ifdef __STDC__
void 
scm_init_ioext (void)
#else
void 
scm_init_ioext ()
#endif
{
  /* fseek() symbols.  */
  scm_sysintern ("SEEK_SET", SCM_MAKINUM (SEEK_SET));
  scm_sysintern ("SEEK_CUR", SCM_MAKINUM (SEEK_CUR));
  scm_sysintern ("SEEK_END", SCM_MAKINUM (SEEK_END));

  /* access() symbols.  */
  scm_sysintern ("R_OK", SCM_MAKINUM (R_OK));
  scm_sysintern ("W_OK", SCM_MAKINUM (W_OK));
  scm_sysintern ("X_OK", SCM_MAKINUM (X_OK));
  scm_sysintern ("F_OK", SCM_MAKINUM (F_OK));

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
#include "ioext.x"
}

