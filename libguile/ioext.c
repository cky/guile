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



#include <stdio.h>
#include "_scm.h"
#include "genio.h"
#include "read.h"
#include "fports.h"
#include "unif.h"
#include "chars.h"

#include "ioext.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


SCM_PROC (s_read_delimited_x, "%read-delimited!", 3, 3, 0, scm_read_delimited_x);

SCM 
scm_read_delimited_x (delims, buf, gobble, port, start, end)
     SCM delims;
     SCM buf;
     SCM gobble;
     SCM port;
     SCM start;
     SCM end;
{
  long j;
  char *cbuf;
  long cstart;
  long cend;
  int c;
  char *cdelims;
  int num_delims;

  SCM_ASSERT (SCM_NIMP (delims) && SCM_ROSTRINGP (delims),
	      delims, SCM_ARG1, s_read_delimited_x);
  cdelims = SCM_ROCHARS (delims);
  num_delims = SCM_ROLENGTH (delims);
  SCM_ASSERT (SCM_NIMP (buf) && SCM_STRINGP (buf),
	      buf, SCM_ARG2, s_read_delimited_x);
  cbuf = SCM_CHARS (buf);
  cend = SCM_LENGTH (buf);
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    {
      SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port),
		  port, SCM_ARG1, s_read_delimited_x);
    }

  if (SCM_UNBNDP (start))
    cstart = 0;
  else
    {
      cstart = scm_num2long (start,
			     (char *) SCM_ARG5, s_read_delimited_x);
      if (cstart < 0 || cstart >= cend)
	scm_out_of_range (s_read_delimited_x, start);

      if (!SCM_UNBNDP (end))
	{
	  long tend = scm_num2long (end, (char *) SCM_ARG6,
				    s_read_delimited_x);
	  if (tend <= cstart || tend > cend)
	    scm_out_of_range (s_read_delimited_x, end);
	  cend = tend;
	}
    }

  for (j = cstart; j < cend; j++)
    {  
      int k;

      c = scm_gen_getc (port);
      for (k = 0; k < num_delims; k++)
	{
	  if (cdelims[k] == c)
	    {
	      if (SCM_FALSEP (gobble))
		scm_gen_ungetc (c, port);

	      return scm_cons (SCM_MAKICHR (c),
			       scm_long2num (j - cstart));
	    }
	}
      if (c == EOF)
	return scm_cons (SCM_EOF_VAL, 
			 scm_long2num (j - cstart));

      cbuf[j] = c;
    }
  return scm_cons (SCM_BOOL_F, scm_long2num (j - cstart));
}

SCM_PROC (s_write_line, "write-line", 1, 1, 0, scm_write_line);

SCM 
scm_write_line (obj, port)
     SCM obj;
     SCM port;
{
  scm_display (obj, port);
  return scm_newline (port);
}

SCM_PROC (s_ftell, "ftell", 1, 0, 0, scm_ftell);

SCM 
scm_ftell (port)
     SCM port;
{
  long pos;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_ftell);
  SCM_SYSCALL (pos = ftell ((FILE *)SCM_STREAM (port)));
  if (pos < 0)
    scm_syserror (s_ftell);
  if (pos > 0 && SCM_CRDYP (port))
    pos--;
  return scm_long2num (pos);
}



SCM_PROC (s_fseek, "fseek", 3, 0, 0, scm_fseek);

SCM 
scm_fseek (port, offset, whence)
     SCM port;
     SCM offset;
     SCM whence;
{
  int rv;
  long loff;

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_fseek);
  loff = scm_num2long (offset, (char *)SCM_ARG2, s_fseek);
  SCM_ASSERT (SCM_INUMP (whence) && (SCM_INUM (whence) < 3) && (SCM_INUM (whence) >= 0),
	  whence, SCM_ARG3, s_fseek);
  
  SCM_CLRDY (port);			/* Clear ungetted char */
  /* Values of whence are interned in scm_init_ioext.  */
  rv = fseek ((FILE *)SCM_STREAM (port), loff, SCM_INUM (whence));
  if (rv != 0)
    scm_syserror (s_fseek);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_freopen, "freopen", 3, 0, 0, scm_freopen);

SCM 
scm_freopen (filename, modes, port)
     SCM filename;
     SCM modes;
     SCM port;
{
  FILE *f;
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_freopen);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2,
	      s_freopen);

  SCM_COERCE_SUBSTR (filename);
  SCM_COERCE_SUBSTR (modes);
  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (port) && SCM_FPORTP (port), port, SCM_ARG3, s_freopen);
  SCM_SYSCALL (f = freopen (SCM_ROCHARS (filename), SCM_ROCHARS (modes),
			    (FILE *)SCM_STREAM (port)));
  if (!f)
    {
      SCM p;
      p = port;
      port = SCM_MAKINUM (errno);
      SCM_SETAND_CAR (p, ~SCM_OPN);
      scm_remove_from_port_table (p);
    }
  else
    {
      SCM_SETCAR (port, scm_tc16_fport | scm_mode_bits (SCM_ROCHARS (modes)));
      SCM_SETSTREAM (port, (SCM)f);
      SCM_SETCAR (port, scm_tc16_fport | scm_mode_bits (SCM_ROCHARS (modes)));
      if (SCM_BUF0 & SCM_CAR (port))
	scm_setbuf0 (port);
    }
  SCM_ALLOW_INTS;
  return port;
}

SCM_PROC (s_redirect_port, "redirect-port", 2, 0, 0, scm_redirect_port);

SCM 
scm_redirect_port (old, new)
     SCM old;
     SCM new;
{
  int ans, oldfd, newfd;

  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (old) && SCM_OPPORTP (old), old, SCM_ARG1, s_redirect_port);
  SCM_ASSERT (SCM_NIMP (new) && SCM_OPPORTP (new), new, SCM_ARG2, s_redirect_port);
  oldfd = fileno ((FILE *)SCM_STREAM (old));
  if (oldfd == -1)
    scm_syserror (s_redirect_port);
  newfd = fileno ((FILE *)SCM_STREAM (new));
  if (newfd == -1)
    scm_syserror (s_redirect_port);
  SCM_SYSCALL (ans = dup2 (oldfd, newfd));
  if (ans == -1)
    scm_syserror (s_redirect_port);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_primitive_dup, "primitive-dup", 1, 0, 0, scm_primitive_dup);
SCM 
scm_primitive_dup (SCM fd_or_port)
{
  int fd, newfd;

  SCM_DEFER_INTS;
  if (SCM_INUMP (fd_or_port))
    fd = SCM_INUM (fd_or_port);
  else
    {
      SCM_ASSERT (SCM_NIMP (fd_or_port) && SCM_OPPORTP (fd_or_port),
		  fd_or_port, SCM_ARG1, s_primitive_dup);
      fd = fileno ((FILE *)SCM_STREAM (fd_or_port));
      if (fd == -1)
	scm_syserror (s_primitive_dup);
    }
  SCM_SYSCALL (newfd = dup (fd));
  if (newfd == -1)
    scm_syserror (s_primitive_dup);
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (newfd);
}

SCM_PROC (s_primitive_dup2, "primitive-dup2", 2, 0, 0, scm_primitive_dup2);
SCM 
scm_primitive_dup2 (SCM fd_or_port, SCM fd)
{
  int oldfd, newfd, rv;

  SCM_DEFER_INTS;
  if (SCM_INUMP (fd_or_port))
    oldfd = SCM_INUM (fd_or_port);
  else
    {
      SCM_ASSERT (SCM_NIMP (fd_or_port) && SCM_OPPORTP (fd_or_port),
		  fd_or_port, SCM_ARG1, s_primitive_dup2);
      oldfd = fileno ((FILE *)SCM_STREAM (fd_or_port));
      if (oldfd == -1)
	scm_syserror (s_primitive_dup2);
    }
  
  SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG2, s_primitive_dup2);
  newfd = SCM_INUM (fd);
  if (oldfd == newfd)
    {
      SCM_ALLOW_INTS;
      return fd;
    }
  scm_evict_ports (newfd);	/* see scsh manual.  */
  SCM_SYSCALL (rv = dup2 (oldfd, newfd));
  if (rv == -1)
    scm_syserror (s_primitive_dup2);
  SCM_ALLOW_INTS;
  return fd;
}

SCM_PROC (s_fileno, "fileno", 1, 0, 0, scm_fileno);

SCM 
scm_fileno (port)
     SCM port;
{
  int fd;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_fileno);
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1)
    scm_syserror (s_fileno);
  return SCM_MAKINUM (fd);
}

SCM_PROC (s_isatty, "isatty?", 1, 0, 0, scm_isatty_p);

SCM 
scm_isatty_p (port)
     SCM port;
{
  int rv;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_isatty);
  rv = fileno ((FILE *)SCM_STREAM (port));
  if (rv == -1)
    scm_syserror (s_isatty);
  rv = isatty (rv);
  return  rv ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC (s_fdopen, "fdopen", 2, 0, 0, scm_fdopen);

SCM
scm_fdopen (fdes, modes)
     SCM fdes;
     SCM modes;
{
  FILE *f;
  SCM port;
  struct scm_port_table * pt;

  SCM_ASSERT (SCM_INUMP (fdes), fdes, SCM_ARG1, s_fdopen);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2,
	      s_fdopen);
  SCM_COERCE_SUBSTR (modes);
  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  f = fdopen (SCM_INUM (fdes), SCM_ROCHARS (modes));
  if (f == NULL)
    scm_syserror (s_fdopen);
  pt = scm_add_to_port_table (port);
  SCM_SETPTAB_ENTRY (port, pt);
  SCM_SETCAR (port, scm_tc16_fport | scm_mode_bits (SCM_ROCHARS (modes)));
  SCM_SETSTREAM (port, (SCM)f);
  if (SCM_BUF0 & SCM_CAR (port))
    scm_setbuf0 (port);
  SCM_ALLOW_INTS;
  return port;
}



/* Move a port's underlying file descriptor to a given value.
 * Returns  #f if fdes is already the given value.
 *          #t if fdes moved. 
 * MOVE->FDES is implemented in Scheme and calls this primitive.
 */
SCM_PROC (s_primitive_move_to_fdes, "primitive-move->fdes", 2, 0, 0, scm_primitive_move_to_fdes);

SCM
scm_primitive_move_to_fdes (port, fd)
     SCM port;
     SCM fd;
{
  FILE *stream;
  int old_fd;
  int new_fd;
  int rv;

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_primitive_move_to_fdes);
  SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG2, s_primitive_move_to_fdes);
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
    scm_syserror (s_primitive_move_to_fdes);
  scm_setfileno (stream, new_fd);
  SCM_SYSCALL (close (old_fd));  
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}

#ifdef FD_SETTER
#define SET_FILE_FD_FIELD(F,D) ((F)->FD_SETTER = (D))
#endif

void
scm_setfileno (fs, fd)
     FILE *fs;
     int fd;
{
#ifdef SET_FILE_FD_FIELD
  SET_FILE_FD_FIELD(fs, fd);
#else
  scm_misc_error ("scm_setfileno", "Not fully implemented on this platform",
		  SCM_EOL);
#endif
}

/* Return a list of ports using a given file descriptor.  */
SCM_PROC(s_fdes_to_ports, "fdes->ports", 1, 0, 0, scm_fdes_to_ports);

SCM
scm_fdes_to_ports (fd)
     SCM fd;
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


void 
scm_init_ioext ()
{
  /* fseek() symbols.  */
  scm_sysintern ("SEEK_SET", SCM_MAKINUM (SEEK_SET));
  scm_sysintern ("SEEK_CUR", SCM_MAKINUM (SEEK_CUR));
  scm_sysintern ("SEEK_END", SCM_MAKINUM (SEEK_END));

#include "ioext.x"
}

