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

      c = scm_getc (port);
      for (k = 0; k < num_delims; k++)
	{
	  if (cdelims[k] == c)
	    {
	      if (SCM_FALSEP (gobble))
		scm_ungetc (c, port);

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

/*
 * %read-line uses a port's fgets method for fast line i/o.  It
 * truncates any terminating newline from its input, and returns
 * a cons of the string read and its terminating character.  Doing
 * so makes it easy to implement the hairy `read-line' options
 * efficiently in Scheme.
 */

SCM_PROC (s_read_line, "%read-line", 0, 1, 0, scm_read_line);

SCM
scm_read_line (port)
     SCM port;
{
  char *s;
  int slen;
  SCM line, term;

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    {
      SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port),
		  port, SCM_ARG1, s_read_line);
    }

  s = scm_do_read_line (port, &slen);

  if (s == NULL)
    term = line = SCM_EOF_VAL;
  else
    {
      if (s[slen-1] == '\n')
	{
	  term = SCM_MAKICHR ('\n');
	  line = scm_makfromstr (s, slen-1, 0);
	}
      else
	{
	  /* Fix: we should check for eof on the port before assuming this. */
	  term = SCM_EOF_VAL;
	  line = scm_makfromstr (s, slen, 0);
	}	  
      free (s);
    }

  return scm_cons (line, term);
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
scm_ftell (object)
     SCM object;
{
  long pos;

  object = SCM_COERCE_OUTPORT (object);

  SCM_DEFER_INTS;
  if (SCM_NIMP (object) && SCM_OPFPORTP (object))
    {
      SCM_SYSCALL (pos = ftell ((FILE *)SCM_STREAM (object)));
      if (pos > 0 && SCM_CRDYP (object))
	pos--;
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (object), object, SCM_ARG1, s_ftell);
      SCM_SYSCALL (pos = lseek (SCM_INUM (object), 0, SEEK_CUR));
    }
  if (pos < 0)
    scm_syserror (s_ftell);
  SCM_ALLOW_INTS;
  return scm_long2num (pos);
}



SCM_PROC (s_fseek, "fseek", 3, 0, 0, scm_fseek);

SCM 
scm_fseek (object, offset, whence)
     SCM object;
     SCM offset;
     SCM whence;
{
  int rv;
  long loff;

  object = SCM_COERCE_OUTPORT (object);

  loff = scm_num2long (offset, (char *)SCM_ARG2, s_fseek);
  SCM_ASSERT (SCM_INUMP (whence), whence, SCM_ARG3, s_fseek);
  SCM_DEFER_INTS;
  if (SCM_NIMP (object) && SCM_OPFPORTP (object))
    {
      SCM_CLRDY (object);			/* Clear ungetted char */
      rv = fseek ((FILE *)SCM_STREAM (object), loff, SCM_INUM (whence));
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (object), object, SCM_ARG1, s_fseek);
      rv = lseek (SCM_INUM (object), loff, SCM_INUM (whence));
    }
  if (rv < 0)
    scm_syserror (s_fseek);
  SCM_ALLOW_INTS;
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
  port = SCM_COERCE_OUTPORT (port);
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

  old = SCM_COERCE_OUTPORT (old);
  new = SCM_COERCE_OUTPORT (new);

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

SCM_PROC (s_dup_to_fdes, "dup->fdes", 1, 1, 0, scm_dup_to_fdes);
SCM 
scm_dup_to_fdes (SCM fd_or_port, SCM fd)
{
  int oldfd, newfd, rv;

  fd_or_port = SCM_COERCE_OUTPORT (fd_or_port);

  SCM_DEFER_INTS;
  if (SCM_INUMP (fd_or_port))
    oldfd = SCM_INUM (fd_or_port);
  else
    {
      SCM_ASSERT (SCM_NIMP (fd_or_port) && SCM_OPPORTP (fd_or_port),
		  fd_or_port, SCM_ARG1, s_dup_to_fdes);
      oldfd = fileno ((FILE *)SCM_STREAM (fd_or_port));
      if (oldfd == -1)
	scm_syserror (s_dup_to_fdes);
    }

  if (SCM_UNBNDP (fd))
    {
      SCM_SYSCALL (newfd = dup (oldfd));
      if (newfd == -1)
	scm_syserror (s_dup_to_fdes);
      fd = SCM_MAKINUM (newfd);
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG2, s_dup_to_fdes);
      newfd = SCM_INUM (fd);
      if (oldfd != newfd)
	{
	  scm_evict_ports (newfd);	/* see scsh manual.  */
	  SCM_SYSCALL (rv = dup2 (oldfd, newfd));
	  if (rv == -1)
	    scm_syserror (s_dup_to_fdes);
	}
    }
  SCM_ALLOW_INTS;
  return fd;
}

SCM_PROC (s_fileno, "fileno", 1, 0, 0, scm_fileno);

SCM 
scm_fileno (port)
     SCM port;
{
  int fd;

  port = SCM_COERCE_OUTPORT (port);

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

  port = SCM_COERCE_OUTPORT (port);

  if (!(SCM_NIMP (port) && SCM_OPFPORTP (port)))
    return SCM_BOOL_F;
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

  port = SCM_COERCE_OUTPORT (port);

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

