/*	Copyright (C) 1995, 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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
#include "ports.h"
#include "read.h"
#include "fports.h"
#include "unif.h"
#include "chars.h"

#include "ioext.h"

#include <fcntl.h>

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

static unsigned char *
scm_do_read_line (SCM port, int *len_p)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  unsigned char *end;

  /* I thought reading lines was simple.  Mercy me.  */

  /* The common case: the buffer contains a complete line. 
     This needs to be fast.  */
  if ((end = memchr (pt->read_pos, '\n', (pt->read_end - pt->read_pos)))
	   != 0)
    {
      int buf_len = (end + 1) - pt->read_pos;
      /* Allocate a buffer of the perfect size.  */
      unsigned char *buf = malloc (buf_len + 1);

      memcpy (buf, pt->read_pos, buf_len);
      pt->read_pos += buf_len;

      buf[buf_len] = '\0';

      *len_p = buf_len;
      return buf;
    }

  /* The buffer contains no newlines.  */
  {
    /* When live, len is always the number of characters in the
       current buffer that are part of the current line.  */
    int len = (pt->read_end - pt->read_pos);
    int buf_size = (len < 50) ? 60 : len * 2;
    /* Invariant: buf always has buf_size + 1 characters allocated;
       the `+ 1' is for the final '\0'.  */
    unsigned char *buf = malloc (buf_size + 1);
    int buf_len = 0;
    int c;

    for (;;)
      {
	if (buf_len + len > buf_size)
	  {
	    int new_size = (buf_len + len) * 2;
	    buf = realloc (buf, new_size + 1);
	    buf_size = new_size;
	  }

	/* Copy what we've got out of the port, into our buffer.  */
	memcpy (buf + buf_len, pt->read_pos, len);
	buf_len += len;
	pt->read_pos += len;

	/* If we had seen a newline, we're done now.  */
	if (end)
	  break;

	/* Get more characters.  I think having fill_buffer return a
           character is not terribly graceful...  */
	c = scm_fill_buffer (port, pt);
	if (c == EOF)
	  {
	    /* If we're missing a final newline in the file, return
	       what we did get, sans newline.  */
	    if (buf_len > 0)
	      break;

	    free (buf);
	    return 0;
	  }

	/* ... because it makes us duplicate code here...  */
	if (buf_len + 1 > buf_size)
	  {
	    int new_size = buf_size * 2;
	    buf = realloc (buf, new_size + 1);
	    buf_size = new_size;
	  }

	/* ... and this is really a duplication of the memcpy and
           memchr calls, on a single-byte buffer.  */
	buf[buf_len++] = c;
	if (c == '\n')
	  break;

	/* Search the buffer for newlines.  */
	if ((end = memchr (pt->read_pos, '\n',
			   (len = (pt->read_end - pt->read_pos))))
	    != 0)
	  len = (end - pt->read_pos) + 1;
      }

    /* I wonder how expensive this realloc is.  */
    buf = realloc (buf, buf_len + 1);
    buf[buf_len] = '\0';
    *len_p = buf_len;
    return buf;
  }
}  


/*
 * %read-line 
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
  scm_port *pt;
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

  pt = SCM_PTAB_ENTRY (port);
  if (pt->rw_active == SCM_PORT_WRITE)
    scm_ptobs[SCM_PTOBNUM (port)].fflush (port);

  s = scm_do_read_line (port, &slen);

  if (s == NULL)
    term = line = SCM_EOF_VAL;
  else
    {
      if (s[slen-1] == '\n')
	{
	  term = SCM_MAKICHR ('\n');
	  s[slen-1] = '\0';
	  line = scm_take_str (s, slen-1);
	  SCM_INCLINE (port);
	}
      else
	{
	  /* Fix: we should check for eof on the port before assuming this. */
	  term = SCM_EOF_VAL;
	  line = scm_take_str (s, slen);
	  SCM_COL (port) += slen;
	}	  
    }

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

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
  return scm_lseek (object, SCM_INUM0, SCM_MAKINUM (SEEK_CUR));
}

SCM_PROC (s_fseek, "fseek", 3, 0, 0, scm_fseek);

SCM 
scm_fseek (object, offset, whence)
     SCM object;
     SCM offset;
     SCM whence;
{
  scm_lseek (object, offset, whence);

  return SCM_UNSPECIFIED;
}

SCM_PROC (s_redirect_port, "redirect-port", 2, 0, 0, scm_redirect_port);

SCM 
scm_redirect_port (old, new)
     SCM old;
     SCM new;
{
  int ans, oldfd, newfd;
  struct scm_fport *fp;

  old = SCM_COERCE_OUTPORT (old);
  new = SCM_COERCE_OUTPORT (new);

  SCM_ASSERT (SCM_NIMP (old) && SCM_OPFPORTP (old), old, SCM_ARG1, s_redirect_port);
  SCM_ASSERT (SCM_NIMP (new) && SCM_OPFPORTP (new), new, SCM_ARG2, s_redirect_port);
  oldfd = SCM_FPORT_FDES (old);
  fp = SCM_FSTREAM (new);
  newfd = fp->fdes;
  if (oldfd != newfd)
    {
      scm_port *pt = SCM_PTAB_ENTRY (new);
      scm_port *old_pt = SCM_PTAB_ENTRY (old);
      scm_ptobfuns *ptob = &scm_ptobs[SCM_PTOBNUM (new)];

      /* must flush to old fdes.  */
      if (pt->rw_active == SCM_PORT_WRITE)
	ptob->fflush (new);
      else if (pt->rw_active == SCM_PORT_READ)
	ptob->read_flush (new);
      ans = dup2 (oldfd, newfd);
      if (ans == -1)
	scm_syserror (s_redirect_port);
      pt->rw_random = old_pt->rw_random;
      /* continue using existing buffers, even if inappropriate.  */
    }
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_dup_to_fdes, "dup->fdes", 1, 1, 0, scm_dup_to_fdes);
SCM 
scm_dup_to_fdes (SCM fd_or_port, SCM fd)
{
  int oldfd, newfd, rv;

  fd_or_port = SCM_COERCE_OUTPORT (fd_or_port);

  if (SCM_INUMP (fd_or_port))
    oldfd = SCM_INUM (fd_or_port);
  else
    {
      SCM_ASSERT (SCM_NIMP (fd_or_port) && SCM_OPFPORTP (fd_or_port),
		  fd_or_port, SCM_ARG1, s_dup_to_fdes);
      oldfd = SCM_FPORT_FDES (fd_or_port);
    }

  if (SCM_UNBNDP (fd))
    {
      newfd = dup (oldfd);
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
	  rv = dup2 (oldfd, newfd);
	  if (rv == -1)
	    scm_syserror (s_dup_to_fdes);
	}
    }
  return fd;
}

SCM_PROC (s_fileno, "fileno", 1, 0, 0, scm_fileno);

SCM 
scm_fileno (port)
     SCM port;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1,
	      s_fileno);
  return SCM_MAKINUM (SCM_FPORT_FDES (port));
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
  
  rv = isatty (SCM_FPORT_FDES (port));
  return  rv ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC (s_fdopen, "fdopen", 2, 0, 0, scm_fdopen);

SCM
scm_fdopen (fdes, modes)
     SCM fdes;
     SCM modes;
{
  SCM port;

  SCM_ASSERT (SCM_INUMP (fdes), fdes, SCM_ARG1, s_fdopen);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2,
	      s_fdopen);
  SCM_COERCE_SUBSTR (modes);
  port = scm_fdes_to_port (SCM_INUM (fdes), SCM_ROCHARS (modes), SCM_BOOL_F);
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
  struct scm_fport *stream;
  int old_fd;
  int new_fd;
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_primitive_move_to_fdes);
  SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG2, s_primitive_move_to_fdes);
  stream = SCM_FSTREAM (port);
  old_fd = stream->fdes;
  new_fd = SCM_INUM (fd);
  if  (old_fd == new_fd)
    {
      return SCM_BOOL_F;
    }
  scm_evict_ports (new_fd);
  rv = dup2 (old_fd, new_fd);
  if (rv == -1)
    scm_syserror (s_primitive_move_to_fdes);
  stream->fdes = new_fd;
  SCM_SYSCALL (close (old_fd));  
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

  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_OPFPORTP (scm_port_table[i]->port)
	  && ((struct scm_fport *) scm_port_table[i]->stream)->fdes == int_fd)
	result = scm_cons (scm_port_table[i]->port, result);
    }
  return result;
}    


void 
scm_init_ioext ()
{
#include "ioext.x"
}

