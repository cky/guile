/*	Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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
#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/read.h"
#include "libguile/fports.h"
#include "libguile/unif.h"
#include "libguile/chars.h"
#include "libguile/feature.h"
#include "libguile/root.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/ioext.h"

#include <fcntl.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


SCM_DEFINE (scm_read_delimited_x, "%read-delimited!", 3, 3, 0,
            (SCM delims, SCM buf, SCM gobble, SCM port, SCM start, SCM end),
	    "Read characters from @var{port} into @var{buf} until one of the\n"
	    "characters in the @var{delims} string is encountered.  If @var{gobble?}\n"
	    "is true, store the delimiter character in @var{buf} as well; otherwise,\n"
	    "discard it.  If @var{port} is not specified, use the value of\n"
	    "@code{(current-input-port)}.  If @var{start} or @var{end} are specified,\n"
	    "store data only into the substring of @var{buf} bounded by @var{start}\n"
	    "and @var{end} (which default to the beginning and end of the buffer,\n"
	    "respectively).\n\n"
	    "Return a pair consisting of the delimiter that terminated the string and\n"
	    "the number of characters read.  If reading stopped at the end of file,\n"
	    "the delimiter returned is the @var{eof-object}; if the buffer was filled\n"
	    "without encountering a delimiter, this value is @var{#f}.")
#define FUNC_NAME s_scm_read_delimited_x
{
  long j;
  char *cbuf;
  long cstart;
  long cend, tend;
  int c;
  char *cdelims;
  int num_delims;

  SCM_VALIDATE_ROSTRING_COPY (1,delims,cdelims);
  num_delims = SCM_ROLENGTH (delims);
  SCM_VALIDATE_STRING_COPY (2,buf,cbuf);
  cend = SCM_STRING_LENGTH (buf);
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_VALIDATE_OPINPORT (4,port);

  SCM_VALIDATE_INUM_DEF_COPY (5,start,0,cstart);
  SCM_ASSERT_RANGE(5, start, cstart >= 0 && cstart < cend);

  SCM_VALIDATE_INUM_DEF_COPY (6,end,cend,tend);
  SCM_ASSERT_RANGE(6, end, tend > cstart && tend <= cend);

  cend = tend;

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

	      return scm_cons (SCM_MAKE_CHAR (c),
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
#undef FUNC_NAME

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
      unsigned char *buf = scm_must_malloc (buf_len + 1, "%read-line");

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
    unsigned char *buf = scm_must_malloc (buf_size + 1, "%read-line");
    int buf_len = 0;

    for (;;)
      {
	if (buf_len + len > buf_size)
	  {
	    int new_size = (buf_len + len) * 2;
	    buf = scm_must_realloc (buf, buf_size + 1, new_size + 1,
				    "%read-line");
	    buf_size = new_size;
	  }

	/* Copy what we've got out of the port, into our buffer.  */
	memcpy (buf + buf_len, pt->read_pos, len);
	buf_len += len;
	pt->read_pos += len;

	/* If we had seen a newline, we're done now.  */
	if (end)
	  break;

	/* Get more characters.  */
	if (scm_fill_input (port) == EOF)
	  {
	    /* If we're missing a final newline in the file, return
	       what we did get, sans newline.  */
	    if (buf_len > 0)
	      break;

	    free (buf);
	    return 0;
	  }

	/* Search the buffer for newlines.  */
	if ((end = memchr (pt->read_pos, '\n',
			   (len = (pt->read_end - pt->read_pos))))
	    != 0)
	  len = (end - pt->read_pos) + 1;
      }

    /* I wonder how expensive this realloc is.  */
    buf = scm_must_realloc (buf, buf_size + 1, buf_len + 1, "%read-line");
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

SCM_DEFINE (scm_read_line, "%read-line", 0, 1, 0, 
            (SCM port),
	    "Read a newline-terminated line from @var{port}, allocating storage as\n"
	    "necessary.  The newline terminator (if any) is removed from the string,\n"
	    "and a pair consisting of the line and its delimiter is returned.  The\n"
	    "delimiter may be either a newline or the @var{eof-object}; if\n"
	    "@code{%read-line} is called at the end of file, it returns the pair\n"
	    "@code{(#<eof> . #<eof>)}.")
#define FUNC_NAME s_scm_read_line
{
  scm_port *pt;
  char *s;
  int slen;
  SCM line, term;

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  SCM_VALIDATE_OPINPORT (1,port);

  pt = SCM_PTAB_ENTRY (port);
  if (pt->rw_active == SCM_PORT_WRITE)
    scm_ptobs[SCM_PTOBNUM (port)].flush (port);

  s = (char *) scm_do_read_line (port, &slen);

  if (s == NULL)
    term = line = SCM_EOF_VAL;
  else
    {
      if (s[slen-1] == '\n')
	{
	  term = SCM_MAKE_CHAR ('\n');
	  s[slen-1] = '\0';
	  line = scm_take_str (s, slen-1);
	  scm_done_malloc (-1);
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
#undef FUNC_NAME

SCM_DEFINE (scm_write_line, "write-line", 1, 1, 0,
            (SCM obj, SCM port),
	    "Display @var{obj} and a newline character to @var{port}.  If @var{port}\n"
	    "is not specified, @code{(current-output-port)} is used.  This function\n"
	    "is equivalent to:\n\n"
	    "@smalllisp\n"
	    "(display obj [port])\n"
	    "(newline [port])\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_write_line
{
  scm_display (obj, port);
  return scm_newline (port);
}
#undef FUNC_NAME

SCM_DEFINE (scm_ftell, "ftell", 1, 0, 0, 
            (SCM object),
	    "Returns an integer representing the current position of @var{fd/port},\n"
	    "measured from the beginning.  Equivalent to:\n"
	    "@smalllisp\n"
	    "(seek port 0 SEEK_CUR)\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_ftell
{
  return scm_seek (object, SCM_INUM0, SCM_MAKINUM (SEEK_CUR));
}
#undef FUNC_NAME


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_DEFINE (scm_fseek, "fseek", 3, 0, 0,
            (SCM object, SCM offset, SCM whence),
	    "Obsolete.  Almost the same as seek, above, but the return value is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_fseek
{
  scm_seek (object, offset, whence);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif  /* SCM_DEBUG_DEPRECATED == 0 */


SCM_DEFINE (scm_redirect_port, "redirect-port", 2, 0, 0,
            (SCM old, SCM new),
	    "This procedure takes two ports and duplicates the underlying file\n"
	    "descriptor from @var{old-port} into @var{new-port}.  The\n"
	    "current file descriptor in @var{new-port} will be closed.\n"
	    "After the redirection the two ports will share a file position\n"
	    "and file status flags.\n\n"
	    "The return value is unspecified.\n\n"
	    "Unexpected behaviour can result if both ports are subsequently used\n"
	    "and the original and/or duplicate ports are buffered.\n\n"
	    "This procedure does not have any side effects on other ports or\n"
	    "revealed counts.")
#define FUNC_NAME s_scm_redirect_port
{
  int ans, oldfd, newfd;
  struct scm_fport *fp;

  old = SCM_COERCE_OUTPORT (old);
  new = SCM_COERCE_OUTPORT (new);
  
  SCM_VALIDATE_OPFPORT (1,old);
  SCM_VALIDATE_OPFPORT (2,new);
  oldfd = SCM_FPORT_FDES (old);
  fp = SCM_FSTREAM (new);
  newfd = fp->fdes;
  if (oldfd != newfd)
    {
      scm_port *pt = SCM_PTAB_ENTRY (new);
      scm_port *old_pt = SCM_PTAB_ENTRY (old);
      scm_ptob_descriptor *ptob = &scm_ptobs[SCM_PTOBNUM (new)];

      /* must flush to old fdes.  */
      if (pt->rw_active == SCM_PORT_WRITE)
	ptob->flush (new);
      else if (pt->rw_active == SCM_PORT_READ)
	scm_end_input (new);
      ans = dup2 (oldfd, newfd);
      if (ans == -1)
	SCM_SYSERROR;
      pt->rw_random = old_pt->rw_random;
      /* continue using existing buffers, even if inappropriate.  */
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_dup_to_fdes, "dup->fdes", 1, 1, 0, 
            (SCM fd_or_port, SCM fd),
	    "Returns an integer file descriptor.")
#define FUNC_NAME s_scm_dup_to_fdes
{
  int oldfd, newfd, rv;

  fd_or_port = SCM_COERCE_OUTPORT (fd_or_port);

  if (SCM_INUMP (fd_or_port))
    oldfd = SCM_INUM (fd_or_port);
  else
    {
      SCM_VALIDATE_OPFPORT (1,fd_or_port);
      oldfd = SCM_FPORT_FDES (fd_or_port);
    }

  if (SCM_UNBNDP (fd))
    {
      newfd = dup (oldfd);
      if (newfd == -1)
	SCM_SYSERROR;
      fd = SCM_MAKINUM (newfd);
    }
  else
    {
      SCM_VALIDATE_INUM_COPY (2, fd, newfd);
      if (oldfd != newfd)
	{
	  scm_evict_ports (newfd);	/* see scsh manual.  */
	  rv = dup2 (oldfd, newfd);
	  if (rv == -1)
	    SCM_SYSERROR;
	}
    }
  return fd;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fileno, "fileno", 1, 0, 0, 
            (SCM port),
	    "Returns the integer file descriptor underlying @var{port}.\n"
	    "Does not change its revealed count.")
#define FUNC_NAME s_scm_fileno
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPFPORT (1,port);
  return SCM_MAKINUM (SCM_FPORT_FDES (port));
}
#undef FUNC_NAME

/* GJB:FIXME:: why does this not throw
   an error if the arg is not a port?
   This proc as is would be better names isattyport?
   if it is not going to assume that the arg is a port */
SCM_DEFINE (scm_isatty_p, "isatty?", 1, 0, 0, 
            (SCM port),
	    "Returns @code{#t} if @var{port} is using a serial\n"
	    "non-file device, otherwise @code{#f}.")
#define FUNC_NAME s_scm_isatty_p
{
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  if (!SCM_OPFPORTP (port))
    return SCM_BOOL_F;
  
  rv = isatty (SCM_FPORT_FDES (port));
  return  SCM_BOOL(rv);
}
#undef FUNC_NAME



SCM_DEFINE (scm_fdopen, "fdopen", 2, 0, 0,
            (SCM fdes, SCM modes),
	    "Returns a new port based on the file descriptor @var{fdes}.\n"
	    "Modes are given by the string @var{modes}.  The revealed count of the port\n"
	    "is initialized to zero.  The modes string is the same as that accepted\n"
	    "by @ref{File Ports, open-file}.")
#define FUNC_NAME s_scm_fdopen
{
  SCM_VALIDATE_INUM (1,fdes);
  SCM_VALIDATE_ROSTRING (2,modes);
  SCM_COERCE_SUBSTR (modes);

  return scm_fdes_to_port (SCM_INUM (fdes), SCM_ROCHARS (modes), SCM_BOOL_F);
}
#undef FUNC_NAME



/* Move a port's underlying file descriptor to a given value.
 * Returns  #f if fdes is already the given value.
 *          #t if fdes moved. 
 * MOVE->FDES is implemented in Scheme and calls this primitive.
 */
SCM_DEFINE (scm_primitive_move_to_fdes, "primitive-move->fdes", 2, 0, 0,
            (SCM port, SCM fd),
	    "Moves the underlying file descriptor for @var{port} to the integer\n"
	    "value @var{fdes} without changing the revealed count of @var{port}.\n"
	    "Any other ports already using this descriptor will be automatically\n"
	    "shifted to new descriptors and their revealed counts reset to zero.\n"
	    "The return value is @code{#f} if the file descriptor already had the\n"
	    "required value or @code{#t} if it was moved.")
#define FUNC_NAME s_scm_primitive_move_to_fdes
{
  struct scm_fport *stream;
  int old_fd;
  int new_fd;
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPFPORT (1,port);
  SCM_VALIDATE_INUM (2,fd);
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
    SCM_SYSERROR;
  stream->fdes = new_fd;
  SCM_SYSCALL (close (old_fd));  
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/* Return a list of ports using a given file descriptor.  */
SCM_DEFINE (scm_fdes_to_ports, "fdes->ports", 1, 0, 0, 
           (SCM fd),
	    "Returns a list of existing ports which have @var{fdes} as an\n"
	    "underlying file descriptor, without changing their revealed counts.")
#define FUNC_NAME s_scm_fdes_to_ports
{
  SCM result = SCM_EOL;
  int int_fd;
  int i;
  
  SCM_VALIDATE_INUM_COPY (1,fd,int_fd);

  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_OPFPORTP (scm_port_table[i]->port)
	  && ((struct scm_fport *) scm_port_table[i]->stream)->fdes == int_fd)
	result = scm_cons (scm_port_table[i]->port, result);
    }
  return result;
}
#undef FUNC_NAME    


void 
scm_init_ioext ()
{
  scm_add_feature ("i/o-extensions");

#include "libguile/ioext.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
