/* 	Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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



#include "libguile/_scm.h"
#include "libguile/ioext.h"
#include "libguile/fports.h"
#include "libguile/feature.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/validate.h"

#include <fcntl.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#if defined (EAGAIN)
#define SCM_MAYBE_EAGAIN || errno == EAGAIN
#else
#define SCM_MAYBE_EAGAIN
#endif

#if defined (EWOULDBLOCK)
#define SCM_MAYBE_EWOULDBLOCK || errno == EWOULDBLOCK
#else
#define SCM_MAYBE_EWOULDBLOCK
#endif

/* MAYBE there is EAGAIN way of defining this macro but now I EWOULDBLOCK.  */
#define SCM_EBLOCK(errno) \
   (0 SCM_MAYBE_EAGAIN SCM_MAYBE_EWOULDBLOCK)

SCM_DEFINE (scm_read_string_x_partial, "read-string!/partial", 1, 3, 0,
	    (SCM str, SCM port_or_fdes, SCM start, SCM end),
	    "Read characters from an fport or file descriptor into a\n"
	    "string @var{str}.  This procedure is scsh-compatible\n"
	    "and can efficiently read large strings.  It will:\n\n"
	    "@itemize\n"
	    "@item\n"
	    "attempt to fill the entire string, unless the @var{start}\n"
	    "and/or @var{end} arguments are supplied.  i.e., @var{start}\n"
	    "defaults to 0 and @var{end} defaults to\n"
	    "@code{(string-length str)}\n"
	    "@item\n"
	    "use the current input port if @var{port_or_fdes} is not\n"
	    "supplied.\n" 
	    "@item\n"
	    "read any characters that are currently available,\n"
	    "without waiting for the rest (short reads are possible).\n\n"
	    "@item\n"
	    "wait for as long as it needs to for the first character to\n"
	    "become available, unless the port is in non-blocking mode\n"
	    "@item\n"
	    "return @code{#f} if end-of-file is encountered before reading\n"
            "any characters, otherwise return the number of characters\n"
	    "read.\n"
	    "@item\n"
	    "return 0 if the port is in non-blocking mode and no characters\n"
	    "are immediately available.\n"
	    "@item\n"
	    "return 0 if the request is for 0 bytes, with no\n"
	    "end-of-file check\n"
	    "@end itemize")
#define FUNC_NAME s_scm_read_string_x_partial
{
  char *dest;
  long read_len;
  long chars_read = 0;
  int fdes;

  {
    long offset;
    long last;

    SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, dest, 3, start, offset,
				      4, end, last);
    dest += offset;
    read_len = last - offset;
  }

  if (SCM_INUMP (port_or_fdes))
    fdes = SCM_INUM (port_or_fdes);
  else
    {
      SCM port = SCM_UNBNDP (port_or_fdes) ? scm_cur_inp : port_or_fdes;

      SCM_VALIDATE_OPFPORT (2, port);
      SCM_VALIDATE_INPUT_PORT (2, port);

      /* if there's anything in the port buffers, use it, but then
	 don't touch the file descriptor.  otherwise the
	 "return immediately if something is available" rule may
	 be violated.  */
      chars_read = scm_take_from_input_buffers (port, dest, read_len);
      fdes = SCM_FPORT_FDES (port);
    }

  if (chars_read == 0 && read_len > 0) /* don't confuse read_len == 0 with
					  EOF.  */
    {
      SCM_SYSCALL (chars_read = read (fdes, dest, read_len));
      if (chars_read == -1)
	{
	  if (SCM_EBLOCK (errno))
	    chars_read = 0;
	  else
	    SCM_SYSERROR;
        }
      else if (chars_read == 0)
	return SCM_BOOL_F;
    }
  return scm_long2num (chars_read);
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


SCM_DEFINE (scm_dup2, "dup2", 2, 0, 0, 
            (SCM oldfd, SCM newfd),
	    "A simple wrapper for the @code{dup2} system call.\n"
	    "Copies the file descriptor @var{oldfd} to descriptor\n"
	    "number @var{newfd}, replacing the previous meaning\n"
	    "of @var{newfd}.  Both @var{oldfd} and @var{newfd} must\n"
	    "be integers.\n"
	    "Unlike for dup->fdes or primitive-move->fdes, no attempt\n"
	    "is made to move away ports which are using @var{newfd}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_dup2
{
  int c_oldfd;
  int c_newfd;
  int rv;

  SCM_VALIDATE_INUM_COPY (1, oldfd, c_oldfd);
  SCM_VALIDATE_INUM_COPY (2, newfd, c_newfd);
  rv = dup2 (c_oldfd, c_newfd);
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
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
  SCM_VALIDATE_STRING (2, modes);
  SCM_STRING_COERCE_0TERMINATION_X (modes);

  return scm_fdes_to_port (SCM_INUM (fdes), SCM_STRING_CHARS (modes), SCM_BOOL_F);
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

#ifndef SCM_MAGIC_SNARFER
#include "libguile/ioext.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
