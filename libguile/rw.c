/* 	Copyright (C) 2001 Free Software Foundation, Inc.
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



/* This is the C part of the (ice-9 rw) module.  */

#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/fports.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/rw.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/modules.h"
#include "libguile/strports.h"

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
	    "Read characters from a port or file descriptor into a\n"
	    "string @var{str}.  A port must have an underlying file\n"
	    "descriptor --- a so-called fport.  This procedure is\n"
	    "scsh-compatible and can efficiently read large strings.\n"
	    "It will:\n\n"
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
	    "return fewer than the requested number of characters in some\n"
	    "cases, e.g., on end of file, if interrupted by a signal, or if\n"
	    "not all the characters are immediately available.\n"
	    "@item\n"
	    "wait indefinitely for some input if no characters are\n"
	    "currently available,\n"
	    "unless the port is in non-blocking mode.\n"
	    "@item\n"
	    "read characters from the port's input buffers if available,\n"
	    "instead from the underlying file descriptor.\n"
	    "@item\n"
	    "return @code{#f} if end-of-file is encountered before reading\n"
            "any characters, otherwise return the number of characters\n"
	    "read.\n"
	    "@item\n"
	    "return 0 if the port is in non-blocking mode and no characters\n"
	    "are immediately available.\n"
	    "@item\n"
	    "return 0 if the request is for 0 bytes, with no\n"
	    "end-of-file check.\n"
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

SCM_DEFINE (scm_write_string_partial, "write-string/partial", 1, 3, 0,
	    (SCM str, SCM port_or_fdes, SCM start, SCM end),
	    "Write characters from a string @var{str} to a port or file\n"
	    "descriptor.  A port must have an underlying file descriptor\n"
	    "--- a so-called fport.  This procedure is\n"
	    "scsh-compatible and can efficiently write large strings.\n"
	    "It will:\n\n"
	    "@itemize\n"
	    "@item\n"
	    "attempt to write the entire string, unless the @var{start}\n"
	    "and/or @var{end} arguments are supplied.  i.e., @var{start}\n"
	    "defaults to 0 and @var{end} defaults to\n"
	    "@code{(string-length str)}\n"
	    "@item\n"
	    "use the current output port if @var{port_of_fdes} is not\n"
	    "supplied.\n"
	    "@item\n"
	    "in the case of a buffered port, store the characters in the\n"
	    "port's output buffer, if all will fit.  If they will not fit\n"
	    "then any existing buffered characters will be flushed\n"
	    "before attempting\n"
	    "to write the new characters directly to the underlying file\n"
	    "descriptor.  If the port is in non-blocking mode and\n"
	    "buffered characters can not be flushed immediately, then an\n"
	    "@code{EAGAIN} system-error exception will be raised (Note:\n"
	    "scsh does not support the use of non-blocking buffered ports.)\n"
	    "@item\n"
	    "write fewer than the requested number of\n"
	    "characters in some cases, e.g., if interrupted by a signal or\n"
	    "if not all of the output can be accepted immediately.\n"
	    "@item\n"
	    "wait indefinitely for at least one character\n"
	    "from @var{str} to be accepted by the port, unless the port is\n"
	    "in non-blocking mode.\n"
	    "@item\n"
	    "return the number of characters accepted by the port.\n"
	    "@item\n"
	    "return 0 if the port is in non-blocking mode and can not accept\n"
	    "at least one character from @var{str} immediately\n"
	    "@item\n"
	    "return 0 immediately if the request size is 0 bytes.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_write_string_partial
{
  char *src;
  long write_len;
  int fdes;

  {
    long offset;
    long last;

    SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, src, 3, start, offset,
				      4, end, last);
    src += offset;
    write_len = last - offset;
  }

  if (write_len == 0)
    return SCM_INUM0;

  if (SCM_INUMP (port_or_fdes))
    fdes = SCM_INUM (port_or_fdes);
  else
    {
      SCM port = SCM_UNBNDP (port_or_fdes) ? scm_cur_outp : port_or_fdes;
      scm_t_port *pt;
      off_t space;

      SCM_VALIDATE_OPFPORT (2, port);
      SCM_VALIDATE_OUTPUT_PORT (2, port);
      pt = SCM_PTAB_ENTRY (port);
      /* filling the last character in the buffer would require a flush.  */
      space = pt->write_end - pt->write_pos - 1;
      if (space >= write_len)
	{
	  memcpy (pt->write_pos, src, write_len);
	  pt->write_pos += write_len;
	  return scm_long2num (write_len);
	}
      if (pt->write_pos > pt->write_buf)
	scm_flush (port);
      fdes = SCM_FPORT_FDES (port);
    }
  {
    long rv;

    SCM_SYSCALL (rv = write (fdes, src, write_len));
    if (rv == -1)
      {
	if (SCM_EBLOCK (errno))
	  rv = 0;
	else
	  SCM_SYSERROR;
      }
    
    return scm_long2num (rv);
  }
}
#undef FUNC_NAME

SCM 
scm_init_rw_builtins ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/rw.x"
#endif

  return SCM_UNSPECIFIED;
}

void
scm_init_rw ()
{
  scm_c_define_gsubr ("%init-rw-builtins", 0, 0, 0, scm_init_rw_builtins);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
