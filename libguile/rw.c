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

void 
scm_init_rw ()
{
  SCM rw_module = scm_make_module (scm_read_0str ("(ice-9 rw)"));
  SCM old_module = scm_set_current_module (rw_module);

#ifndef SCM_MAGIC_SNARFER
#include "libguile/rw.x"
#endif

  scm_set_current_module (old_module);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
