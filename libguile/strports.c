/*	Copyright (C) 1995,1996,1998,1999, 2000 Free Software Foundation, Inc.
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

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libguile/unif.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/read.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/modules.h"

#include "libguile/strports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - string ports}
 *
 */

/* NOTES:
   write_buf/write_end point to the ends of the allocated string.
   read_buf/read_end in principle point to the part of the string which
   has been written to, but this is only updated after a flush.
   read_pos and write_pos in principle should be equal, but this is only true
   when rw_active is SCM_PORT_NEITHER.
*/


static scm_bits_t scm_tc16_strport;


static int
stfill_buffer (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  
  if (pt->read_pos >= pt->read_end)
    return EOF;
  else
    return scm_return_first_int (*pt->read_pos, port);
}

/* change the size of a port's string to new_size.  this doesn't
   change read_buf_size.  */
static void 
st_resize_port (scm_port *pt, off_t new_size)
{
  SCM old_stream = SCM_PACK (pt->stream);
  SCM new_stream = scm_makstr (new_size, 0);
  unsigned long int old_size = SCM_STRING_LENGTH (old_stream);
  unsigned long int min_size = min (old_size, new_size);
  unsigned long int i;

  off_t index = pt->write_pos - pt->write_buf;

  pt->write_buf_size = new_size;

  for (i = 0; i != min_size; ++i)
    SCM_STRING_CHARS (new_stream) [i] = SCM_STRING_CHARS (old_stream) [i];

  /* reset buffer. */
  {
    pt->stream = new_stream;
    pt->read_buf = pt->write_buf = SCM_STRING_UCHARS (new_stream);
    pt->read_pos = pt->write_pos = pt->write_buf + index;
    pt->write_end = pt->write_buf + pt->write_buf_size;
    pt->read_end = pt->read_buf + pt->read_buf_size;
  }
}

/* amount by which write_buf is expanded.  */
#define SCM_WRITE_BLOCK 80

/* ensure that write_pos < write_end by enlarging the buffer when
   necessary.  update read_buf to account for written chars.  */
static void
st_flush (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->write_pos == pt->write_end)
    {
      st_resize_port (pt, pt->write_buf_size + SCM_WRITE_BLOCK);
    }
  pt->read_pos = pt->write_pos;
  if (pt->read_pos > pt->read_end)
    {
      pt->read_end = (unsigned char *) pt->read_pos;
      pt->read_buf_size = pt->read_end - pt->read_buf;
    }
  pt->rw_active = SCM_PORT_NEITHER;
}

static void
st_write (SCM port, const void *data, size_t size)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  const char *input = (char *) data;

  while (size > 0)
    {
      int space = pt->write_end - pt->write_pos;
      int write_len = (size > space) ? space : size;
      
      strncpy ((char *) pt->write_pos, input, write_len);
      pt->write_pos += write_len;
      size -= write_len;
      input += write_len;
      if (write_len == space)
	st_flush (port);
    }
}

static void
st_end_input (SCM port, int offset)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  
  if (pt->read_pos - pt->read_buf < offset)
    scm_misc_error ("st_end_input", "negative position", SCM_EOL);

  pt->write_pos = (unsigned char *) (pt->read_pos = pt->read_pos - offset);
  pt->rw_active = SCM_PORT_NEITHER;
}

static off_t
st_seek (SCM port, off_t offset, int whence)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  off_t target;

  if (pt->rw_active == SCM_PORT_READ && offset == 0 && whence == SEEK_CUR)
    /* special case to avoid disturbing the unread-char buffer.  */
    {
      if (pt->read_buf == pt->putback_buf)
	{
	  target = pt->saved_read_pos - pt->saved_read_buf
	    - (pt->read_end - pt->read_pos);
	}
      else
	{
	  target = pt->read_pos - pt->read_buf;
	}
    }
  else
    /* all other cases.  */
    {
      if (pt->rw_active == SCM_PORT_WRITE)
	st_flush (port);
  
      if (pt->rw_active == SCM_PORT_READ)
	scm_end_input (port);

      switch (whence)
	{
	case SEEK_CUR:
	  target = pt->read_pos - pt->read_buf + offset;
	  break;
	case SEEK_END:
	  target = pt->read_end - pt->read_buf + offset;
	  break;
	default: /* SEEK_SET */
	  target = offset;
	  break;
	}

      if (target < 0)
	scm_misc_error ("st_seek", "negative offset", SCM_EOL);
  
      if (target >= pt->write_buf_size)
	{
	  if (!(SCM_CELL_WORD_0 (port) & SCM_WRTNG))
	    {
	      if (target > pt->write_buf_size)
		{
		  scm_misc_error ("st_seek", 
				  "seek past end of read-only strport",
				  SCM_EOL);
		}
	    }
	  else
	    {
	      st_resize_port (pt, target + (target == pt->write_buf_size
					    ? SCM_WRITE_BLOCK
					    : 0));
	    }
	}
      pt->read_pos = pt->write_pos = pt->read_buf + target;
      if (pt->read_pos > pt->read_end)
	{
	  pt->read_end = (unsigned char *) pt->read_pos;
	  pt->read_buf_size = pt->read_end - pt->read_buf;
	}
    }
  return target;
}

static void
st_truncate (SCM port, off_t length)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (length > pt->write_buf_size)
    st_resize_port (pt, length);

  pt->read_buf_size = length;
  pt->read_end = pt->read_buf + length;
  if (pt->read_pos > pt->read_end)
    pt->read_pos = pt->read_end;
  
  if (pt->write_pos > pt->read_end)
    pt->write_pos = pt->read_end;
}

SCM 
scm_mkstrport (SCM pos, SCM str, long modes, const char *caller)
{
  SCM z;
  scm_port *pt;
  int str_len;

  SCM_ASSERT (SCM_INUMP(pos) && SCM_INUM(pos) >= 0, pos, SCM_ARG1, caller);
  SCM_ASSERT (SCM_STRINGP (str), str, SCM_ARG1, caller);
  str_len = SCM_STRING_LENGTH (str);
  if (SCM_INUM (pos) > str_len)
    scm_out_of_range (caller, pos);
  if (!((modes & SCM_WRTNG) || (modes & SCM_RDNG)))
    scm_misc_error ("scm_mkstrport", "port must read or write", SCM_EOL);
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (z);
  SCM_SET_CELL_TYPE (z, scm_tc16_strport | modes);
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETSTREAM (z, SCM_UNPACK (str));
  pt->write_buf = pt->read_buf = SCM_STRING_UCHARS (str);
  pt->read_pos = pt->write_pos = pt->read_buf + SCM_INUM (pos);
  pt->write_buf_size = pt->read_buf_size = str_len;
  pt->write_end = pt->read_end = pt->read_buf + pt->read_buf_size;

  pt->rw_random = 1;

  SCM_ALLOW_INTS;

  /* ensure write_pos is writable. */
  if ((modes & SCM_WRTNG) && pt->write_pos == pt->write_end)
    st_flush (z);
  return z;
}

/* create a new string from a string port's buffer.  */
SCM scm_strport_to_string (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    st_flush (port);
  return scm_makfromstr ((char *) pt->read_buf, pt->read_buf_size, 0);
}

SCM_DEFINE (scm_call_with_output_string, "call-with-output-string", 1, 0, 0, 
           (SCM proc),
	    "Calls the one-argument procedure @var{proc} with a newly created output\n"
	    "port.  When the function returns, the string composed of the characters\n"
	    "written into the port is returned.")
#define FUNC_NAME s_scm_call_with_output_string
{
  SCM p;

  p = scm_mkstrport (SCM_INUM0, 
		     scm_make_string (SCM_INUM0, SCM_UNDEFINED),
		     SCM_OPN | SCM_WRTNG,
                     FUNC_NAME);
  scm_apply (proc, p, scm_listofnull);

  return scm_strport_to_string (p);
}
#undef FUNC_NAME



/* Return a Scheme string obtained by printing a given object.
 */


SCM
scm_strprint_obj (SCM obj)
{
  SCM str;
  SCM port;

  str = scm_makstr (0, 0);
  port = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_WRTNG, "scm_strprint_obj");
  scm_prin1 (obj, port, 1);
  {
    return scm_strport_to_string (port);
  }
}




SCM_DEFINE (scm_call_with_input_string, "call-with-input-string", 2, 0, 0,
           (SCM str, SCM proc),
	    "Calls the one-argument procedure @var{proc} with a newly created input\n"
	    "port from which @var{string}'s contents may be read.  The value yielded\n"
	    "by the @var{proc} is returned.")
#define FUNC_NAME s_scm_call_with_input_string
{
  SCM p = scm_mkstrport(SCM_INUM0, str, SCM_OPN | SCM_RDNG, FUNC_NAME);
  return scm_apply (proc, p, scm_listofnull);
}
#undef FUNC_NAME



/* Given a null-terminated string EXPR containing a Scheme expression
   read it, and return it as an SCM value. */
SCM
scm_read_0str (char *expr)
{
  SCM port = scm_mkstrport (SCM_INUM0,
			    scm_makfrom0str (expr),
			    SCM_OPN | SCM_RDNG,
			    "scm_eval_0str");
  SCM form;

  /* Read expressions from that port; ignore the values.  */
  form = scm_read (port);

  scm_close_port (port);
  return form;
}

/* Given a null-terminated string EXPR containing Scheme program text,
   evaluate it, and return the result of the last expression evaluated.  */
SCM
scm_eval_0str (const char *expr)
{
  return scm_eval_string (scm_makfrom0str (expr));
}


SCM_DEFINE (scm_eval_string, "eval-string", 1, 0, 0, 
            (SCM string),
	    "Evaluate @var{string} as the text representation of a Scheme form\n"
	    "or forms, and return whatever value they produce.\n"
	    "Evaluation takes place in (interaction-environment).")
#define FUNC_NAME s_scm_eval_string
{
  SCM port = scm_mkstrport (SCM_INUM0, string, SCM_OPN | SCM_RDNG,
			    "scm_eval_0str");
  SCM form;
  SCM ans = SCM_UNSPECIFIED;
  SCM module = scm_interaction_environment ();

  /* Read expressions from that port; ignore the values.  */
  while (!SCM_EOF_OBJECT_P (form = scm_read (port)))
    ans = scm_eval_x (form, module);

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early.  */

  return ans;
}
#undef FUNC_NAME

static scm_bits_t
scm_make_stptob ()
{
  scm_bits_t tc = scm_make_port_type ("string", stfill_buffer, st_write);

  scm_set_port_mark        (tc, scm_markstream);
  scm_set_port_end_input   (tc, st_end_input);
  scm_set_port_flush       (tc, st_flush);
  scm_set_port_seek        (tc, st_seek);
  scm_set_port_truncate    (tc, st_truncate);

  return tc;
}

void
scm_init_strports ()
{
  scm_tc16_strport = scm_make_stptob ();

#ifndef SCM_MAGIC_SNARFER
#include "libguile/strports.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
