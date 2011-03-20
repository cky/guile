/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002, 2003, 2005, 2006, 2009, 2010, 2011 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */




#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libguile/bytevectors.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/read.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/modules.h"
#include "libguile/validate.h"
#include "libguile/deprecation.h"
#include "libguile/srfi-4.h"

#include "libguile/strports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - string ports}
 *
 */

/* NOTES:

   write_buf/write_end point to the ends of the allocated bytevector.
   read_buf/read_end in principle point to the part of the bytevector which
   has been written to, but this is only updated after a flush.
   read_pos and write_pos in principle should be equal, but this is only true
   when rw_active is SCM_PORT_NEITHER.

   ENHANCE-ME - output blocks:

   The current code keeps an output string as a single block.  That means
   when the size is increased the entire old contents must be copied.  It'd
   be more efficient to begin a new block when the old one is full, so
   there's no re-copying of previous data.

   To make seeking efficient, keeping the pieces in a vector might be best,
   though appending is probably the most common operation.  The size of each
   block could be progressively increased, so the bigger the string the
   bigger the blocks.

   When `get-output-string' is called the blocks have to be coalesced into a
   string, the result could be kept as a single big block.  If blocks were
   strings then `get-output-string' could notice when there's just one and
   return that with a copy-on-write (though repeated calls to
   `get-output-string' are probably unlikely).

   Another possibility would be to extend the port mechanism to let SCM
   strings come through directly from `display' and friends.  That way if a
   big string is written it can be kept as a copy-on-write, saving time
   copying and maybe saving some space.  */


scm_t_bits scm_tc16_strport;


static int
stfill_buffer (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  
  if (pt->read_pos >= pt->read_end)
    return EOF;
  else
    return scm_return_first_int (*pt->read_pos, port);
}

/* Change the size of a port's bytevector to NEW_SIZE.  This doesn't
   change `read_buf_size'.  */
static void
st_resize_port (scm_t_port *pt, scm_t_off new_size)
{
  SCM old_stream = SCM_PACK (pt->stream);
  const signed char *src = SCM_BYTEVECTOR_CONTENTS (old_stream);
  SCM new_stream = scm_c_make_bytevector (new_size);
  signed char *dst = SCM_BYTEVECTOR_CONTENTS (new_stream);
  unsigned long int old_size = SCM_BYTEVECTOR_LENGTH (old_stream);
  unsigned long int min_size = min (old_size, new_size);

  scm_t_off index = pt->write_pos - pt->write_buf;

  pt->write_buf_size = new_size;

  memcpy (dst, src, min_size);

  scm_remember_upto_here_1 (old_stream);

  /* reset buffer. */
  {
    pt->stream = SCM_UNPACK (new_stream);
    pt->read_buf = pt->write_buf = (unsigned char *)dst;
    pt->read_pos = pt->write_pos = pt->write_buf + index;
    pt->write_end = pt->write_buf + pt->write_buf_size;
    pt->read_end = pt->read_buf + pt->read_buf_size;
  }
}

/* Ensure that `write_pos' < `write_end' by enlarging the buffer when
   necessary.  Update `read_buf' to account for written chars.  The
   buffer is enlarged geometrically.  */
static void
st_flush (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->write_pos == pt->write_end)
    st_resize_port (pt, pt->write_buf_size * 2);

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
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  const char *input = (char *) data;

  while (size > 0)
    {
      int space = pt->write_end - pt->write_pos;
      int write_len = (size > space) ? space : size;
      
      memcpy ((char *) pt->write_pos, input, write_len);
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
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  
  if (pt->read_pos - pt->read_buf < offset)
    scm_misc_error ("st_end_input", "negative position", SCM_EOL);

  pt->write_pos = (unsigned char *) (pt->read_pos = pt->read_pos - offset);
  pt->rw_active = SCM_PORT_NEITHER;
}

static scm_t_off
st_seek (SCM port, scm_t_off offset, int whence)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_off target;

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
	  else if (target == pt->write_buf_size)
	    st_resize_port (pt, target * 2);
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
st_truncate (SCM port, scm_t_off length)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (length > pt->write_buf_size)
    st_resize_port (pt, length);

  pt->read_buf_size = length;
  pt->read_end = pt->read_buf + length;
  if (pt->read_pos > pt->read_end)
    pt->read_pos = pt->read_end;
  
  if (pt->write_pos > pt->read_end)
    pt->write_pos = pt->read_end;
}

/* The initial size in bytes of a string port's buffer.  */
#define INITIAL_BUFFER_SIZE 128

/* Return a new string port with MODES.  If STR is #f, a new backing
   buffer is allocated; otherwise STR must be a string and a copy of it
   serves as the buffer for the new port.  */
SCM
scm_mkstrport (SCM pos, SCM str, long modes, const char *caller)
{
  SCM z, buf;
  scm_t_port *pt;
  size_t str_len, c_pos;
  char *c_buf;

  if (!((modes & SCM_WRTNG) || (modes & SCM_RDNG)))
    scm_misc_error ("scm_mkstrport", "port must read or write", SCM_EOL);

  scm_dynwind_begin (0);
  scm_i_dynwind_pthread_mutex_lock (&scm_i_port_table_mutex);

  z = scm_new_port_table_entry (scm_tc16_strport);
  pt = SCM_PTAB_ENTRY(z);

  if (scm_is_false (str))
    {
      /* Allocate a new buffer to write to.  */
      str_len = INITIAL_BUFFER_SIZE;
      buf = scm_c_make_bytevector (str_len);
      c_buf = (char *) SCM_BYTEVECTOR_CONTENTS (buf);

      /* Reset `read_buf_size'.  It will contain the actual number of
	 bytes written to PT.  */
      pt->read_buf_size = 0;
      c_pos = 0;
    }
  else
    {
      /* STR is a string.  */
      char *copy;

      SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, caller);

      /* Create a copy of STR in the encoding of PT.  */
      copy = scm_to_stringn (str, &str_len, pt->encoding,
			     SCM_FAILED_CONVERSION_ERROR);
      buf = scm_c_make_bytevector (str_len);
      c_buf = (char *) SCM_BYTEVECTOR_CONTENTS (buf);
      memcpy (c_buf, copy, str_len);
      free (copy);

      c_pos = scm_to_unsigned_integer (pos, 0, str_len);
      pt->read_buf_size = str_len;
    }

  SCM_SETSTREAM (z, SCM_UNPACK (buf));
  SCM_SET_CELL_TYPE (z, scm_tc16_strport | modes);

  pt->write_buf = pt->read_buf = (unsigned char *) c_buf;
  pt->read_pos = pt->write_pos = pt->read_buf + c_pos;
  pt->write_buf_size = str_len;
  pt->write_end = pt->read_end = pt->read_buf + pt->read_buf_size;

  pt->rw_random = 1;

  scm_dynwind_end ();

  /* Ensure WRITE_POS is writable.  */
  if ((modes & SCM_WRTNG) && pt->write_pos == pt->write_end)
    st_flush (z);

  scm_i_set_conversion_strategy_x (z, SCM_FAILED_CONVERSION_ERROR);
  return z;
}

/* Create a new string from the buffer of PORT, a string port, converting from
   PORT's encoding to the standard string representation.  */
SCM
scm_strport_to_string (SCM port)
{
  SCM str;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    st_flush (port);

  if (pt->read_buf_size == 0)
    return scm_nullstr;

  if (pt->encoding == NULL)
    {
      char *buf;
      str = scm_i_make_string (pt->read_buf_size, &buf, 0);
      memcpy (buf, pt->read_buf, pt->read_buf_size);
    }
  else
    str = scm_from_stringn ((char *)pt->read_buf, pt->read_buf_size,
                            pt->encoding, pt->ilseq_handler);
  scm_remember_upto_here_1 (port);
  return str;
}

SCM_DEFINE (scm_object_to_string, "object->string", 1, 1, 0,
	    (SCM obj, SCM printer),
	    "Return a Scheme string obtained by printing @var{obj}.\n"
	    "Printing function can be specified by the optional second\n"
	    "argument @var{printer} (default: @code{write}).")
#define FUNC_NAME s_scm_object_to_string
{
  SCM port, result;

  if (!SCM_UNBNDP (printer))
    SCM_VALIDATE_PROC (2, printer);

  port = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
			SCM_OPN | SCM_WRTNG, FUNC_NAME);

  if (SCM_UNBNDP (printer))
    scm_write (obj, port);
  else
    scm_call_2 (printer, obj, port);

  result = scm_strport_to_string (port);

  /* Explicitly close PORT so that the iconv CDs associated with it are
     deallocated right away.  This is important because CDs use a lot of
     memory that's not visible to the GC, so not freeing them can lead
     to almost large heap usage.  See
     <http://wingolog.org/archives/2011/02/25/ports-weaks-gc-and-dark-matter>
     for details.  */
  scm_close_port (port);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_call_with_output_string, "call-with-output-string", 1, 0, 0, 
           (SCM proc),
	    "Calls the one-argument procedure @var{proc} with a newly created output\n"
	    "port.  When the function returns, the string composed of the characters\n"
	    "written into the port is returned.")
#define FUNC_NAME s_scm_call_with_output_string
{
  SCM p;

  p = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
		     SCM_OPN | SCM_WRTNG,
                     FUNC_NAME);
  scm_call_1 (proc, p);

  return scm_get_output_string (p);
}
#undef FUNC_NAME

SCM_DEFINE (scm_call_with_input_string, "call-with-input-string", 2, 0, 0,
           (SCM string, SCM proc),
	    "Calls the one-argument procedure @var{proc} with a newly\n"
	    "created input port from which @var{string}'s contents may be\n"
	    "read.  The value yielded by the @var{proc} is returned.")
#define FUNC_NAME s_scm_call_with_input_string
{
  SCM p = scm_mkstrport(SCM_INUM0, string, SCM_OPN | SCM_RDNG, FUNC_NAME);
  return scm_call_1 (proc, p);
}
#undef FUNC_NAME

SCM_DEFINE (scm_open_input_string, "open-input-string", 1, 0, 0,
	    (SCM str),
	    "Take a string and return an input port that delivers characters\n"
	    "from the string. The port can be closed by\n"
	    "@code{close-input-port}, though its storage will be reclaimed\n"
	    "by the garbage collector if it becomes inaccessible.")
#define FUNC_NAME s_scm_open_input_string
{
  SCM p = scm_mkstrport(SCM_INUM0, str, SCM_OPN | SCM_RDNG, FUNC_NAME);
  return p;
}
#undef FUNC_NAME

SCM_DEFINE (scm_open_output_string, "open-output-string", 0, 0, 0, 
	    (void),
	    "Return an output port that will accumulate characters for\n"
	    "retrieval by @code{get-output-string}. The port can be closed\n"
	    "by the procedure @code{close-output-port}, though its storage\n"
	    "will be reclaimed by the garbage collector if it becomes\n"
	    "inaccessible.")
#define FUNC_NAME s_scm_open_output_string
{
  SCM p;

  p = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
		     SCM_OPN | SCM_WRTNG,
                     FUNC_NAME);
  return p;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_output_string, "get-output-string", 1, 0, 0, 
	    (SCM port),
	    "Given an output port created by @code{open-output-string},\n"
	    "return a string consisting of the characters that have been\n"
	    "output to the port so far.")
#define FUNC_NAME s_scm_get_output_string
{
  SCM_VALIDATE_OPOUTSTRPORT (1, port);
  return scm_strport_to_string (port);
}
#undef FUNC_NAME


/* Given a null-terminated string EXPR containing a Scheme expression
   read it, and return it as an SCM value. */
SCM
scm_c_read_string (const char *expr)
{
  SCM port = scm_mkstrport (SCM_INUM0,
			    scm_from_locale_string (expr),
			    SCM_OPN | SCM_RDNG,
			    "scm_c_read_string");
  SCM form;

  form = scm_read (port);

  scm_close_port (port);
  return form;
}

/* Given a null-terminated string EXPR containing Scheme program text,
   evaluate it, and return the result of the last expression evaluated.  */
SCM
scm_c_eval_string (const char *expr)
{
  return scm_eval_string (scm_from_locale_string (expr));
}

SCM
scm_c_eval_string_in_module (const char *expr, SCM module)
{
  return scm_eval_string_in_module (scm_from_locale_string (expr), module);
}


SCM_DEFINE (scm_eval_string_in_module, "eval-string", 1, 1, 0, 
            (SCM string, SCM module),
	    "Evaluate @var{string} as the text representation of a Scheme\n"
	    "form or forms, and return whatever value they produce.\n"
	    "Evaluation takes place in the given module, or the current\n"
            "module when no module is given.\n"
            "While the code is evaluated, the given module is made the\n"
	    "current one.  The current module is restored when this\n"
            "procedure returns.")
#define FUNC_NAME s_scm_eval_string_in_module
{
  static SCM eval_string = SCM_BOOL_F, k_module = SCM_BOOL_F;

  if (scm_is_false (eval_string))
    {
      eval_string = scm_c_public_lookup ("ice-9 eval-string", "eval-string");
      k_module = scm_from_locale_keyword ("module");
    }
  
  if (SCM_UNBNDP (module))
    module = scm_current_module ();
  else
    SCM_VALIDATE_MODULE (2, module);

  return scm_call_3 (scm_variable_ref (eval_string), string, k_module, module);
}
#undef FUNC_NAME

SCM
scm_eval_string (SCM string)
{
  return scm_eval_string_in_module (string, SCM_UNDEFINED);
}

static scm_t_bits
scm_make_stptob ()
{
  scm_t_bits tc = scm_make_port_type ("string", stfill_buffer, st_write);

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

#include "libguile/strports.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
