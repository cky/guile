/*	Copyright (C) 1995,1996,1998 Free Software Foundation, Inc.
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
#include "unif.h"
#include "eval.h"
#include "read.h"

#include "strports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - string ports}
 *
 */


static int 
prinstpt (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_prinport (exp, port, "string");
  return !0;
}

static int
stfill_buffer (SCM port)
{
  SCM str = SCM_STREAM (port);
  struct scm_port_table *pt = SCM_PTAB_ENTRY (port);
  
  pt->read_buf = SCM_ROCHARS (str);
  pt->read_buf_size = SCM_ROLENGTH (str);
  pt->read_end = pt->read_buf + pt->read_buf_size;

  if (pt->read_pos >= pt->read_end)
    return EOF;
  else
    return scm_return_first (*(pt->read_pos++), port);
}

/* not a conventional "flush": it extends the string for more data.  */
static void
st_flush (SCM port)
{
  struct scm_port_table *pt = SCM_PTAB_ENTRY (port);

  if (pt->write_pos == pt->write_end)
    {
      pt->write_buf_size += pt->write_buf_size >> 1;
      scm_vector_set_length_x (pt->stream,
			       SCM_MAKINUM (pt->write_buf_size));
      /* reset buffer in case reallocation moved the string. */
      {
	int read = pt->read_pos - pt->read_buf;
	int written = pt->write_pos - pt->write_buf;

	pt->read_buf = pt->write_buf = SCM_CHARS (pt->stream);
	pt->read_pos = pt->read_buf + read;
	pt->write_pos = pt->write_buf + written;
	pt->write_end = pt->write_buf + pt->write_buf_size;
	pt->read_end = pt->read_buf + pt->read_buf_size;
      }
    }
}

SCM 
scm_mkstrport (pos, str, modes, caller)
     SCM pos;
     SCM str;
     long modes;
     const char * caller;
{
  SCM z;
  SCM stream;
  struct scm_port_table * pt;

  SCM_ASSERT(SCM_INUMP(pos) && SCM_INUM(pos) >= 0, pos, SCM_ARG1, caller);
  SCM_ASSERT(SCM_NIMP(str) && SCM_ROSTRINGP(str), str, SCM_ARG1, caller);
  stream = str;
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (z);
  SCM_SETCAR (z, scm_tc16_strport | modes);
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETSTREAM (z, stream);
  pt->write_buf = pt->read_buf = SCM_ROCHARS (str);
  pt->write_pos = pt->read_pos = pt->read_buf + SCM_INUM (pos);
  pt->read_buf_size = SCM_ROLENGTH (str);
  pt->read_end = pt->read_buf + pt->read_buf_size;
  /* after the last (normally NUL) character is written to,
     the port will be "flushed".  */
  pt->write_buf_size = pt->read_buf_size + 1;
  pt->write_end = pt->write_buf + pt->write_buf_size;
  SCM_ALLOW_INTS;
  return z;
}

SCM_PROC(s_call_with_output_string, "call-with-output-string", 1, 0, 0, scm_call_with_output_string);

SCM 
scm_call_with_output_string (proc)
     SCM proc;
{
  SCM p;
  p = scm_mkstrport(SCM_INUM0, scm_make_string(SCM_MAKINUM(30), SCM_UNDEFINED),
			SCM_OPN | SCM_WRTNG,
			s_call_with_output_string);
  scm_apply (proc, p, scm_listofnull);
  {
    SCM answer;
    struct scm_port_table *pt = SCM_PTAB_ENTRY (p);

    answer = scm_makfromstr (SCM_CHARS (SCM_STREAM (p)),
			     pt->write_pos - pt->write_buf,
			     0);
    return answer;
  }
}



/* Return a Scheme string obtained by printing a given object.
 */


SCM
scm_strprint_obj (obj)
     SCM obj;
{
  SCM str;
  SCM port;

  str = scm_makstr (64, 0);
  port = scm_mkstrport (SCM_MAKINUM (0), str, SCM_OPN | SCM_WRTNG, "scm_strprint_obj");
  scm_prin1 (obj, port, 1);
  {
    struct scm_port_table *pt = SCM_PTAB_ENTRY (obj);
    SCM answer;

    answer = scm_makfromstr (SCM_CHARS (SCM_STREAM (port)),
			     pt->write_pos - pt->write_buf,
			     0);
    return answer;
  }
}




SCM_PROC(s_call_with_input_string, "call-with-input-string", 2, 0, 0, scm_call_with_input_string);

SCM 
scm_call_with_input_string (str, proc)
     SCM str;
     SCM proc;
{
  SCM p = scm_mkstrport(SCM_INUM0, str, SCM_OPN | SCM_RDNG, s_call_with_input_string);
  return scm_apply (proc, p, scm_listofnull);
}



/* Given a null-terminated string EXPR containing a Scheme expression
   read it, and return it as an SCM value. */
SCM
scm_read_0str (expr)
     char *expr;
{
  SCM port = scm_mkstrport (SCM_MAKINUM (0),
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
scm_eval_0str (expr)
     char *expr;
{
  return scm_eval_string (scm_makfrom0str (expr));
}


SCM_PROC (s_eval_string, "eval-string", 1, 0, 0, scm_eval_string);

SCM
scm_eval_string (string)
     SCM string;
{
  SCM port = scm_mkstrport (SCM_MAKINUM (0), string, SCM_OPN | SCM_RDNG,
			    "scm_eval_0str");
  SCM form;
  SCM ans = SCM_UNSPECIFIED;

  /* Read expressions from that port; ignore the values.  */
  while (!SCM_EOF_OBJECT_P (form = scm_read (port)))
    ans = scm_eval_x (form);

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early.  */

  return ans;
}



static int noop0 SCM_P ((SCM stream));

static int 
noop0 (stream)
     SCM stream;
{
  return 0;
}


scm_ptobfuns scm_stptob =
{
  scm_markstream,
  noop0,
  prinstpt,
  0,
  st_flush,
  0,
  stfill_buffer,
  0,
  0,
};



void
scm_init_strports ()
{
#include "strports.x"
}

