/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
stputc (int c, SCM port)
{
  SCM p = SCM_STREAM (port);
  scm_sizet ind = SCM_INUM (SCM_CAR (p));
  SCM_DEFER_INTS;
  if (ind >= SCM_LENGTH (SCM_CDR (p)))
    scm_vector_set_length_x (SCM_CDR (p), SCM_MAKINUM (ind + (ind >> 1)));
  SCM_ALLOW_INTS;
  SCM_CHARS (SCM_CDR (p))[ind] = c;
  SCM_SETCAR (p, SCM_MAKINUM (ind + 1));
  return c;
}


static scm_sizet 
stwrite (char *str,
	 scm_sizet siz,
	 scm_sizet num,
	 SCM port)
{
  SCM p = SCM_STREAM (port);

  scm_sizet ind = SCM_INUM (SCM_CAR (p));
  scm_sizet len = siz * num;
  char *dst;
  SCM_DEFER_INTS;
  if (ind + len >= SCM_LENGTH (SCM_CDR (p)))
    scm_vector_set_length_x (SCM_CDR (p), SCM_MAKINUM (ind + len + ((ind + len) >> 1)));
  SCM_ALLOW_INTS;
  dst = &(SCM_CHARS (SCM_CDR (p))[ind]);
  while (len--)
    dst[len] = str[len];
  SCM_SETCAR (p, SCM_MAKINUM (ind + siz * num));
  return num;
}


static int 
stputs (char *s, SCM port)
{
  stwrite (s, 1, strlen (s), port);
  return 0;
}


static int 
stgetc (SCM port)
{
  SCM p = SCM_STREAM (port);

  scm_sizet ind = SCM_INUM (SCM_CAR (p));
  if (ind >= SCM_ROLENGTH (SCM_CDR (p)))
    return EOF;
  SCM_SETCAR (p, SCM_MAKINUM (ind + 1));
  return SCM_ROUCHARS (SCM_CDR (p))[ind];
}


SCM 
scm_mkstrport (pos, str, modes, caller)
     SCM pos;
     SCM str;
     long modes;
     char * caller;
{
  SCM z;
  SCM stream;
  struct scm_port_table * pt;

  SCM_ASSERT(SCM_INUMP(pos) && SCM_INUM(pos) >= 0, pos, SCM_ARG1, caller);
  SCM_ASSERT(SCM_NIMP(str) && SCM_ROSTRINGP(str), str, SCM_ARG1, caller);
  stream = scm_cons(pos, str);
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (z);
  SCM_SETCAR (z, scm_tc16_strport | modes);
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETSTREAM (z, stream);
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
    SCM_DEFER_INTS;
    answer = scm_makfromstr (SCM_CHARS (SCM_CDR (SCM_STREAM (p))),
			     SCM_INUM (SCM_CAR (SCM_STREAM (p))),
			     0);
    SCM_ALLOW_INTS;
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
    SCM answer;
    SCM_DEFER_INTS;
    answer = scm_makfromstr (SCM_CHARS (SCM_CDR (SCM_STREAM (port))),
			     SCM_INUM (SCM_CAR (SCM_STREAM (port))),
			     0);
    SCM_ALLOW_INTS;
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
  stputc,
  stputs,
  stwrite,
  noop0,
  stgetc,
  scm_generic_fgets,
  0
};



void
scm_init_strports ()
{
#include "strports.x"
}

