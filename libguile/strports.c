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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"



/* {Ports - string ports}
 *
 */

#ifdef __STDC__
static int 
prinstpt (SCM exp, SCM port, int writing)
#else
static int 
prinstpt (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  scm_prinport (exp, port, "string");
  return !0;
}

#ifdef __STDC__
static int 
stputc (int c, SCM p)
#else
static int 
stputc (c, p)
     int c;
     SCM p;
#endif
{
  scm_sizet ind = SCM_INUM (SCM_CAR (p));
  SCM_DEFER_INTS;
  if (ind >= SCM_LENGTH (SCM_CDR (p)))
    scm_vector_set_length_x (SCM_CDR (p), SCM_MAKINUM (ind + (ind >> 1)));
  SCM_ALLOW_INTS;
  SCM_CHARS (SCM_CDR (p))[ind] = c;
  SCM_CAR (p) = SCM_MAKINUM (ind + 1);
  return c;
}

#ifdef __STDC__
static scm_sizet 
stwrite (char *str, scm_sizet siz, scm_sizet num, SCM p)
#else
static scm_sizet 
stwrite (str, siz, num, p)
     char *str;
     scm_sizet siz;
     scm_sizet num;
     SCM p;
#endif
{
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
  SCM_CAR (p) = SCM_MAKINUM (ind + siz * num);
  return num;
}

#ifdef __STDC__
static int 
stputs (char *s, SCM p)
#else
static int 
stputs (s, p)
     char *s;
     SCM p;
#endif
{
  stwrite (s, 1, strlen (s), p);
  return 0;
}

#ifdef __STDC__
static int 
stgetc (SCM p)
#else
static int 
stgetc (p)
     SCM p;
#endif
{
  scm_sizet ind = SCM_INUM (SCM_CAR (p));
  if (ind >= SCM_ROLENGTH (SCM_CDR (p)))
    return EOF;
  SCM_CAR (p) = SCM_MAKINUM (ind + 1);
  return SCM_ROUCHARS (SCM_CDR (p))[ind];
}

#ifdef __STDC__
SCM 
scm_mkstrport (SCM pos, SCM str, long modes, char * caller)
#else
SCM 
scm_mkstrport (pos, str, modes, caller)
     SCM pos;
     SCM str;
     long modes;
     char * caller;
#endif
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
  SCM_CAR (z) = scm_tc16_strport | modes;
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETSTREAM (z, stream);
  SCM_ALLOW_INTS;
  return z;
}

SCM_PROC(s_call_with_output_string, "call-with-output-string", 1, 0, 0, scm_call_with_output_string);
#ifdef __STDC__
SCM 
scm_call_with_output_string (SCM proc)
#else
SCM 
scm_call_with_output_string (proc)
     SCM proc;
#endif
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

#ifdef __STDC__
SCM
scm_strprint_obj (SCM obj)
#else
SCM
scm_strprint_obj (obj)
     SCM obj;
#endif
{
  SCM str;
  SCM port;

  str = scm_makstr (64, 0);
  port = scm_mkstrport (SCM_MAKINUM (0), str, SCM_OPN | SCM_WRTNG, "scm_strprint_obj");
  scm_iprin1 (obj, port, 1);
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
#ifdef __STDC__
SCM 
scm_call_with_input_string (SCM str, SCM proc)
#else
SCM 
scm_call_with_input_string (str, proc)
     SCM str;
     SCM proc;
#endif
{
  SCM p = scm_mkstrport(SCM_INUM0, str, SCM_OPN | SCM_RDNG, s_call_with_input_string);
  return scm_apply (proc, p, scm_listofnull);
}

#ifdef __STDC__
static int 
noop0 (FILE *stream)
#else
static int 
noop0 (stream)
     FILE *stream;
#endif
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
  0
};


#ifdef __STDC__
void
scm_init_strports (void)
#else
void
scm_init_strports ()
#endif
{
#include "strports.x"
}

