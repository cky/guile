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



/* {Ports - soft ports}
 * 
 */


#ifdef __STDC__
static int 
prinsfpt (SCM exp, SCM port, int writing)
#else
static int 
prinsfpt (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  scm_prinport (exp, port, "soft");
  return !0;
}

/* sfputc sfwrite sfputs sfclose 
 * are called within a SCM_SYSCALL.  
 *
 * So we need to set errno to 0 before returning.  sfflush
 * may be called within a SCM_SYSCALL.  So we need to set errno to 0
 * before returning.
 */

#ifdef __STDC__
static int 
sfputc (int c, SCM p)
#else
static int 
sfputc (c, p)
     int c;
     SCM p;
#endif
{
  scm_apply (SCM_VELTS (p)[0], SCM_MAKICHR (c), scm_listofnull);
  errno = 0;
  return c;
}

#ifdef __STDC__
static scm_sizet 
sfwrite (char *str, scm_sizet siz, scm_sizet num, SCM p)
#else
static scm_sizet 
sfwrite (str, siz, num, p)
     char *str;
     scm_sizet siz;
     scm_sizet num;
     SCM p;
#endif
{
  SCM sstr;
  sstr = scm_makfromstr (str, siz * num, 0);
  scm_apply (SCM_VELTS (p)[1], sstr, scm_listofnull);
  errno = 0;
  return num;
}

#ifdef __STDC__
static int 
sfputs (char *s, SCM p)
#else
static int 
sfputs (s, p)
     char *s;
     SCM p;
#endif
{
  sfwrite (s, 1, strlen (s), p);
  return 0;
}

#ifdef __STDC__
static int 
sfflush (SCM stream)
#else
static int 
sfflush (stream)
     SCM stream;
#endif
{
  SCM f = SCM_VELTS (stream)[2];
  if (SCM_BOOL_F == f)
    return 0;
  f = scm_apply (f, SCM_EOL, SCM_EOL);
  errno = 0;
  return SCM_BOOL_F == f ? EOF : 0;
}

#ifdef __STDC__
static int 
sfgetc (SCM p)
#else
static int 
sfgetc (p)
     SCM p;
#endif
{
  SCM ans;
  ans = scm_apply (SCM_VELTS (p)[3], SCM_EOL, SCM_EOL);
  errno = 0;
  if (SCM_FALSEP (ans) || SCM_EOF_VAL == ans)
    return EOF;
  SCM_ASSERT (SCM_ICHRP (ans), ans, SCM_ARG1, "getc");
  return SCM_ICHR (ans);
}

#ifdef __STDC__
static int 
sfclose (SCM p)
#else
static int 
sfclose (p)
     SCM p;
#endif
{
  SCM f = SCM_VELTS (p)[4];
  if (SCM_BOOL_F == f)
    return 0;
  f = scm_apply (f, SCM_EOL, SCM_EOL);
  errno = 0;
  return SCM_BOOL_F == f ? EOF : 0;
}



SCM_PROC(s_make_soft_port, "make-soft-port", 2, 0, 0, scm_make_soft_port);
#ifdef __STDC__
SCM 
scm_make_soft_port (SCM pv, SCM modes)
#else
SCM 
scm_make_soft_port (pv, modes)
     SCM pv;
     SCM modes;
#endif
{
  struct scm_port_table * pt;
  SCM z;
  SCM_ASSERT (SCM_NIMP (pv) && SCM_VECTORP (pv) && 5 == SCM_LENGTH (pv), pv, SCM_ARG1, s_make_soft_port);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_STRINGP (modes), modes, SCM_ARG2, s_make_soft_port);
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (z);
  SCM_CAR (z) = scm_tc16_sfport | scm_mode_bits (SCM_CHARS (modes));
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETSTREAM (z, pv);
  SCM_ALLOW_INTS;
  return z;
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


scm_ptobfuns scm_sfptob =
{
  scm_markstream,
  noop0,
  prinsfpt,
  0,
  sfputc,
  sfputs,
  sfwrite,
  sfflush,
  sfgetc,
  sfclose
};


#ifdef __STDC__
void
scm_init_vports (void)
#else
void
scm_init_vports ()
#endif
{
#include "vports.x"
}

