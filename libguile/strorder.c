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



SCM_PROC1 (s_string_equal_p, "string=?", scm_tc7_rpsubr, scm_string_equal_p);
#ifdef __STDC__
SCM
scm_string_equal_p (SCM s1, SCM s2)
#else
SCM
scm_string_equal_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  register scm_sizet i;
  register char *c1, *c2;
  SCM_ASSERT (SCM_NIMP (s1) && SCM_ROSTRINGP (s1), s1, SCM_ARG1, s_string_equal_p);
  SCM_ASSERT (SCM_NIMP (s2) && SCM_ROSTRINGP (s2), s2, SCM_ARG2, s_string_equal_p);

  i = SCM_ROLENGTH (s2);
  if (SCM_ROLENGTH (s1) != i)
    {
      return SCM_BOOL_F;
    }
  c1 = SCM_ROCHARS (s1);
  c2 = SCM_ROCHARS (s2);
  while (0 != i--)
    if (*c1++ != *c2++)
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM_PROC1 (s_string_ci_equal_p, "string-ci=?", scm_tc7_rpsubr, scm_string_ci_equal_p);
#ifdef __STDC__
SCM
scm_string_ci_equal_p (SCM s1, SCM s2)
#else
SCM
scm_string_ci_equal_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  register scm_sizet i;
  register unsigned char *c1, *c2;
  SCM_ASSERT (SCM_NIMP (s1) && SCM_ROSTRINGP (s1), s1, SCM_ARG1, s_string_ci_equal_p);
  SCM_ASSERT (SCM_NIMP (s2) && SCM_ROSTRINGP (s2), s2, SCM_ARG2, s_string_ci_equal_p);
  i = SCM_ROLENGTH (s2);
  if (SCM_ROLENGTH (s1) != i)
    {
      return SCM_BOOL_F;
    }
  c1 = SCM_ROUCHARS (s1);
  c2 = SCM_ROUCHARS (s2);
  while (0 != i--)
    if (scm_upcase(*c1++) != scm_upcase(*c2++))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM_PROC1 (s_string_less_p, "string<?", scm_tc7_rpsubr, scm_string_less_p);
#ifdef __STDC__
SCM
scm_string_less_p (SCM s1, SCM s2)
#else
SCM
scm_string_less_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  register scm_sizet i, len, s2len;
  register unsigned char *c1, *c2;
  register int c;

  SCM_ASSERT (SCM_NIMP (s1) && SCM_ROSTRINGP (s1), s1, SCM_ARG1, s_string_less_p);
  SCM_ASSERT (SCM_NIMP (s2) && SCM_ROSTRINGP (s2), s2, SCM_ARG2, s_string_less_p);
  len = SCM_ROLENGTH (s1);
  s2len = i = SCM_ROLENGTH (s2);
  if (len>i) i = len;
  c1 = SCM_ROUCHARS (s1);
  c2 = SCM_ROUCHARS (s2);

  for (i = 0;i<len;i++) {
    c = (*c1++ - *c2++);
    if (c>0)
      return SCM_BOOL_F;
    if (c<0)
      return SCM_BOOL_T;
  }
  {
    SCM answer;
    answer = (s2len != len) ? SCM_BOOL_T : SCM_BOOL_F;
    return answer;
  }
}

SCM_PROC1 (s_string_leq_p, "string<=?", scm_tc7_rpsubr, scm_string_leq_p);
#ifdef __STDC__
SCM
scm_string_leq_p (SCM s1, SCM s2)
#else
SCM
scm_string_leq_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  return SCM_BOOL_NOT (scm_string_less_p (s2, s1));
}

SCM_PROC1 (s_string_gr_p, "string>?", scm_tc7_rpsubr, scm_string_gr_p);
#ifdef __STDC__
SCM
scm_string_gr_p (SCM s1, SCM s2)
#else
SCM
scm_string_gr_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  return scm_string_less_p (s2, s1);
}

SCM_PROC1 (s_string_geq_p, "string>=?", scm_tc7_rpsubr, scm_string_geq_p);
#ifdef __STDC__
SCM
scm_string_geq_p (SCM s1, SCM s2)
#else
SCM
scm_string_geq_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  return SCM_BOOL_NOT (scm_string_less_p (s1, s2));
}

SCM_PROC1 (s_string_ci_less_p, "string-ci<?", scm_tc7_rpsubr, scm_string_ci_less_p);
#ifdef __STDC__
SCM
scm_string_ci_less_p (SCM s1, SCM s2)
#else
SCM
scm_string_ci_less_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  register scm_sizet i, len, s2len;
  register unsigned char *c1, *c2;
  register int c;
  SCM_ASSERT (SCM_NIMP (s1) && SCM_ROSTRINGP (s1), s1, SCM_ARG1, s_string_ci_less_p);
  SCM_ASSERT (SCM_NIMP (s2) && SCM_ROSTRINGP (s2), s2, SCM_ARG2, s_string_ci_less_p);
  len = SCM_ROLENGTH (s1);
  s2len = i = SCM_ROLENGTH (s2);
  if (len>i) i=len;
  c1 = SCM_ROUCHARS (s1);
  c2 = SCM_ROUCHARS (s2);
  for (i = 0;i<len;i++) {
    c = (scm_upcase(*c1++) - scm_upcase(*c2++));
    if (c>0) return SCM_BOOL_F;
    if (c<0) return SCM_BOOL_T;
  }
  return (s2len != len) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC1 (s_string_ci_leq_p, "string-ci<=?", scm_tc7_rpsubr, scm_string_ci_leq_p);
#ifdef __STDC__
SCM
scm_string_ci_leq_p (SCM s1, SCM s2)
#else
SCM
scm_string_ci_leq_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  return SCM_BOOL_NOT (scm_string_ci_less_p (s2, s1));
}

SCM_PROC1 (s_string_ci_gr_p, "string-ci>?", scm_tc7_rpsubr, scm_string_ci_gr_p);
#ifdef __STDC__
SCM
scm_string_ci_gr_p (SCM s1, SCM s2)
#else
SCM
scm_string_ci_gr_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  return scm_string_ci_less_p (s2, s1);
}

SCM_PROC1 (s_string_ci_geq_p, "string-ci>=?", scm_tc7_rpsubr, scm_string_ci_geq_p);
#ifdef __STDC__
SCM
scm_string_ci_geq_p (SCM s1, SCM s2)
#else
SCM
scm_string_ci_geq_p (s1, s2)
     SCM s1;
     SCM s2;
#endif
{
  return SCM_BOOL_NOT (scm_string_ci_less_p (s1, s2));
}


#ifdef __STDC__
void
scm_init_strorder (void)
#else
void
scm_init_strorder ()
#endif
{
#include "strorder.x"
}

