/*	Copyright (C) 1995, 1996, 1999, 2000 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/strings.h"
#include "libguile/symbols.h"

#include "libguile/validate.h"
#include "libguile/strorder.h"


SCM_DEFINE1 (scm_string_equal_p, "string=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Lexicographic equality predicate; \n"
	     "Returns @t{#t} if the two strings are the same length and contain the same\n"
	     "characters in the same positions, otherwise returns @t{#f}. (r5rs)\n\n"
	     "@samp{String-ci=?} treats\n"
	     "upper and lower case letters as though they were the same character, but\n"
	     "@samp{string=?} treats upper and lower case as distinct characters.")
#define FUNC_NAME s_scm_string_equal_p
{
  register scm_sizet i;
  register unsigned char *c1, *c2;
  SCM_VALIDATE_ROSTRING (1,s1);
  SCM_VALIDATE_ROSTRING (2,s2);

  i = SCM_ROLENGTH (s2);
  if (SCM_ROLENGTH (s1) != i)
    {
      return SCM_BOOL_F;
    }
  c1 = SCM_ROUCHARS (s1);
  c2 = SCM_ROUCHARS (s2);
  while (0 != i--)
    if (*c1++ != *c2++)
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_ci_equal_p, "string-ci=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Case-insensitive string equality predicate; returns @t{#t} if\n"
	     "the two strings are the same length and their component characters\n"
	     "match (ignoring case) at each position; otherwise returns @t{#f}. (r5rs)")
#define FUNC_NAME s_scm_string_ci_equal_p
{
  register scm_sizet i;
  register unsigned char *c1, *c2;
  SCM_VALIDATE_ROSTRING (1,s1);
  SCM_VALIDATE_ROSTRING (2,s2);

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
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_less_p, "string<?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Lexicographic ordering predicate; returns @t{#t} if @var{s1}\n"
	     "is lexicographically less than @var{s2}.  (r5rs)")
#define FUNC_NAME s_scm_string_less_p
{
  register scm_sizet i, len, s2len;
  register unsigned char *c1, *c2;
  register int c;

  SCM_VALIDATE_ROSTRING (1,s1);
  SCM_VALIDATE_ROSTRING (2,s2);
  len = SCM_ROLENGTH (s1);
  s2len = SCM_ROLENGTH (s2);
  if (len>s2len) len = s2len;
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
    answer = SCM_BOOL(s2len != len);
    return answer;
  }
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_leq_p, "string<=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Lexicographic ordering predicate; returns @t{#t} if @var{s1}\n"
	     "is lexicographically less than or equal to @var{s2}.  (r5rs)")
#define FUNC_NAME s_scm_string_leq_p
{
  return SCM_BOOL_NOT (scm_string_less_p (s2, s1));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_gr_p, "string>?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Lexicographic ordering predicate; returns @t{#t} if @var{s1}\n"
	     "is lexicographically greater than @var{s2}.  (r5rs)")
#define FUNC_NAME s_scm_string_gr_p
{
  return scm_string_less_p (s2, s1);
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_geq_p, "string>=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Lexicographic ordering predicate; returns @t{#t} if @var{s1}\n"
	     "is lexicographically greater than or equal to @var{s2}.  (r5rs)")
#define FUNC_NAME s_scm_string_geq_p
{
  return SCM_BOOL_NOT (scm_string_less_p (s1, s2));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_ci_less_p, "string-ci<?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Case insensitive lexicographic ordering predicate; \n"
	     "returns @t{#t} if @var{s1} is lexicographically less than\n"
	     "@var{s2} regardless of case.  (r5rs)")
#define FUNC_NAME s_scm_string_ci_less_p
{
  register scm_sizet i, len, s2len;
  register unsigned char *c1, *c2;
  register int c;
  SCM_VALIDATE_ROSTRING (1,s1);
  SCM_VALIDATE_ROSTRING (2,s2);
  len = SCM_ROLENGTH (s1);
  s2len = SCM_ROLENGTH (s2);
  if (len>s2len) len = s2len;
  c1 = SCM_ROUCHARS (s1);
  c2 = SCM_ROUCHARS (s2);
  for (i = 0;i<len;i++) {
    c = (scm_upcase(*c1++) - scm_upcase(*c2++));
    if (c>0) return SCM_BOOL_F;
    if (c<0) return SCM_BOOL_T;
  }
  return SCM_BOOL(s2len != len);
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_ci_leq_p, "string-ci<=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Case insensitive lexicographic ordering predicate; \n"
	     "returns @t{#t} if @var{s1} is lexicographically less than\n"
	     "or equal to @var{s2} regardless of case.  (r5rs)")
#define FUNC_NAME s_scm_string_ci_leq_p
{
  return SCM_BOOL_NOT (scm_string_ci_less_p (s2, s1));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_ci_gr_p, "string-ci>?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Case insensitive lexicographic ordering predicate; \n"
	     "returns @t{#t} if @var{s1} is lexicographically greater than\n"
	     "@var{s2} regardless of case.  (r5rs)")
#define FUNC_NAME s_scm_string_ci_gr_p
{
  return scm_string_ci_less_p (s2, s1);
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_string_ci_geq_p, "string-ci>=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	     "Case insensitive lexicographic ordering predicate; \n"
	     "returns @t{#t} if @var{s1} is lexicographically greater than\n"
	     "or equal to @var{s2} regardless of case.  (r5rs)")
#define FUNC_NAME s_scm_string_ci_geq_p
{
  return SCM_BOOL_NOT (scm_string_ci_less_p (s1, s2));
}
#undef FUNC_NAME



void
scm_init_strorder ()
{
#include "libguile/strorder.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
