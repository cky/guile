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



#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/strings.h"
#include "libguile/symbols.h"

#include "libguile/validate.h"
#include "libguile/strorder.h"


SCM_DEFINE1 (scm_string_equal_p, "string=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Lexicographic equality predicate; return @code{#t} if the two\n"
	    "strings are the same length and contain the same characters in\n"
	    "the same positions, otherwise return @code{#f}.\n"
	    "\n"
	    "The procedure @code{string-ci=?} treats upper and lower case\n"
	    "letters as though they were the same character, but\n"
	    "@code{string=?} treats upper and lower case as distinct\n"
	    "characters.")
#define FUNC_NAME s_scm_string_equal_p
{
  size_t length;

  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  length = SCM_STRING_LENGTH (s2);
  if (SCM_STRING_LENGTH (s1) == length)
    {
      unsigned char *c1 = SCM_STRING_UCHARS (s1) + length - 1;
      unsigned char *c2 = SCM_STRING_UCHARS (s2) + length - 1;
      size_t i;

      /* comparing from back to front typically finds mismatches faster */
      for (i = 0; i != length; ++i, --c1, --c2)
	if (*c1 != *c2)
	  return SCM_BOOL_F;

      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_string_ci_equal_p, "string-ci=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Case-insensitive string equality predicate; return @code{#t} if\n"
	    "the two strings are the same length and their component\n"
	    "characters match (ignoring case) at each position; otherwise\n"
	    "return @code{#f}.")
#define FUNC_NAME s_scm_string_ci_equal_p
{
  size_t length;

  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  length = SCM_STRING_LENGTH (s2);
  if (SCM_STRING_LENGTH (s1) == length)
    {
      unsigned char *c1 = SCM_STRING_UCHARS (s1) + length - 1;
      unsigned char *c2 = SCM_STRING_UCHARS (s2) + length - 1;
      size_t i;

      /* comparing from back to front typically finds mismatches faster */
      for (i = 0; i != length; ++i, --c1, --c2)
	if (scm_upcase (*c1) != scm_upcase (*c2))
	  return SCM_BOOL_F;

      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME


/* Helper function for the lexicographic ordering predicates.
 * No argument checking is performed.  */
static SCM
string_less_p (SCM s1, SCM s2)
{
  size_t i, length1, length2, lengthm;
  unsigned char *c1, *c2;

  length1 = SCM_STRING_LENGTH (s1);
  length2 = SCM_STRING_LENGTH (s2);
  lengthm = min (length1, length2);
  c1 = SCM_STRING_UCHARS (s1);
  c2 = SCM_STRING_UCHARS (s2);

  for (i = 0; i != lengthm; ++i, ++c1, ++c2) {
    int c = *c1 - *c2;
    if (c < 0) return SCM_BOOL_T;
    if (c > 0) return SCM_BOOL_F;
  }

  return SCM_BOOL (length1 < length2);
}


SCM_DEFINE1 (scm_string_less_p, "string<?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically less than @var{s2}.")
#define FUNC_NAME s_scm_string_less_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return string_less_p (s1, s2);
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_string_leq_p, "string<=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically less than or equal to @var{s2}.")
#define FUNC_NAME s_scm_string_leq_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return SCM_BOOL_NOT (string_less_p (s2, s1));
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_string_gr_p, "string>?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically greater than @var{s2}.")
#define FUNC_NAME s_scm_string_gr_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return string_less_p (s2, s1);
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_string_geq_p, "string>=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically greater than or equal to @var{s2}.")
#define FUNC_NAME s_scm_string_geq_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return SCM_BOOL_NOT (string_less_p (s1, s2));
}
#undef FUNC_NAME


/* Helper function for the case insensitive lexicographic ordering
 * predicates.  No argument checking is performed.  */
static SCM
string_ci_less_p (SCM s1, SCM s2)
{
  size_t i, length1, length2, lengthm;
  unsigned char *c1, *c2;

  length1 = SCM_STRING_LENGTH (s1);
  length2 = SCM_STRING_LENGTH (s2);
  lengthm = min (length1, length2);
  c1 = SCM_STRING_UCHARS (s1);
  c2 = SCM_STRING_UCHARS (s2);

  for (i = 0; i != lengthm; ++i, ++c1, ++c2) {
    int c = scm_upcase (*c1) - scm_upcase (*c2);
    if (c < 0) return SCM_BOOL_T;
    if (c > 0) return SCM_BOOL_F;
  }

  return SCM_BOOL (length1 < length2);
}


SCM_DEFINE1 (scm_string_ci_less_p, "string-ci<?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically less than @var{s2}\n"
	    "regardless of case.")
#define FUNC_NAME s_scm_string_ci_less_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return string_ci_less_p (s1, s2);
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_string_ci_leq_p, "string-ci<=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically less than or equal\n"
	    "to @var{s2} regardless of case.")
#define FUNC_NAME s_scm_string_ci_leq_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return SCM_BOOL_NOT (string_ci_less_p (s2, s1));
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_string_ci_gr_p, "string-ci>?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically greater than\n"
	    "@var{s2} regardless of case.")
#define FUNC_NAME s_scm_string_ci_gr_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return string_ci_less_p (s2, s1);
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_string_ci_geq_p, "string-ci>=?", scm_tc7_rpsubr,
             (SCM s1, SCM s2),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically greater than or\n"
	    "equal to @var{s2} regardless of case.")
#define FUNC_NAME s_scm_string_ci_geq_p
{
  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  return SCM_BOOL_NOT (string_ci_less_p (s1, s2));
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
