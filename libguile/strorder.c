/*	Copyright (C) 1995, 1996, 1999, 2000, 2004, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/strings.h"
#include "libguile/symbols.h"

#include "libguile/validate.h"
#include "libguile/strorder.h"
#include "libguile/srfi-13.h"


SCM_C_INLINE_KEYWORD static SCM
srfi13_cmp (SCM s1, SCM s2, SCM (*cmp) (SCM, SCM, SCM, SCM, SCM, SCM))
{
  if (scm_is_true (cmp (s1, s2,
			SCM_UNDEFINED, SCM_UNDEFINED,
			SCM_UNDEFINED, SCM_UNDEFINED)))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

static SCM scm_i_string_equal_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_equal_p, "string=?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Lexicographic equality predicate; return @code{#t} if the two\n"
	    "strings are the same length and contain the same characters in\n"
	    "the same positions, otherwise return @code{#f}.\n"
	    "\n"
	    "The procedure @code{string-ci=?} treats upper and lower case\n"
	    "letters as though they were the same character, but\n"
	    "@code{string=?} treats upper and lower case as distinct\n"
	    "characters.")
#define FUNC_NAME s_scm_i_string_equal_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_eq)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_eq);
}
#undef FUNC_NAME

SCM scm_string_equal_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_equal_p
{
  return srfi13_cmp (s1, s2, scm_string_eq);
}
#undef FUNC_NAME

static SCM scm_i_string_ci_equal_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_ci_equal_p, "string-ci=?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Case-insensitive string equality predicate; return @code{#t} if\n"
	    "the two strings are the same length and their component\n"
	    "characters match (ignoring case) at each position; otherwise\n"
	    "return @code{#f}.")
#define FUNC_NAME s_scm_i_string_ci_equal_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_ci_eq)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_ci_eq);
}
#undef FUNC_NAME

SCM scm_string_ci_equal_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_ci_equal_p
{
  return srfi13_cmp (s1, s2, scm_string_ci_eq);
}
#undef FUNC_NAME

static SCM scm_i_string_less_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_less_p, "string<?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically less than @var{s2}.")
#define FUNC_NAME s_scm_i_string_less_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_lt)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_lt);
}
#undef FUNC_NAME

SCM scm_string_less_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_less_p
{
  return srfi13_cmp (s1, s2, scm_string_lt);
}
#undef FUNC_NAME

static SCM scm_i_string_leq_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_leq_p, "string<=?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically less than or equal to @var{s2}.")
#define FUNC_NAME s_scm_i_string_leq_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_le)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_le);
}
#undef FUNC_NAME

SCM scm_string_leq_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_leq_p
{
  return srfi13_cmp (s1, s2, scm_string_le);
}
#undef FUNC_NAME

static SCM scm_i_string_gr_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_gr_p, "string>?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically greater than @var{s2}.")
#define FUNC_NAME s_scm_i_string_gr_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_gt)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_gt);
}
#undef FUNC_NAME

SCM scm_string_gr_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_gr_p
{
  return srfi13_cmp (s1, s2, scm_string_gt);
}
#undef FUNC_NAME

static SCM scm_i_string_geq_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_geq_p, "string>=?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Lexicographic ordering predicate; return @code{#t} if @var{s1}\n"
	    "is lexicographically greater than or equal to @var{s2}.")
#define FUNC_NAME s_scm_i_string_geq_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_ge)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_ge);
}
#undef FUNC_NAME

SCM scm_string_geq_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_geq_p
{
  return srfi13_cmp (s1, s2, scm_string_ge);
}
#undef FUNC_NAME

static SCM scm_i_string_ci_less_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_ci_less_p, "string-ci<?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically less than @var{s2}\n"
	    "regardless of case.")
#define FUNC_NAME s_scm_i_string_ci_less_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_ci_lt)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_ci_lt);
}
#undef FUNC_NAME

SCM scm_string_ci_less_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_ci_less_p
{
  return srfi13_cmp (s1, s2, scm_string_ci_lt);
}
#undef FUNC_NAME

static SCM scm_i_string_ci_leq_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_ci_leq_p, "string-ci<=?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically less than or equal\n"
	    "to @var{s2} regardless of case.")
#define FUNC_NAME s_scm_i_string_ci_leq_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_ci_le)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_ci_le);
}
#undef FUNC_NAME

SCM scm_string_ci_leq_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_ci_leq_p
{
  return srfi13_cmp (s1, s2, scm_string_ci_le);
}
#undef FUNC_NAME

static SCM scm_i_string_ci_gr_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_ci_gr_p, "string-ci>?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically greater than\n"
	    "@var{s2} regardless of case.")
#define FUNC_NAME s_scm_i_string_ci_gr_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_ci_gt)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_ci_gt);
}
#undef FUNC_NAME

SCM scm_string_ci_gr_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_ci_gr_p
{
  return srfi13_cmp (s1, s2, scm_string_ci_gt);
}
#undef FUNC_NAME

static SCM scm_i_string_ci_geq_p (SCM s1, SCM s2, SCM rest);
SCM_DEFINE (scm_i_string_ci_geq_p, "string-ci>=?", 0, 2, 1,
            (SCM s1, SCM s2, SCM rest),
	    "Case insensitive lexicographic ordering predicate; return\n"
	    "@code{#t} if @var{s1} is lexicographically greater than or\n"
	    "equal to @var{s2} regardless of case.")
#define FUNC_NAME s_scm_i_string_ci_geq_p
{
  if (SCM_UNBNDP (s1) || SCM_UNBNDP (s2))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (srfi13_cmp (s1, s2, scm_string_ci_ge)))
        return SCM_BOOL_F;
      s1 = s2;
      s2 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return srfi13_cmp (s1, s2, scm_string_ci_ge);
}
#undef FUNC_NAME

SCM scm_string_ci_geq_p (SCM s1, SCM s2)
#define FUNC_NAME s_scm_i_string_ci_geq_p
{
  return srfi13_cmp (s1, s2, scm_string_ci_ge);
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
