/*	Copyright (C) 1995,1996,1998, 2000, 2001, 2004, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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

#include <ctype.h>
#include <limits.h>
#include <unicase.h>
#include <unictype.h>

#include "libguile/_scm.h"
#include "libguile/validate.h"

#include "libguile/chars.h"
#include "libguile/srfi-14.h"



SCM_DEFINE (scm_char_p, "char?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a character, else @code{#f}.")
#define FUNC_NAME s_scm_char_p
{
  return scm_from_bool (SCM_CHARP(x));
}
#undef FUNC_NAME

static SCM scm_i_char_eq_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_eq_p, "char=?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} if the Unicode code point of @var{x} is equal to the\n"
            "code point of @var{y}, else @code{#f}.\n")
#define FUNC_NAME s_scm_i_char_eq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_eq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_eq_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_eq_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_eq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_is_eq (x, y));
}
#undef FUNC_NAME


static SCM scm_i_char_less_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_less_p, "char<?", 0, 2, 1, 
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} iff the code point of @var{x} is less than the code\n"
            "point of @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_i_char_less_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_less_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_less_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_less_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_less_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) < SCM_CHAR(y));
}
#undef FUNC_NAME

static SCM scm_i_char_leq_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_leq_p, "char<=?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} if the Unicode code point of @var{x} is less than or\n"
            "equal to the code point of @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_i_char_leq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_leq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_leq_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_leq_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_leq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) <= SCM_CHAR(y));
}
#undef FUNC_NAME

static SCM scm_i_char_gr_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_gr_p, "char>?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} if the Unicode code point of @var{x} is greater than\n"
            "the code point of @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_i_char_gr_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_gr_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_gr_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_gr_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_gr_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) > SCM_CHAR(y));
}
#undef FUNC_NAME

static SCM scm_i_char_geq_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_geq_p, "char>=?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} if the Unicode code point of @var{x} is greater than\n"
            "or equal to the code point of @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_i_char_geq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_geq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_geq_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_geq_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_geq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) >= SCM_CHAR(y));
}
#undef FUNC_NAME

/* FIXME?: R6RS specifies that these comparisons are case-folded.
   This is the same thing as comparing the uppercase characters in
   practice, but, not in theory.  Unicode has table containing their
   definition of case-folded character mappings.  A more correct
   implementation would be to use that table and make a char-foldcase
   function.  */

static SCM scm_i_char_ci_eq_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_ci_eq_p, "char-ci=?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} if the case-folded Unicode code point of @var{x} is\n"
            "the same as the case-folded code point of @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_i_char_ci_eq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_ci_eq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_ci_eq_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_ci_eq_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_ci_eq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x))==scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

static SCM scm_i_char_ci_less_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_ci_less_p, "char-ci<?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} if the case-folded Unicode code point of @var{x} is\n"
            "less than the case-folded code point of @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_i_char_ci_less_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_ci_less_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_ci_less_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_ci_less_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_ci_less_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool ((scm_c_upcase(SCM_CHAR(x))) < scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

static SCM scm_i_char_ci_leq_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_ci_leq_p, "char-ci<=?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} iff the case-folded Unicode code point of @var{x} is\n"
            "less than or equal to the case-folded code point of @var{y}, else\n"
            "@code{#f}")
#define FUNC_NAME s_scm_i_char_ci_leq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_ci_leq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_ci_leq_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_ci_leq_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_ci_leq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x)) <= scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

static SCM scm_i_char_ci_gr_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_ci_gr_p, "char-ci>?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} iff the case-folded code point of @var{x} is greater\n"
            "than the case-folded code point of @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_i_char_ci_gr_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_ci_gr_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_ci_gr_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_ci_gr_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_ci_gr_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x)) > scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

static SCM scm_i_char_ci_geq_p (SCM x, SCM y, SCM rest);
SCM_DEFINE (scm_i_char_ci_geq_p, "char-ci>=?", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return @code{#t} iff the case-folded Unicode code point of @var{x} is\n"
            "greater than or equal to the case-folded code point of @var{y}, else\n"
            "@code{#f}.")
#define FUNC_NAME s_scm_i_char_ci_geq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_char_ci_geq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_char_ci_geq_p (x, y);
}
#undef FUNC_NAME

SCM scm_char_ci_geq_p (SCM x, SCM y)
#define FUNC_NAME s_scm_i_char_ci_geq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x)) >= scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_alphabetic_p, "char-alphabetic?", 1, 0, 0,
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is alphabetic, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_alphabetic_p
{
  return scm_char_set_contains_p (scm_char_set_letter, chr);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_numeric_p, "char-numeric?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is numeric, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_numeric_p
{
  return scm_char_set_contains_p (scm_char_set_digit, chr);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_whitespace_p, "char-whitespace?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is whitespace, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_whitespace_p
{
  return scm_char_set_contains_p (scm_char_set_whitespace, chr);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_upper_case_p, "char-upper-case?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is uppercase, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_upper_case_p
{
  return scm_char_set_contains_p (scm_char_set_upper_case, chr);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_lower_case_p, "char-lower-case?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is lowercase, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_lower_case_p
{
  return scm_char_set_contains_p (scm_char_set_lower_case, chr);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_is_both_p, "char-is-both?", 1, 0, 0, 
            (SCM chr),
	    "Return @code{#t} iff @var{chr} is either uppercase or lowercase, else\n"
            "@code{#f}.\n")
#define FUNC_NAME s_scm_char_is_both_p
{
  if (scm_is_true (scm_char_set_contains_p (scm_char_set_lower_case, chr)))
    return SCM_BOOL_T;
  return scm_char_set_contains_p (scm_char_set_upper_case, chr);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_to_integer, "char->integer", 1, 0, 0, 
            (SCM chr),
            "Return the Unicode code point of @var{chr}.")
#define FUNC_NAME s_scm_char_to_integer
{
  SCM_VALIDATE_CHAR (1, chr);
  return scm_from_uint32 (SCM_CHAR(chr));
}
#undef FUNC_NAME


SCM_DEFINE (scm_integer_to_char, "integer->char", 1, 0, 0, 
           (SCM n),
            "Return the character that has Unicode code point @var{n}.  The integer\n"
            "@var{n} must be a valid code point.  Valid code points are in the\n"
            "ranges 0 to @code{#xD7FF} inclusive or @code{#xE000} to\n"
            "@code{#x10FFFF} inclusive.")
#define FUNC_NAME s_scm_integer_to_char
{
  scm_t_wchar cn;

  cn = scm_to_wchar (n);

  /* Avoid the surrogates.  */
  if (!SCM_IS_UNICODE_CHAR (cn))
    scm_out_of_range (FUNC_NAME, n);

  return SCM_MAKE_CHAR (cn);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_upcase, "char-upcase", 1, 0, 0, 
           (SCM chr),
	    "Return the uppercase character version of @var{chr}.")
#define FUNC_NAME s_scm_char_upcase
{
  SCM_VALIDATE_CHAR (1, chr);
  return SCM_MAKE_CHAR (scm_c_upcase (SCM_CHAR (chr)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_downcase, "char-downcase", 1, 0, 0, 
           (SCM chr),
	    "Return the lowercase character version of @var{chr}.")
#define FUNC_NAME s_scm_char_downcase
{
  SCM_VALIDATE_CHAR (1, chr);
  return SCM_MAKE_CHAR (scm_c_downcase (SCM_CHAR(chr)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_titlecase, "char-titlecase", 1, 0, 0,
	   (SCM chr),
	    "Return the titlecase character version of @var{chr}.")
#define FUNC_NAME s_scm_char_titlecase
{
  SCM_VALIDATE_CHAR (1, chr);
  return SCM_MAKE_CHAR (scm_c_titlecase (SCM_CHAR(chr)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_general_category, "char-general-category", 1, 0, 0,
           (SCM chr),
            "Return a symbol representing the Unicode general category of "
            "@var{chr} or @code{#f} if a named category cannot be found.")
#define FUNC_NAME s_scm_char_general_category
{
  const char *sym;
  uc_general_category_t cat;

  SCM_VALIDATE_CHAR (1, chr);
  cat = uc_general_category (SCM_CHAR (chr));
  sym = uc_general_category_name (cat);

  if (sym != NULL)
    return scm_from_locale_symbol (sym);
  return SCM_BOOL_F;
}
#undef FUNC_NAME





/*
TODO: change name  to scm_i_.. ? --hwn
*/


scm_t_wchar
scm_c_upcase (scm_t_wchar c)
{
  return uc_toupper ((int) c);
}


scm_t_wchar
scm_c_downcase (scm_t_wchar c)
{
  return uc_tolower ((int) c);
}

scm_t_wchar
scm_c_titlecase (scm_t_wchar c)
{
  return uc_totitle ((int) c);
}



/* There are a few sets of character names: R5RS, Guile
   extensions for control characters, and leftover Guile extensions.
   They are listed in order of precedence.  */

static const char *const scm_r5rs_charnames[] = {
  "space", "newline"
};

static const scm_t_uint32 const scm_r5rs_charnums[] = {
  0x20, 0x0a
};

#define SCM_N_R5RS_CHARNAMES (sizeof (scm_r5rs_charnames) / sizeof (char *))

static const char *const scm_r6rs_charnames[] = {
  "nul", "alarm", "backspace", "tab", "linefeed", "vtab", "page",
  "return", "esc", "delete"
  /* 'space' and 'newline' are already included from the R5RS list.  */
};

static const scm_t_uint32 const scm_r6rs_charnums[] = {
  0x00, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c,
  0x0d, 0x1b, 0x7f
};

#define SCM_N_R6RS_CHARNAMES (sizeof (scm_r6rs_charnames) / sizeof (char *))

/* The abbreviated names for control characters.  */
static const char *const scm_C0_control_charnames[] = {
  /* C0 controls */
  "nul", "soh", "stx", "etx", "eot", "enq", "ack", "bel",
  "bs",  "ht",  "lf",  "vt",  "ff",  "cr",  "so",  "si",
  "dle", "dc1", "dc2", "dc3", "dc4", "nak", "syn", "etb", 
  "can", "em",  "sub", "esc", "fs",  "gs",  "rs",  "us",
  "sp", "del"
};

static const scm_t_uint32 const scm_C0_control_charnums[] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x20, 0x7f
};

#define SCM_N_C0_CONTROL_CHARNAMES (sizeof (scm_C0_control_charnames) / sizeof (char *))

static const char *const scm_alt_charnames[] = {
  "null", "nl", "np"
};

static const scm_t_uint32 const scm_alt_charnums[] = {
  0x00, 0x0a, 0x0c
};

#define SCM_N_ALT_CHARNAMES (sizeof (scm_alt_charnames) / sizeof (char *))

/* Returns the string charname for a character if it exists, or NULL
   otherwise.  */
const char *
scm_i_charname (SCM chr)
{
  size_t c;
  scm_t_uint32 i = SCM_CHAR (chr);

  for (c = 0; c < SCM_N_R5RS_CHARNAMES; c++)
    if (scm_r5rs_charnums[c] == i)
      return scm_r5rs_charnames[c];

  for (c = 0; c < SCM_N_R6RS_CHARNAMES; c++)
    if (scm_r6rs_charnums[c] == i)
      return scm_r6rs_charnames[c];

  for (c = 0; c < SCM_N_C0_CONTROL_CHARNAMES; c++)
    if (scm_C0_control_charnums[c] == i)
      return scm_C0_control_charnames[c];

  /* Since the characters in scm_alt_charnums is a subset of
     scm_C0_control_charnums, this code is never reached.  */
  for (c = 0; c < SCM_N_ALT_CHARNAMES; c++)
    if (scm_alt_charnums[c] == i)
      return scm_alt_charnames[c];

  return NULL;
}

/* Return a character from a string charname.  */
SCM
scm_i_charname_to_char (const char *charname, size_t charname_len)
{
  size_t c;

  /* The R5RS charnames.  These are supposed to be case insensitive. */
  for (c = 0; c < SCM_N_R5RS_CHARNAMES; c++)
    if ((strlen (scm_r5rs_charnames[c]) == charname_len)
	&& (!strncasecmp (scm_r5rs_charnames[c], charname, charname_len)))
      return SCM_MAKE_CHAR (scm_r5rs_charnums[c]);

  /* The R6RS charnames.  R6RS says that these should be case-sensitive.  They
     are left as case-insensitive to avoid confusion.  */
  for (c = 0; c < SCM_N_R6RS_CHARNAMES; c++)
    if ((strlen (scm_r6rs_charnames[c]) == charname_len)
	&& (!strncasecmp (scm_r6rs_charnames[c], charname, charname_len)))
      return SCM_MAKE_CHAR (scm_r6rs_charnums[c]);

  /* Then come the controls.  By Guile convention, these are not case
     sensitive.  */
  for (c = 0; c < SCM_N_C0_CONTROL_CHARNAMES; c++)
    if ((strlen (scm_C0_control_charnames[c]) == charname_len)
	&& (!strncasecmp (scm_C0_control_charnames[c], charname, charname_len)))
      return SCM_MAKE_CHAR (scm_C0_control_charnums[c]);

  /* Lastly are some old names carried over for compatibility.  */
  for (c = 0; c < SCM_N_ALT_CHARNAMES; c++)
    if ((strlen (scm_alt_charnames[c]) == charname_len)
	&& (!strncasecmp (scm_alt_charnames[c], charname, charname_len)))
      return SCM_MAKE_CHAR (scm_alt_charnums[c]);

  return SCM_BOOL_F;
}




void
scm_init_chars ()
{
#include "libguile/chars.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
