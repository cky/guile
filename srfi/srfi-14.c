/* srfi-14.c --- SRFI-14 procedures for Guile
 *
 * Copyright (C) 2001 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives
 * permission for additional uses of the text contained in its release
 * of GUILE.
 *
 * The exception is that, if you link the GUILE library with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public
 * License.  Your use of that executable is in no way restricted on
 * account of linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public
 * License.
 *
 * This exception applies only to the code released by the Free
 * Software Foundation under the name GUILE.  If you copy code from
 * other Free Software Foundation releases into a copy of GUILE, as
 * the General Public License permits, the exception does not apply to
 * the code that you add in this way.  To avoid misleading anyone as
 * to the status of such modified files, you must delete this
 * exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include <string.h>

#include <libguile.h>

#include "srfi-14.h"


#define SCM_CHARSET_SET(cs, idx) \
  (((long *) SCM_SMOB_DATA (cs))[(idx) / SCM_BITS_PER_LONG] |= \
    (1L << ((idx) % SCM_BITS_PER_LONG)))

#define BYTES_PER_CHARSET (SCM_CHARSET_SIZE / 8)
#define LONGS_PER_CHARSET (SCM_CHARSET_SIZE / SCM_BITS_PER_LONG)


/* Smob type code for character sets.  */
int scm_tc16_charset = 0;


/* Smob print hook for character sets.  */
static int
charset_print (SCM charset, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  int i;
  int first = 1;

  scm_puts ("#<charset {", port);
  for (i = 0; i < SCM_CHARSET_SIZE; i++)
    if (SCM_CHARSET_GET (charset, i))
      {
	if (first)
	  first = 0;
	else
	  scm_puts (" ", port);
	scm_write (SCM_MAKE_CHAR (i), port);
      }
  scm_puts ("}>", port);
  return 1;
}


/* Smob free hook for character sets. */
static size_t
charset_free (SCM charset)
{
  return scm_smob_free (charset);
}


/* Create a new, empty character set.  */
static SCM
make_char_set (const char * func_name)
{
  long * p;
  
  p = scm_must_malloc (BYTES_PER_CHARSET, func_name);
  memset (p, 0, BYTES_PER_CHARSET);
  SCM_RETURN_NEWSMOB (scm_tc16_charset, p);
}


SCM_DEFINE (scm_char_set_p, "char-set?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a character set, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_char_set_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_charset, obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_eq, "char-set=", 0, 0, 1,
	    (SCM char_sets),
	    "Return @code{#t} if all given character sets are equal.")
#define FUNC_NAME s_scm_char_set_eq
{
  int argnum = 1;
  long *cs1_data = NULL;

  SCM_VALIDATE_REST_ARGUMENT (char_sets);

  while (!SCM_NULLP (char_sets))
    {
      SCM csi = SCM_CAR (char_sets);
      long *csi_data;

      SCM_VALIDATE_SMOB (argnum, csi, charset);
      argnum++;
      csi_data = (long *) SCM_SMOB_DATA (csi);
      if (cs1_data == NULL)
	cs1_data = csi_data;
      else if (memcmp (cs1_data, csi_data, BYTES_PER_CHARSET) != 0)
	return SCM_BOOL_F;
      char_sets = SCM_CDR (char_sets);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_leq, "char-set<=", 0, 0, 1,
	    (SCM char_sets),
	    "Return @code{#t} if every character set @var{cs}i is a subset\n"
	    "of character set @var{cs}i+1.")
#define FUNC_NAME s_scm_char_set_leq
{
  int argnum = 1;
  long *prev_data = NULL;

  SCM_VALIDATE_REST_ARGUMENT (char_sets);

  while (!SCM_NULLP (char_sets))
    {
      SCM csi = SCM_CAR (char_sets);
      long *csi_data;

      SCM_VALIDATE_SMOB (argnum, csi, charset);
      argnum++;
      csi_data = (long *) SCM_SMOB_DATA (csi);
      if (prev_data)
	{
	  int k;
	  
	  for (k = 0; k < LONGS_PER_CHARSET; k++)
	    {
	      if ((prev_data[k] & csi_data[k]) != prev_data[k])
		return SCM_BOOL_F;
	    }
	}
      prev_data = csi_data;
      char_sets = SCM_CDR (char_sets);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_hash, "char-set-hash", 1, 1, 0,
	    (SCM cs, SCM bound),
	    "Compute a hash value for the character set @var{cs}.  If\n"
	    "@var{bound} is given and non-zero, it restricts the\n"
	    "returned value to the range 0 @dots{} @var{bound - 1}.")
#define FUNC_NAME s_scm_char_set_hash
{
  const int default_bnd = 871;
  int bnd;
  long * p;
  unsigned long val = 0;
  int k;

  SCM_VALIDATE_SMOB (1, cs, charset);
  
  if (SCM_UNBNDP (bound))
    bnd = default_bnd;
  else
    {
      SCM_VALIDATE_INUM_MIN_COPY (2, bound, 0, bnd);
      if (bnd == 0)
	bnd = default_bnd;
    }

  p = (long *) SCM_SMOB_DATA (cs);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    {
      if (p[k] != 0)
        val = p[k] + (val << 1);
    }
  return SCM_MAKINUM (val % bnd);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_cursor, "char-set-cursor", 1, 0, 0,
	    (SCM cs),
	    "Return a cursor into the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_cursor
{
  int idx;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (idx = 0; idx < SCM_CHARSET_SIZE; idx++)
    {
      if (SCM_CHARSET_GET (cs, idx))
	break;
    }
  return SCM_MAKINUM (idx);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_ref, "char-set-ref", 2, 0, 0,
	    (SCM cs, SCM cursor),
	    "Return the character at the current cursor position\n"
	    "@var{cursor} in the character set @var{cs}.  It is an error to\n"
	    "pass a cursor for which @code{end-of-char-set?} returns true.")
#define FUNC_NAME s_scm_char_set_ref
{
  int ccursor;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_INUM_MIN_COPY (2, cursor, 0, ccursor);

  if (ccursor >= SCM_CHARSET_SIZE || !SCM_CHARSET_GET (cs, ccursor))
    SCM_MISC_ERROR ("invalid character set cursor: ~A", scm_list_1 (cursor));
  return SCM_MAKE_CHAR (ccursor);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_cursor_next, "char-set-cursor-next", 2, 0, 0,
	    (SCM cs, SCM cursor),
	    "Advance the character set cursor @var{cursor} to the next\n"
	    "character in the character set @var{cs}.  It is an error if the\n"
	    "cursor given satisfies @code{end-of-char-set?}.")
#define FUNC_NAME s_scm_char_set_cursor_next
{
  int ccursor;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_INUM_MIN_COPY (2, cursor, 0, ccursor);

  if (ccursor >= SCM_CHARSET_SIZE || !SCM_CHARSET_GET (cs, ccursor))
    SCM_MISC_ERROR ("invalid character set cursor: ~A", scm_list_1 (cursor));
  for (ccursor++; ccursor < SCM_CHARSET_SIZE; ccursor++)
    {
      if (SCM_CHARSET_GET (cs, ccursor))
	break;
    }
  return SCM_MAKINUM (ccursor);
}
#undef FUNC_NAME


SCM_DEFINE (scm_end_of_char_set_p, "end-of-char-set?", 1, 0, 0,
	    (SCM cursor),
	    "Return @code{#t} if @var{cursor} has reached the end of a\n"
	    "character set, @code{#f} otherwise.")
#define FUNC_NAME s_scm_end_of_char_set_p
{
  int ccursor;

  SCM_VALIDATE_INUM_MIN_COPY (1, cursor, 0, ccursor);
  return SCM_BOOL (ccursor >= SCM_CHARSET_SIZE);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_fold, "char-set-fold", 3, 0, 0,
	    (SCM kons, SCM knil, SCM cs),
	    "Fold the procedure @var{kons} over the character set @var{cs},\n"
	    "initializing it with @var{knil}.")
#define FUNC_NAME s_scm_char_set_fold
{
  int k;

  SCM_VALIDATE_PROC (1, kons);
  SCM_VALIDATE_SMOB (3, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	knil = scm_call_2 (kons, SCM_MAKE_CHAR (k), knil);
      }
  return knil;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_unfold, "char-set-unfold", 4, 1, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base_cs),
	    "This is a fundamental constructor for character sets.\n"
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of ``seed'' values\n"
	    "from the initial seed: @var{seed}, (@var{g} @var{seed}),\n"
	    "(@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}), @dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of the seed values.\n"
	    "@item @var{f} maps each seed value to a character. These\n"
	    "characters are added to the base character set @var{base_cs} to\n"
	    "form the result; @var{base_cs} defaults to the empty set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_char_set_unfold
{
  SCM result, tmp;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  if (!SCM_UNBNDP (base_cs))
    {
      SCM_VALIDATE_SMOB (5, base_cs, charset);
      result = scm_char_set_copy (base_cs);
    }
  else
    result = make_char_set (FUNC_NAME);

  tmp = scm_call_1 (p, seed);
  while (SCM_FALSEP (tmp))
    {
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      SCM_CHARSET_SET (result, SCM_CHAR (ch));

      seed = scm_call_1 (g, seed);
      tmp = scm_call_1 (p, seed);
    }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_unfold_x, "char-set-unfold!", 5, 0, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base_cs),
	    "This is a fundamental constructor for character sets.\n"
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of ``seed'' values\n"
	    "from the initial seed: @var{seed}, (@var{g} @var{seed}),\n"
	    "(@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}), @dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of the seed values.\n"
	    "@item @var{f} maps each seed value to a character. These\n"
	    "characters are added to the base character set @var{base_cs} to\n"
	    "form the result; @var{base_cs} defaults to the empty set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_char_set_unfold_x
{
  SCM tmp;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  SCM_VALIDATE_SMOB (5, base_cs, charset);

  tmp = scm_call_1 (p, seed);
  while (SCM_FALSEP (tmp))
    {
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      SCM_CHARSET_SET (base_cs, SCM_CHAR (ch));

      seed = scm_call_1 (g, seed);
      tmp = scm_call_1 (p, seed);
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_for_each, "char-set-for-each", 2, 0, 0,
	    (SCM proc, SCM cs),
	    "Apply @var{proc} to every character in the character set\n"
	    "@var{cs}.  The return value is not specified.")
#define FUNC_NAME s_scm_char_set_for_each
{
  int k;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      scm_call_1 (proc, SCM_MAKE_CHAR (k));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_map, "char-set-map", 2, 0, 0,
	    (SCM proc, SCM cs),
	    "Map the procedure @var{proc} over every character in @var{cs}.\n"
	    "@var{proc} must be a character -> character procedure.")
#define FUNC_NAME s_scm_char_set_map
{
  SCM result;
  int k;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_SMOB (2, cs, charset);

  result = make_char_set (FUNC_NAME);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	SCM ch = scm_call_1 (proc, SCM_MAKE_CHAR (k));
	if (!SCM_CHARP (ch))
	  SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (proc));
	SCM_CHARSET_SET (result, SCM_CHAR (ch));
      }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_copy, "char-set-copy", 1, 0, 0,
	    (SCM cs),
	    "Return a newly allocated character set containing all\n"
	    "characters in @var{cs}.")
#define FUNC_NAME s_scm_char_set_copy
{
  SCM ret;
  long * p1, * p2;
  int k;

  SCM_VALIDATE_SMOB (1, cs, charset);
  ret = make_char_set (FUNC_NAME);
  p1 = (long *) SCM_SMOB_DATA (cs);
  p2 = (long *) SCM_SMOB_DATA (ret);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    p2[k] = p1[k];
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set, "char-set", 0, 0, 1,
	    (SCM rest),
	    "Return a character set containing all given characters.")
#define FUNC_NAME s_scm_char_set
{
  SCM cs;
  long * p;
  int argnum = 1;

  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (cs);
  while (!SCM_NULLP (rest))
    {
      int c;

      SCM_VALIDATE_CHAR_COPY (argnum, SCM_CAR (rest), c);
      argnum++;
      rest = SCM_CDR (rest);
      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_char_set, "list->char-set", 1, 1, 0,
	    (SCM list, SCM base_cs),
	    "Convert the character list @var{list} to a character set.  If\n"
	    "the character set @var{base_cs} is given, the character in this\n"
	    "set are also included in the result.")
#define FUNC_NAME s_scm_list_to_char_set
{
  SCM cs;
  long * p;

  SCM_VALIDATE_LIST (1, list);
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (2, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  p = (long *) SCM_SMOB_DATA (cs);
  while (!SCM_NULLP (list))
    {
      SCM chr = SCM_CAR (list);
      int c;

      SCM_VALIDATE_CHAR_COPY (0, chr, c);
      list = SCM_CDR (list);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_char_set_x, "list->char-set!", 2, 0, 0,
	    (SCM list, SCM base_cs),
	    "Convert the character list @var{list} to a character set.  The\n"
	    "characters are added to @var{base_cs} and @var{base_cs} is\n"
	    "returned.")
#define FUNC_NAME s_scm_list_to_char_set_x
{
  long * p;

  SCM_VALIDATE_LIST (1, list);
  SCM_VALIDATE_SMOB (2, base_cs, charset);
  p = (long *) SCM_SMOB_DATA (base_cs);
  while (!SCM_NULLP (list))
    {
      SCM chr = SCM_CAR (list);
      int c;

      SCM_VALIDATE_CHAR_COPY (0, chr, c);
      list = SCM_CDR (list);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_char_set, "string->char-set", 1, 1, 0,
	    (SCM str, SCM base_cs),
	    "Convert the string @var{str} to a character set.  If the\n"
	    "character set @var{base_cs} is given, the characters in this\n"
	    "set are also included in the result.")
#define FUNC_NAME s_scm_string_to_char_set
{
  SCM cs;
  long * p;
  char * s;
  size_t k = 0;

  SCM_VALIDATE_STRING (1, str);
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (2, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  p = (long *) SCM_SMOB_DATA (cs);
  s = SCM_STRING_CHARS (str);
  while (k < SCM_STRING_LENGTH (str))
    {
      int c = s[k++];
      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_char_set_x, "string->char-set!", 2, 0, 0,
	    (SCM str, SCM base_cs),
	    "Convert the string @var{str} to a character set.  The\n"
	    "characters from the string are added to @var{base_cs}, and\n"
	    "@var{base_cs} is returned.")
#define FUNC_NAME s_scm_string_to_char_set_x
{
  long * p;
  char * s;
  size_t k = 0;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_SMOB (2, base_cs, charset);
  p = (long *) SCM_SMOB_DATA (base_cs);
  s = SCM_STRING_CHARS (str);
  while (k < SCM_STRING_LENGTH (str))
    {
      int c = s[k++];
      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_filter, "char-set-filter", 2, 1, 0,
	    (SCM pred, SCM cs, SCM base_cs),
	    "Return a character set containing every character from @var{cs}\n"
	    "so that it satisfies @var{pred}.  If provided, the characters\n"
	    "from @var{base_cs} are added to the result.")
#define FUNC_NAME s_scm_char_set_filter
{
  SCM ret;
  int k;
  long * p;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);
  if (!SCM_UNBNDP (base_cs))
    {
      SCM_VALIDATE_SMOB (3, base_cs, charset);
      ret = scm_char_set_copy (base_cs);
    }
  else
    ret = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (ret);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    {
      if (SCM_CHARSET_GET (cs, k))
	{
	  SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));

	  if (!SCM_FALSEP (res))
	    p[k / SCM_BITS_PER_LONG] |= 1L << (k % SCM_BITS_PER_LONG);
	}
    }
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_filter_x, "char-set-filter!", 3, 0, 0,
	    (SCM pred, SCM cs, SCM base_cs),
	    "Return a character set containing every character from @var{cs}\n"
	    "so that it satisfies @var{pred}.  The characters are added to\n"
	    "@var{base_cs} and @var{base_cs} is returned.")
#define FUNC_NAME s_scm_char_set_filter_x
{
  int k;
  long * p;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);
  SCM_VALIDATE_SMOB (3, base_cs, charset);
  p = (long *) SCM_SMOB_DATA (base_cs);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    {
      if (SCM_CHARSET_GET (cs, k))
	{
	  SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));

	  if (!SCM_FALSEP (res))
	    p[k / SCM_BITS_PER_LONG] |= 1L << (k % SCM_BITS_PER_LONG);
	}
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_ucs_range_to_char_set, "ucs-range->char-set", 2, 2, 0,
	    (SCM lower, SCM upper, SCM error, SCM base_cs),
	    "Return a character set containing all characters whose\n"
	    "character codes lie in the half-open range\n"
	    "[@var{lower},@var{upper}).\n"
	    "\n"
	    "If @var{error} is a true value, an error is signalled if the\n"
	    "specified range contains characters which are not contained in\n"
	    "the implemented character range.  If @var{error} is @code{#f},\n"
	    "these characters are silently left out of the resultung\n"
	    "character set.\n"
	    "\n"
	    "The characters in @var{base_cs} are added to the result, if\n"
	    "given.")
#define FUNC_NAME s_scm_ucs_range_to_char_set
{
  SCM cs;
  int clower, cupper;
  long * p;

  SCM_VALIDATE_INUM_COPY (1, lower, clower);
  SCM_VALIDATE_INUM_COPY (2, upper, cupper);
  SCM_ASSERT_RANGE (1, lower, clower >= 0);
  SCM_ASSERT_RANGE (2, upper, cupper >= 0 && cupper >= clower);
  if (!SCM_UNBNDP (error))
    {
      if (!SCM_FALSEP (error))
	{
	  SCM_ASSERT_RANGE (1, lower, clower <= SCM_CHARSET_SIZE);
	  SCM_ASSERT_RANGE (2, upper, cupper <= SCM_CHARSET_SIZE);
	}
    }
  if (clower > SCM_CHARSET_SIZE)
    clower = SCM_CHARSET_SIZE;
  if (cupper > SCM_CHARSET_SIZE)
    cupper = SCM_CHARSET_SIZE;
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (2, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  p = (long *) SCM_SMOB_DATA (cs);
  while (clower < cupper)
    {
      p[clower / SCM_BITS_PER_LONG] |= 1L << (clower % SCM_BITS_PER_LONG);
      clower++;
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_ucs_range_to_char_set_x, "ucs-range->char-set!", 4, 0, 0,
	    (SCM lower, SCM upper, SCM error, SCM base_cs),
	    "Return a character set containing all characters whose\n"
	    "character codes lie in the half-open range\n"
	    "[@var{lower},@var{upper}).\n"
	    "\n"
	    "If @var{error} is a true value, an error is signalled if the\n"
	    "specified range contains characters which are not contained in\n"
	    "the implemented character range.  If @var{error} is @code{#f},\n"
	    "these characters are silently left out of the resultung\n"
	    "character set.\n"
	    "\n"
	    "The characters are added to @var{base_cs} and @var{base_cs} is\n"
	    "returned.")
#define FUNC_NAME s_scm_ucs_range_to_char_set_x
{
  int clower, cupper;
  long * p;

  SCM_VALIDATE_INUM_COPY (1, lower, clower);
  SCM_VALIDATE_INUM_COPY (2, upper, cupper);
  SCM_ASSERT_RANGE (1, lower, clower >= 0);
  SCM_ASSERT_RANGE (2, upper, cupper >= 0 && cupper >= clower);
  if (!SCM_FALSEP (error))
    {
      SCM_ASSERT_RANGE (1, lower, clower <= SCM_CHARSET_SIZE);
      SCM_ASSERT_RANGE (2, upper, cupper <= SCM_CHARSET_SIZE);
    }
  if (clower > SCM_CHARSET_SIZE)
    clower = SCM_CHARSET_SIZE;
  if (cupper > SCM_CHARSET_SIZE)
    cupper = SCM_CHARSET_SIZE;
  p = (long *) SCM_SMOB_DATA (base_cs);
  while (clower < cupper)
    {
      p[clower / SCM_BITS_PER_LONG] |= 1L << (clower % SCM_BITS_PER_LONG);
      clower++;
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_size, "char-set-size", 1, 0, 0,
	    (SCM cs),
	    "Return the number of elements in character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_size
{
  int k, count = 0;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      count++;
  return SCM_MAKINUM (count);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_count, "char-set-count", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return the number of the elements int the character set\n"
	    "@var{cs} which satisfy the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_count
{
  int k, count = 0;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));
	if (!SCM_FALSEP (res))
	  count++;
      }
  return SCM_MAKINUM (count);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_to_list, "char-set->list", 1, 0, 0,
	    (SCM cs),
	    "Return a list containing the elements of the character set\n"
	    "@var{cs}.")
#define FUNC_NAME s_scm_char_set_to_list
{
  int k;
  SCM result = SCM_EOL;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (k = SCM_CHARSET_SIZE; k > 0; k--)
    if (SCM_CHARSET_GET (cs, k - 1))
      result = scm_cons (SCM_MAKE_CHAR (k - 1), result);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_to_string, "char-set->string", 1, 0, 0,
	    (SCM cs),
	    "Return a string containing the elements of the character set\n"
	    "@var{cs}.  The order in which the characters are placed in the\n"
	    "string is not defined.")
#define FUNC_NAME s_scm_char_set_to_string
{
  int k;
  int count = 0;
  int idx = 0;
  SCM result;
  char * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      count++;
  result = scm_allocate_string (count);
  p = SCM_STRING_CHARS (result);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      p[idx++] = k;
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_contains_p, "char-set-contains?", 2, 0, 0,
	    (SCM cs, SCM ch),
	    "Return @code{#t} iff the character @var{ch} is contained in the\n"
	    "character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_contains_p
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_CHAR (2, ch);
  return SCM_BOOL (SCM_CHARSET_GET (cs, SCM_CHAR (ch)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_every, "char-set-every", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return a true value if every character in the character set\n"
	    "@var{cs} satisfies the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_every
{
  int k;
  SCM res = SCM_BOOL_T;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	res = scm_call_1 (pred, SCM_MAKE_CHAR (k));
	if (SCM_FALSEP (res))
	  return res;
      }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_any, "char-set-any", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return a true value if any character in the character set\n"
	    "@var{cs} satisfies the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_any
{
  int k;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));
	if (!SCM_FALSEP (res))
	  return res;
      }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_adjoin, "char-set-adjoin", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Add all character arguments to the first argument, which must\n"
	    "be a character set.")
#define FUNC_NAME s_scm_char_set_adjoin
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = scm_char_set_copy (cs);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!SCM_NULLP (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_delete, "char-set-delete", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Delete all character arguments from the first argument, which\n"
	    "must be a character set.")
#define FUNC_NAME s_scm_char_set_delete
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = scm_char_set_copy (cs);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!SCM_NULLP (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] &= ~(1L << (c % SCM_BITS_PER_LONG));
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_adjoin_x, "char-set-adjoin!", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Add all character arguments to the first argument, which must\n"
	    "be a character set.")
#define FUNC_NAME s_scm_char_set_adjoin_x
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!SCM_NULLP (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_delete_x, "char-set-delete!", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Delete all character arguments from the first argument, which\n"
	    "must be a character set.")
#define FUNC_NAME s_scm_char_set_delete_x
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!SCM_NULLP (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] &= ~(1L << (c % SCM_BITS_PER_LONG));
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_complement, "char-set-complement", 1, 0, 0,
	    (SCM cs),
	    "Return the complement of the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_complement
{
  int k;
  SCM res;
  long * p, * q;

  SCM_VALIDATE_SMOB (1, cs, charset);

  res = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (res);
  q = (long *) SCM_SMOB_DATA (cs);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    p[k] = ~q[k];
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_union, "char-set-union", 0, 0, 1,
	    (SCM rest),
	    "Return the union of all argument character sets.")
#define FUNC_NAME s_scm_char_set_union
{
  int c = 1;
  SCM res;
  long * p;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  res = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (res);
  while (!SCM_NULLP (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] |= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_intersection, "char-set-intersection", 0, 0, 1,
	    (SCM rest),
	    "Return the intersection of all argument character sets.")
#define FUNC_NAME s_scm_char_set_intersection
{
  SCM res;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  if (SCM_NULLP (rest))
    res = make_char_set (FUNC_NAME);
  else
    {
      long *p;
      int argnum = 2;

      res = scm_char_set_copy (SCM_CAR (rest));
      p = (long *) SCM_SMOB_DATA (res);
      rest = SCM_CDR (rest);

      while (SCM_CONSP (rest))
	{
	  int k;
	  SCM cs = SCM_CAR (rest);
	  long *cs_data;

	  SCM_VALIDATE_SMOB (argnum, cs, charset);
	  argnum++;
	  cs_data = (long *) SCM_SMOB_DATA (cs);
	  rest = SCM_CDR (rest);
	  for (k = 0; k < LONGS_PER_CHARSET; k++)
	    p[k] &= cs_data[k];
	}
    }

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_difference, "char-set-difference", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference of all argument character sets.")
#define FUNC_NAME s_scm_char_set_difference
{
  int c = 2;
  SCM res;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  res = scm_char_set_copy (cs1);
  p = (long *) SCM_SMOB_DATA (res);
  while (!SCM_NULLP (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] &= ~((long *) SCM_SMOB_DATA (cs))[k];
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_xor, "char-set-xor", 0, 0, 1,
	    (SCM rest),
	    "Return the exclusive-or of all argument character sets.")
#define FUNC_NAME s_scm_char_set_xor
{
  SCM res;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  if (SCM_NULLP (rest))
    res = make_char_set (FUNC_NAME);
  else
    {
      int argnum = 2;
      long * p;

      res = scm_char_set_copy (SCM_CAR (rest));
      p = (long *) SCM_SMOB_DATA (res);
      rest = SCM_CDR (rest);

      while (SCM_CONSP (rest))
	{
	  SCM cs = SCM_CAR (rest);
	  long *cs_data;
	  int k;

	  SCM_VALIDATE_SMOB (argnum, cs, charset);
	  argnum++;
	  cs_data = (long *) SCM_SMOB_DATA (cs);
	  rest = SCM_CDR (rest);

	  for (k = 0; k < LONGS_PER_CHARSET; k++)
	    p[k] ^= cs_data[k];
	}
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_diff_plus_intersection, "char-set-diff+intersection", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference and the intersection of all argument\n"
	    "character sets.")
#define FUNC_NAME s_scm_char_set_diff_plus_intersection
{
  int c = 2;
  SCM res1, res2;
  long * p, * q;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  res1 = scm_char_set_copy (cs1);
  res2 = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (res1);
  q = (long *) SCM_SMOB_DATA (res2);
  while (!SCM_NULLP (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      long *r;

      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      r = (long *) SCM_SMOB_DATA (cs);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	{
	  q[k] |= p[k] & r[k];
	  p[k] &= ~r[k];
	}
      rest = SCM_CDR (rest);
    }
  return scm_values (scm_list_2 (res1, res2));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_complement_x, "char-set-complement!", 1, 0, 0,
	    (SCM cs),
	    "Return the complement of the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_complement_x
{
  int k;
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  p = (long *) SCM_SMOB_DATA (cs);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    p[k] = ~p[k];
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_union_x, "char-set-union!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the union of all argument character sets.")
#define FUNC_NAME s_scm_char_set_union_x
{
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!SCM_NULLP (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] |= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_intersection_x, "char-set-intersection!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the intersection of all argument character sets.")
#define FUNC_NAME s_scm_char_set_intersection_x
{
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!SCM_NULLP (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] &= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_difference_x, "char-set-difference!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference of all argument character sets.")
#define FUNC_NAME s_scm_char_set_difference_x
{
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!SCM_NULLP (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] &= ~((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_xor_x, "char-set-xor!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the exclusive-or of all argument character sets.")
#define FUNC_NAME s_scm_char_set_xor_x
{
  /* a side-effecting variant should presumably give consistent results:
     (define a (char-set #\a))
     (char-set-xor a a a) -> char set #\a
     (char-set-xor! a a a) -> char set #\a
  */
  return scm_char_set_xor (scm_cons (cs1, rest));

#if 0
  /* this would give (char-set-xor! a a a) -> empty char set.  */
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!SCM_NULLP (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] ^= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_diff_plus_intersection_x, "char-set-diff+intersection!", 2, 0, 1,
	    (SCM cs1, SCM cs2, SCM rest),
	    "Return the difference and the intersection of all argument\n"
	    "character sets.")
#define FUNC_NAME s_scm_char_set_diff_plus_intersection_x
{
  int c = 3;
  long * p, * q;
  int k;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_SMOB (2, cs2, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  q = (long *) SCM_SMOB_DATA (cs2);
  if (p == q)
    {
      /* (char-set-diff+intersection! a a ...): can't share storage,
	 but we know the answer without checking for further
	 arguments.  */
      return scm_values (scm_list_2 (make_char_set (FUNC_NAME), cs1));
    }
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    {
      long t = p[k];

      p[k] &= ~q[k];
      q[k] = t & q[k];
    }
  while (!SCM_NULLP (rest))
    {
      SCM cs = SCM_CAR (rest);
      long *r;

      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      r = (long *) SCM_SMOB_DATA (cs);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	{
	  q[k] |= p[k] & r[k];
	  p[k] &= ~r[k];
	}
      rest = SCM_CDR (rest);
    }
  return scm_values (scm_list_2 (cs1, cs2));
}
#undef FUNC_NAME


/* Create the charset smob type.  */
void
scm_c_init_srfi_14 (void)
{
  /* Charset smob creation is protected by this variable because this
     function can be both called from the SRFI-13 and SRFI-14
     initialization functions.  This is because the SRFI-13 procedures
     access the charset smob type code.  */
  static int initialized = 0;

  if (!initialized)
    {
      scm_tc16_charset = scm_make_smob_type ("character-set",
					     BYTES_PER_CHARSET);
      scm_set_smob_free (scm_tc16_charset, charset_free);
      scm_set_smob_print (scm_tc16_charset, charset_print);
      initialized = 1;
    }
}


/* Initialize the SRFI-14 module.  This function will be called by the
   loading Scheme module.  */
void
scm_init_srfi_14 (void)
{
#if 0
  fprintf(stderr, "bytes-per-charset: %d\n", BYTES_PER_CHARSET);
  fprintf(stderr, "bits-per-long: %d\n", SCM_BITS_PER_LONG);
  fprintf(stderr, "longs-per-charset: %d\n", LONGS_PER_CHARSET);
  fflush (stderr);
#endif /* 0 */

  /* Do the smob type initialization.  */
  scm_c_init_srfi_14 ();

  /* Install the charset primitives.  */
#ifndef SCM_MAGIC_SNARFER
#include "srfi/srfi-14.x"
#endif
}

/* End of srfi-14.c.  */
