/* Copyright (C) 1995,1996,1998,2000,2001 Free Software Foundation, Inc.
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



#include <string.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/validate.h"


/* {Strings}
 */

SCM_DEFINE (scm_string_p, "string?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a string, else returns\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_string_p
{
  return SCM_BOOL (SCM_STRINGP (obj));
}
#undef FUNC_NAME

#if SCM_DEBUG_DEPRECATED == 0

/* The concept of read-only strings will disappear in next release
 * of Guile.
 */

SCM_DEFINE (scm_read_only_string_p, "read-only-string?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is either a string or a symbol,\n"
            "otherwise return @code{#f}.")
#define FUNC_NAME s_scm_read_only_string_p
{
  return SCM_BOOL(SCM_ROSTRINGP (obj));
}
#undef FUNC_NAME

#endif /* DEPRECATED */

SCM_REGISTER_PROC (s_scm_list_to_string, "list->string", 1, 0, 0, scm_string);

SCM_DEFINE (scm_string, "string", 0, 0, 1, 
            (SCM chrs),
	    "@deffnx primitive list->string chrs\n"
	    "Return a newly allocated string composed of the arguments,\n"
	    "@var{chrs}.")
#define FUNC_NAME s_scm_string
{
  SCM result;

  {
    scm_bits_t i = scm_ilength (chrs);

    SCM_ASSERT (i >= 0, chrs, SCM_ARGn, FUNC_NAME);
    result = scm_allocate_string (i);
  }

  {
    unsigned char *data = SCM_STRING_UCHARS (result);

    while (SCM_NNULLP (chrs))
      {
	SCM elt = SCM_CAR (chrs);

	SCM_VALIDATE_CHAR (SCM_ARGn, elt);
	*data++ = SCM_CHAR (elt);
	chrs = SCM_CDR (chrs);
      }
  }
  return result;
}
#undef FUNC_NAME

#if (SCM_DEBUG_DEPRECATED == 0)

SCM 
scm_makstr (size_t len, int dummy)
#define FUNC_NAME "scm_makstr"
{
  SCM s;
  char *mem;

  SCM_ASSERT_RANGE (1, scm_long2num (len), len <= SCM_STRING_MAX_LENGTH);

  mem = (char *) scm_must_malloc (len + 1, FUNC_NAME);
  mem[len] = 0;

  SCM_NEWCELL (s);
  SCM_SET_STRING_CHARS (s, mem);
  SCM_SET_STRING_LENGTH (s, len);

  return s;
}
#undef FUNC_NAME

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

/* converts C scm_array of strings to SCM scm_list of strings. */
/* If argc < 0, a null terminated scm_array is assumed. */

SCM 
scm_makfromstrs (int argc, char **argv)
{
  int i = argc;
  SCM lst = SCM_EOL;
  if (0 > i)
    for (i = 0; argv[i]; i++);
  while (i--)
    lst = scm_cons (scm_makfromstr (argv[i], (size_t) strlen (argv[i]), 0), lst);
  return lst;
}


/* This function must only be applied to memory obtained via malloc,
   since the GC is going to apply `free' to it when the string is
   dropped.

   Also, s[len] must be `\0', since we promise that strings are
   null-terminated.  Perhaps we could handle non-null-terminated
   strings by claiming they're shared substrings of a string we just
   made up.  */
SCM
scm_take_str (char *s, size_t len)
#define FUNC_NAME "scm_take_str"
{
  SCM answer;

  SCM_ASSERT_RANGE (2, scm_ulong2num (len), len <= SCM_STRING_MAX_LENGTH);

  SCM_NEWCELL (answer);
  SCM_SET_STRING_CHARS (answer, s);
  SCM_SET_STRING_LENGTH (answer, len);
  scm_done_malloc (len + 1);

  return answer;
}
#undef FUNC_NAME


/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  return scm_take_str (s, strlen (s));
}

SCM 
scm_makfromstr (const char *src, size_t len, int dummy)
{
  SCM s = scm_allocate_string (len);
  char *dst = SCM_STRING_CHARS (s);

  while (len--)
    *dst++ = *src++;
  return s;
}

SCM 
scm_makfrom0str (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_makfromstr (src, (size_t) strlen (src), 0);
}


SCM 
scm_makfrom0str_opt (const char *src)
{
  return scm_makfrom0str (src);
}


SCM
scm_allocate_string (size_t len)
#define FUNC_NAME "scm_allocate_string"
{
  char *mem;
  SCM s;

  SCM_ASSERT_RANGE (1, scm_long2num (len), len <= SCM_STRING_MAX_LENGTH);

  mem = (char *) scm_must_malloc (len + 1, FUNC_NAME);
  mem[len] = 0;

  SCM_NEWCELL (s);
  SCM_SET_STRING_CHARS (s, mem);
  SCM_SET_STRING_LENGTH (s, len);

  return s;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_string, "make-string", 1, 1, 0,
            (SCM k, SCM chr),
	    "Return a newly allocated string of\n"
            "length @var{k}.  If @var{chr} is given, then all elements of\n"
	    "the string are initialized to @var{chr}, otherwise the contents\n"
	    "of the @var{string} are unspecified.\n")
#define FUNC_NAME s_scm_make_string
{
  if (SCM_INUMP (k))
    {
      scm_bits_t i = SCM_INUM (k);
      SCM res;

      SCM_ASSERT_RANGE (1, k, i >= 0);

      res = scm_allocate_string (i);
      if (!SCM_UNBNDP (chr))
	{
	  unsigned char *dst;

	  SCM_VALIDATE_CHAR (2, chr);

	  dst = SCM_STRING_UCHARS (res);
	  memset (dst, SCM_CHAR (chr), i);
	}

      return res;
    }
  else if (SCM_BIGP (k))
    SCM_OUT_OF_RANGE (1, k);
  else
    SCM_WRONG_TYPE_ARG (1, k);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_length, "string-length", 1, 0, 0, 
	    (SCM string),
	    "Return the number of characters in @var{string}.")
#define FUNC_NAME s_scm_string_length
{
  SCM_VALIDATE_STRING (1, string);
  return SCM_MAKINUM (SCM_STRING_LENGTH (string));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ref, "string-ref", 2, 0, 0,
            (SCM str, SCM k),
	    "Return character @var{k} of @var{str} using zero-origin\n"
	    "indexing. @var{k} must be a valid index of @var{str}.")
#define FUNC_NAME s_scm_string_ref
{
  scm_bits_t idx;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_INUM_COPY (2, k, idx);
  SCM_ASSERT_RANGE (2, k, idx >= 0 && idx < SCM_STRING_LENGTH (str));
  return SCM_MAKE_CHAR (SCM_STRING_UCHARS (str)[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_set_x, "string-set!", 3, 0, 0,
            (SCM str, SCM k, SCM chr),
	    "Store @var{chr} in element @var{k} of @var{str} and return\n"
	    "an unspecified value. @var{k} must be a valid index of\n"
	    "@var{str}.")
#define FUNC_NAME s_scm_string_set_x
{
#if (SCM_DEBUG_DEPRECATED == 0)
  SCM_VALIDATE_RWSTRING (1, str);
#else
  SCM_VALIDATE_STRING (1, str);
#endif
  SCM_VALIDATE_INUM_RANGE (2,k,0,SCM_STRING_LENGTH(str));
  SCM_VALIDATE_CHAR (3,chr);
  SCM_STRING_UCHARS (str)[SCM_INUM (k)] = SCM_CHAR (chr);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_substring, "substring", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring
{
  scm_bits_t from;
  scm_bits_t to;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_INUM (2, start);
  SCM_VALIDATE_INUM_DEF (3, end, SCM_STRING_LENGTH (str));

  from = SCM_INUM (start);
  SCM_ASSERT_RANGE (2, start, 0 <= from && from <= SCM_STRING_LENGTH (str));
  to = SCM_INUM (end);
  SCM_ASSERT_RANGE (3, end, from <= to && to <= SCM_STRING_LENGTH (str));

  return scm_makfromstr (&SCM_STRING_CHARS (str)[from], (size_t) (to - from), 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_append, "string-append", 0, 0, 1, 
            (SCM args),
	    "Return a newly allocated string whose characters form the\n"
            "concatenation of the given strings, @var{args}.")
#define FUNC_NAME s_scm_string_append
{
  SCM res;
  size_t i = 0;
  register SCM l, s;
  register unsigned char *data;

  SCM_VALIDATE_REST_ARGUMENT (args);
  for (l = args; !SCM_NULLP (l); l = SCM_CDR (l)) {
    s = SCM_CAR (l);
    SCM_VALIDATE_STRING (SCM_ARGn,s);
    i += SCM_STRING_LENGTH (s);
  }
  res = scm_allocate_string (i);
  data = SCM_STRING_UCHARS (res);
  for (l = args;SCM_NIMP (l);l = SCM_CDR (l)) {
    s = SCM_CAR (l);
    for (i = 0;i<SCM_STRING_LENGTH (s);i++) *data++ = SCM_STRING_UCHARS (s)[i];
  }
  return res;
}
#undef FUNC_NAME

#if SCM_DEBUG_DEPRECATED == 0

/* Explicit shared substrings will disappear from Guile.
 *
 * Instead, "normal" strings will be implemented using sharing
 * internally, combined with a copy-on-write strategy.
 */

SCM_DEFINE (scm_make_shared_substring, "make-shared-substring", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a shared substring of @var{str}.  The arguments are the\n"
	    "same as for the @code{substring} function: the shared substring\n"
	    "returned includes all of the text from @var{str} between\n"
	    "indexes @var{start} (inclusive) and @var{end} (exclusive).  If\n"
	    "@var{end} is omitted, it defaults to the end of @var{str}.  The\n"
	    "shared substring returned by @code{make-shared-substring}\n"
	    "occupies the same storage space as @var{str}.")
#define FUNC_NAME s_scm_make_shared_substring
{
  scm_bits_t f;
  scm_bits_t t;
  SCM answer;
  SCM len_str;

  SCM_VALIDATE_ROSTRING (1,str);
  SCM_VALIDATE_INUM_DEF_COPY (2,start,0,f);
  SCM_VALIDATE_INUM_DEF_COPY (3,end,SCM_ROLENGTH(str),t);

  SCM_ASSERT_RANGE (2,start,(f >= 0));
  SCM_ASSERT_RANGE (3,end, (f <= t) && (t <= SCM_ROLENGTH (str)));

  SCM_NEWCELL (answer);
  SCM_NEWCELL (len_str);

  SCM_DEFER_INTS;
  if (SCM_SUBSTRP (str))
    {
      scm_bits_t offset;
      offset = SCM_INUM (SCM_SUBSTR_OFFSET (str));
      f += offset;
      t += offset;
      SCM_SETCAR (len_str, SCM_MAKINUM (f));
      SCM_SETCDR (len_str, SCM_SUBSTR_STR (str));
      SCM_SETCDR (answer, len_str);
      SCM_SETLENGTH (answer, t - f, scm_tc7_substring);
    }
  else
    {
      SCM_SETCAR (len_str, SCM_MAKINUM (f));
      SCM_SETCDR (len_str, str);
      SCM_SETCDR (answer, len_str);
      SCM_SETLENGTH (answer, t - f, scm_tc7_substring);
    }
  SCM_ALLOW_INTS;
  return answer;
}
#undef FUNC_NAME

#endif /* DEPRECATED */

void
scm_init_strings ()
{
  scm_nullstr = scm_allocate_string (0);

#ifndef SCM_MAGIC_SNARFER
#include "libguile/strings.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
