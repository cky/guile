/* Copyright (C) 1995,1996,1998,2000,2001 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */




#include <string.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"
#include "libguile/dynwind.h"



/* {Strings}
 */

SCM_DEFINE (scm_string_p, "string?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a string, else @code{#f}.")
#define FUNC_NAME s_scm_string_p
{
  return scm_from_bool (SCM_I_STRINGP (obj));
}
#undef FUNC_NAME


SCM_REGISTER_PROC (s_scm_list_to_string, "list->string", 1, 0, 0, scm_string);

SCM_DEFINE (scm_string, "string", 0, 0, 1, 
            (SCM chrs),
	    "@deffnx {Scheme Procedure} list->string chrs\n"
	    "Return a newly allocated string composed of the arguments,\n"
	    "@var{chrs}.")
#define FUNC_NAME s_scm_string
{
  SCM result;

  {
    long i = scm_ilength (chrs);

    SCM_ASSERT (i >= 0, chrs, SCM_ARG1, FUNC_NAME);
    result = scm_allocate_string (i);
  }

  {
    unsigned char *data = SCM_I_STRING_UCHARS (result);

    while (!SCM_NULLP (chrs))
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
    lst = scm_cons (scm_mem2string (argv[i], strlen (argv[i])), lst);
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

  SCM_ASSERT_RANGE (2, scm_from_ulong (len), len <= SCM_STRING_MAX_LENGTH);

  answer = scm_cell (SCM_I_MAKE_STRING_TAG (len), (scm_t_bits) s);
  scm_gc_register_collectable_memory (s, len+1, "string");

  return answer;
}
#undef FUNC_NAME


/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  return scm_take_locale_string (s);
}


SCM 
scm_mem2string (const char *src, size_t len)
{
  return scm_from_locale_stringn (src, len);
}


SCM
scm_str2string (const char *src)
{
  return scm_from_locale_string (src);
}


SCM 
scm_makfrom0str (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_from_locale_string (src);
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

  SCM_ASSERT_RANGE (1, scm_from_size_t (len), len <= SCM_STRING_MAX_LENGTH);

  mem = (char *) scm_gc_malloc (len + 1, "string");
  mem[len] = 0;

  s = scm_cell (SCM_I_MAKE_STRING_TAG (len), (scm_t_bits) mem);

  return s;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_string, "make-string", 1, 1, 0,
            (SCM k, SCM chr),
	    "Return a newly allocated string of\n"
            "length @var{k}.  If @var{chr} is given, then all elements of\n"
	    "the string are initialized to @var{chr}, otherwise the contents\n"
	    "of the @var{string} are unspecified.")
#define FUNC_NAME s_scm_make_string
{
  size_t i = scm_to_unsigned_integer (k, 0, SCM_STRING_MAX_LENGTH);
  SCM res = scm_allocate_string (i);

  if (!SCM_UNBNDP (chr))
    {
      unsigned char *dst;
      
      SCM_VALIDATE_CHAR (2, chr);
      
      dst = SCM_I_STRING_UCHARS (res);
      memset (dst, SCM_CHAR (chr), i);
    }

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_length, "string-length", 1, 0, 0, 
	    (SCM string),
	    "Return the number of characters in @var{string}.")
#define FUNC_NAME s_scm_string_length
{
  SCM_VALIDATE_STRING (1, string);
  return scm_from_size_t (SCM_I_STRING_LENGTH (string));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ref, "string-ref", 2, 0, 0,
            (SCM str, SCM k),
	    "Return character @var{k} of @var{str} using zero-origin\n"
	    "indexing. @var{k} must be a valid index of @var{str}.")
#define FUNC_NAME s_scm_string_ref
{
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);
  idx = scm_to_unsigned_integer (k, 0, SCM_I_STRING_LENGTH(str)-1);
  return SCM_MAKE_CHAR (SCM_I_STRING_UCHARS (str)[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_set_x, "string-set!", 3, 0, 0,
            (SCM str, SCM k, SCM chr),
	    "Store @var{chr} in element @var{k} of @var{str} and return\n"
	    "an unspecified value. @var{k} must be a valid index of\n"
	    "@var{str}.")
#define FUNC_NAME s_scm_string_set_x
{
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);
  idx = scm_to_unsigned_integer (k, 0, SCM_I_STRING_LENGTH(str)-1);
  SCM_VALIDATE_CHAR (3, chr);
  SCM_I_STRING_UCHARS (str)[idx] = SCM_CHAR (chr);
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
  unsigned long int from;
  unsigned long int to;
  SCM substr;

  SCM_VALIDATE_STRING (1, str);
  from = scm_to_unsigned_integer (start, 0, SCM_I_STRING_LENGTH(str));
  if (SCM_UNBNDP (end))
    to = SCM_I_STRING_LENGTH(str);
  else
    to = scm_to_unsigned_integer (end, from, SCM_I_STRING_LENGTH(str));
  substr = scm_allocate_string (to - from);
  memcpy (SCM_I_STRING_CHARS (substr), SCM_I_STRING_CHARS (str) + from,
	  to - from);
  scm_remember_upto_here_1 (str);
  return substr;
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
  SCM l, s;
  char *data;

  SCM_VALIDATE_REST_ARGUMENT (args);
  for (l = args; !SCM_NULLP (l); l = SCM_CDR (l)) 
    {
      s = SCM_CAR (l);
      SCM_VALIDATE_STRING (SCM_ARGn, s);
      i += SCM_I_STRING_LENGTH (s);
    }
  res = scm_allocate_string (i);
  data = SCM_I_STRING_CHARS (res);
  for (l = args; !SCM_NULLP (l); l = SCM_CDR (l)) 
    {
      s = SCM_CAR (l);
      memcpy (data, SCM_I_STRING_CHARS (s), SCM_I_STRING_LENGTH (s));
      data += SCM_I_STRING_LENGTH (s);
      scm_remember_upto_here_1 (s);
    }
  return res;
}
#undef FUNC_NAME

int
scm_is_string (SCM obj)
{
  return SCM_I_STRINGP (obj);
}

SCM
scm_from_locale_stringn (const char *str, size_t len)
{
  SCM res;
  char *dst;

  if (len == (size_t)-1)
    len = strlen (str);
  res = scm_allocate_string (len);
  dst = SCM_I_STRING_CHARS (res);
  memcpy (dst, str, len);
  return res;
}

SCM
scm_from_locale_string (const char *str)
{
  return scm_from_locale_stringn (str, -1);
}

SCM
scm_take_locale_stringn (char *str, size_t len)
{
  if (len == (size_t)-1)
    return scm_take_locale_string (str);
  else
    {
      /* STR might not be zero terminated and we are not allowed to
	 look at str[len], so we have to make a new one...
      */
      SCM res = scm_from_locale_stringn (str, len);
      free (str);
      return res;
    }
}

SCM
scm_take_locale_string (char *str)
{
  size_t len = strlen (str);
  SCM res;

  if (len > SCM_STRING_MAX_LENGTH)
    {
      free (str);
      scm_out_of_range (NULL, scm_from_size_t (len));
    }

  res = scm_cell (SCM_I_MAKE_STRING_TAG (len), (scm_t_bits) str);
  scm_gc_register_collectable_memory (str, len+1, "string");

  return res;
}

char *
scm_to_locale_stringn (SCM str, size_t *lenp)
{
  char *res;
  size_t len;

  if (!SCM_I_STRINGP (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  len = SCM_I_STRING_LENGTH (str);
  res = scm_malloc (len + ((lenp==NULL)? 1 : 0));
  memcpy (res, SCM_I_STRING_CHARS (str), len);
  if (lenp == NULL)
    {
      res[len] = '\0';
      if (strlen (res) != len)
	{
	  free (res);
	  scm_misc_error (NULL,
			  "string contains #\\nul character: ~S",
			  scm_list_1 (str));
	}
    }
  else
    *lenp = len;

  scm_remember_upto_here_1 (str);
  return res;
}

char *
scm_to_locale_string (SCM str)
{
  return scm_to_locale_stringn (str, NULL);
}

size_t
scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len)
{
  size_t len;
  
  if (!SCM_I_STRINGP (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  len = SCM_I_STRING_LENGTH (str);
  memcpy (buf, SCM_I_STRING_CHARS (str), (len > max_len)? max_len : len);
  scm_remember_upto_here_1 (str);
  return len;
}

/* Return a newly allocated array of char pointers to each of the strings
   in args, with a terminating NULL pointer.  */

char **
scm_i_allocate_string_pointers (SCM list)
{
  char **result;
  int len = scm_ilength (list);
  int i;

  if (len < 0)
    scm_wrong_type_arg_msg (NULL, 0, list, "proper list");

  scm_frame_begin (0);

  result = (char **) scm_malloc ((len + 1) * sizeof (char *));
  result[len] = NULL;
  scm_frame_unwind_handler (free, result, 0);

  /* The list might be have been modified in another thread, so
     we check LIST before each access.
   */
  for (i = 0; i < len && SCM_CONSP (list); i++)
    {
      result[i] = scm_to_locale_string (SCM_CAR (list));
      list = SCM_CDR (list);
    }

  scm_frame_end ();
  return result;
}

void
scm_i_free_string_pointers (char **pointers)
{
  int i;
  
  for (i = 0; pointers[i]; i++)
    free (pointers[i]);
  free (pointers);
}

void
scm_init_strings ()
{
  scm_nullstr = scm_allocate_string (0);

#include "libguile/strings.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
