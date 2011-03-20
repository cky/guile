/* srfi-13.c --- SRFI-13 procedures for Guile
 *
 * Copyright (C) 2001, 2004, 2005, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#include <string.h>
#include <unicase.h>
#include <unictype.h>

#include "libguile.h"

#include <libguile/deprecation.h>
#include "libguile/srfi-13.h"
#include "libguile/srfi-14.h"

#define MY_VALIDATE_SUBSTRING_SPEC(pos_str, str,              \
                                   pos_start, start, c_start, \
                                   pos_end, end, c_end)       \
  do {                                                        \
    SCM_VALIDATE_STRING (pos_str, str);                       \
    scm_i_get_substring_spec (scm_i_string_length (str),      \
			      start, &c_start, end, &c_end);  \
  } while (0)

#define MY_SUBF_VALIDATE_SUBSTRING_SPEC(fname, pos_str, str,            \
					pos_start, start, c_start,      \
					pos_end, end, c_end)            \
  do {                                                                  \
    SCM_ASSERT_TYPE (scm_is_string (str), str, pos_str, fname, "string"); \
    scm_i_get_substring_spec (scm_i_string_length (str),                \
			      start, &c_start, end, &c_end);            \
  } while (0)

#define REF_IN_CHARSET(s, i, cs)					\
  (scm_is_true (scm_char_set_contains_p ((cs), SCM_MAKE_CHAR (scm_i_string_ref (s, i)))))

SCM_DEFINE (scm_string_null_p, "string-null?", 1, 0, 0,
           (SCM str),
	    "Return @code{#t} if @var{str}'s length is zero, and\n"
	    "@code{#f} otherwise.\n"
	    "@lisp\n"
	    "(string-null? \"\")  @result{} #t\n"
	    "y                    @result{} \"foo\"\n"
	    "(string-null? y)     @result{} #f\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_null_p
{
  SCM_VALIDATE_STRING (1, str);
  return scm_from_bool (scm_i_string_length (str) == 0);
}
#undef FUNC_NAME

#if 0
static void
race_error ()
{
  scm_misc_error (NULL, "race condition detected", SCM_EOL);
}
#endif

SCM_DEFINE (scm_string_any, "string-any-c-code", 2, 2, 0,
            (SCM char_pred, SCM s, SCM start, SCM end),
"Check if @var{char_pred} is true for any character in string @var{s}.\n"
"\n"
"@var{char_pred} can be a character to check for any equal to that, or\n"
"a character set (@pxref{Character Sets}) to check for any in that set,\n"
"or a predicate procedure to call.\n"
"\n"
"For a procedure, calls @code{(@var{char_pred} c)} are made\n"
"successively on the characters from @var{start} to @var{end}.  If\n"
"@var{char_pred} returns true (ie.@: non-@code{#f}), @code{string-any}\n"
"stops and that return value is the return from @code{string-any}.  The\n"
"call on the last character (ie.@: at @math{@var{end}-1}), if that\n"
"point is reached, is a tail call.\n"
"\n"
"If there are no characters in @var{s} (ie.@: @var{start} equals\n"
"@var{end}) then the return is @code{#f}.\n")
#define FUNC_NAME s_scm_string_any
{
  size_t cstart, cend;
  SCM res = SCM_BOOL_F;

  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);

  if (SCM_CHARP (char_pred))
    {
      size_t i;
      for (i = cstart; i < cend; i ++)
	if (scm_i_string_ref (s, i) == SCM_CHAR (char_pred))
	  {
	    res = SCM_BOOL_T;
	    break;
	  }
    }
  else if (SCM_CHARSETP (char_pred))
    {
      size_t i;
      for (i = cstart; i < cend; i++)
        if (REF_IN_CHARSET (s, i, char_pred))
	  {
	    res = SCM_BOOL_T;
	    break;
	  }
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG1, FUNC_NAME);

      while (cstart < cend)
        {
          res = scm_call_1 (char_pred, 
                            SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
          if (scm_is_true (res))
            break;
          cstart++;
        }
    }

  scm_remember_upto_here_1 (s);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_every, "string-every-c-code", 2, 2, 0,
            (SCM char_pred, SCM s, SCM start, SCM end),
"Check if @var{char_pred} is true for every character in string\n"
"@var{s}.\n"
"\n"
"@var{char_pred} can be a character to check for every character equal\n"
"to that, or a character set (@pxref{Character Sets}) to check for\n"
"every character being in that set, or a predicate procedure to call.\n"
"\n"
"For a procedure, calls @code{(@var{char_pred} c)} are made\n"
"successively on the characters from @var{start} to @var{end}.  If\n"
"@var{char_pred} returns @code{#f}, @code{string-every} stops and\n"
"returns @code{#f}.  The call on the last character (ie.@: at\n"
"@math{@var{end}-1}), if that point is reached, is a tail call and the\n"
"return from that call is the return from @code{string-every}.\n"
"\n"
"If there are no characters in @var{s} (ie.@: @var{start} equals\n"
"@var{end}) then the return is @code{#t}.\n")
#define FUNC_NAME s_scm_string_every
{
  size_t cstart, cend;
  SCM res = SCM_BOOL_T;

  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      size_t i;
      for (i = cstart; i < cend; i++)
        if (scm_i_string_ref (s, i) != SCM_CHAR (char_pred))
	  {
	    res = SCM_BOOL_F;
	    break;
	  }
    }
  else if (SCM_CHARSETP (char_pred))
    {
      size_t i;
      for (i = cstart; i < cend; i++)
        if (!REF_IN_CHARSET (s, i, char_pred))
	  {
	    res = SCM_BOOL_F;
	    break;
	  }
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG1, FUNC_NAME);

      while (cstart < cend)
        {
          res = scm_call_1 (char_pred, 
                            SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
          if (scm_is_false (res))
            break;
          cstart++;
        }
    }

  scm_remember_upto_here_1 (s);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_tabulate, "string-tabulate", 2, 0, 0,
            (SCM proc, SCM len),
	    "@var{proc} is an integer->char procedure.  Construct a string\n"
	    "of size @var{len} by applying @var{proc} to each index to\n"
	    "produce the corresponding string element.  The order in which\n"
	    "@var{proc} is applied to the indices is not specified.")
#define FUNC_NAME s_scm_string_tabulate
{
  size_t clen, i;
  SCM res;
  SCM ch;

  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
              proc, SCM_ARG1, FUNC_NAME);

  SCM_ASSERT_RANGE (2, len, scm_to_int (len) >= 0);
  clen = scm_to_size_t (len);

  {
    /* This function is more complicated than necessary for the sake
       of speed.  */
    scm_t_wchar *buf = scm_malloc (clen * sizeof (scm_t_wchar));
    int wide = 0;
    i = 0; 
    while (i < clen)
      {
        ch = scm_call_1 (proc, scm_from_size_t (i));
        if (!SCM_CHARP (ch))
          {
            SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (proc));
          }
        if (SCM_CHAR (ch) > 255)
          wide = 1;
        buf[i] = SCM_CHAR (ch);
        i++;
      }
    if (wide)
      {
        scm_t_wchar *wbuf = NULL;
        res = scm_i_make_wide_string (clen, &wbuf, 0);
        memcpy (wbuf, buf, clen * sizeof (scm_t_wchar));
        free (buf);
      }
    else
      {
        char *nbuf = NULL;
        res = scm_i_make_string (clen, &nbuf, 0);
        for (i = 0; i < clen; i ++)
          nbuf[i] = (unsigned char) buf[i];
        free (buf);
      }
  }

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_substring_to_list, "string->list", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Convert the string @var{str} into a list of characters.")
#define FUNC_NAME s_scm_substring_to_list
{
  size_t cstart, cend;
  int narrow;
  SCM result = SCM_EOL;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);

  /* This explicit narrow/wide logic (instead of just using
     scm_i_string_ref) is for speed optimizaion.  */
  narrow = scm_i_is_narrow_string (str);
  if (narrow)
    {
      const char *buf = scm_i_string_chars (str);
      while (cstart < cend)
        {
          cend--;
          result = scm_cons (SCM_MAKE_CHAR (buf[cend]), result);
        }
    }
  else
    {
      const scm_t_wchar *buf = scm_i_string_wide_chars (str);
      while (cstart < cend)
        {
          cend--;
          result = scm_cons (SCM_MAKE_CHAR (buf[cend]), result);
        }
    }
  scm_remember_upto_here_1 (str);
  return result;
}
#undef FUNC_NAME

/* We export scm_substring_to_list as "string->list" since it is
   compatible and more general.  This function remains for the benefit
   of C code that used it.
*/

SCM
scm_string_to_list (SCM str)
{
  return scm_substring_to_list (str, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_reverse_list_to_string, "reverse-list->string", 1, 0, 0,
            (SCM chrs),
	    "An efficient implementation of @code{(compose string->list\n"
	    "reverse)}:\n"
	    "\n"
	    "@smalllisp\n"
	    "(reverse-list->string '(#\\a #\\B #\\c)) @result{} \"cBa\"\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_reverse_list_to_string
{
  SCM result;
  long i = scm_ilength (chrs), j;
  char *data;

  if (i < 0)
    SCM_WRONG_TYPE_ARG (1, chrs);
  result = scm_i_make_string (i, &data, 0);

  {
    SCM rest;
    rest = chrs;
    j = 0;
    while (j < i && scm_is_pair (rest))
      {
        SCM elt = SCM_CAR (rest);
        SCM_VALIDATE_CHAR (SCM_ARGn, elt);
        j++;
        rest = SCM_CDR (rest);
      }
    rest = chrs;
    j = i;
    result = scm_i_string_start_writing (result);
    while (j > 0 && scm_is_pair (rest))
      {
        SCM elt = SCM_CAR (rest);
        scm_i_string_set_x (result, j-1, SCM_CHAR (elt));
        rest = SCM_CDR (rest);
        j--;
      }
    scm_i_string_stop_writing ();
  }

  return result;
}
#undef FUNC_NAME


SCM_SYMBOL (scm_sym_infix, "infix");
SCM_SYMBOL (scm_sym_strict_infix, "strict-infix");
SCM_SYMBOL (scm_sym_suffix, "suffix");
SCM_SYMBOL (scm_sym_prefix, "prefix");

SCM_DEFINE (scm_string_join, "string-join", 1, 2, 0,
            (SCM ls, SCM delimiter, SCM grammar),
	    "Append the string in the string list @var{ls}, using the string\n"
	    "@var{delim} as a delimiter between the elements of @var{ls}.\n"
	    "@var{grammar} is a symbol which specifies how the delimiter is\n"
	    "placed between the strings, and defaults to the symbol\n"
	    "@code{infix}.\n"
	    "\n"
	    "@table @code\n"
	    "@item infix\n"
	    "Insert the separator between list elements.  An empty string\n"
	    "will produce an empty list.\n"
	    "@item string-infix\n"
	    "Like @code{infix}, but will raise an error if given the empty\n"
	    "list.\n"
	    "@item suffix\n"
	    "Insert the separator after every list element.\n"
	    "@item prefix\n"
	    "Insert the separator before each list element.\n"
	    "@end table")
#define FUNC_NAME s_scm_string_join
{
#define GRAM_INFIX        0
#define GRAM_STRICT_INFIX 1
#define GRAM_SUFFIX       2
#define GRAM_PREFIX       3
  SCM tmp;
  SCM result;
  int gram = GRAM_INFIX;
  size_t del_len = 0;
  long strings = scm_ilength (ls);

  /* Validate the string list.  */
  if (strings < 0)
    SCM_WRONG_TYPE_ARG (1, ls);

  /* Validate the delimiter and record its length.  */
  if (SCM_UNBNDP (delimiter))
    {
      delimiter = scm_from_locale_string (" ");
      del_len = 1;
    }
  else
    {
      SCM_VALIDATE_STRING (2, delimiter);
      del_len = scm_i_string_length (delimiter);
    }

  /* Validate the grammar symbol and remember the grammar.  */
  if (SCM_UNBNDP (grammar))
    gram = GRAM_INFIX;
  else if (scm_is_eq (grammar, scm_sym_infix))
    gram = GRAM_INFIX;
  else if (scm_is_eq (grammar, scm_sym_strict_infix))
    gram = GRAM_STRICT_INFIX;
  else if (scm_is_eq (grammar, scm_sym_suffix))
    gram = GRAM_SUFFIX;
  else if (scm_is_eq (grammar, scm_sym_prefix))
    gram = GRAM_PREFIX;
  else
    SCM_WRONG_TYPE_ARG (3, grammar);

  /* Check grammar constraints.  */
  if (strings == 0 && gram == GRAM_STRICT_INFIX)
    SCM_MISC_ERROR ("strict-infix grammar requires non-empty list",
		    SCM_EOL);

  result = scm_i_make_string (0, NULL, 0);

  tmp = ls;
  switch (gram)
    {
    case GRAM_INFIX:
    case GRAM_STRICT_INFIX:
      while (scm_is_pair (tmp))
	{
	  result = scm_string_append (scm_list_2 (result, SCM_CAR (tmp)));
	  if (!scm_is_null (SCM_CDR (tmp)) && del_len > 0)
	    result = scm_string_append (scm_list_2 (result, delimiter));
	  tmp = SCM_CDR (tmp);
	}
      break;
    case GRAM_SUFFIX:
      while (scm_is_pair (tmp))
	{
	  result = scm_string_append (scm_list_2 (result, SCM_CAR (tmp)));
	  if (del_len > 0)
	    result = scm_string_append (scm_list_2 (result, delimiter));
	  tmp = SCM_CDR (tmp);
	}
      break;
    case GRAM_PREFIX:
      while (scm_is_pair (tmp))
	{
	  if (del_len > 0)
	    result = scm_string_append (scm_list_2 (result, delimiter));
	  result = scm_string_append (scm_list_2 (result, SCM_CAR (tmp)));
	  tmp = SCM_CDR (tmp);
	}
      break;
    }

  return result;
#undef GRAM_INFIX
#undef GRAM_STRICT_INFIX
#undef GRAM_SUFFIX
#undef GRAM_PREFIX
}
#undef FUNC_NAME


/* There are a number of functions to consider here for Scheme and C:

   string-copy STR [start [end]]    ;; SRFI-13 variant of R5RS string-copy
   substring/copy STR start [end]   ;; Guile variant of R5RS substring

   scm_string_copy (str)            ;; Old function from Guile
   scm_substring_copy (str, [start, [end]])
                                    ;; C version of SRFI-13 string-copy
                                    ;; and C version of substring/copy

   The C function underlying string-copy is not exported to C
   programs.  scm_substring_copy is defined in strings.c as the
   underlying function of substring/copy and allows an optional START
   argument.
*/

SCM scm_srfi13_substring_copy (SCM str, SCM start, SCM end);

SCM_DEFINE (scm_srfi13_substring_copy, "string-copy", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a freshly allocated copy of the string @var{str}.  If\n"
	    "given, @var{start} and @var{end} delimit the portion of\n"
	    "@var{str} which is copied.")
#define FUNC_NAME s_scm_srfi13_substring_copy
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  return scm_i_substring_copy (str, cstart, cend);
}
#undef FUNC_NAME

SCM 
scm_string_copy (SCM str)
{
  if (!scm_is_string (str))
    scm_wrong_type_arg ("scm_string_copy", 0, str);

  return scm_i_substring (str, 0, scm_i_string_length (str));
}

SCM_DEFINE (scm_string_copy_x, "string-copy!", 3, 2, 0,
            (SCM target, SCM tstart, SCM s, SCM start, SCM end),
	    "Copy the sequence of characters from index range [@var{start},\n"
	    "@var{end}) in string @var{s} to string @var{target}, beginning\n"
	    "at index @var{tstart}.  The characters are copied left-to-right\n"
	    "or right-to-left as needed -- the copy is guaranteed to work,\n"
	    "even if @var{target} and @var{s} are the same string.  It is an\n"
	    "error if the copy operation runs off the end of the target\n"
	    "string.")
#define FUNC_NAME s_scm_string_copy_x
{
  size_t cstart, cend, ctstart, dummy, len, i;
  SCM sdummy = SCM_UNDEFINED;

  MY_VALIDATE_SUBSTRING_SPEC (1, target,
			      2, tstart, ctstart,
			      2, sdummy, dummy);
  MY_VALIDATE_SUBSTRING_SPEC (3, s,
			      4, start, cstart,
			      5, end, cend);
  len = cend - cstart;
  SCM_ASSERT_RANGE (3, s, len <= scm_i_string_length (target) - ctstart);

  target = scm_i_string_start_writing (target);
  for (i = 0; i < cend - cstart; i++)
    {
      scm_i_string_set_x (target, ctstart + i, 
                          scm_i_string_ref (s, cstart + i));
    }
  scm_i_string_stop_writing ();
  scm_remember_upto_here_1 (target);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_move_x, "substring-move!", 5, 0, 0,
	    (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2),
	    "Copy the substring of @var{str1} bounded by @var{start1} and @var{end1}\n"
	    "into @var{str2} beginning at position @var{start2}.\n"
	    "@var{str1} and @var{str2} can be the same string.")
#define FUNC_NAME s_scm_substring_move_x
{
  return scm_string_copy_x (str2, start2, str1, start1, end1);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_take, "string-take", 2, 0, 0,
            (SCM s, SCM n),
	    "Return the @var{n} first characters of @var{s}.")
#define FUNC_NAME s_scm_string_take
{
  return scm_substring (s, SCM_INUM0, n);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_drop, "string-drop", 2, 0, 0,
            (SCM s, SCM n),
	    "Return all but the first @var{n} characters of @var{s}.")
#define FUNC_NAME s_scm_string_drop
{
  return scm_substring (s, n, SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_take_right, "string-take-right", 2, 0, 0,
            (SCM s, SCM n),
	    "Return the @var{n} last characters of @var{s}.")
#define FUNC_NAME s_scm_string_take_right
{
  return scm_substring (s,
			scm_difference (scm_string_length (s), n),
			SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_drop_right, "string-drop-right", 2, 0, 0,
            (SCM s, SCM n),
	    "Return all but the last @var{n} characters of @var{s}.")
#define FUNC_NAME s_scm_string_drop_right
{
  return scm_substring (s,
			SCM_INUM0,
			scm_difference (scm_string_length (s), n));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_pad, "string-pad", 2, 3, 0,
	    (SCM s, SCM len, SCM chr, SCM start, SCM end),
	    "Take that characters from @var{start} to @var{end} from the\n"
	    "string @var{s} and return a new string, right-padded by the\n"
	    "character @var{chr} to length @var{len}.  If the resulting\n"
	    "string is longer than @var{len}, it is truncated on the right.")
#define FUNC_NAME s_scm_string_pad
{
  size_t cstart, cend, clen;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      4, start, cstart,
			      5, end, cend);
  clen = scm_to_size_t (len);

  if (SCM_UNBNDP (chr))
    chr = SCM_MAKE_CHAR (' ');
  else
    {
      SCM_VALIDATE_CHAR (3, chr);
    }
  if (clen < (cend - cstart))
    return scm_i_substring (s, cend - clen, cend);
  else
    {
      SCM result;
      result = (scm_string_append 
		(scm_list_2 (scm_c_make_string (clen - (cend - cstart), chr),
			     scm_i_substring (s, cstart, cend))));
      return result;
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_pad_right, "string-pad-right", 2, 3, 0,
	    (SCM s, SCM len, SCM chr, SCM start, SCM end),
	    "Take that characters from @var{start} to @var{end} from the\n"
	    "string @var{s} and return a new string, left-padded by the\n"
	    "character @var{chr} to length @var{len}.  If the resulting\n"
	    "string is longer than @var{len}, it is truncated on the left.")
#define FUNC_NAME s_scm_string_pad_right
{
  size_t cstart, cend, clen;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      4, start, cstart,
			      5, end, cend);
  clen = scm_to_size_t (len);

  if (SCM_UNBNDP (chr))
    chr = SCM_MAKE_CHAR (' ');
  else
    {
      SCM_VALIDATE_CHAR (3, chr);
    }
  if (clen < (cend - cstart))
    return scm_i_substring (s, cstart, cstart + clen);
  else
    {
      SCM result;

      result = (scm_string_append 
		(scm_list_2 (scm_i_substring (s, cstart, cend),
			     scm_c_make_string (clen - (cend - cstart), chr))));

      return result;
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_trim, "string-trim", 1, 3, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Trim @var{s} by skipping over all characters on the left\n"
	    "that satisfy the parameter @var{char_pred}:\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "if it is the character @var{ch}, characters equal to\n"
	    "@var{ch} are trimmed,\n"
	    "\n"
	    "@item\n"
	    "if it is a procedure @var{pred} characters that\n"
	    "satisfy @var{pred} are trimmed,\n"
	    "\n"
	    "@item\n"
	    "if it is a character set, characters in that set are trimmed.\n"
	    "@end itemize\n"
	    "\n"
	    "If called without a @var{char_pred} argument, all whitespace is\n"
	    "trimmed.")
#define FUNC_NAME s_scm_string_trim
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_UNBNDP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!uc_is_c_whitespace (scm_i_string_ref (s, cstart)))
	    break;
	  cstart++;
	}
    }
  else if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  if (scm_i_string_ref (s, cstart) != SCM_CHAR (char_pred))
	    break;
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!REF_IN_CHARSET (s, cstart, char_pred))
	    break;
	  cstart++;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;

	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
	  if (scm_is_false (res))
	    break;
	  cstart++;
	}
    }
  return scm_i_substring (s, cstart, cend);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_trim_right, "string-trim-right", 1, 3, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Trim @var{s} by skipping over all characters on the right\n"
	    "that satisfy the parameter @var{char_pred}:\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "if it is the character @var{ch}, characters equal to @var{ch}\n"
	    "are trimmed,\n"
	    "\n"
	    "@item\n"
	    "if it is a procedure @var{pred} characters that satisfy\n"
	    "@var{pred} are trimmed,\n"
	    "\n"
	    "@item\n"
	    "if it is a character sets, all characters in that set are\n"
	    "trimmed.\n"
	    "@end itemize\n"
	    "\n"
	    "If called without a @var{char_pred} argument, all whitespace is\n"
	    "trimmed.")
#define FUNC_NAME s_scm_string_trim_right
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_UNBNDP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!uc_is_c_whitespace (scm_i_string_ref (s, cend - 1)))
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  if (scm_i_string_ref (s, cend - 1) != SCM_CHAR (char_pred))
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!REF_IN_CHARSET (s, cend-1, char_pred))
	    break;
	  cend--;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;

	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cend - 1)));
	  if (scm_is_false (res))
	    break;
	  cend--;
	}
    }
  return scm_i_substring (s, cstart, cend);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_trim_both, "string-trim-both", 1, 3, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Trim @var{s} by skipping over all characters on both sides of\n"
	    "the string that satisfy the parameter @var{char_pred}:\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "if it is the character @var{ch}, characters equal to @var{ch}\n"
	    "are trimmed,\n"
	    "\n"
	    "@item\n"
	    "if it is a procedure @var{pred} characters that satisfy\n"
	    "@var{pred} are trimmed,\n"
	    "\n"
	    "@item\n"
	    "if it is a character set, the characters in the set are\n"
	    "trimmed.\n"
	    "@end itemize\n"
	    "\n"
	    "If called without a @var{char_pred} argument, all whitespace is\n"
	    "trimmed.")
#define FUNC_NAME s_scm_string_trim_both
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_UNBNDP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!uc_is_c_whitespace (scm_i_string_ref (s, cstart)))
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  if (!uc_is_c_whitespace (scm_i_string_ref (s, cend - 1)))
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  if (scm_i_string_ref (s, cstart) != SCM_CHAR(char_pred))
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  if (scm_i_string_ref (s, cend - 1) != SCM_CHAR (char_pred))
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!REF_IN_CHARSET (s, cstart, char_pred))
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  if (!REF_IN_CHARSET (s, cend-1, char_pred))
	    break;
	  cend--;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;

	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
	  if (scm_is_false (res))
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  SCM res;

	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cend - 1)));
	  if (scm_is_false (res))
	    break;
	  cend--;
	}
    }
  return scm_i_substring (s, cstart, cend);
}
#undef FUNC_NAME


SCM_DEFINE (scm_substring_fill_x, "string-fill!", 2, 2, 0,
	    (SCM str, SCM chr, SCM start, SCM end),
	    "Stores @var{chr} in every element of the given @var{str} and\n"
	    "returns an unspecified value.")
#define FUNC_NAME s_scm_substring_fill_x
{
  size_t cstart, cend;
  size_t k;

  /* Older versions of Guile provided the function
     scm_substring_fill_x with the following order of arguments:

         str, start, end, chr

     We accomodate this here by detecting such a usage and reordering
     the arguments.
  */
  if (SCM_CHARP (end))
    {
      SCM tmp = end;
      end = start;
      start = chr;
      chr = tmp;
    }

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      3, start, cstart,
			      4, end, cend);
  SCM_VALIDATE_CHAR (2, chr);


  str = scm_i_string_start_writing (str);
  for (k = cstart; k < cend; k++)
    scm_i_string_set_x (str, k, SCM_CHAR (chr));
  scm_i_string_stop_writing ();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_string_fill_x (SCM str, SCM chr)
{
  return scm_substring_fill_x (str, chr, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_string_compare, "string-compare", 5, 4, 0,
	    (SCM s1, SCM s2, SCM proc_lt, SCM proc_eq, SCM proc_gt, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Apply @var{proc_lt}, @var{proc_eq}, @var{proc_gt} to the\n"
	    "mismatch index, depending upon whether @var{s1} is less than,\n"
	    "equal to, or greater than @var{s2}.  The mismatch index is the\n"
	    "largest index @var{i} such that for every 0 <= @var{j} <\n"
	    "@var{i}, @var{s1}[@var{j}] = @var{s2}[@var{j}] -- that is,\n"
	    "@var{i} is the first position that does not match.")
#define FUNC_NAME s_scm_string_compare
{
  size_t cstart1, cend1, cstart2, cend2;
  SCM proc;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      6, start1, cstart1,
			      7, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      8, start2, cstart2,
			      9, end2, cend2);
  SCM_VALIDATE_PROC (3, proc_lt);
  SCM_VALIDATE_PROC (4, proc_eq);
  SCM_VALIDATE_PROC (5, proc_gt);

  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_i_string_ref (s1, cstart1)
	  < scm_i_string_ref (s2, cstart2))
	{
	  proc = proc_lt;
	  goto ret;
	}
      else if (scm_i_string_ref (s1, cstart1) 
	       > scm_i_string_ref (s2, cstart2))
	{
	  proc = proc_gt;
	  goto ret;
	}
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    proc = proc_gt;
  else if (cstart2 < cend2)
    proc = proc_lt;
  else
    proc = proc_eq;

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_call_1 (proc, scm_from_size_t (cstart1));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_compare_ci, "string-compare-ci", 5, 4, 0,
	    (SCM s1, SCM s2, SCM proc_lt, SCM proc_eq, SCM proc_gt, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Apply @var{proc_lt}, @var{proc_eq}, @var{proc_gt} to the\n"
	    "mismatch index, depending upon whether @var{s1} is less than,\n"
	    "equal to, or greater than @var{s2}.  The mismatch index is the\n"
	    "largest index @var{i} such that for every 0 <= @var{j} <\n"
	    "@var{i}, @var{s1}[@var{j}] = @var{s2}[@var{j}] -- that is,\n"
	    "@var{i} is the first position where the lowercased letters \n"
	    "do not match.\n")
#define FUNC_NAME s_scm_string_compare_ci
{
  size_t cstart1, cend1, cstart2, cend2;
  SCM proc;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      6, start1, cstart1,
			      7, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      8, start2, cstart2,
			      9, end2, cend2);
  SCM_VALIDATE_PROC (3, proc_lt);
  SCM_VALIDATE_PROC (4, proc_eq);
  SCM_VALIDATE_PROC (5, proc_gt);

  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (uc_tolower (uc_toupper (scm_i_string_ref (s1, cstart1)))
	  < uc_tolower (uc_toupper (scm_i_string_ref (s2, cstart2))))
	{
	  proc = proc_lt;
	  goto ret;
	}
      else if (uc_tolower (uc_toupper (scm_i_string_ref (s1, cstart1)))
	       > uc_tolower (uc_toupper (scm_i_string_ref (s2, cstart2))))
	{
	  proc = proc_gt;
	  goto ret;
	}
      cstart1++;
      cstart2++;
    }

  if (cstart1 < cend1)
    proc = proc_gt;
  else if (cstart2 < cend2)
    proc = proc_lt;
  else
    proc = proc_eq;

 ret:
  scm_remember_upto_here (s1, s2);
  return scm_call_1 (proc, scm_from_size_t (cstart1));
}
#undef FUNC_NAME

/* This function compares two substrings, S1 from START1 to END1 and
   S2 from START2 to END2, possibly case insensitively, and returns
   one of the parameters LESSTHAN, GREATERTHAN, SHORTER, LONGER, or
   EQUAL depending if S1 is less than S2, greater than S2, shorter,
   longer, or equal. */
static SCM
compare_strings (const char *fname, int case_insensitive,
		 SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2,
		 SCM lessthan, SCM greaterthan, SCM shorter, SCM longer, SCM equal)
{
  size_t cstart1, cend1, cstart2, cend2;
  SCM ret;
  scm_t_wchar a, b;

  MY_SUBF_VALIDATE_SUBSTRING_SPEC (fname, 1, s1,
				   3, start1, cstart1,
				   4, end1, cend1);
  MY_SUBF_VALIDATE_SUBSTRING_SPEC (fname, 2, s2,
				   5, start2, cstart2,
				   6, end2, cend2);

  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (case_insensitive)
	{
	  a = uc_tolower (uc_toupper (scm_i_string_ref (s1, cstart1)));
	  b = uc_tolower (uc_toupper (scm_i_string_ref (s2, cstart2)));
	}
      else
	{
	  a = scm_i_string_ref (s1, cstart1);
	  b = scm_i_string_ref (s2, cstart2);
	}
      if (a < b)
	{
	  ret = lessthan;
	  goto done;
	}
      else if (a > b)
	{
	  ret = greaterthan;
	  goto done;
	}
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    {
      ret = longer;
      goto done;
    }
  else if (cstart2 < cend2)
    {
      ret = shorter;
      goto done;
    }
  else
    {
      ret = equal;
      goto done;
    }

 done:
  scm_remember_upto_here_2 (s1, s2);
  return ret;
}


SCM_DEFINE (scm_string_eq, "string=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are not equal, a true\n"
	    "value otherwise.")
#define FUNC_NAME s_scm_string_eq
{
  if (SCM_LIKELY (scm_is_string (s1) && scm_is_string (s2) &&
		  scm_i_is_narrow_string (s1) == scm_i_is_narrow_string (s2)
		  && SCM_UNBNDP (start1) && SCM_UNBNDP (end1)
		  && SCM_UNBNDP (start2) && SCM_UNBNDP (end2)))
    {
      /* Fast path for this common case, which avoids the repeated calls to
	 `scm_i_string_ref'.  */
      size_t len1, len2;

      len1 = scm_i_string_length (s1);
      len2 = scm_i_string_length (s2);

      if (len1 != len2)
	return SCM_BOOL_F;
      else
	{
	  if (!scm_i_is_narrow_string (s1))
	    len1 *= 4;

	  return scm_from_bool (memcmp (scm_i_string_data (s1),
					scm_i_string_data (s2),
					len1) == 0);
	}
    }

  return compare_strings (FUNC_NAME, 0, 
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_neq, "string<>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are equal, a true\n"
	    "value otherwise.")
#define FUNC_NAME s_scm_string_neq
{
  return compare_strings (FUNC_NAME, 0,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_T, SCM_BOOL_T, SCM_BOOL_T, SCM_BOOL_T, SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_lt, "string<", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater or equal to @var{s2}, a\n"
	    "true value otherwise.")
#define FUNC_NAME s_scm_string_lt
{
  return compare_strings (FUNC_NAME, 0,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_gt, "string>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less or equal to @var{s2}, a\n"
	    "true value otherwise.")
#define FUNC_NAME s_scm_string_gt
{
  return compare_strings (FUNC_NAME, 0,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_le, "string<=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater to @var{s2}, a true\n"
	    "value otherwise.")
#define FUNC_NAME s_scm_string_le
{
  return compare_strings (FUNC_NAME, 0,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ge, "string>=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less to @var{s2}, a true value\n"
	    "otherwise.")
#define FUNC_NAME s_scm_string_ge
{
  return compare_strings (FUNC_NAME, 0,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_eq, "string-ci=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are not equal, a true\n"
	    "value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_eq
{
  return compare_strings (FUNC_NAME, 1,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_neq, "string-ci<>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are equal, a true\n"
	    "value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_neq
{
  return compare_strings (FUNC_NAME, 1,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_T, SCM_BOOL_T, SCM_BOOL_T, SCM_BOOL_T, SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_lt, "string-ci<", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater or equal to @var{s2}, a\n"
	    "true value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_lt
{
  return compare_strings (FUNC_NAME, 1,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_gt, "string-ci>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less or equal to @var{s2}, a\n"
	    "true value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_gt
{
  return compare_strings (FUNC_NAME, 1,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_le, "string-ci<=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater to @var{s2}, a true\n"
	    "value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_le
{
  return compare_strings (FUNC_NAME, 1,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_ge, "string-ci>=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less to @var{s2}, a true value\n"
	    "otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_ge
{
  return compare_strings (FUNC_NAME, 1,
			  s1, s2, start1, end1, start2, end2,
			  SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_F, SCM_BOOL_T, SCM_BOOL_T);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_hash, "string-hash", 1, 3, 0,
	    (SCM s, SCM bound, SCM start, SCM end),
	    "Compute a hash value for @var{S}.  the optional argument "
	    "@var{bound} is a non-negative exact "
            "integer specifying the range of the hash function. "
	    "A positive value restricts the return value to the "
	    "range [0,bound).")
#define FUNC_NAME s_scm_substring_hash
{
  if (SCM_UNBNDP (bound))
    bound = scm_from_intmax (SCM_MOST_POSITIVE_FIXNUM);
  if (SCM_UNBNDP (start))
    start = SCM_INUM0;
  return scm_hash (scm_substring_shared (s, start, end), bound);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_hash_ci, "string-hash-ci", 1, 3, 0,
	    (SCM s, SCM bound, SCM start, SCM end),
	    "Compute a hash value for @var{S}.  the optional argument "
	    "@var{bound} is a non-negative exact "
            "integer specifying the range of the hash function. "
	    "A positive value restricts the return value to the "
	    "range [0,bound).")
#define FUNC_NAME s_scm_substring_hash_ci
{
  return scm_substring_hash (scm_substring_downcase (s, start, end),
			     bound,
			     SCM_UNDEFINED, SCM_UNDEFINED);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_prefix_length, "string-prefix-length", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common prefix of the two\n"
	    "strings.")
#define FUNC_NAME s_scm_string_prefix_length
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_i_string_ref (s1, cstart1)
          != scm_i_string_ref (s2, cstart2))
	goto ret;
      len++;
      cstart1++;
      cstart2++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_size_t (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_prefix_length_ci, "string-prefix-length-ci", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common prefix of the two\n"
	    "strings, ignoring character case.")
#define FUNC_NAME s_scm_string_prefix_length_ci
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (uc_tolower (uc_toupper (scm_i_string_ref (s1, cstart1)))
	  != uc_tolower (uc_toupper (scm_i_string_ref (s2, cstart2))))
	goto ret;
      len++;
      cstart1++;
      cstart2++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_size_t (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_length, "string-suffix-length", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common suffix of the two\n"
	    "strings.")
#define FUNC_NAME s_scm_string_suffix_length
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (scm_i_string_ref (s1, cend1) 
	  != scm_i_string_ref (s2, cend2))
	goto ret;
      len++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_size_t (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_length_ci, "string-suffix-length-ci", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common suffix of the two\n"
	    "strings, ignoring character case.")
#define FUNC_NAME s_scm_string_suffix_length_ci
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (uc_tolower (uc_toupper (scm_i_string_ref (s1, cend1)))
	  != uc_tolower (uc_toupper (scm_i_string_ref (s2, cend2))))
	goto ret;
      len++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_size_t (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_prefix_p, "string-prefix?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a prefix of @var{s2}?")
#define FUNC_NAME s_scm_string_prefix_p
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0, len1;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_i_string_ref (s1, cstart1)
          != scm_i_string_ref (s2, cstart2))
	goto ret;
      len++;
      cstart1++;
      cstart2++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_bool (len == len1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_prefix_ci_p, "string-prefix-ci?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a prefix of @var{s2}, ignoring character case?")
#define FUNC_NAME s_scm_string_prefix_ci_p
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0, len1;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      scm_t_wchar a = uc_tolower (uc_toupper (scm_i_string_ref (s1, cstart1)));
      scm_t_wchar b = uc_tolower (uc_toupper (scm_i_string_ref (s2, cstart2)));
      if (a != b)
	goto ret;
      len++;
      cstart1++;
      cstart2++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_bool (len == len1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_p, "string-suffix?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a suffix of @var{s2}?")
#define FUNC_NAME s_scm_string_suffix_p
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0, len1;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (scm_i_string_ref (s1, cend1) 
	  != scm_i_string_ref (s2, cend2))
	goto ret;
      len++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_bool (len == len1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_ci_p, "string-suffix-ci?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a suffix of @var{s2}, ignoring character case?")
#define FUNC_NAME s_scm_string_suffix_ci_p
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len = 0, len1;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (uc_tolower (uc_toupper (scm_i_string_ref (s1, cend1)))
	  != uc_tolower (uc_toupper (scm_i_string_ref (s2, cend2))))
	goto ret;
      len++;
    }

 ret:
  scm_remember_upto_here_2 (s1, s2);
  return scm_from_bool (len == len1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_index, "string-index", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from left to right, returning\n"
	    "the index of the first occurrence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "equals @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "satisfies the predicate @var{char_pred}, if it is a procedure,\n"
	    "\n"
	    "@item\n"
	    "is in the set @var{char_pred}, if it is a character set.\n"
	    "@end itemize\n\n"
	    "Return @code{#f} if no match is found.")
#define FUNC_NAME s_scm_string_index
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  if (scm_i_string_ref (s, cstart) == SCM_CHAR (char_pred))
	    goto found;
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (REF_IN_CHARSET (s, cstart, char_pred))
	    goto found;
	  cstart++;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;
	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
	  if (scm_is_true (res))
	    goto found;
	  cstart++;
	}
    }
  
  scm_remember_upto_here_1 (s);
  return SCM_BOOL_F;
  
 found:
  scm_remember_upto_here_1 (s);
  return scm_from_size_t (cstart);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_index_right, "string-index-right", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from right to left, returning\n"
	    "the index of the last occurrence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "equals @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "satisfies the predicate @var{char_pred}, if it is a procedure,\n"
	    "\n"
	    "@item\n"
	    "is in the set if @var{char_pred} is a character set.\n"
	    "@end itemize\n\n"
	    "Return @code{#f} if no match is found.")
#define FUNC_NAME s_scm_string_index_right
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  cend--;
	  if (scm_i_string_ref (s, cend) == SCM_CHAR (char_pred))
	    goto found;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  cend--;
	  if (REF_IN_CHARSET (s, cend, char_pred))
	    goto found;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;
	  cend--;
	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cend)));
	  if (scm_is_true (res))
	    goto found;
	}
    }

  scm_remember_upto_here_1 (s);
  return SCM_BOOL_F;

 found:
  scm_remember_upto_here_1 (s);
  return scm_from_size_t (cend);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_rindex, "string-rindex", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from right to left, returning\n"
	    "the index of the last occurrence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "equals @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "satisfies the predicate @var{char_pred}, if it is a procedure,\n"
	    "\n"
	    "@item\n"
	    "is in the set if @var{char_pred} is a character set.\n"
	    "@end itemize\n\n"
	    "Return @code{#f} if no match is found.")
#define FUNC_NAME s_scm_string_rindex
{
  return scm_string_index_right (s, char_pred, start, end);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_skip, "string-skip", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from left to right, returning\n"
	    "the index of the first occurrence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "does not equal @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "does not satisfy the predicate @var{char_pred}, if it is a\n"
	    "procedure,\n"
	    "\n"
	    "@item\n"
	    "is not in the set if @var{char_pred} is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_skip
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  if (scm_i_string_ref (s, cstart) !=  SCM_CHAR (char_pred))
	    goto found;
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!REF_IN_CHARSET (s, cstart, char_pred))
	    goto found;
	  cstart++;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;
	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
	  if (scm_is_false (res))
	    goto found;
	  cstart++;
	}
    }

  scm_remember_upto_here_1 (s);
  return SCM_BOOL_F;

 found:
  scm_remember_upto_here_1 (s);
  return scm_from_size_t (cstart);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_skip_right, "string-skip-right", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from right to left, returning\n"
	    "the index of the last occurrence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "does not equal @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "does not satisfy the predicate @var{char_pred}, if it is a\n"
	    "procedure,\n"
	    "\n"
	    "@item\n"
	    "is not in the set if @var{char_pred} is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_skip_right
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  cend--;
	  if (scm_i_string_ref (s, cend) != SCM_CHAR (char_pred))
	    goto found;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  cend--;
	  if (!REF_IN_CHARSET (s, cend, char_pred))
	    goto found;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;
	  cend--;
	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cend)));
	  if (scm_is_false (res))
	    goto found;
	}
    }

  scm_remember_upto_here_1 (s);
  return SCM_BOOL_F;

 found:
  scm_remember_upto_here_1 (s);
  return scm_from_size_t (cend);

}
#undef FUNC_NAME


SCM_DEFINE (scm_string_count, "string-count", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Return the count of the number of characters in the string\n"
	    "@var{s} which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "equals @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "satisfies the predicate @var{char_pred}, if it is a procedure.\n"
	    "\n"
	    "@item\n"
	    "is in the set @var{char_pred}, if it is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_count
{
  size_t cstart, cend;
  size_t count = 0;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      while (cstart < cend)
	{
	  if (scm_i_string_ref (s, cstart) == SCM_CHAR(char_pred))
	    count++;
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (REF_IN_CHARSET (s, cstart, char_pred))
	    count++;
	  cstart++;
	}
    }
  else
    {
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG2, FUNC_NAME);

      while (cstart < cend)
	{
	  SCM res;
	  res = scm_call_1 (char_pred, SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
	  if (scm_is_true (res))
	    count++;
	  cstart++;
	}
    }

  scm_remember_upto_here_1 (s);
  return scm_from_size_t (count);
}
#undef FUNC_NAME


/* FIXME::martin: This should definitely get implemented more
   efficiently -- maybe with Knuth-Morris-Pratt, like in the reference
   implementation.  */
SCM_DEFINE (scm_string_contains, "string-contains", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Does string @var{s1} contain string @var{s2}?  Return the index\n"
	    "in @var{s1} where @var{s2} occurs as a substring, or false.\n"
	    "The optional start/end indices restrict the operation to the\n"
	    "indicated substrings.")
#define FUNC_NAME s_scm_string_contains
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len2, i, j;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  len2 = cend2 - cstart2;
  if (cend1 - cstart1 >= len2)
    while (cstart1 <= cend1 - len2)
      {
	i = cstart1;
	j = cstart2;
	while (i < cend1 
	       && j < cend2 
	       && (scm_i_string_ref (s1, i)
		   == scm_i_string_ref (s2, j)))
	  {
	    i++;
	    j++;
	  }
	if (j == cend2)
	  {
	    scm_remember_upto_here_2 (s1, s2);
	    return scm_from_size_t (cstart1);
	  }
	cstart1++;
      }

  scm_remember_upto_here_2 (s1, s2);
  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* FIXME::martin: This should definitely get implemented more
   efficiently -- maybe with Knuth-Morris-Pratt, like in the reference
   implementation.  */
SCM_DEFINE (scm_string_contains_ci, "string-contains-ci", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Does string @var{s1} contain string @var{s2}?  Return the index\n"
	    "in @var{s1} where @var{s2} occurs as a substring, or false.\n"
	    "The optional start/end indices restrict the operation to the\n"
	    "indicated substrings.  Character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_contains_ci
{
  size_t cstart1, cend1, cstart2, cend2;
  size_t len2, i, j;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  len2 = cend2 - cstart2;
  if (cend1 - cstart1 >= len2)
    while (cstart1 <= cend1 - len2)
      {
	i = cstart1;
	j = cstart2;
	while (i < cend1 
	       && j < cend2 
	       && (uc_tolower (uc_toupper (scm_i_string_ref (s1, i)))
		   == uc_tolower (uc_toupper (scm_i_string_ref (s2, j)))))
	  {
	    i++;
	    j++;
	  }
	if (j == cend2)
	  {
	    scm_remember_upto_here_2 (s1, s2);
	    return scm_from_size_t (cstart1);
	  }
	cstart1++;
      }
  
  scm_remember_upto_here_2 (s1, s2);
  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* Helper function for the string uppercase conversion functions. */
static SCM
string_upcase_x (SCM v, size_t start, size_t end)
{
  size_t k;

  v = scm_i_string_start_writing (v);
  for (k = start; k < end; ++k)
    scm_i_string_set_x (v, k, uc_toupper (scm_i_string_ref (v, k)));
  scm_i_string_stop_writing ();
  scm_remember_upto_here_1 (v);

  return v;
}

SCM_DEFINE (scm_substring_upcase_x, "string-upcase!", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Destructively upcase every character in @code{str}.\n"
	    "\n"
	    "@lisp\n"
	    "(string-upcase! y)\n"
	    "@result{} \"ARRDEFG\"\n"
	    "y\n"
	    "@result{} \"ARRDEFG\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_substring_upcase_x
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  return string_upcase_x (str, cstart, cend);
}
#undef FUNC_NAME

SCM
scm_string_upcase_x (SCM str)
{
  return scm_substring_upcase_x (str, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_substring_upcase, "string-upcase", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Upcase every character in @code{str}.")
#define FUNC_NAME s_scm_substring_upcase
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  return string_upcase_x (scm_string_copy (str), cstart, cend);
}
#undef FUNC_NAME

SCM
scm_string_upcase (SCM str)
{
  return scm_substring_upcase (str, SCM_UNDEFINED, SCM_UNDEFINED);
}

/* Helper function for the string lowercase conversion functions.
 * No argument checking is performed.  */
static SCM
string_downcase_x (SCM v, size_t start, size_t end)
{
  size_t k;

  v = scm_i_string_start_writing (v);
  for (k = start; k < end; ++k)
    scm_i_string_set_x (v, k, uc_tolower (scm_i_string_ref (v, k)));
  scm_i_string_stop_writing ();
  scm_remember_upto_here_1 (v);

  return v;
}

SCM_DEFINE (scm_substring_downcase_x, "string-downcase!", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Destructively downcase every character in @var{str}.\n"
	    "\n"
	    "@lisp\n"
	    "y\n"
	    "@result{} \"ARRDEFG\"\n"
	    "(string-downcase! y)\n"
	    "@result{} \"arrdefg\"\n"
	    "y\n"
	    "@result{} \"arrdefg\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_substring_downcase_x
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  return string_downcase_x (str, cstart, cend);
}
#undef FUNC_NAME

SCM
scm_string_downcase_x (SCM str)
{
  return scm_substring_downcase_x (str, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_substring_downcase, "string-downcase", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Downcase every character in @var{str}.")
#define FUNC_NAME s_scm_substring_downcase
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  return string_downcase_x (scm_string_copy (str), cstart, cend);
}
#undef FUNC_NAME

SCM
scm_string_downcase (SCM str)
{
  return scm_substring_downcase (str, SCM_UNDEFINED, SCM_UNDEFINED);
}

/* Helper function for the string capitalization functions.
 * No argument checking is performed.  */
static SCM
string_titlecase_x (SCM str, size_t start, size_t end)
{
  SCM ch;
  size_t i;
  int in_word = 0;

  str = scm_i_string_start_writing (str);
  for(i = start; i < end;  i++)
    {
      ch = SCM_MAKE_CHAR (scm_i_string_ref (str, i));
      if (scm_is_true (scm_char_alphabetic_p (ch)))
	{
	  if (!in_word)
	    {
	      scm_i_string_set_x (str, i, uc_totitle (SCM_CHAR (ch)));
	      in_word = 1;
	    }
	  else
	    {
	      scm_i_string_set_x (str, i, uc_tolower (SCM_CHAR (ch)));
	    }
	}
      else
	in_word = 0;
    }
  scm_i_string_stop_writing ();
  scm_remember_upto_here_1 (str);

  return str;
}


SCM_DEFINE (scm_string_titlecase_x, "string-titlecase!", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Destructively titlecase every first character in a word in\n"
	    "@var{str}.")
#define FUNC_NAME s_scm_string_titlecase_x
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  return string_titlecase_x (str, cstart, cend);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_titlecase, "string-titlecase", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Titlecase every first character in a word in @var{str}.")
#define FUNC_NAME s_scm_string_titlecase
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  return string_titlecase_x (scm_string_copy (str), cstart, cend);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_capitalize_x, "string-capitalize!", 1, 0, 0,
	    (SCM str),
	    "Upcase the first character of every word in @var{str}\n"
	    "destructively and return @var{str}.\n"
	    "\n"
	    "@lisp\n"
	    "y                      @result{} \"hello world\"\n"
	    "(string-capitalize! y) @result{} \"Hello World\"\n"
	    "y                      @result{} \"Hello World\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_capitalize_x
{
  return scm_string_titlecase_x (str, SCM_UNDEFINED, SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_capitalize, "string-capitalize", 1, 0, 0,
	    (SCM str),
	    "Return a freshly allocated string with the characters in\n"
	    "@var{str}, where the first character of every word is\n"
	    "capitalized.")
#define FUNC_NAME s_scm_string_capitalize
{
  return scm_string_capitalize_x (scm_string_copy (str));
}
#undef FUNC_NAME


/* Reverse the portion of @var{str} between str[cstart] (including)
   and str[cend] excluding.  */
static void
string_reverse_x (SCM str, size_t cstart, size_t cend)
{
  SCM tmp;

  str = scm_i_string_start_writing (str);
  if (cend > 0)
    {
      cend--;
      while (cstart < cend)
	{
	  tmp = SCM_MAKE_CHAR (scm_i_string_ref (str, cstart));
	  scm_i_string_set_x (str, cstart, scm_i_string_ref (str, cend));
	  scm_i_string_set_x (str, cend, SCM_CHAR (tmp));
	  cstart++;
	  cend--;
	}
    }
  scm_i_string_stop_writing ();
}


SCM_DEFINE (scm_string_reverse, "string-reverse", 1, 2, 0,
            (SCM str, SCM start, SCM end),
	    "Reverse the string @var{str}.  The optional arguments\n"
	    "@var{start} and @var{end} delimit the region of @var{str} to\n"
	    "operate on.")
#define FUNC_NAME s_scm_string_reverse
{
  size_t cstart, cend;
  SCM result;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);
  result = scm_string_copy (str);
  string_reverse_x (result, cstart, cend);
  scm_remember_upto_here_1 (str);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_reverse_x, "string-reverse!", 1, 2, 0,
            (SCM str, SCM start, SCM end),
	    "Reverse the string @var{str} in-place.  The optional arguments\n"
	    "@var{start} and @var{end} delimit the region of @var{str} to\n"
	    "operate on.  The return value is unspecified.")
#define FUNC_NAME s_scm_string_reverse_x
{
  size_t cstart, cend;

  MY_VALIDATE_SUBSTRING_SPEC (1, str,
			      2, start, cstart,
			      3, end, cend);

  string_reverse_x (str, cstart, cend);
  scm_remember_upto_here_1 (str);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_append_shared, "string-append/shared", 0, 0, 1,
            (SCM rest),
	    "Like @code{string-append}, but the result may share memory\n"
	    "with the argument strings.")
#define FUNC_NAME s_scm_string_append_shared
{
  /* If "rest" contains just one non-empty string, return that.
     If it's entirely empty strings, then return scm_nullstr.
     Otherwise use scm_string_concatenate.  */

  SCM ret = scm_nullstr;
  int seen_nonempty = 0;
  SCM l, s;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  for (l = rest; scm_is_pair (l); l = SCM_CDR (l))
    {
      s = SCM_CAR (l);
      if (!scm_is_string (s))
	scm_wrong_type_arg (FUNC_NAME, 0, s);
      if (scm_i_string_length (s) != 0)
        {
          if (seen_nonempty)
            /* two or more non-empty strings, need full concat */
            return scm_string_append (rest);

          seen_nonempty = 1;
          ret = s;
        }
    }
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_concatenate, "string-concatenate", 1, 0, 0,
            (SCM ls),
	    "Append the elements of @var{ls} (which must be strings)\n"
	    "together into a single string.  Guaranteed to return a freshly\n"
	    "allocated string.")
#define FUNC_NAME s_scm_string_concatenate
{
  SCM_VALIDATE_LIST (SCM_ARG1, ls);
  return scm_string_append (ls);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_concatenate_reverse, "string-concatenate-reverse", 1, 2, 0,
            (SCM ls, SCM final_string, SCM end),
	    "Without optional arguments, this procedure is equivalent to\n"
	    "\n"
	    "@smalllisp\n"
	    "(string-concatenate (reverse ls))\n"
	    "@end smalllisp\n"
	    "\n"
	    "If the optional argument @var{final_string} is specified, it is\n"
	    "consed onto the beginning to @var{ls} before performing the\n"
	    "list-reverse and string-concatenate operations.  If @var{end}\n"
	    "is given, only the characters of @var{final_string} up to index\n"
	    "@var{end} are used.\n"
	    "\n"
	    "Guaranteed to return a freshly allocated string.")
#define FUNC_NAME s_scm_string_concatenate_reverse
{
  if (!SCM_UNBNDP (end))
    final_string = scm_substring (final_string, SCM_INUM0, end);

  if (!SCM_UNBNDP (final_string))
    ls = scm_cons (final_string, ls);

  return scm_string_concatenate (scm_reverse (ls));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_concatenate_shared, "string-concatenate/shared", 1, 0, 0,
            (SCM ls),
	    "Like @code{string-concatenate}, but the result may share memory\n"
	    "with the strings in the list @var{ls}.")
#define FUNC_NAME s_scm_string_concatenate_shared
{
  SCM_VALIDATE_LIST (SCM_ARG1, ls);
  return scm_string_append_shared (ls);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_concatenate_reverse_shared, "string-concatenate-reverse/shared", 1, 2, 0,
            (SCM ls, SCM final_string, SCM end),
	    "Like @code{string-concatenate-reverse}, but the result may\n"
	    "share memory with the strings in the @var{ls} arguments.")
#define FUNC_NAME s_scm_string_concatenate_reverse_shared
{
  /* Just call the non-sharing version.  */
  return scm_string_concatenate_reverse (ls, final_string, end);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_map, "string-map", 2, 2, 0,
	    (SCM proc, SCM s, SCM start, SCM end),
	    "@var{proc} is a char->char procedure, it is mapped over\n"
	    "@var{s}.  The order in which the procedure is applied to the\n"
	    "string elements is not specified.")
#define FUNC_NAME s_scm_string_map
{
  size_t p;
  size_t cstart, cend;
  SCM result;

  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
              proc, SCM_ARG1, FUNC_NAME);
  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);
  result = scm_i_make_string (cend - cstart, NULL, 0);
  p = 0;
  while (cstart < cend)
    {
      SCM ch = scm_call_1 (proc, scm_c_string_ref (s, cstart));
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (proc));
      cstart++;
      result = scm_i_string_start_writing (result);
      scm_i_string_set_x (result, p, SCM_CHAR (ch));
      scm_i_string_stop_writing ();
      p++;
    }
  
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_map_x, "string-map!", 2, 2, 0,
	    (SCM proc, SCM s, SCM start, SCM end),
	    "@var{proc} is a char->char procedure, it is mapped over\n"
	    "@var{s}.  The order in which the procedure is applied to the\n"
	    "string elements is not specified.  The string @var{s} is\n"
	    "modified in-place, the return value is not specified.")
#define FUNC_NAME s_scm_string_map_x
{
  size_t cstart, cend;

  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
              proc, SCM_ARG1, FUNC_NAME);
  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);
  while (cstart < cend)
    {
      SCM ch = scm_call_1 (proc, scm_c_string_ref (s, cstart));
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (proc));
      s = scm_i_string_start_writing (s);
      scm_i_string_set_x (s, cstart, SCM_CHAR (ch));
      scm_i_string_stop_writing ();
      cstart++;
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_fold, "string-fold", 3, 2, 0,
	    (SCM kons, SCM knil, SCM s, SCM start, SCM end),
	    "Fold @var{kons} over the characters of @var{s}, with @var{knil}\n"
	    "as the terminating element, from left to right.  @var{kons}\n"
	    "must expect two arguments: The actual character and the last\n"
	    "result of @var{kons}' application.")
#define FUNC_NAME s_scm_string_fold
{
  size_t cstart, cend;
  SCM result;

  SCM_VALIDATE_PROC (1, kons);
  MY_VALIDATE_SUBSTRING_SPEC (3, s,
			      4, start, cstart,
			      5, end, cend);
  result = knil;
  while (cstart < cend)
    {
      result = scm_call_2 (kons, SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)), result);
      cstart++;
    }

  scm_remember_upto_here_1 (s);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_fold_right, "string-fold-right", 3, 2, 0,
	    (SCM kons, SCM knil, SCM s, SCM start, SCM end),
	    "Fold @var{kons} over the characters of @var{s}, with @var{knil}\n"
	    "as the terminating element, from right to left.  @var{kons}\n"
	    "must expect two arguments: The actual character and the last\n"
	    "result of @var{kons}' application.")
#define FUNC_NAME s_scm_string_fold_right
{
  size_t cstart, cend;
  SCM result;

  SCM_VALIDATE_PROC (1, kons);
  MY_VALIDATE_SUBSTRING_SPEC (3, s,
			      4, start, cstart,
			      5, end, cend);
  result = knil;
  while (cstart < cend)
    {
      result = scm_call_2 (kons, SCM_MAKE_CHAR (scm_i_string_ref (s, cend-1)), result);
      cend--;
    }

  scm_remember_upto_here_1 (s);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_unfold, "string-unfold", 4, 2, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base, SCM make_final),
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of @emph{seed}\n"
	    "values from the initial @var{seed}: @var{seed}, (@var{g}\n"
	    "@var{seed}), (@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}),\n"
	    "@dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of these seed values.\n"
	    "@item @var{f} maps each seed value to the corresponding\n"
	    "character in the result string.  These chars are assembled\n"
	    "into the string in a left-to-right order.\n"
	    "@item @var{base} is the optional initial/leftmost portion\n"
	    "of the constructed string; it default to the empty\n"
	    "string.\n"
	    "@item @var{make_final} is applied to the terminal seed\n"
	    "value (on which @var{p} returns true) to produce\n"
	    "the final/rightmost portion of the constructed string.\n"
	    "It defaults to @code{(lambda (x) "")}.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_unfold
{
  SCM res, ans;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  if (!SCM_UNBNDP (base))
    {
      SCM_VALIDATE_STRING (5, base);
      ans = base;
    }
  else
    ans = scm_i_make_string (0, NULL, 0);
  if (!SCM_UNBNDP (make_final))
    SCM_VALIDATE_PROC (6, make_final);

  res = scm_call_1 (p, seed);
  while (scm_is_false (res))
    {
      SCM str;
      size_t i = 0;
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      str = scm_i_make_string (1, NULL, 0);
      str = scm_i_string_start_writing (str);
      scm_i_string_set_x (str, i, SCM_CHAR (ch));
      scm_i_string_stop_writing ();
      i++;

      ans = scm_string_append (scm_list_2 (ans, str));
      seed = scm_call_1 (g, seed);
      res = scm_call_1 (p, seed);
    }
  if (!SCM_UNBNDP (make_final))
    {
      res = scm_call_1 (make_final, seed);
      return scm_string_append (scm_list_2 (ans, res));
    }
  else
    return ans;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_unfold_right, "string-unfold-right", 4, 2, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base, SCM make_final),
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of @emph{seed}\n"
	    "values from the initial @var{seed}: @var{seed}, (@var{g}\n"
	    "@var{seed}), (@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}),\n"
	    "@dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of these seed values.\n"
	    "@item @var{f} maps each seed value to the corresponding\n"
	    "character in the result string.  These chars are assembled\n"
	    "into the string in a right-to-left order.\n"
	    "@item @var{base} is the optional initial/rightmost portion\n"
	    "of the constructed string; it default to the empty\n"
	    "string.\n"
	    "@item @var{make_final} is applied to the terminal seed\n"
	    "value (on which @var{p} returns true) to produce\n"
	    "the final/leftmost portion of the constructed string.\n"
	    "It defaults to @code{(lambda (x) "")}.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_unfold_right
{
  SCM res, ans;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  if (!SCM_UNBNDP (base))
    {
      SCM_VALIDATE_STRING (5, base);
      ans = base;
    }
  else
    ans = scm_i_make_string (0, NULL, 0);
  if (!SCM_UNBNDP (make_final))
    SCM_VALIDATE_PROC (6, make_final);

  res = scm_call_1 (p, seed);
  while (scm_is_false (res))
    {
      SCM str;
      size_t i = 0;
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      str = scm_i_make_string (1, NULL, 0);
      str = scm_i_string_start_writing (str);
      scm_i_string_set_x (str, i, SCM_CHAR (ch));
      scm_i_string_stop_writing ();
      i++;

      ans = scm_string_append (scm_list_2 (str, ans));
      seed = scm_call_1 (g, seed);
      res = scm_call_1 (p, seed);
    }
  if (!SCM_UNBNDP (make_final))
    {
      res = scm_call_1 (make_final, seed);
      return scm_string_append (scm_list_2 (res, ans));
    }
  else
    return ans;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_for_each, "string-for-each", 2, 2, 0,
	    (SCM proc, SCM s, SCM start, SCM end),
	    "@var{proc} is mapped over @var{s} in left-to-right order.  The\n"
	    "return value is not specified.")
#define FUNC_NAME s_scm_string_for_each
{
  size_t cstart, cend;

  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
              proc, SCM_ARG1, FUNC_NAME);
  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);
  while (cstart < cend)
    {
      scm_call_1 (proc, SCM_MAKE_CHAR (scm_i_string_ref (s, cstart)));
      cstart++;
    }

  scm_remember_upto_here_1 (s);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_for_each_index, "string-for-each-index", 2, 2, 0,
	    (SCM proc, SCM s, SCM start, SCM end),
	    "Call @code{(@var{proc} i)} for each index i in @var{s}, from\n"
	    "left to right.\n"
	    "\n"
	    "For example, to change characters to alternately upper and\n"
	    "lower case,\n"
	    "\n"
	    "@example\n"
	    "(define str (string-copy \"studly\"))\n"
	    "(string-for-each-index\n"
	    "    (lambda (i)\n"
	    "      (string-set! str i\n"
	    "        ((if (even? i) char-upcase char-downcase)\n"
	    "         (string-ref str i))))\n"
	    "    str)\n"
	    "str @result{} \"StUdLy\"\n"
	    "@end example")
#define FUNC_NAME s_scm_string_for_each_index
{
  size_t cstart, cend;

  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
              proc, SCM_ARG1, FUNC_NAME);
  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);

  while (cstart < cend)
    {
      scm_call_1 (proc, scm_from_size_t (cstart));
      cstart++;
    }

  scm_remember_upto_here_1 (s);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_xsubstring, "xsubstring", 2, 3, 0,
	    (SCM s, SCM from, SCM to, SCM start, SCM end),
	    "This is the @emph{extended substring} procedure that implements\n"
	    "replicated copying of a substring of some string.\n"
	    "\n"
	    "@var{s} is a string, @var{start} and @var{end} are optional\n"
	    "arguments that demarcate a substring of @var{s}, defaulting to\n"
	    "0 and the length of @var{s}.  Replicate this substring up and\n"
	    "down index space, in both the positive and negative directions.\n"
	    "@code{xsubstring} returns the substring of this string\n"
	    "beginning at index @var{from}, and ending at @var{to}, which\n"
	    "defaults to @var{from} + (@var{end} - @var{start}).")
#define FUNC_NAME s_scm_xsubstring
{
  size_t p;
  size_t cstart, cend;
  int cfrom, cto;
  SCM result;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      4, start, cstart,
			      5, end, cend);

  cfrom = scm_to_int (from);
  if (SCM_UNBNDP (to))
    cto = cfrom + (cend - cstart);
  else
    cto = scm_to_int (to);
  if (cstart == cend && cfrom != cto)
    SCM_MISC_ERROR ("start and end indices must not be equal", SCM_EOL);

  result = scm_i_make_string (cto - cfrom, NULL, 0);
  result = scm_i_string_start_writing (result);

  p = 0;
  while (cfrom < cto)
    {
      size_t t = ((cfrom < 0) ? -cfrom : cfrom) % (cend - cstart);
      if (cfrom < 0)
	scm_i_string_set_x (result, p, 
                            scm_i_string_ref (s, (cend - cstart) - t));
      else
	scm_i_string_set_x (result, p, scm_i_string_ref (s, t));
      cfrom++;
      p++;
    }
  scm_i_string_stop_writing ();

  scm_remember_upto_here_1 (s);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_xcopy_x, "string-xcopy!", 4, 3, 0,
	    (SCM target, SCM tstart, SCM s, SCM sfrom, SCM sto, SCM start, SCM end),
	    "Exactly the same as @code{xsubstring}, but the extracted text\n"
	    "is written into the string @var{target} starting at index\n"
	    "@var{tstart}.  The operation is not defined if @code{(eq?\n"
	    "@var{target} @var{s})} or these arguments share storage -- you\n"
	    "cannot copy a string on top of itself.")
#define FUNC_NAME s_scm_string_xcopy_x
{
  size_t p;
  size_t ctstart, cstart, cend;
  int csfrom, csto;
  SCM dummy = SCM_UNDEFINED;
  size_t cdummy;

  MY_VALIDATE_SUBSTRING_SPEC (1, target,
			      2, tstart, ctstart,
			      2, dummy, cdummy);
  MY_VALIDATE_SUBSTRING_SPEC (3, s,
			      6, start, cstart,
			      7, end, cend);
  csfrom = scm_to_int (sfrom);
  if (SCM_UNBNDP (sto))
    csto = csfrom + (cend - cstart);
  else
    csto = scm_to_int (sto); 
  if (cstart == cend && csfrom != csto)
    SCM_MISC_ERROR ("start and end indices must not be equal", SCM_EOL);
  SCM_ASSERT_RANGE (1, tstart,
		    ctstart + (csto - csfrom) <= scm_i_string_length (target));

  p = 0;
  target = scm_i_string_start_writing (target);
  while (csfrom < csto)
    {
      size_t t = ((csfrom < 0) ? -csfrom : csfrom) % (cend - cstart);
      if (csfrom < 0)
	scm_i_string_set_x (target, p + cstart, scm_i_string_ref (s, (cend - cstart) - t));
      else
	scm_i_string_set_x (target, p + cstart, scm_i_string_ref (s, t));
      csfrom++;
      p++;
    }
  scm_i_string_stop_writing ();

  scm_remember_upto_here_2 (target, s);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_replace, "string-replace", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the string @var{s1}, but with the characters\n"
	    "@var{start1} @dots{} @var{end1} replaced by the characters\n"
	    "@var{start2} @dots{} @var{end2} from @var{s2}.")
#define FUNC_NAME s_scm_string_replace
{
  size_t cstart1, cend1, cstart2, cend2;
  SCM result;

  MY_VALIDATE_SUBSTRING_SPEC (1, s1,
			      3, start1, cstart1,
			      4, end1, cend1);
  MY_VALIDATE_SUBSTRING_SPEC (2, s2,
			      5, start2, cstart2,
			      6, end2, cend2);
  return (scm_string_append 
	  (scm_list_3 (scm_i_substring (s1, 0, cstart1),
		       scm_i_substring (s2, cstart2, cend2),
		       scm_i_substring (s1, cend1, scm_i_string_length (s1)))));
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_tokenize, "string-tokenize", 1, 3, 0,
	    (SCM s, SCM token_set, SCM start, SCM end),
	    "Split the string @var{s} into a list of substrings, where each\n"
	    "substring is a maximal non-empty contiguous sequence of\n"
	    "characters from the character set @var{token_set}, which\n"
	    "defaults to @code{char-set:graphic}.\n"
	    "If @var{start} or @var{end} indices are provided, they restrict\n"
	    "@code{string-tokenize} to operating on the indicated substring\n"
	    "of @var{s}.")
#define FUNC_NAME s_scm_string_tokenize
{
  size_t cstart, cend;
  SCM result = SCM_EOL;

  MY_VALIDATE_SUBSTRING_SPEC (1, s,
			      3, start, cstart,
			      4, end, cend);

  if (SCM_UNBNDP (token_set))
    token_set = scm_char_set_graphic;

  if (SCM_CHARSETP (token_set))
    {
      size_t idx;

      while (cstart < cend)
	{
	  while (cstart < cend)
	    {
	      if (REF_IN_CHARSET (s, cend-1, token_set))
		break;
	      cend--;
	    }
	  if (cstart >= cend)
	    break;
	  idx = cend;
	  while (cstart < cend)
	    {
	      if (!REF_IN_CHARSET (s, cend-1, token_set))
		break;
	      cend--;
	    }
	  result = scm_cons (scm_i_substring (s, cend, idx), result);
	}
    }
  else
    SCM_WRONG_TYPE_ARG (2, token_set);

  scm_remember_upto_here_1 (s);
  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_split, "string-split", 2, 0, 0,
	    (SCM str, SCM chr),
	    "Split the string @var{str} into a list of the substrings delimited\n"
	    "by appearances of the character @var{chr}.  Note that an empty substring\n"
	    "between separator characters will result in an empty string in the\n"
	    "result list.\n"
	    "\n"
	    "@lisp\n"
	    "(string-split \"root:x:0:0:root:/root:/bin/bash\" #\\:)\n"
	    "@result{}\n"
	    "(\"root\" \"x\" \"0\" \"0\" \"root\" \"/root\" \"/bin/bash\")\n"
	    "\n"
	    "(string-split \"::\" #\\:)\n"
	    "@result{}\n"
	    "(\"\" \"\" \"\")\n"
	    "\n"
	    "(string-split \"\" #\\:)\n"
	    "@result{}\n"
	    "(\"\")\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_split
{
  long idx, last_idx;
  int narrow;
  SCM res = SCM_EOL;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_CHAR (2, chr);
  
  /* This is explicit wide/narrow logic (instead of using
     scm_i_string_ref) is a speed optimization.  */
  idx = scm_i_string_length (str);
  narrow = scm_i_is_narrow_string (str);
  if (narrow)
    {
      const char *buf = scm_i_string_chars (str);
      while (idx >= 0)
        {
          last_idx = idx;
          while (idx > 0 && buf[idx-1] != (char) SCM_CHAR(chr))
            idx--;
          if (idx >= 0)
            {
              res = scm_cons (scm_i_substring (str, idx, last_idx), res);
              idx--;
            }
        }
    }
  else
    {
      const scm_t_wchar *buf = scm_i_string_wide_chars (str);
      while (idx >= 0)
        {
          last_idx = idx;
          while (idx > 0 && buf[idx-1] != SCM_CHAR(chr))
            idx--;
          if (idx >= 0)
            {
              res = scm_cons (scm_i_substring (str, idx, last_idx), res);
              idx--;
            }
        }
    }
  scm_remember_upto_here_1 (str);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_filter, "string-filter", 2, 2, 0,
	    (SCM char_pred, SCM s, SCM start, SCM end),
	    "Filter the string @var{s}, retaining only those characters\n"
	    "which satisfy @var{char_pred}.\n"
	    "\n"
	    "If @var{char_pred} is a procedure, it is applied to each\n"
	    "character as a predicate, if it is a character, it is tested\n"
	    "for equality and if it is a character set, it is tested for\n"
	    "membership.")
#define FUNC_NAME s_scm_string_filter
{
  size_t cstart, cend;
  SCM result;
  size_t idx;

#if SCM_ENABLE_DEPRECATED == 1
  if (scm_is_string (char_pred))
    {
      SCM tmp;

      scm_c_issue_deprecation_warning
        ("Guile used to use the wrong argument order for string-filter.\n"
         "This call to string-filter had the arguments in the wrong order.\n"
         "See SRFI-13 for more details. At some point we will remove this hack.");

      tmp = char_pred;
      char_pred = s;
      s = tmp;
    }
#endif

  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);

  /* The explicit loops below stripping leading and trailing non-matches
     mean we can return a substring if those are the only deletions, making
     string-filter as efficient as string-trim-both in that case.  */

  if (SCM_CHARP (char_pred))
    {
      size_t count;

      /* strip leading non-matches by incrementing cstart */
      while (cstart < cend && scm_i_string_ref (s, cstart) != SCM_CHAR (char_pred))
        cstart++;

      /* strip trailing non-matches by decrementing cend */
      while (cend > cstart && scm_i_string_ref (s, cend-1) != SCM_CHAR (char_pred))
        cend--;

      /* count chars to keep */
      count = 0;
      for (idx = cstart; idx < cend; idx++)
        if (scm_i_string_ref (s, idx) == SCM_CHAR (char_pred))
          count++;

      if (count == cend - cstart)
        {
          /* whole of cstart to cend is to be kept, return a copy-on-write
             substring */
        result_substring:
          result = scm_i_substring (s, cstart, cend);
        }
      else
        result = scm_c_make_string (count, char_pred);
    }
  else if (SCM_CHARSETP (char_pred))
    {
      size_t count;

      /* strip leading non-matches by incrementing cstart */
      while (cstart < cend && ! REF_IN_CHARSET (s, cstart, char_pred))
        cstart++;

      /* strip trailing non-matches by decrementing cend */
      while (cend > cstart && ! REF_IN_CHARSET (s, cend-1, char_pred))
        cend--;

      /* count chars to be kept */
      count = 0;
      for (idx = cstart; idx < cend; idx++)
        if (REF_IN_CHARSET (s, idx, char_pred))
          count++;

      /* if whole of start to end kept then return substring */
      if (count == cend - cstart)
        goto result_substring;
      else
        {
          size_t dst = 0;
          result = scm_i_make_string (count, NULL, 0);
	  result = scm_i_string_start_writing (result);

          /* decrement "count" in this loop as well as using idx, so that if
             another thread is simultaneously changing "s" there's no chance
             it'll make us copy more than count characters */
          for (idx = cstart; idx < cend && count != 0; idx++)
            {
              if (REF_IN_CHARSET (s, idx, char_pred))
                {
		  scm_i_string_set_x (result, dst, scm_i_string_ref (s, idx));
		  dst ++;
                  count--;
                }
            }
	  scm_i_string_stop_writing ();
        }
    }
  else
    {
      SCM ls = SCM_EOL;

      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG1, FUNC_NAME);
      idx = cstart;
      while (idx < cend)
	{
	  SCM res, ch;
	  ch = SCM_MAKE_CHAR (scm_i_string_ref (s, idx));
	  res = scm_call_1 (char_pred, ch);
	  if (scm_is_true (res))
	    ls = scm_cons (ch, ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }

  scm_remember_upto_here_1 (s);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_delete, "string-delete", 2, 2, 0,
	    (SCM char_pred, SCM s, SCM start, SCM end),
	    "Delete characters satisfying @var{char_pred} from @var{s}.\n"
	    "\n"
	    "If @var{char_pred} is a procedure, it is applied to each\n"
	    "character as a predicate, if it is a character, it is tested\n"
	    "for equality and if it is a character set, it is tested for\n"
	    "membership.")
#define FUNC_NAME s_scm_string_delete
{
  size_t cstart, cend;
  SCM result;
  size_t idx;

#if SCM_ENABLE_DEPRECATED == 1
  if (scm_is_string (char_pred))
    {
      SCM tmp;

      scm_c_issue_deprecation_warning
        ("Guile used to use the wrong argument order for string-delete.\n"
         "This call to string-filter had the arguments in the wrong order.\n"
         "See SRFI-13 for more details. At some point we will remove this hack.");

      tmp = char_pred;
      char_pred = s;
      s = tmp;
    }
#endif

  MY_VALIDATE_SUBSTRING_SPEC (2, s,
			      3, start, cstart,
			      4, end, cend);

  /* The explicit loops below stripping leading and trailing matches mean we
     can return a substring if those are the only deletions, making
     string-delete as efficient as string-trim-both in that case.  */

  if (SCM_CHARP (char_pred))
    {
      size_t count;

      /* strip leading matches by incrementing cstart */
      while (cstart < cend && scm_i_string_ref (s, cstart) == SCM_CHAR(char_pred))
        cstart++;

      /* strip trailing matches by decrementing cend */
      while (cend > cstart && scm_i_string_ref (s, cend-1) == SCM_CHAR (char_pred))
        cend--;

      /* count chars to be kept */
      count = 0;
      for (idx = cstart; idx < cend; idx++)
        if (scm_i_string_ref (s, idx) != SCM_CHAR (char_pred))
          count++;

      if (count == cend - cstart)
        {
          /* whole of cstart to cend is to be kept, return a copy-on-write
             substring */
        result_substring:
          result = scm_i_substring (s, cstart, cend);
        }
      else
        {
	  int i = 0;
          /* new string for retained portion */
          result = scm_i_make_string (count, NULL, 0); 
          result = scm_i_string_start_writing (result);
          /* decrement "count" in this loop as well as using idx, so that if
             another thread is simultaneously changing "s" there's no chance
             it'll make us copy more than count characters */
          for (idx = cstart; idx < cend && count != 0; idx++)
            {
	      scm_t_wchar c = scm_i_string_ref (s, idx);
              if (c != SCM_CHAR (char_pred))
                {
                  scm_i_string_set_x (result, i, c);
		  i++;
                  count--;
                }
            }
	  scm_i_string_stop_writing ();
        }
    }
  else if (SCM_CHARSETP (char_pred))
    {
      size_t count;

      /* strip leading matches by incrementing cstart */
      while (cstart < cend && REF_IN_CHARSET (s, cstart, char_pred))
        cstart++;

      /* strip trailing matches by decrementing cend */
      while (cend > cstart && REF_IN_CHARSET (s, cend-1, char_pred))
        cend--;

      /* count chars to be kept */
      count = 0;
      for (idx = cstart; idx < cend; idx++)
        if (!REF_IN_CHARSET (s, idx, char_pred))
          count++;

      if (count == cend - cstart)
        goto result_substring;
      else
        {
	  size_t i = 0;
          /* new string for retained portion */
          result = scm_i_make_string (count, NULL, 0);
	  result = scm_i_string_start_writing (result);

          /* decrement "count" in this loop as well as using idx, so that if
             another thread is simultaneously changing "s" there's no chance
             it'll make us copy more than count characters */
          for (idx = cstart; idx < cend && count != 0; idx++)
            {
              if (!REF_IN_CHARSET (s, idx, char_pred))
                {
		  scm_i_string_set_x (result, i, scm_i_string_ref (s, idx));
		  i++;
                  count--;
                }
            }
	  scm_i_string_stop_writing ();
        }
    }
  else
    {
      SCM ls = SCM_EOL;
      SCM_ASSERT (scm_is_true (scm_procedure_p (char_pred)),
                  char_pred, SCM_ARG1, FUNC_NAME);

      idx = cstart;
      while (idx < cend)
	{
	  SCM res, ch = SCM_MAKE_CHAR (scm_i_string_ref (s, idx));
	  res = scm_call_1 (char_pred, ch);
	  if (scm_is_false (res))
	    ls = scm_cons (ch, ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }

  scm_remember_upto_here_1 (s);
  return result;
}
#undef FUNC_NAME

void
scm_init_srfi_13 (void)
{
#include "libguile/srfi-13.x"
}

/* End of srfi-13.c.  */
