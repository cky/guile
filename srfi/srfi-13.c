/* srfi-13.c --- SRFI-13 procedures for Guile
 *
 * 	Copyright (C) 2001 Free Software Foundation, Inc.
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
#include <ctype.h>

#include <libguile.h>

#include "srfi-13.h"
#include "srfi-14.h"

SCM_DEFINE (scm_string_any, "string-any", 2, 2, 0, 
            (SCM pred, SCM s, SCM start, SCM end),
	    "Check if the predicate @var{pred} is true for any character in\n"
	    "the string @var{s}, proceeding from left (index @var{start}) to\n"
	    "right (index @var{end}).  If @code{string-any} returns true,\n"
	    "the returned true value is the one produced by the first\n"
	    "successful application of @var{pred}.")
#define FUNC_NAME s_scm_string_any
{
  char * cstr;
  int cstart, cend;
  SCM res;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  cstr += cstart;
  while (cstart < cend)
    {
      res = scm_apply (pred, SCM_MAKE_CHAR (*cstr), scm_listofnull);
      if (!SCM_FALSEP (res))
	return res;
      cstr++;
      cstart++;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_every, "string-every", 2, 2, 0, 
            (SCM pred, SCM s, SCM start, SCM end),
	    "Check if the predicate @var{pred} is true for every character\n"
	    "in the string @var{s}, proceeding from left (index @var{start})\n"
	    "to right (index @var{end}).  If @code{string-every} returns\n"
	    "true, the returned true value is the one produced by the final\n"
	    "application of @var{pred} to the last character of @var{s}.")
#define FUNC_NAME s_scm_string_every
{
  char * cstr;
  int cstart, cend;
  SCM res;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  res = SCM_BOOL_F;
  cstr += cstart;
  while (cstart < cend)
    {
      res = scm_apply (pred, SCM_MAKE_CHAR (*cstr), scm_listofnull);
      if (SCM_FALSEP (res))
	return res;
      cstr++;
      cstart++;
    }
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
  int clen, i;
  SCM res;
  SCM ch;
  char * p;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_INUM_COPY (2, len, clen);
  SCM_ASSERT_RANGE (2, len, clen >= 0);

  res = scm_allocate_string (clen);
  p = SCM_STRING_CHARS (res);
  i = 0;
  while (i < clen)
    {
      ch = scm_apply (proc, SCM_MAKINUM (i), scm_listofnull);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", SCM_LIST1 (proc));
      *p++ = SCM_CHAR (ch);
      i++;
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_listS, "string->list", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Convert the string @var{str} into a list of characters.")
#define FUNC_NAME s_scm_string_to_listS
{
  char * cstr;
  int cstart, cend;
  SCM result = SCM_EOL;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  while (cstart < cend)
    {
      cend--;
      result = scm_cons (SCM_MAKE_CHAR (cstr[cend]), result);
    }
  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_reverse_list_to_string, "reverse-list->string", 1, 0, 0, 
            (SCM chrs),
	    "An efficient implementation of @code{(compose string->list\n"
	    "reverse)}:\n"
	    "\n"
	    "@smalllisp\n"
	    "(reverse-list->string '(#\a #\B #\c)) @result{} \"cBa\"\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_reverse_list_to_string
{
  SCM result;
  long i = scm_ilength (chrs);
  
  if (i < 0)
    SCM_WRONG_TYPE_ARG (1, chrs);
  result = scm_allocate_string (i);

  {
    unsigned char *data = SCM_STRING_UCHARS (result) + i;

    while (SCM_NNULLP (chrs))
      {
	SCM elt = SCM_CAR (chrs);

	SCM_VALIDATE_CHAR (SCM_ARGn, elt);
	data--;
	*data = SCM_CHAR (elt);
	chrs = SCM_CDR (chrs);
      }
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
  int del_len = 0, extra_len = 0;
  int len = 0;
  char * p;
  long strings = scm_ilength (ls);

  /* Validate the string list.  */
  if (strings < 0)
    SCM_WRONG_TYPE_ARG (1, ls);

  /* Validate the delimiter and record its length.  */
  if (SCM_UNBNDP (delimiter))
    {
      delimiter = scm_makfrom0str (" ");
      del_len = 1;
    }
  else
    {
      SCM_VALIDATE_STRING (2, delimiter);
      del_len = SCM_STRING_LENGTH (delimiter);
    }

  /* Validate the grammar symbol and remember the grammar.  */
  if (SCM_UNBNDP (grammar))
    gram = GRAM_INFIX;
  else if (SCM_EQ_P (grammar, scm_sym_infix))
    gram = GRAM_INFIX;
  else if (SCM_EQ_P (grammar, scm_sym_strict_infix))
    gram = GRAM_STRICT_INFIX;
  else if (SCM_EQ_P (grammar, scm_sym_suffix))
    gram = GRAM_SUFFIX;
  else if (SCM_EQ_P (grammar, scm_sym_prefix))
    gram = GRAM_PREFIX;
  else
    SCM_WRONG_TYPE_ARG (3, grammar);

  /* Check grammar constraints and calculate the space required for
     the delimiter(s).  */
  switch (gram)
    {
    case GRAM_INFIX:
      if (!SCM_NULLP (ls))
	extra_len = (strings > 0) ? ((strings - 1) * del_len) : 0;
      break;
    case GRAM_STRICT_INFIX:
      if (strings == 0)
	SCM_MISC_ERROR ("strict-infix grammar requires non-empty list",
			SCM_EOL);
      extra_len = (strings - 1) * del_len;
      break;
    default:
      extra_len = strings * del_len;
      break;
    }

  tmp = ls;
  while (SCM_CONSP (tmp))
    {
      SCM elt = SCM_CAR (tmp);
      SCM_VALIDATE_STRING (1, elt);
      len += SCM_STRING_LENGTH (elt);
      tmp = SCM_CDR (tmp);
    }

  result = scm_allocate_string (len + extra_len);
  p = SCM_STRING_CHARS (result);

  tmp = ls;
  switch (gram)
    {
    case GRAM_INFIX:
    case GRAM_STRICT_INFIX:
      while (!SCM_NULLP (tmp))
	{
	  SCM elt = SCM_CAR (tmp);
	  memmove (p, SCM_STRING_CHARS (elt),
		   SCM_STRING_LENGTH (elt) * sizeof (char));
	  p += SCM_STRING_LENGTH (elt);
	  if (!SCM_NULLP (SCM_CDR (tmp)) && del_len > 0)
	    {
	      memmove (p, SCM_STRING_CHARS (delimiter),
		       SCM_STRING_LENGTH (delimiter) * sizeof (char));
	      p += del_len;
	    }
	  tmp = SCM_CDR (tmp);
	}
      break;
    case GRAM_SUFFIX:
      while (!SCM_NULLP (tmp))
	{
	  SCM elt = SCM_CAR (tmp);
	  memmove (p, SCM_STRING_CHARS (elt),
		   SCM_STRING_LENGTH (elt) * sizeof (char));
	  p += SCM_STRING_LENGTH (elt);
	  if (del_len > 0)
	    {
	      memmove (p, SCM_STRING_CHARS (delimiter),
		       SCM_STRING_LENGTH (delimiter) * sizeof (char));
	      p += del_len;
	    }
	  tmp = SCM_CDR (tmp);
	}
      break;
    case GRAM_PREFIX:
      while (!SCM_NULLP (tmp))
	{
	  SCM elt = SCM_CAR (tmp);
	  if (del_len > 0)
	    {
	      memmove (p, SCM_STRING_CHARS (delimiter),
		       SCM_STRING_LENGTH (delimiter) * sizeof (char));
	      p += del_len;
	    }
	  memmove (p, SCM_STRING_CHARS (elt),
		   SCM_STRING_LENGTH (elt) * sizeof (char));
	  p += SCM_STRING_LENGTH (elt);
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


SCM_DEFINE (scm_string_copyS, "string-copy", 1, 2, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a freshly allocated copy of the string @var{str}.  If\n"
	    "given, @var{start} and @var{end} delimit the portion of\n"
	    "@var{str} which is copied.")
#define FUNC_NAME s_scm_string_copyS
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  return scm_makfromstr (cstr + cstart, cend - cstart, 0);
  
}
#undef FUNC_NAME


SCM_DEFINE (scm_substring_shared, "substring/shared", 2, 1, 0, 
            (SCM str, SCM start, SCM end),
	    "Like @code{substring}, but the result may share memory with the\n"
	    "argument @var{str}.")
#define FUNC_NAME s_scm_substring_shared
{
  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_INUM (2, start);
  if (SCM_UNBNDP (end))
    end = SCM_MAKINUM (SCM_STRING_LENGTH (str));
  else
    SCM_VALIDATE_INUM (3, end);
  if (SCM_INUM (start) == 0 &&
      SCM_INUM (end) == SCM_STRING_LENGTH (str))
    return str;
  return scm_substring (str, start, end);
}
#undef FUNC_NAME


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
  char * cstr, * ctarget;
  int cstart, cend, ctstart, dummy;
  int len;
  SCM sdummy = SCM_UNDEFINED;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, target, ctarget,
				    2, tstart, ctstart,
				    2, sdummy, dummy);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (3, s, cstr,
				    4, start, cstart,
				    5, end, cend);
  len = cend - cstart;
  SCM_ASSERT_RANGE (3, s, len <= SCM_STRING_LENGTH (target) - ctstart);

  memmove (SCM_STRING_CHARS (target) + ctstart,
	   SCM_STRING_CHARS (s) + cstart,
	   len * sizeof (char));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_take, "string-take", 2, 0, 0, 
            (SCM s, SCM n),
	    "Return the @var{n} first characters of @var{s}.")
#define FUNC_NAME s_scm_string_take
{
  char * cstr;
  int cn;

  SCM_VALIDATE_STRING_COPY (1, s, cstr);
  SCM_VALIDATE_INUM_COPY (2, n, cn);
  SCM_ASSERT_RANGE (2, n, cn >= 0 && cn <= SCM_STRING_LENGTH (s));
  
  return scm_makfromstr (cstr, cn, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_drop, "string-drop", 2, 0, 0, 
            (SCM s, SCM n),
	    "Return all but the first @var{n} characters of @var{s}.")
#define FUNC_NAME s_scm_string_drop
{
  char * cstr;
  int cn;

  SCM_VALIDATE_STRING_COPY (1, s, cstr);
  SCM_VALIDATE_INUM_COPY (2, n, cn);
  SCM_ASSERT_RANGE (2, n, cn >= 0 && cn <= SCM_STRING_LENGTH (s));
  
  return scm_makfromstr (cstr + cn, SCM_STRING_LENGTH (s) - cn, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_take_right, "string-take-right", 2, 0, 0, 
            (SCM s, SCM n),
	    "Return the @var{n} last characters of @var{s}.")
#define FUNC_NAME s_scm_string_take_right
{
  char * cstr;
  int cn;

  SCM_VALIDATE_STRING_COPY (1, s, cstr);
  SCM_VALIDATE_INUM_COPY (2, n, cn);
  SCM_ASSERT_RANGE (2, n, cn >= 0 && cn <= SCM_STRING_LENGTH (s));
  
  return scm_makfromstr (cstr + SCM_STRING_LENGTH (s) - cn, cn, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_drop_right, "string-drop-right", 2, 0, 0, 
            (SCM s, SCM n),
	    "Return all but the last @var{n} characters of @var{s}.")
#define FUNC_NAME s_scm_string_drop_right
{
  char * cstr;
  int cn;

  SCM_VALIDATE_STRING_COPY (1, s, cstr);
  SCM_VALIDATE_INUM_COPY (2, n, cn);
  SCM_ASSERT_RANGE (2, n, cn >= 0 && cn <= SCM_STRING_LENGTH (s));
  
  return scm_makfromstr (cstr, SCM_STRING_LENGTH (s) - cn, 0);
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
  char cchr;
  char * cstr;
  int cstart, cend, clen;
  SCM result;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    4, start, cstart,
				    5, end, cend);
  SCM_VALIDATE_INUM_COPY (2, len, clen);
  if (SCM_UNBNDP (chr))
    cchr = ' ';
  else
    {
      SCM_VALIDATE_CHAR (3, chr);
      cchr = SCM_CHAR (chr);
    }
  result = scm_allocate_string (clen);
  if (clen < (cend - cstart))
    memmove (SCM_STRING_CHARS (result),
	     cstr + cend - clen,
	     clen * sizeof (char));
  else
    {
      memset (SCM_STRING_CHARS (result), cchr,
	      (clen - (cend - cstart)) * sizeof (char));
      memmove (SCM_STRING_CHARS (result) + (clen - (cend - cstart)),
	       cstr + cstart,
	       (cend - cstart) * sizeof (char));
    }
  return result;
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
  char cchr;
  char * cstr;
  int cstart, cend, clen;
  SCM result;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    4, start, cstart,
				    5, end, cend);
  SCM_VALIDATE_INUM_COPY (2, len, clen);
  if (SCM_UNBNDP (chr))
    cchr = ' ';
  else
    {
      SCM_VALIDATE_CHAR (3, chr);
      cchr = SCM_CHAR (chr);
    }
  result = scm_allocate_string (clen);
  if (clen < (cend - cstart))
    memmove (SCM_STRING_CHARS (result), cstr + cstart, clen * sizeof (char));
  else
    {
      memset (SCM_STRING_CHARS (result) + (cend - cstart),
	      cchr, (clen - (cend - cstart)) * sizeof (char));
      memmove (SCM_STRING_CHARS (result), cstr + cstart,
	       (cend - cstart) * sizeof (char));
    }
  return result;
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
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_UNBNDP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!isspace(cstr[cstart]))
	    break;
	  cstart++;
	}
    }
  else if (SCM_CHARP (char_pred))
    {
      char chr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  if (chr != cstr[cstart])
	    break;
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!SCM_CHARSET_GET (char_pred, cstr[cstart]))
	    break;
	  cstart++;
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;

	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cstart]),
			   scm_listofnull);
	  if (SCM_FALSEP (res))
	    break;
	  cstart++;
	}
    }
  return scm_makfromstr (cstr + cstart, cend - cstart, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_trim_right, "string-trim-right", 1, 3, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Trim @var{s} by skipping over all characters on the rightt\n"
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
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_UNBNDP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!isspace(cstr[cend - 1]))
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARP (char_pred))
    {
      char chr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  if (chr != cstr[cend - 1])
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!SCM_CHARSET_GET (char_pred, cstr[cend - 1]))
	    break;
	  cend--;
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;

	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cend - 1]),
			   scm_listofnull);
	  if (SCM_FALSEP (res))
	    break;
	  cend--;
	}
    }
  return scm_makfromstr (cstr + cstart, cend - cstart, 0);
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
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_UNBNDP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!isspace(cstr[cstart]))
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  if (!isspace(cstr[cend - 1]))
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARP (char_pred))
    {
      char chr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  if (chr != cstr[cstart])
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  if (chr != cstr[cend - 1])
	    break;
	  cend--;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!SCM_CHARSET_GET (char_pred, cstr[cstart]))
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  if (!SCM_CHARSET_GET (char_pred, cstr[cend - 1]))
	    break;
	  cend--;
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;

	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cstart]),
			   scm_listofnull);
	  if (SCM_FALSEP (res))
	    break;
	  cstart++;
	}
      while (cstart < cend)
	{
	  SCM res;

	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cend - 1]),
			   scm_listofnull);
	  if (SCM_FALSEP (res))
	    break;
	  cend--;
	}
    }
  return scm_makfromstr (cstr + cstart, cend - cstart, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_fill_xS, "string-fill!", 2, 2, 0,
	    (SCM str, SCM chr, SCM start, SCM end),
	    "Stores @var{chr} in every element of the given @var{str} and\n"
	    "returns an unspecified value.")
#define FUNC_NAME s_scm_string_fill_xS
{
  char * cstr;
  int cstart, cend;
  int c;
  long k;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    3, start, cstart,
				    4, end, cend);
  SCM_VALIDATE_CHAR_COPY (2, chr, c);
  for (k = cstart; k < cend; k++)
    cstr[k] = c;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


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
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    6, start1, cstart1,
				    7, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    8, start2, cstart2,
				    9, end2, cend2);
  SCM_VALIDATE_PROC (3, proc_lt);
  SCM_VALIDATE_PROC (4, proc_eq);
  SCM_VALIDATE_PROC (5, proc_gt);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] < cstr2[cstart2])
	return scm_apply (proc_lt, SCM_MAKINUM (cstart1), scm_listofnull);
      else if (cstr1[cstart1] > cstr2[cstart2])
	return scm_apply (proc_gt, SCM_MAKINUM (cstart1), scm_listofnull);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return scm_apply (proc_gt, SCM_MAKINUM (cstart1), scm_listofnull);
  else if (cstart2 < cend2)
    return scm_apply (proc_lt, SCM_MAKINUM (cstart1), scm_listofnull);
  else
    return scm_apply (proc_eq, SCM_MAKINUM (cstart1), scm_listofnull);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_compare_ci, "string-compare-ci", 5, 4, 0,
	    (SCM s1, SCM s2, SCM proc_lt, SCM proc_eq, SCM proc_gt, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Apply @var{proc_lt}, @var{proc_eq}, @var{proc_gt} to the\n"
	    "mismatch index, depending upon whether @var{s1} is less than,\n"
	    "equal to, or greater than @var{s2}.  The mismatch index is the\n"
	    "largest index @var{i} such that for every 0 <= @var{j} <\n"
	    "@var{i}, @var{s1}[@var{j}] = @var{s2}[@var{j}] -- that is,\n"
	    "@var{i} is the first position that does not match.  The\n"
	    "character comparison is done case-insensitively.")
#define FUNC_NAME s_scm_string_compare_ci
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    6, start1, cstart1,
				    7, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    8, start2, cstart2,
				    9, end2, cend2);
  SCM_VALIDATE_PROC (3, proc_lt);
  SCM_VALIDATE_PROC (4, proc_eq);
  SCM_VALIDATE_PROC (5, proc_gt);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) < scm_downcase (cstr2[cstart2]))
	return scm_apply (proc_lt, SCM_MAKINUM (cstart1), scm_listofnull);
      else if (scm_downcase (cstr1[cstart1]) > scm_downcase (cstr2[cstart2]))
	return scm_apply (proc_gt, SCM_MAKINUM (cstart1), scm_listofnull);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return scm_apply (proc_gt, SCM_MAKINUM (cstart1), scm_listofnull);
  else if (cstart2 < cend2)
    return scm_apply (proc_lt, SCM_MAKINUM (cstart1), scm_listofnull);
  else
    return scm_apply (proc_eq, SCM_MAKINUM (cstart1), scm_listofnull);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_eq, "string=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are not equal, a true\n"
	    "value otherwise.")
#define FUNC_NAME s_scm_string_eq
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] < cstr2[cstart2])
	return SCM_BOOL_F;
      else if (cstr1[cstart1] > cstr2[cstart2])
	return SCM_BOOL_F;
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_BOOL_F;
  else if (cstart2 < cend2)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (cstart1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_neq, "string<>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are equal, a true\n"
	    "value otherwise.")
#define FUNC_NAME s_scm_string_neq
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] < cstr2[cstart2])
	return SCM_MAKINUM (cstart1);
      else if (cstr1[cstart1] > cstr2[cstart2])
	return SCM_MAKINUM (cstart1);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_MAKINUM (cstart1);
  else if (cstart2 < cend2)
    return SCM_MAKINUM (cstart1);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_lt, "string<", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater or equal to @var{s2}, a\n"
	    "true value otherwise.")
#define FUNC_NAME s_scm_string_lt
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] < cstr2[cstart2])
	return SCM_MAKINUM (cstart1);
      else if (cstr1[cstart1] > cstr2[cstart2])
	return SCM_BOOL_F;
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_BOOL_F;
  else if (cstart2 < cend2)
    return SCM_MAKINUM (cstart1);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_gt, "string>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less or equal to @var{s2}, a\n"
	    "true value otherwise.")
#define FUNC_NAME s_scm_string_gt
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] < cstr2[cstart2])
	return SCM_BOOL_F;
      else if (cstr1[cstart1] > cstr2[cstart2])
	return SCM_MAKINUM (cstart1);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_MAKINUM (cstart1);
  else if (cstart2 < cend2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_le, "string<=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater to @var{s2}, a true\n"
	    "value otherwise.")
#define FUNC_NAME s_scm_string_le
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] < cstr2[cstart2])
	return SCM_MAKINUM (cstart1);
      else if (cstr1[cstart1] > cstr2[cstart2])
	return SCM_BOOL_F;
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_BOOL_F;
  else if (cstart2 < cend2)
    return SCM_MAKINUM (cstart1);
  else
    return SCM_MAKINUM (cstart1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ge, "string>=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less to @var{s2}, a true value\n"
	    "otherwise.")
#define FUNC_NAME s_scm_string_ge
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] < cstr2[cstart2])
	return SCM_BOOL_F;
      else if (cstr1[cstart1] > cstr2[cstart2])
	return SCM_MAKINUM (cstart1);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_MAKINUM (cstart1);
  else if (cstart2 < cend2)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (cstart1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_eq, "string-ci=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are not equal, a true\n"
	    "value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_eq
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) < scm_downcase (cstr2[cstart2]))
	return SCM_BOOL_F;
      else if (scm_downcase (cstr1[cstart1]) > scm_downcase (cstr2[cstart2]))
	return SCM_BOOL_F;
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_BOOL_F;
  else if (cstart2 < cend2)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (cstart1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_neq, "string-ci<>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} and @var{s2} are equal, a true\n"
	    "value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_neq
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) < scm_downcase (cstr2[cstart2]))
	return SCM_MAKINUM (cstart1);
      else if (scm_downcase (cstr1[cstart1]) > scm_downcase (cstr2[cstart2]))
	return SCM_MAKINUM (cstart1);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_MAKINUM (cstart1);
  else if (cstart2 < cend2)
    return SCM_MAKINUM (cstart1);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_lt, "string-ci<", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater or equal to @var{s2}, a\n"
	    "true value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_lt
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) < scm_downcase (cstr2[cstart2]))
	return SCM_MAKINUM (cstart1);
      else if (scm_downcase (cstr1[cstart1]) > scm_downcase (cstr2[cstart2]))
	return SCM_BOOL_F;
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_BOOL_F;
  else if (cstart2 < cend2)
    return SCM_MAKINUM (cstart1);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_gt, "string-ci>", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less or equal to @var{s2}, a\n"
	    "true value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_gt
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) < scm_downcase (cstr2[cstart2]))
	return SCM_BOOL_F;
      else if (scm_downcase (cstr1[cstart1]) > scm_downcase (cstr2[cstart2]))
	return SCM_MAKINUM (cstart1);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_MAKINUM (cstart1);
  else if (cstart2 < cend2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_le, "string-ci<=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is greater to @var{s2}, a true\n"
	    "value otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_le
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) < scm_downcase (cstr2[cstart2]))
	return SCM_MAKINUM (cstart1);
      else if (scm_downcase (cstr1[cstart1]) > scm_downcase (cstr2[cstart2]))
	return SCM_BOOL_F;
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_BOOL_F;
  else if (cstart2 < cend2)
    return SCM_MAKINUM (cstart1);
  else
    return SCM_MAKINUM (cstart1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_ge, "string-ci>=", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return @code{#f} if @var{s1} is less to @var{s2}, a true value\n"
	    "otherwise.  The character comparison is done\n"
	    "case-insensitively.")
#define FUNC_NAME s_scm_string_ci_ge
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) < scm_downcase (cstr2[cstart2]))
	return SCM_BOOL_F;
      else if (scm_downcase (cstr1[cstart1]) > scm_downcase (cstr2[cstart2]))
	return SCM_MAKINUM (cstart1);
      cstart1++;
      cstart2++;
    }
  if (cstart1 < cend1)
    return SCM_MAKINUM (cstart1);
  else if (cstart2 < cend2)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (cstart1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_prefix_length, "string-prefix-length", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common prefix of the two\n"
	    "strings.")
#define FUNC_NAME s_scm_string_prefix_length
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] != cstr2[cstart2])
	return SCM_MAKINUM (len);
      len++;
      cstart1++;
      cstart2++;
    }
  return SCM_MAKINUM (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_prefix_length_ci, "string-prefix-length-ci", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common prefix of the two\n"
	    "strings, ignoring character case.")
#define FUNC_NAME s_scm_string_prefix_length_ci
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) != scm_downcase (cstr2[cstart2]))
	return SCM_MAKINUM (len);
      len++;
      cstart1++;
      cstart2++;
    }
  return SCM_MAKINUM (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_length, "string-suffix-length", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common suffix of the two\n"
	    "strings.")
#define FUNC_NAME s_scm_string_suffix_length
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (cstr1[cend1] != cstr2[cend2])
	return SCM_MAKINUM (len);
      len++;
    }
  return SCM_MAKINUM (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_length_ci, "string-suffix-length-ci", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Return the length of the longest common suffix of the two\n"
	    "strings, ignoring character case.")
#define FUNC_NAME s_scm_string_suffix_length_ci
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (scm_downcase (cstr1[cend1]) != scm_downcase (cstr2[cend2]))
	return SCM_MAKINUM (len);
      len++;
    }
  return SCM_MAKINUM (len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_prefix_p, "string-prefix?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a prefix of @var{s2}?")
#define FUNC_NAME s_scm_string_prefix_p
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0, len1;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (cstr1[cstart1] != cstr2[cstart2])
	return SCM_BOOL (len == len1);
      len++;
      cstart1++;
      cstart2++;
    }
  return SCM_BOOL (len == len1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_prefix_ci_p, "string-prefix-ci?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a prefix of @var{s2}, ignoring character case?")
#define FUNC_NAME s_scm_string_prefix_ci_p
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0, len1;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      if (scm_downcase (cstr1[cstart1]) != scm_downcase (cstr2[cstart2]))
	return SCM_BOOL (len == len1);
      len++;
      cstart1++;
      cstart2++;
    }
  return SCM_BOOL (len == len1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_p, "string-suffix?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a suffix of @var{s2}?")
#define FUNC_NAME s_scm_string_suffix_p
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0, len1;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (cstr1[cend1] != cstr2[cend2])
	return SCM_BOOL (len == len1);
      len++;
    }
  return SCM_BOOL (len == len1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_suffix_ci_p, "string-suffix-ci?", 2, 4, 0,
	    (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2),
	    "Is @var{s1} a suffix of @var{s2}, ignoring character case?")
#define FUNC_NAME s_scm_string_suffix_ci_p
{
  char * cstr1, * cstr2;
  int cstart1, cend1, cstart2, cend2;
  int len = 0, len1;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  len1 = cend1 - cstart1;
  while (cstart1 < cend1 && cstart2 < cend2)
    {
      cend1--;
      cend2--;
      if (scm_downcase (cstr1[cend1]) != scm_downcase (cstr2[cend2]))
	return SCM_BOOL (len == len1);
      len++;
    }
  return SCM_BOOL (len == len1);
}
#undef FUNC_NAME


/* FIXME::martin: The `S' is to avoid a name clash with the procedure
   in the core, which does not accept a predicate. */
SCM_DEFINE (scm_string_indexS, "string-index", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from left to right, returning\n"
	    "the index of the first occurence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "equals @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "satisifies the predicate @var{char_pred}, if it is a procedure,\n"
	    "\n"
	    "@item\n"
	    "is in the set @var{char_pred}, if it is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_indexS
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      char cchr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  if (cchr == cstr[cstart])
	    return SCM_MAKINUM (cstart);
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (SCM_CHARSET_GET (char_pred, cstr[cstart]))
	    return SCM_MAKINUM (cstart);
	  cstart++;
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;
	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cstart]),
			   scm_listofnull);
	  if (!SCM_FALSEP (res))
	    return SCM_MAKINUM (cstart);
	  cstart++;
	}
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_index_right, "string-index-right", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from right to left, returning\n"
	    "the index of the last occurence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "equals @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "satisifies the predicate @var{char_pred}, if it is a procedure,\n"
	    "\n"
	    "@item\n"
	    "is in the set if @var{char_pred} is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_index_right
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      char cchr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  cend--;
	  if (cchr == cstr[cend])
	    return SCM_MAKINUM (cend);
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  cend--;
	  if (SCM_CHARSET_GET (char_pred, cstr[cend]))
	    return SCM_MAKINUM (cend);
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;
	  cend--;
	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cend]),
			   scm_listofnull);
	  if (!SCM_FALSEP (res))
	    return SCM_MAKINUM (cend);
	}
    }
  return SCM_BOOL_F;  
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_skip, "string-skip", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from left to right, returning\n"
	    "the index of the first occurence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "does not equal @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "does not satisify the predicate @var{char_pred}, if it is a\n"
	    "procedure,\n"
	    "\n"
	    "@item\n"
	    "is not in the set if @var{char_pred} is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_skip
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      char cchr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  if (cchr != cstr[cstart])
	    return SCM_MAKINUM (cstart);
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (!SCM_CHARSET_GET (char_pred, cstr[cstart]))
	    return SCM_MAKINUM (cstart);
	  cstart++;
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;
	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cstart]),
			   scm_listofnull);
	  if (SCM_FALSEP (res))
	    return SCM_MAKINUM (cstart);
	  cstart++;
	}
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_skip_right, "string-skip-right", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Search through the string @var{s} from right to left, returning\n"
	    "the index of the last occurence of a character which\n"
	    "\n"
	    "@itemize @bullet\n"
	    "@item\n"
	    "does not equal @var{char_pred}, if it is character,\n"
	    "\n"
	    "@item\n"
	    "does not satisifie the predicate @var{char_pred}, if it is a\n"
	    "procedure,\n"
	    "\n"
	    "@item\n"
	    "is not in the set if @var{char_pred} is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_skip_right
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      char cchr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  cend--;
	  if (cchr != cstr[cend])
	    return SCM_MAKINUM (cend);
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  cend--;
	  if (!SCM_CHARSET_GET (char_pred, cstr[cend]))
	    return SCM_MAKINUM (cend);
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;
	  cend--;
	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cend]),
			   scm_listofnull);
	  if (SCM_FALSEP (res))
	    return SCM_MAKINUM (cend);
	}
    }
  return SCM_BOOL_F;  
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
	    "satisifies the predicate @var{char_pred}, if it is a procedure.\n"
	    "\n"
	    "@item\n"
	    "is in the set @var{char_pred}, if it is a character set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_string_count
{
  char * cstr;
  int cstart, cend;
  int count = 0;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      char cchr = SCM_CHAR (char_pred);
      while (cstart < cend)
	{
	  if (cchr == cstr[cstart])
	    count++;
	  cstart++;
	}
    }
  else if (SCM_CHARSETP (char_pred))
    {
      while (cstart < cend)
	{
	  if (SCM_CHARSET_GET (char_pred, cstr[cstart]))
	    count++;
	  cstart++;
	}
    }
  else
    {
      SCM_VALIDATE_PROC (2, char_pred);
      while (cstart < cend)
	{
	  SCM res;
	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[cstart]),
			   scm_listofnull);
	  if (!SCM_FALSEP (res))
	    count++;
	  cstart++;
	}
    }
  return SCM_MAKINUM (count);
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
  char * cs1, * cs2;
  int cstart1, cend1, cstart2, cend2;
  int len2, i, j;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cs1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cs2,
				    5, start2, cstart2,
				    6, end2, cend2);
  len2 = cend2 - cstart2;
  while (cstart1 <= cend1 - len2)
    {
      i = cstart1;
      j = cstart2;
      while (i < cend1 && j < cend2 && cs1[i] == cs2[j])
	{
	  i++;
	  j++;
	}
      if (j == cend2)
	return SCM_MAKINUM (cstart1);
      cstart1++;
    }
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
  char * cs1, * cs2;
  int cstart1, cend1, cstart2, cend2;
  int len2, i, j;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cs1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cs2,
				    5, start2, cstart2,
				    6, end2, cend2);
  len2 = cend2 - cstart2;
  while (cstart1 <= cend1 - len2)
    {
      i = cstart1;
      j = cstart2;
      while (i < cend1 && j < cend2 &&
	     scm_downcase (cs1[i]) == scm_downcase (cs2[j]))
	{
	  i++;
	  j++;
	}
      if (j == cend2)
	return SCM_MAKINUM (cstart1);
      cstart1++;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* Helper function for the string uppercase conversion functions.  
 * No argument checking is performed.  */
static SCM
string_upcase_x (SCM v, int start, int end)
{
  unsigned long k;

  for (k = start; k < end; ++k)
    SCM_STRING_UCHARS (v) [k] = scm_upcase (SCM_STRING_UCHARS (v) [k]);

  return v;
}


/* FIXME::martin: The `S' is to avoid a name clash with the procedure
   in the core, which does not accept start/end indices */
SCM_DEFINE (scm_string_upcase_xS, "string-upcase!", 1, 2, 0, 
	    (SCM str, SCM start, SCM end),
	    "Destructively upcase every character in @code{str}.\n"
	    "\n"
	    "@lisp\n"
	    "(string-upcase! y)\n"
	    "@result{} \"ARRDEFG\"\n"
	    "y\n"
	    "@result{} \"ARRDEFG\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_upcase_xS
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  return string_upcase_x (str, cstart, cend);
}
#undef FUNC_NAME


/* FIXME::martin: The `S' is to avoid a name clash with the procedure
   in the core, which does not accept start/end indices */
SCM_DEFINE (scm_string_upcaseS, "string-upcase", 1, 2, 0, 
	    (SCM str, SCM start, SCM end),
	    "Upcase every character in @code{str}.")
#define FUNC_NAME s_scm_string_upcaseS
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  return string_upcase_x (scm_string_copy (str), cstart, cend);
}
#undef FUNC_NAME


/* Helper function for the string lowercase conversion functions.  
 * No argument checking is performed.  */
static SCM
string_downcase_x (SCM v, int start, int end)
{
  unsigned long k;

  for (k = start; k < end; ++k)
    SCM_STRING_UCHARS (v) [k] = scm_downcase (SCM_STRING_UCHARS (v) [k]);

  return v;
}


/* FIXME::martin: The `S' is to avoid a name clash with the procedure
   in the core, which does not accept start/end indices */
SCM_DEFINE (scm_string_downcase_xS, "string-downcase!", 1, 2, 0, 
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
#define FUNC_NAME s_scm_string_downcase_xS
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  return string_downcase_x (str, cstart, cend);
}
#undef FUNC_NAME


/* FIXME::martin: The `S' is to avoid a name clash with the procedure
   in the core, which does not accept start/end indices */
SCM_DEFINE (scm_string_downcaseS, "string-downcase", 1, 2, 0, 
	    (SCM str, SCM start, SCM end),
	    "Downcase every character in @var{str}.")
#define FUNC_NAME s_scm_string_downcaseS
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  return string_downcase_x (scm_string_copy (str), cstart, cend);
}
#undef FUNC_NAME


/* Helper function for the string capitalization functions.  
 * No argument checking is performed.  */
static SCM
string_titlecase_x (SCM str, int start, int end)
{
  char * sz;
  int i, in_word = 0;

  sz = SCM_STRING_CHARS (str);
  for(i = start; i < end;  i++)
    {
      if(SCM_NFALSEP(scm_char_alphabetic_p(SCM_MAKE_CHAR(sz[i]))))
	{
	  if (!in_word)
	    {
	      sz[i] = scm_upcase(sz[i]);
	      in_word = 1;
	    }
	  else
	    {
	      sz[i] = scm_downcase(sz[i]);
	    }
	}
      else
	in_word = 0;
    }
  return str;
}


SCM_DEFINE (scm_string_titlecase_x, "string-titlecase!", 1, 2, 0, 
	    (SCM str, SCM start, SCM end),
	    "Destructively titlecase every first character in a word in\n"
	    "@var{str}.")
#define FUNC_NAME s_scm_string_titlecase_x
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
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
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  return string_titlecase_x (scm_string_copy (str), cstart, cend);
}
#undef FUNC_NAME


/* Reverse the portion of @var{str} between str[cstart] (including)
   and str[cend] excluding.  */
static void
string_reverse_x (char * str, int cstart, int cend)
{
  char tmp;

  cend--;
  while (cstart < cend)
    {
      tmp = str[cstart];
      str[cstart] = str[cend];
      str[cend] = tmp;
      cstart++;
      cend--;
    }
}


SCM_DEFINE (scm_string_reverse, "string-reverse", 1, 2, 0, 
            (SCM str, SCM start, SCM end),
	    "Reverse the string @var{str}.  The optional arguments\n"
	    "@var{start} and @var{end} delimit the region of @var{str} to\n"
	    "operate on.")
#define FUNC_NAME s_scm_string_reverse
{
  char * cstr;
  int cstart;
  int cend;
  SCM result;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  result = scm_string_copy (str);
  string_reverse_x (SCM_STRING_CHARS (result), cstart, cend);
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
  char * cstr;
  int cstart;
  int cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, str, cstr,
				    2, start, cstart,
				    3, end, cend);
  string_reverse_x (SCM_STRING_CHARS (str), cstart, cend);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_append_shared, "string-append/shared", 0, 0, 1, 
            (SCM ls),
	    "Like @code{string-append}, but the result may share memory\n"
	    "with the argument strings.")
#define FUNC_NAME s_scm_string_append_shared
{
  long i;

  SCM_VALIDATE_REST_ARGUMENT (ls);

  /* Optimize the one-argument case.  */
  i = scm_ilength (ls);
  if (i == 1)
    return SCM_CAR (ls);
  else
    return scm_string_append (ls);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_concatenate, "string-concatenate", 1, 0, 0, 
            (SCM ls),
	    "Append the elements of @var{ls} (which must be strings)\n"
	    "together into a single string.  Guaranteed to return a freshly\n"
	    "allocated string.")
#define FUNC_NAME s_scm_string_concatenate
{
  long strings = scm_ilength (ls);
  SCM tmp, result;
  int len = 0;
  char * p;

  /* Validate the string list.  */
  if (strings < 0)
    SCM_WRONG_TYPE_ARG (1, ls);

  /* Calculate the size of the result string.  */
  tmp = ls;
  while (!SCM_NULLP (tmp))
    {
      SCM elt = SCM_CAR (tmp);
      SCM_VALIDATE_STRING (1, elt);
      len += SCM_STRING_LENGTH (elt);
      tmp = SCM_CDR (tmp);
    }
  result = scm_allocate_string (len);

  /* Copy the list elements into the result.  */
  p = SCM_STRING_CHARS (result);
  tmp = ls;
  while (!SCM_NULLP (tmp))
    {
      SCM elt = SCM_CAR (tmp);
      memmove (p, SCM_STRING_CHARS (elt),
	       SCM_STRING_LENGTH (elt) * sizeof (char));
      p += SCM_STRING_LENGTH (elt);
      tmp = SCM_CDR (tmp);
    }
  return result;
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
  long strings;
  SCM tmp, result;
  int len = 0;
  char * p;
  int cend = 0;

  /* Check the optional arguments and calculate the additional length
     of the result string.  */
  if (!SCM_UNBNDP (final_string))
    {
      SCM_VALIDATE_STRING (2, final_string);
      if (!SCM_UNBNDP (end))
	{
	  SCM_VALIDATE_INUM_COPY (3, end, cend);
	  SCM_ASSERT_RANGE (3, end, 
			    (cend >= 0) &&
			    (cend <= SCM_STRING_LENGTH (final_string)));
	}
      else
	{
	  cend = SCM_STRING_LENGTH (final_string);
	}
      len += cend;
    }
  strings = scm_ilength (ls);
  /* Validate the string list.  */
  if (strings < 0)
    SCM_WRONG_TYPE_ARG (1, ls);

  /* Calculate the length of the result string.  */
  tmp = ls;
  while (!SCM_NULLP (tmp))
    {
      SCM elt = SCM_CAR (tmp);
      SCM_VALIDATE_STRING (1, elt);
      len += SCM_STRING_LENGTH (elt);
      tmp = SCM_CDR (tmp);
    }

  result = scm_allocate_string (len);

  p = SCM_STRING_CHARS (result) + len;

  /* Construct the result string, possibly by using the optional final
     string.  */
  if (!SCM_UNBNDP (final_string))
    {
      p -= cend;
      memmove (p, SCM_STRING_CHARS (final_string), cend * sizeof (char));
    }
  tmp = ls;
  while (!SCM_NULLP (tmp))
    {
      SCM elt = SCM_CAR (tmp);
      p -= SCM_STRING_LENGTH (elt);
      memmove (p, SCM_STRING_CHARS (elt),
	       SCM_STRING_LENGTH (elt) * sizeof (char));
      tmp = SCM_CDR (tmp);
    }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_concatenate_shared, "string-concatenate/shared", 1, 0, 0, 
            (SCM ls),
	    "Like @code{string-concatenate}, but the result may share memory\n"
	    "with the strings in the list @var{ls}.")
#define FUNC_NAME s_scm_string_concatenate_shared
{
  /* Optimize the one-string case.  */
  long i = scm_ilength (ls);
  if (i == 1)
    {
      SCM_VALIDATE_STRING (1, SCM_CAR (ls));
      return SCM_CAR (ls);
    }
  return scm_string_concatenate (ls);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_concatenate_reverse_shared, "string-concatenate-reverse/shared", 1, 2, 0, 
            (SCM ls, SCM final_string, SCM end),
	    "Like @code{string-concatenate-reverse}, but the result may\n"
	    "share memory with the the strings in the @var{ls} arguments.")
#define FUNC_NAME s_scm_string_concatenate_reverse_shared
{
  /* Just call the non-sharing version.  */
  return scm_string_concatenate_reverse (ls, final_string, end);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_map, "string-map", 2, 2, 0,
	    (SCM s, SCM proc, SCM start, SCM end),
	    "@var{proc} is a char->char procedure, it is mapped over\n"
	    "@var{s}.  The order in which the procedure is applied to the\n"
	    "string elements is not specified.")
#define FUNC_NAME s_scm_string_map
{
  char * cstr, *p;
  int cstart, cend;
  SCM result;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  SCM_VALIDATE_PROC (2, proc);
  result = scm_allocate_string (cend - cstart);
  p = SCM_STRING_CHARS (result);
  while (cstart < cend)
    {
      SCM ch = scm_apply (proc, SCM_MAKE_CHAR (cstr[cstart]), 
			  scm_listofnull);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", SCM_LIST1 (proc));
      cstart++;
      *p++ = SCM_CHAR (ch);
    }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_map_x, "string-map!", 2, 2, 0,
	    (SCM s, SCM proc, SCM start, SCM end),
	    "@var{proc} is a char->char procedure, it is mapped over\n"
	    "@var{s}.  The order in which the procedure is applied to the\n"
	    "string elements is not specified.  The string @var{s} is\n"
	    "modified in-place, the return value is not specified.")
#define FUNC_NAME s_scm_string_map_x
{
  char * cstr, *p;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  SCM_VALIDATE_PROC (2, proc);
  p = SCM_STRING_CHARS (s) + cstart;
  while (cstart < cend)
    {
      SCM ch = scm_apply (proc, SCM_MAKE_CHAR (cstr[cstart]), 
			  scm_listofnull);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", SCM_LIST1 (proc));
      cstart++;
      *p++ = SCM_CHAR (ch);
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
  char * cstr;
  int cstart, cend;
  SCM result;

  SCM_VALIDATE_PROC (1, kons);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (3, s, cstr,
				    4, start, cstart,
				    5, end, cend);
  result = knil;
  while (cstart < cend)
    {
      result = scm_apply (kons, SCM_LIST2 (SCM_MAKE_CHAR (cstr[cstart]),
					   result), SCM_EOL);
      cstart++;
    }
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
  char * cstr;
  int cstart, cend;
  SCM result;

  SCM_VALIDATE_PROC (1, kons);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (3, s, cstr,
				    4, start, cstart,
				    5, end, cend);
  result = knil;
  while (cstart < cend)
    {
      result = scm_apply (kons, SCM_LIST2 (SCM_MAKE_CHAR (cstr[cend - 1]),
					   result), SCM_EOL);
      cend--;
    }
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
	    "@item @var{f} maps each seed value to the corresponding \n"
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
    ans = scm_allocate_string (0);
  if (!SCM_UNBNDP (make_final))
    SCM_VALIDATE_PROC (6, make_final);

  res = scm_apply (p, seed, scm_listofnull);
  while (SCM_FALSEP (res))
    {
      SCM str;
      SCM ch = scm_apply (f, seed, scm_listofnull);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", SCM_LIST1 (f));
      str = scm_allocate_string (1);
      *SCM_STRING_CHARS (str) = SCM_CHAR (ch);

      ans = scm_string_append (SCM_LIST2 (ans, str));
      seed = scm_apply (g, seed, scm_listofnull);
      res = scm_apply (p, seed, scm_listofnull);
    }
  if (!SCM_UNBNDP (make_final))
    {
      res = scm_apply (make_final, seed, scm_listofnull);
      return scm_string_append (SCM_LIST2 (ans, res));
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
	    "@item @var{f} maps each seed value to the corresponding \n"
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
    ans = scm_allocate_string (0);
  if (!SCM_UNBNDP (make_final))
    SCM_VALIDATE_PROC (6, make_final);

  res = scm_apply (p, seed, scm_listofnull);
  while (SCM_FALSEP (res))
    {
      SCM str;
      SCM ch = scm_apply (f, seed, scm_listofnull);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", SCM_LIST1 (f));
      str = scm_allocate_string (1);
      *SCM_STRING_CHARS (str) = SCM_CHAR (ch);

      ans = scm_string_append (SCM_LIST2 (str, ans));
      seed = scm_apply (g, seed, scm_listofnull);
      res = scm_apply (p, seed, scm_listofnull);
    }
  if (!SCM_UNBNDP (make_final))
    {
      res = scm_apply (make_final, seed, scm_listofnull);
      return scm_string_append (SCM_LIST2 (res, ans));
    }
  else
    return ans;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_for_each, "string-for-each", 2, 2, 0,
	    (SCM s, SCM proc, SCM start, SCM end),
	    "@var{proc} is mapped over @var{s} in left-to-right order.  The\n"
	    "return value is not specified.")
#define FUNC_NAME s_scm_string_for_each
{
  char * cstr;
  int cstart, cend;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  SCM_VALIDATE_PROC (2, proc);
  while (cstart < cend)
    {
      scm_apply (proc, SCM_MAKE_CHAR (cstr[cstart]), scm_listofnull);
      cstart++;
    }
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
  char * cs, * p;
  int cstart, cend, cfrom, cto;
  SCM result;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cs,
				    4, start, cstart,
				    5, end, cend);
  SCM_VALIDATE_INUM_COPY (2, from, cfrom);
  SCM_VALIDATE_INUM_DEF_COPY (3, to, cfrom + (cend - cstart), cto);
  if (cstart == cend && cfrom != cto)
    SCM_MISC_ERROR ("start and end indices must not be equal", SCM_EOL);
  
  result = scm_allocate_string (cto - cfrom);
  
  p = SCM_STRING_CHARS (result);
  while (cfrom < cto)
    {
      int t = ((cfrom < 0) ? -cfrom : cfrom) % (cend - cstart);
      if (cfrom < 0)
	*p = cs[(cend - cstart) - t];
      else
	*p = cs[t];
      cfrom++;
      p++;
    }
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
  char * ctarget, * cs, * p;
  int ctstart, csfrom, csto, cstart, cend;
  SCM dummy = SCM_UNDEFINED;
  int cdummy;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, target, ctarget,
				    2, tstart, ctstart,
				    2, dummy, cdummy);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (3, s, cs,
				    6, start, cstart,
				    7, end, cend);
  SCM_VALIDATE_INUM_COPY (4, sfrom, csfrom);
  SCM_VALIDATE_INUM_DEF_COPY (5, sto, csfrom + (cend - cstart), csto);
  if (cstart == cend && csfrom != csto)
    SCM_MISC_ERROR ("start and end indices must not be equal", SCM_EOL);
  SCM_ASSERT_RANGE (1, tstart,
		    ctstart + (csto - csfrom) <= SCM_STRING_LENGTH (target));

  p = ctarget + ctstart;
  while (csfrom < csto)
    {
      int t = ((csfrom < 0) ? -csfrom : csfrom) % (cend - cstart);
      if (csfrom < 0)
	*p = cs[(cend - cstart) - t];
      else
	*p = cs[t];
      csfrom++;
      p++;
    }
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
  char * cstr1, * cstr2, * p;
  int cstart1, cend1, cstart2, cend2;
  SCM result;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s1, cstr1,
				    3, start1, cstart1,
				    4, end1, cend1);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, s2, cstr2,
				    5, start2, cstart2,
				    6, end2, cend2);
  result = scm_allocate_string (cstart1 + (cend2 - cstart2) +
				SCM_STRING_LENGTH (s1) - cend1);
  p = SCM_STRING_CHARS (result);
  memmove (p, cstr1, cstart1 * sizeof (char));
  memmove (p + cstart1, cstr2 + cstart2, (cend2 - cstart2) * sizeof (char));
  memmove (p + cstart1 + (cend2 - cstart2),
	   cstr1 + cend1,
	   (SCM_STRING_LENGTH (s1) - cend1) * sizeof (char));
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_tokenize, "string-tokenize", 1, 3, 0,
	    (SCM s, SCM token_char, SCM start, SCM end),
	    "Split the string @var{s} into a list of substrings, where each\n"
	    "substring is a maximal non-empty contiguous sequence of\n"
	    "characters equal to the character @var{token_char}, or\n"
	    "whitespace, if @var{token_char} is not given.  If\n"
	    "@var{token_char} is a character set, it is used for finding the\n"
	    "token borders.")
#define FUNC_NAME s_scm_string_tokenize
{
  char * cstr;
  int cstart, cend;
  SCM result = SCM_EOL;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_UNBNDP (token_char))
    {
      int idx;

      while (cstart < cend)
	{
	  while (cstart < cend)
	    {
	      if (!isspace (cstr[cend - 1]))
		break;
	      cend--;
	    }
	  if (cstart >= cend)
	    break;
	  idx = cend;
	  while (cstart < cend)
	    {
	      if (isspace (cstr[cend - 1]))
		break;
	      cend--;
	    }
	  result = scm_cons (scm_makfromstr (cstr + cend, idx - cend,
					     0), result);
	}
    }
  else if (SCM_CHARSETP (token_char))
    {
      int idx;

      while (cstart < cend)
	{
	  while (cstart < cend)
	    {
	      if (!SCM_CHARSET_GET (token_char, cstr[cend - 1]))
		break;
	      cend--;
	    }
	  if (cstart >= cend)
	    break;
	  idx = cend;
	  while (cstart < cend)
	    {
	      if (SCM_CHARSET_GET (token_char, cstr[cend - 1]))
		break;
	      cend--;
	    }
	  result = scm_cons (scm_makfromstr (cstr + cend, idx - cend,
					     0), result);
	}
    }
  else
    {
      int idx;
      char chr;

      SCM_VALIDATE_CHAR (2, token_char);
      chr = SCM_CHAR (token_char);

      while (cstart < cend)
	{
	  while (cstart < cend)
	    {
	      if (cstr[cend - 1] != chr)
		break;
	      cend--;
	    }
	  if (cstart >= cend)
	    break;
	  idx = cend;
	  while (cstart < cend)
	    {
	      if (cstr[cend - 1] == chr)
		break;
	      cend--;
	    }
	  result = scm_cons (scm_makfromstr (cstr + cend, idx - cend,
					     0), result);
	}
    }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_filter, "string-filter", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Filter the string @var{s}, retaining only those characters that\n"
	    "satisfy the @var{char_pred} argument.  If the argument is a\n"
	    "procedure, it is applied to each character as a predicate, if\n"
	    "it is a character, it is tested for equality and if it is a\n"
	    "character set, it is tested for membership.")
#define FUNC_NAME s_scm_string_filter
{
  char * cstr;
  int cstart, cend;
  SCM result;
  int idx;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      SCM ls = SCM_EOL;
      char chr;
      
      chr = SCM_CHAR (char_pred);
      idx = cstart;
      while (idx < cend)
	{
	  if (cstr[idx] == chr)
	    ls = scm_cons (SCM_MAKE_CHAR (cstr[idx]), ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }
  else if (SCM_CHARSETP (char_pred))
    {
      SCM ls = SCM_EOL;
      
      idx = cstart;
      while (idx < cend)
	{
	  if (SCM_CHARSET_GET (char_pred, cstr[idx]))
	    ls = scm_cons (SCM_MAKE_CHAR (cstr[idx]), ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }
  else
    {
      SCM ls = SCM_EOL;

      SCM_VALIDATE_PROC (2, char_pred);
      idx = cstart;
      while (idx < cend)
	{
	  SCM res;
	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[idx]),
			   scm_listofnull);
	  if (!SCM_FALSEP (res))
	    ls = scm_cons (SCM_MAKE_CHAR (cstr[idx]), ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_delete, "string-delete", 2, 2, 0,
	    (SCM s, SCM char_pred, SCM start, SCM end),
	    "Filter the string @var{s}, retaining only those characters that\n"
	    "do not satisfy the @var{char_pred} argument.  If the argument\n"
	    "is a procedure, it is applied to each character as a predicate,\n"
	    "if it is a character, it is tested for equality and if it is a\n"
	    "character set, it is tested for membership.")
#define FUNC_NAME s_scm_string_delete
{
  char * cstr;
  int cstart, cend;
  SCM result;
  int idx;

  SCM_VALIDATE_SUBSTRING_SPEC_COPY (1, s, cstr,
				    3, start, cstart,
				    4, end, cend);
  if (SCM_CHARP (char_pred))
    {
      SCM ls = SCM_EOL;
      char chr;
      
      chr = SCM_CHAR (char_pred);
      idx = cstart;
      while (idx < cend)
	{
	  if (cstr[idx] != chr)
	    ls = scm_cons (SCM_MAKE_CHAR (cstr[idx]), ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }
  else if (SCM_CHARSETP (char_pred))
    {
      SCM ls = SCM_EOL;
      
      idx = cstart;
      while (idx < cend)
	{
	  if (SCM_CHARSET_GET (char_pred, cstr[idx]))
	    ls = scm_cons (SCM_MAKE_CHAR (cstr[idx]), ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }
  else
    {
      SCM ls = SCM_EOL;

      SCM_VALIDATE_PROC (2, char_pred);
      idx = cstart;
      while (idx < cend)
	{
	  SCM res;
	  res = scm_apply (char_pred, SCM_MAKE_CHAR (cstr[idx]),
			   scm_listofnull);
	  if (SCM_FALSEP (res))
	    ls = scm_cons (SCM_MAKE_CHAR (cstr[idx]), ls);
	  idx++;
	}
      result = scm_reverse_list_to_string (ls);
    }
  return result;
}
#undef FUNC_NAME


void
scm_init_srfi_13 (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "srfi/srfi-13.x"
#endif
}


void
scm_init_srfi_13_14 (void)
{
  static int initialized = 0;

  if (!initialized)
    {
      SCM srfi_13_module = scm_make_module (scm_read_0str ("(srfi srfi-13)"));
      SCM srfi_14_module = scm_make_module (scm_read_0str ("(srfi srfi-14)"));
      SCM old_module;

      initialized = 1;

      old_module = scm_set_current_module (srfi_13_module);
      scm_init_srfi_13 ();
      scm_set_current_module (srfi_14_module);
      scm_init_srfi_14 ();

      scm_set_current_module (old_module);
    }
}
