/* classes: src_files */

/* Copyright (C) 1994,1996,1997,1999,2000,2001 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA

As a special exception, the Free Software Foundation gives permission
for additional uses of the text contained in its release of GUILE.

The exception is that, if you link the GUILE library with other files
to produce an executable, this does not by itself cause the
resulting executable to be covered by the GNU General Public License.
Your use of that executable is in no way restricted on account of
linking the GUILE library code into it.

This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License.

This exception applies only to the code released by the
Free Software Foundation under the name GUILE.  If you copy
code from other Free Software Foundation releases into a copy of
GUILE, as the General Public License permits, the exception does
not apply to the code that you add in this way.  To avoid misleading
anyone as to the status of such modified files, you must delete
this exception notice from them.

If you write modifications of your own for GUILE, it is your choice
whether to permit this exception to apply to your modifications.
If you do not wish that, delete this exception notice.  */





#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/strop.h"
#include "libguile/read.h" /*For SCM_CASE_INSENSITIVE_P*/

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/*
xSCM_DEFINE (scm_i_index, "i-index", 2, 2, 0,
           (SCM str, SCM chr, SCM frm, SCM to),
	   "@deftypefn {Internal C Function} {static int} scm_i_index (SCM *@var{str},\n"
	   "SCM @var{chr}, int @var{direction}, SCM @var{sub_start}, SCM @var{sub_end}, char *@var{why})
	   "This is a workhorse function that performs either an @code{index} or\n"
	   "@code{rindex} function, depending on the value of @var{direction}."
*/
/* implements index if direction > 0 otherwise rindex.  */
static long
scm_i_index (SCM *str, SCM chr, int direction, SCM sub_start,
	     SCM sub_end, const char *why)
{
  unsigned char * p;
  long x;
  long lower;
  long upper;
  int ch;

  SCM_ASSERT (SCM_STRINGP (*str), *str, SCM_ARG1, why);
  SCM_ASSERT (SCM_CHARP (chr), chr, SCM_ARG2, why);

  if (SCM_FALSEP (sub_start))
    sub_start = SCM_MAKINUM (0);

  SCM_ASSERT (SCM_INUMP (sub_start), sub_start, SCM_ARG3, why);
  lower = SCM_INUM (sub_start);
  if (lower < 0 || lower > SCM_STRING_LENGTH (*str))
    scm_out_of_range (why, sub_start);

  if (SCM_FALSEP (sub_end))
    sub_end = SCM_MAKINUM (SCM_STRING_LENGTH (*str));

  SCM_ASSERT (SCM_INUMP (sub_end), sub_end, SCM_ARG4, why);
  upper = SCM_INUM (sub_end);
  if (upper < SCM_INUM (sub_start) || upper > SCM_STRING_LENGTH (*str))
    scm_out_of_range (why, sub_end);

  if (direction > 0)
    {
      p = SCM_STRING_UCHARS (*str) + lower;
      ch = SCM_CHAR (chr);

      for (x = SCM_INUM (sub_start); x < upper; ++x, ++p)
	if (*p == ch)
	  return x;
    }
  else
    {
      p = upper - 1 + SCM_STRING_UCHARS (*str);
      ch = SCM_CHAR (chr);
      for (x = upper - 1; x >= lower; --x, --p)
	if (*p == ch)
	  return x;
    }

  return -1;
}

SCM_DEFINE (scm_string_index, "string-index", 2, 2, 0,
           (SCM str, SCM chr, SCM frm, SCM to),
	    "Return the index of the first occurrence of @var{chr} in\n"
	    "@var{str}.  The optional integer arguments @var{frm} and\n"
	    "@var{to} limit the search to a portion of the string.  This\n"
	    "procedure essentially implements the @code{index} or\n"
	    "@code{strchr} functions from the C library.\n"
	    "\n"
	    "@lisp\n"
	    "(string-index \"weiner\" #\\e)\n"
	    "@result{} 1\n\n"
	    "(string-index \"weiner\" #\\e 2)\n"
	    "@result{} 4\n\n"
	    "(string-index \"weiner\" #\\e 2 4)\n"
	    "@result{} #f\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_index
{
  long pos;

  if (SCM_UNBNDP (frm))
    frm = SCM_BOOL_F;
  if (SCM_UNBNDP (to))
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, 1, frm, to, FUNC_NAME);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_rindex, "string-rindex", 2, 2, 0,
           (SCM str, SCM chr, SCM frm, SCM to),
	    "Like @code{string-index}, but search from the right of the\n"
	    "string rather than from the left.  This procedure essentially\n"
	    "implements the @code{rindex} or @code{strrchr} functions from\n"
	    "the C library.\n"
	    "\n"
	    "@lisp\n"
	    "(string-rindex \"weiner\" #\\e)\n"
	    "@result{} 4\n\n"
	    "(string-rindex \"weiner\" #\\e 2 4)\n"
	    "@result{} #f\n\n"
	    "(string-rindex \"weiner\" #\\e 2 5)\n"
	    "@result{} 4\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_rindex
{
  long pos;

  if (SCM_UNBNDP (frm))
    frm = SCM_BOOL_F;
  if (SCM_UNBNDP (to))
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, -1, frm, to, FUNC_NAME);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_move_x, "substring-move!", 5, 0, 0,
           (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2),
	    "Copy the substring of @var{str1} bounded by @var{start1} and @var{end1}\n"
	    "into @var{str2} beginning at position @var{start2}.\n"
	    "@var{str1} and @var{str2} can be the same string.")
#define FUNC_NAME s_scm_substring_move_x
{
  long s1, s2, e, len;

  SCM_VALIDATE_STRING (1, str1);
  SCM_VALIDATE_INUM_COPY (2, start1, s1);
  SCM_VALIDATE_INUM_COPY (3, end1, e);
  SCM_VALIDATE_STRING (4, str2);
  SCM_VALIDATE_INUM_COPY (5, start2, s2);
  len = e - s1;
  SCM_ASSERT_RANGE (3, end1, len >= 0);
  SCM_ASSERT_RANGE (2, start1, s1 <= SCM_STRING_LENGTH (str1) && s1 >= 0);
  SCM_ASSERT_RANGE (5, start2, s2 <= SCM_STRING_LENGTH (str2) && s2 >= 0);
  SCM_ASSERT_RANGE (3, end1, e <= SCM_STRING_LENGTH (str1) && e >= 0);
  SCM_ASSERT_RANGE (5, start2, len+s2 <= SCM_STRING_LENGTH (str2));

  SCM_SYSCALL(memmove((void *)(&(SCM_STRING_CHARS(str2)[s2])),
		      (void *)(&(SCM_STRING_CHARS(str1)[s1])),
		      len));

  return scm_return_first(SCM_UNSPECIFIED, str1, str2);
}
#undef FUNC_NAME


SCM_DEFINE (scm_substring_fill_x, "substring-fill!", 4, 0, 0,
           (SCM str, SCM start, SCM end, SCM fill),
	    "Change every character in @var{str} between @var{start} and\n"
	    "@var{end} to @var{fill}.\n"
	    "\n"
	    "@lisp\n"
	    "(define y \"abcdefg\")\n"
	    "(substring-fill! y 1 3 #\\r)\n"
	    "y\n"
	    "@result{} \"arrdefg\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_substring_fill_x
{
  long i, e;
  char c;
  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_INUM_COPY (2, start, i);
  SCM_VALIDATE_INUM_COPY (3, end, e);
  SCM_VALIDATE_CHAR_COPY (4, fill, c);
  SCM_ASSERT_RANGE (2, start, i <= SCM_STRING_LENGTH (str) && i >= 0);
  SCM_ASSERT_RANGE (3, end, e <= SCM_STRING_LENGTH (str) && e >= 0);
  while (i<e) SCM_STRING_CHARS (str)[i++] = c;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


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
  return SCM_BOOL (SCM_STRING_LENGTH (str) == 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_list, "string->list", 1, 0, 0,
           (SCM str),
	    "Return a newly allocated list of the characters that make up\n"
	    "the given string @var{str}. @code{string->list} and\n"
	    "@code{list->string} are inverses as far as @samp{equal?} is\n"
	    "concerned.")
#define FUNC_NAME s_scm_string_to_list
{
  long i;
  SCM res = SCM_EOL;
  unsigned char *src;
  SCM_VALIDATE_STRING (1, str);
  src = SCM_STRING_UCHARS (str);
  for (i = SCM_STRING_LENGTH (str)-1;i >= 0;i--) res = scm_cons (SCM_MAKE_CHAR (src[i]), res);
  return res;
}
#undef FUNC_NAME


/* Helper function for the string copy and string conversion functions.
 * No argument checking is performed.  */
static SCM
string_copy (SCM str)
{
  const char* chars = SCM_STRING_CHARS (str);
  size_t length = SCM_STRING_LENGTH (str);
  SCM new_string = scm_mem2string (chars, length);
  scm_remember_upto_here_1 (str);
  return new_string;
}


SCM_DEFINE (scm_string_copy, "string-copy", 1, 0, 0,
	    (SCM str),
	    "Return a newly allocated copy of the given @var{string}.")
#define FUNC_NAME s_scm_string_copy
{
  SCM_VALIDATE_STRING (1, str);

  return string_copy (str);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_fill_x, "string-fill!", 2, 0, 0,
           (SCM str, SCM chr),
	    "Store @var{char} in every element of the given @var{string} and\n"
	    "return an unspecified value.")
#define FUNC_NAME s_scm_string_fill_x
{
  register char *dst, c;
  register long k;
  SCM_VALIDATE_STRING_COPY (1, str, dst);
  SCM_VALIDATE_CHAR_COPY (2, chr, c);
  for (k = SCM_STRING_LENGTH (str)-1;k >= 0;k--) dst[k] = c;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* Helper function for the string uppercase conversion functions.
 * No argument checking is performed.  */
static SCM
string_upcase_x (SCM v)
{
  unsigned long k;

  for (k = 0; k < SCM_STRING_LENGTH (v); ++k)
    SCM_STRING_UCHARS (v) [k] = scm_upcase (SCM_STRING_UCHARS (v) [k]);

  return v;
}


SCM_DEFINE (scm_string_upcase_x, "string-upcase!", 1, 0, 0,
	    (SCM str),
	    "Destructively upcase every character in @var{str} and return\n"
	    "@var{str}.\n"
	    "@lisp\n"
	    "y                  @result{} \"arrdefg\"\n"
	    "(string-upcase! y) @result{} \"ARRDEFG\"\n"
	    "y                  @result{} \"ARRDEFG\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_upcase_x
{
  SCM_VALIDATE_STRING (1, str);

  return string_upcase_x (str);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_upcase, "string-upcase", 1, 0, 0,
	    (SCM str),
	    "Return a freshly allocated string containing the characters of\n"
	    "@var{str} in upper case.")
#define FUNC_NAME s_scm_string_upcase
{
  SCM_VALIDATE_STRING (1, str);

  return string_upcase_x (string_copy (str));
}
#undef FUNC_NAME


/* Helper function for the string lowercase conversion functions.
 * No argument checking is performed.  */
static SCM
string_downcase_x (SCM v)
{
  unsigned long k;

  for (k = 0; k < SCM_STRING_LENGTH (v); ++k)
    SCM_STRING_UCHARS (v) [k] = scm_downcase (SCM_STRING_UCHARS (v) [k]);

  return v;
}


SCM_DEFINE (scm_string_downcase_x, "string-downcase!", 1, 0, 0,
	    (SCM str),
	    "Destructively downcase every character in @var{str} and return\n"
	    "@var{str}.\n"
	    "@lisp\n"
	    "y                     @result{} \"ARRDEFG\"\n"
	    "(string-downcase! y)  @result{} \"arrdefg\"\n"
	    "y                     @result{} \"arrdefg\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_downcase_x
{
  SCM_VALIDATE_STRING (1, str);

  return string_downcase_x (str);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_downcase, "string-downcase", 1, 0, 0,
	    (SCM str),
	    "Return a freshly allocation string containing the characters in\n"
	    "@var{str} in lower case.")
#define FUNC_NAME s_scm_string_downcase
{
  SCM_VALIDATE_STRING (1, str);

  return string_downcase_x (string_copy (str));
}
#undef FUNC_NAME


/* Helper function for the string capitalization functions.
 * No argument checking is performed.  */
static SCM
string_capitalize_x (SCM str)
{
  unsigned char *sz;
  long i, len;
  int in_word=0;

  len = SCM_STRING_LENGTH(str);
  sz = SCM_STRING_UCHARS (str);
  for(i=0; i<len;  i++) {
    if (!SCM_FALSEP (scm_char_alphabetic_p (SCM_MAKE_CHAR (sz[i])))) {
      if(!in_word) {
        sz[i] = scm_upcase(sz[i]);
        in_word = 1;
      } else {
        sz[i] = scm_downcase(sz[i]);
      }
    }
    else in_word = 0;
  }
  return str;
}


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
  SCM_VALIDATE_STRING (1, str);

  return string_capitalize_x (str);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_capitalize, "string-capitalize", 1, 0, 0,
	    (SCM str),
	    "Return a freshly allocated string with the characters in\n"
	    "@var{str}, where the first character of every word is\n"
	    "capitalized.")
#define FUNC_NAME s_scm_string_capitalize
{
  SCM_VALIDATE_STRING (1, str);

  return string_capitalize_x (string_copy (str));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_split, "string-split", 2, 0, 0,
	    (SCM str, SCM chr),
	    "Split the string @var{str} into the a list of the substrings delimited\n"
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
  char * p;
  int ch;
  SCM res = SCM_EOL;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_CHAR (2, chr);

  idx = SCM_STRING_LENGTH (str);
  p = SCM_STRING_CHARS (str);
  ch = SCM_CHAR (chr);
  while (idx >= 0)
    {
      last_idx = idx;
      while (idx > 0 && p[idx - 1] != ch)
	idx--;
      if (idx >= 0)
	{
	  res = scm_cons (scm_mem2string (p + idx, last_idx - idx), res);
	  idx--;
	}
    }
  scm_remember_upto_here_1 (str);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_ci_to_symbol, "string-ci->symbol", 1, 0, 0,
	    (SCM str),
	    "Return the symbol whose name is @var{str}.  @var{str} is\n"
	    "converted to lowercase before the conversion is done, if Guile\n"
	    "is currently reading symbols case-insensitively.")
#define FUNC_NAME s_scm_string_ci_to_symbol
{
  return scm_string_to_symbol (SCM_CASE_INSENSITIVE_P
			       ? scm_string_downcase(str)
			       : str);
}
#undef FUNC_NAME

void
scm_init_strop ()
{
#include "libguile/strop.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
