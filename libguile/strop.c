/* classes: src_files */

/*	Copyright (C) 1994, 1996, 1997, 1999, 2000 Free Software Foundation, Inc.

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
02111-1307 USA */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */




#include <stdio.h>
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
	   "@deftypefn {Internal C Function} {static int} scm_i_index (SCM *@var{str}, \n"
	   "SCM @var{chr}, int @var{direction}, SCM @var{sub_start}, SCM @var{sub_end}, char *@var{why})
	   "This is a workhorse function that performs either an @code{index} or\n"
	   "@code{rindex} function, depending on the value of @var{direction}. I'm\n"
	   "not at all clear on the usage of the pos arguments, though the purpose\n"
	   "seems to be correct reporting of which argument values are reporting\n"
	   "errors. Why you would do that, rather than just use @code{SCM_ARG[1234]}\n"
	   "explicitly is beyond me. Anyone holding any enlightenment?"
*/
/* implements index if direction > 0 otherwise rindex.  */
static int
scm_i_index (SCM *str, SCM chr, int direction, SCM sub_start, 
	     SCM sub_end, const char *why)
{
  unsigned char * p;
  int x;
  int lower;
  int upper;
  int ch;

  SCM_ASSERT (SCM_ROSTRINGP (*str), *str, SCM_ARG1, why);
  SCM_ASSERT (SCM_CHARP (chr), chr, SCM_ARG2, why);

  if (SCM_FALSEP (sub_start))
    sub_start = SCM_MAKINUM (0);

  SCM_ASSERT (SCM_INUMP (sub_start), sub_start, SCM_ARG3, why);
  lower = SCM_INUM (sub_start);
  if (lower < 0
      || lower > SCM_ROLENGTH (*str))
    scm_out_of_range (why, sub_start);

  if (SCM_FALSEP (sub_end))
    sub_end = SCM_MAKINUM (SCM_ROLENGTH (*str));

  SCM_ASSERT (SCM_INUMP (sub_end), sub_end, SCM_ARG4, why);
  upper = SCM_INUM (sub_end);
  if (upper < SCM_INUM (sub_start)
      || upper > SCM_ROLENGTH (*str))
    scm_out_of_range (why, sub_end);

  if (direction > 0)
    {
      p = (unsigned char *)SCM_ROCHARS (*str) + lower;
      ch = SCM_CHAR (chr);

      for (x = SCM_INUM (sub_start); x < upper; ++x, ++p)
	if (*p == ch)
	  return x;
    }
  else
    {
      p = upper - 1 + (unsigned char *)SCM_ROCHARS (*str);
      ch = SCM_CHAR (chr);
      for (x = upper - 1; x >= lower; --x, --p)
	if (*p == ch)
	  return x;
    }

  return -1;
}

SCM_DEFINE (scm_string_index, "string-index", 2, 2, 0, 
           (SCM str, SCM chr, SCM frm, SCM to),
	    "Return the index of the first occurrence of @var{chr} in @var{str}.  The\n"
	    "optional integer arguments @var{frm} and @var{to} limit the search to\n"
	    "a portion of the string.  This procedure essentially implements the\n"
	    "@code{index} or @code{strchr} functions from the C library.\n\n"
	    "(qdocs:)  Returns the index of @var{char} in @var{str}, or @code{#f} if the\n"
	    "@var{char} isn't in @var{str}. If @var{frm} is given and not @code{#f},\n"
	    "it is used as the starting index; if @var{to} is given and not @var{#f},\n"
	    "it is used as the ending index (exclusive).\n\n"
	    "@example\n"
	    "(string-index \"weiner\" #\\e)\n"
	    "@result{} 1\n\n"
	    "(string-index \"weiner\" #\\e 2)\n"
	    "@result{} 4\n\n"
	    "(string-index \"weiner\" #\\e 2 4)\n"
	    "@result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_string_index
{
  int pos;
  
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
	    "Like @code{string-index}, but search from the right of the string rather\n"
	    "than from the left.  This procedure essentially implements the\n"
	    "@code{rindex} or @code{strrchr} functions from the C library.\n\n"
	    "(qdocs:) The same as @code{string-index}, except it gives the rightmost occurance\n"
	    "of @var{char} in the range [@var{frm}, @var{to}-1], which defaults to\n"
	    "the entire string.\n\n"
	    "@example\n"
	    "(string-rindex \"weiner\" #\\e)\n"
	    "@result{} 4\n\n"
	    "(string-rindex \"weiner\" #\\e 2 4)\n"
	    "@result{} #f\n\n"
	    "(string-rindex \"weiner\" #\\e 2 5)\n"
	    "@result{} 4\n"
	    "@end example")
#define FUNC_NAME s_scm_string_rindex
{
  int pos;
  
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


SCM_REGISTER_PROC(s_substring_move_left_x, "substring-move-left!", 5, 0, 0, scm_substring_move_x);
SCM_REGISTER_PROC(s_substring_move_right_x, "substring-move-right!", 5, 0, 0, scm_substring_move_x);

/*
@defun substring-move-left! str1 start1 end1 str2 start2
@end defun
@deftypefn {C Function} SCM scm_substring_move_left_x (SCM @var{str1}, SCM @var{start1}, SCM @var{end1}, SCM @var{str2}, SCM @var{start2})
[@strong{Note:} this is only valid if you've applied the strop patch].

Moves a substring of @var{str1}, from @var{start1} to @var{end1}
(@var{end1} is exclusive), into @var{str2}, starting at
@var{start2}. Allows overlapping strings.

@example
(define x (make-string 10 #\a))
(define y "bcd")
(substring-move-left! x 2 5 y 0)
y
@result{} "aaa"

x
@result{} "aaaaaaaaaa"

(define y "bcdefg")
(substring-move-left! x 2 5 y 0)
y
@result{} "aaaefg"

(define y "abcdefg")
(substring-move-left! y 2 5 y 3)
y
@result{} "abccccg"
@end example
*/

/*
@defun substring-move-right! str1 start1 end1 str2 start2
@end defun
@deftypefn {C Function} SCM scm_substring_move_right_x (SCM @var{str1}, SCM @var{start1}, SCM @var{end1}, SCM @var{str2}, SCM @var{start2})
[@strong{Note:} this is only valid if you've applied the strop patch, if
it hasn't made it into the guile tree].

Does much the same thing as @code{substring-move-left!}, except it
starts moving at the end of the sequence, rather than the beginning.
@example
(define y "abcdefg")
(substring-move-right! y 2 5 y 0)
y
@result{} "ededefg"

(define y "abcdefg")
(substring-move-right! y 2 5 y 3)
y
@result{} "abccdeg"
@end example
*/ 

SCM_DEFINE (scm_substring_move_x, "substring-move!", 5, 0, 0, 
           (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2),
	    "@deffnx primitive substring-move-left! str1 start1 end1 str2 start2\n"
	    "@deffnx primitive substring-move-right! str1 start1 end1 str2 start2\n"
	    "Copy the substring of @var{str1} bounded by @var{start1} and @var{end1}\n"
	    "into @var{str2} beginning at position @var{end2}.\n"
	    "@code{substring-move-right!} begins copying from the rightmost character\n"
	    "and moves left, and @code{substring-move-left!} copies from the leftmost\n"
	    "character moving right.\n\n"
	    "It is useful to have two functions that copy in different directions so\n"
	    "that substrings can be copied back and forth within a single string.  If\n"
	    "you wish to copy text from the left-hand side of a string to the\n"
	    "right-hand side of the same string, and the source and destination\n"
	    "overlap, you must be careful to copy the rightmost characters of the\n"
	    "text first, to avoid clobbering your data.  Hence, when @var{str1} and\n"
	    "@var{str2} are the same string, you should use\n"
	    "@code{substring-move-right!} when moving text from left to right, and\n"
	    "@code{substring-move-left!}  otherwise.  If @code{str1} and @samp{str2}\n"
	    "are different strings, it does not matter which function you use.")
#define FUNC_NAME s_scm_substring_move_x
{
  long s1, s2, e, len;

  SCM_VALIDATE_STRING (1,str1);
  SCM_VALIDATE_INUM_COPY (2,start1,s1);
  SCM_VALIDATE_INUM_COPY (3,end1,e);
  SCM_VALIDATE_STRING (4,str2);
  SCM_VALIDATE_INUM_COPY (5,start2,s2);
  len = e - s1;
  SCM_ASSERT_RANGE (3,end1,len >= 0);
  SCM_ASSERT_RANGE (2,start1,s1 <= SCM_LENGTH (str1) && s1 >= 0);
  SCM_ASSERT_RANGE (5,start2,s2 <= SCM_LENGTH (str2) && s2 >= 0);
  SCM_ASSERT_RANGE (3,end1,e <= SCM_LENGTH (str1) && e >= 0);
  SCM_ASSERT_RANGE (5,start2,len+s2 <= SCM_LENGTH (str2));

  SCM_SYSCALL(memmove((void *)(&(SCM_CHARS(str2)[s2])),
		      (void *)(&(SCM_CHARS(str1)[s1])),
		      len));
  
  return scm_return_first(SCM_UNSPECIFIED, str1, str2);
}
#undef FUNC_NAME


SCM_DEFINE (scm_substring_fill_x, "substring-fill!", 4, 0, 0, 
           (SCM str, SCM start, SCM end, SCM fill),
	    "Change every character in @var{str} between @var{start} and @var{end} to\n"
	    "@var{fill-char}.\n\n"
	    "(qdocs:) Destructively fills @var{str}, from @var{start} to @var{end}, with @var{fill}.\n\n"
	    "@example\n"
	    "(define y \"abcdefg\")\n"
	    "(substring-fill! y 1 3 #\\r)\n"
	    "y\n"
	    "@result{} \"arrdefg\"\n"
	    "@end example")
#define FUNC_NAME s_scm_substring_fill_x
{
  long i, e;
  char c;
  SCM_VALIDATE_STRING (1,str);
  SCM_VALIDATE_INUM_COPY (2,start,i);
  SCM_VALIDATE_INUM_COPY (3,end,e);
  SCM_VALIDATE_CHAR_COPY (4,fill,c);
  SCM_ASSERT_RANGE (2,start,i <= SCM_LENGTH (str) && i >= 0);
  SCM_ASSERT_RANGE (3,end,e <= SCM_LENGTH (str) && e >= 0);
  while (i<e) SCM_CHARS (str)[i++] = c;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_null_p, "string-null?", 1, 0, 0, 
           (SCM str),
	    "Return @code{#t} if @var{str}'s length is nonzero, and @code{#f}\n"
	    "otherwise.\n\n"
	    "(qdocs:) Returns @code{#t} if @var{str} is empty, else returns @code{#f}.\n\n"
	    "@example\n"
	    "(string-null? \"\")\n"
	    "@result{} #t\n\n"
	    "(string-null? y)\n"
	    "@result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_string_null_p
{
  SCM_VALIDATE_ROSTRING (1,str);
  return SCM_NEGATE_BOOL(SCM_ROLENGTH (str));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_list, "string->list", 1, 0, 0, 
           (SCM str),
	    "@samp{String->list} returns a newly allocated list of the\n"
	    "characters that make up the given string.  @samp{List->string}\n"
	    "returns a newly allocated string formed from the characters in the list\n"
	    "@var{list}, which must be a list of characters. @samp{String->list}\n"
	    "and @samp{list->string} are\n"
	    "inverses so far as @samp{equal?} is concerned.  (r5rs)")
#define FUNC_NAME s_scm_string_to_list
{
  long i;
  SCM res = SCM_EOL;
  unsigned char *src;
  SCM_VALIDATE_ROSTRING (1,str);
  src = SCM_ROUCHARS (str);
  for (i = SCM_ROLENGTH (str)-1;i >= 0;i--) res = scm_cons (SCM_MAKE_CHAR (src[i]), res);
  return res;
}
#undef FUNC_NAME



SCM_DEFINE (scm_string_copy, "string-copy", 1, 0, 0, 
           (SCM str),
	    "Returns a newly allocated copy of the given @var{string}. (r5rs)")
#define FUNC_NAME s_scm_string_copy
{
  SCM_VALIDATE_STRINGORSUBSTR (1,str);
  return scm_makfromstr (SCM_ROCHARS (str), (scm_sizet)SCM_ROLENGTH (str), 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_fill_x, "string-fill!", 2, 0, 0,
           (SCM str, SCM chr),
	    "Stores @var{char} in every element of the given @var{string} and returns an\n"
	    "unspecified value.  (r5rs)")
#define FUNC_NAME s_scm_string_fill_x
{
  register char *dst, c;
  register long k;
  SCM_VALIDATE_STRING_COPY (1,str,dst);
  SCM_VALIDATE_CHAR_COPY (2,chr,c);
  for (k = SCM_LENGTH (str)-1;k >= 0;k--) dst[k] = c;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_upcase_x, "string-upcase!", 1, 0, 0, 
           (SCM v),
	    "Destructively upcase every character in @code{str}.\n\n"
	    "(qdocs:) Converts each element in @var{str} to upper case.\n\n"
	    "@example\n"
	    "(string-upcase! y)\n"
	    "@result{} \"ARRDEFG\"\n\n"
	    "y\n"
	    "@result{} \"ARRDEFG\"\n"
	    "@end example")
#define FUNC_NAME s_scm_string_upcase_x
{
  register long k;
  register unsigned char *cs;
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  k = SCM_LENGTH (v);
  switch SCM_TYP7
    (v)
    {
    case scm_tc7_string:
      cs = SCM_UCHARS (v);
      while (k--)
	cs[k] = scm_upcase(cs[k]);
      break;
    default:
    badarg1:SCM_WTA (1,v);
    }
  return v;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_upcase, "string-upcase", 1, 0, 0, 
           (SCM str),
	    "Upcase every character in @code{str}.")
#define FUNC_NAME s_scm_string_upcase
{
  return scm_string_upcase_x(scm_string_copy(str));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_downcase_x, "string-downcase!", 1, 0, 0, 
           (SCM v),
	    "Destructively downcase every character in @code{str}.\n\n"
	    "(qdocs:) Converts each element in @var{str} to lower case.\n\n"
	    "@example\n"
	    "y\n"
	    "@result{} \"ARRDEFG\"\n\n"
	    "(string-downcase! y)\n"
	    "@result{} \"arrdefg\"\n\n"
	    "y\n"
	    "@result{} \"arrdefg\"\n"
	    "@end example")
#define FUNC_NAME s_scm_string_downcase_x
{
  register long k;
  register unsigned char *cs;
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  k = SCM_LENGTH (v);
  switch (SCM_TYP7(v))
    {
      case scm_tc7_string:
        cs = SCM_UCHARS (v);
        while (k--)
          cs[k] = scm_downcase(cs[k]);
        break;
      default:
    badarg1:SCM_WTA (1,v);
    }
  return v;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_downcase, "string-downcase", 1, 0, 0, 
           (SCM str),
	    "Downcase every character in @code{str}.")
#define FUNC_NAME s_scm_string_downcase
{
  SCM_VALIDATE_STRING (1,str);
  return scm_string_downcase_x(scm_string_copy(str));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_capitalize_x, "string-capitalize!", 1, 0, 0, 
           (SCM str),
	    "Destructively capitalize every character in @code{str}.")
#define FUNC_NAME s_scm_string_capitalize_x
{
  char *sz;
  int i, len, in_word=0;
  SCM_VALIDATE_STRING (1,str);
  len = SCM_LENGTH(str);
  sz = SCM_CHARS(str);
  for(i=0; i<len;  i++) {
    if(SCM_NFALSEP(scm_char_alphabetic_p(SCM_MAKE_CHAR(sz[i])))) {
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
#undef FUNC_NAME

SCM_DEFINE (scm_string_capitalize, "string-capitalize", 1, 0, 0, 
           (SCM str),
	    "Capitalize every character in @code{str}.")
#define FUNC_NAME s_scm_string_capitalize
{
  SCM_VALIDATE_STRING (1,str);
  return scm_string_capitalize_x(scm_string_copy(str));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ci_to_symbol, "string-ci->symbol", 1, 0, 0, 
           (SCM str),
	    "Return the symbol whose name is @var{str}, downcased in necessary(???).")
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
