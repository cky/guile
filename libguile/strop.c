/* classes: src_files */

/*	Copyright (C) 1994, 1996, 1997, 1999, 2000, 2001 Free Software Foundation, Inc.

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
	   "@deftypefn {Internal C Function} {static int} scm_i_index (SCM *@var{str}, \n"
	   "SCM @var{chr}, int @var{direction}, SCM @var{sub_start}, SCM @var{sub_end}, char *@var{why})
	   "This is a workhorse function that performs either an @code{index} or\n"
	   "@code{rindex} function, depending on the value of @var{direction}."
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
	    "@code{strchr} functions from the C library.\n\n"
            "(qdocs:) Returns\n"
	    "the index of @var{char} in @var{str}, or @code{#f} if the\n"
	    "@var{char} isn't in @var{str}. If @var{frm} is given and not\n"
	    "@code{#f}, it is used as the starting index; if @var{to} is\n"
	    "given and not @code{#f}, it is used as the ending index\n"
	    "(exclusive).\n\n"
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
  SCM_ASSERT_RANGE (2,start1,s1 <= SCM_STRING_LENGTH (str1) && s1 >= 0);
  SCM_ASSERT_RANGE (5,start2,s2 <= SCM_STRING_LENGTH (str2) && s2 >= 0);
  SCM_ASSERT_RANGE (3,end1,e <= SCM_STRING_LENGTH (str1) && e >= 0);
  SCM_ASSERT_RANGE (5,start2,len+s2 <= SCM_STRING_LENGTH (str2));

  SCM_SYSCALL(memmove((void *)(&(SCM_STRING_CHARS(str2)[s2])),
		      (void *)(&(SCM_STRING_CHARS(str1)[s1])),
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
  SCM_ASSERT_RANGE (2,start,i <= SCM_STRING_LENGTH (str) && i >= 0);
  SCM_ASSERT_RANGE (3,end,e <= SCM_STRING_LENGTH (str) && e >= 0);
  while (i<e) SCM_STRING_CHARS (str)[i++] = c;
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
  SCM_VALIDATE_STRING (1,str);
  return SCM_NEGATE_BOOL (SCM_STRING_LENGTH (str));
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
  SCM_VALIDATE_STRING (1,str);
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
  return scm_makfromstr (SCM_STRING_CHARS (str), SCM_STRING_LENGTH (str), 0);
}


SCM_DEFINE (scm_string_copy, "string-copy", 1, 0, 0, 
	    (SCM str),
	    "Returns a newly allocated copy of the given @var{string}. (r5rs)")
#define FUNC_NAME s_scm_string_copy
{
  SCM_VALIDATE_STRING (1, str);

  return string_copy (str);
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
  char *sz;
  int i, len, in_word=0;

  len = SCM_STRING_LENGTH(str);
  sz = SCM_STRING_CHARS (str);
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


SCM_DEFINE (scm_string_capitalize_x, "string-capitalize!", 1, 0, 0, 
	    (SCM str),
	    "Upcase the first character of every word in @var{str}\n"
	    "destructively and return @var{str}.\n"
	    "\n"
	    "@lisp\n"
	    "y                      @result{} "hello world"\n"
	    "(string-capitalize! y) @result{} "Hello World"\n"
	    "y                      @result{} "Hello World"\n"
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


SCM_DEFINE (scm_string_ci_to_symbol, "string-ci->symbol", 1, 0, 0, 
	    (SCM str),
	    "Return the symbol whose name is @var{str}.  @var{str} is\n"
	    "converted to lowercase before the conversion is done, if Guile\n"
	    "is currently reading symbols case--insensitively.")
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
#ifndef SCM_MAGIC_SNARFER
#include "libguile/strop.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
