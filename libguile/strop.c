/* classes: src_files */

/*	Copyright (C) 1994, 1996, 1997, 1999 Free Software Foundation, Inc.

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
#include "_scm.h"
#include "chars.h"

#include "scm_validate.h"
#include "strop.h"
#include "read.h" /*For SCM_CASE_INSENSITIVE_P*/



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
  SCM_ASSERT (SCM_ICHRP (chr), chr, SCM_ARG2, why);

  if (sub_start == SCM_BOOL_F)
    sub_start = SCM_MAKINUM (0);

  SCM_ASSERT (SCM_INUMP (sub_start), sub_start, SCM_ARG3, why);
  lower = SCM_INUM (sub_start);
  if (lower < 0
      || lower > SCM_ROLENGTH (*str))
    scm_out_of_range (why, sub_start);

  if (sub_end == SCM_BOOL_F)
    sub_end = SCM_MAKINUM (SCM_ROLENGTH (*str));

  SCM_ASSERT (SCM_INUMP (sub_end), sub_end, SCM_ARG4, why);
  upper = SCM_INUM (sub_end);
  if (upper < SCM_INUM (sub_start)
      || upper > SCM_ROLENGTH (*str))
    scm_out_of_range (why, sub_end);

  if (direction > 0)
    {
      p = (unsigned char *)SCM_ROCHARS (*str) + lower;
      ch = SCM_ICHR (chr);

      for (x = SCM_INUM (sub_start); x < upper; ++x, ++p)
	if (*p == ch)
	  return x;
    }
  else
    {
      p = upper - 1 + (unsigned char *)SCM_ROCHARS (*str);
      ch = SCM_ICHR (chr);
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
	    "@code{index} or @code{strchr} functions from the C library.")
#define FUNC_NAME s_scm_string_index
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
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
	    "@code{rindex} or @code{strrchr} functions from the C library.")
#define FUNC_NAME s_scm_string_rindex
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, -1, frm, to, FUNC_NAME);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_substring_move_left_x, "substring-move-left!", 5, 0, 0, scm_substring_move_x);
SCM_REGISTER_PROC(s_substring_move_right_x, "substring-move-right!", 5, 0, 0, scm_substring_move_x);


SCM_DEFINE (scm_substring_move_x, "substring-move!", 5, 0, 0, 
           (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2),
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
	    "@var{fill-char}.")
#define FUNC_NAME s_scm_substring_fill_x
{
  long i, e;
  char c;
  SCM_VALIDATE_STRING (1,str);
  SCM_VALIDATE_INUM_COPY (2,start,i);
  SCM_VALIDATE_INUM_COPY (3,end,e);
  SCM_VALIDATE_ICHR_COPY (4,fill,c);
  SCM_ASSERT_RANGE (2,start,i <= SCM_LENGTH (str) && i >= 0);
  SCM_ASSERT_RANGE (3,end,e <= SCM_LENGTH (str) && e >= 0);
  while (i<e) SCM_CHARS (str)[i++] = c;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_null_p, "string-null?", 1, 0, 0, 
           (SCM str),
	    "Return @code{#t} if @var{str}'s length is nonzero, and @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_string_null_p
{
  SCM_VALIDATE_ROSTRING (1,str);
  return SCM_NEGATE_BOOL(SCM_ROLENGTH (str));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_list, "string->list", 1, 0, 0, 
           (SCM str),
	    "")
#define FUNC_NAME s_scm_string_to_list
{
  long i;
  SCM res = SCM_EOL;
  unsigned char *src;
  SCM_VALIDATE_ROSTRING (1,str);
  src = SCM_ROUCHARS (str);
  for (i = SCM_ROLENGTH (str)-1;i >= 0;i--) res = scm_cons ((SCM)SCM_MAKICHR (src[i]), res);
  return res;
}
#undef FUNC_NAME



SCM_DEFINE (scm_string_copy, "string-copy", 1, 0, 0, 
           (SCM str),
	    "")
#define FUNC_NAME s_scm_string_copy
{
  SCM_VALIDATE_STRINGORSUBSTR (1,str);
  return scm_makfromstr (SCM_ROCHARS (str), (scm_sizet)SCM_ROLENGTH (str), 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_fill_x, "string-fill!", 2, 0, 0,
           (SCM str, SCM chr),
	    "")
#define FUNC_NAME s_scm_string_fill_x
{
  register char *dst, c;
  register long k;
  SCM_VALIDATE_STRING_COPY (1,str,dst);
  SCM_VALIDATE_ICHR_COPY (2,chr,c);
  for (k = SCM_LENGTH (str)-1;k >= 0;k--) dst[k] = c;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_upcase_x, "string-upcase!", 1, 0, 0, 
           (SCM v),
	    "@deffnx primitive string-downcase! str\n"
	    "Upcase or downcase every character in @code{str}, respectively.")
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
	    "")
#define FUNC_NAME s_scm_string_upcase
{
  return scm_string_upcase_x(scm_string_copy(str));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_downcase_x, "string-downcase!", 1, 0, 0, 
           (SCM v),
	    "")
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
	    "")
#define FUNC_NAME s_scm_string_downcase
{
  SCM_VALIDATE_STRING (1,str);
  return scm_string_downcase_x(scm_string_copy(str));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_capitalize_x, "string-capitalize!", 1, 0, 0, 
           (SCM s),
	    "")
#define FUNC_NAME s_scm_string_capitalize_x
{
  char *str;
  int i, len, in_word=0;
  SCM_VALIDATE_STRING (1,s);
  len = SCM_LENGTH(s);
  str = SCM_CHARS(s);
  for(i=0; i<len;  i++) {
    if(SCM_NFALSEP(scm_char_alphabetic_p(SCM_MAKICHR(str[i])))) {
      if(!in_word) {
        str[i] = scm_upcase(str[i]);
        in_word = 1;
      } else {
        str[i] = scm_downcase(str[i]);
      }
    }
    else in_word = 0;
  }
  return s;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_capitalize, "string-capitalize", 1, 0, 0, 
           (SCM s),
	    "")
#define FUNC_NAME s_scm_string_capitalize
{
  SCM_VALIDATE_STRING (1,s);
  return scm_string_capitalize_x(scm_string_copy(s));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ci_to_symbol, "string-ci->symbol", 1, 0, 0, 
           (SCM str),
	    "")
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
#include "strop.x"
}
