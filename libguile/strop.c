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



#include <stdio.h>
#include "_scm.h"
#include "chars.h"

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

  SCM_ASSERT (SCM_NIMP (*str) && SCM_ROSTRINGP (*str), *str, SCM_ARG1, why);
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

SCM_PROC(s_string_index, "string-index", 2, 2, 0, scm_string_index);

SCM 
scm_string_index (SCM str, SCM chr, SCM frm, SCM to)
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, 1, frm, to, s_string_index);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}

SCM_PROC(s_string_rindex, "string-rindex", 2, 2, 0, scm_string_rindex);

SCM 
scm_string_rindex (SCM str, SCM chr, SCM frm, SCM to)
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, -1, frm, to, s_string_rindex);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}


SCM_PROC(s_substring_move_left_x, "substring-move-left!", 5, 0, 0, scm_substring_move_x);
SCM_PROC(s_substring_move_right_x, "substring-move-right!", 5, 0, 0, scm_substring_move_x);
SCM_PROC(s_substring_move_x, "substring-move!", 5, 0, 0, scm_substring_move_x);

SCM
scm_substring_move_x (SCM str1, SCM start1, SCM end1, 
		      SCM str2, SCM start2)
    
{
  long s1, s2, e, len;

  SCM_ASSERT (SCM_NIMP (str1) && SCM_STRINGP (str1), str1, 
	      SCM_ARG1, s_substring_move_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, SCM_ARG2, s_substring_move_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, SCM_ARG3, s_substring_move_x);
  SCM_ASSERT (SCM_NIMP (str2) && SCM_STRINGP (str2), str2, 
	      SCM_ARG4, s_substring_move_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, SCM_ARG5, s_substring_move_x);

  s1 = SCM_INUM (start1), s2 = SCM_INUM (start2), e = SCM_INUM (end1);
  len = e - s1;
  SCM_ASSERT (len >= 0, end1, SCM_OUTOFRANGE, s_substring_move_x);
  SCM_ASSERT (s1 <= SCM_LENGTH (str1) && s1 >= 0, start1, 
	      SCM_OUTOFRANGE, s_substring_move_x);
  SCM_ASSERT (s2 <= SCM_LENGTH (str2) && s2 >= 0, start2, 
	      SCM_OUTOFRANGE, s_substring_move_x);
  SCM_ASSERT (e <= SCM_LENGTH (str1) && e >= 0, end1, 
	      SCM_OUTOFRANGE, s_substring_move_x);
  SCM_ASSERT (len+s2 <= SCM_LENGTH (str2), start2, 
	      SCM_OUTOFRANGE, s_substring_move_x);

  SCM_SYSCALL(memmove((void *)(&(SCM_CHARS(str2)[s2])),
		      (void *)(&(SCM_CHARS(str1)[s1])),
		      len));
  
  return scm_return_first(SCM_UNSPECIFIED, str1, str2);
}


SCM_PROC(s_substring_fill_x, "substring-fill!", 4, 0, 0, scm_substring_fill_x);

SCM
scm_substring_fill_x (SCM str, SCM start, SCM end, SCM fill)
    
{
  long i, e;
  char c;
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_substring_fill_x);
  SCM_ASSERT (SCM_INUMP (start), start, SCM_ARG2, s_substring_fill_x);
  SCM_ASSERT (SCM_INUMP (end), end, SCM_ARG3, s_substring_fill_x);
  SCM_ASSERT (SCM_ICHRP (fill), fill, SCM_ARG4, s_substring_fill_x);
  i = SCM_INUM (start), e = SCM_INUM (end);c = SCM_ICHR (fill);
  SCM_ASSERT (i <= SCM_LENGTH (str) && i >= 0, start, 
	      SCM_OUTOFRANGE, s_substring_fill_x);
  SCM_ASSERT (e <= SCM_LENGTH (str) && e >= 0, end, 
	      SCM_OUTOFRANGE, s_substring_fill_x);
  while (i<e) SCM_CHARS (str)[i++] = c;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_string_null_p, "string-null?", 1, 0, 0, scm_string_null_p);

SCM
scm_string_null_p (str)
     SCM str;
{
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_string_null_p);
  return (SCM_ROLENGTH (str)
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}


SCM_PROC(s_string_to_list, "string->list", 1, 0, 0, scm_string_to_list);

SCM
scm_string_to_list (str)
     SCM str;
{
  long i;
  SCM res = SCM_EOL;
  unsigned char *src;
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_string_to_list);
  src = SCM_ROUCHARS (str);
  for (i = SCM_ROLENGTH (str)-1;i >= 0;i--) res = scm_cons ((SCM)SCM_MAKICHR (src[i]), res);
  return res;
}



SCM_PROC(s_string_copy, "string-copy", 1, 0, 0, scm_string_copy);

SCM
scm_string_copy (str)
     SCM str;
{
  SCM_ASSERT (SCM_NIMP (str) && (SCM_STRINGP (str) || SCM_SUBSTRP (str)),
	      str, SCM_ARG1, s_string_copy);
  return scm_makfromstr (SCM_ROCHARS (str), (scm_sizet)SCM_ROLENGTH (str), 0);
}


SCM_PROC(s_string_fill_x, "string-fill!", 2, 0, 0, scm_string_fill_x);

SCM
scm_string_fill_x (str, chr)
     SCM str;
     SCM chr;
{
  register char *dst, c;
  register long k;
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_string_fill_x);
  SCM_ASSERT (SCM_ICHRP (chr), chr, SCM_ARG2, s_string_fill_x);
  c = SCM_ICHR (chr);
  dst = SCM_CHARS (str);
  for (k = SCM_LENGTH (str)-1;k >= 0;k--) dst[k] = c;
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_string_upcase_x, "string-upcase!", 1, 0, 0, scm_string_upcase_x);

SCM 
scm_string_upcase_x (v)
     SCM v;
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
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_string_upcase_x);
    }
  return v;
}

SCM_PROC(s_string_upcase, "string-upcase", 1, 0, 0, scm_string_upcase);

SCM
scm_string_upcase(SCM str)
{
  return scm_string_upcase_x(scm_string_copy(str));
}

SCM_PROC(s_string_downcase_x, "string-downcase!", 1, 0, 0, scm_string_downcase_x);

SCM 
scm_string_downcase_x (v)
     SCM v;
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
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_string_downcase_x);
    }
  return v;
}

SCM_PROC(s_string_downcase, "string-downcase", 1, 0, 0, scm_string_downcase);

SCM
scm_string_downcase(SCM str)
{
  SCM_ASSERT(SCM_NIMP(str) && SCM_STRINGP(str), str, SCM_ARG1, s_string_downcase);
  return scm_string_downcase_x(scm_string_copy(str));
}

SCM_PROC(s_string_capitalize_x, "string-capitalize!", 1, 0, 0, scm_string_capitalize_x);

SCM
scm_string_capitalize_x (SCM s)
{
  char *str;
  int i, len, in_word=0;
  SCM_ASSERT(SCM_NIMP(s) && SCM_STRINGP(s), s, SCM_ARG1, s_string_capitalize_x);
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

SCM_PROC(s_string_capitalize, "string-capitalize", 1, 0, 0, scm_string_capitalize);

SCM
scm_string_capitalize(SCM s)
{
  SCM_ASSERT((SCM_NIMP(s)) && (SCM_STRINGP(s)), s, SCM_ARG1, s_string_capitalize);
  return scm_string_capitalize_x(scm_string_copy(s));
}

SCM_PROC(s_string_ci_to_symbol, "string-ci->symbol", 1, 0, 0, scm_string_ci_to_symbol);

SCM
scm_string_ci_to_symbol(SCM str)
{
  return scm_string_to_symbol (SCM_CASE_INSENSITIVE_P
			       ? scm_string_downcase(str)
			       : str);
}

void
scm_init_strop ()
{
#include "strop.x"
}

