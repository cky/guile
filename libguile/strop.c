/* classes: src_files */

/*	Copyright (C) 1994 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */



#include <stdio.h>
#include "_scm.h"
#include "chars.h"

#include "strop.h"


#ifdef __STDC__
int
scm_i_index (SCM * str, SCM chr, SCM sub_start, SCM sub_end, int pos, int pos2, int pos3, int pos4, char * why)
#else
int
scm_i_index (str, chr, sub_start, sub_end, pos, pos2, pos3, pos4, why)
     SCM * str;
     SCM chr;
     SCM sub_start;
     SCM sub_end;
     int pos;
     int pos2;
     int pos3;
     int pos4;
     char * why;
#endif
{
  unsigned char * p;
  int x;
  int bound;
  int ch;

  SCM_ASSERT (SCM_NIMP (*str) && SCM_ROSTRINGP (*str), *str, pos, why);
  SCM_ASSERT (SCM_ICHRP (chr), chr, pos2, why);

  if (sub_start == SCM_BOOL_F)
    sub_start = SCM_MAKINUM (0);
  else
    SCM_ASSERT (   SCM_INUMP (sub_start)
	    && (0 <= SCM_INUM (sub_start))
	    && (SCM_INUM (sub_start) <= SCM_ROLENGTH (*str)),
	    sub_start, pos3, why);

  if (sub_end == SCM_BOOL_F)
    sub_end = SCM_MAKINUM (SCM_ROLENGTH (*str));
  else
    SCM_ASSERT (   SCM_INUMP (sub_end)
	    && (SCM_INUM (sub_start) <= SCM_INUM (sub_end))
	    && (SCM_INUM (sub_end) <= SCM_ROLENGTH (*str)),
	    sub_end, pos4, why);

  p = (unsigned char *)SCM_ROCHARS (*str) + SCM_INUM (sub_start);
  bound = SCM_INUM (sub_end);
  ch = SCM_ICHR (chr);

  for (x = SCM_INUM (sub_start); x < bound; ++x, ++p)
    if (*p == ch)
      return x;

  return -1;
}

#ifdef __STDC__
int
scm_i_rindex (SCM * str, SCM chr, SCM sub_start, SCM sub_end, int pos, int pos2, int pos3, int pos4, char * why)
#else
int
scm_i_rindex (str, chr, sub_start, sub_end, pos, pos2, pos3, pos4, why)
     SCM * str;
     SCM chr;
     SCM sub_start;
     SCM sub_end;
     int pos;
     int pos2;
     int pos3;
     int pos4;
     char * why;
#endif
{
  unsigned char * p;
  int x;
  int upper_bound;
  int lower_bound;
  int ch;

  SCM_ASSERT (SCM_NIMP (*str) && SCM_ROSTRINGP (*str), *str, pos, why);
  SCM_ASSERT (SCM_ICHRP (chr), chr, pos2, why);

  if (sub_start == SCM_BOOL_F)
    sub_start = SCM_MAKINUM (0);
  else
    SCM_ASSERT (   SCM_INUMP (sub_start)
	    && (0 <= SCM_INUM (sub_start))
	    && (SCM_INUM (sub_start) <= SCM_ROLENGTH (*str)),
	    sub_start, pos3, why);

  if (sub_end == SCM_BOOL_F)
    sub_end = SCM_MAKINUM (SCM_ROLENGTH (*str));
  else
    SCM_ASSERT (   SCM_INUMP (sub_end)
	    && (SCM_INUM (sub_start) <= SCM_INUM (sub_end))
	    && (SCM_INUM (sub_end) <= SCM_ROLENGTH (*str)),
	    sub_end, pos4, why);

  upper_bound = SCM_INUM (sub_end);
  lower_bound = SCM_INUM (sub_start);
  p = upper_bound - 1 + (unsigned char *)SCM_ROCHARS (*str);
  ch = SCM_ICHR (chr);
  for (x = upper_bound - 1; x >= lower_bound; --x, --p)
    if (*p == ch)
      return x;

  return -1;
}


SCM_PROC(s_string_index, "string-index", 2, 2, 0, scm_string_index);
#ifdef __STDC__
SCM 
scm_string_index (SCM str, SCM chr, SCM frm, SCM to)
#else
SCM 
scm_string_index (str, chr, frm, to)
     SCM str;
     SCM chr;
     SCM frm;
     SCM to;
#endif
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, frm, to, SCM_ARG1, SCM_ARG2, SCM_ARG3, SCM_ARG4, s_string_index);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}

SCM_PROC(s_string_rindex, "string-rindex", 2, 2, 0, scm_string_rindex);
#ifdef __STDC__
SCM 
scm_string_rindex (SCM str, SCM chr, SCM frm, SCM to)
#else
SCM 
scm_string_rindex (str, chr, frm, to)
     SCM str;
     SCM chr;
     SCM frm;
     SCM to;
#endif
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_rindex (&str, chr, frm, to, SCM_ARG1, SCM_ARG2, SCM_ARG3, SCM_ARG4, s_string_index);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}





 
SCM_PROC(s_substring_move_left_x, "substring-move-left!", 2, 0, 1, scm_substring_move_left_x);
#ifdef __STDC__
SCM
scm_substring_move_left_x (SCM str1, SCM start1, SCM args)
#else
SCM
scm_substring_move_left_x (str1, start1, args)
     SCM str1;
     SCM start1;
     SCM args;
#endif
{
  SCM end1, str2, start2;
  long i, j, e;
  SCM_ASSERT (3==scm_ilength (args), args, SCM_WNA, s_substring_move_left_x);
  end1 = SCM_CAR (args); args = SCM_CDR (args);
  str2 = SCM_CAR (args); args = SCM_CDR (args);
  start2 = SCM_CAR (args);
  SCM_ASSERT (SCM_NIMP (str1) && SCM_STRINGP (str1), str1, SCM_ARG1, s_substring_move_left_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, SCM_ARG2, s_substring_move_left_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, SCM_ARG3, s_substring_move_left_x);
  SCM_ASSERT (SCM_NIMP (str2) && SCM_STRINGP (str2), str2, SCM_ARG4, s_substring_move_left_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, SCM_ARG5, s_substring_move_left_x);
  i = SCM_INUM (start1), j = SCM_INUM (start2), e = SCM_INUM (end1);
  SCM_ASSERT (i <= SCM_LENGTH (str1) && i >= 0, start1, SCM_OUTOFRANGE, s_substring_move_left_x);
  SCM_ASSERT (j <= SCM_LENGTH (str2) && j >= 0, start2, SCM_OUTOFRANGE, s_substring_move_left_x);
  SCM_ASSERT (e <= SCM_LENGTH (str1) && e >= 0, end1, SCM_OUTOFRANGE, s_substring_move_left_x);
  SCM_ASSERT (e-i+j <= SCM_LENGTH (str2), start2, SCM_OUTOFRANGE, s_substring_move_left_x);
  while (i<e) SCM_CHARS (str2)[j++] = SCM_CHARS (str1)[i++];
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_substring_move_right_x, "substring-move-right!", 2, 0, 1, scm_substring_move_right_x);
#ifdef __STDC__
SCM
scm_substring_move_right_x (SCM str1, SCM start1, SCM args)
#else
SCM
scm_substring_move_right_x (str1, start1, args)
     SCM str1;
     SCM start1;
     SCM args;
#endif
{
  SCM end1, str2, start2;
  long i, j, e;
  SCM_ASSERT (3==scm_ilength (args), args, SCM_WNA, s_substring_move_right_x);
  end1 = SCM_CAR (args); args = SCM_CDR (args);
  str2 = SCM_CAR (args); args = SCM_CDR (args);
  start2 = SCM_CAR (args);
  SCM_ASSERT (SCM_NIMP (str1) && SCM_STRINGP (str1), str1, SCM_ARG1, s_substring_move_right_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, SCM_ARG2, s_substring_move_right_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, SCM_ARG3, s_substring_move_right_x);
  SCM_ASSERT (SCM_NIMP (str2) && SCM_STRINGP (str2), str2, SCM_ARG4, s_substring_move_right_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, SCM_ARG5, s_substring_move_right_x);
  i = SCM_INUM (start1), j = SCM_INUM (start2), e = SCM_INUM (end1);
  SCM_ASSERT (i <= SCM_LENGTH (str1) && i >= 0, start1, SCM_OUTOFRANGE, s_substring_move_right_x);
  SCM_ASSERT (j <= SCM_LENGTH (str2) && j >= 0, start2, SCM_OUTOFRANGE, s_substring_move_right_x);
  SCM_ASSERT (e <= SCM_LENGTH (str1) && e >= 0, end1, SCM_OUTOFRANGE, s_substring_move_right_x);
  SCM_ASSERT ((j = e-i+j) <= SCM_LENGTH (str2), start2, SCM_OUTOFRANGE, s_substring_move_right_x);
  while (i<e) SCM_CHARS (str2)[--j] = SCM_CHARS (str1)[--e];
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_substring_fill_x, "substring-fill!", 2, 0, 1, scm_substring_fill_x);
#ifdef __STDC__
SCM
scm_substring_fill_x (SCM str, SCM start, SCM args)
#else
SCM
scm_substring_fill_x (str, start, args)
     SCM str;
     SCM start;
     SCM args;
#endif
{
  SCM end, fill;
  long i, e;
  char c;
  SCM_ASSERT (2==scm_ilength (args), args, SCM_WNA, s_substring_fill_x);
  end = SCM_CAR (args); args = SCM_CDR (args);
  fill = SCM_CAR (args);
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_substring_fill_x);
  SCM_ASSERT (SCM_INUMP (start), start, SCM_ARG2, s_substring_fill_x);
  SCM_ASSERT (SCM_INUMP (end), end, SCM_ARG3, s_substring_fill_x);
  SCM_ASSERT (SCM_ICHRP (fill), fill, SCM_ARG4, s_substring_fill_x);
  i = SCM_INUM (start), e = SCM_INUM (end);c = SCM_ICHR (fill);
  SCM_ASSERT (i <= SCM_LENGTH (str) && i >= 0, start, SCM_OUTOFRANGE, s_substring_fill_x);
  SCM_ASSERT (e <= SCM_LENGTH (str) && e >= 0, end, SCM_OUTOFRANGE, s_substring_fill_x);
  while (i<e) SCM_CHARS (str)[i++] = c;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_string_null_p, "string-null?", 1, 0, 0, scm_string_null_p);
#ifdef __STDC__
SCM
scm_string_null_p (SCM str)
#else
SCM
scm_string_null_p (str)
     SCM str;
#endif
{
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_string_null_p);
  return (SCM_ROLENGTH (str)
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}


SCM_PROC(s_string_to_list, "string->list", 1, 0, 0, scm_string_to_list);
#ifdef __STDC__
SCM
scm_string_to_list (SCM str)
#else
SCM
scm_string_to_list (str)
     SCM str;
#endif
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
#ifdef __STDC__
SCM
scm_string_copy (SCM str)
#else
SCM
scm_string_copy (str)
     SCM str;
#endif
{
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_string_copy);
  return scm_makfromstr (SCM_CHARS (str), (scm_sizet)SCM_LENGTH (str), 0);
}


SCM_PROC(s_string_fill_x, "string-fill!", 2, 0, 0, scm_string_fill_x);
#ifdef __STDC__
SCM
scm_string_fill_x (SCM str, SCM chr)
#else
SCM
scm_string_fill_x (str, chr)
     SCM str;
     SCM chr;
#endif
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


#ifdef __STDC__
void
scm_init_strop (void)
#else
void
scm_init_strop ()
#endif
{
#include "strop.x"
}

