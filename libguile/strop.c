/* classes: src_files */

/*	Copyright (C) 1994, 1996, 1997 Free Software Foundation, Inc.

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



static int scm_i_index SCM_P ((SCM * str, SCM chr, int direction, SCM sub_start, SCM sub_end, int pos, int pos2, int pos3, int pos4, char * why));

/* implements index if direction > 0 otherwise rindex.  */
static int
scm_i_index (str, chr, direction, sub_start, sub_end, pos, pos2, pos3, pos4,
	     why)
     SCM * str;
     SCM chr;
     int direction;
     SCM sub_start;
     SCM sub_end;
     int pos;
     int pos2;
     int pos3;
     int pos4;
     char * why;
{
  unsigned char * p;
  int x;
  int lower;
  int upper;
  int ch;

  SCM_ASSERT (SCM_NIMP (*str) && SCM_ROSTRINGP (*str), *str, pos, why);
  SCM_ASSERT (SCM_ICHRP (chr), chr, pos2, why);

  if (sub_start == SCM_BOOL_F)
    sub_start = SCM_MAKINUM (0);

  SCM_ASSERT (SCM_INUMP (sub_start), sub_start, pos3, why);
  lower = SCM_INUM (sub_start);
  if (lower < 0
      || lower > SCM_ROLENGTH (*str))
    scm_out_of_range (why, sub_start);

  if (sub_end == SCM_BOOL_F)
    sub_end = SCM_MAKINUM (SCM_ROLENGTH (*str));

  SCM_ASSERT (SCM_INUMP (sub_end), sub_end, pos4, why);
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
scm_string_index (str, chr, frm, to)
     SCM str;
     SCM chr;
     SCM frm;
     SCM to;
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, 1, frm, to, SCM_ARG1, SCM_ARG2, SCM_ARG3, SCM_ARG4, s_string_index);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}

SCM_PROC(s_string_rindex, "string-rindex", 2, 2, 0, scm_string_rindex);

SCM 
scm_string_rindex (str, chr, frm, to)
     SCM str;
     SCM chr;
     SCM frm;
     SCM to;
{
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, -1, frm, to, SCM_ARG1, SCM_ARG2, SCM_ARG3, SCM_ARG4, s_string_rindex);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}

/* What is the purpose of these strange assertions in the following
   `substring' functions?

  SCM_ASSERT (3==scm_ilength (args), scm_makfrom0str (s_substring_move_left_x),
	      SCM_WNA, NULL);

   Why bother to make args a `rest argument' if we are merely going to
   force it to include exactly three arguments?  Why not merely make
   them all required arguments instead?  This makes me suspicious that
   the functions haven't been fully implemented.  If anyone can
   clarify what's going on here, please do so. -twp */

SCM_PROC(s_substring_move_left_x, "substring-move-left!", 2, 0, 1, scm_substring_move_left_x);

SCM
scm_substring_move_left_x (str1, start1, args)
     SCM str1;
     SCM start1;
     SCM args;
{
  SCM end1, str2, start2;
  long i, j, e;
  SCM_ASSERT (3==scm_ilength (args), scm_makfrom0str (s_substring_move_left_x),
	      SCM_WNA, NULL);
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

SCM
scm_substring_move_right_x (str1, start1, args)
     SCM str1;
     SCM start1;
     SCM args;
{
  SCM end1, str2, start2;
  long i, j, e;
  SCM_ASSERT (3==scm_ilength (args),
	      scm_makfrom0str (s_substring_move_right_x), SCM_WNA, NULL);
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

SCM
scm_substring_fill_x (str, start, args)
     SCM str;
     SCM start;
     SCM args;
{
  SCM end, fill;
  long i, e;
  char c;
  SCM_ASSERT (2==scm_ilength (args),  scm_makfrom0str (s_substring_fill_x),
	      SCM_WNA, NULL);
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

SCM_PROC(s_string_downcase_x, "string-downcase!", 1, 0, 0, scm_string_downcase_x);

SCM 
scm_string_downcase_x (v)
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
	cs[k] = scm_downcase(cs[k]);
      break;
    default:
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_string_downcase_x);
    }
  return v;
}


void
scm_init_strop ()
{
#include "strop.x"
}

