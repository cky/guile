/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"
#include "chars.h"

#include "strings.h"


/* {Strings}
 */

SCM_PROC(s_string_p, "string?", 1, 0, 0, scm_string_p);
#ifdef __STDC__
SCM
scm_string_p (SCM x)
#else
SCM
scm_string_p (x)
     SCM x;
#endif
{
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  return SCM_STRINGP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_read_only_string_p, "read-only-string?", 1, 0, 0, scm_read_only_string_p);
#ifdef __STDC__
SCM
scm_read_only_string_p (SCM x)
#else
SCM
scm_read_only_string_p (x)
     SCM x;
#endif
{
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  return SCM_ROSTRINGP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_list_to_string, "list->string", 1, 0, 0, scm_string);
SCM_PROC(s_string, "string", 0, 0, 1, scm_string);
#ifdef __STDC__
SCM
scm_string (SCM chrs)
#else
SCM
scm_string (chrs)
     SCM chrs;
#endif
{
  SCM res;
  register char *data;
  long i;
  long len;
  SCM_DEFER_INTS;
  i = scm_ilength (chrs);
  if (i < 0)
    {
      SCM_ALLOW_INTS;
      SCM_ASSERT (0, chrs, SCM_ARG1, s_string);
    }
  len = 0;
  {
    SCM s;

    for (len = 0, s = chrs; s != SCM_EOL; s = SCM_CDR (s))
      if (SCM_ICHRP (SCM_CAR (s)))
	len += 1;
      else if (SCM_NIMP (SCM_CAR (s)) && SCM_ROSTRINGP (SCM_CAR (s)))
	len += SCM_ROLENGTH (SCM_CAR (s));
      else
	{
	  SCM_ALLOW_INTS;
	  SCM_ASSERT (0, s, SCM_ARG1, s_string);
	}
  }
  res = scm_makstr (len, 0);
  data = SCM_CHARS (res);
  for (;SCM_NNULLP (chrs);chrs = SCM_CDR (chrs))
    {
      if (SCM_ICHRP (SCM_CAR (chrs)))
	*data++ = SCM_ICHR (SCM_CAR (chrs));
      else
	{
	  int l;
	  char * c;
	  l = SCM_ROLENGTH (SCM_CAR (chrs));
	  c = SCM_ROCHARS (SCM_CAR (chrs));
	  while (l)
	    {
	      --l;
	      *data++ = *c++;
	    }
	}
    }
  SCM_ALLOW_INTS;
  return res;
}

#ifdef __STDC__
SCM 
scm_makstr (long len, int slots)
#else
SCM 
scm_makstr (len, slots)
     long len;
     int slots;
#endif
{
  SCM s;
  SCM * mem;
  SCM_NEWCELL (s);
  --slots;
  SCM_REDEFER_INTS;
  mem = (SCM *)scm_must_malloc (sizeof (SCM) * (slots + 1) + len + 1,
				s_string);
  if (slots >= 0)
    {
      int x;
      mem[slots] = (SCM)mem;
      for (x = 0; x < slots; ++x)
	mem[x] = SCM_BOOL_F;
    }
  SCM_SETCHARS (s, (char *) (mem + slots + 1));
  SCM_SETLENGTH (s, len, scm_tc7_string);
  SCM_REALLOW_INTS;
  SCM_CHARS (s)[len] = 0;
  return s;
}

/* converts C scm_array of strings to SCM scm_list of strings. */
/* If argc < 0, a null terminated scm_array is assumed. */
#ifdef __STDC__
SCM 
scm_makfromstrs (int argc, char **argv)
#else
SCM 
scm_makfromstrs (argc, argv)
     int argc;
     char **argv;
#endif
{
  int i = argc;
  SCM lst = SCM_EOL;
  if (0 > i)
    for (i = 0; argv[i]; i++);
  while (i--)
    lst = scm_cons (scm_makfromstr (argv[i], (scm_sizet) strlen (argv[i]), 0), lst);
  return lst;
}


#ifdef __STDC__
SCM
scm_take0str (char * it)
#else
SCM
scm_take0str (it)
     char * it;
#endif
{
  SCM answer;
  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  SCM_SETLENGTH (answer, strlen (it), scm_tc7_string);
  SCM_SETCHARS (answer, it);
  SCM_ALLOW_INTS;
  return answer;
}

#ifdef __STDC__
SCM 
scm_makfromstr (const char *src, scm_sizet len, int slots)
#else
SCM 
scm_makfromstr (src, len, slots)
     const char *src;
     scm_sizet len;
     int slots;
#endif
{
  SCM s;
  register char *dst;
  s = scm_makstr ((long) len, slots);
  dst = SCM_CHARS (s);
  while (len--)
    *dst++ = *src++;
  return s;
}


#ifdef __STDC__
SCM 
scm_makfrom0str (char *src)
#else
SCM 
scm_makfrom0str (src)
     char *src;
#endif
{
  if (!src) return SCM_BOOL_F;
  return scm_makfromstr (src, (scm_sizet) strlen (src), 0);
}

#ifdef __STDC__
SCM 
scm_makfrom0str_opt (char *src)
#else
SCM 
scm_makfrom0str_opt (src)
     char *src;
#endif
{
  return scm_makfrom0str (src);
}




SCM_PROC(s_make_string, "make-string", 1, 1, 0, scm_make_string);
#ifdef __STDC__
SCM
scm_make_string (SCM k, SCM chr)
#else
SCM
scm_make_string (k, chr)
     SCM k;
     SCM chr;
#endif
{
  SCM res;
  register char *dst;
  register long i;
  SCM_ASSERT (SCM_INUMP (k) && (k >= 0), k, SCM_ARG1, s_make_string);
  i = SCM_INUM (k);
  res = scm_makstr (i, 0);
  dst = SCM_CHARS (res);
  if SCM_ICHRP (chr)
    {
      char c = SCM_ICHR (chr);
      for (i--;i >= 0;i--)
	{
	  dst[i] = c;
	}
    }
  return res;
}

SCM_PROC(s_string_length, "string-length", 1, 0, 0, scm_string_length);
#ifdef __STDC__
SCM
scm_string_length (SCM str)
#else
SCM
scm_string_length (str)
     SCM str;
#endif
{
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_string_length);
  return SCM_MAKINUM (SCM_ROLENGTH (str));
}

SCM_PROC(s_string_ref, "string-ref", 1, 1, 0, scm_string_ref);
#ifdef __STDC__
SCM
scm_string_ref (SCM str, SCM k)
#else
SCM
scm_string_ref (str, k)
     SCM str;
     SCM k;
#endif
{
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_string_ref);
  if (k == SCM_UNDEFINED)
    k = SCM_MAKINUM (0);
  SCM_ASSERT (SCM_INUMP (k), k, SCM_ARG2, s_string_ref);
  SCM_ASSERT (SCM_INUM (k) < SCM_ROLENGTH (str) && SCM_INUM (k) >= 0, k, SCM_OUTOFRANGE, s_string_ref);
  return SCM_MAKICHR (SCM_ROCHARS (str)[SCM_INUM (k)]);
}

SCM_PROC(s_string_set_x, "string-set!", 3, 0, 0, scm_string_set_x);
#ifdef __STDC__
SCM
scm_string_set_x (SCM str, SCM k, SCM chr)
#else
SCM
scm_string_set_x (str, k, chr)
     SCM str;
     SCM k;
     SCM chr;
#endif
{
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG1, s_string_set_x);
  SCM_ASSERT (SCM_INUMP (k), k, SCM_ARG2, s_string_set_x);
  SCM_ASSERT (SCM_ICHRP (chr), chr, SCM_ARG3, s_string_set_x);
  SCM_ASSERT (SCM_INUM (k) < SCM_LENGTH (str) && SCM_INUM (k) >= 0, k, SCM_OUTOFRANGE, s_string_set_x);
  SCM_CHARS (str)[SCM_INUM (k)] = SCM_ICHR (chr);
  return SCM_UNSPECIFIED;
}



SCM_PROC(s_substring, "substring", 2, 1, 0, scm_substring);
#ifdef __STDC__
SCM
scm_substring (SCM str, SCM start, SCM end)
#else
SCM
scm_substring (str, start, end)
     SCM str;
     SCM start;
     SCM end;
#endif
{
  long l;
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str),
	 str, SCM_ARG1, s_substring);
  SCM_ASSERT (SCM_INUMP (start), start, SCM_ARG2, s_substring);
  if (end == SCM_UNDEFINED)
    end = SCM_MAKINUM (SCM_ROLENGTH (str));
  SCM_ASSERT (SCM_INUMP (end), end, SCM_ARG3, s_substring);
  SCM_ASSERT (SCM_INUM (start) <= SCM_ROLENGTH (str), start, SCM_OUTOFRANGE, s_substring);
  SCM_ASSERT (SCM_INUM (end) <= SCM_ROLENGTH (str), end, SCM_OUTOFRANGE, s_substring);
  l = SCM_INUM (end)-SCM_INUM (start);
  SCM_ASSERT (l >= 0, SCM_MAKINUM (l), SCM_OUTOFRANGE, s_substring);
  return scm_makfromstr (&SCM_ROCHARS (str)[SCM_INUM (start)], (scm_sizet)l, 0);
}

SCM_PROC(s_string_append, "string-append", 0, 0, 1, scm_string_append);
#ifdef __STDC__
SCM
scm_string_append (SCM args)
#else
SCM
scm_string_append (args)
     SCM args;
#endif
{
  SCM res;
  register long i = 0;
  register SCM l, s;
  register char *data;
  for (l = args;SCM_NIMP (l);) {
    SCM_ASSERT (SCM_CONSP (l), l, SCM_ARGn, s_string_append);
    s = SCM_CAR (l);
    SCM_ASSERT (SCM_NIMP (s) && SCM_ROSTRINGP (s),
	   s, SCM_ARGn, s_string_append);
    i += SCM_ROLENGTH (s);
    l = SCM_CDR (l);
  }
  SCM_ASSERT (SCM_NULLP (l), args, SCM_ARGn, s_string_append);
  res = scm_makstr (i, 0);
  data = SCM_CHARS (res);
  for (l = args;SCM_NIMP (l);l = SCM_CDR (l)) {
    s = SCM_CAR (l);
    for (i = 0;i<SCM_ROLENGTH (s);i++) *data++ = SCM_ROCHARS (s)[i];
  }
  return res;
}

SCM_PROC(s_make_shared_substring, "make-shared-substring", 1, 2, 0, scm_make_shared_substring);
#ifdef __STDC__
SCM
scm_make_shared_substring (SCM str, SCM frm, SCM to)
#else
SCM
scm_make_shared_substring (str, frm, to)
     SCM str;
     SCM frm;
     SCM to;
#endif
{
  long f;
  long t;
  SCM answer;
  SCM len_str;

  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_make_shared_substring);

  if (frm == SCM_UNDEFINED)
    frm = SCM_MAKINUM (0);
  else
    SCM_ASSERT (SCM_INUMP (frm), frm, SCM_ARG2, s_make_shared_substring);

  if (to == SCM_UNDEFINED)
    to = SCM_MAKINUM (SCM_ROLENGTH (str));
  else
    SCM_ASSERT (SCM_INUMP (to), to, SCM_ARG3, s_make_shared_substring);

  f = SCM_INUM (frm);
  t = SCM_INUM (to);
  SCM_ASSERT ((f >= 0), frm, SCM_OUTOFRANGE, s_make_shared_substring);
  SCM_ASSERT ((f <= t) && (t <= SCM_ROLENGTH (str)), to, SCM_OUTOFRANGE, s_make_shared_substring);

  SCM_NEWCELL (answer);
  SCM_NEWCELL (len_str);

  SCM_DEFER_INTS;
  if (SCM_SUBSTRP (str))
    {
      long offset;
      offset = SCM_INUM (SCM_SUBSTR_OFFSET (str));
      f += offset;
      t += offset;
      SCM_SETCAR (len_str, SCM_MAKINUM (f));
      SCM_SETCDR (len_str, SCM_SUBSTR_STR (str));
      SCM_SETCDR (answer, len_str);
      SCM_SETLENGTH (answer, t - f, scm_tc7_substring);
    }
  else
    {
      SCM_SETCAR (len_str, SCM_MAKINUM (f));
      SCM_SETCDR (len_str, str);
      SCM_SETCDR (answer, len_str);
      SCM_SETLENGTH (answer, t - f, scm_tc7_substring);
    }
  SCM_ALLOW_INTS;
  return answer;
}

#ifdef __STDC__
void
scm_init_strings (void)
#else
void
scm_init_strings ()
#endif
{
#include "strings.x"
}

