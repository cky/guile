/*	Copyright (C) 1995,1996,1998 Free Software Foundation, Inc.
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
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
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
 * If you do not wish that, delete this exception notice.  */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "_scm.h"
#include "chars.h"

#include "strings.h"
#include "scm_validate.h"


/* {Strings}
 */

SCM_DEFINE (scm_string_p, "string?", 1, 0, 0, 
           (SCM x),
"")
#define FUNC_NAME s_scm_string_p
{
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  return SCM_BOOL(SCM_STRINGP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_read_only_string_p, "read-only-string?", 1, 0, 0, 
           (SCM x),
"Return true of OBJ can be read as a string,

This illustrates the difference between @code{string?} and
@code{read-only-string?}:

@example
(string? \"a string\") @result{} #t
(string? 'a-symbol) @result{} #f

(read-only-string? \"a string\") @result{} #t
(read-only-string? 'a-symbol) @result{} #t
@end example")
#define FUNC_NAME s_scm_read_only_string_p
{
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  return SCM_BOOL(SCM_ROSTRINGP (x));
}
#undef FUNC_NAME

SCM_REGISTER_PROC(s_list_to_string, "list->string", 1, 0, 0, scm_string);


SCM_DEFINE (scm_string, "string", 0, 0, 1, 
           (SCM chrs),
"")
#define FUNC_NAME s_scm_string
{
  SCM res;
  register unsigned char *data;
  long i;
  long len;
  SCM_DEFER_INTS;
  i = scm_ilength (chrs);
  if (i < 0)
    {
      SCM_ALLOW_INTS;
      SCM_ASSERT (0, chrs, SCM_ARG1, FUNC_NAME);
    }
  len = 0;
  {
    SCM s;

    for (len = 0, s = chrs; s != SCM_EOL; s = SCM_CDR (s))
      if (SCM_ICHRP (SCM_CAR (s)))
	len += 1;
      else if (SCM_ROSTRINGP (SCM_CAR (s)))
	len += SCM_ROLENGTH (SCM_CAR (s));
      else
	{
	  SCM_ALLOW_INTS;
	  SCM_ASSERT (0, s, SCM_ARG1, FUNC_NAME);
	}
  }
  res = scm_makstr (len, 0);
  data = SCM_UCHARS (res);
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
#undef FUNC_NAME


SCM 
scm_makstr (long len, int slots)
{
  SCM s;
  SCM * mem;
  SCM_NEWCELL (s);
  --slots;
  SCM_REDEFER_INTS;
  mem = (SCM *)scm_must_malloc (sizeof (SCM) * (slots + 1) + len + 1,
				"scm_makstr");
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

SCM 
scm_makfromstrs (int argc, char **argv)
{
  int i = argc;
  SCM lst = SCM_EOL;
  if (0 > i)
    for (i = 0; argv[i]; i++);
  while (i--)
    lst = scm_cons (scm_makfromstr (argv[i], (scm_sizet) strlen (argv[i]), 0), lst);
  return lst;
}


/* This function must only be applied to memory obtained via malloc,
   since the GC is going to apply `free' to it when the string is
   dropped.

   Also, s[len] must be `\0', since we promise that strings are
   null-terminated.  Perhaps we could handle non-null-terminated
   strings by claiming they're shared substrings of a string we just
   made up.  */
SCM
scm_take_str (char *s, int len)
{
  SCM answer;
  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  SCM_SETLENGTH (answer, len, scm_tc7_string);
  scm_done_malloc (len + 1);
  SCM_SETCHARS (answer, s);
  SCM_ALLOW_INTS;
  return answer;
}

/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  return scm_take_str (s, strlen (s));
}


SCM 
scm_makfromstr (const char *src, scm_sizet len, int slots)
{
  SCM s;
  register char *dst;
  s = scm_makstr ((long) len, slots);
  dst = SCM_CHARS (s);
  while (len--)
    *dst++ = *src++;
  return s;
}



SCM 
scm_makfrom0str (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_makfromstr (src, (scm_sizet) strlen (src), 0);
}


SCM 
scm_makfrom0str_opt (const char *src)
{
  return scm_makfrom0str (src);
}




SCM_DEFINE (scm_make_string, "make-string", 1, 1, 0,
           (SCM k, SCM chr),
"")
#define FUNC_NAME s_scm_make_string
{
  SCM res;
  register long i;
  SCM_VALIDATE_INUM_MIN_COPY (1,k,0,i);
  res = scm_makstr (i, 0);
  if (!SCM_UNBNDP (chr))
    {
      SCM_VALIDATE_CHAR (2,chr);
      {
	unsigned char *dst = SCM_UCHARS (res);
	char c = SCM_ICHR (chr);
	
	memset (dst, c, i);
      }
    }
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_length, "string-length", 1, 0, 0, 
           (SCM str),
"")
#define FUNC_NAME s_scm_string_length
{
  SCM_VALIDATE_ROSTRING (1,str);
  return SCM_MAKINUM (SCM_ROLENGTH (str));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ref, "string-ref", 1, 1, 0,
           (SCM str, SCM k),
"")
#define FUNC_NAME s_scm_string_ref
{
  SCM_VALIDATE_ROSTRING (1,str);
  SCM_VALIDATE_INUM_DEF (2,k,0);
  SCM_ASSERT_RANGE (2,k,SCM_INUM (k) < SCM_ROLENGTH (str) && SCM_INUM (k) >= 0);
  return SCM_MAKICHR (SCM_ROUCHARS (str)[SCM_INUM (k)]);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_set_x, "string-set!", 3, 0, 0,
           (SCM str, SCM k, SCM chr),
"")
#define FUNC_NAME s_scm_string_set_x
{
  SCM_VALIDATE_RWSTRING (1,str);
  SCM_VALIDATE_INUM_RANGE (2,k,0,SCM_LENGTH(str));
  SCM_VALIDATE_CHAR (3,chr);
  SCM_UCHARS (str)[SCM_INUM (k)] = SCM_ICHR (chr);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_substring, "substring", 2, 1, 0,
           (SCM str, SCM start, SCM end),
"")
#define FUNC_NAME s_scm_substring
{
  long l;
  SCM_VALIDATE_ROSTRING (1,str);
  SCM_VALIDATE_INUM (2,start);
  SCM_VALIDATE_INUM_DEF (3,end,SCM_ROLENGTH(str));
  SCM_ASSERT_RANGE (2,start,SCM_INUM (start) <= SCM_ROLENGTH (str));
  SCM_ASSERT_RANGE (2,end,SCM_INUM (end) <= SCM_ROLENGTH (str));
  l = SCM_INUM (end)-SCM_INUM (start);
  SCM_ASSERT (l >= 0, SCM_MAKINUM (l), SCM_OUTOFRANGE, FUNC_NAME);
  return scm_makfromstr (&SCM_ROCHARS (str)[SCM_INUM (start)], (scm_sizet)l, 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_append, "string-append", 0, 0, 1, 
           (SCM args),
"")
#define FUNC_NAME s_scm_string_append
{
  SCM res;
  register long i = 0;
  register SCM l, s;
  register unsigned char *data;
  for (l = args;SCM_CONSP (l);) {
    s = SCM_CAR (l);
    SCM_VALIDATE_ROSTRING (SCM_ARGn,s);
    i += SCM_ROLENGTH (s);
    l = SCM_CDR (l);
  }
  SCM_ASSERT (SCM_NULLP (l), args, SCM_ARGn, FUNC_NAME);
  res = scm_makstr (i, 0);
  data = SCM_UCHARS (res);
  for (l = args;SCM_NIMP (l);l = SCM_CDR (l)) {
    s = SCM_CAR (l);
    for (i = 0;i<SCM_ROLENGTH (s);i++) *data++ = SCM_ROUCHARS (s)[i];
  }
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_shared_substring, "make-shared-substring", 1, 2, 0,
           (SCM str, SCM frm, SCM to),
"Return a shared substring of @var{str}.  The semantics are the same as
for the @code{substring} function: the shared substring returned
includes all of the text from @var{str} between indexes @var{start}
(inclusive) and @var{end} (exclusive).  If @var{end} is omitted, it
defaults to the end of @var{str}.  The shared substring returned by
@code{make-shared-substring} occupies the same storage space as
@var{str}.")
#define FUNC_NAME s_scm_make_shared_substring
{
  long f;
  long t;
  SCM answer;
  SCM len_str;

  SCM_VALIDATE_ROSTRING (1,str);
  SCM_VALIDATE_INUM_DEF_COPY (2,frm,0,f);
  SCM_VALIDATE_INUM_DEF_COPY (3,to,SCM_ROLENGTH(str),t);

  SCM_ASSERT_RANGE (2,frm,(f >= 0));
  SCM_ASSERT_RANGE (3,to, (f <= t) && (t <= SCM_ROLENGTH (str)));

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
#undef FUNC_NAME

void
scm_init_strings ()
{
#include "strings.x"
}

