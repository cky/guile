/*	Copyright (C) 1997, 1998 Free Software Foundation, Inc.
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


/* regex-posix.c -- POSIX regular expression support.

   This code was written against Henry Spencer's famous regex package.
   The principal reference for POSIX behavior was the man page for this
   library, not the 1003.2 document itself.  Ergo, other `POSIX'
   libraries which do not agree with the Spencer implementation may
   produce varying behavior.  Sigh. */

#include <stdio.h>
#include <sys/types.h>

#include "_scm.h"

/* Supposedly, this file is never compiled unless we know we have
   POSIX regular expressions.  But we still put this in an #ifdef so
   the file is CPP'able (for dependency scanning) even on systems that
   don't have a <regex.h> header.  */
#ifdef HAVE_REGCOMP
#ifdef HAVE_REGEX_H
#include <regex.h>
#else
#ifdef HAVE_RXPOSIX_H
#include <rxposix.h>		/* GNU Rx library */
#else
#ifdef HAVE_RX_RXPOSIX_H
#include <rx/rxposix.h>		/* GNU Rx library on Linux */
#endif
#endif
#endif
#endif

#include "smob.h"
#include "symbols.h"
#include "vectors.h"
#include "strports.h"
#include "ports.h"
#include "feature.h"

#include "regex-posix.h"

/* This is defined by some regex libraries and omitted by others. */
#ifndef REG_BASIC
#define REG_BASIC 0
#endif

long scm_tc16_regex;

static scm_sizet
free_regex (obj)
     SCM obj;
{
  regfree (SCM_RGX (obj));
  free (SCM_RGX (obj));
  return sizeof(regex_t);
}



SCM_SYMBOL (scm_regexp_error_key, "regular-expression-syntax");

static char *
scm_regexp_error_msg (regerrno, rx)
     int regerrno;
     SCM rx;
{
  SCM errmsg;
  int l;

  /* FIXME: must we wrap any external calls in SCM_DEFER_INTS...SCM_ALLOW_INTS?
     Or are these only necessary when a SCM object may be left in an
     undetermined state (half-formed)?  If the latter then I believe we
     may do without the critical section code. -twp */

  /* We could simply make errmsg a char pointer, and allocate space with
     malloc.  But since we are about to pass the pointer to scm_error, which
     never returns, we would never have the opportunity to free it.  Creating
     it as a SCM object means that the system will GC it at some point. */

  errmsg = scm_make_string (SCM_MAKINUM (80), SCM_UNDEFINED);
  SCM_DEFER_INTS;
  l = regerror (regerrno, SCM_RGX (rx), SCM_CHARS (errmsg), 80);
  if (l > 80)
    {
      errmsg = scm_make_string (SCM_MAKINUM (l), SCM_UNDEFINED);
      regerror (regerrno, SCM_RGX (rx), SCM_CHARS (errmsg), l);
    }
  SCM_ALLOW_INTS;
  return SCM_CHARS (errmsg);
}

SCM_PROC (s_regexp_p, "regexp?", 1, 0, 0, scm_regexp_p);

SCM
scm_regexp_p (x)
     SCM x;
{
  return (SCM_NIMP (x) && SCM_RGXP (x) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM_PROC (s_make_regexp, "make-regexp", 1, 0, 1, scm_make_regexp);

SCM
scm_make_regexp (SCM pat, SCM flags)
{
  SCM flag;
  regex_t *rx;
  int status, cflags;

  SCM_ASSERT (SCM_NIMP(pat) && SCM_ROSTRINGP(pat), pat, SCM_ARG1, 
	      s_make_regexp);
  SCM_COERCE_SUBSTR (pat);

  /* Examine list of regexp flags.  If REG_BASIC is supplied, then
     turn off REG_EXTENDED flag (on by default). */
  cflags = REG_EXTENDED;
  flag = flags;
  while (SCM_NNULLP (flag))
    {
      if (SCM_INUM (SCM_CAR (flag)) == REG_BASIC)
	cflags &= ~REG_EXTENDED;
      else
	cflags |= SCM_INUM (SCM_CAR (flag));
      flag = SCM_CDR (flag);
    }
	  
  rx = (regex_t *) scm_must_malloc (sizeof (regex_t), s_make_regexp);
  status = regcomp (rx, SCM_ROCHARS (pat),
		    /* Make sure they're not passing REG_NOSUB;
                       regexp-exec assumes we're getting match data.  */
		    cflags & ~REG_NOSUB);
  if (status != 0)
    {
      scm_error (scm_regexp_error_key,
		 s_make_regexp,
		 scm_regexp_error_msg (status, rx),
		 SCM_BOOL_F,
		 SCM_BOOL_F);
      /* never returns */
    }
  SCM_RETURN_NEWSMOB (scm_tc16_regex, rx);
}

SCM_PROC (s_regexp_exec, "regexp-exec", 2, 2, 0, scm_regexp_exec);

SCM
scm_regexp_exec (SCM rx, SCM str, SCM start, SCM flags)
{
  int status, nmatches, offset;
  regmatch_t *matches;
  SCM mvec = SCM_BOOL_F;

  SCM_ASSERT (SCM_NIMP (rx) && SCM_RGXP (rx), rx, SCM_ARG1, s_regexp_exec);
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG2,
	      s_regexp_exec);

  if (SCM_UNBNDP (start))
    offset = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (start), start, SCM_ARG3, s_regexp_exec);
      offset = SCM_INUM (start);
      SCM_ASSERT (offset >= 0 && (unsigned) offset <= SCM_LENGTH (str), start,
		  SCM_OUTOFRANGE, s_regexp_exec);
    }

  if (SCM_UNBNDP (flags))
    flags = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG2, s_regexp_exec);

  SCM_COERCE_SUBSTR (str);

  /* re_nsub doesn't account for the `subexpression' representing the
     whole regexp, so add 1 to nmatches. */

  nmatches = SCM_RGX(rx)->re_nsub + 1;
  SCM_DEFER_INTS;
  matches = (regmatch_t *) scm_must_malloc (sizeof (regmatch_t) * nmatches,
					    s_regexp_exec);
  status = regexec (SCM_RGX (rx), SCM_ROCHARS (str) + offset,
		    nmatches, matches,
		    SCM_INUM (flags));
  if (!status)
    {
      int i;
      /* The match vector must include a cell for the string that was matched,
	 so add 1. */
      mvec = scm_make_vector (SCM_MAKINUM (nmatches + 1), SCM_UNSPECIFIED);
      SCM_VELTS(mvec)[0] = str;
      for (i = 0; i < nmatches; ++i)
	if (matches[i].rm_so == -1)
	  SCM_VELTS(mvec)[i+1] = scm_cons (SCM_MAKINUM (-1), SCM_MAKINUM (-1));
	else
	  SCM_VELTS(mvec)[i+1]
	    = scm_cons(SCM_MAKINUM(matches[i].rm_so + offset),
		       SCM_MAKINUM(matches[i].rm_eo + offset));
    }
  scm_must_free ((char *) matches);
  SCM_ALLOW_INTS;

  if (status != 0 && status != REG_NOMATCH)
    scm_error (scm_regexp_error_key,
	       s_regexp_exec,
	       scm_regexp_error_msg (status),
	       SCM_BOOL_F,
	       SCM_BOOL_F);
  return mvec;
}

void
scm_init_regex_posix ()
{
  scm_tc16_regex = scm_make_smob_type_mfpe ("regexp", sizeof (regex_t),
                                            NULL, free_regex, NULL, NULL);

  /* Compilation flags.  */
  scm_sysintern ("regexp/basic", scm_long2num (REG_BASIC));
  scm_sysintern ("regexp/extended", scm_long2num (REG_EXTENDED));
  scm_sysintern ("regexp/icase", scm_long2num (REG_ICASE));
  scm_sysintern ("regexp/newline", scm_long2num (REG_NEWLINE));

  /* Execution flags.  */
  scm_sysintern ("regexp/notbol", scm_long2num (REG_NOTBOL));
  scm_sysintern ("regexp/noteol", scm_long2num (REG_NOTEOL));

#include "regex-posix.x"

  scm_add_feature ("regex");
}
