/*	Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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
 * If you do not wish that, delete this exception notice.
 */




/* regex-posix.c -- POSIX regular expression support.

   This code was written against Henry Spencer's famous regex package.
   The principal reference for POSIX behavior was the man page for this
   library, not the 1003.2 document itself.  Ergo, other `POSIX'
   libraries which do not agree with the Spencer implementation may
   produce varying behavior.  Sigh. */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>

#include "libguile/_scm.h"

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

#include "libguile/smob.h"
#include "libguile/symbols.h"
#include "libguile/vectors.h"
#include "libguile/strports.h"
#include "libguile/ports.h"
#include "libguile/feature.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/regex-posix.h"

/* This is defined by some regex libraries and omitted by others. */
#ifndef REG_BASIC
#define REG_BASIC 0
#endif

scm_t_bits scm_tc16_regex;

static size_t
regex_free (SCM obj)
{
  regfree (SCM_RGX (obj));
  scm_gc_free (SCM_RGX (obj), sizeof(regex_t), "regex");
  return 0;
}



SCM_SYMBOL (scm_regexp_error_key, "regular-expression-syntax");

static char *
scm_regexp_error_msg (int regerrno, regex_t *rx)
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
  l = regerror (regerrno, rx, SCM_STRING_CHARS (errmsg), 80);
  if (l > 80)
    {
      errmsg = scm_make_string (SCM_MAKINUM (l), SCM_UNDEFINED);
      regerror (regerrno, rx, SCM_STRING_CHARS (errmsg), l);
    }
  SCM_ALLOW_INTS;
  return SCM_STRING_CHARS (errmsg);
}

SCM_DEFINE (scm_regexp_p, "regexp?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a compiled regular expression,\n"
	    "or @code{#f} otherwise.")
#define FUNC_NAME s_scm_regexp_p
{
  return SCM_BOOL(SCM_RGXP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_regexp, "make-regexp", 1, 0, 1,
            (SCM pat, SCM flags),
	    "Compile the regular expression described by @var{pat}, and\n"
	    "return the compiled regexp structure.  If @var{pat} does not\n"
	    "describe a legal regular expression, @code{make-regexp} throws\n"
	    "a @code{regular-expression-syntax} error.\n"
	    "\n"
	    "The @var{flags} arguments change the behavior of the compiled\n"
	    "regular expression.  The following flags may be supplied:\n"
	    "\n"
	    "@table @code\n"
	    "@item regexp/icase\n"
	    "Consider uppercase and lowercase letters to be the same when\n"
	    "matching.\n"
	    "@item regexp/newline\n"
	    "If a newline appears in the target string, then permit the\n"
	    "@samp{^} and @samp{$} operators to match immediately after or\n"
	    "immediately before the newline, respectively.  Also, the\n"
	    "@samp{.} and @samp{[^...]} operators will never match a newline\n"
	    "character.  The intent of this flag is to treat the target\n"
	    "string as a buffer containing many lines of text, and the\n"
	    "regular expression as a pattern that may match a single one of\n"
	    "those lines.\n"
	    "@item regexp/basic\n"
	    "Compile a basic (``obsolete'') regexp instead of the extended\n"
	    "(``modern'') regexps that are the default.  Basic regexps do\n"
	    "not consider @samp{|}, @samp{+} or @samp{?} to be special\n"
	    "characters, and require the @samp{@{...@}} and @samp{(...)}\n"
	    "metacharacters to be backslash-escaped (@pxref{Backslash\n"
	    "Escapes}).  There are several other differences between basic\n"
	    "and extended regular expressions, but these are the most\n"
	    "significant.\n"
	    "@item regexp/extended\n"
	    "Compile an extended regular expression rather than a basic\n"
	    "regexp.  This is the default behavior; this flag will not\n"
	    "usually be needed.  If a call to @code{make-regexp} includes\n"
	    "both @code{regexp/basic} and @code{regexp/extended} flags, the\n"
	    "one which comes last will override the earlier one.\n"
	    "@end table")
#define FUNC_NAME s_scm_make_regexp
{
  SCM flag;
  regex_t *rx;
  int status, cflags;

  SCM_VALIDATE_STRING (1, pat);
  SCM_VALIDATE_REST_ARGUMENT (flags);

  /* Examine list of regexp flags.  If REG_BASIC is supplied, then
     turn off REG_EXTENDED flag (on by default). */
  cflags = REG_EXTENDED;
  flag = flags;
  while (!SCM_NULLP (flag))
    {
      if (SCM_INUM (SCM_CAR (flag)) == REG_BASIC)
	cflags &= ~REG_EXTENDED;
      else
	cflags |= SCM_INUM (SCM_CAR (flag));
      flag = SCM_CDR (flag);
    }

  rx = scm_gc_malloc (sizeof(regex_t), "regex");
  status = regcomp (rx, SCM_STRING_CHARS (pat),
		    /* Make sure they're not passing REG_NOSUB;
                       regexp-exec assumes we're getting match data.  */
		    cflags & ~REG_NOSUB);
  if (status != 0)
    {
      scm_error (scm_regexp_error_key,
		 FUNC_NAME,
		 scm_regexp_error_msg (status, rx),
		 SCM_BOOL_F,
		 SCM_BOOL_F);
      /* never returns */
    }
  SCM_RETURN_NEWSMOB (scm_tc16_regex, rx);
}
#undef FUNC_NAME

SCM_DEFINE (scm_regexp_exec, "regexp-exec", 2, 2, 0,
            (SCM rx, SCM str, SCM start, SCM flags),
	    "Match the compiled regular expression @var{rx} against\n"
	    "@code{str}.  If the optional integer @var{start} argument is\n"
	    "provided, begin matching from that position in the string.\n"
	    "Return a match structure describing the results of the match,\n"
	    "or @code{#f} if no match could be found.\n"
            "\n"
            "The @var{flags} arguments change the matching behavior.\n"
            "The following flags may be supplied:\n"
	    "\n"
	    "@table @code\n"
	    "@item regexp/notbol\n"
            "Operator @samp{^} always fails (unless @code{regexp/newline}\n"
            "is used).  Use this when the beginning of the string should\n"
            "not be considered the beginning of a line.\n"
	    "@item regexp/noteol\n"
            "Operator @samp{$} always fails (unless @code{regexp/newline}\n"
            "is used).  Use this when the end of the string should not be\n"
            "considered the end of a line.\n"
            "@end table")
#define FUNC_NAME s_scm_regexp_exec
{
  int status, nmatches, offset;
  regmatch_t *matches;
  SCM mvec = SCM_BOOL_F;

  SCM_VALIDATE_RGXP (1, rx);
  SCM_VALIDATE_STRING (2, str);
  SCM_VALIDATE_INUM_DEF_COPY (3, start,0, offset);
  SCM_ASSERT_RANGE (3, start, offset >= 0 && offset <= SCM_STRING_LENGTH (str));
  if (SCM_UNBNDP (flags))
    flags = SCM_INUM0;
  SCM_VALIDATE_INUM (4, flags);

  /* re_nsub doesn't account for the `subexpression' representing the
     whole regexp, so add 1 to nmatches. */

  nmatches = SCM_RGX(rx)->re_nsub + 1;
  SCM_DEFER_INTS;
  matches = scm_malloc (sizeof (regmatch_t) * nmatches);
  status = regexec (SCM_RGX (rx), SCM_STRING_CHARS (str) + offset,
		    nmatches, matches,
		    SCM_INUM (flags));
  if (!status)
    {
      int i;
      /* The match vector must include a cell for the string that was matched,
	 so add 1. */
      mvec = scm_c_make_vector (nmatches + 1, SCM_UNSPECIFIED);
      SCM_VECTOR_SET(mvec,0, str);
      for (i = 0; i < nmatches; ++i)
	if (matches[i].rm_so == -1)
	  SCM_VECTOR_SET(mvec,i+1, scm_cons (SCM_MAKINUM (-1), SCM_MAKINUM (-1)));
	else
	  SCM_VECTOR_SET(mvec,i+1,scm_cons (scm_long2num (matches[i].rm_so + offset),
				       scm_long2num (matches[i].rm_eo + offset)));
    }
  free (matches);
  SCM_ALLOW_INTS;

  if (status != 0 && status != REG_NOMATCH)
    scm_error (scm_regexp_error_key,
	       FUNC_NAME,
	       scm_regexp_error_msg (status, SCM_RGX (rx)),
	       SCM_BOOL_F,
	       SCM_BOOL_F);
  return mvec;
}
#undef FUNC_NAME

void
scm_init_regex_posix ()
{
  scm_tc16_regex = scm_make_smob_type ("regexp", sizeof (regex_t));
  scm_set_smob_free (scm_tc16_regex, regex_free);

  /* Compilation flags.  */
  scm_c_define ("regexp/basic", scm_long2num (REG_BASIC));
  scm_c_define ("regexp/extended", scm_long2num (REG_EXTENDED));
  scm_c_define ("regexp/icase", scm_long2num (REG_ICASE));
  scm_c_define ("regexp/newline", scm_long2num (REG_NEWLINE));

  /* Execution flags.  */
  scm_c_define ("regexp/notbol", scm_long2num (REG_NOTBOL));
  scm_c_define ("regexp/noteol", scm_long2num (REG_NOTEOL));

#include "libguile/regex-posix.x"

  scm_add_feature ("regex");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
