/* Copyright (C) 1995,1996,1997,1998, 1999, 2000 Free Software Foundation, Inc.
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
#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/stime.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   ifdef HAVE_TIME_H
#    include <time.h>
#   endif
#  endif
# endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#ifdef HAVE_SYS_TIMEB_H
# include <sys/timeb.h>
#endif

#ifndef tzname /* For SGI.  */
extern char *tzname[]; /* RS6000 and others reject char **tzname.  */
#endif

#ifdef MISSING_STRPTIME_DECL
extern char *strptime ();
#endif

/* This should be figured out by autoconf.  */
#if ! defined(CLKTCK) && defined(CLK_TCK)
#  define CLKTCK CLK_TCK
#endif
#if ! defined(CLKTCK) && defined(CLOCKS_PER_SEC)
#  define CLKTCK CLOCKS_PER_SEC
#endif
#if ! defined(CLKTCK)
#  define CLKTCK 60
#endif

#ifdef __STDC__
# define timet time_t
#else
# define timet long
#endif

#ifdef HAVE_TIMES
static
timet mytime()
{
  struct tms time_buffer;
  times(&time_buffer);
  return time_buffer.tms_utime + time_buffer.tms_stime;
}
#else
# ifdef LACK_CLOCK
#    define mytime() ((time((timet*)0) - scm_your_base) * CLKTCK)
# else
#  define mytime clock
# endif
#endif

extern int errno;

#ifdef HAVE_FTIME
struct timeb scm_your_base = {0};
#else
timet scm_your_base = 0;
#endif

SCM_DEFINE (scm_get_internal_real_time, "get-internal-real-time", 0, 0, 0,
           (),
	    "Returns the number of time units since the interpreter was started.")
#define FUNC_NAME s_scm_get_internal_real_time
{
#ifdef HAVE_FTIME
  struct timeb time_buffer;

  SCM tmp;
  ftime (&time_buffer);
  time_buffer.time -= scm_your_base.time;
  tmp = scm_long2num (time_buffer.millitm - scm_your_base.millitm);
  tmp = scm_sum (tmp,
		 scm_product (SCM_MAKINUM (1000),
			      SCM_MAKINUM (time_buffer.time)));
  return scm_quotient (scm_product (tmp, SCM_MAKINUM (CLKTCK)),
		       SCM_MAKINUM (1000));
#else
  return scm_long2num((time((timet*)0) - scm_your_base) * (int)CLKTCK);
#endif /* HAVE_FTIME */
}
#undef FUNC_NAME


#ifdef HAVE_TIMES
SCM_DEFINE (scm_times, "times", 0, 0, 0,
            (void),
	    "Returns an object with information about real and processor time.\n"
	    "The following procedures accept such an object as an argument and\n"
	    "return a selected component:\n\n"
	    "@table @code\n"
	    "@item tms:clock\n"
	    "The current real time, expressed as time units relative to an\n"
	    "arbitrary base.\n"
	    "@item tms:utime\n"
	    "The CPU time units used by the calling process.\n"
	    "@item tms:stime\n"
	    "The CPU time units used by the system on behalf of the calling process.\n"
	    "@item tms:cutime\n"
	    "The CPU time units used by terminated child processes of the calling\n"
	    "process, whose status has been collected (e.g., using @code{waitpid}).\n"
	    "@item tms:cstime\n"
	    "Similarly, the CPU times units used by the system on behalf of \n"
	    "terminated child processes.\n"
	    "@end table")
#define FUNC_NAME s_scm_times
{
  struct tms t;
  clock_t rv;

  SCM result = scm_make_vector (SCM_MAKINUM(5), SCM_UNDEFINED);
  rv = times (&t);
  if (rv == -1)
    SCM_SYSERROR;
  SCM_VELTS (result)[0] = scm_long2num (rv);
  SCM_VELTS (result)[1] = scm_long2num (t.tms_utime);
  SCM_VELTS (result)[2] = scm_long2num (t.tms_stime);
  SCM_VELTS (result)[3] = scm_long2num (t.tms_cutime);
  SCM_VELTS (result)[4] = scm_long2num (t.tms_cstime);
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_TIMES */

static long scm_my_base = 0;

long
scm_c_get_internal_run_time ()
{
  return mytime () - scm_my_base;
}

SCM_DEFINE (scm_get_internal_run_time, "get-internal-run-time", 0, 0, 0,
           (void),
	    "Returns the number of time units of processor time used by the interpreter.\n"
	    "Both \"system\" and \"user\" time are included but subprocesses are not.")
#define FUNC_NAME s_scm_get_internal_run_time
{
  return scm_long2num (scm_c_get_internal_run_time ());
}
#undef FUNC_NAME

SCM_DEFINE (scm_current_time, "current-time", 0, 0, 0,
           (void),
	    "Returns the number of seconds since 1970-01-01 00:00:00 UTC, excluding\n"
	    "leap seconds.")
#define FUNC_NAME s_scm_current_time
{
  timet timv;

  SCM_DEFER_INTS;
  if ((timv = time (0)) == -1)
    SCM_SYSERROR;
  SCM_ALLOW_INTS;
  return scm_long2num((long) timv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gettimeofday, "gettimeofday", 0, 0, 0,
            (void),
	    "Returns a pair containing the number of seconds and microseconds since\n"
	    "1970-01-01 00:00:00 UTC, excluding leap seconds.  Note: whether true\n"
	    "microsecond resolution is available depends on the operating system.")
#define FUNC_NAME s_scm_gettimeofday
{
#ifdef HAVE_GETTIMEOFDAY
  struct timeval time;

  SCM_DEFER_INTS;
  if (gettimeofday (&time, NULL) == -1)
    SCM_SYSERROR;
  SCM_ALLOW_INTS;
  return scm_cons (scm_long2num ((long) time.tv_sec),
		   scm_long2num ((long) time.tv_usec));
#else
# ifdef HAVE_FTIME
  struct timeb time;

  ftime(&time);
  return scm_cons (scm_long2num ((long) time.time),
		   SCM_MAKINUM (time.millitm * 1000));
# else
  timet timv;

  SCM_DEFER_INTS;
  if ((timv = time (0)) == -1)
    SCM_SYSERROR;
  SCM_ALLOW_INTS;
  return scm_cons (scm_long2num (timv), SCM_MAKINUM (0));
# endif
#endif
}
#undef FUNC_NAME

static SCM
filltime (struct tm *bd_time, int zoff, char *zname)
{
  SCM result = scm_make_vector (SCM_MAKINUM(11), SCM_UNDEFINED);

  SCM_VELTS (result)[0] = SCM_MAKINUM (bd_time->tm_sec);
  SCM_VELTS (result)[1] = SCM_MAKINUM (bd_time->tm_min);
  SCM_VELTS (result)[2] = SCM_MAKINUM (bd_time->tm_hour);
  SCM_VELTS (result)[3] = SCM_MAKINUM (bd_time->tm_mday);
  SCM_VELTS (result)[4] = SCM_MAKINUM (bd_time->tm_mon);
  SCM_VELTS (result)[5] = SCM_MAKINUM (bd_time->tm_year);
  SCM_VELTS (result)[6] = SCM_MAKINUM (bd_time->tm_wday);
  SCM_VELTS (result)[7] = SCM_MAKINUM (bd_time->tm_yday);
  SCM_VELTS (result)[8] = SCM_MAKINUM (bd_time->tm_isdst);
  SCM_VELTS (result)[9] = SCM_MAKINUM (zoff);
  SCM_VELTS (result)[10] = zname ? scm_makfrom0str (zname) : SCM_BOOL_F;
  return result;
}

static char tzvar[3] = "TZ";
extern char ** environ;

/* if zone is set, create a temporary environment with only a TZ
   string.  other threads or interrupt handlers shouldn't be allowed
   to run until the corresponding restorezone is called.  hence the use
   of a static variable for tmpenv is no big deal.  */
static char **
setzone (SCM zone, int pos, const char *subr)
{
  char **oldenv = 0;

  if (!SCM_UNBNDP (zone))
    {
      static char *tmpenv[2];
      char *buf;

      SCM_ASSERT (SCM_STRINGP (zone), zone, pos, subr);
      SCM_COERCE_SUBSTR (zone);
      buf = scm_must_malloc (SCM_STRING_LENGTH (zone) + sizeof (tzvar) + 1, subr);
      sprintf (buf, "%s=%s", tzvar, SCM_ROCHARS (zone));
      oldenv = environ;
      tmpenv[0] = buf;
      tmpenv[1] = 0;
      environ = tmpenv;
    }
  return oldenv;
}

static void
restorezone (SCM zone, char **oldenv, const char *subr)
{
  if (!SCM_UNBNDP (zone))
    {
      scm_must_free (environ[0]);
      environ = oldenv;
#ifdef HAVE_TZSET
      /* for the possible benefit of user code linked with libguile.  */
      tzset();
#endif
    }
}

SCM_DEFINE (scm_localtime, "localtime", 1, 1, 0,
            (SCM time, SCM zone),
	    "Returns an object representing the broken down components of @var{time},\n"
	    "an integer like the one returned by @code{current-time}.  The time zone\n"
	    "for the calculation is optionally specified by @var{zone} (a string),\n"
	    "otherwise the @code{TZ} environment variable or the system default is\n"
	    "used.")
#define FUNC_NAME s_scm_localtime
{
  timet itime;
  struct tm *ltptr, lt, *utc;
  SCM result;
  int zoff;
  char *zname = 0;
  char **oldenv;
  int err;

  itime = SCM_NUM2LONG (1,time);

  /* deferring interupts is essential since a) setzone may install a temporary
     environment b) localtime uses a static buffer.  */
  SCM_DEFER_INTS;
  oldenv = setzone (zone, SCM_ARG2, FUNC_NAME);
#ifdef LOCALTIME_CACHE
  tzset ();
#endif
  ltptr = localtime (&itime);
  err = errno;
  if (ltptr)
    {
      const char *ptr;

      /* copy zone name before calling gmtime or restoring zone.  */
#if defined (HAVE_TM_ZONE)
      ptr = ltptr->tm_zone;
#elif defined (HAVE_TZNAME)
      ptr = tzname[ (ltptr->tm_isdst == 1) ? 1 : 0 ];
#else
      ptr = "";
#endif
      zname = SCM_MUST_MALLOC (strlen (ptr) + 1);
      strcpy (zname, ptr);
    }
  /* the struct is copied in case localtime and gmtime share a buffer.  */
  if (ltptr)
    lt = *ltptr;
  utc = gmtime (&itime);
  if (utc == NULL)
    err = errno;
  restorezone (zone, oldenv, FUNC_NAME);
  /* delayed until zone has been restored.  */
  errno = err;
  if (utc == NULL || ltptr == NULL)
    SCM_SYSERROR;

  /* calculate timezone offset in seconds west of UTC.  */
  zoff = (utc->tm_hour - lt.tm_hour) * 3600 + (utc->tm_min - lt.tm_min) * 60
    + utc->tm_sec - lt.tm_sec;
  if (utc->tm_year < lt.tm_year)
    zoff -= 24 * 60 * 60;
  else if (utc->tm_year > lt.tm_year)
    zoff += 24 * 60 * 60;
  else if (utc->tm_yday < lt.tm_yday)
    zoff -= 24 * 60 * 60;
  else if (utc->tm_yday > lt.tm_yday)
    zoff += 24 * 60 * 60;

  result = filltime (&lt, zoff, zname);
  SCM_ALLOW_INTS;
  scm_must_free (zname);
  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gmtime, "gmtime", 1, 0, 0,
            (SCM time),
	    "Returns an object representing the broken down components of @var{time},\n"
	    "an integer like the one returned by @code{current-time}.  The values\n"
	    "are calculated for UTC.")
#define FUNC_NAME s_scm_gmtime
{
  timet itime;
  struct tm *bd_time;
  SCM result;

  itime = SCM_NUM2LONG (1,time);
  SCM_DEFER_INTS;
  bd_time = gmtime (&itime);
  if (bd_time == NULL)
    SCM_SYSERROR;
  result = filltime (bd_time, 0, "GMT");
  SCM_ALLOW_INTS;
  return result;
}
#undef FUNC_NAME

/* copy time components from a Scheme object to a struct tm.  */
static void
bdtime2c (SCM sbd_time, struct tm *lt, int pos, const char *subr)
{
  SCM *velts;
  int i;

  SCM_ASSERT (SCM_VECTORP (sbd_time)
	      && SCM_VECTOR_LENGTH (sbd_time) == 11,
	      sbd_time, pos, subr);
  velts = SCM_VELTS (sbd_time);
  for (i = 0; i < 10; i++)
    {
      SCM_ASSERT (SCM_INUMP (velts[i]), sbd_time, pos, subr);
    }
  SCM_ASSERT (SCM_FALSEP (velts[10]) || SCM_STRINGP (velts[10]),
	      sbd_time, pos, subr);

  lt->tm_sec = SCM_INUM (velts[0]);
  lt->tm_min = SCM_INUM (velts[1]);
  lt->tm_hour = SCM_INUM (velts[2]);
  lt->tm_mday = SCM_INUM (velts[3]);
  lt->tm_mon = SCM_INUM (velts[4]);
  lt->tm_year = SCM_INUM (velts[5]);
  lt->tm_wday = SCM_INUM (velts[6]);
  lt->tm_yday = SCM_INUM (velts[7]);
  lt->tm_isdst = SCM_INUM (velts[8]);
#ifdef HAVE_TM_ZONE
  lt->tm_gmtoff = SCM_INUM (velts[9]);
  if (SCM_FALSEP (velts[10]))
    lt->tm_zone = NULL;
  else
    lt->tm_zone  = SCM_STRING_CHARS (velts[10]);
#endif
}

SCM_DEFINE (scm_mktime, "mktime", 1, 1, 0,
            (SCM sbd_time, SCM zone),
	    "@var{bd-time} is an object representing broken down time and @code{zone}\n"
	    "is an optional time zone specifier (otherwise the TZ environment variable\n"
	    "or the system default is used).\n\n"
	    "Returns a pair: the CAR is a corresponding\n"
	    "integer time value like that returned\n"
	    "by @code{current-time}; the CDR is a broken down time object, similar to\n"
	    "as @var{bd-time} but with normalized values.")
#define FUNC_NAME s_scm_mktime
{
  timet itime;
  struct tm lt, *utc;
  SCM result;
  int zoff;
  char *zname = 0;
  char **oldenv;
  int err;

  bdtime2c (sbd_time, &lt, SCM_ARG1, FUNC_NAME);

  SCM_DEFER_INTS;
  oldenv = setzone (zone, SCM_ARG2, FUNC_NAME);
#ifdef LOCALTIME_CACHE
  tzset ();
#endif
  itime = mktime (&lt);
  err = errno;

  if (itime != -1)
    {
      const char *ptr;

      /* copy zone name before calling gmtime or restoring the zone.  */
#if defined (HAVE_TM_ZONE)
      ptr = lt.tm_zone;
#elif defined (HAVE_TZNAME)
      ptr = tzname[ (lt.tm_isdst == 1) ? 1 : 0 ];
#else
      ptr = "";
#endif
      zname = SCM_MUST_MALLOC (strlen (ptr) + 1);
      strcpy (zname, ptr);
    }

  /* get timezone offset in seconds west of UTC.  */
  utc = gmtime (&itime);
  if (utc == NULL)
    err = errno;

  restorezone (zone, oldenv, FUNC_NAME);
  /* delayed until zone has been restored.  */
  errno = err;
  if (utc == NULL || itime == -1)
    SCM_SYSERROR;

  zoff = (utc->tm_hour - lt.tm_hour) * 3600 + (utc->tm_min - lt.tm_min) * 60
    + utc->tm_sec - lt.tm_sec;
  if (utc->tm_year < lt.tm_year)
    zoff -= 24 * 60 * 60;
  else if (utc->tm_year > lt.tm_year)
    zoff += 24 * 60 * 60;
  else if (utc->tm_yday < lt.tm_yday)
    zoff -= 24 * 60 * 60;
  else if (utc->tm_yday > lt.tm_yday)
    zoff += 24 * 60 * 60;

  result = scm_cons (scm_long2num ((long) itime),
		     filltime (&lt, zoff, zname));
  SCM_ALLOW_INTS;
  scm_must_free (zname);
  return result;
}
#undef FUNC_NAME

#ifdef HAVE_TZSET
SCM_DEFINE (scm_tzset, "tzset", 0, 0, 0,
            (void),
	    "Initialize the timezone from the TZ environment variable\n"
	    "or the system default.  It's not usually necessary to call this procedure\n"
	    "since it's done automatically by other procedures that depend on the\n"
	    "timezone.")
#define FUNC_NAME s_scm_tzset
{
  tzset();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_TZSET */

SCM_DEFINE (scm_strftime, "strftime", 2, 0, 0,
            (SCM format, SCM stime),
	    "Formats a time specification @var{time} using @var{template}.  @var{time}\n"
	    "is an object with time components in the form returned by @code{localtime}\n"
	    "or @code{gmtime}.  @var{template} is a string which can include formatting\n"
	    "specifications introduced by a @code{%} character.  The formatting of\n"
	    "month and day names is dependent on the current locale.  The value returned\n"
	    "is the formatted string.\n"
	    "@xref{Formatting Date and Time, , , libc, The GNU C Library Reference Manual}.)")
#define FUNC_NAME s_scm_strftime
{
  struct tm t;

  char *tbuf;
  int size = 50;
  char *fmt, *myfmt;
  int len;
  SCM result;

  SCM_VALIDATE_ROSTRING (1,format);
  bdtime2c (stime, &t, SCM_ARG2, FUNC_NAME);

  SCM_COERCE_SUBSTR (format);
  fmt = SCM_ROCHARS (format);
  len = SCM_ROLENGTH (format);

  /* Ugly hack: strftime can return 0 if its buffer is too small,
     but some valid time strings (e.g. "%p") can sometimes produce
     a zero-byte output string!  Workaround is to prepend a junk
     character to the format string, so that valid returns are always
     nonzero. */
  myfmt = SCM_MUST_MALLOC (len+2);
  *myfmt = 'x';
  strncpy(myfmt+1, fmt, len);
  myfmt[len+1] = 0;

  tbuf = SCM_MUST_MALLOC (size);
  {
#if !defined (HAVE_TM_ZONE)
    /* it seems the only way to tell non-GNU versions of strftime what
       zone to use (for the %Z format) is to set TZ in the
       environment.  interrupts and thread switching must be deferred
       until TZ is restored.  */
    char **oldenv = NULL;
    SCM *velts = SCM_VELTS (stime);
    int have_zone = 0;

    if (SCM_NFALSEP (velts[10]) && *SCM_STRING_CHARS (velts[10]) != 0)
      {
	/* it's not required that the TZ setting be correct, just that
	   it has the right name.  so try something like TZ=EST0.
	   using only TZ=EST would be simpler but it doesn't work on
	   some OSs, e.g., Solaris.  */
	SCM zone =
	  scm_string_append (scm_cons (velts[10],
				       scm_cons (scm_makfrom0str ("0"),
						 SCM_EOL)));

	have_zone = 1;
	SCM_DEFER_INTS;
	oldenv = setzone (zone, SCM_ARG2, FUNC_NAME);
      }
#endif

#ifdef LOCALTIME_CACHE
    tzset ();
#endif

    /* POSIX says strftime returns 0 on buffer overrun, but old
       systems (i.e. libc 4 on GNU/Linux) might return `size' in that
       case. */
    while ((len = strftime (tbuf, size, myfmt, &t)) == 0 || len == size)
      {
	scm_must_free (tbuf);
	size *= 2;
	tbuf = SCM_MUST_MALLOC (size);
      }

#if !defined (HAVE_TM_ZONE)
    if (have_zone)
      {
	restorezone (velts[10], oldenv, FUNC_NAME);
	SCM_ALLOW_INTS;
      }
#endif
    }

  result = scm_makfromstr (tbuf+1, len-1, 0);
  scm_must_free (tbuf);
  scm_must_free(myfmt);
  return result;
}
#undef FUNC_NAME

#ifdef HAVE_STRPTIME
SCM_DEFINE (scm_strptime, "strptime", 2, 0, 0,
            (SCM format, SCM string),
	    "Performs the reverse action to @code{strftime}, parsing @var{string}\n"
	    "according to the specification supplied in @var{template}.  The\n"
	    "interpretation of month and day names is dependent on the current\n"
	    "locale.  The\n"
	    "value returned is a pair.  The CAR has an object with time components \n"
	    "in the form returned by @code{localtime} or @code{gmtime},\n"
	    "but the time zone components\n"
	    "are not usefully set.\n"
	    "The CDR reports the number of characters from @var{string} which\n"
	    "were used for the conversion.")
#define FUNC_NAME s_scm_strptime
{
  struct tm t;
  char *fmt, *str, *rest;

  SCM_VALIDATE_ROSTRING (1,format);
  SCM_VALIDATE_ROSTRING (2,string);

  SCM_COERCE_SUBSTR (format);
  SCM_COERCE_SUBSTR (string);
  fmt = SCM_ROCHARS (format);
  str = SCM_ROCHARS (string);

  /* initialize the struct tm */
#define tm_init(field) t.field = 0
  tm_init (tm_sec);
  tm_init (tm_min);
  tm_init (tm_hour);
  tm_init (tm_mday);
  tm_init (tm_mon);
  tm_init (tm_year);
  tm_init (tm_wday);
  tm_init (tm_yday);
#undef tm_init

  t.tm_isdst = -1;
  SCM_DEFER_INTS;
  if ((rest = strptime (str, fmt, &t)) == NULL)
    SCM_SYSERROR;

  SCM_ALLOW_INTS;
  return scm_cons (filltime (&t, 0, NULL),  SCM_MAKINUM (rest - str));
}
#undef FUNC_NAME
#endif /* HAVE_STRPTIME */

void
scm_init_stime()
{
  scm_sysintern("internal-time-units-per-second",
		scm_long2num((long)CLKTCK));

#ifdef HAVE_FTIME
  if (!scm_your_base.time) ftime(&scm_your_base);
#else
  if (!scm_your_base) time(&scm_your_base);
#endif

  if (!scm_my_base) scm_my_base = mytime();

  scm_add_feature ("current-time");
#include "libguile/stime.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
