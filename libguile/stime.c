/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2011, 2013 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */




/* _POSIX_C_SOURCE is not defined always, because it causes problems on some
   systems, notably

       - FreeBSD loses all BSD and XOPEN defines.
       - glibc loses some things like CLK_TCK.
       - On MINGW it conflicts with the pthread headers.

   But on HP-UX _POSIX_C_SOURCE is needed, as noted, for gmtime_r.

   Perhaps a configure test could figure out what _POSIX_C_SOURCE gives and
   what it takes away, and decide from that whether to use it, instead of
   hard coding __hpux.  */

#ifndef _REENTRANT
# define _REENTRANT   /* ask solaris for gmtime_r prototype */
#endif
#ifdef __hpux
#define _POSIX_C_SOURCE 199506L  /* for gmtime_r prototype */
#endif

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <strftime.h>
#include <unistr.h>

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/dynwind.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/stime.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#ifdef HAVE_CLOCK_GETTIME
# include <time.h>
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#ifdef HAVE_SYS_TIMEB_H
# include <sys/timeb.h>
#endif

#if ! HAVE_DECL_STRPTIME
extern char *strptime ();
#endif

#ifdef __STDC__
# define timet time_t
#else
# define timet long
#endif


#if SCM_SIZEOF_LONG >= 8 && defined HAVE_CLOCK_GETTIME
/* Nanoseconds on 64-bit systems with POSIX timers.  */
#define TIME_UNITS_PER_SECOND 1000000000
#else
/* Milliseconds for everyone else.  */
#define TIME_UNITS_PER_SECOND 1000
#endif

long scm_c_time_units_per_second = TIME_UNITS_PER_SECOND;

static long
time_from_seconds_and_nanoseconds (long s, long ns)
{
  return s * TIME_UNITS_PER_SECOND
    + ns / (1000000000 / TIME_UNITS_PER_SECOND);
}


/* A runtime-selectable mechanism to choose a timing mechanism.  Really
   we want to use POSIX timers, but that's not always possible.  Notably,
   the user may have everything she needs at compile-time, but if she's
   running on an SMP machine without a common clock source, she can't
   use POSIX CPUTIME clocks.  */
static long (*get_internal_real_time) (void);
static long (*get_internal_run_time) (void);


#ifdef HAVE_CLOCK_GETTIME
struct timespec posix_real_time_base;

static long
get_internal_real_time_posix_timer (void)
{
  struct timespec ts;
  clock_gettime (CLOCK_REALTIME, &ts);
  return time_from_seconds_and_nanoseconds
    (ts.tv_sec - posix_real_time_base.tv_sec,
     ts.tv_nsec - posix_real_time_base.tv_nsec);
}

#if defined _POSIX_CPUTIME && defined CLOCK_PROCESS_CPUTIME_ID
/* You see, FreeBSD defines _POSIX_CPUTIME but not
   CLOCK_PROCESS_CPUTIME_ID.  */
#define HAVE_POSIX_CPUTIME 1

struct timespec posix_run_time_base;

static long
get_internal_run_time_posix_timer (void)
{
  struct timespec ts;
  clock_gettime (CLOCK_PROCESS_CPUTIME_ID, &ts);
  return time_from_seconds_and_nanoseconds
    (ts.tv_sec - posix_run_time_base.tv_sec,
     ts.tv_nsec - posix_run_time_base.tv_nsec);
}
#endif /* _POSIX_CPUTIME */
#endif /* HAVE_CLOCKTIME */
  
  
#ifdef HAVE_GETTIMEOFDAY
struct timeval gettimeofday_real_time_base;

static long
get_internal_real_time_gettimeofday (void)
{
  struct timeval tv;
  gettimeofday (&tv, NULL);
  return time_from_seconds_and_nanoseconds
    (tv.tv_sec - gettimeofday_real_time_base.tv_sec,
     (tv.tv_usec - gettimeofday_real_time_base.tv_usec) * 1000);
}
#endif


#if defined HAVE_TIMES
static long ticks_per_second;

static long
get_internal_run_time_times (void)
{
  struct tms time_buffer;
  times(&time_buffer);
  return (time_buffer.tms_utime + time_buffer.tms_stime)
    * TIME_UNITS_PER_SECOND / ticks_per_second;
}
#endif

static timet fallback_real_time_base;
static long
get_internal_real_time_fallback (void)
{
  return time_from_seconds_and_nanoseconds
    ((long) time (NULL) - fallback_real_time_base, 0);
}


SCM_DEFINE (scm_get_internal_real_time, "get-internal-real-time", 0, 0, 0,
           (),
	    "Return the number of time units since the interpreter was\n"
	    "started.")
#define FUNC_NAME s_scm_get_internal_real_time
{
  return scm_from_long (get_internal_real_time ());
}
#undef FUNC_NAME


#ifdef HAVE_TIMES
SCM_DEFINE (scm_times, "times", 0, 0, 0,
            (void),
	    "Return an object with information about real and processor\n"
	    "time.  The following procedures accept such an object as an\n"
	    "argument and return a selected component:\n"
	    "\n"
	    "@table @code\n"
	    "@item tms:clock\n"
	    "The current real time, expressed as time units relative to an\n"
	    "arbitrary base.\n"
	    "@item tms:utime\n"
	    "The CPU time units used by the calling process.\n"
	    "@item tms:stime\n"
	    "The CPU time units used by the system on behalf of the calling\n"
	    "process.\n"
	    "@item tms:cutime\n"
	    "The CPU time units used by terminated child processes of the\n"
	    "calling process, whose status has been collected (e.g., using\n"
	    "@code{waitpid}).\n"
	    "@item tms:cstime\n"
	    "Similarly, the CPU times units used by the system on behalf of\n"
	    "terminated child processes.\n"
	    "@end table")
#define FUNC_NAME s_scm_times
{
  struct tms t;
  clock_t rv;
  SCM factor;

  SCM result = scm_c_make_vector (5, SCM_UNDEFINED);
  rv = times (&t);
  if (rv == -1)
    SCM_SYSERROR;

  factor = scm_quotient (scm_from_long (TIME_UNITS_PER_SECOND),
                         scm_from_long (ticks_per_second));

  SCM_SIMPLE_VECTOR_SET (result, 0,
                         scm_product (scm_from_long (rv), factor));
  SCM_SIMPLE_VECTOR_SET (result, 1,
                         scm_product (scm_from_long (t.tms_utime), factor));
  SCM_SIMPLE_VECTOR_SET (result, 2,
                         scm_product (scm_from_long (t.tms_stime), factor));
  SCM_SIMPLE_VECTOR_SET (result ,3,
                         scm_product (scm_from_long (t.tms_cutime), factor));
  SCM_SIMPLE_VECTOR_SET (result, 4,
                         scm_product (scm_from_long (t.tms_cstime), factor));
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_TIMES */

long
scm_c_get_internal_run_time (void)
{
  return get_internal_run_time ();
}

SCM_DEFINE (scm_get_internal_run_time, "get-internal-run-time", 0, 0, 0,
           (void),
	    "Return the number of time units of processor time used by the\n"
	    "interpreter.  Both @emph{system} and @emph{user} time are\n"
	    "included but subprocesses are not.")
#define FUNC_NAME s_scm_get_internal_run_time
{
  return scm_from_long (scm_c_get_internal_run_time ());
}
#undef FUNC_NAME

/* For reference, note that current-time and gettimeofday both should be
   protected against setzone/restorezone changes in another thread, since on
   DOS the system time is normally kept as local time, which means TZ
   affects the return from current-time and gettimeofday.  Not sure if DJGPP
   etc actually has concurrent multi-threading, but it seems prudent not to
   make assumptions about this.  */

SCM_DEFINE (scm_current_time, "current-time", 0, 0, 0,
           (void),
	    "Return the number of seconds since 1970-01-01 00:00:00 UTC,\n"
	    "excluding leap seconds.")
#define FUNC_NAME s_scm_current_time
{
  timet timv;

  SCM_CRITICAL_SECTION_START;
  timv = time (NULL);
  SCM_CRITICAL_SECTION_END;
  if (timv == -1)
    SCM_MISC_ERROR ("current time not available", SCM_EOL);
  return scm_from_long (timv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gettimeofday, "gettimeofday", 0, 0, 0,
            (void),
	    "Return a pair containing the number of seconds and microseconds\n"
	    "since 1970-01-01 00:00:00 UTC, excluding leap seconds.  Note:\n"
	    "whether true microsecond resolution is available depends on the\n"
	    "operating system.")
#define FUNC_NAME s_scm_gettimeofday
{
#ifdef HAVE_GETTIMEOFDAY
  struct timeval time;

  if (gettimeofday (&time, NULL))
    SCM_SYSERROR;
  
  return scm_cons (scm_from_long (time.tv_sec),
		   scm_from_long (time.tv_usec));
#else
  timet t = time (NULL);
  if (errno)
    SCM_SYSERROR;
  else
    return scm_cons (scm_from_long ((long)t), SCM_INUM0);
#endif
}
#undef FUNC_NAME

static SCM
filltime (struct tm *bd_time, int zoff, const char *zname)
{
  SCM result = scm_c_make_vector (11, SCM_UNDEFINED);

  SCM_SIMPLE_VECTOR_SET (result,0, scm_from_int (bd_time->tm_sec));
  SCM_SIMPLE_VECTOR_SET (result,1, scm_from_int (bd_time->tm_min));
  SCM_SIMPLE_VECTOR_SET (result,2, scm_from_int (bd_time->tm_hour));
  SCM_SIMPLE_VECTOR_SET (result,3, scm_from_int (bd_time->tm_mday));
  SCM_SIMPLE_VECTOR_SET (result,4, scm_from_int (bd_time->tm_mon));
  SCM_SIMPLE_VECTOR_SET (result,5, scm_from_int (bd_time->tm_year));
  SCM_SIMPLE_VECTOR_SET (result,6, scm_from_int (bd_time->tm_wday));
  SCM_SIMPLE_VECTOR_SET (result,7, scm_from_int (bd_time->tm_yday));
  SCM_SIMPLE_VECTOR_SET (result,8, scm_from_int (bd_time->tm_isdst));
  SCM_SIMPLE_VECTOR_SET (result,9, scm_from_int (zoff));
  SCM_SIMPLE_VECTOR_SET (result,10, (zname 
				     ? scm_from_locale_string (zname)
				     : SCM_BOOL_F));
  return result;
}

static char tzvar[3] = "TZ";

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
      size_t zone_len;
      
      zone_len = scm_to_locale_stringbuf (zone, NULL, 0);
      buf = scm_malloc (zone_len + sizeof (tzvar) + 1);
      strcpy (buf, tzvar);
      buf[sizeof(tzvar)-1] = '=';
      scm_to_locale_stringbuf (zone, buf+sizeof(tzvar), zone_len);
      buf[sizeof(tzvar)+zone_len] = '\0';
      oldenv = environ;
      tmpenv[0] = buf;
      tmpenv[1] = 0;
      environ = tmpenv;
    }
  return oldenv;
}

static void
restorezone (SCM zone, char **oldenv, const char *subr SCM_UNUSED)
{
  if (!SCM_UNBNDP (zone))
    {
      free (environ[0]);
      environ = oldenv;
#ifdef HAVE_TZSET
      /* for the possible benefit of user code linked with libguile.  */
      tzset();
#endif
    }
}

SCM_DEFINE (scm_localtime, "localtime", 1, 1, 0,
            (SCM time, SCM zone),
	    "Return an object representing the broken down components of\n"
	    "@var{time}, an integer like the one returned by\n"
	    "@code{current-time}.  The time zone for the calculation is\n"
	    "optionally specified by @var{zone} (a string), otherwise the\n"
	    "@code{TZ} environment variable or the system default is used.")
#define FUNC_NAME s_scm_localtime
{
  timet itime;
  struct tm *ltptr, lt, *utc;
  SCM result;
  int zoff;
  char *zname = 0;
  char **oldenv;
  int err;

  itime = SCM_NUM2LONG (1, time);

  /* deferring interupts is essential since a) setzone may install a temporary
     environment b) localtime uses a static buffer.  */
  SCM_CRITICAL_SECTION_START;
  oldenv = setzone (zone, SCM_ARG2, FUNC_NAME);
#ifdef LOCALTIME_CACHE
  tzset ();
#endif
  /* POSIX says localtime sets errno, but C99 doesn't say that.
     Give a sensible default value in case localtime doesn't set it.  */
  errno = EINVAL;
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
      zname = scm_malloc (strlen (ptr) + 1);
      strcpy (zname, ptr);
    }
  /* the struct is copied in case localtime and gmtime share a buffer.  */
  if (ltptr)
    lt = *ltptr;
  /* POSIX says gmtime sets errno, but C99 doesn't say that.
     Give a sensible default value in case gmtime doesn't set it.  */
  errno = EINVAL;
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
  SCM_CRITICAL_SECTION_END;

  free (zname);
  return result;
}
#undef FUNC_NAME

/* tm_zone is normally a pointer, not an array within struct tm, so we might
   have to worry about the lifespan of what it points to.  The posix specs
   don't seem to say anything about this, let's assume here that tm_zone
   will be a constant and therefore no protection or anything is needed
   until we copy it in filltime().  */

SCM_DEFINE (scm_gmtime, "gmtime", 1, 0, 0,
            (SCM time),
	    "Return an object representing the broken down components of\n"
	    "@var{time}, an integer like the one returned by\n"
	    "@code{current-time}.  The values are calculated for UTC.")
#define FUNC_NAME s_scm_gmtime
{
  timet itime;
  struct tm bd_buf, *bd_time;
  const char *zname;

  itime = SCM_NUM2LONG (1, time);

  /* POSIX says gmtime sets errno, but C99 doesn't say that.
     Give a sensible default value in case gmtime doesn't set it.  */
  errno = EINVAL;

#if HAVE_GMTIME_R
  bd_time = gmtime_r (&itime, &bd_buf);
#else
  SCM_CRITICAL_SECTION_START;
  bd_time = gmtime (&itime);
  if (bd_time != NULL)
    bd_buf = *bd_time;
  SCM_CRITICAL_SECTION_END;
#endif
  if (bd_time == NULL)
    SCM_SYSERROR;

#if HAVE_STRUCT_TM_TM_ZONE
  zname = bd_buf.tm_zone;
#else
  zname = "GMT";
#endif
  return filltime (&bd_buf, 0, zname);
}
#undef FUNC_NAME

/* copy time components from a Scheme object to a struct tm.  */
static void
bdtime2c (SCM sbd_time, struct tm *lt, int pos, const char *subr)
{
  SCM_ASSERT (scm_is_simple_vector (sbd_time)
	      && SCM_SIMPLE_VECTOR_LENGTH (sbd_time) == 11,
	      sbd_time, pos, subr);

  lt->tm_sec = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 0));
  lt->tm_min = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 1));
  lt->tm_hour = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 2));
  lt->tm_mday = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 3));
  lt->tm_mon = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 4));
  lt->tm_year = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 5));
  lt->tm_wday = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 6));
  lt->tm_yday = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 7));
  lt->tm_isdst = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 8));
#if HAVE_STRUCT_TM_TM_GMTOFF
  lt->tm_gmtoff = - scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 9));
#endif
#ifdef HAVE_TM_ZONE
  if (scm_is_false (SCM_SIMPLE_VECTOR_REF (sbd_time, 10)))
    lt->tm_zone = NULL;
  else
    lt->tm_zone  = scm_to_locale_string (SCM_SIMPLE_VECTOR_REF (sbd_time, 10));
#endif
}

SCM_DEFINE (scm_mktime, "mktime", 1, 1, 0,
            (SCM sbd_time, SCM zone),
	    "@var{sbd_time} is an object representing broken down time and\n"
	    "@code{zone} is an optional time zone specifier (otherwise the\n"
	    "TZ environment variable or the system default is used).\n"
	    "\n"
	    "Returns a pair: the car is a corresponding integer time value\n"
	    "like that returned by @code{current-time}; the cdr is a broken\n"
	    "down time object, similar to as @var{sbd_time} but with\n"
	    "normalized values.")
#define FUNC_NAME s_scm_mktime
{
  timet itime;
  struct tm lt, *utc;
  SCM result;
  int zoff;
  char *zname = 0;
  char **oldenv;
  int err;

  scm_dynwind_begin (0);

  bdtime2c (sbd_time, &lt, SCM_ARG1, FUNC_NAME);
#if HAVE_STRUCT_TM_TM_ZONE
  scm_dynwind_free ((char *)lt.tm_zone);
#endif

  scm_dynwind_critical_section (SCM_BOOL_F);

  oldenv = setzone (zone, SCM_ARG2, FUNC_NAME);
#ifdef LOCALTIME_CACHE
  tzset ();
#endif
  itime = mktime (&lt);
  /* POSIX doesn't say mktime sets errno, and on glibc 2.3.2 for instance it
     doesn't.  Force a sensible value for our error message.  */
  err = EINVAL;

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
      zname = scm_malloc (strlen (ptr) + 1);
      strcpy (zname, ptr);
    }

  /* get timezone offset in seconds west of UTC.  */
  /* POSIX says gmtime sets errno, but C99 doesn't say that.
     Give a sensible default value in case gmtime doesn't set it.  */
  errno = EINVAL;
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

  result = scm_cons (scm_from_long (itime),
		     filltime (&lt, zoff, zname));
  free (zname);

  scm_dynwind_end ();
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
	    "Return a string which is broken-down time structure @var{stime}\n"
	    "formatted according to the given @var{format} string.\n"
	    "\n"
	    "@var{format} contains field specifications introduced by a\n"
	    "@samp{%} character.  See @ref{Formatting Calendar Time,,, libc,\n"
	    "The GNU C Library Reference Manual}, or @samp{man 3 strftime},\n"
	    "for the available formatting.\n"
	    "\n"
	    "@lisp\n"
	    "(strftime \"%c\" (localtime (current-time)))\n"
	    "@result{} \"Mon Mar 11 20:17:43 2002\"\n"
	    "@end lisp\n"
	    "\n"
	    "If @code{setlocale} has been called (@pxref{Locales}), month\n"
	    "and day names are from the current locale and in the locale\n"
	    "character set.")
#define FUNC_NAME s_scm_strftime
{
  struct tm t;

  char *tbuf;
  int size = 50;
  char *fmt;
  char *myfmt;
  size_t len;
  SCM result;

  SCM_VALIDATE_STRING (1, format);
  bdtime2c (stime, &t, SCM_ARG2, FUNC_NAME);

  /* Convert string to UTF-8 so that non-ASCII characters in the
     format are passed through unchanged.  */
  fmt = scm_to_utf8_stringn (format, &len);

  /* Ugly hack: strftime can return 0 if its buffer is too small,
     but some valid time strings (e.g. "%p") can sometimes produce
     a zero-byte output string!  Workaround is to prepend a junk
     character to the format string, so that valid returns are always
     nonzero. */
  myfmt = scm_malloc (len+2);
  *myfmt = (scm_t_uint8) 'x';
  strncpy (myfmt + 1, fmt, len);
  myfmt[len + 1] = 0;
  scm_remember_upto_here_1 (format);
  free (fmt);

  tbuf = scm_malloc (size);
  {
#if !defined (HAVE_TM_ZONE)
    /* it seems the only way to tell non-GNU versions of strftime what
       zone to use (for the %Z format) is to set TZ in the
       environment.  interrupts and thread switching must be deferred
       until TZ is restored.  */
    char **oldenv = NULL;
    SCM zone_spec = SCM_SIMPLE_VECTOR_REF (stime, 10);
    int have_zone = 0;

    if (scm_is_true (zone_spec) && scm_c_string_length (zone_spec) > 0)
      {
	/* it's not required that the TZ setting be correct, just that
	   it has the right name.  so try something like TZ=EST0.
	   using only TZ=EST would be simpler but it doesn't work on
	   some OSs, e.g., Solaris.  */
	SCM zone =
	  scm_string_append (scm_list_2 (zone_spec,
					 scm_from_locale_string ("0")));

	have_zone = 1;
	SCM_CRITICAL_SECTION_START;
	oldenv = setzone (zone, SCM_ARG2, FUNC_NAME);
      }
#endif

#ifdef LOCALTIME_CACHE
    tzset ();
#endif

    /* Use `nstrftime ()' from Gnulib, which supports all GNU extensions
       supported by glibc.  */
    while ((len = nstrftime (tbuf, size, myfmt, &t, 0, 0)) == 0)
      {
	free (tbuf);
	size *= 2;
	tbuf = scm_malloc (size);
      }

#if !defined (HAVE_TM_ZONE)
    if (have_zone)
      {
	restorezone (zone_spec, oldenv, FUNC_NAME);
	SCM_CRITICAL_SECTION_END;
      }
#endif
    }

  result = scm_from_utf8_string (tbuf + 1);
  free (tbuf);
  free (myfmt);
#if HAVE_STRUCT_TM_TM_ZONE
  free ((char *) t.tm_zone);
#endif
  return result;
}
#undef FUNC_NAME

#ifdef HAVE_STRPTIME
SCM_DEFINE (scm_strptime, "strptime", 2, 0, 0,
            (SCM format, SCM string),
	    "Performs the reverse action to @code{strftime}, parsing\n"
	    "@var{string} according to the specification supplied in\n"
	    "@var{format}.  The interpretation of month and day names is\n"
	    "dependent on the current locale.  The value returned is a pair.\n"
	    "The car has an object with time components\n"
	    "in the form returned by @code{localtime} or @code{gmtime},\n"
	    "but the time zone components\n"
	    "are not usefully set.\n"
	    "The cdr reports the number of characters from @var{string}\n"
	    "which were used for the conversion.")
#define FUNC_NAME s_scm_strptime
{
  struct tm t;
  char *fmt, *str, *rest;
  size_t used_len;
  long zoff;

  SCM_VALIDATE_STRING (1, format);
  SCM_VALIDATE_STRING (2, string);

  /* Convert strings to UTF-8 so that non-ASCII characters are passed
     through unchanged.  */
  fmt = scm_to_utf8_string (format);
  str = scm_to_utf8_string (string);

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
#if HAVE_STRUCT_TM_TM_GMTOFF
  tm_init (tm_gmtoff);
#endif
#undef tm_init

  /* GNU glibc strptime() "%s" is affected by the current timezone, since it
     reads a UTC time_t value and converts with localtime_r() to set the tm
     fields, hence the use of SCM_CRITICAL_SECTION_START.  */
  t.tm_isdst = -1;
  SCM_CRITICAL_SECTION_START;
  rest = strptime (str, fmt, &t);
  SCM_CRITICAL_SECTION_END;
  if (rest == NULL)
    {
      /* POSIX doesn't say strptime sets errno, and on glibc 2.3.2 for
         instance it doesn't.  Force a sensible value for our error
         message.  */
      errno = EINVAL;
      scm_remember_upto_here_2 (format, string);
      free (str);
      free (fmt);
      SCM_SYSERROR;
    }

  /* tm_gmtoff is set by GNU glibc strptime "%s", so capture it when
     available */
#if HAVE_STRUCT_TM_TM_GMTOFF
  zoff = - t.tm_gmtoff;  /* seconds west, not east */
#else
  zoff = 0;
#endif

  /* Compute the number of UTF-8 characters.  */
  used_len = u8_strnlen ((scm_t_uint8*) str, rest-str);
  scm_remember_upto_here_2 (format, string);
  free (str);
  free (fmt);

  return scm_cons (filltime (&t, zoff, NULL),
		   scm_from_signed_integer (used_len));
}
#undef FUNC_NAME
#endif /* HAVE_STRPTIME */

void
scm_init_stime()
{
  scm_c_define ("internal-time-units-per-second",
		scm_from_long (SCM_TIME_UNITS_PER_SECOND));

  /* Init POSIX timers, and see if we can use them. */
#ifdef HAVE_CLOCK_GETTIME
  if (clock_gettime (CLOCK_REALTIME, &posix_real_time_base) == 0)
    get_internal_real_time = get_internal_real_time_posix_timer;

#ifdef HAVE_POSIX_CPUTIME
  {
    clockid_t dummy;
    
    /* Only use the _POSIX_CPUTIME clock if it's going to work across
       CPUs. */
    if (clock_getcpuclockid (0, &dummy) == 0 &&
        clock_gettime (CLOCK_PROCESS_CPUTIME_ID, &posix_run_time_base) == 0)
      get_internal_run_time = get_internal_run_time_posix_timer;
    else
      errno = 0;
  }
#endif /* HAVE_POSIX_CPUTIME */
#endif /* HAVE_CLOCKTIME */

  /* If needed, init and use gettimeofday timer. */
#ifdef HAVE_GETTIMEOFDAY
  if (!get_internal_real_time
      && gettimeofday (&gettimeofday_real_time_base, NULL) == 0)
    get_internal_real_time = get_internal_real_time_gettimeofday;
#endif

  /* Init ticks_per_second for scm_times, and use times(2)-based
     run-time timer if needed. */
#ifdef HAVE_TIMES
#ifdef _SC_CLK_TCK
  ticks_per_second = sysconf (_SC_CLK_TCK);
#else
  ticks_per_second = CLK_TCK;
#endif
  if (!get_internal_run_time)
    get_internal_run_time = get_internal_run_time_times;
#endif

  if (!get_internal_real_time)
    /* No POSIX timers, gettimeofday doesn't work... badness!  */
    {
      fallback_real_time_base = time (NULL);
      get_internal_real_time = get_internal_real_time_fallback;
    }

  /* If we don't have a run-time timer, use real-time.  */
  if (!get_internal_run_time)
    get_internal_run_time = get_internal_real_time;

  scm_add_feature ("current-time");
#include "libguile/stime.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
