/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "_scm.h"
#include "feature.h"

#include "stime.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif

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
#ifdef CLK_TCK
# define CLKTCK CLK_TCK
# ifdef CLOCKS_PER_SEC
#  if defined (unix) || defined (__unix)
#   ifndef ARM_ULIB
#    include <sys/times.h>
#   endif
#   define LACK_CLOCK
    /* This is because clock() might be POSIX rather than ANSI.
       This occurs on HP-UX machines */
#  endif
# endif
#else
# ifdef CLOCKS_PER_SEC
#  define CLKTCK CLOCKS_PER_SEC
# else
#  define LACK_CLOCK
#  define CLKTCK 60
# endif
#endif

#ifdef __STDC__
# define timet time_t
#else
# define timet long
#endif

#ifdef HAVE_TIMES
static
long mytime()
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

extern int ftime (struct timeb *);

struct timeb scm_your_base = {0};
SCM_PROC(s_get_internal_real_time, "get-internal-real-time", 0, 0, 0, scm_get_internal_real_time);
SCM
scm_get_internal_real_time()
{
  struct timeb time_buffer;
  long tmp;
  ftime(&time_buffer);
  time_buffer.time -= scm_your_base.time;
  tmp = time_buffer.millitm - scm_your_base.millitm;
  tmp = time_buffer.time*1000L + tmp;
  tmp *= CLKTCK;
  tmp /= 1000;
  return scm_long2num (tmp);
}

#else

timet scm_your_base = 0;
SCM_PROC(s_get_internal_real_time, "get-internal-real-time", 0, 0, 0, scm_get_internal_real_time);
SCM
scm_get_internal_real_time()
{
  return scm_long2num((time((timet*)0) - scm_your_base) * (int)CLKTCK);
}
#endif

SCM_PROC (s_times, "times", 0, 0, 0, scm_times);
SCM
scm_times (void)
{
#ifdef HAVE_TIMES
  struct tms t;
  clock_t rv;

  SCM result = scm_make_vector (SCM_MAKINUM(5), SCM_UNDEFINED);
  rv = times (&t);
  if (rv == -1)
    scm_syserror (s_times);
  SCM_VELTS (result)[0] = scm_long2num (rv);
  SCM_VELTS (result)[1] = scm_long2num (t.tms_utime);
  SCM_VELTS (result)[2] = scm_long2num (t.tms_stime);
  SCM_VELTS (result)[3] = scm_long2num (t.tms_cutime);
  SCM_VELTS (result)[4] = scm_long2num (t.tms_cstime);
  return result;
#else
  scm_sysmissing (s_times);
#endif
}

#ifndef HAVE_TZSET
/* GNU-WIN32's cygwin.dll doesn't have this. */
#define tzset()
#endif


static long scm_my_base = 0;

SCM_PROC(s_get_internal_run_time, "get-internal-run-time", 0, 0, 0, scm_get_internal_run_time);
SCM
scm_get_internal_run_time()
{
  return scm_long2num(mytime()-scm_my_base);
}

SCM_PROC(s_current_time, "current-time", 0, 0, 0, scm_current_time);
SCM
scm_current_time()
{
  timet timv;

  SCM_DEFER_INTS;
  if ((timv = time (0)) == -1)
    scm_syserror (s_current_time);
  SCM_ALLOW_INTS;
  return scm_long2num((long) timv);
}

SCM_PROC (s_gettimeofday, "gettimeofday", 0, 0, 0, scm_gettimeofday);
SCM
scm_gettimeofday (void)
{
#ifdef HAVE_GETTIMEOFDAY
  struct timeval time;

  SCM_DEFER_INTS;
  if (gettimeofday (&time, NULL) == -1)
    scm_syserror (s_gettimeofday);
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
    scm_syserror (s_gettimeofday);
  SCM_ALLOW_INTS;
  return scm_cons (scm_long2num (timv), SCM_MAKINUM (0));
# endif
#endif
}

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

static char **
setzone (SCM zone, int pos, char *subr)
{
  char **oldenv = 0;

  if (!SCM_UNBNDP (zone))
    {
      static char *tmpenv[2];
      char *buf;

      /* if zone was supplied, set the environment temporarily.  */
      SCM_ASSERT (SCM_NIMP (zone) && SCM_ROSTRINGP (zone), zone, pos, subr);
      SCM_COERCE_SUBSTR (zone);
      buf = scm_must_malloc (SCM_LENGTH (zone) + sizeof (tzvar) + 1,
			     subr);
      sprintf (buf, "%s=%s", tzvar, SCM_ROCHARS (zone));
      oldenv = environ;
      tmpenv[0] = buf;
      tmpenv[1] = 0;
      environ = tmpenv;
      tzset();
    }
  return oldenv;
}

static void
restorezone (SCM zone, char **oldenv, char *subr)
{
  if (!SCM_UNBNDP (zone))
    {
      scm_must_free (environ[0]);
      environ = oldenv;
      tzset();
    }
}


SCM_PROC (s_localtime, "localtime", 1, 1, 0, scm_localtime);
SCM
scm_localtime (SCM time, SCM zone)
{
  timet itime;
  struct tm *ltptr, lt, *utc;
  SCM result;
  int zoff;
  char *zname = 0;
  char **oldenv;
  int err;

  itime = scm_num2long (time, (char *) SCM_ARG1, s_localtime);
  SCM_DEFER_INTS;
  oldenv = setzone (zone, SCM_ARG2, s_localtime);
  ltptr = localtime (&itime);
  err = errno;
  if (ltptr)
    {
      const char *ptr;

      /* copy zone name before calling gmtime or tzset.  */
#ifdef HAVE_TM_ZONE
      ptr = ltptr->tm_zone;
#else
# ifdef HAVE_TZNAME
      ptr = tzname[ (ltptr->tm_isdst == 1) ? 1 : 0 ];
# else
      scm_misc_error (s_localtime, "Not fully implemented on this platform",
		      SCM_EOL);
# endif
#endif
      zname = scm_must_malloc (strlen (ptr) + 1, s_localtime);
      strcpy (zname, ptr);
    }
  /* the struct is copied in case localtime and gmtime share a buffer.  */
  if (ltptr)
    lt = *ltptr;
  utc = gmtime (&itime);
  if (utc == NULL)
    err = errno;
  restorezone (zone, oldenv, s_localtime);
  /* delayed until zone has been restored.  */
  errno = err;
  if (utc == NULL || ltptr == NULL)
    scm_syserror (s_localtime);

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

SCM_PROC (s_gmtime, "gmtime", 1, 0, 0, scm_gmtime);
SCM
scm_gmtime (SCM time)
{
  timet itime;
  struct tm *bd_time;
  SCM result;

  itime = scm_num2long (time, (char *) SCM_ARG1, s_gmtime);
  SCM_DEFER_INTS;
  bd_time = gmtime (&itime);
  if (bd_time == NULL)
    scm_syserror (s_gmtime);
  result = filltime (bd_time, 0, "GMT");
  SCM_ALLOW_INTS;
  return result;
}

/* copy time components from a Scheme object to a struct tm.  */
static void
bdtime2c (SCM sbd_time, struct tm *lt, int pos, char *subr)
{
  SCM_ASSERT (SCM_NIMP (sbd_time) && SCM_VECTORP (sbd_time)
	      && SCM_LENGTH (sbd_time) == 11
	      && SCM_INUMP (SCM_VELTS (sbd_time)[0]) 
	      && SCM_INUMP (SCM_VELTS (sbd_time)[1])
	      && SCM_INUMP (SCM_VELTS (sbd_time)[2])
	      && SCM_INUMP (SCM_VELTS (sbd_time)[3])
	      && SCM_INUMP (SCM_VELTS (sbd_time)[4])
	      && SCM_INUMP (SCM_VELTS (sbd_time)[5])
	      && SCM_INUMP (SCM_VELTS (sbd_time)[6])
	      && SCM_INUMP (SCM_VELTS (sbd_time)[7])
	      && SCM_INUMP (SCM_VELTS (sbd_time)[8]),
	      sbd_time, pos, subr);
  lt->tm_sec = SCM_INUM (SCM_VELTS (sbd_time)[0]);
  lt->tm_min = SCM_INUM (SCM_VELTS (sbd_time)[1]);
  lt->tm_hour = SCM_INUM (SCM_VELTS (sbd_time)[2]);
  lt->tm_mday = SCM_INUM (SCM_VELTS (sbd_time)[3]);
  lt->tm_mon = SCM_INUM (SCM_VELTS (sbd_time)[4]);
  lt->tm_year = SCM_INUM (SCM_VELTS (sbd_time)[5]);
  lt->tm_wday = SCM_INUM (SCM_VELTS (sbd_time)[6]);
  lt->tm_yday = SCM_INUM (SCM_VELTS (sbd_time)[7]);
  lt->tm_isdst = SCM_INUM (SCM_VELTS (sbd_time)[8]);
}

SCM_PROC (s_mktime, "mktime", 1, 1, 0, scm_mktime);
SCM
scm_mktime (SCM sbd_time, SCM zone)
{
  timet itime;
  struct tm lt, *utc;
  SCM result;
  int zoff;
  char *zname = 0;
  char **oldenv;
  int err;

  SCM_ASSERT (SCM_NIMP (sbd_time) && SCM_VECTORP (sbd_time), sbd_time,
	      SCM_ARG1, s_mktime);
  bdtime2c (sbd_time, &lt, SCM_ARG1, s_mktime);

  SCM_DEFER_INTS;
  oldenv = setzone (zone, SCM_ARG2, s_mktime);
  itime = mktime (&lt);
  err = errno;

  if (itime != -1)
    {
      const char *ptr;

      /* copy zone name before calling gmtime or tzset.  */
#ifdef HAVE_TM_ZONE
      ptr = lt.tm_zone;
#else
# ifdef HAVE_TZNAME
      ptr = tzname[ (lt.tm_isdst == 1) ? 1 : 0 ];
# else
      scm_misc_error (s_mktime, "Not fully implemented on this platform",
		      SCM_EOL);
# endif
#endif
      zname = scm_must_malloc (strlen (ptr) + 1, s_mktime);
      strcpy (zname, ptr);
    }

  /* get timezone offset in seconds west of UTC.  */
  utc = gmtime (&itime);
  if (utc == NULL)
    err = errno;

  restorezone (zone, oldenv, s_mktime);
  /* delayed until zone has been restored.  */
  errno = err;
  if (utc == NULL || itime == -1)
    scm_syserror (s_mktime);

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

SCM_PROC (s_tzset, "tzset", 0, 0, 0, scm_tzset);
SCM
scm_tzset (void)
{
  tzset();
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_strftime, "strftime", 2, 0, 0, scm_strftime);

SCM
scm_strftime (format, stime)
     SCM format;
     SCM stime;
{
  struct tm t;

  char *tbuf;
  int size = 50;
  char *fmt;
  int len;
  SCM result;

  SCM_ASSERT (SCM_NIMP (format) && SCM_ROSTRINGP (format), format, SCM_ARG1,
	      s_strftime);
  bdtime2c (stime, &t, SCM_ARG2, s_strftime);

  SCM_COERCE_SUBSTR (format);
  fmt = SCM_ROCHARS (format);
  len = SCM_ROLENGTH (format);

  tbuf = scm_must_malloc (size, s_strftime);
  while ((len = strftime (tbuf, size, fmt, &t)) == size)
    {
      scm_must_free (tbuf);
      size *= 2;
      tbuf = scm_must_malloc (size, s_strftime);
    }
  result = scm_makfromstr (tbuf, len, 0);
  scm_must_free (tbuf);
  return result;
}

SCM_PROC (s_strptime, "strptime", 2, 0, 0, scm_strptime);

SCM
scm_strptime (format, string)
     SCM format;
     SCM string;
{
#ifdef HAVE_STRPTIME
  struct tm t;
  char *fmt, *str, *rest;

  SCM_ASSERT (SCM_NIMP (format) && SCM_ROSTRINGP (format), format, SCM_ARG1,
	      s_strptime);
  SCM_ASSERT (SCM_NIMP (string) && SCM_ROSTRINGP (string), string, SCM_ARG2,
	      s_strptime);

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
    scm_syserror (s_strptime);

  SCM_ALLOW_INTS;
  return scm_cons (filltime (&t, 0, NULL),  SCM_MAKINUM (rest - str));

#else
  scm_sysmissing (s_strptime);
#endif
}

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
#include "stime.x"
}

