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

# ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
# else
#  ifdef HAVE_SYS_TIMEB_H
#   include <sys/timeb.h>
#  endif
# endif

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


# ifdef HAVE_FTIME
#   include <sys/timeb.h>
# endif


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
  return SCM_MAKINUM(tmp);
}

#else

timet scm_your_base = 0;
SCM_PROC(s_get_internal_real_time, "get-internal-real-time", 0, 0, 0, scm_get_internal_real_time);
SCM
scm_get_internal_real_time()
{
	return SCM_MAKINUM((time((timet*)0) - scm_your_base) * (int)CLKTCK);
}
#endif



static long scm_my_base = 0;

SCM_PROC(s_get_internal_run_time, "get-internal-run-time", 0, 0, 0, scm_get_internal_run_time);
SCM
scm_get_internal_run_time()
{
  return SCM_MAKINUM(mytime()-scm_my_base);
}

SCM_PROC(s_current_time, "current-time", 0, 0, 0, scm_current_time);
SCM
scm_current_time()
{
  timet timv = time((timet*)0);
  SCM ans;
  ans = scm_ulong2num(timv);
  return SCM_BOOL_F==ans ? SCM_MAKINUM(timv) : ans;
}

long 
scm_time_in_msec(x)
     long x;
{
  if (CLKTCK==60) return (x*50)/3;
  else
    return (CLKTCK < 1000 ? x*(1000L/(long)CLKTCK) : (x*1000L)/(long)CLKTCK);
}

void
scm_init_stime()
{
  scm_sysintern("internal-time-units-per-second",
		SCM_MAKINUM((long)CLKTCK));

#ifdef HAVE_FTIME
  if (!scm_your_base.time) ftime(&scm_your_base);
#else
  if (!scm_your_base) time(&scm_your_base);
#endif

  if (!scm_my_base) scm_my_base = mytime();

#include "stime.x"
}

