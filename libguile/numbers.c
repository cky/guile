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
#include <math.h>
#include "_scm.h"
#include "genio.h"
#include "unif.h"

#include "numbers.h"

#define DIGITS '0':case '1':case '2':case '3':case '4':\
 case '5':case '6':case '7':case '8':case '9'


/* IS_INF tests its floating point number for infiniteness
 */
#ifndef IS_INF
# define IS_INF(x) ((x)==(x)/2)
#endif

/* MAXEXP is the maximum double precision expontent
 * FLTMAX is less than or scm_equal the largest single precision float
 */

#ifdef SCM_FLOATS
# ifdef STDC_HEADERS
#  ifndef GO32
#   include <float.h>
#  endif /* ndef GO32 */
# endif /* def STDC_HEADERS */
# ifdef DBL_MAX_10_EXP
#  define MAXEXP DBL_MAX_10_EXP
# else
#  define MAXEXP 308   /* IEEE doubles */
# endif /* def DBL_MAX_10_EXP */
# ifdef FLT_MAX
#  define FLTMAX FLT_MAX
# else
#  define FLTMAX 1e+23
# endif /* def FLT_MAX */
#endif /* def SCM_FLOATS */



SCM_PROC(s_exact_p, "exact?", 1, 0, 0, scm_exact_p);
#ifdef __STDC__
SCM
scm_exact_p(SCM x)
#else
SCM
scm_exact_p(x)
     SCM x;
#endif
{
  if SCM_INUMP(x) return SCM_BOOL_T;
#ifdef SCM_BIGDIG
  if (SCM_NIMP(x) && SCM_BIGP(x)) return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}

SCM_PROC(s_odd_p, "odd?", 1, 0, 0, scm_odd_p);
#ifdef __STDC__
SCM
scm_odd_p(SCM n)
#else
SCM
scm_odd_p(n)
     SCM n;
#endif
{
#ifdef SCM_BIGDIG
  if SCM_NINUMP(n) {
    SCM_ASSERT(SCM_NIMP(n) && SCM_BIGP(n), n, SCM_ARG1, s_odd_p);
    return (1 & SCM_BDIGITS(n)[0]) ? SCM_BOOL_T : SCM_BOOL_F;
  }
#else
  SCM_ASSERT(SCM_INUMP(n), n, SCM_ARG1, s_odd_p);
#endif
  return (4 & (int)n) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_even_p, "even?", 1, 0, 0, scm_even_p);
#ifdef __STDC__
SCM
scm_even_p(SCM n)
#else
SCM
scm_even_p(n)
     SCM n;
#endif
{
#ifdef SCM_BIGDIG
  if SCM_NINUMP(n) {
    SCM_ASSERT(SCM_NIMP(n) && SCM_BIGP(n), n, SCM_ARG1, s_even_p);
    return (1 & SCM_BDIGITS(n)[0]) ? SCM_BOOL_F : SCM_BOOL_T;
  }
#else
  SCM_ASSERT(SCM_INUMP(n), n, SCM_ARG1, s_even_p);
#endif
  return (4 & (int)n) ? SCM_BOOL_F : SCM_BOOL_T;
}

SCM_PROC(s_abs, "abs", 1, 0, 0, scm_abs);
#ifdef __STDC__
SCM
scm_abs(SCM x)
#else
SCM
scm_abs(x)
     SCM x;
#endif
{
#ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_abs);
    if (SCM_TYP16(x)==scm_tc16_bigpos) return x;
    return scm_copybig(x, 0);
  }
#else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_abs);
#endif
  if (SCM_INUM(x) >= 0) return x;
  x = -SCM_INUM(x);
  if (!SCM_POSFIXABLE(x))
#ifdef SCM_BIGDIG
    return scm_long2big(x);
#else
  SCM_NUM_OVERFLOW (s_abs);
#endif
  return SCM_MAKINUM(x);
}

SCM_PROC(s_quotient, "quotient", 2, 0, 0, scm_quotient);
#ifdef __STDC__
SCM
scm_quotient(SCM x, SCM y)
#else
SCM
scm_quotient(x, y)
     SCM x;
     SCM y;
#endif
{
  register long z;
#ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    long w;
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_quotient);
    if SCM_NINUMP(y) {
      SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
      return scm_divbigbig(SCM_BDIGITS(x),
			   SCM_NUMDIGS(x),
			   SCM_BDIGITS(y),
			   SCM_NUMDIGS(y),
			   SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y),
			   2);
    }
    z = SCM_INUM(y);
    SCM_ASRTGO(z, ov);
    if (1==z) return x;
    if (z < 0) z = -z;
    if (z < SCM_BIGRAD) {
      w = scm_copybig(x, SCM_BIGSIGN(x) ? (y>0) : (y<0));
      scm_divbigdig(SCM_BDIGITS(w), SCM_NUMDIGS(w), (SCM_BIGDIG)z);
      return scm_normbig(w);
    }
#ifndef SCM_DIGSTOOBIG
    w = scm_pseudolong(z);
    return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), (SCM_BIGDIG *)&w, SCM_DIGSPERLONG,
			 SCM_BIGSIGN(x) ? (y>0) : (y<0), 2);
#else
    { SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
      scm_longdigs(z, zdigs);
      return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), zdigs, SCM_DIGSPERLONG,
			   SCM_BIGSIGN(x) ? (y>0) : (y<0), 2);
    }
#endif
  }
  if SCM_NINUMP(y) {
# ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_quotient);
# endif
    return SCM_INUM0;
  }
#else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_quotient);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_quotient);
#endif
  if ((z = SCM_INUM(y))==0)
    ov: SCM_NUM_OVERFLOW (s_quotient);
  z = SCM_INUM(x)/z;
#ifdef BADIVSGNS
  {
#if (__TURBOC__==1)
    long t = ((y<0) ? -SCM_INUM(x) : SCM_INUM(x))%SCM_INUM(y);
#else
    long t = SCM_INUM(x)%SCM_INUM(y);
#endif
    if (t==0) ;
    else if (t < 0)
      if (x < 0) ;
      else z--;
    else if (x < 0) z++;
  }
#endif
  if (!SCM_FIXABLE(z))
#ifdef SCM_BIGDIG
    return scm_long2big(z);
#else
  SCM_NUM_OVERFLOW (s_quotient);
#endif
  return SCM_MAKINUM(z);
}

SCM_PROC(s_remainder, "remainder", 2, 0, 0, scm_remainder);
#ifdef __STDC__
SCM
scm_remainder(SCM x, SCM y)
#else
SCM
scm_remainder(x, y)
     SCM x;
     SCM y;
#endif
{
  register long z;
#ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_remainder);
    if SCM_NINUMP(y) {
      SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
      return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			   SCM_BIGSIGN(x), 0);
    }
    if (!(z = SCM_INUM(y))) goto ov;
    return scm_divbigint(x, z, SCM_BIGSIGN(x), 0);
  }
  if SCM_NINUMP(y) {
# ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_remainder);
# endif
    return x;
  }
#else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_remainder);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_remainder);
#endif
  if (!(z = SCM_INUM(y)))
    ov: SCM_NUM_OVERFLOW (s_remainder);
#if (__TURBOC__==1)
  if (z < 0) z = -z;
#endif
  z = SCM_INUM(x)%z;
#ifdef BADIVSGNS
  if (!z) ;
  else if (z < 0)
    if (x < 0) ;
    else z += SCM_INUM(y);
  else if (x < 0) z -= SCM_INUM(y);
#endif
  return SCM_MAKINUM(z);
}

SCM_PROC(s_modulo, "modulo", 2, 0, 0, scm_modulo);
#ifdef __STDC__
SCM
scm_modulo(SCM x, SCM y)
#else
SCM
scm_modulo(x, y)
     SCM x;
     SCM y;
#endif
{
  register long yy, z;
#ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_modulo);
    if SCM_NINUMP(y) {
      SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
      return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			   SCM_BIGSIGN(y), (SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y)) ? 1 : 0);
    }
    if (!(z = SCM_INUM(y))) goto ov;
    return scm_divbigint(x, z, y < 0, (SCM_BIGSIGN(x) ? (y > 0) : (y < 0)) ? 1 : 0);
  }
  if SCM_NINUMP(y) {
# ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_modulo);
# endif
    return (SCM_BIGSIGN(y) ? (x>0) : (x<0)) ? scm_sum(x, y) : x;
  }
#else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_modulo);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_modulo);
#endif
  if (!(yy = SCM_INUM(y)))
    ov: SCM_NUM_OVERFLOW (s_modulo);
#if (__TURBOC__==1)
  z = SCM_INUM(x);
  z = ((yy<0) ? -z : z)%yy;
#else
  z = SCM_INUM(x)%yy;
#endif
  return SCM_MAKINUM(((yy<0) ? (z>0) : (z<0)) ? z+yy : z);
}

SCM_PROC1 (s_gcd, "gcd", scm_tc7_asubr, scm_gcd);
#ifdef __STDC__
SCM
scm_gcd(SCM x, SCM y)
#else
SCM
scm_gcd(x, y)
     SCM x;
     SCM y;
#endif
{
  register long u, v, k, t;
  if SCM_UNBNDP(y) return SCM_UNBNDP(x) ? SCM_INUM0 : x;
 tailrec:
#ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
  big_gcd:
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_gcd);
    if SCM_BIGSIGN(x) x = scm_copybig(x, 0);
  newy:
    if SCM_NINUMP(y) {
      SCM_ASSERT(SCM_NIMP(y) && SCM_BIGP(y), y, SCM_ARG2, s_gcd);
      if SCM_BIGSIGN(y) y = scm_copybig(y, 0);
      switch (scm_bigcomp(x, y)) {
      case -1:
      swaprec: t = scm_remainder(x, y); x = y; y = t; goto tailrec;
      case  0: return x;
      case  1: y = scm_remainder(y, x); goto newy;
      }
      /* instead of the switch, we could just return scm_gcd(y, scm_modulo(x, y)); */
    }
    if (SCM_INUM0==y) return x; goto swaprec;
  }
  if SCM_NINUMP(y) { t=x; x=y; y=t; goto big_gcd;}
#else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_gcd);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_gcd);
#endif
  u = SCM_INUM(x);
  if (u<0) u = -u;
  v = SCM_INUM(y);
  if (v<0) v = -v;
  else if (0==v) goto getout;
  if (0==u) {u = v; goto getout;}
  for (k = 1;!(1 & ((int)u|(int)v));k <<= 1, u >>= 1, v >>= 1);
  if (1 & (int)u) t = -v;
  else {
    t = u;
  b3:
    t = SCM_SRS(t, 1);
  }
  if (!(1 & (int)t)) goto b3;
  if (t>0) u = t;
  else v = -t;
  if ((t = u-v)) goto b3;
  u = u*k;
 getout:
  if (!SCM_POSFIXABLE(u))
#ifdef SCM_BIGDIG
    return scm_long2big(u);
#else
  SCM_NUM_OVERFLOW (s_gcd);
#endif
  return SCM_MAKINUM(u);
}

SCM_PROC1 (s_lcm, "lcm", scm_tc7_asubr, scm_lcm);
#ifdef __STDC__
SCM
scm_lcm(SCM n1, SCM n2)
#else
SCM
scm_lcm(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  SCM d;
  if SCM_UNBNDP(n2) {
    n2 = SCM_MAKINUM(1L);
    if SCM_UNBNDP(n1) return n2;
  }
  d = scm_gcd(n1, n2);
  if (SCM_INUM0==d) return d;
  return scm_abs(scm_product(n1, scm_quotient(n2, d)));
}

#ifndef SCM_BIGDIG
# ifndef SCM_FLOATS
#  define scm_long2num SCM_MAKINUM
# endif
#endif

#ifndef scm_long2num
SCM_PROC1 (s_logand, "logand", scm_tc7_asubr, scm_logand);
#ifdef __STDC__
SCM
scm_logand(SCM n1, SCM n2)
#else
SCM
scm_logand(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  return scm_long2num(scm_num2long(n1, (char *)SCM_ARG1, s_logand)
		      & scm_num2long(n2, (char *)SCM_ARG2, s_logand));
}

SCM_PROC1 (s_logior, "logior", scm_tc7_asubr, scm_logior);
#ifdef __STDC__
SCM
scm_logior(SCM n1, SCM n2)
#else
SCM
scm_logior(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  return scm_long2num(scm_num2long(n1, (char *)SCM_ARG1, s_logior)
		      | scm_num2long(n2, (char *)SCM_ARG2, s_logior));
}

SCM_PROC1 (s_logxor, "logxor", scm_tc7_asubr, scm_logxor);
#ifdef __STDC__
SCM
scm_logxor(SCM n1, SCM n2)
#else
SCM
scm_logxor(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  return scm_long2num(scm_num2long(n1, (char *)SCM_ARG1, s_logxor)
		      ^ scm_num2long(n2, (char *)SCM_ARG2, s_logxor));
}

SCM_PROC(s_logtest, "logtest", 2, 0, 0, scm_logtest);
#ifdef __STDC__
SCM
scm_logtest(SCM n1, SCM n2)
#else
SCM
scm_logtest(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  return ((scm_num2long (n1, (char *)SCM_ARG1, s_logtest)
	   & scm_num2long (n2, (char *)SCM_ARG2, s_logtest))
	  ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM_PROC(s_logbit_p, "logbit?", 2, 0, 0, scm_logbit_p);
#ifdef __STDC__
SCM
scm_logbit_p(SCM n1, SCM n2)
#else
SCM
scm_logbit_p(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  return (((1 << scm_num2long (n1, (char *)SCM_ARG1, s_logtest))
	   & scm_num2long (n2, (char *)SCM_ARG2, s_logtest))
	  ? SCM_BOOL_T : SCM_BOOL_F);
}

#else

SCM_PROC1 (s_logand, "logand", scm_tc7_asubr, scm_logand);
#ifdef __STDC__
SCM
scm_logand(SCM n1, SCM n2)
#else
SCM
scm_logand(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  SCM_ASSERT(SCM_INUMP(n1), n1, SCM_ARG1, s_logand);
  SCM_ASSERT(SCM_INUMP(n2), n2, SCM_ARG2, s_logand);
  return SCM_MAKINUM(SCM_INUM(n1) & SCM_INUM(n2));
}

SCM_PROC1 (s_logior, "logior", scm_tc7_asubr, scm_logior);
#ifdef __STDC__
SCM
scm_logior(SCM n1, SCM n2)
#else
SCM
scm_logior(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  SCM_ASSERT(SCM_INUMP(n1), n1, SCM_ARG1, s_logior);
  SCM_ASSERT(SCM_INUMP(n2), n2, SCM_ARG2, s_logior);
  return SCM_MAKINUM(SCM_INUM(n1) | SCM_INUM(n2));
}

SCM_PROC1 (s_logxor, "logxor", scm_tc7_asubr, scm_logxor);
#ifdef __STDC__
SCM
scm_logxor(SCM n1, SCM n2)
#else
SCM
scm_logxor(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  SCM_ASSERT(SCM_INUMP(n1), n1, SCM_ARG1, s_logxor);
  SCM_ASSERT(SCM_INUMP(n2), n2, SCM_ARG2, s_logxor);
  return SCM_MAKINUM(SCM_INUM(n1) ^ SCM_INUM(n2));
}

SCM_PROC(s_logtest, "logtest", 2, 0, 0, scm_logtest);
#ifdef __STDC__
SCM
scm_logtest(SCM n1, SCM n2)
#else
SCM
scm_logtest(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  SCM_ASSERT(SCM_INUMP(n1), n1, SCM_ARG1, s_logtest);
  SCM_ASSERT(SCM_INUMP(n2), n2, SCM_ARG2, s_logtest);
  return (SCM_INUM(n1) & SCM_INUM(n2)) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_logbit_p, "logbit?", 2, 0, 0, scm_logbit_p);
#ifdef __STDC__
SCM
scm_logbit_p(SCM n1, SCM n2)
#else
SCM
scm_logbit_p(n1, n2)
     SCM n1;
     SCM n2;
#endif
{
  SCM_ASSERT(SCM_INUMP(n1) && SCM_INUM(n1) >= 0, n1, SCM_ARG1, s_logbit_p);
  SCM_ASSERT(SCM_INUMP(n2), n2, SCM_ARG2, s_logbit_p);
  return ((1 << SCM_INUM(n1)) & SCM_INUM(n2)) ? SCM_BOOL_T : SCM_BOOL_F;
}
#endif

SCM_PROC(s_lognot, "lognot", 1, 0, 0, scm_lognot);
#ifdef __STDC__
SCM
scm_lognot(SCM n)
#else
SCM
scm_lognot(n)
     SCM n;
#endif
{
  SCM_ASSERT(SCM_INUMP(n), n, SCM_ARG1, s_lognot);
  return scm_difference(SCM_MAKINUM(-1L), n);
}

SCM_PROC(s_integer_expt, "integer-expt", 2, 0, 0, scm_integer_expt);
#ifdef __STDC__
SCM
scm_integer_expt(SCM z1, SCM z2)
#else
SCM
scm_integer_expt(z1, z2)
     SCM z1;
     SCM z2;
#endif
{
  SCM acc = SCM_MAKINUM(1L);
#ifdef SCM_BIGDIG
  if (SCM_INUM0==z1 || acc==z1) return z1;
  else if (SCM_MAKINUM(-1L)==z1) return SCM_BOOL_F==scm_even_p(z2)?z1:acc;
#endif
  SCM_ASSERT(SCM_INUMP(z2), z2, SCM_ARG2, s_integer_expt);
  z2 = SCM_INUM(z2);
  if (z2 < 0) {
    z2 = -z2;
    z1 = scm_divide(z1, SCM_UNDEFINED);
  }
  while(1) {
    if (0==z2) return acc;
    if (1==z2) return scm_product(acc, z1);
    if (z2 & 1) acc = scm_product(acc, z1);
    z1 = scm_product(z1, z1);
    z2 >>= 1;
  }
}

SCM_PROC(s_ash, "ash", 2, 0, 0, scm_ash);
#ifdef __STDC__
SCM
scm_ash(SCM n, SCM cnt)
#else
SCM
scm_ash(n, cnt)
     SCM n;
     SCM cnt;
#endif
{
  SCM res = SCM_INUM(n);
  SCM_ASSERT(SCM_INUMP(cnt), cnt, SCM_ARG2, s_ash);
#ifdef SCM_BIGDIG
  if(cnt < 0) {
    res = scm_integer_expt(SCM_MAKINUM(2), SCM_MAKINUM(-SCM_INUM(cnt)));
    if (SCM_NFALSEP(scm_negative_p(n)))
      return scm_sum(SCM_MAKINUM(-1L), scm_quotient(scm_sum(SCM_MAKINUM(1L), n), res));
    else return scm_quotient(n, res);
  }
  else return scm_product(n, scm_integer_expt(SCM_MAKINUM(2), cnt));
#else
  SCM_ASSERT(SCM_INUMP(n), n, SCM_ARG1, s_ash);
  cnt = SCM_INUM(cnt);
  if (cnt < 0) return SCM_MAKINUM(SCM_SRS(res, -cnt));
  res = SCM_MAKINUM(res<<cnt);
  if (SCM_INUM(res)>>cnt != SCM_INUM(n)) 
    SCM_NUM_OVERFLOW (s_ash);
  return res;
#endif
}

SCM_PROC(s_bit_extract, "bit-extract", 3, 0, 0, scm_bit_extract);
#ifdef __STDC__
SCM
scm_bit_extract(SCM n, SCM start, SCM end)
#else
SCM
scm_bit_extract(n, start, end)
     SCM n;
     SCM start;
     SCM end;
#endif
{
  SCM_ASSERT(SCM_INUMP(start), start, SCM_ARG2, s_bit_extract);
  SCM_ASSERT(SCM_INUMP(end), end, SCM_ARG3, s_bit_extract);
  start = SCM_INUM(start); end = SCM_INUM(end);
  SCM_ASSERT(end >= start, SCM_MAKINUM(end), SCM_OUTOFRANGE, s_bit_extract);
#ifdef SCM_BIGDIG
  if SCM_NINUMP(n)
    return
      scm_logand(scm_difference(scm_integer_expt(SCM_MAKINUM(2), SCM_MAKINUM(end - start)),
				SCM_MAKINUM(1L)),
		 scm_ash(n, SCM_MAKINUM(-start)));
#else
  SCM_ASSERT(SCM_INUMP(n), n, SCM_ARG1, s_bit_extract);
#endif
  return SCM_MAKINUM((SCM_INUM(n)>>start) & ((1L<<(end-start))-1));
}

char scm_logtab[] = {0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};
SCM_PROC(s_logcount, "logcount", 1, 0, 0, scm_logcount);
#ifdef __STDC__
SCM
scm_logcount (SCM n)
#else
SCM
scm_logcount(n)
     SCM n;
#endif
{
  register unsigned long c = 0;
  register long nn;
#ifdef SCM_BIGDIG
  if SCM_NINUMP(n) {
    scm_sizet i; SCM_BIGDIG *ds, d;
    SCM_ASSERT(SCM_NIMP(n) && SCM_BIGP(n), n, SCM_ARG1, s_logcount);
    if SCM_BIGSIGN(n) return scm_logcount(scm_difference(SCM_MAKINUM(-1L), n));
    ds = SCM_BDIGITS(n);
    for(i = SCM_NUMDIGS(n); i--; )
      for(d = ds[i]; d; d >>= 4) c += scm_logtab[15 & d];
    return SCM_MAKINUM(c);
  }
#else
  SCM_ASSERT(SCM_INUMP(n), n, SCM_ARG1, s_logcount);
#endif
  if ((nn = SCM_INUM(n)) < 0) nn = -1 - nn;
  for(; nn; nn >>= 4) c += scm_logtab[15 & nn];
  return SCM_MAKINUM(c);
}

char scm_ilentab[] = {0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4};
SCM_PROC(s_integer_length, "integer-length", 1, 0, 0, scm_integer_length);
#ifdef __STDC__
SCM
scm_integer_length(SCM n)
#else
SCM
scm_integer_length(n)
     SCM n;
#endif
{
  register unsigned long c = 0;
  register long nn;
  unsigned int l = 4;
#ifdef SCM_BIGDIG
  if SCM_NINUMP(n) {
    SCM_BIGDIG *ds, d;
    SCM_ASSERT(SCM_NIMP(n) && SCM_BIGP(n), n, SCM_ARG1, s_integer_length);
    if SCM_BIGSIGN(n) return scm_integer_length(scm_difference(SCM_MAKINUM(-1L), n));
    ds = SCM_BDIGITS(n);
    d = ds[c = SCM_NUMDIGS(n)-1];
    for(c *= SCM_BITSPERDIG; d; d >>= 4) {c += 4; l = scm_ilentab[15 & d];}
    return SCM_MAKINUM(c - 4 + l);
  }
#else
  SCM_ASSERT(SCM_INUMP(n), n, SCM_ARG1, s_integer_length);
#endif
  if ((nn = SCM_INUM(n)) < 0) nn = -1 - nn;
  for(;nn; nn >>= 4) {c += 4; l = scm_ilentab[15 & nn];}
  return SCM_MAKINUM(c - 4 + l);
}


#ifdef SCM_BIGDIG
char s_bignum[] = "bignum";
#ifdef __STDC__
SCM
scm_mkbig(scm_sizet nlen, int sign)
#else
SCM
scm_mkbig(nlen, sign)
     scm_sizet nlen;
     int sign;
#endif
{
  SCM v = nlen;
  if (((v << 16) >> 16) != nlen)
    scm_wta(SCM_MAKINUM(nlen), (char *)SCM_NALLOC, s_bignum);
  SCM_NEWCELL(v);
  SCM_DEFER_INTS;
  SCM_SETCHARS(v, scm_must_malloc((long)(nlen*sizeof(SCM_BIGDIG)), s_bignum));
  SCM_SETNUMDIGS(v, nlen, sign?scm_tc16_bigneg:scm_tc16_bigpos);
  SCM_ALLOW_INTS;
  return v;
}

#ifdef __STDC__
SCM
scm_big2inum(SCM b, scm_sizet l)
#else
SCM
scm_big2inum(b, l)
     SCM b;
     scm_sizet l;
#endif
{
  unsigned long num = 0;
  SCM_BIGDIG *tmp = SCM_BDIGITS(b);
  while (l--) num = SCM_BIGUP(num) + tmp[l];
  if (SCM_TYP16(b)==scm_tc16_bigpos) {
    if SCM_POSFIXABLE(num) return SCM_MAKINUM(num);
  }
  else if SCM_UNEGFIXABLE(num) return SCM_MAKINUM(-num);
  return b;
}


char s_adjbig[] = "scm_adjbig";
#ifdef __STDC__
SCM
scm_adjbig(SCM b, scm_sizet nlen)
#else
SCM
scm_adjbig(b, nlen)
     SCM b;
     scm_sizet nlen;
#endif
{
  long nsiz = nlen;
  if (((nsiz << 16) >> 16) != nlen) scm_wta(SCM_MAKINUM(nsiz), (char *)SCM_NALLOC, s_adjbig);
  SCM_DEFER_INTS;
  SCM_SETCHARS(b, (SCM_BIGDIG *)scm_must_realloc((char *)SCM_CHARS(b),
					 (long)(SCM_NUMDIGS(b)*sizeof(SCM_BIGDIG)),
					 (long)(nsiz*sizeof(SCM_BIGDIG)), s_adjbig));
  SCM_SETNUMDIGS(b, nsiz, SCM_TYP16(b));
  SCM_ALLOW_INTS;
  return b;
}


#ifdef __STDC__
SCM
scm_normbig(SCM b)
#else
SCM
scm_normbig(b)
     SCM b;
#endif
{
#ifndef _UNICOS  
  scm_sizet nlen = SCM_NUMDIGS(b);
#else
  int nlen = SCM_NUMDIGS(b);	/* unsigned nlen breaks on Cray when nlen => 0 */
#endif
  SCM_BIGDIG *zds = SCM_BDIGITS(b);
  while (nlen-- && !zds[nlen]); nlen++;
  if (nlen * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM))
    if SCM_INUMP(b = scm_big2inum(b, (scm_sizet)nlen)) return b;
  if (SCM_NUMDIGS(b)==nlen) return b;
  return scm_adjbig(b, (scm_sizet)nlen);
}


#ifdef __STDC__
SCM
scm_copybig(SCM b, int sign)
#else
SCM
scm_copybig(b, sign)
     SCM b;
     int sign;
#endif
{
  scm_sizet i = SCM_NUMDIGS(b);
  SCM ans = scm_mkbig(i, sign);
  SCM_BIGDIG *src = SCM_BDIGITS(b), *dst = SCM_BDIGITS(ans);
  while (i--) dst[i] = src[i];
  return ans;
}


#ifdef __STDC__
SCM
scm_long2big(long n)
#else
SCM
scm_long2big(n)
     long n;
#endif
{
  scm_sizet i = 0;
  SCM_BIGDIG *digits;
  SCM ans = scm_mkbig(SCM_DIGSPERLONG, n<0);
  digits = SCM_BDIGITS(ans);
  if (n < 0) n = -n;
  while (i < SCM_DIGSPERLONG) {
    digits[i++] = SCM_BIGLO(n);
    n = SCM_BIGDN((unsigned long)n);
  }
  return ans;
}

#ifdef LONGLONGS
#ifdef __STDC__
SCM
scm_long_long2big(long_long n)
#else
SCM
scm_long_long2big(n)
     long_long n;
#endif
{
  scm_sizet i;
  SCM_BIGDIG *digits;
  SCM ans;
  int n_digits;

  {
    long tn;
    tn = (long) n;
    if ((long long)tn == n)
      return scm_long2big (tn);
  }

  {
    long_long tn;

    for (tn = n, n_digits = 0;
	 tn;
	 ++n_digits, tn = SCM_BIGDN ((ulong_long)tn))
      ;
  }

  i = 0;
  ans = scm_mkbig(n_digits, n<0);
  digits = SCM_BDIGITS(ans);
  if (n < 0)
    n = -n;
  while (i < n_digits) {
    digits[i++] = SCM_BIGLO(n);
    n = SCM_BIGDN((ulong_long)n);
  }
  return ans;
}
#endif

#ifdef __STDC__
SCM
scm_2ulong2big(unsigned long * np)
#else
SCM
scm_2ulong2big(np)
     unsigned long * np;
#endif
{
  unsigned long n;
  scm_sizet i;
  SCM_BIGDIG *digits;
  SCM ans;

  ans = scm_mkbig(2 * SCM_DIGSPERLONG, 0);
  digits = SCM_BDIGITS(ans);

  n = np[0];
  for (i = 0; i < SCM_DIGSPERLONG; ++i)
    {
      digits[i] = SCM_BIGLO(n);
      n = SCM_BIGDN((unsigned long)n);
    }
  n = np[1];
  for (i = 0; i < SCM_DIGSPERLONG; ++i)
    {
      digits[i + SCM_DIGSPERLONG] = SCM_BIGLO(n);
      n = SCM_BIGDN((unsigned long)n);
    }
  return ans;
}


#ifdef __STDC__
SCM
scm_ulong2big(unsigned long n)
#else
SCM
scm_ulong2big(n)
     unsigned long n;
#endif
{
  scm_sizet i = 0;
  SCM_BIGDIG *digits;
  SCM ans = scm_mkbig(SCM_DIGSPERLONG, 0);
  digits = SCM_BDIGITS(ans);
  while (i < SCM_DIGSPERLONG) {
    digits[i++] = SCM_BIGLO(n);
    n = SCM_BIGDN(n);
  }
  return ans;
}


#ifdef __STDC__
int
scm_bigcomp(SCM x, SCM y)
#else
int
scm_bigcomp(x, y)
     SCM x;
     SCM y;
#endif
{
  int xsign = SCM_BIGSIGN(x);
  int ysign = SCM_BIGSIGN(y);
  scm_sizet xlen, ylen;
  if (ysign < xsign) return 1;
  if (ysign > xsign) return -1;
  if ((ylen = SCM_NUMDIGS(y)) > (xlen = SCM_NUMDIGS(x))) return (xsign) ? -1 : 1;
  if (ylen < xlen) return (xsign) ? 1 : -1;
  while(xlen-- && (SCM_BDIGITS(y)[xlen]==SCM_BDIGITS(x)[xlen]));
  if (-1==xlen) return 0;
  return (SCM_BDIGITS(y)[xlen] > SCM_BDIGITS(x)[xlen]) ?
    (xsign ? -1 : 1) : (xsign ? 1 : -1);
}

#ifndef SCM_DIGSTOOBIG

#ifdef __STDC__
long
scm_pseudolong(long x)
#else
long
scm_pseudolong(x)
     long x;
#endif
{
  union {
    long l;
    SCM_BIGDIG bd[SCM_DIGSPERLONG];
  } p;
  scm_sizet i = 0;
  if (x < 0) x = -x;
  while (i < SCM_DIGSPERLONG) {p.bd[i++] = SCM_BIGLO(x); x = SCM_BIGDN(x);}
  /*  p.bd[0] = SCM_BIGLO(x); p.bd[1] = SCM_BIGDN(x); */
  return p.l;
}

#else

#ifdef __STDC__
void
scm_longdigs(long x, SCM_BIGDIG digs[])
#else
void
scm_longdigs(x, digs)
     long x;
     SCM_BIGDIG digs[];
#endif
{
  scm_sizet i = 0;
  if (x < 0) x = -x;
  while (i < SCM_DIGSPERLONG) {digs[i++] = SCM_BIGLO(x); x = SCM_BIGDN(x);}
}
#endif


#ifdef __STDC__
SCM
scm_addbig(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy, int sgny)
#else
SCM
scm_addbig(x, nx, xsgn, bigy, sgny)
     SCM_BIGDIG *x;
     scm_sizet nx;
     int xsgn;
     SCM bigy;
     int sgny;
#endif
{
  /* Assumes nx <= SCM_NUMDIGS(bigy) */
  /* Assumes xsgn and sgny scm_equal either 0 or 0x0100 */
  long num = 0;
  scm_sizet i = 0, ny = SCM_NUMDIGS(bigy);
  SCM z = scm_copybig(bigy, SCM_BIGSIGN(bigy) ^ sgny);
  SCM_BIGDIG *zds = SCM_BDIGITS(z);
  if (xsgn ^ SCM_BIGSIGN(z)) {
    do {
      num += (long) zds[i] - x[i];
      if (num < 0) {zds[i] = num + SCM_BIGRAD; num = -1;}
      else {zds[i] = SCM_BIGLO(num); num = 0;}
    } while (++i < nx);
    if (num && nx==ny) {
      num = 1; i = 0;
      SCM_CAR(z) ^= 0x0100;
      do {
	num += (SCM_BIGRAD-1) - zds[i];
	zds[i++] = SCM_BIGLO(num);
	num = SCM_BIGDN(num);
      } while (i < ny);
    }
    else while (i < ny) {
      num += zds[i];
      if (num < 0) {zds[i++] = num + SCM_BIGRAD; num = -1;}
      else {zds[i++] = SCM_BIGLO(num); num = 0;}
    }
  } else {
    do {
      num += (long) zds[i] + x[i];
      zds[i++] = SCM_BIGLO(num);
      num = SCM_BIGDN(num);
    } while (i < nx);
    if (!num) return z;
    while (i < ny) {
      num += zds[i];
      zds[i++] = SCM_BIGLO(num);
      num = SCM_BIGDN(num);
      if (!num) return z;
    }
    if (num) {z = scm_adjbig(z, ny+1); SCM_BDIGITS(z)[ny] = num; return z;}
  }
  return scm_normbig(z);
}

#ifdef __STDC__
SCM
scm_mulbig(SCM_BIGDIG *x, scm_sizet nx, SCM_BIGDIG *y, scm_sizet ny, int sgn)
#else
SCM
scm_mulbig(x, nx, y, ny, sgn)
     SCM_BIGDIG *x;
     scm_sizet nx;
     SCM_BIGDIG *y;
     scm_sizet ny;
     int sgn;
#endif
{
  scm_sizet i = 0, j = nx + ny;
  unsigned long n = 0;
  SCM z = scm_mkbig(j, sgn);
  SCM_BIGDIG *zds = SCM_BDIGITS(z);
  while (j--) zds[j] = 0;
  do {
    j = 0;
    if (x[i]) {
      do {
	n += zds[i + j] + ((unsigned long) x[i] * y[j]);
	zds[i + j++] = SCM_BIGLO(n);
	n = SCM_BIGDN(n);
      } while (j < ny);
      if (n) {zds[i + j] = n; n = 0;}
    }
  } while (++i < nx);
  return scm_normbig(z);
}

#ifdef __STDC__
unsigned int
scm_divbigdig(SCM_BIGDIG *ds, scm_sizet h, SCM_BIGDIG div)
#else
unsigned int
scm_divbigdig(ds, h, div)
     SCM_BIGDIG *ds;
     scm_sizet h;
     SCM_BIGDIG div;
#endif
{
  register unsigned long t2 = 0;
  while(h--) {
    t2 = SCM_BIGUP(t2) + ds[h];
    ds[h] = t2 / div;
    t2 %= div;
  }
  return t2;
}


#ifdef __STDC__
SCM
scm_divbigint(SCM x, long z, int sgn, int mode)
#else
SCM
scm_divbigint(x, z, sgn, mode)
     SCM x;
     long z;
     int sgn;
     int mode;
#endif
{
  if (z < 0) z = -z;
  if (z < SCM_BIGRAD) {
    register unsigned long t2 = 0;
    register SCM_BIGDIG *ds = SCM_BDIGITS(x);
    scm_sizet nd = SCM_NUMDIGS(x);
    while(nd--) t2 = (SCM_BIGUP(t2) + ds[nd]) % z;
    if (mode) t2 = z - t2;
    return SCM_MAKINUM(sgn ? -t2 : t2);
  }
  {
#ifndef SCM_DIGSTOOBIG
    unsigned long t2 = scm_pseudolong(z);
    return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), (SCM_BIGDIG *)&t2,
			 SCM_DIGSPERLONG, sgn, mode); 
#else
    SCM_BIGDIG t2[SCM_DIGSPERLONG];
    scm_longdigs(z, t2);
    return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), t2, SCM_DIGSPERLONG, sgn, mode);
#endif
  }
}

#ifdef __STDC__
SCM
scm_divbigbig(SCM_BIGDIG *x, scm_sizet nx, SCM_BIGDIG *y, scm_sizet ny, int sgn, int modes)
#else
SCM
scm_divbigbig(x, nx, y, ny, sgn, modes)
     SCM_BIGDIG *x;
     scm_sizet nx;
     SCM_BIGDIG *y;
     scm_sizet ny;
     int sgn;
     int modes;
#endif
{
  /* modes description
     0	remainder
     1	scm_modulo
     2	quotient
     3	quotient but returns 0 if division is not exact. */
  scm_sizet i = 0, j = 0;
  long num = 0;
  unsigned long t2 = 0;
  SCM z, newy;
  SCM_BIGDIG  d = 0, qhat, *zds, *yds;
  /* algorithm requires nx >= ny */
  if (nx < ny)
    switch (modes) {
    case 0:			/* remainder -- just return x */
      z = scm_mkbig(nx, sgn); zds = SCM_BDIGITS(z);
      do {zds[i] = x[i];} while (++i < nx);
      return z;
    case 1:			/* scm_modulo -- return y-x */
      z = scm_mkbig(ny, sgn); zds = SCM_BDIGITS(z);
      do {
	num += (long) y[i] - x[i];
	if (num < 0) {zds[i] = num + SCM_BIGRAD; num = -1;}
	else {zds[i] = num; num = 0;}
      } while (++i < nx);
      while (i < ny) {
	num += y[i];
	if (num < 0) {zds[i++] = num + SCM_BIGRAD; num = -1;}
	else {zds[i++] = num; num = 0;}
      }
      goto doadj;
    case 2: return SCM_INUM0;	/* quotient is zero */
    case 3: return 0;		/* the division is not exact */
    }

  z = scm_mkbig(nx==ny ? nx+2 : nx+1, sgn); zds = SCM_BDIGITS(z);
  if (nx==ny) zds[nx+1] = 0;
  while(!y[ny-1]) ny--;		/* in case y came in as a psuedolong */
  if (y[ny-1] < (SCM_BIGRAD>>1)) {  /* normalize operands */
    d = SCM_BIGRAD/(y[ny-1]+1);
    newy = scm_mkbig(ny, 0); yds = SCM_BDIGITS(newy);
    while(j < ny)
      {t2 += (unsigned long) y[j]*d; yds[j++] = SCM_BIGLO(t2); t2 = SCM_BIGDN(t2);}
    y = yds; j = 0; t2 = 0;
    while(j < nx)
      {t2 += (unsigned long) x[j]*d; zds[j++] = SCM_BIGLO(t2); t2 = SCM_BIGDN(t2);}
    zds[j] = t2;
  }
  else {zds[j = nx] = 0; while (j--) zds[j] = x[j];}
  j = nx==ny ? nx+1 : nx;	/* dividend needs more digits than divisor */
  do {				/* loop over digits of quotient */
    if (zds[j]==y[ny-1]) qhat = SCM_BIGRAD-1;
    else qhat = (SCM_BIGUP(zds[j]) + zds[j-1])/y[ny-1];
    if (!qhat) continue;
    i = 0; num = 0; t2 = 0;
    do {			/* multiply and subtract */
      t2 += (unsigned long) y[i] * qhat;
      num += zds[j - ny + i] - SCM_BIGLO(t2);
      if (num < 0) {zds[j - ny + i] = num + SCM_BIGRAD; num = -1;}
      else {zds[j - ny + i] = num; num = 0;}
      t2 = SCM_BIGDN(t2);
    } while (++i < ny);
    num += zds[j - ny + i] - t2; /* borrow from high digit; don't update */
    while (num) {		/* "add back" required */
      i = 0; num = 0; qhat--;
      do {
	num += (long) zds[j - ny + i] + y[i];
	zds[j - ny + i] = SCM_BIGLO(num);
	num = SCM_BIGDN(num);
      } while (++i < ny);
      num--;
    }
    if (modes & 2) zds[j] = qhat;
  } while (--j >= ny);
  switch (modes) {
  case 3:			/* check that remainder==0 */
    for(j = ny;j && !zds[j-1];--j) ; if (j) return 0;
  case 2:			/* move quotient down in z */
    j = (nx==ny ? nx+2 : nx+1) - ny;
    for (i = 0;i < j;i++) zds[i] = zds[i+ny];
    ny = i;
    break;
  case 1:			/* subtract for scm_modulo */
    i = 0; num = 0; j = 0;
    do {num += y[i] - zds[i];
	j = j | zds[i];
	if (num < 0) {zds[i] = num + SCM_BIGRAD; num = -1;}
	else {zds[i] = num; num = 0;}
      } while (++i < ny);
    if (!j) return SCM_INUM0;
  case 0:			/* just normalize remainder */
    if (d) scm_divbigdig(zds, ny, d);
  }
 doadj:
  for(j = ny;j && !zds[j-1];--j) ;
  if (j * SCM_BITSPERDIG <= sizeof(SCM)*SCM_CHAR_BIT)
    if SCM_INUMP(z = scm_big2inum(z, j)) return z;
  return scm_adjbig(z, j);
}
#endif





/*** NUMBERS -> STRINGS ***/
#ifdef SCM_FLOATS
int scm_dblprec;
static double fx[] = {0.0, 5e-1, 5e-2, 5e-3, 5e-4, 5e-5,
			5e-6, 5e-7, 5e-8, 5e-9, 5e-10,
			5e-11,5e-12,5e-13,5e-14,5e-15,
			5e-16,5e-17,5e-18,5e-19,5e-20};



#ifdef __STDC__
static scm_sizet
idbl2str(double f, char *a)
#else
static scm_sizet
idbl2str(f, a)
     double f;
     char *a;
#endif
{
  int efmt, dpt, d, i, wp = scm_dblprec;
  scm_sizet ch = 0;
  int exp = 0;

  if (f == 0.0) goto zero;	/*{a[0]='0'; a[1]='.'; a[2]='0'; return 3;}*/
  if (f < 0.0) {f = -f;a[ch++]='-';}
  else if (f > 0.0) ;
  else goto funny;
  if (IS_INF(f))
    {
      if (ch == 0) a[ch++]='+';
    funny: a[ch++]='#'; a[ch++]='.'; a[ch++]='#'; return ch;
    }
# ifdef DBL_MIN_10_EXP		/* Prevent unnormalized values, as from 
				   make-uniform-vector, from causing infinite loops. */
  while (f < 1.0) {f *= 10.0;  if (exp-- < DBL_MIN_10_EXP) goto funny;}
  while (f > 10.0) {f *= 0.10; if (exp++ > DBL_MAX_10_EXP) goto funny;}
# else
  while (f < 1.0) {f *= 10.0; exp--;}
  while (f > 10.0) {f /= 10.0; exp++;}
# endif
  if (f+fx[wp] >= 10.0) {f = 1.0; exp++;}
 zero:
# ifdef ENGNOT
  dpt = (exp+9999)%3;
  exp -= dpt++;
  efmt = 1;
# else
  efmt = (exp < -3) || (exp > wp+2);
  if (!efmt)
    if (exp < 0) {
      a[ch++] = '0';
      a[ch++] = '.';
      dpt = exp;
      while (++dpt)  a[ch++] = '0';
    } else
      dpt = exp+1;
  else
    dpt = 1;
# endif

  do {
    d = f;
    f -= d;
    a[ch++] = d+'0';
    if (f < fx[wp])  break;
    if (f+fx[wp] >= 1.0) {
      a[ch-1]++;
      break;
    }
    f *= 10.0;
    if (!(--dpt))  a[ch++] = '.';
  } while (wp--);

  if (dpt > 0)
# ifndef ENGNOT
    if ((dpt > 4) && (exp > 6)) {
      d = (a[0]=='-'?2:1);
      for (i = ch++; i > d; i--)
	a[i] = a[i-1];
      a[d] = '.';
      efmt = 1;
    } else
# endif
      {
	while (--dpt)  a[ch++] = '0';
	a[ch++] = '.';
      }
  if (a[ch-1]=='.')  a[ch++]='0'; /* trailing zero */
  if (efmt && exp) {
    a[ch++] = 'e';
    if (exp < 0) {
      exp = -exp;
      a[ch++] = '-';
    }
    for (i = 10; i <= exp; i *= 10);
    for (i /= 10; i; i /= 10) {
      a[ch++] = exp/i + '0';
      exp %= i;
    }
  }
  return ch;
}

#ifdef __STDC__
static scm_sizet
iflo2str(SCM flt, char *str)
#else
static scm_sizet
iflo2str(flt, str)
     SCM flt;
     char *str;
#endif
{
  scm_sizet i;
# ifdef SCM_SINGLES
  if SCM_SINGP(flt) i = idbl2str(SCM_FLO(flt), str);
  else
# endif
    i = idbl2str(SCM_REAL(flt), str);
  if SCM_CPLXP(flt) {
    if(0 <= SCM_IMAG(flt))		/* jeh */
      str[i++] = '+';		/* jeh */
    i += idbl2str(SCM_IMAG(flt), &str[i]);
    str[i++] = 'i';
  }
  return i;
}
#endif				/* SCM_FLOATS */

#ifdef __STDC__
scm_sizet
scm_iint2str(long num, int rad, char *p)
#else
scm_sizet
scm_iint2str(num, rad, p)
     long num;
     int rad;
     char *p;
#endif
{
  scm_sizet j;
  register int i = 1, d;
  register long n = num;
  if (n < 0) {n = -n; i++;}
  for (n /= rad;n > 0;n /= rad) i++;
  j = i;
  n = num;
  if (n < 0) {n = -n; *p++ = '-'; i--;}
  while (i--) {
    d = n % rad;
    n /= rad;
    p[i] = d + ((d < 10) ? '0' : 'a' - 10);
  }
  return j;
}


#ifdef SCM_BIGDIG
#ifdef __STDC__
static SCM
big2str(SCM b, register unsigned int radix)
#else
static SCM
big2str(b, radix)
     SCM b;
     register unsigned int radix;
#endif
{
  SCM t = scm_copybig(b, 0);	/* sign of temp doesn't matter */
  register SCM_BIGDIG *ds = SCM_BDIGITS(t);
  scm_sizet i = SCM_NUMDIGS(t);
  scm_sizet j = radix==16 ? (SCM_BITSPERDIG*i)/4+2
    : radix >= 10 ? (SCM_BITSPERDIG*i*241L)/800+2
      : (SCM_BITSPERDIG*i)+2;
  scm_sizet k = 0;
  scm_sizet radct = 0;
  scm_sizet ch;			/* jeh */
  SCM_BIGDIG radpow = 1, radmod = 0;
  SCM ss = scm_makstr((long)j, 0);
  char *s = SCM_CHARS(ss), c;
  while ((long) radpow * radix < SCM_BIGRAD) {
    radpow *= radix;
    radct++;
  }
  s[0] = scm_tc16_bigneg==SCM_TYP16(b) ? '-' : '+';
  while ((i || radmod) && j) {
    if (k == 0) {
      radmod = (SCM_BIGDIG)scm_divbigdig(ds, i, radpow);
      k = radct;
      if (!ds[i-1]) i--;
    }
    c = radmod % radix; radmod /= radix; k--;
    s[--j] = c < 10 ? c + '0' : c + 'a' - 10;
  }
  ch = s[0] == '-' ? 1 : 0;	/* jeh */
  if (ch < j) {			/* jeh */
    for(i = j;j < SCM_LENGTH(ss);j++) s[ch+j-i] = s[j]; /* jeh */
    scm_vector_set_length_x(ss, (SCM)SCM_MAKINUM(ch+SCM_LENGTH(ss)-i)); /* jeh */
  }
  return ss;
}
#endif


SCM_PROC(s_number_to_string, "number->string", 1, 1, 0, scm_number_to_string);
#ifdef __STDC__
SCM
scm_number_to_string(SCM x, SCM radix)
#else
SCM
scm_number_to_string(x, radix)
     SCM x;
     SCM radix;
#endif
{
  if SCM_UNBNDP(radix) radix=SCM_MAKINUM(10L);
  else SCM_ASSERT(SCM_INUMP(radix), radix, SCM_ARG2, s_number_to_string);
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
    char num_buf[SCM_FLOBUFLEN];
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(x), badx);
    if SCM_BIGP(x) return big2str(x, (unsigned int)SCM_INUM(radix));
#  ifndef RECKLESS
    if (!(SCM_INEXP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_number_to_string);
#  endif
# else
    SCM_ASSERT(SCM_NIMP(x) && SCM_INEXP(x), x, SCM_ARG1, s_number_to_string);
# endif
    return scm_makfromstr(num_buf, iflo2str(x, num_buf), 0);
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_number_to_string);
    return big2str(x, (unsigned int)SCM_INUM(radix));
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_number_to_string);
# endif
#endif
  {
    char num_buf[SCM_INTBUFLEN];
    return scm_makfromstr(num_buf,
			  scm_iint2str(SCM_INUM(x), (int)SCM_INUM(radix), num_buf), 0);
  }
}


/* These print routines are stubbed here so that scm_repl.c doesn't need
   SCM_FLOATS or SCM_BIGDIGs conditionals */
#ifdef __STDC__
int
scm_floprint(SCM sexp, SCM port, int writing)
#else
int
scm_floprint(sexp, port, writing)
     SCM sexp;
     SCM port;
     int writing;
#endif
{
#ifdef SCM_FLOATS
  char num_buf[SCM_FLOBUFLEN];
  scm_gen_write (scm_regular_string, num_buf, iflo2str(sexp, num_buf), port);
#else
  scm_ipruk("float", sexp, port);
#endif
  return !0;
}


#ifdef __STDC__
int
scm_bigprint(SCM exp, SCM port, int writing)
#else
int
scm_bigprint(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
#ifdef SCM_BIGDIG
  exp = big2str(exp, (unsigned int)10);
  scm_gen_write (scm_regular_string, SCM_CHARS(exp), (scm_sizet)SCM_LENGTH(exp), port);
#else
  scm_ipruk("bignum", exp, port);
#endif
  return !0;
}
/*** END nums->strs ***/

/*** STRINGS -> NUMBERS ***/
#ifdef SCM_BIGDIG
#ifdef __STDC__
SCM
scm_istr2int(char *str, long len, long radix)
#else
SCM
scm_istr2int(str, len, radix)
     char *str;
     long len;
     long radix;
#endif
{
  scm_sizet j;
  register scm_sizet k, blen = 1;
  scm_sizet i = 0;
  int c;
  SCM res;
  register SCM_BIGDIG *ds;
  register unsigned long t2;

  if (0 >= len) return SCM_BOOL_F;	/* zero scm_length */
  if (16==radix) j = 1+(4*len*sizeof(char))/(SCM_BITSPERDIG);
  else if (10 <= radix)
    j = 1+(84*len*sizeof(char))/(SCM_BITSPERDIG*25);
  else j = 1+(len*sizeof(char))/(SCM_BITSPERDIG);
  switch (str[0]) {		/* leading sign */
  case '-':
  case '+': if (++i==len) return SCM_BOOL_F; /* bad if lone `+' or `-' */
  }
  res = scm_mkbig(j, '-'==str[0]);
  ds = SCM_BDIGITS(res);
  for (k = j;k--;) ds[k] = 0;
  do {
    switch (c = str[i++]) {
    case DIGITS:
      c = c - '0';
      goto accumulate;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      c = c-'A'+10;
      goto accumulate;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      c = c-'a'+10;
    accumulate:
      if (c >= radix) return SCM_BOOL_F; /* bad digit for radix */
      k = 0;
      t2 = c;
    moretodo:
      while(k < blen) {
	/*	printf("k = %d, blen = %d, t2 = %ld, ds[k] = %d\n", k, blen, t2, ds[k]);*/
	t2 += ds[k]*radix;
	ds[k++] = SCM_BIGLO(t2);
	t2 = SCM_BIGDN(t2);
      }
      if (blen > j)
	SCM_NUM_OVERFLOW ("bignum");
      if (t2) {blen++; goto moretodo;}
      break;
    default:
      return SCM_BOOL_F;		/* not a digit */
    }
  } while (i < len);
  if (blen * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM))
    if SCM_INUMP(res = scm_big2inum(res, blen)) return res;
  if (j==blen) return res;
  return scm_adjbig(res, blen);
}
#else



#ifdef __STDC__
SCM
scm_istr2int(char *str, long len, long radix)
#else
SCM
scm_istr2int(str, len, radix)
     char *str;
     long len;
     long radix;
#endif
{
  register long n = 0, ln;
  register int c;
  register int i = 0;
  int lead_neg = 0;
  if (0 >= len) return SCM_BOOL_F;	/* zero scm_length */
  switch (*str) {		/* leading sign */
  case '-': lead_neg = 1;
  case '+': if (++i==len) return SCM_BOOL_F; /* bad if lone `+' or `-' */
  }

  do {
    switch (c = str[i++]) {
    case DIGITS:
      c = c - '0';
      goto accumulate;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      c = c-'A'+10;
      goto accumulate;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      c = c-'a'+10;
    accumulate:
      if (c >= radix) return SCM_BOOL_F; /* bad digit for radix */
      ln = n;
      n = n * radix - c;
      /* Negation is a workaround for HP700 cc bug */
      if (n > ln || (-n > -SCM_MOST_NEGATIVE_FIXNUM)) goto ovfl;
      break;
    default:
      return SCM_BOOL_F;		/* not a digit */
    }
  } while (i < len);
  if (!lead_neg) if ((n = -n) > SCM_MOST_POSITIVE_FIXNUM) goto ovfl;
  return SCM_MAKINUM(n);
 ovfl:				/* overflow scheme integer */
  return SCM_BOOL_F;
}
#endif

#ifdef SCM_FLOATS
#ifdef __STDC__
SCM
scm_istr2flo(char *str, long len, long radix)
#else
SCM
scm_istr2flo(str, len, radix)
     char *str;
     long len;
     long radix;
#endif
{
  register int c, i = 0;
  double lead_sgn;
  double res = 0.0, tmp = 0.0;
  int flg = 0;
  int point = 0;
  SCM second;

  if (i >= len) return SCM_BOOL_F;	/* zero scm_length */

  switch (*str) {		/* leading sign */
  case '-': lead_sgn = -1.0; i++; break;
  case '+': lead_sgn = 1.0; i++; break;
  default : lead_sgn = 0.0;
  }
  if (i==len) return SCM_BOOL_F;	/* bad if lone `+' or `-' */

  if (str[i]=='i' || str[i]=='I') { /* handle `+i' and `-i'   */
    if (lead_sgn==0.0) return SCM_BOOL_F; /* must have leading sign */
    if (++i < len) return SCM_BOOL_F; /* `i' not last character */
    return scm_makdbl(0.0, lead_sgn);
  }
  do {				/* check initial digits */
    switch (c = str[i]) {
    case DIGITS:
      c = c - '0';
      goto accum1;
    case 'D': case 'E': case 'F':
      if (radix==10) goto out1; /* must be exponent */
    case 'A': case 'B': case 'C':
      c = c-'A'+10;
      goto accum1;
    case 'd': case 'e': case 'f':
      if (radix==10) goto out1;
    case 'a': case 'b': case 'c':
      c = c-'a'+10;
    accum1:
      if (c >= radix) return SCM_BOOL_F; /* bad digit for radix */
      res = res * radix + c;
      flg = 1;			/* res is valid */
      break;
    default:
      goto out1;
    }
  } while (++i < len);
 out1:

  /* if true, then we did see a digit above, and res is valid */
  if (i==len) goto done;

  /* By here, must have seen a digit,
     or must have next char be a `.' with radix==10 */
  if (!flg)
    if (!(str[i]=='.' && radix==10))
      return SCM_BOOL_F;

  while (str[i]=='#') {		/* optional sharps */
    res *= radix;
    if (++i==len) goto done;
  }

  if (str[i]=='/') {
    while (++i < len) {
      switch (c = str[i]) {
      case DIGITS:
	c = c - '0';
	goto accum2;
      case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	c = c-'A'+10;
	goto accum2;
      case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	c = c-'a'+10;
      accum2:
	if (c >= radix) return SCM_BOOL_F;
	tmp = tmp * radix + c;
	break;
      default:
	goto out2;
      }
    }
  out2:
    if (tmp==0.0) return SCM_BOOL_F; /* `slash zero' not allowed */
    if (i < len)
      while (str[i]=='#') {	/* optional sharps */
	tmp *= radix;
	if (++i==len) break;
      }
    res /= tmp;
    goto done;
  }

  if (str[i]=='.') {		/* decimal point notation */
    if (radix != 10) return SCM_BOOL_F; /* must be radix 10 */
    while (++i < len) {
      switch (c = str[i]) {
      case DIGITS:
	point--;
	res = res*10.0 + c-'0';
	flg = 1;
	break;
      default:
	goto out3;
      }
    }
  out3:
    if (!flg) return SCM_BOOL_F;	/* no digits before or after decimal point */
    if (i==len) goto adjust;
    while (str[i]=='#') {	/* ignore remaining sharps */
      if (++i==len) goto adjust;
    }
  }

  switch (str[i]) {		/* exponent */
  case 'd': case 'D':
  case 'e': case 'E':
  case 'f': case 'F':
  case 'l': case 'L':
  case 's': case 'S': {
    int expsgn = 1, expon = 0;
    if (radix != 10) return SCM_BOOL_F; /* only in radix 10 */
    if (++i==len) return SCM_BOOL_F; /* bad exponent */
    switch (str[i]) {
    case '-':  expsgn=(-1);
    case '+':  if (++i==len) return SCM_BOOL_F; /* bad exponent */
    }
    if (str[i] < '0' || str[i] > '9') return SCM_BOOL_F; /* bad exponent */
    do {
      switch (c = str[i]) {
      case DIGITS:
	expon = expon*10 + c-'0';
	if (expon > MAXEXP)  return SCM_BOOL_F; /* exponent too large */
	break;
      default:
	goto out4;
      }
    } while (++i < len);
  out4:
    point += expsgn*expon;
  }
  }

 adjust:
  if (point >= 0)
    while (point--)  res *= 10.0;
  else
# ifdef _UNICOS
    while (point++)  res *= 0.1; 
# else
  while (point++)  res /= 10.0;
# endif

 done:
  /* at this point, we have a legitimate floating point result */
  if (lead_sgn==-1.0)  res = -res;
  if (i==len) return scm_makdbl(res, 0.0);

  if (str[i]=='i' || str[i]=='I') { /* pure imaginary number  */
    if (lead_sgn==0.0) return SCM_BOOL_F; /* must have leading sign */
    if (++i < len) return SCM_BOOL_F; /* `i' not last character */
    return scm_makdbl(0.0, res);
  }

  switch (str[i++]) {
  case '-':  lead_sgn = -1.0; break;
  case '+':  lead_sgn = 1.0;  break;
  case '@': {			/* polar input for complex number */
    /* get a `real' for scm_angle */
    second = scm_istr2flo(&str[i], (long)(len-i), radix);
    if (!(SCM_INEXP(second))) return SCM_BOOL_F; /* not `real' */
    if (SCM_CPLXP(second))    return SCM_BOOL_F; /* not `real' */
    tmp = SCM_REALPART(second);
    return scm_makdbl(res*cos(tmp), res*sin(tmp));
  }
  default: return SCM_BOOL_F;
  }

  /* at this point, last char must be `i' */
  if (str[len-1] != 'i' && str[len-1] != 'I') return SCM_BOOL_F;
  /* handles `x+i' and `x-i' */
  if (i==(len-1))  return scm_makdbl(res, lead_sgn);
  /* get a `ureal' for complex part */
  second = scm_istr2flo(&str[i], (long)((len-i)-1), radix);
  if (!(SCM_INEXP(second))) return SCM_BOOL_F; /* not `ureal' */
  if (SCM_CPLXP(second))    return SCM_BOOL_F; /* not `ureal' */
  tmp = SCM_REALPART(second);
  if (tmp < 0.0)	return SCM_BOOL_F; /* not `ureal' */
  return scm_makdbl(res, (lead_sgn*tmp));
}
#endif				/* SCM_FLOATS */


#ifdef __STDC__
SCM
scm_istring2number(char *str, long len, long radix)
#else
SCM
scm_istring2number(str, len, radix)
     char *str;
     long len;
     long radix;
#endif
{
  int i = 0;
  char ex = 0;
  char ex_p = 0, rx_p = 0;	/* Only allow 1 exactness and 1 radix prefix */
  SCM res;
  if (len==1)
    if (*str=='+' || *str=='-') /* Catches lone `+' and `-' for speed */
      return SCM_BOOL_F;

  while ((len-i) >= 2  &&  str[i]=='#' && ++i)
    switch (str[i++]) {
    case 'b': case 'B':  if (rx_p++) return SCM_BOOL_F; radix = 2;  break;
    case 'o': case 'O':  if (rx_p++) return SCM_BOOL_F; radix = 8;  break;
    case 'd': case 'D':  if (rx_p++) return SCM_BOOL_F; radix = 10; break;
    case 'x': case 'X':  if (rx_p++) return SCM_BOOL_F; radix = 16; break;
    case 'i': case 'I':  if (ex_p++) return SCM_BOOL_F; ex = 2;     break;
    case 'e': case 'E':  if (ex_p++) return SCM_BOOL_F; ex = 1;     break;
    default:  return SCM_BOOL_F;
    }

  switch (ex) {
  case 1:
    return scm_istr2int(&str[i], len-i, radix);
  case 0:
    res = scm_istr2int(&str[i], len-i, radix);
    if SCM_NFALSEP(res) return res;
#ifdef SCM_FLOATS
  case 2: return scm_istr2flo(&str[i], len-i, radix);
#endif
  }
  return SCM_BOOL_F;
}


SCM_PROC(s_string_to_number, "string->number", 1, 1, 0, scm_string_to_number);
#ifdef __STDC__
SCM
scm_string_to_number(SCM str, SCM radix)
#else
SCM
scm_string_to_number(str, radix)
     SCM str;
     SCM radix;
#endif
{
  SCM answer;
  if SCM_UNBNDP(radix) radix=SCM_MAKINUM(10L);
  else SCM_ASSERT(SCM_INUMP(radix), radix, SCM_ARG2, s_string_to_number);
  SCM_ASSERT(SCM_NIMP(str) && SCM_ROSTRINGP(str), str, SCM_ARG1, s_string_to_number);
  answer = scm_istring2number(SCM_ROCHARS(str), SCM_ROLENGTH(str), SCM_INUM(radix));
  return scm_return_first (answer, str);
}
/*** END strs->nums ***/

#ifdef SCM_FLOATS
#ifdef __STDC__
SCM
scm_makdbl (double x, double y)
#else
SCM
scm_makdbl (x, y)
     double x;
     double y;
#endif
{
  SCM z;
  if ((y==0.0) && (x==0.0)) return scm_flo0;
  SCM_NEWCELL(z);
  SCM_DEFER_INTS;
  if (y==0.0) {
# ifdef SCM_SINGLES
    float fx = x;
#  ifndef SCM_SINGLESONLY
    if ((-FLTMAX < x) && (x < FLTMAX) && (fx==x))
#  endif
      {
	SCM_CAR(z) = scm_tc_flo;
	SCM_FLO(z) = x;
	SCM_ALLOW_INTS;
	return z;
      }
# endif/* def SCM_SINGLES */
    SCM_CDR(z) = (SCM)scm_must_malloc(1L*sizeof(double), "real");
    SCM_CAR(z) = scm_tc_dblr;
  }
  else {
    SCM_CDR(z) = (SCM)scm_must_malloc(2L*sizeof(double), "complex");
    SCM_CAR(z) = scm_tc_dblc;
    SCM_IMAG(z) = y;
  }
  SCM_REAL(z) = x;
  SCM_ALLOW_INTS;
  return z;
}
#endif


#ifdef __STDC__
SCM
scm_bigequal(SCM x, SCM y)
#else
SCM
scm_bigequal(x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_BIGDIG
  if (0==scm_bigcomp(x, y)) return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}


#ifdef __STDC__
SCM
scm_floequal(SCM x, SCM y)
#else
SCM
scm_floequal(x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_FLOATS
  if (SCM_REALPART(x) != SCM_REALPART(y)) return SCM_BOOL_F;
  if (!(SCM_CPLXP(x) && (SCM_IMAG(x) != SCM_IMAG(y)))) return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}




SCM_PROC(s_number_p, "number?", 1, 0, 0, scm_number_p);
SCM_PROC(s_complex_p, "complex?", 1, 0, 0, scm_number_p);
#ifdef __STDC__
SCM
scm_number_p(SCM x)
#else
SCM
scm_number_p(x)
     SCM x;
#endif
{
  if SCM_INUMP(x) return SCM_BOOL_T;
#ifdef SCM_FLOATS
  if (SCM_NIMP(x) && SCM_NUMP(x)) return SCM_BOOL_T;
#else
# ifdef SCM_BIGDIG
  if (SCM_NIMP(x) && SCM_NUMP(x)) return SCM_BOOL_T;
# endif
#endif
  return SCM_BOOL_F;
}



#ifdef SCM_FLOATS
SCM_PROC(s_real_p, "real?", 1, 0, 0, scm_real_p);
SCM_PROC(s_rational_p, "rational?", 1, 0, 0, scm_real_p);
#ifdef __STDC__
SCM
scm_real_p(SCM x)
#else
SCM
scm_real_p(x)
     SCM x;
#endif
{
  if (SCM_INUMP(x))
    return SCM_BOOL_T;
  if (SCM_IMP(x))
    return SCM_BOOL_F;
  if (SCM_REALP(x))
    return SCM_BOOL_T;
# ifdef SCM_BIGDIG
  if (SCM_BIGP(x))
    return SCM_BOOL_T;
# endif
  return SCM_BOOL_F;
}



SCM_PROC(s_int_p, "int?", 1, 0, 0, scm_int_p);
#ifdef __STDC__
SCM
scm_int_p(SCM x)
#else
SCM
scm_int_p(x)
     SCM x;
#endif
{
  double r;
  if SCM_INUMP(x) return SCM_BOOL_T;
  if SCM_IMP(x) return SCM_BOOL_F;
# ifdef SCM_BIGDIG
  if SCM_BIGP(x) return SCM_BOOL_T;
# endif
  if (!SCM_INEXP(x)) return SCM_BOOL_F;
  if SCM_CPLXP(x) return SCM_BOOL_F;
  r = SCM_REALPART(x);
  if (r==floor(r)) return SCM_BOOL_T;
  return SCM_BOOL_F;
}



#endif				/* SCM_FLOATS */

SCM_PROC(s_inexact_p, "inexact?", 1, 0, 0, scm_inexact_p);
#ifdef __STDC__
SCM
scm_inexact_p(SCM x)
#else
SCM
scm_inexact_p(x)
     SCM x;
#endif
{
#ifdef SCM_FLOATS
  if (SCM_NIMP(x) && SCM_INEXP(x)) return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}




SCM_PROC1 (s_eq_p, "=?", scm_tc7_rpsubr, scm_num_eq_p);
#ifdef __STDC__
SCM
scm_num_eq_p (SCM x, SCM y)
#else
SCM
scm_num_eq_p (x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_FLOATS
  SCM t;
  if SCM_NINUMP(x) {
# ifdef SCM_BIGDIG
#  ifndef RECKLESS
    if (!(SCM_NIMP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_eq_p);
#  endif
    if SCM_BIGP(x) {
      if SCM_INUMP(y) return SCM_BOOL_F;
      SCM_ASRTGO(SCM_NIMP(y), bady);
      if SCM_BIGP(y) return (0==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
      SCM_ASRTGO(SCM_INEXP(y), bady);
    bigreal:
      return (SCM_REALP(y) && (scm_big2dbl(x)==SCM_REALPART(y))) ? SCM_BOOL_T : SCM_BOOL_F;
    }
    SCM_ASRTGO(SCM_INEXP(x), badx);
# else
    SCM_ASSERT(SCM_NIMP(x) && SCM_INEXP(x), x, SCM_ARG1, s_eq_p);
# endif
    if SCM_INUMP(y) {t = x; x = y; y = t; goto realint;}
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) {t = x; x = y; y = t; goto bigreal;}
    SCM_ASRTGO(SCM_INEXP(y), bady);
# else
    SCM_ASRTGO(SCM_NIMP(y) && SCM_INEXP(y), bady);
# endif
    if (SCM_REALPART(x) != SCM_REALPART(y)) return SCM_BOOL_F;
    if SCM_CPLXP(x)
      return (SCM_CPLXP(y) && (SCM_IMAG(x)==SCM_IMAG(y))) ? SCM_BOOL_T : SCM_BOOL_F;
    return SCM_CPLXP(y) ? SCM_BOOL_F : SCM_BOOL_T;
  }
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) return SCM_BOOL_F;
#  ifndef RECKLESS
    if (!(SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_eq_p);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_eq_p);
#  endif
# endif
  realint:
    return (SCM_REALP(y) && (((double)SCM_INUM(x))==SCM_REALPART(y))) ? SCM_BOOL_T : SCM_BOOL_F;
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_eq_p);
    if SCM_INUMP(y) return SCM_BOOL_F;
    SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
    return (0==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
  }
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_eq_p);
#  endif
    return SCM_BOOL_F;
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_eq_p);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_eq_p);
# endif
#endif
  return ((long)x==(long)y) ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC1 (s_less_p, "<?", scm_tc7_rpsubr, scm_less_p);
#ifdef __STDC__
SCM
scm_less_p(SCM x, SCM y)
#else
SCM
scm_less_p(x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
# ifdef SCM_BIGDIG
#  ifndef RECKLESS
    if (!(SCM_NIMP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_less_p);
#  endif
    if SCM_BIGP(x) {
      if SCM_INUMP(y) return SCM_BIGSIGN(x) ? SCM_BOOL_T : SCM_BOOL_F;
      SCM_ASRTGO(SCM_NIMP(y), bady);
      if SCM_BIGP(y) return (1==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
      SCM_ASRTGO(SCM_REALP(y), bady);
      return (scm_big2dbl(x) < SCM_REALPART(y)) ? SCM_BOOL_T : SCM_BOOL_F;
    }
    SCM_ASRTGO(SCM_REALP(x), badx);
# else
    SCM_ASSERT(SCM_NIMP(x) && SCM_REALP(x), x, SCM_ARG1, s_less_p);
# endif
    if (SCM_INUMP(y))
      return (SCM_REALPART(x) < ((double)SCM_INUM(y))) ? SCM_BOOL_T : SCM_BOOL_F;
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) return (SCM_REALPART(x) < scm_big2dbl(y)) ? SCM_BOOL_T : SCM_BOOL_F;
    SCM_ASRTGO(SCM_REALP(y), bady);
# else
    SCM_ASRTGO(SCM_NIMP(y) && SCM_REALP(y), bady);
# endif
    return (SCM_REALPART(x) < SCM_REALPART(y)) ? SCM_BOOL_T : SCM_BOOL_F;
  }
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) return SCM_BIGSIGN(y) ? SCM_BOOL_F : SCM_BOOL_T;
#  ifndef RECKLESS
    if (!(SCM_REALP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_less_p);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_REALP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_less_p);
#  endif
# endif
    return (((double)SCM_INUM(x)) < SCM_REALPART(y)) ? SCM_BOOL_T : SCM_BOOL_F;
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_less_p);
    if SCM_INUMP(y) return SCM_BIGSIGN(x) ? SCM_BOOL_T : SCM_BOOL_F;
    SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
    return (1==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
  }
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_less_p);
#  endif
    return SCM_BIGSIGN(y) ? SCM_BOOL_F : SCM_BOOL_T;
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_less_p);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_less_p);
# endif
#endif
  return ((long)x < (long)y) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC1 (s_gr_p, ">?", scm_tc7_rpsubr, scm_gr_p);
#ifdef __STDC__
SCM
scm_gr_p(SCM x, SCM y)
#else
SCM
scm_gr_p(x, y)
     SCM x;
     SCM y;
#endif
{
  return scm_less_p(y, x);
}



SCM_PROC1 (s_leq_p, "<=?", scm_tc7_rpsubr, scm_leq_p);
#ifdef __STDC__
SCM
scm_leq_p(SCM x, SCM y)
#else
SCM
scm_leq_p(x, y)
     SCM x;
     SCM y;
#endif
{
  return SCM_BOOL_NOT(scm_less_p(y, x));
}



SCM_PROC1 (s_geq_p, ">=?", scm_tc7_rpsubr, scm_geq_p);
#ifdef __STDC__
SCM
scm_geq_p(SCM x, SCM y)
#else
SCM
scm_geq_p(x, y)
     SCM x;
     SCM y;
#endif
{
  return SCM_BOOL_NOT(scm_less_p(x, y));
}



SCM_PROC(s_zero_p, "zero?", 1, 0, 0, scm_zero_p);
#ifdef __STDC__
SCM
scm_zero_p(SCM z)
#else
SCM
scm_zero_p(z)
     SCM z;
#endif
{
#ifdef SCM_FLOATS
  if SCM_NINUMP(z) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(z), badz);
    if SCM_BIGP(z) return SCM_BOOL_F;
#  ifndef RECKLESS
    if (!(SCM_INEXP(z)))
    badz: scm_wta(z, (char *)SCM_ARG1, s_zero_p);
#  endif
# else
    SCM_ASSERT(SCM_NIMP(z) && SCM_INEXP(z), z, SCM_ARG1, s_zero_p);
# endif
    return (z==scm_flo0) ? SCM_BOOL_T : SCM_BOOL_F;
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(z) {
    SCM_ASSERT(SCM_NIMP(z) && SCM_BIGP(z), z, SCM_ARG1, s_zero_p);
    return SCM_BOOL_F;
  }
# else
  SCM_ASSERT(SCM_INUMP(z), z, SCM_ARG1, s_zero_p);
# endif
#endif
  return (z==SCM_INUM0) ? SCM_BOOL_T: SCM_BOOL_F;
}



SCM_PROC(s_positive_p, "positive?", 1, 0, 0, scm_positive_p);
#ifdef __STDC__
SCM
scm_positive_p(SCM x)
#else
SCM
scm_positive_p(x)
     SCM x;
#endif
{
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(x), badx);
    if SCM_BIGP(x) return SCM_TYP16(x)==scm_tc16_bigpos ? SCM_BOOL_T : SCM_BOOL_F;
#  ifndef RECKLESS
    if (!(SCM_REALP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_positive_p);
#  endif
# else
    SCM_ASSERT(SCM_NIMP(x) && SCM_REALP(x), x, SCM_ARG1, s_positive_p);
# endif
    return (SCM_REALPART(x) > 0.0) ? SCM_BOOL_T : SCM_BOOL_F;
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_positive_p);
    return SCM_TYP16(x)==scm_tc16_bigpos ? SCM_BOOL_T : SCM_BOOL_F;
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_positive_p);
# endif
#endif
  return (x > SCM_INUM0) ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC(s_negative_p, "negative?", 1, 0, 0, scm_negative_p);
#ifdef __STDC__
SCM
scm_negative_p(SCM x)
#else
SCM
scm_negative_p(x)
     SCM x;
#endif
{
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(x), badx);
    if SCM_BIGP(x) return SCM_TYP16(x)==scm_tc16_bigpos ? SCM_BOOL_F : SCM_BOOL_T;
#  ifndef RECKLESS
    if (!(SCM_REALP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_negative_p);
#  endif
# else
    SCM_ASSERT(SCM_NIMP(x) && SCM_REALP(x), x, SCM_ARG1, s_negative_p);
# endif
    return (SCM_REALPART(x) < 0.0) ? SCM_BOOL_T : SCM_BOOL_F;
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_negative_p);
    return (SCM_TYP16(x)==scm_tc16_bigneg) ? SCM_BOOL_T : SCM_BOOL_F;
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_negative_p);
# endif
#endif
  return (x < SCM_INUM0) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC1 (s_max, "max", scm_tc7_asubr, scm_max);
#ifdef __STDC__
SCM
scm_max(SCM x, SCM y)
#else
SCM
scm_max(x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_FLOATS
  double z;
#endif
  if SCM_UNBNDP(y) {
#ifndef RECKLESS
    if (!(SCM_NUMBERP(x)))
      badx: scm_wta(x, (char *)SCM_ARG1, s_max);
#endif
    return x;
  }
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(x), badx);
    if SCM_BIGP(x) {
      if SCM_INUMP(y) return SCM_BIGSIGN(x) ? y : x;
      SCM_ASRTGO(SCM_NIMP(y), bady);
      if SCM_BIGP(y) return (1==scm_bigcomp(x, y)) ? y : x;
      SCM_ASRTGO(SCM_REALP(y), bady);
      z = scm_big2dbl(x);
      return (z < SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
    }
    SCM_ASRTGO(SCM_REALP(x), badx);
# else
    SCM_ASSERT(SCM_NIMP(x) && SCM_REALP(x), x, SCM_ARG1, s_max);
# endif
    if (SCM_INUMP(y))
      return (SCM_REALPART(x) < (z = SCM_INUM(y))) ? scm_makdbl(z, 0.0) : x;
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if (SCM_BIGP(y))
      return (SCM_REALPART(x) < (z = scm_big2dbl(y))) ? scm_makdbl(z, 0.0) : x;
    SCM_ASRTGO(SCM_REALP(y), bady);
# else
    SCM_ASRTGO(SCM_NIMP(y) && SCM_REALP(y), bady);
# endif
    return (SCM_REALPART(x) < SCM_REALPART(y)) ? y : x;
  }
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) return SCM_BIGSIGN(y) ? x : y;
#  ifndef RECKLESS
    if (!(SCM_REALP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_max);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_REALP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_max);
#  endif
# endif
    return ((z = SCM_INUM(x)) < SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_max);
    if SCM_INUMP(y) return SCM_BIGSIGN(x) ? y : x;
    SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
    return (1==scm_bigcomp(x, y)) ? y : x;
  }
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_max);
#  endif
    return SCM_BIGSIGN(y) ? x : y;
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_max);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_max);
# endif
#endif
  return ((long)x < (long)y) ? y : x;
}




SCM_PROC1 (s_min, "min", scm_tc7_asubr, scm_min);
#ifdef __STDC__
SCM
scm_min(SCM x, SCM y)
#else
SCM
scm_min(x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_FLOATS
  double z;
#endif
  if SCM_UNBNDP(y) {
#ifndef RECKLESS
    if (!(SCM_NUMBERP(x)))
      badx:scm_wta(x, (char *)SCM_ARG1, s_min);
#endif
    return x;
  }
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(x), badx);
    if SCM_BIGP(x) {
      if SCM_INUMP(y) return SCM_BIGSIGN(x) ? x : y;
      SCM_ASRTGO(SCM_NIMP(y), bady);
      if SCM_BIGP(y) return (-1==scm_bigcomp(x, y)) ? y : x;
      SCM_ASRTGO(SCM_REALP(y), bady);
      z = scm_big2dbl(x);
      return (z > SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
    }
    SCM_ASRTGO(SCM_REALP(x), badx);
# else
    SCM_ASSERT(SCM_NIMP(x) && SCM_REALP(x), x, SCM_ARG1, s_min);
# endif
    if SCM_INUMP(y) return (SCM_REALPART(x) > (z = SCM_INUM(y))) ? scm_makdbl(z, 0.0) : x;
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) return (SCM_REALPART(x) > (z = scm_big2dbl(y))) ? scm_makdbl(z, 0.0) : x;
    SCM_ASRTGO(SCM_REALP(y), bady);
# else
    SCM_ASRTGO(SCM_NIMP(y) && SCM_REALP(y), bady);
# endif
    return (SCM_REALPART(x) > SCM_REALPART(y)) ? y : x;
  }
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) return SCM_BIGSIGN(y) ? y : x;
#  ifndef RECKLESS
    if (!(SCM_REALP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_min);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_REALP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_min);
#  endif
# endif
    return ((z = SCM_INUM(x)) > SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_min);
    if SCM_INUMP(y) return SCM_BIGSIGN(x) ? x : y;
    SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
    return (-1==scm_bigcomp(x, y)) ? y : x;
  }
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_min);
#  endif
    return SCM_BIGSIGN(y) ? y : x;
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_min);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_min);
# endif
#endif
  return ((long)x > (long)y) ? y : x;
}




SCM_PROC1 (s_sum, "+", scm_tc7_asubr, scm_sum);
#ifdef __STDC__
SCM
scm_sum(SCM x, SCM y)
#else
SCM
scm_sum(x, y)
     SCM x;
     SCM y;
#endif
{
  if SCM_UNBNDP(y) {
    if SCM_UNBNDP(x) return SCM_INUM0;
#ifndef RECKLESS
    if (!(SCM_NUMBERP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_sum);
#endif
    return x;
  }
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
    SCM t;
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(x), badx);
    if SCM_BIGP(x) {
      if SCM_INUMP(y) {t = x; x = y; y = t; goto intbig;}
      SCM_ASRTGO(SCM_NIMP(y), bady);
      if SCM_BIGP(y) {
	if (SCM_NUMDIGS(x) > SCM_NUMDIGS(y)) {t = x; x = y; y = t;}
	return scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0);
      }
      SCM_ASRTGO(SCM_INEXP(y), bady);
    bigreal: return scm_makdbl(scm_big2dbl(x)+SCM_REALPART(y), SCM_CPLXP(y)?SCM_IMAG(y):0.0);
    }
    SCM_ASRTGO(SCM_INEXP(x), badx);
# else
    SCM_ASRTGO(SCM_NIMP(x) && SCM_INEXP(x), badx);
# endif
    if SCM_INUMP(y) {t = x; x = y; y = t; goto intreal;}
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) {t = x; x = y; y = t; goto bigreal;}
#  ifndef RECKLESS
    else if (!(SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_sum);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_sum);
#  endif
# endif
    { double i = 0.0;
      if SCM_CPLXP(x) i = SCM_IMAG(x);
      if SCM_CPLXP(y) i += SCM_IMAG(y);
      return scm_makdbl(SCM_REALPART(x)+SCM_REALPART(y), i); }
  }
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y)
    intbig: {
#  ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong(SCM_INUM(x));
      return scm_addbig((SCM_BIGDIG *)&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  else
      SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
      scm_longdigs(SCM_INUM(x), zdigs);
      return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  endif
    }
    SCM_ASRTGO(SCM_INEXP(y), bady);
# else
    SCM_ASRTGO(SCM_NIMP(y) && SCM_INEXP(y), bady);
# endif
  intreal: return scm_makdbl(SCM_INUM(x)+SCM_REALPART(y), SCM_CPLXP(y)?SCM_IMAG(y):0.0);
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM t;
    SCM_ASRTGO(SCM_NIMP(x) && SCM_BIGP(x), badx);
    if SCM_INUMP(y) {t = x; x = y; y = t; goto intbig;}
    SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
    if (SCM_NUMDIGS(x) > SCM_NUMDIGS(y)) {t = x; x = y; y = t;}
    return scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0);
  }
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_sum);
#  endif
  intbig: {
#  ifndef SCM_DIGSTOOBIG
    long z = scm_pseudolong(SCM_INUM(x));
    return scm_addbig(&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  else
    SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
    scm_longdigs(SCM_INUM(x), zdigs);
    return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  endif
  }
  }
# else
  SCM_ASRTGO(SCM_INUMP(x), badx);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_sum);
# endif
#endif
  x = SCM_INUM(x)+SCM_INUM(y);
  if SCM_FIXABLE(x) return SCM_MAKINUM(x);
#ifdef SCM_BIGDIG
  return scm_long2big(x);
#else
# ifdef SCM_FLOATS
  return scm_makdbl((double)x, 0.0);
# else
  SCM_NUM_OVERFLOW (s_sum);
  return SCM_UNSPECIFIED;
# endif
#endif
}




SCM_PROC1 (s_difference, "-", scm_tc7_asubr, scm_difference);
#ifdef __STDC__
SCM
scm_difference(SCM x, SCM y)
#else
SCM
scm_difference(x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
# ifndef RECKLESS
    if (!(SCM_NIMP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_difference);
# endif
    if SCM_UNBNDP(y) {
# ifdef SCM_BIGDIG
      if SCM_BIGP(x) {
	x = scm_copybig(x, !SCM_BIGSIGN(x));
	return SCM_NUMDIGS(x) * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM) ?
	  scm_big2inum(x, SCM_NUMDIGS(x)) : x;
      }
# endif
      SCM_ASRTGO(SCM_INEXP(x), badx);
      return scm_makdbl(-SCM_REALPART(x), SCM_CPLXP(x)?-SCM_IMAG(x):0.0);
    }
    if SCM_INUMP(y) return scm_sum(x, SCM_MAKINUM(-SCM_INUM(y)));
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(x) {
      if SCM_BIGP(y) return (SCM_NUMDIGS(x) < SCM_NUMDIGS(y)) ?
	scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0x0100) :
      scm_addbig(SCM_BDIGITS(y), SCM_NUMDIGS(y), SCM_BIGSIGN(y) ^ 0x0100, x, 0);
      SCM_ASRTGO(SCM_INEXP(y), bady);
      return scm_makdbl(scm_big2dbl(x)-SCM_REALPART(y), SCM_CPLXP(y)?-SCM_IMAG(y):0.0);
    }
    SCM_ASRTGO(SCM_INEXP(x), badx);
    if SCM_BIGP(y) return scm_makdbl(SCM_REALPART(x)-scm_big2dbl(y), SCM_CPLXP(x)?SCM_IMAG(x):0.0);
    SCM_ASRTGO(SCM_INEXP(y), bady);
# else
    SCM_ASRTGO(SCM_INEXP(x), badx);
    SCM_ASRTGO(SCM_NIMP(y) && SCM_INEXP(y), bady);
# endif
    if SCM_CPLXP(x)
      if SCM_CPLXP(y)
	return scm_makdbl(SCM_REAL(x)-SCM_REAL(y), SCM_IMAG(x)-SCM_IMAG(y));
      else
	return scm_makdbl(SCM_REAL(x)-SCM_REALPART(y), SCM_IMAG(x));
    return scm_makdbl(SCM_REALPART(x)-SCM_REALPART(y), SCM_CPLXP(y)?-SCM_IMAG(y):0.0);
  }
  if SCM_UNBNDP(y) {x = -SCM_INUM(x); goto checkx;}
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) {
#  ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong(SCM_INUM(x));
      return scm_addbig((SCM_BIGDIG *)&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  else
      SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
      scm_longdigs(SCM_INUM(x), zdigs);
      return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  endif
    }
#  ifndef RECKLESS
    if (!(SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_difference);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_difference);
#  endif
# endif
    return scm_makdbl(SCM_INUM(x)-SCM_REALPART(y), SCM_CPLXP(y)?-SCM_IMAG(y):0.0);
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_difference);
    if SCM_UNBNDP(y) {
      x = scm_copybig(x, !SCM_BIGSIGN(x));
      return SCM_NUMDIGS(x) * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM) ?
	scm_big2inum(x, SCM_NUMDIGS(x)) : x;
    }
    if SCM_INUMP(y) {
#  ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong(SCM_INUM(y));
      return scm_addbig(&z, SCM_DIGSPERLONG, (y < 0) ? 0 : 0x0100, x, 0);
#  else
      SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
      scm_longdigs(SCM_INUM(x), zdigs);
      return scm_addbig(zdigs, SCM_DIGSPERLONG, (y < 0) ? 0 : 0x0100, x, 0);
#  endif
    }
    SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
    return (SCM_NUMDIGS(x) < SCM_NUMDIGS(y)) ?
      scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0x0100) :
    scm_addbig(SCM_BDIGITS(y), SCM_NUMDIGS(y), SCM_BIGSIGN(y) ^ 0x0100, x, 0);
  }
  if SCM_UNBNDP(y) {x = -SCM_INUM(x); goto checkx;}
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_difference);
#  endif
    {
#  ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong(SCM_INUM(x));
      return scm_addbig(&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  else
      SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
      scm_longdigs(SCM_INUM(x), zdigs);
      return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  endif
    }
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_difference);
  if SCM_UNBNDP(y) {x = -SCM_INUM(x); goto checkx;}
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_difference);
# endif
#endif
  x = SCM_INUM(x)-SCM_INUM(y);
 checkx:
  if SCM_FIXABLE(x) return SCM_MAKINUM(x);
#ifdef SCM_BIGDIG
  return scm_long2big(x);
#else
# ifdef SCM_FLOATS
  return scm_makdbl((double)x, 0.0);
# else
  SCM_NUM_OVERFLOW (s_difference);
  return SCM_UNSPECIFIED;
# endif
#endif
}




SCM_PROC1 (s_product, "*", scm_tc7_asubr, scm_product);
#ifdef __STDC__
SCM
scm_product(SCM x, SCM y)
#else
SCM
scm_product(x, y)
     SCM x;
     SCM y;
#endif
{
  if SCM_UNBNDP(y) {
    if SCM_UNBNDP(x) return SCM_MAKINUM(1L);
#ifndef RECKLESS
    if (!(SCM_NUMBERP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_product);
#endif
    return x;
  }
#ifdef SCM_FLOATS
  if SCM_NINUMP(x) {
    SCM t;
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(x), badx);
    if SCM_BIGP(x) {
      if SCM_INUMP(y) {t = x; x = y; y = t; goto intbig;}
      SCM_ASRTGO(SCM_NIMP(y), bady);
      if SCM_BIGP(y) return scm_mulbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
				   SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y));
      SCM_ASRTGO(SCM_INEXP(y), bady);
    bigreal: {
      double bg = scm_big2dbl(x);
      return scm_makdbl(bg*SCM_REALPART(y), SCM_CPLXP(y)?bg*SCM_IMAG(y):0.0); }
    }
    SCM_ASRTGO(SCM_INEXP(x), badx);
# else
    SCM_ASRTGO(SCM_NIMP(x) && SCM_INEXP(x), badx);
# endif
    if SCM_INUMP(y) {t = x; x = y; y = t; goto intreal;}
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) {t = x; x = y; y = t; goto bigreal;}
#  ifndef RECKLESS
    else if (!(SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_product);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_product);
#  endif
# endif
    if SCM_CPLXP(x)
      if SCM_CPLXP(y)
	return scm_makdbl(SCM_REAL(x)*SCM_REAL(y)-SCM_IMAG(x)*SCM_IMAG(y),
			  SCM_REAL(x)*SCM_IMAG(y)+SCM_IMAG(x)*SCM_REAL(y));
      else
	return scm_makdbl(SCM_REAL(x)*SCM_REALPART(y), SCM_IMAG(x)*SCM_REALPART(y));
    return scm_makdbl(SCM_REALPART(x)*SCM_REALPART(y),
		      SCM_CPLXP(y)?SCM_REALPART(x)*SCM_IMAG(y):0.0);
  }
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) {
    intbig: if (SCM_INUM0==x) return x; if (SCM_MAKINUM(1L)==x) return y;
      {
#  ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong(SCM_INUM(x));
	return scm_mulbig((SCM_BIGDIG *)&z, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			  SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  else
	SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	scm_longdigs(SCM_INUM(x), zdigs);
	return scm_mulbig(zdigs, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			  SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  endif
      }
    }
    SCM_ASRTGO(SCM_INEXP(y), bady);
# else
    SCM_ASRTGO(SCM_NIMP(y) && SCM_INEXP(y), bady);
# endif
  intreal: return scm_makdbl(SCM_INUM(x)*SCM_REALPART(y), SCM_CPLXP(y)?SCM_INUM(x)*SCM_IMAG(y):0.0);
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM_ASRTGO(SCM_NIMP(x) && SCM_BIGP(x), badx);
    if SCM_INUMP(y) {SCM t = x; x = y; y = t; goto intbig;}
    SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
    return scm_mulbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
		      SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y));
  }
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_product);
#  endif
  intbig: if (SCM_INUM0==x) return x; if (SCM_MAKINUM(1L)==x) return y;
    {
#  ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong(SCM_INUM(x));
      return scm_mulbig(&z, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  else
      SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
      scm_longdigs(SCM_INUM(x), zdigs);
      return scm_mulbig(zdigs, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  endif
    }
  }
# else
  SCM_ASRTGO(SCM_INUMP(x), badx);
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_product);
# endif
#endif
  {
    long i, j, k;
    i = SCM_INUM(x);
    if (0==i) return x;
    j = SCM_INUM(y);
    k = i * j;
    y = SCM_MAKINUM(k);
    if (k != SCM_INUM(y) || k/i != j)
#ifdef SCM_BIGDIG
      { int sgn = (i < 0) ^ (j < 0);
# ifndef SCM_DIGSTOOBIG
	i = scm_pseudolong(i);
	j = scm_pseudolong(j);
	return scm_mulbig((SCM_BIGDIG *)&i, SCM_DIGSPERLONG,
			  (SCM_BIGDIG *)&j, SCM_DIGSPERLONG, sgn);
# else /* SCM_DIGSTOOBIG */
	SCM_BIGDIG idigs[SCM_DIGSPERLONG];
	SCM_BIGDIG jdigs[SCM_DIGSPERLONG];
	scm_longdigs(i, idigs);
	scm_longdigs(j, jdigs);
	return scm_mulbig(idigs, SCM_DIGSPERLONG, jdigs, SCM_DIGSPERLONG, sgn);
# endif
      }
#else
# ifdef SCM_FLOATS
    return scm_makdbl(((double)i)*((double)j), 0.0);
# else
    SCM_NUM_OVERFLOW (s_product);
# endif
#endif
    return y;
  }
}


#ifdef __STDC__
double
scm_num2dbl (SCM a, char * why)
#else
double
scm_num2dbl (a, why)
     SCM a;
     char * why;
#endif
{
  if (SCM_INUMP (a))
    return (double) SCM_INUM (a);
#ifdef SCM_FLOATS
  SCM_ASSERT (SCM_NIMP (a), a, "wrong type argument", why);
  if (SCM_REALP (a))
    return (SCM_REALPART (a));
#endif
#ifdef SCM_BIGDIG
  return scm_big2dbl (a);
#endif
  SCM_ASSERT (0, a, "wrong type argument", why);
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_fuck, "fuck", 1, 0, 0, scm_fuck);
#ifdef __STDC__
SCM
scm_fuck (SCM a)
#else
SCM
scm_fuck (a)
     SCM a;
#endif
{
  return scm_makdbl (scm_num2dbl (a, "just because"), 0.0);
}

SCM_PROC1 (s_divide, "/", scm_tc7_asubr, scm_divide);
#ifdef __STDC__
SCM
scm_divide(SCM x, SCM y)
#else
SCM
scm_divide(x, y)
     SCM x;
     SCM y;
#endif
{
#ifdef SCM_FLOATS
  double d, r, i, a;
  if SCM_NINUMP(x) {
# ifndef RECKLESS
    if (!(SCM_NIMP(x)))
    badx: scm_wta(x, (char *)SCM_ARG1, s_divide);
# endif
    if SCM_UNBNDP(y) {
# ifdef SCM_BIGDIG
      if SCM_BIGP(x) return scm_makdbl(1.0/scm_big2dbl(x), 0.0);
# endif
      SCM_ASRTGO(SCM_INEXP(x), badx);
      if SCM_REALP(x) return scm_makdbl(1.0/SCM_REALPART(x), 0.0);
      r = SCM_REAL(x);  i = SCM_IMAG(x);  d = r*r+i*i;
      return scm_makdbl(r/d, -i/d);
    }
# ifdef SCM_BIGDIG
    if SCM_BIGP(x) {
      SCM z;
      if SCM_INUMP(y) {
        z = SCM_INUM(y);
#ifndef RECKLESS
	if (!z)
	  SCM_NUM_OVERFLOW (s_divide);
#endif
	if (1==z) return x;
        if (z < 0) z = -z;
        if (z < SCM_BIGRAD) {
          SCM w = scm_copybig(x, SCM_BIGSIGN(x) ? (y>0) : (y<0));
          return scm_divbigdig(SCM_BDIGITS(w), SCM_NUMDIGS(w), (SCM_BIGDIG)z) ?
	    scm_makdbl(scm_big2dbl(x)/SCM_INUM(y), 0.0) : scm_normbig(w);
	}
#  ifndef SCM_DIGSTOOBIG
        z = scm_pseudolong(z);
        z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), (SCM_BIGDIG *)&z, SCM_DIGSPERLONG,
			  SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);
#  else
	{ SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	  scm_longdigs(z, zdigs);
	  z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), zdigs, SCM_DIGSPERLONG,
			    SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);}
#  endif
        return z ? z : scm_makdbl(scm_big2dbl(x)/SCM_INUM(y), 0.0);
      }
      SCM_ASRTGO(SCM_NIMP(y), bady);
      if SCM_BIGP(y) {
	z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			  SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y), 3);
	return z ? z : scm_makdbl(scm_big2dbl(x)/scm_big2dbl(y), 0.0);
      }
      SCM_ASRTGO(SCM_INEXP(y), bady);
      if SCM_REALP(y) return scm_makdbl(scm_big2dbl(x)/SCM_REALPART(y), 0.0);
      a = scm_big2dbl(x);
      goto complex_div;
    }
# endif
    SCM_ASRTGO(SCM_INEXP(x), badx);
    if SCM_INUMP(y) {d = SCM_INUM(y); goto basic_div;}
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) {d = scm_big2dbl(y); goto basic_div;}
    SCM_ASRTGO(SCM_INEXP(y), bady);
# else
    SCM_ASRTGO(SCM_NIMP(y) && SCM_INEXP(y), bady);
# endif
    if SCM_REALP(y) {
      d = SCM_REALPART(y);
    basic_div: return scm_makdbl(SCM_REALPART(x)/d, SCM_CPLXP(x)?SCM_IMAG(x)/d:0.0);
    }
    a = SCM_REALPART(x);
    if SCM_REALP(x) goto complex_div;
    r = SCM_REAL(y);  i = SCM_IMAG(y);  d = r*r+i*i;
    return scm_makdbl((a*r+SCM_IMAG(x)*i)/d, (SCM_IMAG(x)*r-a*i)/d);
  }
  if SCM_UNBNDP(y) {
    if ((SCM_MAKINUM(1L)==x) || (SCM_MAKINUM(-1L)==x)) return x;
    return scm_makdbl(1.0/((double)SCM_INUM(x)), 0.0);
  }
  if SCM_NINUMP(y) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(y), bady);
    if SCM_BIGP(y) return scm_makdbl(SCM_INUM(x)/scm_big2dbl(y), 0.0);
#  ifndef RECKLESS
    if (!(SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_divide);
#  endif
# else
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_INEXP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_divide);
#  endif
# endif
    if (SCM_REALP(y))
      return scm_makdbl(SCM_INUM(x)/SCM_REALPART(y), 0.0);
    a = SCM_INUM(x);
  complex_div:
    r = SCM_REAL(y);  i = SCM_IMAG(y);  d = r*r+i*i;
    return scm_makdbl((a*r)/d, (-a*i)/d);
  }
#else
# ifdef SCM_BIGDIG
  if SCM_NINUMP(x) {
    SCM z;
    SCM_ASSERT(SCM_NIMP(x) && SCM_BIGP(x), x, SCM_ARG1, s_divide);
    if SCM_UNBNDP(y) goto ov;
    if SCM_INUMP(y) {
      z = SCM_INUM(y);
      if (!z) goto ov;
      if (1==z) return x;
      if (z < 0) z = -z;
      if (z < SCM_BIGRAD) {
        SCM w = scm_copybig(x, SCM_BIGSIGN(x) ? (y>0) : (y<0));
        if (scm_divbigdig(SCM_BDIGITS(w), SCM_NUMDIGS(w), (SCM_BIGDIG)z)) goto ov;
        return w;
      }
#  ifndef SCM_DIGSTOOBIG
      z = scm_pseudolong(z);
      z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), &z, SCM_DIGSPERLONG,
			SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);
#  else
      { SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	scm_longdigs(z, zdigs);
	z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), zdigs, SCM_DIGSPERLONG,
			  SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);}
#  endif
    } else {
      SCM_ASRTGO(SCM_NIMP(y) && SCM_BIGP(y), bady);
      z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y), 3);
    }
    if (!z) goto ov;
    return z;
  }
  if SCM_UNBNDP(y) {
    if ((SCM_MAKINUM(1L)==x) || (SCM_MAKINUM(-1L)==x)) return x;
    goto ov;
  }
  if SCM_NINUMP(y) {
#  ifndef RECKLESS
    if (!(SCM_NIMP(y) && SCM_BIGP(y)))
    bady: scm_wta(y, (char *)SCM_ARG2, s_divide);
#  endif
    goto ov;
  }
# else
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_divide);
  if SCM_UNBNDP(y) {
    if ((SCM_MAKINUM(1L)==x) || (SCM_MAKINUM(-1L)==x)) return x;
    goto ov;
  }
  SCM_ASSERT(SCM_INUMP(y), y, SCM_ARG2, s_divide);
# endif
#endif
  {
    long z = SCM_INUM(y);
    if ((0==z) || SCM_INUM(x)%z) goto ov;
    z = SCM_INUM(x)/z;
    if SCM_FIXABLE(z) return SCM_MAKINUM(z);
#ifdef SCM_BIGDIG
    return scm_long2big(z);
#endif
#ifdef SCM_FLOATS
  ov: return scm_makdbl(((double)SCM_INUM(x))/((double)SCM_INUM(y)), 0.0);
#else
  ov: SCM_NUM_OVERFLOW (s_divide);
    return SCM_UNSPECIFIED;
#endif
  }
}




#ifdef SCM_FLOATS
SCM_PROC1 (s_asinh, "$asinh", scm_tc7_cxr, (SCM (*)()) scm_asinh);
#ifdef __STDC__
double
scm_asinh(double x)
#else
double
scm_asinh(x)
     double x;
#endif
{
  return log(x+sqrt(x*x+1));
}




SCM_PROC1 (s_acosh, "$acosh", scm_tc7_cxr, (SCM (*)()) scm_acosh);
#ifdef __STDC__
double
scm_acosh(double x)
#else
double
scm_acosh(x)
     double x;
#endif
{
  return log(x+sqrt(x*x-1));
}




SCM_PROC1 (s_atanh, "$atanh", scm_tc7_cxr, (SCM (*)()) scm_atanh);
#ifdef __STDC__
double
scm_atanh(double x)
#else
double
scm_atanh(x)
     double x;
#endif
{
  return 0.5*log((1+x)/(1-x));
}




SCM_PROC1 (s_truncate, "truncate", scm_tc7_cxr, (SCM (*)()) scm_truncate);
#ifdef __STDC__
double
scm_truncate(double x)
#else
double
scm_truncate(x)
     double x;
#endif
{
  if (x < 0.0) return -floor(-x);
  return floor(x);
}



SCM_PROC1 (s_round, "round", scm_tc7_cxr, (SCM (*)()) scm_round);
#ifdef __STDC__
double
scm_round(double x)
#else
double
scm_round(x)
     double x;
#endif
{
  double plus_half = x + 0.5;
  double result = floor(plus_half);
  /* Adjust so that the scm_round is towards even.  */
  return (plus_half == result && plus_half / 2 != floor(plus_half / 2))
    ? result - 1 : result;
}



SCM_PROC1 (s_exact_to_inexact, "exact->inexact", scm_tc7_cxr, (SCM (*)()) scm_exact_to_inexact);
#ifdef __STDC__
double
scm_exact_to_inexact(double z)
#else
double
scm_exact_to_inexact(z)
     double z;
#endif
{
  return z;
}


SCM_PROC1 (s_i_floor, "floor", scm_tc7_cxr, (SCM (*)()) floor);
SCM_PROC1 (s_i_ceil, "ceiling", scm_tc7_cxr, (SCM (*)()) ceil);
SCM_PROC1 (s_i_sqrt, "$sqrt", scm_tc7_cxr, (SCM (*)())sqrt);
SCM_PROC1 (s_i_abs, "$abs", scm_tc7_cxr, (SCM (*)())fabs);
SCM_PROC1 (s_i_exp, "$exp", scm_tc7_cxr, (SCM (*)())exp);
SCM_PROC1 (s_i_log, "$log", scm_tc7_cxr, (SCM (*)())log);
SCM_PROC1 (s_i_sin, "$sin", scm_tc7_cxr, (SCM (*)())sin);
SCM_PROC1 (s_i_cos, "$cos", scm_tc7_cxr, (SCM (*)())cos);
SCM_PROC1 (s_i_tan, "$tan", scm_tc7_cxr, (SCM (*)())tan);
SCM_PROC1 (s_i_asin, "$asin", scm_tc7_cxr, (SCM (*)())asin);
SCM_PROC1 (s_i_acos, "$acos", scm_tc7_cxr, (SCM (*)())acos);
SCM_PROC1 (s_i_atan, "$atan", scm_tc7_cxr, (SCM (*)())atan);
SCM_PROC1 (s_i_sinh, "$sinh", scm_tc7_cxr, (SCM (*)())sinh);
SCM_PROC1 (s_i_cosh, "$cosh", scm_tc7_cxr, (SCM (*)())cosh);
SCM_PROC1 (s_i_tanh, "$tanh", scm_tc7_cxr, (SCM (*)())tanh);

struct dpair {double x, y;};

static void 
scm_two_doubles(z1, z2, sstring, xy)
     SCM z1, z2;
     char *sstring;
     struct dpair *xy;
{
  if SCM_INUMP(z1) xy->x = SCM_INUM(z1);
  else {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(z1), badz1);
    if SCM_BIGP(z1) xy->x = scm_big2dbl(z1);
    else {
#  ifndef RECKLESS
      if (!(SCM_REALP(z1)))
      badz1: scm_wta(z1, (char *)SCM_ARG1, sstring);
#  endif
      xy->x = SCM_REALPART(z1);}
# else
    {SCM_ASSERT(SCM_NIMP(z1) && SCM_REALP(z1), z1, SCM_ARG1, sstring);
     xy->x = SCM_REALPART(z1);}
# endif
  }
  if SCM_INUMP(z2) xy->y = SCM_INUM(z2);
  else {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(z2), badz2);
    if SCM_BIGP(z2) xy->y = scm_big2dbl(z2);
    else {
#  ifndef RECKLESS
      if (!(SCM_REALP(z2)))
      badz2: scm_wta(z2, (char *)SCM_ARG2, sstring);
#  endif
      xy->y = SCM_REALPART(z2);}
# else
    {SCM_ASSERT(SCM_NIMP(z2) && SCM_REALP(z2), z2, SCM_ARG2, sstring);
     xy->y = SCM_REALPART(z2);}
# endif
  }
}




SCM_PROC(s_sys_expt, "$expt", 2, 0, 0, scm_sys_expt);
#ifdef __STDC__
SCM
scm_sys_expt(SCM z1, SCM z2)
#else
SCM
scm_sys_expt(z1, z2)
     SCM z1;
     SCM z2;
#endif
{
  struct dpair xy;
  scm_two_doubles(z1, z2, s_sys_expt, &xy);
  return scm_makdbl(pow(xy.x, xy.y), 0.0);
}



SCM_PROC(s_sys_atan2, "$atan2", 2, 0, 0, scm_sys_atan2);
#ifdef __STDC__
SCM
scm_sys_atan2(SCM z1, SCM z2)
#else
SCM
scm_sys_atan2(z1, z2)
     SCM z1;
     SCM z2;
#endif
{
  struct dpair xy;
  scm_two_doubles(z1, z2, s_sys_atan2, &xy);
  return scm_makdbl(atan2(xy.x, xy.y), 0.0);
}



SCM_PROC(s_make_rectangular, "make-rectangular", 2, 0, 0, scm_make_rectangular);
#ifdef __STDC__
SCM
scm_make_rectangular(SCM z1, SCM z2)
#else
SCM
scm_make_rectangular(z1, z2)
     SCM z1;
     SCM z2;
#endif
{
  struct dpair xy;
  scm_two_doubles(z1, z2, s_make_rectangular, &xy);
  return scm_makdbl(xy.x, xy.y);
}



SCM_PROC(s_make_polar, "make-polar", 2, 0, 0, scm_make_polar);
#ifdef __STDC__
SCM
scm_make_polar(SCM z1, SCM z2)
#else
SCM
scm_make_polar(z1, z2)
     SCM z1;
     SCM z2;
#endif
{
  struct dpair xy;
  scm_two_doubles(z1, z2, s_make_polar, &xy);
  return scm_makdbl(xy.x*cos(xy.y), xy.x*sin(xy.y));
}




SCM_PROC(s_real_part, "real-part", 1, 0, 0, scm_real_part);
#ifdef __STDC__
SCM
scm_real_part(SCM z)
#else
SCM
scm_real_part(z)
     SCM z;
#endif
{
  if SCM_NINUMP(z) {
# ifdef SCM_BIGDIG
    SCM_ASRTGO(SCM_NIMP(z), badz);
    if SCM_BIGP(z) return z;
#  ifndef RECKLESS
    if (!(SCM_INEXP(z)))
    badz: scm_wta(z, (char *)SCM_ARG1, s_real_part);
#  endif
# else
    SCM_ASSERT(SCM_NIMP(z) && SCM_INEXP(z), z, SCM_ARG1, s_real_part);
# endif
    if SCM_CPLXP(z) return scm_makdbl(SCM_REAL(z), 0.0);
  }
  return z;
}



SCM_PROC(s_imag_part, "imag-part", 1, 0, 0, scm_imag_part);
#ifdef __STDC__
SCM
scm_imag_part(SCM z)
#else
SCM
scm_imag_part(z)
     SCM z;
#endif
{
  if SCM_INUMP(z) return SCM_INUM0;
# ifdef SCM_BIGDIG
  SCM_ASRTGO(SCM_NIMP(z), badz);
  if SCM_BIGP(z) return SCM_INUM0;
#  ifndef RECKLESS
  if (!(SCM_INEXP(z)))
  badz: scm_wta(z, (char *)SCM_ARG1, s_imag_part);
#  endif
# else
  SCM_ASSERT(SCM_NIMP(z) && SCM_INEXP(z), z, SCM_ARG1, s_imag_part);
# endif
  if SCM_CPLXP(z) return scm_makdbl(SCM_IMAG(z), 0.0);
  return scm_flo0;
}



SCM_PROC(s_magnitude, "magnitude", 1, 0, 0, scm_magnitude);
#ifdef __STDC__
SCM
scm_magnitude(SCM z)
#else
SCM
scm_magnitude(z)
     SCM z;
#endif
{
  if SCM_INUMP(z) return scm_abs(z);
# ifdef SCM_BIGDIG
  SCM_ASRTGO(SCM_NIMP(z), badz);
  if SCM_BIGP(z) return scm_abs(z);
#  ifndef RECKLESS
  if (!(SCM_INEXP(z)))
  badz: scm_wta(z, (char *)SCM_ARG1, s_magnitude);
#  endif
# else
  SCM_ASSERT(SCM_NIMP(z) && SCM_INEXP(z), z, SCM_ARG1, s_magnitude);
# endif
  if SCM_CPLXP(z)
    {
      double i = SCM_IMAG(z), r = SCM_REAL(z);
      return scm_makdbl(sqrt(i*i+r*r), 0.0);
    }
  return scm_makdbl(fabs(SCM_REALPART(z)), 0.0);
}




SCM_PROC(s_angle, "angle", 1, 0, 0, scm_angle);
#ifdef __STDC__
SCM
scm_angle(SCM z)
#else
SCM
scm_angle(z)
     SCM z;
#endif
{
  double x, y = 0.0;
  if SCM_INUMP(z) {x = (z>=SCM_INUM0) ? 1.0 : -1.0; goto do_angle;}
# ifdef SCM_BIGDIG
  SCM_ASRTGO(SCM_NIMP(z), badz);
  if SCM_BIGP(z) {x = (SCM_TYP16(z)==scm_tc16_bigpos) ? 1.0 : -1.0; goto do_angle;}
#  ifndef RECKLESS
  if (!(SCM_INEXP(z))) {
  badz: scm_wta(z, (char *)SCM_ARG1, s_angle);}
#  endif
# else
  SCM_ASSERT(SCM_NIMP(z) && SCM_INEXP(z), z, SCM_ARG1, s_angle);
# endif
  if (SCM_REALP(z))
    {
      x = SCM_REALPART(z);
      goto do_angle;
    }
  x = SCM_REAL(z); y = SCM_IMAG(z);
 do_angle:
  return scm_makdbl(atan2(y, x), 0.0);
}


SCM_PROC(s_inexact_to_exact, "inexact->exact", 1, 0, 0, scm_inexact_to_exact);
#ifdef __STDC__
SCM
scm_inexact_to_exact(SCM z)
#else
SCM
scm_inexact_to_exact(z)
     SCM z;
#endif
{
  if SCM_INUMP(z) return z;
# ifdef SCM_BIGDIG
  SCM_ASRTGO(SCM_NIMP(z), badz);
  if SCM_BIGP(z) return z;
#  ifndef RECKLESS
  if (!(SCM_REALP(z)))
  badz: scm_wta(z, (char *)SCM_ARG1, s_inexact_to_exact);
#  endif
# else
  SCM_ASSERT(SCM_NIMP(z) && SCM_REALP(z), z, SCM_ARG1, s_inexact_to_exact);
# endif
# ifdef SCM_BIGDIG
  {
    double u = floor(SCM_REALPART(z)+0.5);
    if ((u <= SCM_MOST_POSITIVE_FIXNUM) && (-u <= -SCM_MOST_NEGATIVE_FIXNUM)) {
      /* Negation is a workaround for HP700 cc bug */
      SCM ans = SCM_MAKINUM((long)u);
      if (SCM_INUM(ans)==(long)u) return ans;
    }
    SCM_ASRTGO(!IS_INF(u), badz);	/* problem? */
    return scm_dbl2big(u);
  }
# else
  return SCM_MAKINUM((long)floor(SCM_REALPART(z)+0.5));
# endif
}



#else				/* ~SCM_FLOATS */
SCM_PROC(s_trunc, "truncate", 1, 0, 0, scm_trunc);
#ifdef __STDC__
SCM
scm_trunc(SCM x)
#else
SCM
scm_trunc(x)
     SCM x;
#endif
{
  SCM_ASSERT(SCM_INUMP(x), x, SCM_ARG1, s_truncate);
  return x;
}



#endif				/* SCM_FLOATS */

#ifdef SCM_BIGDIG
# ifdef SCM_FLOATS
/* d must be integer */
#ifdef __STDC__
SCM
scm_dbl2big(double d)
#else
SCM
scm_dbl2big(d)
     double d;
#endif
{
  scm_sizet i = 0;
  long c;
  SCM_BIGDIG *digits;
  SCM ans;
  double u = (d < 0)?-d:d;
  while (0 != floor(u)) {u /= SCM_BIGRAD;i++;}
  ans = scm_mkbig(i, d < 0);
  digits = SCM_BDIGITS(ans);
  while (i--) {
    u *= SCM_BIGRAD;
    c = floor(u);
    u -= c;
    digits[i] = c;
  }
#ifndef RECKLESS
  if (u != 0)
    SCM_NUM_OVERFLOW ("dbl2big");
#endif
  return ans;
}



#ifdef __STDC__
double
scm_big2dbl(SCM b)
#else
double
scm_big2dbl(b)
     SCM b;
#endif
{
  double ans = 0.0;
  scm_sizet i = SCM_NUMDIGS(b);
  SCM_BIGDIG *digits = SCM_BDIGITS(b);
  while (i--) ans = digits[i] + SCM_BIGRAD*ans;
  if (scm_tc16_bigneg==SCM_TYP16(b)) return -ans;
  return ans;
}
# endif
#endif

#ifdef __STDC__
SCM
scm_long2num(long sl)
#else
SCM
scm_long2num(sl)
     long sl;
#endif
{
  if (!SCM_FIXABLE(sl)) {
#ifdef SCM_BIGDIG
    return scm_long2big(sl);
#else
# ifdef SCM_FLOATS
    return scm_makdbl((double) sl, 0.0);
# else
    return SCM_BOOL_F;
# endif
#endif
  }
  return SCM_MAKINUM(sl);
}


#ifdef LONGLONGS
#ifdef __STDC__
SCM
scm_long_long2num(long_long sl)
#else
SCM
scm_long_long2num(sl)
     long_long sl;
#endif
{
  if (!SCM_FIXABLE(sl)) {
#ifdef SCM_BIGDIG
    return scm_long_long2big(sl);
#else
# ifdef SCM_FLOATS
    return scm_makdbl((double) sl, 0.0);
# else
    return SCM_BOOL_F;
# endif
#endif
  }
  return SCM_MAKINUM(sl);
}
#endif


#ifdef __STDC__
SCM
scm_ulong2num(unsigned long sl)
#else
SCM
scm_ulong2num(sl)
     unsigned long sl;
#endif
{
  if (!SCM_POSFIXABLE(sl)) {
#ifdef SCM_BIGDIG
    return scm_ulong2big(sl);
#else
# ifdef SCM_FLOATS
    return scm_makdbl((double) sl, 0.0);
# else
    return SCM_BOOL_F;
# endif
#endif
  }
  return SCM_MAKINUM(sl);
}

#ifdef __STDC__
long
scm_num2long(SCM num, char *pos, char *s_caller)
#else
long
scm_num2long(num, pos, s_caller)
     SCM num;
     char *pos;
     char *s_caller;
#endif
{
  long res;
  if (SCM_INUMP(num))
    {
      res = SCM_INUM(num);
      return res;
    }
  SCM_ASRTGO(SCM_NIMP(num), errout);
#ifdef SCM_FLOATS
  if (SCM_REALP(num))
    {
      double u = SCM_REALPART(num);
      res = u;
      if ((double)res == u)
	{
	  return res;
	}
    }
#endif
#ifdef SCM_BIGDIG
  if (SCM_BIGP(num)) {
    long oldres;
    scm_sizet l;
    res = 0;
    oldres = 0;
    for(l = SCM_NUMDIGS(num);l--;)
      {
	res = SCM_BIGUP(res) + SCM_BDIGITS(num)[l];
	if (res < oldres)
	  goto errout;
	oldres = res;
      }
    if (SCM_TYP16 (num) == scm_tc16_bigpos)
      return res;
    else
      return -res;
  }
#endif
 errout: scm_wta(num, pos, s_caller);
  return SCM_UNSPECIFIED;
}




#ifdef __STDC__
long
num2long(SCM num, char *pos, char *s_caller)
#else
long
num2long(num, pos, s_caller)
     SCM num;
     char *pos;
     char *s_caller;
#endif
{
  long res;
  if SCM_INUMP(num) {
    res = SCM_INUM((long)num);
    return res;
  }
  SCM_ASRTGO(SCM_NIMP(num), errout);
#ifdef SCM_FLOATS
  if SCM_REALP(num) {
    double u = SCM_REALPART(num);
    if (((SCM_MOST_NEGATIVE_FIXNUM * 4) <= u)
	&& (u <= (SCM_MOST_POSITIVE_FIXNUM * 4 + 3))) {
      res = u;
      return res;
    }
  }
#endif
#ifdef SCM_BIGDIG
  if SCM_BIGP(num) {
    scm_sizet l = SCM_NUMDIGS(num);
    SCM_ASRTGO(SCM_DIGSPERLONG >= l, errout);
    res = 0;
    for(;l--;) res = SCM_BIGUP(res) + SCM_BDIGITS(num)[l];
    return res;
  }
#endif
 errout: scm_wta(num, pos, s_caller);
  return SCM_UNSPECIFIED;
}


#ifdef LONGLONGS
#ifdef __STDC__
long_long
scm_num2long_long(SCM num, char *pos, char *s_caller)
#else
long_long
scm_num2long_long(num, pos, s_caller)
     SCM num;
     char *pos;
     char *s_caller;
#endif
{
  long_long res;
  if SCM_INUMP(num) {
    res = SCM_INUM((long_long)num);
    return res;
  }
  SCM_ASRTGO(SCM_NIMP(num), errout);
#ifdef SCM_FLOATS
  if SCM_REALP(num) {
    double u = SCM_REALPART(num);
    if (((SCM_MOST_NEGATIVE_FIXNUM * 4) <= u)
	&& (u <= (SCM_MOST_POSITIVE_FIXNUM * 4 + 3))) {
      res = u;
      return res;
    }
  }
#endif
#ifdef SCM_BIGDIG
  if SCM_BIGP(num) {
    scm_sizet l = SCM_NUMDIGS(num);
    SCM_ASRTGO(SCM_DIGSPERLONGLONG >= l, errout);
    res = 0;
    for(;l--;) res = SCM_LONGLONGBIGUP(res) + SCM_BDIGITS(num)[l];
    return res;
  }
#endif
 errout: scm_wta(num, pos, s_caller);
  return SCM_UNSPECIFIED;
}
#endif


#ifdef __STDC__
unsigned long
scm_num2ulong(SCM num, char *pos, char *s_caller)
#else
unsigned long
scm_num2ulong(num, pos, s_caller)
     SCM num;
     char *pos;
     char *s_caller;
#endif
{
  unsigned long res;
  if (SCM_INUMP(num))
    {
      res = SCM_INUM((unsigned long)num);
      return res;
    }
  SCM_ASRTGO(SCM_NIMP(num), errout);
#ifdef SCM_FLOATS
  if (SCM_REALP(num))
    {
      double u = SCM_REALPART(num);
      if ((0 <= u) && (u <= (unsigned long)~0L))
	{
	  res = u;
	  return res;
	}
    }
#endif
#ifdef SCM_BIGDIG
  if (SCM_BIGP(num)) {
    unsigned long oldres;
    scm_sizet l;
    res = 0;
    oldres = 0;
    for(l = SCM_NUMDIGS(num);l--;)
      {
	res = SCM_BIGUP(res) + SCM_BDIGITS(num)[l];
	if (res < oldres)
	  goto errout;
	oldres = res;
      }
    return res;
  }
#endif
 errout: scm_wta(num, pos, s_caller);
  return SCM_UNSPECIFIED;
}


#ifdef SCM_FLOATS
# ifndef DBL_DIG
static void add1(f, fsum)
     double f, *fsum;
{
  *fsum = f + 1.0;
}
# endif
#endif


#ifdef __STDC__
void
scm_init_numbers (void)
#else
void
scm_init_numbers ()
#endif
{
#ifdef SCM_FLOATS
  SCM_NEWCELL(scm_flo0);
# ifdef SCM_SINGLES
  SCM_CAR(scm_flo0) = scm_tc_flo;
  SCM_FLO(scm_flo0) = 0.0;
# else
  SCM_CDR(scm_flo0) = (SCM)scm_must_malloc(1L*sizeof(double), "real");
  SCM_REAL(scm_flo0) = 0.0;
  SCM_CAR(scm_flo0) = scm_tc_dblr;
# endif
# ifdef DBL_DIG
  scm_dblprec = (DBL_DIG > 20) ? 20 : DBL_DIG;
# else
  {				/* determine floating point precision */
    double f = 0.1;
    double fsum = 1.0+f;
    while (fsum != 1.0) {
      f /= 10.0;
      if (++scm_dblprec > 20) break;
      add1(f, &fsum);
    }
    scm_dblprec = scm_dblprec-1;
  }
# endif /* DBL_DIG */
#endif
#include "numbers.x"
}

