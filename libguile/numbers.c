/* Copyright (C) 1995,1996,1997,1998,1999,2000 Free Software Foundation, Inc.
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



#include <math.h>
#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/numbers.h"



static SCM scm_divbigbig (SCM_BIGDIG *x, scm_sizet nx, SCM_BIGDIG *y, scm_sizet ny, int sgn, int modes);
static SCM scm_divbigint (SCM x, long z, int sgn, int mode);


#define DIGITS '0':case '1':case '2':case '3':case '4':\
 case '5':case '6':case '7':case '8':case '9'


#define SCM_SWAP(x,y) do { SCM __t = x; x = y; y = __t; } while (0)


#if (SCM_DEBUG_DEPRECATED == 1)  /* not defined in header yet? */

/* SCM_FLOBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an inexact number.
 */
#define SCM_FLOBUFLEN (10+2*(sizeof(double)/sizeof(char)*SCM_CHAR_BIT*3+9)/10)

#endif  /* SCM_DEBUG_DEPRECATED == 1 */


/* IS_INF tests its floating point number for infiniteness
   Dirk:FIXME:: This test does not work if x == 0
 */
#ifndef IS_INF
#define IS_INF(x) ((x) == (x) / 2)
#endif


/* Return true if X is not infinite and is not a NaN
   Dirk:FIXME:: Since IS_INF is broken, this test does not work if x == 0
 */
#ifndef isfinite
#define isfinite(x) (!IS_INF (x) && (x) == (x))
#endif



static SCM abs_most_negative_fixnum;




SCM_DEFINE (scm_exact_p, "exact?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is an exact number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_exact_p
{
  if (SCM_INUMP (x)) {
    return SCM_BOOL_T;
  } else if (SCM_BIGP (x)) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_odd_p, "odd?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is an odd number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_odd_p
{
  if (SCM_INUMP (n)) {
    return SCM_BOOL ((4 & SCM_UNPACK (n)) != 0);
  } else if (SCM_BIGP (n)) {
    return SCM_BOOL ((1 & SCM_BDIGITS (n) [0]) != 0);
  } else {
    SCM_WRONG_TYPE_ARG (1, n);
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_even_p, "even?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is an even number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_even_p
{
  if (SCM_INUMP (n)) {
    return SCM_BOOL ((4 & SCM_UNPACK (n)) == 0);
  } else if (SCM_BIGP (n)) {
    return SCM_BOOL ((1 & SCM_BDIGITS (n) [0]) == 0);
  } else {
    SCM_WRONG_TYPE_ARG (1, n);
  }
}
#undef FUNC_NAME


SCM_GPROC (s_abs, "abs", 1, 0, 0, scm_abs, g_abs);
/* "Return the absolute value of @var{x}."
 */
SCM
scm_abs (SCM x)
{
  if (SCM_INUMP (x)) {
    long int xx = SCM_INUM (x);
    if (xx >= 0) {
      return x;
    } else if (SCM_POSFIXABLE (-xx)) {
      return SCM_MAKINUM (-xx);
    } else {
#ifdef SCM_BIGDIG
      return scm_long2big (-xx);
#else
      scm_num_overflow (s_abs);
#endif
    }
  } else if (SCM_BIGP (x)) {
    if (!SCM_BIGSIGN (x)) {
      return x;
    } else {
      return scm_copybig (x, 0);
    }
  } else if (SCM_REALP (x)) {
    return scm_make_real (fabs (SCM_REAL_VALUE (x)));
  } else {
    SCM_WTA_DISPATCH_1 (g_abs, x, 1, s_abs);
  }
}


SCM_GPROC (s_quotient, "quotient", 2, 0, 0, scm_quotient, g_quotient);
/* "Return the quotient of the numbers @var{x} and @var{y}."
 */
SCM
scm_quotient (SCM x, SCM y)
{
  if (SCM_INUMP (x)) {
    long xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_quotient);
      } else {
	long z = xx / yy;
	if (SCM_FIXABLE (z)) {
	  return SCM_MAKINUM (z);
	} else {
#ifdef SCM_BIGDIG
	  return scm_long2big (z);
#else
	  scm_num_overflow (s_quotient);
#endif
	}
      }
    } else if (SCM_BIGP (y)) {
      if (SCM_INUM (x) == SCM_MOST_NEGATIVE_FIXNUM
	  && scm_bigcomp (abs_most_negative_fixnum, y) == 0)
	{
	  /* Special case:  x == fixnum-min && y == abs (fixnum-min) */
	  return SCM_MAKINUM (-1);
	}
      else
	return SCM_MAKINUM (0);
    } else {
      SCM_WTA_DISPATCH_2 (g_quotient, x, y, SCM_ARG2, s_quotient);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_quotient);
      } else if (yy == 1) {
	return x;
      } else {
	long z = yy < 0 ? -yy : yy;
	
	if (z < SCM_BIGRAD) {
	  SCM sw = scm_copybig (x, SCM_BIGSIGN (x) ? (yy > 0) : (yy < 0));
	  scm_divbigdig (SCM_BDIGITS (sw), SCM_NUMDIGS (sw), (SCM_BIGDIG) z);
	  return scm_normbig (sw);
	} else {
#ifndef SCM_DIGSTOOBIG
	  long w = scm_pseudolong (z);
	  return scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
				(SCM_BIGDIG *) & w, SCM_DIGSPERLONG,
				SCM_BIGSIGN (x) ? (yy > 0) : (yy < 0), 2);
#else
	  SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	  scm_longdigs (z, zdigs);
	  return scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
				zdigs, SCM_DIGSPERLONG,
				SCM_BIGSIGN (x) ? (yy > 0) : (yy < 0), 2);
#endif
	}
      }
    } else if (SCM_BIGP (y)) {
      return scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			    SCM_BDIGITS (y), SCM_NUMDIGS (y),
			    SCM_BIGSIGN (x) ^ SCM_BIGSIGN (y), 2);
    } else {
      SCM_WTA_DISPATCH_2 (g_quotient, x, y, SCM_ARG2, s_quotient);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_quotient, x, y, SCM_ARG1, s_quotient);
  }
}


SCM_GPROC (s_remainder, "remainder", 2, 0, 0, scm_remainder, g_remainder);
/* "Return the remainder of the numbers @var{x} and @var{y}.\n"
 * "@lisp\n"
 * "(remainder 13 4) @result{} 1\n"
 * "(remainder -13 4) @result{} -1\n"
 * "@end lisp"
 */
SCM
scm_remainder (SCM x, SCM y)
{
  if (SCM_INUMP (x)) {
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_remainder);
      } else {
	long z = SCM_INUM (x) % yy;
	return SCM_MAKINUM (z);
      }
    } else if (SCM_BIGP (y)) {
      if (SCM_INUM (x) == SCM_MOST_NEGATIVE_FIXNUM
	  && scm_bigcomp (abs_most_negative_fixnum, y) == 0)
	{
	  /* Special case:  x == fixnum-min && y == abs (fixnum-min) */
	  return SCM_MAKINUM (0);
	}
      else
	return x;
    } else {
      SCM_WTA_DISPATCH_2 (g_remainder, x, y, SCM_ARG2, s_remainder);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_remainder);
      } else {
	return scm_divbigint (x, yy, SCM_BIGSIGN (x), 0);
      }
    } else if (SCM_BIGP (y)) {
      return scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			    SCM_BDIGITS (y), SCM_NUMDIGS (y),
			    SCM_BIGSIGN (x), 0);
    } else {
      SCM_WTA_DISPATCH_2 (g_remainder, x, y, SCM_ARG2, s_remainder);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_remainder, x, y, SCM_ARG1, s_remainder);
  }
}


SCM_GPROC (s_modulo, "modulo", 2, 0, 0, scm_modulo, g_modulo);
/* "Return the modulo of the numbers @var{x} and @var{y}.\n"
 * "@lisp\n"
 * "(modulo 13 4) @result{} 1\n"
 * "(modulo -13 4) @result{} 3\n"
 * "@end lisp"
 */
SCM
scm_modulo (SCM x, SCM y)
{
  if (SCM_INUMP (x)) {
    long xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_modulo);
      } else {
	long z = xx % yy;
	return SCM_MAKINUM (((yy < 0) ? (z > 0) : (z < 0)) ? z + yy : z);
      }
    } else if (SCM_BIGP (y)) {
      return (SCM_BIGSIGN (y) ? (xx > 0) : (xx < 0)) ? scm_sum (x, y) : x;
    } else {
      SCM_WTA_DISPATCH_2 (g_modulo, x, y, SCM_ARG2, s_modulo);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_modulo);
      } else {
	return scm_divbigint (x, yy, yy < 0,
			      (SCM_BIGSIGN (x) ? (yy > 0) : (yy < 0)) ? 1 : 0);
      }
    } else if (SCM_BIGP (y)) {
      return scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			    SCM_BDIGITS (y), SCM_NUMDIGS (y),
			    SCM_BIGSIGN (y),
			    (SCM_BIGSIGN (x) ^ SCM_BIGSIGN (y)) ? 1 : 0);
    } else {
      SCM_WTA_DISPATCH_2 (g_modulo, x, y, SCM_ARG2, s_modulo);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_modulo, x, y, SCM_ARG1, s_modulo);
  }
}


SCM_GPROC1 (s_gcd, "gcd", scm_tc7_asubr, scm_gcd, g_gcd);
/* "Return the greatest common divisor of all arguments.\n"
 * "If called without arguments, 0 is returned."
 */
SCM
scm_gcd (SCM x, SCM y)
{
  if (SCM_UNBNDP (y)) {
    if (SCM_UNBNDP (x)) {
      return SCM_INUM0;
    } else {
      return x;
    }
  }

 tailrec:
  if (SCM_INUMP (x)) {
    if (SCM_INUMP (y)) {
      long xx = SCM_INUM (x);
      long yy = SCM_INUM (y);
      long u = xx < 0 ? -xx : xx;
      long v = yy < 0 ? -yy : yy;
      long result;

      if (xx == 0) {
	result = v;
      } else if (yy == 0) {
	result = u;
      } else {
	int k = 1;
	long t;

	/* Determine a common factor 2^k */
	while (!(1 & (u | v))) {
	  k <<= 1;
	  u >>= 1;
	  v >>= 1;
	}

	/* Now, any factor 2^n can be eliminated */
	if (u & 1) {
	  t = -v;
	} else {
	  t = u;
	b3:
	  t = SCM_SRS (t, 1);
	}
	if (!(1 & t))
	  goto b3;
	if (t > 0)
	  u = t;
	else
	  v = -t;
	t = u - v;
	if (t != 0)
	  goto b3;

	result = u * k;
      }
      if (SCM_POSFIXABLE (result)) {
	return SCM_MAKINUM (result);
      } else {
#ifdef SCM_BIGDIG
	return scm_long2big (result);
#else
	scm_num_overflow (s_gcd);
#endif
      }
    } else if (SCM_BIGP (y)) {
      SCM_SWAP (x, y);
      goto big_gcd;
    } else {
      SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG2, s_gcd);
    }
  } else if (SCM_BIGP (x)) {
  big_gcd:
    if (SCM_BIGSIGN (x))
      x = scm_copybig (x, 0);
  newy:
    if (SCM_INUMP (y)) {
      if (SCM_EQ_P (y, SCM_INUM0)) {
	return x;
      } else {
	goto swaprec;
      }
    } else if (SCM_BIGP (y)) {
      if (SCM_BIGSIGN (y))
	y = scm_copybig (y, 0);
      switch (scm_bigcomp (x, y))
	{
	case -1:  /* x > y */
	swaprec:
	{
	  SCM t = scm_remainder (x, y);
	  x = y;
	  y = t;
	}
	goto tailrec;
	case 1:  /* x < y */
	  y = scm_remainder (y, x);
	  goto newy;
	default:  /* x == y */
	  return x;
	}
      /* instead of the switch, we could just
	 return scm_gcd (y, scm_modulo (x, y)); */
    } else {
      SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG2, s_gcd);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG1, s_gcd);
  }
}


SCM_GPROC1 (s_lcm, "lcm", scm_tc7_asubr, scm_lcm, g_lcm);
/* "Return the least common multiple of the arguments.\n"
 * "If called without arguments, 1 is returned."
 */
SCM
scm_lcm (SCM n1, SCM n2)
{
  if (SCM_UNBNDP (n2)) {
    if (SCM_UNBNDP (n1)) {
      return SCM_MAKINUM (1L);
    } else {
      n2 = SCM_MAKINUM (1L);
    }
  };

#ifndef SCM_BIGDIG
  SCM_GASSERT2 (SCM_INUMP (n1), g_lcm, n1, n2, SCM_ARG1, s_lcm);
  SCM_GASSERT2 (SCM_INUMP (n2), g_lcm, n1, n2, SCM_ARGn, s_lcm);
#else
  SCM_GASSERT2 (SCM_INUMP (n1) || SCM_BIGP (n1),
		g_lcm, n1, n2, SCM_ARG1, s_lcm);
  SCM_GASSERT2 (SCM_INUMP (n2) || SCM_BIGP (n2),
		g_lcm, n1, n2, SCM_ARGn, s_lcm);
#endif

  {
    SCM d = scm_gcd (n1, n2);
    if (SCM_EQ_P (d, SCM_INUM0)) {
      return d;
    } else {
      return scm_abs (scm_product (n1, scm_quotient (n2, d)));
    }
  }
}


#ifndef scm_long2num
#define SCM_LOGOP_RETURN(x) scm_ulong2num(x)
#else
#define SCM_LOGOP_RETURN(x) SCM_MAKINUM(x)
#endif


/* Emulating 2's complement bignums with sign magnitude arithmetic:

   Logand:
   X	Y	Result	Method:
		 (len)
   +	+	+ x	(map digit:logand X Y)
   +	-	+ x	(map digit:logand X (lognot (+ -1 Y)))
   -	+	+ y	(map digit:logand (lognot (+ -1 X)) Y)
   -	-	-	(+ 1 (map digit:logior (+ -1 X) (+ -1 Y)))

   Logior:
   X	Y	Result	Method:

   +	+	+	(map digit:logior X Y)
   +	-	- y	(+ 1 (map digit:logand (lognot X) (+ -1 Y)))
   -	+	- x	(+ 1 (map digit:logand (+ -1 X) (lognot Y)))
   -	-	- x	(+ 1 (map digit:logand (+ -1 X) (+ -1 Y)))

   Logxor:
   X	Y	Result	Method:

   +	+	+	(map digit:logxor X Y)
   +	-	-	(+ 1 (map digit:logxor X (+ -1 Y)))
   -	+	-	(+ 1 (map digit:logxor (+ -1 X) Y))
   -	-	+	(map digit:logxor (+ -1 X) (+ -1 Y))

   Logtest:
   X	Y	Result

   +	+	(any digit:logand X Y)
   +	-	(any digit:logand X (lognot (+ -1 Y)))
   -	+	(any digit:logand (lognot (+ -1 X)) Y)
   -	-	#t

*/

#ifdef SCM_BIGDIG

SCM scm_copy_big_dec(SCM b, int sign);
SCM scm_copy_smaller(SCM_BIGDIG *x, scm_sizet nx, int zsgn);
SCM scm_big_ior(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy);
SCM scm_big_xor(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy);
SCM scm_big_and(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy, int zsgn);
SCM scm_big_test(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy);

SCM scm_copy_big_dec(SCM b, int sign)
{
  long num = -1;
  scm_sizet nx = SCM_NUMDIGS(b);
  scm_sizet i = 0;
  SCM ans = scm_mkbig(nx, sign);
  SCM_BIGDIG *src = SCM_BDIGITS(b), *dst = SCM_BDIGITS(ans);
  if SCM_BIGSIGN(b) do {
    num += src[i];
    if (num < 0) {dst[i] = num + SCM_BIGRAD; num = -1;}
    else {dst[i] = SCM_BIGLO(num); num = 0;}
  } while (++i < nx);
  else
    while (nx--) dst[nx] = src[nx];
  return ans;
}

SCM scm_copy_smaller(SCM_BIGDIG *x, scm_sizet nx, int zsgn)
{
  long num = -1;
  scm_sizet i = 0;
  SCM z = scm_mkbig(nx, zsgn);
  SCM_BIGDIG *zds = SCM_BDIGITS(z);
  if (zsgn) do {
    num += x[i];
    if (num < 0) {zds[i] = num + SCM_BIGRAD; num = -1;}
    else {zds[i] = SCM_BIGLO(num); num = 0;}
  } while (++i < nx);
  else do zds[i] = x[i]; while (++i < nx);
  return z;
}

SCM scm_big_ior(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy)
/* Assumes nx <= SCM_NUMDIGS(bigy) */
/* Assumes xsgn equals either 0 or SCM_BIGSIGNFLAG */
{
  long num = -1;
  scm_sizet i = 0, ny = SCM_NUMDIGS(bigy);
  SCM z = scm_copy_big_dec (bigy, xsgn & SCM_BIGSIGN (bigy));
  SCM_BIGDIG *zds = SCM_BDIGITS(z);
  if (xsgn) {
    do {
      num += x[i];
      if (num < 0) {zds[i] |= num + SCM_BIGRAD; num = -1;}
      else {zds[i] |= SCM_BIGLO(num); num = 0;}
    } while (++i < nx);
    /* =========  Need to increment zds now =========== */
    i = 0; num = 1;
    while (i < ny) {
      num += zds[i];
      zds[i++] = SCM_BIGLO(num);
      num = SCM_BIGDN(num);
      if (!num) return z;
    }
    scm_adjbig(z, 1 + ny);		/* OOPS, overflowed into next digit. */
    SCM_BDIGITS(z)[ny] = 1;
    return z;
  }
  else do zds[i] = zds[i] | x[i]; while (++i < nx);
  return z;
}

SCM scm_big_xor(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy)
/* Assumes nx <= SCM_NUMDIGS(bigy) */
/* Assumes xsgn equals either 0 or SCM_BIGSIGNFLAG */
{
  long num = -1;
  scm_sizet i = 0, ny = SCM_NUMDIGS(bigy);
  SCM z = scm_copy_big_dec(bigy, xsgn ^ SCM_BIGSIGN(bigy));
  SCM_BIGDIG *zds = SCM_BDIGITS(z);
  if (xsgn) do {
    num += x[i];
    if (num < 0) {zds[i] ^= num + SCM_BIGRAD; num = -1;}
    else {zds[i] ^= SCM_BIGLO(num); num = 0;}
  } while (++i < nx);
  else do {
    zds[i] = zds[i] ^ x[i];
  } while (++i < nx);

  if (xsgn ^ SCM_BIGSIGN(bigy)) {
    /* =========  Need to increment zds now =========== */
    i = 0; num = 1;
    while (i < ny) {
      num += zds[i];
      zds[i++] = SCM_BIGLO(num);
      num = SCM_BIGDN(num);
      if (!num) return scm_normbig(z);
    }
  }
  return scm_normbig(z);
}

SCM scm_big_and(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy, int zsgn)
/* Assumes nx <= SCM_NUMDIGS(bigy) */
/* Assumes xsgn equals either 0 or SCM_BIGSIGNFLAG */
/* return sign equals either 0 or SCM_BIGSIGNFLAG */
{
  long num = -1;
  scm_sizet i = 0;
  SCM z;
  SCM_BIGDIG *zds;
  if (xsgn==zsgn) {
    z = scm_copy_smaller(x, nx, zsgn);
    x = SCM_BDIGITS(bigy);
    xsgn = SCM_BIGSIGN(bigy);
  }
  else z = scm_copy_big_dec(bigy, zsgn);
  zds = SCM_BDIGITS(z);

  if (zsgn) {
    if (xsgn) do {
      num += x[i];
      if (num < 0) {zds[i] &= num + SCM_BIGRAD; num = -1;}
      else {zds[i] &= SCM_BIGLO(num); num = 0;}
    } while (++i < nx);
    else do zds[i] = zds[i] & ~x[i]; while (++i < nx);
    /* =========  need to increment zds now =========== */
    i = 0; num = 1;
    while (i < nx) {
      num += zds[i];
      zds[i++] = SCM_BIGLO(num);
      num = SCM_BIGDN(num);
      if (!num) return scm_normbig(z);
    }
  }
  else if (xsgn) {
    unsigned long int carry = 1;
    do {
      unsigned long int mask = (SCM_BIGDIG) ~x[i] + carry;
      zds[i] = zds[i] & (SCM_BIGDIG) mask;
      carry = (mask >= SCM_BIGRAD) ? 1 : 0;
    } while (++i < nx);
  } else do zds[i] = zds[i] & x[i]; while (++i < nx);
  return scm_normbig(z);
}

SCM scm_big_test(SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy)
/* Assumes nx <= SCM_NUMDIGS(bigy) */
/* Assumes xsgn equals either 0 or SCM_BIGSIGNFLAG */
{
  SCM_BIGDIG *y;
  scm_sizet i = 0;
  long num = -1;
  if (SCM_BIGSIGN(bigy) & xsgn) return SCM_BOOL_T;
  if (SCM_NUMDIGS(bigy) != nx && xsgn) return SCM_BOOL_T;
  y = SCM_BDIGITS(bigy);
  if (xsgn)
    do {
      num += x[i];
      if (num < 0) {
	if (y[i] & ~(num + SCM_BIGRAD)) return SCM_BOOL_T;
	num = -1;
      }
      else {
	if (y[i] & ~SCM_BIGLO(num)) return SCM_BOOL_T;
	num = 0;
      }
    } while (++i < nx);
  else if SCM_BIGSIGN(bigy)
    do {
      num += y[i];
      if (num < 0) {
	if (x[i] & ~(num + SCM_BIGRAD)) return SCM_BOOL_T;
	num = -1;
      }
      else {
	if (x[i] & ~SCM_BIGLO(num)) return SCM_BOOL_T;
	num = 0;
      }
    } while (++i < nx);
  else
    do if (x[i] & y[i]) return SCM_BOOL_T;
    while (++i < nx);
  return SCM_BOOL_F;
}

#endif


SCM_DEFINE1 (scm_logand, "logand", scm_tc7_asubr,
             (SCM n1, SCM n2),
	     "Returns the integer which is the bit-wise AND of the two integer\n"
	     "arguments.\n\n"
	     "Example:\n"
	     "@lisp\n"
	     "(number->string (logand #b1100 #b1010) 2)\n"
	     "   @result{} \"1000\"\n"
	     "@end lisp")
#define FUNC_NAME s_scm_logand
{
  long int nn1;

  if (SCM_UNBNDP (n2)) {
    if (SCM_UNBNDP (n1)) {
      return SCM_MAKINUM (-1);
    } else if (!SCM_NUMBERP (n1)) {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
#ifndef SCM_RECKLESS
    } else if (SCM_NUMBERP (n1)) {
      return n1;
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
#else
    } else {
      return n1;
#endif
    }
  }

  if (SCM_INUMP (n1)) {
    nn1 = SCM_INUM (n1);
    if (SCM_INUMP (n2)) {
      long nn2 = SCM_INUM (n2);
      return SCM_MAKINUM (nn1 & nn2);
    } else if SCM_BIGP (n2) {
    intbig: 
      {
# ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong (nn1);
	if ((nn1 < 0) && SCM_BIGSIGN (n2)) {
	  return scm_big_ior ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG, 
			      SCM_BIGSIGNFLAG, n2);
	} else {
	  return scm_big_and ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG, 
			      (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2, 0);
	}
# else
	SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
	scm_longdigs (nn1, zdigs);
	if ((nn1 < 0) && SCM_BIGSIGN (n2)) {
	  return scm_big_ior (zdigs, SCM_DIGSPERLONG, SCM_BIGSIGNFLAG, n2);
	} else {
	  return scm_big_and (zdigs, SCM_DIGSPERLONG, 
			      (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2, 0);
	}
# endif
      }
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else if (SCM_BIGP (n1)) {
    if (SCM_INUMP (n2)) {
      SCM_SWAP (n1, n2);
      nn1 = SCM_INUM (n1);
      goto intbig;
    } else if (SCM_BIGP (n2)) {
      if (SCM_NUMDIGS (n1) > SCM_NUMDIGS (n2)) {
	SCM_SWAP (n1, n2);
      };
      if ((SCM_BIGSIGN (n1)) && SCM_BIGSIGN (n2)) {
	return scm_big_ior (SCM_BDIGITS (n1), SCM_NUMDIGS (n1),
			    SCM_BIGSIGNFLAG, n2);
      } else {
	return scm_big_and (SCM_BDIGITS (n1), SCM_NUMDIGS (n1),
			    SCM_BIGSIGN (n1), n2, 0);
      }
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
  }
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_logior, "logior", scm_tc7_asubr,
             (SCM n1, SCM n2),
	     "Returns the integer which is the bit-wise OR of the two integer\n"
	     "arguments.\n\n"
	     "Example:\n"
	     "@lisp\n"
	     "(number->string (logior #b1100 #b1010) 2)\n"
	     "   @result{} \"1110\"\n"
	     "@end lisp")
#define FUNC_NAME s_scm_logior
{
  long int nn1;

  if (SCM_UNBNDP (n2)) {
    if (SCM_UNBNDP (n1)) {
      return SCM_INUM0;
#ifndef SCM_RECKLESS
    } else if (SCM_NUMBERP (n1)) {
      return n1;
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
#else
    } else {
      return n1;
#endif
    }
  }

  if (SCM_INUMP (n1)) {
    nn1 = SCM_INUM (n1);
    if (SCM_INUMP (n2)) {
      long nn2 = SCM_INUM (n2);
      return SCM_MAKINUM (nn1 | nn2);
    } else if (SCM_BIGP (n2)) {
    intbig:
      {
# ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong (nn1);
	if ((!(nn1 < 0)) && !SCM_BIGSIGN (n2)) {
	  return scm_big_ior ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG, 
			      (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2);
	} else {
	  return scm_big_and ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG, 
			      (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2, SCM_BIGSIGNFLAG);
	}
# else
	SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
	scm_longdigs (nn1, zdigs);
	if ((!(nn1 < 0)) && !SCM_BIGSIGN (n2)) {
	  return scm_big_ior (zdigs, SCM_DIGSPERLONG, 
			      (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2);
	} else {
	  return scm_big_and (zdigs, SCM_DIGSPERLONG, 
			      (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2, SCM_BIGSIGNFLAG);
	}
# endif
      }
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else if (SCM_BIGP (n1)) {
    if (SCM_INUMP (n2)) {
      SCM_SWAP (n1, n2); 
      nn1 = SCM_INUM (n1);
      goto intbig;
    } else if (SCM_BIGP (n2)) {
      if (SCM_NUMDIGS (n1) > SCM_NUMDIGS (n2)) {
	SCM_SWAP (n1, n2);
      };
      if ((!SCM_BIGSIGN (n1)) && !SCM_BIGSIGN (n2)) {
	return scm_big_ior (SCM_BDIGITS (n1), SCM_NUMDIGS (n1), 
			    SCM_BIGSIGN (n1), n2);
      } else {
	return scm_big_and (SCM_BDIGITS (n1), SCM_NUMDIGS (n1), 
			    SCM_BIGSIGN (n1), n2, SCM_BIGSIGNFLAG);
      }
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
  }
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_logxor, "logxor", scm_tc7_asubr,
             (SCM n1, SCM n2),
	     "Returns the integer which is the bit-wise XOR of the two integer\n"
	     "arguments.\n\n"
	     "Example:\n"
	     "@lisp\n"
	     "(number->string (logxor #b1100 #b1010) 2)\n"
	     "   @result{} \"110\"\n"
	     "@end lisp")
#define FUNC_NAME s_scm_logxor
{
  long int nn1;

  if (SCM_UNBNDP (n2)) {
    if (SCM_UNBNDP (n1)) {
      return SCM_INUM0;
#ifndef SCM_RECKLESS
    } else if (SCM_NUMBERP (n1)) {
      return n1;
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
#else
    } else {
      return n1;
#endif
    }
  }

  if (SCM_INUMP (n1)) {
    nn1 = SCM_INUM (n1);
    if (SCM_INUMP (n2)) {
      long nn2 = SCM_INUM (n2);
      return SCM_MAKINUM (nn1 ^ nn2);
    } else if (SCM_BIGP (n2)) {
    intbig: 
      {
# ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong (nn1);
	return scm_big_xor ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG, 
			    (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2);
# else
	SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
	scm_longdigs (nn1, zdigs);
	return scm_big_xor (zdigs, SCM_DIGSPERLONG, 
			    (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2);
# endif
      }
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else if (SCM_BIGP (n1)) {
    if (SCM_INUMP (n2)) {
      SCM_SWAP (n1, n2);
      nn1 = SCM_INUM (n1);
      goto intbig;
    } else if (SCM_BIGP (n2)) {
      if (SCM_NUMDIGS(n1) > SCM_NUMDIGS(n2)) {
	SCM_SWAP (n1, n2);
      }
      return scm_big_xor (SCM_BDIGITS (n1), SCM_NUMDIGS (n1), 
			  SCM_BIGSIGN (n1), n2);
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_logtest, "logtest", 2, 0, 0,
            (SCM n1, SCM n2),
	    "@example\n"
	    "(logtest j k) @equiv{} (not (zero? (logand j k)))\n\n"
	    "(logtest #b0100 #b1011) @result{} #f\n"
	    "(logtest #b0100 #b0111) @result{} #t\n"
	    "@end example")
#define FUNC_NAME s_scm_logtest
{
  long int nn1;

  if (SCM_INUMP (n1)) {
    nn1 = SCM_INUM (n1);
    if (SCM_INUMP (n2)) {
      long nn2 = SCM_INUM (n2);
      return SCM_BOOL (nn1 & nn2);
    } else if (SCM_BIGP (n2)) {
    intbig: 
      {
# ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong (nn1);
	return scm_big_test ((SCM_BIGDIG *)&z, SCM_DIGSPERLONG, 
			     (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2);
# else
	SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
	scm_longdigs (nn1, zdigs);
	return scm_big_test (zdigs, SCM_DIGSPERLONG, 
			     (nn1 < 0) ? SCM_BIGSIGNFLAG : 0, n2);
# endif
      }
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else if (SCM_BIGP (n1)) {
    if (SCM_INUMP (n2)) {
      SCM_SWAP (n1, n2);
      nn1 = SCM_INUM (n1);
      goto intbig;
    } else if (SCM_BIGP (n2)) {
      if (SCM_NUMDIGS (n1) > SCM_NUMDIGS (n2)) {
	SCM_SWAP (n1, n2);
      }
      return scm_big_test (SCM_BDIGITS (n1), SCM_NUMDIGS (n1), 
			   SCM_BIGSIGN (n1), n2);
    } else {
      SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_logbit_p, "logbit?", 2, 0, 0,
            (SCM index, SCM j),
	    "@example\n"
	    "(logbit? index j) @equiv{} (logtest (integer-expt 2 index) j)\n\n"
	    "(logbit? 0 #b1101) @result{} #t\n"
	    "(logbit? 1 #b1101) @result{} #f\n"
	    "(logbit? 2 #b1101) @result{} #t\n"
	    "(logbit? 3 #b1101) @result{} #t\n"
	    "(logbit? 4 #b1101) @result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_logbit_p
{
  unsigned long int iindex;

  SCM_VALIDATE_INUM_MIN (SCM_ARG1, index, 0);
  iindex = (unsigned long int) SCM_INUM (index);

  if (SCM_INUMP (j)) {
    return SCM_BOOL ((1L << iindex) & SCM_INUM (j));
  } else if (SCM_BIGP (j)) {
    if (SCM_NUMDIGS (j) * SCM_BITSPERDIG < iindex) {
      return SCM_BOOL_F;
    } else if (SCM_BIGSIGN (j)) {
      long num = -1;
      scm_sizet i = 0;
      SCM_BIGDIG * x = SCM_BDIGITS (j);
      scm_sizet nx = iindex / SCM_BITSPERDIG;
      while (1) {
	num += x[i];
	if (nx == i++) {
	  return SCM_BOOL (((1L << (iindex % SCM_BITSPERDIG)) & num) == 0);
	} else if (num < 0) {
	  num = -1;
	} else {
	  num = 0;
	}
      }
    } else {
      return SCM_BOOL (SCM_BDIGITS (j) [iindex / SCM_BITSPERDIG]
		       & (1L << (iindex % SCM_BITSPERDIG)));
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG2, j);
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_lognot, "lognot", 1, 0, 0, 
            (SCM n),
	    "Returns the integer which is the 2s-complement of the integer argument.\n\n"
	    "Example:\n"
	    "@lisp\n"
	    "(number->string (lognot #b10000000) 2)\n"
	    "   @result{} \"-10000001\"\n"
	    "(number->string (lognot #b0) 2)\n"
	    "   @result{} \"-1\"\n"
	    "@end lisp\n")
#define FUNC_NAME s_scm_lognot
{
  return scm_difference (SCM_MAKINUM (-1L), n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_integer_expt, "integer-expt", 2, 0, 0,
            (SCM n, SCM k),
	    "Returns @var{n} raised to the non-negative integer exponent @var{k}.\n\n"
	    "Example:\n"
	    "@lisp\n"
	    "(integer-expt 2 5)\n"
	    "   @result{} 32\n"
	    "(integer-expt -3 3)\n"
	    "   @result{} -27\n"
	    "@end lisp")
#define FUNC_NAME s_scm_integer_expt
{
  SCM acc = SCM_MAKINUM (1L);
  int i2;
#ifdef SCM_BIGDIG
  if (SCM_EQ_P (n, SCM_INUM0) || SCM_EQ_P (n, acc))
    return n;
  else if (SCM_EQ_P (n, SCM_MAKINUM (-1L)))
    return SCM_FALSEP (scm_even_p (k)) ? n : acc;
#endif
  SCM_VALIDATE_ULONG_COPY (2,k,i2);
  if (i2 < 0)
    {
      i2 = -i2;
      n = scm_divide (n, SCM_UNDEFINED);
    }
  while (1)
    {
      if (0 == i2)
	return acc;
      if (1 == i2)
	return scm_product (acc, n);
      if (i2 & 1)
	acc = scm_product (acc, n);
      n = scm_product (n, n);
      i2 >>= 1;
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_ash, "ash", 2, 0, 0,
            (SCM n, SCM cnt),
	    "The function ash performs an arithmetic shift left by @var{CNT}\n"
	    "bits (or shift right, if @var{cnt} is negative).\n"
	    "'Arithmetic' means, that the function does not guarantee to\n"
	    "keep the bit structure of @var{n}, but rather guarantees that\n"
	    "the result will always be rounded towards minus infinity.\n"
	    "Therefore, the results of ash and a corresponding bitwise\n"
	    "shift will differ if N is negative.\n\n"
	    "Formally, the function returns an integer equivalent to\n"
	    "@code{(inexact->exact (floor (* @var{n} (expt 2 @var{cnt}))))}.\n\n"
	    "Example:\n"
	    "@lisp\n"
	    "(number->string (ash #b1 3) 2)\n"
	    "   @result{} \"1000\"\n"
	    "(number->string (ash #b1010 -1) 2)\n"
	    "   @result{} \"101\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_ash
{
  long bits_to_shift;

#ifndef SCM_BIGDIG
  SCM_VALIDATE_INUM (1, n)
#endif
  SCM_VALIDATE_INUM (2, cnt);

  bits_to_shift = SCM_INUM (cnt);
#ifdef SCM_BIGDIG
  if (bits_to_shift < 0) {
    /* Shift right by abs(cnt) bits.  This is realized as a division by
       div:=2^abs(cnt).  However, to guarantee the floor rounding, negative
       values require some special treatment.
     */
    SCM div = scm_integer_expt (SCM_MAKINUM (2), SCM_MAKINUM (-bits_to_shift));
    if (SCM_FALSEP (scm_negative_p (n)))
      return scm_quotient (n, div);
    else
      return scm_sum (SCM_MAKINUM (-1L),
		      scm_quotient (scm_sum (SCM_MAKINUM (1L), n), div));
  } else
    /* Shift left is done by multiplication with 2^CNT */
    return scm_product (n, scm_integer_expt (SCM_MAKINUM (2), cnt));
#else
  if (bits_to_shift < 0)
    /* Signed right shift (SCM_SRS does it right) by abs(cnt) bits. */
    return SCM_MAKINUM (SCM_SRS (SCM_INUM (n), -bits_to_shift));
  else {
    /* Shift left, but make sure not to leave the range of inums */
    SCM res = SCM_MAKINUM (SCM_INUM (n) << cnt);
    if (SCM_INUM (res) >> cnt != SCM_INUM (n))
      scm_num_overflow (FUNC_NAME);
    return res;
  }
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_bit_extract, "bit-extract", 3, 0, 0,
            (SCM n, SCM start, SCM end),
	    "Returns the integer composed of the @var{start} (inclusive) through\n"
	    "@var{end} (exclusive) bits of @var{n}.  The @var{start}th bit becomes\n"
	    "the 0-th bit in the result.@refill\n\n"
	    "Example:\n"
	    "@lisp\n"
	    "(number->string (bit-extract #b1101101010 0 4) 2)\n"
	    "   @result{} \"1010\"\n"
	    "(number->string (bit-extract #b1101101010 4 9) 2)\n"
	    "   @result{} \"10110\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_bit_extract
{
  unsigned long int istart, iend;
  SCM_VALIDATE_INUM_MIN_COPY (2,start,0,istart);
  SCM_VALIDATE_INUM_MIN_COPY (3, end, 0, iend);
  SCM_ASSERT_RANGE (3, end, (iend >= istart));

  if (SCM_INUMP (n)) {
    long int in = SCM_INUM (n);
    unsigned long int bits = iend - istart;

    if (in < 0 && bits >= SCM_FIXNUM_BIT)
      {
	/* Since we emulate two's complement encoded numbers, this special
	 * case requires us to produce a result that has more bits than can be
	 * stored in a fixnum.  Thus, we fall back to the more general
	 * algorithm that is used for bignums.  
	 */
	goto generalcase;
      }

    if (istart < SCM_FIXNUM_BIT)
      {
	in = in >> istart;
	if (bits < SCM_FIXNUM_BIT)
	  return SCM_MAKINUM (in & ((1L << bits) - 1));
	else /* we know: in >= 0 */
	  return SCM_MAKINUM (in);
      }
    else if (in < 0)
      {
	return SCM_MAKINUM (-1L & ((1L << bits) - 1));
      }
    else
      {
	return SCM_MAKINUM (0);
      }
  } else if (SCM_BIGP (n)) {
  generalcase:
    {
      SCM num1 = SCM_MAKINUM (1L);
      SCM num2 = SCM_MAKINUM (2L);
      SCM bits = SCM_MAKINUM (iend - istart);
      SCM mask  = scm_difference (scm_integer_expt (num2, bits), num1);
      return scm_logand (mask, scm_ash (n, SCM_MAKINUM (-istart)));
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
  }
}
#undef FUNC_NAME


static const char scm_logtab[] = {
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
};

SCM_DEFINE (scm_logcount, "logcount", 1, 0, 0,
            (SCM n),
	    "Returns the number of bits in integer @var{n}.  If integer is positive,\n"
	    "the 1-bits in its binary representation are counted.  If negative, the\n"
	    "0-bits in its two's-complement binary representation are counted.  If 0,\n"
	    "0 is returned.\n\n"
	    "Example:\n"
	    "@lisp\n"
	    "(logcount #b10101010)\n"
	    "   @result{} 4\n"
	    "(logcount 0)\n"
	    "   @result{} 0\n"
	    "(logcount -2)\n"
	    "   @result{} 1\n"
	    "@end lisp")
#define FUNC_NAME s_scm_logcount
{
  if (SCM_INUMP (n)) {
    unsigned long int c = 0;
    long int nn = SCM_INUM (n);
    if (nn < 0) {
      nn = -1 - nn;
    };
    while (nn) {
      c += scm_logtab[15 & nn];
      nn >>= 4;
    };
    return SCM_MAKINUM (c);
  } else if (SCM_BIGP (n)) {
    if (SCM_BIGSIGN (n)) {
      return scm_logcount (scm_difference (SCM_MAKINUM (-1L), n));
    } else {
      unsigned long int c = 0;
      scm_sizet i = SCM_NUMDIGS (n);
      SCM_BIGDIG * ds = SCM_BDIGITS (n);
      while (i--) {
	SCM_BIGDIG d;
	for (d = ds[i]; d; d >>= 4) {
	  c += scm_logtab[15 & d];
	}
      }
      return SCM_MAKINUM (c);
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
  }
}
#undef FUNC_NAME


static const char scm_ilentab[] = {
  0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4
};

SCM_DEFINE (scm_integer_length, "integer-length", 1, 0, 0,
            (SCM n),
	    "Returns the number of bits neccessary to represent @var{n}.\n\n"
	    "Example:\n"
	    "@lisp\n"
	    "(integer-length #b10101010)\n"
	    "   @result{} 8\n"
	    "(integer-length 0)\n"
	    "   @result{} 0\n"
	    "(integer-length #b1111)\n"
	    "   @result{} 4\n"
	    "@end lisp")
#define FUNC_NAME s_scm_integer_length
{
  if (SCM_INUMP (n)) {
    unsigned long int c = 0;
    unsigned int l = 4;
    long int nn = SCM_INUM (n);
    if (nn < 0) {
      nn = -1 - nn;
    };
    while (nn) {
      c += 4;
      l = scm_ilentab [15 & nn];
      nn >>= 4;
    };
    return SCM_MAKINUM (c - 4 + l);
  } else if (SCM_BIGP (n)) {
    if (SCM_BIGSIGN (n)) {
      return scm_integer_length (scm_difference (SCM_MAKINUM (-1L), n));
    } else {
      unsigned long int digs = SCM_NUMDIGS (n) - 1;
      unsigned long int c = digs * SCM_BITSPERDIG;
      unsigned int l = 4;
      SCM_BIGDIG * ds = SCM_BDIGITS (n);
      SCM_BIGDIG d = ds [digs];
      while (d) {
	c += 4;
	l = scm_ilentab [15 & d];
	d >>= 4;
      };
      return SCM_MAKINUM (c - 4 + l);
    }
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
  }
}
#undef FUNC_NAME


#ifdef SCM_BIGDIG
static const char s_bignum[] = "bignum";

SCM
scm_mkbig (scm_sizet nlen, int sign)
{
  SCM v;
  /* Cast to long int to avoid signed/unsigned comparison warnings.  */
  if ((( ((long int) nlen) << SCM_BIGSIZEFIELD) >> SCM_BIGSIZEFIELD)
      != (long int) nlen)
    scm_memory_error (s_bignum);
  
  SCM_NEWCELL (v);
  SCM_DEFER_INTS;
  SCM_SET_BIGNUM_BASE (v, scm_must_malloc (nlen * sizeof (SCM_BIGDIG), s_bignum));
  SCM_SETNUMDIGS (v, nlen, sign);
  SCM_ALLOW_INTS;
  return v;
}


SCM
scm_big2inum (SCM b, scm_sizet l)
{
  unsigned long num = 0;
  SCM_BIGDIG *tmp = SCM_BDIGITS (b);
  while (l--)
    num = SCM_BIGUP (num) + tmp[l];
  if (!SCM_BIGSIGN (b))
    {
      if (SCM_POSFIXABLE (num))
	return SCM_MAKINUM (num);
    }
  else if (num <= -SCM_MOST_NEGATIVE_FIXNUM)
    return SCM_MAKINUM (-num);
  return b;
}


static const char s_adjbig[] = "scm_adjbig";

SCM
scm_adjbig (SCM b, scm_sizet nlen)
{
  scm_sizet nsiz = nlen;
  if (((nsiz << SCM_BIGSIZEFIELD) >> SCM_BIGSIZEFIELD) != nlen)
    scm_memory_error (s_adjbig);

  SCM_DEFER_INTS;
  {
    SCM_BIGDIG *digits
      = ((SCM_BIGDIG *)
	 scm_must_realloc ((char *) SCM_BDIGITS (b),
			   (long) (SCM_NUMDIGS (b) * sizeof (SCM_BIGDIG)),
			   (long) (nsiz * sizeof (SCM_BIGDIG)), s_bignum));

    SCM_SET_BIGNUM_BASE (b, digits);
    SCM_SETNUMDIGS (b, nsiz, SCM_BIGSIGN (b));
  }
  SCM_ALLOW_INTS;
  return b;
}



SCM
scm_normbig (SCM b)
{
#ifndef _UNICOS
  scm_sizet nlen = SCM_NUMDIGS (b);
#else
  int nlen = SCM_NUMDIGS (b); /* unsigned nlen breaks on Cray when nlen => 0 */
#endif
  SCM_BIGDIG *zds = SCM_BDIGITS (b);
  while (nlen-- && !zds[nlen]);
  nlen++;
  if (nlen * SCM_BITSPERDIG / SCM_CHAR_BIT <= sizeof (SCM))
    if (SCM_INUMP (b = scm_big2inum (b, (scm_sizet) nlen)))
      return b;
  if (SCM_NUMDIGS (b) == nlen)
    return b;
  return scm_adjbig (b, (scm_sizet) nlen);
}



SCM
scm_copybig (SCM b, int sign)
{
  scm_sizet i = SCM_NUMDIGS (b);
  SCM ans = scm_mkbig (i, sign);
  SCM_BIGDIG *src = SCM_BDIGITS (b), *dst = SCM_BDIGITS (ans);
  while (i--)
    dst[i] = src[i];
  return ans;
}



SCM
scm_long2big (long n)
{
  scm_sizet i = 0;
  SCM_BIGDIG *digits;
  SCM ans = scm_mkbig (SCM_DIGSPERLONG, n < 0);
  digits = SCM_BDIGITS (ans);
  if (n < 0)
    n = -n;
  while (i < SCM_DIGSPERLONG)
    {
      digits[i++] = SCM_BIGLO (n);
      n = SCM_BIGDN ((unsigned long) n);
    }
  return ans;
}

#ifdef HAVE_LONG_LONGS

SCM
scm_long_long2big (long_long n)
{
  scm_sizet i;
  SCM_BIGDIG *digits;
  SCM ans;
  int n_digits;

  {
    long tn;
    tn = (long) n;
    if ((long long) tn == n)
      return scm_long2big (tn);
  }

  {
    long_long tn;

    for (tn = n, n_digits = 0;
	 tn;
	 ++n_digits, tn = SCM_BIGDN ((ulong_long) tn))
      ;
  }

  i = 0;
  ans = scm_mkbig (n_digits, n < 0);
  digits = SCM_BDIGITS (ans);
  if (n < 0)
    n = -n;
  while (i < n_digits)
    {
      digits[i++] = SCM_BIGLO (n);
      n = SCM_BIGDN ((ulong_long) n);
    }
  return ans;
}
#endif /* HAVE_LONG_LONGS */


SCM
scm_2ulong2big (unsigned long *np)
{
  unsigned long n;
  scm_sizet i;
  SCM_BIGDIG *digits;
  SCM ans;

  ans = scm_mkbig (2 * SCM_DIGSPERLONG, 0);
  digits = SCM_BDIGITS (ans);

  n = np[0];
  for (i = 0; i < SCM_DIGSPERLONG; ++i)
    {
      digits[i] = SCM_BIGLO (n);
      n = SCM_BIGDN ((unsigned long) n);
    }
  n = np[1];
  for (i = 0; i < SCM_DIGSPERLONG; ++i)
    {
      digits[i + SCM_DIGSPERLONG] = SCM_BIGLO (n);
      n = SCM_BIGDN ((unsigned long) n);
    }
  return ans;
}



SCM
scm_ulong2big (unsigned long n)
{
  scm_sizet i = 0;
  SCM_BIGDIG *digits;
  SCM ans = scm_mkbig (SCM_DIGSPERLONG, 0);
  digits = SCM_BDIGITS (ans);
  while (i < SCM_DIGSPERLONG)
    {
      digits[i++] = SCM_BIGLO (n);
      n = SCM_BIGDN (n);
    }
  return ans;
}



int
scm_bigcomp (SCM x, SCM y)
{
  int xsign = SCM_BIGSIGN (x);
  int ysign = SCM_BIGSIGN (y);
  scm_sizet xlen, ylen;

  /* Look at the signs, first.  */
  if (ysign < xsign)
    return 1;
  if (ysign > xsign)
    return -1;

  /* They're the same sign, so see which one has more digits.  Note
     that, if they are negative, the longer number is the lesser.  */
  ylen = SCM_NUMDIGS (y);
  xlen = SCM_NUMDIGS (x);
  if (ylen > xlen)
    return (xsign) ? -1 : 1;
  if (ylen < xlen)
    return (xsign) ? 1 : -1;

  /* They have the same number of digits, so find the most significant
     digit where they differ.  */
  while (xlen)
    {
      --xlen;
      if (SCM_BDIGITS (y)[xlen] != SCM_BDIGITS (x)[xlen])
	/* Make the discrimination based on the digit that differs.  */
	return ((SCM_BDIGITS (y)[xlen] > SCM_BDIGITS (x)[xlen])
		? (xsign ? -1 :  1)
		: (xsign ?  1 : -1));
    }

  /* The numbers are identical.  */
  return 0;
}

#ifndef SCM_DIGSTOOBIG


long
scm_pseudolong (long x)
{
  union
  {
    long l;
    SCM_BIGDIG bd[SCM_DIGSPERLONG];
  }
  p;
  scm_sizet i = 0;
  if (x < 0)
    x = -x;
  while (i < SCM_DIGSPERLONG)
    {
      p.bd[i++] = SCM_BIGLO (x);
      x = SCM_BIGDN (x);
    }
  /*  p.bd[0] = SCM_BIGLO(x); p.bd[1] = SCM_BIGDN(x); */
  return p.l;
}

#else


void
scm_longdigs (long x, SCM_BIGDIG digs[])
{
  scm_sizet i = 0;
  if (x < 0)
    x = -x;
  while (i < SCM_DIGSPERLONG)
    {
      digs[i++] = SCM_BIGLO (x);
      x = SCM_BIGDN (x);
    }
}
#endif



SCM
scm_addbig (SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy, int sgny)
{
  /* Assumes nx <= SCM_NUMDIGS(bigy) */
  /* Assumes xsgn and sgny scm_equal either 0 or SCM_BIGSIGNFLAG */
  long num = 0;
  scm_sizet i = 0, ny = SCM_NUMDIGS (bigy);
  SCM z = scm_copybig (bigy, SCM_BIGSIGN (bigy) ^ sgny);
  SCM_BIGDIG *zds = SCM_BDIGITS (z);
  if (xsgn ^ SCM_BIGSIGN (z))
    {
      do
	{
	  num += (long) zds[i] - x[i];
	  if (num < 0)
	    {
	      zds[i] = num + SCM_BIGRAD;
	      num = -1;
	    }
	  else
	    {
	      zds[i] = SCM_BIGLO (num);
	      num = 0;
	    }
	}
      while (++i < nx);
      if (num && nx == ny)
	{
	  num = 1;
	  i = 0;
	  SCM_SET_CELL_WORD_0 (z, SCM_CELL_WORD_0 (z) ^ SCM_BIGSIGNFLAG);
	  do
	    {
	      num += (SCM_BIGRAD - 1) - zds[i];
	      zds[i++] = SCM_BIGLO (num);
	      num = SCM_BIGDN (num);
	    }
	  while (i < ny);
	}
      else
	while (i < ny)
	  {
	    num += zds[i];
	    if (num < 0)
	      {
		zds[i++] = num + SCM_BIGRAD;
		num = -1;
	      }
	    else
	      {
		zds[i++] = SCM_BIGLO (num);
		num = 0;
	      }
	  }
    }
  else
    {
      do
	{
	  num += (long) zds[i] + x[i];
	  zds[i++] = SCM_BIGLO (num);
	  num = SCM_BIGDN (num);
	}
      while (i < nx);
      if (!num)
	return z;
      while (i < ny)
	{
	  num += zds[i];
	  zds[i++] = SCM_BIGLO (num);
	  num = SCM_BIGDN (num);
	  if (!num)
	    return z;
	}
      if (num)
	{
	  z = scm_adjbig (z, ny + 1);
	  SCM_BDIGITS (z)[ny] = num;
	  return z;
	}
    }
  return scm_normbig (z);
}


SCM
scm_mulbig (SCM_BIGDIG *x, scm_sizet nx, SCM_BIGDIG *y, scm_sizet ny, int sgn)
{
  scm_sizet i = 0, j = nx + ny;
  unsigned long n = 0;
  SCM z = scm_mkbig (j, sgn);
  SCM_BIGDIG *zds = SCM_BDIGITS (z);
  while (j--)
    zds[j] = 0;
  do
    {
      j = 0;
      if (x[i])
	{
	  do
	    {
	      n += zds[i + j] + ((unsigned long) x[i] * y[j]);
	      zds[i + j++] = SCM_BIGLO (n);
	      n = SCM_BIGDN (n);
	    }
	  while (j < ny);
	  if (n)
	    {
	      zds[i + j] = n;
	      n = 0;
	    }
	}
    }
  while (++i < nx);
  return scm_normbig (z);
}


unsigned int
scm_divbigdig (SCM_BIGDIG * ds, scm_sizet h, SCM_BIGDIG div)
{
  register unsigned long t2 = 0;
  while (h--)
    {
      t2 = SCM_BIGUP (t2) + ds[h];
      ds[h] = t2 / div;
      t2 %= div;
    }
  return t2;
}



static SCM
scm_divbigint (SCM x, long z, int sgn, int mode)
{
  if (z < 0)
    z = -z;
  if (z < SCM_BIGRAD)
    {
      register unsigned long t2 = 0;
      register SCM_BIGDIG *ds = SCM_BDIGITS (x);
      scm_sizet nd = SCM_NUMDIGS (x);
      while (nd--)
	t2 = (SCM_BIGUP (t2) + ds[nd]) % z;
      if (mode && t2)
	t2 = z - t2;
      return SCM_MAKINUM (sgn ? -t2 : t2);
    }
  {
#ifndef SCM_DIGSTOOBIG
    unsigned long t2 = scm_pseudolong (z);
    return scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			  (SCM_BIGDIG *) & t2, SCM_DIGSPERLONG,
			  sgn, mode);
#else
    SCM_BIGDIG t2[SCM_DIGSPERLONG];
    scm_longdigs (z, t2);
    return scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			  t2, SCM_DIGSPERLONG,
			  sgn, mode);
#endif
  }
}


static SCM
scm_divbigbig (SCM_BIGDIG *x, scm_sizet nx, SCM_BIGDIG *y, scm_sizet ny, int sgn, int modes)
{
  /* modes description
     0  remainder
     1  scm_modulo
     2  quotient
     3  quotient but returns SCM_UNDEFINED if division is not exact. */
  scm_sizet i = 0, j = 0;
  long num = 0;
  unsigned long t2 = 0;
  SCM z, newy;
  SCM_BIGDIG d = 0, qhat, *zds, *yds;
  /* algorithm requires nx >= ny */
  if (nx < ny)
    switch (modes)
      {
      case 0:			/* remainder -- just return x */
	z = scm_mkbig (nx, sgn);
	zds = SCM_BDIGITS (z);
	do
	  {
	    zds[i] = x[i];
	  }
	while (++i < nx);
	return z;
      case 1:			/* scm_modulo -- return y-x */
	z = scm_mkbig (ny, sgn);
	zds = SCM_BDIGITS (z);
	do
	  {
	    num += (long) y[i] - x[i];
	    if (num < 0)
	      {
		zds[i] = num + SCM_BIGRAD;
		num = -1;
	      }
	    else
	      {
		zds[i] = num;
		num = 0;
	      }
	  }
	while (++i < nx);
	while (i < ny)
	  {
	    num += y[i];
	    if (num < 0)
	      {
		zds[i++] = num + SCM_BIGRAD;
		num = -1;
	      }
	    else
	      {
		zds[i++] = num;
		num = 0;
	      }
	  }
	goto doadj;
      case 2:
	return SCM_INUM0;	/* quotient is zero */
      case 3:
	return SCM_UNDEFINED;   /* the division is not exact */
      }

  z = scm_mkbig (nx == ny ? nx + 2 : nx + 1, sgn);
  zds = SCM_BDIGITS (z);
  if (nx == ny)
    zds[nx + 1] = 0;
  while (!y[ny - 1])
    ny--;			/* in case y came in as a psuedolong */
  if (y[ny - 1] < (SCM_BIGRAD >> 1))
    {				/* normalize operands */
      d = SCM_BIGRAD / (y[ny - 1] + 1);
      newy = scm_mkbig (ny, 0);
      yds = SCM_BDIGITS (newy);
      while (j < ny)
	{
	  t2 += (unsigned long) y[j] * d;
	  yds[j++] = SCM_BIGLO (t2);
	  t2 = SCM_BIGDN (t2);
	}
      y = yds;
      j = 0;
      t2 = 0;
      while (j < nx)
	{
	  t2 += (unsigned long) x[j] * d;
	  zds[j++] = SCM_BIGLO (t2);
	  t2 = SCM_BIGDN (t2);
	}
      zds[j] = t2;
    }
  else
    {
      zds[j = nx] = 0;
      while (j--)
	zds[j] = x[j];
    }
  j = nx == ny ? nx + 1 : nx;	/* dividend needs more digits than divisor */
  do
    {				/* loop over digits of quotient */
      if (zds[j] == y[ny - 1])
	qhat = SCM_BIGRAD - 1;
      else
	qhat = (SCM_BIGUP (zds[j]) + zds[j - 1]) / y[ny - 1];
      if (!qhat)
	continue;
      i = 0;
      num = 0;
      t2 = 0;
      do
	{			/* multiply and subtract */
	  t2 += (unsigned long) y[i] * qhat;
	  num += zds[j - ny + i] - SCM_BIGLO (t2);
	  if (num < 0)
	    {
	      zds[j - ny + i] = num + SCM_BIGRAD;
	      num = -1;
	    }
	  else
	    {
	      zds[j - ny + i] = num;
	      num = 0;
	    }
	  t2 = SCM_BIGDN (t2);
	}
      while (++i < ny);
      num += zds[j - ny + i] - t2; /* borrow from high digit; don't update */
      while (num)
	{			/* "add back" required */
	  i = 0;
	  num = 0;
	  qhat--;
	  do
	    {
	      num += (long) zds[j - ny + i] + y[i];
	      zds[j - ny + i] = SCM_BIGLO (num);
	      num = SCM_BIGDN (num);
	    }
	  while (++i < ny);
	  num--;
	}
      if (modes & 2)
	zds[j] = qhat;
    }
  while (--j >= ny);
  switch (modes)
    {
    case 3:			/* check that remainder==0 */
      for (j = ny; j && !zds[j - 1]; --j);
      if (j)
	return SCM_UNDEFINED;
    case 2:			/* move quotient down in z */
      j = (nx == ny ? nx + 2 : nx + 1) - ny;
      for (i = 0; i < j; i++)
	zds[i] = zds[i + ny];
      ny = i;
      break;
    case 1:			/* subtract for scm_modulo */
      i = 0;
      num = 0;
      j = 0;
      do
	{
	  num += y[i] - zds[i];
	  j = j | zds[i];
	  if (num < 0)
	    {
	      zds[i] = num + SCM_BIGRAD;
	      num = -1;
	    }
	  else
	    {
	      zds[i] = num;
	      num = 0;
	    }
	}
      while (++i < ny);
      if (!j)
	return SCM_INUM0;
    case 0:			/* just normalize remainder */
      if (d)
	scm_divbigdig (zds, ny, d);
    }
 doadj:
  for (j = ny; j && !zds[j - 1]; --j);
  if (j * SCM_BITSPERDIG <= sizeof (SCM) * SCM_CHAR_BIT)
    if (SCM_INUMP (z = scm_big2inum (z, j)))
      return z;
  return scm_adjbig (z, j);
}
#endif





/*** NUMBERS -> STRINGS ***/
int scm_dblprec;
static const double fx[] =
{  0.0,  5e-1,  5e-2,  5e-3,   5e-4, 5e-5,
  5e-6,  5e-7,  5e-8,  5e-9,  5e-10,
 5e-11, 5e-12, 5e-13, 5e-14,  5e-15,
 5e-16, 5e-17, 5e-18, 5e-19,  5e-20};




static scm_sizet
idbl2str (double f, char *a)
{
  int efmt, dpt, d, i, wp = scm_dblprec;
  scm_sizet ch = 0;
  int exp = 0;

  if (f == 0.0)
    goto zero;			/*{a[0]='0'; a[1]='.'; a[2]='0'; return 3;} */
  if (f < 0.0)
    {
      f = -f;
      a[ch++] = '-';
    }
  else if (f > 0.0);
  else
    goto funny;
  if (IS_INF (f))
    {
      if (ch == 0)
	a[ch++] = '+';
    funny:
      a[ch++] = '#';
      a[ch++] = '.';
      a[ch++] = '#';
      return ch;
    }
#ifdef DBL_MIN_10_EXP  /* Prevent unnormalized values, as from 
			  make-uniform-vector, from causing infinite loops. */
  while (f < 1.0)
    {
      f *= 10.0;
      if (exp-- < DBL_MIN_10_EXP)
	goto funny;
    }
  while (f > 10.0)
    {
      f *= 0.10;
      if (exp++ > DBL_MAX_10_EXP)
	goto funny;
    }
#else
  while (f < 1.0)
    {
      f *= 10.0;
      exp--;
    }
  while (f > 10.0)
    {
      f /= 10.0;
      exp++;
    }
#endif
  if (f + fx[wp] >= 10.0)
    {
      f = 1.0;
      exp++;
    }
 zero:
#ifdef ENGNOT
  dpt = (exp + 9999) % 3;
  exp -= dpt++;
  efmt = 1;
#else
  efmt = (exp < -3) || (exp > wp + 2);
  if (!efmt)
    {
      if (exp < 0)
	{
	  a[ch++] = '0';
	  a[ch++] = '.';
	  dpt = exp;
	  while (++dpt)
	    a[ch++] = '0';
	}
      else
	dpt = exp + 1;
    }
  else
    dpt = 1;
#endif

  do
    {
      d = f;
      f -= d;
      a[ch++] = d + '0';
      if (f < fx[wp])
	break;
      if (f + fx[wp] >= 1.0)
	{
	  a[ch - 1]++;
	  break;
	}
      f *= 10.0;
      if (!(--dpt))
	a[ch++] = '.';
    }
  while (wp--);

  if (dpt > 0)
    {
#ifndef ENGNOT
      if ((dpt > 4) && (exp > 6))
	{
	  d = (a[0] == '-' ? 2 : 1);
	  for (i = ch++; i > d; i--)
	    a[i] = a[i - 1];
	  a[d] = '.';
	  efmt = 1;
	}
      else
#endif
	{
	  while (--dpt)
	    a[ch++] = '0';
	  a[ch++] = '.';
	}
    }
  if (a[ch - 1] == '.')
    a[ch++] = '0';		/* trailing zero */
  if (efmt && exp)
    {
      a[ch++] = 'e';
      if (exp < 0)
	{
	  exp = -exp;
	  a[ch++] = '-';
	}
      for (i = 10; i <= exp; i *= 10);
      for (i /= 10; i; i /= 10)
	{
	  a[ch++] = exp / i + '0';
	  exp %= i;
	}
    }
  return ch;
}


static scm_sizet
iflo2str (SCM flt, char *str)
{
  scm_sizet i;
  if (SCM_SLOPPY_REALP (flt))
    i = idbl2str (SCM_REAL_VALUE (flt), str);
  else
    {
      i = idbl2str (SCM_COMPLEX_REAL (flt), str);
      if (SCM_COMPLEX_IMAG (flt) != 0.0)
	{
	  if (0 <= SCM_COMPLEX_IMAG (flt))
	    str[i++] = '+';
	  i += idbl2str (SCM_COMPLEX_IMAG (flt), &str[i]);
	  str[i++] = 'i';
	}
    }
  return i;
}

/* convert a long to a string (unterminated).  returns the number of
   characters in the result. 
   rad is output base
   p is destination: worst case (base 2) is SCM_INTBUFLEN  */
scm_sizet
scm_iint2str (long num, int rad, char *p)
{
  scm_sizet j = 1;
  scm_sizet i;
  unsigned long n = (num < 0) ? -num : num;

  for (n /= rad; n > 0; n /= rad)
    j++;

  i = j;
  if (num < 0)
    {
      *p++ = '-';
      j++;
      n = -num;
    }
  else
    n = num;
  while (i--)
    {
      int d = n % rad;

      n /= rad;
      p[i] = d + ((d < 10) ? '0' : 'a' - 10);
    }
  return j;
}


#ifdef SCM_BIGDIG

static SCM
big2str (SCM b, unsigned int radix)
{
  SCM t = scm_copybig (b, 0);	/* sign of temp doesn't matter */
  register SCM_BIGDIG *ds = SCM_BDIGITS (t);
  scm_sizet i = SCM_NUMDIGS (t);
  scm_sizet j = radix == 16 ? (SCM_BITSPERDIG * i) / 4 + 2
    : radix >= 10 ? (SCM_BITSPERDIG * i * 241L) / 800 + 2
    : (SCM_BITSPERDIG * i) + 2;
  scm_sizet k = 0;
  scm_sizet radct = 0;
  SCM_BIGDIG radpow = 1, radmod = 0;
  SCM ss = scm_makstr ((long) j, 0);
  char *s = SCM_STRING_CHARS (ss), c;
  while ((long) radpow * radix < SCM_BIGRAD)
    {
      radpow *= radix;
      radct++;
    }
  while ((i || radmod) && j)
    {
      if (k == 0)
	{
	  radmod = (SCM_BIGDIG) scm_divbigdig (ds, i, radpow);
	  k = radct;
	  if (!ds[i - 1])
	    i--;
	}
      c = radmod % radix;
      radmod /= radix;
      k--;
      s[--j] = c < 10 ? c + '0' : c + 'a' - 10;
    }

  if (SCM_BIGSIGN (b))
    s[--j] = '-';

  if (j > 0)
    {
      /* The pre-reserved string length was too large. */
      unsigned long int length = SCM_STRING_LENGTH (ss);
      ss = scm_substring (ss, SCM_MAKINUM (j), SCM_MAKINUM (length));
    }

  return scm_return_first (ss, t);
}
#endif


SCM_DEFINE (scm_number_to_string, "number->string", 1, 1, 0,
            (SCM n, SCM radix),
	    "Return a string holding the external representation of the\n"
	    "number @var{n} in the given @var{radix}.  If @var{n} is\n"
	    "inexact, a radix of 10 will be used.")
#define FUNC_NAME s_scm_number_to_string
{
  int base;

  if (SCM_UNBNDP (radix)) {
    base = 10;
  } else {
    SCM_VALIDATE_INUM (2, radix);
    base = SCM_INUM (radix);
    SCM_ASSERT_RANGE (2, radix, base >= 2);
  }

  if (SCM_INUMP (n)) {
    char num_buf [SCM_INTBUFLEN];
    scm_sizet length = scm_iint2str (SCM_INUM (n), base, num_buf);
    return scm_makfromstr (num_buf, length, 0);
  } else if (SCM_BIGP (n)) {
    return big2str (n, (unsigned int) base);
  } else if (SCM_INEXACTP (n)) {
    char num_buf [SCM_FLOBUFLEN];
    return scm_makfromstr (num_buf, iflo2str (n, num_buf), 0);
  } else {
    SCM_WRONG_TYPE_ARG (1, n);
  }
}
#undef FUNC_NAME


/* These print routines are stubbed here so that scm_repl.c doesn't need
   SCM_BIGDIG conditionals */

int
scm_print_real (SCM sexp, SCM port, scm_print_state *pstate)
{
  char num_buf[SCM_FLOBUFLEN];
  scm_lfwrite (num_buf, iflo2str (sexp, num_buf), port);
  return !0;
}

int
scm_print_complex (SCM sexp, SCM port, scm_print_state *pstate)
{
  char num_buf[SCM_FLOBUFLEN];
  scm_lfwrite (num_buf, iflo2str (sexp, num_buf), port);
  return !0;
}

int
scm_bigprint (SCM exp, SCM port, scm_print_state *pstate)
{
#ifdef SCM_BIGDIG
  exp = big2str (exp, (unsigned int) 10);
  scm_lfwrite (SCM_STRING_CHARS (exp), (scm_sizet) SCM_STRING_LENGTH (exp), port);
#else
  scm_ipruk ("bignum", exp, port);
#endif
  return !0;
}
/*** END nums->strs ***/

/*** STRINGS -> NUMBERS ***/

static SCM
scm_small_istr2int (char *str, long len, long radix)
{
  register long n = 0, ln;
  register int c;
  register int i = 0;
  int lead_neg = 0;
  if (0 >= len)
    return SCM_BOOL_F;		/* zero scm_length */
  switch (*str)
    {				/* leading sign */
    case '-':
      lead_neg = 1;
    case '+':
      if (++i == len)
	return SCM_BOOL_F;	/* bad if lone `+' or `-' */
    }

  do
    {
      switch (c = str[i++])
	{
	case DIGITS:
	  c = c - '0';
	  goto accumulate;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	  c = c - 'A' + 10;
	  goto accumulate;
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	  c = c - 'a' + 10;
	accumulate:
	  if (c >= radix)
	    return SCM_BOOL_F;	/* bad digit for radix */
	  ln = n;
	  n = n * radix - c;
	  /* Negation is a workaround for HP700 cc bug */
	  if (n > ln || (-n > -SCM_MOST_NEGATIVE_FIXNUM))
	    goto ovfl;
	  break;
	default:
	  return SCM_BOOL_F;	/* not a digit */
	}
    }
  while (i < len);
  if (!lead_neg)
    if ((n = -n) > SCM_MOST_POSITIVE_FIXNUM)
      goto ovfl;
  return SCM_MAKINUM (n);
 ovfl:				/* overflow scheme integer */
  return SCM_BOOL_F;
}



SCM
scm_istr2int (char *str, long len, long radix)
{
  scm_sizet j;
  register scm_sizet k, blen = 1;
  scm_sizet i = 0;
  int c;
  SCM res;
  register SCM_BIGDIG *ds;
  register unsigned long t2;

  if (0 >= len)
    return SCM_BOOL_F;		/* zero scm_length */

  /* Short numbers we parse directly into an int, to avoid the overhead 
     of creating a bignum.  */
  if (len < 6)
    return scm_small_istr2int (str, len, radix);

  if (16 == radix)
    j = 1 + (4 * len * sizeof (char)) / (SCM_BITSPERDIG);
  else if (10 <= radix)
    j = 1 + (84 * len * sizeof (char)) / (SCM_BITSPERDIG * 25);
  else
    j = 1 + (len * sizeof (char)) / (SCM_BITSPERDIG);
  switch (str[0])
    {				/* leading sign */
    case '-':
    case '+':
      if (++i == (unsigned) len)
	return SCM_BOOL_F;	/* bad if lone `+' or `-' */
    }
  res = scm_mkbig (j, '-' == str[0]);
  ds = SCM_BDIGITS (res);
  for (k = j; k--;)
    ds[k] = 0;
  do
    {
      switch (c = str[i++])
	{
	case DIGITS:
	  c = c - '0';
	  goto accumulate;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	  c = c - 'A' + 10;
	  goto accumulate;
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	  c = c - 'a' + 10;
	accumulate:
	  if (c >= radix)
	    return SCM_BOOL_F;	/* bad digit for radix */
	  k = 0;
	  t2 = c;
	moretodo:
	  while (k < blen)
	    {
/* printf ("k = %d, blen = %d, t2 = %ld, ds[k] = %d\n", k, blen, t2, ds[k]); */
	      t2 += ds[k] * radix;
	      ds[k++] = SCM_BIGLO (t2);
	      t2 = SCM_BIGDN (t2);
	    }
	  if (blen > j)
	    scm_num_overflow ("bignum");
	  if (t2)
	    {
	      blen++;
	      goto moretodo;
	    }
	  break;
	default:
	  return SCM_BOOL_F;	/* not a digit */
	}
    }
  while (i < (unsigned) len);
  if (blen * SCM_BITSPERDIG / SCM_CHAR_BIT <= sizeof (SCM))
    if (SCM_INUMP (res = scm_big2inum (res, blen)))
      return res;
  if (j == blen)
    return res;
  return scm_adjbig (res, blen);
}

SCM
scm_istr2flo (char *str, long len, long radix)
{
  register int c, i = 0;
  double lead_sgn;
  double res = 0.0, tmp = 0.0;
  int flg = 0;
  int point = 0;
  SCM second;

  if (i >= len)
    return SCM_BOOL_F;		/* zero scm_length */

  switch (*str)
    {				/* leading sign */
    case '-':
      lead_sgn = -1.0;
      i++;
      break;
    case '+':
      lead_sgn = 1.0;
      i++;
      break;
    default:
      lead_sgn = 0.0;
    }
  if (i == len)
    return SCM_BOOL_F;		/* bad if lone `+' or `-' */

  if (str[i] == 'i' || str[i] == 'I')
    {				/* handle `+i' and `-i'   */
      if (lead_sgn == 0.0)
	return SCM_BOOL_F;	/* must have leading sign */
      if (++i < len)
	return SCM_BOOL_F;	/* `i' not last character */
      return scm_make_complex (0.0, lead_sgn);
    }
  do
    {				/* check initial digits */
      switch (c = str[i])
	{
	case DIGITS:
	  c = c - '0';
	  goto accum1;
	case 'D':
	case 'E':
	case 'F':
	  if (radix == 10)
	    goto out1;		/* must be exponent */
	case 'A':
	case 'B':
	case 'C':
	  c = c - 'A' + 10;
	  goto accum1;
	case 'd':
	case 'e':
	case 'f':
	  if (radix == 10)
	    goto out1;
	case 'a':
	case 'b':
	case 'c':
	  c = c - 'a' + 10;
	accum1:
	  if (c >= radix)
	    return SCM_BOOL_F;	/* bad digit for radix */
	  res = res * radix + c;
	  flg = 1;		/* res is valid */
	  break;
	default:
	  goto out1;
	}
    }
  while (++i < len);
 out1:

  /* if true, then we did see a digit above, and res is valid */
  if (i == len)
    goto done;

  /* By here, must have seen a digit,
     or must have next char be a `.' with radix==10 */
  if (!flg)
    if (!(str[i] == '.' && radix == 10))
      return SCM_BOOL_F;

  while (str[i] == '#')
    {				/* optional sharps */
      res *= radix;
      if (++i == len)
	goto done;
    }

  if (str[i] == '/')
    {
      while (++i < len)
	{
	  switch (c = str[i])
	    {
	    case DIGITS:
	      c = c - '0';
	      goto accum2;
	    case 'A':
	    case 'B':
	    case 'C':
	    case 'D':
	    case 'E':
	    case 'F':
	      c = c - 'A' + 10;
	      goto accum2;
	    case 'a':
	    case 'b':
	    case 'c':
	    case 'd':
	    case 'e':
	    case 'f':
	      c = c - 'a' + 10;
	    accum2:
	      if (c >= radix)
		return SCM_BOOL_F;
	      tmp = tmp * radix + c;
	      break;
	    default:
	      goto out2;
	    }
	}
    out2:
      if (tmp == 0.0)
	return SCM_BOOL_F;	/* `slash zero' not allowed */
      if (i < len)
	while (str[i] == '#')
	  {			/* optional sharps */
	    tmp *= radix;
	    if (++i == len)
	      break;
	  }
      res /= tmp;
      goto done;
    }

  if (str[i] == '.')
    {				/* decimal point notation */
      if (radix != 10)
	return SCM_BOOL_F;	/* must be radix 10 */
      while (++i < len)
	{
	  switch (c = str[i])
	    {
	    case DIGITS:
	      point--;
	      res = res * 10.0 + c - '0';
	      flg = 1;
	      break;
	    default:
	      goto out3;
	    }
	}
    out3:
      if (!flg)
	return SCM_BOOL_F;	/* no digits before or after decimal point */
      if (i == len)
	goto adjust;
      while (str[i] == '#')
	{			/* ignore remaining sharps */
	  if (++i == len)
	    goto adjust;
	}
    }

  switch (str[i])
    {				/* exponent */
    case 'd':
    case 'D':
    case 'e':
    case 'E':
    case 'f':
    case 'F':
    case 'l':
    case 'L':
    case 's':
    case 'S':
      {
	int expsgn = 1, expon = 0;
	if (radix != 10)
	  return SCM_BOOL_F;	/* only in radix 10 */
	if (++i == len)
	  return SCM_BOOL_F;	/* bad exponent */
	switch (str[i])
	  {
	  case '-':
	    expsgn = (-1);
	  case '+':
	    if (++i == len)
	      return SCM_BOOL_F;	/* bad exponent */
	  }
	if (str[i] < '0' || str[i] > '9')
	  return SCM_BOOL_F;	/* bad exponent */
	do
	  {
	    switch (c = str[i])
	      {
	      case DIGITS:
		expon = expon * 10 + c - '0';
		if (expon > SCM_MAXEXP)
		  scm_out_of_range ("string->number", SCM_MAKINUM (expon));
		break;
	      default:
		goto out4;
	      }
	  }
	while (++i < len);
      out4:
	point += expsgn * expon;
      }
    }

 adjust:
  if (point >= 0)
    while (point--)
      res *= 10.0;
  else
#ifdef _UNICOS
    while (point++)
      res *= 0.1;
#else
  while (point++)
    res /= 10.0;
#endif

 done:
  /* at this point, we have a legitimate floating point result */
  if (lead_sgn == -1.0)
    res = -res;
  if (i == len)
    return scm_make_real (res);

  if (str[i] == 'i' || str[i] == 'I')
    {				/* pure imaginary number  */
      if (lead_sgn == 0.0)
	return SCM_BOOL_F;	/* must have leading sign */
      if (++i < len)
	return SCM_BOOL_F;	/* `i' not last character */
      return scm_make_complex (0.0, res);
    }

  switch (str[i++])
    {
    case '-':
      lead_sgn = -1.0;
      break;
    case '+':
      lead_sgn = 1.0;
      break;
    case '@':
      {				/* polar input for complex number */
	/* get a `real' for scm_angle */
	second = scm_istr2flo (&str[i], (long) (len - i), radix);
	if (!SCM_SLOPPY_INEXACTP (second))
	  return SCM_BOOL_F;	/* not `real' */
	if (SCM_SLOPPY_COMPLEXP (second))
	  return SCM_BOOL_F;	/* not `real' */
	tmp = SCM_REAL_VALUE (second);
	return scm_make_complex (res * cos (tmp), res * sin (tmp));
      }
    default:
      return SCM_BOOL_F;
    }

  /* at this point, last char must be `i' */
  if (str[len - 1] != 'i' && str[len - 1] != 'I')
    return SCM_BOOL_F;
  /* handles `x+i' and `x-i' */
  if (i == (len - 1))
    return scm_make_complex (res, lead_sgn);
  /* get a `ureal' for complex part */
  second = scm_istr2flo (&str[i], (long) ((len - i) - 1), radix);
  if (!SCM_INEXACTP (second))
    return SCM_BOOL_F;		/* not `ureal' */
  if (SCM_SLOPPY_COMPLEXP (second))
    return SCM_BOOL_F;		/* not `ureal' */
  tmp = SCM_REAL_VALUE (second);
  if (tmp < 0.0)
    return SCM_BOOL_F;		/* not `ureal' */
  return scm_make_complex (res, (lead_sgn * tmp));
}



SCM
scm_istring2number (char *str, long len, long radix)
{
  int i = 0;
  char ex = 0;
  char ex_p = 0, rx_p = 0;	/* Only allow 1 exactness and 1 radix prefix */
  SCM res;
  if (len == 1)
    if (*str == '+' || *str == '-')    /* Catches lone `+' and `-' for speed */
      return SCM_BOOL_F;

  while ((len - i) >= 2 && str[i] == '#' && ++i)
    switch (str[i++])
      {
      case 'b':
      case 'B':
	if (rx_p++)
	  return SCM_BOOL_F;
	radix = 2;
	break;
      case 'o':
      case 'O':
	if (rx_p++)
	  return SCM_BOOL_F;
	radix = 8;
	break;
      case 'd':
      case 'D':
	if (rx_p++)
	  return SCM_BOOL_F;
	radix = 10;
	break;
      case 'x':
      case 'X':
	if (rx_p++)
	  return SCM_BOOL_F;
	radix = 16;
	break;
      case 'i':
      case 'I':
	if (ex_p++)
	  return SCM_BOOL_F;
	ex = 2;
	break;
      case 'e':
      case 'E':
	if (ex_p++)
	  return SCM_BOOL_F;
	ex = 1;
	break;
      default:
	return SCM_BOOL_F;
      }

  switch (ex)
    {
    case 1:
      return scm_istr2int (&str[i], len - i, radix);
    case 0:
      res = scm_istr2int (&str[i], len - i, radix);
      if (SCM_NFALSEP (res))
	return res;
    case 2:
      return scm_istr2flo (&str[i], len - i, radix);
    }
  return SCM_BOOL_F;
}


SCM_DEFINE (scm_string_to_number, "string->number", 1, 1, 0,
            (SCM string, SCM radix),
	    "Returns a number of the maximally precise representation\n"
	    "expressed by the given @var{string}. @var{radix} must be an\n"
	    "exact integer, either 2, 8, 10, or 16. If supplied, @var{RADIX}\n"
	    "is a default radix that may be overridden by an explicit\n"
	    "radix prefix in @var{string} (e.g. \"#o177\"). If @var{radix}\n"
	    "is not supplied, then the default radix is 10. If string is\n"
	    "not a syntactically valid notation for a number, then\n"
	    "@code{string->number} returns @code{#f}.  (r5rs)") 
#define FUNC_NAME s_scm_string_to_number
{
  SCM answer;
  int base;
  SCM_VALIDATE_STRING (1, string);
  SCM_VALIDATE_INUM_MIN_DEF_COPY (2,radix,2,10,base);
  answer = scm_istring2number (SCM_STRING_CHARS (string),
			       SCM_STRING_LENGTH (string),
                               base);
  return scm_return_first (answer, string);
}
#undef FUNC_NAME
/*** END strs->nums ***/


SCM
scm_make_real (double x)
{
  SCM z;
  SCM_NEWCELL2 (z);
  SCM_SET_CELL_TYPE (z, scm_tc16_real);
  SCM_REAL_VALUE (z) = x;
  return z;
}


SCM
scm_make_complex (double x, double y)
{
  if (y == 0.0) {
    return scm_make_real (x);
  } else {
    SCM z;
    SCM_NEWSMOB (z, scm_tc16_complex, scm_must_malloc (2L * sizeof (double), "complex"));
    SCM_COMPLEX_REAL (z) = x;
    SCM_COMPLEX_IMAG (z) = y;
    return z;
  }
}


SCM
scm_bigequal (SCM x, SCM y)
{
#ifdef SCM_BIGDIG
  if (0 == scm_bigcomp (x, y))
    return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}

SCM
scm_real_equalp (SCM x, SCM y)
{
  return SCM_BOOL (SCM_REAL_VALUE (x) == SCM_REAL_VALUE (y));
}

SCM
scm_complex_equalp (SCM x, SCM y)
{
  return SCM_BOOL (SCM_COMPLEX_REAL (x) == SCM_COMPLEX_REAL (y)
		   && SCM_COMPLEX_IMAG (x) == SCM_COMPLEX_IMAG (y));
}



SCM_REGISTER_PROC (s_number_p, "number?", 1, 0, 0, scm_number_p);
/* "Return @code{#t} if @var{x} is a number, @code{#f}\n"
 * "else.  Note that the sets of complex, real, rational and\n"
 * "integer values form subsets of the set of numbers, i. e. the\n"
 * "predicate will be fulfilled for any number."
 */
SCM_DEFINE (scm_number_p, "complex?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a complex number, @code{#f}\n"
	    "else.  Note that the sets of real, rational and integer\n"
	    "values form subsets of the set of complex numbers, i. e. the\n"
	    "predicate will also be fulfilled if @var{x} is a real,\n"
	    "rational or integer number.")
#define FUNC_NAME s_scm_number_p
{
  return SCM_BOOL (SCM_NUMBERP (x));
}
#undef FUNC_NAME


SCM_REGISTER_PROC (s_real_p, "real?", 1, 0, 0, scm_real_p);
/* "Return @code{#t} if @var{x} is a real number, @code{#f} else.\n"
 * "Note that the sets of integer and rational values form a subset\n"
 * "of the set of real numbers, i. e. the predicate will also\n"
 * "be fulfilled if @var{x} is an integer or a rational number."
 */
SCM_DEFINE (scm_real_p, "rational?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a rational number, @code{#f}\n"
	    "else.  Note that the set of integer values forms a subset of\n"
	    "the set of rational numbers, i. e. the predicate will also be\n"
	    "fulfilled if @var{x} is an integer number.  Real numbers\n"
	    "will also satisfy this predicate, because of their limited\n"
	    "precision.")
#define FUNC_NAME s_scm_real_p
{
  if (SCM_INUMP (x)) {
    return SCM_BOOL_T;
  } else if (SCM_IMP (x)) {
    return SCM_BOOL_F;
  } else if (SCM_SLOPPY_REALP (x)) {
    return SCM_BOOL_T;
  } else if (SCM_BIGP (x)) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_integer_p, "integer?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is an integer number, @code{#f}\n"
	    "else.")
#define FUNC_NAME s_scm_integer_p
{
  double r;
  if (SCM_INUMP (x))
    return SCM_BOOL_T;
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_BIGP (x))
    return SCM_BOOL_T;
  if (!SCM_SLOPPY_INEXACTP (x))
    return SCM_BOOL_F;
  if (SCM_SLOPPY_COMPLEXP (x))
    return SCM_BOOL_F;
  r = SCM_REAL_VALUE (x);
  if (r == floor (r))
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_inexact_p, "inexact?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is an inexact number, @code{#f}\n"
	    "else.")
#define FUNC_NAME s_scm_inexact_p
{
  return SCM_BOOL (SCM_INEXACTP (x));
}
#undef FUNC_NAME


SCM_GPROC1 (s_eq_p, "=", scm_tc7_rpsubr, scm_num_eq_p, g_eq_p);
/* "Return @code{#t} if all parameters are numerically equal."  */
SCM
scm_num_eq_p (SCM x, SCM y)
{
  if (SCM_INUMP (x)) {
    long xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      return SCM_BOOL (xx == yy);
    } else if (SCM_BIGP (y)) {
      return SCM_BOOL_F;
    } else if (SCM_REALP (y)) {
      return SCM_BOOL ((double) xx == SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return SCM_BOOL (((double) xx == SCM_COMPLEX_REAL (y))
		       && (0.0 == SCM_COMPLEX_IMAG (y)));
    } else {
      SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      return SCM_BOOL_F;
    } else if (SCM_BIGP (y)) {
      return SCM_BOOL (0 == scm_bigcomp (x, y));
    } else if (SCM_REALP (y)) {
      return SCM_BOOL (scm_big2dbl (x) == SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return SCM_BOOL ((scm_big2dbl (x) == SCM_COMPLEX_REAL (y))
		       && (0.0 == SCM_COMPLEX_IMAG (y)));
    } else {
      SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  } else if (SCM_REALP (x)) {
    if (SCM_INUMP (y)) {
      return SCM_BOOL (SCM_REAL_VALUE (x) == (double) SCM_INUM (y));
    } else if (SCM_BIGP (y)) {
      return SCM_BOOL (SCM_REAL_VALUE (x) == scm_big2dbl (y));
    } else if (SCM_REALP (y)) {
      return SCM_BOOL (SCM_REAL_VALUE (x) == SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return SCM_BOOL ((SCM_REAL_VALUE (x) == SCM_COMPLEX_REAL (y))
		       && (0.0 == SCM_COMPLEX_IMAG (y)));
    } else {
      SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  } else if (SCM_COMPLEXP (x)) {
    if (SCM_INUMP (y)) {
      return SCM_BOOL ((SCM_COMPLEX_REAL (x) == (double) SCM_INUM (y))
		       && (SCM_COMPLEX_IMAG (x) == 0.0));
    } else if (SCM_BIGP (y)) {
      return SCM_BOOL ((SCM_COMPLEX_REAL (x) == scm_big2dbl (y))
		       && (SCM_COMPLEX_IMAG (x) == 0.0));
    } else if (SCM_REALP (y)) {
      return SCM_BOOL ((SCM_COMPLEX_REAL (x) == SCM_REAL_VALUE (y))
		       && (SCM_COMPLEX_IMAG (x) == 0.0));
    } else if (SCM_COMPLEXP (y)) {
      return SCM_BOOL ((SCM_COMPLEX_REAL (x) == SCM_COMPLEX_REAL (y))
		       && (SCM_COMPLEX_IMAG (x) == SCM_COMPLEX_IMAG (y)));
    } else {
      SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARG1, s_eq_p);
  }
}


SCM_GPROC1 (s_less_p, "<", scm_tc7_rpsubr, scm_less_p, g_less_p);
/* "Return @code{#t} if the list of parameters is monotonically\n"
 * "increasing."
 */
SCM
scm_less_p (SCM x, SCM y)
{
  if (SCM_INUMP (x)) {
    long xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      return SCM_BOOL (xx < yy);
    } else if (SCM_BIGP (y)) {
      return SCM_BOOL (!SCM_BIGSIGN (y));
    } else if (SCM_REALP (y)) {
      return SCM_BOOL ((double) xx < SCM_REAL_VALUE (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARGn, s_less_p);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      return SCM_BOOL (SCM_BIGSIGN (x));
    } else if (SCM_BIGP (y)) {
      return SCM_BOOL (1 == scm_bigcomp (x, y));
    } else if (SCM_REALP (y)) {
      return SCM_BOOL (scm_big2dbl (x) < SCM_REAL_VALUE (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARGn, s_less_p);
    }
  } else if (SCM_REALP (x)) {
    if (SCM_INUMP (y)) {
      return SCM_BOOL (SCM_REAL_VALUE (x) < (double) SCM_INUM (y));
    } else if (SCM_BIGP (y)) {
      return SCM_BOOL (SCM_REAL_VALUE (x) < scm_big2dbl (y));
    } else if (SCM_REALP (y)) {
      return SCM_BOOL (SCM_REAL_VALUE (x) < SCM_REAL_VALUE (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARGn, s_less_p);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARG1, s_less_p);
  }
}


SCM_GPROC1 (s_scm_gr_p, ">", scm_tc7_rpsubr, scm_gr_p, g_gr_p);
/* "Return @code{#t} if the list of parameters is monotonically\n"
 * "decreasing."
 */
#define FUNC_NAME s_scm_gr_p
SCM
scm_gr_p (SCM x, SCM y)
{
  if (!SCM_NUMBERP (x))
    SCM_WTA_DISPATCH_2 (g_gr_p, x, y, SCM_ARG1, FUNC_NAME);
  else if (!SCM_NUMBERP (y))
    SCM_WTA_DISPATCH_2 (g_gr_p, x, y, SCM_ARG2, FUNC_NAME);
  else
    return scm_less_p (y, x);
}
#undef FUNC_NAME


SCM_GPROC1 (s_scm_leq_p, "<=", scm_tc7_rpsubr, scm_leq_p, g_leq_p);
/* "Return @code{#t} if the list of parameters is monotonically\n"
 * "non-decreasing."
 */
#define FUNC_NAME s_scm_leq_p
SCM
scm_leq_p (SCM x, SCM y)
{
  if (!SCM_NUMBERP (x))
    SCM_WTA_DISPATCH_2 (g_leq_p, x, y, SCM_ARG1, FUNC_NAME);
  else if (!SCM_NUMBERP (y))
    SCM_WTA_DISPATCH_2 (g_leq_p, x, y, SCM_ARG2, FUNC_NAME);
  else
    return SCM_BOOL_NOT (scm_less_p (y, x));
}
#undef FUNC_NAME


SCM_GPROC1 (s_scm_geq_p, ">=", scm_tc7_rpsubr, scm_geq_p, g_geq_p);
/* "Return @code{#t} if the list of parameters is monotonically\n"
 * "non-increasing."
 */
#define FUNC_NAME s_scm_geq_p
SCM
scm_geq_p (SCM x, SCM y)
{
  if (!SCM_NUMBERP (x))
    SCM_WTA_DISPATCH_2 (g_geq_p, x, y, SCM_ARG1, FUNC_NAME);
  else if (!SCM_NUMBERP (y))
    SCM_WTA_DISPATCH_2 (g_geq_p, x, y, SCM_ARG2, FUNC_NAME);
  else
  return SCM_BOOL_NOT (scm_less_p (x, y));
}
#undef FUNC_NAME


SCM_GPROC (s_zero_p, "zero?", 1, 0, 0, scm_zero_p, g_zero_p);
/* "Return @code{#t} if @var{z} is an exact or inexact number equal to\n"
 * "zero."
 */
SCM
scm_zero_p (SCM z)
{
  if (SCM_INUMP (z)) {
    return SCM_BOOL (SCM_EQ_P (z, SCM_INUM0));
  } else if (SCM_BIGP (z)) {
    return SCM_BOOL_F;
  } else if (SCM_REALP (z)) {
    return SCM_BOOL (SCM_REAL_VALUE (z) == 0.0);
  } else if (SCM_COMPLEXP (z)) {
    return SCM_BOOL (SCM_COMPLEX_REAL (z) == 0.0
		     && SCM_COMPLEX_IMAG (z) == 0.0);
  } else {
    SCM_WTA_DISPATCH_1 (g_zero_p, z, SCM_ARG1, s_zero_p);
  }
}


SCM_GPROC (s_positive_p, "positive?", 1, 0, 0, scm_positive_p, g_positive_p);
/* "Return @code{#t} if @var{x} is an exact or inexact number greater than\n"
 * "zero."
 */
SCM
scm_positive_p (SCM x)
{
  if (SCM_INUMP (x)) {
    return SCM_BOOL (SCM_INUM (x) > 0);
  } else if (SCM_BIGP (x)) {
    return SCM_BOOL (!SCM_BIGSIGN (x));
  } else if (SCM_REALP (x)) {
    return SCM_BOOL(SCM_REAL_VALUE (x) > 0.0);
  } else {
    SCM_WTA_DISPATCH_1 (g_positive_p, x, SCM_ARG1, s_positive_p);
  }
}


SCM_GPROC (s_negative_p, "negative?", 1, 0, 0, scm_negative_p, g_negative_p);
/* "Return @code{#t} if @var{x} is an exact or inexact number less than\n"
 * "zero."
 */
SCM
scm_negative_p (SCM x)
{
  if (SCM_INUMP (x)) {
    return SCM_BOOL (SCM_INUM (x) < 0);
  } else if (SCM_BIGP (x)) {
    return SCM_BOOL (SCM_BIGSIGN (x));
  } else if (SCM_REALP (x)) {
    return SCM_BOOL(SCM_REAL_VALUE (x) < 0.0);
  } else {
    SCM_WTA_DISPATCH_1 (g_negative_p, x, SCM_ARG1, s_negative_p);
  }
}


SCM_GPROC1 (s_max, "max", scm_tc7_asubr, scm_max, g_max);
/* "Return the maximum of all parameter values."
 */
SCM
scm_max (SCM x, SCM y)
{
  if (SCM_UNBNDP (y)) {
    if (SCM_UNBNDP (x)) {
      SCM_WTA_DISPATCH_0 (g_max, x, SCM_ARG1, s_max);
    } else if (SCM_NUMBERP (x)) {
      return x;
    } else {
      SCM_WTA_DISPATCH_1 (g_max, x, SCM_ARG1, s_max);
    }
  }
  
  if (SCM_INUMP (x)) {
    long xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      return (xx < yy) ? y : x;
    } else if (SCM_BIGP (y)) {
      return SCM_BIGSIGN (y) ? x : y;
    } else if (SCM_REALP (y)) {
      double z = xx;
      return (z <= SCM_REAL_VALUE (y)) ? y : scm_make_real (z);
    } else {
      SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      return SCM_BIGSIGN (x) ? y : x;
    } else if (SCM_BIGP (y)) {
      return (1 == scm_bigcomp (x, y)) ? y : x;
    } else if (SCM_REALP (y)) {
      double z = scm_big2dbl (x);
      return (z <= SCM_REAL_VALUE (y)) ? y : scm_make_real (z);
    } else {
      SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  } else if (SCM_REALP (x)) {
    if (SCM_INUMP (y)) {
      double z = SCM_INUM (y);
      return (SCM_REAL_VALUE (x) < z) ? scm_make_real (z) : x;
    } else if (SCM_BIGP (y)) {
      double z = scm_big2dbl (y);
      return (SCM_REAL_VALUE (x) < z) ? scm_make_real (z) : x;
    } else if (SCM_REALP (y)) {
      return (SCM_REAL_VALUE (x) < SCM_REAL_VALUE (y)) ? y : x;
    } else {
      SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARG1, s_max);
  }
}


SCM_GPROC1 (s_min, "min", scm_tc7_asubr, scm_min, g_min);
/* "Return the minium of all parameter values."
 */
SCM
scm_min (SCM x, SCM y)
{
  if (SCM_UNBNDP (y)) {
    if (SCM_UNBNDP (x)) {
      SCM_WTA_DISPATCH_0 (g_min, x, SCM_ARG1, s_min);
    } else if (SCM_NUMBERP (x)) {
      return x;
    } else {
      SCM_WTA_DISPATCH_1 (g_min, x, SCM_ARG1, s_min);
    }
  }
  
  if (SCM_INUMP (x)) {
    long xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      return (xx < yy) ? x : y;
    } else if (SCM_BIGP (y)) {
      return SCM_BIGSIGN (y) ? y : x;
    } else if (SCM_REALP (y)) {
      double z = xx;
      return (z < SCM_REAL_VALUE (y)) ? scm_make_real (z) : y;
    } else {
      SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      return SCM_BIGSIGN (x) ? x : y;
    } else if (SCM_BIGP (y)) {
      return (-1 == scm_bigcomp (x, y)) ? y : x;
    } else if (SCM_REALP (y)) {
      double z = scm_big2dbl (x);
      return (z < SCM_REAL_VALUE (y)) ? scm_make_real (z) : y;
    } else {
      SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  } else if (SCM_REALP (x)) {
    if (SCM_INUMP (y)) {
      double z = SCM_INUM (y);
      return (SCM_REAL_VALUE (x) <= z) ? x : scm_make_real (z);
    } else if (SCM_BIGP (y)) {
      double z = scm_big2dbl (y);
      return (SCM_REAL_VALUE (x) <= z) ? x : scm_make_real (z);
    } else if (SCM_REALP (y)) {
      return (SCM_REAL_VALUE (x) < SCM_REAL_VALUE (y)) ? x : y;
    } else {
      SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARG1, s_min);
  }
}


SCM_GPROC1 (s_sum, "+", scm_tc7_asubr, scm_sum, g_sum);
/* "Return the sum of all parameter values.  Return 0 if called without\n"
 * "any parameters." 
 */
SCM
scm_sum (SCM x, SCM y)
{
  if (SCM_UNBNDP (y)) {
    if (SCM_UNBNDP (x)) {
      return SCM_INUM0;
    } else if (SCM_NUMBERP (x)) {
      return x;
    } else {
      SCM_WTA_DISPATCH_1 (g_sum, x, SCM_ARG1, s_sum);
    }
  }

  if (SCM_INUMP (x)) {
    long int xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long int yy = SCM_INUM (y);
      long int z = xx + yy;
      if (SCM_FIXABLE (z)) {
	return SCM_MAKINUM (z);
      } else {
#ifdef SCM_BIGDIG
	return scm_long2big (z);
#else  /* SCM_BIGDIG */
	return scm_make_real ((double) z);
#endif /* SCM_BIGDIG */ 
      }
    } else if (SCM_BIGP (y)) {
    intbig:
      {
	long int xx = SCM_INUM (x);
#ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong (xx);
	return scm_addbig ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG,
			   (xx < 0) ? SCM_BIGSIGNFLAG : 0, y, 0);
#else  /* SCM_DIGSTOOBIG */
	SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
	scm_longdigs (xx, zdigs);
	return scm_addbig (zdigs, SCM_DIGSPERLONG, 
			   (xx < 0) ? SCM_BIGSIGNFLAG : 0, y, 0);
#endif /* SCM_DIGSTOOBIG */
      }
    } else if (SCM_REALP (y)) {
      return scm_make_real (xx + SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (xx + SCM_COMPLEX_REAL (y),
			       SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      SCM_SWAP (x, y);
      goto intbig;
    } else if (SCM_BIGP (y)) {
      if (SCM_NUMDIGS (x) > SCM_NUMDIGS (y)) {
	SCM_SWAP (x, y);
      }
      return scm_addbig (SCM_BDIGITS (x), SCM_NUMDIGS (x), 
			 SCM_BIGSIGN (x), y, 0);
    } else if (SCM_REALP (y)) {
      return scm_make_real (scm_big2dbl (x) + SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (scm_big2dbl (x) + SCM_COMPLEX_REAL (y),
			       SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  } else if (SCM_REALP (x)) {
    if (SCM_INUMP (y)) {
      return scm_make_real (SCM_REAL_VALUE (x) + SCM_INUM (y));
    } else if (SCM_BIGP (y)) {
      return scm_make_real (SCM_REAL_VALUE (x) + scm_big2dbl (y));
    } else if (SCM_REALP (y)) {
      return scm_make_real (SCM_REAL_VALUE (x) + SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (SCM_REAL_VALUE (x) + SCM_COMPLEX_REAL (y),
			       SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  } else if (SCM_COMPLEXP (x)) {
    if (SCM_INUMP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) + SCM_INUM (y),
			       SCM_COMPLEX_IMAG (x));
    } else if (SCM_BIGP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) + scm_big2dbl (y),
			       SCM_COMPLEX_IMAG (x));
    } else if (SCM_REALP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) + SCM_REAL_VALUE (y),
			       SCM_COMPLEX_IMAG (x));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) + SCM_COMPLEX_REAL (y),
			       SCM_COMPLEX_IMAG (x) + SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARG1, s_sum);
  }
}


SCM_GPROC1 (s_difference, "-", scm_tc7_asubr, scm_difference, g_difference);
/* "If called without arguments, 0 is returned. Otherwise the sum of\n"
 * "all but the first argument are subtracted from the first\n"
 * "argument."
 */
SCM
scm_difference (SCM x, SCM y)
{
  if (SCM_UNBNDP (y)) {
    if (SCM_INUMP (x)) {
      long xx = -SCM_INUM (x);
      if (SCM_FIXABLE (xx)) {
	return SCM_MAKINUM (xx);
      } else {
#ifdef SCM_BIGDIG
	return scm_long2big (xx);
#else
	return scm_make_real ((double) xx);
#endif
      }
    } else if (SCM_BIGP (x)) {
      SCM z = scm_copybig (x, !SCM_BIGSIGN (x));
      unsigned int digs = SCM_NUMDIGS (z);
      unsigned int size = digs * SCM_BITSPERDIG / SCM_CHAR_BIT;
      return size <= sizeof (SCM) ? scm_big2inum (z, digs) : z;
    } else if (SCM_REALP (x)) {
      return scm_make_real (-SCM_REAL_VALUE (x));
    } else if (SCM_COMPLEXP (x)) {
      return scm_make_complex (-SCM_COMPLEX_REAL (x), -SCM_COMPLEX_IMAG (x));
    } else {
      SCM_WTA_DISPATCH_1 (g_difference, x, SCM_ARG1, s_difference);
    }
  }

  if (SCM_INUMP (x)) {
    long int xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long int yy = SCM_INUM (y);
      long int z = xx - yy;
      if (SCM_FIXABLE (z)) {
	return SCM_MAKINUM (z);
      } else {
#ifdef SCM_BIGDIG
	return scm_long2big (z);
#else
	return scm_make_real ((double) z);
#endif
      }
    } else if (SCM_BIGP (y)) {
#ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong (xx);
      return scm_addbig ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG,
			 (xx < 0) ? SCM_BIGSIGNFLAG : 0, y, SCM_BIGSIGNFLAG);
#else
      SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
      scm_longdigs (xx, zdigs);
      return scm_addbig (zdigs, SCM_DIGSPERLONG, 
			 (xx < 0) ? SCM_BIGSIGNFLAG : 0, y, SCM_BIGSIGNFLAG);
#endif
    } else if (SCM_REALP (y)) {
      return scm_make_real (xx - SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (xx - SCM_COMPLEX_REAL (y),
			       -SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      long int yy = SCM_INUM (y);
#ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong (yy);
      return scm_addbig ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG,
			 (yy < 0) ? 0 : SCM_BIGSIGNFLAG, x, 0);
#else
      SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
      scm_longdigs (yy, zdigs);
      return scm_addbig (zdigs, SCM_DIGSPERLONG, 
			 (yy < 0) ? 0 : SCM_BIGSIGNFLAG, x, 0);
#endif
    } else if (SCM_BIGP (y)) {
      return (SCM_NUMDIGS (x) < SCM_NUMDIGS (y))
	? scm_addbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
		      SCM_BIGSIGN (x), y, SCM_BIGSIGNFLAG)
	: scm_addbig (SCM_BDIGITS (y), SCM_NUMDIGS (y),
		      SCM_BIGSIGN (y) ^ SCM_BIGSIGNFLAG, x, 0);
    } else if (SCM_REALP (y)) {
      return scm_make_real (scm_big2dbl (x) - SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (scm_big2dbl (x) - SCM_COMPLEX_REAL (y),
			       - SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  } else if (SCM_REALP (x)) {
    if (SCM_INUMP (y)) {
      return scm_make_real (SCM_REAL_VALUE (x) - SCM_INUM (y));
    } else if (SCM_BIGP (y)) {
      return scm_make_real (SCM_REAL_VALUE (x) - scm_big2dbl (y));
    } else if (SCM_REALP (y)) {
      return scm_make_real (SCM_REAL_VALUE (x) - SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (SCM_REAL_VALUE (x) - SCM_COMPLEX_REAL (y),
			       -SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  } else if (SCM_COMPLEXP (x)) {
    if (SCM_INUMP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) - SCM_INUM (y),
			       SCM_COMPLEX_IMAG (x));
    } else if (SCM_BIGP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) - scm_big2dbl (y),
			       SCM_COMPLEX_IMAG (x));
    } else if (SCM_REALP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) - SCM_REAL_VALUE (y),
			       SCM_COMPLEX_IMAG (x));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) - SCM_COMPLEX_REAL (y),
			       SCM_COMPLEX_IMAG (x) - SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARG1, s_difference);
  }
}


SCM_GPROC1 (s_product, "*", scm_tc7_asubr, scm_product, g_product);
/* "Return the product of all arguments.  If called without arguments,\n"
 * "1 is returned."
 */
SCM
scm_product (SCM x, SCM y)
{
  if (SCM_UNBNDP (y)) {
    if (SCM_UNBNDP (x)) {
      return SCM_MAKINUM (1L);
    } else if (SCM_NUMBERP (x)) {
      return x;
    } else {
      SCM_WTA_DISPATCH_1 (g_product, x, SCM_ARG1, s_product);
    }
  }

  if (SCM_INUMP (x)) {
    long xx;

  intbig:
    xx = SCM_INUM (x);

    if (xx == 0) {
      return x;
    } else if (xx == 1) {
      return y;
    }

    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      long kk = xx * yy;
      SCM k = SCM_MAKINUM (kk);
      if (kk != SCM_INUM (k) || kk / xx != yy) {
#ifdef SCM_BIGDIG
	int sgn = (xx < 0) ^ (yy < 0);
#ifndef SCM_DIGSTOOBIG
	long i = scm_pseudolong (xx);
	long j = scm_pseudolong (yy);
	return scm_mulbig ((SCM_BIGDIG *) & i, SCM_DIGSPERLONG,
			   (SCM_BIGDIG *) & j, SCM_DIGSPERLONG, sgn);
#else /* SCM_DIGSTOOBIG */
	SCM_BIGDIG xdigs [SCM_DIGSPERLONG];
	SCM_BIGDIG ydigs [SCM_DIGSPERLONG];
	scm_longdigs (xx, xdigs);
	scm_longdigs (yy, ydigs);
	return scm_mulbig (xdigs, SCM_DIGSPERLONG,
			   ydigs, SCM_DIGSPERLONG,
			   sgn);
#endif
#else
	return scm_make_real (((double) xx) * ((double) yy));
#endif
      } else {
	return k;
      }
    } else if (SCM_BIGP (y)) {
#ifndef SCM_DIGSTOOBIG
      long z = scm_pseudolong (xx);
      return scm_mulbig ((SCM_BIGDIG *) & z, SCM_DIGSPERLONG,
			 SCM_BDIGITS (y), SCM_NUMDIGS (y),
			 SCM_BIGSIGN (y) ? (xx > 0) : (xx < 0));
#else
      SCM_BIGDIG zdigs [SCM_DIGSPERLONG];
      scm_longdigs (xx, zdigs);
      return scm_mulbig (zdigs, SCM_DIGSPERLONG,
			 SCM_BDIGITS (y), SCM_NUMDIGS (y),
			 SCM_BIGSIGN (y) ? (xx > 0) : (xx < 0));
#endif
    } else if (SCM_REALP (y)) {
      return scm_make_real (xx * SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (xx * SCM_COMPLEX_REAL (y),
			       xx * SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      SCM_SWAP (x, y);
      goto intbig;
    } else if (SCM_BIGP (y)) {
      return scm_mulbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			 SCM_BDIGITS (y), SCM_NUMDIGS (y),
			 SCM_BIGSIGN (x) ^ SCM_BIGSIGN (y));
    } else if (SCM_REALP (y)) {
      return scm_make_real (scm_big2dbl (x) * SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      double z = scm_big2dbl (x);
      return scm_make_complex (z * SCM_COMPLEX_REAL (y),
			       z * SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  } else if (SCM_REALP (x)) {
    if (SCM_INUMP (y)) {
      return scm_make_real (SCM_INUM (y) * SCM_REAL_VALUE (x));
    } else if (SCM_BIGP (y)) {
      return scm_make_real (scm_big2dbl (y) * SCM_REAL_VALUE (x));
    } else if (SCM_REALP (y)) {
      return scm_make_real (SCM_REAL_VALUE (x) * SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (SCM_REAL_VALUE (x) * SCM_COMPLEX_REAL (y),
			       SCM_REAL_VALUE (x) * SCM_COMPLEX_IMAG (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  } else if (SCM_COMPLEXP (x)) {
    if (SCM_INUMP (y)) {
      return scm_make_complex (SCM_INUM (y) * SCM_COMPLEX_REAL (x),
			       SCM_INUM (y) * SCM_COMPLEX_IMAG (x));
    } else if (SCM_BIGP (y)) {
      double z = scm_big2dbl (y);
      return scm_make_complex (z * SCM_COMPLEX_REAL (x),
			       z * SCM_COMPLEX_IMAG (x));
    } else if (SCM_REALP (y)) {
      return scm_make_complex (SCM_REAL_VALUE (y) * SCM_COMPLEX_REAL (x),
			       SCM_REAL_VALUE (y) * SCM_COMPLEX_IMAG (x));
    } else if (SCM_COMPLEXP (y)) {
      return scm_make_complex (SCM_COMPLEX_REAL (x) * SCM_COMPLEX_REAL (y)
			       - SCM_COMPLEX_IMAG (x) * SCM_COMPLEX_IMAG (y),
			       SCM_COMPLEX_REAL (x) * SCM_COMPLEX_IMAG (y)
			       + SCM_COMPLEX_IMAG (x) * SCM_COMPLEX_REAL (y));
    } else {
      SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARG1, s_product);
  }
}


double
scm_num2dbl (SCM a, const char *why)
#define FUNC_NAME why
{
  if (SCM_INUMP (a)) {
    return (double) SCM_INUM (a);
  } else if (SCM_BIGP (a)) {
    return scm_big2dbl (a);
  } else if (SCM_REALP (a)) {
    return (SCM_REAL_VALUE (a));
  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARGn, a);
  }
}
#undef FUNC_NAME


SCM_GPROC1 (s_divide, "/", scm_tc7_asubr, scm_divide, g_divide);
/* "Divide the first argument by the product of the remaining arguments."
 */
SCM
scm_divide (SCM x, SCM y)
{
  double a;

  if (SCM_UNBNDP (y)) {
    if (SCM_UNBNDP (x)) {
      SCM_WTA_DISPATCH_0 (g_divide, x, SCM_ARG1, s_divide);
    } else if (SCM_INUMP (x)) {
      if (SCM_EQ_P (x, SCM_MAKINUM (1L)) || SCM_EQ_P (x, SCM_MAKINUM (-1L))) {
	return x;
      } else {
	return scm_make_real (1.0 / (double) SCM_INUM (x));
      }
    } else if (SCM_BIGP (x)) {
      return scm_make_real (1.0 / scm_big2dbl (x));
    } else if (SCM_REALP (x)) {
      return scm_make_real (1.0 / SCM_REAL_VALUE (x));
    } else if (SCM_COMPLEXP (x)) {
      double r = SCM_COMPLEX_REAL (x);
      double i = SCM_COMPLEX_IMAG (x);
      double d = r * r + i * i;
      return scm_make_complex (r / d, -i / d);
    } else {
      SCM_WTA_DISPATCH_1 (g_divide, x, SCM_ARG1, s_divide);
    }
  }

  if (SCM_INUMP (x)) {
    long xx = SCM_INUM (x);
    if (SCM_INUMP (y)) {
      long yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_divide);
      } else if (xx % yy != 0) {
	return scm_make_real ((double) xx / (double) yy);
      } else {
	long z = xx / yy;
	if (SCM_FIXABLE (z)) {
	  return SCM_MAKINUM (z);
	} else {
#ifdef SCM_BIGDIG
	  return scm_long2big (z);
#else
	  return scm_make_real ((double) xx / (double) yy);
#endif
	}
      }
    } else if (SCM_BIGP (y)) {
      return scm_make_real ((double) xx / scm_big2dbl (y));
    } else if (SCM_REALP (y)) {
      return scm_make_real ((double) xx / SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      a = xx;
    complex_div: /* y _must_ be a complex number */
      {
	double r = SCM_COMPLEX_REAL (y);
	double i = SCM_COMPLEX_IMAG (y);
	double d = r * r + i * i;
	return scm_make_complex ((a * r) / d, (-a * i) / d);
      }
    } else {
      SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  } else if (SCM_BIGP (x)) {
    if (SCM_INUMP (y)) {
      long int yy = SCM_INUM (y);
      if (yy == 0) {
	scm_num_overflow (s_divide);
      } else if (yy == 1) {
	return x;
      } else {
	long z = yy < 0 ? -yy : yy;
	if (z < SCM_BIGRAD) {
	  SCM w = scm_copybig (x, SCM_BIGSIGN (x) ? (yy > 0) : (yy < 0));
	  return scm_divbigdig (SCM_BDIGITS (w), SCM_NUMDIGS (w),
				(SCM_BIGDIG) z)
	    ? scm_make_real (scm_big2dbl (x) / (double) yy)
	    : scm_normbig (w);
	} else {
	  SCM w;
#ifndef SCM_DIGSTOOBIG
	  z = scm_pseudolong (z);
	  w = scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			     (SCM_BIGDIG *) & z, SCM_DIGSPERLONG,
			     SCM_BIGSIGN (x) ? (yy > 0) : (yy < 0), 3);
#else
	  SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	  scm_longdigs (z, zdigs);
	  w = scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			     zdigs, SCM_DIGSPERLONG,
			     SCM_BIGSIGN (x) ? (yy > 0) : (yy < 0), 3);
#endif
	  return (!SCM_UNBNDP (w)) 
	    ? w 
	    : scm_make_real (scm_big2dbl (x) / (double) yy);
	}
      }
    } else if (SCM_BIGP (y)) {
      SCM w = scm_divbigbig (SCM_BDIGITS (x), SCM_NUMDIGS (x),
			     SCM_BDIGITS (y), SCM_NUMDIGS (y),
			     SCM_BIGSIGN (x) ^ SCM_BIGSIGN (y), 3);
      return (!SCM_UNBNDP (w)) 
	? w 
	: scm_make_real (scm_big2dbl (x) / scm_big2dbl (y));
    } else if (SCM_REALP (y)) {
      return scm_make_real (scm_big2dbl (x) / SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      a = scm_big2dbl (x);
      goto complex_div;
    } else {
      SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  } else if (SCM_REALP (x)) {
    double rx = SCM_REAL_VALUE (x);
    if (SCM_INUMP (y)) {
      return scm_make_real (rx / (double) SCM_INUM (y));
    } else if (SCM_BIGP (y)) {
      return scm_make_real (rx / scm_big2dbl (y));
    } else if (SCM_REALP (y)) {
      return scm_make_real (rx / SCM_REAL_VALUE (y));
    } else if (SCM_COMPLEXP (y)) {
      a = rx;
      goto complex_div;
    } else {
      SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  } else if (SCM_COMPLEXP (x)) {
    double rx = SCM_COMPLEX_REAL (x);
    double ix = SCM_COMPLEX_IMAG (x);
    if (SCM_INUMP (y)) {
      double d = SCM_INUM (y);
      return scm_make_complex (rx / d, ix / d);
    } else if (SCM_BIGP (y)) {
      double d = scm_big2dbl (y);
      return scm_make_complex (rx / d, ix / d);
    } else if (SCM_REALP (y)) {
      double d = SCM_REAL_VALUE (y);
      return scm_make_complex (rx / d, ix / d);
    } else if (SCM_COMPLEXP (y)) {
      double ry = SCM_COMPLEX_REAL (y);
      double iy = SCM_COMPLEX_IMAG (y);
      double d = ry * ry + iy * iy;
      return scm_make_complex ((rx * ry + ix * iy) / d, 
			       (ix * ry - rx * iy) / d);
    } else {
      SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  } else {
    SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARG1, s_divide);
  }
}


SCM_GPROC1 (s_asinh, "$asinh", scm_tc7_cxr, (SCM (*)()) scm_asinh, g_asinh);
/* "Return the inverse hyperbolic sine of @var{x}."
 */
double
scm_asinh (double x)
{
  return log (x + sqrt (x * x + 1));
}




SCM_GPROC1 (s_acosh, "$acosh", scm_tc7_cxr, (SCM (*)()) scm_acosh, g_acosh);
/* "Return the inverse hyperbolic cosine of @var{x}."
 */
double
scm_acosh (double x)
{
  return log (x + sqrt (x * x - 1));
}




SCM_GPROC1 (s_atanh, "$atanh", scm_tc7_cxr, (SCM (*)()) scm_atanh, g_atanh);
/* "Return the inverse hyperbolic tangent of @var{x}."
 */
double
scm_atanh (double x)
{
  return 0.5 * log ((1 + x) / (1 - x));
}




SCM_GPROC1 (s_truncate, "truncate", scm_tc7_cxr, (SCM (*)()) scm_truncate, g_truncate);
/* "Round the inexact number @var{x} towards zero."
 */
double
scm_truncate (double x)
{
  if (x < 0.0)
    return -floor (-x);
  return floor (x);
}



SCM_GPROC1 (s_round, "round", scm_tc7_cxr, (SCM (*)()) scm_round, g_round);
/* "Round the inexact number @var{x}. If @var{x} is halfway between two\n"
 * "numbers, round towards even."
 */
double
scm_round (double x)
{
  double plus_half = x + 0.5;
  double result = floor (plus_half);
  /* Adjust so that the scm_round is towards even.  */
  return (plus_half == result && plus_half / 2 != floor (plus_half / 2))
    ? result - 1 : result;
}



SCM_GPROC1 (s_exact_to_inexact, "exact->inexact", scm_tc7_cxr, (SCM (*)()) scm_exact_to_inexact, g_exact_to_inexact);
/* Convert the number @var{x} to its inexact representation.\n" 
 */
double
scm_exact_to_inexact (double z)
{
  return z;
}


SCM_GPROC1 (s_i_floor, "floor", scm_tc7_cxr, (SCM (*)()) floor, g_i_floor);
/* "Round the number @var{x} towards minus infinity."
 */
SCM_GPROC1 (s_i_ceil, "ceiling", scm_tc7_cxr, (SCM (*)()) ceil, g_i_ceil);
/* "Round the number @var{x} towards infinity."
 */
SCM_GPROC1 (s_i_sqrt, "$sqrt", scm_tc7_cxr, (SCM (*)()) sqrt, g_i_sqrt);
/* "Return the square root of the real number @var{x}."
 */
SCM_GPROC1 (s_i_abs, "$abs", scm_tc7_cxr, (SCM (*)()) fabs, g_i_abs);
/* "Return the absolute value of the real number @var{x}."
 */
SCM_GPROC1 (s_i_exp, "$exp", scm_tc7_cxr, (SCM (*)()) exp, g_i_exp);
/* "Return the @var{x}th power of e."
 */
SCM_GPROC1 (s_i_log, "$log", scm_tc7_cxr, (SCM (*)()) log, g_i_log);
/* "Return the natural logarithm of the real number@var{x}."
 */
SCM_GPROC1 (s_i_sin, "$sin", scm_tc7_cxr, (SCM (*)()) sin, g_i_sin);
/* "Return the sine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_cos, "$cos", scm_tc7_cxr, (SCM (*)()) cos, g_i_cos);
/* "Return the cosine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_tan, "$tan", scm_tc7_cxr, (SCM (*)()) tan, g_i_tan);
/* "Return the tangent of the real number @var{x}."
 */
SCM_GPROC1 (s_i_asin, "$asin", scm_tc7_cxr, (SCM (*)()) asin, g_i_asin);
/* "Return the arc sine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_acos, "$acos", scm_tc7_cxr, (SCM (*)()) acos, g_i_acos);
/* "Return the arc cosine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_atan, "$atan", scm_tc7_cxr, (SCM (*)()) atan, g_i_atan);
/* "Return the arc tangent of the real number @var{x}."
 */
SCM_GPROC1 (s_i_sinh, "$sinh", scm_tc7_cxr, (SCM (*)()) sinh, g_i_sinh);
/* "Return the hyperbolic sine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_cosh, "$cosh", scm_tc7_cxr, (SCM (*)()) cosh, g_i_cosh);
/* "Return the hyperbolic cosine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_tanh, "$tanh", scm_tc7_cxr, (SCM (*)()) tanh, g_i_tanh);
/* "Return the hyperbolic tangent of the real number @var{x}."
 */

struct dpair
{
  double x, y;
};

static void scm_two_doubles (SCM x,
			     SCM y,
			     const char *sstring,
			     struct dpair * xy);

static void
scm_two_doubles (SCM x, SCM y, const char *sstring, struct dpair *xy)
{
  if (SCM_INUMP (x)) {
    xy->x = SCM_INUM (x);
  } else if (SCM_BIGP (x)) {
    xy->x = scm_big2dbl (x);
  } else if (SCM_REALP (x)) {
    xy->x = SCM_REAL_VALUE (x);
  } else {
    scm_wrong_type_arg (sstring, SCM_ARG1, x);
  }

  if (SCM_INUMP (y)) {
    xy->y = SCM_INUM (y);
  } else if (SCM_BIGP (y)) {
    xy->y = scm_big2dbl (y);
  } else if (SCM_REALP (y)) {
    xy->y = SCM_REAL_VALUE (y);
  } else {
    scm_wrong_type_arg (sstring, SCM_ARG2, y);
  }
}


SCM_DEFINE (scm_sys_expt, "$expt", 2, 0, 0,
            (SCM x, SCM y),
	    "Return @var{x} raised to the power of @var{y}. This\n"
	    "procedure does not accept complex arguments.") 
#define FUNC_NAME s_scm_sys_expt
{
  struct dpair xy;
  scm_two_doubles (x, y, FUNC_NAME, &xy);
  return scm_make_real (pow (xy.x, xy.y));
}
#undef FUNC_NAME


SCM_DEFINE (scm_sys_atan2, "$atan2", 2, 0, 0,
            (SCM x, SCM y),
	    "Return the arc tangent of the two arguments @var{x} and\n"
	    "@var{y}. This is similar to calculating the arc tangent of\n"
	    "@var{x} / @var{y}, except that the signs of both arguments\n"
	    "are used to determine the quadrant of the result. This\n"
	    "procedure does not accept complex arguments.")
#define FUNC_NAME s_scm_sys_atan2
{
  struct dpair xy;
  scm_two_doubles (x, y, FUNC_NAME, &xy);
  return scm_make_real (atan2 (xy.x, xy.y));
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_rectangular, "make-rectangular", 2, 0, 0,
            (SCM real, SCM imaginary),
	    "Return a complex number constructed of the given @var{real} and\n"
	    "@var{imaginary} parts.")
#define FUNC_NAME s_scm_make_rectangular
{
  struct dpair xy;
  scm_two_doubles (real, imaginary, FUNC_NAME, &xy);
  return scm_make_complex (xy.x, xy.y);
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_polar, "make-polar", 2, 0, 0,
            (SCM x, SCM y),
	    "Return the complex number @var{x} * e^(i * @var{y}).")
#define FUNC_NAME s_scm_make_polar
{
  struct dpair xy;
  scm_two_doubles (x, y, FUNC_NAME, &xy);
  return scm_make_complex (xy.x * cos (xy.y), xy.x * sin (xy.y));
}
#undef FUNC_NAME


SCM_GPROC (s_real_part, "real-part", 1, 0, 0, scm_real_part, g_real_part);
/* "Return the real part of the number @var{z}."
 */
SCM
scm_real_part (SCM z)
{
  if (SCM_INUMP (z)) {
    return z;
  } else if (SCM_BIGP (z)) {
    return z;
  } else if (SCM_REALP (z)) {
    return z;
  } else if (SCM_COMPLEXP (z)) {
    return scm_make_real (SCM_COMPLEX_REAL (z));
  } else {
    SCM_WTA_DISPATCH_1 (g_real_part, z, SCM_ARG1, s_real_part);
  }
}


SCM_GPROC (s_imag_part, "imag-part", 1, 0, 0, scm_imag_part, g_imag_part);
/* "Return the imaginary part of the number @var{z}."
 */
SCM
scm_imag_part (SCM z)
{
  if (SCM_INUMP (z)) {
    return SCM_INUM0;
  } else if (SCM_BIGP (z)) {
    return SCM_INUM0;
  } else if (SCM_REALP (z)) {
    return scm_flo0;
  } else if (SCM_COMPLEXP (z)) {
    return scm_make_real (SCM_COMPLEX_IMAG (z));
  } else {
    SCM_WTA_DISPATCH_1 (g_imag_part, z, SCM_ARG1, s_imag_part);
  }
}


SCM_GPROC (s_magnitude, "magnitude", 1, 0, 0, scm_magnitude, g_magnitude);
/* "Return the magnitude of the number @var{z}. This is the same as\n"
 * "@code{abs} for real arguments, but also allows complex numbers."
 */
SCM
scm_magnitude (SCM z)
{
  if (SCM_INUMP (z)) {
    long int zz = SCM_INUM (z);
    if (zz >= 0) {
      return z;
    } else if (SCM_POSFIXABLE (-zz)) {
      return SCM_MAKINUM (-zz);
    } else {
#ifdef SCM_BIGDIG
      return scm_long2big (-zz);
#else
      scm_num_overflow (s_magnitude);
#endif
    }
  } else if (SCM_BIGP (z)) {
    if (!SCM_BIGSIGN (z)) {
      return z;
    } else {
      return scm_copybig (z, 0);
    }
  } else if (SCM_REALP (z)) {
    return scm_make_real (fabs (SCM_REAL_VALUE (z)));
  } else if (SCM_COMPLEXP (z)) {
    double r = SCM_COMPLEX_REAL (z);
    double i = SCM_COMPLEX_IMAG (z);
    return scm_make_real (sqrt (i * i + r * r));
  } else {
    SCM_WTA_DISPATCH_1 (g_magnitude, z, SCM_ARG1, s_magnitude);
  }
}


SCM_GPROC (s_angle, "angle", 1, 0, 0, scm_angle, g_angle);
/* "Return the angle of the complex number @var{z}."
 */
SCM
scm_angle (SCM z)
{
  if (SCM_INUMP (z)) {
    if (SCM_INUM (z) >= 0) {
      return scm_make_real (atan2 (0.0, 1.0));
    } else {
      return scm_make_real (atan2 (0.0, -1.0));
    }
  } else if (SCM_BIGP (z)) {
    if (SCM_BIGSIGN (z)) {
      return scm_make_real (atan2 (0.0, -1.0));
    } else {
      return scm_make_real (atan2 (0.0, 1.0));
    }
  } else if (SCM_REALP (z)) {
    return scm_make_real (atan2 (0.0, SCM_REAL_VALUE (z)));
  } else if (SCM_COMPLEXP (z)) {
    return scm_make_real (atan2 (SCM_COMPLEX_IMAG (z), SCM_COMPLEX_REAL (z)));
  } else {
    SCM_WTA_DISPATCH_1 (g_angle, z, SCM_ARG1, s_angle);
  }
}


SCM_DEFINE (scm_inexact_to_exact, "inexact->exact", 1, 0, 0, 
            (SCM z),
	    "Returns an exact number that is numerically closest to @var{z}.")
#define FUNC_NAME s_scm_inexact_to_exact
{
  if (SCM_INUMP (z)) {
    return z;
  } else if (SCM_BIGP (z)) {
    return z;
  } else if (SCM_REALP (z)) {
    double u = floor (SCM_REAL_VALUE (z) + 0.5);
    long lu = (long) u;
    if (SCM_FIXABLE (lu)) {
      return SCM_MAKINUM (lu);
#ifdef SCM_BIGDIG
    } else if (isfinite (u)) {
      return scm_dbl2big (u);
#endif
    } else {
      scm_num_overflow (s_scm_inexact_to_exact);
    }
  } else {
    SCM_WRONG_TYPE_ARG (1, z);
  }
}
#undef FUNC_NAME


#ifdef SCM_BIGDIG
/* d must be integer */

SCM
scm_dbl2big (double d)
{
  scm_sizet i = 0;
  long c;
  SCM_BIGDIG *digits;
  SCM ans;
  double u = (d < 0) ? -d : d;
  while (0 != floor (u))
    {
      u /= SCM_BIGRAD;
      i++;
    }
  ans = scm_mkbig (i, d < 0);
  digits = SCM_BDIGITS (ans);
  while (i--)
    {
      u *= SCM_BIGRAD;
      c = floor (u);
      u -= c;
      digits[i] = c;
    }
#ifndef SCM_RECKLESS
  if (u != 0)
    scm_num_overflow ("dbl2big");
#endif
  return ans;
}



double
scm_big2dbl (SCM b)
{
  double ans = 0.0;
  scm_sizet i = SCM_NUMDIGS (b);
  SCM_BIGDIG *digits = SCM_BDIGITS (b);
  while (i--)
    ans = digits[i] + SCM_BIGRAD * ans;
  if (SCM_BIGSIGN (b))
    return - ans;
  return ans;
}
#endif


SCM
scm_long2num (long sl)
{
  if (!SCM_FIXABLE (sl))
    {
#ifdef SCM_BIGDIG
      return scm_long2big (sl);
#else
      return scm_make_real ((double) sl);
#endif
    }
  return SCM_MAKINUM (sl);
}


#ifdef HAVE_LONG_LONGS

SCM
scm_long_long2num (long_long sl)
{
  if (!SCM_FIXABLE (sl))
    {
#ifdef SCM_BIGDIG
      return scm_long_long2big (sl);
#else
      return scm_make_real ((double) sl);
#endif
    }
  else
    {
      /* we know that sl fits into an inum */
      return SCM_MAKINUM ((scm_bits_t) sl);
    }
}

#endif /* HAVE_LONG_LONGS */


SCM
scm_ulong2num (unsigned long sl)
{
  if (!SCM_POSFIXABLE (sl))
    {
#ifdef SCM_BIGDIG
      return scm_ulong2big (sl);
#else
      return scm_make_real ((double) sl);
#endif
    }
  return SCM_MAKINUM (sl);
}


long
scm_num2long (SCM num, char *pos, const char *s_caller)
{
  if (SCM_INUMP (num)) {
    return SCM_INUM (num);
  } else if (SCM_BIGP (num)) {
    long int res;
    /* can't use res directly in case num is -2^31.  */
    unsigned long int pos_res = 0;
    unsigned long int old_res = 0;
    scm_sizet l;

    for (l = SCM_NUMDIGS (num); l--;) {
      pos_res = SCM_BIGUP (pos_res) + SCM_BDIGITS (num)[l];
      if (pos_res >= old_res) {
	old_res = pos_res;
      } else {
	/* overflow. */
	scm_out_of_range (s_caller, num);
      }
    }
    if (SCM_BIGSIGN (num)) {
      res = -pos_res;
      if (res <= 0) {
	return res;
      } else {
	scm_out_of_range (s_caller, num);
      }
    } else {
      res = pos_res;
      if (res >= 0) {
	return res;
      } else {
	scm_out_of_range (s_caller, num);
      }
    }
  } else if (SCM_REALP (num)) {
    double u = SCM_REAL_VALUE (num);
    long int res = u;
    if ((double) res == u) {
      return res;
    } else {
      scm_out_of_range (s_caller, num);
    }
  } else {
    scm_wrong_type_arg (s_caller, (int) pos, num);
  }
}


#ifdef HAVE_LONG_LONGS

long_long
scm_num2long_long (SCM num, char *pos, const char *s_caller)
{
  if (SCM_INUMP (num)) {
    return SCM_INUM (num);
  } else if (SCM_BIGP (num)) {
    long long res;
    /* can't use res directly in case num is -2^63.  */
    unsigned long long int pos_res = 0;
    unsigned long long int old_res = 0;
    scm_sizet l;

    for (l = SCM_NUMDIGS (num); l--;) {
      pos_res = SCM_LONGLONGBIGUP (pos_res) + SCM_BDIGITS (num)[l];
      if (pos_res >= old_res) {
	old_res = pos_res;
      } else {
	/* overflow. */
	scm_out_of_range (s_caller, num);
      }
    }
    if (SCM_BIGSIGN (num)) {
      res = -pos_res;
      if (res <= 0) {
	return res;
      } else {
	scm_out_of_range (s_caller, num);
      }
    } else {
      res = pos_res;
      if (res >= 0) {
	return res;
      } else {
	scm_out_of_range (s_caller, num);
      }
    }
  } else if (SCM_REALP (num)) {
    double u = SCM_REAL_VALUE (num);
    long long int res = u;
    if ((double) res == u) {
      return res;
    } else {
      scm_out_of_range (s_caller, num);
    }
  } else {
    scm_wrong_type_arg (s_caller, (int) pos, num);
  }
}

#endif /* HAVE_LONG_LONGS */


unsigned long
scm_num2ulong (SCM num, char *pos, const char *s_caller)
{
  if (SCM_INUMP (num)) {
    long nnum = SCM_INUM (num);
    if (nnum >= 0) {
      return nnum;
    } else {
      scm_out_of_range (s_caller, num);
    }
  } else if (SCM_BIGP (num)) {
    unsigned long int res = 0;
    unsigned long int old_res = 0;
    scm_sizet l;
    
    for (l = SCM_NUMDIGS (num); l--;) {
      res = SCM_BIGUP (res) + SCM_BDIGITS (num)[l];
      if (res >= old_res) {
	old_res = res;
      } else {
	scm_out_of_range (s_caller, num);
      }
    }
    return res;
  } else if (SCM_REALP (num)) {
    double u = SCM_REAL_VALUE (num);
    unsigned long int res = u;
    if ((double) res == u) {
      return res;
    } else {
      scm_out_of_range (s_caller, num);
    }
  } else {
    scm_wrong_type_arg (s_caller, (int) pos, num);
  }
}


void
scm_init_numbers ()
{
  abs_most_negative_fixnum = scm_long2big (- SCM_MOST_NEGATIVE_FIXNUM);
  scm_permanent_object (abs_most_negative_fixnum);

  /* It may be possible to tune the performance of some algorithms by using
   * the following constants to avoid the creation of bignums.  Please, before
   * using these values, remember the two rules of program optimization:
   * 1st Rule:  Don't do it.  2nd Rule (experts only):  Don't do it yet. */
  scm_sysintern ("most-positive-fixnum", SCM_MAKINUM (SCM_MOST_POSITIVE_FIXNUM));
  scm_sysintern ("most-negative-fixnum", SCM_MAKINUM (SCM_MOST_NEGATIVE_FIXNUM));

  scm_add_feature ("complex");
  scm_add_feature ("inexact");
  scm_flo0 = scm_make_real (0.0);
#ifdef DBL_DIG
  scm_dblprec = (DBL_DIG > 20) ? 20 : DBL_DIG;
#else
  {				/* determine floating point precision */
    double f = 0.1;
    double fsum = 1.0 + f;
    while (fsum != 1.0) {
      if (++scm_dblprec > 20) {
	fsum = 1.0;
      } else {
	f /= 10.0;
	fsum = f + 1.0;
      }
    }
    scm_dblprec = scm_dblprec - 1;
  }
#endif /* DBL_DIG */
#ifndef SCM_MAGIC_SNARFER
#include "libguile/numbers.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
