/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003 Free Software Foundation, Inc.
 *
 * Portions Copyright 1990, 1991, 1992, 1993 by AT&T Bell Laboratories
 * and Bellcore.  See scm_divide.
 *
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */


/* General assumptions:
 * All objects satisfying SCM_COMPLEXP() have a non-zero complex component.
 * All objects satisfying SCM_BIGP() are too large to fit in a fixnum.
 * If an object satisfies integer?, it's either an inum, a bignum, or a real.
 * If floor (r) == r, r is an int, and mpz_set_d will DTRT.
 */

/* TODO:
   
   - see if special casing bignums and reals in integer-exponent when
     possible (to use mpz_pow and mpf_pow_ui) is faster.

   - look in to better short-circuiting of common cases in
     integer-expt and elsewhere.

   - see if direct mpz operations can help in ash and elsewhere.

 */

/* tell glibc (2.3) to give prototype for C99 trunc() */
#define _GNU_SOURCE

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <math.h>
#include <ctype.h>
#include <string.h>
#include <gmp.h>
#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/numbers.h"
#include "libguile/deprecation.h"



/*
  Wonder if this might be faster for some of our code?  A switch on
  the numtag would jump directly to the right case, and the
  SCM_I_NUMTAG code might be faster than repeated SCM_FOOP tests...

  #define SCM_I_NUMTAG_NOTNUM 0
  #define SCM_I_NUMTAG_INUM 1
  #define SCM_I_NUMTAG_BIG scm_tc16_big
  #define SCM_I_NUMTAG_REAL scm_tc16_real
  #define SCM_I_NUMTAG_COMPLEX scm_tc16_complex
  #define SCM_I_NUMTAG(x) \
    (SCM_INUMP(x) ? SCM_I_NUMTAG_INUM \
       : (SCM_IMP(x) ? SCM_I_NUMTAG_NOTNUM \
         : (((0xfcff & SCM_CELL_TYPE (x)) == scm_tc7_smob) ? SCM_TYP16(x) \
           : SCM_I_NUMTAG_NOTNUM)))
*/


#define SCM_SWAP(x, y) do { SCM __t = x; x = y; y = __t; } while (0)

/* FLOBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an inexact number.
 */
#define FLOBUFLEN (10+2*(sizeof(double)/sizeof(char)*SCM_CHAR_BIT*3+9)/10)

#if defined (SCO)
#if ! defined (HAVE_ISNAN)
#define HAVE_ISNAN
static int
isnan (double x)
{
  return (IsNANorINF (x) && NaN (x) && ! IsINF (x)) ? 1 : 0;
}
#endif
#if ! defined (HAVE_ISINF)
#define HAVE_ISINF
static int
isinf (double x)
{
  return (IsNANorINF (x) && IsINF (x)) ? 1 : 0;
}

#endif
#endif


/* mpz_cmp_d only recognises infinities in gmp 4.2 and up.
   For prior versions use an explicit check here.  */
#if __GNU_MP_VERSION < 4                                        \
  || (__GNU_MP_VERSION == 4 && __GNU_MP_VERSION_MINOR < 2)
#define xmpz_cmp_d(z, d)                                \
  (xisinf (d) ? (d < 0.0 ? 1 : -1) : mpz_cmp_d (z, d))
#else
#define xmpz_cmp_d(z, d)  mpz_cmp_d (z, d)
#endif



static SCM abs_most_negative_fixnum;
static mpz_t z_negative_one;



static const char s_bignum[] = "bignum";

SCM_C_INLINE_KEYWORD SCM
scm_i_mkbig ()
{
  /* Return a newly created bignum. */
  SCM z = scm_double_cell (scm_tc16_big, 0, 0, 0);
  mpz_init (SCM_I_BIG_MPZ (z));
  return z;
}

SCM_C_INLINE_KEYWORD static SCM
scm_i_clonebig (SCM src_big, int same_sign_p)
{
  /* Copy src_big's value, negate it if same_sign_p is false, and return. */
  SCM z = scm_double_cell (scm_tc16_big, 0, 0, 0);
  mpz_init_set (SCM_I_BIG_MPZ (z), SCM_I_BIG_MPZ (src_big));
  if (!same_sign_p)
    mpz_neg (SCM_I_BIG_MPZ (z), SCM_I_BIG_MPZ (z));
  return z;
}

SCM_C_INLINE_KEYWORD int
scm_i_bigcmp (SCM x, SCM y)
{
  /* Return neg if x < y, pos if x > y, and 0 if x == y */
  /* presume we already know x and y are bignums */
  int result = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
  scm_remember_upto_here_2 (x, y);
  return result;
}

SCM_C_INLINE_KEYWORD SCM
scm_i_dbl2big (double d)
{
  /* results are only defined if d is an integer */
  SCM z = scm_double_cell (scm_tc16_big, 0, 0, 0);
  mpz_init_set_d (SCM_I_BIG_MPZ (z), d);
  return z;
}

SCM_C_INLINE_KEYWORD double
scm_i_big2dbl (SCM b)
{
  double result = mpz_get_d (SCM_I_BIG_MPZ (b));
  scm_remember_upto_here_1 (b);
  return result;
}

SCM_C_INLINE_KEYWORD SCM
scm_i_normbig (SCM b)
{
  /* convert a big back to a fixnum if it'll fit */
  /* presume b is a bignum */
  if (mpz_fits_slong_p (SCM_I_BIG_MPZ (b)))
    {
      long val = mpz_get_si (SCM_I_BIG_MPZ (b));
      if (SCM_FIXABLE (val))
        b = SCM_MAKINUM (val);
    }
  return b;
}

SCM_DEFINE (scm_exact_p, "exact?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is an exact number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_exact_p
{
  if (SCM_INUMP (x))
    return SCM_BOOL_T;
  if (SCM_BIGP (x))
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_odd_p, "odd?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is an odd number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_odd_p
{
  if (SCM_INUMP (n))
    {
      long val = SCM_INUM (n);
      return SCM_BOOL ((val & 1L) != 0);
    }
  else if (SCM_BIGP (n))
    {
      int odd_p = mpz_odd_p (SCM_I_BIG_MPZ (n));
      scm_remember_upto_here_1 (n);
      return SCM_BOOL (odd_p);
    }
  else if (!SCM_FALSEP (scm_inf_p (n)))
    return SCM_BOOL_T;
  else
    SCM_WRONG_TYPE_ARG (1, n);
}
#undef FUNC_NAME


SCM_DEFINE (scm_even_p, "even?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is an even number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_even_p
{
  if (SCM_INUMP (n))
    {
      long val = SCM_INUM (n);
      return SCM_BOOL ((val & 1L) == 0);
    }
  else if (SCM_BIGP (n))
    {
      int even_p = mpz_even_p (SCM_I_BIG_MPZ (n));
      scm_remember_upto_here_1 (n);
      return SCM_BOOL (even_p);
    }
  else if (!SCM_FALSEP (scm_inf_p (n)))
    return SCM_BOOL_T;
  else
    SCM_WRONG_TYPE_ARG (1, n);
}
#undef FUNC_NAME

static int
xisinf (double x)
{
#if defined (HAVE_ISINF)
  return isinf (x);
#elif defined (HAVE_FINITE) && defined (HAVE_ISNAN)
  return (! (finite (x) || isnan (x)));
#else
  return 0;
#endif
}

static int
xisnan (double x)
{
#if defined (HAVE_ISNAN)
  return isnan (x);
#else
  return 0;
#endif
}

SCM_DEFINE (scm_inf_p, "inf?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is infinite, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_inf_p
{
  if (SCM_REALP (n))
    return SCM_BOOL (xisinf (SCM_REAL_VALUE (n)));
  else if (SCM_COMPLEXP (n))
    return SCM_BOOL (xisinf (SCM_COMPLEX_REAL (n))
		     || xisinf (SCM_COMPLEX_IMAG (n)));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_nan_p, "nan?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is a NaN, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_nan_p
{
  if (SCM_REALP (n))
    return SCM_BOOL (xisnan (SCM_REAL_VALUE (n)));
  else if (SCM_COMPLEXP (n))
    return SCM_BOOL (xisnan (SCM_COMPLEX_REAL (n))
		     || xisnan (SCM_COMPLEX_IMAG (n)));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Guile's idea of infinity.  */
static double guile_Inf;

/* Guile's idea of not a number.  */
static double guile_NaN;

static void
guile_ieee_init (void)
{
#if defined (HAVE_ISINF) || defined (HAVE_FINITE)

/* Some version of gcc on some old version of Linux used to crash when
   trying to make Inf and NaN.  */

#if defined (SCO)
  double tmp = 1.0;
  guile_Inf = 1.0 / (tmp - tmp);
#elif defined (__alpha__) && ! defined (linux)
  extern unsigned int DINFINITY[2];
  guile_Inf = (*(X_CAST(double *, DINFINITY)));
#else
  double tmp = 1e+10;
  guile_Inf = tmp;
  for (;;)
    {
      guile_Inf *= 1e+10;
      if (guile_Inf == tmp)
	break;
      tmp = guile_Inf;
    }
#endif

#endif

#if defined (HAVE_ISNAN)

#if defined (__alpha__) && ! defined (linux)
  extern unsigned int DQNAN[2];
  guile_NaN =  (*(X_CAST(double *, DQNAN)));
#else
  guile_NaN = guile_Inf / guile_Inf;
#endif

#endif
}

SCM_DEFINE (scm_inf, "inf", 0, 0, 0, 
            (void),
	    "Return Inf.")
#define FUNC_NAME s_scm_inf
{
  static int initialized = 0;
  if (! initialized)
    {
      guile_ieee_init ();
      initialized = 1;
    }
  return scm_make_real (guile_Inf);
}
#undef FUNC_NAME

SCM_DEFINE (scm_nan, "nan", 0, 0, 0, 
            (void),
	    "Return NaN.")
#define FUNC_NAME s_scm_nan
{
  static int initialized = 0;
  if (!initialized)
    {
      guile_ieee_init ();
      initialized = 1;
    }
  return scm_make_real (guile_NaN);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_abs, "abs", 1, 0, 0,
		       (SCM x),
		       "Return the absolute value of @var{x}.")
#define FUNC_NAME
{
  if (SCM_INUMP (x))
    {
      long int xx = SCM_INUM (x);
      if (xx >= 0)
	return x;
      else if (SCM_POSFIXABLE (-xx))
	return SCM_MAKINUM (-xx);
      else
	return scm_i_long2big (-xx);
    }
  else if (SCM_BIGP (x))
    {
      const int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
      if (sgn < 0)
	return scm_i_clonebig (x, 0);
      else
	return x;
    }
  else if (SCM_REALP (x))
    return scm_make_real (fabs (SCM_REAL_VALUE (x)));
  else
    SCM_WTA_DISPATCH_1 (g_scm_abs, x, 1, s_scm_abs);
}
#undef FUNC_NAME


SCM_GPROC (s_quotient, "quotient", 2, 0, 0, scm_quotient, g_quotient);
/* "Return the quotient of the numbers @var{x} and @var{y}."
 */
SCM
scm_quotient (SCM x, SCM y)
{
  if (SCM_INUMP (x))
    {
      long xx = SCM_INUM (x);
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  if (yy == 0)
	    scm_num_overflow (s_quotient);
	  else
	    {
	      long z = xx / yy;
	      if (SCM_FIXABLE (z))
		return SCM_MAKINUM (z);
	      else
		return scm_i_long2big (z);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  if ((SCM_INUM (x) == SCM_MOST_NEGATIVE_FIXNUM)
	      && (scm_i_bigcmp (abs_most_negative_fixnum, y) == 0))
	    /* Special case:  x == fixnum-min && y == abs (fixnum-min) */
	    return SCM_MAKINUM (-1);
	  else
	    return SCM_MAKINUM (0);
	}
      else
	SCM_WTA_DISPATCH_2 (g_quotient, x, y, SCM_ARG2, s_quotient);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  if (yy == 0)
	    scm_num_overflow (s_quotient);
	  else if (yy == 1)
	    return x;
	  else
	    {
	      SCM result = scm_i_mkbig ();
	      if (yy < 0)
		{
		  mpz_tdiv_q_ui (SCM_I_BIG_MPZ (result),
				 SCM_I_BIG_MPZ (x),
				 - yy);
		  mpz_neg (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result));
		}
	      else
		mpz_tdiv_q_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (x), yy);
	      scm_remember_upto_here_1 (x);
	      return scm_i_normbig (result);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM result = scm_i_mkbig ();
	  mpz_tdiv_q (SCM_I_BIG_MPZ (result),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (result);
	}
      else
	SCM_WTA_DISPATCH_2 (g_quotient, x, y, SCM_ARG2, s_quotient);
    }
  else
    SCM_WTA_DISPATCH_2 (g_quotient, x, y, SCM_ARG1, s_quotient);
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
  if (SCM_INUMP (x))
    {
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  if (yy == 0)
	    scm_num_overflow (s_remainder);
	  else
	    {
	      long z = SCM_INUM (x) % yy;
	      return SCM_MAKINUM (z);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  if ((SCM_INUM (x) == SCM_MOST_NEGATIVE_FIXNUM)
	      && (scm_i_bigcmp (abs_most_negative_fixnum, y) == 0))
	    /* Special case:  x == fixnum-min && y == abs (fixnum-min) */
	    return SCM_MAKINUM (0);
	  else
	    return x;
	}
      else
	SCM_WTA_DISPATCH_2 (g_remainder, x, y, SCM_ARG2, s_remainder);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  if (yy == 0)
	    scm_num_overflow (s_remainder);
	  else
	    {
	      SCM result = scm_i_mkbig ();
	      if (yy < 0)
		yy = - yy;
	      mpz_tdiv_r_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ(x), yy);
	      scm_remember_upto_here_1 (x);
	      return scm_i_normbig (result);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM result = scm_i_mkbig ();
	  mpz_tdiv_r (SCM_I_BIG_MPZ (result),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (result);
	}
      else
	SCM_WTA_DISPATCH_2 (g_remainder, x, y, SCM_ARG2, s_remainder);
    }
  else
    SCM_WTA_DISPATCH_2 (g_remainder, x, y, SCM_ARG1, s_remainder);
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
  if (SCM_INUMP (x))
    {
      long xx = SCM_INUM (x);
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  if (yy == 0)
	    scm_num_overflow (s_modulo);
	  else
	    {
	      /* FIXME: I think this may be a bug on some arches -- results
		 of % with negative second arg are undefined... */
	      long z = xx % yy;
	      long result;

	      if (yy < 0)
		{
		  if (z > 0)
		    result = z + yy;
		  else
		    result = z;
		}
	      else
		{
		  if (z < 0)
		    result = z + yy;
		  else
		    result = z;
		}
	      return SCM_MAKINUM (result);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  int sgn_y = mpz_sgn (SCM_I_BIG_MPZ (y));

	  if (sgn_y == 0)
	    scm_num_overflow (s_modulo);
	  else
	    {
	      mpz_t z_x;
	      SCM result;

	      if (sgn_y < 0)
		{
		  SCM pos_y = scm_i_clonebig (y, 0);
		  /* do this after the last scm_op */
		  mpz_init_set_si (z_x, xx);
		  result = pos_y; /* re-use this bignum */
		  mpz_mod (SCM_I_BIG_MPZ (result),
			   z_x,
			   SCM_I_BIG_MPZ (pos_y));        
		  scm_remember_upto_here_1 (pos_y);
		}
	      else
		{
		  result = scm_i_mkbig ();
		  /* do this after the last scm_op */
		  mpz_init_set_si (z_x, xx);
		  mpz_mod (SCM_I_BIG_MPZ (result),
			   z_x,
			   SCM_I_BIG_MPZ (y));        
		  scm_remember_upto_here_1 (y);
		}
        
	      if ((sgn_y < 0) && mpz_sgn (SCM_I_BIG_MPZ (result)) != 0)
		mpz_add (SCM_I_BIG_MPZ (result),
			 SCM_I_BIG_MPZ (y),
			 SCM_I_BIG_MPZ (result));
	      scm_remember_upto_here_1 (y);
	      /* and do this before the next one */
	      mpz_clear (z_x);
	      return scm_i_normbig (result);
	    }
	}
      else
	SCM_WTA_DISPATCH_2 (g_modulo, x, y, SCM_ARG2, s_modulo);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  if (yy == 0)
	    scm_num_overflow (s_modulo);
	  else
	    {
	      SCM result = scm_i_mkbig ();
	      mpz_mod_ui (SCM_I_BIG_MPZ (result),
			  SCM_I_BIG_MPZ (x),
			  (yy < 0) ? - yy : yy);
	      scm_remember_upto_here_1 (x);
	      if ((yy < 0) && (mpz_sgn (SCM_I_BIG_MPZ (result)) != 0))
		mpz_sub_ui (SCM_I_BIG_MPZ (result),
			    SCM_I_BIG_MPZ (result),
			    - yy);
	      return scm_i_normbig (result);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  int sgn_y = mpz_sgn (SCM_I_BIG_MPZ (y));
	  if (sgn_y == 0)
	    scm_num_overflow (s_modulo);
	  else
	    {
	      SCM result = scm_i_mkbig ();
	      int y_sgn = mpz_sgn (SCM_I_BIG_MPZ (y));
	      SCM pos_y = scm_i_clonebig (y, y_sgn >= 0);
	      mpz_mod (SCM_I_BIG_MPZ (result),
		       SCM_I_BIG_MPZ (x),
		       SCM_I_BIG_MPZ (pos_y));
        
	      scm_remember_upto_here_1 (x);
	      if ((y_sgn < 0) && (mpz_sgn (SCM_I_BIG_MPZ (result)) != 0))
		mpz_add (SCM_I_BIG_MPZ (result),
			 SCM_I_BIG_MPZ (y),
			 SCM_I_BIG_MPZ (result));
	      scm_remember_upto_here_2 (y, pos_y);
	      return scm_i_normbig (result);
	    }
	}
      else
	SCM_WTA_DISPATCH_2 (g_modulo, x, y, SCM_ARG2, s_modulo);
    }
  else
    SCM_WTA_DISPATCH_2 (g_modulo, x, y, SCM_ARG1, s_modulo);
}

SCM_GPROC1 (s_gcd, "gcd", scm_tc7_asubr, scm_gcd, g_gcd);
/* "Return the greatest common divisor of all arguments.\n"
 * "If called without arguments, 0 is returned."
 */
SCM
scm_gcd (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    return SCM_UNBNDP (x) ? SCM_INUM0 : x;
  
  if (SCM_INUMP (x))
    {
      if (SCM_INUMP (y))
        {
          long xx = SCM_INUM (x);
          long yy = SCM_INUM (y);
          long u = xx < 0 ? -xx : xx;
          long v = yy < 0 ? -yy : yy;
          long result;
          if (xx == 0)
	    result = v;
	  else if (yy == 0)
	    result = u;
	  else
	    {
	      long k = 1;
	      long t;
	      /* Determine a common factor 2^k */
	      while (!(1 & (u | v)))
		{
		  k <<= 1;
		  u >>= 1;
		  v >>= 1;
		}
	      /* Now, any factor 2^n can be eliminated */
	      if (u & 1)
		t = -v;
	      else
		{
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
          return (SCM_POSFIXABLE (result)
		  ? SCM_MAKINUM (result)
		  : scm_i_long2big (result));
        }
      else if (SCM_BIGP (y))
        {
          SCM result = scm_i_mkbig ();
          SCM mx = scm_i_mkbig ();
          mpz_set_si (SCM_I_BIG_MPZ (mx), SCM_INUM (x));
          scm_remember_upto_here_1 (x);
          mpz_gcd (SCM_I_BIG_MPZ (result),
		   SCM_I_BIG_MPZ (mx),
		   SCM_I_BIG_MPZ (y));
          scm_remember_upto_here_2 (mx, y);
          return scm_i_normbig (result);
        }
      else
        SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG2, s_gcd);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
        {
          unsigned long result;
          long yy = SCM_INUM (y);
          if (yy == 0)
            return scm_abs (x);
          if (yy < 0)
	    yy = -yy;
          result = mpz_gcd_ui (NULL, SCM_I_BIG_MPZ (x), yy);
          scm_remember_upto_here_1 (x);
          return (SCM_POSFIXABLE (result) 
		  ? SCM_MAKINUM (result)
		  : scm_ulong2num (result));
        }
      else if (SCM_BIGP (y))
        {
          SCM result = scm_i_mkbig ();
          mpz_gcd (SCM_I_BIG_MPZ (result),
		   SCM_I_BIG_MPZ (x),
		   SCM_I_BIG_MPZ (y));
          scm_remember_upto_here_2 (x, y);
          return scm_i_normbig (result);
        }
      else
        SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG2, s_gcd);
    }
  else
    SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG1, s_gcd);
}

SCM_GPROC1 (s_lcm, "lcm", scm_tc7_asubr, scm_lcm, g_lcm);
/* "Return the least common multiple of the arguments.\n"
 * "If called without arguments, 1 is returned."
 */
SCM
scm_lcm (SCM n1, SCM n2)
{
  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
        return SCM_MAKINUM (1L);
      n2 = SCM_MAKINUM (1L);
    }

  SCM_GASSERT2 (SCM_INUMP (n1) || SCM_BIGP (n1),
                g_lcm, n1, n2, SCM_ARG1, s_lcm);
  SCM_GASSERT2 (SCM_INUMP (n2) || SCM_BIGP (n2),
                g_lcm, n1, n2, SCM_ARGn, s_lcm);

  if (SCM_INUMP (n1))
    {
      if (SCM_INUMP (n2))
        {
          SCM d = scm_gcd (n1, n2);
          if (SCM_EQ_P (d, SCM_INUM0))
            return d;
          else
            return scm_abs (scm_product (n1, scm_quotient (n2, d)));
        }
      else
        {
          /* inum n1, big n2 */
        inumbig:
          {
            SCM result = scm_i_mkbig ();
            long nn1 = SCM_INUM (n1);
            if (nn1 == 0) return SCM_INUM0;
            if (nn1 < 0) nn1 = - nn1;
            mpz_lcm_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (n2), nn1);
            scm_remember_upto_here_1 (n2);
            return result;
          }
        }
    }
  else
    {
      /* big n1 */
      if (SCM_INUMP (n2))
        {
          SCM_SWAP (n1, n2);
          goto inumbig;
        }
      else
        {
          SCM result = scm_i_mkbig ();
          mpz_lcm(SCM_I_BIG_MPZ (result),
                  SCM_I_BIG_MPZ (n1),
                  SCM_I_BIG_MPZ (n2));
          scm_remember_upto_here_2(n1, n2);
          /* shouldn't need to normalize b/c lcm of 2 bigs should be big */
          return result;
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

SCM_DEFINE1 (scm_logand, "logand", scm_tc7_asubr,
             (SCM n1, SCM n2),
	     "Return the bitwise AND of the integer arguments.\n\n"
	     "@lisp\n"
	     "(logand) @result{} -1\n"
	     "(logand 7) @result{} 7\n"
	     "(logand #b111 #b011 #\b001) @result{} 1\n"
	     "@end lisp")
#define FUNC_NAME s_scm_logand
{
  long int nn1;

  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
	return SCM_MAKINUM (-1);
      else if (!SCM_NUMBERP (n1))
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
      else if (SCM_NUMBERP (n1))
	return n1;
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
    }

  if (SCM_INUMP (n1))
    {
      nn1 = SCM_INUM (n1);
      if (SCM_INUMP (n2))
	{
	  long nn2 = SCM_INUM (n2);
	  return SCM_MAKINUM (nn1 & nn2);
	}
      else if SCM_BIGP (n2)
	{
	intbig: 
	  if (n1 == 0)
	    return SCM_INUM0;
	  {
	    SCM result_z = scm_i_mkbig ();
	    mpz_t nn1_z;
	    mpz_init_set_si (nn1_z, nn1);
	    mpz_and (SCM_I_BIG_MPZ (result_z), nn1_z, SCM_I_BIG_MPZ (n2));
	    scm_remember_upto_here_1 (n2);
	    mpz_clear (nn1_z);
	    return scm_i_normbig (result_z);
	  }
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  else if (SCM_BIGP (n1))
    {
      if (SCM_INUMP (n2))
	{
	  SCM_SWAP (n1, n2);
	  nn1 = SCM_INUM (n1);
	  goto intbig;
	}
      else if (SCM_BIGP (n2))
	{
	  SCM result_z = scm_i_mkbig ();
	  mpz_and (SCM_I_BIG_MPZ (result_z),
		   SCM_I_BIG_MPZ (n1),
		   SCM_I_BIG_MPZ (n2));
	  scm_remember_upto_here_2 (n1, n2);
	  return scm_i_normbig (result_z);
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_logior, "logior", scm_tc7_asubr,
             (SCM n1, SCM n2),
	     "Return the bitwise OR of the integer arguments.\n\n"
	     "@lisp\n"
	     "(logior) @result{} 0\n"
	     "(logior 7) @result{} 7\n"
	     "(logior #b000 #b001 #b011) @result{} 3\n"
	    "@end lisp")
#define FUNC_NAME s_scm_logior
{
  long int nn1;

  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
	return SCM_INUM0;
      else if (SCM_NUMBERP (n1))
	return n1;
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
    }

  if (SCM_INUMP (n1))
    {
      nn1 = SCM_INUM (n1);
      if (SCM_INUMP (n2))
	{
	  long nn2 = SCM_INUM (n2);
	  return SCM_MAKINUM (nn1 | nn2);
	}
      else if (SCM_BIGP (n2))
	{
	intbig:
	  if (nn1 == 0)
	    return n2;
	  {
	    SCM result_z = scm_i_mkbig ();
	    mpz_t nn1_z;
	    mpz_init_set_si (nn1_z, nn1);
	    mpz_ior (SCM_I_BIG_MPZ (result_z), nn1_z, SCM_I_BIG_MPZ (n2));
	    scm_remember_upto_here_1 (n2);
	    mpz_clear (nn1_z);
	    return result_z;
	  }
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  else if (SCM_BIGP (n1))
    {
      if (SCM_INUMP (n2))
	{
	  SCM_SWAP (n1, n2); 
	  nn1 = SCM_INUM (n1);
	  goto intbig;
	}
      else if (SCM_BIGP (n2))
	{
	  SCM result_z = scm_i_mkbig ();
	  mpz_ior (SCM_I_BIG_MPZ (result_z),
		   SCM_I_BIG_MPZ (n1),
		   SCM_I_BIG_MPZ (n2));
	  scm_remember_upto_here_2 (n1, n2);
	  return result_z;
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_logxor, "logxor", scm_tc7_asubr,
             (SCM n1, SCM n2),
	     "Return the bitwise XOR of the integer arguments.  A bit is\n"
	     "set in the result if it is set in an odd number of arguments.\n"
	     "@lisp\n"
	     "(logxor) @result{} 0\n"
	     "(logxor 7) @result{} 7\n"
	     "(logxor #b000 #b001 #b011) @result{} 2\n"
	     "(logxor #b000 #b001 #b011 #b011) @result{} 1\n"
	    "@end lisp")
#define FUNC_NAME s_scm_logxor
{
  long int nn1;

  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
	return SCM_INUM0;
      else if (SCM_NUMBERP (n1))
	return n1;
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
    }

  if (SCM_INUMP (n1))
    {
      nn1 = SCM_INUM (n1);
      if (SCM_INUMP (n2))
	{
	  long nn2 = SCM_INUM (n2);
	  return SCM_MAKINUM (nn1 ^ nn2);
	}
      else if (SCM_BIGP (n2))
	{
	intbig:
	  {
	    SCM result_z = scm_i_mkbig ();
	    mpz_t nn1_z;
	    mpz_init_set_si (nn1_z, nn1);
	    mpz_xor (SCM_I_BIG_MPZ (result_z), nn1_z, SCM_I_BIG_MPZ (n2));
	    scm_remember_upto_here_1 (n2);
	    mpz_clear (nn1_z);
	    return scm_i_normbig (result_z);
	  }
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  else if (SCM_BIGP (n1))
    {
      if (SCM_INUMP (n2))
	{
	  SCM_SWAP (n1, n2);
	  nn1 = SCM_INUM (n1);
	  goto intbig;
	}
      else if (SCM_BIGP (n2))
	{
	  SCM result_z = scm_i_mkbig ();
	  mpz_xor (SCM_I_BIG_MPZ (result_z),
		   SCM_I_BIG_MPZ (n1),
		   SCM_I_BIG_MPZ (n2));
	  scm_remember_upto_here_2 (n1, n2);
	  return scm_i_normbig (result_z);
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
}
#undef FUNC_NAME


SCM_DEFINE (scm_logtest, "logtest", 2, 0, 0,
            (SCM j, SCM k),
	    "@lisp\n"
	    "(logtest j k) @equiv{} (not (zero? (logand j k)))\n\n"
	    "(logtest #b0100 #b1011) @result{} #f\n"
	    "(logtest #b0100 #b0111) @result{} #t\n"
	    "@end lisp")
#define FUNC_NAME s_scm_logtest
{
  long int nj;

  if (SCM_INUMP (j))
    {
      nj = SCM_INUM (j);
      if (SCM_INUMP (k))
	{
	  long nk = SCM_INUM (k);
	  return SCM_BOOL (nj & nk);
	}
      else if (SCM_BIGP (k))
	{
	intbig: 
	  if (nj == 0)
	    return SCM_BOOL_F;
	  {
	    SCM result;
	    mpz_t nj_z;
	    mpz_init_set_si (nj_z, nj);
	    mpz_and (nj_z, nj_z, SCM_I_BIG_MPZ (k));
	    scm_remember_upto_here_1 (k);
	    result = SCM_BOOL (mpz_sgn (nj_z) != 0);
	    mpz_clear (nj_z);
	    return result;
	  }
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, k);
    }
  else if (SCM_BIGP (j))
    {
      if (SCM_INUMP (k))
	{
	  SCM_SWAP (j, k);
	  nj = SCM_INUM (j);
	  goto intbig;
	}
      else if (SCM_BIGP (k))
	{
	  SCM result;
	  mpz_t result_z;
	  mpz_init (result_z);
	  mpz_and (result_z,
		   SCM_I_BIG_MPZ (j),
		   SCM_I_BIG_MPZ (k));
	  scm_remember_upto_here_2 (j, k);
	  result = SCM_BOOL (mpz_sgn (result_z) != 0);
	  mpz_clear (result_z);
	  return result;
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, k);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, j);
}
#undef FUNC_NAME


SCM_DEFINE (scm_logbit_p, "logbit?", 2, 0, 0,
            (SCM index, SCM j),
	    "@lisp\n"
	    "(logbit? index j) @equiv{} (logtest (integer-expt 2 index) j)\n\n"
	    "(logbit? 0 #b1101) @result{} #t\n"
	    "(logbit? 1 #b1101) @result{} #f\n"
	    "(logbit? 2 #b1101) @result{} #t\n"
	    "(logbit? 3 #b1101) @result{} #t\n"
	    "(logbit? 4 #b1101) @result{} #f\n"
	    "@end lisp")
#define FUNC_NAME s_scm_logbit_p
{
  unsigned long int iindex;

  SCM_VALIDATE_INUM_MIN (SCM_ARG1, index, 0);
  iindex = (unsigned long int) SCM_INUM (index);

  if (SCM_INUMP (j))
    return SCM_BOOL ((1L << iindex) & SCM_INUM (j));
  else if (SCM_BIGP (j))
    {
      int val = mpz_tstbit (SCM_I_BIG_MPZ (j), iindex);
      scm_remember_upto_here_1 (j);
      return SCM_BOOL (val);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG2, j);
}
#undef FUNC_NAME


SCM_DEFINE (scm_lognot, "lognot", 1, 0, 0, 
            (SCM n),
	    "Return the integer which is the 2s-complement of the integer\n"
	    "argument.\n"
	    "\n"
	    "@lisp\n"
	    "(number->string (lognot #b10000000) 2)\n"
	    "   @result{} \"-10000001\"\n"
	    "(number->string (lognot #b0) 2)\n"
	    "   @result{} \"-1\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_lognot
{
  return scm_difference (SCM_MAKINUM (-1L), n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_integer_expt, "integer-expt", 2, 0, 0,
            (SCM n, SCM k),
	    "Return @var{n} raised to the non-negative integer exponent\n"
	    "@var{k}.\n"
	    "\n"
	    "@lisp\n"
	    "(integer-expt 2 5)\n"
	    "   @result{} 32\n"
	    "(integer-expt -3 3)\n"
	    "   @result{} -27\n"
	    "@end lisp")
#define FUNC_NAME s_scm_integer_expt
{
  long i2 = 0;
  SCM z_i2 = SCM_BOOL_F;
  int i2_is_big = 0;
  SCM acc = SCM_MAKINUM (1L);

  /* 0^0 == 1 according to R5RS */
  if (SCM_EQ_P (n, SCM_INUM0) || SCM_EQ_P (n, acc))
    return SCM_FALSEP (scm_zero_p(k)) ? n : acc;
  else if (SCM_EQ_P (n, SCM_MAKINUM (-1L)))
    return SCM_FALSEP (scm_even_p (k)) ? n : acc;

  if (SCM_INUMP (k))
    i2 = SCM_INUM (k);
  else if (SCM_BIGP (k))
    {
      z_i2 = scm_i_clonebig (k, 1);
      mpz_init_set (SCM_I_BIG_MPZ (z_i2), SCM_I_BIG_MPZ (k));
      scm_remember_upto_here_1 (k);
      i2_is_big = 1;
    }
  else if (SCM_REALP (k))
    {
      double r = SCM_REAL_VALUE (k);
      if (floor (r) != r)
        SCM_WRONG_TYPE_ARG (2, k);
      if ((r > SCM_MOST_POSITIVE_FIXNUM) || (r < SCM_MOST_NEGATIVE_FIXNUM))
        {
          z_i2 = scm_i_mkbig ();
          mpz_init_set_d (SCM_I_BIG_MPZ (z_i2), r);
          i2_is_big = 1;
        }
      else
        {
          i2 = r;
        }
    }
  else
    SCM_WRONG_TYPE_ARG (2, k);
  
  if (i2_is_big)
    {
      if (mpz_sgn(SCM_I_BIG_MPZ (z_i2)) == -1)
        {
          mpz_neg (SCM_I_BIG_MPZ (z_i2), SCM_I_BIG_MPZ (z_i2));
          n = scm_divide (n, SCM_UNDEFINED);
        }
      while (1)
        {
          if (mpz_sgn(SCM_I_BIG_MPZ (z_i2)) == 0)
            {
              mpz_clear (SCM_I_BIG_MPZ (z_i2));
              return acc;
            }
          if (mpz_cmp_ui(SCM_I_BIG_MPZ (z_i2), 1) == 0)
            {
              mpz_clear (SCM_I_BIG_MPZ (z_i2));
              return scm_product (acc, n);
            }
          if (mpz_tstbit(SCM_I_BIG_MPZ (z_i2), 0))
            acc = scm_product (acc, n);
          n = scm_product (n, n);
          mpz_fdiv_q_2exp (SCM_I_BIG_MPZ (z_i2), SCM_I_BIG_MPZ (z_i2), 1);
        }
    }
  else
    {
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
}
#undef FUNC_NAME

SCM_DEFINE (scm_ash, "ash", 2, 0, 0,
            (SCM n, SCM cnt),
	    "The function ash performs an arithmetic shift left by @var{cnt}\n"
	    "bits (or shift right, if @var{cnt} is negative).  'Arithmetic'\n"
	    "means, that the function does not guarantee to keep the bit\n"
	    "structure of @var{n}, but rather guarantees that the result\n"
	    "will always be rounded towards minus infinity.  Therefore, the\n"
	    "results of ash and a corresponding bitwise shift will differ if\n"
	    "@var{n} is negative.\n"
	    "\n"
	    "Formally, the function returns an integer equivalent to\n"
	    "@code{(inexact->exact (floor (* @var{n} (expt 2 @var{cnt}))))}.\n"
	    "\n"
	    "@lisp\n"
	    "(number->string (ash #b1 3) 2)     @result{} \"1000\"\n"
	    "(number->string (ash #b1010 -1) 2) @result{} \"101\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_ash
{
  long bits_to_shift;

  SCM_VALIDATE_INUM (2, cnt);

  bits_to_shift = SCM_INUM (cnt);

  if (bits_to_shift < 0)
    {
      /* Shift right by abs(cnt) bits.  This is realized as a division
         by div:=2^abs(cnt).  However, to guarantee the floor
         rounding, negative values require some special treatment.
      */
      SCM div = scm_integer_expt (SCM_MAKINUM (2),
                                  SCM_MAKINUM (-bits_to_shift));
      if (SCM_FALSEP (scm_negative_p (n)))
        return scm_quotient (n, div);
      else
        return scm_sum (SCM_MAKINUM (-1L),
                        scm_quotient (scm_sum (SCM_MAKINUM (1L), n), div));
    }
  else
    /* Shift left is done by multiplication with 2^CNT */
    return scm_product (n, scm_integer_expt (SCM_MAKINUM (2), cnt));
}
#undef FUNC_NAME


SCM_DEFINE (scm_bit_extract, "bit-extract", 3, 0, 0,
            (SCM n, SCM start, SCM end),
	    "Return the integer composed of the @var{start} (inclusive)\n"
	    "through @var{end} (exclusive) bits of @var{n}.  The\n"
	    "@var{start}th bit becomes the 0-th bit in the result.\n"
	    "\n"
	    "@lisp\n"
	    "(number->string (bit-extract #b1101101010 0 4) 2)\n"
	    "   @result{} \"1010\"\n"
	    "(number->string (bit-extract #b1101101010 4 9) 2)\n"
	    "   @result{} \"10110\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_bit_extract
{
  unsigned long int istart, iend;
  SCM_VALIDATE_INUM_MIN_COPY (2, start,0, istart);
  SCM_VALIDATE_INUM_MIN_COPY (3, end, 0, iend);
  SCM_ASSERT_RANGE (3, end, (iend >= istart));

  if (SCM_INUMP (n))
    {
      long int in = SCM_INUM (n);
      unsigned long int bits = iend - istart;

      if (in < 0 && bits >= SCM_I_FIXNUM_BIT)
	{
	  /* Since we emulate two's complement encoded numbers, this
	   * special case requires us to produce a result that has
	   * more bits than can be stored in a fixnum.  Thus, we fall
	   * back to the more general algorithm that is used for
	   * bignums.
	   */
	  goto generalcase;
	}

      if (istart < SCM_I_FIXNUM_BIT)
	{
	  in = in >> istart;
	  if (bits < SCM_I_FIXNUM_BIT)
	    return SCM_MAKINUM (in & ((1L << bits) - 1));
	  else /* we know: in >= 0 */
	    return SCM_MAKINUM (in);
	}
      else if (in < 0)
	return SCM_MAKINUM (-1L & ((1L << bits) - 1));
      else
	return SCM_MAKINUM (0);
    }
  else if (SCM_BIGP (n))
    {
    generalcase:
      {
	SCM num1 = SCM_MAKINUM (1L);
	SCM num2 = SCM_MAKINUM (2L);
	SCM bits = SCM_MAKINUM (iend - istart);
	SCM mask  = scm_difference (scm_integer_expt (num2, bits), num1);
	return scm_logand (mask, scm_ash (n, SCM_MAKINUM (-istart)));
      }
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME

static const char scm_logtab[] = {
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
};

SCM_DEFINE (scm_logcount, "logcount", 1, 0, 0,
            (SCM n),
	    "Return the number of bits in integer @var{n}.  If integer is\n"
	    "positive, the 1-bits in its binary representation are counted.\n"
	    "If negative, the 0-bits in its two's-complement binary\n"
	    "representation are counted.  If 0, 0 is returned.\n"
	    "\n"
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
  if (SCM_INUMP (n))
    {
      unsigned long int c = 0;
      long int nn = SCM_INUM (n);
      if (nn < 0)
        nn = -1 - nn;
      while (nn)
        {
          c += scm_logtab[15 & nn];
          nn >>= 4;
        }
      return SCM_MAKINUM (c);
    }
  else if (SCM_BIGP (n))
    {
      unsigned long count;
      if (mpz_sgn (SCM_I_BIG_MPZ (n)) >= 0)
        count = mpz_popcount (SCM_I_BIG_MPZ (n));
      else
        count = mpz_hamdist (SCM_I_BIG_MPZ (n), z_negative_one);
      scm_remember_upto_here_1 (n);
      return SCM_MAKINUM (count);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME


static const char scm_ilentab[] = {
  0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4
};


SCM_DEFINE (scm_integer_length, "integer-length", 1, 0, 0,
            (SCM n),
	    "Return the number of bits necessary to represent @var{n}.\n"
	    "\n"
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
  if (SCM_INUMP (n))
    {
      unsigned long int c = 0;
      unsigned int l = 4;
      long int nn = SCM_INUM (n);
      if (nn < 0)
	nn = -1 - nn;
      while (nn)
	{
	  c += 4;
	  l = scm_ilentab [15 & nn];
	  nn >>= 4;
	}
      return SCM_MAKINUM (c - 4 + l);
    }
  else if (SCM_BIGP (n))
    {
      /* mpz_sizeinbase looks at the absolute value of negatives, whereas we
	 want a ones-complement.  If n is ...111100..00 then mpz_sizeinbase is
	 1 too big, so check for that and adjust.  */
      size_t size = mpz_sizeinbase (SCM_I_BIG_MPZ (n), 2);
      if (mpz_sgn (SCM_I_BIG_MPZ (n)) < 0
	  && mpz_scan0 (SCM_I_BIG_MPZ (n),  /* no 0 bits above the lowest 1 */
			mpz_scan1 (SCM_I_BIG_MPZ (n), 0)) == ULONG_MAX)
	size--;
      scm_remember_upto_here_1 (n);
      return SCM_MAKINUM (size);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME

/*** NUMBERS -> STRINGS ***/
int scm_dblprec;
static const double fx[] =
{  0.0,  5e-1,  5e-2,  5e-3,   5e-4, 5e-5,
  5e-6,  5e-7,  5e-8,  5e-9,  5e-10,
 5e-11, 5e-12, 5e-13, 5e-14,  5e-15,
 5e-16, 5e-17, 5e-18, 5e-19,  5e-20};

static size_t
idbl2str (double f, char *a)
{
  int efmt, dpt, d, i, wp = scm_dblprec;
  size_t ch = 0;
  int exp = 0;

  if (f == 0.0)
    {
#ifdef HAVE_COPYSIGN
      double sgn = copysign (1.0, f);

      if (sgn < 0.0)
	a[ch++] = '-';
#endif

      goto zero;	/*{a[0]='0'; a[1]='.'; a[2]='0'; return 3;} */
    }

  if (xisinf (f))
    {
      if (f < 0)
	strcpy (a, "-inf.0");
      else
	strcpy (a, "+inf.0");
      return ch+6;
    }
  else if (xisnan (f))
    {
      strcpy (a, "+nan.0");
      return ch+6;
    }

  if (f < 0.0)
    {
      f = -f;
      a[ch++] = '-';
    }

#ifdef DBL_MIN_10_EXP  /* Prevent unnormalized values, as from 
			  make-uniform-vector, from causing infinite loops. */
  while (f < 1.0)
    {
      f *= 10.0;
      if (exp-- < DBL_MIN_10_EXP)
	{
	  a[ch++] = '#';
	  a[ch++] = '.';
	  a[ch++] = '#';
	  return ch;
	}
    }
  while (f > 10.0)
    {
      f *= 0.10;
      if (exp++ > DBL_MAX_10_EXP)
	{
	  a[ch++] = '#';
	  a[ch++] = '.';
	  a[ch++] = '#';
	  return ch;
	}
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


static size_t
iflo2str (SCM flt, char *str)
{
  size_t i;
  if (SCM_REALP (flt))
    i = idbl2str (SCM_REAL_VALUE (flt), str);
  else
    {
      i = idbl2str (SCM_COMPLEX_REAL (flt), str);
      if (SCM_COMPLEX_IMAG (flt) != 0.0)
	{
	  double imag = SCM_COMPLEX_IMAG (flt);
	  /* Don't output a '+' for negative numbers or for Inf and
	     NaN.  They will provide their own sign. */
	  if (0 <= imag && !xisinf (imag) && !xisnan (imag))
	    str[i++] = '+';
	  i += idbl2str (imag, &str[i]);
	  str[i++] = 'i';
	}
    }
  return i;
}

/* convert a long to a string (unterminated).  returns the number of
   characters in the result. 
   rad is output base
   p is destination: worst case (base 2) is SCM_INTBUFLEN  */
size_t
scm_iint2str (long num, int rad, char *p)
{
  size_t j = 1;
  size_t i;
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


SCM_DEFINE (scm_number_to_string, "number->string", 1, 1, 0,
            (SCM n, SCM radix),
	    "Return a string holding the external representation of the\n"
	    "number @var{n} in the given @var{radix}.  If @var{n} is\n"
	    "inexact, a radix of 10 will be used.")
#define FUNC_NAME s_scm_number_to_string
{
  int base;

  if (SCM_UNBNDP (radix))
    base = 10;
  else
    {
      SCM_VALIDATE_INUM (2, radix);
      base = SCM_INUM (radix);
      /* FIXME: ask if range limit was OK, and if so, document */
      SCM_ASSERT_RANGE (2, radix, (base >= 2) && (base <= 36));
    }

  if (SCM_INUMP (n))
    {
      char num_buf [SCM_INTBUFLEN];
      size_t length = scm_iint2str (SCM_INUM (n), base, num_buf);
      return scm_mem2string (num_buf, length);
    }
  else if (SCM_BIGP (n))
    {
      char *str = mpz_get_str (NULL, base, SCM_I_BIG_MPZ (n));
      scm_remember_upto_here_1 (n);
      return scm_take0str (str);
    }
  else if (SCM_INEXACTP (n))
    {
      char num_buf [FLOBUFLEN];
      return scm_mem2string (num_buf, iflo2str (n, num_buf));
    }
  else
    SCM_WRONG_TYPE_ARG (1, n);
}
#undef FUNC_NAME


/* These print routines used to be stubbed here so that scm_repl.c
   wouldn't need SCM_BIGDIG conditionals (pre GMP) */

int
scm_print_real (SCM sexp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  char num_buf[FLOBUFLEN];
  scm_lfwrite (num_buf, iflo2str (sexp, num_buf), port);
  return !0;
}

int
scm_print_complex (SCM sexp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  char num_buf[FLOBUFLEN];
  scm_lfwrite (num_buf, iflo2str (sexp, num_buf), port);
  return !0;
}

int
scm_bigprint (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  char *str = mpz_get_str (NULL, 10, SCM_I_BIG_MPZ (exp));
  scm_remember_upto_here_1 (exp);
  scm_lfwrite (str, (size_t) strlen (str), port);
  free (str);
  return !0;
}
/*** END nums->strs ***/


/*** STRINGS -> NUMBERS ***/

/* The following functions implement the conversion from strings to numbers.
 * The implementation somehow follows the grammar for numbers as it is given
 * in R5RS.  Thus, the functions resemble syntactic units (<ureal R>,
 * <uinteger R>, ...) that are used to build up numbers in the grammar.  Some
 * points should be noted about the implementation:
 * * Each function keeps a local index variable 'idx' that points at the
 * current position within the parsed string.  The global index is only
 * updated if the function could parse the corresponding syntactic unit
 * successfully.
 * * Similarly, the functions keep track of indicators of inexactness ('#',
 * '.' or exponents) using local variables ('hash_seen', 'x').  Again, the
 * global exactness information is only updated after each part has been
 * successfully parsed.
 * * Sequences of digits are parsed into temporary variables holding fixnums.
 * Only if these fixnums would overflow, the result variables are updated
 * using the standard functions scm_add, scm_product, scm_divide etc.  Then,
 * the temporary variables holding the fixnums are cleared, and the process
 * starts over again.  If for example fixnums were able to store five decimal
 * digits, a number 1234567890 would be parsed in two parts 12345 and 67890,
 * and the result was computed as 12345 * 100000 + 67890.  In other words,
 * only every five digits two bignum operations were performed.
 */

enum t_exactness {NO_EXACTNESS, INEXACT, EXACT};

/* R5RS, section 7.1.1, lexical structure of numbers: <uinteger R>. */

/* In non ASCII-style encodings the following macro might not work. */
#define XDIGIT2UINT(d) (isdigit (d) ? (d) - '0' : tolower (d) - 'a' + 10)

static SCM
mem2uinteger (const char* mem, size_t len, unsigned int *p_idx,
	      unsigned int radix, enum t_exactness *p_exactness)
{
  unsigned int idx = *p_idx;
  unsigned int hash_seen = 0;
  scm_t_bits shift = 1;
  scm_t_bits add = 0;
  unsigned int digit_value;
  SCM result;
  char c;

  if (idx == len)
    return SCM_BOOL_F;

  c = mem[idx];
  if (!isxdigit (c))
    return SCM_BOOL_F;
  digit_value = XDIGIT2UINT (c);
  if (digit_value >= radix)
    return SCM_BOOL_F;

  idx++;
  result = SCM_MAKINUM (digit_value);
  while (idx != len)
    {
      char c = mem[idx];
      if (isxdigit (c))
	{
	  if (hash_seen)
	    break;
	  digit_value = XDIGIT2UINT (c);
	  if (digit_value >= radix)
	    break;
	}
      else if (c == '#')
	{
	  hash_seen = 1;
	  digit_value = 0;
	}
      else
	break;

      idx++;
      if (SCM_MOST_POSITIVE_FIXNUM / radix < shift)
	{
	  result = scm_product (result, SCM_MAKINUM (shift));
	  if (add > 0)
	    result = scm_sum (result, SCM_MAKINUM (add));

	  shift = radix;
	  add = digit_value;
	}
      else
	{
	  shift = shift * radix;
	  add = add * radix + digit_value;
	}
    };

  if (shift > 1)
    result = scm_product (result, SCM_MAKINUM (shift));
  if (add > 0)
    result = scm_sum (result, SCM_MAKINUM (add));

  *p_idx = idx;
  if (hash_seen)
    *p_exactness = INEXACT;

  return result;
}


/* R5RS, section 7.1.1, lexical structure of numbers: <decimal 10>.  Only
 * covers the parts of the rules that start at a potential point.  The value
 * of the digits up to the point have been parsed by the caller and are given
 * in variable result.  The content of *p_exactness indicates, whether a hash
 * has already been seen in the digits before the point.
 */

/* In non ASCII-style encodings the following macro might not work. */
#define DIGIT2UINT(d) ((d) - '0')

static SCM
mem2decimal_from_point (SCM result, const char* mem, size_t len, 
			unsigned int *p_idx, enum t_exactness *p_exactness)
{
  unsigned int idx = *p_idx;
  enum t_exactness x = *p_exactness;

  if (idx == len)
    return result;

  if (mem[idx] == '.')
    {
      scm_t_bits shift = 1;
      scm_t_bits add = 0;
      unsigned int digit_value;
      SCM big_shift = SCM_MAKINUM (1);

      idx++;
      while (idx != len)
	{
	  char c = mem[idx];
	  if (isdigit (c))
	    {
	      if (x == INEXACT)
		return SCM_BOOL_F;
	      else
		digit_value = DIGIT2UINT (c);
	    }
	  else if (c == '#')
	    {
	      x = INEXACT;
	      digit_value = 0;
	    }
	  else
	    break;

	  idx++;
	  if (SCM_MOST_POSITIVE_FIXNUM / 10 < shift)
	    {
	      big_shift = scm_product (big_shift, SCM_MAKINUM (shift));
	      result = scm_product (result, SCM_MAKINUM (shift));
	      if (add > 0)
		result = scm_sum (result, SCM_MAKINUM (add));
	      
	      shift = 10;
	      add = digit_value;
	    }
	  else
	    {
	      shift = shift * 10;
	      add = add * 10 + digit_value;
	    }
	};

      if (add > 0)
	{
	  big_shift = scm_product (big_shift, SCM_MAKINUM (shift));
	  result = scm_product (result, SCM_MAKINUM (shift));
	  result = scm_sum (result, SCM_MAKINUM (add));
	}

      result = scm_divide (result, big_shift);

      /* We've seen a decimal point, thus the value is implicitly inexact. */
      x = INEXACT;
    }

  if (idx != len)
    {
      int sign = 1;
      unsigned int start;
      char c;
      int exponent;
      SCM e;

      /* R5RS, section 7.1.1, lexical structure of numbers: <suffix> */

      switch (mem[idx])
	{
	case 'd': case 'D':
	case 'e': case 'E':
	case 'f': case 'F':
	case 'l': case 'L':
	case 's': case 'S':
	  idx++;
	  start = idx;
	  c = mem[idx];
	  if (c == '-')
	    {
	      idx++;
	      sign = -1;
	      c = mem[idx];
	    }
	  else if (c == '+')
	    {
	      idx++;
	      sign = 1;
	      c = mem[idx];
	    }
	  else
	    sign = 1;

	  if (!isdigit (c))
	    return SCM_BOOL_F;

	  idx++;
	  exponent = DIGIT2UINT (c);
	  while (idx != len)
	    {
	      char c = mem[idx];
	      if (isdigit (c))
		{
		  idx++;
		  if (exponent <= SCM_MAXEXP)
		    exponent = exponent * 10 + DIGIT2UINT (c);
		}
	      else
		break;
	    }

	  if (exponent > SCM_MAXEXP)
	    {
	      size_t exp_len = idx - start;
	      SCM exp_string = scm_mem2string (&mem[start], exp_len);
	      SCM exp_num = scm_string_to_number (exp_string, SCM_UNDEFINED);
	      scm_out_of_range ("string->number", exp_num);
	    }

	  e = scm_integer_expt (SCM_MAKINUM (10), SCM_MAKINUM (exponent));
	  if (sign == 1)
	    result = scm_product (result, e);
	  else
	    result = scm_divide (result, e);

	  /* We've seen an exponent, thus the value is implicitly inexact. */
	  x = INEXACT;

	  break;

	default:
	  break;
	}
    }

  *p_idx = idx;
  if (x == INEXACT)
    *p_exactness = x;

  return result;
}


/* R5RS, section 7.1.1, lexical structure of numbers: <ureal R> */

static SCM
mem2ureal (const char* mem, size_t len, unsigned int *p_idx,
	   unsigned int radix, enum t_exactness *p_exactness)
{
  unsigned int idx = *p_idx;
  SCM result;

  if (idx == len)
    return SCM_BOOL_F;

  if (idx+5 <= len && !strncmp (mem+idx, "inf.0", 5))
    {
      *p_idx = idx+5;
      return scm_inf ();
    }

  if (idx+4 < len && !strncmp (mem+idx, "nan.", 4))
    {
      enum t_exactness x = EXACT;

      /* Cobble up the fraction.  We might want to set the NaN's
	 mantissa from it. */
      idx += 4;
      mem2uinteger (mem, len, &idx, 10, &x);
      *p_idx = idx;
      return scm_nan ();
    }

  if (mem[idx] == '.')
    {
      if (radix != 10)
	return SCM_BOOL_F;
      else if (idx + 1 == len)
	return SCM_BOOL_F;
      else if (!isdigit (mem[idx + 1]))
	return SCM_BOOL_F;
      else
	result = mem2decimal_from_point (SCM_MAKINUM (0), mem, len,
					 p_idx, p_exactness);
    }
  else
    {
      enum t_exactness x = EXACT;
      SCM uinteger;

      uinteger = mem2uinteger (mem, len, &idx, radix, &x);
      if (SCM_FALSEP (uinteger))
	return SCM_BOOL_F;

      if (idx == len)
	result = uinteger;
      else if (mem[idx] == '/')
	{
	  SCM divisor;

	  idx++;

	  divisor = mem2uinteger (mem, len, &idx, radix, &x);
	  if (SCM_FALSEP (divisor))
	    return SCM_BOOL_F;

	  result = scm_divide (uinteger, divisor);
	}
      else if (radix == 10)
	{
	  result = mem2decimal_from_point (uinteger, mem, len, &idx, &x);
	  if (SCM_FALSEP (result))
	    return SCM_BOOL_F;
	}
      else
	result = uinteger;

      *p_idx = idx;
      if (x == INEXACT)
	*p_exactness = x;
    }

  /* When returning an inexact zero, make sure it is represented as a
     floating point value so that we can change its sign. 
  */
  if (SCM_EQ_P (result, SCM_MAKINUM(0)) && *p_exactness == INEXACT)
    result = scm_make_real (0.0);

  return result;
}


/* R5RS, section 7.1.1, lexical structure of numbers: <complex R> */

static SCM
mem2complex (const char* mem, size_t len, unsigned int idx,
	     unsigned int radix, enum t_exactness *p_exactness)
{
  char c;
  int sign = 0;
  SCM ureal;

  if (idx == len)
    return SCM_BOOL_F;

  c = mem[idx];
  if (c == '+')
    {
      idx++;
      sign = 1;
    }
  else if (c == '-')
    {
      idx++;
      sign = -1;
    }

  if (idx == len)
    return SCM_BOOL_F;

  ureal = mem2ureal (mem, len, &idx, radix, p_exactness);
  if (SCM_FALSEP (ureal))
    {
      /* input must be either +i or -i */

      if (sign == 0)
	return SCM_BOOL_F;

      if (mem[idx] == 'i' || mem[idx] == 'I')
	{
	  idx++;
	  if (idx != len)
	    return SCM_BOOL_F;
	  
	  return scm_make_rectangular (SCM_MAKINUM (0), SCM_MAKINUM (sign));
	}
      else
	return SCM_BOOL_F;
    }
  else
    {
      if (sign == -1 && SCM_FALSEP (scm_nan_p (ureal)))
	ureal = scm_difference (ureal, SCM_UNDEFINED);

      if (idx == len)
	return ureal;

      c = mem[idx];
      switch (c)
	{
	case 'i': case 'I':
	  /* either +<ureal>i or -<ureal>i */

	  idx++;
	  if (sign == 0)
	    return SCM_BOOL_F;
	  if (idx != len)
	    return SCM_BOOL_F;
	  return scm_make_rectangular (SCM_MAKINUM (0), ureal);

	case '@':
	  /* polar input: <real>@<real>. */

	  idx++;
	  if (idx == len)
	    return SCM_BOOL_F;
	  else
	    {
	      int sign;
	      SCM angle;
	      SCM result;

	      c = mem[idx];
	      if (c == '+')
		{
		  idx++;
		  sign = 1;
		}
	      else if (c == '-')
		{
		  idx++;
		  sign = -1;
		}
	      else
		sign = 1;

	      angle = mem2ureal (mem, len, &idx, radix, p_exactness);
	      if (SCM_FALSEP (angle))
		return SCM_BOOL_F;
	      if (idx != len)
		return SCM_BOOL_F;

	      if (sign == -1 && SCM_FALSEP (scm_nan_p (ureal)))
		angle = scm_difference (angle, SCM_UNDEFINED);

	      result = scm_make_polar (ureal, angle);
	      return result;
	    }
	case '+':
	case '-':
	  /* expecting input matching <real>[+-]<ureal>?i */

	  idx++;
	  if (idx == len)
	    return SCM_BOOL_F;
	  else
	    {
	      int sign = (c == '+') ? 1 : -1;
	      SCM imag = mem2ureal (mem, len, &idx, radix, p_exactness);

	      if (SCM_FALSEP (imag))
		imag = SCM_MAKINUM (sign);
	      else if (sign == -1 && SCM_FALSEP (scm_nan_p (ureal)))
		imag = scm_difference (imag, SCM_UNDEFINED);

	      if (idx == len)
		return SCM_BOOL_F;
	      if (mem[idx] != 'i' && mem[idx] != 'I')
		return SCM_BOOL_F;

	      idx++;
	      if (idx != len)
		return SCM_BOOL_F;

	      return scm_make_rectangular (ureal, imag);
	    }
	default:
	  return SCM_BOOL_F;
	}
    }
}


/* R5RS, section 7.1.1, lexical structure of numbers: <number> */

enum t_radix {NO_RADIX=0, DUAL=2, OCT=8, DEC=10, HEX=16};

SCM
scm_i_mem2number (const char* mem, size_t len, unsigned int default_radix)
{
  unsigned int idx = 0;
  unsigned int radix = NO_RADIX;
  enum t_exactness forced_x = NO_EXACTNESS;
  enum t_exactness implicit_x = EXACT;
  SCM result;

  /* R5RS, section 7.1.1, lexical structure of numbers: <prefix R> */
  while (idx + 2 < len && mem[idx] == '#')
    {
      switch (mem[idx + 1])
	{
	case 'b': case 'B':
	  if (radix != NO_RADIX)
	    return SCM_BOOL_F;
	  radix = DUAL;
	  break;
	case 'd': case 'D':
	  if (radix != NO_RADIX)
	    return SCM_BOOL_F;
	  radix = DEC;
	  break;
	case 'i': case 'I':
	  if (forced_x != NO_EXACTNESS)
	    return SCM_BOOL_F;
	  forced_x = INEXACT;
	  break;
	case 'e': case 'E':
	  if (forced_x != NO_EXACTNESS)
	    return SCM_BOOL_F;
	  forced_x = EXACT;
	  break;
	case 'o': case 'O':
	  if (radix != NO_RADIX)
	    return SCM_BOOL_F;
	  radix = OCT;
	  break;
	case 'x': case 'X':
	  if (radix != NO_RADIX)
	    return SCM_BOOL_F;
	  radix = HEX;
	  break;
	default:
	  return SCM_BOOL_F;
	}
      idx += 2;
    }

  /* R5RS, section 7.1.1, lexical structure of numbers: <complex R> */
  if (radix == NO_RADIX)
    result = mem2complex (mem, len, idx, default_radix, &implicit_x);
  else
    result = mem2complex (mem, len, idx, (unsigned int) radix, &implicit_x);

  if (SCM_FALSEP (result))
    return SCM_BOOL_F;

  switch (forced_x)
    {
    case EXACT:
      if (SCM_INEXACTP (result))
	/* FIXME: This may change the value. */
	return scm_inexact_to_exact (result);
      else
	return result;
    case INEXACT:
      if (SCM_INEXACTP (result))
	return result;
      else
	return scm_exact_to_inexact (result);
    case NO_EXACTNESS:
    default:
      if (implicit_x == INEXACT)
	{
	  if (SCM_INEXACTP (result))
	    return result;
	  else
	    return scm_exact_to_inexact (result);
	}
      else
	return result;
    }
}


SCM_DEFINE (scm_string_to_number, "string->number", 1, 1, 0,
            (SCM string, SCM radix),
	    "Return a number of the maximally precise representation\n"
	    "expressed by the given @var{string}. @var{radix} must be an\n"
	    "exact integer, either 2, 8, 10, or 16. If supplied, @var{radix}\n"
	    "is a default radix that may be overridden by an explicit radix\n"
	    "prefix in @var{string} (e.g. \"#o177\"). If @var{radix} is not\n"
	    "supplied, then the default radix is 10. If string is not a\n"
	    "syntactically valid notation for a number, then\n"
	    "@code{string->number} returns @code{#f}.") 
#define FUNC_NAME s_scm_string_to_number
{
  SCM answer;
  int base;
  SCM_VALIDATE_STRING (1, string);
  SCM_VALIDATE_INUM_MIN_DEF_COPY (2, radix,2,10, base);
  answer = scm_i_mem2number (SCM_STRING_CHARS (string),
			   SCM_STRING_LENGTH (string),
			   base);
  return scm_return_first (answer, string);
}
#undef FUNC_NAME


/*** END strs->nums ***/


SCM
scm_make_real (double x)
{
  SCM z = scm_double_cell (scm_tc16_real, 0, 0, 0);

  SCM_REAL_VALUE (z) = x;
  return z;
}


SCM
scm_make_complex (double x, double y)
{
  if (y == 0.0)
    return scm_make_real (x);
  else
    {
      SCM z;
      SCM_NEWSMOB (z, scm_tc16_complex, scm_gc_malloc (2*sizeof (double),
						       "complex"));
      SCM_COMPLEX_REAL (z) = x;
      SCM_COMPLEX_IMAG (z) = y;
      return z;
    }
}


SCM
scm_bigequal (SCM x, SCM y)
{
  int result = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (x));
  scm_remember_upto_here_2 (x, y);
  return SCM_BOOL (0 == result);
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
	    "otherwise.  Note that the sets of real, rational and integer\n"
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
	    "otherwise.  Note that the set of integer values forms a subset of\n"
	    "the set of rational numbers, i. e. the predicate will also be\n"
	    "fulfilled if @var{x} is an integer number.  Real numbers\n"
	    "will also satisfy this predicate, because of their limited\n"
	    "precision.")
#define FUNC_NAME s_scm_real_p
{
  if (SCM_INUMP (x))
    return SCM_BOOL_T;
  else if (SCM_IMP (x))
    return SCM_BOOL_F;
  else if (SCM_REALP (x))
    return SCM_BOOL_T;
  else if (SCM_BIGP (x))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
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
  if (!SCM_INEXACTP (x))
    return SCM_BOOL_F;
  if (SCM_COMPLEXP (x))
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
  if (SCM_INUMP (x))
    {
      long xx = SCM_INUM (x);
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  return SCM_BOOL (xx == yy);
	}
      else if (SCM_BIGP (y))
	return SCM_BOOL_F;
      else if (SCM_REALP (y))
	return SCM_BOOL ((double) xx == SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return SCM_BOOL (((double) xx == SCM_COMPLEX_REAL (y))
			 && (0.0 == SCM_COMPLEX_IMAG (y)));
      else
	SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	return SCM_BOOL_F;
      else if (SCM_BIGP (y))
	{
	  int cmp = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return SCM_BOOL (0 == cmp);
	}
      else if (SCM_REALP (y))
	{
	  int cmp;
	  if (xisnan (SCM_REAL_VALUE (y)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), SCM_REAL_VALUE (y));
	  scm_remember_upto_here_1 (x);
	  return SCM_BOOL (0 == cmp);
	}
      else if (SCM_COMPLEXP (y))
	{
	  int cmp;
	  if (0.0 != SCM_COMPLEX_IMAG (y))
	    return SCM_BOOL_F;
	  if (xisnan (SCM_COMPLEX_REAL (y)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), SCM_COMPLEX_REAL (y));
	  scm_remember_upto_here_1 (x);
	  return SCM_BOOL (0 == cmp);
	}
      else
	SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_INUMP (y))
	return SCM_BOOL (SCM_REAL_VALUE (x) == (double) SCM_INUM (y));
      else if (SCM_BIGP (y))
	{
	  int cmp;
	  if (xisnan (SCM_REAL_VALUE (x)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), SCM_REAL_VALUE (x));
	  scm_remember_upto_here_1 (y);
	  return SCM_BOOL (0 == cmp);
	}
      else if (SCM_REALP (y))
	return SCM_BOOL (SCM_REAL_VALUE (x) == SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return SCM_BOOL ((SCM_REAL_VALUE (x) == SCM_COMPLEX_REAL (y))
			 && (0.0 == SCM_COMPLEX_IMAG (y)));
      else
	SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_INUMP (y))
	return SCM_BOOL ((SCM_COMPLEX_REAL (x) == (double) SCM_INUM (y))
			 && (SCM_COMPLEX_IMAG (x) == 0.0));
      else if (SCM_BIGP (y))
	{
	  int cmp;
	  if (0.0 != SCM_COMPLEX_IMAG (x))
	    return SCM_BOOL_F;
	  if (xisnan (SCM_COMPLEX_REAL (x)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), SCM_COMPLEX_REAL (x));
	  scm_remember_upto_here_1 (y);
	  return SCM_BOOL (0 == cmp);
	}
      else if (SCM_REALP (y))
	return SCM_BOOL ((SCM_COMPLEX_REAL (x) == SCM_REAL_VALUE (y))
			 && (SCM_COMPLEX_IMAG (x) == 0.0));
      else if (SCM_COMPLEXP (y))
	return SCM_BOOL ((SCM_COMPLEX_REAL (x) == SCM_COMPLEX_REAL (y))
			 && (SCM_COMPLEX_IMAG (x) == SCM_COMPLEX_IMAG (y)));
      else
	SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARGn, s_eq_p);
    }
  else
    SCM_WTA_DISPATCH_2 (g_eq_p, x, y, SCM_ARG1, s_eq_p);
}


SCM_GPROC1 (s_less_p, "<", scm_tc7_rpsubr, scm_less_p, g_less_p);
/* "Return @code{#t} if the list of parameters is monotonically\n"
 * "increasing."
 */
SCM
scm_less_p (SCM x, SCM y)
{
  if (SCM_INUMP (x))
    {
      long xx = SCM_INUM (x);
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  return SCM_BOOL (xx < yy);
	}
      else if (SCM_BIGP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return SCM_BOOL (sgn > 0);
	}
      else if (SCM_REALP (y))
	return SCM_BOOL ((double) xx < SCM_REAL_VALUE (y));
      else
	SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARGn, s_less_p);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
	  scm_remember_upto_here_1 (x);
	  return SCM_BOOL (sgn < 0);
	}
      else if (SCM_BIGP (y))
	{
	  int cmp = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return SCM_BOOL (cmp < 0);
	}
      else if (SCM_REALP (y))
	{
	  int cmp;
	  if (xisnan (SCM_REAL_VALUE (y)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), SCM_REAL_VALUE (y));
	  scm_remember_upto_here_1 (x);
	  return SCM_BOOL (cmp < 0);
	}
      else
	SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARGn, s_less_p);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_INUMP (y))
	return SCM_BOOL (SCM_REAL_VALUE (x) < (double) SCM_INUM (y));
      else if (SCM_BIGP (y))
	{
	  int cmp;
	  if (xisnan (SCM_REAL_VALUE (x)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), SCM_REAL_VALUE (x));
	  scm_remember_upto_here_1 (y);
	  return SCM_BOOL (cmp > 0);
	}
      else if (SCM_REALP (y))
	return SCM_BOOL (SCM_REAL_VALUE (x) < SCM_REAL_VALUE (y));
      else
	SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARGn, s_less_p);
    }
  else
    SCM_WTA_DISPATCH_2 (g_less_p, x, y, SCM_ARG1, s_less_p);
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
  else if (SCM_NFALSEP (scm_nan_p (x)) || SCM_NFALSEP (scm_nan_p (y)))
    return SCM_BOOL_F;
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
  else if (SCM_NFALSEP (scm_nan_p (x)) || SCM_NFALSEP (scm_nan_p (y)))
    return SCM_BOOL_F;
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
  if (SCM_INUMP (z))
    return SCM_BOOL (SCM_EQ_P (z, SCM_INUM0));
  else if (SCM_BIGP (z))
    return SCM_BOOL_F;
  else if (SCM_REALP (z))
    return SCM_BOOL (SCM_REAL_VALUE (z) == 0.0);
  else if (SCM_COMPLEXP (z))
    return SCM_BOOL (SCM_COMPLEX_REAL (z) == 0.0
		     && SCM_COMPLEX_IMAG (z) == 0.0);
  else
    SCM_WTA_DISPATCH_1 (g_zero_p, z, SCM_ARG1, s_zero_p);
}


SCM_GPROC (s_positive_p, "positive?", 1, 0, 0, scm_positive_p, g_positive_p);
/* "Return @code{#t} if @var{x} is an exact or inexact number greater than\n"
 * "zero."
 */
SCM
scm_positive_p (SCM x)
{
  if (SCM_INUMP (x))
    return SCM_BOOL (SCM_INUM (x) > 0);
  else if (SCM_BIGP (x))
    {
      int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
      scm_remember_upto_here_1 (x);
      return SCM_BOOL (sgn > 0);
    }
  else if (SCM_REALP (x))
    return SCM_BOOL(SCM_REAL_VALUE (x) > 0.0);
  else
    SCM_WTA_DISPATCH_1 (g_positive_p, x, SCM_ARG1, s_positive_p);
}


SCM_GPROC (s_negative_p, "negative?", 1, 0, 0, scm_negative_p, g_negative_p);
/* "Return @code{#t} if @var{x} is an exact or inexact number less than\n"
 * "zero."
 */
SCM
scm_negative_p (SCM x)
{
  if (SCM_INUMP (x))
    return SCM_BOOL (SCM_INUM (x) < 0);
  else if (SCM_BIGP (x))
    {
      int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
      scm_remember_upto_here_1 (x);
      return SCM_BOOL (sgn < 0);
    }
  else if (SCM_REALP (x))
    return SCM_BOOL(SCM_REAL_VALUE (x) < 0.0);
  else
    SCM_WTA_DISPATCH_1 (g_negative_p, x, SCM_ARG1, s_negative_p);
}


SCM_GPROC1 (s_max, "max", scm_tc7_asubr, scm_max, g_max);
/* "Return the maximum of all parameter values."
 */
SCM
scm_max (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_UNBNDP (x))
	SCM_WTA_DISPATCH_0 (g_max, s_max);
      else if (SCM_NUMBERP (x))
	return x;
      else
	SCM_WTA_DISPATCH_1 (g_max, x, SCM_ARG1, s_max);
    }
  
  if (SCM_INUMP (x))
    {
      long xx = SCM_INUM (x);
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  return (xx < yy) ? y : x;
	}
      else if (SCM_BIGP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return (sgn < 0) ? x : y;
	}
      else if (SCM_REALP (y))
	{
	  double z = xx;
	  /* if y==NaN then ">" is false and we return NaN */
	  return (z > SCM_REAL_VALUE (y)) ? scm_make_real (z) : y;
	}
      else
	SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
	  scm_remember_upto_here_1 (x);
	  return (sgn < 0) ? y : x;
	}
      else if (SCM_BIGP (y))
	{
	  int cmp = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return (cmp > 0) ? x : y;
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
	  int cmp;
	  if (xisnan (yy))
	    return y;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), yy);
	  scm_remember_upto_here_1 (x);
	  return (cmp > 0) ? x : y;
	}
      else
	SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_INUMP (y))
	{
	  double z = SCM_INUM (y);
	  /* if x==NaN then "<" is false and we return NaN */
	  return (SCM_REAL_VALUE (x) < z) ? scm_make_real (z) : x;
	}
      else if (SCM_BIGP (y))
	{
	  double xx = SCM_REAL_VALUE (x);
	  int cmp;
	  if (xisnan (xx))
	    return x;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), xx);
	  scm_remember_upto_here_1 (y);
	  return (cmp < 0) ? x : y;
	}
      else if (SCM_REALP (y))
	{
	  /* if x==NaN then our explicit check means we return NaN
	     if y==NaN then ">" is false and we return NaN
	     calling isnan is unavoidable, since it's the only way to know
	     which of x or y causes any compares to be false */
	  double xx = SCM_REAL_VALUE (x);
	  return (xisnan (xx) || xx > SCM_REAL_VALUE (y)) ? x : y;
	}
      else
	SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  else
    SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARG1, s_max);
}


SCM_GPROC1 (s_min, "min", scm_tc7_asubr, scm_min, g_min);
/* "Return the minium of all parameter values."
 */
SCM
scm_min (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_UNBNDP (x))
	SCM_WTA_DISPATCH_0 (g_min, s_min);
      else if (SCM_NUMBERP (x))
	return x;
      else
	SCM_WTA_DISPATCH_1 (g_min, x, SCM_ARG1, s_min);
    }
  
  if (SCM_INUMP (x))
    {
      long xx = SCM_INUM (x);
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  return (xx < yy) ? x : y;
	}
      else if (SCM_BIGP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return (sgn < 0) ? y : x;
	}
      else if (SCM_REALP (y))
	{
	  double z = xx;
	  /* if y==NaN then "<" is false and we return NaN */
	  return (z < SCM_REAL_VALUE (y)) ? scm_make_real (z) : y;
	}
      else
	SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
	  scm_remember_upto_here_1 (x);
	  return (sgn < 0) ? x : y;
	}
      else if (SCM_BIGP (y))
	{
	  int cmp = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return (cmp > 0) ? y : x;
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
	  int cmp;
	  if (xisnan (yy))
	    return y;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), yy);
	  scm_remember_upto_here_1 (x);
	  return (cmp > 0) ? y : x;
	}
      else
	SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_INUMP (y))
	{
	  double z = SCM_INUM (y);
	  /* if x==NaN then "<" is false and we return NaN */
	  return (z < SCM_REAL_VALUE (x)) ? scm_make_real (z) : x;
	}
      else if (SCM_BIGP (y))
	{
	  double xx = SCM_REAL_VALUE (x);
	  int cmp;
	  if (xisnan (xx))
	    return x;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), xx);
	  scm_remember_upto_here_1 (y);
	  return (cmp < 0) ? y : x;
	}
      else if (SCM_REALP (y))
	{
	  /* if x==NaN then our explicit check means we return NaN
	     if y==NaN then "<" is false and we return NaN
	     calling isnan is unavoidable, since it's the only way to know
	     which of x or y causes any compares to be false */
	  double xx = SCM_REAL_VALUE (x);
	  return (xisnan (xx) || xx < SCM_REAL_VALUE (y)) ? x : y;
	}
      else
	SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  else
    SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARG1, s_min);
}


SCM_GPROC1 (s_sum, "+", scm_tc7_asubr, scm_sum, g_sum);
/* "Return the sum of all parameter values.  Return 0 if called without\n"
 * "any parameters." 
 */
SCM
scm_sum (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_NUMBERP (x)) return x;
      if (SCM_UNBNDP (x)) return SCM_INUM0;
      SCM_WTA_DISPATCH_1 (g_sum, x, SCM_ARG1, s_sum);
    }

  if (SCM_INUMP (x))
    {
      if (SCM_INUMP (y))
        {
          long xx = SCM_INUM (x);
          long yy = SCM_INUM (y);
          long int z = xx + yy;
          return SCM_FIXABLE (z) ? SCM_MAKINUM (z) : scm_i_long2big (z);
        }
      else if (SCM_BIGP (y))
        {
          SCM_SWAP (x, y);
          goto add_big_inum;
        }
      else if (SCM_REALP (y))
        {
          long int xx = SCM_INUM (x);
          return scm_make_real (xx + SCM_REAL_VALUE (y));
        }
      else if (SCM_COMPLEXP (y))
        {
          long int xx = SCM_INUM (x);
          return scm_make_complex (xx + SCM_COMPLEX_REAL (y),
                                   SCM_COMPLEX_IMAG (y));
        }
      else
        SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    } else if (SCM_BIGP (x))
      {
	if (SCM_INUMP (y))
	  {
	    long int inum;
	    int bigsgn;
	  add_big_inum:
	    inum = SCM_INUM (y);      
	    if (inum == 0)
	      return x;
	    bigsgn = mpz_sgn (SCM_I_BIG_MPZ (x));
	    if (inum < 0)
	      {
		SCM result = scm_i_mkbig ();
		mpz_sub_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (x), - inum);
		scm_remember_upto_here_1 (x);
		/* we know the result will have to be a bignum */
		if (bigsgn == -1)
		  return result;
		return scm_i_normbig (result);
	      }
	    else
	      {
		SCM result = scm_i_mkbig ();
		mpz_add_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (x), inum);
		scm_remember_upto_here_1 (x);
		/* we know the result will have to be a bignum */
		if (bigsgn == 1)
		  return result;
		return scm_i_normbig (result);        
	      }
	  }
	else if (SCM_BIGP (y))
	  {
	    SCM result = scm_i_mkbig ();
	    int sgn_x = mpz_sgn (SCM_I_BIG_MPZ (x)); 
	    int sgn_y = mpz_sgn (SCM_I_BIG_MPZ (y)); 
	    mpz_add (SCM_I_BIG_MPZ (result),
		     SCM_I_BIG_MPZ (x),
		     SCM_I_BIG_MPZ (y));
	    scm_remember_upto_here_2 (x, y);
	    /* we know the result will have to be a bignum */
	    if (sgn_x == sgn_y)
	      return result;
	    return scm_i_normbig (result);
	  }
	else if (SCM_REALP (y))
	  {
	    double result = mpz_get_d (SCM_I_BIG_MPZ (x)) + SCM_REAL_VALUE (y);
	    scm_remember_upto_here_1 (x);
	    return scm_make_real (result);
	  }
	else if (SCM_COMPLEXP (y))
	  {
	    double real_part = (mpz_get_d (SCM_I_BIG_MPZ (x))
				+ SCM_COMPLEX_REAL (y));
	    scm_remember_upto_here_1 (x);
	    return scm_make_complex (real_part, SCM_COMPLEX_IMAG (y));
	  }
	else
	  SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
      }
  else if (SCM_REALP (x))
    {
      if (SCM_INUMP (y))
	return scm_make_real (SCM_REAL_VALUE (x) + SCM_INUM (y));
      else if (SCM_BIGP (y))
	{
	  double result = mpz_get_d (SCM_I_BIG_MPZ (y)) + SCM_REAL_VALUE (x);
	  scm_remember_upto_here_1 (y);
	  return scm_make_real (result);
	}
      else if (SCM_REALP (y))
	return scm_make_real (SCM_REAL_VALUE (x) + SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_make_complex (SCM_REAL_VALUE (x) + SCM_COMPLEX_REAL (y),
				 SCM_COMPLEX_IMAG (y));
      else
	SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_INUMP (y))
	return scm_make_complex (SCM_COMPLEX_REAL (x) + SCM_INUM (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_BIGP (y))
	{
	  double real_part = (mpz_get_d (SCM_I_BIG_MPZ (y))
			      + SCM_COMPLEX_REAL (x));
	  scm_remember_upto_here_1 (y);
	  return scm_make_complex (real_part, SCM_COMPLEX_IMAG (x));
	}
      else if (SCM_REALP (y))
	return scm_make_complex (SCM_COMPLEX_REAL (x) + SCM_REAL_VALUE (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_COMPLEXP (y))
	return scm_make_complex (SCM_COMPLEX_REAL (x) + SCM_COMPLEX_REAL (y),
				 SCM_COMPLEX_IMAG (x) + SCM_COMPLEX_IMAG (y));
      else
	SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  else
    SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARG1, s_sum);
}


SCM_GPROC1 (s_difference, "-", scm_tc7_asubr, scm_difference, g_difference);
/* If called with one argument @var{z1}, -@var{z1} returned. Otherwise
 * the sum of all but the first argument are subtracted from the first
 * argument.  */
#define FUNC_NAME s_difference
SCM
scm_difference (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_UNBNDP (x))
        SCM_WTA_DISPATCH_0 (g_difference, s_difference);
      else 
        if (SCM_INUMP (x))
          {
            long xx = -SCM_INUM (x);
            if (SCM_FIXABLE (xx))
              return SCM_MAKINUM (xx);
            else
              return scm_i_long2big (xx);
          }
        else if (SCM_BIGP (x))
          /* FIXME: do we really need to normalize here? */
          return scm_i_normbig (scm_i_clonebig (x, 0));
        else if (SCM_REALP (x))
          return scm_make_real (-SCM_REAL_VALUE (x));
        else if (SCM_COMPLEXP (x))
          return scm_make_complex (-SCM_COMPLEX_REAL (x),
                                   -SCM_COMPLEX_IMAG (x));
        else
          SCM_WTA_DISPATCH_1 (g_difference, x, SCM_ARG1, s_difference);
    }
  
  if (SCM_INUMP (x))
    {
      if (SCM_INUMP (y))
	{
	  long int xx = SCM_INUM (x);
	  long int yy = SCM_INUM (y);
	  long int z = xx - yy;
	  if (SCM_FIXABLE (z))
	    return SCM_MAKINUM (z);
	  else
	    return scm_i_long2big (z);
	}
      else if (SCM_BIGP (y))
	{
	  /* inum-x - big-y */
	  long xx = SCM_INUM (x);

	  if (xx == 0)
	    return scm_i_clonebig (y, 0);
	  else
	    {
	      int sgn_y = mpz_sgn (SCM_I_BIG_MPZ (y));
	      SCM result = scm_i_mkbig ();

	      if (xx >= 0)
		mpz_ui_sub (SCM_I_BIG_MPZ (result), xx, SCM_I_BIG_MPZ (y));
	      else
		{
		  /* x - y == -(y + -x) */
		  mpz_add_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (y), -xx);
		  mpz_neg (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result));
		}
	      scm_remember_upto_here_1 (y);

	      if ((xx < 0 && (sgn_y > 0)) || ((xx > 0) && sgn_y < 0))
		/* we know the result will have to be a bignum */
		return result;
	      else
		return scm_i_normbig (result);
	    }
	}
      else if (SCM_REALP (y))
	{
	  long int xx = SCM_INUM (x);
	  return scm_make_real (xx - SCM_REAL_VALUE (y));
	}
      else if (SCM_COMPLEXP (y))
	{
	  long int xx = SCM_INUM (x);
	  return scm_make_complex (xx - SCM_COMPLEX_REAL (y),
				   - SCM_COMPLEX_IMAG (y));
	}
      else
	SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  /* big-x - inum-y */
	  long yy = SCM_INUM (y);
	  int sgn_x = mpz_sgn (SCM_I_BIG_MPZ (x));

	  scm_remember_upto_here_1 (x);
	  if (sgn_x == 0)
	    return SCM_FIXABLE (-yy) ? SCM_MAKINUM (-yy) : scm_long2num (-yy);
	  else
	    {
	      SCM result = scm_i_mkbig ();

	      if (yy >= 0)
		mpz_sub_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (x), yy);
	      else
		mpz_add_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (x), -yy);
	      scm_remember_upto_here_1 (x);

	      if ((sgn_x < 0 && (yy > 0)) || ((sgn_x > 0) && yy < 0))
		/* we know the result will have to be a bignum */
		return result;
	      else
		return scm_i_normbig (result);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  int sgn_x = mpz_sgn (SCM_I_BIG_MPZ (x)); 
	  int sgn_y = mpz_sgn (SCM_I_BIG_MPZ (y)); 
	  SCM result = scm_i_mkbig ();
	  mpz_sub (SCM_I_BIG_MPZ (result),
		   SCM_I_BIG_MPZ (x),
		   SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  /* we know the result will have to be a bignum */
	  if ((sgn_x == 1) && (sgn_y == -1))
	    return result;
	  if ((sgn_x == -1) && (sgn_y == 1))
	    return result;
	  return scm_i_normbig (result);
	}
      else if (SCM_REALP (y))
	{
	  double result = mpz_get_d (SCM_I_BIG_MPZ (x)) - SCM_REAL_VALUE (y);
	  scm_remember_upto_here_1 (x);
	  return scm_make_real (result);
	}
      else if (SCM_COMPLEXP (y))
	{
	  double real_part = (mpz_get_d (SCM_I_BIG_MPZ (x))
			      - SCM_COMPLEX_REAL (y));
	  scm_remember_upto_here_1 (x);
	  return scm_make_complex (real_part, - SCM_COMPLEX_IMAG (y));      
	}
      else SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_INUMP (y))
	return scm_make_real (SCM_REAL_VALUE (x) - SCM_INUM (y));
      else if (SCM_BIGP (y))
	{
	  double result = SCM_REAL_VALUE (x) - mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (x);
	  return scm_make_real (result);      
	}
      else if (SCM_REALP (y))
	return scm_make_real (SCM_REAL_VALUE (x) - SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_make_complex (SCM_REAL_VALUE (x) - SCM_COMPLEX_REAL (y),
				 -SCM_COMPLEX_IMAG (y));
      else
	SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_INUMP (y))
	return scm_make_complex (SCM_COMPLEX_REAL (x) - SCM_INUM (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_BIGP (y))
	{
	  double real_part = (SCM_COMPLEX_REAL (x)
			      - mpz_get_d (SCM_I_BIG_MPZ (y)));
	  scm_remember_upto_here_1 (x);
	  return scm_make_complex (real_part, SCM_COMPLEX_IMAG (y));      
	}
      else if (SCM_REALP (y))
	return scm_make_complex (SCM_COMPLEX_REAL (x) - SCM_REAL_VALUE (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_COMPLEXP (y))
	return scm_make_complex (SCM_COMPLEX_REAL (x) - SCM_COMPLEX_REAL (y),
				 SCM_COMPLEX_IMAG (x) - SCM_COMPLEX_IMAG (y));
      else
	SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else
    SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARG1, s_difference);
}
#undef FUNC_NAME


SCM_GPROC1 (s_product, "*", scm_tc7_asubr, scm_product, g_product);
/* "Return the product of all arguments.  If called without arguments,\n"
 * "1 is returned."
 */
SCM
scm_product (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_UNBNDP (x))
	return SCM_MAKINUM (1L);
      else if (SCM_NUMBERP (x))
	return x;
      else
	SCM_WTA_DISPATCH_1 (g_product, x, SCM_ARG1, s_product);
    }
  
  if (SCM_INUMP (x))
    {
      long xx;

    intbig:
      xx = SCM_INUM (x);

      switch (xx)
	{
        case 0: return x; break;
        case 1: return y; break;
	}

      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  long kk = xx * yy;
	  SCM k = SCM_MAKINUM (kk);
	  if ((kk == SCM_INUM (k)) && (kk / xx == yy))
	    return k;
	  else
	    {
	      SCM result = scm_i_long2big (xx);
	      mpz_mul_si (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result), yy);
	      return scm_i_normbig (result);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM result = scm_i_mkbig ();
	  mpz_mul_si (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (y), xx);
	  scm_remember_upto_here_1 (y);
	  return result;
	}
      else if (SCM_REALP (y))
	return scm_make_real (xx * SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_make_complex (xx * SCM_COMPLEX_REAL (y),
				 xx * SCM_COMPLEX_IMAG (y));
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  SCM_SWAP (x, y);
	  goto intbig;
	}
      else if (SCM_BIGP (y))
	{
	  SCM result = scm_i_mkbig ();
	  mpz_mul (SCM_I_BIG_MPZ (result),
		   SCM_I_BIG_MPZ (x),
		   SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return result;
	}
      else if (SCM_REALP (y))
	{
	  double result = mpz_get_d (SCM_I_BIG_MPZ (x)) * SCM_REAL_VALUE (y);
	  scm_remember_upto_here_1 (x);
	  return scm_make_real (result);
	}
      else if (SCM_COMPLEXP (y))
	{
	  double z = mpz_get_d (SCM_I_BIG_MPZ (x));
	  scm_remember_upto_here_1 (x);
	  return scm_make_complex (z * SCM_COMPLEX_REAL (y),
				   z * SCM_COMPLEX_IMAG (y));
	}
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_INUMP (y))
	return scm_make_real (SCM_INUM (y) * SCM_REAL_VALUE (x));
      else if (SCM_BIGP (y))
	{
	  double result = mpz_get_d (SCM_I_BIG_MPZ (y)) * SCM_REAL_VALUE (x);
	  scm_remember_upto_here_1 (y);
	  return scm_make_real (result);
	}
      else if (SCM_REALP (y))
	return scm_make_real (SCM_REAL_VALUE (x) * SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_make_complex (SCM_REAL_VALUE (x) * SCM_COMPLEX_REAL (y),
				 SCM_REAL_VALUE (x) * SCM_COMPLEX_IMAG (y));
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_INUMP (y))
	return scm_make_complex (SCM_INUM (y) * SCM_COMPLEX_REAL (x),
				 SCM_INUM (y) * SCM_COMPLEX_IMAG (x));
      else if (SCM_BIGP (y))
	{
	  double z = mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return scm_make_complex (z * SCM_COMPLEX_REAL (x),
				   z * SCM_COMPLEX_IMAG (x));
	}
      else if (SCM_REALP (y))
	return scm_make_complex (SCM_REAL_VALUE (y) * SCM_COMPLEX_REAL (x),
				 SCM_REAL_VALUE (y) * SCM_COMPLEX_IMAG (x));
      else if (SCM_COMPLEXP (y))
	{
	  return scm_make_complex (SCM_COMPLEX_REAL (x) * SCM_COMPLEX_REAL (y)
				   - SCM_COMPLEX_IMAG (x) * SCM_COMPLEX_IMAG (y),
				   SCM_COMPLEX_REAL (x) * SCM_COMPLEX_IMAG (y)
				   + SCM_COMPLEX_IMAG (x) * SCM_COMPLEX_REAL (y));
	}
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else
    SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARG1, s_product);
}

double
scm_num2dbl (SCM a, const char *why)
#define FUNC_NAME why
{
  if (SCM_INUMP (a))
    return (double) SCM_INUM (a);
  else if (SCM_BIGP (a))
    {
      double result = mpz_get_d (SCM_I_BIG_MPZ (a));
      scm_remember_upto_here_1 (a);
      return result;
    }
  else if (SCM_REALP (a))
    return (SCM_REAL_VALUE (a));
  else
    SCM_WRONG_TYPE_ARG (SCM_ARGn, a);
}
#undef FUNC_NAME

#if ((defined (HAVE_ISINF) && defined (HAVE_ISNAN)) \
     || (defined (HAVE_FINITE) && defined (HAVE_ISNAN)))
#define ALLOW_DIVIDE_BY_ZERO
/* #define ALLOW_DIVIDE_BY_EXACT_ZERO */
#endif

/* The code below for complex division is adapted from the GNU
   libstdc++, which adapted it from f2c's libF77, and is subject to
   this copyright:  */

/****************************************************************
Copyright 1990, 1991, 1992, 1993 by AT&T Bell Laboratories and Bellcore.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the names of AT&T Bell Laboratories or
Bellcore or any of their entities not be used in advertising or
publicity pertaining to distribution of the software without
specific, written prior permission.

AT&T and Bellcore disclaim all warranties with regard to this
software, including all implied warranties of merchantability
and fitness.  In no event shall AT&T or Bellcore be liable for
any special, indirect or consequential damages or any damages
whatsoever resulting from loss of use, data or profits, whether
in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of
this software.
****************************************************************/

SCM_GPROC1 (s_divide, "/", scm_tc7_asubr, scm_divide, g_divide);
/* Divide the first argument by the product of the remaining
   arguments.  If called with one argument @var{z1}, 1/@var{z1} is
   returned.  */
#define FUNC_NAME s_divide
SCM
scm_divide (SCM x, SCM y)
{
  double a;

  if (SCM_UNBNDP (y))
    {
      if (SCM_UNBNDP (x))
	SCM_WTA_DISPATCH_0 (g_divide, s_divide);
      else if (SCM_INUMP (x))
	{
	  long xx = SCM_INUM (x);
	  if (xx == 1 || xx == -1)
	    return x;
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	  else if (xx == 0)
	    scm_num_overflow (s_divide);
#endif
	  else
	    return scm_make_real (1.0 / (double) xx);
	}
      else if (SCM_BIGP (x))
	return scm_make_real (1.0 / scm_i_big2dbl (x));
      else if (SCM_REALP (x))
	{
	  double xx = SCM_REAL_VALUE (x);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (xx == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_make_real (1.0 / xx);
	}
      else if (SCM_COMPLEXP (x))
	{
	  double r = SCM_COMPLEX_REAL (x);
	  double i = SCM_COMPLEX_IMAG (x);
	  if (r <= i)
	    {
	      double t = r / i;
	      double d = i * (1.0 + t * t);
	      return scm_make_complex (t / d, -1.0 / d);
	    }
	  else
	    {
	      double t = i / r;
	      double d = r * (1.0 + t * t);
	      return scm_make_complex (1.0 / d, -t / d);
	    }
	}
      else
	SCM_WTA_DISPATCH_1 (g_divide, x, SCM_ARG1, s_divide);
    }

  if (SCM_INUMP (x))
    {
      long xx = SCM_INUM (x);
      if (SCM_INUMP (y))
	{
	  long yy = SCM_INUM (y);
	  if (yy == 0)
	    {
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	      scm_num_overflow (s_divide);
#else
	      return scm_make_real ((double) xx / (double) yy);
#endif
	    }
	  else if (xx % yy != 0)
	    return scm_make_real ((double) xx / (double) yy);
	  else
	    {
	      long z = xx / yy;
	      if (SCM_FIXABLE (z))
		return SCM_MAKINUM (z);
	      else
		return scm_i_long2big (z);
	    }
	}
      else if (SCM_BIGP (y))
	return scm_make_real ((double) xx / scm_i_big2dbl (y));
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_make_real ((double) xx / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  a = xx;
	complex_div: /* y _must_ be a complex number */
	  {
	    double r = SCM_COMPLEX_REAL (y);
	    double i = SCM_COMPLEX_IMAG (y);
	    if (r <= i)
	      {
		double t = r / i;
		double d = i * (1.0 + t * t);
		return scm_make_complex ((a * t) / d,  -a / d);
	      }
	    else
	      {
		double t = i / r;
		double d = r * (1.0 + t * t);
		return scm_make_complex (a / d,  -(a * t) / d);
	      }
	  }
	}
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_INUMP (y))
	{
	  long int yy = SCM_INUM (y);
	  if (yy == 0)
	    {
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	      scm_num_overflow (s_divide);
#else
	      int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
	      scm_remember_upto_here_1 (x);
	      return (sgn == 0) ? scm_nan () : scm_inf ();
#endif
	    }
	  else if (yy == 1)
	    return x;
	  else
	    {
	      /* FIXME: HMM, what are the relative performance issues here?
		 We need to test.  Is it faster on average to test
		 divisible_p, then perform whichever operation, or is it
		 faster to perform the integer div opportunistically and
		 switch to real if there's a remainder?  For now we take the
		 middle ground: test, then if divisible, use the faster div
		 func. */

	      long abs_yy = yy < 0 ? -yy : yy;
	      int divisible_p = mpz_divisible_ui_p (SCM_I_BIG_MPZ (x), abs_yy);

	      if (divisible_p)
		{
		  SCM result = scm_i_mkbig ();
		  mpz_divexact_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (x), abs_yy);
		  scm_remember_upto_here_1 (x);
		  if (yy < 0)
		    mpz_neg (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result));
		  return scm_i_normbig (result);
		}
	      else
		return scm_make_real (scm_i_big2dbl (x) / (double) yy);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  int y_is_zero = (mpz_sgn (SCM_I_BIG_MPZ (y)) == 0);
	  if (y_is_zero)
	    {
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	      scm_num_overflow (s_divide);
#else
	      int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
	      scm_remember_upto_here_1 (x);
	      return (sgn == 0) ? scm_nan () : scm_inf ();
#endif
	    }
	  else
	    {
	      /* big_x / big_y */
	      int divisible_p = mpz_divisible_p (SCM_I_BIG_MPZ (x),
						 SCM_I_BIG_MPZ (y));
	      if (divisible_p)
		{
		  SCM result = scm_i_mkbig ();
		  mpz_divexact (SCM_I_BIG_MPZ (result),
				SCM_I_BIG_MPZ (x),
				SCM_I_BIG_MPZ (y));
		  scm_remember_upto_here_2 (x, y);
		  return scm_i_normbig (result);
		}
	      else
		{
		  double dbx = mpz_get_d (SCM_I_BIG_MPZ (x));
		  double dby = mpz_get_d (SCM_I_BIG_MPZ (y));
		  scm_remember_upto_here_2 (x, y);
		  return scm_make_real (dbx / dby);
		}
	    }
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_make_real (scm_i_big2dbl (x) / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  a = scm_i_big2dbl (x);
	  goto complex_div;
	}
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else if (SCM_REALP (x))
    {
      double rx = SCM_REAL_VALUE (x);
      if (SCM_INUMP (y))
	{
	  long int yy = SCM_INUM (y);
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	  if (yy == 0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_make_real (rx / (double) yy);
	}
      else if (SCM_BIGP (y))
	{
	  double dby = mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return scm_make_real (rx / dby);
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_make_real (rx / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  a = rx;
	  goto complex_div;
	}
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else if (SCM_COMPLEXP (x))
    {
      double rx = SCM_COMPLEX_REAL (x);
      double ix = SCM_COMPLEX_IMAG (x);
      if (SCM_INUMP (y))
	{
	  long int yy = SCM_INUM (y);
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	  if (yy == 0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    {
	      double d = yy;
	      return scm_make_complex (rx / d, ix / d);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  double dby = mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return scm_make_complex (rx / dby, ix / dby);
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_make_complex (rx / yy, ix / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  double ry = SCM_COMPLEX_REAL (y);
	  double iy = SCM_COMPLEX_IMAG (y);
	  if (ry <= iy)
	    {
	      double t = ry / iy;
	      double d = iy * (1.0 + t * t);
	      return scm_make_complex ((rx * t + ix) / d, (ix * t - rx) / d);
	    }
	  else
	    {
	      double t = iy / ry;
	      double d = ry * (1.0 + t * t);
	      return scm_make_complex ((rx + ix * t) / d, (ix - rx * t) / d);
	    }
	}
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else
    SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARG1, s_divide);
}
#undef FUNC_NAME


double
scm_asinh (double x)
{
#if HAVE_ASINH
  return asinh (x);
#else
#define asinh scm_asinh
  return log (x + sqrt (x * x + 1));
#endif
}
SCM_GPROC1 (s_asinh, "$asinh", scm_tc7_dsubr, (SCM (*)()) asinh, g_asinh);
/* "Return the inverse hyperbolic sine of @var{x}."
 */


double
scm_acosh (double x)
{
#if HAVE_ACOSH
  return acosh (x);
#else
#define acosh scm_acosh
  return log (x + sqrt (x * x - 1));
#endif
}
SCM_GPROC1 (s_acosh, "$acosh", scm_tc7_dsubr, (SCM (*)()) acosh, g_acosh);
/* "Return the inverse hyperbolic cosine of @var{x}."
 */


double
scm_atanh (double x)
{
#if HAVE_ATANH
  return atanh (x);
#else
#define atanh scm_atanh
  return 0.5 * log ((1 + x) / (1 - x));
#endif
}
SCM_GPROC1 (s_atanh, "$atanh", scm_tc7_dsubr, (SCM (*)()) atanh, g_atanh);
/* "Return the inverse hyperbolic tangent of @var{x}."
 */


double
scm_truncate (double x)
{
#if HAVE_TRUNC
  return trunc (x);
#else
#define trunc scm_truncate
  if (x < 0.0)
    return -floor (-x);
  return floor (x);
#endif
}
SCM_GPROC1 (s_truncate, "truncate", scm_tc7_dsubr, (SCM (*)()) trunc, g_truncate);
/* "Round the inexact number @var{x} towards zero."
 */


SCM_GPROC1 (s_round, "round", scm_tc7_dsubr, (SCM (*)()) scm_round, g_round);
/* "Round the inexact number @var{x}. If @var{x} is halfway between two\n"
 * "numbers, round towards even."
 */
double
scm_round (double x)
{
  double plus_half = x + 0.5;
  double result = floor (plus_half);
  /* Adjust so that the scm_round is towards even.  */
  return ((plus_half == result && plus_half / 2 != floor (plus_half / 2))
	  ? result - 1
	  : result);
}


SCM_GPROC1 (s_i_floor, "floor", scm_tc7_dsubr, (SCM (*)()) floor, g_i_floor);
/* "Round the number @var{x} towards minus infinity."
 */
SCM_GPROC1 (s_i_ceil, "ceiling", scm_tc7_dsubr, (SCM (*)()) ceil, g_i_ceil);
/* "Round the number @var{x} towards infinity."
 */
SCM_GPROC1 (s_i_sqrt, "$sqrt", scm_tc7_dsubr, (SCM (*)()) sqrt, g_i_sqrt);
/* "Return the square root of the real number @var{x}."
 */
SCM_GPROC1 (s_i_abs, "$abs", scm_tc7_dsubr, (SCM (*)()) fabs, g_i_abs);
/* "Return the absolute value of the real number @var{x}."
 */
SCM_GPROC1 (s_i_exp, "$exp", scm_tc7_dsubr, (SCM (*)()) exp, g_i_exp);
/* "Return the @var{x}th power of e."
 */
SCM_GPROC1 (s_i_log, "$log", scm_tc7_dsubr, (SCM (*)()) log, g_i_log);
/* "Return the natural logarithm of the real number @var{x}."
 */
SCM_GPROC1 (s_i_sin, "$sin", scm_tc7_dsubr, (SCM (*)()) sin, g_i_sin);
/* "Return the sine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_cos, "$cos", scm_tc7_dsubr, (SCM (*)()) cos, g_i_cos);
/* "Return the cosine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_tan, "$tan", scm_tc7_dsubr, (SCM (*)()) tan, g_i_tan);
/* "Return the tangent of the real number @var{x}."
 */
SCM_GPROC1 (s_i_asin, "$asin", scm_tc7_dsubr, (SCM (*)()) asin, g_i_asin);
/* "Return the arc sine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_acos, "$acos", scm_tc7_dsubr, (SCM (*)()) acos, g_i_acos);
/* "Return the arc cosine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_atan, "$atan", scm_tc7_dsubr, (SCM (*)()) atan, g_i_atan);
/* "Return the arc tangent of the real number @var{x}."
 */
SCM_GPROC1 (s_i_sinh, "$sinh", scm_tc7_dsubr, (SCM (*)()) sinh, g_i_sinh);
/* "Return the hyperbolic sine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_cosh, "$cosh", scm_tc7_dsubr, (SCM (*)()) cosh, g_i_cosh);
/* "Return the hyperbolic cosine of the real number @var{x}."
 */
SCM_GPROC1 (s_i_tanh, "$tanh", scm_tc7_dsubr, (SCM (*)()) tanh, g_i_tanh);
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
  if (SCM_INUMP (x))
    xy->x = SCM_INUM (x);
  else if (SCM_BIGP (x))
    xy->x = scm_i_big2dbl (x);
  else if (SCM_REALP (x))
    xy->x = SCM_REAL_VALUE (x);
  else
    scm_wrong_type_arg (sstring, SCM_ARG1, x);

  if (SCM_INUMP (y))
    xy->y = SCM_INUM (y);
  else if (SCM_BIGP (y))
    xy->y = scm_i_big2dbl (y);
  else if (SCM_REALP (y))
    xy->y = SCM_REAL_VALUE (y);
  else
    scm_wrong_type_arg (sstring, SCM_ARG2, y);
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
  double s, c;
  scm_two_doubles (x, y, FUNC_NAME, &xy);
#if HAVE_SINCOS
  sincos (xy.y, &s, &c);
#else
  s = sin (xy.y);
  c = cos (xy.y);
#endif
  return scm_make_complex (xy.x * c, xy.x * s);
}
#undef FUNC_NAME


SCM_GPROC (s_real_part, "real-part", 1, 0, 0, scm_real_part, g_real_part);
/* "Return the real part of the number @var{z}."
 */
SCM
scm_real_part (SCM z)
{
  if (SCM_INUMP (z))
    return z;
  else if (SCM_BIGP (z))
    return z;
  else if (SCM_REALP (z))
    return z;
  else if (SCM_COMPLEXP (z))
    return scm_make_real (SCM_COMPLEX_REAL (z));
  else
    SCM_WTA_DISPATCH_1 (g_real_part, z, SCM_ARG1, s_real_part);
}


SCM_GPROC (s_imag_part, "imag-part", 1, 0, 0, scm_imag_part, g_imag_part);
/* "Return the imaginary part of the number @var{z}."
 */
SCM
scm_imag_part (SCM z)
{
  if (SCM_INUMP (z))
    return SCM_INUM0;
  else if (SCM_BIGP (z))
    return SCM_INUM0;
  else if (SCM_REALP (z))
    return scm_flo0;
  else if (SCM_COMPLEXP (z))
    return scm_make_real (SCM_COMPLEX_IMAG (z));
  else
    SCM_WTA_DISPATCH_1 (g_imag_part, z, SCM_ARG1, s_imag_part);
}


SCM_GPROC (s_magnitude, "magnitude", 1, 0, 0, scm_magnitude, g_magnitude);
/* "Return the magnitude of the number @var{z}. This is the same as\n"
 * "@code{abs} for real arguments, but also allows complex numbers."
 */
SCM
scm_magnitude (SCM z)
{
  if (SCM_INUMP (z))
    {
      long int zz = SCM_INUM (z);
      if (zz >= 0)
	return z;
      else if (SCM_POSFIXABLE (-zz))
	return SCM_MAKINUM (-zz);
      else
	return scm_i_long2big (-zz);
    }
  else if (SCM_BIGP (z))
    {
      int sgn = mpz_sgn (SCM_I_BIG_MPZ (z));
      scm_remember_upto_here_1 (z);
      if (sgn < 0)
	return scm_i_clonebig (z, 0);
      else
	return z;
    }
  else if (SCM_REALP (z))
    return scm_make_real (fabs (SCM_REAL_VALUE (z)));
  else if (SCM_COMPLEXP (z))
    return scm_make_real (hypot (SCM_COMPLEX_REAL (z), SCM_COMPLEX_IMAG (z)));
  else
    SCM_WTA_DISPATCH_1 (g_magnitude, z, SCM_ARG1, s_magnitude);
}


SCM_GPROC (s_angle, "angle", 1, 0, 0, scm_angle, g_angle);
/* "Return the angle of the complex number @var{z}."
 */
SCM
scm_angle (SCM z)
{
  /* atan(0,-1) is pi and it'd be possible to have that as a constant like
     scm_flo0 to save allocating a new flonum with scm_make_real each time.
     But if atan2 follows the floating point rounding mode, then the value
     is not a constant.  Maybe it'd be close enough though.  */
  if (SCM_INUMP (z))
    {
      if (SCM_INUM (z) >= 0)
        return scm_flo0;
      else
	return scm_make_real (atan2 (0.0, -1.0));
    }
  else if (SCM_BIGP (z))
    {
      int sgn = mpz_sgn (SCM_I_BIG_MPZ (z));
      scm_remember_upto_here_1 (z);
      if (sgn < 0)
	return scm_make_real (atan2 (0.0, -1.0));
      else
        return scm_flo0;
    }
  else if (SCM_REALP (z))
    {
      if (SCM_REAL_VALUE (z) >= 0)
        return scm_flo0;
      else
        return scm_make_real (atan2 (0.0, -1.0));
    }
  else if (SCM_COMPLEXP (z))
    return scm_make_real (atan2 (SCM_COMPLEX_IMAG (z), SCM_COMPLEX_REAL (z)));
  else
    SCM_WTA_DISPATCH_1 (g_angle, z, SCM_ARG1, s_angle);
}


SCM_GPROC (s_exact_to_inexact, "exact->inexact", 1, 0, 0, scm_exact_to_inexact, g_exact_to_inexact);
/* Convert the number @var{x} to its inexact representation.\n" 
 */
SCM
scm_exact_to_inexact (SCM z)
{
  if (SCM_INUMP (z))
    return scm_make_real ((double) SCM_INUM (z));
  else if (SCM_BIGP (z))
    return scm_make_real (scm_i_big2dbl (z));
  else if (SCM_INEXACTP (z))
    return z;
  else
    SCM_WTA_DISPATCH_1 (g_exact_to_inexact, z, 1, s_exact_to_inexact);
}


SCM_DEFINE (scm_inexact_to_exact, "inexact->exact", 1, 0, 0, 
            (SCM z),
	    "Return an exact number that is numerically closest to @var{z}.")
#define FUNC_NAME s_scm_inexact_to_exact
{
  if (SCM_INUMP (z))
    return z;
  else if (SCM_BIGP (z))
    return z;
  else if (SCM_REALP (z))
    {
      double u = floor (SCM_REAL_VALUE (z) + 0.5);
      long lu = (long) u;
      if (SCM_FIXABLE (lu))
	return SCM_MAKINUM (lu);
      else if (!xisinf (u) && !xisnan (u))
	return scm_i_dbl2big (u);
      else
	scm_num_overflow (s_scm_inexact_to_exact);
    }
  else
    SCM_WRONG_TYPE_ARG (1, z);
}
#undef FUNC_NAME

/* if you need to change this, change test-num2integral.c as well */
#if SCM_SIZEOF_LONG_LONG != 0
# ifndef LLONG_MAX
#  define ULLONG_MAX ((unsigned long long) (-1))
#  define LLONG_MAX ((long long) (ULLONG_MAX >> 1))
#  define LLONG_MIN (~LLONG_MAX)
# endif
#endif

/* Parameters for creating integer conversion routines.

   Define the following preprocessor macros before including
   "libguile/num2integral.i.c":

   NUM2INTEGRAL - the name of the function for converting from a
     Scheme object to the integral type.  This function will be
     defined when including "num2integral.i.c".

   INTEGRAL2NUM - the name of the function for converting from the
     integral type to a Scheme object.  This function will be defined.

   INTEGRAL2BIG - the name of an internal function that createas a
     bignum from the integral type.  This function will be defined.
     The name should start with "scm_i_".

   ITYPE - the name of the integral type.

   UNSIGNED - Define this to 1 when ITYPE is an unsigned type.  Define
   it to 0 otherwise.

   UNSIGNED_ITYPE - the name of the the unsigned variant of the
     integral type.  If you don't define this, it defaults to
     "unsigned ITYPE" for signed types and simply "ITYPE" for unsigned
     ones.

   SIZEOF_ITYPE - an expression giving the size of the integral type
     in bytes.  This expression must be computable by the
     preprocessor.  (SIZEOF_FOO values are calculated by configure.in
     for common types).

*/

#define NUM2INTEGRAL scm_num2short
#define INTEGRAL2NUM scm_short2num
#define INTEGRAL2BIG scm_i_short2big
#define UNSIGNED 0
#define ITYPE short
#define SIZEOF_ITYPE SIZEOF_SHORT
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2ushort
#define INTEGRAL2NUM scm_ushort2num
#define INTEGRAL2BIG scm_i_ushort2big
#define UNSIGNED 1
#define ITYPE unsigned short
#define SIZEOF_ITYPE SIZEOF_UNSIGNED_SHORT
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2int
#define INTEGRAL2NUM scm_int2num
#define INTEGRAL2BIG scm_i_int2big
#define UNSIGNED 0
#define ITYPE int
#define SIZEOF_ITYPE SIZEOF_INT
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2uint
#define INTEGRAL2NUM scm_uint2num
#define INTEGRAL2BIG scm_i_uint2big
#define UNSIGNED 1
#define ITYPE unsigned int
#define SIZEOF_ITYPE SIZEOF_UNSIGNED_INT
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2long
#define INTEGRAL2NUM scm_long2num
#define INTEGRAL2BIG scm_i_long2big
#define UNSIGNED 0
#define ITYPE long
#define SIZEOF_ITYPE SIZEOF_LONG
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2ulong
#define INTEGRAL2NUM scm_ulong2num
#define INTEGRAL2BIG scm_i_ulong2big
#define UNSIGNED 1
#define ITYPE unsigned long
#define SIZEOF_ITYPE SIZEOF_UNSIGNED_LONG
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2ptrdiff
#define INTEGRAL2NUM scm_ptrdiff2num
#define INTEGRAL2BIG scm_i_ptrdiff2big
#define UNSIGNED 0
#define ITYPE scm_t_ptrdiff
#define UNSIGNED_ITYPE size_t
#define SIZEOF_ITYPE SCM_SIZEOF_SCM_T_PTRDIFF
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2size
#define INTEGRAL2NUM scm_size2num
#define INTEGRAL2BIG scm_i_size2big
#define UNSIGNED 1
#define ITYPE size_t
#define SIZEOF_ITYPE SIZEOF_SIZE_T
#include "libguile/num2integral.i.c"

#if SCM_SIZEOF_LONG_LONG != 0

#ifndef ULONG_LONG_MAX
#define ULONG_LONG_MAX (~0ULL)
#endif

#define NUM2INTEGRAL scm_num2long_long
#define INTEGRAL2NUM scm_long_long2num
#define INTEGRAL2BIG scm_i_long_long2big
#define UNSIGNED 0
#define ITYPE long long
#define SIZEOF_ITYPE SIZEOF_LONG_LONG
#include "libguile/num2integral.i.c"

#define NUM2INTEGRAL scm_num2ulong_long
#define INTEGRAL2NUM scm_ulong_long2num
#define INTEGRAL2BIG scm_i_ulong_long2big
#define UNSIGNED 1
#define ITYPE unsigned long long
#define SIZEOF_ITYPE SIZEOF_UNSIGNED_LONG_LONG
#include "libguile/num2integral.i.c"

#endif /* SCM_SIZEOF_LONG_LONG != 0 */

#define NUM2FLOAT scm_num2float
#define FLOAT2NUM scm_float2num
#define FTYPE float
#include "libguile/num2float.i.c"

#define NUM2FLOAT scm_num2double
#define FLOAT2NUM scm_double2num
#define FTYPE double
#include "libguile/num2float.i.c"

#ifdef GUILE_DEBUG

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t) (-1))
#endif
#ifndef PTRDIFF_MIN
#define PTRDIFF_MIN \
 ((scm_t_ptrdiff) ((scm_t_ptrdiff) 1 \
  << ((sizeof (scm_t_ptrdiff) * SCM_CHAR_BIT) - 1)))
#endif
#ifndef PTRDIFF_MAX
#define PTRDIFF_MAX (~ PTRDIFF_MIN)
#endif

#define CHECK(type, v)							   \
  do									   \
    {									   \
      if ((v) != scm_num2##type (scm_##type##2num (v), 1, "check_sanity")) \
	abort ();							   \
    }									   \
  while (0)

static void
check_sanity ()
{
  CHECK (short, 0);
  CHECK (ushort, 0U);
  CHECK (int, 0);
  CHECK (uint, 0U);
  CHECK (long, 0L);
  CHECK (ulong, 0UL);
  CHECK (size, 0);
  CHECK (ptrdiff, 0);

  CHECK (short, -1);
  CHECK (int, -1);
  CHECK (long, -1L);
  CHECK (ptrdiff, -1);

  CHECK (short, SHRT_MAX);
  CHECK (short, SHRT_MIN);
  CHECK (ushort, USHRT_MAX);
  CHECK (int, INT_MAX);
  CHECK (int, INT_MIN);
  CHECK (uint, UINT_MAX);
  CHECK (long, LONG_MAX);
  CHECK (long, LONG_MIN);
  CHECK (ulong, ULONG_MAX);
  CHECK (size, SIZE_MAX);
  CHECK (ptrdiff, PTRDIFF_MAX);
  CHECK (ptrdiff, PTRDIFF_MIN);

#if SCM_SIZEOF_LONG_LONG != 0
  CHECK (long_long, 0LL);
  CHECK (ulong_long, 0ULL);
  CHECK (long_long, -1LL);
  CHECK (long_long, LLONG_MAX);
  CHECK (long_long, LLONG_MIN);
  CHECK (ulong_long, ULLONG_MAX);
#endif
}

#undef CHECK

#define CHECK \
        scm_internal_catch (SCM_BOOL_T, check_body, &data, check_handler, &data); \
        if (!SCM_FALSEP (data)) abort();

static SCM
check_body (void *data)
{
  SCM num = *(SCM *) data;
  scm_num2ulong (num, 1, NULL);
  
  return SCM_UNSPECIFIED;
}

static SCM
check_handler (void *data, SCM tag, SCM throw_args)
{
  SCM *num = (SCM *) data;
  *num = SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}
  
SCM_DEFINE (scm_sys_check_number_conversions, "%check-number-conversions", 0, 0, 0, 
            (void),
	    "Number conversion sanity checking.")
#define FUNC_NAME s_scm_sys_check_number_conversions
{
  SCM data = SCM_MAKINUM (-1);
  CHECK;
  data = scm_int2num (INT_MIN);
  CHECK;
  data = scm_ulong2num (ULONG_MAX);
  data = scm_difference (SCM_INUM0, data);
  CHECK;
  data = scm_ulong2num (ULONG_MAX);
  data = scm_sum (SCM_MAKINUM (1), data); data = scm_difference (SCM_INUM0, data);
  CHECK;
  data = scm_int2num (-10000); data = scm_product (data, data); data = scm_product (data, data);
  CHECK;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif

void
scm_init_numbers ()
{
  abs_most_negative_fixnum = scm_i_long2big (- SCM_MOST_NEGATIVE_FIXNUM);
  scm_permanent_object (abs_most_negative_fixnum);

  mpz_init_set_si (z_negative_one, -1);

  /* It may be possible to tune the performance of some algorithms by using
   * the following constants to avoid the creation of bignums.  Please, before
   * using these values, remember the two rules of program optimization:
   * 1st Rule:  Don't do it.  2nd Rule (experts only):  Don't do it yet. */
  scm_c_define ("most-positive-fixnum",
		SCM_MAKINUM (SCM_MOST_POSITIVE_FIXNUM));
  scm_c_define ("most-negative-fixnum",
		SCM_MAKINUM (SCM_MOST_NEGATIVE_FIXNUM));

  scm_add_feature ("complex");
  scm_add_feature ("inexact");
  scm_flo0 = scm_make_real (0.0);
#ifdef DBL_DIG
  scm_dblprec = (DBL_DIG > 20) ? 20 : DBL_DIG;
#else
  {				/* determine floating point precision */
    double f = 0.1;
    double fsum = 1.0 + f;
    while (fsum != 1.0)
      {
	if (++scm_dblprec > 20)
	  fsum = 1.0;
	else
	  {
	    f /= 10.0;
	    fsum = f + 1.0;
	  }
      }
    scm_dblprec = scm_dblprec - 1;
  }
#endif /* DBL_DIG */

#ifdef GUILE_DEBUG
  check_sanity ();
#endif
  
#include "libguile/numbers.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
