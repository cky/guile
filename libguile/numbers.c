/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 *   2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
 *   2013 Free Software Foundation, Inc.
 *
 * Portions Copyright 1990, 1991, 1992, 1993 by AT&T Bell Laboratories
 * and Bellcore.  See scm_divide.
 *
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


/* General assumptions:
 * All objects satisfying SCM_BIGP() are too large to fit in a fixnum.
 * If an object satisfies integer?, it's either an inum, a bignum, or a real.
 * If floor (r) == r, r is an int, and mpz_set_d will DTRT.
 *     XXX What about infinities?  They are equal to their own floor!  -mhw
 * All objects satisfying SCM_FRACTIONP are never an integer.
 */

/* TODO:
   
   - see if special casing bignums and reals in integer-exponent when
     possible (to use mpz_pow and mpf_pow_ui) is faster.

   - look in to better short-circuiting of common cases in
     integer-expt and elsewhere.

   - see if direct mpz operations can help in ash and elsewhere.

 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <verify.h>
#include <assert.h>

#include <math.h>
#include <string.h>
#include <unicase.h>
#include <unictype.h>

#if HAVE_COMPLEX_H
#include <complex.h>
#endif

#include <stdarg.h>

#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"
#include "libguile/bdw-gc.h"

#include "libguile/validate.h"
#include "libguile/numbers.h"
#include "libguile/deprecation.h"

#include "libguile/eq.h"

/* values per glibc, if not already defined */
#ifndef M_LOG10E
#define M_LOG10E   0.43429448190325182765
#endif
#ifndef M_LN2
#define M_LN2	   0.69314718055994530942
#endif
#ifndef M_PI
#define M_PI       3.14159265358979323846
#endif

/* FIXME: We assume that FLT_RADIX is 2 */
verify (FLT_RADIX == 2);

typedef scm_t_signed_bits scm_t_inum;
#define scm_from_inum(x) (scm_from_signed_integer (x))

/* Test an inum to see if it can be converted to a double without loss
   of precision.  Note that this will sometimes return 0 even when 1
   could have been returned, e.g. for large powers of 2.  It is designed
   to be a fast check to optimize common cases. */
#define INUM_LOSSLESSLY_CONVERTIBLE_TO_DOUBLE(n)                        \
  (SCM_I_FIXNUM_BIT-1 <= DBL_MANT_DIG                                   \
   || ((n) ^ ((n) >> (SCM_I_FIXNUM_BIT-1))) < (1L << DBL_MANT_DIG))

#if ! HAVE_DECL_MPZ_INITS

/* GMP < 5.0.0 lacks `mpz_inits' and `mpz_clears'.  Provide them.  */

#define VARARG_MPZ_ITERATOR(func)		\
  static void					\
  func ## s (mpz_t x, ...)			\
  {						\
    va_list  ap;				\
						\
    va_start (ap, x);				\
    while (x != NULL)				\
      {						\
	func (x);				\
	x = va_arg (ap, mpz_ptr);		\
      }						\
    va_end (ap);				\
  }

VARARG_MPZ_ITERATOR (mpz_init)
VARARG_MPZ_ITERATOR (mpz_clear)

#endif



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
    (SCM_I_INUMP(x) ? SCM_I_NUMTAG_INUM \
       : (SCM_IMP(x) ? SCM_I_NUMTAG_NOTNUM \
         : (((0xfcff & SCM_CELL_TYPE (x)) == scm_tc7_number) ? SCM_TYP16(x) \
           : SCM_I_NUMTAG_NOTNUM)))
*/
/* the macro above will not work as is with fractions */


/* Default to 1, because as we used to hard-code `free' as the
   deallocator, we know that overriding these functions with
   instrumented `malloc' / `free' is OK.  */
int scm_install_gmp_memory_functions = 1;
static SCM flo0;
static SCM exactly_one_half;
static SCM flo_log10e;

#define SCM_SWAP(x, y) do { SCM __t = x; x = y; y = __t; } while (0)

/* FLOBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an inexact number.
 */
#define FLOBUFLEN (40+2*(sizeof(double)/sizeof(char)*SCM_CHAR_BIT*3+9)/10)


#if !defined (HAVE_ASINH)
static double asinh (double x) { return log (x + sqrt (x * x + 1)); }
#endif
#if !defined (HAVE_ACOSH)
static double acosh (double x) { return log (x + sqrt (x * x - 1)); }
#endif
#if !defined (HAVE_ATANH)
static double atanh (double x) { return 0.5 * log ((1 + x) / (1 - x)); }
#endif

/* mpz_cmp_d in GMP before 4.2 didn't recognise infinities, so
   xmpz_cmp_d uses an explicit check.  Starting with GMP 4.2 (released
   in March 2006), mpz_cmp_d now handles infinities properly.  */
#if 1
#define xmpz_cmp_d(z, d)                                \
  (isinf (d) ? (d < 0.0 ? 1 : -1) : mpz_cmp_d (z, d))
#else
#define xmpz_cmp_d(z, d)  mpz_cmp_d (z, d)
#endif


#if defined (GUILE_I)
#if defined HAVE_COMPLEX_DOUBLE

/* For an SCM object Z which is a complex number (ie. satisfies
   SCM_COMPLEXP), return its value as a C level "complex double". */
#define SCM_COMPLEX_VALUE(z)                                    \
  (SCM_COMPLEX_REAL (z) + GUILE_I * SCM_COMPLEX_IMAG (z))

static inline SCM scm_from_complex_double (complex double z) SCM_UNUSED;

/* Convert a C "complex double" to an SCM value. */
static inline SCM
scm_from_complex_double (complex double z)
{
  return scm_c_make_rectangular (creal (z), cimag (z));
}

#endif /* HAVE_COMPLEX_DOUBLE */
#endif /* GUILE_I */



static mpz_t z_negative_one;



/* Clear the `mpz_t' embedded in bignum PTR.  */
static void
finalize_bignum (void *ptr, void *data)
{
  SCM bignum;

  bignum = PTR2SCM (ptr);
  mpz_clear (SCM_I_BIG_MPZ (bignum));
}

/* The next three functions (custom_libgmp_*) are passed to
   mp_set_memory_functions (in GMP) so that memory used by the digits
   themselves is known to the garbage collector.  This is needed so
   that GC will be run at appropriate times.  Otherwise, a program which
   creates many large bignums would malloc a huge amount of memory
   before the GC runs. */
static void *
custom_gmp_malloc (size_t alloc_size)
{
  return scm_malloc (alloc_size);
}

static void *
custom_gmp_realloc (void *old_ptr, size_t old_size, size_t new_size)
{
  return scm_realloc (old_ptr, new_size);
}

static void
custom_gmp_free (void *ptr, size_t size)
{
  free (ptr);
}


/* Return a new uninitialized bignum.  */
static inline SCM
make_bignum (void)
{
  scm_t_bits *p;

  /* Allocate one word for the type tag and enough room for an `mpz_t'.  */
  p = scm_gc_malloc_pointerless (sizeof (scm_t_bits) + sizeof (mpz_t),
				 "bignum");
  p[0] = scm_tc16_big;

  scm_i_set_finalizer (p, finalize_bignum, NULL);

  return SCM_PACK (p);
}


SCM
scm_i_mkbig ()
{
  /* Return a newly created bignum. */
  SCM z = make_bignum ();
  mpz_init (SCM_I_BIG_MPZ (z));
  return z;
}

static SCM
scm_i_inum2big (scm_t_inum x)
{
  /* Return a newly created bignum initialized to X. */
  SCM z = make_bignum ();
#if SIZEOF_VOID_P == SIZEOF_LONG
  mpz_init_set_si (SCM_I_BIG_MPZ (z), x);
#else
  /* Note that in this case, you'll also have to check all mpz_*_ui and
     mpz_*_si invocations in Guile. */
#error creation of mpz not implemented for this inum size
#endif
  return z;
}

SCM
scm_i_long2big (long x)
{
  /* Return a newly created bignum initialized to X. */
  SCM z = make_bignum ();
  mpz_init_set_si (SCM_I_BIG_MPZ (z), x);
  return z;
}

SCM
scm_i_ulong2big (unsigned long x)
{
  /* Return a newly created bignum initialized to X. */
  SCM z = make_bignum ();
  mpz_init_set_ui (SCM_I_BIG_MPZ (z), x);
  return z;
}

SCM
scm_i_clonebig (SCM src_big, int same_sign_p)
{
  /* Copy src_big's value, negate it if same_sign_p is false, and return. */
  SCM z = make_bignum ();
  mpz_init_set (SCM_I_BIG_MPZ (z), SCM_I_BIG_MPZ (src_big));
  if (!same_sign_p)
    mpz_neg (SCM_I_BIG_MPZ (z), SCM_I_BIG_MPZ (z));
  return z;
}

int
scm_i_bigcmp (SCM x, SCM y)
{
  /* Return neg if x < y, pos if x > y, and 0 if x == y */
  /* presume we already know x and y are bignums */
  int result = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
  scm_remember_upto_here_2 (x, y);
  return result;
}

SCM
scm_i_dbl2big (double d)
{
  /* results are only defined if d is an integer */
  SCM z = make_bignum ();
  mpz_init_set_d (SCM_I_BIG_MPZ (z), d);
  return z;
}

/* Convert a integer in double representation to a SCM number. */

SCM
scm_i_dbl2num (double u)
{
  /* SCM_MOST_POSITIVE_FIXNUM+1 and SCM_MOST_NEGATIVE_FIXNUM are both
     powers of 2, so there's no rounding when making "double" values
     from them.  If plain SCM_MOST_POSITIVE_FIXNUM was used it could
     get rounded on a 64-bit machine, hence the "+1".

     The use of floor() to force to an integer value ensures we get a
     "numerically closest" value without depending on how a
     double->long cast or how mpz_set_d will round.  For reference,
     double->long probably follows the hardware rounding mode,
     mpz_set_d truncates towards zero.  */

  /* XXX - what happens when SCM_MOST_POSITIVE_FIXNUM etc is not
     representable as a double? */

  if (u < (double) (SCM_MOST_POSITIVE_FIXNUM+1)
      && u >= (double) SCM_MOST_NEGATIVE_FIXNUM)
    return SCM_I_MAKINUM ((scm_t_inum) u);
  else
    return scm_i_dbl2big (u);
}

static SCM round_right_shift_exact_integer (SCM n, long count);

/* scm_i_big2dbl_2exp() is like frexp for bignums: it converts the
   bignum b into a normalized significand and exponent such that
   b = significand * 2^exponent and 1/2 <= abs(significand) < 1.
   The return value is the significand rounded to the closest
   representable double, and the exponent is placed into *expon_p.
   If b is zero, then the returned exponent and significand are both
   zero. */

static double
scm_i_big2dbl_2exp (SCM b, long *expon_p)
{
  size_t bits = mpz_sizeinbase (SCM_I_BIG_MPZ (b), 2);
  size_t shift = 0;

  if (bits > DBL_MANT_DIG)
    {
      shift = bits - DBL_MANT_DIG;
      b = round_right_shift_exact_integer (b, shift);
      if (SCM_I_INUMP (b))
        {
          int expon;
          double signif = frexp (SCM_I_INUM (b), &expon);
          *expon_p = expon + shift;
          return signif;
        }
    }

  {
    long expon;
    double signif = mpz_get_d_2exp (&expon, SCM_I_BIG_MPZ (b));
    scm_remember_upto_here_1 (b);
    *expon_p = expon + shift;
    return signif;
  }
}

/* scm_i_big2dbl() rounds to the closest representable double,
   in accordance with R5RS exact->inexact.  */
double
scm_i_big2dbl (SCM b)
{
  long expon;
  double signif = scm_i_big2dbl_2exp (b, &expon);
  return ldexp (signif, expon);
}

SCM
scm_i_normbig (SCM b)
{
  /* convert a big back to a fixnum if it'll fit */
  /* presume b is a bignum */
  if (mpz_fits_slong_p (SCM_I_BIG_MPZ (b)))
    {
      scm_t_inum val = mpz_get_si (SCM_I_BIG_MPZ (b));
      if (SCM_FIXABLE (val))
        b = SCM_I_MAKINUM (val);
    }
  return b;
}

static SCM_C_INLINE_KEYWORD SCM
scm_i_mpz2num (mpz_t b)
{
  /* convert a mpz number to a SCM number. */
  if (mpz_fits_slong_p (b))
    {
      scm_t_inum val = mpz_get_si (b);
      if (SCM_FIXABLE (val))
        return SCM_I_MAKINUM (val);
    }

  {
    SCM z = make_bignum ();
    mpz_init_set (SCM_I_BIG_MPZ (z), b);
    return z;
  }
}

/* Make the ratio NUMERATOR/DENOMINATOR, where:
    1. NUMERATOR and DENOMINATOR are exact integers
    2. NUMERATOR and DENOMINATOR are reduced to lowest terms: gcd(n,d) == 1 */
static SCM
scm_i_make_ratio_already_reduced (SCM numerator, SCM denominator)
{
  /* Flip signs so that the denominator is positive. */
  if (scm_is_false (scm_positive_p (denominator)))
    {
      if (SCM_UNLIKELY (scm_is_eq (denominator, SCM_INUM0)))
	scm_num_overflow ("make-ratio");
      else
	{
	  numerator = scm_difference (numerator, SCM_UNDEFINED);
	  denominator = scm_difference (denominator, SCM_UNDEFINED);
	}
    }

  /* Check for the integer case */
  if (scm_is_eq (denominator, SCM_INUM1))
    return numerator;

  return scm_double_cell (scm_tc16_fraction,
			  SCM_UNPACK (numerator),
			  SCM_UNPACK (denominator), 0);
}

static SCM scm_exact_integer_quotient (SCM x, SCM y);

/* Make the ratio NUMERATOR/DENOMINATOR */
static SCM
scm_i_make_ratio (SCM numerator, SCM denominator)
#define FUNC_NAME "make-ratio"
{
  /* Make sure the arguments are proper */
  if (!SCM_LIKELY (SCM_I_INUMP (numerator) || SCM_BIGP (numerator)))
    SCM_WRONG_TYPE_ARG (1, numerator);
  else if (!SCM_LIKELY (SCM_I_INUMP (denominator) || SCM_BIGP (denominator)))
    SCM_WRONG_TYPE_ARG (2, denominator);
  else
    {
      SCM the_gcd = scm_gcd (numerator, denominator);
      if (!(scm_is_eq (the_gcd, SCM_INUM1)))
	{
	  /* Reduce to lowest terms */
	  numerator = scm_exact_integer_quotient (numerator, the_gcd);
	  denominator = scm_exact_integer_quotient (denominator, the_gcd);
	}
      return scm_i_make_ratio_already_reduced (numerator, denominator);
    }
}
#undef FUNC_NAME

static mpz_t scm_i_divide2double_lo2b;

/* Return the double that is closest to the exact rational N/D, with
   ties rounded toward even mantissas.  N and D must be exact
   integers. */
static double
scm_i_divide2double (SCM n, SCM d)
{
  int neg;
  mpz_t nn, dd, lo, hi, x;
  ssize_t e;

  if (SCM_LIKELY (SCM_I_INUMP (d)))
    {
      if (SCM_LIKELY
          (SCM_I_INUMP (n)
           && INUM_LOSSLESSLY_CONVERTIBLE_TO_DOUBLE (SCM_I_INUM (n))
           && INUM_LOSSLESSLY_CONVERTIBLE_TO_DOUBLE (SCM_I_INUM (d))))
        /* If both N and D can be losslessly converted to doubles, then
           we can rely on IEEE floating point to do proper rounding much
           faster than we can. */
        return ((double) SCM_I_INUM (n)) / ((double) SCM_I_INUM (d));

      if (SCM_UNLIKELY (scm_is_eq (d, SCM_INUM0)))
        {
          if (scm_is_true (scm_positive_p (n)))
            return 1.0 / 0.0;
          else if (scm_is_true (scm_negative_p (n)))
            return -1.0 / 0.0;
          else
            return 0.0 / 0.0;
        }

      mpz_init_set_si (dd, SCM_I_INUM (d));
    }
  else
    mpz_init_set (dd, SCM_I_BIG_MPZ (d));

  if (SCM_I_INUMP (n))
    mpz_init_set_si (nn, SCM_I_INUM (n));
  else
    mpz_init_set (nn, SCM_I_BIG_MPZ (n));

  neg = (mpz_sgn (nn) < 0) ^ (mpz_sgn (dd) < 0);
  mpz_abs (nn, nn);
  mpz_abs (dd, dd);

  /* Now we need to find the value of e such that:
 
     For e <= 0:
          b^{p-1} - 1/2b  <=      b^-e n / d  <  b^p - 1/2            [1A]
             (2 b^p - 1)  <=  2 b b^-e n / d  <  (2 b^p - 1) b        [2A]
           (2 b^p - 1) d  <=  2 b b^-e n      <  (2 b^p - 1) d b      [3A]

     For e >= 0:
          b^{p-1} - 1/2b  <=      n / b^e d   <  b^p - 1/2            [1B]
             (2 b^p - 1)  <=  2 b n / b^e d   <  (2 b^p - 1) b        [2B]
       (2 b^p - 1) d b^e  <=  2 b n           <  (2 b^p - 1) d b b^e  [3B]

         where:  p = DBL_MANT_DIG
                 b = FLT_RADIX  (here assumed to be 2)

     After rounding, the mantissa must be an integer between b^{p-1} and
     (b^p - 1), except for subnormal numbers.  In the inequations [1A]
     and [1B], the middle expression represents the mantissa *before*
     rounding, and therefore is bounded by the range of values that will
     round to a floating-point number with the exponent e.  The upper
     bound is (b^p - 1 + 1/2) = (b^p - 1/2), and is exclusive because
     ties will round up to the next power of b.  The lower bound is
     (b^{p-1} - 1/2b), and is inclusive because ties will round toward
     this power of b.  Here we subtract 1/2b instead of 1/2 because it
     is in the range of the next smaller exponent, where the
     representable numbers are closer together by a factor of b.

     Inequations [2A] and [2B] are derived from [1A] and [1B] by
     multiplying by 2b, and in [3A] and [3B] we multiply by the
     denominator of the middle value to obtain integer expressions.

     In the code below, we refer to the three expressions in [3A] or
     [3B] as lo, x, and hi.  If the number is normalizable, we will
     achieve the goal: lo <= x < hi */

  /* Make an initial guess for e */
  e = mpz_sizeinbase (nn, 2) - mpz_sizeinbase (dd, 2) - (DBL_MANT_DIG-1);
  if (e < DBL_MIN_EXP - DBL_MANT_DIG)
    e = DBL_MIN_EXP - DBL_MANT_DIG;

  /* Compute the initial values of lo, x, and hi
     based on the initial guess of e */
  mpz_inits (lo, hi, x, NULL);
  mpz_mul_2exp (x, nn, 2 + ((e < 0) ? -e : 0));
  mpz_mul (lo, dd, scm_i_divide2double_lo2b);
  if (e > 0)
    mpz_mul_2exp (lo, lo, e);
  mpz_mul_2exp (hi, lo, 1);

  /* Adjust e as needed to satisfy the inequality lo <= x < hi,
     (but without making e less then the minimum exponent) */
  while (mpz_cmp (x, lo) < 0 && e > DBL_MIN_EXP - DBL_MANT_DIG)
    {
      mpz_mul_2exp (x, x, 1);
      e--;
    }
  while (mpz_cmp (x, hi) >= 0)
    {
      /* If we ever used lo's value again,
         we would need to double lo here. */
      mpz_mul_2exp (hi, hi, 1);
      e++;
    }

  /* Now compute the rounded mantissa:
     n / b^e d   (if e >= 0)
     n b^-e / d  (if e <= 0) */
  {
    int cmp;
    double result;

    if (e < 0)
      mpz_mul_2exp (nn, nn, -e);
    else
      mpz_mul_2exp (dd, dd, e);

    /* mpz does not directly support rounded right
       shifts, so we have to do it the hard way.
       For efficiency, we reuse lo and hi.
       hi == quotient, lo == remainder */
    mpz_fdiv_qr (hi, lo, nn, dd);

    /* The fractional part of the unrounded mantissa would be
       remainder/dividend, i.e. lo/dd.  So we have a tie if
       lo/dd = 1/2.  Multiplying both sides by 2*dd yields the
       integer expression 2*lo = dd.  Here we do that comparison
       to decide whether to round up or down. */
    mpz_mul_2exp (lo, lo, 1);
    cmp = mpz_cmp (lo, dd);
    if (cmp > 0 || (cmp == 0 && mpz_odd_p (hi)))
      mpz_add_ui (hi, hi, 1);

    result = ldexp (mpz_get_d (hi), e);
    if (neg)
      result = -result;

    mpz_clears (nn, dd, lo, hi, x, NULL);
    return result;
  }
}

double
scm_i_fraction2double (SCM z)
{
  return scm_i_divide2double (SCM_FRACTION_NUMERATOR (z),
                              SCM_FRACTION_DENOMINATOR (z));
}

static SCM
scm_i_from_double (double val)
{
  SCM z;

  z = PTR2SCM (scm_gc_malloc_pointerless (sizeof (scm_t_double), "real"));

  SCM_SET_CELL_TYPE (z, scm_tc16_real);
  SCM_REAL_VALUE (z) = val;

  return z;
}

SCM_PRIMITIVE_GENERIC (scm_exact_p, "exact?", 1, 0, 0, 
		       (SCM x),
	    "Return @code{#t} if @var{x} is an exact number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_exact_p
{
  if (SCM_INEXACTP (x))
    return SCM_BOOL_F;
  else if (SCM_NUMBERP (x))
    return SCM_BOOL_T;
  else
    SCM_WTA_DISPATCH_1 (g_scm_exact_p, x, 1, s_scm_exact_p);
}
#undef FUNC_NAME

int
scm_is_exact (SCM val)
{
  return scm_is_true (scm_exact_p (val));
}

SCM_PRIMITIVE_GENERIC (scm_inexact_p, "inexact?", 1, 0, 0,
            (SCM x),
	    "Return @code{#t} if @var{x} is an inexact number, @code{#f}\n"
	    "else.")
#define FUNC_NAME s_scm_inexact_p
{
  if (SCM_INEXACTP (x))
    return SCM_BOOL_T;
  else if (SCM_NUMBERP (x))
    return SCM_BOOL_F;
  else
    SCM_WTA_DISPATCH_1 (g_scm_inexact_p, x, 1, s_scm_inexact_p);
}
#undef FUNC_NAME

int
scm_is_inexact (SCM val)
{
  return scm_is_true (scm_inexact_p (val));
}

SCM_PRIMITIVE_GENERIC (scm_odd_p, "odd?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is an odd number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_odd_p
{
  if (SCM_I_INUMP (n))
    {
      scm_t_inum val = SCM_I_INUM (n);
      return scm_from_bool ((val & 1L) != 0);
    }
  else if (SCM_BIGP (n))
    {
      int odd_p = mpz_odd_p (SCM_I_BIG_MPZ (n));
      scm_remember_upto_here_1 (n);
      return scm_from_bool (odd_p);
    }
  else if (SCM_REALP (n))
    {
      double val = SCM_REAL_VALUE (n);
      if (isfinite (val))
	{
	  double rem = fabs (fmod (val, 2.0));
	  if (rem == 1.0)
	    return SCM_BOOL_T;
	  else if (rem == 0.0)
	    return SCM_BOOL_F;
	}
    }
  SCM_WTA_DISPATCH_1 (g_scm_odd_p, n, 1, s_scm_odd_p);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_even_p, "even?", 1, 0, 0, 
            (SCM n),
	    "Return @code{#t} if @var{n} is an even number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_even_p
{
  if (SCM_I_INUMP (n))
    {
      scm_t_inum val = SCM_I_INUM (n);
      return scm_from_bool ((val & 1L) == 0);
    }
  else if (SCM_BIGP (n))
    {
      int even_p = mpz_even_p (SCM_I_BIG_MPZ (n));
      scm_remember_upto_here_1 (n);
      return scm_from_bool (even_p);
    }
  else if (SCM_REALP (n))
    {
      double val = SCM_REAL_VALUE (n);
      if (isfinite (val))
	{
	  double rem = fabs (fmod (val, 2.0));
	  if (rem == 1.0)
	    return SCM_BOOL_F;
	  else if (rem == 0.0)
	    return SCM_BOOL_T;
	}
    }
  SCM_WTA_DISPATCH_1 (g_scm_even_p, n, 1, s_scm_even_p);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_finite_p, "finite?", 1, 0, 0,
		       (SCM x),
	    "Return @code{#t} if the real number @var{x} is neither\n"
	    "infinite nor a NaN, @code{#f} otherwise.")
#define FUNC_NAME s_scm_finite_p
{
  if (SCM_REALP (x))
    return scm_from_bool (isfinite (SCM_REAL_VALUE (x)));
  else if (scm_is_real (x))
    return SCM_BOOL_T;
  else
    SCM_WTA_DISPATCH_1 (g_scm_finite_p, x, 1, s_scm_finite_p);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_inf_p, "inf?", 1, 0, 0, 
		       (SCM x),
	"Return @code{#t} if the real number @var{x} is @samp{+inf.0} or\n"
        "@samp{-inf.0}.  Otherwise return @code{#f}.")
#define FUNC_NAME s_scm_inf_p
{
  if (SCM_REALP (x))
    return scm_from_bool (isinf (SCM_REAL_VALUE (x)));
  else if (scm_is_real (x))
    return SCM_BOOL_F;
  else
    SCM_WTA_DISPATCH_1 (g_scm_inf_p, x, 1, s_scm_inf_p);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_nan_p, "nan?", 1, 0, 0, 
		       (SCM x),
	    "Return @code{#t} if the real number @var{x} is a NaN,\n"
            "or @code{#f} otherwise.")
#define FUNC_NAME s_scm_nan_p
{
  if (SCM_REALP (x))
    return scm_from_bool (isnan (SCM_REAL_VALUE (x)));
  else if (scm_is_real (x))
    return SCM_BOOL_F;
  else
    SCM_WTA_DISPATCH_1 (g_scm_nan_p, x, 1, s_scm_nan_p);
}
#undef FUNC_NAME

/* Guile's idea of infinity.  */
static double guile_Inf;

/* Guile's idea of not a number.  */
static double guile_NaN;

static void
guile_ieee_init (void)
{
/* Some version of gcc on some old version of Linux used to crash when
   trying to make Inf and NaN.  */

#ifdef INFINITY
  /* C99 INFINITY, when available.
     FIXME: The standard allows for INFINITY to be something that overflows
     at compile time.  We ought to have a configure test to check for that
     before trying to use it.  (But in practice we believe this is not a
     problem on any system guile is likely to target.)  */
  guile_Inf = INFINITY;
#elif defined HAVE_DINFINITY
  /* OSF */
  extern unsigned int DINFINITY[2];
  guile_Inf = (*((double *) (DINFINITY)));
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

#ifdef NAN
  /* C99 NAN, when available */
  guile_NaN = NAN;
#elif defined HAVE_DQNAN
  {
    /* OSF */
    extern unsigned int DQNAN[2];
    guile_NaN = (*((double *)(DQNAN)));
  }
#else
  guile_NaN = guile_Inf / guile_Inf;
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
  return scm_i_from_double (guile_Inf);
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
  return scm_i_from_double (guile_NaN);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_abs, "abs", 1, 0, 0,
		       (SCM x),
		       "Return the absolute value of @var{x}.")
#define FUNC_NAME s_scm_abs
{
  if (SCM_I_INUMP (x))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (xx >= 0)
	return x;
      else if (SCM_POSFIXABLE (-xx))
	return SCM_I_MAKINUM (-xx);
      else
	return scm_i_inum2big (-xx);
    }
  else if (SCM_LIKELY (SCM_REALP (x)))
    {
      double xx = SCM_REAL_VALUE (x);
      /* If x is a NaN then xx<0 is false so we return x unchanged */
      if (xx < 0.0)
        return scm_i_from_double (-xx);
      /* Handle signed zeroes properly */
      else if (SCM_UNLIKELY (xx == 0.0))
	return flo0;
      else
        return x;
    }
  else if (SCM_BIGP (x))
    {
      const int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
      if (sgn < 0)
	return scm_i_clonebig (x, 0);
      else
	return x;
    }
  else if (SCM_FRACTIONP (x))
    {
      if (scm_is_false (scm_negative_p (SCM_FRACTION_NUMERATOR (x))))
	return x;
      return scm_i_make_ratio_already_reduced
	(scm_difference (SCM_FRACTION_NUMERATOR (x), SCM_UNDEFINED),
	 SCM_FRACTION_DENOMINATOR (x));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_abs, x, 1, s_scm_abs);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_quotient, "quotient", 2, 0, 0,
		       (SCM x, SCM y),
	"Return the quotient of the numbers @var{x} and @var{y}.")
#define FUNC_NAME s_scm_quotient
{
  if (SCM_LIKELY (scm_is_integer (x)))
    {
      if (SCM_LIKELY (scm_is_integer (y)))
	return scm_truncate_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_quotient, x, y, SCM_ARG2, s_scm_quotient);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_quotient, x, y, SCM_ARG1, s_scm_quotient);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_remainder, "remainder", 2, 0, 0,
		       (SCM x, SCM y),
	"Return the remainder of the numbers @var{x} and @var{y}.\n"
	"@lisp\n"
	"(remainder 13 4) @result{} 1\n"
	"(remainder -13 4) @result{} -1\n"
	"@end lisp")
#define FUNC_NAME s_scm_remainder
{
  if (SCM_LIKELY (scm_is_integer (x)))
    {
      if (SCM_LIKELY (scm_is_integer (y)))
	return scm_truncate_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_remainder, x, y, SCM_ARG2, s_scm_remainder);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_remainder, x, y, SCM_ARG1, s_scm_remainder);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_modulo, "modulo", 2, 0, 0,
		       (SCM x, SCM y),
	"Return the modulo of the numbers @var{x} and @var{y}.\n"
	"@lisp\n"
	"(modulo 13 4) @result{} 1\n"
	"(modulo -13 4) @result{} 3\n"
	"@end lisp")
#define FUNC_NAME s_scm_modulo
{
  if (SCM_LIKELY (scm_is_integer (x)))
    {
      if (SCM_LIKELY (scm_is_integer (y)))
	return scm_floor_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_modulo, x, y, SCM_ARG2, s_scm_modulo);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_modulo, x, y, SCM_ARG1, s_scm_modulo);
}
#undef FUNC_NAME

/* Return the exact integer q such that n = q*d, for exact integers n
   and d, where d is known in advance to divide n evenly (with zero
   remainder).  For large integers, this can be computed more
   efficiently than when the remainder is unknown. */
static SCM
scm_exact_integer_quotient (SCM n, SCM d)
#define FUNC_NAME "exact-integer-quotient"
{
  if (SCM_LIKELY (SCM_I_INUMP (n)))
    {
      scm_t_inum nn = SCM_I_INUM (n);
      if (SCM_LIKELY (SCM_I_INUMP (d)))
	{
	  scm_t_inum dd = SCM_I_INUM (d);
	  if (SCM_UNLIKELY (dd == 0))
	    scm_num_overflow ("exact-integer-quotient");
	  else
	    {
	      scm_t_inum qq = nn / dd;
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		return SCM_I_MAKINUM (qq);
	      else
		return scm_i_inum2big (qq);
	    }
	}
      else if (SCM_LIKELY (SCM_BIGP (d)))
	{
	  /* n is an inum and d is a bignum.  Given that d is known to
	     divide n evenly, there are only two possibilities: n is 0,
	     or else n is fixnum-min and d is abs(fixnum-min). */
	  if (nn == 0)
	    return SCM_INUM0;
	  else
	    return SCM_I_MAKINUM (-1);
	}
      else
	SCM_WRONG_TYPE_ARG (2, d);
    }
  else if (SCM_LIKELY (SCM_BIGP (n)))
    {
      if (SCM_LIKELY (SCM_I_INUMP (d)))
	{
	  scm_t_inum dd = SCM_I_INUM (d);
	  if (SCM_UNLIKELY (dd == 0))
	    scm_num_overflow ("exact-integer-quotient");
	  else if (SCM_UNLIKELY (dd == 1))
	    return n;
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      if (dd > 0)
		mpz_divexact_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (n), dd);
	      else
		{
		  mpz_divexact_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (n), -dd);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		}
	      scm_remember_upto_here_1 (n);
	      return scm_i_normbig (q);
	    }
	}
      else if (SCM_LIKELY (SCM_BIGP (d)))
	{
	  SCM q = scm_i_mkbig ();
	  mpz_divexact (SCM_I_BIG_MPZ (q),
			SCM_I_BIG_MPZ (n),
			SCM_I_BIG_MPZ (d));
	  scm_remember_upto_here_2 (n, d);
	  return scm_i_normbig (q);
	}
      else
	SCM_WRONG_TYPE_ARG (2, d);
    }
  else
    SCM_WRONG_TYPE_ARG (1, n);
}
#undef FUNC_NAME

/* two_valued_wta_dispatch_2 is a version of SCM_WTA_DISPATCH_2 for
   two-valued functions.  It is called from primitive generics that take
   two arguments and return two values, when the core procedure is
   unable to handle the given argument types.  If there are GOOPS
   methods for this primitive generic, it dispatches to GOOPS and, if
   successful, expects two values to be returned, which are placed in
   *rp1 and *rp2.  If there are no GOOPS methods, it throws a
   wrong-type-arg exception.

   FIXME: This obviously belongs somewhere else, but until we decide on
   the right API, it is here as a static function, because it is needed
   by the *_divide functions below.
*/
static void
two_valued_wta_dispatch_2 (SCM gf, SCM a1, SCM a2, int pos,
			   const char *subr, SCM *rp1, SCM *rp2)
{
  if (SCM_UNPACK (gf))
    scm_i_extract_values_2 (scm_call_generic_2 (gf, a1, a2), rp1, rp2);
  else
    scm_wrong_type_arg (subr, pos, (pos == SCM_ARG1) ? a1 : a2);
}

SCM_DEFINE (scm_euclidean_quotient, "euclidean-quotient", 2, 0, 0,
	    (SCM x, SCM y),
	    "Return the integer @var{q} such that\n"
	    "@math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
	    "where @math{0 <= @var{r} < abs(@var{y})}.\n"
	    "@lisp\n"
	    "(euclidean-quotient 123 10) @result{} 12\n"
	    "(euclidean-quotient 123 -10) @result{} -12\n"
	    "(euclidean-quotient -123 10) @result{} -13\n"
	    "(euclidean-quotient -123 -10) @result{} 13\n"
	    "(euclidean-quotient -123.2 -63.5) @result{} 2.0\n"
	    "(euclidean-quotient 16/3 -10/7) @result{} -3\n"
	    "@end lisp")
#define FUNC_NAME s_scm_euclidean_quotient
{
  if (scm_is_false (scm_negative_p (y)))
    return scm_floor_quotient (x, y);
  else
    return scm_ceiling_quotient (x, y);
}
#undef FUNC_NAME

SCM_DEFINE (scm_euclidean_remainder, "euclidean-remainder", 2, 0, 0,
	    (SCM x, SCM y),
	    "Return the real number @var{r} such that\n"
	    "@math{0 <= @var{r} < abs(@var{y})} and\n"
	    "@math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
	    "for some integer @var{q}.\n"
	    "@lisp\n"
	    "(euclidean-remainder 123 10) @result{} 3\n"
	    "(euclidean-remainder 123 -10) @result{} 3\n"
	    "(euclidean-remainder -123 10) @result{} 7\n"
	    "(euclidean-remainder -123 -10) @result{} 7\n"
	    "(euclidean-remainder -123.2 -63.5) @result{} 3.8\n"
	    "(euclidean-remainder 16/3 -10/7) @result{} 22/21\n"
	    "@end lisp")
#define FUNC_NAME s_scm_euclidean_remainder
{
  if (scm_is_false (scm_negative_p (y)))
    return scm_floor_remainder (x, y);
  else
    return scm_ceiling_remainder (x, y);
}
#undef FUNC_NAME

SCM_DEFINE (scm_i_euclidean_divide, "euclidean/", 2, 0, 0,
	    (SCM x, SCM y),
	    "Return the integer @var{q} and the real number @var{r}\n"
	    "such that @math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
	    "and @math{0 <= @var{r} < abs(@var{y})}.\n"
	    "@lisp\n"
	    "(euclidean/ 123 10) @result{} 12 and 3\n"
	    "(euclidean/ 123 -10) @result{} -12 and 3\n"
	    "(euclidean/ -123 10) @result{} -13 and 7\n"
	    "(euclidean/ -123 -10) @result{} 13 and 7\n"
	    "(euclidean/ -123.2 -63.5) @result{} 2.0 and 3.8\n"
	    "(euclidean/ 16/3 -10/7) @result{} -3 and 22/21\n"
	    "@end lisp")
#define FUNC_NAME s_scm_i_euclidean_divide
{
  if (scm_is_false (scm_negative_p (y)))
    return scm_i_floor_divide (x, y);
  else
    return scm_i_ceiling_divide (x, y);
}
#undef FUNC_NAME

void
scm_euclidean_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  if (scm_is_false (scm_negative_p (y)))
    return scm_floor_divide (x, y, qp, rp);
  else
    return scm_ceiling_divide (x, y, qp, rp);
}

static SCM scm_i_inexact_floor_quotient (double x, double y);
static SCM scm_i_exact_rational_floor_quotient (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_floor_quotient, "floor-quotient", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the floor of @math{@var{x} / @var{y}}.\n"
		       "@lisp\n"
		       "(floor-quotient 123 10) @result{} 12\n"
		       "(floor-quotient 123 -10) @result{} -13\n"
		       "(floor-quotient -123 10) @result{} -13\n"
		       "(floor-quotient -123 -10) @result{} 12\n"
		       "(floor-quotient -123.2 -63.5) @result{} 1.0\n"
		       "(floor-quotient 16/3 -10/7) @result{} -4\n"
		       "@end lisp")
#define FUNC_NAME s_scm_floor_quotient
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  scm_t_inum xx1 = xx;
	  scm_t_inum qq;
	  if (SCM_LIKELY (yy > 0))
	    {
	      if (SCM_UNLIKELY (xx < 0))
		xx1 = xx - yy + 1;
	    }
	  else if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_floor_quotient);
	  else if (xx > 0)
	    xx1 = xx - yy - 1;
	  qq = xx1 / yy;
	  if (SCM_LIKELY (SCM_FIXABLE (qq)))
	    return SCM_I_MAKINUM (qq);
	  else
	    return scm_i_inum2big (qq);
	}
      else if (SCM_BIGP (y))
	{
	  int sign = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  if (sign > 0)
	    return SCM_I_MAKINUM ((xx < 0) ? -1 : 0);
	  else
	    return SCM_I_MAKINUM ((xx > 0) ? -1 : 0);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_floor_quotient (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_quotient, x, y, SCM_ARG2,
			    s_scm_floor_quotient);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_floor_quotient);
	  else if (SCM_UNLIKELY (yy == 1))
	    return x;
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      if (yy > 0)
		mpz_fdiv_q_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (x), yy);
	      else
		{
		  mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		}
	      scm_remember_upto_here_1 (x);
	      return scm_i_normbig (q);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM q = scm_i_mkbig ();
	  mpz_fdiv_q (SCM_I_BIG_MPZ (q),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (q);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_floor_quotient
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_quotient, x, y, SCM_ARG2,
			    s_scm_floor_quotient);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_floor_quotient
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_quotient, x, y, SCM_ARG2,
			    s_scm_floor_quotient);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_floor_quotient
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_quotient, x, y, SCM_ARG2,
			    s_scm_floor_quotient);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_floor_quotient, x, y, SCM_ARG1,
			s_scm_floor_quotient);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_floor_quotient (double x, double y)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_floor_quotient);  /* or return a NaN? */
  else
    return scm_i_from_double (floor (x / y));
}

static SCM
scm_i_exact_rational_floor_quotient (SCM x, SCM y)
{
  return scm_floor_quotient
    (scm_product (scm_numerator (x), scm_denominator (y)),
     scm_product (scm_numerator (y), scm_denominator (x)));
}

static SCM scm_i_inexact_floor_remainder (double x, double y);
static SCM scm_i_exact_rational_floor_remainder (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_floor_remainder, "floor-remainder", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the real number @var{r} such that\n"
		       "@math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "where @math{@var{q} = floor(@var{x} / @var{y})}.\n"
		       "@lisp\n"
		       "(floor-remainder 123 10) @result{} 3\n"
		       "(floor-remainder 123 -10) @result{} -7\n"
		       "(floor-remainder -123 10) @result{} 7\n"
		       "(floor-remainder -123 -10) @result{} -3\n"
		       "(floor-remainder -123.2 -63.5) @result{} -59.7\n"
		       "(floor-remainder 16/3 -10/7) @result{} -8/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_floor_remainder
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_floor_remainder);
	  else
	    {
	      scm_t_inum rr = xx % yy;
	      int needs_adjustment;

	      if (SCM_LIKELY (yy > 0))
		needs_adjustment = (rr < 0);
	      else
		needs_adjustment = (rr > 0);

	      if (needs_adjustment)
		rr += yy;
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  int sign = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  if (sign > 0)
	    {
	      if (xx < 0)
		{
		  SCM r = scm_i_mkbig ();
		  mpz_sub_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), -xx);
		  scm_remember_upto_here_1 (y);
		  return scm_i_normbig (r);
		}
	      else
		return x;
	    }
	  else if (xx <= 0)
	    return x;
	  else
	    {
	      SCM r = scm_i_mkbig ();
	      mpz_add_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), xx);
	      scm_remember_upto_here_1 (y);
	      return scm_i_normbig (r);
	    }
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_floor_remainder (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_remainder, x, y, SCM_ARG2,
			    s_scm_floor_remainder);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_floor_remainder);
	  else
	    {
	      scm_t_inum rr;
	      if (yy > 0)
		rr = mpz_fdiv_ui (SCM_I_BIG_MPZ (x), yy);
	      else
		rr = -mpz_cdiv_ui (SCM_I_BIG_MPZ (x), -yy);
	      scm_remember_upto_here_1 (x);
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM r = scm_i_mkbig ();
	  mpz_fdiv_r (SCM_I_BIG_MPZ (r),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (r);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_floor_remainder
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_remainder, x, y, SCM_ARG2,
			    s_scm_floor_remainder);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_floor_remainder
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_remainder, x, y, SCM_ARG2,
			    s_scm_floor_remainder);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_floor_remainder
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_floor_remainder, x, y, SCM_ARG2,
			    s_scm_floor_remainder);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_floor_remainder, x, y, SCM_ARG1,
			s_scm_floor_remainder);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_floor_remainder (double x, double y)
{
  /* Although it would be more efficient to use fmod here, we can't
     because it would in some cases produce results inconsistent with
     scm_i_inexact_floor_quotient, such that x != q * y + r (not even
     close).  In particular, when x is very close to a multiple of y,
     then r might be either 0.0 or y, but those two cases must
     correspond to different choices of q.  If r = 0.0 then q must be
     x/y, and if r = y then q must be x/y-1.  If quotient chooses one
     and remainder chooses the other, it would be bad.  */
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_floor_remainder);  /* or return a NaN? */
  else
    return scm_i_from_double (x - y * floor (x / y));
}

static SCM
scm_i_exact_rational_floor_remainder (SCM x, SCM y)
{
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);
  SCM r1 = scm_floor_remainder (scm_product (scm_numerator (x), yd),
				scm_product (scm_numerator (y), xd));
  return scm_divide (r1, scm_product (xd, yd));
}


static void scm_i_inexact_floor_divide (double x, double y,
					SCM *qp, SCM *rp);
static void scm_i_exact_rational_floor_divide (SCM x, SCM y,
					       SCM *qp, SCM *rp);

SCM_PRIMITIVE_GENERIC (scm_i_floor_divide, "floor/", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the integer @var{q} and the real number @var{r}\n"
		       "such that @math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "and @math{@var{q} = floor(@var{x} / @var{y})}.\n"
		       "@lisp\n"
		       "(floor/ 123 10) @result{} 12 and 3\n"
		       "(floor/ 123 -10) @result{} -13 and -7\n"
		       "(floor/ -123 10) @result{} -13 and 7\n"
		       "(floor/ -123 -10) @result{} 12 and -3\n"
		       "(floor/ -123.2 -63.5) @result{} 1.0 and -59.7\n"
		       "(floor/ 16/3 -10/7) @result{} -4 and -8/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_i_floor_divide
{
  SCM q, r;

  scm_floor_divide(x, y, &q, &r);
  return scm_values (scm_list_2 (q, r));
}
#undef FUNC_NAME

#define s_scm_floor_divide s_scm_i_floor_divide
#define g_scm_floor_divide g_scm_i_floor_divide

void
scm_floor_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_floor_divide);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      int needs_adjustment;

	      if (SCM_LIKELY (yy > 0))
		needs_adjustment = (rr < 0);
	      else
		needs_adjustment = (rr > 0);

	      if (needs_adjustment)
		{
		  rr += yy;
		  qq--;
		}

	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		*qp = SCM_I_MAKINUM (qq);
	      else
		*qp = scm_i_inum2big (qq);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  int sign = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  if (sign > 0)
	    {
	      if (xx < 0)
		{
		  SCM r = scm_i_mkbig ();
		  mpz_sub_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), -xx);
		  scm_remember_upto_here_1 (y);
		  *qp = SCM_I_MAKINUM (-1);
		  *rp = scm_i_normbig (r);
		}
	      else
		{
		  *qp = SCM_INUM0;
		  *rp = x;
		}
	    }
	  else if (xx <= 0)
	    {
	      *qp = SCM_INUM0;
	      *rp = x;
	    }
	  else
	    {
	      SCM r = scm_i_mkbig ();
	      mpz_add_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), xx);
	      scm_remember_upto_here_1 (y);
	      *qp = SCM_I_MAKINUM (-1);
	      *rp = scm_i_normbig (r);
	    }
	  return;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_floor_divide (xx, SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_floor_divide, x, y, SCM_ARG2,
					  s_scm_floor_divide, qp, rp);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_floor_divide);
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      SCM r = scm_i_mkbig ();
	      if (yy > 0)
		mpz_fdiv_qr_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
				SCM_I_BIG_MPZ (x), yy);
	      else
		{
		  mpz_cdiv_qr_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
				  SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		}
	      scm_remember_upto_here_1 (x);
	      *qp = scm_i_normbig (q);
	      *rp = scm_i_normbig (r);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  SCM q = scm_i_mkbig ();
	  SCM r = scm_i_mkbig ();
	  mpz_fdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
		       SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  *qp = scm_i_normbig (q);
	  *rp = scm_i_normbig (r);
	  return;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_floor_divide
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_floor_divide, x, y, SCM_ARG2,
					  s_scm_floor_divide, qp, rp);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_floor_divide
	  (SCM_REAL_VALUE (x), scm_to_double (y), qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_floor_divide, x, y, SCM_ARG2,
					  s_scm_floor_divide, qp, rp);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_floor_divide
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_floor_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_floor_divide, x, y, SCM_ARG2,
					  s_scm_floor_divide, qp, rp);
    }
  else
    return two_valued_wta_dispatch_2 (g_scm_floor_divide, x, y, SCM_ARG1,
				      s_scm_floor_divide, qp, rp);
}

static void
scm_i_inexact_floor_divide (double x, double y, SCM *qp, SCM *rp)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_floor_divide);  /* or return a NaN? */
  else
    {
      double q = floor (x / y);
      double r = x - q * y;
      *qp = scm_i_from_double (q);
      *rp = scm_i_from_double (r);
    }
}

static void
scm_i_exact_rational_floor_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  SCM r1;
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);

  scm_floor_divide (scm_product (scm_numerator (x), yd),
		    scm_product (scm_numerator (y), xd),
		    qp, &r1);
  *rp = scm_divide (r1, scm_product (xd, yd));
}

static SCM scm_i_inexact_ceiling_quotient (double x, double y);
static SCM scm_i_exact_rational_ceiling_quotient (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_ceiling_quotient, "ceiling-quotient", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the ceiling of @math{@var{x} / @var{y}}.\n"
		       "@lisp\n"
		       "(ceiling-quotient 123 10) @result{} 13\n"
		       "(ceiling-quotient 123 -10) @result{} -12\n"
		       "(ceiling-quotient -123 10) @result{} -12\n"
		       "(ceiling-quotient -123 -10) @result{} 13\n"
		       "(ceiling-quotient -123.2 -63.5) @result{} 2.0\n"
		       "(ceiling-quotient 16/3 -10/7) @result{} -3\n"
		       "@end lisp")
#define FUNC_NAME s_scm_ceiling_quotient
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_ceiling_quotient);
	  else
	    {
	      scm_t_inum xx1 = xx;
	      scm_t_inum qq;
	      if (SCM_LIKELY (yy > 0))
		{
		  if (SCM_LIKELY (xx >= 0))
		    xx1 = xx + yy - 1;
		}
	      else if (xx < 0)
		xx1 = xx + yy + 1;
	      qq = xx1 / yy;
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		return SCM_I_MAKINUM (qq);
	      else
		return scm_i_inum2big (qq);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  int sign = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  if (SCM_LIKELY (sign > 0))
	    {
	      if (SCM_LIKELY (xx > 0))
		return SCM_INUM1;
	      else if (SCM_UNLIKELY (xx == SCM_MOST_NEGATIVE_FIXNUM)
		       && SCM_UNLIKELY (mpz_cmp_ui (SCM_I_BIG_MPZ (y),
				       - SCM_MOST_NEGATIVE_FIXNUM) == 0))
		{
		  /* Special case: x == fixnum-min && y == abs (fixnum-min) */
		  scm_remember_upto_here_1 (y);
		  return SCM_I_MAKINUM (-1);
		}
	      else
		return SCM_INUM0;
	    }
	  else if (xx >= 0)
	    return SCM_INUM0;
	  else
	    return SCM_INUM1;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_ceiling_quotient (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_quotient, x, y, SCM_ARG2,
			    s_scm_ceiling_quotient);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_ceiling_quotient);
	  else if (SCM_UNLIKELY (yy == 1))
	    return x;
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      if (yy > 0)
		mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (x), yy);
	      else
		{
		  mpz_fdiv_q_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		}
	      scm_remember_upto_here_1 (x);
	      return scm_i_normbig (q);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM q = scm_i_mkbig ();
	  mpz_cdiv_q (SCM_I_BIG_MPZ (q),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (q);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_ceiling_quotient
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_quotient, x, y, SCM_ARG2,
			    s_scm_ceiling_quotient);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_ceiling_quotient
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_quotient, x, y, SCM_ARG2,
			    s_scm_ceiling_quotient);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_ceiling_quotient
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_quotient, x, y, SCM_ARG2,
			    s_scm_ceiling_quotient);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_ceiling_quotient, x, y, SCM_ARG1,
			s_scm_ceiling_quotient);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_ceiling_quotient (double x, double y)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_ceiling_quotient);  /* or return a NaN? */
  else
    return scm_i_from_double (ceil (x / y));
}

static SCM
scm_i_exact_rational_ceiling_quotient (SCM x, SCM y)
{
  return scm_ceiling_quotient
    (scm_product (scm_numerator (x), scm_denominator (y)),
     scm_product (scm_numerator (y), scm_denominator (x)));
}

static SCM scm_i_inexact_ceiling_remainder (double x, double y);
static SCM scm_i_exact_rational_ceiling_remainder (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_ceiling_remainder, "ceiling-remainder", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the real number @var{r} such that\n"
		       "@math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "where @math{@var{q} = ceiling(@var{x} / @var{y})}.\n"
		       "@lisp\n"
		       "(ceiling-remainder 123 10) @result{} -7\n"
		       "(ceiling-remainder 123 -10) @result{} 3\n"
		       "(ceiling-remainder -123 10) @result{} -3\n"
		       "(ceiling-remainder -123 -10) @result{} 7\n"
		       "(ceiling-remainder -123.2 -63.5) @result{} 3.8\n"
		       "(ceiling-remainder 16/3 -10/7) @result{} 22/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_ceiling_remainder
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_ceiling_remainder);
	  else
	    {
	      scm_t_inum rr = xx % yy;
	      int needs_adjustment;

	      if (SCM_LIKELY (yy > 0))
		needs_adjustment = (rr > 0);
	      else
		needs_adjustment = (rr < 0);

	      if (needs_adjustment)
		rr -= yy;
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  int sign = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  if (SCM_LIKELY (sign > 0))
	    {
	      if (SCM_LIKELY (xx > 0))
		{
		  SCM r = scm_i_mkbig ();
		  mpz_sub_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), xx);
		  scm_remember_upto_here_1 (y);
		  mpz_neg (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r));
		  return scm_i_normbig (r);
		}
	      else if (SCM_UNLIKELY (xx == SCM_MOST_NEGATIVE_FIXNUM)
		       && SCM_UNLIKELY (mpz_cmp_ui (SCM_I_BIG_MPZ (y),
				       - SCM_MOST_NEGATIVE_FIXNUM) == 0))
		{
		  /* Special case: x == fixnum-min && y == abs (fixnum-min) */
		  scm_remember_upto_here_1 (y);
		  return SCM_INUM0;
		}
	      else
		return x;
	    }
	  else if (xx >= 0)
	    return x;
	  else
	    {
	      SCM r = scm_i_mkbig ();
	      mpz_add_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), -xx);
	      scm_remember_upto_here_1 (y);
	      mpz_neg (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r));
	      return scm_i_normbig (r);
	    }
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_ceiling_remainder (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_remainder, x, y, SCM_ARG2,
			    s_scm_ceiling_remainder);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_ceiling_remainder);
	  else
	    {
	      scm_t_inum rr;
	      if (yy > 0)
		rr = -mpz_cdiv_ui (SCM_I_BIG_MPZ (x), yy);
	      else
		rr = mpz_fdiv_ui (SCM_I_BIG_MPZ (x), -yy);
	      scm_remember_upto_here_1 (x);
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM r = scm_i_mkbig ();
	  mpz_cdiv_r (SCM_I_BIG_MPZ (r),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (r);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_ceiling_remainder
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_remainder, x, y, SCM_ARG2,
			    s_scm_ceiling_remainder);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_ceiling_remainder
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_remainder, x, y, SCM_ARG2,
			    s_scm_ceiling_remainder);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_ceiling_remainder
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_ceiling_remainder, x, y, SCM_ARG2,
			    s_scm_ceiling_remainder);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_ceiling_remainder, x, y, SCM_ARG1,
			s_scm_ceiling_remainder);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_ceiling_remainder (double x, double y)
{
  /* Although it would be more efficient to use fmod here, we can't
     because it would in some cases produce results inconsistent with
     scm_i_inexact_ceiling_quotient, such that x != q * y + r (not even
     close).  In particular, when x is very close to a multiple of y,
     then r might be either 0.0 or -y, but those two cases must
     correspond to different choices of q.  If r = 0.0 then q must be
     x/y, and if r = -y then q must be x/y+1.  If quotient chooses one
     and remainder chooses the other, it would be bad.  */
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_ceiling_remainder);  /* or return a NaN? */
  else
    return scm_i_from_double (x - y * ceil (x / y));
}

static SCM
scm_i_exact_rational_ceiling_remainder (SCM x, SCM y)
{
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);
  SCM r1 = scm_ceiling_remainder (scm_product (scm_numerator (x), yd),
				  scm_product (scm_numerator (y), xd));
  return scm_divide (r1, scm_product (xd, yd));
}

static void scm_i_inexact_ceiling_divide (double x, double y,
					  SCM *qp, SCM *rp);
static void scm_i_exact_rational_ceiling_divide (SCM x, SCM y,
						 SCM *qp, SCM *rp);

SCM_PRIMITIVE_GENERIC (scm_i_ceiling_divide, "ceiling/", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the integer @var{q} and the real number @var{r}\n"
		       "such that @math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "and @math{@var{q} = ceiling(@var{x} / @var{y})}.\n"
		       "@lisp\n"
		       "(ceiling/ 123 10) @result{} 13 and -7\n"
		       "(ceiling/ 123 -10) @result{} -12 and 3\n"
		       "(ceiling/ -123 10) @result{} -12 and -3\n"
		       "(ceiling/ -123 -10) @result{} 13 and 7\n"
		       "(ceiling/ -123.2 -63.5) @result{} 2.0 and 3.8\n"
		       "(ceiling/ 16/3 -10/7) @result{} -3 and 22/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_i_ceiling_divide
{
  SCM q, r;

  scm_ceiling_divide(x, y, &q, &r);
  return scm_values (scm_list_2 (q, r));
}
#undef FUNC_NAME

#define s_scm_ceiling_divide s_scm_i_ceiling_divide
#define g_scm_ceiling_divide g_scm_i_ceiling_divide

void
scm_ceiling_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_ceiling_divide);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      int needs_adjustment;

	      if (SCM_LIKELY (yy > 0))
		needs_adjustment = (rr > 0);
	      else
		needs_adjustment = (rr < 0);

	      if (needs_adjustment)
		{
		  rr -= yy;
		  qq++;
		}
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		*qp = SCM_I_MAKINUM (qq);
	      else
		*qp = scm_i_inum2big (qq);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  int sign = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  if (SCM_LIKELY (sign > 0))
	    {
	      if (SCM_LIKELY (xx > 0))
		{
		  SCM r = scm_i_mkbig ();
		  mpz_sub_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), xx);
		  scm_remember_upto_here_1 (y);
		  mpz_neg (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r));
		  *qp = SCM_INUM1;
		  *rp = scm_i_normbig (r);
		}
	      else if (SCM_UNLIKELY (xx == SCM_MOST_NEGATIVE_FIXNUM)
		       && SCM_UNLIKELY (mpz_cmp_ui (SCM_I_BIG_MPZ (y),
				       - SCM_MOST_NEGATIVE_FIXNUM) == 0))
		{
		  /* Special case: x == fixnum-min && y == abs (fixnum-min) */
		  scm_remember_upto_here_1 (y);
		  *qp = SCM_I_MAKINUM (-1);
		  *rp = SCM_INUM0;
		}
	      else
		{
		  *qp = SCM_INUM0;
		  *rp = x;
		}
	    }
	  else if (xx >= 0)
	    {
	      *qp = SCM_INUM0;
	      *rp = x;
	    }
	  else
	    {
	      SCM r = scm_i_mkbig ();
	      mpz_add_ui (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y), -xx);
	      scm_remember_upto_here_1 (y);
	      mpz_neg (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r));
	      *qp = SCM_INUM1;
	      *rp = scm_i_normbig (r);
	    }
	  return;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_ceiling_divide (xx, SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_ceiling_divide, x, y, SCM_ARG2,
					  s_scm_ceiling_divide, qp, rp);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_ceiling_divide);
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      SCM r = scm_i_mkbig ();
	      if (yy > 0)
		mpz_cdiv_qr_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
				SCM_I_BIG_MPZ (x), yy);
	      else
		{
		  mpz_fdiv_qr_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
				  SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		}
	      scm_remember_upto_here_1 (x);
	      *qp = scm_i_normbig (q);
	      *rp = scm_i_normbig (r);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  SCM q = scm_i_mkbig ();
	  SCM r = scm_i_mkbig ();
	  mpz_cdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
		       SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  *qp = scm_i_normbig (q);
	  *rp = scm_i_normbig (r);
	  return;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_ceiling_divide
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_ceiling_divide, x, y, SCM_ARG2,
					  s_scm_ceiling_divide, qp, rp);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_ceiling_divide
	  (SCM_REAL_VALUE (x), scm_to_double (y), qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_ceiling_divide, x, y, SCM_ARG2,
					  s_scm_ceiling_divide, qp, rp);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_ceiling_divide
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_ceiling_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_ceiling_divide, x, y, SCM_ARG2,
					  s_scm_ceiling_divide, qp, rp);
    }
  else
    return two_valued_wta_dispatch_2 (g_scm_ceiling_divide, x, y, SCM_ARG1,
				      s_scm_ceiling_divide, qp, rp);
}

static void
scm_i_inexact_ceiling_divide (double x, double y, SCM *qp, SCM *rp)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_ceiling_divide);  /* or return a NaN? */
  else
    {
      double q = ceil (x / y);
      double r = x - q * y;
      *qp = scm_i_from_double (q);
      *rp = scm_i_from_double (r);
    }
}

static void
scm_i_exact_rational_ceiling_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  SCM r1;
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);

  scm_ceiling_divide (scm_product (scm_numerator (x), yd),
		      scm_product (scm_numerator (y), xd),
		      qp, &r1);
  *rp = scm_divide (r1, scm_product (xd, yd));
}

static SCM scm_i_inexact_truncate_quotient (double x, double y);
static SCM scm_i_exact_rational_truncate_quotient (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_truncate_quotient, "truncate-quotient", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return @math{@var{x} / @var{y}} rounded toward zero.\n"
		       "@lisp\n"
		       "(truncate-quotient 123 10) @result{} 12\n"
		       "(truncate-quotient 123 -10) @result{} -12\n"
		       "(truncate-quotient -123 10) @result{} -12\n"
		       "(truncate-quotient -123 -10) @result{} 12\n"
		       "(truncate-quotient -123.2 -63.5) @result{} 1.0\n"
		       "(truncate-quotient 16/3 -10/7) @result{} -3\n"
		       "@end lisp")
#define FUNC_NAME s_scm_truncate_quotient
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_truncate_quotient);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		return SCM_I_MAKINUM (qq);
	      else
		return scm_i_inum2big (qq);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  if (SCM_UNLIKELY (xx == SCM_MOST_NEGATIVE_FIXNUM)
	      && SCM_UNLIKELY (mpz_cmp_ui (SCM_I_BIG_MPZ (y),
					   - SCM_MOST_NEGATIVE_FIXNUM) == 0))
	    {
	      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
	      scm_remember_upto_here_1 (y);
	      return SCM_I_MAKINUM (-1);
	    }
	  else
	    return SCM_INUM0;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_truncate_quotient (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_quotient, x, y, SCM_ARG2,
			    s_scm_truncate_quotient);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_truncate_quotient);
	  else if (SCM_UNLIKELY (yy == 1))
	    return x;
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      if (yy > 0)
		mpz_tdiv_q_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (x), yy);
	      else
		{
		  mpz_tdiv_q_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		}
	      scm_remember_upto_here_1 (x);
	      return scm_i_normbig (q);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM q = scm_i_mkbig ();
	  mpz_tdiv_q (SCM_I_BIG_MPZ (q),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (q);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_truncate_quotient
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_quotient, x, y, SCM_ARG2,
			    s_scm_truncate_quotient);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_truncate_quotient
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_quotient, x, y, SCM_ARG2,
			    s_scm_truncate_quotient);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_truncate_quotient
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_quotient, x, y, SCM_ARG2,
			    s_scm_truncate_quotient);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_truncate_quotient, x, y, SCM_ARG1,
			s_scm_truncate_quotient);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_truncate_quotient (double x, double y)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_truncate_quotient);  /* or return a NaN? */
  else
    return scm_i_from_double (trunc (x / y));
}

static SCM
scm_i_exact_rational_truncate_quotient (SCM x, SCM y)
{
  return scm_truncate_quotient
    (scm_product (scm_numerator (x), scm_denominator (y)),
     scm_product (scm_numerator (y), scm_denominator (x)));
}

static SCM scm_i_inexact_truncate_remainder (double x, double y);
static SCM scm_i_exact_rational_truncate_remainder (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_truncate_remainder, "truncate-remainder", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the real number @var{r} such that\n"
		       "@math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "where @math{@var{q} = truncate(@var{x} / @var{y})}.\n"
		       "@lisp\n"
		       "(truncate-remainder 123 10) @result{} 3\n"
		       "(truncate-remainder 123 -10) @result{} 3\n"
		       "(truncate-remainder -123 10) @result{} -3\n"
		       "(truncate-remainder -123 -10) @result{} -3\n"
		       "(truncate-remainder -123.2 -63.5) @result{} -59.7\n"
		       "(truncate-remainder 16/3 -10/7) @result{} 22/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_truncate_remainder
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_truncate_remainder);
	  else
	    return SCM_I_MAKINUM (xx % yy);
	}
      else if (SCM_BIGP (y))
	{
	  if (SCM_UNLIKELY (xx == SCM_MOST_NEGATIVE_FIXNUM)
	      && SCM_UNLIKELY (mpz_cmp_ui (SCM_I_BIG_MPZ (y),
					   - SCM_MOST_NEGATIVE_FIXNUM) == 0))
	    {
	      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
	      scm_remember_upto_here_1 (y);
	      return SCM_INUM0;
	    }
	  else
	    return x;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_truncate_remainder (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_remainder, x, y, SCM_ARG2,
			    s_scm_truncate_remainder);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_truncate_remainder);
	  else
	    {
	      scm_t_inum rr = (mpz_tdiv_ui (SCM_I_BIG_MPZ (x),
					    (yy > 0) ? yy : -yy)
			       * mpz_sgn (SCM_I_BIG_MPZ (x)));
	      scm_remember_upto_here_1 (x);
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  SCM r = scm_i_mkbig ();
	  mpz_tdiv_r (SCM_I_BIG_MPZ (r),
		      SCM_I_BIG_MPZ (x),
		      SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_i_normbig (r);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_truncate_remainder
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_remainder, x, y, SCM_ARG2,
			    s_scm_truncate_remainder);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_truncate_remainder
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_remainder, x, y, SCM_ARG2,
			    s_scm_truncate_remainder);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_truncate_remainder
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_truncate_remainder, x, y, SCM_ARG2,
			    s_scm_truncate_remainder);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_truncate_remainder, x, y, SCM_ARG1,
			s_scm_truncate_remainder);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_truncate_remainder (double x, double y)
{
  /* Although it would be more efficient to use fmod here, we can't
     because it would in some cases produce results inconsistent with
     scm_i_inexact_truncate_quotient, such that x != q * y + r (not even
     close).  In particular, when x is very close to a multiple of y,
     then r might be either 0.0 or sgn(x)*|y|, but those two cases must
     correspond to different choices of q.  If quotient chooses one and
     remainder chooses the other, it would be bad.  */
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_truncate_remainder);  /* or return a NaN? */
  else
    return scm_i_from_double (x - y * trunc (x / y));
}

static SCM
scm_i_exact_rational_truncate_remainder (SCM x, SCM y)
{
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);
  SCM r1 = scm_truncate_remainder (scm_product (scm_numerator (x), yd),
				   scm_product (scm_numerator (y), xd));
  return scm_divide (r1, scm_product (xd, yd));
}


static void scm_i_inexact_truncate_divide (double x, double y,
					   SCM *qp, SCM *rp);
static void scm_i_exact_rational_truncate_divide (SCM x, SCM y,
						  SCM *qp, SCM *rp);

SCM_PRIMITIVE_GENERIC (scm_i_truncate_divide, "truncate/", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the integer @var{q} and the real number @var{r}\n"
		       "such that @math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "and @math{@var{q} = truncate(@var{x} / @var{y})}.\n"
		       "@lisp\n"
		       "(truncate/ 123 10) @result{} 12 and 3\n"
		       "(truncate/ 123 -10) @result{} -12 and 3\n"
		       "(truncate/ -123 10) @result{} -12 and -3\n"
		       "(truncate/ -123 -10) @result{} 12 and -3\n"
		       "(truncate/ -123.2 -63.5) @result{} 1.0 and -59.7\n"
		       "(truncate/ 16/3 -10/7) @result{} -3 and 22/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_i_truncate_divide
{
  SCM q, r;

  scm_truncate_divide(x, y, &q, &r);
  return scm_values (scm_list_2 (q, r));
}
#undef FUNC_NAME

#define s_scm_truncate_divide s_scm_i_truncate_divide
#define g_scm_truncate_divide g_scm_i_truncate_divide

void
scm_truncate_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_truncate_divide);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		*qp = SCM_I_MAKINUM (qq);
	      else
		*qp = scm_i_inum2big (qq);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  if (SCM_UNLIKELY (xx == SCM_MOST_NEGATIVE_FIXNUM)
	      && SCM_UNLIKELY (mpz_cmp_ui (SCM_I_BIG_MPZ (y),
					   - SCM_MOST_NEGATIVE_FIXNUM) == 0))
	    {
	      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
	      scm_remember_upto_here_1 (y);
	      *qp = SCM_I_MAKINUM (-1);
	      *rp = SCM_INUM0;
	    }
	  else
	    {
	      *qp = SCM_INUM0;
	      *rp = x;
	    }
	  return;
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_truncate_divide (xx, SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_truncate_divide, x, y, SCM_ARG2,
	   s_scm_truncate_divide, qp, rp);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_truncate_divide);
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      scm_t_inum rr;
	      if (yy > 0)
		rr = mpz_tdiv_q_ui (SCM_I_BIG_MPZ (q),
				    SCM_I_BIG_MPZ (x), yy);
	      else
		{
		  rr = mpz_tdiv_q_ui (SCM_I_BIG_MPZ (q),
				      SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		}
	      rr *= mpz_sgn (SCM_I_BIG_MPZ (x));
	      scm_remember_upto_here_1 (x);
	      *qp = scm_i_normbig (q);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  SCM q = scm_i_mkbig ();
	  SCM r = scm_i_mkbig ();
	  mpz_tdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
		       SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  *qp = scm_i_normbig (q);
	  *rp = scm_i_normbig (r);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_truncate_divide
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_truncate_divide, x, y, SCM_ARG2,
	   s_scm_truncate_divide, qp, rp);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_truncate_divide
	  (SCM_REAL_VALUE (x), scm_to_double (y), qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_truncate_divide, x, y, SCM_ARG2,
	   s_scm_truncate_divide, qp, rp);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_truncate_divide
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_truncate_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_truncate_divide, x, y, SCM_ARG2,
	   s_scm_truncate_divide, qp, rp);
    }
  else
    return two_valued_wta_dispatch_2 (g_scm_truncate_divide, x, y, SCM_ARG1,
				      s_scm_truncate_divide, qp, rp);
}

static void
scm_i_inexact_truncate_divide (double x, double y, SCM *qp, SCM *rp)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_truncate_divide);  /* or return a NaN? */
  else
    {
      double q = trunc (x / y);
      double r = x - q * y;
      *qp = scm_i_from_double (q);
      *rp = scm_i_from_double (r);
    }
}

static void
scm_i_exact_rational_truncate_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  SCM r1;
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);

  scm_truncate_divide (scm_product (scm_numerator (x), yd),
		       scm_product (scm_numerator (y), xd),
		       qp, &r1);
  *rp = scm_divide (r1, scm_product (xd, yd));
}

static SCM scm_i_inexact_centered_quotient (double x, double y);
static SCM scm_i_bigint_centered_quotient (SCM x, SCM y);
static SCM scm_i_exact_rational_centered_quotient (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_centered_quotient, "centered-quotient", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the integer @var{q} such that\n"
		       "@math{@var{x} = @var{q}*@var{y} + @var{r}} where\n"
		       "@math{-abs(@var{y}/2) <= @var{r} < abs(@var{y}/2)}.\n"
		       "@lisp\n"
		       "(centered-quotient 123 10) @result{} 12\n"
		       "(centered-quotient 123 -10) @result{} -12\n"
		       "(centered-quotient -123 10) @result{} -12\n"
		       "(centered-quotient -123 -10) @result{} 12\n"
		       "(centered-quotient -123.2 -63.5) @result{} 2.0\n"
		       "(centered-quotient 16/3 -10/7) @result{} -4\n"
		       "@end lisp")
#define FUNC_NAME s_scm_centered_quotient
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_centered_quotient);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      if (SCM_LIKELY (xx > 0))
		{
		  if (SCM_LIKELY (yy > 0))
		    {
		      if (rr >= (yy + 1) / 2)
			qq++;
		    }
		  else
		    {
		      if (rr >= (1 - yy) / 2)
			qq--;
		    }
		}
	      else
		{
		  if (SCM_LIKELY (yy > 0))
		    {
		      if (rr < -yy / 2)
			qq--;
		    }
		  else
		    {
		      if (rr < yy / 2)
			qq++;
		    }
		}
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		return SCM_I_MAKINUM (qq);
	      else
		return scm_i_inum2big (qq);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  /* Pass a denormalized bignum version of x (even though it
	     can fit in a fixnum) to scm_i_bigint_centered_quotient */
	  return scm_i_bigint_centered_quotient (scm_i_long2big (xx), y);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_centered_quotient (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_quotient, x, y, SCM_ARG2,
			    s_scm_centered_quotient);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_centered_quotient);
	  else if (SCM_UNLIKELY (yy == 1))
	    return x;
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      scm_t_inum rr;
	      /* Arrange for rr to initially be non-positive,
		 because that simplifies the test to see
		 if it is within the needed bounds. */
	      if (yy > 0)
		{
		  rr = - mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q),
					SCM_I_BIG_MPZ (x), yy);
		  scm_remember_upto_here_1 (x);
		  if (rr < -yy / 2)
		    mpz_sub_ui (SCM_I_BIG_MPZ (q),
				SCM_I_BIG_MPZ (q), 1);
		}
	      else
		{
		  rr = - mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q),
					SCM_I_BIG_MPZ (x), -yy);
		  scm_remember_upto_here_1 (x);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		  if (rr < yy / 2)
		    mpz_add_ui (SCM_I_BIG_MPZ (q),
				SCM_I_BIG_MPZ (q), 1);
		}
	      return scm_i_normbig (q);
	    }
	}
      else if (SCM_BIGP (y))
	return scm_i_bigint_centered_quotient (x, y);
      else if (SCM_REALP (y))
	return scm_i_inexact_centered_quotient
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_quotient, x, y, SCM_ARG2,
			    s_scm_centered_quotient);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_centered_quotient
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_quotient, x, y, SCM_ARG2,
			    s_scm_centered_quotient);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_centered_quotient
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_quotient, x, y, SCM_ARG2,
			    s_scm_centered_quotient);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_centered_quotient, x, y, SCM_ARG1,
			s_scm_centered_quotient);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_centered_quotient (double x, double y)
{
  if (SCM_LIKELY (y > 0))
    return scm_i_from_double (floor (x/y + 0.5));
  else if (SCM_LIKELY (y < 0))
    return scm_i_from_double (ceil (x/y - 0.5));
  else if (y == 0)
    scm_num_overflow (s_scm_centered_quotient);  /* or return a NaN? */
  else
    return scm_nan ();
}

/* Assumes that both x and y are bigints, though
   x might be able to fit into a fixnum. */
static SCM
scm_i_bigint_centered_quotient (SCM x, SCM y)
{
  SCM q, r, min_r;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  q = scm_i_mkbig ();
  r = scm_i_mkbig ();

  /* min_r will eventually become -abs(y)/2 */
  min_r = scm_i_mkbig ();
  mpz_tdiv_q_2exp (SCM_I_BIG_MPZ (min_r),
		   SCM_I_BIG_MPZ (y), 1);

  /* Arrange for rr to initially be non-positive,
     because that simplifies the test to see
     if it is within the needed bounds. */
  if (mpz_sgn (SCM_I_BIG_MPZ (y)) > 0)
    {
      mpz_cdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
      scm_remember_upto_here_2 (x, y);
      mpz_neg (SCM_I_BIG_MPZ (min_r), SCM_I_BIG_MPZ (min_r));
      if (mpz_cmp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (min_r)) < 0)
	mpz_sub_ui (SCM_I_BIG_MPZ (q),
		    SCM_I_BIG_MPZ (q), 1);
    }
  else
    {
      mpz_fdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
      scm_remember_upto_here_2 (x, y);
      if (mpz_cmp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (min_r)) < 0)
	mpz_add_ui (SCM_I_BIG_MPZ (q),
		    SCM_I_BIG_MPZ (q), 1);
    }
  scm_remember_upto_here_2 (r, min_r);
  return scm_i_normbig (q);
}

static SCM
scm_i_exact_rational_centered_quotient (SCM x, SCM y)
{
  return scm_centered_quotient
    (scm_product (scm_numerator (x), scm_denominator (y)),
     scm_product (scm_numerator (y), scm_denominator (x)));
}

static SCM scm_i_inexact_centered_remainder (double x, double y);
static SCM scm_i_bigint_centered_remainder (SCM x, SCM y);
static SCM scm_i_exact_rational_centered_remainder (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_centered_remainder, "centered-remainder", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the real number @var{r} such that\n"
		       "@math{-abs(@var{y}/2) <= @var{r} < abs(@var{y}/2)}\n"
		       "and @math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "for some integer @var{q}.\n"
		       "@lisp\n"
		       "(centered-remainder 123 10) @result{} 3\n"
		       "(centered-remainder 123 -10) @result{} 3\n"
		       "(centered-remainder -123 10) @result{} -3\n"
		       "(centered-remainder -123 -10) @result{} -3\n"
		       "(centered-remainder -123.2 -63.5) @result{} 3.8\n"
		       "(centered-remainder 16/3 -10/7) @result{} -8/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_centered_remainder
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_centered_remainder);
	  else
	    {
	      scm_t_inum rr = xx % yy;
	      if (SCM_LIKELY (xx > 0))
		{
		  if (SCM_LIKELY (yy > 0))
		    {
		      if (rr >= (yy + 1) / 2)
			rr -= yy;
		    }
		  else
		    {
		      if (rr >= (1 - yy) / 2)
			rr += yy;
		    }
		}
	      else
		{
		  if (SCM_LIKELY (yy > 0))
		    {
		      if (rr < -yy / 2)
			rr += yy;
		    }
		  else
		    {
		      if (rr < yy / 2)
			rr -= yy;
		    }
		}
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  /* Pass a denormalized bignum version of x (even though it
	     can fit in a fixnum) to scm_i_bigint_centered_remainder */
	  return scm_i_bigint_centered_remainder (scm_i_long2big (xx), y);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_centered_remainder (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_remainder, x, y, SCM_ARG2,
			    s_scm_centered_remainder);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_centered_remainder);
	  else
	    {
	      scm_t_inum rr;
	      /* Arrange for rr to initially be non-positive,
		 because that simplifies the test to see
		 if it is within the needed bounds. */
	      if (yy > 0)
		{
		  rr = - mpz_cdiv_ui (SCM_I_BIG_MPZ (x), yy);
		  scm_remember_upto_here_1 (x);
		  if (rr < -yy / 2)
		    rr += yy;
		}
	      else
		{
		  rr = - mpz_cdiv_ui (SCM_I_BIG_MPZ (x), -yy);
		  scm_remember_upto_here_1 (x);
		  if (rr < yy / 2)
		    rr -= yy;
		}
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	return scm_i_bigint_centered_remainder (x, y);
      else if (SCM_REALP (y))
	return scm_i_inexact_centered_remainder
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_remainder, x, y, SCM_ARG2,
			    s_scm_centered_remainder);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_centered_remainder
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_remainder, x, y, SCM_ARG2,
			    s_scm_centered_remainder);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_centered_remainder
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_centered_remainder, x, y, SCM_ARG2,
			    s_scm_centered_remainder);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_centered_remainder, x, y, SCM_ARG1,
			s_scm_centered_remainder);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_centered_remainder (double x, double y)
{
  double q;

  /* Although it would be more efficient to use fmod here, we can't
     because it would in some cases produce results inconsistent with
     scm_i_inexact_centered_quotient, such that x != r + q * y (not even
     close).  In particular, when x-y/2 is very close to a multiple of
     y, then r might be either -abs(y/2) or abs(y/2)-epsilon, but those
     two cases must correspond to different choices of q.  If quotient
     chooses one and remainder chooses the other, it would be bad. */
  if (SCM_LIKELY (y > 0))
    q = floor (x/y + 0.5);
  else if (SCM_LIKELY (y < 0))
    q = ceil (x/y - 0.5);
  else if (y == 0)
    scm_num_overflow (s_scm_centered_remainder);  /* or return a NaN? */
  else
    return scm_nan ();
  return scm_i_from_double (x - q * y);
}

/* Assumes that both x and y are bigints, though
   x might be able to fit into a fixnum. */
static SCM
scm_i_bigint_centered_remainder (SCM x, SCM y)
{
  SCM r, min_r;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  r = scm_i_mkbig ();

  /* min_r will eventually become -abs(y)/2 */
  min_r = scm_i_mkbig ();
  mpz_tdiv_q_2exp (SCM_I_BIG_MPZ (min_r),
		   SCM_I_BIG_MPZ (y), 1);

  /* Arrange for rr to initially be non-positive,
     because that simplifies the test to see
     if it is within the needed bounds. */
  if (mpz_sgn (SCM_I_BIG_MPZ (y)) > 0)
    {
      mpz_cdiv_r (SCM_I_BIG_MPZ (r),
		  SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
      mpz_neg (SCM_I_BIG_MPZ (min_r), SCM_I_BIG_MPZ (min_r));
      if (mpz_cmp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (min_r)) < 0)
	mpz_add (SCM_I_BIG_MPZ (r),
		 SCM_I_BIG_MPZ (r),
		 SCM_I_BIG_MPZ (y));
    }
  else
    {
      mpz_fdiv_r (SCM_I_BIG_MPZ (r),
		  SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
      if (mpz_cmp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (min_r)) < 0)
	mpz_sub (SCM_I_BIG_MPZ (r),
		 SCM_I_BIG_MPZ (r),
		 SCM_I_BIG_MPZ (y));
    }
  scm_remember_upto_here_2 (x, y);
  return scm_i_normbig (r);
}

static SCM
scm_i_exact_rational_centered_remainder (SCM x, SCM y)
{
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);
  SCM r1 = scm_centered_remainder (scm_product (scm_numerator (x), yd),
				   scm_product (scm_numerator (y), xd));
  return scm_divide (r1, scm_product (xd, yd));
}


static void scm_i_inexact_centered_divide (double x, double y,
					   SCM *qp, SCM *rp);
static void scm_i_bigint_centered_divide (SCM x, SCM y, SCM *qp, SCM *rp);
static void scm_i_exact_rational_centered_divide (SCM x, SCM y,
						  SCM *qp, SCM *rp);

SCM_PRIMITIVE_GENERIC (scm_i_centered_divide, "centered/", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the integer @var{q} and the real number @var{r}\n"
		       "such that @math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "and @math{-abs(@var{y}/2) <= @var{r} < abs(@var{y}/2)}.\n"
		       "@lisp\n"
		       "(centered/ 123 10) @result{} 12 and 3\n"
		       "(centered/ 123 -10) @result{} -12 and 3\n"
		       "(centered/ -123 10) @result{} -12 and -3\n"
		       "(centered/ -123 -10) @result{} 12 and -3\n"
		       "(centered/ -123.2 -63.5) @result{} 2.0 and 3.8\n"
		       "(centered/ 16/3 -10/7) @result{} -4 and -8/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_i_centered_divide
{
  SCM q, r;

  scm_centered_divide(x, y, &q, &r);
  return scm_values (scm_list_2 (q, r));
}
#undef FUNC_NAME

#define s_scm_centered_divide s_scm_i_centered_divide
#define g_scm_centered_divide g_scm_i_centered_divide

void
scm_centered_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_centered_divide);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      if (SCM_LIKELY (xx > 0))
		{
		  if (SCM_LIKELY (yy > 0))
		    {
		      if (rr >= (yy + 1) / 2)
			{ qq++; rr -= yy; }
		    }
		  else
		    {
		      if (rr >= (1 - yy) / 2)
			{ qq--; rr += yy; }
		    }
		}
	      else
		{
		  if (SCM_LIKELY (yy > 0))
		    {
		      if (rr < -yy / 2)
			{ qq--; rr += yy; }
		    }
		  else
		    {
		      if (rr < yy / 2)
			{ qq++; rr -= yy; }
		    }
		}
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		*qp = SCM_I_MAKINUM (qq);
	      else
		*qp = scm_i_inum2big (qq);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  /* Pass a denormalized bignum version of x (even though it
	     can fit in a fixnum) to scm_i_bigint_centered_divide */
	  return scm_i_bigint_centered_divide (scm_i_long2big (xx), y, qp, rp);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_centered_divide (xx, SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_centered_divide, x, y, SCM_ARG2,
	   s_scm_centered_divide, qp, rp);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_centered_divide);
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      scm_t_inum rr;
	      /* Arrange for rr to initially be non-positive,
		 because that simplifies the test to see
		 if it is within the needed bounds. */
	      if (yy > 0)
		{
		  rr = - mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q),
					SCM_I_BIG_MPZ (x), yy);
		  scm_remember_upto_here_1 (x);
		  if (rr < -yy / 2)
		    {
		      mpz_sub_ui (SCM_I_BIG_MPZ (q),
				  SCM_I_BIG_MPZ (q), 1);
		      rr += yy;
		    }
		}
	      else
		{
		  rr = - mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q),
					SCM_I_BIG_MPZ (x), -yy);
		  scm_remember_upto_here_1 (x);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		  if (rr < yy / 2)
		    {
		      mpz_add_ui (SCM_I_BIG_MPZ (q),
				  SCM_I_BIG_MPZ (q), 1);
		      rr -= yy;
		    }
		}
	      *qp = scm_i_normbig (q);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	return scm_i_bigint_centered_divide (x, y, qp, rp);
      else if (SCM_REALP (y))
	return scm_i_inexact_centered_divide
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_centered_divide, x, y, SCM_ARG2,
	   s_scm_centered_divide, qp, rp);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_centered_divide
	  (SCM_REAL_VALUE (x), scm_to_double (y), qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_centered_divide, x, y, SCM_ARG2,
	   s_scm_centered_divide, qp, rp);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_centered_divide
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_centered_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2
	  (g_scm_centered_divide, x, y, SCM_ARG2,
	   s_scm_centered_divide, qp, rp);
    }
  else
    return two_valued_wta_dispatch_2 (g_scm_centered_divide, x, y, SCM_ARG1,
				      s_scm_centered_divide, qp, rp);
}

static void
scm_i_inexact_centered_divide (double x, double y, SCM *qp, SCM *rp)
{
  double q, r;

  if (SCM_LIKELY (y > 0))
    q = floor (x/y + 0.5);
  else if (SCM_LIKELY (y < 0))
    q = ceil (x/y - 0.5);
  else if (y == 0)
    scm_num_overflow (s_scm_centered_divide);  /* or return a NaN? */
  else
    q = guile_NaN;
  r = x - q * y;
  *qp = scm_i_from_double (q);
  *rp = scm_i_from_double (r);
}

/* Assumes that both x and y are bigints, though
   x might be able to fit into a fixnum. */
static void
scm_i_bigint_centered_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  SCM q, r, min_r;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  q = scm_i_mkbig ();
  r = scm_i_mkbig ();

  /* min_r will eventually become -abs(y/2) */
  min_r = scm_i_mkbig ();
  mpz_tdiv_q_2exp (SCM_I_BIG_MPZ (min_r),
		   SCM_I_BIG_MPZ (y), 1);

  /* Arrange for rr to initially be non-positive,
     because that simplifies the test to see
     if it is within the needed bounds. */
  if (mpz_sgn (SCM_I_BIG_MPZ (y)) > 0)
    {
      mpz_cdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
      mpz_neg (SCM_I_BIG_MPZ (min_r), SCM_I_BIG_MPZ (min_r));
      if (mpz_cmp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (min_r)) < 0)
	{
	  mpz_sub_ui (SCM_I_BIG_MPZ (q),
		      SCM_I_BIG_MPZ (q), 1);
	  mpz_add (SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (y));
	}
    }
  else
    {
      mpz_fdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
      if (mpz_cmp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (min_r)) < 0)
	{
	  mpz_add_ui (SCM_I_BIG_MPZ (q),
		      SCM_I_BIG_MPZ (q), 1);
	  mpz_sub (SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (r),
		   SCM_I_BIG_MPZ (y));
	}
    }
  scm_remember_upto_here_2 (x, y);
  *qp = scm_i_normbig (q);
  *rp = scm_i_normbig (r);
}

static void
scm_i_exact_rational_centered_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  SCM r1;
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);

  scm_centered_divide (scm_product (scm_numerator (x), yd),
		       scm_product (scm_numerator (y), xd),
		       qp, &r1);
  *rp = scm_divide (r1, scm_product (xd, yd));
}

static SCM scm_i_inexact_round_quotient (double x, double y);
static SCM scm_i_bigint_round_quotient (SCM x, SCM y);
static SCM scm_i_exact_rational_round_quotient (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_round_quotient, "round-quotient", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return @math{@var{x} / @var{y}} to the nearest integer,\n"
		       "with ties going to the nearest even integer.\n"
		       "@lisp\n"
		       "(round-quotient 123 10) @result{} 12\n"
		       "(round-quotient 123 -10) @result{} -12\n"
		       "(round-quotient -123 10) @result{} -12\n"
		       "(round-quotient -123 -10) @result{} 12\n"
		       "(round-quotient 125 10) @result{} 12\n"
		       "(round-quotient 127 10) @result{} 13\n"
		       "(round-quotient 135 10) @result{} 14\n"
		       "(round-quotient -123.2 -63.5) @result{} 2.0\n"
		       "(round-quotient 16/3 -10/7) @result{} -4\n"
		       "@end lisp")
#define FUNC_NAME s_scm_round_quotient
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_round_quotient);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      scm_t_inum ay = yy;
	      scm_t_inum r2 = 2 * rr;

	      if (SCM_LIKELY (yy < 0))
		{
		  ay = -ay;
		  r2 = -r2;
		}

	      if (qq & 1L)
		{
		  if (r2 >= ay)
		    qq++;
		  else if (r2 <= -ay)
		    qq--;
		}
	      else
		{
		  if (r2 > ay)
		    qq++;
		  else if (r2 < -ay)
		    qq--;
		}
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		return SCM_I_MAKINUM (qq);
	      else
		return scm_i_inum2big (qq);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  /* Pass a denormalized bignum version of x (even though it
	     can fit in a fixnum) to scm_i_bigint_round_quotient */
	  return scm_i_bigint_round_quotient (scm_i_long2big (xx), y);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_round_quotient (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_quotient, x, y, SCM_ARG2,
			    s_scm_round_quotient);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_round_quotient);
	  else if (SCM_UNLIKELY (yy == 1))
	    return x;
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      scm_t_inum rr;
	      int needs_adjustment;

	      if (yy > 0)
		{
		  rr = mpz_fdiv_q_ui (SCM_I_BIG_MPZ (q),
				      SCM_I_BIG_MPZ (x), yy);
		  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
		    needs_adjustment = (2*rr >= yy);
		  else
		    needs_adjustment = (2*rr > yy);
		}
	      else
		{
		  rr = - mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q),
					SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
		    needs_adjustment = (2*rr <= yy);
		  else
		    needs_adjustment = (2*rr < yy);
		}
	      scm_remember_upto_here_1 (x);
	      if (needs_adjustment)
		mpz_add_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q), 1);
	      return scm_i_normbig (q);
	    }
	}
      else if (SCM_BIGP (y))
	return scm_i_bigint_round_quotient (x, y);
      else if (SCM_REALP (y))
	return scm_i_inexact_round_quotient
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_quotient, x, y, SCM_ARG2,
			    s_scm_round_quotient);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_round_quotient
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_quotient, x, y, SCM_ARG2,
			    s_scm_round_quotient);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_round_quotient
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_quotient (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_quotient, x, y, SCM_ARG2,
			    s_scm_round_quotient);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_round_quotient, x, y, SCM_ARG1,
			s_scm_round_quotient);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_round_quotient (double x, double y)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_round_quotient);  /* or return a NaN? */
  else
    return scm_i_from_double (scm_c_round (x / y));
}

/* Assumes that both x and y are bigints, though
   x might be able to fit into a fixnum. */
static SCM
scm_i_bigint_round_quotient (SCM x, SCM y)
{
  SCM q, r, r2;
  int cmp, needs_adjustment;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  q = scm_i_mkbig ();
  r = scm_i_mkbig ();
  r2 = scm_i_mkbig ();

  mpz_fdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
	       SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
  mpz_mul_2exp (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (r), 1);  /* r2 = 2*r */
  scm_remember_upto_here_2 (x, r);

  cmp = mpz_cmpabs (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (y));
  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);
  scm_remember_upto_here_2 (r2, y);

  if (needs_adjustment)
    mpz_add_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q), 1);

  return scm_i_normbig (q);
}

static SCM
scm_i_exact_rational_round_quotient (SCM x, SCM y)
{
  return scm_round_quotient
    (scm_product (scm_numerator (x), scm_denominator (y)),
     scm_product (scm_numerator (y), scm_denominator (x)));
}

static SCM scm_i_inexact_round_remainder (double x, double y);
static SCM scm_i_bigint_round_remainder (SCM x, SCM y);
static SCM scm_i_exact_rational_round_remainder (SCM x, SCM y);

SCM_PRIMITIVE_GENERIC (scm_round_remainder, "round-remainder", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the real number @var{r} such that\n"
		       "@math{@var{x} = @var{q}*@var{y} + @var{r}}, where\n"
		       "@var{q} is @math{@var{x} / @var{y}} rounded to the\n"
		       "nearest integer, with ties going to the nearest\n"
		       "even integer.\n"
		       "@lisp\n"
		       "(round-remainder 123 10) @result{} 3\n"
		       "(round-remainder 123 -10) @result{} 3\n"
		       "(round-remainder -123 10) @result{} -3\n"
		       "(round-remainder -123 -10) @result{} -3\n"
		       "(round-remainder 125 10) @result{} 5\n"
		       "(round-remainder 127 10) @result{} -3\n"
		       "(round-remainder 135 10) @result{} -5\n"
		       "(round-remainder -123.2 -63.5) @result{} 3.8\n"
		       "(round-remainder 16/3 -10/7) @result{} -8/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_round_remainder
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_round_remainder);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      scm_t_inum ay = yy;
	      scm_t_inum r2 = 2 * rr;

	      if (SCM_LIKELY (yy < 0))
		{
		  ay = -ay;
		  r2 = -r2;
		}

	      if (qq & 1L)
		{
		  if (r2 >= ay)
		    rr -= yy;
		  else if (r2 <= -ay)
		    rr += yy;
		}
	      else
		{
		  if (r2 > ay)
		    rr -= yy;
		  else if (r2 < -ay)
		    rr += yy;
		}
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	{
	  /* Pass a denormalized bignum version of x (even though it
	     can fit in a fixnum) to scm_i_bigint_round_remainder */
	  return scm_i_bigint_round_remainder
	    (scm_i_long2big (xx), y);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_round_remainder (xx, SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_remainder, x, y, SCM_ARG2,
			    s_scm_round_remainder);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_round_remainder);
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      scm_t_inum rr;
	      int needs_adjustment;

	      if (yy > 0)
		{
		  rr = mpz_fdiv_q_ui (SCM_I_BIG_MPZ (q),
				      SCM_I_BIG_MPZ (x), yy);
		  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
		    needs_adjustment = (2*rr >= yy);
		  else
		    needs_adjustment = (2*rr > yy);
		}
	      else
		{
		  rr = - mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q),
					SCM_I_BIG_MPZ (x), -yy);
		  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
		    needs_adjustment = (2*rr <= yy);
		  else
		    needs_adjustment = (2*rr < yy);
		}
	      scm_remember_upto_here_2 (x, q);
	      if (needs_adjustment)
		rr -= yy;
	      return SCM_I_MAKINUM (rr);
	    }
	}
      else if (SCM_BIGP (y))
	return scm_i_bigint_round_remainder (x, y);
      else if (SCM_REALP (y))
	return scm_i_inexact_round_remainder
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_remainder, x, y, SCM_ARG2,
			    s_scm_round_remainder);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_round_remainder
	  (SCM_REAL_VALUE (x), scm_to_double (y));
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_remainder, x, y, SCM_ARG2,
			    s_scm_round_remainder);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_round_remainder
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y));
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_remainder (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_round_remainder, x, y, SCM_ARG2,
			    s_scm_round_remainder);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_round_remainder, x, y, SCM_ARG1,
			s_scm_round_remainder);
}
#undef FUNC_NAME

static SCM
scm_i_inexact_round_remainder (double x, double y)
{
  /* Although it would be more efficient to use fmod here, we can't
     because it would in some cases produce results inconsistent with
     scm_i_inexact_round_quotient, such that x != r + q * y (not even
     close).  In particular, when x-y/2 is very close to a multiple of
     y, then r might be either -abs(y/2) or abs(y/2), but those two
     cases must correspond to different choices of q.  If quotient
     chooses one and remainder chooses the other, it would be bad. */

  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_round_remainder);  /* or return a NaN? */
  else
    {
      double q = scm_c_round (x / y);
      return scm_i_from_double (x - q * y);
    }
}

/* Assumes that both x and y are bigints, though
   x might be able to fit into a fixnum. */
static SCM
scm_i_bigint_round_remainder (SCM x, SCM y)
{
  SCM q, r, r2;
  int cmp, needs_adjustment;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  q = scm_i_mkbig ();
  r = scm_i_mkbig ();
  r2 = scm_i_mkbig ();

  mpz_fdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
	       SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
  scm_remember_upto_here_1 (x);
  mpz_mul_2exp (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (r), 1);  /* r2 = 2*r */

  cmp = mpz_cmpabs (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (y));
  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);
  scm_remember_upto_here_2 (q, r2);

  if (needs_adjustment)
    mpz_sub (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y));

  scm_remember_upto_here_1 (y);
  return scm_i_normbig (r);
}

static SCM
scm_i_exact_rational_round_remainder (SCM x, SCM y)
{
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);
  SCM r1 = scm_round_remainder (scm_product (scm_numerator (x), yd),
				scm_product (scm_numerator (y), xd));
  return scm_divide (r1, scm_product (xd, yd));
}


static void scm_i_inexact_round_divide (double x, double y, SCM *qp, SCM *rp);
static void scm_i_bigint_round_divide (SCM x, SCM y, SCM *qp, SCM *rp);
static void scm_i_exact_rational_round_divide (SCM x, SCM y, SCM *qp, SCM *rp);

SCM_PRIMITIVE_GENERIC (scm_i_round_divide, "round/", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return the integer @var{q} and the real number @var{r}\n"
		       "such that @math{@var{x} = @var{q}*@var{y} + @var{r}}\n"
		       "and @var{q} is @math{@var{x} / @var{y}} rounded to the\n"
		       "nearest integer, with ties going to the nearest even integer.\n"
		       "@lisp\n"
		       "(round/ 123 10) @result{} 12 and 3\n"
		       "(round/ 123 -10) @result{} -12 and 3\n"
		       "(round/ -123 10) @result{} -12 and -3\n"
		       "(round/ -123 -10) @result{} 12 and -3\n"
		       "(round/ 125 10) @result{} 12 and 5\n"
		       "(round/ 127 10) @result{} 13 and -3\n"
		       "(round/ 135 10) @result{} 14 and -5\n"
		       "(round/ -123.2 -63.5) @result{} 2.0 and 3.8\n"
		       "(round/ 16/3 -10/7) @result{} -4 and -8/21\n"
		       "@end lisp")
#define FUNC_NAME s_scm_i_round_divide
{
  SCM q, r;

  scm_round_divide(x, y, &q, &r);
  return scm_values (scm_list_2 (q, r));
}
#undef FUNC_NAME

#define s_scm_round_divide s_scm_i_round_divide
#define g_scm_round_divide g_scm_i_round_divide

void
scm_round_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_round_divide);
	  else
	    {
	      scm_t_inum qq = xx / yy;
	      scm_t_inum rr = xx % yy;
	      scm_t_inum ay = yy;
	      scm_t_inum r2 = 2 * rr;

	      if (SCM_LIKELY (yy < 0))
		{
		  ay = -ay;
		  r2 = -r2;
		}

	      if (qq & 1L)
		{
		  if (r2 >= ay)
		    { qq++; rr -= yy; }
		  else if (r2 <= -ay)
		    { qq--; rr += yy; }
		}
	      else
		{
		  if (r2 > ay)
		    { qq++; rr -= yy; }
		  else if (r2 < -ay)
		    { qq--; rr += yy; }
		}
	      if (SCM_LIKELY (SCM_FIXABLE (qq)))
		*qp = SCM_I_MAKINUM (qq);
	      else
		*qp = scm_i_inum2big (qq);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	{
	  /* Pass a denormalized bignum version of x (even though it
	     can fit in a fixnum) to scm_i_bigint_round_divide */
	  return scm_i_bigint_round_divide
	    (scm_i_long2big (SCM_I_INUM (x)), y, qp, rp);
	}
      else if (SCM_REALP (y))
	return scm_i_inexact_round_divide (xx, SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_round_divide, x, y, SCM_ARG2,
					  s_scm_round_divide, qp, rp);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (SCM_UNLIKELY (yy == 0))
	    scm_num_overflow (s_scm_round_divide);
	  else
	    {
	      SCM q = scm_i_mkbig ();
	      scm_t_inum rr;
	      int needs_adjustment;

	      if (yy > 0)
		{
		  rr = mpz_fdiv_q_ui (SCM_I_BIG_MPZ (q),
				      SCM_I_BIG_MPZ (x), yy);
		  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
		    needs_adjustment = (2*rr >= yy);
		  else
		    needs_adjustment = (2*rr > yy);
		}
	      else
		{
		  rr = - mpz_cdiv_q_ui (SCM_I_BIG_MPZ (q),
					SCM_I_BIG_MPZ (x), -yy);
		  mpz_neg (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q));
		  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
		    needs_adjustment = (2*rr <= yy);
		  else
		    needs_adjustment = (2*rr < yy);
		}
	      scm_remember_upto_here_1 (x);
	      if (needs_adjustment)
		{
		  mpz_add_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q), 1);
		  rr -= yy;
		}
	      *qp = scm_i_normbig (q);
	      *rp = SCM_I_MAKINUM (rr);
	    }
	  return;
	}
      else if (SCM_BIGP (y))
	return scm_i_bigint_round_divide (x, y, qp, rp);
      else if (SCM_REALP (y))
	return scm_i_inexact_round_divide
	  (scm_i_big2dbl (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_round_divide, x, y, SCM_ARG2,
					  s_scm_round_divide, qp, rp);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_REALP (y) || SCM_I_INUMP (y) ||
	  SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_inexact_round_divide
	  (SCM_REAL_VALUE (x), scm_to_double (y), qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_round_divide, x, y, SCM_ARG2,
					  s_scm_round_divide, qp, rp);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_REALP (y))
	return scm_i_inexact_round_divide
	  (scm_i_fraction2double (x), SCM_REAL_VALUE (y), qp, rp);
      else if (SCM_I_INUMP (y) || SCM_BIGP (y) || SCM_FRACTIONP (y))
	return scm_i_exact_rational_round_divide (x, y, qp, rp);
      else
	return two_valued_wta_dispatch_2 (g_scm_round_divide, x, y, SCM_ARG2,
					  s_scm_round_divide, qp, rp);
    }
  else
    return two_valued_wta_dispatch_2 (g_scm_round_divide, x, y, SCM_ARG1,
				      s_scm_round_divide, qp, rp);
}

static void
scm_i_inexact_round_divide (double x, double y, SCM *qp, SCM *rp)
{
  if (SCM_UNLIKELY (y == 0))
    scm_num_overflow (s_scm_round_divide);  /* or return a NaN? */
  else
    {
      double q = scm_c_round (x / y);
      double r = x - q * y;
      *qp = scm_i_from_double (q);
      *rp = scm_i_from_double (r);
    }
}

/* Assumes that both x and y are bigints, though
   x might be able to fit into a fixnum. */
static void
scm_i_bigint_round_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  SCM q, r, r2;
  int cmp, needs_adjustment;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  q = scm_i_mkbig ();
  r = scm_i_mkbig ();
  r2 = scm_i_mkbig ();

  mpz_fdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
	       SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
  scm_remember_upto_here_1 (x);
  mpz_mul_2exp (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (r), 1);  /* r2 = 2*r */

  cmp = mpz_cmpabs (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (y));
  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);

  if (needs_adjustment)
    {
      mpz_add_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q), 1);
      mpz_sub (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (y));
    }

  scm_remember_upto_here_2 (r2, y);
  *qp = scm_i_normbig (q);
  *rp = scm_i_normbig (r);
}

static void
scm_i_exact_rational_round_divide (SCM x, SCM y, SCM *qp, SCM *rp)
{
  SCM r1;
  SCM xd = scm_denominator (x);
  SCM yd = scm_denominator (y);

  scm_round_divide (scm_product (scm_numerator (x), yd),
		    scm_product (scm_numerator (y), xd),
		    qp, &r1);
  *rp = scm_divide (r1, scm_product (xd, yd));
}


SCM_PRIMITIVE_GENERIC (scm_i_gcd, "gcd", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return the greatest common divisor of all parameter values.\n"
                       "If called without arguments, 0 is returned.")
#define FUNC_NAME s_scm_i_gcd
{
  while (!scm_is_null (rest))
    { x = scm_gcd (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_gcd (x, y);
}
#undef FUNC_NAME
                       
#define s_gcd s_scm_i_gcd
#define g_gcd g_scm_i_gcd

SCM
scm_gcd (SCM x, SCM y)
{
  if (SCM_UNLIKELY (SCM_UNBNDP (y)))
    return SCM_UNBNDP (x) ? SCM_INUM0 : scm_abs (x);
  
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
        {
          scm_t_inum xx = SCM_I_INUM (x);
          scm_t_inum yy = SCM_I_INUM (y);
          scm_t_inum u = xx < 0 ? -xx : xx;
          scm_t_inum v = yy < 0 ? -yy : yy;
          scm_t_inum result;
          if (SCM_UNLIKELY (xx == 0))
	    result = v;
	  else if (SCM_UNLIKELY (yy == 0))
	    result = u;
	  else
	    {
	      int k = 0;
	      /* Determine a common factor 2^k */
	      while (((u | v) & 1) == 0)
		{
		  k++;
		  u >>= 1;
		  v >>= 1;
		}
	      /* Now, any factor 2^n can be eliminated */
	      if ((u & 1) == 0)
		while ((u & 1) == 0)
		  u >>= 1;
	      else
		while ((v & 1) == 0)
		  v >>= 1;
	      /* Both u and v are now odd.  Subtract the smaller one
		 from the larger one to produce an even number, remove
		 more factors of two, and repeat. */
	      while (u != v)
		{
		  if (u > v)
		    {
		      u -= v;
		      while ((u & 1) == 0)
			u >>= 1;
		    }
		  else
		    {
		      v -= u;
		      while ((v & 1) == 0)
			v >>= 1;
		    }
		}
	      result = u << k;
	    }
          return (SCM_POSFIXABLE (result)
		  ? SCM_I_MAKINUM (result)
		  : scm_i_inum2big (result));
        }
      else if (SCM_BIGP (y))
        {
          SCM_SWAP (x, y);
          goto big_inum;
        }
      else if (SCM_REALP (y) && scm_is_integer (y))
        goto handle_inexacts;
      else
        SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG2, s_gcd);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
        {
          scm_t_bits result;
          scm_t_inum yy;
        big_inum:
          yy = SCM_I_INUM (y);
          if (yy == 0)
            return scm_abs (x);
          if (yy < 0)
	    yy = -yy;
          result = mpz_gcd_ui (NULL, SCM_I_BIG_MPZ (x), yy);
          scm_remember_upto_here_1 (x);
          return (SCM_POSFIXABLE (result) 
		  ? SCM_I_MAKINUM (result)
		  : scm_from_unsigned_integer (result));
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
      else if (SCM_REALP (y) && scm_is_integer (y))
        goto handle_inexacts;
      else
        SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG2, s_gcd);
    }
  else if (SCM_REALP (x) && scm_is_integer (x))
    {
      if (SCM_I_INUMP (y) || SCM_BIGP (y)
          || (SCM_REALP (y) && scm_is_integer (y)))
        {
        handle_inexacts:
          return scm_exact_to_inexact (scm_gcd (scm_inexact_to_exact (x),
                                                scm_inexact_to_exact (y)));
        }
      else
        SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG2, s_gcd);
    }
  else
    SCM_WTA_DISPATCH_2 (g_gcd, x, y, SCM_ARG1, s_gcd);
}

SCM_PRIMITIVE_GENERIC (scm_i_lcm, "lcm", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return the least common multiple of the arguments.\n"
                       "If called without arguments, 1 is returned.")
#define FUNC_NAME s_scm_i_lcm
{
  while (!scm_is_null (rest))
    { x = scm_lcm (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_lcm (x, y);
}
#undef FUNC_NAME
                       
#define s_lcm s_scm_i_lcm
#define g_lcm g_scm_i_lcm

SCM
scm_lcm (SCM n1, SCM n2)
{
  if (SCM_UNLIKELY (SCM_UNBNDP (n2)))
    return SCM_UNBNDP (n1) ? SCM_INUM1 : scm_abs (n1);

  if (SCM_LIKELY (SCM_I_INUMP (n1)))
    {
      if (SCM_LIKELY (SCM_I_INUMP (n2)))
        {
          SCM d = scm_gcd (n1, n2);
          if (scm_is_eq (d, SCM_INUM0))
            return d;
          else
            return scm_abs (scm_product (n1, scm_quotient (n2, d)));
        }
      else if (SCM_LIKELY (SCM_BIGP (n2)))
        {
          /* inum n1, big n2 */
        inumbig:
          {
            SCM result = scm_i_mkbig ();
            scm_t_inum nn1 = SCM_I_INUM (n1);
            if (nn1 == 0) return SCM_INUM0;
            if (nn1 < 0) nn1 = - nn1;
            mpz_lcm_ui (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (n2), nn1);
            scm_remember_upto_here_1 (n2);
            return result;
          }
        }
      else if (SCM_REALP (n2) && scm_is_integer (n2))
        goto handle_inexacts;
      else
        SCM_WTA_DISPATCH_2 (g_lcm, n1, n2, SCM_ARG2, s_lcm);
    }
  else if (SCM_LIKELY (SCM_BIGP (n1)))
    {
      /* big n1 */
      if (SCM_I_INUMP (n2))
        {
          SCM_SWAP (n1, n2);
          goto inumbig;
        }
      else if (SCM_LIKELY (SCM_BIGP (n2)))
        {
          SCM result = scm_i_mkbig ();
          mpz_lcm(SCM_I_BIG_MPZ (result),
                  SCM_I_BIG_MPZ (n1),
                  SCM_I_BIG_MPZ (n2));
          scm_remember_upto_here_2(n1, n2);
          /* shouldn't need to normalize b/c lcm of 2 bigs should be big */
          return result;
        }
      else if (SCM_REALP (n2) && scm_is_integer (n2))
        goto handle_inexacts;
      else
        SCM_WTA_DISPATCH_2 (g_lcm, n1, n2, SCM_ARG2, s_lcm);
    }
  else if (SCM_REALP (n1) && scm_is_integer (n1))
    {
      if (SCM_I_INUMP (n2) || SCM_BIGP (n2)
          || (SCM_REALP (n2) && scm_is_integer (n2)))
        {
        handle_inexacts:
          return scm_exact_to_inexact (scm_lcm (scm_inexact_to_exact (n1),
                                                scm_inexact_to_exact (n2)));
        }
      else
        SCM_WTA_DISPATCH_2 (g_lcm, n1, n2, SCM_ARG2, s_lcm);
    }
  else
    SCM_WTA_DISPATCH_2 (g_lcm, n1, n2, SCM_ARG1, s_lcm);
}

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

SCM_DEFINE (scm_i_logand, "logand", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return the bitwise AND of the integer arguments.\n\n"
            "@lisp\n"
            "(logand) @result{} -1\n"
            "(logand 7) @result{} 7\n"
            "(logand #b111 #b011 #b001) @result{} 1\n"
            "@end lisp")
#define FUNC_NAME s_scm_i_logand
{
  while (!scm_is_null (rest))
    { x = scm_logand (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_logand (x, y);
}
#undef FUNC_NAME
                       
#define s_scm_logand s_scm_i_logand

SCM scm_logand (SCM n1, SCM n2)
#define FUNC_NAME s_scm_logand
{
  scm_t_inum nn1;

  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
	return SCM_I_MAKINUM (-1);
      else if (!SCM_NUMBERP (n1))
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
      else if (SCM_NUMBERP (n1))
	return n1;
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
    }

  if (SCM_I_INUMP (n1))
    {
      nn1 = SCM_I_INUM (n1);
      if (SCM_I_INUMP (n2))
	{
	  scm_t_inum nn2 = SCM_I_INUM (n2);
	  return SCM_I_MAKINUM (nn1 & nn2);
	}
      else if SCM_BIGP (n2)
	{
	intbig: 
	  if (nn1 == 0)
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
      if (SCM_I_INUMP (n2))
	{
	  SCM_SWAP (n1, n2);
	  nn1 = SCM_I_INUM (n1);
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


SCM_DEFINE (scm_i_logior, "logior", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
            "Return the bitwise OR of the integer arguments.\n\n"
            "@lisp\n"
            "(logior) @result{} 0\n"
            "(logior 7) @result{} 7\n"
            "(logior #b000 #b001 #b011) @result{} 3\n"
            "@end lisp")
#define FUNC_NAME s_scm_i_logior
{
  while (!scm_is_null (rest))
    { x = scm_logior (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_logior (x, y);
}
#undef FUNC_NAME
                       
#define s_scm_logior s_scm_i_logior

SCM scm_logior (SCM n1, SCM n2)
#define FUNC_NAME s_scm_logior
{
  scm_t_inum nn1;

  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
	return SCM_INUM0;
      else if (SCM_NUMBERP (n1))
	return n1;
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
    }

  if (SCM_I_INUMP (n1))
    {
      nn1 = SCM_I_INUM (n1);
      if (SCM_I_INUMP (n2))
	{
	  long nn2 = SCM_I_INUM (n2);
	  return SCM_I_MAKINUM (nn1 | nn2);
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
	    return scm_i_normbig (result_z);
	  }
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, n2);
    }
  else if (SCM_BIGP (n1))
    {
      if (SCM_I_INUMP (n2))
	{
	  SCM_SWAP (n1, n2); 
	  nn1 = SCM_I_INUM (n1);
	  goto intbig;
	}
      else if (SCM_BIGP (n2))
	{
	  SCM result_z = scm_i_mkbig ();
	  mpz_ior (SCM_I_BIG_MPZ (result_z),
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


SCM_DEFINE (scm_i_logxor, "logxor", 0, 2, 1,
            (SCM x, SCM y, SCM rest),
	     "Return the bitwise XOR of the integer arguments.  A bit is\n"
	     "set in the result if it is set in an odd number of arguments.\n"
	     "@lisp\n"
	     "(logxor) @result{} 0\n"
	     "(logxor 7) @result{} 7\n"
	     "(logxor #b000 #b001 #b011) @result{} 2\n"
	     "(logxor #b000 #b001 #b011 #b011) @result{} 1\n"
	    "@end lisp")
#define FUNC_NAME s_scm_i_logxor
{
  while (!scm_is_null (rest))
    { x = scm_logxor (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_logxor (x, y);
}
#undef FUNC_NAME
                       
#define s_scm_logxor s_scm_i_logxor

SCM scm_logxor (SCM n1, SCM n2)
#define FUNC_NAME s_scm_logxor
{
  scm_t_inum nn1;

  if (SCM_UNBNDP (n2))
    {
      if (SCM_UNBNDP (n1))
	return SCM_INUM0;
      else if (SCM_NUMBERP (n1))
	return n1;
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG1, n1);
    }

  if (SCM_I_INUMP (n1))
    {
      nn1 = SCM_I_INUM (n1);
      if (SCM_I_INUMP (n2))
	{
	  scm_t_inum nn2 = SCM_I_INUM (n2);
	  return SCM_I_MAKINUM (nn1 ^ nn2);
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
      if (SCM_I_INUMP (n2))
	{
	  SCM_SWAP (n1, n2);
	  nn1 = SCM_I_INUM (n1);
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
	    "Test whether @var{j} and @var{k} have any 1 bits in common.\n"
	    "This is equivalent to @code{(not (zero? (logand j k)))}, but\n"
	    "without actually calculating the @code{logand}, just testing\n"
	    "for non-zero.\n"
	    "\n"
	    "@lisp\n"
	    "(logtest #b0100 #b1011) @result{} #f\n"
	    "(logtest #b0100 #b0111) @result{} #t\n"
	    "@end lisp")
#define FUNC_NAME s_scm_logtest
{
  scm_t_inum nj;

  if (SCM_I_INUMP (j))
    {
      nj = SCM_I_INUM (j);
      if (SCM_I_INUMP (k))
	{
	  scm_t_inum nk = SCM_I_INUM (k);
	  return scm_from_bool (nj & nk);
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
	    result = scm_from_bool (mpz_sgn (nj_z) != 0);
	    mpz_clear (nj_z);
	    return result;
	  }
	}
      else
	SCM_WRONG_TYPE_ARG (SCM_ARG2, k);
    }
  else if (SCM_BIGP (j))
    {
      if (SCM_I_INUMP (k))
	{
	  SCM_SWAP (j, k);
	  nj = SCM_I_INUM (j);
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
	  result = scm_from_bool (mpz_sgn (result_z) != 0);
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
	    "Test whether bit number @var{index} in @var{j} is set.\n"
	    "@var{index} starts from 0 for the least significant bit.\n"
	    "\n"
	    "@lisp\n"
	    "(logbit? 0 #b1101) @result{} #t\n"
	    "(logbit? 1 #b1101) @result{} #f\n"
	    "(logbit? 2 #b1101) @result{} #t\n"
	    "(logbit? 3 #b1101) @result{} #t\n"
	    "(logbit? 4 #b1101) @result{} #f\n"
	    "@end lisp")
#define FUNC_NAME s_scm_logbit_p
{
  unsigned long int iindex;
  iindex = scm_to_ulong (index);

  if (SCM_I_INUMP (j))
    {
      /* bits above what's in an inum follow the sign bit */
      iindex = min (iindex, SCM_LONG_BIT - 1);
      return scm_from_bool ((1L << iindex) & SCM_I_INUM (j));
    }
  else if (SCM_BIGP (j))
    {
      int val = mpz_tstbit (SCM_I_BIG_MPZ (j), iindex);
      scm_remember_upto_here_1 (j);
      return scm_from_bool (val);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG2, j);
}
#undef FUNC_NAME


SCM_DEFINE (scm_lognot, "lognot", 1, 0, 0, 
            (SCM n),
	    "Return the integer which is the ones-complement of the integer\n"
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
  if (SCM_I_INUMP (n)) {
    /* No overflow here, just need to toggle all the bits making up the inum.
       Enhancement: No need to strip the tag and add it back, could just xor
       a block of 1 bits, if that worked with the various debug versions of
       the SCM typedef.  */
    return SCM_I_MAKINUM (~ SCM_I_INUM (n));

  } else if (SCM_BIGP (n)) {
    SCM result = scm_i_mkbig ();
    mpz_com (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (n));
    scm_remember_upto_here_1 (n);
    return result;

  } else {
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
  }
}
#undef FUNC_NAME

/* returns 0 if IN is not an integer.  OUT must already be
   initialized. */
static int
coerce_to_big (SCM in, mpz_t out)
{
  if (SCM_BIGP (in))
    mpz_set (out, SCM_I_BIG_MPZ (in));
  else if (SCM_I_INUMP (in))
    mpz_set_si (out, SCM_I_INUM (in));
  else
    return 0;

  return 1;
}

SCM_DEFINE (scm_modulo_expt, "modulo-expt", 3, 0, 0,
            (SCM n, SCM k, SCM m),
            "Return @var{n} raised to the integer exponent\n"
	    "@var{k}, modulo @var{m}.\n"
	    "\n"
	    "@lisp\n"
	    "(modulo-expt 2 3 5)\n"
	    "   @result{} 3\n"
	    "@end lisp")
#define FUNC_NAME s_scm_modulo_expt
{
  mpz_t n_tmp; 
  mpz_t k_tmp; 
  mpz_t m_tmp; 
    
  /* There are two classes of error we might encounter --
     1) Math errors, which we'll report by calling scm_num_overflow,
     and
     2) wrong-type errors, which of course we'll report by calling
     SCM_WRONG_TYPE_ARG.
     We don't report those errors immediately, however; instead we do
     some cleanup first.  These variables tell us which error (if
     any) we should report after cleaning up.  
  */
  int report_overflow = 0;

  int position_of_wrong_type = 0;
  SCM value_of_wrong_type = SCM_INUM0;

  SCM result = SCM_UNDEFINED;

  mpz_init (n_tmp);
  mpz_init (k_tmp);
  mpz_init (m_tmp);
    
  if (scm_is_eq (m, SCM_INUM0))
    {
      report_overflow = 1;
      goto cleanup;
    }
  
  if (!coerce_to_big (n, n_tmp))
    {
      value_of_wrong_type = n;
      position_of_wrong_type = 1;
      goto cleanup;
    }

  if (!coerce_to_big (k, k_tmp))
    {
      value_of_wrong_type = k;
      position_of_wrong_type = 2;
      goto cleanup;
    }

  if (!coerce_to_big (m, m_tmp))
    {
      value_of_wrong_type = m;
      position_of_wrong_type = 3;
      goto cleanup;
    }

  /* if the exponent K is negative, and we simply call mpz_powm, we
     will get a divide-by-zero exception when an inverse 1/n mod m
     doesn't exist (or is not unique).  Since exceptions are hard to
     handle, we'll attempt the inversion "by hand" -- that way, we get
     a simple failure code, which is easy to handle. */
  
  if (-1 == mpz_sgn (k_tmp))
    {
      if (!mpz_invert (n_tmp, n_tmp, m_tmp))
        {
          report_overflow = 1;
          goto cleanup;
        }
      mpz_neg (k_tmp, k_tmp);
    }

  result = scm_i_mkbig ();
  mpz_powm (SCM_I_BIG_MPZ (result),
            n_tmp,
            k_tmp,
            m_tmp);

  if (mpz_sgn (m_tmp) < 0 && mpz_sgn (SCM_I_BIG_MPZ (result)) != 0)
    mpz_add (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result), m_tmp);

 cleanup:
  mpz_clear (m_tmp);
  mpz_clear (k_tmp);
  mpz_clear (n_tmp);

  if (report_overflow)
    scm_num_overflow (FUNC_NAME);

  if (position_of_wrong_type)
    SCM_WRONG_TYPE_ARG (position_of_wrong_type,
                        value_of_wrong_type);
  
  return scm_i_normbig (result);
}
#undef FUNC_NAME

SCM_DEFINE (scm_integer_expt, "integer-expt", 2, 0, 0,
            (SCM n, SCM k),
	    "Return @var{n} raised to the power @var{k}.  @var{k} must be an\n"
	    "exact integer, @var{n} can be any number.\n"
	    "\n"
	    "Negative @var{k} is supported, and results in\n"
	    "@math{1/@var{n}^abs(@var{k})} in the usual way.\n"
	    "@math{@var{n}^0} is 1, as usual, and that\n"
	    "includes @math{0^0} is 1.\n"
	    "\n"
	    "@lisp\n"
	    "(integer-expt 2 5)   @result{} 32\n"
	    "(integer-expt -3 3)  @result{} -27\n"
	    "(integer-expt 5 -3)  @result{} 1/125\n"
	    "(integer-expt 0 0)   @result{} 1\n"
	    "@end lisp")
#define FUNC_NAME s_scm_integer_expt
{
  scm_t_inum i2 = 0;
  SCM z_i2 = SCM_BOOL_F;
  int i2_is_big = 0;
  SCM acc = SCM_I_MAKINUM (1L);

  /* Specifically refrain from checking the type of the first argument.
     This allows us to exponentiate any object that can be multiplied.
     If we must raise to a negative power, we must also be able to
     take its reciprocal. */
  if (!SCM_LIKELY (SCM_I_INUMP (k)) && !SCM_LIKELY (SCM_BIGP (k)))
    SCM_WRONG_TYPE_ARG (2, k);

  if (SCM_UNLIKELY (scm_is_eq (k, SCM_INUM0)))
    return SCM_INUM1;  /* n^(exact0) is exact 1, regardless of n */
  else if (SCM_UNLIKELY (scm_is_eq (n, SCM_I_MAKINUM (-1L))))
    return scm_is_false (scm_even_p (k)) ? n : SCM_INUM1;
  /* The next check is necessary only because R6RS specifies different
     behavior for 0^(-k) than for (/ 0).  If n is not a scheme number,
     we simply skip this case and move on. */
  else if (SCM_NUMBERP (n) && scm_is_true (scm_zero_p (n)))
    {
      /* k cannot be 0 at this point, because we
	 have already checked for that case above */
      if (scm_is_true (scm_positive_p (k)))
	return n;
      else  /* return NaN for (0 ^ k) for negative k per R6RS */
	return scm_nan ();
    }
  else if (SCM_FRACTIONP (n))
    {
      /* Optimize the fraction case by (a/b)^k ==> (a^k)/(b^k), to avoid
         needless reduction of intermediate products to lowest terms.
         If a and b have no common factors, then a^k and b^k have no
         common factors.  Use 'scm_i_make_ratio_already_reduced' to
         construct the final result, so that no gcd computations are
         needed to exponentiate a fraction.  */
      if (scm_is_true (scm_positive_p (k)))
	return scm_i_make_ratio_already_reduced
	  (scm_integer_expt (SCM_FRACTION_NUMERATOR (n), k),
	   scm_integer_expt (SCM_FRACTION_DENOMINATOR (n), k));
      else
	{
	  k = scm_difference (k, SCM_UNDEFINED);
	  return scm_i_make_ratio_already_reduced
	    (scm_integer_expt (SCM_FRACTION_DENOMINATOR (n), k),
	     scm_integer_expt (SCM_FRACTION_NUMERATOR (n), k));
	}
    }

  if (SCM_I_INUMP (k))
    i2 = SCM_I_INUM (k);
  else if (SCM_BIGP (k))
    {
      z_i2 = scm_i_clonebig (k, 1);
      scm_remember_upto_here_1 (k);
      i2_is_big = 1;
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
              return acc;
            }
          if (mpz_cmp_ui(SCM_I_BIG_MPZ (z_i2), 1) == 0)
            {
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

/* Efficiently compute (N * 2^COUNT),
   where N is an exact integer, and COUNT > 0. */
static SCM
left_shift_exact_integer (SCM n, long count)
{
  if (SCM_I_INUMP (n))
    {
      scm_t_inum nn = SCM_I_INUM (n);

      /* Left shift of count >= SCM_I_FIXNUM_BIT-1 will almost[*] always
         overflow a non-zero fixnum.  For smaller shifts we check the
         bits going into positions above SCM_I_FIXNUM_BIT-1.  If they're
         all 0s for nn>=0, or all 1s for nn<0 then there's no overflow.
         Those bits are "nn >> (SCM_I_FIXNUM_BIT-1 - count)".

         [*] There's one exception:
             (-1) << SCM_I_FIXNUM_BIT-1 == SCM_MOST_NEGATIVE_FIXNUM  */

      if (nn == 0)
        return n;
      else if (count < SCM_I_FIXNUM_BIT-1 &&
               ((scm_t_bits) (SCM_SRS (nn, (SCM_I_FIXNUM_BIT-1 - count)) + 1)
                <= 1))
        return SCM_I_MAKINUM (nn << count);
      else
        {
          SCM result = scm_i_inum2big (nn);
          mpz_mul_2exp (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result),
                        count);
          return scm_i_normbig (result);
        }
    }
  else if (SCM_BIGP (n))
    {
      SCM result = scm_i_mkbig ();
      mpz_mul_2exp (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (n), count);
      scm_remember_upto_here_1 (n);
      return result;
    }
  else
    assert (0);
}

/* Efficiently compute floor (N / 2^COUNT),
   where N is an exact integer and COUNT > 0. */
static SCM
floor_right_shift_exact_integer (SCM n, long count)
{
  if (SCM_I_INUMP (n))
    {
      scm_t_inum nn = SCM_I_INUM (n);

      if (count >= SCM_I_FIXNUM_BIT)
        return (nn >= 0 ? SCM_INUM0 : SCM_I_MAKINUM (-1));
      else
        return SCM_I_MAKINUM (SCM_SRS (nn, count));
    }
  else if (SCM_BIGP (n))
    {
      SCM result = scm_i_mkbig ();
      mpz_fdiv_q_2exp (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (n),
                       count);
      scm_remember_upto_here_1 (n);
      return scm_i_normbig (result);
    }
  else
    assert (0);
}

/* Efficiently compute round (N / 2^COUNT),
   where N is an exact integer and COUNT > 0. */
static SCM
round_right_shift_exact_integer (SCM n, long count)
{
  if (SCM_I_INUMP (n))
    {
      if (count >= SCM_I_FIXNUM_BIT)
        return SCM_INUM0;
      else
        {
          scm_t_inum nn = SCM_I_INUM (n);
          scm_t_inum qq = SCM_SRS (nn, count);

          if (0 == (nn & (1L << (count-1))))
            return SCM_I_MAKINUM (qq);                /* round down */
          else if (nn & ((1L << (count-1)) - 1))
            return SCM_I_MAKINUM (qq + 1);            /* round up */
          else
            return SCM_I_MAKINUM ((~1L) & (qq + 1));  /* round to even */
        }
    }
  else if (SCM_BIGP (n))
    {
      SCM q = scm_i_mkbig ();

      mpz_fdiv_q_2exp (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (n), count);
      if (mpz_tstbit (SCM_I_BIG_MPZ (n), count-1)
          && (mpz_odd_p (SCM_I_BIG_MPZ (q))
              || (mpz_scan1 (SCM_I_BIG_MPZ (n), 0) < count-1)))
        mpz_add_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q), 1);
      scm_remember_upto_here_1 (n);
      return scm_i_normbig (q);
    }
  else
    assert (0);
}

SCM_DEFINE (scm_ash, "ash", 2, 0, 0,
            (SCM n, SCM count),
	    "Return @math{floor(@var{n} * 2^@var{count})}.\n"
	    "@var{n} and @var{count} must be exact integers.\n"
	    "\n"
	    "With @var{n} viewed as an infinite-precision twos-complement\n"
	    "integer, @code{ash} means a left shift introducing zero bits\n"
	    "when @var{count} is positive, or a right shift dropping bits\n"
	    "when @var{count} is negative.  This is an ``arithmetic'' shift.\n"
	    "\n"
	    "@lisp\n"
	    "(number->string (ash #b1 3) 2)     @result{} \"1000\"\n"
	    "(number->string (ash #b1010 -1) 2) @result{} \"101\"\n"
	    "\n"
	    ";; -23 is bits ...11101001, -6 is bits ...111010\n"
	    "(ash -23 -2) @result{} -6\n"
	    "@end lisp")
#define FUNC_NAME s_scm_ash
{
  if (SCM_I_INUMP (n) || SCM_BIGP (n))
    {
      long bits_to_shift = scm_to_long (count);

      if (bits_to_shift > 0)
        return left_shift_exact_integer (n, bits_to_shift);
      else if (SCM_LIKELY (bits_to_shift < 0))
        return floor_right_shift_exact_integer (n, -bits_to_shift);
      else
        return n;
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_round_ash, "round-ash", 2, 0, 0,
            (SCM n, SCM count),
	    "Return @math{round(@var{n} * 2^@var{count})}.\n"
	    "@var{n} and @var{count} must be exact integers.\n"
	    "\n"
	    "With @var{n} viewed as an infinite-precision twos-complement\n"
	    "integer, @code{round-ash} means a left shift introducing zero\n"
	    "bits when @var{count} is positive, or a right shift rounding\n"
	    "to the nearest integer (with ties going to the nearest even\n"
	    "integer) when @var{count} is negative.  This is a rounded\n"
	    "``arithmetic'' shift.\n"
	    "\n"
	    "@lisp\n"
	    "(number->string (round-ash #b1 3) 2)     @result{} \"1000\"\n"
	    "(number->string (round-ash #b1010 -1) 2) @result{} \"101\"\n"
	    "(number->string (round-ash #b1010 -2) 2) @result{} \"10\"\n"
	    "(number->string (round-ash #b1011 -2) 2) @result{} \"11\"\n"
	    "(number->string (round-ash #b1101 -2) 2) @result{} \"11\"\n"
	    "(number->string (round-ash #b1110 -2) 2) @result{} \"100\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_round_ash
{
  if (SCM_I_INUMP (n) || SCM_BIGP (n))
    {
      long bits_to_shift = scm_to_long (count);

      if (bits_to_shift > 0)
        return left_shift_exact_integer (n, bits_to_shift);
      else if (SCM_LIKELY (bits_to_shift < 0))
        return round_right_shift_exact_integer (n, -bits_to_shift);
      else
        return n;
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
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
  unsigned long int istart, iend, bits;
  istart = scm_to_ulong (start);
  iend = scm_to_ulong (end);
  SCM_ASSERT_RANGE (3, end, (iend >= istart));

  /* how many bits to keep */
  bits = iend - istart;

  if (SCM_I_INUMP (n))
    {
      scm_t_inum in = SCM_I_INUM (n);

      /* When istart>=SCM_I_FIXNUM_BIT we can just limit the shift to
         SCM_I_FIXNUM_BIT-1 to get either 0 or -1 per the sign of "in". */
      in = SCM_SRS (in, min (istart, SCM_I_FIXNUM_BIT-1));

      if (in < 0 && bits >= SCM_I_FIXNUM_BIT)
	{
	  /* Since we emulate two's complement encoded numbers, this
	   * special case requires us to produce a result that has
	   * more bits than can be stored in a fixnum.
	   */
          SCM result = scm_i_inum2big (in);
          mpz_fdiv_r_2exp (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result),
                           bits);
          return result;
	}

      /* mask down to requisite bits */
      bits = min (bits, SCM_I_FIXNUM_BIT);
      return SCM_I_MAKINUM (in & ((1L << bits) - 1));
    }
  else if (SCM_BIGP (n))
    {
      SCM result;
      if (bits == 1)
        {
          result = SCM_I_MAKINUM (mpz_tstbit (SCM_I_BIG_MPZ (n), istart));
        }
      else
        {
          /* ENHANCE-ME: It'd be nice not to allocate a new bignum when
             bits<SCM_I_FIXNUM_BIT.  Would want some help from GMP to get
             such bits into a ulong.  */
          result = scm_i_mkbig ();
          mpz_fdiv_q_2exp (SCM_I_BIG_MPZ(result), SCM_I_BIG_MPZ(n), istart);
          mpz_fdiv_r_2exp (SCM_I_BIG_MPZ(result), SCM_I_BIG_MPZ(result), bits);
          result = scm_i_normbig (result);
        }
      scm_remember_upto_here_1 (n);
      return result;
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
  if (SCM_I_INUMP (n))
    {
      unsigned long c = 0;
      scm_t_inum nn = SCM_I_INUM (n);
      if (nn < 0)
        nn = -1 - nn;
      while (nn)
        {
          c += scm_logtab[15 & nn];
          nn >>= 4;
        }
      return SCM_I_MAKINUM (c);
    }
  else if (SCM_BIGP (n))
    {
      unsigned long count;
      if (mpz_sgn (SCM_I_BIG_MPZ (n)) >= 0)
        count = mpz_popcount (SCM_I_BIG_MPZ (n));
      else
        count = mpz_hamdist (SCM_I_BIG_MPZ (n), z_negative_one);
      scm_remember_upto_here_1 (n);
      return SCM_I_MAKINUM (count);
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
  if (SCM_I_INUMP (n))
    {
      unsigned long c = 0;
      unsigned int l = 4;
      scm_t_inum nn = SCM_I_INUM (n);
      if (nn < 0)
	nn = -1 - nn;
      while (nn)
	{
	  c += 4;
	  l = scm_ilentab [15 & nn];
	  nn >>= 4;
	}
      return SCM_I_MAKINUM (c - 4 + l);
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
      return SCM_I_MAKINUM (size);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME

/*** NUMBERS -> STRINGS ***/
#define SCM_MAX_DBL_RADIX 36

/* use this array as a way to generate a single digit */
static const char number_chars[] = "0123456789abcdefghijklmnopqrstuvwxyz";

static mpz_t dbl_minimum_normal_mantissa;

static size_t
idbl2str (double dbl, char *a, int radix)
{
  int ch = 0;

  if (radix < 2 || radix > SCM_MAX_DBL_RADIX)
    /* revert to existing behavior */
    radix = 10;

  if (isinf (dbl))
    {
      strcpy (a, (dbl > 0.0) ? "+inf.0" : "-inf.0");
      return 6;
    }
  else if (dbl > 0.0)
    ;
  else if (dbl < 0.0)
    {
      dbl = -dbl;
      a[ch++] = '-';
    }
  else if (dbl == 0.0)
    {
      if (copysign (1.0, dbl) < 0.0)
        a[ch++] = '-';
      strcpy (a + ch, "0.0");
      return ch + 3;
    }
  else if (isnan (dbl))
    {
      strcpy (a, "+nan.0");
      return 6;
    }

  /* Algorithm taken from "Printing Floating-Point Numbers Quickly and
     Accurately" by Robert G. Burger and R. Kent Dybvig */
  {
    int e, k;
    mpz_t f, r, s, mplus, mminus, hi, digit;
    int f_is_even, f_is_odd;
    int expon;
    int show_exp = 0;

    mpz_inits (f, r, s, mplus, mminus, hi, digit, NULL);
    mpz_set_d (f, ldexp (frexp (dbl, &e), DBL_MANT_DIG));
    if (e < DBL_MIN_EXP)
      {
        mpz_tdiv_q_2exp (f, f, DBL_MIN_EXP - e);
        e = DBL_MIN_EXP;
      }
    e -= DBL_MANT_DIG;

    f_is_even = !mpz_odd_p (f);
    f_is_odd = !f_is_even;

    /* Initialize r, s, mplus, and mminus according
       to Table 1 from the paper. */
    if (e < 0)
      {
        mpz_set_ui (mminus, 1);
        if (mpz_cmp (f, dbl_minimum_normal_mantissa) != 0
            || e == DBL_MIN_EXP - DBL_MANT_DIG)
          {
            mpz_set_ui (mplus, 1);
            mpz_mul_2exp (r, f, 1);
            mpz_mul_2exp (s, mminus, 1 - e);
          }
        else
          {
            mpz_set_ui (mplus, 2);
            mpz_mul_2exp (r, f, 2);
            mpz_mul_2exp (s, mminus, 2 - e);
          }
      }
    else
      {
        mpz_set_ui (mminus, 1);
        mpz_mul_2exp (mminus, mminus, e);
        if (mpz_cmp (f, dbl_minimum_normal_mantissa) != 0)
          {
            mpz_set (mplus, mminus);
            mpz_mul_2exp (r, f, 1 + e);
            mpz_set_ui (s, 2);
          }
        else
          {
            mpz_mul_2exp (mplus, mminus, 1);
            mpz_mul_2exp (r, f, 2 + e);
            mpz_set_ui (s, 4);
          }
      }

    /* Find the smallest k such that:
         (r + mplus) / s <  radix^k  (if f is even)
         (r + mplus) / s <= radix^k  (if f is odd) */
    {
      /* IMPROVE-ME: Make an initial guess to speed this up */
      mpz_add (hi, r, mplus);
      k = 0;
      while (mpz_cmp (hi, s) >= f_is_odd)
        {
          mpz_mul_ui (s, s, radix);
          k++;
        }
      if (k == 0)
        {
          mpz_mul_ui (hi, hi, radix);
          while (mpz_cmp (hi, s) < f_is_odd)
            {
              mpz_mul_ui (r, r, radix);
              mpz_mul_ui (mplus, mplus, radix);
              mpz_mul_ui (mminus, mminus, radix);
              mpz_mul_ui (hi, hi, radix);
              k--;
            }
        }
    }

    expon = k - 1;
    if (k <= 0)
      {
        if (k <= -3)
          {
            /* Use scientific notation */
            show_exp = 1;
            k = 1;
          }
        else
          {
            int i;

            /* Print leading zeroes */
            a[ch++] = '0';
            a[ch++] = '.';
            for (i = 0; i > k; i--)
              a[ch++] = '0';
          }
      }

    for (;;)
      {
        int end_1_p, end_2_p;
        int d;

        mpz_mul_ui (mplus, mplus, radix);
        mpz_mul_ui (mminus, mminus, radix);
        mpz_mul_ui (r, r, radix);
        mpz_fdiv_qr (digit, r, r, s);
        d = mpz_get_ui (digit);

        mpz_add (hi, r, mplus);
        end_1_p = (mpz_cmp (r, mminus) < f_is_even);
        end_2_p = (mpz_cmp (s, hi) < f_is_even);
        if (end_1_p || end_2_p)
          {
            mpz_mul_2exp (r, r, 1);
            if (!end_2_p)
              ;
            else if (!end_1_p)
              d++;
            else if (mpz_cmp (r, s) >= !(d & 1))
              d++;
            a[ch++] = number_chars[d];
            if (--k == 0)
              a[ch++] = '.';
            break;
          }
        else
          {
            a[ch++] = number_chars[d];
            if (--k == 0)
              a[ch++] = '.';
          }
      }

    if (k > 0)
      {
        if (expon >= 7 && k >= 4 && expon >= k)
          {
            /* Here we would have to print more than three zeroes
               followed by a decimal point and another zero.  It
               makes more sense to use scientific notation. */

            /* Adjust k to what it would have been if we had chosen
               scientific notation from the beginning. */
            k -= expon;

            /* k will now be <= 0, with magnitude equal to the number of
               digits that we printed which should now be put after the
               decimal point. */

            /* Insert a decimal point */
            memmove (a + ch + k + 1, a + ch + k, -k);
            a[ch + k] = '.';
            ch++;

            show_exp = 1;
          }
        else
          {
            for (; k > 0; k--)
              a[ch++] = '0';
            a[ch++] = '.';
          }
      }

    if (k == 0)
      a[ch++] = '0';

    if (show_exp)
      {
        a[ch++] = 'e';
        ch += scm_iint2str (expon, radix, a + ch);
      }

    mpz_clears (f, r, s, mplus, mminus, hi, digit, NULL);
  }
  return ch;
}


static size_t
icmplx2str (double real, double imag, char *str, int radix)
{
  size_t i;
  double sgn;
  
  i = idbl2str (real, str, radix);
#ifdef HAVE_COPYSIGN
  sgn = copysign (1.0, imag);
#else
  sgn = imag;
#endif
  /* Don't output a '+' for negative numbers or for Inf and
     NaN.  They will provide their own sign. */
  if (sgn >= 0 && isfinite (imag))
    str[i++] = '+';
  i += idbl2str (imag, &str[i], radix);
  str[i++] = 'i';
  return i;
}

static size_t
iflo2str (SCM flt, char *str, int radix)
{
  size_t i;
  if (SCM_REALP (flt))
    i = idbl2str (SCM_REAL_VALUE (flt), str, radix);
  else
    i = icmplx2str (SCM_COMPLEX_REAL (flt), SCM_COMPLEX_IMAG (flt),
		    str, radix);
  return i;
}

/* convert a scm_t_intmax to a string (unterminated).  returns the number of
   characters in the result. 
   rad is output base
   p is destination: worst case (base 2) is SCM_INTBUFLEN  */
size_t
scm_iint2str (scm_t_intmax num, int rad, char *p)
{
  if (num < 0)
    {
      *p++ = '-';
      return scm_iuint2str (-num, rad, p) + 1;
    }
  else
    return scm_iuint2str (num, rad, p);
}

/* convert a scm_t_intmax to a string (unterminated).  returns the number of
   characters in the result. 
   rad is output base
   p is destination: worst case (base 2) is SCM_INTBUFLEN  */
size_t
scm_iuint2str (scm_t_uintmax num, int rad, char *p)
{
  size_t j = 1;
  size_t i;
  scm_t_uintmax n = num;

  if (rad < 2 || rad > 36)
    scm_out_of_range ("scm_iuint2str", scm_from_int (rad));

  for (n /= rad; n > 0; n /= rad)
    j++;

  i = j;
  n = num;
  while (i--)
    {
      int d = n % rad;

      n /= rad;
      p[i] = number_chars[d];
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
    base = scm_to_signed_integer (radix, 2, 36);

  if (SCM_I_INUMP (n))
    {
      char num_buf [SCM_INTBUFLEN];
      size_t length = scm_iint2str (SCM_I_INUM (n), base, num_buf);
      return scm_from_locale_stringn (num_buf, length);
    }
  else if (SCM_BIGP (n))
    {
      char *str = mpz_get_str (NULL, base, SCM_I_BIG_MPZ (n));
      size_t len = strlen (str);
      void (*freefunc) (void *, size_t);
      SCM ret;
      mp_get_memory_functions (NULL, NULL, &freefunc);
      scm_remember_upto_here_1 (n);
      ret = scm_from_latin1_stringn (str, len);
      freefunc (str, len + 1);
      return ret;
    }
  else if (SCM_FRACTIONP (n))
    {
      return scm_string_append (scm_list_3 (scm_number_to_string (SCM_FRACTION_NUMERATOR (n), radix),
					    scm_from_locale_string ("/"), 
					    scm_number_to_string (SCM_FRACTION_DENOMINATOR (n), radix)));
    }
  else if (SCM_INEXACTP (n))
    {
      char num_buf [FLOBUFLEN];
      return scm_from_locale_stringn (num_buf, iflo2str (n, num_buf, base));
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
  scm_lfwrite (num_buf, iflo2str (sexp, num_buf, 10), port);
  return !0;
}

void
scm_i_print_double (double val, SCM port)
{
  char num_buf[FLOBUFLEN];
  scm_lfwrite (num_buf, idbl2str (val, num_buf, 10), port);
}

int
scm_print_complex (SCM sexp, SCM port, scm_print_state *pstate SCM_UNUSED)

{
  char num_buf[FLOBUFLEN];
  scm_lfwrite (num_buf, iflo2str (sexp, num_buf, 10), port);
  return !0;
}

void
scm_i_print_complex (double real, double imag, SCM port)
{
  char num_buf[FLOBUFLEN];
  scm_lfwrite (num_buf, icmplx2str (real, imag, num_buf, 10), port);
}

int
scm_i_print_fraction (SCM sexp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  SCM str;
  str = scm_number_to_string (sexp, SCM_UNDEFINED);
  scm_display (str, port);
  scm_remember_upto_here_1 (str);
  return !0;
}

int
scm_bigprint (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  char *str = mpz_get_str (NULL, 10, SCM_I_BIG_MPZ (exp));
  size_t len = strlen (str);
  void (*freefunc) (void *, size_t);
  mp_get_memory_functions (NULL, NULL, &freefunc);
  scm_remember_upto_here_1 (exp);
  scm_lfwrite (str, len, port);
  freefunc (str, len + 1);
  return !0;
}
/*** END nums->strs ***/


/*** STRINGS -> NUMBERS ***/

/* The following functions implement the conversion from strings to numbers.
 * The implementation somehow follows the grammar for numbers as it is given
 * in R5RS.  Thus, the functions resemble syntactic units (<ureal R>,
 * <uinteger R>, ...) that are used to build up numbers in the grammar.  Some
 * points should be noted about the implementation:
 *
 * * Each function keeps a local index variable 'idx' that points at the
 * current position within the parsed string.  The global index is only
 * updated if the function could parse the corresponding syntactic unit
 * successfully.
 *
 * * Similarly, the functions keep track of indicators of inexactness ('#',
 * '.' or exponents) using local variables ('hash_seen', 'x').
 *
 * * Sequences of digits are parsed into temporary variables holding fixnums.
 * Only if these fixnums would overflow, the result variables are updated
 * using the standard functions scm_add, scm_product, scm_divide etc.  Then,
 * the temporary variables holding the fixnums are cleared, and the process
 * starts over again.  If for example fixnums were able to store five decimal
 * digits, a number 1234567890 would be parsed in two parts 12345 and 67890,
 * and the result was computed as 12345 * 100000 + 67890.  In other words,
 * only every five digits two bignum operations were performed.
 *
 * Notes on the handling of exactness specifiers:
 *
 * When parsing non-real complex numbers, we apply exactness specifiers on
 * per-component basis, as is done in PLT Scheme.  For complex numbers
 * written in rectangular form, exactness specifiers are applied to the
 * real and imaginary parts before calling scm_make_rectangular.  For
 * complex numbers written in polar form, exactness specifiers are applied
 * to the magnitude and angle before calling scm_make_polar.
 * 
 * There are two kinds of exactness specifiers: forced and implicit.  A
 * forced exactness specifier is a "#e" or "#i" prefix at the beginning of
 * the entire number, and applies to both components of a complex number.
 * "#e" causes each component to be made exact, and "#i" causes each
 * component to be made inexact.  If no forced exactness specifier is
 * present, then the exactness of each component is determined
 * independently by the presence or absence of a decimal point or hash mark
 * within that component.  If a decimal point or hash mark is present, the
 * component is made inexact, otherwise it is made exact.
 *  
 * After the exactness specifiers have been applied to each component, they
 * are passed to either scm_make_rectangular or scm_make_polar to produce
 * the final result.  Note that this will result in a real number if the
 * imaginary part, magnitude, or angle is an exact 0.
 * 
 * For example, (string->number "#i5.0+0i") does the equivalent of:
 * 
 *   (make-rectangular (exact->inexact 5) (exact->inexact 0))
 */

enum t_exactness {NO_EXACTNESS, INEXACT, EXACT};

/* R5RS, section 7.1.1, lexical structure of numbers: <uinteger R>. */

/* Caller is responsible for checking that the return value is in range
   for the given radix, which should be <= 36. */
static unsigned int
char_decimal_value (scm_t_uint32 c)
{
  /* uc_decimal_value returns -1 on error. When cast to an unsigned int,
     that's certainly above any valid decimal, so we take advantage of
     that to elide some tests. */
  unsigned int d = (unsigned int) uc_decimal_value (c);

  /* If that failed, try extended hexadecimals, then. Only accept ascii
     hexadecimals. */
  if (d >= 10U)
    {
      c = uc_tolower (c);
      if (c >= (scm_t_uint32) 'a')
        d = c - (scm_t_uint32)'a' + 10U;
    }
  return d;
}

/* Parse the substring of MEM starting at *P_IDX for an unsigned integer
   in base RADIX.  Upon success, return the unsigned integer and update
   *P_IDX and *P_EXACTNESS accordingly.  Return #f on failure.  */
static SCM
mem2uinteger (SCM mem, unsigned int *p_idx,
	      unsigned int radix, enum t_exactness *p_exactness)
{
  unsigned int idx = *p_idx;
  unsigned int hash_seen = 0;
  scm_t_bits shift = 1;
  scm_t_bits add = 0;
  unsigned int digit_value;
  SCM result;
  char c;
  size_t len = scm_i_string_length (mem);

  if (idx == len)
    return SCM_BOOL_F;

  c = scm_i_string_ref (mem, idx);
  digit_value = char_decimal_value (c);
  if (digit_value >= radix)
    return SCM_BOOL_F;

  idx++;
  result = SCM_I_MAKINUM (digit_value);
  while (idx != len)
    {
      scm_t_wchar c = scm_i_string_ref (mem, idx);
      if (c == '#')
	{
	  hash_seen = 1;
	  digit_value = 0;
	}
      else if (hash_seen)
        break;
      else
        {
          digit_value = char_decimal_value (c);
          /* This check catches non-decimals in addition to out-of-range
             decimals.  */
          if (digit_value >= radix)
	    break;
	}

      idx++;
      if (SCM_MOST_POSITIVE_FIXNUM / radix < shift)
	{
	  result = scm_product (result, SCM_I_MAKINUM (shift));
	  if (add > 0)
	    result = scm_sum (result, SCM_I_MAKINUM (add));

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
    result = scm_product (result, SCM_I_MAKINUM (shift));
  if (add > 0)
    result = scm_sum (result, SCM_I_MAKINUM (add));

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

#define DIGIT2UINT(d) (uc_numeric_value(d).numerator)

static SCM
mem2decimal_from_point (SCM result, SCM mem, 
			unsigned int *p_idx, enum t_exactness *p_exactness)
{
  unsigned int idx = *p_idx;
  enum t_exactness x = *p_exactness;
  size_t len = scm_i_string_length (mem);

  if (idx == len)
    return result;

  if (scm_i_string_ref (mem, idx) == '.')
    {
      scm_t_bits shift = 1;
      scm_t_bits add = 0;
      unsigned int digit_value;
      SCM big_shift = SCM_INUM1;

      idx++;
      while (idx != len)
	{
	  scm_t_wchar c = scm_i_string_ref (mem, idx);
	  if (uc_is_property_decimal_digit ((scm_t_uint32) c))
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
	      big_shift = scm_product (big_shift, SCM_I_MAKINUM (shift));
	      result = scm_product (result, SCM_I_MAKINUM (shift));
	      if (add > 0)
		result = scm_sum (result, SCM_I_MAKINUM (add));
	      
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
	  big_shift = scm_product (big_shift, SCM_I_MAKINUM (shift));
	  result = scm_product (result, SCM_I_MAKINUM (shift));
	  result = scm_sum (result, SCM_I_MAKINUM (add));
	}

      result = scm_divide (result, big_shift);

      /* We've seen a decimal point, thus the value is implicitly inexact. */
      x = INEXACT;
    }

  if (idx != len)
    {
      int sign = 1;
      unsigned int start;
      scm_t_wchar c;
      int exponent;
      SCM e;

      /* R5RS, section 7.1.1, lexical structure of numbers: <suffix> */

      switch (scm_i_string_ref (mem, idx))
	{
	case 'd': case 'D':
	case 'e': case 'E':
	case 'f': case 'F':
	case 'l': case 'L':
	case 's': case 'S':
	  idx++;
          if (idx == len)
            return SCM_BOOL_F;

	  start = idx;
	  c = scm_i_string_ref (mem, idx);
	  if (c == '-')
	    {
	      idx++;
              if (idx == len)
                return SCM_BOOL_F;

	      sign = -1;
	      c = scm_i_string_ref (mem, idx);
	    }
	  else if (c == '+')
	    {
	      idx++;
              if (idx == len)
                return SCM_BOOL_F;

	      sign = 1;
	      c = scm_i_string_ref (mem, idx);
	    }
	  else
	    sign = 1;

	  if (!uc_is_property_decimal_digit ((scm_t_uint32) c))
	    return SCM_BOOL_F;

	  idx++;
	  exponent = DIGIT2UINT (c);
	  while (idx != len)
	    {
	      scm_t_wchar c = scm_i_string_ref (mem, idx);
	      if (uc_is_property_decimal_digit ((scm_t_uint32) c))
		{
		  idx++;
		  if (exponent <= SCM_MAXEXP)
		    exponent = exponent * 10 + DIGIT2UINT (c);
		}
	      else
		break;
	    }

	  if (exponent > ((sign == 1) ? SCM_MAXEXP : SCM_MAXEXP + DBL_DIG + 1))
	    {
	      size_t exp_len = idx - start;
	      SCM exp_string = scm_i_substring_copy (mem, start, start + exp_len);
	      SCM exp_num = scm_string_to_number (exp_string, SCM_UNDEFINED);
	      scm_out_of_range ("string->number", exp_num);
	    }

	  e = scm_integer_expt (SCM_I_MAKINUM (10), SCM_I_MAKINUM (exponent));
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
mem2ureal (SCM mem, unsigned int *p_idx,
	   unsigned int radix, enum t_exactness forced_x,
           int allow_inf_or_nan)
{
  unsigned int idx = *p_idx;
  SCM result;
  size_t len = scm_i_string_length (mem);

  /* Start off believing that the number will be exact.  This changes
     to INEXACT if we see a decimal point or a hash. */
  enum t_exactness implicit_x = EXACT;

  if (idx == len)
    return SCM_BOOL_F;

  if (allow_inf_or_nan && forced_x != EXACT && idx+5 <= len)
    switch (scm_i_string_ref (mem, idx))
      {
      case 'i': case 'I':
        switch (scm_i_string_ref (mem, idx + 1))
          {
          case 'n': case 'N':
            switch (scm_i_string_ref (mem, idx + 2))
              {
              case 'f': case 'F':
                if (scm_i_string_ref (mem, idx + 3) == '.'
                    && scm_i_string_ref (mem, idx + 4) == '0')
                  {
                    *p_idx = idx+5;
                    return scm_inf ();
                  }
              }
          }
      case 'n': case 'N':
        switch (scm_i_string_ref (mem, idx + 1))
          {
          case 'a': case 'A':
            switch (scm_i_string_ref (mem, idx + 2))
              {
              case 'n': case 'N':
                if (scm_i_string_ref (mem, idx + 3) == '.')
                  {
                    /* Cobble up the fractional part.  We might want to
                       set the NaN's mantissa from it. */
                    idx += 4;
                    if (!scm_is_eq (mem2uinteger (mem, &idx, 10, &implicit_x),
                                    SCM_INUM0))
                      {
#if SCM_ENABLE_DEPRECATED == 1
                        scm_c_issue_deprecation_warning
                          ("Non-zero suffixes to `+nan.' are deprecated.  Use `+nan.0'.");
#else
                        return SCM_BOOL_F;
#endif
                      }
          
                    *p_idx = idx;
                    return scm_nan ();
                  }
              }
          }
      }

  if (scm_i_string_ref (mem, idx) == '.')
    {
      if (radix != 10)
	return SCM_BOOL_F;
      else if (idx + 1 == len)
	return SCM_BOOL_F;
      else if (!uc_is_property_decimal_digit ((scm_t_uint32) scm_i_string_ref (mem, idx+1)))
	return SCM_BOOL_F;
      else
	result = mem2decimal_from_point (SCM_INUM0, mem,
					 p_idx, &implicit_x);
    }
  else
    {
      SCM uinteger;

      uinteger = mem2uinteger (mem, &idx, radix, &implicit_x);
      if (scm_is_false (uinteger))
	return SCM_BOOL_F;

      if (idx == len)
	result = uinteger;
      else if (scm_i_string_ref (mem, idx) == '/')
	{
	  SCM divisor;

	  idx++;
          if (idx == len)
            return SCM_BOOL_F;

	  divisor = mem2uinteger (mem, &idx, radix, &implicit_x);
	  if (scm_is_false (divisor) || scm_is_eq (divisor, SCM_INUM0))
	    return SCM_BOOL_F;

	  /* both are int/big here, I assume */
	  result = scm_i_make_ratio (uinteger, divisor);
	}
      else if (radix == 10)
	{
	  result = mem2decimal_from_point (uinteger, mem, &idx, &implicit_x);
	  if (scm_is_false (result))
	    return SCM_BOOL_F;
	}
      else
	result = uinteger;

      *p_idx = idx;
    }

  switch (forced_x)
    {
    case EXACT:
      if (SCM_INEXACTP (result))
	return scm_inexact_to_exact (result);
      else
	return result;
    case INEXACT:
      if (SCM_INEXACTP (result))
	return result;
      else
	return scm_exact_to_inexact (result);
    case NO_EXACTNESS:
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

  /* We should never get here */
  assert (0);
}


/* R5RS, section 7.1.1, lexical structure of numbers: <complex R> */

static SCM
mem2complex (SCM mem, unsigned int idx,
	     unsigned int radix, enum t_exactness forced_x)
{
  scm_t_wchar c;
  int sign = 0;
  SCM ureal;
  size_t len = scm_i_string_length (mem);

  if (idx == len)
    return SCM_BOOL_F;

  c = scm_i_string_ref (mem, idx);
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

  ureal = mem2ureal (mem, &idx, radix, forced_x, sign != 0);
  if (scm_is_false (ureal))
    {
      /* input must be either +i or -i */

      if (sign == 0)
	return SCM_BOOL_F;

      if (scm_i_string_ref (mem, idx) == 'i'
	  || scm_i_string_ref (mem, idx) == 'I')
	{
	  idx++;
	  if (idx != len)
	    return SCM_BOOL_F;
	  
	  return scm_make_rectangular (SCM_INUM0, SCM_I_MAKINUM (sign));
	}
      else
	return SCM_BOOL_F;
    }
  else
    {
      if (sign == -1 && scm_is_false (scm_nan_p (ureal)))
	ureal = scm_difference (ureal, SCM_UNDEFINED);

      if (idx == len)
	return ureal;

      c = scm_i_string_ref (mem, idx);
      switch (c)
	{
	case 'i': case 'I':
	  /* either +<ureal>i or -<ureal>i */

	  idx++;
	  if (sign == 0)
	    return SCM_BOOL_F;
	  if (idx != len)
	    return SCM_BOOL_F;
	  return scm_make_rectangular (SCM_INUM0, ureal);

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

	      c = scm_i_string_ref (mem, idx);
	      if (c == '+')
		{
		  idx++;
                  if (idx == len)
                    return SCM_BOOL_F;
		  sign = 1;
		}
	      else if (c == '-')
		{
		  idx++;
                  if (idx == len)
                    return SCM_BOOL_F;
		  sign = -1;
		}
	      else
		sign = 0;

	      angle = mem2ureal (mem, &idx, radix, forced_x, sign != 0);
	      if (scm_is_false (angle))
		return SCM_BOOL_F;
	      if (idx != len)
		return SCM_BOOL_F;

	      if (sign == -1 && scm_is_false (scm_nan_p (ureal)))
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
	      SCM imag = mem2ureal (mem, &idx, radix, forced_x, sign != 0);

	      if (scm_is_false (imag))
		imag = SCM_I_MAKINUM (sign);
	      else if (sign == -1 && scm_is_false (scm_nan_p (imag)))
		imag = scm_difference (imag, SCM_UNDEFINED);

	      if (idx == len)
		return SCM_BOOL_F;
	      if (scm_i_string_ref (mem, idx) != 'i'
		  && scm_i_string_ref (mem, idx) != 'I')
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
scm_i_string_to_number (SCM mem, unsigned int default_radix)
{
  unsigned int idx = 0;
  unsigned int radix = NO_RADIX;
  enum t_exactness forced_x = NO_EXACTNESS;
  size_t len = scm_i_string_length (mem);

  /* R5RS, section 7.1.1, lexical structure of numbers: <prefix R> */
  while (idx + 2 < len && scm_i_string_ref (mem, idx) == '#')
    {
      switch (scm_i_string_ref (mem, idx + 1))
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
    radix = default_radix;

  return mem2complex (mem, idx, radix, forced_x);
}

SCM
scm_c_locale_stringn_to_number (const char* mem, size_t len,
				unsigned int default_radix)
{
  SCM str = scm_from_locale_stringn (mem, len);

  return scm_i_string_to_number (str, default_radix);
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
  unsigned int base;
  SCM_VALIDATE_STRING (1, string);

  if (SCM_UNBNDP (radix))
    base = 10;
  else
    base = scm_to_unsigned_integer (radix, 2, INT_MAX);

  answer = scm_i_string_to_number (string, base);
  scm_remember_upto_here_1 (string);
  return answer;
}
#undef FUNC_NAME


/*** END strs->nums ***/


SCM_DEFINE (scm_number_p, "number?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a number, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_number_p
{
  return scm_from_bool (SCM_NUMBERP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_complex_p, "complex?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a complex number, @code{#f}\n"
	    "otherwise.  Note that the sets of real, rational and integer\n"
	    "values form subsets of the set of complex numbers, i. e. the\n"
	    "predicate will also be fulfilled if @var{x} is a real,\n"
	    "rational or integer number.")
#define FUNC_NAME s_scm_complex_p
{
  /* all numbers are complex. */
  return scm_number_p (x);
}
#undef FUNC_NAME

SCM_DEFINE (scm_real_p, "real?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a real number, @code{#f}\n"
	    "otherwise.  Note that the set of integer values forms a subset of\n"
	    "the set of real numbers, i. e. the predicate will also be\n"
	    "fulfilled if @var{x} is an integer number.")
#define FUNC_NAME s_scm_real_p
{
  return scm_from_bool
    (SCM_I_INUMP (x) || SCM_REALP (x) || SCM_BIGP (x) || SCM_FRACTIONP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_rational_p, "rational?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a rational number, @code{#f}\n"
	    "otherwise.  Note that the set of integer values forms a subset of\n"
	    "the set of rational numbers, i. e. the predicate will also be\n"
	    "fulfilled if @var{x} is an integer number.")
#define FUNC_NAME s_scm_rational_p
{
  if (SCM_I_INUMP (x) || SCM_BIGP (x) || SCM_FRACTIONP (x))
    return SCM_BOOL_T;
  else if (SCM_REALP (x))
    /* due to their limited precision, finite floating point numbers are
       rational as well. (finite means neither infinity nor a NaN) */
    return scm_from_bool (isfinite (SCM_REAL_VALUE (x)));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_integer_p, "integer?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is an integer number,\n"
	    "else return @code{#f}.")
#define FUNC_NAME s_scm_integer_p
{
  if (SCM_I_INUMP (x) || SCM_BIGP (x))
    return SCM_BOOL_T;
  else if (SCM_REALP (x))
    {
      double val = SCM_REAL_VALUE (x);
      return scm_from_bool (!isinf (val) && (val == floor (val)));
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_exact_integer_p, "exact-integer?", 1, 0, 0,
            (SCM x),
	    "Return @code{#t} if @var{x} is an exact integer number,\n"
	    "else return @code{#f}.")
#define FUNC_NAME s_scm_exact_integer_p
{
  if (SCM_I_INUMP (x) || SCM_BIGP (x))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM scm_i_num_eq_p (SCM, SCM, SCM);
SCM_PRIMITIVE_GENERIC (scm_i_num_eq_p, "=", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return @code{#t} if all parameters are numerically equal.")
#define FUNC_NAME s_scm_i_num_eq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_num_eq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_num_eq_p (x, y);
}
#undef FUNC_NAME
SCM
scm_num_eq_p (SCM x, SCM y)
{
 again:
  if (SCM_I_INUMP (x))
    {
      scm_t_signed_bits xx = SCM_I_INUM (x);
      if (SCM_I_INUMP (y))
	{
	  scm_t_signed_bits yy = SCM_I_INUM (y);
	  return scm_from_bool (xx == yy);
	}
      else if (SCM_BIGP (y))
	return SCM_BOOL_F;
      else if (SCM_REALP (y))
        {
          /* On a 32-bit system an inum fits a double, we can cast the inum
             to a double and compare.

             But on a 64-bit system an inum is bigger than a double and
             casting it to a double (call that dxx) will round.
             Although dxx will not in general be equal to xx, dxx will
             always be an integer and within a factor of 2 of xx, so if
             dxx==yy, we know that yy is an integer and fits in
             scm_t_signed_bits.  So we cast yy to scm_t_signed_bits and
             compare with plain xx.

             An alternative (for any size system actually) would be to check
             yy is an integer (with floor) and is in range of an inum
             (compare against appropriate powers of 2) then test
             xx==(scm_t_signed_bits)yy.  It's just a matter of which
             casts/comparisons might be fastest or easiest for the cpu.  */

          double yy = SCM_REAL_VALUE (y);
          return scm_from_bool ((double) xx == yy
				&& (DBL_MANT_DIG >= SCM_I_FIXNUM_BIT-1
				    || xx == (scm_t_signed_bits) yy));
        }
      else if (SCM_COMPLEXP (y))
        {
          /* see comments with inum/real above */
          double ry = SCM_COMPLEX_REAL (y);
          return scm_from_bool ((double) xx == ry
                                && 0.0 == SCM_COMPLEX_IMAG (y)
                                && (DBL_MANT_DIG >= SCM_I_FIXNUM_BIT-1
                                    || xx == (scm_t_signed_bits) ry));
        }
      else if (SCM_FRACTIONP (y))
	return SCM_BOOL_F;
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_eq_p, x, y, SCM_ARGn, s_scm_i_num_eq_p);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
	return SCM_BOOL_F;
      else if (SCM_BIGP (y))
	{
	  int cmp = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_from_bool (0 == cmp);
	}
      else if (SCM_REALP (y))
	{
	  int cmp;
	  if (isnan (SCM_REAL_VALUE (y)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), SCM_REAL_VALUE (y));
	  scm_remember_upto_here_1 (x);
	  return scm_from_bool (0 == cmp);
	}
      else if (SCM_COMPLEXP (y))
	{
	  int cmp;
	  if (0.0 != SCM_COMPLEX_IMAG (y))
	    return SCM_BOOL_F;
	  if (isnan (SCM_COMPLEX_REAL (y)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), SCM_COMPLEX_REAL (y));
	  scm_remember_upto_here_1 (x);
	  return scm_from_bool (0 == cmp);
	}
      else if (SCM_FRACTIONP (y))
	return SCM_BOOL_F;
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_eq_p, x, y, SCM_ARGn, s_scm_i_num_eq_p);
    }
  else if (SCM_REALP (x))
    {
      double xx = SCM_REAL_VALUE (x);
      if (SCM_I_INUMP (y))
        {
          /* see comments with inum/real above */
          scm_t_signed_bits yy = SCM_I_INUM (y);
          return scm_from_bool (xx == (double) yy
				&& (DBL_MANT_DIG >= SCM_I_FIXNUM_BIT-1
				    || (scm_t_signed_bits) xx == yy));
        }
      else if (SCM_BIGP (y))
	{
	  int cmp;
	  if (isnan (xx))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), xx);
	  scm_remember_upto_here_1 (y);
	  return scm_from_bool (0 == cmp);
	}
      else if (SCM_REALP (y))
	return scm_from_bool (xx == SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_from_bool ((xx == SCM_COMPLEX_REAL (y))
                              && (0.0 == SCM_COMPLEX_IMAG (y)));
      else if (SCM_FRACTIONP (y))
        {
          if (isnan (xx) || isinf (xx))
            return SCM_BOOL_F;
          x = scm_inexact_to_exact (x);  /* with x as frac or int */
          goto again;
        }
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_eq_p, x, y, SCM_ARGn, s_scm_i_num_eq_p);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_I_INUMP (y))
        {
          /* see comments with inum/real above */
          double rx = SCM_COMPLEX_REAL (x);
          scm_t_signed_bits yy = SCM_I_INUM (y);
          return scm_from_bool (rx == (double) yy
                                && 0.0 == SCM_COMPLEX_IMAG (x)
                                && (DBL_MANT_DIG >= SCM_I_FIXNUM_BIT-1
                                    || (scm_t_signed_bits) rx == yy));
        }
      else if (SCM_BIGP (y))
	{
	  int cmp;
	  if (0.0 != SCM_COMPLEX_IMAG (x))
	    return SCM_BOOL_F;
	  if (isnan (SCM_COMPLEX_REAL (x)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), SCM_COMPLEX_REAL (x));
	  scm_remember_upto_here_1 (y);
	  return scm_from_bool (0 == cmp);
	}
      else if (SCM_REALP (y))
	return scm_from_bool ((SCM_COMPLEX_REAL (x) == SCM_REAL_VALUE (y))
                              && (SCM_COMPLEX_IMAG (x) == 0.0));
      else if (SCM_COMPLEXP (y))
	return scm_from_bool ((SCM_COMPLEX_REAL (x) == SCM_COMPLEX_REAL (y))
                              && (SCM_COMPLEX_IMAG (x) == SCM_COMPLEX_IMAG (y)));
      else if (SCM_FRACTIONP (y))
        {
          double  xx;
          if (SCM_COMPLEX_IMAG (x) != 0.0)
            return SCM_BOOL_F;
          xx = SCM_COMPLEX_REAL (x);
          if (isnan (xx) || isinf (xx))
            return SCM_BOOL_F;
          x = scm_inexact_to_exact (x);  /* with x as frac or int */
          goto again;
        }
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_eq_p, x, y, SCM_ARGn, s_scm_i_num_eq_p);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y))
	return SCM_BOOL_F;
      else if (SCM_BIGP (y))
	return SCM_BOOL_F;
      else if (SCM_REALP (y))
        {
          double yy = SCM_REAL_VALUE (y);
          if (isnan (yy) || isinf (yy))
            return SCM_BOOL_F;
          y = scm_inexact_to_exact (y);  /* with y as frac or int */
          goto again;
        }
      else if (SCM_COMPLEXP (y))
        {
          double yy;
          if (SCM_COMPLEX_IMAG (y) != 0.0)
            return SCM_BOOL_F;
          yy = SCM_COMPLEX_REAL (y);
          if (isnan (yy) || isinf(yy))
            return SCM_BOOL_F;
          y = scm_inexact_to_exact (y);  /* with y as frac or int */
          goto again;
        }
      else if (SCM_FRACTIONP (y))
	return scm_i_fraction_equalp (x, y);
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_eq_p, x, y, SCM_ARGn, s_scm_i_num_eq_p);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_i_num_eq_p, x, y, SCM_ARG1, s_scm_i_num_eq_p);
}


/* OPTIMIZE-ME: For int/frac and frac/frac compares, the multiplications
   done are good for inums, but for bignums an answer can almost always be
   had by just examining a few high bits of the operands, as done by GMP in
   mpq_cmp.  flonum/frac compares likewise, but with the slight complication
   of the float exponent to take into account.  */

SCM_INTERNAL SCM scm_i_num_less_p (SCM, SCM, SCM);
SCM_PRIMITIVE_GENERIC (scm_i_num_less_p, "<", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return @code{#t} if the list of parameters is monotonically\n"
                       "increasing.")
#define FUNC_NAME s_scm_i_num_less_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_less_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_less_p (x, y);
}
#undef FUNC_NAME
SCM
scm_less_p (SCM x, SCM y)
{
 again:
  if (SCM_I_INUMP (x))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_I_INUMP (y))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  return scm_from_bool (xx < yy);
	}
      else if (SCM_BIGP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return scm_from_bool (sgn > 0);
	}
      else if (SCM_REALP (y))
        {
          /* We can safely take the ceiling of y without changing the
             result of x<y, given that x is an integer. */
          double yy = ceil (SCM_REAL_VALUE (y));

          /* In the following comparisons, it's important that the right
             hand side always be a power of 2, so that it can be
             losslessly converted to a double even on 64-bit
             machines. */
          if (yy >= (double) (SCM_MOST_POSITIVE_FIXNUM+1))
            return SCM_BOOL_T;
          else if (!(yy > (double) SCM_MOST_NEGATIVE_FIXNUM))
            /* The condition above is carefully written to include the
               case where yy==NaN. */
            return SCM_BOOL_F;
          else
            /* yy is a finite integer that fits in an inum. */
            return scm_from_bool (xx < (scm_t_inum) yy);
        }
      else if (SCM_FRACTIONP (y))
        {
          /* "x < a/b" becomes "x*b < a" */
        int_frac:
          x = scm_product (x, SCM_FRACTION_DENOMINATOR (y));
          y = SCM_FRACTION_NUMERATOR (y);
          goto again;
        }
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_less_p, x, y, SCM_ARGn, s_scm_i_num_less_p);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
	  scm_remember_upto_here_1 (x);
	  return scm_from_bool (sgn < 0);
	}
      else if (SCM_BIGP (y))
	{
	  int cmp = mpz_cmp (SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_2 (x, y);
	  return scm_from_bool (cmp < 0);
	}
      else if (SCM_REALP (y))
	{
	  int cmp;
	  if (isnan (SCM_REAL_VALUE (y)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (x), SCM_REAL_VALUE (y));
	  scm_remember_upto_here_1 (x);
	  return scm_from_bool (cmp < 0);
	}
      else if (SCM_FRACTIONP (y))
        goto int_frac;
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_less_p, x, y, SCM_ARGn, s_scm_i_num_less_p);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_I_INUMP (y))
        {
          /* We can safely take the floor of x without changing the
             result of x<y, given that y is an integer. */
          double xx = floor (SCM_REAL_VALUE (x));

          /* In the following comparisons, it's important that the right
             hand side always be a power of 2, so that it can be
             losslessly converted to a double even on 64-bit
             machines. */
          if (xx < (double) SCM_MOST_NEGATIVE_FIXNUM)
            return SCM_BOOL_T;
          else if (!(xx < (double) (SCM_MOST_POSITIVE_FIXNUM+1)))
            /* The condition above is carefully written to include the
               case where xx==NaN. */
            return SCM_BOOL_F;
          else
            /* xx is a finite integer that fits in an inum. */
            return scm_from_bool ((scm_t_inum) xx < SCM_I_INUM (y));
        }
      else if (SCM_BIGP (y))
	{
	  int cmp;
	  if (isnan (SCM_REAL_VALUE (x)))
	    return SCM_BOOL_F;
	  cmp = xmpz_cmp_d (SCM_I_BIG_MPZ (y), SCM_REAL_VALUE (x));
	  scm_remember_upto_here_1 (y);
	  return scm_from_bool (cmp > 0);
	}
      else if (SCM_REALP (y))
	return scm_from_bool (SCM_REAL_VALUE (x) < SCM_REAL_VALUE (y));
      else if (SCM_FRACTIONP (y))
        {
          double  xx = SCM_REAL_VALUE (x);
	  if (isnan (xx))
	    return SCM_BOOL_F;
          if (isinf (xx))
            return scm_from_bool (xx < 0.0);
          x = scm_inexact_to_exact (x);  /* with x as frac or int */
          goto again;
        }
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_less_p, x, y, SCM_ARGn, s_scm_i_num_less_p);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y) || SCM_BIGP (y))
        {
          /* "a/b < y" becomes "a < y*b" */
          y = scm_product (y, SCM_FRACTION_DENOMINATOR (x));
          x = SCM_FRACTION_NUMERATOR (x);
          goto again;
        }
      else if (SCM_REALP (y))
        {
          double yy = SCM_REAL_VALUE (y);
          if (isnan (yy))
            return SCM_BOOL_F;
          if (isinf (yy))
            return scm_from_bool (0.0 < yy);
          y = scm_inexact_to_exact (y);  /* with y as frac or int */
          goto again;
        }
      else if (SCM_FRACTIONP (y))
        {
          /* "a/b < c/d" becomes "a*d < c*b" */
          SCM new_x = scm_product (SCM_FRACTION_NUMERATOR (x),
                                   SCM_FRACTION_DENOMINATOR (y));
          SCM new_y = scm_product (SCM_FRACTION_NUMERATOR (y),
                                   SCM_FRACTION_DENOMINATOR (x));
          x = new_x;
          y = new_y;
          goto again;
        }
      else
	SCM_WTA_DISPATCH_2 (g_scm_i_num_less_p, x, y, SCM_ARGn, s_scm_i_num_less_p);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_i_num_less_p, x, y, SCM_ARG1, s_scm_i_num_less_p);
}


SCM scm_i_num_gr_p (SCM, SCM, SCM);
SCM_PRIMITIVE_GENERIC (scm_i_num_gr_p, ">", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return @code{#t} if the list of parameters is monotonically\n"
                       "decreasing.")
#define FUNC_NAME s_scm_i_num_gr_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_gr_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_gr_p (x, y);
}
#undef FUNC_NAME
#define FUNC_NAME s_scm_i_num_gr_p
SCM
scm_gr_p (SCM x, SCM y)
{
  if (!SCM_NUMBERP (x))
    SCM_WTA_DISPATCH_2 (g_scm_i_num_gr_p, x, y, SCM_ARG1, FUNC_NAME);
  else if (!SCM_NUMBERP (y))
    SCM_WTA_DISPATCH_2 (g_scm_i_num_gr_p, x, y, SCM_ARG2, FUNC_NAME);
  else
    return scm_less_p (y, x);
}
#undef FUNC_NAME


SCM scm_i_num_leq_p (SCM, SCM, SCM);
SCM_PRIMITIVE_GENERIC (scm_i_num_leq_p, "<=", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return @code{#t} if the list of parameters is monotonically\n"
                       "non-decreasing.")
#define FUNC_NAME s_scm_i_num_leq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_leq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_leq_p (x, y);
}
#undef FUNC_NAME
#define FUNC_NAME s_scm_i_num_leq_p
SCM
scm_leq_p (SCM x, SCM y)
{
  if (!SCM_NUMBERP (x))
    SCM_WTA_DISPATCH_2 (g_scm_i_num_leq_p, x, y, SCM_ARG1, FUNC_NAME);
  else if (!SCM_NUMBERP (y))
    SCM_WTA_DISPATCH_2 (g_scm_i_num_leq_p, x, y, SCM_ARG2, FUNC_NAME);
  else if (scm_is_true (scm_nan_p (x)) || scm_is_true (scm_nan_p (y)))
    return SCM_BOOL_F;
  else
    return scm_not (scm_less_p (y, x));
}
#undef FUNC_NAME


SCM scm_i_num_geq_p (SCM, SCM, SCM);
SCM_PRIMITIVE_GENERIC (scm_i_num_geq_p, ">=", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return @code{#t} if the list of parameters is monotonically\n"
                       "non-increasing.")
#define FUNC_NAME s_scm_i_num_geq_p
{
  if (SCM_UNBNDP (x) || SCM_UNBNDP (y))
    return SCM_BOOL_T;
  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_geq_p (x, y)))
        return SCM_BOOL_F;
      x = y;
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_geq_p (x, y);
}
#undef FUNC_NAME
#define FUNC_NAME s_scm_i_num_geq_p
SCM
scm_geq_p (SCM x, SCM y)
{
  if (!SCM_NUMBERP (x))
    SCM_WTA_DISPATCH_2 (g_scm_i_num_geq_p, x, y, SCM_ARG1, FUNC_NAME);
  else if (!SCM_NUMBERP (y))
    SCM_WTA_DISPATCH_2 (g_scm_i_num_geq_p, x, y, SCM_ARG2, FUNC_NAME);
  else if (scm_is_true (scm_nan_p (x)) || scm_is_true (scm_nan_p (y)))
    return SCM_BOOL_F;
  else
    return scm_not (scm_less_p (x, y));
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_zero_p, "zero?", 1, 0, 0,
		       (SCM z),
	"Return @code{#t} if @var{z} is an exact or inexact number equal to\n"
	"zero.")
#define FUNC_NAME s_scm_zero_p
{
  if (SCM_I_INUMP (z))
    return scm_from_bool (scm_is_eq (z, SCM_INUM0));
  else if (SCM_BIGP (z))
    return SCM_BOOL_F;
  else if (SCM_REALP (z))
    return scm_from_bool (SCM_REAL_VALUE (z) == 0.0);
  else if (SCM_COMPLEXP (z))
    return scm_from_bool (SCM_COMPLEX_REAL (z) == 0.0
		     && SCM_COMPLEX_IMAG (z) == 0.0);
  else if (SCM_FRACTIONP (z))
    return SCM_BOOL_F;
  else
    SCM_WTA_DISPATCH_1 (g_scm_zero_p, z, SCM_ARG1, s_scm_zero_p);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_positive_p, "positive?", 1, 0, 0,
		       (SCM x),
	"Return @code{#t} if @var{x} is an exact or inexact number greater than\n"
	"zero.")
#define FUNC_NAME s_scm_positive_p
{
  if (SCM_I_INUMP (x))
    return scm_from_bool (SCM_I_INUM (x) > 0);
  else if (SCM_BIGP (x))
    {
      int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
      scm_remember_upto_here_1 (x);
      return scm_from_bool (sgn > 0);
    }
  else if (SCM_REALP (x))
    return scm_from_bool(SCM_REAL_VALUE (x) > 0.0);
  else if (SCM_FRACTIONP (x))
    return scm_positive_p (SCM_FRACTION_NUMERATOR (x));
  else
    SCM_WTA_DISPATCH_1 (g_scm_positive_p, x, SCM_ARG1, s_scm_positive_p);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_negative_p, "negative?", 1, 0, 0,
		       (SCM x),
	"Return @code{#t} if @var{x} is an exact or inexact number less than\n"
	"zero.")
#define FUNC_NAME s_scm_negative_p
{
  if (SCM_I_INUMP (x))
    return scm_from_bool (SCM_I_INUM (x) < 0);
  else if (SCM_BIGP (x))
    {
      int sgn = mpz_sgn (SCM_I_BIG_MPZ (x));
      scm_remember_upto_here_1 (x);
      return scm_from_bool (sgn < 0);
    }
  else if (SCM_REALP (x))
    return scm_from_bool(SCM_REAL_VALUE (x) < 0.0);
  else if (SCM_FRACTIONP (x))
    return scm_negative_p (SCM_FRACTION_NUMERATOR (x));
  else
    SCM_WTA_DISPATCH_1 (g_scm_negative_p, x, SCM_ARG1, s_scm_negative_p);
}
#undef FUNC_NAME


/* scm_min and scm_max return an inexact when either argument is inexact, as
   required by r5rs.  On that basis, for exact/inexact combinations the
   exact is converted to inexact to compare and possibly return.  This is
   unlike scm_less_p above which takes some trouble to preserve all bits in
   its test, such trouble is not required for min and max.  */

SCM_PRIMITIVE_GENERIC (scm_i_max, "max", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return the maximum of all parameter values.")
#define FUNC_NAME s_scm_i_max
{
  while (!scm_is_null (rest))
    { x = scm_max (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_max (x, y);
}
#undef FUNC_NAME
                       
#define s_max s_scm_i_max
#define g_max g_scm_i_max

SCM
scm_max (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_UNBNDP (x))
	SCM_WTA_DISPATCH_0 (g_max, s_max);
      else if (SCM_I_INUMP(x) || SCM_BIGP(x) || SCM_REALP(x) || SCM_FRACTIONP(x))
	return x;
      else
	SCM_WTA_DISPATCH_1 (g_max, x, SCM_ARG1, s_max);
    }
  
  if (SCM_I_INUMP (x))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_I_INUMP (y))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
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
	  double xxd = xx;
	  double yyd = SCM_REAL_VALUE (y);

	  if (xxd > yyd)
	    return scm_i_from_double (xxd);
	  /* If y is a NaN, then "==" is false and we return the NaN */
	  else if (SCM_LIKELY (!(xxd == yyd)))
	    return y;
	  /* Handle signed zeroes properly */
	  else if (xx == 0)
	    return flo0;
	  else
	    return y;
	}
      else if (SCM_FRACTIONP (y))
	{
        use_less:
          return (scm_is_false (scm_less_p (x, y)) ? x : y);
	}
      else
	SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
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
          /* if y==NaN then xx>yy is false, so we return the NaN y */
          double xx, yy;
        big_real:
          xx = scm_i_big2dbl (x);
          yy = SCM_REAL_VALUE (y);
	  return (xx > yy ? scm_i_from_double (xx) : y);
	}
      else if (SCM_FRACTIONP (y))
	{
          goto use_less;
	}
      else
	SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  double xxd = SCM_REAL_VALUE (x);
	  double yyd = yy;

	  if (yyd > xxd)
	    return scm_i_from_double (yyd);
	  /* If x is a NaN, then "==" is false and we return the NaN */
	  else if (SCM_LIKELY (!(xxd == yyd)))
	    return x;
	  /* Handle signed zeroes properly */
	  else if (yy == 0)
	    return flo0;
	  else
	    return x;
	}
      else if (SCM_BIGP (y))
	{
          SCM_SWAP (x, y);
          goto big_real;
	}
      else if (SCM_REALP (y))
	{
	  double xx = SCM_REAL_VALUE (x);
	  double yy = SCM_REAL_VALUE (y);

	  /* For purposes of max: nan > +inf.0 > everything else,
             per the R6RS errata */
	  if (xx > yy)
	    return x;
	  else if (SCM_LIKELY (xx < yy))
	    return y;
	  /* If neither (xx > yy) nor (xx < yy), then
	     either they're equal or one is a NaN */
	  else if (SCM_UNLIKELY (xx != yy))
	    return (xx != xx) ? x : y;  /* Return the NaN */
	  /* xx == yy, but handle signed zeroes properly */
	  else if (copysign (1.0, yy) < 0.0)
	    return x;
	  else
	    return y;
	}
      else if (SCM_FRACTIONP (y))
	{
	  double yy = scm_i_fraction2double (y);
	  double xx = SCM_REAL_VALUE (x);
	  return (xx < yy) ? scm_i_from_double (yy) : x;
	}
      else
	SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y))
	{
          goto use_less;
	}
      else if (SCM_BIGP (y))
	{
          goto use_less;
	}
      else if (SCM_REALP (y))
	{
	  double xx = scm_i_fraction2double (x);
	  /* if y==NaN then ">" is false, so we return the NaN y */
	  return (xx > SCM_REAL_VALUE (y)) ? scm_i_from_double (xx) : y;
	}
      else if (SCM_FRACTIONP (y))
	{
          goto use_less;
	}
      else
	SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARGn, s_max);
    }
  else
    SCM_WTA_DISPATCH_2 (g_max, x, y, SCM_ARG1, s_max);
}


SCM_PRIMITIVE_GENERIC (scm_i_min, "min", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return the minimum of all parameter values.")
#define FUNC_NAME s_scm_i_min
{
  while (!scm_is_null (rest))
    { x = scm_min (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_min (x, y);
}
#undef FUNC_NAME
                       
#define s_min s_scm_i_min
#define g_min g_scm_i_min

SCM
scm_min (SCM x, SCM y)
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_UNBNDP (x))
	SCM_WTA_DISPATCH_0 (g_min, s_min);
      else if (SCM_I_INUMP(x) || SCM_BIGP(x) || SCM_REALP(x) || SCM_FRACTIONP(x))
	return x;
      else
	SCM_WTA_DISPATCH_1 (g_min, x, SCM_ARG1, s_min);
    }
  
  if (SCM_I_INUMP (x))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_I_INUMP (y))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
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
	  return (z < SCM_REAL_VALUE (y)) ? scm_i_from_double (z) : y;
	}
      else if (SCM_FRACTIONP (y))
	{
        use_less:
          return (scm_is_false (scm_less_p (x, y)) ? y : x);
	}
      else
	SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
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
          /* if y==NaN then xx<yy is false, so we return the NaN y */
          double xx, yy;
        big_real:
          xx = scm_i_big2dbl (x);
          yy = SCM_REAL_VALUE (y);
	  return (xx < yy ? scm_i_from_double (xx) : y);
	}
      else if (SCM_FRACTIONP (y))
	{
          goto use_less;
	}
      else
	SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  double z = SCM_I_INUM (y);
	  /* if x==NaN then "<" is false and we return NaN */
	  return (z < SCM_REAL_VALUE (x)) ? scm_i_from_double (z) : x;
	}
      else if (SCM_BIGP (y))
	{
          SCM_SWAP (x, y);
          goto big_real;
	}
      else if (SCM_REALP (y))
	{
	  double xx = SCM_REAL_VALUE (x);
	  double yy = SCM_REAL_VALUE (y);

	  /* For purposes of min: nan < -inf.0 < everything else,
             per the R6RS errata */
	  if (xx < yy)
	    return x;
	  else if (SCM_LIKELY (xx > yy))
	    return y;
	  /* If neither (xx < yy) nor (xx > yy), then
	     either they're equal or one is a NaN */
	  else if (SCM_UNLIKELY (xx != yy))
	    return (xx != xx) ? x : y;  /* Return the NaN */
	  /* xx == yy, but handle signed zeroes properly */
	  else if (copysign (1.0, xx) < 0.0)
	    return x;
	  else
	    return y;
	}
      else if (SCM_FRACTIONP (y))
	{
	  double yy = scm_i_fraction2double (y);
	  double xx = SCM_REAL_VALUE (x);
	  return (yy < xx) ? scm_i_from_double (yy) : x;
	}
      else
	SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y))
	{
          goto use_less;
	}
      else if (SCM_BIGP (y))
	{
          goto use_less;
	}
      else if (SCM_REALP (y))
	{
	  double xx = scm_i_fraction2double (x);
	  /* if y==NaN then "<" is false, so we return the NaN y */
	  return (xx < SCM_REAL_VALUE (y)) ? scm_i_from_double (xx) : y;
	}
      else if (SCM_FRACTIONP (y))
	{
          goto use_less;
	}
      else
	SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARGn, s_min);
    }
  else
    SCM_WTA_DISPATCH_2 (g_min, x, y, SCM_ARG1, s_min);
}


SCM_PRIMITIVE_GENERIC (scm_i_sum, "+", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return the sum of all parameter values.  Return 0 if called without\n"
                       "any parameters." )
#define FUNC_NAME s_scm_i_sum
{
  while (!scm_is_null (rest))
    { x = scm_sum (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_sum (x, y);
}
#undef FUNC_NAME
                       
#define s_sum s_scm_i_sum
#define g_sum g_scm_i_sum

SCM
scm_sum (SCM x, SCM y)
{
  if (SCM_UNLIKELY (SCM_UNBNDP (y)))
    {
      if (SCM_NUMBERP (x)) return x;
      if (SCM_UNBNDP (x)) return SCM_INUM0;
      SCM_WTA_DISPATCH_1 (g_sum, x, SCM_ARG1, s_sum);
    }

  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
        {
          scm_t_inum xx = SCM_I_INUM (x);
          scm_t_inum yy = SCM_I_INUM (y);
          scm_t_inum z = xx + yy;
          return SCM_FIXABLE (z) ? SCM_I_MAKINUM (z) : scm_i_inum2big (z);
        }
      else if (SCM_BIGP (y))
        {
          SCM_SWAP (x, y);
          goto add_big_inum;
        }
      else if (SCM_REALP (y))
        {
          scm_t_inum xx = SCM_I_INUM (x);
          return scm_i_from_double (xx + SCM_REAL_VALUE (y));
        }
      else if (SCM_COMPLEXP (y))
        {
          scm_t_inum xx = SCM_I_INUM (x);
          return scm_c_make_rectangular (xx + SCM_COMPLEX_REAL (y),
                                   SCM_COMPLEX_IMAG (y));
        }
      else if (SCM_FRACTIONP (y))
	return scm_i_make_ratio (scm_sum (SCM_FRACTION_NUMERATOR (y), 
					scm_product (x, SCM_FRACTION_DENOMINATOR (y))),
			       SCM_FRACTION_DENOMINATOR (y));
      else
        SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    } else if (SCM_BIGP (x))
      {
	if (SCM_I_INUMP (y))
	  {
	    scm_t_inum inum;
	    int bigsgn;
	  add_big_inum:
	    inum = SCM_I_INUM (y);      
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
	    return scm_i_from_double (result);
	  }
	else if (SCM_COMPLEXP (y))
	  {
	    double real_part = (mpz_get_d (SCM_I_BIG_MPZ (x))
				+ SCM_COMPLEX_REAL (y));
	    scm_remember_upto_here_1 (x);
	    return scm_c_make_rectangular (real_part, SCM_COMPLEX_IMAG (y));
	  }
	else if (SCM_FRACTIONP (y))
	  return scm_i_make_ratio (scm_sum (SCM_FRACTION_NUMERATOR (y), 
					  scm_product (x, SCM_FRACTION_DENOMINATOR (y))),
				 SCM_FRACTION_DENOMINATOR (y));
	else
	  SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
      }
  else if (SCM_REALP (x))
    {
      if (SCM_I_INUMP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) + SCM_I_INUM (y));
      else if (SCM_BIGP (y))
	{
	  double result = mpz_get_d (SCM_I_BIG_MPZ (y)) + SCM_REAL_VALUE (x);
	  scm_remember_upto_here_1 (y);
	  return scm_i_from_double (result);
	}
      else if (SCM_REALP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) + SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (SCM_REAL_VALUE (x) + SCM_COMPLEX_REAL (y),
				 SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) + scm_i_fraction2double (y));
      else
	SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_I_INUMP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) + SCM_I_INUM (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_BIGP (y))
	{
	  double real_part = (mpz_get_d (SCM_I_BIG_MPZ (y))
			      + SCM_COMPLEX_REAL (x));
	  scm_remember_upto_here_1 (y);
	  return scm_c_make_rectangular (real_part, SCM_COMPLEX_IMAG (x));
	}
      else if (SCM_REALP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) + SCM_REAL_VALUE (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) + SCM_COMPLEX_REAL (y),
				 SCM_COMPLEX_IMAG (x) + SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) + scm_i_fraction2double (y),
				 SCM_COMPLEX_IMAG (x));
      else
	SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y))
	return scm_i_make_ratio (scm_sum (SCM_FRACTION_NUMERATOR (x), 
					scm_product (y, SCM_FRACTION_DENOMINATOR (x))),
			       SCM_FRACTION_DENOMINATOR (x));
      else if (SCM_BIGP (y))
	return scm_i_make_ratio (scm_sum (SCM_FRACTION_NUMERATOR (x), 
					scm_product (y, SCM_FRACTION_DENOMINATOR (x))),
			       SCM_FRACTION_DENOMINATOR (x));
      else if (SCM_REALP (y))
	return scm_i_from_double (SCM_REAL_VALUE (y) + scm_i_fraction2double (x));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (y) + scm_i_fraction2double (x),
				 SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	/* a/b + c/d = (ad + bc) / bd */
	return scm_i_make_ratio (scm_sum (scm_product (SCM_FRACTION_NUMERATOR (x), SCM_FRACTION_DENOMINATOR (y)),
					scm_product (SCM_FRACTION_NUMERATOR (y), SCM_FRACTION_DENOMINATOR (x))),
			       scm_product (SCM_FRACTION_DENOMINATOR (x), SCM_FRACTION_DENOMINATOR (y)));
      else
	SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARGn, s_sum);
    }
  else
    SCM_WTA_DISPATCH_2 (g_sum, x, y, SCM_ARG1, s_sum);
}


SCM_DEFINE (scm_oneplus, "1+", 1, 0, 0, 
            (SCM x),
	    "Return @math{@var{x}+1}.")
#define FUNC_NAME s_scm_oneplus
{
  return scm_sum (x, SCM_INUM1);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_i_difference, "-", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "If called with one argument @var{z1}, -@var{z1} returned. Otherwise\n"
                       "the sum of all but the first argument are subtracted from the first\n"
                       "argument.")
#define FUNC_NAME s_scm_i_difference
{
  while (!scm_is_null (rest))
    { x = scm_difference (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_difference (x, y);
}
#undef FUNC_NAME
                       
#define s_difference s_scm_i_difference
#define g_difference g_scm_i_difference

SCM
scm_difference (SCM x, SCM y)
#define FUNC_NAME s_difference
{
  if (SCM_UNLIKELY (SCM_UNBNDP (y)))
    {
      if (SCM_UNBNDP (x))
        SCM_WTA_DISPATCH_0 (g_difference, s_difference);
      else 
        if (SCM_I_INUMP (x))
          {
            scm_t_inum xx = -SCM_I_INUM (x);
            if (SCM_FIXABLE (xx))
              return SCM_I_MAKINUM (xx);
            else
              return scm_i_inum2big (xx);
          }
        else if (SCM_BIGP (x))
          /* Must scm_i_normbig here because -SCM_MOST_NEGATIVE_FIXNUM is a
             bignum, but negating that gives a fixnum.  */
          return scm_i_normbig (scm_i_clonebig (x, 0));
        else if (SCM_REALP (x))
          return scm_i_from_double (-SCM_REAL_VALUE (x));
        else if (SCM_COMPLEXP (x))
          return scm_c_make_rectangular (-SCM_COMPLEX_REAL (x),
                                   -SCM_COMPLEX_IMAG (x));
	else if (SCM_FRACTIONP (x))
	  return scm_i_make_ratio_already_reduced
	    (scm_difference (SCM_FRACTION_NUMERATOR (x), SCM_UNDEFINED),
	     SCM_FRACTION_DENOMINATOR (x));
        else
          SCM_WTA_DISPATCH_1 (g_difference, x, SCM_ARG1, s_difference);
    }
  
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum xx = SCM_I_INUM (x);
	  scm_t_inum yy = SCM_I_INUM (y);
	  scm_t_inum z = xx - yy;
	  if (SCM_FIXABLE (z))
	    return SCM_I_MAKINUM (z);
	  else
	    return scm_i_inum2big (z);
	}
      else if (SCM_BIGP (y))
	{
	  /* inum-x - big-y */
	  scm_t_inum xx = SCM_I_INUM (x);

	  if (xx == 0)
	    {
	      /* Must scm_i_normbig here because -SCM_MOST_NEGATIVE_FIXNUM is a
		 bignum, but negating that gives a fixnum.  */
	      return scm_i_normbig (scm_i_clonebig (y, 0));
	    }
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
	  scm_t_inum xx = SCM_I_INUM (x);

	  /*
	   * We need to handle x == exact 0
	   * specially because R6RS states that:
	   *   (- 0.0)     ==> -0.0  and
	   *   (- 0.0 0.0) ==>  0.0
	   * and the scheme compiler changes
	   *   (- 0.0) into (- 0 0.0)
	   * So we need to treat (- 0 0.0) like (- 0.0).
	   * At the C level, (-x) is different than (0.0 - x).
	   * (0.0 - 0.0) ==> 0.0, but (- 0.0) ==> -0.0.
	   */
	  if (xx == 0)
	    return scm_i_from_double (- SCM_REAL_VALUE (y));
	  else
	    return scm_i_from_double (xx - SCM_REAL_VALUE (y));
	}
      else if (SCM_COMPLEXP (y))
	{
	  scm_t_inum xx = SCM_I_INUM (x);

	  /* We need to handle x == exact 0 specially.
	     See the comment above (for SCM_REALP (y)) */
	  if (xx == 0)
	    return scm_c_make_rectangular (- SCM_COMPLEX_REAL (y),
					   - SCM_COMPLEX_IMAG (y));
	  else
	    return scm_c_make_rectangular (xx - SCM_COMPLEX_REAL (y),
					      - SCM_COMPLEX_IMAG (y));
	}
      else if (SCM_FRACTIONP (y))
	/* a - b/c = (ac - b) / c */
	return scm_i_make_ratio (scm_difference (scm_product (x, SCM_FRACTION_DENOMINATOR (y)),
					       SCM_FRACTION_NUMERATOR (y)),
			       SCM_FRACTION_DENOMINATOR (y));
      else
	SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  /* big-x - inum-y */
	  scm_t_inum yy = SCM_I_INUM (y);
	  int sgn_x = mpz_sgn (SCM_I_BIG_MPZ (x));

	  scm_remember_upto_here_1 (x);
	  if (sgn_x == 0)
	    return (SCM_FIXABLE (-yy) ?
		    SCM_I_MAKINUM (-yy) : scm_from_inum (-yy));
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
	  return scm_i_from_double (result);
	}
      else if (SCM_COMPLEXP (y))
	{
	  double real_part = (mpz_get_d (SCM_I_BIG_MPZ (x))
			      - SCM_COMPLEX_REAL (y));
	  scm_remember_upto_here_1 (x);
	  return scm_c_make_rectangular (real_part, - SCM_COMPLEX_IMAG (y));      
	}
      else if (SCM_FRACTIONP (y))
	return scm_i_make_ratio (scm_difference (scm_product (x, SCM_FRACTION_DENOMINATOR (y)),
					       SCM_FRACTION_NUMERATOR (y)),
			       SCM_FRACTION_DENOMINATOR (y));
      else SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_I_INUMP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) - SCM_I_INUM (y));
      else if (SCM_BIGP (y))
	{
	  double result = SCM_REAL_VALUE (x) - mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (x);
	  return scm_i_from_double (result);
	}
      else if (SCM_REALP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) - SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (SCM_REAL_VALUE (x) - SCM_COMPLEX_REAL (y),
				 -SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) - scm_i_fraction2double (y));
      else
	SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_I_INUMP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) - SCM_I_INUM (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_BIGP (y))
	{
	  double real_part = (SCM_COMPLEX_REAL (x)
			      - mpz_get_d (SCM_I_BIG_MPZ (y)));
	  scm_remember_upto_here_1 (x);
	  return scm_c_make_rectangular (real_part, SCM_COMPLEX_IMAG (y));      
	}
      else if (SCM_REALP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) - SCM_REAL_VALUE (y),
				 SCM_COMPLEX_IMAG (x));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) - SCM_COMPLEX_REAL (y),
				 SCM_COMPLEX_IMAG (x) - SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) - scm_i_fraction2double (y),
				 SCM_COMPLEX_IMAG (x));
      else
	SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y))
	/* a/b - c = (a - cb) / b */
	return scm_i_make_ratio (scm_difference (SCM_FRACTION_NUMERATOR (x), 
					       scm_product(y, SCM_FRACTION_DENOMINATOR (x))),
			       SCM_FRACTION_DENOMINATOR (x));
      else if (SCM_BIGP (y))
	return scm_i_make_ratio (scm_difference (SCM_FRACTION_NUMERATOR (x), 
					       scm_product(y, SCM_FRACTION_DENOMINATOR (x))),
			       SCM_FRACTION_DENOMINATOR (x));
      else if (SCM_REALP (y))
	return scm_i_from_double (scm_i_fraction2double (x) - SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (scm_i_fraction2double (x) - SCM_COMPLEX_REAL (y),
				 -SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	/* a/b - c/d = (ad - bc) / bd */
	return scm_i_make_ratio (scm_difference (scm_product (SCM_FRACTION_NUMERATOR (x), SCM_FRACTION_DENOMINATOR (y)),
					       scm_product (SCM_FRACTION_NUMERATOR (y), SCM_FRACTION_DENOMINATOR (x))),
			       scm_product (SCM_FRACTION_DENOMINATOR (x), SCM_FRACTION_DENOMINATOR (y)));
      else
	SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARGn, s_difference);
    }
  else
    SCM_WTA_DISPATCH_2 (g_difference, x, y, SCM_ARG1, s_difference);
}
#undef FUNC_NAME


SCM_DEFINE (scm_oneminus, "1-", 1, 0, 0, 
            (SCM x),
	    "Return @math{@var{x}-1}.")
#define FUNC_NAME s_scm_oneminus
{
  return scm_difference (x, SCM_INUM1);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_i_product, "*", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Return the product of all arguments.  If called without arguments,\n"
                       "1 is returned.")
#define FUNC_NAME s_scm_i_product
{
  while (!scm_is_null (rest))
    { x = scm_product (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_product (x, y);
}
#undef FUNC_NAME
                       
#define s_product s_scm_i_product
#define g_product g_scm_i_product

SCM
scm_product (SCM x, SCM y)
{
  if (SCM_UNLIKELY (SCM_UNBNDP (y)))
    {
      if (SCM_UNBNDP (x))
	return SCM_I_MAKINUM (1L);
      else if (SCM_NUMBERP (x))
	return x;
      else
	SCM_WTA_DISPATCH_1 (g_product, x, SCM_ARG1, s_product);
    }
  
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx;

    xinum:
      xx = SCM_I_INUM (x);

      switch (xx)
	{
        case 1:
	  /* exact1 is the universal multiplicative identity */
	  return y;
	  break;
        case 0:
	  /* exact0 times a fixnum is exact0: optimize this case */
	  if (SCM_LIKELY (SCM_I_INUMP (y)))
	    return SCM_INUM0;
	  /* if the other argument is inexact, the result is inexact,
	     and we must do the multiplication in order to handle
	     infinities and NaNs properly. */
	  else if (SCM_REALP (y))
	    return scm_i_from_double (0.0 * SCM_REAL_VALUE (y));
	  else if (SCM_COMPLEXP (y))
	    return scm_c_make_rectangular (0.0 * SCM_COMPLEX_REAL (y),
					   0.0 * SCM_COMPLEX_IMAG (y));
	  /* we've already handled inexact numbers,
	     so y must be exact, and we return exact0 */
	  else if (SCM_NUMP (y))
	    return SCM_INUM0;
	  else
	    SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
	  break;
        case -1:
	  /*
	   * This case is important for more than just optimization.
	   * It handles the case of negating
	   * (+ 1 most-positive-fixnum) aka (- most-negative-fixnum),
	   * which is a bignum that must be changed back into a fixnum.
	   * Failure to do so will cause the following to return #f:
	   * (= most-negative-fixnum (* -1 (- most-negative-fixnum)))
	   */
	  return scm_difference(y, SCM_UNDEFINED);
	  break;
	}

      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
#if SCM_I_FIXNUM_BIT < 32 && SCM_HAVE_T_INT64
          scm_t_int64 kk = xx * (scm_t_int64) yy;
          if (SCM_FIXABLE (kk))
            return SCM_I_MAKINUM (kk);
#else
          scm_t_inum axx = (xx > 0) ? xx : -xx;
          scm_t_inum ayy = (yy > 0) ? yy : -yy;
          if (SCM_MOST_POSITIVE_FIXNUM / axx >= ayy)
            return SCM_I_MAKINUM (xx * yy);
#endif
	  else
	    {
	      SCM result = scm_i_inum2big (xx);
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
	return scm_i_from_double (xx * SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (xx * SCM_COMPLEX_REAL (y),
				 xx * SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_make_ratio (scm_product (x, SCM_FRACTION_NUMERATOR (y)),
			       SCM_FRACTION_DENOMINATOR (y));
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  SCM_SWAP (x, y);
	  goto xinum;
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
	  return scm_i_from_double (result);
	}
      else if (SCM_COMPLEXP (y))
	{
	  double z = mpz_get_d (SCM_I_BIG_MPZ (x));
	  scm_remember_upto_here_1 (x);
	  return scm_c_make_rectangular (z * SCM_COMPLEX_REAL (y),
				   z * SCM_COMPLEX_IMAG (y));
	}
      else if (SCM_FRACTIONP (y))
	return scm_i_make_ratio (scm_product (x, SCM_FRACTION_NUMERATOR (y)),
			       SCM_FRACTION_DENOMINATOR (y));
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else if (SCM_REALP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  SCM_SWAP (x, y);
	  goto xinum;
	}
      else if (SCM_BIGP (y))
	{
	  double result = mpz_get_d (SCM_I_BIG_MPZ (y)) * SCM_REAL_VALUE (x);
	  scm_remember_upto_here_1 (y);
	  return scm_i_from_double (result);
	}
      else if (SCM_REALP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) * SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	return scm_c_make_rectangular (SCM_REAL_VALUE (x) * SCM_COMPLEX_REAL (y),
				 SCM_REAL_VALUE (x) * SCM_COMPLEX_IMAG (y));
      else if (SCM_FRACTIONP (y))
	return scm_i_from_double (SCM_REAL_VALUE (x) * scm_i_fraction2double (y));
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else if (SCM_COMPLEXP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  SCM_SWAP (x, y);
	  goto xinum;
	}
      else if (SCM_BIGP (y))
	{
	  double z = mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return scm_c_make_rectangular (z * SCM_COMPLEX_REAL (x),
				   z * SCM_COMPLEX_IMAG (x));
	}
      else if (SCM_REALP (y))
	return scm_c_make_rectangular (SCM_REAL_VALUE (y) * SCM_COMPLEX_REAL (x),
				 SCM_REAL_VALUE (y) * SCM_COMPLEX_IMAG (x));
      else if (SCM_COMPLEXP (y))
	{
	  return scm_c_make_rectangular (SCM_COMPLEX_REAL (x) * SCM_COMPLEX_REAL (y)
				   - SCM_COMPLEX_IMAG (x) * SCM_COMPLEX_IMAG (y),
				   SCM_COMPLEX_REAL (x) * SCM_COMPLEX_IMAG (y)
				   + SCM_COMPLEX_IMAG (x) * SCM_COMPLEX_REAL (y));
	}
      else if (SCM_FRACTIONP (y))
	{
	  double yy = scm_i_fraction2double (y);
	  return scm_c_make_rectangular (yy * SCM_COMPLEX_REAL (x),
				   yy * SCM_COMPLEX_IMAG (x));
	}
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y))
	return scm_i_make_ratio (scm_product (y, SCM_FRACTION_NUMERATOR (x)),
			       SCM_FRACTION_DENOMINATOR (x));
      else if (SCM_BIGP (y))
	return scm_i_make_ratio (scm_product (y, SCM_FRACTION_NUMERATOR (x)),
			       SCM_FRACTION_DENOMINATOR (x));
      else if (SCM_REALP (y))
	return scm_i_from_double (scm_i_fraction2double (x) * SCM_REAL_VALUE (y));
      else if (SCM_COMPLEXP (y))
	{
	  double xx = scm_i_fraction2double (x);
	  return scm_c_make_rectangular (xx * SCM_COMPLEX_REAL (y),
				   xx * SCM_COMPLEX_IMAG (y));
	}
      else if (SCM_FRACTIONP (y))
	/* a/b * c/d = ac / bd */
	return scm_i_make_ratio (scm_product (SCM_FRACTION_NUMERATOR (x),
					    SCM_FRACTION_NUMERATOR (y)),
			       scm_product (SCM_FRACTION_DENOMINATOR (x),
					    SCM_FRACTION_DENOMINATOR (y)));
      else
	SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARGn, s_product);
    }
  else
    SCM_WTA_DISPATCH_2 (g_product, x, y, SCM_ARG1, s_product);
}

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

SCM_PRIMITIVE_GENERIC (scm_i_divide, "/", 0, 2, 1,
                       (SCM x, SCM y, SCM rest),
                       "Divide the first argument by the product of the remaining\n"
                       "arguments.  If called with one argument @var{z1}, 1/@var{z1} is\n"
                       "returned.")
#define FUNC_NAME s_scm_i_divide
{
  while (!scm_is_null (rest))
    { x = scm_divide (x, y);
      y = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_divide (x, y);
}
#undef FUNC_NAME
                       
#define s_divide s_scm_i_divide
#define g_divide g_scm_i_divide

SCM
scm_divide (SCM x, SCM y)
#define FUNC_NAME s_divide
{
  double a;

  if (SCM_UNLIKELY (SCM_UNBNDP (y)))
    {
      if (SCM_UNBNDP (x))
	SCM_WTA_DISPATCH_0 (g_divide, s_divide);
      else if (SCM_I_INUMP (x))
	{
	  scm_t_inum xx = SCM_I_INUM (x);
	  if (xx == 1 || xx == -1)
	    return x;
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	  else if (xx == 0)
	    scm_num_overflow (s_divide);
#endif
	  else
	    return scm_i_make_ratio_already_reduced (SCM_INUM1, x);
	}
      else if (SCM_BIGP (x))
	return scm_i_make_ratio_already_reduced (SCM_INUM1, x);
      else if (SCM_REALP (x))
	{
	  double xx = SCM_REAL_VALUE (x);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (xx == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_i_from_double (1.0 / xx);
	}
      else if (SCM_COMPLEXP (x))
	{
	  double r = SCM_COMPLEX_REAL (x);
	  double i = SCM_COMPLEX_IMAG (x);
	  if (fabs(r) <= fabs(i))
	    {
	      double t = r / i;
	      double d = i * (1.0 + t * t);
	      return scm_c_make_rectangular (t / d, -1.0 / d);
	    }
	  else
	    {
	      double t = i / r;
	      double d = r * (1.0 + t * t);
	      return scm_c_make_rectangular (1.0 / d, -t / d);
	    }
	}
      else if (SCM_FRACTIONP (x))
	return scm_i_make_ratio_already_reduced (SCM_FRACTION_DENOMINATOR (x),
						 SCM_FRACTION_NUMERATOR (x));
      else
	SCM_WTA_DISPATCH_1 (g_divide, x, SCM_ARG1, s_divide);
    }

  if (SCM_LIKELY (SCM_I_INUMP (x)))
    {
      scm_t_inum xx = SCM_I_INUM (x);
      if (SCM_LIKELY (SCM_I_INUMP (y)))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
	  if (yy == 0)
	    {
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	      scm_num_overflow (s_divide);
#else
	      return scm_i_from_double ((double) xx / (double) yy);
#endif
	    }
	  else if (xx % yy != 0)
	    return scm_i_make_ratio (x, y);
	  else
	    {
	      scm_t_inum z = xx / yy;
	      if (SCM_FIXABLE (z))
		return SCM_I_MAKINUM (z);
	      else
		return scm_i_inum2big (z);
	    }
	}
      else if (SCM_BIGP (y))
	return scm_i_make_ratio (x, y);
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
            /* FIXME: Precision may be lost here due to:
               (1) The cast from 'scm_t_inum' to 'double'
               (2) Double rounding */
	    return scm_i_from_double ((double) xx / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  a = xx;
	complex_div: /* y _must_ be a complex number */
	  {
	    double r = SCM_COMPLEX_REAL (y);
	    double i = SCM_COMPLEX_IMAG (y);
	    if (fabs(r) <= fabs(i))
	      {
		double t = r / i;
		double d = i * (1.0 + t * t);
		return scm_c_make_rectangular ((a * t) / d,  -a / d);
	      }
	    else
	      {
		double t = i / r;
		double d = r * (1.0 + t * t);
		return scm_c_make_rectangular (a / d,  -(a * t) / d);
	      }
	  }
	}
      else if (SCM_FRACTIONP (y))
	/* a / b/c = ac / b */
	return scm_i_make_ratio (scm_product (x, SCM_FRACTION_DENOMINATOR (y)),
                                 SCM_FRACTION_NUMERATOR (y));
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else if (SCM_BIGP (x))
    {
      if (SCM_I_INUMP (y))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
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

	      scm_t_inum abs_yy = yy < 0 ? -yy : yy;
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
		return scm_i_make_ratio (x, y);
	    }
	}
      else if (SCM_BIGP (y))
	{
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
            return scm_i_make_ratio (x, y);
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
            /* FIXME: Precision may be lost here due to:
               (1) scm_i_big2dbl (2) Double rounding */
	    return scm_i_from_double (scm_i_big2dbl (x) / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  a = scm_i_big2dbl (x);
	  goto complex_div;
	}
      else if (SCM_FRACTIONP (y))
	return scm_i_make_ratio (scm_product (x, SCM_FRACTION_DENOMINATOR (y)),
                                 SCM_FRACTION_NUMERATOR (y));
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else if (SCM_REALP (x))
    {
      double rx = SCM_REAL_VALUE (x);
      if (SCM_I_INUMP (y))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	  if (yy == 0)
	    scm_num_overflow (s_divide);
	  else
#endif
            /* FIXME: Precision may be lost here due to:
               (1) The cast from 'scm_t_inum' to 'double'
               (2) Double rounding */
	    return scm_i_from_double (rx / (double) yy);
	}
      else if (SCM_BIGP (y))
	{
          /* FIXME: Precision may be lost here due to:
             (1) The conversion from bignum to double
             (2) Double rounding */
	  double dby = mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return scm_i_from_double (rx / dby);
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_i_from_double (rx / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  a = rx;
	  goto complex_div;
	}
      else if (SCM_FRACTIONP (y))
	return scm_i_from_double (rx / scm_i_fraction2double (y));
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else if (SCM_COMPLEXP (x))
    {
      double rx = SCM_COMPLEX_REAL (x);
      double ix = SCM_COMPLEX_IMAG (x);
      if (SCM_I_INUMP (y))
	{
	  scm_t_inum yy = SCM_I_INUM (y);
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	  if (yy == 0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    {
              /* FIXME: Precision may be lost here due to:
                 (1) The conversion from 'scm_t_inum' to double
                 (2) Double rounding */
	      double d = yy;
	      return scm_c_make_rectangular (rx / d, ix / d);
	    }
	}
      else if (SCM_BIGP (y))
	{
          /* FIXME: Precision may be lost here due to:
             (1) The conversion from bignum to double
             (2) Double rounding */
	  double dby = mpz_get_d (SCM_I_BIG_MPZ (y));
	  scm_remember_upto_here_1 (y);
	  return scm_c_make_rectangular (rx / dby, ix / dby);
	}
      else if (SCM_REALP (y))
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_c_make_rectangular (rx / yy, ix / yy);
	}
      else if (SCM_COMPLEXP (y))
	{
	  double ry = SCM_COMPLEX_REAL (y);
	  double iy = SCM_COMPLEX_IMAG (y);
	  if (fabs(ry) <= fabs(iy))
	    {
	      double t = ry / iy;
	      double d = iy * (1.0 + t * t);
	      return scm_c_make_rectangular ((rx * t + ix) / d, (ix * t - rx) / d);
	    }
	  else
	    {
	      double t = iy / ry;
	      double d = ry * (1.0 + t * t);
	      return scm_c_make_rectangular ((rx + ix * t) / d, (ix - rx * t) / d);
	    }
	}
      else if (SCM_FRACTIONP (y))
	{
          /* FIXME: Precision may be lost here due to:
             (1) The conversion from fraction to double
             (2) Double rounding */
	  double yy = scm_i_fraction2double (y);
	  return scm_c_make_rectangular (rx / yy, ix / yy);
	}
      else
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else if (SCM_FRACTIONP (x))
    {
      if (SCM_I_INUMP (y)) 
	{
	  scm_t_inum yy = SCM_I_INUM (y);
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
	  if (yy == 0)
	    scm_num_overflow (s_divide);
	  else
#endif
	    return scm_i_make_ratio (SCM_FRACTION_NUMERATOR (x),
                                     scm_product (SCM_FRACTION_DENOMINATOR (x), y));
	} 
      else if (SCM_BIGP (y)) 
	{
	  return scm_i_make_ratio (SCM_FRACTION_NUMERATOR (x),
                                   scm_product (SCM_FRACTION_DENOMINATOR (x), y));
	} 
      else if (SCM_REALP (y)) 
	{
	  double yy = SCM_REAL_VALUE (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
	  if (yy == 0.0)
	    scm_num_overflow (s_divide);
	  else
#endif
            /* FIXME: Precision may be lost here due to:
               (1) The conversion from fraction to double
               (2) Double rounding */
	    return scm_i_from_double (scm_i_fraction2double (x) / yy);
	}
      else if (SCM_COMPLEXP (y)) 
	{
          /* FIXME: Precision may be lost here due to:
             (1) The conversion from fraction to double
             (2) Double rounding */
	  a = scm_i_fraction2double (x);
	  goto complex_div;
	} 
      else if (SCM_FRACTIONP (y))
	return scm_i_make_ratio (scm_product (SCM_FRACTION_NUMERATOR (x), SCM_FRACTION_DENOMINATOR (y)),
                                 scm_product (SCM_FRACTION_NUMERATOR (y), SCM_FRACTION_DENOMINATOR (x)));
      else 
	SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARGn, s_divide);
    }
  else
    SCM_WTA_DISPATCH_2 (g_divide, x, y, SCM_ARG1, s_divide);
}
#undef FUNC_NAME


double
scm_c_truncate (double x)
{
  return trunc (x);
}

/* scm_c_round is done using floor(x+0.5) to round to nearest and with
   half-way case (ie. when x is an integer plus 0.5) going upwards.
   Then half-way cases are identified and adjusted down if the
   round-upwards didn't give the desired even integer.

   "plus_half == result" identifies a half-way case.  If plus_half, which is
   x + 0.5, is an integer then x must be an integer plus 0.5.

   An odd "result" value is identified with result/2 != floor(result/2).
   This is done with plus_half, since that value is ready for use sooner in
   a pipelined cpu, and we're already requiring plus_half == result.

   Note however that we need to be careful when x is big and already an
   integer.  In that case "x+0.5" may round to an adjacent integer, causing
   us to return such a value, incorrectly.  For instance if the hardware is
   in the usual default nearest-even rounding, then for x = 0x1FFFFFFFFFFFFF
   (ie. 53 one bits) we will have x+0.5 = 0x20000000000000 and that value
   returned.  Or if the hardware is in round-upwards mode, then other bigger
   values like say x == 2^128 will see x+0.5 rounding up to the next higher
   representable value, 2^128+2^76 (or whatever), again incorrect.

   These bad roundings of x+0.5 are avoided by testing at the start whether
   x is already an integer.  If it is then clearly that's the desired result
   already.  And if it's not then the exponent must be small enough to allow
   an 0.5 to be represented, and hence added without a bad rounding.  */

double
scm_c_round (double x)
{
  double plus_half, result;

  if (x == floor (x))
    return x;

  plus_half = x + 0.5;
  result = floor (plus_half);
  /* Adjust so that the rounding is towards even.  */
  return ((plus_half == result && plus_half / 2 != floor (plus_half / 2))
	  ? result - 1
	  : result);
}

SCM_PRIMITIVE_GENERIC (scm_truncate_number, "truncate", 1, 0, 0,
		       (SCM x),
		       "Round the number @var{x} towards zero.")
#define FUNC_NAME s_scm_truncate_number
{
  if (SCM_I_INUMP (x) || SCM_BIGP (x))
    return x;
  else if (SCM_REALP (x))
    return scm_i_from_double (trunc (SCM_REAL_VALUE (x)));
  else if (SCM_FRACTIONP (x))
    return scm_truncate_quotient (SCM_FRACTION_NUMERATOR (x),
				  SCM_FRACTION_DENOMINATOR (x));
  else
    SCM_WTA_DISPATCH_1 (g_scm_truncate_number, x, SCM_ARG1,
			s_scm_truncate_number);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_round_number, "round", 1, 0, 0,
		       (SCM x),
		       "Round the number @var{x} towards the nearest integer. "
		       "When it is exactly halfway between two integers, "
		       "round towards the even one.")
#define FUNC_NAME s_scm_round_number
{
  if (SCM_I_INUMP (x) || SCM_BIGP (x))
    return x;
  else if (SCM_REALP (x))
    return scm_i_from_double (scm_c_round (SCM_REAL_VALUE (x)));
  else if (SCM_FRACTIONP (x))
    return scm_round_quotient (SCM_FRACTION_NUMERATOR (x),
			       SCM_FRACTION_DENOMINATOR (x));
  else
    SCM_WTA_DISPATCH_1 (g_scm_round_number, x, SCM_ARG1,
			s_scm_round_number);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_floor, "floor", 1, 0, 0,
		       (SCM x),
		       "Round the number @var{x} towards minus infinity.")
#define FUNC_NAME s_scm_floor
{
  if (SCM_I_INUMP (x) || SCM_BIGP (x))
    return x;
  else if (SCM_REALP (x))
    return scm_i_from_double (floor (SCM_REAL_VALUE (x)));
  else if (SCM_FRACTIONP (x))
    return scm_floor_quotient (SCM_FRACTION_NUMERATOR (x),
			       SCM_FRACTION_DENOMINATOR (x));
  else
    SCM_WTA_DISPATCH_1 (g_scm_floor, x, 1, s_scm_floor);
}  
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_ceiling, "ceiling", 1, 0, 0,
		       (SCM x),
		       "Round the number @var{x} towards infinity.")
#define FUNC_NAME s_scm_ceiling
{
  if (SCM_I_INUMP (x) || SCM_BIGP (x))
    return x;
  else if (SCM_REALP (x))
    return scm_i_from_double (ceil (SCM_REAL_VALUE (x)));
  else if (SCM_FRACTIONP (x))
    return scm_ceiling_quotient (SCM_FRACTION_NUMERATOR (x),
				 SCM_FRACTION_DENOMINATOR (x));
  else
    SCM_WTA_DISPATCH_1 (g_scm_ceiling, x, 1, s_scm_ceiling);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_expt, "expt", 2, 0, 0,
		       (SCM x, SCM y),
		       "Return @var{x} raised to the power of @var{y}.")
#define FUNC_NAME s_scm_expt
{
  if (scm_is_integer (y))
    {
      if (scm_is_true (scm_exact_p (y)))
	return scm_integer_expt (x, y);
      else
	{
	  /* Here we handle the case where the exponent is an inexact
	     integer.  We make the exponent exact in order to use
	     scm_integer_expt, and thus avoid the spurious imaginary
	     parts that may result from round-off errors in the general
	     e^(y log x) method below (for example when squaring a large
	     negative number).  In this case, we must return an inexact
	     result for correctness.  We also make the base inexact so
	     that scm_integer_expt will use fast inexact arithmetic
	     internally.  Note that making the base inexact is not
	     sufficient to guarantee an inexact result, because
	     scm_integer_expt will return an exact 1 when the exponent
	     is 0, even if the base is inexact. */
	  return scm_exact_to_inexact
	    (scm_integer_expt (scm_exact_to_inexact (x),
			       scm_inexact_to_exact (y)));
	}
    }
  else if (scm_is_real (x) && scm_is_real (y) && scm_to_double (x) >= 0.0)
    {
      return scm_i_from_double (pow (scm_to_double (x), scm_to_double (y)));
    }
  else if (scm_is_complex (x) && scm_is_complex (y))
    return scm_exp (scm_product (scm_log (x), y));
  else if (scm_is_complex (x))
    SCM_WTA_DISPATCH_2 (g_scm_expt, x, y, SCM_ARG2, s_scm_expt);
  else
    SCM_WTA_DISPATCH_2 (g_scm_expt, x, y, SCM_ARG1, s_scm_expt);
}
#undef FUNC_NAME

/* sin/cos/tan/asin/acos/atan
   sinh/cosh/tanh/asinh/acosh/atanh
   Derived from "Transcen.scm", Complex trancendental functions for SCM.
   Written by Jerry D. Hedden, (C) FSF.
   See the file `COPYING' for terms applying to this program. */

SCM_PRIMITIVE_GENERIC (scm_sin, "sin", 1, 0, 0,
                       (SCM z),
                       "Compute the sine of @var{z}.")
#define FUNC_NAME s_scm_sin
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return z;  /* sin(exact0) = exact0 */
  else if (scm_is_real (z))
    return scm_i_from_double (sin (scm_to_double (z)));
  else if (SCM_COMPLEXP (z))
    { double x, y;
      x = SCM_COMPLEX_REAL (z);
      y = SCM_COMPLEX_IMAG (z);
      return scm_c_make_rectangular (sin (x) * cosh (y),
                                     cos (x) * sinh (y));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_sin, z, 1, s_scm_sin);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_cos, "cos", 1, 0, 0,
                       (SCM z),
                       "Compute the cosine of @var{z}.")
#define FUNC_NAME s_scm_cos
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return SCM_INUM1;  /* cos(exact0) = exact1 */
  else if (scm_is_real (z))
    return scm_i_from_double (cos (scm_to_double (z)));
  else if (SCM_COMPLEXP (z))
    { double x, y;
      x = SCM_COMPLEX_REAL (z);
      y = SCM_COMPLEX_IMAG (z);
      return scm_c_make_rectangular (cos (x) * cosh (y),
                                     -sin (x) * sinh (y));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_cos, z, 1, s_scm_cos);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_tan, "tan", 1, 0, 0,
                       (SCM z),
                       "Compute the tangent of @var{z}.")
#define FUNC_NAME s_scm_tan
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return z;  /* tan(exact0) = exact0 */
  else if (scm_is_real (z))
    return scm_i_from_double (tan (scm_to_double (z)));
  else if (SCM_COMPLEXP (z))
    { double x, y, w;
      x = 2.0 * SCM_COMPLEX_REAL (z);
      y = 2.0 * SCM_COMPLEX_IMAG (z);
      w = cos (x) + cosh (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
      if (w == 0.0)
        scm_num_overflow (s_scm_tan);
#endif
      return scm_c_make_rectangular (sin (x) / w, sinh (y) / w);
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_tan, z, 1, s_scm_tan);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_sinh, "sinh", 1, 0, 0,
                       (SCM z),
                       "Compute the hyperbolic sine of @var{z}.")
#define FUNC_NAME s_scm_sinh
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return z;  /* sinh(exact0) = exact0 */
  else if (scm_is_real (z))
    return scm_i_from_double (sinh (scm_to_double (z)));
  else if (SCM_COMPLEXP (z))
    { double x, y;
      x = SCM_COMPLEX_REAL (z);
      y = SCM_COMPLEX_IMAG (z);
      return scm_c_make_rectangular (sinh (x) * cos (y),
                                     cosh (x) * sin (y));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_sinh, z, 1, s_scm_sinh);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_cosh, "cosh", 1, 0, 0,
                       (SCM z),
                       "Compute the hyperbolic cosine of @var{z}.")
#define FUNC_NAME s_scm_cosh
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return SCM_INUM1;  /* cosh(exact0) = exact1 */
  else if (scm_is_real (z))
    return scm_i_from_double (cosh (scm_to_double (z)));
  else if (SCM_COMPLEXP (z))
    { double x, y;
      x = SCM_COMPLEX_REAL (z);
      y = SCM_COMPLEX_IMAG (z);
      return scm_c_make_rectangular (cosh (x) * cos (y),
                                     sinh (x) * sin (y));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_cosh, z, 1, s_scm_cosh);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_tanh, "tanh", 1, 0, 0,
                       (SCM z),
                       "Compute the hyperbolic tangent of @var{z}.")
#define FUNC_NAME s_scm_tanh
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return z;  /* tanh(exact0) = exact0 */
  else if (scm_is_real (z))
    return scm_i_from_double (tanh (scm_to_double (z)));
  else if (SCM_COMPLEXP (z))
    { double x, y, w;
      x = 2.0 * SCM_COMPLEX_REAL (z);
      y = 2.0 * SCM_COMPLEX_IMAG (z);
      w = cosh (x) + cos (y);
#ifndef ALLOW_DIVIDE_BY_ZERO
      if (w == 0.0)
        scm_num_overflow (s_scm_tanh);
#endif
      return scm_c_make_rectangular (sinh (x) / w, sin (y) / w);
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_tanh, z, 1, s_scm_tanh);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_asin, "asin", 1, 0, 0,
                       (SCM z),
                       "Compute the arc sine of @var{z}.")
#define FUNC_NAME s_scm_asin
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return z;  /* asin(exact0) = exact0 */
  else if (scm_is_real (z))
    {
      double w = scm_to_double (z);
      if (w >= -1.0 && w <= 1.0)
        return scm_i_from_double (asin (w));
      else
        return scm_product (scm_c_make_rectangular (0, -1),
                            scm_sys_asinh (scm_c_make_rectangular (0, w)));
    }
  else if (SCM_COMPLEXP (z))
    { double x, y;
      x = SCM_COMPLEX_REAL (z);
      y = SCM_COMPLEX_IMAG (z);
      return scm_product (scm_c_make_rectangular (0, -1),
                          scm_sys_asinh (scm_c_make_rectangular (-y, x)));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_asin, z, 1, s_scm_asin);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_acos, "acos", 1, 0, 0,
                       (SCM z),
                       "Compute the arc cosine of @var{z}.")
#define FUNC_NAME s_scm_acos
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM1)))
    return SCM_INUM0;  /* acos(exact1) = exact0 */
  else if (scm_is_real (z))
    {
      double w = scm_to_double (z);
      if (w >= -1.0 && w <= 1.0)
        return scm_i_from_double (acos (w));
      else
        return scm_sum (scm_i_from_double (acos (0.0)),
                        scm_product (scm_c_make_rectangular (0, 1),
                                     scm_sys_asinh (scm_c_make_rectangular (0, w))));
    }
  else if (SCM_COMPLEXP (z))
    { double x, y;
      x = SCM_COMPLEX_REAL (z);
      y = SCM_COMPLEX_IMAG (z);
      return scm_sum (scm_i_from_double (acos (0.0)),
                      scm_product (scm_c_make_rectangular (0, 1),
                                   scm_sys_asinh (scm_c_make_rectangular (-y, x))));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_acos, z, 1, s_scm_acos);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_atan, "atan", 1, 1, 0,
                       (SCM z, SCM y),
                       "With one argument, compute the arc tangent of @var{z}.\n"
                       "If @var{y} is present, compute the arc tangent of @var{z}/@var{y},\n"
                       "using the sign of @var{z} and @var{y} to determine the quadrant.")
#define FUNC_NAME s_scm_atan
{
  if (SCM_UNBNDP (y))
    {
      if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
	return z;  /* atan(exact0) = exact0 */
      else if (scm_is_real (z))
        return scm_i_from_double (atan (scm_to_double (z)));
      else if (SCM_COMPLEXP (z))
        {
          double v, w;
          v = SCM_COMPLEX_REAL (z);
          w = SCM_COMPLEX_IMAG (z);
          return scm_divide (scm_log (scm_divide (scm_c_make_rectangular (v, w - 1.0),
                                                  scm_c_make_rectangular (v, w + 1.0))),
                             scm_c_make_rectangular (0, 2));
        }
      else
        SCM_WTA_DISPATCH_1 (g_scm_atan, z, SCM_ARG1, s_scm_atan);
    }
  else if (scm_is_real (z))
    {
      if (scm_is_real (y))
        return scm_i_from_double (atan2 (scm_to_double (z), scm_to_double (y)));
      else
        SCM_WTA_DISPATCH_2 (g_scm_atan, z, y, SCM_ARG2, s_scm_atan);
    }
  else
    SCM_WTA_DISPATCH_2 (g_scm_atan, z, y, SCM_ARG1, s_scm_atan);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_sys_asinh, "asinh", 1, 0, 0,
                       (SCM z),
                       "Compute the inverse hyperbolic sine of @var{z}.")
#define FUNC_NAME s_scm_sys_asinh
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return z;  /* asinh(exact0) = exact0 */
  else if (scm_is_real (z))
    return scm_i_from_double (asinh (scm_to_double (z)));
  else if (scm_is_number (z))
    return scm_log (scm_sum (z,
                             scm_sqrt (scm_sum (scm_product (z, z),
                                                SCM_INUM1))));
  else
    SCM_WTA_DISPATCH_1 (g_scm_sys_asinh, z, 1, s_scm_sys_asinh);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_sys_acosh, "acosh", 1, 0, 0,
                       (SCM z),
                       "Compute the inverse hyperbolic cosine of @var{z}.")
#define FUNC_NAME s_scm_sys_acosh
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM1)))
    return SCM_INUM0;  /* acosh(exact1) = exact0 */
  else if (scm_is_real (z) && scm_to_double (z) >= 1.0)
    return scm_i_from_double (acosh (scm_to_double (z)));
  else if (scm_is_number (z))
    return scm_log (scm_sum (z,
                             scm_sqrt (scm_difference (scm_product (z, z),
                                                       SCM_INUM1))));
  else
    SCM_WTA_DISPATCH_1 (g_scm_sys_acosh, z, 1, s_scm_sys_acosh);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_sys_atanh, "atanh", 1, 0, 0,
                       (SCM z),
                       "Compute the inverse hyperbolic tangent of @var{z}.")
#define FUNC_NAME s_scm_sys_atanh
{
  if (SCM_UNLIKELY (scm_is_eq (z, SCM_INUM0)))
    return z;  /* atanh(exact0) = exact0 */
  else if (scm_is_real (z) && scm_to_double (z) >= -1.0 && scm_to_double (z) <= 1.0)
    return scm_i_from_double (atanh (scm_to_double (z)));
  else if (scm_is_number (z))
    return scm_divide (scm_log (scm_divide (scm_sum (SCM_INUM1, z),
                                            scm_difference (SCM_INUM1, z))),
                       SCM_I_MAKINUM (2));
  else
    SCM_WTA_DISPATCH_1 (g_scm_sys_atanh, z, 1, s_scm_sys_atanh);
}
#undef FUNC_NAME

SCM
scm_c_make_rectangular (double re, double im)
{
  SCM z;

  z = PTR2SCM (scm_gc_malloc_pointerless (sizeof (scm_t_complex),
					  "complex"));
  SCM_SET_CELL_TYPE (z, scm_tc16_complex);
  SCM_COMPLEX_REAL (z) = re;
  SCM_COMPLEX_IMAG (z) = im;
  return z;
}

SCM_DEFINE (scm_make_rectangular, "make-rectangular", 2, 0, 0,
            (SCM real_part, SCM imaginary_part),
	    "Return a complex number constructed of the given @var{real_part} "
	    "and @var{imaginary_part} parts.")
#define FUNC_NAME s_scm_make_rectangular
{
  SCM_ASSERT_TYPE (scm_is_real (real_part), real_part,
                   SCM_ARG1, FUNC_NAME, "real");
  SCM_ASSERT_TYPE (scm_is_real (imaginary_part), imaginary_part,
                   SCM_ARG2, FUNC_NAME, "real");

  /* Return a real if and only if the imaginary_part is an _exact_ 0 */
  if (scm_is_eq (imaginary_part, SCM_INUM0))
    return real_part;
  else
    return scm_c_make_rectangular (scm_to_double (real_part),
				   scm_to_double (imaginary_part));
}
#undef FUNC_NAME

SCM
scm_c_make_polar (double mag, double ang)
{
  double s, c;

  /* The sincos(3) function is undocumented an broken on Tru64.  Thus we only
     use it on Glibc-based systems that have it (it's a GNU extension).  See
     http://lists.gnu.org/archive/html/guile-user/2009-04/msg00033.html for
     details.  */
#if (defined HAVE_SINCOS) && (defined __GLIBC__) && (defined _GNU_SOURCE)
  sincos (ang, &s, &c);
#else
  s = sin (ang);
  c = cos (ang);
#endif

  /* If s and c are NaNs, this indicates that the angle is a NaN,
     infinite, or perhaps simply too large to determine its value
     mod 2*pi.  However, we know something that the floating-point
     implementation doesn't know:  We know that s and c are finite.
     Therefore, if the magnitude is zero, return a complex zero.

     The reason we check for the NaNs instead of using this case
     whenever mag == 0.0 is because when the angle is known, we'd
     like to return the correct kind of non-real complex zero:
     +0.0+0.0i, -0.0+0.0i, -0.0-0.0i, or +0.0-0.0i, depending
     on which quadrant the angle is in.
  */
  if (SCM_UNLIKELY (isnan(s)) && isnan(c) && (mag == 0.0))
    return scm_c_make_rectangular (0.0, 0.0);
  else
    return scm_c_make_rectangular (mag * c, mag * s);
}

SCM_DEFINE (scm_make_polar, "make-polar", 2, 0, 0,
            (SCM mag, SCM ang),
	    "Return the complex number @var{mag} * e^(i * @var{ang}).")
#define FUNC_NAME s_scm_make_polar
{
  SCM_ASSERT_TYPE (scm_is_real (mag), mag, SCM_ARG1, FUNC_NAME, "real");
  SCM_ASSERT_TYPE (scm_is_real (ang), ang, SCM_ARG2, FUNC_NAME, "real");

  /* If mag is exact0, return exact0 */
  if (scm_is_eq (mag, SCM_INUM0))
    return SCM_INUM0;
  /* Return a real if ang is exact0 */
  else if (scm_is_eq (ang, SCM_INUM0))
    return mag;
  else
    return scm_c_make_polar (scm_to_double (mag), scm_to_double (ang));
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_real_part, "real-part", 1, 0, 0,
		       (SCM z),
		       "Return the real part of the number @var{z}.")
#define FUNC_NAME s_scm_real_part
{
  if (SCM_COMPLEXP (z))
    return scm_i_from_double (SCM_COMPLEX_REAL (z));
  else if (SCM_I_INUMP (z) || SCM_BIGP (z) || SCM_REALP (z) || SCM_FRACTIONP (z))
    return z;
  else
    SCM_WTA_DISPATCH_1 (g_scm_real_part, z, SCM_ARG1, s_scm_real_part);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_imag_part, "imag-part", 1, 0, 0,
		       (SCM z),
		       "Return the imaginary part of the number @var{z}.")
#define FUNC_NAME s_scm_imag_part
{
  if (SCM_COMPLEXP (z))
    return scm_i_from_double (SCM_COMPLEX_IMAG (z));
  else if (SCM_I_INUMP (z) || SCM_REALP (z) || SCM_BIGP (z) || SCM_FRACTIONP (z))
    return SCM_INUM0;
  else
    SCM_WTA_DISPATCH_1 (g_scm_imag_part, z, SCM_ARG1, s_scm_imag_part);
}
#undef FUNC_NAME

SCM_PRIMITIVE_GENERIC (scm_numerator, "numerator", 1, 0, 0,
		       (SCM z),
		       "Return the numerator of the number @var{z}.")
#define FUNC_NAME s_scm_numerator
{
  if (SCM_I_INUMP (z) || SCM_BIGP (z))
    return z;
  else if (SCM_FRACTIONP (z))
    return SCM_FRACTION_NUMERATOR (z);
  else if (SCM_REALP (z))
    {
      double zz = SCM_REAL_VALUE (z);
      if (zz == floor (zz))
        /* Handle -0.0 and infinities in accordance with R6RS
           flnumerator, and optimize handling of integers. */
        return z;
      else
        return scm_exact_to_inexact (scm_numerator (scm_inexact_to_exact (z)));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_numerator, z, SCM_ARG1, s_scm_numerator);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_denominator, "denominator", 1, 0, 0,
		       (SCM z),
		       "Return the denominator of the number @var{z}.")
#define FUNC_NAME s_scm_denominator
{
  if (SCM_I_INUMP (z) || SCM_BIGP (z)) 
    return SCM_INUM1;
  else if (SCM_FRACTIONP (z))
    return SCM_FRACTION_DENOMINATOR (z);
  else if (SCM_REALP (z))
    {
      double zz = SCM_REAL_VALUE (z);
      if (zz == floor (zz))
        /* Handle infinities in accordance with R6RS fldenominator, and
           optimize handling of integers. */
        return scm_i_from_double (1.0);
      else
        return scm_exact_to_inexact (scm_denominator (scm_inexact_to_exact (z)));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_denominator, z, SCM_ARG1, s_scm_denominator);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_magnitude, "magnitude", 1, 0, 0,
		       (SCM z),
	"Return the magnitude of the number @var{z}. This is the same as\n"
	"@code{abs} for real arguments, but also allows complex numbers.")
#define FUNC_NAME s_scm_magnitude
{
  if (SCM_I_INUMP (z))
    {
      scm_t_inum zz = SCM_I_INUM (z);
      if (zz >= 0)
	return z;
      else if (SCM_POSFIXABLE (-zz))
	return SCM_I_MAKINUM (-zz);
      else
	return scm_i_inum2big (-zz);
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
    return scm_i_from_double (fabs (SCM_REAL_VALUE (z)));
  else if (SCM_COMPLEXP (z))
    return scm_i_from_double (hypot (SCM_COMPLEX_REAL (z), SCM_COMPLEX_IMAG (z)));
  else if (SCM_FRACTIONP (z))
    {
      if (scm_is_false (scm_negative_p (SCM_FRACTION_NUMERATOR (z))))
	return z;
      return scm_i_make_ratio_already_reduced
	(scm_difference (SCM_FRACTION_NUMERATOR (z), SCM_UNDEFINED),
	 SCM_FRACTION_DENOMINATOR (z));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_magnitude, z, SCM_ARG1, s_scm_magnitude);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_angle, "angle", 1, 0, 0,
		       (SCM z),
		       "Return the angle of the complex number @var{z}.")
#define FUNC_NAME s_scm_angle
{
  /* atan(0,-1) is pi and it'd be possible to have that as a constant like
     flo0 to save allocating a new flonum with scm_i_from_double each time.
     But if atan2 follows the floating point rounding mode, then the value
     is not a constant.  Maybe it'd be close enough though.  */
  if (SCM_I_INUMP (z))
    {
      if (SCM_I_INUM (z) >= 0)
        return flo0;
      else
	return scm_i_from_double (atan2 (0.0, -1.0));
    }
  else if (SCM_BIGP (z))
    {
      int sgn = mpz_sgn (SCM_I_BIG_MPZ (z));
      scm_remember_upto_here_1 (z);
      if (sgn < 0)
	return scm_i_from_double (atan2 (0.0, -1.0));
      else
        return flo0;
    }
  else if (SCM_REALP (z))
    {
      double x = SCM_REAL_VALUE (z);
      if (copysign (1.0, x) > 0.0)
        return flo0;
      else
        return scm_i_from_double (atan2 (0.0, -1.0));
    }
  else if (SCM_COMPLEXP (z))
    return scm_i_from_double (atan2 (SCM_COMPLEX_IMAG (z), SCM_COMPLEX_REAL (z)));
  else if (SCM_FRACTIONP (z))
    {
      if (scm_is_false (scm_negative_p (SCM_FRACTION_NUMERATOR (z))))
	return flo0;
      else return scm_i_from_double (atan2 (0.0, -1.0));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_angle, z, SCM_ARG1, s_scm_angle);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_exact_to_inexact, "exact->inexact", 1, 0, 0,
		       (SCM z),
	"Convert the number @var{z} to its inexact representation.\n")
#define FUNC_NAME s_scm_exact_to_inexact
{
  if (SCM_I_INUMP (z))
    return scm_i_from_double ((double) SCM_I_INUM (z));
  else if (SCM_BIGP (z))
    return scm_i_from_double (scm_i_big2dbl (z));
  else if (SCM_FRACTIONP (z))
    return scm_i_from_double (scm_i_fraction2double (z));
  else if (SCM_INEXACTP (z))
    return z;
  else
    SCM_WTA_DISPATCH_1 (g_scm_exact_to_inexact, z, 1, s_scm_exact_to_inexact);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_inexact_to_exact, "inexact->exact", 1, 0, 0, 
		       (SCM z),
	"Return an exact number that is numerically closest to @var{z}.")
#define FUNC_NAME s_scm_inexact_to_exact
{
  if (SCM_I_INUMP (z) || SCM_BIGP (z) || SCM_FRACTIONP (z))
    return z;
  else
    {
      double val;

      if (SCM_REALP (z))
	val = SCM_REAL_VALUE (z);
      else if (SCM_COMPLEXP (z) && SCM_COMPLEX_IMAG (z) == 0.0)
	val = SCM_COMPLEX_REAL (z);
      else
	SCM_WTA_DISPATCH_1 (g_scm_inexact_to_exact, z, 1, s_scm_inexact_to_exact);

      if (!SCM_LIKELY (isfinite (val)))
	SCM_OUT_OF_RANGE (1, z);
      else if (val == 0.0)
        return SCM_INUM0;
      else
	{
          int expon;
          SCM numerator;

          numerator = scm_i_dbl2big (ldexp (frexp (val, &expon),
                                            DBL_MANT_DIG));
          expon -= DBL_MANT_DIG;
          if (expon < 0)
            {
              int shift = mpz_scan1 (SCM_I_BIG_MPZ (numerator), 0);

              if (shift > -expon)
                shift = -expon;
              mpz_fdiv_q_2exp (SCM_I_BIG_MPZ (numerator),
                               SCM_I_BIG_MPZ (numerator),
                               shift);
              expon += shift;
            }
          numerator = scm_i_normbig (numerator);
          if (expon < 0)
            return scm_i_make_ratio_already_reduced
              (numerator, left_shift_exact_integer (SCM_INUM1, -expon));
          else if (expon > 0)
            return left_shift_exact_integer (numerator, expon);
          else
            return numerator;
	}
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_rationalize, "rationalize", 2, 0, 0, 
            (SCM x, SCM eps),
	    "Returns the @emph{simplest} rational number differing\n"
	    "from @var{x} by no more than @var{eps}.\n"
	    "\n"
	    "As required by @acronym{R5RS}, @code{rationalize} only returns an\n"
	    "exact result when both its arguments are exact.  Thus, you might need\n"
	    "to use @code{inexact->exact} on the arguments.\n"
	    "\n"
	    "@lisp\n"
	    "(rationalize (inexact->exact 1.2) 1/100)\n"
	    "@result{} 6/5\n"
	    "@end lisp")
#define FUNC_NAME s_scm_rationalize
{
  SCM_ASSERT_TYPE (scm_is_real (x), x, SCM_ARG1, FUNC_NAME, "real");
  SCM_ASSERT_TYPE (scm_is_real (eps), eps, SCM_ARG2, FUNC_NAME, "real");

  if (SCM_UNLIKELY (!scm_is_exact (eps) || !scm_is_exact (x)))
    {
      if (SCM_UNLIKELY (scm_is_false (scm_finite_p (eps))))
        {
          if (scm_is_false (scm_nan_p (eps)) && scm_is_true (scm_finite_p (x)))
            return flo0;
          else
            return scm_nan ();
        }
      else if (SCM_UNLIKELY (scm_is_false (scm_finite_p (x))))
        return x;
      else
        return scm_exact_to_inexact
          (scm_rationalize (scm_inexact_to_exact (x),
                            scm_inexact_to_exact (eps)));
    }
  else
    {
      /* X and EPS are exact rationals.

         The code that follows is equivalent to the following Scheme code:

         (define (exact-rationalize x eps)
           (let ((n1  (if (negative? x) -1 1))
                 (x   (abs x))
                 (eps (abs eps)))
             (let ((lo (- x eps))
                   (hi (+ x eps)))
               (if (<= lo 0)
                   0
                   (let loop ((nlo (numerator lo)) (dlo (denominator lo))
                              (nhi (numerator hi)) (dhi (denominator hi))
                              (n1 n1) (d1 0) (n2 0) (d2 1))
                     (let-values (((qlo rlo) (floor/ nlo dlo))
                                  ((qhi rhi) (floor/ nhi dhi)))
                       (let ((n0 (+ n2 (* n1 qlo)))
                             (d0 (+ d2 (* d1 qlo))))
                         (cond ((zero? rlo) (/ n0 d0))
                               ((< qlo qhi) (/ (+ n0 n1) (+ d0 d1)))
                               (else (loop dhi rhi dlo rlo n0 d0 n1 d1))))))))))
      */

      int n1_init = 1;
      SCM lo, hi;

      eps = scm_abs (eps);
      if (scm_is_true (scm_negative_p (x)))
        {
          n1_init = -1;
          x = scm_difference (x, SCM_UNDEFINED);
        }

      /* X and EPS are non-negative exact rationals. */

      lo = scm_difference (x, eps);
      hi = scm_sum (x, eps);

      if (scm_is_false (scm_positive_p (lo)))
        /* If zero is included in the interval, return it.
           It is the simplest rational of all. */
        return SCM_INUM0;
      else
        {
          SCM result;
          mpz_t n0, d0, n1, d1, n2, d2;
          mpz_t nlo, dlo, nhi, dhi;
          mpz_t qlo, rlo, qhi, rhi;

          /* LO and HI are positive exact rationals. */

          /* Our approach here follows the method described by Alan
             Bawden in a message entitled "(rationalize x y)" on the
             rrrs-authors mailing list, dated 16 Feb 1988 14:08:28 EST:

             http://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1988/msg00063.html

             In brief, we compute the continued fractions of the two
             endpoints of the interval (LO and HI).  The continued
             fraction of the result consists of the common prefix of the
             continued fractions of LO and HI, plus one final term.  The
             final term of the result is the smallest integer contained
             in the interval between the remainders of LO and HI after
             the common prefix has been removed.

             The following code lazily computes the continued fraction
             representations of LO and HI, and simultaneously converts
             the continued fraction of the result into a rational
             number.  We use MPZ functions directly to avoid type
             dispatch and GC allocation during the loop. */

          mpz_inits (n0, d0, n1, d1, n2, d2,
                     nlo, dlo, nhi, dhi,
                     qlo, rlo, qhi, rhi,
                     NULL);

          /* The variables N1, D1, N2 and D2 are used to compute the
             resulting rational from its continued fraction.  At each
             step, N2/D2 and N1/D1 are the last two convergents.  They
             are normally initialized to 0/1 and 1/0, respectively.
             However, if we negated X then we must negate the result as
             well, and we do that by initializing N1/D1 to -1/0. */
          mpz_set_si (n1, n1_init);
          mpz_set_ui (d1, 0);
          mpz_set_ui (n2, 0);
          mpz_set_ui (d2, 1);

          /* The variables NLO, DLO, NHI, and DHI are used to lazily
             compute the continued fraction representations of LO and HI
             using Euclid's algorithm.  Initially, NLO/DLO == LO and
             NHI/DHI == HI. */
          scm_to_mpz (scm_numerator   (lo), nlo);
          scm_to_mpz (scm_denominator (lo), dlo);
          scm_to_mpz (scm_numerator   (hi), nhi);
          scm_to_mpz (scm_denominator (hi), dhi);

          /* As long as we're using exact arithmetic, the following loop
             is guaranteed to terminate. */
          for (;;)
            {
              /* Compute the next terms (QLO and QHI) of the continued
                 fractions of LO and HI. */
              mpz_fdiv_qr (qlo, rlo, nlo, dlo);  /* QLO <-- floor (NLO/DLO), RLO <-- NLO - QLO * DLO */
              mpz_fdiv_qr (qhi, rhi, nhi, dhi);  /* QHI <-- floor (NHI/DHI), RHI <-- NHI - QHI * DHI */

              /* The next term of the result will be either QLO or
                 QLO+1.  Here we compute the next convergent of the
                 result based on the assumption that QLO is the next
                 term.  If that turns out to be wrong, we'll adjust
                 these later by adding N1 to N0 and D1 to D0. */
              mpz_set (n0, n2); mpz_addmul (n0, n1, qlo);  /* N0 <-- N2 + (QLO * N1) */
              mpz_set (d0, d2); mpz_addmul (d0, d1, qlo);  /* D0 <-- D2 + (QLO * D1) */

              /* We stop iterating when an integer is contained in the
                 interval between the remainders NLO/DLO and NHI/DHI.
                 There are two cases to consider: either NLO/DLO == QLO
                 is an integer (indicated by RLO == 0), or QLO < QHI. */
              if (mpz_sgn (rlo) == 0 || mpz_cmp (qlo, qhi) != 0)
                break;

              /* Efficiently shuffle variables around for the next
                 iteration.  First we shift the recent convergents. */
              mpz_swap (n2, n1); mpz_swap (n1, n0);      /* N2 <-- N1 <-- N0 */
              mpz_swap (d2, d1); mpz_swap (d1, d0);      /* D2 <-- D1 <-- D0 */

              /* The following shuffling is a bit confusing, so some
                 explanation is in order.  Conceptually, we're doing a
                 couple of things here.  After substracting the floor of
                 NLO/DLO, the remainder is RLO/DLO.  The rest of the
                 continued fraction will represent the remainder's
                 reciprocal DLO/RLO.  Similarly for the HI endpoint.
                 So in the next iteration, the new endpoints will be
                 DLO/RLO and DHI/RHI.  However, when we take the
                 reciprocals of these endpoints, their order is
                 switched.  So in summary, we want NLO/DLO <-- DHI/RHI
                 and NHI/DHI <-- DLO/RLO. */
              mpz_swap (nlo, dhi); mpz_swap (dhi, rlo); /* NLO <-- DHI <-- RLO */
              mpz_swap (nhi, dlo); mpz_swap (dlo, rhi); /* NHI <-- DLO <-- RHI */
            }

          /* There is now an integer in the interval [NLO/DLO NHI/DHI].
             The last term of the result will be the smallest integer in
             that interval, which is ceiling(NLO/DLO).  We have already
             computed floor(NLO/DLO) in QLO, so now we adjust QLO to be
             equal to the ceiling.  */
          if (mpz_sgn (rlo) != 0)
            {
              /* If RLO is non-zero, then NLO/DLO is not an integer and
                 the next term will be QLO+1.  QLO was used in the
                 computation of N0 and D0 above.  Here we adjust N0 and
                 D0 to be based on QLO+1 instead of QLO.  */
              mpz_add (n0, n0, n1);  /* N0 <-- N0 + N1 */
              mpz_add (d0, d0, d1);  /* D0 <-- D0 + D1 */
            }

          /* The simplest rational in the interval is N0/D0 */
          result = scm_i_make_ratio_already_reduced (scm_from_mpz (n0),
                                                     scm_from_mpz (d0));
          mpz_clears (n0, d0, n1, d1, n2, d2,
                      nlo, dlo, nhi, dhi,
                      qlo, rlo, qhi, rhi,
                      NULL);
          return result;
        }
    }
}
#undef FUNC_NAME

/* conversion functions */

int
scm_is_integer (SCM val)
{
  return scm_is_true (scm_integer_p (val));
}

int
scm_is_exact_integer (SCM val)
{
  return scm_is_true (scm_exact_integer_p (val));
}

int
scm_is_signed_integer (SCM val, scm_t_intmax min, scm_t_intmax max)
{
  if (SCM_I_INUMP (val))
    {
      scm_t_signed_bits n = SCM_I_INUM (val);
      return n >= min && n <= max;
    }
  else if (SCM_BIGP (val))
    {
      if (min >= SCM_MOST_NEGATIVE_FIXNUM && max <= SCM_MOST_POSITIVE_FIXNUM)
	return 0;
      else if (min >= LONG_MIN && max <= LONG_MAX)
	{
	  if (mpz_fits_slong_p (SCM_I_BIG_MPZ (val)))
	    {
	      long n = mpz_get_si (SCM_I_BIG_MPZ (val));
	      return n >= min && n <= max;
	    }
	  else
	    return 0;
	}
      else
	{
	  scm_t_intmax n;
	  size_t count;

	  if (mpz_sizeinbase (SCM_I_BIG_MPZ (val), 2) 
	      > CHAR_BIT*sizeof (scm_t_uintmax))
	    return 0;
	  
	  mpz_export (&n, &count, 1, sizeof (scm_t_uintmax), 0, 0,
		      SCM_I_BIG_MPZ (val));

	  if (mpz_sgn (SCM_I_BIG_MPZ (val)) >= 0)
	    {
	      if (n < 0)
		return 0;
	    }
	  else
	    {
	      n = -n;
	      if (n >= 0)
		return 0;
	    }

	  return n >= min && n <= max;
	}
    }
  else
    return 0;
}

int
scm_is_unsigned_integer (SCM val, scm_t_uintmax min, scm_t_uintmax max)
{
  if (SCM_I_INUMP (val))
    {
      scm_t_signed_bits n = SCM_I_INUM (val);
      return n >= 0 && ((scm_t_uintmax)n) >= min && ((scm_t_uintmax)n) <= max;
    }
  else if (SCM_BIGP (val))
    {
      if (max <= SCM_MOST_POSITIVE_FIXNUM)
	return 0;
      else if (max <= ULONG_MAX)
	{
	  if (mpz_fits_ulong_p (SCM_I_BIG_MPZ (val)))
	    {
	      unsigned long n = mpz_get_ui (SCM_I_BIG_MPZ (val));
	      return n >= min && n <= max;
	    }
	  else
	    return 0;
	}
      else
	{
	  scm_t_uintmax n;
	  size_t count;

	  if (mpz_sgn (SCM_I_BIG_MPZ (val)) < 0)
	    return 0;

	  if (mpz_sizeinbase (SCM_I_BIG_MPZ (val), 2)
	      > CHAR_BIT*sizeof (scm_t_uintmax))
	    return 0;
	  
	  mpz_export (&n, &count, 1, sizeof (scm_t_uintmax), 0, 0,
		      SCM_I_BIG_MPZ (val));

	  return n >= min && n <= max;
	}
    }
  else
    return 0;
}

static void
scm_i_range_error (SCM bad_val, SCM min, SCM max)
{
  scm_error (scm_out_of_range_key,
	     NULL,
	     "Value out of range ~S to ~S: ~S",
             scm_list_3 (min, max, bad_val),
             scm_list_1 (bad_val));
}

#define TYPE                     scm_t_intmax
#define TYPE_MIN                 min
#define TYPE_MAX                 max
#define SIZEOF_TYPE              0
#define SCM_TO_TYPE_PROTO(arg)   scm_to_signed_integer (arg, scm_t_intmax min, scm_t_intmax max)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_signed_integer (arg)
#include "libguile/conv-integer.i.c"

#define TYPE                     scm_t_uintmax
#define TYPE_MIN                 min
#define TYPE_MAX                 max
#define SIZEOF_TYPE              0
#define SCM_TO_TYPE_PROTO(arg)   scm_to_unsigned_integer (arg, scm_t_uintmax min, scm_t_uintmax max)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_unsigned_integer (arg)
#include "libguile/conv-uinteger.i.c"

#define TYPE                     scm_t_int8
#define TYPE_MIN                 SCM_T_INT8_MIN
#define TYPE_MAX                 SCM_T_INT8_MAX
#define SIZEOF_TYPE              1
#define SCM_TO_TYPE_PROTO(arg)   scm_to_int8 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_int8 (arg)
#include "libguile/conv-integer.i.c"

#define TYPE                     scm_t_uint8
#define TYPE_MIN                 0
#define TYPE_MAX                 SCM_T_UINT8_MAX
#define SIZEOF_TYPE              1
#define SCM_TO_TYPE_PROTO(arg)   scm_to_uint8 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_uint8 (arg)
#include "libguile/conv-uinteger.i.c"

#define TYPE                     scm_t_int16
#define TYPE_MIN                 SCM_T_INT16_MIN
#define TYPE_MAX                 SCM_T_INT16_MAX
#define SIZEOF_TYPE              2
#define SCM_TO_TYPE_PROTO(arg)   scm_to_int16 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_int16 (arg)
#include "libguile/conv-integer.i.c"

#define TYPE                     scm_t_uint16
#define TYPE_MIN                 0
#define TYPE_MAX                 SCM_T_UINT16_MAX
#define SIZEOF_TYPE              2
#define SCM_TO_TYPE_PROTO(arg)   scm_to_uint16 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_uint16 (arg)
#include "libguile/conv-uinteger.i.c"

#define TYPE                     scm_t_int32
#define TYPE_MIN                 SCM_T_INT32_MIN
#define TYPE_MAX                 SCM_T_INT32_MAX
#define SIZEOF_TYPE              4
#define SCM_TO_TYPE_PROTO(arg)   scm_to_int32 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_int32 (arg)
#include "libguile/conv-integer.i.c"

#define TYPE                     scm_t_uint32
#define TYPE_MIN                 0
#define TYPE_MAX                 SCM_T_UINT32_MAX
#define SIZEOF_TYPE              4
#define SCM_TO_TYPE_PROTO(arg)   scm_to_uint32 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_uint32 (arg)
#include "libguile/conv-uinteger.i.c"

#define TYPE                     scm_t_wchar
#define TYPE_MIN                 (scm_t_int32)-1
#define TYPE_MAX                 (scm_t_int32)0x10ffff
#define SIZEOF_TYPE              4
#define SCM_TO_TYPE_PROTO(arg)   scm_to_wchar (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_wchar (arg)
#include "libguile/conv-integer.i.c"

#define TYPE                     scm_t_int64
#define TYPE_MIN                 SCM_T_INT64_MIN
#define TYPE_MAX                 SCM_T_INT64_MAX
#define SIZEOF_TYPE              8
#define SCM_TO_TYPE_PROTO(arg)   scm_to_int64 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_int64 (arg)
#include "libguile/conv-integer.i.c"

#define TYPE                     scm_t_uint64
#define TYPE_MIN                 0
#define TYPE_MAX                 SCM_T_UINT64_MAX
#define SIZEOF_TYPE              8
#define SCM_TO_TYPE_PROTO(arg)   scm_to_uint64 (arg)
#define SCM_FROM_TYPE_PROTO(arg) scm_from_uint64 (arg)
#include "libguile/conv-uinteger.i.c"

void
scm_to_mpz (SCM val, mpz_t rop)
{
  if (SCM_I_INUMP (val))
    mpz_set_si (rop, SCM_I_INUM (val));
  else if (SCM_BIGP (val))
    mpz_set (rop, SCM_I_BIG_MPZ (val));
  else
    scm_wrong_type_arg_msg (NULL, 0, val, "exact integer");
}

SCM
scm_from_mpz (mpz_t val)
{
  return scm_i_mpz2num (val);
}

int
scm_is_real (SCM val)
{
  return scm_is_true (scm_real_p (val));
}

int
scm_is_rational (SCM val)
{
  return scm_is_true (scm_rational_p (val));
}

double
scm_to_double (SCM val)
{
  if (SCM_I_INUMP (val))
    return SCM_I_INUM (val);
  else if (SCM_BIGP (val))
    return scm_i_big2dbl (val);
  else if (SCM_FRACTIONP (val))
    return scm_i_fraction2double (val);
  else if (SCM_REALP (val))
    return SCM_REAL_VALUE (val);
  else
    scm_wrong_type_arg_msg (NULL, 0, val, "real number");
}

SCM
scm_from_double (double val)
{
  return scm_i_from_double (val);
}

#if SCM_ENABLE_DEPRECATED == 1

float
scm_num2float (SCM num, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2float' is deprecated. Use scm_to_double instead.");

  if (SCM_BIGP (num))
    {
      float res = mpz_get_d (SCM_I_BIG_MPZ (num));
      if (!isinf (res))
	return res;
      else
	scm_out_of_range (NULL, num);
    }
  else
    return scm_to_double (num);
}

double
scm_num2double (SCM num, unsigned long pos, const char *s_caller)
{
  scm_c_issue_deprecation_warning
    ("`scm_num2double' is deprecated. Use scm_to_double instead.");

  if (SCM_BIGP (num))
    {
      double res = mpz_get_d (SCM_I_BIG_MPZ (num));
      if (!isinf (res))
	return res;
      else
	scm_out_of_range (NULL, num);
    }
  else
    return scm_to_double (num);
}

#endif

int
scm_is_complex (SCM val)
{
  return scm_is_true (scm_complex_p (val));
}

double
scm_c_real_part (SCM z)
{
  if (SCM_COMPLEXP (z))
    return SCM_COMPLEX_REAL (z);
  else
    {
      /* Use the scm_real_part to get proper error checking and
	 dispatching.
      */
      return scm_to_double (scm_real_part (z)); 
    }
}

double
scm_c_imag_part (SCM z)
{
  if (SCM_COMPLEXP (z))
    return SCM_COMPLEX_IMAG (z);
  else
    {
      /* Use the scm_imag_part to get proper error checking and
	 dispatching.  The result will almost always be 0.0, but not
	 always.
      */
      return scm_to_double (scm_imag_part (z));
    }
}

double
scm_c_magnitude (SCM z)
{
  return scm_to_double (scm_magnitude (z));
}

double
scm_c_angle (SCM z)
{
  return scm_to_double (scm_angle (z));
}

int
scm_is_number (SCM z)
{
  return scm_is_true (scm_number_p (z));
}


/* Returns log(x * 2^shift) */
static SCM
log_of_shifted_double (double x, long shift)
{
  double ans = log (fabs (x)) + shift * M_LN2;

  if (copysign (1.0, x) > 0.0)
    return scm_i_from_double (ans);
  else
    return scm_c_make_rectangular (ans, M_PI);
}

/* Returns log(n), for exact integer n */
static SCM
log_of_exact_integer (SCM n)
{
  if (SCM_I_INUMP (n))
    return log_of_shifted_double (SCM_I_INUM (n), 0);
  else if (SCM_BIGP (n))
    {
      long expon;
      double signif = scm_i_big2dbl_2exp (n, &expon);
      return log_of_shifted_double (signif, expon);
    }
  else
    scm_wrong_type_arg ("log_of_exact_integer", SCM_ARG1, n);
}

/* Returns log(n/d), for exact non-zero integers n and d */
static SCM
log_of_fraction (SCM n, SCM d)
{
  long n_size = scm_to_long (scm_integer_length (n));
  long d_size = scm_to_long (scm_integer_length (d));

  if (abs (n_size - d_size) > 1)
    return (scm_difference (log_of_exact_integer (n),
			    log_of_exact_integer (d)));
  else if (scm_is_false (scm_negative_p (n)))
    return scm_i_from_double
      (log1p (scm_i_divide2double (scm_difference (n, d), d)));
  else
    return scm_c_make_rectangular
      (log1p (scm_i_divide2double (scm_difference (scm_abs (n), d),
                                   d)),
       M_PI);
}


/* In the following functions we dispatch to the real-arg funcs like log()
   when we know the arg is real, instead of just handing everything to
   clog() for instance.  This is in case clog() doesn't optimize for a
   real-only case, and because we have to test SCM_COMPLEXP anyway so may as
   well use it to go straight to the applicable C func.  */

SCM_PRIMITIVE_GENERIC (scm_log, "log", 1, 0, 0,
		       (SCM z),
		       "Return the natural logarithm of @var{z}.")
#define FUNC_NAME s_scm_log
{
  if (SCM_COMPLEXP (z))
    {
#if defined HAVE_COMPLEX_DOUBLE && defined HAVE_CLOG \
  && defined (SCM_COMPLEX_VALUE)
      return scm_from_complex_double (clog (SCM_COMPLEX_VALUE (z)));
#else
      double re = SCM_COMPLEX_REAL (z);
      double im = SCM_COMPLEX_IMAG (z);
      return scm_c_make_rectangular (log (hypot (re, im)),
                                     atan2 (im, re));
#endif
    }
  else if (SCM_REALP (z))
    return log_of_shifted_double (SCM_REAL_VALUE (z), 0);
  else if (SCM_I_INUMP (z))
    {
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
      if (scm_is_eq (z, SCM_INUM0))
	scm_num_overflow (s_scm_log);
#endif
      return log_of_shifted_double (SCM_I_INUM (z), 0);
    }
  else if (SCM_BIGP (z))
    return log_of_exact_integer (z);
  else if (SCM_FRACTIONP (z))
    return log_of_fraction (SCM_FRACTION_NUMERATOR (z),
			    SCM_FRACTION_DENOMINATOR (z));
  else
    SCM_WTA_DISPATCH_1 (g_scm_log, z, 1, s_scm_log);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_log10, "log10", 1, 0, 0,
		       (SCM z),
		       "Return the base 10 logarithm of @var{z}.")
#define FUNC_NAME s_scm_log10
{
  if (SCM_COMPLEXP (z))
    {
      /* Mingw has clog() but not clog10().  (Maybe it'd be worth using
         clog() and a multiply by M_LOG10E, rather than the fallback
         log10+hypot+atan2.)  */
#if defined HAVE_COMPLEX_DOUBLE && defined HAVE_CLOG10	\
      && defined SCM_COMPLEX_VALUE
      return scm_from_complex_double (clog10 (SCM_COMPLEX_VALUE (z)));
#else
      double re = SCM_COMPLEX_REAL (z);
      double im = SCM_COMPLEX_IMAG (z);
      return scm_c_make_rectangular (log10 (hypot (re, im)),
                                     M_LOG10E * atan2 (im, re));
#endif
    }
  else if (SCM_REALP (z) || SCM_I_INUMP (z))
    {
#ifndef ALLOW_DIVIDE_BY_EXACT_ZERO
      if (scm_is_eq (z, SCM_INUM0))
	scm_num_overflow (s_scm_log10);
#endif
      {
	double re = scm_to_double (z);
	double l = log10 (fabs (re));
	if (copysign (1.0, re) > 0.0)
	  return scm_i_from_double (l);
	else
	  return scm_c_make_rectangular (l, M_LOG10E * M_PI);
      }
    }
  else if (SCM_BIGP (z))
    return scm_product (flo_log10e, log_of_exact_integer (z));
  else if (SCM_FRACTIONP (z))
    return scm_product (flo_log10e,
			log_of_fraction (SCM_FRACTION_NUMERATOR (z),
					 SCM_FRACTION_DENOMINATOR (z)));
  else
    SCM_WTA_DISPATCH_1 (g_scm_log10, z, 1, s_scm_log10);
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC (scm_exp, "exp", 1, 0, 0,
		       (SCM z),
	"Return @math{e} to the power of @var{z}, where @math{e} is the\n"
	"base of natural logarithms (2.71828@dots{}).")
#define FUNC_NAME s_scm_exp
{
  if (SCM_COMPLEXP (z))
    {
#if defined HAVE_COMPLEX_DOUBLE && defined HAVE_CEXP \
  && defined (SCM_COMPLEX_VALUE)
      return scm_from_complex_double (cexp (SCM_COMPLEX_VALUE (z)));
#else
      return scm_c_make_polar (exp (SCM_COMPLEX_REAL (z)),
                               SCM_COMPLEX_IMAG (z));
#endif
    }
  else if (SCM_NUMBERP (z))
    {
      /* When z is a negative bignum the conversion to double overflows,
         giving -infinity, but that's ok, the exp is still 0.0.  */
      return scm_i_from_double (exp (scm_to_double (z)));
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_exp, z, 1, s_scm_exp);
}
#undef FUNC_NAME


SCM_DEFINE (scm_i_exact_integer_sqrt, "exact-integer-sqrt", 1, 0, 0,
	    (SCM k),
	    "Return two exact non-negative integers @var{s} and @var{r}\n"
	    "such that @math{@var{k} = @var{s}^2 + @var{r}} and\n"
	    "@math{@var{s}^2 <= @var{k} < (@var{s} + 1)^2}.\n"
	    "An error is raised if @var{k} is not an exact non-negative integer.\n"
	    "\n"
	    "@lisp\n"
	    "(exact-integer-sqrt 10) @result{} 3 and 1\n"
	    "@end lisp")
#define FUNC_NAME s_scm_i_exact_integer_sqrt
{
  SCM s, r;

  scm_exact_integer_sqrt (k, &s, &r);
  return scm_values (scm_list_2 (s, r));
}
#undef FUNC_NAME

void
scm_exact_integer_sqrt (SCM k, SCM *sp, SCM *rp)
{
  if (SCM_LIKELY (SCM_I_INUMP (k)))
    {
      mpz_t kk, ss, rr;

      if (SCM_I_INUM (k) < 0)
	scm_wrong_type_arg_msg ("exact-integer-sqrt", SCM_ARG1, k,
				"exact non-negative integer");
      mpz_init_set_ui (kk, SCM_I_INUM (k));
      mpz_inits (ss, rr, NULL);
      mpz_sqrtrem (ss, rr, kk);
      *sp = SCM_I_MAKINUM (mpz_get_ui (ss));
      *rp = SCM_I_MAKINUM (mpz_get_ui (rr));
      mpz_clears (kk, ss, rr, NULL);
    }
  else if (SCM_LIKELY (SCM_BIGP (k)))
    {
      SCM s, r;

      if (mpz_sgn (SCM_I_BIG_MPZ (k)) < 0)
	scm_wrong_type_arg_msg ("exact-integer-sqrt", SCM_ARG1, k,
				"exact non-negative integer");
      s = scm_i_mkbig ();
      r = scm_i_mkbig ();
      mpz_sqrtrem (SCM_I_BIG_MPZ (s), SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (k));
      scm_remember_upto_here_1 (k);
      *sp = scm_i_normbig (s);
      *rp = scm_i_normbig (r);
    }
  else
    scm_wrong_type_arg_msg ("exact-integer-sqrt", SCM_ARG1, k,
			    "exact non-negative integer");
}

/* Return true iff K is a perfect square.
   K must be an exact integer. */
static int
exact_integer_is_perfect_square (SCM k)
{
  int result;

  if (SCM_LIKELY (SCM_I_INUMP (k)))
    {
      mpz_t kk;

      mpz_init_set_si (kk, SCM_I_INUM (k));
      result = mpz_perfect_square_p (kk);
      mpz_clear (kk);
    }
  else
    {
      result = mpz_perfect_square_p (SCM_I_BIG_MPZ (k));
      scm_remember_upto_here_1 (k);
    }
  return result;
}

/* Return the floor of the square root of K.
   K must be an exact integer. */
static SCM
exact_integer_floor_square_root (SCM k)
{
  if (SCM_LIKELY (SCM_I_INUMP (k)))
    {
      mpz_t kk;
      scm_t_inum ss;

      mpz_init_set_ui (kk, SCM_I_INUM (k));
      mpz_sqrt (kk, kk);
      ss = mpz_get_ui (kk);
      mpz_clear (kk);
      return SCM_I_MAKINUM (ss);
    }
  else
    {
      SCM s;

      s = scm_i_mkbig ();
      mpz_sqrt (SCM_I_BIG_MPZ (s), SCM_I_BIG_MPZ (k));
      scm_remember_upto_here_1 (k);
      return scm_i_normbig (s);
    }
}


SCM_PRIMITIVE_GENERIC (scm_sqrt, "sqrt", 1, 0, 0,
		       (SCM z),
	"Return the square root of @var{z}.  Of the two possible roots\n"
	"(positive and negative), the one with positive real part\n"
	"is returned, or if that's zero then a positive imaginary part.\n"
	"Thus,\n"
	"\n"
	"@example\n"
	"(sqrt 9.0)       @result{} 3.0\n"
	"(sqrt -9.0)      @result{} 0.0+3.0i\n"
	"(sqrt 1.0+1.0i)  @result{} 1.09868411346781+0.455089860562227i\n"
	"(sqrt -1.0-1.0i) @result{} 0.455089860562227-1.09868411346781i\n"
	"@end example")
#define FUNC_NAME s_scm_sqrt
{
  if (SCM_COMPLEXP (z))
    {
#if defined HAVE_COMPLEX_DOUBLE && defined HAVE_USABLE_CSQRT	\
      && defined SCM_COMPLEX_VALUE
      return scm_from_complex_double (csqrt (SCM_COMPLEX_VALUE (z)));
#else
      double re = SCM_COMPLEX_REAL (z);
      double im = SCM_COMPLEX_IMAG (z);
      return scm_c_make_polar (sqrt (hypot (re, im)),
                               0.5 * atan2 (im, re));
#endif
    }
  else if (SCM_NUMBERP (z))
    {
      if (SCM_I_INUMP (z))
        {
          scm_t_inum x = SCM_I_INUM (z);

          if (SCM_LIKELY (x >= 0))
            {
              if (SCM_LIKELY (SCM_I_FIXNUM_BIT < DBL_MANT_DIG
                              || x < (1L << (DBL_MANT_DIG - 1))))
                {
                  double root = sqrt (x);

                  /* If 0 <= x < 2^(DBL_MANT_DIG-1) and sqrt(x) is an
                     integer, then the result is exact. */
                  if (root == floor (root))
                    return SCM_I_MAKINUM ((scm_t_inum) root);
                  else
                    return scm_i_from_double (root);
                }
              else
                {
                  mpz_t xx;
                  scm_t_inum root;

                  mpz_init_set_ui (xx, x);
                  if (mpz_perfect_square_p (xx))
                    {
                      mpz_sqrt (xx, xx);
                      root = mpz_get_ui (xx);
                      mpz_clear (xx);
                      return SCM_I_MAKINUM (root);
                    }
                  else
                    mpz_clear (xx);
                }
            }
        }
      else if (SCM_BIGP (z))
        {
          if (mpz_perfect_square_p (SCM_I_BIG_MPZ (z)))
            {
              SCM root = scm_i_mkbig ();

              mpz_sqrt (SCM_I_BIG_MPZ (root), SCM_I_BIG_MPZ (z));
              scm_remember_upto_here_1 (z);
              return scm_i_normbig (root);
            }
          else
            {
              long expon;
              double signif = scm_i_big2dbl_2exp (z, &expon);

              if (expon & 1)
                {
                  signif *= 2;
                  expon--;
                }
              if (signif < 0)
                return scm_c_make_rectangular
                  (0.0, ldexp (sqrt (-signif), expon / 2));
              else
                return scm_i_from_double (ldexp (sqrt (signif), expon / 2));
            }
        }
      else if (SCM_FRACTIONP (z))
        {
          SCM n = SCM_FRACTION_NUMERATOR (z);
          SCM d = SCM_FRACTION_DENOMINATOR (z);

          if (exact_integer_is_perfect_square (n)
              && exact_integer_is_perfect_square (d))
            return scm_i_make_ratio_already_reduced
              (exact_integer_floor_square_root (n),
               exact_integer_floor_square_root (d));
          else
            {
              double xx = scm_i_divide2double (n, d);
              double abs_xx = fabs (xx);
              long shift = 0;

              if (SCM_UNLIKELY (abs_xx > DBL_MAX || abs_xx < DBL_MIN))
                {
                  shift = (scm_to_long (scm_integer_length (n))
                           - scm_to_long (scm_integer_length (d))) / 2;
                  if (shift > 0)
                    d = left_shift_exact_integer (d, 2 * shift);
                  else
                    n = left_shift_exact_integer (n, -2 * shift);
                  xx = scm_i_divide2double (n, d);
                }

              if (xx < 0)
                return scm_c_make_rectangular (0.0, ldexp (sqrt (-xx), shift));
              else
                return scm_i_from_double (ldexp (sqrt (xx), shift));
            }
        }

      /* Fallback method, when the cases above do not apply. */
      {
        double xx = scm_to_double (z);
        if (xx < 0)
          return scm_c_make_rectangular (0.0, sqrt (-xx));
        else
          return scm_i_from_double (sqrt (xx));
      }
    }
  else
    SCM_WTA_DISPATCH_1 (g_scm_sqrt, z, 1, s_scm_sqrt);
}
#undef FUNC_NAME



void
scm_init_numbers ()
{
  if (scm_install_gmp_memory_functions)
    mp_set_memory_functions (custom_gmp_malloc,
                             custom_gmp_realloc,
                             custom_gmp_free);

  mpz_init_set_si (z_negative_one, -1);

  /* It may be possible to tune the performance of some algorithms by using
   * the following constants to avoid the creation of bignums.  Please, before
   * using these values, remember the two rules of program optimization:
   * 1st Rule:  Don't do it.  2nd Rule (experts only):  Don't do it yet. */
  scm_c_define ("most-positive-fixnum",
		SCM_I_MAKINUM (SCM_MOST_POSITIVE_FIXNUM));
  scm_c_define ("most-negative-fixnum",
		SCM_I_MAKINUM (SCM_MOST_NEGATIVE_FIXNUM));

  scm_add_feature ("complex");
  scm_add_feature ("inexact");
  flo0 = scm_i_from_double (0.0);
  flo_log10e = scm_i_from_double (M_LOG10E);

  exactly_one_half = scm_divide (SCM_INUM1, SCM_I_MAKINUM (2));

  {
    /* Set scm_i_divide2double_lo2b to (2 b^p - 1) */
    mpz_init_set_ui (scm_i_divide2double_lo2b, 1);
    mpz_mul_2exp (scm_i_divide2double_lo2b,
                  scm_i_divide2double_lo2b,
                  DBL_MANT_DIG + 1); /* 2 b^p */
    mpz_sub_ui (scm_i_divide2double_lo2b, scm_i_divide2double_lo2b, 1);
  }

  {
    /* Set dbl_minimum_normal_mantissa to b^{p-1} */
    mpz_init_set_ui (dbl_minimum_normal_mantissa, 1);
    mpz_mul_2exp (dbl_minimum_normal_mantissa,
                  dbl_minimum_normal_mantissa,
                  DBL_MANT_DIG - 1);
  }

#include "libguile/numbers.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
