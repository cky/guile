/* classes: h_files */

#ifndef SCM_NUMBERS_H
#define SCM_NUMBERS_H

/* Copyright (C) 1995,1996,1998,2000,2001,2002,2003,2004 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"
#include "libguile/print.h"

#if SCM_HAVE_FLOATINGPOINT_H
# include <floatingpoint.h>
#endif

#if SCM_HAVE_IEEEFP_H
# include <ieeefp.h>
#endif

#if SCM_HAVE_NAN_H
# if defined (SCO)
#   define _IEEE 1
# endif
# include <nan.h>
# if defined (SCO)
#   undef _IEEE
# endif
#endif /* SCM_HAVE_NAN_H */



/* Immediate Numbers, also known as fixnums
 *
 * Inums are exact integer data that fits within an SCM word.  */

/* SCM_T_SIGNED_MAX is                 (- (expt 2 n) 1),
 * SCM_MOST_POSITIVE_FIXNUM should be  (- (expt 2 (- n 2)) 1)
 * which is the same as                (/ (- (expt 2 n) 4) 4)
 */

#define SCM_I_FIXNUM_BIT         (SCM_LONG_BIT - 2)
#define SCM_MOST_POSITIVE_FIXNUM ((SCM_T_SIGNED_BITS_MAX-3)/4)
#define SCM_MOST_NEGATIVE_FIXNUM (-SCM_MOST_POSITIVE_FIXNUM-1)

/* SCM_SRS is signed right shift */
#if (-1 == (((-1) << 2) + 2) >> 2)
# define SCM_SRS(x, y) ((x) >> (y))
#else
# define SCM_SRS(x, y) ((x) < 0 ? ~((~(x)) >> (y)) : ((x) >> (y)))
#endif /* (-1 == (((-1) << 2) + 2) >> 2) */


#define SCM_INUMP(x)	(2 & SCM_UNPACK (x))
#define SCM_NINUMP(x) 	(!SCM_INUMP (x))
#define SCM_MAKINUM(x) \
  (SCM_PACK ((((scm_t_signed_bits) (x)) << 2) + scm_tc2_int))
#define SCM_INUM(x)     (SCM_SRS ((scm_t_signed_bits) SCM_UNPACK (x), 2))


/* SCM_FIXABLE is true if its long argument can be encoded in an SCM_INUM. */
#define SCM_POSFIXABLE(n) ((n) <= SCM_MOST_POSITIVE_FIXNUM)
#define SCM_NEGFIXABLE(n) ((n) >= SCM_MOST_NEGATIVE_FIXNUM)
#define SCM_FIXABLE(n) (SCM_POSFIXABLE (n) && SCM_NEGFIXABLE (n))


/* A name for 0. */
#define SCM_INUM0 (SCM_MAKINUM (0))


/* SCM_MAXEXP is the maximum double precision exponent
 * SCM_FLTMAX is less than or scm_equal the largest single precision float
 */

#if SCM_HAVE_STDC_HEADERS
# ifndef GO32
#  include <float.h>
#  ifdef __MINGW32__
#   define copysign _copysign
#   define isnan _isnan
#   define finite _finite
#  endif /* __MINGW32__ */
# endif /* ndef GO32 */
#endif /* def STDC_HEADERS */

#ifdef DBL_MAX_10_EXP
# define SCM_MAXEXP DBL_MAX_10_EXP
#else
# define SCM_MAXEXP 308		/* IEEE doubles */
#endif /* def DBL_MAX_10_EXP */

#ifdef FLT_MAX
# define SCM_FLTMAX FLT_MAX
#else
# define SCM_FLTMAX 1e+23
#endif /* def FLT_MAX */


/* SCM_INTBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an exact immediate.
 */
#define SCM_INTBUFLEN (5 + SCM_LONG_BIT)



/* Numbers 
 */


/* Note that scm_tc16_real and scm_tc16_complex are given tc16-codes that only
 * differ in one bit: This way, checking if an object is an inexact number can
 * be done quickly (using the TYP16S macro).  */

/* Number subtype 1 to 3 (note the dependency on the predicates SCM_INEXACTP
 * and SCM_NUMP)  */
#define scm_tc16_big		(scm_tc7_number + 1 * 256L)
#define scm_tc16_real           (scm_tc7_number + 2 * 256L)
#define scm_tc16_complex        (scm_tc7_number + 3 * 256L)
#define scm_tc16_fraction       (scm_tc7_number + 4 * 256L)

#define SCM_INEXACTP(x) \
  (!SCM_IMP (x) && (0xfeff & SCM_CELL_TYPE (x)) == scm_tc16_real)
#define SCM_REALP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_real)
#define SCM_COMPLEXP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_complex)

#define SCM_REAL_VALUE(x) (((scm_t_double *) SCM2PTR (x))->real)
#define SCM_COMPLEX_MEM(x) ((scm_t_complex *) SCM_CELL_WORD_1 (x))
#define SCM_COMPLEX_REAL(x) (SCM_COMPLEX_MEM (x)->real)
#define SCM_COMPLEX_IMAG(x) (SCM_COMPLEX_MEM (x)->imag)

/* Each bignum is just an mpz_t stored in a double cell starting at word 1. */
#define SCM_I_BIG_MPZ(x) (*((mpz_t *) (SCM_CELL_OBJECT_LOC((x),1))))
#define SCM_BIGP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_big)

#define SCM_NUMBERP(x) (SCM_INUMP(x) || SCM_NUMP(x))
#define SCM_NUMP(x) (!SCM_IMP(x) \
  && (((0xfcff & SCM_CELL_TYPE (x)) == scm_tc7_number) \
      || ((0xfbff & SCM_CELL_TYPE (x)) == scm_tc7_number)))
/* 0xfcff (#b1100) for 0 free, 1 big, 2 real, 3 complex, then 0xfbff (#b1011) for 4 fraction */

#define SCM_FRACTIONP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_fraction)
#define SCM_FRACTION_NUMERATOR(x) (SCM_CELL_OBJECT_1 (x))
#define SCM_FRACTION_DENOMINATOR(x) (SCM_CELL_OBJECT_2 (x))
#define SCM_FRACTION_SET_NUMERATOR(x, v) (SCM_SET_CELL_OBJECT_1 ((x), (v)))
#define SCM_FRACTION_SET_DENOMINATOR(x, v) (SCM_SET_CELL_OBJECT_2 ((x), (v)))

  /* I think the left half word is free in the type, so I'll use bit 17 */
#define SCM_FRACTION_REDUCED_BIT 0x10000
#define SCM_FRACTION_REDUCED_SET(x) (SCM_SET_CELL_TYPE((x), (SCM_CELL_TYPE (x) | SCM_FRACTION_REDUCED_BIT)))
#define SCM_FRACTION_REDUCED_CLEAR(x) (SCM_SET_CELL_TYPE((x), (SCM_CELL_TYPE (x) & ~SCM_FRACTION_REDUCED_BIT)))
#define SCM_FRACTION_REDUCED(x)     (0x10000 & SCM_CELL_TYPE (x))



typedef struct scm_t_double
{
  SCM type;
  SCM pad;
  double real;
} scm_t_double;

typedef struct scm_t_complex
{
  double real;
  double imag;
} scm_t_complex;



SCM_API SCM scm_exact_p (SCM x);
SCM_API SCM scm_odd_p (SCM n);
SCM_API SCM scm_even_p (SCM n);
SCM_API SCM scm_inf_p (SCM n);
SCM_API SCM scm_nan_p (SCM n);
SCM_API SCM scm_inf (void);
SCM_API SCM scm_nan (void);
SCM_API SCM scm_abs (SCM x);
SCM_API SCM scm_quotient (SCM x, SCM y);
SCM_API SCM scm_remainder (SCM x, SCM y);
SCM_API SCM scm_modulo (SCM x, SCM y);
SCM_API SCM scm_gcd (SCM x, SCM y);
SCM_API SCM scm_lcm (SCM n1, SCM n2);
SCM_API SCM scm_logand (SCM n1, SCM n2);
SCM_API SCM scm_logior (SCM n1, SCM n2);
SCM_API SCM scm_logxor (SCM n1, SCM n2);
SCM_API SCM scm_logtest (SCM n1, SCM n2);
SCM_API SCM scm_logbit_p (SCM n1, SCM n2);
SCM_API SCM scm_lognot (SCM n);
SCM_API SCM scm_modulo_expt (SCM n, SCM k, SCM m);
SCM_API SCM scm_integer_expt (SCM z1, SCM z2);
SCM_API SCM scm_ash (SCM n, SCM cnt);
SCM_API SCM scm_bit_extract (SCM n, SCM start, SCM end);
SCM_API SCM scm_logcount (SCM n);
SCM_API SCM scm_integer_length (SCM n);

SCM_API size_t scm_iint2str (long num, int rad, char *p);
SCM_API SCM scm_number_to_string (SCM x, SCM radix);
SCM_API int scm_print_real (SCM sexp, SCM port, scm_print_state *pstate);
SCM_API int scm_print_complex (SCM sexp, SCM port, scm_print_state *pstate);
SCM_API int scm_bigprint (SCM exp, SCM port, scm_print_state *pstate);
SCM_API SCM scm_i_mem2number (const char *mem, size_t len, unsigned int radix);
SCM_API SCM scm_string_to_number (SCM str, SCM radix);
SCM_API SCM scm_make_real (double x);
SCM_API SCM scm_make_complex (double x, double y);
SCM_API SCM scm_bigequal (SCM x, SCM y);
SCM_API SCM scm_real_equalp (SCM x, SCM y);
SCM_API SCM scm_complex_equalp (SCM x, SCM y);
SCM_API SCM scm_number_p (SCM x);
SCM_API SCM scm_real_p (SCM x);
SCM_API SCM scm_integer_p (SCM x);
SCM_API SCM scm_inexact_p (SCM x);
SCM_API SCM scm_num_eq_p (SCM x, SCM y);
SCM_API SCM scm_less_p (SCM x, SCM y);
SCM_API SCM scm_gr_p (SCM x, SCM y);
SCM_API SCM scm_leq_p (SCM x, SCM y);
SCM_API SCM scm_geq_p (SCM x, SCM y);
SCM_API SCM scm_zero_p (SCM z);
SCM_API SCM scm_positive_p (SCM x);
SCM_API SCM scm_negative_p (SCM x);
SCM_API SCM scm_max (SCM x, SCM y);
SCM_API SCM scm_min (SCM x, SCM y);
SCM_API SCM scm_sum (SCM x, SCM y);
SCM_API SCM scm_difference (SCM x, SCM y);
SCM_API SCM scm_product (SCM x, SCM y);
SCM_API double scm_num2dbl (SCM a, const char * why);
SCM_API SCM scm_divide (SCM x, SCM y);
SCM_API SCM scm_floor (SCM x);
SCM_API SCM scm_ceiling (SCM x);
SCM_API double scm_asinh (double x);
SCM_API double scm_acosh (double x);
SCM_API double scm_atanh (double x);
SCM_API double scm_truncate (double x);
SCM_API double scm_round (double x);
SCM_API SCM scm_truncate_number (SCM x);
SCM_API SCM scm_round_number (SCM x);
SCM_API SCM scm_sys_expt (SCM z1, SCM z2);
SCM_API SCM scm_sys_atan2 (SCM z1, SCM z2);
SCM_API SCM scm_make_rectangular (SCM z1, SCM z2);
SCM_API SCM scm_make_polar (SCM z1, SCM z2);
SCM_API SCM scm_real_part (SCM z);
SCM_API SCM scm_imag_part (SCM z);
SCM_API SCM scm_magnitude (SCM z);
SCM_API SCM scm_angle (SCM z);
SCM_API SCM scm_exact_to_inexact (SCM z);
SCM_API SCM scm_inexact_to_exact (SCM z);
SCM_API SCM scm_trunc (SCM x);

SCM_API SCM scm_short2num (short n);
SCM_API SCM scm_ushort2num (unsigned short n);
SCM_API SCM scm_int2num (int n);
SCM_API SCM scm_uint2num (unsigned int n);
SCM_API SCM scm_long2num (long n);
SCM_API SCM scm_ulong2num (unsigned long n);
SCM_API SCM scm_size2num (size_t n);
SCM_API SCM scm_ptrdiff2num (scm_t_ptrdiff n);
SCM_API short scm_num2short (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_API unsigned short scm_num2ushort (SCM num, unsigned long int pos,
				       const char *s_caller);
SCM_API int scm_num2int (SCM num, unsigned long int pos,
			 const char *s_caller);
SCM_API unsigned int scm_num2uint (SCM num, unsigned long int pos,
				   const char *s_caller);
SCM_API long scm_num2long (SCM num, unsigned long int pos,
			   const char *s_caller);
SCM_API unsigned long scm_num2ulong (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_API scm_t_ptrdiff scm_num2ptrdiff (SCM num, unsigned long int pos,
                                       const char *s_caller);
SCM_API size_t scm_num2size (SCM num, unsigned long int pos,
			     const char *s_caller);
#if SCM_SIZEOF_LONG_LONG != 0
SCM_API SCM scm_long_long2num (long long sl);
SCM_API SCM scm_ulong_long2num (unsigned long long sl);
SCM_API long long scm_num2long_long (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_API unsigned long long scm_num2ulong_long (SCM num, unsigned long int pos,
					       const char *s_caller);
#endif

SCM_API SCM scm_float2num (float n);
SCM_API SCM scm_double2num (double n);
SCM_API float scm_num2float (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_API double scm_num2double (SCM num, unsigned long int pos,
			       const char *s_caller);


/* bignum internal functions */
SCM_API SCM scm_i_mkbig (void);
SCM_API SCM scm_i_normbig (SCM x);
SCM_API int scm_i_bigcmp (SCM a, SCM b);
SCM_API SCM scm_i_dbl2big (double d);
SCM_API SCM scm_i_dbl2num (double d);
SCM_API double scm_i_big2dbl (SCM b);
SCM_API SCM scm_i_short2big (short n);
SCM_API SCM scm_i_ushort2big (unsigned short n);
SCM_API SCM scm_i_int2big (int n);
SCM_API SCM scm_i_uint2big (unsigned int n);
SCM_API SCM scm_i_long2big (long n);
SCM_API SCM scm_i_ulong2big (unsigned long n);
SCM_API SCM scm_i_size2big (size_t n);
SCM_API SCM scm_i_ptrdiff2big (scm_t_ptrdiff n);

#if SCM_SIZEOF_LONG_LONG != 0
SCM_API SCM scm_i_long_long2big (long long n);
SCM_API SCM scm_i_ulong_long2big (unsigned long long n);
#endif


/* ratio functions */
SCM_API SCM scm_make_ratio (SCM num, SCM den);
SCM_API SCM scm_rationalize (SCM x, SCM err);
SCM_API SCM scm_numerator (SCM z);
SCM_API SCM scm_denominator (SCM z);
SCM_API SCM scm_rational_p (SCM z);

/* fraction internal functions */
SCM_API double scm_i_fraction2double (SCM z);
SCM_API SCM scm_i_fraction_equalp (SCM x, SCM y);
SCM_API int scm_i_print_fraction (SCM sexp, SCM port, scm_print_state *pstate);


#ifdef GUILE_DEBUG
SCM_API SCM scm_sys_check_number_conversions (void);
#endif

SCM_API void scm_init_numbers (void);

#endif  /* SCM_NUMBERS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
