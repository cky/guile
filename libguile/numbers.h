/* classes: h_files */

#ifndef NUMBERSH
#define NUMBERSH
/* Copyright (C) 1995,1996,1998,2000,2001 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"
#include "libguile/print.h"



/* Immediate Numbers 
 *
 * Inums are exact integer data that fits within an SCM word.
 *
 * SCM_INUMP applies only to values known to be Scheme objects.
 * In particular, SCM_INUMP (SCM_CAR (x)) is valid only if x is known
 * to be a SCM_CONSP.  If x is only known to be a SCM_NIMP, 
 * SCM_INUMP (SCM_CAR (x)) can give wrong answers.
 */

#define SCM_I_FIXNUM_BIT (SCM_BITS_LENGTH - 2)
#define SCM_MOST_POSITIVE_FIXNUM ((1L << (SCM_I_FIXNUM_BIT - 1)) - 1)
#define SCM_MOST_NEGATIVE_FIXNUM (-SCM_MOST_POSITIVE_FIXNUM - 1)


/* SCM_SRS is signed right shift */
#if (-1 == (((-1) << 2) + 2) >> 2)
# define SCM_SRS(x, y) ((x) >> (y))
#else
# define SCM_SRS(x, y) ((SCM_UNPACK (x) < 0) ? ~((~SCM_UNPACK (x)) >> (y)) : (SCM_UNPACK (x) >> (y)))
#endif /* (-1 == (((-1) << 2) + 2) >> 2) */


#define SCM_INUMP(x)	(2 & SCM_UNPACK (x))
#define SCM_NINUMP(x) 	(!SCM_INUMP (x))
#define SCM_MAKINUM(x)  (SCM_PACK (((x) << 2) + 2L))
#define SCM_INUM(x)     (SCM_SRS (SCM_UNPACK (x), 2))


/* SCM_FIXABLE is true if its long argument can be encoded in an SCM_INUM. */
#define SCM_POSFIXABLE(n) ((n) <= SCM_MOST_POSITIVE_FIXNUM)
#define SCM_NEGFIXABLE(n) ((n) >= SCM_MOST_NEGATIVE_FIXNUM)
#define SCM_FIXABLE(n) (SCM_POSFIXABLE(n) && SCM_NEGFIXABLE(n))


/* A name for 0. */
#define SCM_INUM0 (SCM_MAKINUM (0))


/* SCM_MAXEXP is the maximum double precision expontent
 * SCM_FLTMAX is less than or scm_equal the largest single precision float
 */

#ifdef STDC_HEADERS
#ifndef GO32
#include <float.h>
#endif /* ndef GO32 */
#endif /* def STDC_HEADERS */
#ifdef DBL_MAX_10_EXP
#define SCM_MAXEXP DBL_MAX_10_EXP
#else
#define SCM_MAXEXP 308		/* IEEE doubles */
#endif /* def DBL_MAX_10_EXP */
#ifdef FLT_MAX
#define SCM_FLTMAX FLT_MAX
#else
#define SCM_FLTMAX 1e+23
#endif /* def FLT_MAX */


/* SCM_INTBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an exact immediate.
 */
#define SCM_INTBUFLEN (5 + SCM_BITS_LENGTH)



/* Numbers 
 */

#define SCM_SLOPPY_INEXACTP(x) (SCM_TYP16S (x) == scm_tc16_real)
#define SCM_SLOPPY_REALP(x) (SCM_TYP16 (x) == scm_tc16_real)
#define SCM_SLOPPY_COMPLEXP(x) (SCM_TYP16 (x) == scm_tc16_complex)
#define SCM_INEXACTP(x) (SCM_NIMP (x) && SCM_TYP16S (x) == scm_tc16_real)
#define SCM_REALP(x) (SCM_NIMP (x) && SCM_TYP16 (x) == scm_tc16_real)
#define SCM_COMPLEXP(x) (SCM_NIMP (x) && SCM_TYP16 (x) == scm_tc16_complex)

#define SCM_REAL_VALUE(x) (((scm_double_t *) SCM2PTR (x))->real)
#define SCM_COMPLEX_MEM(x) ((scm_complex_t *) SCM_CELL_WORD_1 (x))
#define SCM_COMPLEX_REAL(x) (SCM_COMPLEX_MEM (x)->real)
#define SCM_COMPLEX_IMAG(x) (SCM_COMPLEX_MEM (x)->imag)

/* Define SCM_BIGDIG to an integer type whose size is smaller than long if
 * you want bignums.  SCM_BIGRAD is one greater than the biggest SCM_BIGDIG. 
 *
 * Define SCM_DIGSTOOBIG if the digits equivalent to a long won't fit in a long. 
 */
#ifdef BIGNUMS
# ifdef _UNICOS
#  define SCM_DIGSTOOBIG
#  if (1L << 31) <= SCM_USHRT_MAX
#   define SCM_BIGDIG unsigned  short
#  else
#   define SCM_BIGDIG unsigned int
#  endif /*  (1L << 31) <= USHRT_MAX */
#  define SCM_BITSPERDIG 32
# else
#  define SCM_BIGDIG unsigned short
#  define SCM_BITSPERDIG (sizeof(SCM_BIGDIG)*SCM_CHAR_BIT)
# endif /* def _UNICOS */

# define SCM_BIGRAD (1L << SCM_BITSPERDIG)
# define SCM_DIGSPERLONG ((size_t)((sizeof(long)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))
# define SCM_I_BIGUP(type, x) ((type)(x) << SCM_BITSPERDIG)
# define SCM_BIGUP(x) SCM_I_BIGUP (unsigned long, x)
# define SCM_LONGLONGBIGUP(x) SCM_I_BIGUP (unsigned long long, x)
# define SCM_BIGDN(x) ((x) >> SCM_BITSPERDIG)
# define SCM_BIGLO(x) ((x) & (SCM_BIGRAD-1))
#endif /* def BIGNUMS */

#ifndef SCM_BIGDIG
/* Definition is not really used but helps various function
 * prototypes to compile with conditionalization.
 */
# define SCM_BIGDIG unsigned short
#endif /* ndef SCM_BIGDIG */

#define SCM_NUMBERP(x) (SCM_INUMP(x) || SCM_NUMP(x))
#define SCM_NUMP(x) (!SCM_IMP(x) && (0xfcff & SCM_CELL_TYPE (x)) == scm_tc7_smob)
#define SCM_BIGP(x) (!SCM_IMP (x) && (SCM_TYP16 (x) == scm_tc16_big))
#define SCM_BIGSIGNFLAG 0x10000L
#define SCM_BIGSIZEFIELD 17
#define SCM_BIGSIGN(x) (SCM_CELL_WORD_0 (x) & SCM_BIGSIGNFLAG)
#define SCM_BDIGITS(x) ((SCM_BIGDIG *) (SCM_CELL_WORD_1 (x)))
#define SCM_SET_BIGNUM_BASE(n, b) (SCM_SET_CELL_WORD_1 ((n), (b)))
#define SCM_NUMDIGS(x) ((size_t) ((scm_ubits_t) SCM_CELL_WORD_0 (x) >> SCM_BIGSIZEFIELD))
#define SCM_SETNUMDIGS(x, v, sign) \
  SCM_SET_CELL_WORD_0 (x, \
	      scm_tc16_big \
	      | ((sign) ? SCM_BIGSIGNFLAG : 0) \
	      | (((v) + 0L) << SCM_BIGSIZEFIELD))



typedef struct scm_double_t
{
  SCM type;
  SCM pad;
  double real;
} scm_double_t;

typedef struct scm_complex_t
{
  double real;
  double imag;
} scm_complex_t;



extern SCM scm_exact_p (SCM x);
extern SCM scm_odd_p (SCM n);
extern SCM scm_even_p (SCM n);
extern SCM scm_abs (SCM x);
extern SCM scm_quotient (SCM x, SCM y);
extern SCM scm_remainder (SCM x, SCM y);
extern SCM scm_modulo (SCM x, SCM y);
extern SCM scm_gcd (SCM x, SCM y);
extern SCM scm_lcm (SCM n1, SCM n2);
extern SCM scm_logand (SCM n1, SCM n2);
extern SCM scm_logior (SCM n1, SCM n2);
extern SCM scm_logxor (SCM n1, SCM n2);
extern SCM scm_logtest (SCM n1, SCM n2);
extern SCM scm_logbit_p (SCM n1, SCM n2);
extern SCM scm_lognot (SCM n);
extern SCM scm_integer_expt (SCM z1, SCM z2);
extern SCM scm_ash (SCM n, SCM cnt);
extern SCM scm_bit_extract (SCM n, SCM start, SCM end);
extern SCM scm_logcount (SCM n);
extern SCM scm_integer_length (SCM n);
extern SCM scm_i_mkbig (size_t nlen, int sign);
extern SCM scm_i_big2inum (SCM b, size_t l);
extern SCM scm_i_adjbig (SCM b, size_t nlen);
extern SCM scm_i_normbig (SCM b);
extern SCM scm_i_copybig (SCM b, int sign);
extern SCM scm_i_short2big (short n);
extern SCM scm_i_ushort2big (unsigned short n);
extern SCM scm_i_int2big (int n);
extern SCM scm_i_uint2big (unsigned int n);
extern SCM scm_i_long2big (long n);
extern SCM scm_i_ulong2big (unsigned long n);
extern SCM scm_i_bits2big (scm_bits_t n);
extern SCM scm_i_ubits2big (scm_ubits_t n);
extern SCM scm_i_size2big (size_t n);
extern SCM scm_i_ptrdiff2big (ptrdiff_t n);


#if (SCM_DEBUG_DEPRECATED == 0)
extern SCM scm_big2inum (SCM b, size_t l);
extern SCM scm_mkbig (size_t nlen, int sign);
extern SCM scm_adjbig (SCM b, size_t len);
extern SCM scm_normbig (SCM b);
extern SCM scm_copybig (SCM b, int sign);

#define SCM_FIXNUM_BIT SCM_I_FIXNUM_BIT
#endif

#ifdef HAVE_LONG_LONGS
extern SCM scm_i_long_long2big (long long n);
extern SCM scm_i_ulong_long2big (unsigned long long n);
#endif

#if (SCM_DEBUG_DEPRECATED == 0)
extern SCM scm_2ulong2big (unsigned long * np);
#endif

extern int scm_bigcomp (SCM x, SCM y);
extern long scm_pseudolong (long x);
extern void scm_longdigs (long x, SCM_BIGDIG digs[]);
extern SCM scm_addbig (SCM_BIGDIG *x, size_t nx, int xsgn, SCM bigy, int sgny);
extern SCM scm_mulbig (SCM_BIGDIG *x, size_t nx, SCM_BIGDIG *y, size_t ny, int sgn);
extern unsigned int scm_divbigdig (SCM_BIGDIG *ds, size_t h, SCM_BIGDIG div);
extern size_t scm_iint2str (long num, int rad, char *p);
extern SCM scm_number_to_string (SCM x, SCM radix);
extern int scm_print_real (SCM sexp, SCM port, scm_print_state *pstate);
extern int scm_print_complex (SCM sexp, SCM port, scm_print_state *pstate);
extern int scm_bigprint (SCM exp, SCM port, scm_print_state *pstate);
extern SCM scm_istr2int (char *str, long len, long radix);
extern SCM scm_istr2flo (char *str, long len, long radix);
extern SCM scm_istring2number (char *str, long len, long radix);
extern SCM scm_string_to_number (SCM str, SCM radix);
extern SCM scm_make_real (double x);
extern SCM scm_make_complex (double x, double y);
extern SCM scm_bigequal (SCM x, SCM y);
extern SCM scm_real_equalp (SCM x, SCM y);
extern SCM scm_complex_equalp (SCM x, SCM y);
extern SCM scm_number_p (SCM x);
extern SCM scm_real_p (SCM x);
extern SCM scm_integer_p (SCM x);
extern SCM scm_inexact_p (SCM x);
extern SCM scm_num_eq_p (SCM x, SCM y);
extern SCM scm_less_p (SCM x, SCM y);
extern SCM scm_gr_p (SCM x, SCM y);
extern SCM scm_leq_p (SCM x, SCM y);
extern SCM scm_geq_p (SCM x, SCM y);
extern SCM scm_zero_p (SCM z);
extern SCM scm_positive_p (SCM x);
extern SCM scm_negative_p (SCM x);
extern SCM scm_max (SCM x, SCM y);
extern SCM scm_min (SCM x, SCM y);
extern SCM scm_sum (SCM x, SCM y);
extern SCM scm_difference (SCM x, SCM y);
extern SCM scm_product (SCM x, SCM y);
extern double scm_num2dbl (SCM a, const char * why);
extern SCM scm_divide (SCM x, SCM y);
extern double scm_asinh (double x);
extern double scm_acosh (double x);
extern double scm_atanh (double x);
extern double scm_truncate (double x);
extern double scm_round (double x);
extern double scm_exact_to_inexact (double z);
extern SCM scm_sys_expt (SCM z1, SCM z2);
extern SCM scm_sys_atan2 (SCM z1, SCM z2);
extern SCM scm_make_rectangular (SCM z1, SCM z2);
extern SCM scm_make_polar (SCM z1, SCM z2);
extern SCM scm_real_part (SCM z);
extern SCM scm_imag_part (SCM z);
extern SCM scm_magnitude (SCM z);
extern SCM scm_angle (SCM z);
extern SCM scm_inexact_to_exact (SCM z);
extern SCM scm_trunc (SCM x);
extern SCM scm_i_dbl2big (double d);

#if (SCM_DEBUG_DEPRECATED == 0)
extern SCM scm_dbl2big (double d);
#endif

extern double scm_i_big2dbl (SCM b);

#if (SCM_DEBUG_DEPRECATED == 0)
extern double scm_big2dbl (SCM b);
#endif

extern SCM scm_short2num (short n);
extern SCM scm_ushort2num (unsigned short n);
extern SCM scm_int2num (int n);
extern SCM scm_uint2num (unsigned int n);
extern SCM scm_long2num (long n);
extern SCM scm_ulong2num (unsigned long n);
extern SCM scm_bits2num (scm_bits_t n);
extern SCM scm_ubits2num (scm_ubits_t n);
extern SCM scm_size2num (size_t n);
extern SCM scm_ptrdiff2num (ptrdiff_t n);
extern short scm_num2short (SCM num, unsigned long int pos,
                            const char *s_caller);
extern unsigned short scm_num2ushort (SCM num, unsigned long int pos,
                                      const char *s_caller);
extern int scm_num2int (SCM num, unsigned long int pos,
                        const char *s_caller);
extern unsigned int scm_num2uint (SCM num, unsigned long int pos,
                                  const char *s_caller);
extern long scm_num2long (SCM num, unsigned long int pos,
			  const char *s_caller);
extern unsigned long scm_num2ulong (SCM num, unsigned long int pos,
                                    const char *s_caller);
extern scm_bits_t scm_num2bits (SCM num, unsigned long int pos,
                                const char *s_caller);
extern scm_ubits_t scm_num2ubits (SCM num, unsigned long int pos,
                                  const char *s_caller);
extern ptrdiff_t scm_num2ptrdiff (SCM num, unsigned long int pos,
                                  const char *s_caller);
extern size_t scm_num2size (SCM num, unsigned long int pos,
                            const char *s_caller);
#ifdef HAVE_LONG_LONGS
extern SCM scm_long_long2num (long long sl);
extern SCM scm_ulong_long2num (unsigned long long sl);
extern long long scm_num2long_long (SCM num, unsigned long int pos,
                                    const char *s_caller);
extern unsigned long long scm_num2ulong_long (SCM num, unsigned long int pos,
				      const char *s_caller);
#endif

extern void scm_init_numbers (void);

#endif  /* NUMBERSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
