/* classes: h_files */

#ifndef NUMBERSH
#define NUMBERSH
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


#include "libguile/__scm.h"




/* Immediate Numbers 
 *
 * Inums are exact integer data that fits within an SCM word.
 *
 * SCM_INUMP applies only to values known to be Scheme objects.
 * In particular, SCM_INUMP (SCM_CAR (x)) is valid only if x is known
 * to be a SCM_CONSP.  If x is only known to be a SCM_NIMP, 
 * SCM_INUMP (SCM_CAR (x)) can give wrong answers.
 */

#define SCM_INUMP(x)	(2 & (int)(x))
#define SCM_NINUMP(x) 	(!SCM_INUMP(x))

#ifdef __TURBOC__
/* shifts of more than one are done by a library call, single shifts are
 * performed in registers
 */
# define SCM_MAKINUM(x) ((((x)<<1)<<1)+2L)
#else
# define SCM_MAKINUM(x) (((x)<<2)+2L)
#endif /* def __TURBOC__ */


/* SCM_SRS is signed right shift */
/* Turbo C++ v1.0 has a bug with right shifts of signed longs!
 * It is believed to be fixed in Turbo C++ v1.01
 */
#if (-1==(((-1)<<2)+2)>>2) && (__TURBOC__ != 0x295)
# define SCM_SRS(x, y) ((x)>>y)
# ifdef __TURBOC__
#  define SCM_INUM(x) (((x)>>1)>>1)
# else
#  define SCM_INUM(x) SCM_SRS(x, 2)
# endif /* def __TURBOC__ */
#else
# define SCM_SRS(x, y) (((x)<0) ? ~((~(x))>>y) : (x)>>y)
# define SCM_INUM(x) SCM_SRS(x, 2)
#endif /*  (-1==(((-1)<<2)+2)>>2) && (__TURBOC__ != 0x295) */


/* A name for 0.
 */
#define SCM_INUM0 ((SCM) 2)



/* SCM_FIXABLE is non-0 if its long argument can be encoded in an SCM_INUM.
 */
#define SCM_POSFIXABLE(n) ((n) <= SCM_MOST_POSITIVE_FIXNUM)
#define SCM_NEGFIXABLE(n) ((n) >= SCM_MOST_NEGATIVE_FIXNUM)
#define SCM_UNEGFIXABLE(n) ((n) <= -SCM_MOST_NEGATIVE_FIXNUM)
#define SCM_FIXABLE(n) (SCM_POSFIXABLE(n) && SCM_NEGFIXABLE(n))

/* SCM_INTBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an exact immediate.
 */

#ifndef SCM_CHAR_BIT
# define SCM_CHAR_BIT 8
#endif /* ndef SCM_CHAR_BIT */
#ifndef SCM_LONG_BIT
# define SCM_LONG_BIT (SCM_CHAR_BIT*sizeof(long)/sizeof(char))
#endif /* ndef SCM_LONG_BIT */
#define SCM_INTBUFLEN (5+SCM_LONG_BIT)

/* SCM_FLOBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an inexact number.
 */

#define SCM_FLOBUFLEN (10+2*(sizeof(double)/sizeof(char)*SCM_CHAR_BIT*3+9)/10)




/* Numbers 
 */

#define SCM_INEXP(x) (SCM_TYP16(x)==scm_tc16_flo)
#define SCM_CPLXP(x) (SCM_CAR(x)==scm_tc_dblc)
#define SCM_REAL(x) (*(((scm_dbl *) (SCM2PTR(x)))->real))
#define SCM_IMAG(x) (*((double *)(SCM_CHARS(x)+sizeof(double))))
/* ((&SCM_REAL(x))[1]) */


#ifdef SCM_SINGLES
#define SCM_REALP(x) ((~SCM_REAL_PART & SCM_CAR(x))==scm_tc_flo)
#define SCM_SINGP(x) (SCM_CAR(x)==scm_tc_flo)
#define SCM_FLO(x) (((scm_flo *)(SCM2PTR(x)))->num)
#define SCM_REALPART(x) (SCM_SINGP(x)?0.0+SCM_FLO(x):SCM_REAL(x))
#else /* SCM_SINGLES */
#define SCM_REALP(x) (SCM_CAR(x)==scm_tc_dblr)
#define SCM_REALPART SCM_REAL
#endif /* SCM_SINGLES */


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
# define SCM_DIGSPERLONG ((scm_sizet)((sizeof(long)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))
# define SCM_DIGSPERLONGLONG ((scm_sizet)((sizeof(long long)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))
# define SCM_BIGUP(x) ((unsigned long)(x) << SCM_BITSPERDIG)
# define SCM_LONGLONGBIGUP(x) ((ulong_long)(x) << SCM_BITSPERDIG)
# define SCM_BIGDN(x) ((x) >> SCM_BITSPERDIG)
# define SCM_BIGLO(x) ((x) & (SCM_BIGRAD-1))
#endif /* def BIGNUMS */

#ifndef SCM_BIGDIG
/* Definition is not really used but helps various function
 * prototypes to compile with conditionalization.
 */
# define SCM_BIGDIG unsigned short
# define SCM_NO_BIGDIG
# ifndef SCM_FLOATS
#  define SCM_INUMS_ONLY
# endif /* ndef SCM_FLOATS */
#endif /* ndef SCM_BIGDIG */

#ifdef SCM_FLOATS
#define SCM_NUMBERP(x) (SCM_INUMP(x) || (SCM_NIMP(x) && SCM_NUMP(x)))
#else
#ifdef SCM_BIGDIG
#define SCM_NUMBERP(x) (SCM_INUMP(x) || (SCM_NIMP(x) && SCM_NUMP(x)))
#else
#define SCM_NUMBERP SCM_INUMP
#endif
#endif
#define SCM_NUMP(x) ((0xfcff & (int)SCM_CAR(x))==scm_tc7_smob)
#define SCM_BIGP(x) (SCM_TYP16S(x)==scm_tc16_bigpos)
#define SCM_BIGSIGN(x) (0x0100 & (int)SCM_CAR(x))
#define SCM_BDIGITS(x) ((SCM_BIGDIG *)(SCM_CDR(x)))
#define SCM_NUMDIGS(x) ((scm_sizet)(SCM_CAR(x)>>16))
#define SCM_SETNUMDIGS(x, v, t) SCM_SETCAR(x, (((v)+0L)<<16)+(t))


#ifdef SCM_FLOATS
typedef struct scm_dblproc
{
  char *scm_string;
  double (*cproc) ();
} scm_dblproc;

#ifdef SCM_SINGLES
typedef struct scm_flo
{
  SCM type;
  float num;
} scm_flo;
#endif

typedef struct scm_dbl
{
  SCM type;
  double *real;
} scm_dbl;
#endif





extern SCM scm_exact_p SCM_P ((SCM x));
extern SCM scm_odd_p SCM_P ((SCM n));
extern SCM scm_even_p SCM_P ((SCM n));
extern SCM scm_abs SCM_P ((SCM x));
extern SCM scm_quotient SCM_P ((SCM x, SCM y));
extern SCM scm_remainder SCM_P ((SCM x, SCM y));
extern SCM scm_modulo SCM_P ((SCM x, SCM y));
extern SCM scm_gcd SCM_P ((SCM x, SCM y));
extern SCM scm_lcm SCM_P ((SCM n1, SCM n2));
extern SCM scm_logand SCM_P ((SCM n1, SCM n2));
extern SCM scm_logior SCM_P ((SCM n1, SCM n2));
extern SCM scm_logxor SCM_P ((SCM n1, SCM n2));
extern SCM scm_logtest SCM_P ((SCM n1, SCM n2));
extern SCM scm_logbit_p SCM_P ((SCM n1, SCM n2));
extern SCM scm_lognot SCM_P ((SCM n));
extern SCM scm_integer_expt SCM_P ((SCM z1, SCM z2));
extern SCM scm_ash SCM_P ((SCM n, SCM cnt));
extern SCM scm_bit_extract SCM_P ((SCM n, SCM start, SCM end));
extern SCM scm_logcount SCM_P ((SCM n));
extern SCM scm_integer_length SCM_P ((SCM n));
extern SCM scm_mkbig SCM_P ((scm_sizet nlen, int sign));
extern SCM scm_big2inum SCM_P ((SCM b, scm_sizet l));
extern SCM scm_adjbig SCM_P ((SCM b, scm_sizet nlen));
extern SCM scm_normbig SCM_P ((SCM b));
extern SCM scm_copybig SCM_P ((SCM b, int sign));
extern SCM scm_long2big SCM_P ((long n));
extern SCM scm_long_long2big SCM_P ((long_long n));
extern SCM scm_2ulong2big SCM_P ((unsigned long * np));
extern SCM scm_ulong2big SCM_P ((unsigned long n));
extern int scm_bigcomp SCM_P ((SCM x, SCM y));
extern long scm_pseudolong SCM_P ((long x));
extern void scm_longdigs SCM_P ((long x, SCM_BIGDIG digs[]));
extern SCM scm_addbig SCM_P ((SCM_BIGDIG *x, scm_sizet nx, int xsgn, SCM bigy, int sgny));
extern SCM scm_mulbig SCM_P ((SCM_BIGDIG *x, scm_sizet nx, SCM_BIGDIG *y, scm_sizet ny, int sgn));
extern unsigned int scm_divbigdig SCM_P ((SCM_BIGDIG *ds, scm_sizet h, SCM_BIGDIG div));
extern SCM scm_divbigint SCM_P ((SCM x, long z, int sgn, int mode));
extern SCM scm_divbigbig SCM_P ((SCM_BIGDIG *x, scm_sizet nx, SCM_BIGDIG *y, scm_sizet ny, int sgn, int modes));
extern scm_sizet scm_iint2str SCM_P ((long num, int rad, char *p));
extern SCM scm_number_to_string SCM_P ((SCM x, SCM radix));
extern int scm_floprint SCM_P ((SCM sexp, SCM port, scm_print_state *pstate));
extern int scm_bigprint SCM_P ((SCM exp, SCM port, scm_print_state *pstate));
extern SCM scm_istr2int SCM_P ((char *str, long len, long radix));
extern SCM scm_istr2flo SCM_P ((char *str, long len, long radix));
extern SCM scm_istring2number SCM_P ((char *str, long len, long radix));
extern SCM scm_string_to_number SCM_P ((SCM str, SCM radix));
extern SCM scm_makdbl SCM_P ((double x, double y));
extern SCM scm_bigequal SCM_P ((SCM x, SCM y));
extern SCM scm_floequal SCM_P ((SCM x, SCM y));
extern SCM scm_number_p SCM_P ((SCM x));
extern SCM scm_real_p SCM_P ((SCM x));
extern SCM scm_int_p SCM_P ((SCM x));
extern SCM scm_inexact_p SCM_P ((SCM x));
extern SCM scm_num_eq_p SCM_P ((SCM x, SCM y));
extern SCM scm_less_p SCM_P ((SCM x, SCM y));
extern SCM scm_gr_p SCM_P ((SCM x, SCM y));
extern SCM scm_leq_p SCM_P ((SCM x, SCM y));
extern SCM scm_geq_p SCM_P ((SCM x, SCM y));
extern SCM scm_zero_p SCM_P ((SCM z));
extern SCM scm_positive_p SCM_P ((SCM x));
extern SCM scm_negative_p SCM_P ((SCM x));
extern SCM scm_max SCM_P ((SCM x, SCM y));
extern SCM scm_min SCM_P ((SCM x, SCM y));
extern SCM scm_sum SCM_P ((SCM x, SCM y));
extern SCM scm_difference SCM_P ((SCM x, SCM y));
extern SCM scm_product SCM_P ((SCM x, SCM y));
extern double scm_num2dbl SCM_P ((SCM a, char * why));
extern SCM scm_fuck SCM_P ((SCM a));
extern SCM scm_divide SCM_P ((SCM x, SCM y));
extern double scm_asinh SCM_P ((double x));
extern double scm_acosh SCM_P ((double x));
extern double scm_atanh SCM_P ((double x));
extern double scm_truncate SCM_P ((double x));
extern double scm_round SCM_P ((double x));
extern double scm_exact_to_inexact SCM_P ((double z));
extern SCM scm_sys_expt SCM_P ((SCM z1, SCM z2));
extern SCM scm_sys_atan2 SCM_P ((SCM z1, SCM z2));
extern SCM scm_make_rectangular SCM_P ((SCM z1, SCM z2));
extern SCM scm_make_polar SCM_P ((SCM z1, SCM z2));
extern SCM scm_real_part SCM_P ((SCM z));
extern SCM scm_imag_part SCM_P ((SCM z));
extern SCM scm_magnitude SCM_P ((SCM z));
extern SCM scm_angle SCM_P ((SCM z));
extern SCM scm_inexact_to_exact SCM_P ((SCM z));
extern SCM scm_trunc SCM_P ((SCM x));
extern SCM scm_dbl2big SCM_P ((double d));
extern double scm_big2dbl SCM_P ((SCM b));
extern SCM scm_long2num SCM_P ((long sl));
extern SCM scm_long_long2num SCM_P ((long_long sl));
extern SCM scm_ulong2num SCM_P ((unsigned long sl));
extern long scm_num2long SCM_P ((SCM num, char *pos, char *s_caller));
extern long num2long SCM_P ((SCM num, char *pos, char *s_caller));
extern long_long scm_num2long_long SCM_P ((SCM num, char *pos, char *s_caller));
extern unsigned long scm_num2ulong SCM_P ((SCM num, char *pos, char *s_caller));
extern void scm_init_numbers SCM_P ((void));

#endif  /* NUMBERSH */
