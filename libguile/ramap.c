/* Copyright (C) 1996,1998,2000,2001 Free Software Foundation, Inc.
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

/*
  HWN:FIXME::
  Someone should rename this to arraymap.c; that would reflect the
  contents better.  */





#include "libguile/_scm.h"
#include "libguile/strings.h"
#include "libguile/unif.h"
#include "libguile/smob.h"
#include "libguile/chars.h"
#include "libguile/eq.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/root.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/ramap.h"


typedef struct
{
  char *name;
  SCM sproc;
  int (*vproc) ();
} ra_iproc;


/* These tables are a kluge that will not scale well when more
 * vectorized subrs are added.  It is tempting to steal some bits from
 * the SCM_CAR of all subrs (like those selected by SCM_SMOBNUM) to hold an
 * offset into a table of vectorized subrs.  
 */

static ra_iproc ra_rpsubrs[] =
{
  {"=", SCM_UNDEFINED, scm_ra_eqp},
  {"<", SCM_UNDEFINED, scm_ra_lessp},
  {"<=", SCM_UNDEFINED, scm_ra_leqp},
  {">", SCM_UNDEFINED, scm_ra_grp},
  {">=", SCM_UNDEFINED, scm_ra_greqp},
  {0, 0, 0}
};

static ra_iproc ra_asubrs[] =
{
  {"+", SCM_UNDEFINED, scm_ra_sum},
  {"-", SCM_UNDEFINED, scm_ra_difference},
  {"*", SCM_UNDEFINED, scm_ra_product},
  {"/", SCM_UNDEFINED, scm_ra_divide},
  {0, 0, 0}
};



/* Fast, recycling scm_vector ref */
#define RVREF(ra, i, e) (e = scm_cvref(ra, i, e))

/* #define RVREF(ra, i, e) (scm_cvref(ra, i, SCM_UNDEFINED)) to turn off */

/* IVDEP means "ignore scm_vector dependencies", meaning we guarantee that
   elements of scm_vector operands are not aliased */
#ifdef _UNICOS
#define IVDEP(test, line) if (test) {_Pragma("ivdep"); line} else {line}
#else
#define IVDEP(test, line) line
#endif



/* inds must be a uvect or ivect, no check. */



/*
  Yes, this is really ugly, but it prevents multiple code
 */
#define BINARY_ELTS_CODE(OPERATOR, type) \
do { type *v0 = (type*)SCM_VELTS (ra0);\
     type *v1 = (type*)SCM_VELTS (ra1);\
     IVDEP (ra0 != ra1, \
	    for (; n-- > 0; i0 += inc0, i1 += inc1) \
	       v0[i0] OPERATOR v1[i1];) \
     break; \
} while (0)

/* This macro is used for all but binary division and
   multiplication of complex numbers -- see the expanded
   version in the functions later in this file */
#define BINARY_PAIR_ELTS_CODE(OPERATOR, type) \
do { type (*v0)[2] = (type (*)[2]) SCM_VELTS (ra0);\
     type (*v1)[2] = (type (*)[2]) SCM_VELTS (ra1);\
     IVDEP (ra0 != ra1, \
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {\
	       v0[i0][0] OPERATOR v1[i1][0]; \
	       v0[i0][1] OPERATOR v1[i1][1]; \
            }) \
     break; \
} while (0)

#define UNARY_ELTS_CODE(OPERATOR, type) \
	  do { type *v0 = (type *) SCM_VELTS (ra0);\
	    for (; n-- > 0; i0 += inc0) \
	      v0[i0] OPERATOR v0[i0];\
	    break;\
	  } while (0)


/* This macro is used for all but unary divison 
   of complex numbers -- see the expanded version in the
   function later in this file. */
#define UNARY_PAIR_ELTS_CODE(OPERATOR, type) \
	  do { type (*v0)[2] = (type (*)[2]) SCM_VELTS (ra0);\
	    for (; n-- > 0; i0 += inc0) {\
	      v0[i0][0] OPERATOR v0[i0][0];\
	      v0[i0][1] OPERATOR v0[i0][1];\
            }\
	    break;\
	  } while (0)

static scm_sizet 
cind (SCM ra, SCM inds)
{
  scm_sizet i;
  int k;
  long *ve = (long*) SCM_VELTS (inds);
  if (!SCM_ARRAYP (ra))
    return *ve;
  i = SCM_ARRAY_BASE (ra);
  for (k = 0; k < SCM_ARRAY_NDIM (ra); k++)
    i += (ve[k] - SCM_ARRAY_DIMS (ra)[k].lbnd) * SCM_ARRAY_DIMS (ra)[k].inc;
  return i;
}


/* Checker for scm_array mapping functions:
   return values: 4 --> shapes, increments, and bases are the same;
   3 --> shapes and increments are the same;
   2 --> shapes are the same;
   1 --> ras are at least as big as ra0;
   0 --> no match.
   */

int 
scm_ra_matchp (SCM ra0, SCM ras)
{
  SCM ra1;
  scm_array_dim dims;
  scm_array_dim *s0 = &dims;
  scm_array_dim *s1;
  scm_sizet bas0 = 0;
  int i, ndim = 1;
  int exact = 2			/* 4 */ ;	/* Don't care about values >2 (yet?) */
  if (SCM_IMP (ra0)) return 0;
  switch (SCM_TYP7 (ra0))
    {
    default:
      return 0;
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_string:
    case scm_tc7_byvect:
    case scm_tc7_bvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_svect:
#ifdef HAVE_LONG_LONGS
    case scm_tc7_llvect:
#endif
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
      s0->lbnd = 0;
      s0->inc = 1;
      s0->ubnd = SCM_INUM (scm_uniform_vector_length (ra0)) - 1;
      break;
    case scm_tc7_smob:
      if (!SCM_ARRAYP (ra0))
	return 0;
      ndim = SCM_ARRAY_NDIM (ra0);
      s0 = SCM_ARRAY_DIMS (ra0);
      bas0 = SCM_ARRAY_BASE (ra0);
      break;
    }
  while (SCM_NIMP (ras))
    {
      ra1 = SCM_CAR (ras);
      if (SCM_IMP (ra1))
	return 0;
      switch SCM_TYP7
	(ra1)
	{
	default:
	  return 0;
	case scm_tc7_vector:
	case scm_tc7_wvect:
	case scm_tc7_string:
	case scm_tc7_byvect:
	case scm_tc7_bvect:
	case scm_tc7_uvect:
	case scm_tc7_ivect:
	case scm_tc7_svect:
#ifdef HAVE_LONG_LONGS
	case scm_tc7_llvect:
#endif
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
	  {
	    unsigned long int length;

	    if (1 != ndim)
	      return 0;

	    length = SCM_INUM (scm_uniform_vector_length (ra1));

	    switch (exact)
	      {
	      case 4:
		if (0 != bas0)
		  exact = 3;
	      case 3:
		if (1 != s0->inc)
		  exact = 2;
	      case 2:
		if ((0 == s0->lbnd) && (s0->ubnd == length - 1))
		  break;
		exact = 1;
	      case 1:
		if (s0->lbnd < 0 || s0->ubnd >= length)
		  return 0;
	      }
	    break;
	  }
	case scm_tc7_smob:
	  if (!SCM_ARRAYP (ra1) || ndim != SCM_ARRAY_NDIM (ra1))
	    return 0;
	  s1 = SCM_ARRAY_DIMS (ra1);
	  if (bas0 != SCM_ARRAY_BASE (ra1))
	    exact = 3;
	  for (i = 0; i < ndim; i++)
	    switch (exact)
	      {
	      case 4:
	      case 3:
		if (s0[i].inc != s1[i].inc)
		  exact = 2;
	      case 2:
		if (s0[i].lbnd == s1[i].lbnd && s0[i].ubnd == s1[i].ubnd)
		  break;
		exact = 1;
	      default:
		if (s0[i].lbnd < s1[i].lbnd || s0[i].ubnd > s1[i].ubnd)
		  return (s0[i].lbnd <= s0[i].ubnd ? 0 : 1);
	      }
	  break;
	}
      ras = SCM_CDR (ras);
    }
  return exact;
}

/* array mapper: apply cproc to each dimension of the given arrays?. 
     int (*cproc) ();   procedure to call on unrolled arrays?
			   cproc (dest, source list) or
			   cproc (dest, data, source list).  
     SCM data;          data to give to cproc or unbound. 
     SCM ra0;           destination array.
     SCM lra;           list of source arrays.
     const char *what;  caller, for error reporting. */
int 
scm_ramapc (int (*cproc)(), SCM data, SCM ra0, SCM lra, const char *what)
{
  SCM inds, z;
  SCM vra0, ra1, vra1;
  SCM lvra, *plvra;
  long *vinds;
  int k, kmax;
  switch (scm_ra_matchp (ra0, lra))
    {
    default:
    case 0:
      scm_misc_error (what, "array shape mismatch: ~S", ra0);
    case 2:
    case 3:
    case 4:			/* Try unrolling arrays */
      kmax = (SCM_ARRAYP (ra0) ? SCM_ARRAY_NDIM (ra0) - 1 : 0);
      if (kmax < 0)
	goto gencase;
      vra0 = scm_array_contents (ra0, SCM_UNDEFINED);
      if (SCM_IMP (vra0)) goto gencase;
      if (!SCM_ARRAYP (vra0))
	{
	  unsigned long int length = SCM_INUM (scm_uniform_vector_length (vra0));
	  vra1 = scm_make_ra (1);
	  SCM_ARRAY_BASE (vra1) = 0;
	  SCM_ARRAY_DIMS (vra1)->lbnd = 0;
	  SCM_ARRAY_DIMS (vra1)->ubnd = length - 1;
	  SCM_ARRAY_DIMS (vra1)->inc = 1;
	  SCM_ARRAY_V (vra1) = vra0;
	  vra0 = vra1;
	}
      lvra = SCM_EOL;
      plvra = &lvra;
      for (z = lra; SCM_NIMP (z); z = SCM_CDR (z))
	{
	  ra1 = SCM_CAR (z);
	  vra1 = scm_make_ra (1);
	  SCM_ARRAY_DIMS (vra1)->lbnd = SCM_ARRAY_DIMS (vra0)->lbnd;
	  SCM_ARRAY_DIMS (vra1)->ubnd = SCM_ARRAY_DIMS (vra0)->ubnd;
	  if (!SCM_ARRAYP (ra1))
	    {
	      SCM_ARRAY_BASE (vra1) = 0;
	      SCM_ARRAY_DIMS (vra1)->inc = 1;
	      SCM_ARRAY_V (vra1) = ra1;
	    }
	  else if (!SCM_ARRAY_CONTP (ra1))
	    goto gencase;
	  else
	    {
	      SCM_ARRAY_BASE (vra1) = SCM_ARRAY_BASE (ra1);
	      SCM_ARRAY_DIMS (vra1)->inc = SCM_ARRAY_DIMS (ra1)[kmax].inc;
	      SCM_ARRAY_V (vra1) = SCM_ARRAY_V (ra1);
	    }
	  *plvra = scm_cons (vra1, SCM_EOL);
	  plvra = SCM_CDRLOC (*plvra);
	}
      return (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra));
    case 1:
    gencase:			/* Have to loop over all dimensions. */
    vra0 = scm_make_ra (1);
    if (SCM_ARRAYP (ra0))
      {
	kmax = SCM_ARRAY_NDIM (ra0) - 1;
	if (kmax < 0)
	  {
	    SCM_ARRAY_DIMS (vra0)->lbnd = 0;
	    SCM_ARRAY_DIMS (vra0)->ubnd = 0;
	    SCM_ARRAY_DIMS (vra0)->inc = 1;
	  }
	else
	  {
	    SCM_ARRAY_DIMS (vra0)->lbnd = SCM_ARRAY_DIMS (ra0)[kmax].lbnd;
	    SCM_ARRAY_DIMS (vra0)->ubnd = SCM_ARRAY_DIMS (ra0)[kmax].ubnd;
	    SCM_ARRAY_DIMS (vra0)->inc = SCM_ARRAY_DIMS (ra0)[kmax].inc;
	  }
	SCM_ARRAY_BASE (vra0) = SCM_ARRAY_BASE (ra0);
	SCM_ARRAY_V (vra0) = SCM_ARRAY_V (ra0);
      }
    else
      {
	unsigned long int length = SCM_INUM (scm_uniform_vector_length (ra0));
	kmax = 0;
	SCM_ARRAY_DIMS (vra0)->lbnd = 0;
	SCM_ARRAY_DIMS (vra0)->ubnd = length - 1;
	SCM_ARRAY_DIMS (vra0)->inc = 1;
	SCM_ARRAY_BASE (vra0) = 0;
	SCM_ARRAY_V (vra0) = ra0;
	ra0 = vra0;
      }
    lvra = SCM_EOL;
    plvra = &lvra;
    for (z = lra; SCM_NIMP (z); z = SCM_CDR (z))
      {
	ra1 = SCM_CAR (z);
	vra1 = scm_make_ra (1);
	SCM_ARRAY_DIMS (vra1)->lbnd = SCM_ARRAY_DIMS (vra0)->lbnd;
	SCM_ARRAY_DIMS (vra1)->ubnd = SCM_ARRAY_DIMS (vra0)->ubnd;
	if (SCM_ARRAYP (ra1))
	  {
	    if (kmax >= 0)
	      SCM_ARRAY_DIMS (vra1)->inc = SCM_ARRAY_DIMS (ra1)[kmax].inc;
	    SCM_ARRAY_V (vra1) = SCM_ARRAY_V (ra1);
	  }
	else
	  {
	    SCM_ARRAY_DIMS (vra1)->inc = 1;
	    SCM_ARRAY_V (vra1) = ra1;
	  }
	*plvra = scm_cons (vra1, SCM_EOL);
	plvra = SCM_CDRLOC (*plvra);
      }
    inds = scm_make_uve (SCM_ARRAY_NDIM (ra0), SCM_MAKINUM (-1L));
    vinds = (long *) SCM_VELTS (inds);
    for (k = 0; k <= kmax; k++)
      vinds[k] = SCM_ARRAY_DIMS (ra0)[k].lbnd;
    k = kmax;
    do
      {
	if (k == kmax)
	  {
	    SCM y = lra;
	    SCM_ARRAY_BASE (vra0) = cind (ra0, inds);
	    for (z = lvra; SCM_NIMP (z); z = SCM_CDR (z), y = SCM_CDR (y))
	      SCM_ARRAY_BASE (SCM_CAR (z)) = cind (SCM_CAR (y), inds);
	    if (0 == (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra)))
	      return 0;
	    k--;
	    continue;
	  }
	if (vinds[k] < SCM_ARRAY_DIMS (ra0)[k].ubnd)
	  {
	    vinds[k]++;
	    k++;
	    continue;
	  }
	vinds[k] = SCM_ARRAY_DIMS (ra0)[k].lbnd - 1;
	k--;
      }
    while (k >= 0);
    return 1;
    }
}


SCM_DEFINE (scm_array_fill_x, "array-fill!", 2, 0, 0,
	    (SCM ra, SCM fill),
	    "Stores @var{fill} in every element of @var{array}.  The value returned\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_array_fill_x
{
  scm_ramapc (scm_array_fill_int, fill, ra, SCM_EOL, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* to be used as cproc in scm_ramapc to fill an array dimension with
   "fill". */
int 
scm_array_fill_int (SCM ra, SCM fill, SCM ignore)
#define FUNC_NAME s_scm_array_fill_x
{
  scm_sizet i;
  scm_sizet n = SCM_ARRAY_DIMS (ra)->ubnd - SCM_ARRAY_DIMS (ra)->lbnd + 1;
  long inc = SCM_ARRAY_DIMS (ra)->inc;
  scm_sizet base = SCM_ARRAY_BASE (ra);

  ra = SCM_ARRAY_V (ra);
  switch SCM_TYP7 (ra)
    {
    default:
      for (i = base; n--; i += inc)
	scm_array_set_x (ra, fill, SCM_MAKINUM (i));
      break;
    case scm_tc7_vector:
    case scm_tc7_wvect:
      for (i = base; n--; i += inc)
	SCM_VELTS (ra)[i] = fill;
      break;
    case scm_tc7_string:
      SCM_ASRTGO (SCM_CHARP (fill), badarg2);
      for (i = base; n--; i += inc)
	SCM_STRING_CHARS (ra)[i] = SCM_CHAR (fill);
      break;
    case scm_tc7_byvect:
      if (SCM_CHARP (fill))
	fill = SCM_MAKINUM ((char) SCM_CHAR (fill));
      SCM_ASRTGO (SCM_INUMP (fill)
		  && -128 <= SCM_INUM (fill) && SCM_INUM (fill) < 128,
		  badarg2);
      for (i = base; n--; i += inc)
	((char *) SCM_UVECTOR_BASE (ra))[i] = SCM_INUM (fill);
      break;
    case scm_tc7_bvect:
      { /* scope */
	long *ve = (long *) SCM_VELTS (ra);
	if (1 == inc && (n >= SCM_LONG_BIT || n == SCM_BITVECTOR_LENGTH (ra)))
	  {
	    i = base / SCM_LONG_BIT;
	    if (SCM_FALSEP (fill))
	      {
		if (base % SCM_LONG_BIT) /* leading partial word */
		  ve[i++] &= ~(~0L << (base % SCM_LONG_BIT));
		for (; i < (base + n) / SCM_LONG_BIT; i++)
		  ve[i] = 0L;
		if ((base + n) % SCM_LONG_BIT) /* trailing partial word */
		  ve[i] &= (~0L << ((base + n) % SCM_LONG_BIT));
	      }
	    else if (SCM_EQ_P (fill, SCM_BOOL_T))
	      {
		if (base % SCM_LONG_BIT)
		  ve[i++] |= ~0L << (base % SCM_LONG_BIT);
		for (; i < (base + n) / SCM_LONG_BIT; i++)
		  ve[i] = ~0L;
		if ((base + n) % SCM_LONG_BIT)
		  ve[i] |= ~(~0L << ((base + n) % SCM_LONG_BIT));
	      }
	    else
	      badarg2:SCM_WRONG_TYPE_ARG (2, fill);
	  }
	else
	  {
	    if (SCM_FALSEP (fill))
	      for (i = base; n--; i += inc)
		ve[i / SCM_LONG_BIT] &= ~(1L << (i % SCM_LONG_BIT));
	    else if (SCM_EQ_P (fill, SCM_BOOL_T))
	      for (i = base; n--; i += inc)
		ve[i / SCM_LONG_BIT] |= (1L << (i % SCM_LONG_BIT));
	    else
	      goto badarg2;
	  }
	break;
      }
    case scm_tc7_uvect:
      { /* scope */
	unsigned long f = SCM_NUM2ULONG (2, fill);
	unsigned long *ve = (unsigned long *) SCM_VELTS (ra);

	for (i = base; n--; i += inc)
	  ve[i] = f;
	break;
      }
    case scm_tc7_ivect:
      { /* scope */
	long f = SCM_NUM2LONG (2, fill);
	long *ve = (long *) SCM_VELTS (ra);

	for (i = base; n--; i += inc)
	  ve[i] = f;
	break;
      }
    case scm_tc7_svect:
      SCM_ASRTGO (SCM_INUMP (fill), badarg2);
      { /* scope */
	short f = SCM_INUM (fill);
	short *ve = (short *) SCM_VELTS (ra);

	if (f != SCM_INUM (fill))
	  SCM_OUT_OF_RANGE (2, fill);
	for (i = base; n--; i += inc)
	  ve[i] = f;
	break;
      }
#ifdef HAVE_LONG_LONGS
    case scm_tc7_llvect:
      { /* scope */
	long long f = SCM_NUM2LONG_LONG (2, fill);
	long long *ve = (long long *) SCM_VELTS (ra);

	for (i = base; n--; i += inc)
	  ve[i] = f;
	break;
      }
#endif
    case scm_tc7_fvect:
      { /* scope */
	float f, *ve = (float *) SCM_VELTS (ra);
	SCM_ASRTGO (SCM_REALP (fill), badarg2);
	f = SCM_REAL_VALUE (fill);
	for (i = base; n--; i += inc)
	  ve[i] = f;
	break;
      }
    case scm_tc7_dvect:
      { /* scope */
	double f, *ve = (double *) SCM_VELTS (ra);
	SCM_ASRTGO (SCM_REALP (fill), badarg2);
	f = SCM_REAL_VALUE (fill);
	for (i = base; n--; i += inc)
	  ve[i] = f;
	break;
      }
    case scm_tc7_cvect:
      { /* scope */
	double fr, fi;
	double (*ve)[2] = (double (*)[2]) SCM_VELTS (ra);
	SCM_ASRTGO (SCM_INEXACTP (fill), badarg2);
	if (SCM_REALP (fill)) {
	  fr = SCM_REAL_VALUE (fill);
	  fi = 0.0;
	} else {
	  fr = SCM_COMPLEX_REAL (fill);
	  fi = SCM_COMPLEX_IMAG (fill);
	}
	for (i = base; n--; i += inc)
	  {
	    ve[i][0] = fr;
	    ve[i][1] = fi;
	  }
	break;
      }
    }
  return 1;
}
#undef FUNC_NAME



static int 
racp (SCM src, SCM dst)
{
  long n = (SCM_ARRAY_DIMS (src)->ubnd - SCM_ARRAY_DIMS (src)->lbnd + 1);
  long inc_d, inc_s = SCM_ARRAY_DIMS (src)->inc;
  scm_sizet i_d, i_s = SCM_ARRAY_BASE (src);
  dst = SCM_CAR (dst);
  inc_d = SCM_ARRAY_DIMS (dst)->inc;
  i_d = SCM_ARRAY_BASE (dst);
  src = SCM_ARRAY_V (src);
  dst = SCM_ARRAY_V (dst);

  switch SCM_TYP7 (dst)
    {
    default:
    gencase:
    case scm_tc7_vector:
    case scm_tc7_wvect:

      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	scm_array_set_x (dst,
			 scm_cvref (src, i_s, SCM_UNDEFINED),
			 SCM_MAKINUM (i_d));
      break;
    case scm_tc7_string:
      if (SCM_TYP7 (src) != scm_tc7_string)
	goto gencase;
      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	SCM_STRING_CHARS (dst)[i_d] = SCM_STRING_CHARS (src)[i_s];
      break;
    case scm_tc7_byvect:
      if (SCM_TYP7 (src) != scm_tc7_byvect)
	goto gencase;
      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	((char *) SCM_UVECTOR_BASE (dst))[i_d]
	  = ((char *) SCM_UVECTOR_BASE (src))[i_s];
      break;
    case scm_tc7_bvect:
      if (SCM_TYP7 (src) != scm_tc7_bvect)
	goto gencase;
      if (1 == inc_d && 1 == inc_s && i_s % SCM_LONG_BIT == i_d % SCM_LONG_BIT && n >= SCM_LONG_BIT)
	{
	  long *sv = (long *) SCM_VELTS (src);
	  long *dv = (long *) SCM_VELTS (dst);
	  sv += i_s / SCM_LONG_BIT;
	  dv += i_d / SCM_LONG_BIT;
	  if (i_s % SCM_LONG_BIT)
	    {			/* leading partial word */
	      *dv = (*dv & ~(~0L << (i_s % SCM_LONG_BIT))) | (*sv & (~0L << (i_s % SCM_LONG_BIT)));
	      dv++;
	      sv++;
	      n -= SCM_LONG_BIT - (i_s % SCM_LONG_BIT);
	    }
	  IVDEP (src != dst,
		 for (; n >= SCM_LONG_BIT; n -= SCM_LONG_BIT, sv++, dv++)
		 *dv = *sv;)
	    if (n)		/* trailing partial word */
	      *dv = (*dv & (~0L << n)) | (*sv & ~(~0L << n));
	}
      else
	{
	  for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	    if (SCM_BITVEC_REF(src, i_s)) 
	      SCM_BITVEC_SET(dst, i_d); 
	    else
	      SCM_BITVEC_CLR(dst, i_d);
	}
      break;
    case scm_tc7_uvect:
      if (scm_tc7_uvect != SCM_TYP7 (src))
	goto gencase;
      else
	{
	  long *d = (long *) SCM_VELTS (dst), *s = (long *) SCM_VELTS (src);
	  IVDEP (src != dst,
		 for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		 d[i_d] = s[i_s];)
	    break;
	}
    case scm_tc7_ivect:
      if (scm_tc7_uvect != SCM_TYP7 (src) && scm_tc7_ivect != SCM_TYP7 (src))
	goto gencase;
      else
	{
	  long *d = (long *) SCM_VELTS (dst), *s = (long *) SCM_VELTS (src);
	  IVDEP (src != dst,
		 for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		 d[i_d] = s[i_s];)
	    break;
	}
    case scm_tc7_fvect:
      {
	float *d = (float *) SCM_VELTS (dst);
	float *s = (float *) SCM_VELTS (src);
	switch SCM_TYP7
	  (src)
	  {
	  default:
	    goto gencase;
	  case scm_tc7_ivect:
	  case scm_tc7_uvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		   d[i_d] = ((long *) s)[i_s];)
	      break;
	  case scm_tc7_fvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		   d[i_d] = s[i_s];)
	      break;
	  case scm_tc7_dvect:
	    IVDEP (src !=dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		   d[i_d] = ((double *) s)[i_s];)
	      break;
	  }
	break;
      }
    case scm_tc7_dvect:
      {
	double *d = (double *) SCM_VELTS (dst);
	double *s = (double *) SCM_VELTS (src);
	switch SCM_TYP7
	  (src)
	  {
	  default:
	    goto gencase;
	  case scm_tc7_ivect:
	  case scm_tc7_uvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		   d[i_d] = ((long *) s)[i_s];)
	      break;
	  case scm_tc7_fvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		   d[i_d] = ((float *) s)[i_s];)
	      break;
	  case scm_tc7_dvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
		   d[i_d] = s[i_s];)
	      break;
	  }
	break;
      }
    case scm_tc7_cvect:
      {
	double (*d)[2] = (double (*)[2]) SCM_VELTS (dst);
	double (*s)[2] = (double (*)[2]) SCM_VELTS (src);
	switch SCM_TYP7
	  (src)
	  {
	  default:
	    goto gencase;
	  case scm_tc7_ivect:
	  case scm_tc7_uvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	           {
		     d[i_d][0] = ((long *) s)[i_s];
		     d[i_d][1] = 0.0;
		   })
	      break;
	  case scm_tc7_fvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	           {
		     d[i_d][0] = ((float *) s)[i_s];
		     d[i_d][1] = 0.0;
		   })
	      break;
	  case scm_tc7_dvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	           {
		     d[i_d][0] = ((double *) s)[i_s];
		     d[i_d][1] = 0.0;
		   })
	      break;
	  case scm_tc7_cvect:
	    IVDEP (src != dst,
		   for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	           {
		     d[i_d][0] = s[i_s][0];
		     d[i_d][1] = s[i_s][1];
		   })
	      }
	break;
      }
    }
  return 1;
}


SCM_REGISTER_PROC(s_array_copy_in_order_x, "array-copy-in-order!", 2, 0, 0, scm_array_copy_x);


SCM_DEFINE (scm_array_copy_x, "array-copy!", 2, 0, 0,
	    (SCM src, SCM dst),
	    "@deffnx primitive array-copy-in-order! src dst\n"
	    "Copies every element from vector or array @var{source} to the\n"
	    "corresponding element of @var{destination}.  @var{destination} must have\n"
	    "the same rank as @var{source}, and be at least as large in each\n"
	    "dimension.  The order is unspecified.")
#define FUNC_NAME s_scm_array_copy_x
{
  scm_ramapc (racp, SCM_UNDEFINED, src, scm_cons (dst, SCM_EOL), FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Functions callable by ARRAY-MAP! */


int
scm_ra_eqp (SCM ra0, SCM ras)
{
  SCM ra1 = SCM_CAR (ras), ra2 = SCM_CAR (SCM_CDR (ras));
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0), i1 = SCM_ARRAY_BASE (ra1), i2 = SCM_ARRAY_BASE (ra2);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  ra1 = SCM_ARRAY_V (ra1);
  ra2 = SCM_ARRAY_V (ra2);
  switch (SCM_TYP7 (ra1) == SCM_TYP7 (ra2) ? SCM_TYP7 (ra1) : 0)
    {
    default:
      {
	SCM e1 = SCM_UNDEFINED, e2 = SCM_UNDEFINED;
	for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	  if (SCM_BITVEC_REF (ra0, i0))
	    if (SCM_FALSEP(scm_eq_p (RVREF (ra1, i1, e1), RVREF (ra2, i2, e2))))
	      SCM_BITVEC_CLR (ra0, i0);
	break;
      }
    case scm_tc7_uvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF (ra0, i0))
	  if (((unsigned long *) SCM_VELTS (ra1))[i1] != ((unsigned long *) SCM_VELTS (ra2))[i2])
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    case scm_tc7_ivect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF (ra0, i0))
	  if (((signed long *) SCM_VELTS (ra1))[i1] != ((signed long *) SCM_VELTS (ra2))[i2])
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    case scm_tc7_fvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF (ra0, i0))
	  if (((float *) SCM_VELTS (ra1))[i1] != ((float *) SCM_VELTS (ra2))[i2])
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    case scm_tc7_dvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF (ra0, i0))
	  if (((double *) SCM_VELTS (ra1))[i1] != ((double *) SCM_VELTS (ra2))[i2])
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    case scm_tc7_cvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF (ra0, i0))
	  if (((double *) SCM_VELTS (ra1))[2 * i1] != ((double *) SCM_VELTS (ra2))[2 * i2] ||
	      ((double *) SCM_VELTS (ra1))[2 * i1 + 1] != ((double *) SCM_VELTS (ra2))[2 * i2 + 1])
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    }
  return 1;
}

/* opt 0 means <, nonzero means >= */

static int
ra_compare (SCM ra0,SCM ra1,SCM ra2,int opt)
{
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0), i1 = SCM_ARRAY_BASE (ra1), i2 = SCM_ARRAY_BASE (ra2);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  ra1 = SCM_ARRAY_V (ra1);
  ra2 = SCM_ARRAY_V (ra2);
  switch (SCM_TYP7 (ra1) == SCM_TYP7 (ra2) ? SCM_TYP7 (ra1) : 0)
    {
    default:
      {
	SCM e1 = SCM_UNDEFINED, e2 = SCM_UNDEFINED;
	for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	  if (SCM_BITVEC_REF (ra0, i0))
	    if (opt ?
		SCM_NFALSEP (scm_less_p (RVREF (ra1, i1, e1), RVREF (ra2, i2, e2))) :
		SCM_FALSEP (scm_less_p (RVREF (ra1, i1, e1), RVREF (ra2, i2, e2))))
	      SCM_BITVEC_CLR (ra0, i0);
	break;
      }
    case scm_tc7_uvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	{
	  if (SCM_BITVEC_REF (ra0, i0))
	    if (opt ?
		((unsigned long *) SCM_VELTS (ra1))[i1] < ((unsigned long *) SCM_VELTS (ra2))[i2] :
		((unsigned long *) SCM_VELTS (ra1))[i1] >= ((unsigned long *) SCM_VELTS (ra2))[i2])
	      SCM_BITVEC_CLR (ra0, i0);
	}
      break;
    case scm_tc7_ivect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	{
	  if (SCM_BITVEC_REF (ra0, i0))
	    if (opt ?
		((signed long *) SCM_VELTS (ra1))[i1] < ((signed long *) SCM_VELTS (ra2))[i2] :
		((signed long *) SCM_VELTS (ra1))[i1] >= ((signed long *) SCM_VELTS (ra2))[i2])
	      SCM_BITVEC_CLR (ra0, i0);
	}
      break;
    case scm_tc7_fvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF(ra0, i0))
	  if (opt ?
	      ((float *) SCM_VELTS (ra1))[i1] < ((float *) SCM_VELTS (ra2))[i2] :
	      ((float *) SCM_VELTS (ra1))[i1] >= ((float *) SCM_VELTS (ra2))[i2])
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    case scm_tc7_dvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF (ra0, i0))
	  if (opt ?
	      ((double *) SCM_VELTS (ra1))[i1] < ((double *) SCM_VELTS (ra2))[i2] :
	      ((double *) SCM_VELTS (ra1))[i1] >= ((double *) SCM_VELTS (ra2))[i2])
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    }
  return 1;
}



int
scm_ra_lessp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 0);
}


int
scm_ra_leqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 1);
}


int
scm_ra_grp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 0);
}


int
scm_ra_greqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 1);
}


int
scm_ra_sum (SCM ra0, SCM ras)
{
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  if (SCM_NNULLP(ras))
    {
      SCM ra1 = SCM_CAR (ras);
      scm_sizet i1 = SCM_ARRAY_BASE (ra1);
      long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    SCM e0 = SCM_UNDEFINED, e1 = SCM_UNDEFINED;
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      scm_array_set_x (ra0, scm_sum (RVREF (ra0, i0, e0), RVREF (ra1, i1, e1)),
			       SCM_MAKINUM (i0));
	    break;
	  }
	case scm_tc7_uvect:
	case scm_tc7_ivect:
          BINARY_ELTS_CODE( +=, long);
	case scm_tc7_fvect:
          BINARY_ELTS_CODE( +=, float);
	case scm_tc7_dvect:
          BINARY_ELTS_CODE( +=, double);
	case scm_tc7_cvect:
          BINARY_PAIR_ELTS_CODE( +=, double); 
	}
    }
  return 1;
}



int
scm_ra_difference (SCM ra0, SCM ras)
{
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  if (SCM_NULLP (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    SCM e0 = SCM_UNDEFINED;
	    for (; n-- > 0; i0 += inc0)
	      scm_array_set_x (ra0, 
                               scm_difference (RVREF (ra0, i0, e0), SCM_UNDEFINED), 
                               SCM_MAKINUM (i0));
	    break;
	  }
	case scm_tc7_fvect:
          UNARY_ELTS_CODE( =  -, float);
	case scm_tc7_dvect:
          UNARY_ELTS_CODE( =  -, double);
	case scm_tc7_cvect:
          UNARY_PAIR_ELTS_CODE( = -, double);
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      scm_sizet i1 = SCM_ARRAY_BASE (ra1);
      long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    SCM e0 = SCM_UNDEFINED, e1 = SCM_UNDEFINED;
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      scm_array_set_x (ra0, scm_difference (RVREF (ra0, i0, e0), RVREF (ra1, i1, e1)), SCM_MAKINUM (i0));
	    break;
	  }
	case scm_tc7_fvect:
          BINARY_ELTS_CODE( -=, float);
	case scm_tc7_dvect:
          BINARY_ELTS_CODE( -=, double);
	case scm_tc7_cvect:
          BINARY_PAIR_ELTS_CODE( -=, double);
	}
    }
  return 1;
}



int
scm_ra_product (SCM ra0, SCM ras)
{
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  if (SCM_NNULLP (ras))
    {
      SCM ra1 = SCM_CAR (ras);
      scm_sizet i1 = SCM_ARRAY_BASE (ra1);
      long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    SCM e0 = SCM_UNDEFINED, e1 = SCM_UNDEFINED;
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      scm_array_set_x (ra0, scm_product (RVREF (ra0, i0, e0), RVREF (ra1, i1, e1)),
			       SCM_MAKINUM (i0));
	    break;
	  }
	case scm_tc7_uvect:
	case scm_tc7_ivect:
          BINARY_ELTS_CODE( *=, long);
	case scm_tc7_fvect:
          BINARY_ELTS_CODE( *=, float);
	case scm_tc7_dvect:
          BINARY_ELTS_CODE( *=, double);
	case scm_tc7_cvect:
	  {
	    double (*v0)[2] = (double (*)[2]) SCM_VELTS (ra0);
	    register double r;
	    double (*v1)[2] = (double (*)[2]) SCM_VELTS (ra1);
	    IVDEP (ra0 != ra1,
		   for (; n-- > 0; i0 += inc0, i1 += inc1)
	      {
		r = v0[i0][0] * v1[i1][0] - v0[i0][1] * v1[i1][1];
		v0[i0][1] = v0[i0][0] * v1[i1][1] + v0[i0][1] * v1[i1][0];
		v0[i0][0] = r;
	      }
		   );
	    break;
	  }
	}
    }
  return 1;
}


int
scm_ra_divide (SCM ra0, SCM ras)
{
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  if (SCM_NULLP (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    SCM e0 = SCM_UNDEFINED;
	    for (; n-- > 0; i0 += inc0)
	      scm_array_set_x (ra0, scm_divide (RVREF (ra0, i0, e0), SCM_UNDEFINED), SCM_MAKINUM (i0));
	    break;
	  }
	case scm_tc7_fvect:
          UNARY_ELTS_CODE( = 1.0 / , float);
	case scm_tc7_dvect:
          UNARY_ELTS_CODE( = 1.0 / , double);
	case scm_tc7_cvect:
	  {
	    register double d;
	    double (*v0)[2] = (double (*)[2]) SCM_VELTS (ra0);
	    for (; n-- > 0; i0 += inc0)
	      {
		d = v0[i0][0] * v0[i0][0] + v0[i0][1] * v0[i0][1];
		v0[i0][0] /= d;
		v0[i0][1] /= -d;
	      }
	    break;
	  }
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      scm_sizet i1 = SCM_ARRAY_BASE (ra1);
      long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    SCM e0 = SCM_UNDEFINED, e1 = SCM_UNDEFINED;
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      scm_array_set_x (ra0, scm_divide (RVREF (ra0, i0, e0), RVREF (ra1, i1, e1)), SCM_MAKINUM (i0));
	    break;
	  }
	case scm_tc7_fvect:
          BINARY_ELTS_CODE( /=, float);
	case scm_tc7_dvect:
          BINARY_ELTS_CODE( /=, double);
	case scm_tc7_cvect:
	  {
	    register double d, r;
	    double (*v0)[2] = (double (*)[2]) SCM_VELTS (ra0);
	    double (*v1)[2] = (double (*)[2]) SCM_VELTS (ra1);
	    IVDEP (ra0 != ra1,
		   for (; n-- > 0; i0 += inc0, i1 += inc1)
	      {
		d = v1[i1][0] * v1[i1][0] + v1[i1][1] * v1[i1][1];
		r = (v0[i0][0] * v1[i1][0] + v0[i0][1] * v1[i1][1]) / d;
		v0[i0][1] = (v0[i0][1] * v1[i1][0] - v0[i0][0] * v1[i1][1]) / d;
		v0[i0][0] = r;
	      }
		   )
	      break;
	  }
	}
    }
  return 1;
}


int
scm_array_identity (SCM dst, SCM src)
{
  return racp (SCM_CAR (src), scm_cons (dst, SCM_EOL));
}



static int 
ramap (SCM ra0,SCM proc,SCM ras)
{
  long i = SCM_ARRAY_DIMS (ra0)->lbnd;
  long inc = SCM_ARRAY_DIMS (ra0)->inc;
  long n = SCM_ARRAY_DIMS (ra0)->ubnd;
  long base = SCM_ARRAY_BASE (ra0) - i * inc;
  ra0 = SCM_ARRAY_V (ra0);
  if (SCM_NULLP (ras))
    for (; i <= n; i++)
      scm_array_set_x (ra0, scm_apply (proc, SCM_EOL, SCM_EOL), SCM_MAKINUM (i * inc + base));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      SCM args, *ve = &ras;
      scm_sizet k, i1 = SCM_ARRAY_BASE (ra1);
      long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
      ras = SCM_CDR (ras);
      if (SCM_NULLP(ras))
	ras = scm_nullvect;
      else
	{
	  ras = scm_vector (ras);
	  ve = SCM_VELTS (ras);
	}
      for (; i <= n; i++, i1 += inc1)
	{
	  args = SCM_EOL;
	  for (k = SCM_INUM (scm_uniform_vector_length (ras)); k--;)
	    args = scm_cons (scm_uniform_vector_ref (ve[k], SCM_MAKINUM (i)), args);
	  args = scm_cons (scm_cvref (ra1, i1, SCM_UNDEFINED), args);
	  scm_array_set_x (ra0, scm_apply (proc, args, SCM_EOL), SCM_MAKINUM (i * inc + base));
	}
    }
  return 1;
}


static int
ramap_cxr (SCM ra0,SCM proc,SCM ras)
{
  SCM ra1 = SCM_CAR (ras);
  SCM e1 = SCM_UNDEFINED;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0), i1 = SCM_ARRAY_BASE (ra1);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc, inc1 = SCM_ARRAY_DIMS (ra1)->inc;
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra1)->lbnd + 1;
  ra0 = SCM_ARRAY_V (ra0);
  ra1 = SCM_ARRAY_V (ra1);
  switch (SCM_TYP7 (ra0))
    {
    default:
    gencase:
 for (; n-- > 0; i0 += inc0, i1 += inc1)
   scm_array_set_x (ra0, scm_apply (proc, RVREF (ra1, i1, e1), scm_listofnull), SCM_MAKINUM (i0));
 break;
    case scm_tc7_fvect:
      {
	float *dst = (float *) SCM_VELTS (ra0);
	switch (SCM_TYP7 (ra1))
	  {
	  default:
	    goto gencase;
	  case scm_tc7_fvect:
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      dst[i0] = SCM_DSUBRF (proc) ((double) ((float *) SCM_VELTS (ra1))[i1]);
	    break;
	  case scm_tc7_uvect:
	  case scm_tc7_ivect:
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      dst[i0] = SCM_DSUBRF (proc) (SCM_UNPACK (SCM_VELTS (ra1)[i1]));
	    break;
	  }
	break;
      }
    case scm_tc7_dvect:
      {
	double *dst = (double *) SCM_VELTS (ra0);
	switch (SCM_TYP7 (ra1))
	  {
	  default:
	    goto gencase;
	  case scm_tc7_dvect:
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      dst[i0] = SCM_DSUBRF (proc) (((double *) SCM_VELTS (ra1))[i1]);
	    break;
	  case scm_tc7_uvect:
	  case scm_tc7_ivect:
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      dst[i0] = SCM_DSUBRF (proc) (SCM_UNPACK (SCM_VELTS (ra1)[i1]));
	    break;
	  }
	break;
      }
    }
  return 1;
}



static int
ramap_rp (SCM ra0,SCM proc,SCM ras)
{
  SCM ra1 = SCM_CAR (ras), ra2 = SCM_CAR (SCM_CDR (ras));
  SCM e1 = SCM_UNDEFINED, e2 = SCM_UNDEFINED;
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0), i1 = SCM_ARRAY_BASE (ra1), i2 = SCM_ARRAY_BASE (ra2);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  ra1 = SCM_ARRAY_V (ra1);
  ra2 = SCM_ARRAY_V (ra2);
  switch (SCM_TYP7 (ra1) == SCM_TYP7 (ra2) ? SCM_TYP7 (ra1) : 0)
    {
    default:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF  (ra0, i0))
	  if (SCM_FALSEP (SCM_SUBRF (proc) (RVREF (ra1, i1, e1), RVREF (ra2, i2, e2))))
	    SCM_BITVEC_CLR (ra0, i0);
      break;
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	if (SCM_BITVEC_REF (ra0, i0))
	  {
	    /* DIRK:FIXME:: There should be a way to access the elements
	       of a cell as raw data.  Further:  How can we be sure that
	       the values fit into an inum?
	     */
	    SCM n1 = SCM_MAKINUM (((long *) SCM_UNPACK (SCM_CDR (ra1)))[i1]);
	    SCM n2 = SCM_MAKINUM (((long *) SCM_UNPACK (SCM_CDR (ra2)))[i2]);
	    if (SCM_FALSEP (SCM_SUBRF (proc) (n1, n2)));
	      SCM_BITVEC_CLR (ra0, i0);
	  }
      break;
    case scm_tc7_fvect:
      {
	SCM a1 = scm_make_real (1.0), a2 = scm_make_real (1.0);
	for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	  if (SCM_BITVEC_REF (ra0, i0))
	    {
	      SCM_REAL_VALUE (a1) = ((float *) SCM_VELTS (ra1))[i1];
	      SCM_REAL_VALUE (a2) = ((float *) SCM_VELTS (ra2))[i2];
	      if (SCM_FALSEP (SCM_SUBRF (proc) (a1, a2)))
		SCM_BITVEC_CLR (ra0, i0);
	    }
	break;
      }
    case scm_tc7_dvect:
      {
	SCM a1 = scm_make_real (1.0 / 3.0);
	SCM a2 = scm_make_real (1.0 / 3.0);
	for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	  if (SCM_BITVEC_REF (ra0, i0))
	    {
	      SCM_REAL_VALUE (a1) = ((double *) SCM_VELTS (ra1))[i1];
	      SCM_REAL_VALUE (a2) = ((double *) SCM_VELTS (ra2))[i2];
	      if (SCM_FALSEP (SCM_SUBRF (proc) (a1, a2)))
		SCM_BITVEC_CLR (ra0, i0);
	    }
	break;
      }
    case scm_tc7_cvect:
      {
	SCM a1 = scm_make_complex (1.0, 1.0);
	SCM a2 = scm_make_complex (1.0, 1.0);
	for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	  if (SCM_BITVEC_REF (ra0, i0))
	    {
	      SCM_COMPLEX_REAL (a1) = ((double *) SCM_VELTS (ra1))[2 * i1];
	      SCM_COMPLEX_IMAG (a1) = ((double *) SCM_VELTS (ra1))[2 * i1 + 1];
	      SCM_COMPLEX_REAL (a2) = ((double *) SCM_VELTS (ra2))[2 * i2];
	      SCM_COMPLEX_IMAG (a2) = ((double *) SCM_VELTS (ra2))[2 * i2 + 1];
	      if (SCM_FALSEP (SCM_SUBRF (proc) (a1, a2)))
		SCM_BITVEC_CLR (ra0, i0);
	    }
	break;
      }
    }
  return 1;
}



static int
ramap_1 (SCM ra0,SCM proc,SCM ras)
{
  SCM ra1 = SCM_CAR (ras);
  SCM e1 = SCM_UNDEFINED;
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0), i1 = SCM_ARRAY_BASE (ra1);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc, inc1 = SCM_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  ra1 = SCM_ARRAY_V (ra1);
  if (scm_tc7_vector == SCM_TYP7 (ra0) || scm_tc7_wvect == SCM_TYP7 (ra0))
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      scm_array_set_x (ra0, SCM_SUBRF (proc) (scm_cvref (ra1, i1, SCM_UNDEFINED)), SCM_MAKINUM (i0));
  else
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      scm_array_set_x (ra0, SCM_SUBRF (proc) (RVREF (ra1, i1, e1)), SCM_MAKINUM (i0));
  return 1;
}



static int
ramap_2o (SCM ra0,SCM proc,SCM ras)
{
  SCM ra1 = SCM_CAR (ras);
  SCM e1 = SCM_UNDEFINED;
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0), i1 = SCM_ARRAY_BASE (ra1);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc, inc1 = SCM_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  ra1 = SCM_ARRAY_V (ra1);
  ras = SCM_CDR (ras);
  if (SCM_NULLP (ras))
    {
      if (scm_tc7_vector == SCM_TYP7 (ra0)
	  || scm_tc7_wvect == SCM_TYP7 (ra0))

	for (; n-- > 0; i0 += inc0, i1 += inc1)
	  scm_array_set_x (ra0, SCM_SUBRF (proc) (scm_cvref (ra1, i1, SCM_UNDEFINED), SCM_UNDEFINED),
			   SCM_MAKINUM (i0));
      else
	for (; n-- > 0; i0 += inc0, i1 += inc1)
	  scm_array_set_x (ra0, SCM_SUBRF (proc) (RVREF (ra1, i1, e1), SCM_UNDEFINED),
			   SCM_MAKINUM (i0));
    }
  else
    {
      SCM ra2 = SCM_CAR (ras);
      SCM e2 = SCM_UNDEFINED;
      scm_sizet i2 = SCM_ARRAY_BASE (ra2);
      long inc2 = SCM_ARRAY_DIMS (ra2)->inc;
      ra2 = SCM_ARRAY_V (ra2);
      if (scm_tc7_vector == SCM_TYP7 (ra0) || scm_tc7_wvect == SCM_TYP7 (ra0))
	for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	  scm_array_set_x (ra0,
			   SCM_SUBRF (proc) (scm_cvref (ra1, i1, SCM_UNDEFINED), scm_cvref (ra2, i2, SCM_UNDEFINED)),
			   SCM_MAKINUM (i0));
      else
	for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	  scm_array_set_x (ra0,
			   SCM_SUBRF (proc) (RVREF (ra1, i1, e1), RVREF (ra2, i2, e2)),
			   SCM_MAKINUM (i0));
    }
  return 1;
}



static int
ramap_a (SCM ra0,SCM proc,SCM ras)
{
  SCM e0 = SCM_UNDEFINED, e1 = SCM_UNDEFINED;
  long n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_ARRAY_V (ra0);
  if (SCM_NULLP (ras))
    for (; n-- > 0; i0 += inc0)
      scm_array_set_x (ra0, SCM_SUBRF (proc) (RVREF (ra0, i0, e0), SCM_UNDEFINED), SCM_MAKINUM (i0));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      scm_sizet i1 = SCM_ARRAY_BASE (ra1);
      long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	scm_array_set_x (ra0, SCM_SUBRF (proc) (RVREF (ra0, i0, e0), RVREF (ra1, i1, e1)),
			 SCM_MAKINUM (i0));
    }
  return 1;
}


SCM_REGISTER_PROC(s_array_map_in_order_x, "array-map-in-order!", 2, 0, 1, scm_array_map_x);


SCM_DEFINE (scm_array_map_x, "array-map!", 2, 0, 1,
	    (SCM ra0, SCM proc, SCM lra),
	    "@deffnx primitive array-map-in-order! ra0 proc . lra\n"
	    "@var{array1}, @dots{} must have the same number of dimensions as\n"
	    "@var{array0} and have a range for each index which includes the range\n"
	    "for the corresponding index in @var{array0}.  @var{proc} is applied to\n"
	    "each tuple of elements of @var{array1} @dots{} and the result is stored\n"
	    "as the corresponding element in @var{array0}.  The value returned is\n"
	    "unspecified.  The order of application is unspecified.")
#define FUNC_NAME s_scm_array_map_x
{
  SCM_VALIDATE_PROC (2,proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);
  switch (SCM_TYP7 (proc))
    {
    default:
    gencase:
 scm_ramapc (ramap, proc, ra0, lra, FUNC_NAME);
 return SCM_UNSPECIFIED;
    case scm_tc7_subr_1:
      scm_ramapc (ramap_1, proc, ra0, lra, FUNC_NAME);
      return SCM_UNSPECIFIED;
    case scm_tc7_subr_2:
    case scm_tc7_subr_2o:
      scm_ramapc (ramap_2o, proc, ra0, lra, FUNC_NAME);
      return SCM_UNSPECIFIED;
    case scm_tc7_cxr:
      if (!SCM_SUBRF (proc))
	goto gencase;
      scm_ramapc (ramap_cxr, proc, ra0, lra, FUNC_NAME);
      return SCM_UNSPECIFIED;
    case scm_tc7_rpsubr:
      {
	ra_iproc *p;
	if (SCM_FALSEP (scm_array_p (ra0, SCM_BOOL_T)))
	  goto gencase;
	scm_array_fill_x (ra0, SCM_BOOL_T);
	for (p = ra_rpsubrs; p->name; p++)
	  if (SCM_EQ_P (proc, p->sproc))
	    {
	      while (SCM_NNULLP (lra) && SCM_NNULLP (SCM_CDR (lra)))
		{
		  scm_ramapc (p->vproc, SCM_UNDEFINED, ra0, lra, FUNC_NAME);
		  lra = SCM_CDR (lra);
		}
	      return SCM_UNSPECIFIED;
	    }
	while (SCM_NNULLP (lra) && SCM_NNULLP (SCM_CDR (lra)))
	  {
	    scm_ramapc (ramap_rp, proc, ra0, lra, FUNC_NAME);
	    lra = SCM_CDR (lra);
	  }
	return SCM_UNSPECIFIED;
      }
    case scm_tc7_asubr:
      if (SCM_NULLP (lra))
	{
	  SCM prot, fill = SCM_SUBRF (proc) (SCM_UNDEFINED, SCM_UNDEFINED);
	  if (SCM_INUMP(fill))
	    {
	      prot = scm_array_prototype (ra0);
	      if (SCM_INEXACTP (prot))
		fill = scm_make_real ((double) SCM_INUM (fill));
	    }

	  scm_array_fill_x (ra0, fill);
	}
      else
	{
	  SCM tail, ra1 = SCM_CAR (lra);
	  SCM v0 = (SCM_ARRAYP (ra0) ? SCM_ARRAY_V (ra0) : ra0);
	  ra_iproc *p;
	  /* Check to see if order might matter.
	     This might be an argument for a separate
	     SERIAL-ARRAY-MAP! */
	  if (SCM_EQ_P (v0, ra1) 
	      || (SCM_ARRAYP (ra1) && SCM_EQ_P (v0, SCM_ARRAY_V (ra1))))
	    if (!SCM_EQ_P (ra0, ra1) 
		|| (SCM_ARRAYP(ra0) && !SCM_ARRAY_CONTP(ra0)))
	      goto gencase;
	  for (tail = SCM_CDR (lra); SCM_NNULLP (tail); tail = SCM_CDR (tail))
	    {
	      ra1 = SCM_CAR (tail);
	      if (SCM_EQ_P (v0, ra1) 
		  || (SCM_ARRAYP (ra1) && SCM_EQ_P (v0, SCM_ARRAY_V (ra1))))
		goto gencase;
	    }
	  for (p = ra_asubrs; p->name; p++)
	    if (SCM_EQ_P (proc, p->sproc))
	      {
		if (!SCM_EQ_P (ra0, SCM_CAR (lra)))
		  scm_ramapc (scm_array_identity, SCM_UNDEFINED, ra0, scm_cons (SCM_CAR (lra), SCM_EOL), FUNC_NAME);
		lra = SCM_CDR (lra);
		while (1)
		  {
		    scm_ramapc (p->vproc, SCM_UNDEFINED, ra0, lra, FUNC_NAME);
		    if (SCM_IMP (lra) || SCM_IMP (SCM_CDR (lra)))
		      return SCM_UNSPECIFIED;
		    lra = SCM_CDR (lra);
		  }
	      }
	  scm_ramapc (ramap_2o, proc, ra0, lra, FUNC_NAME);
	  lra = SCM_CDR (lra);
	  if (SCM_NIMP (lra))
	    for (lra = SCM_CDR (lra); SCM_NIMP (lra); lra = SCM_CDR (lra))
	      scm_ramapc (ramap_a, proc, ra0, lra, FUNC_NAME);
	}
      return SCM_UNSPECIFIED;
    }
}
#undef FUNC_NAME


static int
rafe (SCM ra0,SCM proc,SCM ras)
{
  long i = SCM_ARRAY_DIMS (ra0)->lbnd;
  scm_sizet i0 = SCM_ARRAY_BASE (ra0);
  long inc0 = SCM_ARRAY_DIMS (ra0)->inc;
  long n = SCM_ARRAY_DIMS (ra0)->ubnd;
  ra0 = SCM_ARRAY_V (ra0);
  if (SCM_NULLP (ras))
    for (; i <= n; i++, i0 += inc0)
      scm_apply (proc, scm_cvref (ra0, i0, SCM_UNDEFINED), scm_listofnull);
  else
    {
      SCM ra1 = SCM_CAR (ras);
      SCM args, *ve = &ras;
      scm_sizet k, i1 = SCM_ARRAY_BASE (ra1);
      long inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
      ras = SCM_CDR (ras);
      if (SCM_NULLP(ras))
	ras = scm_nullvect;
      else
	{
	  ras = scm_vector (ras);
	  ve = SCM_VELTS (ras);
	}
      for (; i <= n; i++, i0 += inc0, i1 += inc1)
	{
	  args = SCM_EOL;
	  for (k = SCM_INUM (scm_uniform_vector_length (ras)); k--;)
	    args = scm_cons (scm_uniform_vector_ref (ve[k], SCM_MAKINUM (i)), args);
	  args = scm_cons2 (scm_cvref (ra0, i0, SCM_UNDEFINED), scm_cvref (ra1, i1, SCM_UNDEFINED), args);
	  scm_apply (proc, args, SCM_EOL);
	}
    }
  return 1;
}


SCM_DEFINE (scm_array_for_each, "array-for-each", 2, 0, 1,
	    (SCM proc, SCM ra0, SCM lra),
	    "@var{proc} is applied to each tuple of elements of @var{array0} @dots{}\n"
	    "in row-major order.  The value returned is unspecified.")
#define FUNC_NAME s_scm_array_for_each
{
  SCM_VALIDATE_PROC (1,proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);
  scm_ramapc (rafe, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_array_index_map_x, "array-index-map!", 2, 0, 0,
	    (SCM ra, SCM proc),
	    "applies @var{proc} to the indices of each element of @var{array} in\n"
	    "turn, storing the result in the corresponding element.  The value\n"
	    "returned and the order of application are unspecified.\n\n"
	    "One can implement @var{array-indexes} as\n"
	    "@lisp\n"
	    "(define (array-indexes array)\n"
	    "    (let ((ra (apply make-array #f (array-shape array))))\n"
	    "      (array-index-map! ra (lambda x x))\n"
	    "      ra))\n"
	    "@end lisp\n"
	    "Another example:\n"
	    "@lisp\n"
	    "(define (apl:index-generator n)\n"
	    "    (let ((v (make-uniform-vector n 1)))\n"
	    "      (array-index-map! v (lambda (i) i))\n"
	    "      v))\n"
	    "@end lisp")
#define FUNC_NAME s_scm_array_index_map_x
{
  scm_sizet i;
  SCM_VALIDATE_NIM (1,ra);
  SCM_VALIDATE_PROC (2,proc);
  switch (SCM_TYP7(ra))
    {
    default:
    badarg:SCM_WRONG_TYPE_ARG (1, ra);
    case scm_tc7_vector:
    case scm_tc7_wvect:
      {
	SCM *ve = SCM_VELTS (ra);
	for (i = 0; i < SCM_VECTOR_LENGTH (ra); i++)
	  ve[i] = scm_apply (proc, SCM_MAKINUM (i), scm_listofnull);
	return SCM_UNSPECIFIED;
      }
    case scm_tc7_string:
    case scm_tc7_byvect:
    case scm_tc7_bvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_svect:
#ifdef HAVE_LONG_LONGS
    case scm_tc7_llvect:
#endif
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
      {
	unsigned long int length = SCM_INUM (scm_uniform_vector_length (ra));
	for (i = 0; i < length; i++)
	  scm_array_set_x (ra, scm_apply (proc, SCM_MAKINUM (i), scm_listofnull),
			   SCM_MAKINUM (i));
	return SCM_UNSPECIFIED;
      }
    case scm_tc7_smob:
      SCM_ASRTGO (SCM_ARRAYP (ra), badarg);
      {
	SCM args = SCM_EOL;
	SCM inds = scm_make_uve (SCM_ARRAY_NDIM (ra), SCM_MAKINUM (-1L));
	long *vinds = (long *) SCM_VELTS (inds);
	int j, k, kmax = SCM_ARRAY_NDIM (ra) - 1;
	if (kmax < 0)
	  return scm_array_set_x (ra, scm_apply(proc, SCM_EOL, SCM_EOL),
				  SCM_EOL);
	for (k = 0; k <= kmax; k++)
	  vinds[k] = SCM_ARRAY_DIMS (ra)[k].lbnd;
	k = kmax;
	do
	  {
	    if (k == kmax)
	      {
		vinds[k] = SCM_ARRAY_DIMS (ra)[k].lbnd;
		i = cind (ra, inds);
		for (; vinds[k] <= SCM_ARRAY_DIMS (ra)[k].ubnd; vinds[k]++)
		  {
		    for (j = kmax + 1, args = SCM_EOL; j--;)
		      args = scm_cons (SCM_MAKINUM (vinds[j]), args);
		    scm_array_set_x (SCM_ARRAY_V (ra),
				     scm_apply (proc, args, SCM_EOL),
				     SCM_MAKINUM (i));
		    i += SCM_ARRAY_DIMS (ra)[k].inc;
		  }
		k--;
		continue;
	      }
	    if (vinds[k] < SCM_ARRAY_DIMS (ra)[k].ubnd)
	      {
		vinds[k]++;
		k++;
		continue;
	      }
	    vinds[k] = SCM_ARRAY_DIMS (ra)[k].lbnd - 1;
	    k--;
	  }
	while (k >= 0);
	return SCM_UNSPECIFIED;
      }
    }
}
#undef FUNC_NAME


static int
raeql_1 (SCM ra0,SCM as_equal,SCM ra1)
{
  SCM e0 = SCM_UNDEFINED, e1 = SCM_UNDEFINED;
  scm_sizet i0 = 0, i1 = 0;
  long inc0 = 1, inc1 = 1;
  scm_sizet n;
  ra1 = SCM_CAR (ra1);
  if (SCM_ARRAYP(ra0))
    {
      n = SCM_ARRAY_DIMS (ra0)->ubnd - SCM_ARRAY_DIMS (ra0)->lbnd + 1;
      i0 = SCM_ARRAY_BASE (ra0);
      inc0 = SCM_ARRAY_DIMS (ra0)->inc;
      ra0 = SCM_ARRAY_V (ra0);
    }
  else
    n = SCM_INUM (scm_uniform_vector_length (ra0));
  if (SCM_ARRAYP (ra1))
    {
      i1 = SCM_ARRAY_BASE (ra1);
      inc1 = SCM_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_ARRAY_V (ra1);
    }
  switch (SCM_TYP7 (ra0))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
    default:
      for (; n--; i0 += inc0, i1 += inc1)
	{
	  if (SCM_FALSEP (as_equal))
	    {
	      if (SCM_FALSEP (scm_array_equal_p (RVREF (ra0, i0, e0), RVREF (ra1, i1, e1))))
		return 0;
	    }
	  else if (SCM_FALSEP (scm_equal_p (RVREF (ra0, i0, e0), RVREF (ra1, i1, e1))))
	    return 0;
	}
      return 1;
    case scm_tc7_string:
      {
	char *v0 = SCM_STRING_CHARS (ra0) + i0;
	char *v1 = SCM_STRING_CHARS (ra1) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  if (*v0 != *v1)
	    return 0;
	return 1;
      }
    case scm_tc7_byvect:
      {
	char *v0 = ((char *) SCM_UVECTOR_BASE (ra0)) + i0;
	char *v1 = ((char *) SCM_UVECTOR_BASE (ra1)) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  if (*v0 != *v1)
	    return 0;
	return 1;
      }
    case scm_tc7_bvect:
      for (; n--; i0 += inc0, i1 += inc1)
	if (SCM_BITVEC_REF (ra0, i0) != SCM_BITVEC_REF (ra1, i1))
	  return 0;
      return 1;
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      {
	long *v0 = (long *) SCM_VELTS (ra0) + i0;
	long *v1 = (long *) SCM_VELTS (ra1) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  if (*v0 != *v1)
	    return 0;
	return 1;
      }
    case scm_tc7_svect:
      {
	short *v0 = (short *) SCM_VELTS (ra0) + i0;
	short *v1 = (short *) SCM_VELTS (ra1) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  if (*v0 != *v1)
	    return 0;
	return 1;
      }
#ifdef HAVE_LONG_LONGS
    case scm_tc7_llvect:
      {
	long long *v0 = (long long *) SCM_VELTS (ra0) + i0;
	long long *v1 = (long long *) SCM_VELTS (ra1) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  if (*v0 != *v1)
	    return 0;
	return 1;
      }
#endif
    case scm_tc7_fvect:
      {
	float *v0 = (float *) SCM_VELTS (ra0) + i0;
	float *v1 = (float *) SCM_VELTS (ra1) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  if (*v0 != *v1)
	    return 0;
	return 1;
      }
    case scm_tc7_dvect:
      {
	double *v0 = (double *) SCM_VELTS (ra0) + i0;
	double *v1 = (double *) SCM_VELTS (ra1) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  if (*v0 != *v1)
	    return 0;
	return 1;
      }
    case scm_tc7_cvect:
      {
	double (*v0)[2] = (double (*)[2]) SCM_VELTS (ra0) + i0;
	double (*v1)[2] = (double (*)[2]) SCM_VELTS (ra1) + i1;
	for (; n--; v0 += inc0, v1 += inc1)
	  {
	    if ((*v0)[0] != (*v1)[0])
	      return 0;
	    if ((*v0)[1] != (*v1)[1])
	      return 0;
	  }
	return 1;
      }
    }
}



static int
raeql (SCM ra0,SCM as_equal,SCM ra1)
{
  SCM v0 = ra0, v1 = ra1;
  scm_array_dim dim0, dim1;
  scm_array_dim *s0 = &dim0, *s1 = &dim1;
  scm_sizet bas0 = 0, bas1 = 0;
  int k, unroll = 1, vlen = 1, ndim = 1;
  if (SCM_ARRAYP (ra0))
    {
      ndim = SCM_ARRAY_NDIM (ra0);
      s0 = SCM_ARRAY_DIMS (ra0);
      bas0 = SCM_ARRAY_BASE (ra0);
      v0 = SCM_ARRAY_V (ra0);
    }
  else
    {
      s0->inc = 1;
      s0->lbnd = 0;
      s0->ubnd = SCM_INUM (scm_uniform_vector_length (v0)) - 1;
      unroll = 0;
    }
  if (SCM_ARRAYP (ra1))
    {
      if (ndim != SCM_ARRAY_NDIM (ra1))
	return 0;
      s1 = SCM_ARRAY_DIMS (ra1);
      bas1 = SCM_ARRAY_BASE (ra1);
      v1 = SCM_ARRAY_V (ra1);
    }
  else
    {
      /*
	Huh ? Schizophrenic return type. --hwn
      */
      if (1 != ndim)
	return 0;
      s1->inc = 1;
      s1->lbnd = 0;
      s1->ubnd = SCM_INUM (scm_uniform_vector_length (v1)) - 1;
      unroll = 0;
    }
  if (SCM_TYP7 (v0) != SCM_TYP7 (v1))
    return 0;
  for (k = ndim; k--;)
    {
      if (s0[k].lbnd != s1[k].lbnd || s0[k].ubnd != s1[k].ubnd)
	return 0;
      if (unroll)
	{
	  unroll = (s0[k].inc == s1[k].inc);
	  vlen *= s0[k].ubnd - s1[k].lbnd + 1;
	}
    }
  if (unroll && bas0 == bas1 && SCM_EQ_P (v0, v1))
    return 1;
  return scm_ramapc (raeql_1, as_equal, ra0, scm_cons (ra1, SCM_EOL), "");
}


SCM
scm_raequal (SCM ra0, SCM ra1)
{
  return SCM_BOOL(raeql (ra0, SCM_BOOL_T, ra1));
}

#if 0
/* GJB:FIXME:: Why not use SCM_DEFINE1 for array-equal? */
SCM_DEFINE1 (scm_array_equal_p, "array-equal?", scm_tc7_rpsubr,
	     (SCM ra0, SCM ra1),
	    "Return @code{#t} iff all arguments are arrays with the same\n"
	    "shape, the same type, and have corresponding elements which are\n"
	    "either @code{equal?}  or @code{array-equal?}.  This function\n"
	    "differs from @code{equal?} in that a one dimensional shared\n"
	    "array may be @var{array-equal?} but not @var{equal?} to a\n"
	    "vector or uniform vector.")
#define FUNC_NAME s_scm_array_equal_p
{
}
#undef FUNC_NAME
#endif

static char s_array_equal_p[] = "array-equal?";


SCM
scm_array_equal_p (SCM ra0, SCM ra1)
{
  if (SCM_IMP (ra0) || SCM_IMP (ra1))
    callequal:return scm_equal_p (ra0, ra1);
  switch (SCM_TYP7(ra0))
    {
    default:
      goto callequal;
    case scm_tc7_bvect:
    case scm_tc7_string:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_vector:
    case scm_tc7_wvect:
      break;
    case scm_tc7_smob:
      if (!SCM_ARRAYP (ra0))
	goto callequal;
    }
  switch (SCM_TYP7 (ra1))
    {
    default:
      goto callequal;
    case scm_tc7_bvect:
    case scm_tc7_string:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_vector:
    case scm_tc7_wvect:
      break;
    case scm_tc7_smob:
      if (!SCM_ARRAYP (ra1))
	goto callequal;
    }
  return SCM_BOOL(raeql (ra0, SCM_BOOL_F, ra1));
}


static void
init_raprocs (ra_iproc *subra)
{
  for (; subra->name; subra++)
    {
      SCM sym = scm_str2symbol (subra->name);
      SCM var =
	scm_sym2var (sym, scm_current_module_lookup_closure (), SCM_BOOL_F);
      if (var != SCM_BOOL_F)
	subra->sproc = SCM_VARIABLE_REF (var);
      else
	subra->sproc = SCM_BOOL_F;
    }
}


void
scm_init_ramap ()
{
  init_raprocs (ra_rpsubrs);
  init_raprocs (ra_asubrs);
  scm_c_define_subr (s_array_equal_p, scm_tc7_rpsubr, scm_array_equal_p);
  scm_smobs[SCM_TC2SMOBNUM (scm_tc16_array)].equalp = scm_raequal;
#ifndef SCM_MAGIC_SNARFER
#include "libguile/ramap.x"
#endif
  scm_add_feature (s_scm_array_for_each);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
