/* Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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



#include "libguile/_scm.h"
#include "libguile/ramap.h"
#include "libguile/stackchk.h"
#include "libguile/strorder.h"
#include "libguile/async.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/unif.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/eq.h"

SCM_DEFINE1 (scm_eq_p, "eq?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} references the same object as @var{y}.\n"
	     "@code{eq?} is similar to @code{eqv?} except that in some cases it is\n"
	     "capable of discerning distinctions finer than those detectable by\n"
	     "@code{eqv?}.")
#define FUNC_NAME s_scm_eq_p
{
  return SCM_BOOL (SCM_EQ_P (x, y));
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_eqv_p, "eqv?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "The @code{eqv?} procedure defines a useful equivalence relation on objects.\n"
	     "Briefly, it returns @code{#t} if @var{x} and @var{y} should normally be\n"
	     "regarded as the same object.  This relation is left slightly open to\n"
	     "interpretation, but works for comparing immediate integers, characters,\n"
	     "and inexact numbers.")
#define FUNC_NAME s_scm_eqv_p
{
  if (SCM_EQ_P (x, y))
    return SCM_BOOL_T;
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_IMP (y))
    return SCM_BOOL_F;
  /* this ensures that types and scm_length are the same. */
  if (SCM_CELL_TYPE (x) != SCM_CELL_TYPE (y))
    {
      /* treat mixes of real and complex types specially */
      if (SCM_SLOPPY_INEXACTP (x))
	{
	  if (SCM_SLOPPY_REALP (x))
	    return SCM_BOOL (SCM_SLOPPY_COMPLEXP (y)
			     && SCM_REAL_VALUE (x) == SCM_COMPLEX_REAL (y)
			     && 0.0 == SCM_COMPLEX_IMAG (y));
	  else
	    return SCM_BOOL (SCM_SLOPPY_REALP (y)
			     && SCM_COMPLEX_REAL (x) == SCM_REAL_VALUE (y)
			     && SCM_COMPLEX_IMAG (x) == 0.0);
	}
      return SCM_BOOL_F;
    }
  if (SCM_NUMP (x))
    {
      if (SCM_BIGP (x)) {
	return SCM_BOOL (0 == scm_bigcomp (x, y));
      } else if (SCM_SLOPPY_REALP (x)) {
	return SCM_BOOL (SCM_REAL_VALUE (x) == SCM_REAL_VALUE (y));
      } else { /* complex */
	return SCM_BOOL (SCM_COMPLEX_REAL (x) == SCM_COMPLEX_REAL (y) 
			 && SCM_COMPLEX_IMAG (x) == SCM_COMPLEX_IMAG (y));
      }
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_equal_p, "equal?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} and @var{y} are recursively @code{eqv?} equivalent.\n"
	     "@code{equal?} recursively compares the contents of pairs,\n"
	     "vectors, and strings, applying @code{eqv?} on other objects such as\n"
	     "numbers and symbols.  A rule of thumb is that objects are generally\n"
	     "@code{equal?}  if they print the same.  @code{equal?} may fail to\n"
	     "terminate if its arguments are circular data structures.")
#define FUNC_NAME s_scm_equal_p
{
  SCM_CHECK_STACK;
 tailrecurse:
  SCM_TICK;
  if (SCM_EQ_P (x, y))
    return SCM_BOOL_T;
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_IMP (y))
    return SCM_BOOL_F;
  if (SCM_CONSP (x) && SCM_CONSP (y))
    {
      if (SCM_FALSEP (scm_equal_p (SCM_CAR (x), SCM_CAR (y))))
	return SCM_BOOL_F;
      x = SCM_CDR(x);
      y = SCM_CDR(y);
      goto tailrecurse;
    }
  if (SCM_TYP7S (x) == scm_tc7_string && SCM_TYP7S (y) == scm_tc7_string)
    return scm_string_equal_p (x, y);
  /* This ensures that types and scm_length are the same.  */
  if (SCM_CELL_TYPE (x) != SCM_CELL_TYPE (y))
    {
      /* treat mixes of real and complex types specially */
      if (SCM_SLOPPY_INEXACTP (x))
	{
	  if (SCM_SLOPPY_REALP (x))
	    return SCM_BOOL (SCM_SLOPPY_COMPLEXP (y)
			     && SCM_REAL_VALUE (x) == SCM_COMPLEX_REAL (y)
			     && 0.0 == SCM_COMPLEX_IMAG (y));
	  else
	    return SCM_BOOL (SCM_SLOPPY_REALP (y)
			     && SCM_COMPLEX_REAL (x) == SCM_REAL_VALUE (y)
			     && SCM_COMPLEX_IMAG (x) == 0.0);
	}
      return SCM_BOOL_F;
    }
  switch (SCM_TYP7 (x))
    {
    default:
      return SCM_BOOL_F;
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return scm_vector_equal_p (x, y);
    case scm_tc7_smob:
      {
	int i = SCM_SMOBNUM (x);
	if (!(i < scm_numsmob))
	  return SCM_BOOL_F;
	if (scm_smobs[i].equalp)
	  return (scm_smobs[i].equalp) (x, y);
	else
	  return SCM_BOOL_F;
      }
#ifdef HAVE_ARRAYS
    case scm_tc7_bvect: case scm_tc7_uvect: case scm_tc7_ivect:
    case scm_tc7_fvect:	case scm_tc7_cvect: case scm_tc7_dvect:
    case scm_tc7_svect:
#ifdef HAVE_LONG_LONGS
    case scm_tc7_llvect:
#endif
    case scm_tc7_byvect:
      if (scm_tc16_array && scm_smobs[SCM_TC2SMOBNUM (scm_tc16_array)].equalp)
	return scm_array_equal_p (x, y);
#endif
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME






void
scm_init_eq ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/eq.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
