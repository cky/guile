/* Copyright (C) 1995,1996,1997,1998,2000,2001,2003, 2004 Free Software Foundation, Inc.
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


#if HAVE_CONFIG_H
#  include <config.h>
#endif

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


#ifdef HAVE_STRING_H
#include <string.h>
#endif


SCM_DEFINE1 (scm_eq_p, "eq?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} references the same object as @var{y}.\n"
	     "@code{eq?} is similar to @code{eqv?} except that in some cases it is\n"
	     "capable of discerning distinctions finer than those detectable by\n"
	     "@code{eqv?}.")
#define FUNC_NAME s_scm_eq_p
{
  return scm_from_bool (scm_is_eq (x, y));
}
#undef FUNC_NAME

/* We compare doubles in a special way for 'eqv?' to be able to
   distinguish plus and minus zero and to identify NaNs.
*/

static int
real_eqv (double x, double y)
{
  return !memcmp (&x, &y, sizeof(double)) || (x != x && y != y);
}

#include <stdio.h>
SCM_PRIMITIVE_GENERIC_1 (scm_eqv_p, "eqv?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "The @code{eqv?} procedure defines a useful equivalence relation on objects.\n"
	     "Briefly, it returns @code{#t} if @var{x} and @var{y} should normally be\n"
	     "regarded as the same object.  This relation is left slightly open to\n"
	     "interpretation, but works for comparing immediate integers, characters,\n"
	     "and inexact numbers.")
#define FUNC_NAME s_scm_eqv_p
{
  if (scm_is_eq (x, y))
    return SCM_BOOL_T;
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_IMP (y))
    return SCM_BOOL_F;
  /* this ensures that types and scm_length are the same. */

  if (SCM_CELL_TYPE (x) != SCM_CELL_TYPE (y))
    {
      /* fractions use 0x10000 as a flag (at the suggestion of Marius Vollmer),
	 but this checks the entire type word, so fractions may be accidentally
	 flagged here as unequal.  Perhaps I should use the 4th double_cell word?
      */

      /* treat mixes of real and complex types specially */
      if (SCM_INEXACTP (x))
	{
	  if (SCM_REALP (x))
	    return scm_from_bool (SCM_COMPLEXP (y)
			     && real_eqv (SCM_REAL_VALUE (x),
					  SCM_COMPLEX_REAL (y))
			     && SCM_COMPLEX_IMAG (y) == 0.0);
	  else
	    return scm_from_bool (SCM_REALP (y)
			     && real_eqv (SCM_COMPLEX_REAL (x),
					  SCM_REAL_VALUE (y))
			     && SCM_COMPLEX_IMAG (x) == 0.0);
	}

      if (SCM_FRACTIONP (x) && SCM_FRACTIONP (y))
	return scm_i_fraction_equalp (x, y);
      return SCM_BOOL_F;
    }
  if (SCM_NUMP (x))
    {
      if (SCM_BIGP (x)) {
	return scm_from_bool (scm_i_bigcmp (x, y) == 0);
      } else if (SCM_REALP (x)) {
	return scm_from_bool (real_eqv (SCM_REAL_VALUE (x), SCM_REAL_VALUE (y)));
      } else if (SCM_FRACTIONP (x)) {
	return scm_i_fraction_equalp (x, y);
      } else { /* complex */
	return scm_from_bool (real_eqv (SCM_COMPLEX_REAL (x),
				   SCM_COMPLEX_REAL (y)) 
			 && real_eqv (SCM_COMPLEX_IMAG (x),
				      SCM_COMPLEX_IMAG (y)));
      }
    }
  if (SCM_UNPACK (g_scm_eqv_p))
    return scm_call_generic_2 (g_scm_eqv_p, x, y);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC_1 (scm_equal_p, "equal?", scm_tc7_rpsubr,
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
  if (scm_is_eq (x, y))
    return SCM_BOOL_T;
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_IMP (y))
    return SCM_BOOL_F;
  if (scm_is_pair (x) && scm_is_pair (y))
    {
      if (scm_is_false (scm_equal_p (SCM_CAR (x), SCM_CAR (y))))
	return SCM_BOOL_F;
      x = SCM_CDR(x);
      y = SCM_CDR(y);
      goto tailrecurse;
    }
  if (SCM_TYP7 (x) == scm_tc7_string && SCM_TYP7 (y) == scm_tc7_string)
    return scm_string_equal_p (x, y);
  if (SCM_TYP7 (x) == scm_tc7_smob && SCM_TYP16 (x) == SCM_TYP16 (y))
    {
      int i = SCM_SMOBNUM (x);
      if (!(i < scm_numsmob))
	return SCM_BOOL_F;
      if (scm_smobs[i].equalp)
	return (scm_smobs[i].equalp) (x, y);
      else
	goto generic_equal;
    }
  /* This ensures that types and scm_length are the same.  */
  if (SCM_CELL_TYPE (x) != SCM_CELL_TYPE (y))
    {
      /* treat mixes of real and complex types specially */
      if (SCM_INEXACTP (x) && SCM_INEXACTP (y))
	{
	  if (SCM_REALP (x))
	    return scm_from_bool (SCM_COMPLEXP (y)
			     && SCM_REAL_VALUE (x) == SCM_COMPLEX_REAL (y)
			     && SCM_COMPLEX_IMAG (y) == 0.0);
	  else
	    return scm_from_bool (SCM_REALP (y)
			     && SCM_COMPLEX_REAL (x) == SCM_REAL_VALUE (y)
			     && SCM_COMPLEX_IMAG (x) == 0.0);
	}

      return SCM_BOOL_F;
    }
  switch (SCM_TYP7 (x))
    {
    default:
      break;
    case scm_tc7_number:
      switch SCM_TYP16 (x)
        {
        case scm_tc16_big:
          return scm_bigequal (x, y);
        case scm_tc16_real:
          return scm_real_equalp (x, y);
        case scm_tc16_complex:
          return scm_complex_equalp (x, y);
	case scm_tc16_fraction:
          return scm_i_fraction_equalp (x, y);
        }
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return scm_vector_equal_p (x, y);
    }
 generic_equal:
  if (SCM_UNPACK (g_scm_equal_p))
    return scm_call_generic_2 (g_scm_equal_p, x, y);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME






void
scm_init_eq ()
{
#include "libguile/eq.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
