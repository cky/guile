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
#include "_scm.h"
#include "ramap.h"
#include "stackchk.h"
#include "strorder.h"
#include "smob.h"
#include "unif.h"

#include "eq.h"

SCM_PROC1 (s_eq_p, "eq?", scm_tc7_rpsubr, scm_eq_p);
#ifdef __STDC__
SCM
scm_eq_p (SCM x, SCM y)
#else
SCM
scm_eq_p (x, y)
     SCM x;
     SCM y;
#endif
{
  return ((x==y)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC1 (s_eqv_p, "eqv?", scm_tc7_rpsubr, scm_eqv_p);
#ifdef __STDC__
SCM
scm_eqv_p (SCM x, SCM y)
#else
SCM
scm_eqv_p (x, y)
     SCM x;
     SCM y;
#endif
{
  if (x==y) return SCM_BOOL_T;
  if SCM_IMP(x) return SCM_BOOL_F;
  if SCM_IMP(y) return SCM_BOOL_F;
  /* this ensures that types and scm_length are the same. */
  if (SCM_CAR(x) != SCM_CAR(y)) return SCM_BOOL_F;
  if SCM_NUMP(x) {
# ifdef SCM_BIGDIG
    if SCM_BIGP(x) return (0==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
# endif
#ifdef SCM_FLOATS
    if (SCM_REALPART(x) != SCM_REALPART(y)) return SCM_BOOL_F;
    if (SCM_CPLXP(x) && (SCM_IMAG(x) != SCM_IMAG(y))) return SCM_BOOL_F;
#endif
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}


SCM_PROC1 (s_equal_p, "equal?", scm_tc7_rpsubr, scm_equal_p);
#ifdef __STDC__
SCM
scm_equal_p (SCM x, SCM y)
#else
SCM
scm_equal_p (x, y)
     SCM x;
     SCM y;
#endif
{
  SCM_CHECK_STACK;
 tailrecurse: SCM_ASYNC_TICK;
	if (x==y) return SCM_BOOL_T;
	if (SCM_IMP(x)) return SCM_BOOL_F;
	if (SCM_IMP(y)) return SCM_BOOL_F;
	if (SCM_CONSP(x) && SCM_CONSP(y)) {
		if SCM_FALSEP(scm_equal_p(SCM_CAR(x), SCM_CAR(y))) return SCM_BOOL_F;
		x = SCM_CDR(x);
		y = SCM_CDR(y);
		goto tailrecurse;
	}
	/* this ensures that types and scm_length are the same. */
	if (SCM_CAR(x) != SCM_CAR(y)) return SCM_BOOL_F;
	switch (SCM_TYP7(x)) {
        default: return SCM_BOOL_F;
	case scm_tc7_substring:
	case scm_tc7_mb_substring:
	case scm_tc7_mb_string:
	case scm_tc7_string: return scm_string_equal_p(x, y);
	case scm_tc7_vector:
	case scm_tc7_wvect:
	  return scm_vector_equal_p(x, y);
	case scm_tc7_smob: {
	        int i = SCM_SMOBNUM(x);
	        if (!(i < scm_numsmob)) return SCM_BOOL_F;
	        if (scm_smobs[i].equalp)
		  return (scm_smobs[i].equalp)(x, y);
		else
		  return SCM_BOOL_F;
	      }
	case scm_tc7_bvect: case scm_tc7_uvect: case scm_tc7_ivect:
	case scm_tc7_fvect:	case scm_tc7_cvect: case scm_tc7_dvect:
	case scm_tc7_svect:
#ifdef LONGLONGS
	case scm_tc7_llvect:
#endif
	case scm_tc7_byvect:
	  if (   scm_tc16_array
	      && scm_smobs[0x0ff & (scm_tc16_array >> 8)].equalp)
	    return scm_array_equal_p(x, y);
	}
	return SCM_BOOL_F;
}





#ifdef __STDC__
void
scm_init_eq (void)
#else
void
scm_init_eq ()
#endif
{
#include "eq.x"
}

