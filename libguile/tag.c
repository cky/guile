/*	Copyright (C) 1996, 1997, 2000 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/struct.h"

#include "libguile/tag.h"


SCM_CONST_LONG (scm_utag_immediate_integer, "utag_immediate_integer", 0);
SCM_CONST_LONG (scm_utag_immediate_char, "utag_immediate_char", 1);
SCM_CONST_LONG (scm_utag_pair, "utag_pair", 2);
SCM_CONST_LONG (scm_utag_closure, "utag_closure", 3);
SCM_CONST_LONG (scm_utag_symbol, "utag_symbol", 4);
SCM_CONST_LONG (scm_utag_vector, "utag_vector", 5);
SCM_CONST_LONG (scm_utag_wvect, "utag_wvect", 6);

#ifdef HAVE_ARRAYS
SCM_CONST_LONG (scm_utag_bvect, "utag_bvect", 7);
SCM_CONST_LONG (scm_utag_byvect, "utag_byvect", 8);
SCM_CONST_LONG (scm_utag_svect, "utag_svect", 9);
SCM_CONST_LONG (scm_utag_ivect, "utag_ivect", 10);
SCM_CONST_LONG (scm_utag_uvect, "utag_uvect", 11);
SCM_CONST_LONG (scm_utag_fvect, "utag_fvect", 12);
SCM_CONST_LONG (scm_utag_dvect, "utag_dvect", 13);
SCM_CONST_LONG (scm_utag_cvect, "utag_cvect", 14);
#endif 

SCM_CONST_LONG (scm_utag_string, "utag_string", 15);
SCM_CONST_LONG (scm_utag_substring, "utag_substring", 17);
SCM_CONST_LONG (scm_utag_asubr, "utag_asubr", 19);
SCM_CONST_LONG (scm_utag_subr_0, "utag_subr_0", 20);
SCM_CONST_LONG (scm_utag_subr_1, "utag_subr_1", 21);
SCM_CONST_LONG (scm_utag_cxr, "utag_cxr", 22);
SCM_CONST_LONG (scm_utag_subr_3, "utag_subr_3", 23);
SCM_CONST_LONG (scm_utag_subr_2, "utag_subr_2", 24);
SCM_CONST_LONG (scm_utag_rpsubr, "utag_rpsubr", 25);
SCM_CONST_LONG (scm_utag_subr_1o, "utag_subr_1o", 26);
SCM_CONST_LONG (scm_utag_subr_2o, "utag_subr_2o", 27);
SCM_CONST_LONG (scm_utag_lsubr_2, "utag_lsubr_2", 28);
SCM_CONST_LONG (scm_utag_lsubr, "utag_lsubr", 29);
SCM_CONST_LONG (scm_utag_smob_base, "utag_smob_base", 252);
SCM_CONST_LONG (scm_utag_port_base, "utag_port_base", 253);
SCM_CONST_LONG (scm_utag_flag_base, "utag_flag_base", 254);
SCM_CONST_LONG (scm_utag_struct_base, "utag_struct_base", 255);


#if (SCM_DEBUG_DEPRECATED == 0)

SCM_DEFINE (scm_tag, "tag", 1, 0, 0, 
            (SCM x),
            "Return an integer corresponding to the type of X.  Deprecated.")
#define FUNC_NAME s_scm_tag
{
  switch (SCM_ITAG3 (x))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      return SCM_CDR (scm_utag_immediate_integer) ;

    case scm_tc3_imm24:
      if (SCM_CHARP (x))
	return SCM_CDR (scm_utag_immediate_char) ;
      else
	{
	  SCM tag = SCM_MAKINUM ((SCM_UNPACK (x) >> 8) & 0xff);
	  return SCM_MAKINUM (SCM_INUM (SCM_CDR (scm_utag_flag_base) ) | (SCM_UNPACK (tag) << 8));
	}

    case scm_tc3_cons:
      switch (SCM_TYP7 (x))
	{
	case scm_tcs_cons_nimcar:
	  return SCM_CDR (scm_utag_pair) ;
	case scm_tcs_closures:
	  return SCM_CDR (scm_utag_closure) ;
	case scm_tc7_symbol:
	  return SCM_CDR (scm_utag_symbol) ;
	case scm_tc7_vector:
	  return SCM_CDR (scm_utag_vector) ;
	case scm_tc7_wvect:
	  return SCM_CDR (scm_utag_wvect) ;

#ifdef HAVE_ARRAYS
	case scm_tc7_bvect:
	  return SCM_CDR (scm_utag_bvect) ;
	case scm_tc7_byvect:
	  return SCM_CDR (scm_utag_byvect) ;
	case scm_tc7_svect:
	  return SCM_CDR (scm_utag_svect) ;
	case scm_tc7_ivect:
	  return SCM_CDR (scm_utag_ivect) ;
	case scm_tc7_uvect:
	  return SCM_CDR (scm_utag_uvect) ;
	case scm_tc7_fvect:
	  return SCM_CDR (scm_utag_fvect) ;
	case scm_tc7_dvect:
	  return SCM_CDR (scm_utag_dvect) ;
	case scm_tc7_cvect:
	  return SCM_CDR (scm_utag_cvect) ;
#endif

	case scm_tc7_string:
	  return SCM_CDR (scm_utag_string) ;
	case scm_tc7_substring:
	  return SCM_CDR (scm_utag_substring) ;
	case scm_tc7_asubr:
	  return SCM_CDR (scm_utag_asubr) ;
	case scm_tc7_subr_0:
	  return SCM_CDR (scm_utag_subr_0) ;
	case scm_tc7_subr_1:
	  return SCM_CDR (scm_utag_subr_1) ;
	case scm_tc7_cxr:
	  return SCM_CDR (scm_utag_cxr) ;
	case scm_tc7_subr_3:
	  return SCM_CDR (scm_utag_subr_3) ;
	case scm_tc7_subr_2:
	  return SCM_CDR (scm_utag_subr_2) ;
	case scm_tc7_rpsubr:
	  return SCM_CDR (scm_utag_rpsubr) ;
	case scm_tc7_subr_1o:
	  return SCM_CDR (scm_utag_subr_1o) ;
	case scm_tc7_subr_2o:
	  return SCM_CDR (scm_utag_subr_2o) ;
	case scm_tc7_lsubr_2:
	  return SCM_CDR (scm_utag_lsubr_2) ;
	case scm_tc7_lsubr:
	  return SCM_CDR (scm_utag_lsubr) ;

	case scm_tc7_port:
	  {
	    int tag;
	    tag = (SCM_TYP16 (x) >> 8) & 0xff;
	    return SCM_MAKINUM (SCM_INUM (SCM_CDR (scm_utag_port_base)) | (tag << 8));
	  }
	case scm_tc7_smob:
	  {
	    int tag;
	    tag = (SCM_TYP16 (x) >> 8) & 0xff;
	    return SCM_MAKINUM (SCM_INUM (SCM_CDR (scm_utag_smob_base))
				| (tag << 8));
	  }
	case scm_tcs_cons_gloc:
	  /* must be a struct */
	  {
	    int tag = (int) SCM_STRUCT_VTABLE_DATA (x) >> 3;
	    return SCM_MAKINUM (SCM_INUM (SCM_CDR (scm_utag_struct_base))
				| (tag << 8));
	  }

	default:
	  if (SCM_CONSP (x))
	    return SCM_CDR (scm_utag_pair);
	  else
	    return SCM_MAKINUM (-1);
	}

    case scm_tc3_cons_gloc:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
    case scm_tc3_closure:
      /* Never reached */
      break;
    }
  return SCM_MAKINUM (-1);
}
#undef FUNC_NAME

#endif  /* SCM_DEBUG_DEPRECATED == 0 */




void
scm_init_tag ()
{
#include "libguile/tag.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
