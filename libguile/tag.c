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


#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/struct.h"

#include "libguile/tag.h"


#define CONST_INUM(c_name, scheme_name, value) \
SCM_VCELL_INIT(c_name, scheme_name, SCM_MAKINUM (value))

CONST_INUM (scm_utag_immediate_integer, "utag_immediate_integer", 0);
CONST_INUM (scm_utag_immediate_char, "utag_immediate_char", 1);
CONST_INUM (scm_utag_pair, "utag_pair", 2);
CONST_INUM (scm_utag_closure, "utag_closure", 3);
CONST_INUM (scm_utag_symbol, "utag_symbol", 4);
CONST_INUM (scm_utag_vector, "utag_vector", 5);
CONST_INUM (scm_utag_wvect, "utag_wvect", 6);

#ifdef HAVE_ARRAYS
CONST_INUM (scm_utag_bvect, "utag_bvect", 7);
CONST_INUM (scm_utag_byvect, "utag_byvect", 8);
CONST_INUM (scm_utag_svect, "utag_svect", 9);
CONST_INUM (scm_utag_ivect, "utag_ivect", 10);
CONST_INUM (scm_utag_uvect, "utag_uvect", 11);
CONST_INUM (scm_utag_fvect, "utag_fvect", 12);
CONST_INUM (scm_utag_dvect, "utag_dvect", 13);
CONST_INUM (scm_utag_cvect, "utag_cvect", 14);
#endif 

CONST_INUM (scm_utag_string, "utag_string", 15);
CONST_INUM (scm_utag_substring, "utag_substring", 17);
CONST_INUM (scm_utag_asubr, "utag_asubr", 19);
CONST_INUM (scm_utag_subr_0, "utag_subr_0", 20);
CONST_INUM (scm_utag_subr_1, "utag_subr_1", 21);
CONST_INUM (scm_utag_cxr, "utag_cxr", 22);
CONST_INUM (scm_utag_subr_3, "utag_subr_3", 23);
CONST_INUM (scm_utag_subr_2, "utag_subr_2", 24);
CONST_INUM (scm_utag_rpsubr, "utag_rpsubr", 25);
CONST_INUM (scm_utag_subr_1o, "utag_subr_1o", 26);
CONST_INUM (scm_utag_subr_2o, "utag_subr_2o", 27);
CONST_INUM (scm_utag_lsubr_2, "utag_lsubr_2", 28);
CONST_INUM (scm_utag_lsubr, "utag_lsubr", 29);
CONST_INUM (scm_utag_smob_base, "utag_smob_base", 252);
CONST_INUM (scm_utag_port_base, "utag_port_base", 253);
CONST_INUM (scm_utag_flag_base, "utag_flag_base", 254);
CONST_INUM (scm_utag_struct_base, "utag_struct_base", 255);



void
scm_init_tag ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/tag.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
