/* classes: h_files */

#ifndef UNIFH
#define UNIFH
/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"


typedef struct scm_array
{
  SCM v;
  scm_sizet base;
} scm_array;

typedef struct scm_array_dim
{
  long lbnd;
  long ubnd;
  long inc;
} scm_array_dim;


extern long scm_tc16_array;
#define SCM_ARRAYP(a) 		(scm_tc16_array==SCM_TYP16(a))
#define SCM_ARRAY_V(a) 		(((scm_array *)SCM_CDR(a))->v)
#define SCM_ARRAY_NDIM(x) 	((scm_sizet)(SCM_CAR(x)>>17))
#define SCM_ARRAY_CONTIGUOUS 	0x10000
#define SCM_ARRAY_CONTP(x) 	(SCM_ARRAY_CONTIGUOUS & (int)SCM_CAR(x))
#define SCM_ARRAY_BASE(a) 	(((scm_array *)SCM_CDR(a))->base)
#define SCM_ARRAY_DIMS(a) 	((scm_array_dim *)(SCM_CHARS(a)+sizeof(scm_array))) 

#define SCM_HUGE_LENGTH(x) (SCM_LENGTH_MAX==SCM_LENGTH(x) ? *((long *)SCM_VELTS(x)) : SCM_LENGTH(x))



extern scm_sizet scm_uniform_element_size (SCM obj);
extern SCM scm_makflo SCM_P ((float x));
extern SCM scm_make_uve SCM_P ((long k, SCM prot));
extern SCM scm_uniform_vector_length SCM_P ((SCM v));
extern SCM scm_array_p SCM_P ((SCM v, SCM prot));
extern SCM scm_array_rank SCM_P ((SCM ra));
extern SCM scm_array_dimensions SCM_P ((SCM ra));
extern long scm_aind SCM_P ((SCM ra, SCM args, const char *what));
extern SCM scm_make_ra SCM_P ((int ndim));
extern SCM scm_shap2ra SCM_P ((SCM args, const char *what));
extern SCM scm_dimensions_to_uniform_array SCM_P ((SCM dims, SCM prot, SCM fill));
extern void scm_ra_set_contp SCM_P ((SCM ra));
extern SCM scm_make_shared_array SCM_P ((SCM oldra, SCM mapfunc, SCM dims));
extern SCM scm_transpose_array SCM_P ((SCM args));
extern SCM scm_enclose_array SCM_P ((SCM axes));
extern SCM scm_array_in_bounds_p SCM_P ((SCM args));
extern SCM scm_uniform_vector_ref SCM_P ((SCM v, SCM args));
extern SCM scm_cvref SCM_P ((SCM v, scm_sizet pos, SCM last));
extern SCM scm_array_set_x SCM_P ((SCM v, SCM obj, SCM args));
extern SCM scm_array_contents SCM_P ((SCM ra, SCM strict));
extern SCM scm_ra2contig SCM_P ((SCM ra, int copy));
extern SCM scm_uniform_array_read_x SCM_P ((SCM ra, SCM port_or_fd, SCM start, SCM end));
extern SCM scm_uniform_array_write SCM_P ((SCM v, SCM port_or_fd, SCM start, SCM end));
extern SCM scm_bit_count SCM_P ((SCM item, SCM seq));
extern SCM scm_bit_position SCM_P ((SCM item, SCM v, SCM k));
extern SCM scm_bit_set_star_x SCM_P ((SCM v, SCM kv, SCM obj));
extern SCM scm_bit_count_star SCM_P ((SCM v, SCM kv, SCM obj));
extern SCM scm_bit_invert_x SCM_P ((SCM v));
extern SCM scm_istr2bve SCM_P ((char *str, long len));
extern SCM scm_array_to_list SCM_P ((SCM v));
extern SCM scm_list_to_uniform_array SCM_P ((SCM ndim, SCM prot, SCM lst));
extern int scm_raprin1 SCM_P ((SCM exp, SCM port, scm_print_state *pstate));
extern SCM scm_array_prototype SCM_P ((SCM ra));
extern void scm_init_unif SCM_P ((void));

#endif  /* UNIFH */
