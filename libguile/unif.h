/* classes: h_files */

#ifndef UNIFH
#define UNIFH
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


#include <libguile/__scm.h>


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


#ifdef __STDC__
extern SCM scm_vector_set_length_x (SCM vect, SCM len);
extern SCM scm_makflo (float x);
extern SCM scm_make_uve (long k, SCM prot);
extern SCM scm_uniform_vector_length (SCM v);
extern SCM scm_array_p (SCM v, SCM prot);
extern SCM scm_array_rank (SCM ra);
extern SCM scm_array_dimensions (SCM ra);
extern long scm_aind (SCM ra, SCM args, char *what);
extern SCM scm_make_ra (int ndim);
extern SCM scm_shap2ra (SCM args, char *what);
extern SCM scm_dimensions_to_uniform_array (SCM dims, SCM prot, SCM fill);
extern void scm_ra_set_contp (SCM ra);
extern SCM scm_make_shared_array (SCM oldra, SCM mapfunc, SCM dims);
extern SCM scm_transpose_array (SCM args);
extern SCM scm_enclose_array (SCM axes);
extern SCM scm_array_in_bounds_p (SCM args);
extern SCM scm_uniform_vector_ref (SCM v, SCM args);
extern SCM scm_cvref (SCM v, scm_sizet pos, SCM last);
extern SCM scm_array_set_x (SCM v, SCM obj, SCM args);
extern SCM scm_array_contents (SCM ra, SCM strict);
extern SCM scm_ra2contig (SCM ra, int copy);
extern SCM scm_uniform_array_read_x (SCM ra, SCM port);
extern SCM scm_uniform_array_write (SCM v, SCM port);
extern SCM scm_bit_count (SCM item, SCM seq);
extern SCM scm_bit_position (SCM item, SCM v, SCM k);
extern SCM scm_bit_set_star_x (SCM v, SCM kv, SCM obj);
extern SCM scm_bit_count_star (SCM v, SCM kv, SCM obj);
extern SCM scm_bit_invert_x (SCM v);
extern SCM scm_string_upcase_x (SCM v);
extern SCM scm_string_downcase_x (SCM v);
extern SCM scm_istr2bve (char *str, long len);
extern SCM scm_array_to_list (SCM v);
extern SCM scm_list_to_uniform_array (SCM ndim, SCM prot, SCM lst);
extern int scm_raprin1 (SCM exp, SCM port, int writing);
extern SCM scm_array_prototype (SCM ra);
extern void scm_init_unif (void);
extern int scm_raprin1 (SCM exp, SCM port, int writing);
extern SCM scm_istr2bve (char *str, long len);
extern SCM scm_array_equal_p (SCM ra0, SCM ra1);

#else /* STDC */
extern SCM scm_vector_set_length_x ();
extern SCM scm_makflo ();
extern SCM scm_make_uve ();
extern SCM scm_uniform_vector_length ();
extern SCM scm_array_p ();
extern SCM scm_array_rank ();
extern SCM scm_array_dimensions ();
extern long scm_aind ();
extern SCM scm_make_ra ();
extern SCM scm_shap2ra ();
extern SCM scm_dimensions_to_uniform_array ();
extern void scm_ra_set_contp ();
extern SCM scm_make_shared_array ();
extern SCM scm_transpose_array ();
extern SCM scm_enclose_array ();
extern SCM scm_array_in_bounds_p ();
extern SCM scm_uniform_vector_ref ();
extern SCM scm_cvref ();
extern SCM scm_array_set_x ();
extern SCM scm_array_contents ();
extern SCM scm_ra2contig ();
extern SCM scm_uniform_array_read_x ();
extern SCM scm_uniform_array_write ();
extern SCM scm_bit_count ();
extern SCM scm_bit_position ();
extern SCM scm_bit_set_star_x ();
extern SCM scm_bit_count_star ();
extern SCM scm_bit_invert_x ();
extern SCM scm_string_upcase_x ();
extern SCM scm_string_downcase_x ();
extern SCM scm_istr2bve ();
extern SCM scm_array_to_list ();
extern SCM scm_list_to_uniform_array ();
extern int scm_raprin1 ();
extern SCM scm_array_prototype ();
extern void scm_init_unif ();
extern int scm_raprin1 ();
extern SCM scm_istr2bve ();
extern SCM scm_array_equal_p ();

#endif /* STDC */





#endif  /* UNIFH */
