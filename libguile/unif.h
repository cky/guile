/* classes: h_files */

#ifndef SCM_UNIFORM_VECTORS_H
#define SCM_UNIFORM_VECTORS_H
/* Copyright (C) 1995,1996,1997,1999,2000,2001 Free Software Foundation, Inc.
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



/*
  an array SCM is a non-immediate pointing to a  heap cell with:

   CAR: bits 0-14 hold the dimension (0 -- 32767)
        bit  15 is the SCM_ARRAY_FLAG_CONTIGUOUS flag
        bits 16-31 hold the smob type id: scm_tc16_array
   CDR: pointer to a malloced block containing an scm_array structure
        followed by an scm_array_dim_t structure for each dimension.
*/

typedef struct scm_array_t
{
  SCM v;  /* the contents of the array, e.g., a vector or uniform vector.  */
  unsigned long base;
} scm_array_t;

typedef struct scm_array_dim_t
{
  long lbnd;
  long ubnd;
  long inc;
} scm_array_dim_t;

#if (SCM_DEBUG_DEPRECATED == 0)
# define scm_array scm_array_t
# define scm_array_dim scm_array_dim_t
#endif

extern scm_bits_t scm_tc16_array;

#define SCM_ARRAY_FLAG_CONTIGUOUS (1 << 16)

#if (SCM_DEBUG_DEPRECATED == 0)
#define SCM_ARRAY_CONTIGUOUS SCM_ARRAY_FLAG_CONTIGUOUS
#endif

#define SCM_ARRAYP(a) 	    SCM_TYP16_PREDICATE (scm_tc16_array, a)
#define SCM_ARRAY_NDIM(x)   ((size_t) (SCM_CELL_WORD_0 (x) >> 17))
#define SCM_ARRAY_CONTP(x)  (SCM_CELL_WORD_0 (x) & SCM_ARRAY_FLAG_CONTIGUOUS)
#define SCM_SET_ARRAY_CONTIGUOUS_FLAG(x) \
  (SCM_SET_CELL_WORD_0 ((x), SCM_CELL_WORD_0 (x) | SCM_ARRAY_FLAG_CONTIGUOUS))
#define SCM_CLR_ARRAY_CONTIGUOUS_FLAG(x) \
  (SCM_SET_CELL_WORD_0 ((x), SCM_CELL_WORD_0 (x) & ~SCM_ARRAY_FLAG_CONTIGUOUS))

#define SCM_ARRAY_MEM(a)  ((scm_array_t *) SCM_CELL_WORD_1 (a))
#define SCM_ARRAY_V(a) 	  (SCM_ARRAY_MEM (a)->v)
#define SCM_ARRAY_BASE(a) (SCM_ARRAY_MEM (a)->base)
#define SCM_ARRAY_DIMS(a) ((scm_array_dim_t *)((char *) SCM_ARRAY_MEM (a) + sizeof (scm_array_t))) 

#define SCM_I_MAX_LENGTH  ((unsigned long) (-1L) >> 8)

#define SCM_UVECTOR_BASE(x) ((void *) (SCM_CELL_WORD_1 (x)))
#define SCM_SET_UVECTOR_BASE(v, b) (SCM_SET_CELL_WORD_1 ((v), (b)))
#define SCM_UVECTOR_MAX_LENGTH SCM_I_MAX_LENGTH
#define SCM_UVECTOR_LENGTH(x) (((unsigned long) SCM_CELL_WORD_0 (x)) >> 8)
#define SCM_SET_UVECTOR_LENGTH(v, l, t) (SCM_SET_CELL_WORD_0 ((v), ((l) << 8) + (t)))

#define SCM_BITVECTOR_P(x) (!SCM_IMP (x) && (SCM_TYP7 (x) == scm_tc7_bvect))
#define SCM_BITVECTOR_BASE(x) ((unsigned long *) (SCM_CELL_WORD_1 (x)))
#define SCM_SET_BITVECTOR_BASE(v, b) (SCM_SET_CELL_WORD_1 ((v), (b)))
#define SCM_BITVECTOR_MAX_LENGTH SCM_I_MAX_LENGTH
#define SCM_BITVECTOR_LENGTH(x) (((unsigned long) SCM_CELL_WORD_0 (x)) >> 8)
#define SCM_SET_BITVECTOR_LENGTH(v, l) (SCM_SET_CELL_WORD_0 ((v), ((l) << 8) + scm_tc7_bvect))



extern size_t scm_uniform_element_size (SCM obj);
extern SCM scm_make_uve (long k, SCM prot);
extern SCM scm_uniform_vector_length (SCM v);
extern SCM scm_array_p (SCM v, SCM prot);
extern SCM scm_array_rank (SCM ra);
extern SCM scm_array_dimensions (SCM ra);
extern SCM scm_shared_array_root (SCM ra);
extern SCM scm_shared_array_offset (SCM ra);
extern SCM scm_shared_array_increments (SCM ra);
extern long scm_aind (SCM ra, SCM args, const char *what);
extern SCM scm_make_ra (int ndim);
extern SCM scm_shap2ra (SCM args, const char *what);
extern SCM scm_dimensions_to_uniform_array (SCM dims, SCM prot, SCM fill);
extern void scm_ra_set_contp (SCM ra);
extern SCM scm_make_shared_array (SCM oldra, SCM mapfunc, SCM dims);
extern SCM scm_transpose_array (SCM ra, SCM args);
extern SCM scm_enclose_array (SCM ra, SCM axes);
extern SCM scm_array_in_bounds_p (SCM v, SCM args);
extern SCM scm_uniform_vector_ref (SCM v, SCM args);
extern SCM scm_cvref (SCM v, unsigned long pos, SCM last);
extern SCM scm_array_set_x (SCM v, SCM obj, SCM args);
extern SCM scm_array_contents (SCM ra, SCM strict);
extern SCM scm_ra2contig (SCM ra, int copy);
extern SCM scm_uniform_array_read_x (SCM ra, SCM port_or_fd, SCM start, SCM end);
extern SCM scm_uniform_array_write (SCM v, SCM port_or_fd, SCM start, SCM end);
extern SCM scm_bit_count (SCM item, SCM seq);
extern SCM scm_bit_position (SCM item, SCM v, SCM k);
extern SCM scm_bit_set_star_x (SCM v, SCM kv, SCM obj);
extern SCM scm_bit_count_star (SCM v, SCM kv, SCM obj);
extern SCM scm_bit_invert_x (SCM v);
extern SCM scm_istr2bve (char *str, long len);
extern SCM scm_array_to_list (SCM v);
extern SCM scm_list_to_uniform_array (SCM ndim, SCM prot, SCM lst);
extern int scm_raprin1 (SCM exp, SCM port, scm_print_state *pstate);
extern SCM scm_array_prototype (SCM ra);
extern void scm_init_unif (void);



#if (SCM_DEBUG_DEPRECATED == 0)

/* apparently it's possible to have more than SCM_LENGTH_MAX elements
   in an array: if the length is SCM_LENGTH_MAX then the SCM_VELTS
   block begins with the true length (a long int).  I wonder if it
   works.  */
#define SCM_HUGE_LENGTH(x)\
  (SCM_LENGTH_MAX==SCM_LENGTH(x) ? *((long *)SCM_VELTS(x)) : SCM_LENGTH(x))

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

#endif  /* SCM_UNIFORM_VECTORS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
