/* classes: h_files */

#ifndef SCM_VECTORS_H
#define SCM_VECTORS_H

/* Copyright (C) 1995,1996,1998,2000,2001, 2002 Free Software Foundation, Inc.
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



#define SCM_VECTORP(x) (!SCM_IMP (x) && (SCM_TYP7S (x) == scm_tc7_vector))
#define SCM_VECTOR_BASE(x) ((scm_t_bits *) SCM_CELL_WORD_1 (x))
#define SCM_SET_VECTOR_BASE(v, b) (SCM_SET_CELL_WORD_1 ((v), (b)))
#define SCM_VECTOR_MAX_LENGTH ((1L << 24) - 1)
#define SCM_VECTOR_LENGTH(x) (((unsigned long) SCM_CELL_WORD_0 (x)) >> 8)
#define SCM_MAKE_VECTOR_TAG(l, t)  (((l) << 8) + (t))
#define SCM_SET_VECTOR_LENGTH(v, l, t) (SCM_SET_CELL_WORD_0 ((v), SCM_MAKE_VECTOR_TAG(l, t)))

#define SCM_VELTS(x) ((const SCM *) SCM_CELL_WORD_1 (x))
#define SCM_VELTS_AS_STACKITEMS(x) ((SCM_STACKITEM *) SCM_CELL_WORD_1 (x))
#define SCM_SETVELTS(x, v) (SCM_SET_CELL_WORD_1 ((x), (v)))
#define SCM_VECTOR_REF(x, idx) (((const SCM *) SCM_CELL_WORD_1 (x))[(idx)])
#define SCM_VECTOR_SET(x, idx, val) (((SCM*)SCM_CELL_WORD_1 (x))[(idx)] = (val))

#define SCM_GC_WRITABLE_VELTS(x) ((SCM*) SCM_VELTS(x))

/*
  no WB yet.
 */
#define SCM_WRITABLE_VELTS(x) ((SCM*) SCM_VELTS(x))


/*
  bit vectors
 */
#define SCM_BITVEC_REF(a, i) ((SCM_BITVECTOR_BASE (a) [(i) / SCM_LONG_BIT] & (1L << ((i) % SCM_LONG_BIT))) ? 1 : 0)
#define SCM_BITVEC_SET(a, i) SCM_BITVECTOR_BASE (a) [(i) / SCM_LONG_BIT] |= (1L << ((i) % SCM_LONG_BIT))
#define SCM_BITVEC_CLR(a, i) SCM_BITVECTOR_BASE (a) [(i) / SCM_LONG_BIT] &= ~(1L << ((i) % SCM_LONG_BIT))




SCM_API SCM scm_c_make_vector (unsigned long int k, SCM fill);

SCM_API SCM scm_vector_p (SCM x);
SCM_API SCM scm_vector_length (SCM v);
SCM_API SCM scm_vector (SCM l);
SCM_API SCM scm_vector_ref (SCM v, SCM k);
SCM_API SCM scm_vector_set_x (SCM v, SCM k, SCM obj);
SCM_API SCM scm_make_vector (SCM k, SCM fill);
SCM_API SCM scm_vector_to_list (SCM v);
SCM_API SCM scm_vector_fill_x (SCM v, SCM fill_x);
SCM_API SCM scm_vector_equal_p (SCM x, SCM y);
SCM_API SCM scm_vector_move_left_x (SCM vec1, SCM start1, SCM end1,
				    SCM vec2, SCM start2);
SCM_API SCM scm_vector_move_right_x (SCM vec1, SCM start1, SCM end1, 
				     SCM vec2, SCM start2);
SCM_API void scm_init_vectors (void);

#endif  /* SCM_VECTORS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
