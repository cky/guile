/* classes: h_files */

#ifndef SCM_WEAKS_H
#define SCM_WEAKS_H

/* Copyright (C) 1995,1996,2000,2001, 2003, 2006, 2008, 2009, 2011, 2014 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#include "libguile/__scm.h"



#define SCM_WVECTF_WEAK_KEY   1
#define SCM_WVECTF_WEAK_VALUE 2

#define SCM_WVECT_WEAK_KEY_P(x) (SCM_I_WVECT_EXTRA(x) & SCM_WVECTF_WEAK_KEY)
#define SCM_WVECT_WEAK_VALUE_P(x) (SCM_I_WVECT_EXTRA(x) & SCM_WVECTF_WEAK_VALUE)

#define SCM_I_WVECT_TYPE(x)       (SCM_I_WVECT_EXTRA(x) & 7)
#define SCM_I_SET_WVECT_TYPE(x,t) (SCM_I_SET_WVECT_EXTRA		\
				   ((x), (SCM_I_WVECT_EXTRA (x) & ~7) | (t)))
#define SCM_IS_WHVEC(X)           (SCM_I_WVECT_TYPE (X) == 1)
#define SCM_IS_WHVEC_V(X)         (SCM_I_WVECT_TYPE (X) == 2)
#define SCM_IS_WHVEC_B(X)         (SCM_I_WVECT_TYPE (X) == 3)
#define SCM_IS_WHVEC_ANY(X)       (SCM_I_WVECT_TYPE (X) != 0)


/* Weak pairs.  */

SCM_INTERNAL SCM scm_weak_car_pair (SCM car, SCM cdr);
SCM_INTERNAL SCM scm_weak_cdr_pair (SCM car, SCM cdr);
SCM_INTERNAL SCM scm_doubly_weak_pair (SCM car, SCM cdr);

/* Testing the weak component(s) of a cell for reachability.  */
#define SCM_WEAK_PAIR_WORD_DELETED_P(_cell, _word)		\
  (SCM_UNPACK (SCM_CELL_OBJECT ((_cell), (_word))) == 0)
#define SCM_WEAK_PAIR_CAR_DELETED_P(_cell)	\
  (SCM_WEAK_PAIR_WORD_DELETED_P ((_cell), 0))
#define SCM_WEAK_PAIR_CDR_DELETED_P(_cell)	\
  (SCM_WEAK_PAIR_WORD_DELETED_P ((_cell), 1))

#define SCM_WEAK_PAIR_DELETED_P(_cell)		\
  ((SCM_WEAK_PAIR_CAR_DELETED_P (_cell))	\
   || (SCM_WEAK_PAIR_CDR_DELETED_P (_cell)))

/* Accessing the components of a weak cell.  These return `SCM_UNDEFINED' if
   the car/cdr has been collected.  */
#define SCM_WEAK_PAIR_WORD(_cell, _word)		\
  (SCM_WEAK_PAIR_WORD_DELETED_P ((_cell), (_word))	\
   ? SCM_UNDEFINED					\
   : SCM_CELL_OBJECT ((_cell), (_word)))
#define SCM_WEAK_PAIR_CAR(_cell)  (SCM_WEAK_PAIR_WORD ((_cell), 0))
#define SCM_WEAK_PAIR_CDR(_cell)  (SCM_WEAK_PAIR_WORD ((_cell), 1))



/* Weak vectors and weak hash tables.  */

SCM_API SCM scm_make_weak_vector (SCM k, SCM fill);
SCM_API SCM scm_weak_vector (SCM l);
SCM_API SCM scm_weak_vector_p (SCM x);
SCM_API SCM scm_weak_vector_length (SCM v);
SCM_API SCM scm_weak_vector_ref (SCM v, SCM k);
SCM_API SCM scm_weak_vector_set_x (SCM v, SCM k, SCM x);

SCM_API SCM scm_c_make_weak_vector (size_t k, SCM fill);
SCM_API int scm_is_weak_vector (SCM obj);
SCM_API size_t scm_c_weak_vector_length (SCM vec);
SCM_API SCM scm_c_weak_vector_ref (SCM v, size_t k);
SCM_API void scm_c_weak_vector_set_x (SCM v, size_t k, SCM x);

SCM_API SCM scm_make_weak_key_alist_vector (SCM k);
SCM_API SCM scm_make_weak_value_alist_vector (SCM k);
SCM_API SCM scm_make_doubly_weak_alist_vector (SCM k);
SCM_API SCM scm_weak_key_alist_vector_p (SCM x);
SCM_API SCM scm_weak_value_alist_vector_p (SCM x);
SCM_API SCM scm_doubly_weak_alist_vector_p (SCM x);
SCM_INTERNAL SCM scm_init_weaks_builtins (void);
SCM_INTERNAL void scm_weaks_prehistory (void);
SCM_INTERNAL void scm_init_weaks (void);

SCM_INTERNAL void scm_i_init_weak_vectors_for_gc (void);
SCM_INTERNAL void scm_i_mark_weak_vector (SCM w);
SCM_INTERNAL int scm_i_mark_weak_vectors_non_weaks (void);
SCM_INTERNAL void scm_i_remove_weaks_from_weak_vectors (void);


#endif  /* SCM_WEAKS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
