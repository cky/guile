/* classes: h_files */

#ifndef SCM_WEAKS_H
#define SCM_WEAKS_H

/* Copyright (C) 1995,1996,2000,2001, 2003 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"



#define SCM_WVECTF_WEAK_KEY   1
#define SCM_WVECTF_WEAK_VALUE 2
#define SCM_WVECTF_NOSCAN     4

#define SCM_WVECTP(x) (!SCM_IMP (x) && SCM_TYP7 (x) == scm_tc7_wvect)
#define SCM_WVECT_TYPE(x) (SCM_CELL_WORD_2 (x))
#define SCM_SET_WVECT_TYPE(x, t) (SCM_SET_CELL_WORD_2 ((x), (t)))
#define SCM_WVECT_WEAK_KEY_P(x) (SCM_WVECT_TYPE (x) & SCM_WVECTF_WEAK_KEY)
#define SCM_WVECT_WEAK_VALUE_P(x) (SCM_WVECT_TYPE (x) & SCM_WVECTF_WEAK_VALUE)
#define SCM_WVECT_NOSCAN_P(x) (SCM_WVECT_TYPE (x) & SCM_WVECTF_NOSCAN)
#define SCM_IS_WHVEC(X) (SCM_WVECT_TYPE (X) == 1)
#define SCM_IS_WHVEC_V(X) (SCM_WVECT_TYPE (X) == 2)
#define SCM_IS_WHVEC_B(X) (SCM_WVECT_TYPE (X) == 3)
#define SCM_IS_WHVEC_ANY(X) (SCM_WVECT_TYPE (X) != 0)
#define SCM_WVECT_GC_CHAIN(X) (SCM_CELL_OBJECT_3 (X))
#define SCM_SET_WVECT_GC_CHAIN(X, o) (SCM_SET_CELL_OBJECT_3 ((X), (o)))

SCM_API SCM scm_weak_vectors;



SCM_API SCM scm_i_allocate_weak_vector (scm_t_bits type, SCM size, SCM fill, const char* caller);
SCM_API SCM scm_make_weak_vector (SCM k, SCM fill);
SCM_API SCM scm_weak_vector (SCM l);
SCM_API SCM scm_weak_vector_p (SCM x);
SCM_API SCM scm_make_weak_key_alist_vector (SCM k);
SCM_API SCM scm_make_weak_value_alist_vector (SCM k);
SCM_API SCM scm_make_doubly_weak_alist_vector (SCM k);
SCM_API SCM scm_weak_key_alist_vector_p (SCM x);
SCM_API SCM scm_weak_value_alist_vector_p (SCM x);
SCM_API SCM scm_doubly_weak_alist_vector_p (SCM x);
SCM_API void scm_weaks_prehistory (void);
SCM_API SCM scm_init_weaks_builtins (void);
SCM_API void scm_init_weaks (void);

#endif  /* SCM_WEAKS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
