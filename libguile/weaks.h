/* classes: h_files */

#ifndef SCM_WEAKS_H
#define SCM_WEAKS_H

/* Copyright (C) 1995,1996,2000,2001 Free Software Foundation, Inc.
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



#define SCM_WVECTP(x) (!SCM_IMP (x) && SCM_TYP7 (x) == scm_tc7_wvect)
#define SCM_WVECT_TYPE(x) (SCM_CELL_WORD_2 (x))
#define SCM_SET_WVECT_TYPE(x, t) (SCM_SET_CELL_WORD_2 ((x), (t)))
#define SCM_IS_WHVEC(X) (SCM_WVECT_TYPE (X) == 1)
#define SCM_IS_WHVEC_V(X) (SCM_WVECT_TYPE (X) == 2)
#define SCM_IS_WHVEC_B(X) (SCM_WVECT_TYPE (X) == 3)
#define SCM_IS_WHVEC_ANY(X) (SCM_WVECT_TYPE (X) != 0)
#define SCM_WVECT_GC_CHAIN(X) (SCM_CELL_OBJECT_3 (X))
#define SCM_SET_WVECT_GC_CHAIN(X, o) (SCM_SET_CELL_OBJECT_3 ((X), (o)))

SCM_API SCM scm_weak_vectors;



SCM_API SCM scm_make_weak_vector (SCM k, SCM fill);
SCM_API SCM scm_weak_vector (SCM l);
SCM_API SCM scm_weak_vector_p (SCM x);
SCM_API SCM scm_make_weak_key_hash_table (SCM k);
SCM_API SCM scm_make_weak_value_hash_table (SCM k);
SCM_API SCM scm_make_doubly_weak_hash_table (SCM k);
SCM_API SCM scm_weak_key_hash_table_p (SCM x);
SCM_API SCM scm_weak_value_hash_table_p (SCM x);
SCM_API SCM scm_doubly_weak_hash_table_p (SCM x);
SCM_API void scm_weaks_prehistory (void);
SCM_API void scm_init_weaks (void);

#endif  /* SCM_WEAKS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
