/* classes: h_files */

#ifndef SCM_HASHTAB_H
#define SCM_HASHTAB_H

/* Copyright (C) 1995,1996,1999,2000,2001, 2003 Free Software Foundation, Inc.
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

#include "weaks.h"



#define SCM_HASHTABLEF_WEAK_CAR SCM_WVECTF_WEAK_KEY
#define SCM_HASHTABLEF_WEAK_CDR SCM_WVECTF_WEAK_VALUE

#define SCM_HASHTABLE_P(x)	   SCM_TYP16_PREDICATE (scm_tc16_hashtable, x)
#define SCM_VALIDATE_HASHTABLE(pos, arg) \
  SCM_MAKE_VALIDATE_MSG (pos, arg, HASHTABLE_P, "hash-table")
#define SCM_HASHTABLE_VECTOR(h)  SCM_CELL_OBJECT_1 (h)
#define SCM_SET_HASHTABLE_VECTOR(x, v) SCM_SET_CELL_OBJECT_1 (x, v)
#define SCM_HASHTABLE(x)	   ((scm_t_hashtable *) SCM_CELL_WORD_2 (x))
#define SCM_HASHTABLE_NEXT(x)	   SCM_CELL_OBJECT_3 (x)
#define SCM_HASHTABLE_NEXTLOC(x)   ((SCM *) SCM_CELL_WORD_LOC (x, 3))
#define SCM_SET_HASHTABLE_NEXT(x, n) SCM_SET_CELL_OBJECT_3 (x, n)
#define SCM_HASHTABLE_FLAGS(x)	   (SCM_HASHTABLE (x)->flags)
#define SCM_HASHTABLE_WEAK_KEY_P(x) \
  (SCM_HASHTABLE_FLAGS (x) & SCM_HASHTABLEF_WEAK_CAR)
#define SCM_HASHTABLE_WEAK_VALUE_P(x) \
  (SCM_HASHTABLE_FLAGS (x) & SCM_HASHTABLEF_WEAK_CDR)
#define SCM_HASHTABLE_DOUBLY_WEAK_P(x)				\
  ((SCM_HASHTABLE_FLAGS (x)					\
    & (SCM_HASHTABLEF_WEAK_CAR | SCM_HASHTABLEF_WEAK_CDR))	\
   == (SCM_HASHTABLEF_WEAK_CAR | SCM_HASHTABLEF_WEAK_CDR))
#define SCM_HASHTABLE_WEAK_P(x)	   SCM_HASHTABLE_FLAGS (x)
#define SCM_HASHTABLE_N_ITEMS(x)   (SCM_HASHTABLE (x)->n_items)
#define SCM_SET_HASHTABLE_N_ITEMS(x, n)   (SCM_HASHTABLE (x)->n_items = n)
#define SCM_HASHTABLE_INCREMENT(x) (SCM_HASHTABLE_N_ITEMS(x)++)
#define SCM_HASHTABLE_DECREMENT(x) (SCM_HASHTABLE_N_ITEMS(x)--)
#define SCM_HASHTABLE_UPPER(x)     (SCM_HASHTABLE (x)->upper)
#define SCM_HASHTABLE_LOWER(x)     (SCM_HASHTABLE (x)->lower)

#define SCM_HASHTABLE_N_BUCKETS(h) \
 SCM_VECTOR_LENGTH (SCM_HASHTABLE_VECTOR (h))
#define SCM_HASHTABLE_BUCKETS(h) SCM_VELTS (SCM_HASHTABLE_VECTOR (h))
#define SCM_SET_HASHTABLE_BUCKET(h, i, x) \
  SCM_VECTOR_SET (SCM_HASHTABLE_VECTOR (h), i, x)

typedef struct scm_t_hashtable {
  int flags;			/* properties of table */
  unsigned long n_items;	/* number of items in table */
  unsigned long lower;		/* when to shrink */
  unsigned long upper;		/* when to grow */
  int size_index;		/* index into hashtable_size */
  int min_size_index;		/* minimum size_index */
  unsigned long (*hash_fn) ();
  void *closure;
} scm_t_hashtable;



#if 0
typedef unsigned int scm_t_hash_fn (SCM obj, unsigned int d, void *closure);
typedef SCM scm_t_assoc_fn (SCM key, SCM alist, void *closure);
typedef SCM scm_t_delete_fn (SCM elt, SCM list);
#endif

SCM_API SCM scm_vector_to_hash_table (SCM vector);
SCM_API SCM scm_c_make_hash_table (unsigned long k);
SCM_API SCM scm_make_hash_table (SCM n);
SCM_API SCM scm_make_weak_key_hash_table (SCM k);
SCM_API SCM scm_make_weak_value_hash_table (SCM k);
SCM_API SCM scm_make_doubly_weak_hash_table (SCM k);

SCM_API SCM scm_hash_table_p (SCM h);
SCM_API SCM scm_weak_key_hash_table_p (SCM h);
SCM_API SCM scm_weak_value_hash_table_p (SCM h);
SCM_API SCM scm_doubly_weak_hash_table_p (SCM h);

SCM_API void scm_i_rehash (SCM table, unsigned long (*hash_fn)(), void *closure, const char*func_name);

SCM_API SCM scm_hash_fn_get_handle (SCM table, SCM obj, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_ref (SCM table, SCM obj, SCM dflt, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_set_x (SCM table, SCM obj, SCM val, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_remove_x (SCM table, SCM obj, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), SCM (*delete_fn) (), void * closure);
SCM_API SCM scm_internal_hash_fold (SCM (*fn) (), void *closure, SCM init, SCM table);
SCM_API SCM scm_hash_clear_x (SCM table);

SCM_API SCM scm_hashq_get_handle (SCM table, SCM obj);
SCM_API SCM scm_hashq_create_handle_x (SCM table, SCM obj, SCM init);
SCM_API SCM scm_hashq_ref (SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hashq_set_x (SCM table, SCM obj, SCM val);
SCM_API SCM scm_hashq_remove_x (SCM table, SCM obj);
SCM_API SCM scm_hashv_get_handle (SCM table, SCM obj);
SCM_API SCM scm_hashv_create_handle_x (SCM table, SCM obj, SCM init);
SCM_API SCM scm_hashv_ref (SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hashv_set_x (SCM table, SCM obj, SCM val);
SCM_API SCM scm_hashv_remove_x (SCM table, SCM obj);
SCM_API SCM scm_hash_get_handle (SCM table, SCM obj);
SCM_API SCM scm_hash_create_handle_x (SCM table, SCM obj, SCM init);
SCM_API SCM scm_hash_ref (SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hash_set_x (SCM table, SCM obj, SCM val);
SCM_API SCM scm_hash_remove_x (SCM table, SCM obj);
SCM_API SCM scm_hashx_get_handle (SCM hash, SCM assoc, SCM table, SCM obj);
SCM_API SCM scm_hashx_create_handle_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM init);
SCM_API SCM scm_hashx_ref (SCM hash, SCM assoc, SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hashx_set_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM val);
SCM_API SCM scm_hashx_remove_x (SCM hash, SCM assoc, SCM del, SCM table, SCM obj);
SCM_API SCM scm_hash_fold (SCM proc, SCM init, SCM hash);
SCM_API SCM scm_hash_for_each (SCM proc, SCM hash);
SCM_API SCM scm_hash_map (SCM proc, SCM hash);
SCM_API void scm_hashtab_prehistory (void);
SCM_API void scm_init_hashtab (void);

#endif  /* SCM_HASHTAB_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
