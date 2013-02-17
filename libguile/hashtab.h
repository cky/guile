/* classes: h_files */

#ifndef SCM_HASHTAB_H
#define SCM_HASHTAB_H

/* Copyright (C) 1995,1996,1999,2000,2001, 2003, 2004, 2006, 2008, 2009 Free Software Foundation, Inc.
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

#include "weaks.h"



#define SCM_HASHTABLEF_WEAK_CAR SCM_WVECTF_WEAK_KEY
#define SCM_HASHTABLEF_WEAK_CDR SCM_WVECTF_WEAK_VALUE

#define SCM_HASHTABLE_P(x) (!SCM_IMP (x) && SCM_TYP7(x) == scm_tc7_hashtable)
#define SCM_VALIDATE_HASHTABLE(pos, arg) \
  SCM_MAKE_VALIDATE_MSG (pos, arg, HASHTABLE_P, "hash-table")
#define SCM_HASHTABLE_VECTOR(h)  SCM_CELL_OBJECT_1 (h)
#define SCM_SET_HASHTABLE_VECTOR(x, v) SCM_SET_CELL_OBJECT_1 ((x), (v))
#define SCM_HASHTABLE(x)	   ((scm_t_hashtable *) SCM_CELL_WORD_2 (x))
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
 SCM_SIMPLE_VECTOR_LENGTH (SCM_HASHTABLE_VECTOR (h))
#define SCM_HASHTABLE_BUCKET(h, i) \
  SCM_SIMPLE_VECTOR_REF (SCM_HASHTABLE_VECTOR (h), i)
#define SCM_SET_HASHTABLE_BUCKET(h, i, x) \
  SCM_SIMPLE_VECTOR_SET (SCM_HASHTABLE_VECTOR (h), i, x)

/* Function that computes a hash of OBJ modulo MAX.  */
typedef unsigned long (*scm_t_hash_fn) (SCM obj, unsigned long max,
					void *closure);

/* Function that returns the value associated with OBJ in ALIST according to
   some equality predicate.  */
typedef SCM (*scm_t_assoc_fn) (SCM obj, SCM alist, void *closure);

/* Function that returns true if the given object is the one we are
   looking for, for scm_hash_fn_ref_by_hash.  */
typedef int (*scm_t_hash_predicate_fn) (SCM obj, void *closure);

/* Function to fold over the entries of a hash table.  */
typedef SCM (*scm_t_hash_fold_fn) (void *closure, SCM key, SCM value,
				   SCM result);

/* Function to iterate over the handles (key-value pairs) of a hash
   table.  */
typedef SCM (*scm_t_hash_handle_fn) (void *closure, SCM handle);

typedef struct scm_t_hashtable {
  int flags;			/* properties of table */
  unsigned long n_items;	/* number of items in table */
  unsigned long lower;		/* when to shrink */
  unsigned long upper;		/* when to grow */
  int size_index;		/* index into hashtable_size */
  int min_size_index;		/* minimum size_index */
  scm_t_hash_fn hash_fn;  /* for rehashing after a GC. */
} scm_t_hashtable;



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

SCM_INTERNAL void scm_i_rehash (SCM table, scm_t_hash_fn hash_fn,
				void *closure, const char *func_name);


SCM_API SCM scm_hash_fn_get_handle (SCM table, SCM obj,
				    scm_t_hash_fn hash_fn,
				    scm_t_assoc_fn assoc_fn,
				    void *closure);
SCM_INTERNAL
SCM scm_hash_fn_get_handle_by_hash (SCM table, unsigned long raw_hash,
                                    scm_t_hash_predicate_fn predicate_fn,
                                    void *closure);
SCM_API SCM scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init,
					 scm_t_hash_fn hash_fn,
					 scm_t_assoc_fn assoc_fn,
					 void *closure);
SCM_API SCM scm_hash_fn_ref (SCM table, SCM obj, SCM dflt,
			     scm_t_hash_fn hash_fn,
			     scm_t_assoc_fn assoc_fn,
			     void *closure);
SCM_API SCM scm_hash_fn_set_x (SCM table, SCM obj, SCM val,
			       scm_t_hash_fn hash_fn,
			       scm_t_assoc_fn assoc_fn,
			       void *closure);
SCM_API SCM scm_hash_fn_remove_x (SCM table, SCM obj,
				  scm_t_hash_fn hash_fn,
				  scm_t_assoc_fn assoc_fn,
				  void *closure);
SCM_API SCM scm_internal_hash_fold (scm_t_hash_fold_fn fn, void *closure,
				    SCM init, SCM table);
SCM_API void scm_internal_hash_for_each_handle (scm_t_hash_handle_fn fn,
						void *closure, SCM table);
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
SCM_API SCM scm_hashx_remove_x (SCM hash, SCM assoc, SCM table, SCM obj);
SCM_API SCM scm_hash_fold (SCM proc, SCM init, SCM hash);
SCM_API SCM scm_hash_for_each (SCM proc, SCM hash);
SCM_API SCM scm_hash_for_each_handle (SCM proc, SCM hash);
SCM_API SCM scm_hash_map_to_list (SCM proc, SCM hash);
SCM_API SCM scm_hash_count (SCM hash, SCM pred);
SCM_INTERNAL void scm_i_hashtable_print (SCM exp, SCM port, scm_print_state *pstate);
SCM_INTERNAL void scm_init_hashtab (void);

#endif  /* SCM_HASHTAB_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
