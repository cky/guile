/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2003 Free Software Foundation, Inc.
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




#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/hash.h"
#include "libguile/eval.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/ports.h"

#include "libguile/validate.h"
#include "libguile/hashtab.h"


/* NOTES
 *
 * 1. The current hash table implementation uses weak alist vectors
 *    (implementation in weaks.c) internally, but we do the scanning
 *    ourselves (in scan_weak_hashtables) because we need to update the
 *    hash table structure when items are dropped during GC.
 *
 * 2. All hash table operations still work on alist vectors.
 *
 */

/* Hash tables are either vectors of association lists or smobs
 * containing such vectors.  Currently, the vector version represents
 * constant size tables while those wrapped in a smob represents
 * resizing tables.
 *
 * Growing or shrinking, with following rehashing, is triggered when
 * the load factor
 *
 *   L = N / S    (N: number of items in table, S: bucket vector length)
 *
 * passes an upper limit of 0.9 or a lower limit of 0.25.
 *
 * The implementation stores the upper and lower number of items which
 * trigger a resize in the hashtable object.
 *
 * Possible hash table sizes (primes) are stored in the array
 * hashtable_size.
 */

scm_t_bits scm_tc16_hashtable;

#define HASHTABLE_SIZE_N 25

static unsigned long hashtable_size[] = {
  31, 61, 113, 223, 443, 883, 1759, 3517, 7027, 14051, 28099, 56197, 112363,
  224717, 449419, 898823, 1797641, 3595271, 7190537, 14381041, 28762081,
  57524111, 115048217, 230096423, 460192829 /* larger values can't be
					       represented as INUMs */
};

/* Turn an empty vector hash table into an opaque resizable one. */

static char *s_hashtable = "hashtable";

SCM weak_hashtables = SCM_EOL;

static SCM
make_hash_table (int flags, unsigned long k, const char *func_name) {
  SCM table, vector;
  scm_t_hashtable *t;
  int i = 0, n = k ? k : 31;
  while (i < HASHTABLE_SIZE_N && n > hashtable_size[i])
    ++i;
  n = hashtable_size[i];
  if (flags)
    /* The SCM_WVECTF_NOSCAN flag informs the weak vector code not to
       perform the final scan for broken references.  Instead we do
       that ourselves in scan_weak_hashtables. */
    vector = scm_i_allocate_weak_vector (flags | SCM_WVECTF_NOSCAN,
					 SCM_MAKINUM (n),
					 SCM_EOL,
					 func_name);
  else
    vector = scm_c_make_vector (n, SCM_EOL);
  t = scm_gc_malloc (sizeof (*t), s_hashtable);
  t->min_size_index = t->size_index = i;
  t->n_items = 0;
  t->lower = 0;
  t->upper = 9 * n / 10;
  t->flags = flags;
  if (flags)
    {
      SCM_NEWSMOB3 (table, scm_tc16_hashtable, vector, t, weak_hashtables);
      weak_hashtables = table;
    }
  else
    SCM_NEWSMOB3 (table, scm_tc16_hashtable, vector, t, SCM_EOL);
  return table;
}


void
scm_i_rehash (SCM table,
	      unsigned long (*hash_fn)(),
	      void *closure,
	      const char* func_name)
{
  SCM buckets, new_buckets;
  int i;
  unsigned long old_size;
  unsigned long new_size;

  if (SCM_HASHTABLE_N_ITEMS (table) < SCM_HASHTABLE_LOWER (table))
    {
      /* rehashing is not triggered when i <= min_size */
      i = SCM_HASHTABLE (table)->size_index;
      do
	--i;
      while (i > SCM_HASHTABLE (table)->min_size_index
	     && SCM_HASHTABLE_N_ITEMS (table) < hashtable_size[i] / 4);
    }
  else
    {
      i = SCM_HASHTABLE (table)->size_index + 1;
      if (i >= HASHTABLE_SIZE_N)
	/* don't rehash */
	return;
      /* store for use in rehash_after_gc */
      SCM_HASHTABLE (table)->hash_fn = hash_fn;
      SCM_HASHTABLE (table)->closure = closure;
    }
  SCM_HASHTABLE (table)->size_index = i;
  
  new_size = hashtable_size[i];
  if (i <= SCM_HASHTABLE (table)->min_size_index)
    SCM_HASHTABLE (table)->lower = 0;
  else
    SCM_HASHTABLE (table)->lower = new_size / 4;
  SCM_HASHTABLE (table)->upper = 9 * new_size / 10;
  buckets = SCM_HASHTABLE_VECTOR (table);
  
  if (SCM_HASHTABLE_WEAK_P (table))
    new_buckets = scm_i_allocate_weak_vector (SCM_HASHTABLE_FLAGS (table)
					      | SCM_WVECTF_NOSCAN,
					      SCM_MAKINUM (new_size),
					      SCM_EOL,
					      func_name);
  else
    new_buckets = scm_c_make_vector (new_size, SCM_EOL);

  old_size = SCM_VECTOR_LENGTH (buckets);
  for (i = 0; i < old_size; ++i)
    {
      SCM ls = SCM_VELTS (buckets)[i], handle;
      while (!SCM_NULLP (ls))
	{
	  unsigned long h;
	  handle = SCM_CAR (ls);
	  h = hash_fn (SCM_CAR (handle), new_size, closure);
	  if (h >= new_size)
	    scm_out_of_range (func_name, scm_ulong2num (h));
	  SCM_VECTOR_SET (new_buckets, h,
			  scm_cons (handle, SCM_VELTS (new_buckets)[h]));
	  ls = SCM_CDR (ls);
	}
    }
  SCM_SET_HASHTABLE_VECTOR (table, new_buckets);
}


static int
hashtable_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_t_hashtable *t = SCM_HASHTABLE (exp);
  scm_puts ("#<", port);
  if (SCM_HASHTABLE_WEAK_KEY_P (exp))
    scm_puts ("weak-key-", port);
  else if (SCM_HASHTABLE_WEAK_VALUE_P (exp))
    scm_puts ("weak-value-", port);
  else if (SCM_HASHTABLE_DOUBLY_WEAK_P (exp))
    scm_puts ("doubly-weak-", port);
  scm_puts ("hash-table ", port);
  scm_intprint ((unsigned long) t->n_items, 10, port);
  scm_putc ('/', port);
  scm_intprint ((unsigned long) SCM_VECTOR_LENGTH (SCM_HASHTABLE_VECTOR (exp)),
		10, port);
  scm_puts (">", port);
  return 1;
}

#define UNMARKED_CELL_P(x) (SCM_NIMP(x) && !SCM_GC_MARK_P (x))

/* keep track of hash tables that need to shrink after scan */
static SCM to_rehash = SCM_EOL;

/* scan hash tables for broken references, remove them, and update
   hash tables item count */
static void *
scan_weak_hashtables (void *dummy1 SCM_UNUSED,
		      void *dummy2 SCM_UNUSED,
		      void *dummy3 SCM_UNUSED)
{
  SCM *next = &weak_hashtables;
  SCM h = *next;
  while (!SCM_NULLP (h))
    {
      if (!SCM_GC_MARK_P (h))
	*next = h = SCM_HASHTABLE_NEXT (h);
      else
	{
	  SCM alist;
	  int i, n = SCM_HASHTABLE_N_BUCKETS (h);
	  int weak_car = SCM_HASHTABLE_FLAGS (h) & SCM_HASHTABLEF_WEAK_CAR;
	  int weak_cdr = SCM_HASHTABLE_FLAGS (h) & SCM_HASHTABLEF_WEAK_CDR;
	  int check_size_p = 0;
	  for (i = 0; i < n; ++i)
	    {
	      SCM *next_spine = (SCM *) &SCM_HASHTABLE_BUCKETS (h)[i];
	      for (alist = *next_spine;
		   !SCM_NULLP (alist);
		   alist = SCM_CDR (alist))
		if ((weak_car && UNMARKED_CELL_P (SCM_CAAR (alist)))
		    || (weak_cdr && UNMARKED_CELL_P (SCM_CDAR (alist))))
		  {
		    *next_spine = SCM_CDR (alist);
		    SCM_HASHTABLE_DECREMENT (h);
		    check_size_p = 1;
		  }
		else
		  next_spine = SCM_CDRLOC (alist);
	    }
	  if (check_size_p
	      && SCM_HASHTABLE_N_ITEMS (h) < SCM_HASHTABLE_LOWER (h))
	    {
	      SCM tmp = SCM_HASHTABLE_NEXT (h);
	      /* temporarily move table from weak_hashtables to to_rehash */
	      SCM_SET_HASHTABLE_NEXT (h, to_rehash);
	      to_rehash = h;
	      *next = h = tmp;
	    }
	  else
	    {
	      next = SCM_HASHTABLE_NEXTLOC (h);
	      h = SCM_HASHTABLE_NEXT (h);
	    }
	}
    }
  return 0;
}

static void *
rehash_after_gc (void *dummy1 SCM_UNUSED,
		 void *dummy2 SCM_UNUSED,
		 void *dummy3 SCM_UNUSED)
{
  if (!SCM_NULLP (to_rehash))
    {
      SCM h = to_rehash, last;
      do
	{
	  scm_i_rehash (h,
			/* use same hash_fn and closure as last time */
			SCM_HASHTABLE (h)->hash_fn,
			SCM_HASHTABLE (h)->closure,
			"rehash_after_gc");
	  last = h;
	  h = SCM_HASHTABLE_NEXT (h);
	} while (!SCM_NULLP (h));
      /* move tables back to weak_hashtables */
      SCM_SET_HASHTABLE_NEXT (last, weak_hashtables);
      weak_hashtables = to_rehash;
      to_rehash = SCM_EOL;
    }
  return 0;
}

static size_t
hashtable_free (SCM obj)
{
  scm_gc_free (SCM_HASHTABLE (obj), sizeof (scm_t_hashtable), s_hashtable);
  return 0;
}


SCM
scm_c_make_hash_table (unsigned long k)
{
  return make_hash_table (0, k, "scm_c_make_hash_table");
}

SCM_DEFINE (scm_make_hash_table, "make-hash-table", 0, 1, 0,
	    (SCM n),
	    "Make a hash table with optional minimum number of buckets @var{n}\n")
#define FUNC_NAME s_scm_make_hash_table
{
  if (SCM_UNBNDP (n))
    return make_hash_table (0, 0, FUNC_NAME);
  else
    {
      int k;
      SCM_VALIDATE_INUM_COPY (1, n, k);
      return make_hash_table (0, k, FUNC_NAME);
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_weak_key_hash_table, "make-weak-key-hash-table", 0, 1, 0, 
	    (SCM n),
	    "@deffnx {Scheme Procedure} make-weak-value-hash-table size\n"
	    "@deffnx {Scheme Procedure} make-doubly-weak-hash-table size\n"
	    "Return a weak hash table with @var{size} buckets. As with any\n"
	    "hash table, choosing a good size for the table requires some\n"
	    "caution.\n"
	    "\n"
	    "You can modify weak hash tables in exactly the same way you\n"
	    "would modify regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_hash_table
{
  if (SCM_UNBNDP (n))
    return make_hash_table (SCM_HASHTABLEF_WEAK_CAR, 0, FUNC_NAME);
  else
    {
      int k;
      SCM_VALIDATE_INUM_COPY (1, n, k);
      return make_hash_table (SCM_HASHTABLEF_WEAK_CAR, k, FUNC_NAME);
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_hash_table, "make-weak-value-hash-table", 0, 1, 0, 
            (SCM n),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_hash_table
{
  if (SCM_UNBNDP (n))
    return make_hash_table (SCM_HASHTABLEF_WEAK_CDR, 0, FUNC_NAME);
  else
    {
      int k;
      SCM_VALIDATE_INUM_COPY (1, n, k);
      return make_hash_table (SCM_HASHTABLEF_WEAK_CDR, k, FUNC_NAME);
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_hash_table, "make-doubly-weak-hash-table", 1, 0, 0, 
            (SCM n),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_hash_table
{
  if (SCM_UNBNDP (n))
    return make_hash_table (SCM_HASHTABLEF_WEAK_CAR | SCM_HASHTABLEF_WEAK_CDR,
			    0,
			    FUNC_NAME);
  else
    {
      int k;
      SCM_VALIDATE_INUM_COPY (1, n, k);
      return make_hash_table (SCM_HASHTABLEF_WEAK_CAR | SCM_HASHTABLEF_WEAK_CDR,
			      k,
			      FUNC_NAME);
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_table_p, "hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a hash table.")
#define FUNC_NAME s_scm_hash_table_p
{
  return SCM_BOOL (SCM_HASHTABLE_P (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_key_hash_table_p, "weak-key-hash-table?", 1, 0, 0, 
           (SCM obj),
	    "@deffnx {Scheme Procedure} weak-value-hash-table? obj\n"
	    "@deffnx {Scheme Procedure} doubly-weak-hash-table? obj\n"
	    "Return @code{#t} if @var{obj} is the specified weak hash\n"
	    "table. Note that a doubly weak hash table is neither a weak key\n"
	    "nor a weak value hash table.")
#define FUNC_NAME s_scm_weak_key_hash_table_p
{
  return SCM_BOOL (SCM_HASHTABLE_P (obj) && SCM_HASHTABLE_WEAK_KEY_P (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_hash_table_p, "weak-value-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak value hash table.")
#define FUNC_NAME s_scm_weak_value_hash_table_p
{
  return SCM_BOOL (SCM_HASHTABLE_P (obj) && SCM_HASHTABLE_WEAK_VALUE_P (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_hash_table_p, "doubly-weak-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a doubly weak hash table.")
#define FUNC_NAME s_scm_doubly_weak_hash_table_p
{
  return SCM_BOOL (SCM_HASHTABLE_P (obj) && SCM_HASHTABLE_DOUBLY_WEAK_P (obj));
}
#undef FUNC_NAME


SCM
scm_hash_fn_get_handle (SCM table, SCM obj, unsigned long (*hash_fn)(), SCM (*assoc_fn)(), void * closure)
#define FUNC_NAME "scm_hash_fn_get_handle"
{
  unsigned long k;
  SCM h;

  if (SCM_HASHTABLE_P (table))
    table = SCM_HASHTABLE_VECTOR (table);
  else
    SCM_VALIDATE_VECTOR (1, table);
  if (SCM_VECTOR_LENGTH (table) == 0)
    return SCM_BOOL_F;
  k = hash_fn (obj, SCM_VECTOR_LENGTH (table), closure);
  if (k >= SCM_VECTOR_LENGTH (table))
    scm_out_of_range ("hash_fn_get_handle", scm_ulong2num (k));
  h = assoc_fn (obj, SCM_VELTS (table)[k], closure);
  return h;
}
#undef FUNC_NAME


SCM
scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init, unsigned long (*hash_fn)(),
                             SCM (*assoc_fn)(), void * closure)
#define FUNC_NAME "scm_hash_fn_create_handle_x"
{
  unsigned long k;
  SCM buckets, it;

  if (SCM_HASHTABLE_P (table))
    buckets = SCM_HASHTABLE_VECTOR (table);
  else
    {
      SCM_ASSERT (SCM_VECTORP (table),
		  table, SCM_ARG1, "hash_fn_create_handle_x");
      buckets = table;
    }
  if (SCM_VECTOR_LENGTH (buckets) == 0)
    SCM_MISC_ERROR ("void hashtable", SCM_EOL);

  k = hash_fn (obj, SCM_VECTOR_LENGTH (buckets), closure);
  if (k >= SCM_VECTOR_LENGTH (buckets))
    scm_out_of_range ("hash_fn_create_handle_x", scm_ulong2num (k));
  it = assoc_fn (obj, SCM_VELTS (buckets)[k], closure);
  if (!SCM_FALSEP (it))
    return it;
  else
    {
      SCM old_bucket = SCM_VELTS (buckets)[k];
      SCM new_bucket = scm_acons (obj, init, old_bucket);
      SCM_VECTOR_SET (buckets, k, new_bucket);
      if (table != buckets)
	{
	  SCM_HASHTABLE_INCREMENT (table);
	  if (SCM_HASHTABLE_N_ITEMS (table) > SCM_HASHTABLE_UPPER (table))
	    scm_i_rehash (table, hash_fn, closure, FUNC_NAME);
	}
      return SCM_CAR (new_bucket);
    }
}
#undef FUNC_NAME


SCM 
scm_hash_fn_ref (SCM table, SCM obj, SCM dflt, unsigned long (*hash_fn)(),
                 SCM (*assoc_fn)(), void * closure)
{
  SCM it = scm_hash_fn_get_handle (table, obj, hash_fn, assoc_fn, closure);
  if (SCM_CONSP (it))
    return SCM_CDR (it);
  else
    return dflt;
}




SCM 
scm_hash_fn_set_x (SCM table, SCM obj, SCM val, unsigned long (*hash_fn)(),
                   SCM (*assoc_fn)(), void * closure)
{
  SCM it;

  it = scm_hash_fn_create_handle_x (table, obj, SCM_BOOL_F, hash_fn, assoc_fn, closure);
  SCM_SETCDR (it, val);
  return val;
}





SCM 
scm_hash_fn_remove_x (SCM table, SCM obj, unsigned long (*hash_fn)(), SCM (*assoc_fn)(),
                      SCM (*delete_fn)(), void * closure)
{
  unsigned long k;
  SCM buckets, h;

  if (SCM_HASHTABLE_P (table))
    buckets = SCM_HASHTABLE_VECTOR (table);
  else
    {
      SCM_ASSERT (SCM_VECTORP (table), table, SCM_ARG1, "hash_fn_remove_x");
      buckets = table;
    }
  if (SCM_VECTOR_LENGTH (table) == 0)
    return SCM_EOL;

  k = hash_fn (obj, SCM_VECTOR_LENGTH (buckets), closure);
  if (k >= SCM_VECTOR_LENGTH (buckets))
    scm_out_of_range ("hash_fn_remove_x", scm_ulong2num (k));
  h = assoc_fn (obj, SCM_VELTS (buckets)[k], closure);
  if (!SCM_FALSEP (h))
    {
      SCM_VECTOR_SET (buckets, k, delete_fn (h, SCM_VELTS (buckets)[k]));
      if (table != buckets)
	{
	  SCM_HASHTABLE_DECREMENT (table);
	  if (SCM_HASHTABLE_N_ITEMS (table) < SCM_HASHTABLE_LOWER (table))
	    scm_i_rehash (table, hash_fn, closure, "scm_hash_fn_remove_x");
	}
    }
  return h;
}

SCM_DEFINE (scm_hash_clear_x, "hash-clear!", 1, 0, 0,
	    (SCM table),
	    "Remove all items from TABLE (without triggering a resize).")
#define FUNC_NAME s_scm_hash_clear_x
{
  SCM_VALIDATE_HASHTABLE (1, table);
  scm_vector_fill_x (SCM_HASHTABLE_VECTOR (table), SCM_EOL);
  SCM_SET_HASHTABLE_N_ITEMS (table, 0);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_get_handle, "hashq-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_get_handle
{
  return scm_hash_fn_get_handle (table, key, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_create_handle_x, "hashq-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hashq_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_ref, "hashq-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{default} (or @code{#f} if no @var{default} argument\n"
	    "is supplied).  Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, key, dflt, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_set_x, "hashq-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{value} there. Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_set_x
{
  return scm_hash_fn_set_x (table, key, val, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_remove_x, "hashq-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{eq?} for equality tests.")
#define FUNC_NAME s_scm_hashq_remove_x
{
  return scm_hash_fn_remove_x (table, key, scm_ihashq, scm_sloppy_assq,
			       scm_delq_x, 0);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashv_get_handle, "hashv-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_get_handle
{
  return scm_hash_fn_get_handle (table, key, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_create_handle_x, "hashv-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hashv_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihashv,
				      scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_ref, "hashv-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{default} (or @code{#f} if no @var{default} argument\n"
	    "is supplied).  Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, key, dflt, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashv_set_x, "hashv-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{value} there. Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_set_x
{
  return scm_hash_fn_set_x (table, key, val, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_remove_x, "hashv-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{eqv?} for equality tests.")
#define FUNC_NAME s_scm_hashv_remove_x
{
  return scm_hash_fn_remove_x (table, key, scm_ihashv, scm_sloppy_assv,
			       scm_delv_x, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_get_handle, "hash-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{equal?} for equality testing.")
#define FUNC_NAME s_scm_hash_get_handle
{
  return scm_hash_fn_get_handle (table, key, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_create_handle_x, "hash-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hash_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_ref, "hash-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{default} (or @code{#f} if no @var{default} argument\n"
	    "is supplied).  Uses @code{equal?} for equality testing.")
#define FUNC_NAME s_scm_hash_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, key, dflt, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_set_x, "hash-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{value} there. Uses @code{equal?} for equality\n"
	    "testing.")
#define FUNC_NAME s_scm_hash_set_x
{
  return scm_hash_fn_set_x (table, key, val, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_remove_x, "hash-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{equal?} for equality tests.")
#define FUNC_NAME s_scm_hash_remove_x
{
  return scm_hash_fn_remove_x (table, key, scm_ihash, scm_sloppy_assoc,
			       scm_delete_x, 0);
}
#undef FUNC_NAME




typedef struct scm_t_ihashx_closure
{
  SCM hash;
  SCM assoc;
  SCM delete;
} scm_t_ihashx_closure;



static unsigned long
scm_ihashx (SCM obj, unsigned long n, scm_t_ihashx_closure *closure)
{
  SCM answer = scm_call_2 (closure->hash,
			   obj,
			   scm_ulong2num ((unsigned long) n));
  return SCM_INUM (answer);
}



static SCM
scm_sloppy_assx (SCM obj, SCM alist, scm_t_ihashx_closure *closure)
{
  return scm_call_2 (closure->assoc, obj, alist);
}




static SCM
scm_delx_x (SCM obj, SCM alist, scm_t_ihashx_closure *closure)
{
  return scm_call_2 (closure->delete, obj, alist);
}



SCM_DEFINE (scm_hashx_get_handle, "hashx-get-handle", 4, 0, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key),
	    "This behaves the same way as the corresponding\n"
	    "@code{-get-handle} function, but uses @var{hash} as a hash\n"
	    "function and @var{assoc} to compare keys.  @code{hash} must be\n"
	    "a function that takes two arguments, a key to be hashed and a\n"
	    "table size.  @code{assoc} must be an associator function, like\n"
	    "@code{assoc}, @code{assq} or @code{assv}.")
#define FUNC_NAME s_scm_hashx_get_handle
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_get_handle (table, key, scm_ihashx, scm_sloppy_assx,
				 (void *) &closure);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashx_create_handle_x, "hashx-create-handle!", 5, 0, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key, SCM init),
	    "This behaves the same way as the corresponding\n"
	    "@code{-create-handle} function, but uses @var{hash} as a hash\n"
	    "function and @var{assoc} to compare keys.  @code{hash} must be\n"
	    "a function that takes two arguments, a key to be hashed and a\n"
	    "table size.  @code{assoc} must be an associator function, like\n"
	    "@code{assoc}, @code{assq} or @code{assv}.")
#define FUNC_NAME s_scm_hashx_create_handle_x
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihashx,
				      scm_sloppy_assx, (void *)&closure);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashx_ref, "hashx-ref", 4, 1, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key, SCM dflt),
	    "This behaves the same way as the corresponding @code{ref}\n"
	    "function, but uses @var{hash} as a hash function and\n"
	    "@var{assoc} to compare keys.  @code{hash} must be a function\n"
	    "that takes two arguments, a key to be hashed and a table size.\n"
	    "@code{assoc} must be an associator function, like @code{assoc},\n"
	    "@code{assq} or @code{assv}.\n"
	    "\n"
	    "By way of illustration, @code{hashq-ref table key} is\n"
	    "equivalent to @code{hashx-ref hashq assq table key}.")
#define FUNC_NAME s_scm_hashx_ref
{
  scm_t_ihashx_closure closure;
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_ref (table, key, dflt, scm_ihashx, scm_sloppy_assx,
			  (void *)&closure);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashx_set_x, "hashx-set!", 5, 0, 0,
            (SCM hash, SCM assoc, SCM table, SCM key, SCM val),
	    "This behaves the same way as the corresponding @code{set!}\n"
	    "function, but uses @var{hash} as a hash function and\n"
	    "@var{assoc} to compare keys.  @code{hash} must be a function\n"
	    "that takes two arguments, a key to be hashed and a table size.\n"
	    "@code{assoc} must be an associator function, like @code{assoc},\n"
	    "@code{assq} or @code{assv}.\n"
	    "\n"
	    " By way of illustration, @code{hashq-set! table key} is\n"
	    "equivalent to @code{hashx-set!  hashq assq table key}.")
#define FUNC_NAME s_scm_hashx_set_x
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_set_x (table, key, val, scm_ihashx, scm_sloppy_assx,
			    (void *)&closure);
}
#undef FUNC_NAME



SCM
scm_hashx_remove_x (SCM hash, SCM assoc, SCM delete, SCM table, SCM obj)
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.delete = delete;
  return scm_hash_fn_remove_x (table, obj, scm_ihashx, scm_sloppy_assx, scm_delx_x, 0);
}

static SCM
fold_proc (void *proc, SCM key, SCM data, SCM value)
{
  return scm_call_3 (SCM_PACK (proc), key, data, value);
}

SCM_DEFINE (scm_hash_fold, "hash-fold", 3, 0, 0, 
            (SCM proc, SCM init, SCM table),
	    "An iterator over hash-table elements.\n"
            "Accumulates and returns a result by applying PROC successively.\n"
            "The arguments to PROC are \"(key value prior-result)\" where key\n"
            "and value are successive pairs from the hash table TABLE, and\n"
            "prior-result is either INIT (for the first application of PROC)\n"
            "or the return value of the previous application of PROC.\n"
            "For example, @code{(hash-fold acons '() tab)} will convert a hash\n"
            "table into an a-list of key-value pairs.")
#define FUNC_NAME s_scm_hash_fold
{
  SCM_VALIDATE_PROC (1, proc);
  if (!SCM_HASHTABLE_P (table))
    SCM_VALIDATE_VECTOR (3, table);
  return scm_internal_hash_fold (fold_proc, (void *) SCM_UNPACK (proc), init, table);
}
#undef FUNC_NAME

SCM
scm_internal_hash_fold (SCM (*fn) (), void *closure, SCM init, SCM table)
{
  long i, n;
  SCM buckets, result = init;
  
  if (SCM_HASHTABLE_P (table))
    buckets = SCM_HASHTABLE_VECTOR (table);
  else
    buckets = table;
  
  n = SCM_VECTOR_LENGTH (buckets);
  for (i = 0; i < n; ++i)
    {
      SCM ls = SCM_VELTS (buckets)[i], handle;
      while (!SCM_NULLP (ls))
	{
	  if (!SCM_CONSP (ls))
	    scm_wrong_type_arg (s_scm_hash_fold, SCM_ARG3, buckets);
	  handle = SCM_CAR (ls);
	  if (!SCM_CONSP (handle))
	    scm_wrong_type_arg (s_scm_hash_fold, SCM_ARG3, buckets);
	  result = fn (closure, SCM_CAR (handle), SCM_CDR (handle), result);
	  ls = SCM_CDR (ls);
	}
    }

  return result;
}

static SCM
for_each_proc (void *proc, SCM key, SCM data, SCM value)
{
  return scm_call_2 (SCM_PACK (proc), key, data);
}

SCM_DEFINE (scm_hash_for_each, "hash-for-each", 2, 0, 0, 
            (SCM proc, SCM table),
	    "An iterator over hash-table elements.\n"
            "Applies PROC successively on all hash table items.\n"
            "The arguments to PROC are \"(key value)\" where key\n"
            "and value are successive pairs from the hash table TABLE.")
#define FUNC_NAME s_scm_hash_for_each
{
  SCM_VALIDATE_PROC (1, proc);
  if (!SCM_HASHTABLE_P (table))
    SCM_VALIDATE_VECTOR (2, table);
  scm_internal_hash_fold (for_each_proc,
			  (void *) SCM_UNPACK (proc),
			  SCM_BOOL_F,
			  table);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
map_proc (void *proc, SCM key, SCM data, SCM value)
{
  return scm_cons (scm_call_2 (SCM_PACK (proc), key, data), value);
}

SCM_DEFINE (scm_hash_map, "hash-map", 2, 0, 0, 
            (SCM proc, SCM table),
	    "An iterator over hash-table elements.\n"
            "Accumulates and returns as a list the results of applying PROC successively.\n"
            "The arguments to PROC are \"(key value)\" where key\n"
            "and value are successive pairs from the hash table TABLE.")
#define FUNC_NAME s_scm_hash_map
{
  SCM_VALIDATE_PROC (1, proc);
  if (!SCM_HASHTABLE_P (table))
    SCM_VALIDATE_VECTOR (2, table);
  return scm_internal_hash_fold (map_proc,
				 (void *) SCM_UNPACK (proc),
				 SCM_EOL,
				 table);
}
#undef FUNC_NAME




void
scm_hashtab_prehistory ()
{
  scm_tc16_hashtable = scm_make_smob_type (s_hashtable, 0);
  scm_set_smob_mark (scm_tc16_hashtable, scm_markcdr);
  scm_set_smob_print (scm_tc16_hashtable, hashtable_print);
  scm_set_smob_free (scm_tc16_hashtable, hashtable_free);
  scm_c_hook_add (&scm_after_sweep_c_hook, scan_weak_hashtables, 0, 0);
  scm_c_hook_add (&scm_after_gc_c_hook, rehash_after_gc, 0, 0);
}

void
scm_init_hashtab ()
{
#include "libguile/hashtab.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
