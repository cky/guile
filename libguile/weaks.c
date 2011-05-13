/* Copyright (C) 1995,1996,1998,2000,2001, 2003, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/weaks.h"

#include "libguile/bdw-gc.h"
#include <gc/gc_typed.h>



/* Weak pairs for use in weak alist vectors and weak hash tables.

   We have weal-car pairs, weak-cdr pairs, and doubly weak pairs.  In weak
   pairs, the weak component(s) are not scanned for pointers and are
   registered as disapperaring links; therefore, the weak component may be
   set to NULL by the garbage collector when no other reference to that word
   exist.  Thus, users should only access weak pairs via the
   `SCM_WEAK_PAIR_C[AD]R ()' macros.  See also `scm_fixup_weak_alist ()' in
   `hashtab.c'.  */

/* Type descriptors for weak-c[ad]r pairs.  */
static GC_descr wcar_pair_descr, wcdr_pair_descr;


SCM
scm_weak_car_pair (SCM car, SCM cdr)
{
  scm_t_cell *cell;

  cell = (scm_t_cell *)GC_malloc_explicitly_typed (sizeof (*cell),
						   wcar_pair_descr);

  cell->word_0 = car;
  cell->word_1 = cdr;

  if (SCM_NIMP (car))
    /* Weak car cells make sense iff the car is non-immediate.  */
    SCM_I_REGISTER_DISAPPEARING_LINK ((GC_PTR) &cell->word_0,
                                      (GC_PTR) SCM2PTR (car));

  return (SCM_PACK (cell));
}

SCM
scm_weak_cdr_pair (SCM car, SCM cdr)
{
  scm_t_cell *cell;

  cell = (scm_t_cell *)GC_malloc_explicitly_typed (sizeof (*cell),
						   wcdr_pair_descr);

  cell->word_0 = car;
  cell->word_1 = cdr;

  if (SCM_NIMP (cdr))
    /* Weak cdr cells make sense iff the cdr is non-immediate.  */
    SCM_I_REGISTER_DISAPPEARING_LINK ((GC_PTR) &cell->word_1,
                                      (GC_PTR) SCM2PTR (cdr));

  return (SCM_PACK (cell));
}

SCM
scm_doubly_weak_pair (SCM car, SCM cdr)
{
  /* Doubly weak cells shall not be scanned at all for pointers.  */
  scm_t_cell *cell = (scm_t_cell *)scm_gc_malloc_pointerless (sizeof (*cell),
							      "weak cell");

  cell->word_0 = car;
  cell->word_1 = cdr;

  if (SCM_NIMP (car))
    SCM_I_REGISTER_DISAPPEARING_LINK ((GC_PTR) &cell->word_0,
                                      (GC_PTR) SCM2PTR (car));
  if (SCM_NIMP (cdr))
    SCM_I_REGISTER_DISAPPEARING_LINK ((GC_PTR) &cell->word_1,
                                      (GC_PTR) SCM2PTR (cdr));

  return (SCM_PACK (cell));
}




/* 1. The current hash table implementation in hashtab.c uses weak alist
 *    vectors (formerly called weak hash tables) internally.
 *
 * 2. All hash table operations still work on alist vectors.
 *
 * 3. The weak vector and alist vector Scheme API is accessed through
 *    the module (ice-9 weak-vector).
 */


/* {Weak Vectors}
 */


SCM_DEFINE (scm_make_weak_vector, "make-weak-vector", 1, 1, 0,
	    (SCM size, SCM fill),
	    "Return a weak vector with @var{size} elements. If the optional\n"
	    "argument @var{fill} is given, all entries in the vector will be\n"
	    "set to @var{fill}. The default value for @var{fill} is the\n"
	    "empty list.")
#define FUNC_NAME s_scm_make_weak_vector
{
  return scm_i_make_weak_vector (0, size, fill);
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);

SCM_DEFINE (scm_weak_vector, "weak-vector", 0, 0, 1, 
           (SCM l),
	    "@deffnx {Scheme Procedure} list->weak-vector l\n"
	    "Construct a weak vector from a list: @code{weak-vector} uses\n"
	    "the list of its arguments while @code{list->weak-vector} uses\n"
	    "its only argument @var{l} (a list) to construct a weak vector\n"
	    "the same way @code{list->vector} would.")
#define FUNC_NAME s_scm_weak_vector
{
  return scm_i_make_weak_vector_from_list (0, l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_vector_p, "weak-vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak vector. Note that all\n"
	    "weak hashes are also weak vectors.")
#define FUNC_NAME s_scm_weak_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && !SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME


/* Weak alist vectors, i.e., vectors of alists.

   The alist vector themselves are _not_ weak.  The `car' (or `cdr', or both)
   of the pairs within it are weak.  See `hashtab.c' for details.  */


/* FIXME: We used to have two implementations of weak hash tables: the one in
   here and the one in `hashtab.c'.  The difference is that weak alist
   vectors could be used as vectors while (weak) hash tables can't.  We need
   to unify that.  */

SCM_DEFINE (scm_make_weak_key_alist_vector, "make-weak-key-alist-vector", 0, 1, 0, 
	    (SCM size),
	    "@deffnx {Scheme Procedure} make-weak-value-alist-vector size\n"
	    "@deffnx {Scheme Procedure} make-doubly-weak-alist-vector size\n"
	    "Return a weak hash table with @var{size} buckets. As with any\n"
	    "hash table, choosing a good size for the table requires some\n"
	    "caution.\n"
	    "\n"
	    "You can modify weak hash tables in exactly the same way you\n"
	    "would modify regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_alist_vector
{
  return scm_make_weak_key_hash_table (size);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_alist_vector, "make-weak-value-alist-vector", 0, 1, 0, 
            (SCM size),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_alist_vector
{
  return scm_make_weak_value_hash_table (size);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_alist_vector, "make-doubly-weak-alist-vector", 1, 0, 0, 
            (SCM size),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_alist_vector
{
  return scm_make_doubly_weak_hash_table (size);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_key_alist_vector_p, "weak-key-alist-vector?", 1, 0, 0, 
           (SCM obj),
	    "@deffnx {Scheme Procedure} weak-value-alist-vector? obj\n"
	    "@deffnx {Scheme Procedure} doubly-weak-alist-vector? obj\n"
	    "Return @code{#t} if @var{obj} is the specified weak hash\n"
	    "table. Note that a doubly weak hash table is neither a weak key\n"
	    "nor a weak value hash table.")
#define FUNC_NAME s_scm_weak_key_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_alist_vector_p, "weak-value-alist-vector?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak value hash table.")
#define FUNC_NAME s_scm_weak_value_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC_V (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_alist_vector_p, "doubly-weak-alist-vector?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a doubly weak hash table.")
#define FUNC_NAME s_scm_doubly_weak_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC_B (obj));
}
#undef FUNC_NAME




SCM
scm_init_weaks_builtins ()
{
#include "libguile/weaks.x"
  return SCM_UNSPECIFIED;
}

void
scm_weaks_prehistory ()
{
  /* Initialize weak pairs.  */
  GC_word wcar_pair_bitmap[GC_BITMAP_SIZE (scm_t_cell)] = { 0 };
  GC_word wcdr_pair_bitmap[GC_BITMAP_SIZE (scm_t_cell)] = { 0 };

  /* In a weak-car pair, only the second word must be scanned for
     pointers.  */
  GC_set_bit (wcar_pair_bitmap, GC_WORD_OFFSET (scm_t_cell, word_1));
  wcar_pair_descr = GC_make_descriptor (wcar_pair_bitmap,
					GC_WORD_LEN (scm_t_cell));

  /* Conversely, in a weak-cdr pair, only the first word must be scanned for
     pointers.  */
  GC_set_bit (wcdr_pair_bitmap, GC_WORD_OFFSET (scm_t_cell, word_0));
  wcdr_pair_descr = GC_make_descriptor (wcdr_pair_bitmap,
					GC_WORD_LEN (scm_t_cell));

}

void
scm_init_weaks ()
{
  scm_c_define_gsubr ("%init-weaks-builtins", 0, 0, 0,
		      scm_init_weaks_builtins);
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
