/* classes: h_files */

#ifndef SCM_GC_H
#define SCM_GC_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001 Free Software Foundation, Inc.
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

#include "libguile/hooks.h"



typedef struct scm_cell
{
  scm_t_bits word_0;
  scm_t_bits word_1;
} scm_cell;


/* SCM_CELLPTR is a pointer to a cons cell which may be compared or
 * differenced.
 */
typedef scm_cell * SCM_CELLPTR;


/* Cray machines have pointers that are incremented once for each word,
 * rather than each byte, the 3 most significant bits encode the byte
 * within the word.  The following macros deal with this by storing the
 * native Cray pointers like the ones that looks like scm expects.  This
 * is done for any pointers that might appear in the car of a scm_cell,
 * pointers to scm_vector elts, functions, &c are not munged.
 */
#ifdef _UNICOS
#  define SCM2PTR(x) ((SCM_CELLPTR) (SCM_UNPACK (x) >> 3))
#  define PTR2SCM(x) (SCM_PACK (((scm_t_bits) (x)) << 3))
#else
#  define SCM2PTR(x) ((SCM_CELLPTR) (SCM_UNPACK (x)))
#  define PTR2SCM(x) (SCM_PACK ((scm_t_bits) (x)))
#endif /* def _UNICOS */

#define SCM_GC_CARD_N_HEADER_CELLS 1
#define SCM_GC_CARD_N_CELLS        256

#define SCM_GC_CARD_SIZE           (SCM_GC_CARD_N_CELLS * sizeof (scm_cell))
#define SCM_GC_CARD_N_DATA_CELLS   (SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS)

#define SCM_GC_CARD_BVEC_SIZE_IN_LIMBS \
    ((SCM_GC_CARD_N_CELLS + SCM_C_BVEC_LIMB_BITS - 1) / SCM_C_BVEC_LIMB_BITS)

#define SCM_GC_IN_CARD_HEADERP(x) \
    SCM_PTR_LT ((scm_cell *) (x), SCM_GC_CELL_CARD (x) + SCM_GC_CARD_N_HEADER_CELLS)

#define SCM_GC_CARD_BVEC(card)  ((scm_t_c_bvec_limb *) ((card)->word_0))
#define SCM_GC_SET_CARD_BVEC(card, bvec) \
    ((card)->word_0 = (scm_t_bits) (bvec))

#define SCM_GC_GET_CARD_FLAGS(card) ((long) ((card)->word_1))
#define SCM_GC_SET_CARD_FLAGS(card, flags) \
    ((card)->word_1 = (scm_t_bits) (flags))
#define SCM_GC_CLR_CARD_FLAGS(card) (SCM_GC_SET_CARD_FLAGS (card, 0L))

#define SCM_GC_GET_CARD_FLAG(card, shift) (SCM_GC_GET_CARD_FLAGS (card) & (1L << (shift)))
#define SCM_GC_SET_CARD_FLAG(card, shift) \
    (SCM_GC_SET_CARD_FLAGS (card, SCM_GC_GET_CARD_FLAGS(card) | (1L << (shift))))
#define SCM_GC_CLR_CARD_FLAG(card, shift) \
    (SCM_GC_SET_CARD_FLAGS (card, SCM_GC_GET_CARD_FLAGS(card) & ~(1L << (shift))))

#define SCM_GC_CARDF_DOUBLECELL 0

#define SCM_GC_CARD_DOUBLECELLP(card)    SCM_GC_GET_CARD_FLAG (card, SCM_GC_CARDF_DOUBLECELL)
#define SCM_GC_SET_CARD_DOUBLECELL(card) SCM_GC_SET_CARD_FLAG (card, SCM_GC_CARDF_DOUBLECELL)

/* card addressing. for efficiency, cards are *always* aligned to
   SCM_GC_CARD_SIZE. */

#define SCM_GC_CARD_SIZE_MASK  (SCM_GC_CARD_SIZE - 1)
#define SCM_GC_CARD_ADDR_MASK  (~SCM_GC_CARD_SIZE_MASK)

#define SCM_GC_CELL_CARD(x)    ((SCM_CELLPTR) ((long) (x) & SCM_GC_CARD_ADDR_MASK))
#define SCM_GC_CELL_SPAN(x)    ((SCM_GC_CARD_DOUBLECELLP (SCM_GC_CELL_CARD (x))) ? 2 : 1)
#define SCM_GC_CELL_OFFSET(x)  (((long) (x) & SCM_GC_CARD_SIZE_MASK) >> SCM_CELL_SIZE_SHIFT)
#define SCM_GC_CELL_BVEC(x)    SCM_GC_CARD_BVEC (SCM_GC_CELL_CARD (x))
#define SCM_GC_CELL_GET_BIT(x) SCM_C_BVEC_GET (SCM_GC_CELL_BVEC (x), SCM_GC_CELL_OFFSET (x))
#define SCM_GC_CELL_SET_BIT(x) SCM_C_BVEC_SET (SCM_GC_CELL_BVEC (x), SCM_GC_CELL_OFFSET (x))
#define SCM_GC_CELL_CLR_BIT(x) SCM_C_BVEC_CLR (SCM_GC_CELL_BVEC (x), SCM_GC_CELL_OFFSET (x))

#define SCM_GC_CARD_UP(x)      SCM_GC_CELL_CARD ((char *) (x) + SCM_GC_CARD_SIZE - 1)
#define SCM_GC_CARD_DOWN       SCM_GC_CELL_CARD

/* low level bit banging aids */

typedef unsigned long scm_t_c_bvec_limb;

#if (SIZEOF_LONG == 8)
#       define SCM_C_BVEC_LIMB_BITS    64
#       define SCM_C_BVEC_OFFSET_SHIFT 6
#       define SCM_C_BVEC_POS_MASK     63
#       define SCM_CELL_SIZE_SHIFT     4
#else
#       define SCM_C_BVEC_LIMB_BITS    32
#       define SCM_C_BVEC_OFFSET_SHIFT 5
#       define SCM_C_BVEC_POS_MASK     31
#       define SCM_CELL_SIZE_SHIFT     3
#endif

#define SCM_C_BVEC_OFFSET(pos) (pos >> SCM_C_BVEC_OFFSET_SHIFT)

#define SCM_C_BVEC_GET(bvec, pos) (bvec[SCM_C_BVEC_OFFSET (pos)] & (1L << (pos & SCM_C_BVEC_POS_MASK)))
#define SCM_C_BVEC_SET(bvec, pos) (bvec[SCM_C_BVEC_OFFSET (pos)] |= (1L << (pos & SCM_C_BVEC_POS_MASK)))
#define SCM_C_BVEC_CLR(bvec, pos) (bvec[SCM_C_BVEC_OFFSET (pos)] &= ~(1L << (pos & SCM_C_BVEC_POS_MASK)))

#define SCM_C_BVEC_BITS2BYTES(bits) \
    (sizeof (scm_t_c_bvec_limb) * ((((bits) & SCM_C_BVEC_POS_MASK) ? 1L : 0L) + SCM_C_BVEC_OFFSET (bits)))

#define SCM_C_BVEC_SET_BYTES(bvec, bytes)   (memset (bvec, 0xff, bytes))
#define SCM_C_BVEC_SET_ALL_BITS(bvec, bits) SCM_C_BVEC_SET_BYTES (bvec, SCM_C_BVEC_BITS2BYTES (bits))

#define SCM_C_BVEC_CLR_BYTES(bvec, bytes)   (memset (bvec, 0, bytes))
#define SCM_C_BVEC_CLR_ALL_BITS(bvec, bits) SCM_C_BVEC_CLR_BYTES (bvec, SCM_C_BVEC_BITS2BYTES (bits))

/* testing and changing GC marks */

#define SCM_GCMARKP(x)   SCM_GC_CELL_GET_BIT (x)
#define SCM_SETGCMARK(x) SCM_GC_CELL_SET_BIT (x)
#define SCM_CLRGCMARK(x) SCM_GC_CELL_CLR_BIT (x)


/* Low level cell data accessing macros.  These macros should only be used
 * from within code related to garbage collection issues, since they will
 * never check the cells they are applied to - not even if guile is compiled
 * in debug mode.  In particular these macros will even work for free cells,
 * which should never be encountered by user code.  */

#define SCM_GC_CELL_WORD(x, n) \
  (((const scm_t_bits *) SCM2PTR (x)) [n])
#define SCM_GC_CELL_OBJECT(x, n) \
  (SCM_PACK (((const scm_t_bits *) SCM2PTR (x)) [n]))
#define SCM_GC_SET_CELL_WORD(x, n, v) \
  (((scm_t_bits *) SCM2PTR (x)) [n] = (scm_t_bits) (v))
#define SCM_GC_SET_CELL_OBJECT(x, n, v) \
  (((scm_t_bits *) SCM2PTR (x)) [n] = SCM_UNPACK (v))
#define SCM_GC_CELL_TYPE(x) SCM_GC_CELL_WORD (x, 0)


/* Except for the garbage collector, no part of guile should ever run over a
 * free cell.  Thus, if guile is compiled in debug mode the SCM_CELL_* and
 * SCM_SET_CELL_* macros below report an error if they are applied to a free
 * cell.  Some other plausibility checks are also performed.  However, if
 * guile is not compiled in debug mode, there won't be any time penalty at all
 * when using these macros.  */

#if (SCM_DEBUG_CELL_ACCESSES == 1)
#  define SCM_VALIDATE_CELL(cell, expr) (scm_assert_cell_valid (cell), (expr))
#else
#  define SCM_VALIDATE_CELL(cell, expr) (expr)
#endif

#define SCM_CELL_WORD(x, n) \
  SCM_VALIDATE_CELL ((x), SCM_GC_CELL_WORD ((x), (n)))
#define SCM_CELL_WORD_0(x) SCM_CELL_WORD (x, 0)
#define SCM_CELL_WORD_1(x) SCM_CELL_WORD (x, 1)
#define SCM_CELL_WORD_2(x) SCM_CELL_WORD (x, 2)
#define SCM_CELL_WORD_3(x) SCM_CELL_WORD (x, 3)

#define SCM_CELL_OBJECT(x, n) \
  SCM_VALIDATE_CELL ((x), SCM_GC_CELL_OBJECT ((x), (n)))
#define SCM_CELL_OBJECT_0(x) SCM_CELL_OBJECT (x, 0)
#define SCM_CELL_OBJECT_1(x) SCM_CELL_OBJECT (x, 1)
#define SCM_CELL_OBJECT_2(x) SCM_CELL_OBJECT (x, 2)
#define SCM_CELL_OBJECT_3(x) SCM_CELL_OBJECT (x, 3)

#define SCM_SET_CELL_WORD(x, n, v) \
  SCM_VALIDATE_CELL ((x), SCM_GC_SET_CELL_WORD ((x), (n), (v)))
#define SCM_SET_CELL_WORD_0(x, v) SCM_SET_CELL_WORD (x, 0, v)
#define SCM_SET_CELL_WORD_1(x, v) SCM_SET_CELL_WORD (x, 1, v)
#define SCM_SET_CELL_WORD_2(x, v) SCM_SET_CELL_WORD (x, 2, v)
#define SCM_SET_CELL_WORD_3(x, v) SCM_SET_CELL_WORD (x, 3, v)

#define SCM_SET_CELL_OBJECT(x, n, v) \
  SCM_VALIDATE_CELL ((x), SCM_GC_SET_CELL_OBJECT ((x), (n), (v)))
#define SCM_SET_CELL_OBJECT_0(x, v) SCM_SET_CELL_OBJECT (x, 0, v)
#define SCM_SET_CELL_OBJECT_1(x, v) SCM_SET_CELL_OBJECT (x, 1, v)
#define SCM_SET_CELL_OBJECT_2(x, v) SCM_SET_CELL_OBJECT (x, 2, v)
#define SCM_SET_CELL_OBJECT_3(x, v) SCM_SET_CELL_OBJECT (x, 3, v)

#define SCM_CELL_TYPE(x) SCM_CELL_WORD_0 (x)
#define SCM_SET_CELL_TYPE(x, t) SCM_SET_CELL_WORD_0 (x, t)


/* Freelists consist of linked cells where the type entry holds the value
 * scm_tc_free_cell and the second entry holds a pointer to the next cell of
 * the freelist.  Due to this structure, freelist cells are not cons cells
 * and thus may not be accessed using SCM_CAR and SCM_CDR.  */

#define SCM_FREE_CELL_P(x) \
  (!SCM_IMP (x) && (SCM_GC_CELL_TYPE (x) == scm_tc_free_cell))
#define SCM_FREE_CELL_CDR(x) \
  (SCM_GC_CELL_OBJECT ((x), 1))
#define SCM_SET_FREE_CELL_CDR(x, v) \
  (SCM_GC_SET_CELL_OBJECT ((x), 1, (v)))


#define SCM_CELL_WORD_LOC(x, n) ((scm_t_bits *) & SCM_CELL_WORD (x, n))
#define SCM_CARLOC(x) ((SCM *) SCM_CELL_WORD_LOC ((x), 0))
#define SCM_CDRLOC(x) ((SCM *) SCM_CELL_WORD_LOC ((x), 1))


/* SCM_PTR_LT and friends define how to compare two SCM_CELLPTRs (which may
 * point to cells in different heap segments).
 */
#define SCM_PTR_LT(x, y) ((x) < (y))
#define SCM_PTR_GT(x, y) (SCM_PTR_LT (y, x))
#define SCM_PTR_LE(x, y) (!SCM_PTR_GT (x, y))
#define SCM_PTR_GE(x, y) (!SCM_PTR_LT (x, y))


#define SCM_MARKEDP    SCM_GCMARKP
#define SCM_NMARKEDP(x) (!SCM_MARKEDP (x))

#if (SCM_DEBUG_CELL_ACCESSES == 1)
SCM_API unsigned int scm_debug_cell_accesses_p;
#endif

SCM_API struct scm_t_heap_seg_data *scm_heap_table;
SCM_API size_t scm_n_heap_segs;
SCM_API int scm_block_gc;
SCM_API int scm_gc_heap_lock;
SCM_API unsigned int scm_gc_running_p;


SCM_API size_t scm_default_init_heap_size_1;
SCM_API int scm_default_min_yield_1;
SCM_API size_t scm_default_init_heap_size_2;
SCM_API int scm_default_min_yield_2;
SCM_API size_t scm_default_max_segment_size;

SCM_API size_t scm_max_segment_size;
SCM_API SCM_CELLPTR scm_heap_org;
SCM_API SCM scm_freelist;
SCM_API struct scm_t_freelist scm_master_freelist;
SCM_API SCM scm_freelist2;
SCM_API struct scm_t_freelist scm_master_freelist2;
SCM_API unsigned long scm_gc_cells_collected;
SCM_API unsigned long scm_gc_yield;
SCM_API unsigned long scm_gc_malloc_collected;
SCM_API unsigned long scm_gc_ports_collected;
SCM_API unsigned long scm_cells_allocated;
SCM_API unsigned long scm_mallocated;
SCM_API unsigned long scm_mtrigger;

SCM_API SCM scm_after_gc_hook;

SCM_API scm_t_c_hook scm_before_gc_c_hook;
SCM_API scm_t_c_hook scm_before_mark_c_hook;
SCM_API scm_t_c_hook scm_before_sweep_c_hook;
SCM_API scm_t_c_hook scm_after_sweep_c_hook;
SCM_API scm_t_c_hook scm_after_gc_c_hook;

#if defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST)
SCM_API SCM scm_map_free_list (void);
SCM_API SCM scm_free_list_length (void);
#endif
#ifdef GUILE_DEBUG_FREELIST
SCM_API SCM scm_gc_set_debug_check_freelist_x (SCM flag);
#endif



#if (SCM_DEBUG_CELL_ACCESSES == 1)
SCM_API void scm_assert_cell_valid (SCM);
SCM_API SCM scm_set_debug_cell_accesses_x (SCM flag);
#endif
SCM_API SCM scm_object_address (SCM obj);
SCM_API SCM scm_gc_stats (void);
SCM_API SCM scm_gc (void);
SCM_API void scm_gc_for_alloc (struct scm_t_freelist *freelist);
SCM_API SCM scm_gc_for_newcell (struct scm_t_freelist *master, SCM *freelist);
#if 0
SCM_API void scm_alloc_cluster (struct scm_t_freelist *master);
#endif
SCM_API void scm_igc (const char *what);
SCM_API void scm_gc_mark (SCM p);
SCM_API void scm_gc_mark_dependencies (SCM p);
SCM_API void scm_mark_locations (SCM_STACKITEM x[], unsigned long n);
SCM_API int scm_cellp (SCM value);
SCM_API void scm_gc_sweep (void);

SCM_API void *scm_malloc (size_t size);
SCM_API void *scm_realloc (void *mem, size_t size);
SCM_API char *scm_strdup (const char *str);
SCM_API char *scm_strndup (const char *str, size_t n);
SCM_API void scm_gc_register_collectable_memory (void *mem, size_t size,
						 const char *what);
SCM_API void scm_gc_unregister_collectable_memory (void *mem, size_t size,
						   const char *what);
SCM_API void *scm_gc_malloc (size_t size, const char *what);
SCM_API void *scm_gc_realloc (void *mem, size_t old_size, 
			      size_t new_size, const char *what);
SCM_API void scm_gc_free (void *mem, size_t size, const char *what);
SCM_API char *scm_gc_strdup (const char *str, const char *what);
SCM_API char *scm_gc_strndup (const char *str, size_t n, const char *what);

SCM_API void scm_remember_upto_here_1 (SCM obj);
SCM_API void scm_remember_upto_here_2 (SCM obj1, SCM obj2);
SCM_API void scm_remember_upto_here (SCM obj1, ...);
SCM_API SCM scm_return_first (SCM elt, ...);
SCM_API int scm_return_first_int (int x, ...);
SCM_API SCM scm_permanent_object (SCM obj);
SCM_API SCM scm_gc_protect_object (SCM obj);
SCM_API SCM scm_gc_unprotect_object (SCM obj);
SCM_API void scm_gc_register_root (SCM *p);
SCM_API void scm_gc_unregister_root (SCM *p);
SCM_API void scm_gc_register_roots (SCM *b, unsigned long n);
SCM_API void scm_gc_unregister_roots (SCM *b, unsigned long n);
SCM_API int scm_init_storage (void);
SCM_API void *scm_get_stack_base (void);
SCM_API void scm_init_gc (void);

#if SCM_ENABLE_DEPRECATED == 1

SCM_API SCM scm_deprecated_newcell (void);
SCM_API SCM scm_deprecated_newcell2 (void);

#define SCM_NEWCELL(_into) \
  do { _into = scm_deprecated_newcell (); } while (0)
#define SCM_NEWCELL2(_into) \
  do { _into = scm_deprecated_newcell2 (); } while (0)

SCM_API void * scm_must_malloc (size_t len, const char *what);
SCM_API void * scm_must_realloc (void *where,
				 size_t olen, size_t len,
				 const char *what);
SCM_API char *scm_must_strdup (const char *str);
SCM_API char *scm_must_strndup (const char *str, size_t n);
SCM_API void scm_done_malloc (long size);
SCM_API void scm_done_free (long size);
SCM_API void scm_must_free (void *obj);

#endif

#endif  /* SCM_GC_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
