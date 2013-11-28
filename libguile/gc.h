/* classes: h_files */

#ifndef SCM_GC_H
#define SCM_GC_H

/* Copyright (C) 1995, 1996, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006,
 *   2007, 2008, 2009, 2010, 2011, 2013 Free Software Foundation, Inc.
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

#include "libguile/hooks.h"
#include "libguile/threads.h"


typedef struct scm_t_cell
{
  SCM word_0;
  SCM word_1;
} scm_t_cell;

/* Cray machines have pointers that are incremented once for each
 * word, rather than each byte, the 3 most significant bits encode the
 * byte within the word.  The following macros deal with this by
 * storing the native Cray pointers like the ones that looks like scm
 * expects.  This is done for any pointers that point to a cell,
 * pointers to scm_vector elts, functions, &c are not munged.
 */
#ifdef _UNICOS
#  define SCM2PTR(x) ((scm_t_cell *) (SCM_UNPACK (x) >> 3))
#  define PTR2SCM(x) (SCM_PACK (((scm_t_bits) (x)) << 3))
#else
#  define SCM2PTR(x) ((scm_t_cell *) (SCM_UNPACK (x)))
#  define PTR2SCM(x) (SCM_PACK ((scm_t_bits) (x)))
#endif /* def _UNICOS */



/* Low level cell data accessing macros.  These macros should only be used
 * from within code related to garbage collection issues, since they will
 * never check the cells they are applied to - not even if guile is compiled
 * in debug mode.  In particular these macros will even work for free cells,
 * which should never be encountered by user code.  */

#define SCM_GC_CELL_OBJECT(x, n) (((SCM *)SCM2PTR (x)) [n])
#define SCM_GC_CELL_WORD(x, n)   (SCM_UNPACK (SCM_GC_CELL_OBJECT ((x), (n))))

#define SCM_GC_SET_CELL_OBJECT(x, n, v) ((((SCM *)SCM2PTR (x)) [n]) = (v))
#define SCM_GC_SET_CELL_WORD(x, n, v)  \
  (SCM_GC_SET_CELL_OBJECT ((x), (n), SCM_PACK (v)))

#define SCM_GC_CELL_TYPE(x) (SCM_GC_CELL_OBJECT ((x), 0))


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
#define SCM_CELL_WORD_0(x) SCM_CELL_WORD ((x), 0)
#define SCM_CELL_WORD_1(x) SCM_CELL_WORD ((x), 1)
#define SCM_CELL_WORD_2(x) SCM_CELL_WORD ((x), 2)
#define SCM_CELL_WORD_3(x) SCM_CELL_WORD ((x), 3)

#define SCM_CELL_OBJECT(x, n) \
  SCM_VALIDATE_CELL ((x), SCM_GC_CELL_OBJECT ((x), (n)))
#define SCM_CELL_OBJECT_0(x) SCM_CELL_OBJECT ((x), 0)
#define SCM_CELL_OBJECT_1(x) SCM_CELL_OBJECT ((x), 1)
#define SCM_CELL_OBJECT_2(x) SCM_CELL_OBJECT ((x), 2)
#define SCM_CELL_OBJECT_3(x) SCM_CELL_OBJECT ((x), 3)

#define SCM_SET_CELL_WORD(x, n, v) \
  SCM_VALIDATE_CELL ((x), SCM_GC_SET_CELL_WORD ((x), (n), (v)))
#define SCM_SET_CELL_WORD_0(x, v) SCM_SET_CELL_WORD ((x), 0, (v))
#define SCM_SET_CELL_WORD_1(x, v) SCM_SET_CELL_WORD ((x), 1, (v))
#define SCM_SET_CELL_WORD_2(x, v) SCM_SET_CELL_WORD ((x), 2, (v))
#define SCM_SET_CELL_WORD_3(x, v) SCM_SET_CELL_WORD ((x), 3, (v))

#define SCM_SET_CELL_OBJECT(x, n, v) \
  SCM_VALIDATE_CELL ((x), SCM_GC_SET_CELL_OBJECT ((x), (n), (v)))
#define SCM_SET_CELL_OBJECT_0(x, v) SCM_SET_CELL_OBJECT ((x), 0, (v))
#define SCM_SET_CELL_OBJECT_1(x, v) SCM_SET_CELL_OBJECT ((x), 1, (v))
#define SCM_SET_CELL_OBJECT_2(x, v) SCM_SET_CELL_OBJECT ((x), 2, (v))
#define SCM_SET_CELL_OBJECT_3(x, v) SCM_SET_CELL_OBJECT ((x), 3, (v))

#define SCM_CELL_OBJECT_LOC(x, n) (SCM_VALIDATE_CELL((x), &SCM_GC_CELL_OBJECT ((x), (n))))
#define SCM_CARLOC(x)             (SCM_CELL_OBJECT_LOC ((x), 0))
#define SCM_CDRLOC(x)             (SCM_CELL_OBJECT_LOC ((x), 1))

#define SCM_CELL_TYPE(x) SCM_CELL_WORD_0 (x)
#define SCM_SET_CELL_TYPE(x, t) SCM_SET_CELL_WORD_0 ((x), (t))


#if (SCM_DEBUG_CELL_ACCESSES == 1)
/* Set this to != 0 if every cell that is accessed shall be checked:
 */
SCM_API int scm_debug_cell_accesses_p;
SCM_API int scm_expensive_debug_cell_accesses_p;
SCM_API int scm_debug_cells_gc_interval ;
SCM_API void scm_i_expensive_validation_check (SCM cell);
#endif

SCM_INTERNAL scm_i_pthread_mutex_t scm_i_gc_admin_mutex;

#define scm_gc_running_p  0
SCM_INTERNAL scm_i_pthread_mutex_t scm_i_sweep_mutex;

#ifdef __ia64__
void *scm_ia64_register_backing_store_base (void);
void *scm_ia64_ar_bsp (const void *);
#endif



#if (SCM_ENABLE_DEPRECATED == 1)
SCM_DEPRECATED size_t scm_default_init_heap_size_1;
SCM_DEPRECATED int scm_default_min_yield_1;
SCM_DEPRECATED size_t scm_default_init_heap_size_2;
SCM_DEPRECATED int scm_default_min_yield_2;
SCM_DEPRECATED size_t scm_default_max_segment_size;
#else
#define  scm_default_init_heap_size_1 deprecated
#define  scm_default_min_yield_1 deprecated
#define  scm_default_init_heap_size_2 deprecated
#define  scm_default_min_yield_2 deprecated
#define  scm_default_max_segment_size deprecated
#endif

SCM_API unsigned long scm_gc_ports_collected;

SCM_API SCM scm_after_gc_hook;

SCM_API scm_t_c_hook scm_before_gc_c_hook;
SCM_API scm_t_c_hook scm_before_mark_c_hook;
SCM_API scm_t_c_hook scm_before_sweep_c_hook;
SCM_API scm_t_c_hook scm_after_sweep_c_hook;
SCM_API scm_t_c_hook scm_after_gc_c_hook;



#if (SCM_DEBUG_CELL_ACCESSES == 1)
SCM_API void scm_assert_cell_valid (SCM);
#endif

SCM_API SCM scm_set_debug_cell_accesses_x (SCM flag);


SCM_API SCM scm_object_address (SCM obj);
SCM_API SCM scm_gc_enable (void);
SCM_API SCM scm_gc_disable (void);
SCM_API SCM scm_gc_dump (void);
SCM_API SCM scm_gc_stats (void);
SCM_API SCM scm_gc (void);
SCM_INTERNAL void scm_i_gc (const char *what);
SCM_API void scm_gc_mark (SCM p);
SCM_API void scm_gc_sweep (void);

SCM_API void scm_gc_register_allocation (size_t size);

SCM_API void *scm_malloc (size_t size) SCM_MALLOC;
SCM_API void *scm_calloc (size_t size) SCM_MALLOC;
SCM_API void *scm_realloc (void *mem, size_t size);
SCM_API char *scm_strdup (const char *str) SCM_MALLOC;
SCM_API char *scm_strndup (const char *str, size_t n) SCM_MALLOC;
SCM_API void scm_gc_register_collectable_memory (void *mem, size_t size,
						 const char *what);
SCM_API void scm_gc_unregister_collectable_memory (void *mem, size_t size,
						   const char *what);
SCM_API void *scm_gc_malloc_pointerless (size_t size, const char *what)
  SCM_MALLOC;
SCM_API void *scm_gc_calloc (size_t size, const char *what)
  SCM_MALLOC;
SCM_API void *scm_gc_malloc (size_t size, const char *what)
  SCM_MALLOC;
SCM_API void *scm_gc_realloc (void *mem, size_t old_size, 
			      size_t new_size, const char *what);
SCM_API void scm_gc_free (void *mem, size_t size, const char *what);
SCM_API char *scm_gc_strdup (const char *str, const char *what)
  SCM_MALLOC;
SCM_API char *scm_gc_strndup (const char *str, size_t n, const char *what)
  SCM_MALLOC;

#define scm_gc_typed_calloc(t) ((t *) scm_gc_calloc (sizeof (t), #t))

#ifdef BUILDING_LIBGUILE
#include "libguile/bdw-gc.h"
#define SCM_GC_MALLOC(size) GC_MALLOC (size)
#define SCM_GC_MALLOC_POINTERLESS(size) GC_MALLOC_ATOMIC (size)
#else
#define SCM_GC_MALLOC(size) scm_gc_malloc (size, NULL)
#define SCM_GC_MALLOC_POINTERLESS(size) scm_gc_malloc_pointerless (size, NULL)
#endif


SCM_INLINE SCM scm_cell (scm_t_bits car, scm_t_bits cdr);
SCM_INLINE SCM scm_double_cell (scm_t_bits car, scm_t_bits cbr,
			     scm_t_bits ccr, scm_t_bits cdr);
SCM_INLINE SCM scm_words (scm_t_bits car, scm_t_uint16 n_words);

#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES

SCM_INLINE_IMPLEMENTATION SCM
scm_cell (scm_t_bits car, scm_t_bits cdr)
{
  SCM cell = PTR2SCM (SCM_GC_MALLOC (sizeof (scm_t_cell)));

  /* Initialize the type slot last so that the cell is ignored by the GC
     until it is completely initialized.  This is only relevant when the GC
     can actually run during this code, which it can't since the GC only runs
     when all other threads are stopped.  */
  SCM_GC_SET_CELL_WORD (cell, 1, cdr);
  SCM_GC_SET_CELL_WORD (cell, 0, car);

  return cell;
}

SCM_INLINE_IMPLEMENTATION SCM
scm_double_cell (scm_t_bits car, scm_t_bits cbr,
		 scm_t_bits ccr, scm_t_bits cdr)
{
  SCM z;

  z = PTR2SCM (SCM_GC_MALLOC (2 * sizeof (scm_t_cell)));
  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't
     since the GC only runs when all other threads are stopped.
  */
  SCM_GC_SET_CELL_WORD (z, 1, cbr);
  SCM_GC_SET_CELL_WORD (z, 2, ccr);
  SCM_GC_SET_CELL_WORD (z, 3, cdr);
  SCM_GC_SET_CELL_WORD (z, 0, car);

  /* When this function is inlined, it's possible that the last
     SCM_GC_SET_CELL_WORD above will be adjacent to a following
     initialization of z.  E.g., it occurred in scm_make_real.  GCC
     from around version 3 (e.g., certainly 3.2) began taking
     advantage of strict C aliasing rules which say that it's OK to
     interchange the initialization above and the one below when the
     pointer types appear to differ sufficiently.  We don't want that,
     of course.  GCC allows this behaviour to be disabled with the
     -fno-strict-aliasing option, but would also need to be supplied
     by Guile users.  Instead, the following statements prevent the
     reordering.
   */
#ifdef __GNUC__
  __asm__ volatile ("" : : : "memory");
#else
  /* portable version, just in case any other compiler does the same
     thing.  */
  scm_remember_upto_here_1 (z);
#endif

  return z;
}

SCM_INLINE_IMPLEMENTATION SCM
scm_words (scm_t_bits car, scm_t_uint16 n_words)
{
  SCM z;

  z = PTR2SCM (SCM_GC_MALLOC (sizeof (scm_t_bits) * n_words));
  SCM_GC_SET_CELL_WORD (z, 0, car);

  /* FIXME: is the following concern even relevant with BDW-GC? */

  /* When this function is inlined, it's possible that the last
     SCM_GC_SET_CELL_WORD above will be adjacent to a following
     initialization of z.  E.g., it occurred in scm_make_real.  GCC
     from around version 3 (e.g., certainly 3.2) began taking
     advantage of strict C aliasing rules which say that it's OK to
     interchange the initialization above and the one below when the
     pointer types appear to differ sufficiently.  We don't want that,
     of course.  GCC allows this behaviour to be disabled with the
     -fno-strict-aliasing option, but would also need to be supplied
     by Guile users.  Instead, the following statements prevent the
     reordering.
   */
#ifdef __GNUC__
  __asm__ volatile ("" : : : "memory");
#else
  /* portable version, just in case any other compiler does the same
     thing.  */
  scm_remember_upto_here_1 (z);
#endif

  return z;
}

#endif /* SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES */

SCM_API void scm_remember_upto_here_1 (SCM obj);
SCM_API void scm_remember_upto_here_2 (SCM obj1, SCM obj2);
SCM_API void scm_remember_upto_here (SCM obj1, ...);

/* In GCC we can force a reference to an SCM by making it an input to an
   empty asm.  This avoids the code size and slowdown of an actual function
   call.  Unfortunately there doesn't seem to be any way to do the varargs
   scm_remember_upto_here like this.

   __volatile__ ensures nothing will be moved across the asm, and it won't
   be optimized away (or only if proved unreachable).  Constraint "g" can be
   used on all processors and allows any memory or general register (or
   immediate) operand.  The actual asm syntax doesn't matter, we don't want
   to use it, just ensure the operand is still alive.  See "Extended Asm" in
   the GCC manual for more.  */

#ifdef __GNUC__
#define scm_remember_upto_here_1(x)             \
  do {                                          \
    __asm__ __volatile__ ("" : : "g" (x));      \
  } while (0)
#define scm_remember_upto_here_2(x, y)          \
  do {                                          \
    scm_remember_upto_here_1 (x);               \
    scm_remember_upto_here_1 (y);               \
  } while (0)
#endif

SCM_API SCM scm_return_first (SCM elt, ...);
SCM_API int scm_return_first_int (int x, ...);
SCM_API SCM scm_permanent_object (SCM obj);
SCM_API SCM scm_gc_protect_object (SCM obj);
SCM_API SCM scm_gc_unprotect_object (SCM obj);
SCM_API void scm_gc_register_root (SCM *p);
SCM_API void scm_gc_unregister_root (SCM *p);
SCM_API void scm_gc_register_roots (SCM *b, unsigned long n);
SCM_API void scm_gc_unregister_roots (SCM *b, unsigned long n);
#if SCM_ENABLE_DEPRECATED == 1
SCM_DEPRECATED SCM scm_protects;
#endif
SCM_INTERNAL void scm_storage_prehistory (void);
SCM_INTERNAL void scm_init_gc_protect_object (void);
SCM_INTERNAL void scm_init_gc (void);

#if SCM_ENABLE_DEPRECATED == 1

SCM_DEPRECATED SCM scm_deprecated_newcell (void);
SCM_DEPRECATED SCM scm_deprecated_newcell2 (void);

#define SCM_NEWCELL(_into) \
  do { _into = scm_deprecated_newcell (); } while (0)
#define SCM_NEWCELL2(_into) \
  do { _into = scm_deprecated_newcell2 (); } while (0)

SCM_DEPRECATED void * scm_must_malloc (size_t len, const char *what);
SCM_DEPRECATED void * scm_must_realloc (void *where,
					size_t olen, size_t len,
					const char *what);
SCM_DEPRECATED char *scm_must_strdup (const char *str);
SCM_DEPRECATED char *scm_must_strndup (const char *str, size_t n);
SCM_DEPRECATED void scm_done_malloc (long size);
SCM_DEPRECATED void scm_done_free (long size);
SCM_DEPRECATED void scm_must_free (void *obj);

#endif

#endif  /* SCM_GC_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
