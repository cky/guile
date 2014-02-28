/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 *   2004, 2006, 2008, 2009, 2010, 2011, 2012,
 *   2014 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#ifdef __ia64__
#include <ucontext.h>
extern unsigned long * __libc_ia64_register_backing_store_base;
#endif

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/stime.h"
#include "libguile/stackchk.h"
#include "libguile/struct.h"
#include "libguile/smob.h"
#include "libguile/arrays.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"
#include "libguile/tags.h"

#include "libguile/validate.h"
#include "libguile/deprecation.h"
#include "libguile/gc.h"

#include "libguile/private-gc.h"

#ifdef GUILE_DEBUG_MALLOC
#include "libguile/debug-malloc.h"
#endif

#include <unistd.h>

/*
  INIT_MALLOC_LIMIT is the initial amount of malloc usage which will
  trigger a GC.
  
  After startup (at the guile> prompt), we have approximately 100k of
  alloced memory, which won't go away on GC. Let's set the init such
  that we get a nice yield on the next allocation:
*/
#define SCM_DEFAULT_INIT_MALLOC_LIMIT 200*1024
#define SCM_DEFAULT_MALLOC_MINYIELD 40

/* #define DEBUGINFO */




static void*
do_realloc (void *from, size_t new_size)
{
  scm_gc_register_allocation (new_size);
  return realloc (from, new_size);
}

static void*
do_calloc (size_t n, size_t size)
{
  scm_gc_register_allocation (size);
  return calloc (n, size);
}

static void*
do_gc_malloc (size_t size, const char *what)
{
  /* Ensure nonzero size to be compatible with always-nonzero return of
     glibc malloc.  */
  return GC_MALLOC (size ? size : sizeof (void *));
}

static void*
do_gc_malloc_atomic (size_t size, const char *what)
{
  return GC_MALLOC_ATOMIC (size ? size : sizeof (void *));
}

static void*
do_gc_realloc (void *from, size_t size, const char *what)
{
  return GC_REALLOC (from, size ? size : sizeof (void *));
}

static void
do_gc_free (void *ptr)
{
  GC_FREE (ptr);
}



/* Function for non-cell memory management.
 */

void *
scm_realloc (void *mem, size_t size)
{
  void *ptr;

  ptr = do_realloc (mem, size);

  if (ptr || size == 0)
    return ptr;

  /* Time is hard: trigger a full, ``stop-the-world'' GC, and try again.  */
#ifdef HAVE_GC_GCOLLECT_AND_UNMAP
  GC_gcollect_and_unmap ();
#else
  GC_gcollect ();
#endif

  ptr = do_realloc (mem, size);
  if (ptr)
    return ptr;

  scm_memory_error ("realloc");
}

void *
scm_malloc (size_t sz)
{
  return scm_realloc (NULL, sz);
}

/*
  Hmm. Should we use the C convention for arguments (i.e. N_ELTS,
  SIZEOF_ELT)? --hwn
 */
void *
scm_calloc (size_t sz)
{
  void * ptr;

  /*
    By default, try to use calloc, as it is likely more efficient than
    calling memset by hand.
   */
  ptr = do_calloc (sz, 1);
  if (ptr || sz == 0)
    return ptr;

  ptr = scm_realloc (NULL, sz);
  memset (ptr, 0x0, sz);
  return ptr;
}


char *
scm_strndup (const char *str, size_t n)
{
  char *dst = scm_malloc (n + 1);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_strdup (const char *str)
{
  return scm_strndup (str, strlen (str));
}



void
scm_gc_register_collectable_memory (void *mem, size_t size, const char *what)
{
  scm_gc_register_allocation (size);

#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_register (mem, what);
#endif
}


void
scm_gc_unregister_collectable_memory (void *mem, size_t size, const char *what)
{
  /* Nothing to do.  */
#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_unregister (mem);
#endif
}

/* Allocate SIZE bytes of memory whose contents should not be scanned
   for pointers (useful, e.g., for strings).  Note though that this
   memory is *not* cleared; be sure to initialize it to prevent
   information leaks.  */
void *
scm_gc_malloc_pointerless (size_t size, const char *what)
{
  return do_gc_malloc_atomic (size, what);
}

void *
scm_gc_malloc (size_t size, const char *what)
{
  return do_gc_malloc (size, what);
}

void *
scm_gc_calloc (size_t size, const char *what)
{
  /* `GC_MALLOC ()' always returns a zeroed buffer.  */
  return do_gc_malloc (size, what);
}

void *
scm_gc_realloc (void *mem, size_t old_size, size_t new_size, const char *what)
{
  return do_gc_realloc (mem, new_size, what);
}

void
scm_gc_free (void *mem, size_t size, const char *what)
{
  do_gc_free (mem);
}

char *
scm_gc_strndup (const char *str, size_t n, const char *what)
{
  char *dst = do_gc_malloc_atomic (n + 1, what);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_gc_strdup (const char *str, const char *what)
{
  return scm_gc_strndup (str, strlen (str), what);
}

#if SCM_ENABLE_DEPRECATED == 1

/* {Deprecated front end to malloc}
 *
 * scm_must_malloc, scm_must_realloc, scm_must_free, scm_done_malloc,
 * scm_done_free
 *
 * These functions provide services comparable to malloc, realloc, and
 * free.
 *
 * There has been a fair amount of confusion around the use of these functions;
 * see "Memory Blocks" in the manual. They are totally unnecessary in 2.0 given
 * the Boehm GC.
 */

void *
scm_must_malloc (size_t size, const char *what)
{
  scm_c_issue_deprecation_warning
    ("scm_must_malloc is deprecated.  "
     "Use scm_gc_malloc and scm_gc_free instead.");

  return scm_gc_malloc (size, what);
}

void *
scm_must_realloc (void *where,
		  size_t old_size,
		  size_t size,
		  const char *what)
{
  scm_c_issue_deprecation_warning
    ("scm_must_realloc is deprecated.  "
     "Use scm_gc_realloc and scm_gc_free instead.");

  return scm_gc_realloc (where, old_size, size, what);
}

char *
scm_must_strndup (const char *str, size_t length)
{
  scm_c_issue_deprecation_warning
    ("scm_must_strndup is deprecated.  "
     "Use scm_gc_strndup and scm_gc_free instead.");

  return scm_gc_strndup (str, length, "string");
}

char *
scm_must_strdup (const char *str)
{
  scm_c_issue_deprecation_warning
    ("scm_must_strdup is deprecated.  "
     "Use scm_gc_strdup and scm_gc_free instead.");

  return scm_gc_strdup (str, "string");
}

void
scm_must_free (void *obj)
#define FUNC_NAME "scm_must_free"
{
  scm_c_issue_deprecation_warning
    ("scm_must_free is deprecated.  "
     "Use scm_gc_malloc and scm_gc_free instead.");

  do_gc_free (obj);
}
#undef FUNC_NAME


void
scm_done_malloc (long size)
{
  scm_c_issue_deprecation_warning
    ("scm_done_malloc is deprecated.  "
     "Use scm_gc_register_collectable_memory instead.");

  if (size >= 0)
    scm_gc_register_collectable_memory (NULL, size, "foreign mallocs");
  else
    scm_gc_unregister_collectable_memory (NULL, -size, "foreign mallocs");
}

void
scm_done_free (long size)
{
  scm_c_issue_deprecation_warning
    ("scm_done_free is deprecated.  "
     "Use scm_gc_unregister_collectable_memory instead.");

  if (size >= 0)
    scm_gc_unregister_collectable_memory (NULL, size, "foreign mallocs");
  else
    scm_gc_register_collectable_memory (NULL, -size, "foreign mallocs");
}

#endif /* SCM_ENABLE_DEPRECATED == 1 */
