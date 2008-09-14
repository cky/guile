#ifndef SCM_BOEHM_GC_H
#define SCM_BOEHM_GC_H

/* Copyright (C) 2006, 2008 Free Software Foundation, Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/* Correct header inclusion.  */

#include "libguile/gen-scmconfig.h"

#ifdef SCM_I_GSC_USE_PTHREAD_THREADS

/* When pthreads are used, let `libgc' know about it and redirect allocation
   calls such as `GC_MALLOC ()' to (contention-free, faster) thread-local
   allocation.  */

# define GC_THREADS 1
# define GC_REDIRECT_TO_LOCAL 1

#endif

#include <gc/gc.h>

#if (! ((defined GC_VERSION_MAJOR) && (GC_VERSION_MAJOR >= 7)))
/* This was needed with `libgc' 6.x.  */
# include <gc/gc_local_alloc.h>
#endif

#if (defined GC_VERSION_MAJOR) && (GC_VERSION_MAJOR >= 7)
/* This type was provided by `libgc' 6.x.  */
typedef void *GC_PTR;
#endif

#endif /* SCM_BOEHM_GC_H */
