/* classes: h_files */

#ifndef SCM_COOP_THREADS_H
#define SCM_COOP_THREADS_H

/* Copyright (C) 1996,1997,1998,2000, 2002, 2006 Free Software Foundation, Inc.
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



/* This file is only included by coop-threads.c while coop-defs.h is
   included by threads.h, which, in turn, is included by
   libguile.h. */

/* The coop_t struct is declared in coop-defs.h. */

#include "libguile/__scm.h"

#include <time.h>

#include "libguile/coop-defs.h"
#include "qt/qt.h"

/* This code is based on a sample thread libraru by David Keppel.
   Portions of this file fall under the following copyright: */

/*
 * QuickThreads -- Threads-building toolkit.
 * Copyright (c) 1993 by David Keppel
 *
 * Permission to use, copy, modify and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice and this notice
 * appear in all copies.  This software is provided as a
 * proof-of-concept and for demonstration purposes; there is no
 * representation about the suitability of this software for any
 * purpose.
 */

/* Each thread starts by calling a user-supplied function of this
   type. */

typedef void (coop_userf_t)(void *p0);

/* Call this before any other primitives. */
SCM_API void coop_init (void);

/* When one or more threads are created by the main thread,
   the system goes multithread when this is called.  It is done
   (no more runable threads) when this returns. */

SCM_API void coop_start (void);

/* Create a thread and make it runable.  When the thread starts
   running it will call `f' with arguments `p0' and `p1'. */

SCM_API coop_t *coop_create (coop_userf_t *f, void *p0);

/* The current thread stops running but stays runable.
   It is an error to call `coop_yield' before `coop_start'
   is called or after `coop_start' returns. */

SCM_API void coop_yield (void);

/* Like `coop_yield' but the thread is discarded.  Any intermediate
   state is lost.  The thread can also terminate by simply
   returning. */

SCM_API void coop_abort (void);

/* The following are needed in iselect.c */

SCM_API coop_t *coop_qget (coop_q_t *);
SCM_API void coop_qput (coop_q_t *, coop_t *);
SCM_API void *coop_sleephelp (qt_t *, void *, void *);

SCM_API coop_t *coop_wait_for_runnable_thread ();

SCM_API coop_q_t coop_global_runq;	/* A queue of runable threads. */
SCM_API coop_q_t coop_global_sleepq;
SCM_API coop_q_t coop_tmp_queue;
SCM_API coop_q_t coop_global_allq;	/* A queue of all threads. */
SCM_API coop_t *coop_global_curr;      	/* Currently-executing thread. */

#endif  /* SCM_COOP_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
