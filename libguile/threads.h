/* classes: h_files */

#ifndef THREADSH
#define THREADSH

/*	Copyright (C) 1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include "libguile/__scm.h"
#include "libguile/procs.h"

/* smob tags for the thread datatypes */
extern long scm_tc16_thread;
extern long scm_tc16_mutex;
extern long scm_tc16_condvar;

#define SCM_THREADP(obj) (scm_tc16_thread == SCM_TYP16 (obj))
#define SCM_THREAD_DATA(obj) ((void *) SCM_CDR (obj))

#define SCM_MUTEXP(obj) (scm_tc16_mutex == SCM_TYP16 (obj))
#define SCM_MUTEX_DATA(obj) ((void *) SCM_CDR (obj))

#define SCM_CONDVARP(obj) (scm_tc16_condvar == SCM_TYP16 (obj))
#define SCM_CONDVAR_DATA(obj) ((void *) SCM_CDR (obj))

/* Initialize implementation specific details of the threads support */
void scm_threads_init SCM_P ((SCM_STACKITEM *));
void scm_threads_mark_stacks SCM_P ((void));
void scm_init_threads SCM_P ((SCM_STACKITEM *));

/* */
SCM scm_threads_make_mutex SCM_P ((void));
SCM scm_threads_lock_mutex SCM_P ((SCM));
SCM scm_threads_unlock_mutex SCM_P ((SCM));
SCM scm_threads_monitor SCM_P ((void));

#if 0
/* These don't work any more.  */ 
#ifdef USE_MIT_PTHREADS
#include "mit-pthreads.h"
#endif

#ifdef USE_FSU_PTHREADS
#include "fsu-pthreads.h"
#endif
#endif

#ifdef USE_COOP_THREADS
#include "libguile/coop-defs.h"
#endif

#endif  /* THREADSH */
