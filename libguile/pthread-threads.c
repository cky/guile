/* Copyright (C) 2002 Free Software Foundation, Inc.
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




#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/scmconfig.h"

/* Should go to threads-plugin */
scm_t_mutexattr scm_i_plugin_mutex;

scm_t_mutexattr scm_i_plugin_rec_mutex;

#ifndef SCM_MUTEXATTR_SETTYPE_DECLARED
int pthread_mutexattr_settype (pthread_mutexattr_t *, int);
#endif

void
scm_init_pthread_threads ()
{
  pthread_mutexattr_init (&scm_i_plugin_mutex);
#ifdef SCM_MUTEX_FAST
  pthread_mutexattr_settype (&scm_i_plugin_mutex, SCM_MUTEX_FAST);
#endif
  /* These values should be passed in a structure. */
  scm_i_plugin_mutex_size = sizeof (pthread_mutex_t);
  scm_i_plugin_mutex_init = (scm_t_mutex_init) pthread_mutex_init;
  scm_i_plugin_mutex_lock = (scm_t_mutex_lock) pthread_mutex_lock;
  scm_i_plugin_mutex_unlock = (scm_t_mutex_unlock) pthread_mutex_unlock;

#if defined (SCM_MUTEX_RECURSIVE) && !defined (SCM_DEBUG_THREADS)
  pthread_mutexattr_init (&scm_i_plugin_rec_mutex);
  pthread_mutexattr_settype (&scm_i_plugin_rec_mutex, SCM_MUTEX_RECURSIVE);
  scm_i_plugin_rec_mutex_size = sizeof (pthread_mutex_t);
  scm_i_plugin_rec_mutex_init = (scm_t_rec_mutex_init) pthread_mutex_init;
  scm_i_plugin_rec_mutex_destroy = (scm_t_rec_mutex_destroy) pthread_mutex_destroy;
  scm_i_plugin_rec_mutex_lock = (scm_t_rec_mutex_lock) pthread_mutex_lock;
  scm_i_plugin_rec_mutex_trylock = (scm_t_rec_mutex_trylock) pthread_mutex_trylock;
  scm_i_plugin_rec_mutex_unlock = (scm_t_rec_mutex_unlock) pthread_mutex_unlock;
#endif

  scm_i_plugin_cond_wait = (scm_t_cond_wait) pthread_cond_wait;
  scm_i_plugin_cond_timedwait = (scm_t_cond_timedwait) pthread_cond_timedwait;

  scm_init_threads_plugin ();
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
