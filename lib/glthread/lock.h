#ifndef SCM_GLTHREADS_H
#define SCM_GLTHREADS_H

/* Copyright (C) 2014 Free Software Foundation, Inc.
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

/* This file implements Gnulib's glthreads/lock.h interface in terms of
   Guile's locking API.  This allows Gnulib modules such as 'regex' to
   be built with thread-safety support via Guile's locks (see
   <http://bugs.gnu.org/14404>.)  */

#include <libguile/threads.h>
#include <stdlib.h>

#define gl_lock_define(klass, name)		\
  klass scm_i_pthread_mutex_t name;

#define glthread_lock_init(lock) scm_i_pthread_mutex_init ((lock), NULL)
#define glthread_lock_destroy    scm_i_pthread_mutex_destroy
#define glthread_lock_lock       scm_i_pthread_mutex_lock
#define glthread_lock_unlock     scm_i_pthread_mutex_unlock

#endif
