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




int scm_i_plugin_mutex_size = 0;
int (*scm_i_plugin_mutex_init) (scm_t_mutex *, const scm_t_mutexattr *);
int (*scm_i_plugin_mutex_lock) (scm_t_mutex *);
int (*scm_i_plugin_mutex_unlock) (scm_t_mutex *);

int scm_i_plugin_rec_mutex_size = 0;
int (*scm_i_plugin_rec_mutex_init) (scm_t_rec_mutex *,
				    const scm_t_mutexattr *);
int (*scm_i_plugin_rec_mutex_destroy) (scm_t_rec_mutex *);
int (*scm_i_plugin_rec_mutex_lock) (scm_t_rec_mutex *);
int (*scm_i_plugin_rec_mutex_trylock) (scm_t_rec_mutex *);
int (*scm_i_plugin_rec_mutex_unlock) (scm_t_rec_mutex *);

int (*scm_i_plugin_cond_wait) (scm_t_cond *, scm_t_mutex *);
int (*scm_i_plugin_cond_timedwait) (scm_t_cond *,
				    scm_t_mutex *,
				    const struct timespec *);

typedef struct rec_mutex {
#ifdef SCM_DEBUG_THREADS
  int kind;
#endif
  scm_t_mutex mutex;
  scm_thread *owner;
  int count;
} rec_mutex;

/* Mutex for recursive mutex administration */
static scm_t_mutex rec_mutex_mutex;

static int
rec_mutex_init (scm_t_rec_mutex *mx, const scm_t_mutexattr *a)
{
  rec_mutex *m = (rec_mutex *) mx;
  scm_i_plugin_mutex_init (&m->mutex, &scm_i_plugin_mutex);
  m->owner = 0;
  m->count = 0;
#ifdef SCM_DEBUG_THREADS
  m->kind = REC_MUTEX;
#endif
  return 0;
}

static int
rec_mutex_destroy (scm_t_rec_mutex *mx)
{
  rec_mutex *m = (rec_mutex *) mx;
  return scm_i_plugin_mutex_destroy (&m->mutex);
}

static int
rec_mutex_lock (scm_t_rec_mutex *mx)
{
  rec_mutex *m = (rec_mutex *) mx;
  scm_thread *t = SCM_CURRENT_THREAD;
  scm_i_plugin_mutex_lock (&rec_mutex_mutex);
#ifdef SCM_DEBUG_THREADS
  if (m->kind != REC_MUTEX)
    {
      fprintf (stderr,
	       m->kind == FAST_MUTEX
	       ? "locking wrong mutex type\n"
	       : "locking uninitialized mutex\n");
      abort ();
    }
#endif
  if (m->count && m->owner == t)
    {
      ++m->count;
      scm_i_plugin_mutex_unlock (&rec_mutex_mutex);
    }
  else
    {
      scm_i_plugin_mutex_unlock (&rec_mutex_mutex);
      scm_i_plugin_mutex_lock (&m->mutex);
      m->count = 1;
      m->owner = t;
    }
  return 0;
}

static int
rec_mutex_trylock (scm_t_rec_mutex *mx)
{
  rec_mutex *m = (rec_mutex *) mx;
  scm_thread *t = SCM_CURRENT_THREAD;
  scm_i_plugin_mutex_lock (&rec_mutex_mutex);
  if (m->owner != 0 && m->owner != t)
    return EBUSY;
  else if (m->count)
    {
      ++m->count;
      scm_i_plugin_mutex_unlock (&rec_mutex_mutex);
    }
  else
    {
      scm_i_plugin_mutex_unlock (&rec_mutex_mutex);
      scm_i_plugin_mutex_lock (&m->mutex);
      m->count = 1;
      m->owner = t;
    }
  return 0;
}

static int
rec_mutex_unlock (scm_t_rec_mutex *mx)
{
  rec_mutex *m = (rec_mutex *) mx;
  scm_i_plugin_mutex_lock (&rec_mutex_mutex);
#ifdef SCM_DEBUG_THREADS
  if (m->kind != REC_MUTEX)
    {
      fprintf (stderr,
	       m->kind == FAST_MUTEX
	       ? "locking wrong mutex type\n"
	       : "locking uninitialized mutex\n");
      abort ();
    }
  if (m->count == 0)
    {
      fprintf (stderr, "unlocking unlocked mutex\n");
      abort ();
    }
#endif
  if (!--m->count)
    {
      m->owner = 0;
      scm_i_plugin_mutex_unlock (&m->mutex);
    }
  scm_i_plugin_mutex_unlock (&rec_mutex_mutex);
  return 0;
}

#ifndef SCM_MUTEXATTR_SETTYPE_DECLARED
int pthread_mutexattr_settype (pthread_mutexattr_t *, int);
#endif

void
scm_init_threads_plugin ()
{
  if (scm_i_plugin_rec_mutex_size == 0)
    {
      scm_i_plugin_rec_mutex_size = sizeof (rec_mutex);
      scm_i_plugin_mutex_init (&rec_mutex_mutex, &scm_i_plugin_mutex);
      scm_i_plugin_rec_mutex_init = rec_mutex_init;
      scm_i_plugin_rec_mutex_destroy = rec_mutex_destroy;
      scm_i_plugin_rec_mutex_lock = rec_mutex_lock;
      scm_i_plugin_rec_mutex_trylock = rec_mutex_trylock;
      scm_i_plugin_rec_mutex_unlock = rec_mutex_unlock;
    }
  if (scm_i_plugin_mutex_size > SCM_MUTEX_MAXSIZE
      || scm_i_plugin_rec_mutex_size > SCM_REC_MUTEX_MAXSIZE)
    {
      fprintf (stderr, "Internal error: Need to upgrade mutex size\n");
      abort ();
    }
}
