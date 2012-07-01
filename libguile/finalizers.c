/* Copyright (C) 2012 Free Software Foundation, Inc.
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

#include "libguile/bdw-gc.h"
#include "libguile/_scm.h"
#include "libguile/finalizers.h"
#include "libguile/gc.h"
#include "libguile/threads.h"



static size_t finalization_count;




void
scm_i_set_finalizer (void *obj, scm_t_finalizer_proc proc, void *data)
{
  GC_finalization_proc prev;
  void *prev_data;
  GC_REGISTER_FINALIZER_NO_ORDER (obj, proc, data, &prev, &prev_data);
}

struct scm_t_chained_finalizer
{
  int resuscitating_p;
  scm_t_finalizer_proc proc;
  void *data;
  scm_t_finalizer_proc prev;
  void *prev_data;
};

static void
chained_finalizer (void *obj, void *data)
{
  struct scm_t_chained_finalizer *chained_data = data;
  if (chained_data->resuscitating_p)
    {
      if (chained_data->prev)
        scm_i_set_finalizer (obj, chained_data->prev, chained_data->prev_data);
      chained_data->proc (obj, chained_data->data);
    }
  else
    {
      chained_data->proc (obj, chained_data->data);
      if (chained_data->prev)
        chained_data->prev (obj, chained_data->prev_data);
    }
}

void
scm_i_add_resuscitator (void *obj, scm_t_finalizer_proc proc, void *data)
{
  struct scm_t_chained_finalizer *chained_data;
  chained_data = scm_gc_malloc (sizeof (*chained_data), "chained finalizer");
  chained_data->resuscitating_p = 1;
  chained_data->proc = proc;
  chained_data->data = data;
  GC_REGISTER_FINALIZER_NO_ORDER (obj, chained_finalizer, chained_data,
                                  &chained_data->prev,
                                  &chained_data->prev_data);
}

static void
shuffle_resuscitators_to_front (struct scm_t_chained_finalizer *cd)
{
  while (cd->prev == chained_finalizer)
    {
      struct scm_t_chained_finalizer *prev = cd->prev_data;
      scm_t_finalizer_proc proc = cd->proc;
      void *data = cd->data;

      if (!prev->resuscitating_p)
        break;

      cd->resuscitating_p = 1;
      cd->proc = prev->proc;
      cd->data = prev->data;

      prev->resuscitating_p = 0;
      prev->proc = proc;
      prev->data = data;

      cd = prev;
    }
}

void
scm_i_add_finalizer (void *obj, scm_t_finalizer_proc proc, void *data)
{
  struct scm_t_chained_finalizer *chained_data;
  chained_data = scm_gc_malloc (sizeof (*chained_data), "chained finalizer");
  chained_data->resuscitating_p = 0;
  chained_data->proc = proc;
  chained_data->data = data;
  GC_REGISTER_FINALIZER_NO_ORDER (obj, chained_finalizer, chained_data,
                                  &chained_data->prev,
                                  &chained_data->prev_data);
  shuffle_resuscitators_to_front (chained_data);
}




static SCM finalizer_async_cell;

static SCM
run_finalizers_async_thunk (void)
{
  finalization_count += GC_invoke_finalizers ();
  return SCM_UNSPECIFIED;
}


/* The function queue_finalizer_async is run by the GC when there are
 * objects to finalize.  It will enqueue an asynchronous call to
 * GC_invoke_finalizers() at the next SCM_TICK in this thread.
 */
static void
queue_finalizer_async (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  static scm_i_pthread_mutex_t lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

  scm_i_pthread_mutex_lock (&lock);
  /* If t is NULL, that could be because we're allocating in
     threads.c:guilify_self_1.  In that case, rely on the
     GC_invoke_finalizers call there after the thread spins up.  */
  if (t && scm_is_false (SCM_CDR (finalizer_async_cell)))
    {
      SCM_SETCDR (finalizer_async_cell, t->active_asyncs);
      t->active_asyncs = finalizer_async_cell;
      t->pending_asyncs = 1;
    }
  scm_i_pthread_mutex_unlock (&lock);
}




#ifndef HAVE_GC_SET_FINALIZER_NOTIFIER
static void
GC_set_finalizer_notifier (void (*notifier) (void))
{
  GC_finalizer_notifier = notifier;
}
#endif

void
scm_init_finalizers (void)
{
  /* When the async is to run, the cdr of the pair gets set to the
     asyncs queue of the current thread.  */
  finalizer_async_cell =
    scm_cons (scm_c_make_gsubr ("%run-finalizers", 0, 0, 0,
                                run_finalizers_async_thunk),
              SCM_BOOL_F);
  GC_set_finalizer_notifier (queue_finalizer_async);
}
