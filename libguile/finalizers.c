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



void
scm_i_set_finalizer (void *obj, scm_t_finalizer_proc proc, void *data)
{
  GC_finalization_proc prev;
  GC_PTR prev_data;
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




void
scm_init_finalizers (void)
{
}
