/* Copyright (C) 2002 Free Software Foundation, Inc.
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include <stdlib.h>
#include "libguile/_scm.h"

#if SCM_USE_NULL_THREADS

#include "libguile/null-threads.h"

static scm_i_pthread_key_t *all_keys = NULL;

static void
destroy_keys (void)
{
  scm_i_pthread_key_t *key;
  int again;

  do {
    again = 0;
    for (key = all_keys; key; key = key->next)
      if (key->value && key->destr_func)
	{
	  void *v = key->value;
	  key->value = NULL;
	  key->destr_func (v);
	  again = 1;
	}
  } while (again);
}

int
scm_i_pthread_key_create (scm_i_pthread_key_t *key,
			  void (*destr_func) (void *))
{
  if (all_keys == NULL)
    atexit (destroy_keys);

  key->next = all_keys;
  all_keys = key;
  key->value = NULL;
  key->destr_func = destr_func;

  return 0;
}

#endif /* SCM_USE_NULL_THREADS */


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
