/* Copyright (C) 2011, 2013 Free Software Foundation, Inc.
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

/* Test whether threads created with `pthread_create' work, and whether
   a secondary thread can call `scm_with_guile'. (bug #32436).  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <pthread.h>
#include <stdlib.h>
#include <libguile.h>

#include <gc/gc.h>


/* Currently, calling `GC_INIT' from a secondary thread is only
   supported on some systems, notably Linux-based systems (and not on
   FreeBSD, for instance.)

   Up to GC 7.2alpha5, calling `GC_INIT' from a secondary thread would
   lead to a segfault.  This was fixed in BDW-GC on 2011-04-16 by Ivan
   Maidanski.  See <http://thread.gmane.org/gmane.lisp.guile.bugs/5340>
   for details.  */

#if defined __linux__						\
  && (GC_VERSION_MAJOR > 7					\
      || (GC_VERSION_MAJOR == 7 && GC_VERSION_MINOR > 2)	\
      || (GC_VERSION_MAJOR == 7 && GC_VERSION_MINOR == 2	\
	  && GC_ALPHA_VERSION > 5))

static void *
do_something (void *arg)
{
  scm_list_copy (scm_make_list (scm_from_int (1234), SCM_BOOL_T));
  scm_gc ();
  return NULL;
}

static void *
thread (void *arg)
{
  scm_with_guile (do_something, NULL);
  return NULL;
}


int
main (int argc, char *argv[])
{
  int i;

  for (i = 0; i < 77; i++)
    {
      pthread_t thr;

      pthread_create (&thr, NULL, thread, NULL);
      pthread_join (thr, NULL);
    }

  return EXIT_SUCCESS;
}


#else /* Linux && GC < 7.2alpha5 */

int
main (int argc, char *argv[])
{
  /* Skip.  */
  return 77;
}

#endif /* GC < 7.2 */
