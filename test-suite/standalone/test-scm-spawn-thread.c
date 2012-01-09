/* Copyright (C) 2011, 2012 Free Software Foundation, Inc.
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

/* Test whether a thread created with `scm_spawn_thread' can be joined.
   See <http://thread.gmane.org/gmane.lisp.guile.devel/11804> for the
   original report.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <time.h>
#include <stdlib.h>

static SCM
thread_main (void *data)
{
  return SCM_BOOL_T;
}

static SCM
thread_handler (void *data, SCM key, SCM args)
{
  return SCM_BOOL_T;
}

static void *
inner_main (void *data)
{
  SCM thread, timeout;

  thread = scm_spawn_thread (thread_main, 0, thread_handler, 0);
  timeout = scm_from_unsigned_integer (time (NULL) + 10);
  return SCM2PTR (scm_join_thread_timed (thread, timeout, SCM_BOOL_F));
}


int
main (int argc, char **argv)
{
  SCM result;

  result = PTR2SCM (scm_with_guile (inner_main, 0));
  return scm_is_true (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
