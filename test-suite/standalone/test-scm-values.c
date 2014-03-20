/* Copyright (C) 2012, 2014 Free Software Foundation, Inc.
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

#undef NDEBUG

#include <assert.h>
#include <libguile.h>
#include <stdlib.h>
#include <string.h>

static void
test_scm_c_value_ref_on_multiple_values ()
{
  SCM values = scm_values (scm_list_3 (scm_from_latin1_string ("foo"),
                                       scm_from_latin1_string ("bar"),
                                       scm_from_latin1_string ("baz")));

  char *foo = scm_to_latin1_string (scm_c_value_ref (values, 0));
  char *bar = scm_to_latin1_string (scm_c_value_ref (values, 1));
  char *baz = scm_to_latin1_string (scm_c_value_ref (values, 2));

  assert (strcmp (foo, "foo") == 0);
  assert (strcmp (bar, "bar") == 0);
  assert (strcmp (baz, "baz") == 0);

  free (foo);
  free (bar);
  free (baz);
}

static void
test_scm_c_value_ref_on_a_single_value ()
{
  SCM value = scm_from_latin1_string ("foo");
  char *foo = scm_to_latin1_string (scm_c_value_ref (value, 0));
  assert (strcmp (foo, "foo") == 0);
  free (foo);
}

static void
tests (void *data, int argc, char **argv)
{
  test_scm_c_value_ref_on_multiple_values ();
  test_scm_c_value_ref_on_a_single_value ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
