/* test-foreign-object-c.c - exercise C foreign object interface */

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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

enum
  {
    CSTR_SLOT_ADDR,
    CSTR_SLOT_LEN,
    CSTR_SLOT_COUNT
  };

static void
finalizer (SCM obj)
{
  free (scm_foreign_object_ref (obj, CSTR_SLOT_ADDR));
}

static SCM
make_cstr_from_static (SCM type, const char *str)
{
  char *ours = strdup (str);

  if (!ours)
    abort ();

  return scm_make_foreign_object_2 (type, ours, (void *) strlen (ours));
}

static int
cstr_equals_static_p (SCM cstr, const char *str)
{
  const char *addr;
  size_t len;

  addr = scm_foreign_object_ref (cstr, CSTR_SLOT_ADDR);
  len = scm_foreign_object_unsigned_ref (cstr, CSTR_SLOT_LEN);

  if (strlen (str) != len)
    return 0;

  return strncmp (addr, str, len) == 0;
}

static void
test_scm_foreign_object (void)
{
  SCM type_name, slot_names, type, cstr;

  type_name = scm_from_utf8_symbol ("<cstr>");
  slot_names = scm_list_2 (scm_from_utf8_symbol ("addr"),
                           scm_from_utf8_symbol ("len"));
  type = scm_make_foreign_object_type (type_name, slot_names, finalizer);

  cstr = make_cstr_from_static (type, "Hello, world!");
  scm_assert_foreign_object_type (type, cstr);

  if (!cstr_equals_static_p (cstr, "Hello, world!"))
    {
      fprintf (stderr, "fail: test-foreign-object 1\n");
      exit (EXIT_FAILURE);
    }

  {
    int i;
    for (i = 0; i < 5000; i++)
      cstr = make_cstr_from_static (type, "Hello, world!");
    cstr = SCM_BOOL_F;
  }

  scm_gc ();
  scm_gc ();
  scm_gc ();

  /* Allow time for the finalizer thread to run.  */
  scm_usleep (scm_from_uint (50 * 1000));
}

static void
tests (void *data, int argc, char **argv)
{
  test_scm_foreign_object ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
