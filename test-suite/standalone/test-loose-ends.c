/* test-loose-ends.c
 *
 * Test items of the Guile C API that aren't covered by any other tests.
 */

/* Copyright (C) 2009, 2012, 2014 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
# include <config.h>
#endif

#undef NDEBUG

#include <libguile.h>

#include <stdio.h>
#include <assert.h>
#include <string.h>

#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif

static void
test_scm_from_locale_keywordn ()
{
  SCM kw = scm_from_locale_keywordn ("thusly", 4);
  assert (scm_is_true (scm_keyword_p (kw)));
}

static void
test_scm_local_eval ()
{
  SCM result;

  scm_c_use_module ("ice-9 local-eval");
  result = scm_local_eval
    (scm_list_3 (scm_from_latin1_symbol ("+"),
                 scm_from_latin1_symbol ("x"),
                 scm_from_latin1_symbol ("y")),
     scm_c_eval_string ("(let ((x 1) (y 2)) (the-environment))"));
     
  assert (scm_is_true (scm_equal_p (result,
                                    scm_from_signed_integer (3))));
}

static void
test_scm_call ()
{
  SCM result;

  result = scm_call (scm_c_public_ref ("guile", "+"),
                     scm_from_int (1),
                     scm_from_int (2),
                     SCM_UNDEFINED);
  assert (scm_is_true (scm_equal_p (result, scm_from_int (3))));

  result = scm_call (scm_c_public_ref ("guile", "list"),
                     SCM_UNDEFINED);
  assert (scm_is_eq (result, SCM_EOL));
}

static void
test_scm_to_pointer ()
{
  int (*add3) (int a, int b, int c);
  SCM int_type = scm_c_public_ref ("system foreign", "int");

  add3 = scm_to_pointer
    (scm_procedure_to_pointer (int_type,
                               scm_c_public_ref ("guile", "+"),
                               scm_list_3 (int_type,
                                           int_type,
                                           int_type)));

  assert ((*add3) (1000000, 1000, -1) == 1000999);
}

static void
tests (void *data, int argc, char **argv)
{
  test_scm_from_locale_keywordn ();
  test_scm_local_eval ();
  test_scm_call ();
  test_scm_to_pointer ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
