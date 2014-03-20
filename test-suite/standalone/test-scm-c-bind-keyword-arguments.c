/* Copyright (C) 2013, 2014 Free Software Foundation, Inc.
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

#include <assert.h>

static SCM
test_unrecognized_keyword (void *data)
{
  SCM k_foo = scm_from_utf8_keyword ("foo");
  SCM k_bar = scm_from_utf8_keyword ("bar");
  SCM k_baz = scm_from_utf8_keyword ("baz");
  SCM arg_foo, arg_bar;

  scm_c_bind_keyword_arguments ("test",
                                scm_list_n (k_foo, SCM_EOL,
                                            k_baz, SCM_BOOL_T,
                                            SCM_UNDEFINED),
                                SCM_ALLOW_NON_KEYWORD_ARGUMENTS,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (0);
}

static SCM
unrecognized_keyword_error_handler (void *data, SCM key, SCM args)
{
  SCM expected_args = scm_list_n
    (scm_from_utf8_string ("test"),
     scm_from_utf8_string ("Unrecognized keyword"),
     SCM_EOL, scm_list_1 (scm_from_utf8_keyword ("baz")),
     SCM_UNDEFINED);

  assert (scm_is_eq (key, scm_from_utf8_symbol ("keyword-argument-error")));
  assert (scm_is_true (scm_equal_p (args, expected_args)));

  return SCM_BOOL_T;
}

static SCM
test_invalid_keyword (void *data)
{
  SCM k_foo = scm_from_utf8_keyword ("foo");
  SCM k_bar = scm_from_utf8_keyword ("bar");
  SCM arg_foo, arg_bar;

  scm_c_bind_keyword_arguments ("test",
                                scm_list_n (k_foo, SCM_EOL,
                                            SCM_INUM0, SCM_INUM1,
                                            SCM_UNDEFINED),
                                SCM_ALLOW_OTHER_KEYS,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (0);
}

static SCM
invalid_keyword_error_handler (void *data, SCM key, SCM args)
{
  SCM expected_args = scm_list_n
    (scm_from_utf8_string ("test"),
     scm_from_utf8_string ("Invalid keyword"),
     SCM_EOL, scm_list_1 (SCM_INUM0),
     SCM_UNDEFINED);

  assert (scm_is_eq (key, scm_from_utf8_symbol ("keyword-argument-error")));
  assert (scm_is_true (scm_equal_p (args, expected_args)));

  return SCM_BOOL_T;
}

static SCM
test_odd_length (void *data)
{
  SCM k_foo = scm_from_utf8_keyword ("foo");
  SCM k_bar = scm_from_utf8_keyword ("bar");
  SCM arg_foo, arg_bar;

  scm_c_bind_keyword_arguments ("test",
                                scm_list_n (k_foo, SCM_EOL,
                                            SCM_INUM0,
                                            SCM_UNDEFINED),
                                SCM_ALLOW_OTHER_KEYS,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (0);
}

static SCM
odd_length_error_handler (void *data, SCM key, SCM args)
{
  SCM expected_args = scm_list_n
    (scm_from_utf8_string ("test"),
     scm_from_utf8_string ("Odd length of keyword argument list"),
     SCM_EOL, SCM_BOOL_F,
     SCM_UNDEFINED);

  assert (scm_is_eq (key, scm_from_utf8_symbol ("keyword-argument-error")));
  assert (scm_is_true (scm_equal_p (args, expected_args)));

  return SCM_BOOL_T;
}

static void
test_scm_c_bind_keyword_arguments ()
{
  SCM k_foo = scm_from_utf8_keyword ("foo");
  SCM k_bar = scm_from_utf8_keyword ("bar");
  SCM k_baz = scm_from_utf8_keyword ("baz");
  SCM arg_foo, arg_bar;

  /* All kwargs provided.  */
  arg_foo = SCM_INUM0;
  arg_bar = SCM_INUM1;
  scm_c_bind_keyword_arguments ("test",
                                scm_list_n (k_bar, SCM_EOL,
                                            k_foo, SCM_BOOL_T,
                                            SCM_UNDEFINED),
                                0,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (scm_is_eq (arg_foo, SCM_BOOL_T));
  assert (scm_is_eq (arg_bar, SCM_EOL));

  /* Some kwargs provided.  */
  arg_foo = SCM_INUM0;
  arg_bar = SCM_INUM1;
  scm_c_bind_keyword_arguments ("test",
                                scm_list_n (k_bar, SCM_EOL,
                                            SCM_UNDEFINED),
                                0,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (scm_is_eq (arg_foo, SCM_INUM0));
  assert (scm_is_eq (arg_bar, SCM_EOL));

  /* No kwargs provided.  */
  arg_foo = SCM_INUM0;
  arg_bar = SCM_INUM1;
  scm_c_bind_keyword_arguments ("test",
                                SCM_EOL,
                                0,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (scm_is_eq (arg_foo, SCM_INUM0));
  assert (scm_is_eq (arg_bar, SCM_INUM1));

  /* Other kwargs provided, when allowed.  */
  arg_foo = SCM_INUM0;
  arg_bar = SCM_INUM1;
  scm_c_bind_keyword_arguments ("test",
                                scm_list_n (k_foo, SCM_EOL,
                                            k_baz, SCM_BOOL_T,
                                            SCM_UNDEFINED),
                                SCM_ALLOW_OTHER_KEYS,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (scm_is_eq (arg_foo, SCM_EOL));
  assert (scm_is_eq (arg_bar, SCM_INUM1));

  /* Other non-kwargs provided, when allowed.  */
  arg_foo = SCM_INUM0;
  arg_bar = SCM_INUM1;
  scm_c_bind_keyword_arguments ("test",
                                scm_list_n (SCM_BOOL_F,
                                            k_foo, SCM_EOL,
                                            SCM_INUM0,
                                            k_bar, SCM_BOOL_T,
                                            SCM_INUM1,
                                            SCM_UNDEFINED),
                                SCM_ALLOW_NON_KEYWORD_ARGUMENTS,
                                k_foo, &arg_foo,
                                k_bar, &arg_bar,
                                SCM_UNDEFINED);
  assert (scm_is_eq (arg_foo, SCM_EOL));
  assert (scm_is_eq (arg_bar, SCM_BOOL_T));

  /* Test unrecognized keyword error.  */
  scm_internal_catch (SCM_BOOL_T,
                      test_unrecognized_keyword, NULL,
                      unrecognized_keyword_error_handler, NULL);

  /* Test invalid keyword error.  */
  scm_internal_catch (SCM_BOOL_T,
                      test_invalid_keyword, NULL,
                      invalid_keyword_error_handler, NULL);

  /* Test odd length error.  */
  scm_internal_catch (SCM_BOOL_T,
                      test_odd_length, NULL,
                      odd_length_error_handler, NULL);
}

static void
tests (void *data, int argc, char **argv)
{
  test_scm_c_bind_keyword_arguments ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
