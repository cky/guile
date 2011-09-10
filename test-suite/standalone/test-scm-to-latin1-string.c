/* Copyright (C) 2011 Free Software Foundation, Inc.
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

/*
  This outputs:

  dhansen@localhorst ~/tmp $ ./a.out
  foo,bar
  bar

*/

#define TEST(x) \
  if (!(x)) abort()

static void
inner_main (void *data, int argc, char **argv)
{
  char *cstr;

  SCM string, tokens, tok;

  string = scm_from_latin1_string ("foo,bar");
  tokens = scm_string_split (string, SCM_MAKE_CHAR (','));

  TEST (scm_is_pair (tokens));
  tok = scm_car (tokens);
  TEST (scm_is_string (tok));
  cstr = scm_to_latin1_string (tok);
  TEST (strcmp (cstr, "foo") == 0);
  free (cstr);
  tokens = scm_cdr (tokens);
  
  TEST (scm_is_pair (tokens));
  tok = scm_car (tokens);
  TEST (scm_is_string (tok));
  cstr = scm_to_latin1_string (tok);
  TEST (strcmp (cstr, "bar") == 0);
  free (cstr);
  tokens = scm_cdr (tokens);
  
  TEST (scm_is_null (tokens));
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, NULL);

  return EXIT_SUCCESS;
}

/* Local Variables: */
/* compile-command: "gcc `pkg-config --cflags --libs guile-2.0` main.c" */
/* End: */
