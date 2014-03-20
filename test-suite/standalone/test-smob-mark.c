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
#include <config.h>
#endif

#undef NDEBUG

#include <assert.h>
#include <libguile.h>
#include <stdio.h>
#include <stdlib.h>

#define SMOBS_COUNT (10000)

struct x_tag
{
  SCM scm_value;
  int c_value;
};

typedef struct x_tag x_t;

unsigned int mark_call_count = 0;

static scm_t_bits x_tag;
static SCM make_x (void);
static SCM mark_x (SCM x);
static int print_x (SCM x, SCM port, scm_print_state * pstate);
static size_t free_x (SCM x);
static void init_smob_type (void);
static void test_scm_smob_mark (void);

static SCM
make_x ()
{
  static int i = 0;
  SCM s_x;
  x_t *c_x;

  i++;
  c_x = (x_t *) scm_gc_malloc (sizeof (x_t), "x");
  c_x->scm_value = scm_from_int (i);
  c_x->c_value = i;
  SCM_NEWSMOB (s_x, x_tag, c_x);
  return s_x;
}

static SCM
mark_x (SCM x)
{
  x_t *c_x;
  c_x = (x_t *) SCM_SMOB_DATA (x);
  scm_gc_mark (c_x->scm_value);
  mark_call_count++;
  return SCM_BOOL_F;
}

static size_t
free_x (SCM x)
{
  x_t *c_x;
  c_x = (x_t *) SCM_SMOB_DATA (x);
  scm_gc_free (c_x, sizeof (x_t), "x");
  c_x = NULL;
  return 0;
}

static int
print_x (SCM x, SCM port, scm_print_state * pstate SCM_UNUSED)
{
  x_t *c_x = (x_t *) SCM_SMOB_DATA (x);
  scm_puts ("#<x ", port);
  if (c_x == (x_t *) NULL)
    scm_puts ("(freed)", port);
  else
    scm_write (c_x->scm_value, port);
  scm_puts (">", port);

  return 1;
}

static void
test_scm_smob_mark ()
{
  int i;
  mark_call_count = 0;
  for (i = 0; i < SMOBS_COUNT; i++)
    make_x ();
  scm_gc ();
  if (mark_call_count < SMOBS_COUNT)
    {
      fprintf (stderr, "FAIL: SMOB mark function called for each SMOB\n");
      exit (EXIT_FAILURE);
    }
}

static void
init_smob_type ()
{
  x_tag = scm_make_smob_type ("x", sizeof (x_t));
  scm_set_smob_free (x_tag, free_x);
  scm_set_smob_print (x_tag, print_x);
  scm_set_smob_mark (x_tag, mark_x);
}

static void
tests (void *data, int argc, char **argv)
{
  init_smob_type ();
  test_scm_smob_mark ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
