/* Copyright (C) 2016 Free Software Foundation, Inc.
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

#include <libguile.h>
#include <assert.h>

static SCM
mark_smob (SCM smob)
{
  assert (SCM_SMOB_DATA (smob) == 1);
  return SCM_BOOL_F;
}

static size_t
finalize_smob (SCM smob)
{
  assert (SCM_SMOB_DATA (smob) == 1);
  SCM_SET_SMOB_DATA (smob, 0);
  /* Allocate a bit in the hopes of triggering a new GC, making the
     marker race with the finalizer.  */
  scm_cons (SCM_BOOL_F, SCM_BOOL_F);
  return 0;
}

static void
tests (void *data, int argc, char **argv)
{
  scm_t_bits tc16;
  int i;

  tc16 = scm_make_smob_type ("smob with finalizer", 0);
  scm_set_smob_mark (tc16, mark_smob);
  scm_set_smob_free (tc16, finalize_smob);

  for (i = 0; i < 1000 * 1000; i++)
    scm_new_smob (tc16, 1);
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
