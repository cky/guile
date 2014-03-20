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

/* Make sure the assertions are tested.  */
#undef NDEBUG

#include <libguile.h>

#include <stdio.h>
#include <assert.h>

static void
test_writable_elements ()
{
  SCM elts = scm_list_4 (scm_from_int (1), scm_from_int (2),
                         scm_from_int (3), scm_from_int (4));

  {
    SCM v = scm_u32vector (elts);
    size_t len;
    ssize_t inc;
    scm_t_array_handle h;
    scm_t_uint32 *elts = scm_u32vector_writable_elements (v, &h, &len, &inc);
    assert (len == 4);
    assert (inc == 1);
    assert (elts[0] == 1);
    assert (elts[3] == 4);
    scm_array_handle_release (&h);
  }

  {
    SCM v = scm_f32vector (elts);
    size_t len;
    ssize_t inc;
    scm_t_array_handle h;
    float *elts = scm_f32vector_writable_elements (v, &h, &len, &inc);
    assert (len == 4);
    assert (inc == 1);
    assert (elts[0] == 1.0);
    assert (elts[3] == 4.0);
    scm_array_handle_release (&h);
  }

  {
    SCM v = scm_c32vector (elts);
    size_t len;
    ssize_t inc;
    scm_t_array_handle h;
    float *elts = scm_c32vector_writable_elements (v, &h, &len, &inc);
    assert (len == 4);
    assert (inc == 1);
    assert (elts[0] == 1.0);
    assert (elts[1] == 0.0);
    assert (elts[6] == 4.0);
    assert (elts[7] == 0.0);
    scm_array_handle_release (&h);
  }
}

static void
tests (void *data, int argc, char **argv)
{
  test_writable_elements ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
