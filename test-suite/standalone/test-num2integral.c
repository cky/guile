/* Copyright (C) 1999,2000,2001,2003,2004 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "libguile.h"

#include <stdio.h>
#include <assert.h>

/* if you need to change this, change numbers.c as well */
#if SCM_SIZEOF_LONG_LONG != 0
# ifndef LLONG_MAX
#  define ULLONG_MAX ((unsigned long long) (-1))
#  define LLONG_MAX ((long long) (ULLONG_MAX >> 1))
#  define LLONG_MIN (~LLONG_MAX)
# endif
#endif


SCM out_of_range_handler (void *data, SCM key, SCM args);
SCM call_num2long_long_body (void *data);
SCM call_num2ulong_long_body (void *data);

/* expect to catch an `out-of-range' exception */
SCM
out_of_range_handler (void *data, SCM key, SCM args)
{
  assert (scm_equal_p (key, scm_str2symbol ("out-of-range")));
  return SCM_BOOL_T;
}

SCM
call_num2long_long_body (void *data)
{
  scm_num2long_long (* (SCM *) data, SCM_ARG1, "call_num2long_long_body");
  return SCM_BOOL_F;
}

SCM
call_num2ulong_long_body (void *data)
{
  scm_num2ulong_long (* (SCM *) data, SCM_ARG1, "call_num2ulong_long_body");
  return SCM_BOOL_F;
}

static void
test_long_long ()
{
#if SCM_SIZEOF_LONG_LONG != 0
  {
    SCM n = scm_long_long2num (LLONG_MIN);
    long long result = scm_num2long_long(n, 0, "main");
    assert (result == LLONG_MIN);
  }

  /* LLONG_MIN - 1 */
  {
    SCM n = scm_difference (scm_long_long2num (LLONG_MIN), SCM_MAKINUM(1));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* LLONG_MIN + LLONG_MIN/2 */
  {
    SCM n = scm_sum (scm_long_long2num (LLONG_MIN),
                     scm_long_long2num (LLONG_MIN / 2));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* LLONG_MAX + 1 */
  {
    SCM n = scm_sum (scm_long_long2num (LLONG_MAX), SCM_MAKINUM(1));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* 2^1024 */
  {
    SCM n = scm_ash (SCM_MAKINUM (1), SCM_MAKINUM (1024));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* -2^1024 */
  {
    SCM n = scm_difference (SCM_MAKINUM (0),
                            scm_ash (SCM_MAKINUM (1), SCM_MAKINUM (1024)));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

#endif /* SCM_SIZEOF_LONG_LONG != 0 */
}

static void
test_ulong_long ()
{
#if SCM_SIZEOF_LONG_LONG != 0

  {
    SCM n = scm_ulong_long2num (ULLONG_MAX);
    unsigned long long result = scm_num2ulong_long(n, 0, "main");
    assert (result == ULLONG_MAX);
  }

  /* -1 */
  {
    SCM n = SCM_MAKINUM (-1);
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2ulong_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* ULLONG_MAX + 1 */
  {
    SCM n = scm_sum (scm_ulong_long2num (ULLONG_MAX), SCM_MAKINUM(1));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2ulong_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* 2^1024 */
  {
    SCM n = scm_ash (SCM_MAKINUM (1), SCM_MAKINUM (1024));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

#endif /* SCM_SIZEOF_LONG_LONG != 0 */
}

int
main (int argc, char *argv[])
{
  scm_init_guile();
  test_long_long ();
  test_ulong_long ();
  return 0;
}
