/* Copyright (C) 1999,2000,2001,2003 Free Software Foundation, Inc.
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

static void
test_long_long ()
{
#if SCM_SIZEOF_LONG_LONG != 0
  {
    SCM n = scm_long_long2num (LLONG_MIN);
    long long result = scm_num2long_long(n, 0, "main");
    assert (result == LLONG_MIN);
  }
  {
    SCM n = scm_ulong_long2num (ULLONG_MAX);
    unsigned long long result = scm_num2ulong_long(n, 0, "main");
    assert (result == ULLONG_MAX);
  }
#endif /* SCM_SIZEOF_LONG_LONG != 0 */
}

int
main (int argc, char *argv[])
{
  scm_init_guile();
  test_long_long ();
  return 0;
}
