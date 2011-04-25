/* Copyright (C) 1999,2000,2001,2003, 2006, 2008 Free Software Foundation, Inc.
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

SCM init2_count;

void libtest_extensions_init2 (void);
void libtest_extensions_init (void);

void
libtest_extensions_init2 (void)
{
  scm_variable_set_x (init2_count,
                      scm_from_int (scm_to_int (scm_variable_ref (init2_count)) + 1));
}

void
libtest_extensions_init (void)
{
  scm_c_define ("init2-count", scm_from_int (0));
  init2_count = scm_permanent_object (scm_c_lookup ("init2-count"));
  scm_c_register_extension ("libtest-extensions", "libtest_extensions_init2",
                            (scm_t_extension_init_func)libtest_extensions_init2, NULL);
}
