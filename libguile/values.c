/* Copyright (C) 2000, 2001, 2006, 2008, 2009, 2011, 2012 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/gc.h"
#include "libguile/numbers.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/struct.h"
#include "libguile/validate.h"

#include "libguile/values.h"

SCM scm_values_vtable;

/* OBJ must be a values object containing exactly two values.
   scm_i_extract_values_2 puts those two values into *p1 and *p2.  */
void
scm_i_extract_values_2 (SCM obj, SCM *p1, SCM *p2)
{
  SCM values;

  SCM_ASSERT_TYPE (SCM_VALUESP (obj), obj, SCM_ARG1,
		   "scm_i_extract_values_2", "values");
  values = scm_struct_ref (obj, SCM_INUM0);
  if (scm_ilength (values) != 2)
    scm_wrong_type_arg_msg
      ("scm_i_extract_values_2", SCM_ARG1, obj,
       "a values object containing exactly two values");
  *p1 = SCM_CAR (values);
  *p2 = SCM_CADR (values);
}

static SCM
print_values (SCM obj, SCM pwps)
{
  SCM values = scm_struct_ref (obj, SCM_INUM0);
  SCM port = SCM_PORT_WITH_PS_PORT (pwps);
  scm_print_state *ps = SCM_PRINT_STATE (SCM_PORT_WITH_PS_PS (pwps));

  scm_puts ("#<values ", port);
  scm_iprin1 (values, port, ps);
  scm_puts (">", port);

  return SCM_UNSPECIFIED;
}

size_t
scm_c_nvalues (SCM obj)
{
  if (SCM_LIKELY (SCM_VALUESP (obj)))
    return scm_ilength (scm_struct_ref (obj, SCM_INUM0));
  else
    return 1;
}

SCM
scm_c_value_ref (SCM obj, size_t idx)
{
  if (SCM_LIKELY (SCM_VALUESP (obj)))
    {
      SCM values = scm_struct_ref (obj, SCM_INUM0);
      size_t i = idx;
      while (SCM_LIKELY (scm_is_pair (values)))
        {
          if (i == 0)
            return SCM_CAR (values);
          values = SCM_CDR (values);
          i--;
        }
    }
  else if (idx == 0)
    return obj;

  scm_error (scm_out_of_range_key,
	     "scm_c_value_ref",
	     "Too few values in ~S to access index ~S",
             scm_list_2 (obj, scm_from_size_t (idx)),
             scm_list_1 (scm_from_size_t (idx)));
}

SCM_DEFINE (scm_values, "values", 0, 0, 1,
	    (SCM args),
	    "Delivers all of its arguments to its continuation.  Except for\n"
	    "continuations created by the @code{call-with-values} procedure,\n"
	    "all continuations take exactly one value.  The effect of\n"
	    "passing no value or more than one value to continuations that\n"
	    "were not created by @code{call-with-values} is unspecified.")
#define FUNC_NAME s_scm_values
{
  long n;
  SCM result;

  SCM_VALIDATE_LIST_COPYLEN (1, args, n);
  if (n == 1)
    result = SCM_CAR (args);
  else
    result = scm_c_make_struct (scm_values_vtable, 0, 1, SCM_UNPACK (args));

  return result;
}
#undef FUNC_NAME

SCM
scm_c_values (SCM *base, size_t nvalues)
{
  SCM ret, *walk;

  if (nvalues == 1)
    return *base;

  for (ret = SCM_EOL, walk = base + nvalues - 1; walk >= base; walk--)
    ret = scm_cons (*walk, ret);

  return scm_values (ret);
}

void
scm_init_values (void)
{
  SCM print = scm_c_define_gsubr ("%print-values", 2, 0, 0, print_values);

  scm_values_vtable = scm_make_vtable (scm_from_locale_string ("pr"), print);

  scm_add_feature ("values");

#include "libguile/values.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
