/* Copyright (C) 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

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

static SCM values_vtable;

#define SCM_VALUESP(x) (SCM_STRUCTP (x)\
                        && SCM_EQ_P (scm_struct_vtable (x), values_vtable))

static SCM
print_values (SCM obj, SCM pwps)
{
  SCM values = scm_struct_ref (obj, SCM_INUM0);
  SCM port = SCM_PORT_WITH_PS_PORT (pwps);
  scm_print_state *ps = SCM_PRINT_STATE (SCM_PORT_WITH_PS_PS (pwps));

  while (SCM_CONSP (values))
    {
      scm_iprin1 (SCM_CAR (values), port, ps);
      values = SCM_CDR (values);
      if (SCM_CONSP (values))
	scm_newline (port);
    }
  return SCM_UNSPECIFIED;
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
    {
      result = scm_make_struct (values_vtable, SCM_INUM0,
				scm_cons (args, SCM_EOL));
    }

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_call_with_values, "call-with-values", 2, 0, 0,
	    (SCM producer, SCM consumer),
	    "Calls its @var{producer} argument with no values and a\n"
	    "continuation that, when passed some values, calls the\n"
	    "@var{consumer} procedure with those values as arguments.  The\n"
	    "continuation for the call to @var{consumer} is the continuation\n"
	    "of the call to @code{call-with-values}.\n\n"
	    "@example\n"
	    "(call-with-values (lambda () (values 4 5))\n"
	    "                  (lambda (a b) b))\n"
	    "                                             ==>  5\n\n"
	    "@end example\n"
	    "@example\n"
	    "(call-with-values * -)                             ==>  -1\n"
	    "@end example")
#define FUNC_NAME s_scm_call_with_values
{
  SCM product;

  SCM_VALIDATE_PROC (1, producer);
  SCM_VALIDATE_PROC (2, consumer);

  product = scm_apply (producer, SCM_EOL, SCM_EOL);
  if (SCM_VALUESP (product))
    product = scm_struct_ref (product, SCM_INUM0);
  else
    product = scm_cons (product, SCM_EOL);
  return scm_apply (consumer, product, SCM_EOL);
}
#undef FUNC_NAME

void
scm_init_values (void)
{
  SCM print = scm_make_subr ("%print-values", scm_tc7_subr_2, print_values);

  values_vtable 
    = scm_permanent_object (scm_make_vtable_vtable (scm_makfrom0str ("pr"),
						    SCM_INUM0, SCM_EOL));
  SCM_SET_STRUCT_PRINTER (values_vtable, print);

  scm_add_feature ("values");

#ifndef SCM_MAGIC_SNARFER
#include "libguile/values.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
