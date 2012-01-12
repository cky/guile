/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011
 * Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include <alloca.h>

#include "libguile/__scm.h"

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/async.h"
#include "libguile/continuations.h"
#include "libguile/debug.h"
#include "libguile/deprecation.h"
#include "libguile/dynwind.h"
#include "libguile/eq.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/fluids.h"
#include "libguile/goops.h"
#include "libguile/hash.h"
#include "libguile/hashtab.h"
#include "libguile/list.h"
#include "libguile/macros.h"
#include "libguile/memoize.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/procprop.h"
#include "libguile/programs.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/srcprop.h"
#include "libguile/stackchk.h"
#include "libguile/strings.h"
#include "libguile/threads.h"
#include "libguile/throw.h"
#include "libguile/validate.h"
#include "libguile/values.h"
#include "libguile/promises.h"





scm_t_bits scm_tc16_promise;

SCM_DEFINE (scm_make_promise, "make-promise", 1, 0, 0, 
	    (SCM thunk),
	    "Create a new promise object.\n\n"
            "@code{make-promise} is a procedural form of @code{delay}.\n"
            "These two expressions are equivalent:\n"
            "@lisp\n"
	    "(delay @var{exp})\n"
	    "(make-promise (lambda () @var{exp}))\n"
            "@end lisp\n")
#define FUNC_NAME s_scm_make_promise
{
  SCM_VALIDATE_THUNK (1, thunk);
  SCM_RETURN_NEWSMOB2 (scm_tc16_promise,
		       SCM_UNPACK (thunk),
		       SCM_UNPACK (scm_make_recursive_mutex ()));
}
#undef FUNC_NAME

static int 
promise_print (SCM exp, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<promise ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (SCM_PROMISE_DATA (exp), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_force, "force", 1, 0, 0, 
	    (SCM promise),
	    "If @var{promise} has not been computed yet, compute and\n"
	    "return @var{promise}, otherwise just return the previously computed\n"
	    "value.")
#define FUNC_NAME s_scm_force
{
  SCM_VALIDATE_SMOB (1, promise, promise);
  scm_lock_mutex (SCM_PROMISE_MUTEX (promise));
  if (!SCM_PROMISE_COMPUTED_P (promise))
    {
      SCM ans = scm_call_0 (SCM_PROMISE_DATA (promise));
      if (!SCM_PROMISE_COMPUTED_P (promise))
	{
	  SCM_SET_PROMISE_DATA (promise, ans);
	  SCM_SET_PROMISE_COMPUTED (promise);
	}
    }
  scm_unlock_mutex (SCM_PROMISE_MUTEX (promise));
  return SCM_PROMISE_DATA (promise);
}
#undef FUNC_NAME


SCM_DEFINE (scm_promise_p, "promise?", 1, 0, 0, 
            (SCM obj),
	    "Return true if @var{obj} is a promise, i.e. a delayed computation\n"
	    "(@pxref{Delayed evaluation,,,r5rs.info,The Revised^5 Report on Scheme}).")
#define FUNC_NAME s_scm_promise_p
{
  return scm_from_bool (SCM_TYP16_PREDICATE (scm_tc16_promise, obj));
}
#undef FUNC_NAME

void 
scm_init_promises ()
{
  scm_tc16_promise = scm_make_smob_type ("promise", 0);
  scm_set_smob_print (scm_tc16_promise, promise_print);

#include "libguile/promises.x"

  scm_add_feature ("delay");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

