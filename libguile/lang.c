/*	Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
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
#include "libguile/macros.h"
#include "libguile/root.h"

#include "libguile/validate.h"
#include "libguile/lang.h"



/* {Multi-language support}
 */

/* Representation of pairs:
 *
 * Since we're going to share data with Scheme, we use EOL instead of nil
 * in all data structures.
 */

SCM_DEFINE (scm_nil_cons, "nil-cons", 2, 0, 0,
            (SCM x, SCM y),
	    "Create a new cons cell with @var{x} as the car and @var{y} as\n"
	    "the cdr, but convert @var{y} to Scheme's end-of-list if it is\n"
	    "a LISP nil.")
#define FUNC_NAME s_scm_nil_cons
{
  return scm_cons (x, SCM_NIL2EOL (y, y));
}
#undef FUNC_NAME


SCM_DEFINE (scm_nil_car, "nil-car", 1, 0, 0, 
            (SCM x),
	    "Return the car of @var{x}, but convert it to LISP nil if it\n"
	    "is Scheme's end-of-list.")
#define FUNC_NAME s_scm_nil_car
{
  if (SCM_NILP (x))
    return scm_lisp_nil;
  SCM_VALIDATE_CONS (1,x);
  return SCM_CAR (x);
}
#undef FUNC_NAME

SCM_DEFINE (scm_nil_cdr, "nil-cdr", 1, 0, 0, 
            (SCM x),
	    "Return the cdr of @var{x}, but convert it to LISP nil if it\n"
	    "is Scheme's end-of-list.")
#define FUNC_NAME s_scm_nil_cdr
{
  if (SCM_NILP (x))
    return scm_lisp_nil;
  SCM_VALIDATE_CONS (1,x);
  return SCM_EOL2NIL (SCM_CDR (x), x);
}
#undef FUNC_NAME

/* GJB:FIXME:: why does this return scm_lisp_nil instead of SCM_BOOL_F?
   Could use SCM_BOOL, below, otherwise */
SCM_DEFINE (scm_null, "null", 1, 0, 0, 
            (SCM x),
	    "Return LISP's @code{t} if @var{x} is nil in the LISP sense,\n"
	    "return LISP's nil otherwise.")
#define FUNC_NAME s_scm_null
{
  return (SCM_NILP (x) || SCM_NULLP (x) || SCM_FALSEP (x)) ? scm_lisp_t : scm_lisp_nil;
}
#undef FUNC_NAME

SCM
scm_m_while (SCM exp, SCM env)
{
  register SCM x = exp = SCM_CDR (exp);
  SCM z = scm_eval_car (x, env);
  while (!SCM_NILP (z) && SCM_NFALSEP (z))
    {
      while (SCM_NNULLP (x = SCM_CDR (x)))
	{
	  if (SCM_NIMP (SCM_CAR (x)))
	    (*scm_ceval_ptr) (SCM_CAR (x), env);
	}
      z = scm_eval_car (x = exp, env);
    }
  return scm_lisp_nil;
}

/* GJB:FIXME:: why does this return scm_lisp_nil instead of SCM_BOOL_F?
   Could use SCM_BOOL, below, otherwise */
SCM_DEFINE1 (scm_nil_eq, "nil-eq", scm_tc7_rpsubr, 
             (SCM x, SCM y),
	     "Compare @var{x} and @var{y} and return LISP's t if they are\n"
	     "@code{eq?}, return LISP's nil otherwise.")
#define FUNC_NAME s_scm_nil_eq
{
  return ((SCM_EQ_P (x, y)
	   || (SCM_NILP (x) && (SCM_NULLP (y) || SCM_FALSEP (y)))
	   || (SCM_NILP (y) && (SCM_NULLP (x) || SCM_FALSEP (x))))
	  ? scm_lisp_t
	  : scm_lisp_nil);
}
#undef FUNC_NAME



void
scm_init_lang ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/lang.x"
#endif
  scm_make_synt ("nil-while", scm_makacro, scm_m_while);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
