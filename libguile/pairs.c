/* Copyright (C) 1995,1996,2000,2001 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include "libguile/_scm.h"
#include "libguile/validate.h"

#include "libguile/pairs.h"



/* {Pairs}
 */

#if (SCM_DEBUG_PAIR_ACCESSES == 1)

#include "libguile/ports.h"
#include "libguile/strings.h"

void scm_error_pair_access (SCM non_pair)
{
  static unsigned int running = 0;
  SCM message = scm_makfrom0str ("Non-pair accessed with SCM_C[AD]R: `~S´\n");

  if (!running)
    {
      running = 1;
      scm_simple_format (scm_current_error_port (),
			 message, SCM_LIST1 (non_pair));
      abort ();
    }
}

#endif

SCM_DEFINE (scm_cons, "cons", 2, 0, 0,
	    (SCM x, SCM y),
	    "Return a newly allocated pair whose car is @var{x} and whose\n"
	    "cdr is @var{y}.  The pair is guaranteed to be different (in the\n"
	    "sense of @code{eq?}) from every previously existing object.")
#define FUNC_NAME s_scm_cons
{
  SCM z;
  SCM_NEWCELL (z);
  SCM_SET_CELL_OBJECT_0 (z, x);
  SCM_SET_CELL_OBJECT_1 (z, y);
  return z;
}
#undef FUNC_NAME


SCM 
scm_cons2 (SCM w, SCM x, SCM y)
{
  SCM z1;
  SCM z2;

  SCM_NEWCELL (z1);
  SCM_SET_CELL_OBJECT_0 (z1, x);
  SCM_SET_CELL_OBJECT_1 (z1, y);

  SCM_NEWCELL (z2);
  SCM_SET_CELL_OBJECT_0 (z2, w);
  SCM_SET_CELL_OBJECT_1 (z2, z1);

  return z2;
}


SCM_DEFINE (scm_pair_p, "pair?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a pair; otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_pair_p
{
  return SCM_BOOL (SCM_CONSP (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_car_x, "set-car!", 2, 0, 0,
            (SCM pair, SCM value),
            "Stores @var{value} in the car field of @var{pair}.  The value returned\n"
            "by @code{set-car!} is unspecified.")
#define FUNC_NAME s_scm_set_car_x
{
  SCM_VALIDATE_CONS (1, pair);
  SCM_SETCAR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_cdr_x, "set-cdr!", 2, 0, 0,
            (SCM pair, SCM value),
            "Stores @var{value} in the cdr field of @var{pair}.  The value returned\n"
            "by @code{set-cdr!} is unspecified.")
#define FUNC_NAME s_scm_set_cdr_x
{
  SCM_VALIDATE_CONS (1, pair);
  SCM_SETCDR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



static const char * cxrs[] = 
{
  "car",
  "cdr",
  "caar",
  "cadr",
  "cdar",
  "cddr",
  "caaar",
  "caadr",
  "cadar",
  "caddr",
  "cdaar",
  "cdadr",
  "cddar",
  "cdddr",
  "caaaar",
  "caaadr",
  "caadar",
  "caaddr",
  "cadaar",
  "cadadr",
  "caddar",
  "cadddr",
  "cdaaar",
  "cdaadr",
  "cdadar",
  "cdaddr",
  "cddaar",
  "cddadr",
  "cdddar",
  "cddddr",
  0
};



void
scm_init_pairs ()
{
  unsigned int subnr = 0;

  for (subnr = 0; cxrs [subnr]; subnr++)
    scm_c_define_subr (cxrs [subnr], scm_tc7_cxr, NULL);

#ifndef SCM_MAGIC_SNARFER
#include "libguile/pairs.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
