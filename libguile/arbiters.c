/*	Copyright (C) 1995,1996, 1997, 2000 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/arbiters.h"


/* {Arbiters}
 *
 * These procedures implement synchronization primitives.  Processors
 * with an atomic test-and-set instruction can use it here (and not
 * SCM_DEFER_INTS). 
 */

static scm_bits_t scm_tc16_arbiter;


#define SCM_ARB_LOCKED(arb)  ((SCM_CELL_WORD_0 (arb)) & (1L << 16))
#define SCM_LOCK_ARB(arb)    (SCM_SET_CELL_WORD_0 ((arb), scm_tc16_arbiter | (1L << 16)));
#define SCM_UNLOCK_ARB(arb)  (SCM_SET_CELL_WORD_0 ((arb), scm_tc16_arbiter));

static int 
arbiter_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<arbiter ", port);
  if (SCM_ARB_LOCKED (exp))
    scm_puts ("locked ", port);
  scm_iprin1 (SCM_CDR (exp), port, pstate);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_make_arbiter, "make-arbiter", 1, 0, 0, 
           (SCM name),
"Returns an object of type arbiter and name name. Its state is initially unlocked.\n"
"Arbiters are a way to achieve process synchronization.")
#define FUNC_NAME s_scm_make_arbiter
{
  SCM_RETURN_NEWSMOB (scm_tc16_arbiter, SCM_UNPACK (name));
}
#undef FUNC_NAME

SCM_DEFINE (scm_try_arbiter, "try-arbiter", 1, 0, 0, 
           (SCM arb),
"Returns #t and locks arbiter if arbiter was unlocked. Otherwise, returns #f.")
#define FUNC_NAME s_scm_try_arbiter
{
  SCM_VALIDATE_SMOB (1,arb,arbiter);
  SCM_DEFER_INTS;
  if (SCM_ARB_LOCKED(arb))
    arb = SCM_BOOL_F;
  else
    {
      SCM_LOCK_ARB(arb);
      arb = SCM_BOOL_T;
    }
  SCM_ALLOW_INTS;
  return arb;
}
#undef FUNC_NAME


SCM_DEFINE (scm_release_arbiter, "release-arbiter", 1, 0, 0, 
           (SCM arb),
"Returns #t and unlocks arbiter if arbiter was locked. Otherwise, returns #f.")
#define FUNC_NAME s_scm_release_arbiter
{
  SCM_VALIDATE_SMOB (1,arb,arbiter);
  if (! SCM_ARB_LOCKED(arb))
    return SCM_BOOL_F;
  SCM_UNLOCK_ARB (arb);
  return SCM_BOOL_T;
}
#undef FUNC_NAME



void
scm_init_arbiters ()
{
  scm_tc16_arbiter = scm_make_smob_type ("arbiter", 0);
  scm_set_smob_mark (scm_tc16_arbiter, scm_markcdr);
  scm_set_smob_print (scm_tc16_arbiter, arbiter_print);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/arbiters.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
