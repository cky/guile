/*	Copyright (C) 1995,1996, 1997, 2000, 2001, 2004 Free Software Foundation, Inc.
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




#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/arbiters.h"


/* ENHANCE-ME: If the cpu has an atomic test-and-set instruction it could be
   used instead of a mutex in try-arbiter and release-arbiter.

   For the i386 family, cmpxchg would suit but it's only available on 80486
   and higher so that would have to be checked, perhaps at run-time when
   setting up the definitions of the scheme procedures, or at compile time
   if we interpret a host cpu type like "i686" to mean not less than that
   chip.  */

static scm_t_bits scm_tc16_arbiter;


#define SCM_ARB_LOCKED(arb)  ((SCM_CELL_WORD_0 (arb)) & (1L << 16))
#define SCM_LOCK_ARB(arb)    (SCM_SET_CELL_WORD_0 ((arb), scm_tc16_arbiter | (1L << 16)));
#define SCM_UNLOCK_ARB(arb)  (SCM_SET_CELL_WORD_0 ((arb), scm_tc16_arbiter));

static int 
arbiter_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<arbiter ", port);
  if (SCM_ARB_LOCKED (exp))
    scm_puts ("locked ", port);
  scm_iprin1 (SCM_PACK (SCM_SMOB_DATA (exp)), port, pstate);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_make_arbiter, "make-arbiter", 1, 0, 0, 
	    (SCM name),
	    "Return an object of type arbiter and name @var{name}. Its\n"
	    "state is initially unlocked.  Arbiters are a way to achieve\n"
	    "process synchronization.")
#define FUNC_NAME s_scm_make_arbiter
{
  SCM_RETURN_NEWSMOB (scm_tc16_arbiter, SCM_UNPACK (name));
}
#undef FUNC_NAME


/* The mutex here is so two threads can't both see the arbiter unlocked and
   both proceed to lock and return #t.  The arbiter itself wouldn't be
   corrupted by this, but two threads both getting #t would be entirely
   contrary to the intended semantics.  */

SCM_DEFINE (scm_try_arbiter, "try-arbiter", 1, 0, 0, 
	    (SCM arb),
	    "Return @code{#t} and lock the arbiter @var{arb} if the arbiter\n"
	    "was unlocked. Otherwise, return @code{#f}.")
#define FUNC_NAME s_scm_try_arbiter
{
  SCM_VALIDATE_SMOB (1, arb, arbiter);

  scm_mutex_lock (&scm_i_misc_mutex);
  if (SCM_ARB_LOCKED(arb))
    arb = SCM_BOOL_F;
  else
    {
      SCM_LOCK_ARB(arb);
      arb = SCM_BOOL_T;
    }
  scm_mutex_unlock (&scm_i_misc_mutex);
  return arb;
}
#undef FUNC_NAME


/* The mutex here is so two threads can't both see the arbiter locked and
   both proceed to unlock and return #t.  The arbiter itself wouldn't be
   corrupted by this, but we don't want two threads both thinking they were
   the unlocker.  The intended usage is for the code which locked to be
   responsible for unlocking, but we guarantee the return value even if
   multiple threads compete.  */

SCM_DEFINE (scm_release_arbiter, "release-arbiter", 1, 0, 0,
	    (SCM arb),
	    "Return @code{#t} and unlock the arbiter @var{arb} if the\n"
	    "arbiter was locked. Otherwise, return @code{#f}.")
#define FUNC_NAME s_scm_release_arbiter
{
  SCM ret;
  SCM_VALIDATE_SMOB (1, arb, arbiter);

  scm_mutex_lock (&scm_i_misc_mutex);
  if (!SCM_ARB_LOCKED(arb))
    ret = SCM_BOOL_F;
  else
    {
      SCM_UNLOCK_ARB (arb);
      ret = SCM_BOOL_T;
    }
  scm_mutex_unlock (&scm_i_misc_mutex);
  return ret;
}
#undef FUNC_NAME



void
scm_init_arbiters ()
{
  scm_tc16_arbiter = scm_make_smob_type ("arbiter", 0);
  scm_set_smob_mark (scm_tc16_arbiter, scm_markcdr);
  scm_set_smob_print (scm_tc16_arbiter, arbiter_print);
#include "libguile/arbiters.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
