/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"
#include "smob.h"

#include "arbiters.h"


/* {Arbiters}
 *
 * These procedures implement synchronization primitives.  Processors
 * with an atomic test-and-set instruction can use it here (and not
 * SCM_DEFER_INTS). 
 */

static long scm_tc16_arbiter;

#ifdef __STDC__
static int 
prinarb (SCM exp, SCM port, int writing)
#else
static int 
prinarb (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  scm_gen_puts (scm_regular_string, "#<arbiter ", port);
  if (SCM_CAR (exp) & (1L << 16))
    scm_gen_puts (scm_regular_string, "locked ", port);
  scm_iprin1 (SCM_CDR (exp), port, writing);
  scm_gen_putc ('>', port);
  return !0;
}

static scm_smobfuns arbsmob =
{
  scm_markcdr, scm_free0, prinarb, 0
};

SCM_PROC(s_make_arbiter, "make-arbiter", 1, 0, 0, scm_make_arbiter);
#ifdef __STDC__
SCM 
scm_make_arbiter (SCM name)
#else
SCM 
scm_make_arbiter (name)
     SCM name;
#endif
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_CDR (z) = name;
  SCM_CAR (z) = scm_tc16_arbiter;
  return z;
}

SCM_PROC(s_try_arbiter, "try-arbiter", 1, 0, 0, scm_try_arbiter);
#ifdef __STDC__
SCM 
scm_try_arbiter (SCM arb)
#else
SCM 
scm_try_arbiter (arb)
     SCM arb;
#endif
{
  SCM_ASSERT ((SCM_TYP16 (arb) == scm_tc16_arbiter), arb, SCM_ARG1, s_try_arbiter);
  SCM_DEFER_INTS;
  if (SCM_CAR (arb) & (1L << 16))
    arb = SCM_BOOL_F;
  else
    {
      SCM_CAR (arb) = scm_tc16_arbiter | (1L << 16);
      arb = SCM_BOOL_T;
    }
  SCM_ALLOW_INTS;
  return arb;
}


SCM_PROC(s_release_arbiter, "release-arbiter", 1, 0, 0, scm_release_arbiter);
#ifdef __STDC__
SCM 
scm_release_arbiter (SCM arb)
#else
SCM 
scm_release_arbiter (arb)
     SCM arb;
#endif
{
  SCM_ASSERT ((SCM_TYP16 (arb) == scm_tc16_arbiter), arb, SCM_ARG1, s_release_arbiter);
  if (!(SCM_CAR (arb) & (1L << 16)))
    return SCM_BOOL_F;
  SCM_CAR (arb) = scm_tc16_arbiter;
  return SCM_BOOL_T;
}


#ifdef __STDC__
void
scm_init_arbiters (void)
#else
void
scm_init_arbiters ()
#endif
{
  scm_tc16_arbiter = scm_newsmob (&arbsmob);
#include "arbiters.x"
}

