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

/* This file is included in vm_engine.c */

#include "vm-snarf.h"

SCM_DEFINE_VM_FUNCTION (zero_p, "zero?", "zero?", 1, 0)
{
  if (SCM_INUMP (ac))
    RETURN (SCM_BOOL (SCM_EQ_P (ac, SCM_INUM0)));
  RETURN (scm_zero_p (ac));
}

SCM_DEFINE_VM_FUNCTION (inc, "1+", "inc", 1, 0)
{
  if (SCM_INUMP (ac))
    {
      int n = SCM_INUM (ac) + 1;
      if (SCM_FIXABLE (n))
	RETURN (SCM_MAKINUM (n));
    }
  RETURN (scm_sum (ac, SCM_MAKINUM (1)));
}

SCM_DEFINE_VM_FUNCTION (dec, "1-", "dec", 1, 0)
{
  if (SCM_INUMP (ac))
    {
      int n = SCM_INUM (ac) - 1;
      if (SCM_FIXABLE (n))
	RETURN (SCM_MAKINUM (n));
    }
  RETURN (scm_difference (ac, SCM_MAKINUM (1)));
}

SCM_DEFINE_VM_FUNCTION (add, "+", "add", 0, 1)
{
  VM_SETUP_ARGSN ();
  ac = SCM_MAKINUM (0);
  while (an-- > 0)
    {
      POP (a2);
      if (SCM_INUMP (ac) && SCM_INUMP (a2))
	{
	  int n = SCM_INUM (ac) + SCM_INUM (a2);
	  if (SCM_FIXABLE (n))
	    {
	      ac = SCM_MAKINUM (n);
	      continue;
	    }
	}
      ac = scm_sum (ac, a2);
    }
  NEXT;
}

SCM_DEFINE_VM_FUNCTION (add2, "+", "add2", 2, 0)
{
  VM_SETUP_ARGS2 ();
  if (SCM_INUMP (ac) && SCM_INUMP (a2))
    {
      int n = SCM_INUM (ac) + SCM_INUM (a2);
      if (SCM_FIXABLE (n))
	RETURN (SCM_MAKINUM (n));
    }
  RETURN (scm_sum (ac, a2));
}

SCM_DEFINE_VM_FUNCTION (sub, "-", "sub", 1, 1)
{
  VM_SETUP_ARGSN ();
  ac = SCM_MAKINUM (0);
  while (an-- > 1)
    {
      POP (a2);
      if (SCM_INUMP (ac) && SCM_INUMP (a2))
	{
	  int n = SCM_INUM (ac) + SCM_INUM (a2);
	  if (SCM_FIXABLE (n))
	    {
	      ac = SCM_MAKINUM (n);
	      continue;
	    }
	}
      ac = scm_difference (ac, a2);
    }
  POP (a2);
  if (SCM_INUMP (ac) && SCM_INUMP (a2))
    {
      int n = SCM_INUM (a2) - SCM_INUM (ac);
      if (SCM_FIXABLE (n))
	RETURN (SCM_MAKINUM (n));
    }
  RETURN (scm_difference (a2, ac));
}

SCM_DEFINE_VM_FUNCTION (sub2, "-", "sub2", 2, 0)
{
  VM_SETUP_ARGS2 ();
  if (SCM_INUMP (ac) && SCM_INUMP (a2))
    {
      int n = SCM_INUM (ac) - SCM_INUM (a2);
      if (SCM_FIXABLE (n))
	RETURN (SCM_MAKINUM (n));
    }
  RETURN (scm_difference (ac, a2));
}

SCM_DEFINE_VM_FUNCTION (minus, "-", "minus", 1, 0)
{
  if (SCM_INUMP (ac))
    {
      int n = - SCM_INUM (ac);
      if (SCM_FIXABLE (n))
	RETURN (SCM_MAKINUM (n));
    }
  RETURN (scm_difference (ac, SCM_UNDEFINED));
}

#define REL2(CREL,SREL)						\
  VM_SETUP_ARGS2 ();						\
  if (SCM_INUMP (ac) && SCM_INUMP (a2))				\
    RETURN (SCM_BOOL (SCM_INUM (ac) CREL SCM_INUM (a2)));	\
  RETURN (SREL (ac, a2))

SCM_DEFINE_VM_FUNCTION (lt2, "<", "lt2", 2, 0)
{
  REL2 (<, scm_less_p);
}

SCM_DEFINE_VM_FUNCTION (gt2, ">", "gt2", 2, 0)
{
  REL2 (>, scm_gr_p);
}

SCM_DEFINE_VM_FUNCTION (le2, "<=", "le2", 2, 0)
{
  REL2 (<=, scm_leq_p);
}

SCM_DEFINE_VM_FUNCTION (ge2, ">=", "ge2", 2, 0)
{
  REL2 (>=, scm_geq_p);
}

SCM_DEFINE_VM_FUNCTION (num_eq2, "=", "num-eq2", 2, 0)
{
  REL2 (==, scm_num_eq_p);
}
