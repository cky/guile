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


/*
 * Predicates
 */

#undef PRED
#define PRED(ctest,stest)			\
{						\
  ARGS1 (a1);					\
  if (SCM_INUMP (a1))				\
    RETURN (SCM_BOOL (ctest));			\
  RETURN (stest (a1));				\
}

VM_DEFINE_FUNCTION (zero, "zero?", 1)
{
  PRED (SCM_INUM (a1) == 0, scm_zero_p);
}


/*
 * Relational tests
 */

#undef REL
#define REL(crel,srel)						\
{								\
  ARGS2 (a1, a2);						\
  if (SCM_INUMP (a1) && SCM_INUMP (a2))				\
    RETURN (SCM_BOOL (SCM_INUM (a1) crel SCM_INUM (a2)));	\
  RETURN (srel (a1, a2));					\
}

VM_DEFINE_FUNCTION (ee, "ee?", 2)
{
  REL (==, scm_num_eq_p);
}

VM_DEFINE_FUNCTION (lt, "lt?", 2)
{
  REL (<, scm_less_p);
}

VM_DEFINE_FUNCTION (le, "le?", 2)
{
  REL (<=, scm_leq_p);
}

VM_DEFINE_FUNCTION (gt, "gt?", 2)
{
  REL (>, scm_gr_p);
}

VM_DEFINE_FUNCTION (ge, "ge?", 2)
{
  REL (>=, scm_geq_p);
}


/*
 * Functions
 */

#undef FUNC1
#define FUNC1(CEXP,SEXP)			\
{						\
  ARGS1 (a1);					\
  if (SCM_INUMP (a1))				\
    {						\
      int n = CEXP;				\
      if (SCM_FIXABLE (n))			\
	RETURN (SCM_MAKINUM (n));		\
    }						\
  RETURN (SEXP);				\
}

#undef FUNC2
#define FUNC2(CFUNC,SFUNC)				\
{							\
  ARGS2 (a1, a2);					\
  if (SCM_INUMP (a1) && SCM_INUMP (a2))			\
    {							\
      int n = SCM_INUM (a1) CFUNC SCM_INUM (a2);	\
      if (SCM_FIXABLE (n))				\
	RETURN (SCM_MAKINUM (n));			\
    }							\
  RETURN (SFUNC (a1, a2));				\
}

VM_DEFINE_FUNCTION (neg, "neg", 1)
{
  FUNC1 (- SCM_INUM (a1), scm_difference (a1, SCM_UNDEFINED));
}

VM_DEFINE_FUNCTION (inc, "inc", 1)
{
  FUNC1 (SCM_INUM (a1) + 1, scm_sum (a1, SCM_MAKINUM (1)));
}

VM_DEFINE_FUNCTION (dec, "dec", 1)
{
  FUNC1 (SCM_INUM (a1) - 1, scm_difference (a1, SCM_MAKINUM (1)));
}

VM_DEFINE_FUNCTION (add, "add", 2)
{
  FUNC2 (+, scm_sum);
}

VM_DEFINE_FUNCTION (sub, "sub", 2)
{
  FUNC2 (-, scm_difference);
}

VM_DEFINE_FUNCTION (remainder, "remainder", 2)
{
  ARGS2 (a1, a2);
  RETURN (scm_remainder (a1, a2));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
