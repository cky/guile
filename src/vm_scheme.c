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

VM_DEFINE_FUNCTION (not, "not", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (SCM_FALSEP (x)));
}

VM_DEFINE_FUNCTION (not_not, "not-not", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (!SCM_FALSEP (x)));
}

VM_DEFINE_FUNCTION (eq, "eq?", 2)
{
  ARGS2 (x, y);
  RETURN (SCM_BOOL (SCM_EQ_P (x, y)));
}

VM_DEFINE_FUNCTION (not_eq, "not-eq?", 2)
{
  ARGS2 (x, y);
  RETURN (SCM_BOOL (!SCM_EQ_P (x, y)));
}

VM_DEFINE_FUNCTION (nullp, "null?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (SCM_NULLP (x)));
}

VM_DEFINE_FUNCTION (not_nullp, "not-null?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (!SCM_NULLP (x)));
}

VM_DEFINE_FUNCTION (eqv, "eqv?", 2)
{
  ARGS2 (x, y);
  if (SCM_EQ_P (x, y))
    RETURN (SCM_BOOL_T);
  if (SCM_IMP (x) || SCM_IMP (y))
    RETURN (SCM_BOOL_F);
  SYNC_BEFORE_GC ();
  RETURN (scm_eqv_p (x, y));
}

VM_DEFINE_FUNCTION (equal, "equal?", 2)
{
  ARGS2 (x, y);
  if (SCM_EQ_P (x, y))
    RETURN (SCM_BOOL_T);
  if (SCM_IMP (x) || SCM_IMP (y))
    RETURN (SCM_BOOL_F);
  SYNC_BEFORE_GC ();
  RETURN (scm_equal_p (x, y));
}

VM_DEFINE_FUNCTION (pairp, "pair?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (SCM_CONSP (x)));
}

VM_DEFINE_FUNCTION (listp, "list?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (scm_ilength (x) >= 0));
}


/*
 * Basic data
 */

VM_DEFINE_FUNCTION (cons, "cons", 2)
{
  ARGS2 (x, y);
  CONS (x, x, y);
  RETURN (x);
}

VM_DEFINE_FUNCTION (car, "car", 1)
{
  ARGS1 (x);
  SCM_VALIDATE_CONS (1, x);
  RETURN (SCM_CAR (x));
}

VM_DEFINE_FUNCTION (cdr, "cdr", 1)
{
  ARGS1 (x);
  SCM_VALIDATE_CONS (1, x);
  RETURN (SCM_CDR (x));
}

VM_DEFINE_FUNCTION (set_car, "set-car!", 2)
{
  ARGS2 (x, y);
  SCM_VALIDATE_CONS (1, x);
  SCM_SETCAR (x, y);
  RETURN (SCM_UNSPECIFIED);
}

VM_DEFINE_FUNCTION (set_cdr, "set-cdr!", 2)
{
  ARGS2 (x, y);
  SCM_VALIDATE_CONS (1, x);
  SCM_SETCDR (x, y);
  RETURN (SCM_UNSPECIFIED);
}

VM_DEFINE_FUNCTION (list, "list", -1)
{
  ARGSN (n);
  POP_LIST (n);
  NEXT;
}

VM_DEFINE_FUNCTION (vector, "vector", -1)
{
  ARGSN (n);
  POP_LIST (n);
  *sp = scm_vector (*sp);
  NEXT;
}


/*
 * Numeric relational tests
 */

#undef REL
#define REL(crel,srel)					\
{							\
  ARGS2 (x, y);						\
  if (SCM_INUMP (x) && SCM_INUMP (y))			\
    RETURN (SCM_BOOL (SCM_INUM (x) crel SCM_INUM (y)));	\
  RETURN (srel (x, y));					\
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
 * Numeric functions
 */

#undef FUNC1
#define FUNC1(CEXP,SEXP)			\
{						\
  ARGS1 (x);					\
  if (SCM_INUMP (x))				\
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
  ARGS2 (x, y);					\
  if (SCM_INUMP (x) && SCM_INUMP (y))			\
    {							\
      int n = SCM_INUM (x) CFUNC SCM_INUM (y);	\
      if (SCM_FIXABLE (n))				\
	RETURN (SCM_MAKINUM (n));			\
    }							\
  RETURN (SFUNC (x, y));				\
}

VM_DEFINE_FUNCTION (add, "add", 2)
{
  FUNC2 (+, scm_sum);
}

VM_DEFINE_FUNCTION (sub, "sub", 2)
{
  FUNC2 (-, scm_difference);
}

VM_DEFINE_FUNCTION (mul, "mul", 2)
{
  ARGS2 (x, y);
  RETURN (scm_product (x, y));
}

VM_DEFINE_FUNCTION (div, "div", 2)
{
  ARGS2 (x, y);
  RETURN (scm_divide (x, y));
}

VM_DEFINE_FUNCTION (quo, "quo", 2)
{
  ARGS2 (x, y);
  RETURN (scm_quotient (x, y));
}

VM_DEFINE_FUNCTION (rem, "rem", 2)
{
  ARGS2 (x, y);
  RETURN (scm_remainder (x, y));
}

VM_DEFINE_FUNCTION (mod, "mod", 2)
{
  ARGS2 (x, y);
  RETURN (scm_modulo (x, y));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
