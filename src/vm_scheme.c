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

VM_DEFINE_FUNCTION (not, "not", 1)
{
  ARGS1 (a1);
  RETURN (SCM_BOOL (SCM_FALSEP (a1)));
}

VM_DEFINE_FUNCTION (not_not, "not-not", 1)
{
  ARGS1 (a1);
  RETURN (SCM_BOOL (!SCM_FALSEP (a1)));
}

VM_DEFINE_FUNCTION (eq, "eq?", 2)
{
  ARGS2 (a1, a2);
  RETURN (SCM_BOOL (SCM_EQ_P (a1, a2)));
}

VM_DEFINE_FUNCTION (not_eq, "not-eq?", 2)
{
  ARGS2 (a1, a2);
  RETURN (SCM_BOOL (!SCM_EQ_P (a1, a2)));
}

VM_DEFINE_FUNCTION (nullp, "null?", 1)
{
  ARGS1 (a1);
  RETURN (SCM_BOOL (SCM_NULLP (a1)));
}

VM_DEFINE_FUNCTION (not_nullp, "not-null?", 1)
{
  ARGS1 (a1);
  RETURN (SCM_BOOL (!SCM_NULLP (a1)));
}

VM_DEFINE_FUNCTION (pairp, "pair?", 1)
{
  ARGS1 (a1);
  RETURN (SCM_BOOL (SCM_CONSP (a1)));
}

VM_DEFINE_FUNCTION (listp, "list?", 1)
{
  ARGS1 (a1);
  RETURN (SCM_BOOL (scm_ilength (a1) >= 0));
}

VM_DEFINE_FUNCTION (cons, "cons", 2)
{
  ARGS2 (a1, a2);
  CONS (a1, a1, a2);
  RETURN (a1);
}

VM_DEFINE_FUNCTION (car, "car", 1)
{
  ARGS1 (a1);
  SCM_VALIDATE_CONS (1, a1);
  RETURN (SCM_CAR (a1));
}

VM_DEFINE_FUNCTION (cdr, "cdr", 1)
{
  ARGS1 (a1);
  SCM_VALIDATE_CONS (1, a1);
  RETURN (SCM_CDR (a1));
}

VM_DEFINE_FUNCTION (set_car, "set-car!", 2)
{
  ARGS2 (a1, a2);
  SCM_VALIDATE_CONS (1, a1);
  SCM_SETCAR (a1, a2);
  RETURN (SCM_UNSPECIFIED);
}

VM_DEFINE_FUNCTION (set_cdr, "set-cdr!", 2)
{
  ARGS2 (a1, a2);
  SCM_VALIDATE_CONS (1, a1);
  SCM_SETCDR (a1, a2);
  RETURN (SCM_UNSPECIFIED);
}

VM_DEFINE_FUNCTION (list, "list", -1)
{
  ARGSN (an);
  POP_LIST (an);
  NEXT;
}

VM_DEFINE_FUNCTION (vector, "vector", -1)
{
  ARGSN (an);
  POP_LIST (an);
  *sp = scm_vector (*sp);
  NEXT;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
