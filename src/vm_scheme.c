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

SCM_DEFINE_VM_FUNCTION (null_p, "null?", "null?", 1, 0)
{
  VM_SETUP_ARGS1 ();
  RETURN (SCM_BOOL (SCM_NULLP (a1)));
}

SCM_DEFINE_VM_FUNCTION (cons, "cons", "cons", 2, 0)
{
  VM_SETUP_ARGS2 ();
  CONS (ac, ac, a2);
  NEXT;
}

SCM_DEFINE_VM_FUNCTION (list, "list", "list", 0, 1)
{
  VM_SETUP_ARGSN ();
  ac = SCM_EOL;
  POP_LIST (nargs, ac);
  NEXT;
}

SCM_DEFINE_VM_FUNCTION (car, "car", "car", 1, 0)
{
  VM_SETUP_ARGS1 ();
  SCM_VALIDATE_CONS (0, a1);
  RETURN (SCM_CAR (a1));
}

SCM_DEFINE_VM_FUNCTION (cdr, "cdr", "cdr", 1, 0)
{
  VM_SETUP_ARGS1 ();
  SCM_VALIDATE_CONS (0, a1);
  RETURN (SCM_CDR (a1));
}

SCM_DEFINE_VM_FUNCTION (not, "not", "not", 1, 0)
{
  VM_SETUP_ARGS1 ();
  RETURN (SCM_BOOL (SCM_FALSEP (a1)));
}

SCM_DEFINE_VM_FUNCTION (append, "append", "append", 0, 1)
{
  VM_SETUP_ARGSN ();
  ac = SCM_EOL;
  POP_LIST (nargs, ac);
  RETURN (scm_append (ac));
}

SCM_DEFINE_VM_FUNCTION (append_x, "append!", "append!", 0, 1)
{
  VM_SETUP_ARGSN ();
  ac = SCM_EOL;
  POP_LIST (nargs, ac);
  RETURN (scm_append_x (ac));
}

SCM_DEFINE_VM_FUNCTION (catch, "catch", "catch", 3, 0)
{
  VM_SETUP_ARGS3 ();
  dynwinds = SCM_EOL;
}

SCM_DEFINE_VM_FUNCTION (call_cc, "call-with-current-continuation", "call/cc", 1, 0)
{
  SYNC (); /* must sync all registers */
  PUSH (SCM_VM_CAPTURE_CONT (vmp)); /* argument 1 */
  nargs = 1; /* the number of arguments */
  goto vm_call;
}
