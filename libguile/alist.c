/* Copyright (C) 1995, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
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
#include "libguile/eq.h"
#include "libguile/list.h"

#include "libguile/validate.h"
#include "libguile/alist.h"



SCM_DEFINE (scm_acons, "acons", 3, 0, 0,
           (SCM key, SCM value, SCM alist),
	    "Adds a new key-value pair to @var{alist}.  A new pair is\n"
	    "created whose car is @var{key} and whose cdr is @var{value}, and the\n"
	    "pair is consed onto @var{alist}, and the new list is returned.  This\n"
	    "function is @emph{not} destructive; @var{alist} is not modified.")
#define FUNC_NAME s_scm_acons
{
  SCM pair;
  SCM head;

  SCM_NEWCELL (pair);
  SCM_SET_CELL_OBJECT_0 (pair, key);
  SCM_SET_CELL_OBJECT_1 (pair, value);

  SCM_NEWCELL (head);
  SCM_SET_CELL_OBJECT_0 (head, pair);
  SCM_SET_CELL_OBJECT_1 (head, alist);

  return head;
}
#undef FUNC_NAME



SCM_DEFINE (scm_sloppy_assq, "sloppy-assq", 2, 0, 0,
            (SCM key, SCM alist),
	    "Behaves like @code{assq} but does not do any error checking.\n"
	    "Recommended only for use in Guile internals.")
#define FUNC_NAME s_scm_sloppy_assq
{
  for (; SCM_CONSP (alist); alist = SCM_CDR (alist))
    {
      SCM tmp = SCM_CAR (alist);
      if (SCM_CONSP (tmp) && SCM_EQ_P (SCM_CAR (tmp), key))
	return tmp;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_sloppy_assv, "sloppy-assv", 2, 0, 0,
            (SCM key, SCM alist),
	    "Behaves like @code{assv} but does not do any error checking.\n"
	    "Recommended only for use in Guile internals.")
#define FUNC_NAME s_scm_sloppy_assv
{
  for (; SCM_CONSP (alist); alist = SCM_CDR (alist))
    {
      SCM tmp = SCM_CAR (alist);
      if (SCM_CONSP (tmp)
	  && SCM_NFALSEP (scm_eqv_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_assoc, "sloppy-assoc", 2, 0, 0,
            (SCM key, SCM alist),
	    "Behaves like @code{assoc} but does not do any error checking.\n"
	    "Recommended only for use in Guile internals.")
#define FUNC_NAME s_scm_sloppy_assoc
{
  for (; SCM_CONSP (alist); alist = SCM_CDR (alist))
    {
      SCM tmp = SCM_CAR (alist);
      if (SCM_CONSP (tmp)
	  && SCM_NFALSEP (scm_equal_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME




SCM_DEFINE (scm_assq, "assq", 2, 0, 0,
           (SCM key, SCM alist),
	    "@deffnx primitive assv key alist\n"
	    "@deffnx primitive assoc key alist\n"
	    "Fetches the entry in @var{alist} that is associated with @var{key}.  To\n"
	    "decide whether the argument @var{key} matches a particular entry in\n"
	    "@var{alist}, @code{assq} compares keys with @code{eq?}, @code{assv}\n"
	    "uses @code{eqv?} and @code{assoc} uses @code{equal?}.  If @var{key}\n"
	    "cannot be found in @var{alist} (according to whichever equality\n"
	    "predicate is in use), then @code{#f} is returned.  These functions\n"
	    "return the entire alist entry found (i.e. both the key and the value).")
#define FUNC_NAME s_scm_assq
{
  for (; SCM_CONSP (alist); alist = SCM_CDR (alist)) 
    {
      SCM tmp = SCM_CAR (alist);
      SCM_VALIDATE_CONS (SCM_ARG2, tmp);
      if (SCM_EQ_P (SCM_CAR (tmp), key))
	return tmp;
    }
  SCM_VALIDATE_NULL (2, alist);
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assv, "assv", 2, 0, 0,
           (SCM key, SCM alist),
	    "Behaves like @code{assq} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv
{
  for(; SCM_CONSP (alist); alist = SCM_CDR (alist)) 
    {
      SCM tmp = SCM_CAR (alist);
      SCM_VALIDATE_CONS (SCM_ARG2, tmp);
      if (SCM_NFALSEP (scm_eqv_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  SCM_VALIDATE_NULL (2, alist);
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assoc, "assoc", 2, 0, 0,
           (SCM key, SCM alist),
	    "Behaves like @code{assq} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc
{
  for(; SCM_CONSP (alist); alist = SCM_CDR (alist)) 
    {
      SCM tmp = SCM_CAR (alist);
      SCM_VALIDATE_CONS (SCM_ARG2, tmp);
      if (SCM_NFALSEP (scm_equal_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  SCM_VALIDATE_NULL (2, alist);
  return SCM_BOOL_F;
}
#undef FUNC_NAME




SCM_DEFINE (scm_assq_ref, "assq-ref", 2, 0, 0,
            (SCM alist, SCM key),
	    "@deffnx primitive assv-ref alist key\n"
	    "@deffnx primitive assoc-ref alist key\n"
	    "Like @code{assq}, @code{assv} and @code{assoc}, except that only the\n"
	    "value associated with @var{key} in @var{alist} is returned.  These\n"
	    "functions are equivalent to\n\n"
	    "@lisp\n"
	    "(let ((ent (@var{associator} @var{key} @var{alist})))\n"
	    "  (and ent (cdr ent)))\n"
	    "@end lisp\n\n"
	    "where @var{associator} is one of @code{assq}, @code{assv} or @code{assoc}.")
#define FUNC_NAME s_scm_assq_ref
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assv_ref, "assv-ref", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-ref} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv_ref
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assoc_ref, "assoc-ref", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-ref} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc_ref
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME






SCM_DEFINE (scm_assq_set_x, "assq-set!", 3, 0, 0,
            (SCM alist, SCM key, SCM val),
	    "@deffnx primitive assv-set! alist key value\n"
	    "@deffnx primitive assoc-set! alist key value\n"
	    "Reassociate @var{key} in @var{alist} with @var{value}: find any existing\n"
	    "@var{alist} entry for @var{key} and associate it with the new\n"
	    "@var{value}.  If @var{alist} does not contain an entry for @var{key},\n"
	    "add a new one.  Return the (possibly new) alist.\n\n"
	    "These functions do not attempt to verify the structure of @var{alist},\n"
	    "and so may cause unusual results if passed an object that is not an\n"
	    "association list.")
#define FUNC_NAME s_scm_assq_set_x
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (SCM_CONSP (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}
#undef FUNC_NAME

SCM_DEFINE (scm_assv_set_x, "assv-set!", 3, 0, 0,
            (SCM alist, SCM key, SCM val),
	    "Behaves like @code{assq-set!} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv_set_x
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (SCM_CONSP (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}
#undef FUNC_NAME

SCM_DEFINE (scm_assoc_set_x, "assoc-set!", 3, 0, 0,
            (SCM alist, SCM key, SCM val),
	    "Behaves like @code{assq-set!} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc_set_x
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (SCM_CONSP (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}
#undef FUNC_NAME




SCM_DEFINE (scm_assq_remove_x, "assq-remove!", 2, 0, 0,
            (SCM alist, SCM key),
	    "@deffnx primitive assv-remove! alist key\n"
	    "@deffnx primitive assoc-remove! alist key\n"
	    "Delete the first entry in @var{alist} associated with @var{key}, and return\n"
	    "the resulting alist.")
#define FUNC_NAME s_scm_assq_remove_x
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (SCM_CONSP (handle))
    alist = scm_delq1_x (handle, alist);

  return alist;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assv_remove_x, "assv-remove!", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-remove!} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv_remove_x
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (SCM_CONSP (handle))
    alist = scm_delq1_x (handle, alist);

  return alist;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assoc_remove_x, "assoc-remove!", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-remove!} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc_remove_x
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (SCM_CONSP (handle))
    alist = scm_delq1_x (handle, alist);

  return alist;
}
#undef FUNC_NAME






void
scm_init_alist ()
{
#include "libguile/alist.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
