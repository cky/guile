/*	Copyright (C) 1995,1996, 2000, 2001 Free Software Foundation, Inc.
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




#include "libguile/_scm.h"
#include "libguile/hashtab.h"
#include "libguile/alist.h"
#include "libguile/root.h"
#include "libguile/weaks.h"

#include "libguile/objprop.h"


/* {Object Properties}
 */

SCM_DEFINE (scm_object_properties, "object-properties", 1, 0, 0, 
           (SCM obj),
	    "@deffnx primitive procedure-properties obj\n"
	    "Return @var{obj}'s property list.")
#define FUNC_NAME s_scm_object_properties
{
  return scm_hashq_ref (scm_object_whash, obj, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_object_properties_x, "set-object-properties!", 2, 0, 0,
	    (SCM obj, SCM alist),
	    "@deffnx primitive set-procedure-properties! obj alist\n"
	    "Set @var{obj}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_object_properties_x
{
  SCM handle = scm_hashq_create_handle_x (scm_object_whash, obj, alist);
  SCM_SETCDR (handle, alist);
  return alist;
}
#undef FUNC_NAME

SCM_DEFINE (scm_object_property, "object-property", 2, 0, 0,
           (SCM obj, SCM key),
	    "@deffnx primitive procedure-property obj key\n"
	    "Return the property of @var{obj} with name @var{key}.")
#define FUNC_NAME s_scm_object_property
{
  SCM assoc;
  assoc = scm_assq (key, scm_object_properties (obj));
  return (SCM_NIMP (assoc) ? SCM_CDR (assoc) : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_object_property_x, "set-object-property!", 3, 0, 0,
	    (SCM obj, SCM key, SCM value),
	    "@deffnx primitive set-procedure-property! obj key value\n"
	    "In @var{obj}'s property list, set the property named @var{key}\n"
	    "to @var{value}.")
#define FUNC_NAME s_scm_set_object_property_x
{
  SCM h;
  SCM assoc;
  h = scm_hashq_create_handle_x (scm_object_whash, obj, SCM_EOL);
  SCM_DEFER_INTS;
  assoc = scm_assq (key, SCM_CDR (h));
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, value);
  else
    {
      assoc = scm_acons (key, value, SCM_CDR (h));
      SCM_SETCDR (h, assoc);
    }
  SCM_ALLOW_INTS;
  return value;
}
#undef FUNC_NAME


void
scm_init_objprop ()
{
  scm_object_whash = scm_make_weak_key_hash_table (SCM_MAKINUM (511));
#ifndef SCM_MAGIC_SNARFER
#include "libguile/objprop.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
