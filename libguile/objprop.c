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
#include "hashtab.h"
#include "alist.h"
#include "weaks.h"

#include "objprop.h"


/* {Object Properties}
 */

SCM_PROC(s_object_properties, "object-properties", 1, 0, 0, scm_object_properties);

SCM
scm_object_properties (obj)
     SCM obj;
{
  return scm_hashq_ref (scm_object_whash, obj, SCM_EOL);
}


SCM_PROC(s_set_object_properties_x, "set-object-properties!", 2, 0, 0, scm_set_object_properties_x);

SCM
scm_set_object_properties_x (obj, plist)
     SCM obj;
     SCM plist;
{
  SCM handle = scm_hashq_create_handle_x (scm_object_whash, obj, plist);
  SCM_SETCDR (handle, plist);
  return plist;
}

SCM_PROC(s_object_property, "object-property", 2, 0, 0, scm_object_property);

SCM
scm_object_property (obj, key)
     SCM obj;
     SCM key;
{
  SCM assoc;
  assoc = scm_assq (key, SCM_CDR (scm_object_properties (obj)));
  return (SCM_NIMP (assoc) ? SCM_CDR (assoc) : SCM_BOOL_F);
}

SCM_PROC(s_set_object_property_x, "set-object-property!", 3, 0, 0, scm_set_object_property_x);

SCM
scm_set_object_property_x (obj, key, val)
     SCM obj;
     SCM key;
     SCM val;
{
  SCM h;
  SCM assoc;
  h = scm_hashq_create_handle_x (scm_object_whash, obj, SCM_EOL);
  SCM_DEFER_INTS;
  assoc = scm_assoc (key, SCM_CDR (h));
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, val);
  else
    {
      assoc = scm_acons (key, val, SCM_CDR (h));
      SCM_SETCDR (h, assoc);
    }
  SCM_ALLOW_INTS;
  return val;
}


void
scm_init_objprop ()
{
  scm_object_whash = scm_make_weak_key_hash_table (SCM_MAKINUM (511));
#include "objprop.x"
}

