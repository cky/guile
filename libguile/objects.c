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


/* This file contains those minimal pieces of the Guile Object
 * Oriented Programming System which needs to be included in
 * libguile.
 */

#include "_scm.h"

#include "struct.h"

#include "objects.h"


SCM scm_metaclass_standard;
SCM *scm_entity_vtable;

void
scm_init_objects ()
{
  SCM ms = scm_makfrom0str (SCM_METACLASS_STANDARD_LAYOUT);
  SCM ml = scm_make_struct_layout (ms);
  SCM mt = scm_make_vtable_vtable (ml, SCM_INUM0,
				   SCM_LIST3 (SCM_BOOL_F, SCM_EOL, SCM_EOL));
  
  SCM es = scm_makfrom0str (SCM_ENTITY_LAYOUT);
  SCM el = scm_make_struct_layout (es);
  SCM et = scm_make_struct (mt, SCM_INUM0,
			    SCM_LIST4 (el, SCM_BOOL_F, SCM_EOL, SCM_EOL));

  scm_sysintern ("<standard-metaclass>", mt);
  scm_metaclass_standard = mt;
  scm_sysintern ("<entity-class>", et);
  scm_entity_vtable = SCM_STRUCT_DATA (et);
}
