/* classes: h_files */

#ifndef OBJECTSH
#define OBJECTSH

/*	Copyright (C) 1996 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"
#include "libguile/struct.h"



#define SCM_I_ENTITYP(OBJ)\
(SCM_STRUCT_VTABLE_DATA (OBJ) == scm_entity_vtable)
#define SCM_ENTITY(OBJ) ((scm_entity*) SCM_STRUCT_DATA (OBJ))
#define SCM_ENTITY_PROC_0(OBJ) (SCM_ENTITY (OBJ)->proc0)
#define SCM_ENTITY_PROC_1(OBJ) (SCM_ENTITY (OBJ)->proc1)
#define SCM_ENTITY_PROC_2(OBJ) (SCM_ENTITY (OBJ)->proc2)
#define SCM_ENTITY_PROC_3(OBJ) (SCM_ENTITY (OBJ)->proc3)

#define SCM_METACLASS_STANDARD_LAYOUT "pwpwpw"
struct scm_metaclass_standard {
  SCM layout;
  SCM vcell;
  SCM vtable;
  SCM print;
  SCM direct_supers;
  SCM direct_slots;
};

#define SCM_ENTITY_LAYOUT "pwpwpwpw"
typedef struct scm_entity {
  SCM proc0;
  SCM proc1;
  SCM proc2;
  SCM proc3;
} scm_entity;

extern SCM scm_metaclass_standard;
extern SCM *scm_entity_vtable;

extern void scm_init_objects SCM_P ((void));

#endif /* OBJECTSH */
