/* classes: h_files */

#ifndef LISTH
#define LISTH
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


#include "libguile/__scm.h"


extern SCM scm_list_head SCM_P ((SCM lst, SCM k));
extern SCM scm_listify SCM_P ((SCM elt, ...));
extern SCM scm_list SCM_P ((SCM objs));
extern SCM scm_null_p SCM_P ((SCM x));
extern SCM scm_list_p SCM_P ((SCM x));
extern long scm_ilength SCM_P ((SCM sx));
extern SCM scm_list_length SCM_P ((SCM x));
extern SCM scm_list_append SCM_P ((SCM args));
extern SCM scm_list_append_x SCM_P ((SCM args));
extern SCM scm_list_reverse SCM_P ((SCM lst));
extern SCM scm_list_reverse_x SCM_P ((SCM lst, SCM newtail));
extern SCM scm_list_ref SCM_P ((SCM lst, SCM k));
extern SCM scm_list_set_x SCM_P ((SCM lst, SCM k, SCM val));
extern SCM scm_list_cdr_set_x SCM_P ((SCM lst, SCM k, SCM val));
extern SCM scm_last_pair SCM_P ((SCM sx));
extern SCM scm_list_tail SCM_P ((SCM lst, SCM k));
extern SCM scm_sloppy_memq SCM_P ((SCM x, SCM lst));
extern SCM scm_sloppy_memv SCM_P ((SCM x, SCM lst));
extern SCM scm_sloppy_member SCM_P ((SCM x, SCM lst));
extern SCM scm_memq SCM_P ((SCM x, SCM lst));
extern SCM scm_memv SCM_P ((SCM x, SCM lst));
extern SCM scm_member SCM_P ((SCM x, SCM lst));
extern SCM scm_delq_x SCM_P ((SCM item, SCM lst));
extern SCM scm_delv_x SCM_P ((SCM item, SCM lst));
extern SCM scm_delete_x SCM_P ((SCM item, SCM lst));
extern SCM scm_list_copy SCM_P ((SCM lst));
extern SCM scm_delq SCM_P ((SCM item, SCM lst));
extern SCM scm_delv SCM_P ((SCM item, SCM lst));
extern SCM scm_delete SCM_P ((SCM item, SCM lst));
extern void scm_init_list SCM_P ((void));

#endif  /* LISTH */
