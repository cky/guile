/* classes: h_files */

#ifndef STRUCTH
#define STRUCTH
/*	Copyright (C) 1995, 1997 Free Software Foundation, Inc.
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
#include "libguile/print.h"



/* Number of words with negative index */
#define scm_struct_n_extra_words 3
#define scm_struct_entity_n_extra_words 8

/* These are how the initial words of a vtable are allocated. */
#define scm_struct_i_setter	-8 /* Setter */
#define scm_struct_i_proc	-7 /* Optional procedure slots */
#define scm_struct_i_ptr	-3 /* start of block (see alloc_struct) */
#define scm_struct_i_n_words	-2 /* How many words allocated to this struct? */
#define scm_struct_i_tag	-1 /* A unique tag for this type.. */
#define scm_struct_i_flags	-1 /* Upper 8 bits used as flags */
#define scm_vtable_index_layout  0 /* A symbol describing the physical arrangement of this type. */
#define scm_vtable_index_vcell   1 /* An opaque word, managed by the garbage collector.  */
#define scm_vtable_index_vtable  2 /* A pointer to the handle for this vtable. */
#define scm_vtable_index_printer 3 /* A printer for this struct type. */
#define scm_vtable_offset_user   4 /* Where do user fields start? */

#define SCM_STRUCTF_MASK   (0xFF << 24)
#define SCM_STRUCTF_ENTITY (1L << 30) /* Indicates presence of proc slots */
#define SCM_STRUCTF_LIGHT  (1L << 31) /* Light representation
					 (no hidden words) */

#define SCM_STRUCTP(X)  		(SCM_TYP3(X) == scm_tc3_cons_gloc)
#define SCM_STRUCT_DATA(X) 		((SCM*)(SCM_CDR(X)))
#define SCM_STRUCT_VTABLE_DATA(X) 	((SCM *)(SCM_CAR(X) - 1))
#define SCM_STRUCT_LAYOUT(X) 		(SCM_STRUCT_VTABLE_DATA(X)[scm_vtable_index_layout])
#define SCM_STRUCT_VTABLE(X) 		(SCM_STRUCT_VTABLE_DATA(X)[scm_vtable_index_vtable])
#define SCM_STRUCT_PRINTER(X) 		(SCM_STRUCT_VTABLE_DATA(X)[scm_vtable_index_printer])
/* Efficiency is important in the following macro, since it's used in GC */
#define SCM_LAYOUT_TAILP(X)		(((X) & 32) == 0) /* R, W or O */

#define SCM_STRUCT_TABLE_NAME(X) SCM_CAR (X)
#define SCM_SET_STRUCT_TABLE_NAME(X, NAME) SCM_SETCAR (X, NAME)
#define SCM_STRUCT_TABLE_CLASS(X) SCM_CDR (X)
#define SCM_SET_STRUCT_TABLE_CLASS(X, CLASS) SCM_SETCDR (X, CLASS)
extern SCM scm_struct_table;



extern SCM *scm_alloc_struct SCM_P ((int n_words, int n_extra, char *who));
extern void scm_struct_init SCM_P ((SCM handle, int tail_elts, SCM inits));
extern SCM scm_make_struct_layout SCM_P ((SCM fields));
extern SCM scm_struct_p SCM_P ((SCM x));
extern SCM scm_struct_vtable_p SCM_P ((SCM x));
extern SCM scm_make_struct SCM_P ((SCM vtable, SCM tail_array_size, SCM init));
extern SCM scm_make_vtable_vtable SCM_P ((SCM extra_fields, SCM tail_array_size, SCM init));
extern SCM scm_struct_ref SCM_P ((SCM handle, SCM pos));
extern SCM scm_struct_set_x SCM_P ((SCM handle, SCM pos, SCM val));
extern SCM scm_struct_vtable SCM_P ((SCM handle));
extern SCM scm_struct_vtable_tag SCM_P ((SCM handle));
extern unsigned int scm_struct_ihashq SCM_P ((SCM obj, unsigned int n));
extern SCM scm_struct_create_handle SCM_P ((SCM obj));
extern SCM scm_struct_vtable_name SCM_P ((SCM vtable));
extern SCM scm_set_struct_vtable_name_x SCM_P ((SCM vtable, SCM name));
extern void scm_print_struct SCM_P ((SCM exp, SCM port, scm_print_state *));
extern void scm_init_struct SCM_P ((void));

#endif  /* STRUCTH */
