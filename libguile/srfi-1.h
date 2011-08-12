/* srfi-1.h --- SRFI-1 procedures for Guile
 *
 * 	Copyright (C) 2002, 2003, 2005, 2006, 2010, 2011 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */


#ifndef SCM_SRFI_1_H
#define SCM_SRFI_1_H

#include "libguile/__scm.h"

SCM_INTERNAL SCM scm_srfi1_append_reverse (SCM revhead, SCM tail);
SCM_INTERNAL SCM scm_srfi1_append_reverse_x (SCM revhead, SCM tail);
SCM_INTERNAL SCM scm_srfi1_concatenate (SCM lstlst);
SCM_INTERNAL SCM scm_srfi1_concatenate_x (SCM lstlst);
SCM_INTERNAL SCM scm_srfi1_count (SCM pred, SCM list1, SCM rest);
SCM_INTERNAL SCM scm_srfi1_delete (SCM x, SCM lst, SCM pred);
SCM_INTERNAL SCM scm_srfi1_delete_x (SCM x, SCM lst, SCM pred);
SCM_INTERNAL SCM scm_srfi1_delete_duplicates (SCM lst, SCM pred);
SCM_INTERNAL SCM scm_srfi1_delete_duplicates_x (SCM lst, SCM pred);
SCM_INTERNAL SCM scm_srfi1_find (SCM pred, SCM lst);
SCM_INTERNAL SCM scm_srfi1_find_tail (SCM pred, SCM lst);
SCM_INTERNAL SCM scm_srfi1_length_plus (SCM lst);
SCM_INTERNAL SCM scm_srfi1_lset_difference_x (SCM equal, SCM lst, SCM rest);
SCM_INTERNAL SCM scm_srfi1_list_copy (SCM lst);
SCM_INTERNAL SCM scm_srfi1_assoc (SCM key, SCM alist, SCM pred);
SCM_INTERNAL SCM scm_srfi1_partition (SCM pred, SCM list);
SCM_INTERNAL SCM scm_srfi1_partition_x (SCM pred, SCM list);
SCM_INTERNAL SCM scm_srfi1_remove (SCM pred, SCM list);
SCM_INTERNAL SCM scm_srfi1_remove_x (SCM pred, SCM list);

SCM_INTERNAL void scm_register_srfi_1 (void);
SCM_INTERNAL void scm_init_srfi_1 (void);

#endif /* SCM_SRFI_1_H */
