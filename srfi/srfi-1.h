#ifndef SCM_SRFI_1_H
#define SCM_SRFI_1_H
/* srfi-1.h --- SRFI-1 procedures for Guile
 *
 * 	Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */


/* SCM_SRFI1_API is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library in the Win32 port. */

#if defined (SCM_SRFI1_IMPORT)
# define SCM_SRFI1_API __declspec (dllimport) extern
#elif defined (SCM_SRFI1_EXPORT) || defined (DLL_EXPORT)
# define SCM_SRFI1_API __declspec (dllexport) extern
#else
# define SCM_SRFI1_API extern
#endif

SCM_SRFI1_API SCM scm_srfi1_count (SCM pred, SCM list1, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_delete (SCM x, SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_delete_x (SCM x, SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_delete_duplicates (SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_delete_duplicates_x (SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_drop_right (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_filter_map (SCM proc, SCM list1, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_find (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_find_tail (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_length_plus (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_list_copy (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_map (SCM proc, SCM arg1, SCM args);
SCM_SRFI1_API SCM scm_srfi1_for_each (SCM proc, SCM arg1, SCM args);
SCM_SRFI1_API SCM scm_srfi1_member (SCM obj, SCM ls, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_assoc (SCM key, SCM alist, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_partition (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_partition_x (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_remove (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_remove_x (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_split_at (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_split_at_x (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_take_right (SCM lst, SCM n);

SCM_SRFI1_API void scm_init_srfi_1 (void);

#endif /* SCM_SRFI_1_H */
