#ifndef SCM_SRFI_1_H
#define SCM_SRFI_1_H
/* srfi-1.h --- SRFI-1 procedures for Guile
 *
 * 	Copyright (C) 2002 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives
 * permission for additional uses of the text contained in its release
 * of GUILE.
 *
 * The exception is that, if you link the GUILE library with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public
 * License.  Your use of that executable is in no way restricted on
 * account of linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public
 * License.
 *
 * This exception applies only to the code released by the Free
 * Software Foundation under the name GUILE.  If you copy code from
 * other Free Software Foundation releases into a copy of GUILE, as
 * the General Public License permits, the exception does not apply to
 * the code that you add in this way.  To avoid misleading anyone as
 * to the status of such modified files, you must delete this
 * exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


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

SCM_SRFI1_API SCM scm_srfi1_map (SCM proc, SCM arg1, SCM args);
SCM_SRFI1_API SCM scm_srfi1_for_each (SCM proc, SCM arg1, SCM args);
SCM_SRFI1_API SCM scm_srfi1_member (SCM obj, SCM ls, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_assoc (SCM key, SCM alist, SCM pred);

SCM_SRFI1_API void scm_init_srfi_1 (void);

#endif /* SCM_SRFI_1_H */
