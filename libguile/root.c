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
#include "stackchk.h"

#include "root.h"


SCM scm_sys_protects[SCM_NUM_PROTECTS];
struct scm_root_state the_scm_root;
struct scm_root_state * scm_root = &the_scm_root;







/* Call thunk(closure) underneath a top-level error handler.
 * If an error occurs, pass the exitval through err_filter and return it.
 * If no error occurs, return the value of thunk.
 */


#ifdef _UNICOS
typedef int setjmp_type;
#else
typedef long setjmp_type;
#endif


#ifdef __STDC__
SCM
scm_call_catching_errors (SCM (*thunk)(), SCM (*err_filter)(), void * closure)
#else
SCM
scm_call_catching_errors (thunk, err_filter, closure)
     SCM (*thunk)();
     SCM (*err_filter)();
     void * closure;
#endif
{
  SCM answer;
  setjmp_type i;
#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (scm_rootcont) = last_debug_info_frame;
#endif
  i = setjmp (SCM_JMPBUF (scm_rootcont));
#ifdef STACK_CHECKING
  scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
#endif
  if (!i)
    {
      scm_gc_heap_lock = 0;
      answer = thunk (closure);
    }
  else
    {
      scm_gc_heap_lock = 1;
      answer = err_filter (scm_exitval, closure);
    }
  return answer;
}





