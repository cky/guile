/*	Copyright (C) 1995,1996,1997, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/root.h"

#include "libguile/stackchk.h"


/* {Stack Checking}
 */

#ifdef STACK_CHECKING
int scm_stack_checking_enabled_p;

SCM_SYMBOL (scm_stack_overflow_key, "stack-overflow");

void
scm_report_stack_overflow ()
{
  scm_stack_checking_enabled_p = 0;
  scm_error (scm_stack_overflow_key,
	     NULL,
	     "Stack overflow",
	     SCM_BOOL_F,
	     SCM_BOOL_F);
}

#endif

long 
scm_stack_size (SCM_STACKITEM *start)
{
  SCM_STACKITEM stack;
#ifdef SCM_STACK_GROWS_UP
  return &stack - start;
#else
  return start - &stack;
#endif /* def SCM_STACK_GROWS_UP */
}


void 
scm_stack_report ()
{
  SCM_STACKITEM stack;
  scm_intprint (scm_stack_size (SCM_BASE (scm_rootcont)) * sizeof (SCM_STACKITEM),
		16, scm_cur_errp);
  scm_puts (" of stack: 0x", scm_cur_errp);
  scm_intprint ((long) SCM_BASE (scm_rootcont), 16, scm_cur_errp);
  scm_puts (" - 0x", scm_cur_errp);
  scm_intprint ((long) &stack, 16, scm_cur_errp);
  scm_puts ("\n", scm_cur_errp);
}




void
scm_init_stackchk ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/stackchk.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
