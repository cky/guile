/*	Copyright (C) 1995,1996,1997, 2000, 2001 Free Software Foundation, Inc.
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
#if SCM_STACK_GROWS_UP
  return &stack - start;
#else
  return start - &stack;
#endif /* SCM_STACK_GROWS_UP */
}


void 
scm_stack_report ()
{
  SCM_STACKITEM stack;
  scm_uintprint (scm_stack_size (SCM_BASE (scm_rootcont)) * sizeof (SCM_STACKITEM),
		16, scm_cur_errp);
  scm_puts (" of stack: 0x", scm_cur_errp);
  scm_uintprint ((scm_t_bits) SCM_BASE (scm_rootcont), 16, scm_cur_errp);
  scm_puts (" - 0x", scm_cur_errp);
  scm_uintprint ((scm_t_bits) &stack, 16, scm_cur_errp);
  scm_puts ("\n", scm_cur_errp);
}




void
scm_init_stackchk ()
{
#include "libguile/stackchk.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
