/* classes: h_files */

#ifndef STACKCHKH
#define STACKCHKH
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

#include "__scm.h"



#ifdef SCM_STACK_LIMIT
# define SCM_STACK_CHECK
# ifdef SCM_STACK_GROWS_UP
#  define SCM_STACK_OVERFLOW_P(s) (s - SCM_BASE (rootcont) > SCM_STACK_LIMIT * sizeof (SCM_STACKITEM))
# else
#  define SCM_STACK_OVERFLOW_P(s) (SCM_BASE (rootcont) - s > SCM_STACK_LIMIT * sizeof (SCM_STACKITEM))
# endif
# define SCM_CHECK_STACK\
    {\
       SCM_STACKITEM stack;\
       if (SCM_STACK_OVERFLOW_P (&stack) && scm_check_stack_p)\
	 scm_report_stack_overflow ();\
    }
#else
# define SCM_CHECK_STACK /**/
#endif /* def SCM_STACK_LIMIT */


extern int scm_check_stack_p;


#ifdef __STDC__
extern long scm_stack_size (SCM_STACKITEM *start);
extern void scm_stack_report (void);
extern void scm_init_stackchk (void);

#else /* STDC */
extern long scm_stack_size ();
extern void scm_stack_report ();
extern void scm_init_stackchk ();

#endif /* STDC */








#endif  /* STACKCHKH */
