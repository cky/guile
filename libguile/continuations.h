/* classes: h_files */

#ifndef CONTINUATIONSH
#define CONTINUATIONSH
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

#include <libguile/__scm.h>



typedef struct 
{
  SCM throw_value;
  jmp_buf jmpbuf;
  SCM dynenv;
  SCM_STACKITEM *base;
  unsigned long seq;

#ifdef DEBUG_EXTENSIONS
  struct scm_debug_frame *dframe;
#endif
} regs;

#define SCM_JMPBUF(x) (((regs *)SCM_CHARS(x))->jmpbuf)
#define SCM_SETJMPBUF SCM_SETCDR
#define SCM_DYNENV(x) (((regs *)SCM_CHARS(x))->dynenv)
#define SCM_THROW_VALUE(x) (((regs *)SCM_CHARS(x))->throw_value)
#define SCM_BASE(x) (((regs *)SCM_CHARS(x))->base)
#define SCM_SEQ(x) (((regs *)SCM_CHARS(x))->seq)
#define SCM_DFRAME(x) (((regs *)SCM_CHARS(x))->dframe)



#ifdef __STDC__
extern SCM scm_make_cont (SCM * answer);
extern void scm_dynthrow (SCM *a);
extern SCM scm_call_continuation (SCM cont, SCM val);
extern void scm_init_continuations (void);

#else /* STDC */
extern SCM scm_make_cont ();
extern void scm_dynthrow ();
extern SCM scm_call_continuation ();
extern void scm_init_continuations ();

#endif /* STDC */
#endif  /* CONTINUATIONSH */
