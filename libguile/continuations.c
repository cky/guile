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



/* {Continuations}
 */

static char s_cont[] = "continuation";

#ifdef __STDC__
SCM 
scm_make_cont (SCM * answer)
#else
SCM 
scm_make_cont (answer)
     SCM * answer;
#endif
{
  long j;
  SCM cont;

#ifdef CHEAP_CONTINUATIONS
  SCM_NEWCELL (cont);
  *answer = cont;
  SCM_DEFER_INTS;
  SCM_SETJMPBUF (cont, scm_must_malloc ((long) sizeof (regs), s_cont));
  SCM_CAR (cont) = scm_tc7_contin;
  SCM_DYNENV (cont) = dynwinds;
  SCM_THROW_VALUE = SCM_EOL;
  SCM_BASE (cont) = SCM_BASE (rootcont);
  SCM_SEQ (cont) = SCM_SEQ (rootcont);
  SCM_ALLOW_INTS;
#else
  register SCM_STACKITEM *src, *dst;

  {
    SCM winds;

    for (winds = scm_dynwinds; winds != SCM_EOL; winds = SCM_CDR (winds))
      {
#if 0
	if (SCM_INUMP (SCM_CAR (winds)))
	  {
	    scm_relocate_chunk_to_heap (SCM_CAR (winds));
	  }
#endif
      }
  }

  SCM_NEWCELL (cont);
  *answer = cont;
  SCM_DEFER_INTS;
  SCM_FLUSH_REGISTER_WINDOWS;
  j = scm_stack_size (SCM_BASE (scm_rootcont));
  SCM_SETJMPBUF (cont,
	     scm_must_malloc ((long) (sizeof (regs) + j * sizeof (SCM_STACKITEM)),
			      s_cont));
  SCM_SETLENGTH (cont, j, scm_tc7_contin);
  SCM_DYNENV (cont) = scm_dynwinds;
  SCM_THROW_VALUE (cont) = SCM_EOL;
  src = SCM_BASE (cont) = SCM_BASE (scm_rootcont);
  SCM_SEQ (cont) = SCM_SEQ (scm_rootcont);
  SCM_ALLOW_INTS;
#ifndef SCM_STACK_GROWS_UP
  src -= SCM_LENGTH (cont);
#endif /* ndef SCM_STACK_GROWS_UP */
  dst = (SCM_STACKITEM *) (SCM_CHARS (cont) + sizeof (regs));
  for (j = SCM_LENGTH (cont); 0 <= --j;)
    *dst++ = *src++;
#endif /* def CHEAP_CONTINUATIONS */
#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (cont) = last_debug_info_frame;
#endif
  return cont;
}


void scm_dynthrow SCM_P ((SCM *a));

/* Grow the stack so that there is room */
/* to copy in the continuation.  Then */
#ifndef CHEAP_CONTINUATIONS
#ifdef __STDC__
static void 
grow_throw (SCM *a)
#else
static void 
grow_throw (a)
     SCM *a;
#endif
{				/* retry the throw. */
  SCM growth[100];
  growth[0] = a[0];
  growth[1] = a[1];
  growth[2] = a[2] + 1;
  growth[3] = (SCM) a;
  scm_dynthrow (growth);
}
#endif /* ndef CHEAP_CONTINUATIONS */

#ifdef __STDC__
void 
scm_dynthrow (SCM *a)
#else
void 
scm_dynthrow (a)
     SCM *a;
#endif
{
  SCM cont = a[0], val = a[1];
#ifndef CHEAP_CONTINUATIONS
  register long j;
  register SCM_STACKITEM *src, *dst = SCM_BASE (scm_rootcont);
#ifdef SCM_STACK_GROWS_UP
  if (a[2] && (a - ((SCM *) a[3]) < 100))
#else
  if (a[2] && (((SCM *) a[3]) - a < 100))
#endif
    fputs ("grow_throw: check if SCM growth[100]; being optimized out\n",
	   stderr);
  /* if (a[2]) fprintf(stderr, " ct = %ld, dist = %ld\n",
			  a[2], (((SCM *)a[3]) - a)); */
#ifdef SCM_STACK_GROWS_UP
  if (SCM_PTR_GE (dst + SCM_LENGTH (cont), (SCM_STACKITEM *) & a))
    grow_throw (a);
#else
  dst -= SCM_LENGTH (cont);
  if (SCM_PTR_LE (dst, (SCM_STACKITEM *) & a))
    grow_throw (a);
#endif /* def SCM_STACK_GROWS_UP */
  SCM_FLUSH_REGISTER_WINDOWS;
  src = (SCM_STACKITEM *) (SCM_CHARS (cont) + sizeof (regs));
  for (j = SCM_LENGTH (cont); 0 <= --j;)
    *dst++ = *src++;
#ifdef sparc			/* clear out stack up to this stackframe */
  /* maybe this would help, maybe not */
/*	bzero((void *)&a, sizeof(SCM_STACKITEM) * (((SCM_STACKITEM *)&a) -
					       (dst - SCM_LENGTH(cont)))) */
#endif
#endif /* ndef CHEAP_CONTINUATIONS */
#ifdef DEBUG_EXTENSIONS
  last_debug_info_frame = SCM_DFRAME (cont);
#endif
  SCM_THROW_VALUE(cont) = val;
  longjmp (SCM_JMPBUF (cont), 1);
}

#ifdef __STDC__
SCM
scm_call_continuation (SCM cont, SCM val)
#else
SCM
scm_call_continuation (cont, val)
     SCM cont;
     SCM val;
#endif
{
  SCM a[3];
  a[0] = cont;
  a[1] = val;
  a[2] = 0;
  if (   (SCM_SEQ (cont) != SCM_SEQ (scm_rootcont))
      || (SCM_BASE (cont) != SCM_BASE (scm_rootcont)))  /* base compare not needed */
    scm_wta (cont, "continuation from wrong top level", s_cont);
  
  scm_dowinds (SCM_DYNENV (cont),
	       scm_ilength (scm_dynwinds) - scm_ilength (SCM_DYNENV (cont)));
  
  scm_dynthrow (a);
  return SCM_UNSPECIFIED; /* not reached */
}


#ifdef __STDC__
void
scm_init_continuations (void)
#else
void
scm_init_continuations ()
#endif
{
#include "continuations.x"
}

