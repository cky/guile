/*	Copyright (C) 1995,1996,1998, 2000 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/root.h"
#include "libguile/stackchk.h"
#ifdef DEBUG_EXTENSIONS
#include "libguile/debug.h"
#endif
#include "libguile/dynwind.h"

#include "libguile/continuations.h"



/* {Continuations}
 */

static char s_cont[] = "continuation";

static void scm_dynthrow (SCM, SCM);


#ifndef CHEAP_CONTINUATIONS


SCM 
scm_make_cont (SCM *answer)
{
  long j;
  SCM cont;
  SCM_STACKITEM * src;
  SCM_STACKITEM * dst;

  SCM_NEWCELL (cont);
  *answer = cont;
  SCM_ENTER_A_SECTION;
  SCM_FLUSH_REGISTER_WINDOWS;
  j = scm_stack_size (SCM_BASE (scm_rootcont));
  SCM_SET_CONTREGS (cont,
		    scm_must_malloc (sizeof (scm_contregs)
				     + j * sizeof (SCM_STACKITEM),
				     s_cont));
  SCM_DYNENV (cont) = scm_dynwinds;
  SCM_THROW_VALUE (cont) = SCM_EOL;
  src = SCM_BASE (cont) = SCM_BASE (scm_rootcont);
  SCM_SEQ (cont) = SCM_SEQ (scm_rootcont);
  SCM_SETLENGTH (cont, j, scm_tc7_contin);
  SCM_EXIT_A_SECTION;
#ifndef SCM_STACK_GROWS_UP
  src -= SCM_CONTINUATION_LENGTH (cont);
#endif /* ndef SCM_STACK_GROWS_UP */
  dst = (SCM_STACKITEM *) ((char *) SCM_CONTREGS (cont) + sizeof (scm_contregs));

  /* memcpy should be safe:  src and dst will never overlap */
  memcpy (dst, src, sizeof (SCM_STACKITEM) * SCM_CONTINUATION_LENGTH (cont));

#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (cont) = scm_last_debug_frame;
#endif

  return cont;
}


/* Grow the stack by a fixed amount to provide space to copy in the
 * continuation.  Possibly this function has to be called several times
 * recursively before enough space is available.  Make sure the compiler does
 * not optimize the growth array away by storing it's address into a global
 * variable.
 */

scm_bits_t scm_i_dummy;

static void 
grow_stack (SCM cont, SCM val)
{
  scm_bits_t growth[100];

  scm_i_dummy = (scm_bits_t) growth;
  scm_dynthrow (cont, val);
}


/* Copy the continuation stack into the current stack.  Calling functions from
 * within this function is safe, since only stack frames below this function's
 * own frame are overwritten.  Thus, memcpy can be used for best performance.
 */
static void
copy_stack_and_call (SCM cont, SCM val, 
		     SCM_STACKITEM * src, SCM_STACKITEM * dst)
{
  /* memcpy should be safe:  src and dst will never overlap */
  memcpy (dst, src, sizeof (SCM_STACKITEM) * SCM_CONTINUATION_LENGTH (cont));

#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = SCM_DFRAME (cont);
#endif

  SCM_THROW_VALUE (cont) = val;
  longjmp (SCM_JMPBUF (cont), 1);
}


/* Call grow_stack until the stack space is large enough, then, as the current
 * stack frame might get overwritten, let copy_stack_and_call perform the
 * actual copying and continuation calling.
 */
static void 
scm_dynthrow (SCM cont, SCM val)
{
  SCM_STACKITEM * src;
  SCM_STACKITEM * dst = SCM_BASE (scm_rootcont);
  SCM_STACKITEM stack_top_element;

#ifdef SCM_STACK_GROWS_UP
  if (SCM_PTR_GE (dst + SCM_CONTINUATION_LENGTH (cont), & stack_top_element))
    grow_stack (cont, val);
#else
  dst -= SCM_CONTINUATION_LENGTH (cont);
  if (SCM_PTR_LE (dst, & stack_top_element))
    grow_stack (cont, val);
#endif /* def SCM_STACK_GROWS_UP */
  SCM_FLUSH_REGISTER_WINDOWS;
  src = (SCM_STACKITEM *) ((char *) SCM_CONTREGS (cont) + sizeof (scm_contregs));
  copy_stack_and_call (cont, val, src, dst);
}


#else /* ifndef CHEAP_CONTINUATIONS */

/* Dirk:FIXME:: It seems that nobody has ever tried to use this code, since it
 * contains syntactic errors and thus would not have compiled anyway.
 */


SCM 
scm_make_cont (SCM *answer)
{
  SCM cont;

  SCM_NEWCELL (cont);
  *answer = cont;
  SCM_ENTER_A_SECTION;
  SCM_SET_CONTREGS (cont, scm_must_malloc (sizeof (scm_contregs), s_cont));
  SCM_DYNENV (cont) = scm_dynwinds;
  SCM_THROW_VALUE = SCM_EOL;
  SCM_BASE (cont) = SCM_BASE (rootcont);
  SCM_SEQ (cont) = SCM_SEQ (rootcont);
  SCM_SETCAR (cont, scm_tc7_contin);
  SCM_EXIT_A_SECTION;

#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (cont) = scm_last_debug_frame;
#endif

  return cont;
}


static void
scm_dynthrow (SCM cont, SCM val)
{
#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = SCM_DFRAME (cont);
#endif
  SCM_THROW_VALUE (cont) = val;
  longjmp (SCM_JMPBUF (cont), 1);
}


#endif


SCM
scm_call_continuation (SCM cont, SCM val)
{
  if ((SCM_SEQ (cont) != SCM_SEQ (scm_rootcont))
      || (SCM_BASE (cont) != SCM_BASE (scm_rootcont)))  
    /* base compare not needed */
    scm_wta (cont, "continuation from wrong top level", s_cont);
  
  scm_dowinds (SCM_DYNENV (cont),
	       scm_ilength (scm_dynwinds) - scm_ilength (SCM_DYNENV (cont)));
  
  scm_dynthrow (cont, val);
  return SCM_UNSPECIFIED; /* not reached */
}


void
scm_init_continuations ()
{
#include "libguile/continuations.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
