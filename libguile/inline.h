/* classes: h_files */

#ifndef SCM_INLINE_H
#define SCM_INLINE_H

/* Copyright (C) 2001 Free Software Foundation, Inc.
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

/* This file is for inline functions.  On platforms that don't support
   inlining functions, they are turned into ordinary functions.  See
   "inline.c".
*/

#include "libguile/pairs.h"
#include "libguile/gc.h"

#ifdef HAVE_INLINE

static inline SCM
scm_alloc_cell (scm_t_bits car, scm_t_bits cdr)
{
  SCM z;

#ifdef GUILE_DEBUG_FREELIST
  scm_newcell_count++;
  if (scm_debug_check_freelist)
    {
      scm_check_freelist (scm_freelist);
      scm_gc();
    }
#endif

  if (SCM_NULLP (scm_freelist))
    {
      z = scm_gc_for_newcell (&scm_master_freelist, &scm_freelist);
    }
  else
    {
      z = scm_freelist;
      scm_freelist = SCM_FREE_CELL_CDR (scm_freelist);
    }

  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't for
     cooperating threads, but it might be important when we get true
     preemptive threads.
  */
  SCM_SET_CELL_WORD_1 (z, cdr);
  SCM_SET_CELL_WORD_0 (z, car);

#ifdef USE_THREADS
#ifndef USE_COOP_THREADS
  /* When we are using non-cooperating threads, we might need to make
     sure that the initial values for the slots are protected until
     the cell is completely initialized.
  */
#error review me
  scm_remember_upto_here_1 (SCM_PACK (cdr));
#endif
#endif

  return z;
}

static inline SCM
scm_alloc_double_cell (scm_t_bits car, scm_t_bits cbr,
		       scm_t_bits ccr, scm_t_bits cdr)
{
  SCM z;

#ifdef GUILE_DEBUG_FREELIST
  scm_newcell2_count++;
  if (scm_debug_check_freelist)
    {
      scm_check_freelist (scm_freelist2);
      scm_gc();
    }
#endif

  if (SCM_NULLP (scm_freelist2))
    {
      z = scm_gc_for_newcell (&scm_master_freelist2, &scm_freelist2);
    }
  else
    {
      z = scm_freelist2;
      scm_freelist2 = SCM_FREE_CELL_CDR (scm_freelist2);
    }

  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't for
     cooperating threads, but it might be important when we get true
     preemptive threads.
  */
  SCM_SET_CELL_WORD_1 (z, cbr);
  SCM_SET_CELL_WORD_2 (z, ccr);
  SCM_SET_CELL_WORD_3 (z, cdr);
  SCM_SET_CELL_WORD_0 (z, car);

#ifdef USE_THREADS
#ifndef USE_COOP_THREADS
  /* When we are using non-cooperating threads, we might need to make
     sure that the initial values for the slots are protected until
     the cell is completely initialized.
  */
#error review me
  scm_remember_upto_here_3 (SCM_PACK (cbr), SCM_PACK (ccr), SCM_PACK (cdr));
#endif
#endif

  return z;
}

#else /* !HAVE_INLINE */

SCM_API SCM scm_alloc_cell (scm_t_bits car, scm_t_bits cdr);
SCM_API SCM scm_alloc_double_cell (scm_t_bits car, scm_t_bits cbr,
				   scm_t_bits ccr, scm_t_bits cdr);

#endif

#endif
