/* classes: h_files */

#ifndef PRINTH
#define PRINTH
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


#include "libguile/__scm.h"

#include "libguile/options.h"

extern scm_option scm_print_opts[];

#define SCM_PRINT_CLOSURE	((SCM) scm_print_opts[0].val)
#define SCM_PRINT_SOURCE_P	((int) scm_print_opts[1].val)
#define SCM_N_PRINT_OPTIONS 2

/* State information passed around during printing.
 */
#define RESET_PRINT_STATE(pstate) \
{ \
  pstate->list_offset = 0; \
  pstate->top = 0; \
}

#define SCM_WRITINGP(pstate) ((pstate)->writingp)
#define SCM_SET_WRITINGP(pstate, x) { (pstate)->writingp = (x); }

#define SCM_PRINT_STATE_LAYOUT "sruwuwuwuwpwuwuwuwpW"
typedef struct scm_print_state {
  SCM handle;			/* Struct handle */
  unsigned long writingp;	/* Writing? */
  unsigned long fancyp;		/* Fancy printing? */
  unsigned long level;		/* Max level */
  unsigned long length;		/* Max number of objects per level */
  SCM hot_ref;			/* Hot reference */
  unsigned long list_offset;
  unsigned long top;		/* Top of reference stack */
  unsigned long ceiling;	/* Max size of reference stack */
  unsigned long n_refs;		/* Size of struct tail array */
  SCM ref_stack[1];		/* Stack of references used during
				   circular reference detection */
} scm_print_state;

#ifdef __STDC__
extern SCM scm_print_options (SCM setting);
extern void scm_intprint (long n, int radix, SCM port);
extern void scm_ipruk (char *hdr, SCM ptr, SCM port);
extern void scm_iprlist (char *hdr, SCM exp, char tlr, SCM port, scm_print_state *pstate);
extern void scm_prin1 (SCM exp, SCM port, int writingp);
extern void scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate);
extern SCM scm_write (SCM obj, SCM port);
extern SCM scm_display (SCM obj, SCM port);
extern SCM scm_newline(SCM port);
extern SCM scm_write_char (SCM chr, SCM port);
extern void scm_init_print (void);

#else /* STDC */
extern SCM scm_print_options ();
extern void scm_intprint ();
extern void scm_ipruk ();
extern void scm_iprlist ();
extern void scm_prin1 ();
extern void scm_iprin1 ();
extern SCM scm_write ();
extern SCM scm_display ();
extern SCM scm_newline();
extern SCM scm_write_char ();
extern void scm_init_print ();

#endif /* STDC */

#endif  /* PRINTH */
