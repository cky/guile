/* classes: h_files */

#ifndef DEBUGH
#define DEBUGH
/*	Copyright (C) 1995,1996 Mikael Djurfeldt
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
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
 */


#include "options.h"
#include "__scm.h"



/*
 * Here comes some definitions for the debugging machinery.
 * It might seem strange to represent debug flags as ints,
 * but consider that any particular piece of code is normally
 * only interested in one flag at a time.  This is then
 * the most efficient representation.
 */

/* {Options}
 */

/* scm_debug_opts and scm_evaluator_trap_table are defined in eval.c.
 */

extern scm_option scm_debug_opts[];

#define SCM_CHEAPTRAPS_P	scm_debug_opts[0].val
#define SCM_BREAKPOINTS_P	scm_debug_opts[1].val
#define SCM_TRACE_P		scm_debug_opts[2].val
#define SCM_REC_PROCNAMES_P	scm_debug_opts[3].val
#define SCM_BACKWARDS_P		scm_debug_opts[4].val
#define SCM_N_FRAMES		scm_debug_opts[5].val
#define SCM_BACKTRACE_DEPTH	scm_debug_opts[6].val
#define SCM_BACKTRACE_P		scm_debug_opts[7].val
#define SCM_DEVAL_P		scm_debug_opts[8].val
#define SCM_STACK_LIMIT		scm_debug_opts[9].val
#define SCM_N_DEBUG_OPTIONS 10

extern scm_option scm_evaluator_trap_table[];

#define SCM_ENTER_FRAME_P      scm_evaluator_trap_table[0].val
#define SCM_APPLY_FRAME_P      scm_evaluator_trap_table[1].val
#define SCM_EXIT_FRAME_P       scm_evaluator_trap_table[2].val
#define SCM_N_EVALUATOR_TRAPS 3

#ifdef __STDC__
extern SCM (*scm_ceval_ptr) (SCM exp, SCM env);
#else
extern SCM (*scm_ceval_ptr) ();
#endif
extern int scm_debug_mode;
extern int scm_check_entry_p, scm_check_apply_p, scm_check_exit_p;

#define CHECK_ENTRY      scm_check_entry_p
#define CHECK_APPLY	 scm_check_apply_p
#define CHECK_EXIT       scm_check_exit_p

#define SCM_RESET_DEBUG_MODE \
{\
  if (SCM_ENTER_FRAME_P || SCM_BREAKPOINTS_P) CHECK_ENTRY = 1;\
  if (SCM_APPLY_FRAME_P || SCM_TRACE_P) CHECK_APPLY = 1;\
  if (SCM_EXIT_FRAME_P || SCM_TRACE_P) CHECK_EXIT = 1;\
  scm_debug_mode = SCM_DEVAL_P || SCM_BACKTRACE_P || CHECK_ENTRY || CHECK_APPLY || CHECK_EXIT;\
  scm_ceval_ptr = scm_debug_mode ? scm_deval : scm_ceval;\
}


/* {Evaluator}
 */

typedef union scm_debug_info
{
  struct { SCM exp, env; } e;
  struct { SCM proc, args; } a;
} scm_debug_info;

extern int scm_debug_eframe_size;

typedef struct scm_debug_frame
{
  struct scm_debug_frame *prev;
  long status;
  scm_debug_info vect[1];
} scm_debug_frame;

extern scm_debug_frame *last_debug_info_frame;

#define SCM_TAILREC     (1L << 10)
#define SCM_TRACED_FRAME (1L << 9)
#define SCM_APPLYFRAME  (1L << 8)
#define SCM_ARGS_READY   (1L << 7)
#define SCM_DOVERFLOW   (1L << 6)
#define SCM_MAX_FRAME_SIZE 63 /* also used as a mask for the size field */

#define SCM_EVALFRAMEP(x) (((x).status & SCM_APPLYFRAME) == 0)
#define SCM_APPLYFRAMEP(x) (((x).status & SCM_APPLYFRAME) != 0)
#define SCM_OVERFLOWP(x) (((x).status & SCM_DOVERFLOW) != 0)
#define SCM_ARGS_READY_P(x) (((x).status & SCM_ARGS_READY) != 0)
#define SCM_TRACED_FRAME_P(x) (((x).status & SCM_TRACED_FRAME) != 0)
#define SCM_TAILRECP(x) (((x).status & SCM_TAILREC) != 0)
#define SCM_SET_OVERFLOW(x) ((x).status |= SCM_DOVERFLOW)
#define SCM_SET_ARGSREADY(x) ((x).status |= SCM_ARGS_READY)
#define SCM_CLEAR_ARGSREADY(x) ((x).status &= ~SCM_ARGS_READY)
#define SCM_SET_TRACED_FRAME(x) ((x).status |= SCM_TRACED_FRAME)
#define SCM_CLEAR_TRACED_FRAME(x) ((x).status &= ~SCM_TRACED_FRAME)
#define SCM_SET_TAILREC(x) ((x).status |= SCM_TAILREC)

#define SCM_DEBUGGINGP scm_debug_mode
#define SCM_DSIDEVAL(x, env) if NIMP(x) scm_deval((x), (env))

/* {Memoized Source}
 */

extern long scm_tc16_memoized;

#define SCM_MEMOIZEDP(x) (scm_tc16_memoized == SCM_TYP16 (x))
#define SCM_MEMOEXP(x) SCM_CAR (SCM_CDR (x))
#define SCM_MEMOENV(x) SCM_CDR (SCM_CDR (x))



#ifdef __STDC__
extern SCM scm_unmemoize (SCM memoized);
extern SCM scm_make_debugobj (scm_debug_frame* debug);
extern void scm_init_debug (void);
#else
extern SCM scm_unmemoize ();
extern SCM scm_make_debugobj ();
extern void scm_init_debug ();
#endif

#endif /* DEBUGH */
