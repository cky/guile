/* classes: h_files */

#ifndef SCM_DEBUG_H
#define SCM_DEBUG_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001 Free Software Foundation, Inc.
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN */



#include "libguile/__scm.h"

#include "libguile/options.h"


/*
 * Here comes some definitions for the debugging machinery.
 * It might seem strange to represent debug flags as ints,
 * but consider that any particular piece of code is normally
 * only interested in one flag at a time.  This is then
 * the most efficient representation.
 */

/* {Options}
 */

/* scm_debug_opts is  defined in eval.c.
 */

SCM_API scm_t_option scm_debug_opts[];

#define SCM_CHEAPTRAPS_P	scm_debug_opts[0].val
#define SCM_BREAKPOINTS_P	scm_debug_opts[1].val
#define SCM_TRACE_P		scm_debug_opts[2].val
#define SCM_REC_PROCNAMES_P	scm_debug_opts[3].val
#define SCM_BACKWARDS_P		scm_debug_opts[4].val
#define SCM_BACKTRACE_WIDTH   	scm_debug_opts[5].val
#define SCM_BACKTRACE_INDENT   	scm_debug_opts[6].val
#define SCM_N_FRAMES		scm_debug_opts[7].val
#define SCM_BACKTRACE_MAXDEPTH	scm_debug_opts[8].val
#define SCM_BACKTRACE_DEPTH	scm_debug_opts[9].val
#define SCM_BACKTRACE_P		scm_debug_opts[10].val
#define SCM_DEVAL_P		scm_debug_opts[11].val
#define SCM_STACK_LIMIT		scm_debug_opts[12].val
#define SCM_SHOW_FILE_NAME	scm_debug_opts[13].val
#define SCM_N_DEBUG_OPTIONS 14

SCM_API SCM (*scm_ceval_ptr) (SCM exp, SCM env);

SCM_API int scm_debug_mode;
SCM_API int scm_check_entry_p;
SCM_API int scm_check_apply_p;
SCM_API int scm_check_exit_p;

#define CHECK_ENTRY      scm_check_entry_p
#define CHECK_APPLY	 scm_check_apply_p
#define CHECK_EXIT       scm_check_exit_p

#define SCM_RESET_DEBUG_MODE \
do {\
  CHECK_ENTRY = (SCM_ENTER_FRAME_P || SCM_BREAKPOINTS_P)\
    && !SCM_FALSEP (SCM_ENTER_FRAME_HDLR);\
  CHECK_APPLY = (SCM_APPLY_FRAME_P || SCM_TRACE_P)\
    && !SCM_FALSEP (SCM_APPLY_FRAME_HDLR);\
  CHECK_EXIT = (SCM_EXIT_FRAME_P || SCM_TRACE_P)\
    && !SCM_FALSEP (SCM_EXIT_FRAME_HDLR);\
  scm_debug_mode = SCM_DEVAL_P || CHECK_ENTRY || CHECK_APPLY || CHECK_EXIT;\
  scm_ceval_ptr = scm_debug_mode ? scm_deval : scm_ceval;\
} while (0)

/* {Evaluator}
 */

typedef union scm_t_debug_info
{
  struct { SCM exp, env; } e;
  struct { SCM proc, args; } a;
  SCM id;
} scm_t_debug_info;

SCM_API long scm_debug_eframe_size;

typedef struct scm_t_debug_frame
{
  struct scm_t_debug_frame *prev;
  long status;
  scm_t_debug_info *vect;
  scm_t_debug_info *info;
} scm_t_debug_frame;

#ifndef USE_THREADS
SCM_API scm_t_debug_frame *scm_last_debug_frame;
#endif

#define SCM_EVALFRAME    (0L << 11)
#define SCM_APPLYFRAME   (1L << 11)
#define SCM_VOIDFRAME    (3L << 11)
#define SCM_MACROEXPF    (1L << 10)
#define SCM_TAILREC      (1L << 9)
#define SCM_TRACED_FRAME (1L << 8)
#define SCM_ARGS_READY   (1L << 7)
#define SCM_DOVERFLOW    (1L << 6)
#define SCM_MAX_FRAME_SIZE 63

#define SCM_FRAMETYPE    (3L << 11)

#define SCM_EVALFRAMEP(x) (((x).status & SCM_FRAMETYPE) == SCM_EVALFRAME)
#define SCM_APPLYFRAMEP(x) (((x).status & SCM_FRAMETYPE) == SCM_APPLYFRAME)
#define SCM_VOIDFRAMEP(x) (((x).status & SCM_FRAMETYPE) == SCM_VOIDFRAME)
#define SCM_OVERFLOWP(x) (((x).status & SCM_DOVERFLOW) != 0)
#define SCM_ARGS_READY_P(x) (((x).status & SCM_ARGS_READY) != 0)
#define SCM_TRACED_FRAME_P(x) (((x).status & SCM_TRACED_FRAME) != 0)
#define SCM_TAILRECP(x) (((x).status & SCM_TAILREC) != 0)
#define SCM_MACROEXPP(x) (((x).status & SCM_MACROEXPF) != 0)
#define SCM_SET_OVERFLOW(x) ((x).status |= SCM_DOVERFLOW)
#define SCM_SET_ARGSREADY(x) ((x).status |= SCM_ARGS_READY)
#define SCM_CLEAR_ARGSREADY(x) ((x).status &= ~SCM_ARGS_READY)
#define SCM_SET_TRACED_FRAME(x) ((x).status |= SCM_TRACED_FRAME)
#define SCM_CLEAR_TRACED_FRAME(x) ((x).status &= ~SCM_TRACED_FRAME)
#define SCM_SET_TAILREC(x) ((x).status |= SCM_TAILREC)
#define SCM_SET_MACROEXP(x) ((x).status |= SCM_MACROEXPF)
#define SCM_CLEAR_MACROEXP(x) ((x).status &= ~SCM_MACROEXPF)

#define SCM_DEBUGGINGP scm_debug_mode

/* {Debug Objects}
 */

SCM_API scm_t_bits scm_tc16_debugobj;

#define SCM_DEBUGOBJP(x) \
  SCM_TYP16_PREDICATE (scm_tc16_debugobj, x)
#define SCM_DEBUGOBJ_FRAME(x) \
  ((scm_t_debug_frame *) SCM_CELL_WORD_1 (x))
#define SCM_SET_DEBUGOBJ_FRAME(x, f)  SCM_SET_CELL_WORD_1 (x, f)

/* {Memoized Source}
 */

SCM_API scm_t_bits scm_tc16_memoized;

#define SCM_MEMOIZEDP(x)	SCM_TYP16_PREDICATE (scm_tc16_memoized, x)
#define SCM_MEMOIZED_EXP(x)	SCM_CAR (SCM_CELL_OBJECT_1 (x))
#define SCM_MEMOIZED_ENV(x)	SCM_CDR (SCM_CELL_OBJECT_1 (x))



SCM_API SCM scm_debug_object_p (SCM obj);
SCM_API SCM scm_local_eval (SCM exp, SCM env);
SCM_API SCM scm_reverse_lookup (SCM env, SCM data);
SCM_API SCM scm_start_stack (SCM id, SCM exp, SCM env);
SCM_API SCM scm_procedure_environment (SCM proc);
SCM_API SCM scm_procedure_source (SCM proc);
SCM_API SCM scm_procedure_name (SCM proc);
SCM_API SCM scm_memoized_environment (SCM m);
SCM_API SCM scm_make_memoized (SCM exp, SCM env);
SCM_API SCM scm_memoized_p (SCM obj);
SCM_API SCM scm_with_traps (SCM thunk);
SCM_API SCM scm_evaluator_traps (SCM setting);
SCM_API SCM scm_debug_options (SCM setting);
SCM_API SCM scm_unmemoize (SCM memoized);
SCM_API SCM scm_make_debugobj (scm_t_debug_frame *debug);
SCM_API void scm_init_debug (void);

#ifdef GUILE_DEBUG
SCM_API SCM scm_make_iloc (SCM frame, SCM binding, SCM cdrp);
SCM_API SCM scm_iloc_p (SCM obj);
SCM_API SCM scm_memcons (SCM car, SCM cdr, SCM env);
SCM_API SCM scm_mem_to_proc (SCM obj);
SCM_API SCM scm_proc_to_mem (SCM obj);
SCM_API SCM scm_debug_hang (SCM obj);
#endif /*GUILE_DEBUG*/

#endif  /* SCM_DEBUG_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
