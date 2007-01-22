/*
 * private-options.h - private declarations for option handling
 *
 * We put this in a private header, since layout of data structures
 * is an implementation detail that we want to hide.
 * 
 * Copyright (C) 2007 Free Software Foundation, Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef PRIVATE_OPTIONS
#define PRIVATE_OPTIONS

/*
  evaluator
 */
SCM_API scm_t_option scm_eval_opts[];

SCM_API long scm_eval_stack;

SCM_API scm_t_option scm_evaluator_trap_table[];

SCM_API SCM scm_eval_options_interface (SCM setting);

#define SCM_EVAL_STACK         scm_eval_opts[0].val

#define SCM_TRAPS_P            scm_evaluator_trap_table[0].val
#define SCM_ENTER_FRAME_P      scm_evaluator_trap_table[1].val
#define SCM_APPLY_FRAME_P      scm_evaluator_trap_table[2].val
#define SCM_EXIT_FRAME_P       scm_evaluator_trap_table[3].val
#define SCM_ENTER_FRAME_HDLR   (SCM_PACK (scm_evaluator_trap_table[4].val))
#define SCM_APPLY_FRAME_HDLR   (SCM_PACK (scm_evaluator_trap_table[5].val))
#define SCM_EXIT_FRAME_HDLR    (SCM_PACK (scm_evaluator_trap_table[6].val))
#define SCM_MEMOIZE_P       scm_evaluator_trap_table[7].val
#define SCM_MEMOIZE_HDLR    (SCM_PACK (scm_evaluator_trap_table[8].val))

/*
  debugging.
 */
SCM_API scm_t_option scm_debug_opts[];

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
#define SCM_WARN_DEPRECATED	scm_debug_opts[14].val
#define SCM_N_DEBUG_OPTIONS 15


/*
  printing
*/
SCM_API scm_t_option scm_print_opts[];

#define SCM_PRINT_CLOSURE	    (SCM_PACK (scm_print_opts[0].val))
#define SCM_PRINT_SOURCE_P	    ((int) scm_print_opts[1].val)
#define SCM_PRINT_HIGHLIGHT_PREFIX  (SCM_PACK (scm_print_opts[2].val))
#define SCM_PRINT_HIGHLIGHT_SUFFIX  (SCM_PACK (scm_print_opts[3].val))
#define SCM_PRINT_KEYWORD_STYLE_I   4
#define SCM_PRINT_KEYWORD_STYLE     (SCM_PACK (scm_print_opts[4].val))
#define SCM_N_PRINT_OPTIONS 5


/*
  read
 */
SCM_API scm_t_option scm_read_opts[];

#define SCM_COPY_SOURCE_P      scm_read_opts[0].val
#define SCM_RECORD_POSITIONS_P scm_read_opts[1].val
#define SCM_CASE_INSENSITIVE_P scm_read_opts[2].val
#define SCM_KEYWORD_STYLE      scm_read_opts[3].val
#if SCM_ENABLE_ELISP
#define SCM_ELISP_VECTORS_P    scm_read_opts[4].val
#define SCM_ESCAPED_PARENS_P   scm_read_opts[5].val
#define SCM_N_READ_OPTIONS 6
#else
#define SCM_N_READ_OPTIONS 4
#endif

#endif  /* PRIVATE_OPTIONS */ 
