/*
 * private-options.h - private declarations for option handling
 *
 * We put this in a private header, since layout of data structures
 * is an implementation detail that we want to hide.
 * 
 * Copyright (C) 2007, 2009, 2010, 2011 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#ifndef PRIVATE_OPTIONS
#define PRIVATE_OPTIONS

/*
  debugging.
 */
SCM_INTERNAL scm_t_option scm_debug_opts[];

#define SCM_BACKWARDS_P		scm_debug_opts[0].val
#define SCM_BACKTRACE_WIDTH   	scm_debug_opts[1].val
#define SCM_BACKTRACE_DEPTH	scm_debug_opts[2].val
#define SCM_BACKTRACE_P		scm_debug_opts[3].val
#define SCM_STACK_LIMIT		scm_debug_opts[4].val
#define SCM_SHOW_FILE_NAME	scm_debug_opts[5].val
#define SCM_WARN_DEPRECATED	scm_debug_opts[6].val
#define SCM_N_DEBUG_OPTIONS 7


/*
  printing
*/
SCM_INTERNAL scm_t_option scm_print_opts[];

#define SCM_PRINT_HIGHLIGHT_PREFIX  (SCM_PACK (scm_print_opts[0].val))
#define SCM_PRINT_HIGHLIGHT_SUFFIX  (SCM_PACK (scm_print_opts[1].val))
#define SCM_PRINT_KEYWORD_STYLE_I   2
#define SCM_PRINT_KEYWORD_STYLE     (SCM_PACK (scm_print_opts[2].val))
#define SCM_N_PRINT_OPTIONS 3


/*
  read
 */
SCM_INTERNAL scm_t_option scm_read_opts[];

#define SCM_COPY_SOURCE_P      scm_read_opts[0].val
#define SCM_RECORD_POSITIONS_P scm_read_opts[1].val
#define SCM_CASE_INSENSITIVE_P scm_read_opts[2].val
#define SCM_KEYWORD_STYLE      scm_read_opts[3].val
#define SCM_R6RS_ESCAPES_P     scm_read_opts[4].val
#define SCM_SQUARE_BRACKETS_P  scm_read_opts[5].val
#define SCM_HUNGRY_EOL_ESCAPES_P scm_read_opts[6].val

#define SCM_N_READ_OPTIONS 6

#endif  /* PRIVATE_OPTIONS */ 
