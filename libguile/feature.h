/* classes: h_files */

#ifndef FEATUREH
#define FEATUREH
/*	Copyright (C) 1995, 1996, 1999 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"

#define SCM_HOOKP(x) (SCM_TYP16 (x) == scm_tc16_hook)
#define SCM_HOOK_ARITY(hook) (SCM_CAR (hook) >> 16)
#define SCM_HOOK_PROCEDURES(hook) SCM_CDR (hook)
#define SCM_SET_HOOK_PROCEDURES(hook, procs) SCM_SETCDR (hook, procs)

extern long scm_tc16_hook;

extern void scm_add_feature (const char* str);
extern SCM scm_program_arguments (void);
extern void scm_set_program_arguments (int argc, char **argv, char *first);
extern SCM scm_make_hook (SCM n_args);
extern SCM scm_make_named_hook (char* name, int n_args);
extern SCM scm_hook_p (SCM x);
extern SCM scm_hook_empty_p (SCM hook);
extern SCM scm_add_hook_x (SCM hook, SCM thunk, SCM appendp);
extern SCM scm_remove_hook_x (SCM hook, SCM thunk);
extern SCM scm_reset_hook_x (SCM hook);
extern SCM scm_run_hook (SCM hook, SCM args);
extern void scm_c_run_hook (SCM hook, SCM args);
extern SCM scm_hook_to_list (SCM hook);
extern void scm_init_feature (void);

#endif  /* FEATUREH */
