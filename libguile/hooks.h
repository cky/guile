/* classes: h_files */

#ifndef SCM_HOOKS_H
#define SCM_HOOKS_H
/* Copyright (C) 1995,1996,1999,2000,2001 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"

/*
 * C level hooks
 */

/*
 * The interface is designed for and- and or-type hooks which
 * both may want to indicate success/failure and return a result.
 */

typedef enum scm_t_c_hookype_t {
  SCM_C_HOOK_NORMAL,
  SCM_C_HOOK_OR,
  SCM_C_HOOK_AND
} scm_t_c_hookype_t;

typedef void  *(*scm_t_c_hook_function) (void *hook_data,
					 void *func_data,
					 void *data);

typedef struct scm_t_c_hook_entry {
  struct scm_t_c_hook_entry *next;
  scm_t_c_hook_function func;
  void *data;
} scm_t_c_hook_entry;

typedef struct scm_t_c_hook {
  scm_t_c_hook_entry *first;
  scm_t_c_hookype_t type;
  void *data;
} scm_t_c_hook;

extern void scm_c_hook_init (scm_t_c_hook *hook,
			     void *hook_data,
			     scm_t_c_hookype_t type);
extern void scm_c_hook_add (scm_t_c_hook *hook,
			    scm_t_c_hook_function func,
			    void *func_data, 
			    int appendp);
extern void scm_c_hook_remove (scm_t_c_hook *hook,
			       scm_t_c_hook_function func,
			       void *func_data);
extern void *scm_c_hook_run (scm_t_c_hook *hook, void *data);

/*
 * Scheme level hooks
 */

extern scm_t_bits scm_tc16_hook;

#define SCM_HOOKP(x)			SCM_TYP16_PREDICATE (scm_tc16_hook, x)
#define SCM_HOOK_ARITY(hook)		(SCM_CELL_WORD_0 (hook) >> 16)
#define SCM_HOOK_PROCEDURES(hook)	SCM_CELL_OBJECT_1 (hook)
#define SCM_SET_HOOK_PROCEDURES(hook, procs) SCM_SET_CELL_OBJECT_1 ((hook), (procs))

extern SCM scm_make_hook (SCM n_args);
extern SCM scm_hook_p (SCM x);
extern SCM scm_hook_empty_p (SCM hook);
extern SCM scm_add_hook_x (SCM hook, SCM thunk, SCM appendp);
extern SCM scm_remove_hook_x (SCM hook, SCM thunk);
extern SCM scm_reset_hook_x (SCM hook);
extern SCM scm_run_hook (SCM hook, SCM args);
extern void scm_c_run_hook (SCM hook, SCM args);
extern SCM scm_hook_to_list (SCM hook);
extern void scm_init_hooks (void);

#if (SCM_DEBUG_DEPRECATED == 0)
extern SCM scm_create_hook (const char* name, int n_args);
#endif

#endif  /* SCM_HOOKS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
