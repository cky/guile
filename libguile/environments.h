/* classes: h_files */

#ifndef ENVIRONMENTS_H
#define ENVIRONMENTS_H
/* Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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



/* The type for folding functions written in C.  A function meant to be passed
 * to scm_c_environment_fold should have the type scm_environment_folder. 
 */
typedef SCM (*scm_environment_folder) (SCM data, SCM sym, SCM val, SCM tail);


/* The type for observer functions written in C.  A function meant to be
 * passed to scm_c_environment_observe should have the type
 * scm_environment_observer.
 */
typedef void (*scm_environment_observer) (SCM env, SCM data);


struct scm_environment_funcs {
  SCM (*ref) (SCM self, SCM symbol);
  SCM (*fold) (SCM self, scm_environment_folder proc, SCM data, SCM init);

  SCM (*define) (SCM self, SCM symbol, SCM value);
  SCM (*undefine) (SCM self, SCM symbol);
  SCM (*set) (SCM self, SCM symbol, SCM value);

  SCM (*cell) (SCM self, SCM symbol, int for_write);
  SCM (*observe) (SCM self, scm_environment_observer proc, SCM data, int weak_p);
  void (*unobserve) (SCM self, SCM token);

  SCM (*mark) (SCM self);
  scm_sizet (*free) (SCM self);
  int (*print) (SCM self, SCM port, scm_print_state *pstate);
};



#define SCM_ENVIRONMENT_SUCCESS SCM_BOOL_T
#define SCM_ENVIRONMENT_BINDING_IMMUTABLE SCM_MAKINUM (0)
#define SCM_ENVIRONMENT_LOCATION_IMMUTABLE SCM_MAKINUM (1)
#define SCM_ENVIRONMENT_LOCATION_NO_CELL SCM_BOOL_F

extern scm_bits_t scm_tc16_environment;

#define SCM_ENVIRONMENT_P(x) \
  (!SCM_IMP (x) && SCM_CELL_TYPE (x) == scm_tc16_environment)
#define SCM_ENVIRONMENT_FUNCS(env) \
  (*((struct scm_environment_funcs **) SCM_CELL_WORD_1 (env)))
#define SCM_ENVIRONMENT_BOUND_P(env, symbol) \
  (!SCM_UNBNDP (SCM_ENVIRONMENT_REF (env, symbol)))
#define SCM_ENVIRONMENT_REF(env, symbol) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->ref)) (env, symbol))
#define SCM_ENVIRONMENT_FOLD(env, proc, data, init) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->fold)) (env, proc, data, init))
#define SCM_ENVIRONMENT_DEFINE(env, symbol, value) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->define)) (env, symbol, value))
#define SCM_ENVIRONMENT_UNDEFINE(env, symbol) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->undefine)) (env, symbol))
#define SCM_ENVIRONMENT_SET(env, symbol, value) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->set)) (env, symbol, value))
#define SCM_ENVIRONMENT_CELL(env, symbol, for_write) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->cell)) (env, symbol, for_write))
#define SCM_ENVIRONMENT_OBSERVE(env, proc, data, weak_p) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->observe)) (env, proc, data, weak_p))
#define SCM_ENVIRONMENT_UNOBSERVE(env, token) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->unobserve)) (env, token))

extern scm_bits_t scm_tc16_observer;

#define SCM_OBSERVER_P(x) \
  (!SCM_IMP (x) && (SCM_CELL_TYPE (x) == scm_tc16_observer))
#define SCM_OBSERVER_ENVIRONMENT(x) \
  (SCM_CELL_OBJECT_1 (x))
#define SCM_OBSERVER_DATA(x) \
  (SCM_CELL_OBJECT_2 (x))
#define SCM_OBSERVER_PROC(x) \
  ((scm_environment_observer) SCM_CELL_WORD_3 (x))

extern void scm_error_environment_unbound (const char *, SCM, SCM) SCM_NORETURN;
extern void scm_error_environment_immutable_binding (const char *, SCM, SCM) SCM_NORETURN;
extern void scm_error_environment_immutable_location (const char *, SCM, SCM) SCM_NORETURN;

extern SCM scm_make_environment (void *type);
extern SCM scm_environment_p (SCM env);
extern SCM scm_environment_bound_p (SCM env, SCM sym);
extern SCM scm_environment_ref (SCM env, SCM sym);
extern SCM scm_c_environment_ref (SCM env, SCM sym);
extern SCM scm_environment_fold (SCM env, SCM proc, SCM init);
extern SCM scm_c_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init);
extern SCM scm_environment_define (SCM env, SCM sym, SCM val);
extern SCM scm_environment_undefine (SCM env, SCM sym);
extern SCM scm_environment_set_x (SCM env, SCM sym, SCM val);
extern SCM scm_environment_cell (SCM env, SCM sym, SCM for_write);
extern SCM scm_c_environment_cell (SCM env, SCM sym, int for_write);
extern SCM scm_environment_observe (SCM env, SCM proc);
extern SCM scm_environment_observe_weak (SCM env, SCM proc);
extern SCM scm_c_environment_observe (SCM env, scm_environment_observer proc, SCM data, int weak_p);
extern SCM scm_environment_unobserve (SCM token);

extern void scm_environments_prehistory (void);
extern void scm_init_environments (void);



extern void *scm_type_leaf_environment;

#define SCM_LEAF_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_leaf_environment)

extern SCM scm_make_leaf_environment (void);
extern SCM scm_leaf_environment_p (SCM env);



extern void *scm_type_eval_environment;

#define SCM_EVAL_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_eval_environment)

extern SCM scm_make_eval_environment (SCM local, SCM imported);
extern SCM scm_eval_environment_p (SCM env);
extern SCM scm_eval_environment_local (SCM env);
extern SCM scm_eval_environment_set_local_x (SCM env, SCM local);
extern SCM scm_eval_environment_imported (SCM env);
extern SCM scm_eval_environment_set_imported_x (SCM env, SCM imported);



extern void *scm_type_import_environment;

#define SCM_IMPORT_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_import_environment)

extern SCM scm_make_import_environment (SCM imports, SCM conflict_proc);
extern SCM scm_import_environment_p (SCM env);
extern SCM scm_import_environment_imports (SCM env);
extern SCM scm_import_environment_set_imports_x (SCM env, SCM imports);



extern void *scm_type_export_environment;

#define SCM_EXPORT_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_export_environment)

extern SCM scm_make_export_environment (SCM private, SCM signature);
extern SCM scm_export_environment_p (SCM env);
extern SCM scm_export_environment_private (SCM env);
extern SCM scm_export_environment_set_private_x (SCM env, SCM private);
extern SCM scm_export_environment_signature (SCM env);
extern SCM scm_export_environment_set_signature_x (SCM env, SCM signature);


#endif


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
