/* classes: h_files */

#ifndef MODULESH
#define MODULESH
/*	Copyright (C) 1998, 2000 Free Software Foundation, Inc.
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

#include "libguile/validate.h"



extern int scm_module_system_booted_p;
extern SCM scm_module_tag;

#define SCM_MODULEP(OBJ) \
  (SCM_NIMP (OBJ) && SCM_CELL_TYPE (OBJ) == scm_module_tag)

#define SCM_VALIDATE_MODULE(pos, scm) SCM_MAKE_VALIDATE (pos, scm, MODULEP)

/* NOTE: Indexes of module fields are dependent upon the definition of
 *       module-type in boot-9.scm.
 */

#define scm_module_index_obarray	0
#define scm_module_index_uses		1
#define scm_module_index_binder		2
#define scm_module_index_eval_closure	3
#define scm_module_index_transformer	4

#define SCM_MODULE_OBARRAY(module) \
  SCM_PACK (SCM_STRUCT_DATA (module) [scm_module_index_obarray])
#define SCM_MODULE_USES(module) \
  SCM_PACK (SCM_STRUCT_DATA (module) [scm_module_index_uses])
#define SCM_MODULE_BINDER(module) \
  SCM_PACK (SCM_STRUCT_DATA (module) [scm_module_index_binder])
#define SCM_MODULE_EVAL_CLOSURE(module) \
  SCM_PACK (SCM_STRUCT_DATA (module)[scm_module_index_eval_closure])
#define SCM_MODULE_TRANSFORMER(module) \
  SCM_PACK (SCM_STRUCT_DATA (module)[scm_module_index_transformer])

extern scm_bits_t scm_tc16_eval_closure;

#define SCM_EVAL_CLOSURE_P(x)	SCM_TYP16_PREDICATE (scm_tc16_eval_closure, x)



extern SCM scm_current_module (void);
extern SCM scm_interaction_environment (void);
extern SCM scm_set_current_module (SCM module);

extern SCM scm_c_call_with_current_module (SCM module,
					   SCM (*func)(void *), void *data);

extern SCM scm_c_lookup (const char *name);
extern SCM scm_c_define (const char *name, SCM val);
extern SCM scm_lookup (SCM symbol);
extern SCM scm_define (SCM symbol, SCM val);

extern SCM scm_c_module_lookup (SCM module, const char *name);
extern SCM scm_c_module_define (SCM module, const char *name, SCM val);
extern SCM scm_module_lookup (SCM module, SCM symbol);
extern SCM scm_module_define (SCM module, SCM symbol, SCM val);
extern SCM scm_module_reverse_lookup (SCM module, SCM variable);

extern SCM scm_c_resolve_module (const char *name);
extern SCM scm_resolve_module (SCM name);
extern SCM scm_c_define_module (const char *name,
				void (*init)(void *), void *data);
extern void scm_c_use_module (const char *name);
extern void scm_c_export (const char *name, ...);

extern SCM scm_sym2var (SCM sym, SCM thunk, SCM definep);

extern SCM scm_module_lookup_closure (SCM module);
extern SCM scm_module_transformer (SCM module);
extern SCM scm_current_module_lookup_closure (void);
extern SCM scm_current_module_transformer (void);
extern SCM scm_eval_closure_lookup (SCM eclo, SCM sym, SCM definep);
extern SCM scm_standard_eval_closure (SCM module);
extern SCM scm_standard_interface_eval_closure (SCM module);
extern SCM scm_get_pre_modules_obarray (void);
extern SCM scm_lookup_closure_module (SCM proc);

extern SCM scm_env_top_level (SCM env);
extern SCM scm_env_module (SCM env);
extern SCM scm_top_level_env (SCM thunk);
extern SCM scm_system_module_env_p (SCM env);

extern void scm_modules_prehistory (void);
extern void scm_init_modules (void);

#if SCM_DEBUG_DEPRECATED == 0

extern SCM scm_the_root_module (void);
extern SCM scm_make_module (SCM name);
extern SCM scm_ensure_user_module (SCM name);
extern SCM scm_load_scheme_module (SCM name);

#endif

#endif  /* MODULESH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
