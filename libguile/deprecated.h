/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

#ifndef SCM_DEPRECATED_H
#define SCM_DEPRECATED_H

/* Copyright (C) 2003 Free Software Foundation, Inc.
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "libguile/__scm.h"
#include "libguile/strings.h"

#if (SCM_ENABLE_DEPRECATED == 1)

/* From eval.h: Macros for handling ilocs.  These were deprecated in guile
 * 1.7.0 on 2003-06-04.  */
#define SCM_ILOC00		SCM_MAKE_ITAG8(0L, scm_tc8_iloc)
#define SCM_IDINC		(0x00100000L)
#define SCM_IDSTMSK		(-SCM_IDINC)


/* From eval.h: Error messages of the evaluator.  These were deprecated in
 * guile 1.7.0 on 2003-06-02.  */
SCM_API const char scm_s_expression[];
SCM_API const char scm_s_test[];
SCM_API const char scm_s_body[];
SCM_API const char scm_s_bindings[];
SCM_API const char scm_s_variable[];
SCM_API const char scm_s_clauses[];
SCM_API const char scm_s_formals[];


/* From eval.h: Helper macros for evaluation and application.  These were
 * deprecated in guile 1.7.0 on 2003-06-02.  */
#define SCM_EVALIM2(x) \
  ((SCM_EQ_P ((x), SCM_EOL) \
    ? scm_misc_error (NULL, scm_s_expression, SCM_EOL), 0 \
    : 0), \
   (x))
#define SCM_EVALIM(x, env) (SCM_ILOCP (x) \
                            ? *scm_ilookup ((x), env) \
			    : SCM_EVALIM2(x))
#define SCM_XEVAL(x, env) (SCM_IMP (x) \
			   ? SCM_EVALIM2(x) \
			   : (*scm_ceval_ptr) ((x), (env)))
#define SCM_XEVALCAR(x, env) (SCM_IMP (SCM_CAR (x)) \
			      ? SCM_EVALIM (SCM_CAR (x), env) \
			      : (SCM_SYMBOLP (SCM_CAR (x)) \
			         ? *scm_lookupcar (x, env, 1) \
			         : (*scm_ceval_ptr) (SCM_CAR (x), env)))


#define scm_substring_move_left_x scm_substring_move_x
#define scm_substring_move_right_x scm_substring_move_x

#if SCM_SIZEOF_LONG_LONG != 0
typedef long long long_long;
typedef unsigned long long ulong_long;
#elif SCM_SIZEOF___INT64 != 0
typedef __int64 long_long;
typedef unsigned __int64 ulong_long;
#endif

#define scm_sizet size_t

SCM_API SCM scm_wta (SCM arg, const char *pos, const char *s_subr);

#define SCM_WNA 		8
#define SCM_OUTOFRANGE         10
#define SCM_NALLOC             11

SCM_API void scm_register_module_xxx (char *module_name, void *init_func);
SCM_API SCM scm_registered_modules (void);
SCM_API SCM scm_clear_registered_modules (void);

SCM_API SCM scm_protect_object (SCM obj);
SCM_API SCM scm_unprotect_object (SCM obj);

#define SCM_SETAND_CAR(x, y) \
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) & (y))))
#define SCM_SETOR_CAR(x, y)\
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) | (y))))
#define SCM_SETAND_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) & (y))))
#define SCM_SETOR_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) | (y))))
#define SCM_FREEP(x) (SCM_FREE_CELL_P (x))
#define SCM_NFREEP(x) (!SCM_FREE_CELL_P (x))
#define SCM_GC8MARKP(x) SCM_GC_MARK_P (x)
#define SCM_SETGC8MARK(x) SCM_SET_GC_MARK (x)
#define SCM_CLRGC8MARK(x) SCM_CLEAR_GC_MARK (x)
#define SCM_GCTYP16(x) SCM_TYP16 (x)
#define SCM_GCCDR(x) SCM_CDR (x)
SCM_API void scm_remember (SCM * ptr);

SCM_API SCM scm_the_root_module (void);
SCM_API SCM scm_make_module (SCM name);
SCM_API SCM scm_ensure_user_module (SCM name);
SCM_API SCM scm_load_scheme_module (SCM name);

#define scm_port scm_t_port
#define scm_ptob_descriptor scm_t_ptob_descriptor
#define scm_port_rw_active scm_t_port_rw_active

SCM_API SCM scm_close_all_ports_except (SCM ports);

#define scm_rstate scm_t_rstate
#define scm_rng scm_t_rng

#define SCM_SLOPPY_CONSP(x)  ((1 & SCM_CELL_TYPE (x)) == 0)
#define SCM_SLOPPY_NCONSP(x) (!SCM_SLOPPY_CONSP(x))

#define scm_tc7_ssymbol		scm_tc7_symbol
#define scm_tc7_msymbol		scm_tc7_symbol
#define scm_tcs_symbols         scm_tc7_symbol

SCM_API SCM scm_makstr (size_t len, int);
SCM_API SCM scm_makfromstr (const char *src, size_t len, int);

SCM_API SCM scm_variable_set_name_hint (SCM var, SCM hint);
SCM_API SCM scm_builtin_variable (SCM name);

SCM_API SCM scm_internal_with_fluids (SCM fluids, SCM vals,
				      SCM (*cproc)(void *), void *cdata);

SCM_API SCM scm_make_gsubr (const char *name, int req, int opt, int rst,
			    SCM (*fcn)());
SCM_API SCM scm_make_gsubr_with_generic (const char *name,
					 int req,
					 int opt,
					 int rst,
					 SCM (*fcn)(),
					 SCM *gf);

extern SCM scm_create_hook (const char* name, int n_args);

#define SCM_LIST0 SCM_EOL
#define SCM_LIST1(e0) scm_cons ((e0), SCM_EOL)
#define SCM_LIST2(e0, e1) scm_cons2 ((e0), (e1), SCM_EOL)
#define SCM_LIST3(e0, e1, e2) scm_cons ((e0), SCM_LIST2 ((e1), (e2)))
#define SCM_LIST4(e0, e1, e2, e3)\
     scm_cons2 ((e0), (e1), SCM_LIST2 ((e2), (e3)))
#define SCM_LIST5(e0, e1, e2, e3, e4)\
     scm_cons ((e0), SCM_LIST4 ((e1), (e2), (e3), (e4)))
#define SCM_LIST6(e0, e1, e2, e3, e4, e5)\
     scm_cons2 ((e0), (e1), SCM_LIST4 ((e2), (e3), (e4), (e5)))
#define SCM_LIST7(e0, e1, e2, e3, e4, e5, e6)\
     scm_cons ((e0), SCM_LIST6 ((e1), (e2), (e3), (e4), (e5), (e6)))
#define SCM_LIST8(e0, e1, e2, e3, e4, e5, e6, e7)\
     scm_cons2 ((e0), (e1), SCM_LIST6 ((e2), (e3), (e4), (e5), (e6), (e7)))
#define SCM_LIST9(e0, e1, e2, e3, e4, e5, e6, e7, e8)\
     scm_cons ((e0),\
	       SCM_LIST8 ((e1), (e2), (e3), (e4), (e5), (e6), (e7), (e8)))

#define scm_listify scm_list_n

SCM_API SCM scm_sloppy_memq (SCM x, SCM lst);
SCM_API SCM scm_sloppy_memv (SCM x, SCM lst);
SCM_API SCM scm_sloppy_member (SCM x, SCM lst);

SCM_API SCM scm_read_and_eval_x (SCM port);

#define scm_subr_entry scm_t_subr_entry

#define SCM_SUBR_DOC(x) SCM_BOOL_F

SCM_API SCM scm_make_subr (const char *name, int type, SCM (*fcn) ());
SCM_API SCM scm_make_subr_with_generic (const char *name,
					int type,
					SCM (*fcn) (),
					SCM *gf);
SCM_API SCM scm_make_subr_opt (const char *name, 
			       int type, 
			       SCM (*fcn) (),
			       int set);

SCM_API SCM scm_call_catching_errors (SCM (*thunk)(), SCM (*err_filter)(),
				      void * closure);

SCM_API long scm_make_smob_type_mfpe (char *name, size_t size,
				      SCM (*mark) (SCM),
				      size_t (*free) (SCM),
				      int (*print) (SCM, SCM,
						    scm_print_state*),
				      SCM (*equalp) (SCM, SCM));

SCM_API void scm_set_smob_mfpe (long tc, 
				SCM (*mark) (SCM),
				size_t (*free) (SCM),
				int (*print) (SCM, SCM, scm_print_state*),
				SCM (*equalp) (SCM, SCM));

SCM_API SCM scm_strprint_obj (SCM obj);
SCM_API SCM scm_read_0str (char *expr);
SCM_API SCM scm_eval_0str (const char *expr);

SCM_API char *scm_i_object_chars (SCM);

#define SCM_CHARS(x)   scm_i_object_chars(x)
#define SCM_UCHARS(x)  ((unsigned char *)SCM_CHARS(x))

SCM_API long scm_i_object_length (SCM);

#define SCM_LENGTH(x) scm_i_object_length(x)

void scm_i_init_deprecated (void);

#endif

#endif /* SCM_DEPRECATED_H */

#if 0 
/* TODO */

scm_strhash

scm_sym2vcell
scm_sym2ovcell_soft
scm_sym2ovcell

scm_intern_obarray_soft
scm_intern_obarray
scm_intern
scm_intern0

scm_sysintern
scm_sysintern0
scm_sysintern0_no_module_lookup

scm_symbol_value0
scm_string_to_obarray_symbol
scm_intern_symbol

scm_unintern_symbol
scm_symbol_binding
scm_symbol_interned_p

scm_symbol_bound_p
scm_symbol_set_x
scm_gentemp

scm_init_symbols_deprecated
scm_vector_set_length_x

SCM_OPDIRP

scm_fport
scm_option
SCM_CONST_LONG
SCM_VCELL
SCM_GLOBAL_VCELL

SCM_VCELL_INIT
SCM_GLOBAL_VCELL_INIT
scm_srcprops
scm_srcprops_chunk

scm_info_frame
scm_stack
scm_array
scm_array_dim
SCM_ARRAY_CONTIGUOUS

SCM_HUGE_LENGTH
SCM_FUNC_NAME
SCM_WTA
RETURN_SCM_WTA

SCM_VALIDATE_NUMBER_COPY
SCM_VALIDATE_NUMBER_DEF_COPY

SCM_VALIDATE_STRINGORSUBSTR
SCM_VALIDATE_ROSTRING

SCM_VALIDATE_ROSTRING_COPY
SCM_VALIDATE_NULLORROSTRING_COPY

SCM_VALIDATE_RWSTRING
SCM_VALIDATE_OPDIR
scm_small_istr2int

scm_istr2int
scm_istr2flo
scm_istring2number
scm_istr2int

scm_istr2flo
scm_istring2number
scm_vtable_index_vcell

SCM_ECONSP
SCM_NECONSP

scm_tc16_variable

#endif
