/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

#ifndef SCM_DEPRECATED_H
#define SCM_DEPRECATED_H

/* Copyright (C) 2003,2004, 2005, 2006, 2007, 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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

#include "libguile/__scm.h"
#include "libguile/strings.h"
#include "libguile/eval.h"
#include "libguile/throw.h"
#include "libguile/iselect.h"

#if (SCM_ENABLE_DEPRECATED == 1)

/* From eval.h: Macros for handling ilocs.  These were deprecated in guile
 * 1.7.0 on 2004-04-22.  */
#define SCM_IFRINC             (0x00000100L)
#define SCM_ICDR               (0x00080000L)
#define SCM_IFRAME(n)          ((long)((SCM_ICDR-SCM_IFRINC)>>8) \
                                & (SCM_UNPACK (n) >> 8))
#define SCM_IDIST(n)           (SCM_UNPACK (n) >> 20)
#define SCM_ICDRP(n)           (SCM_ICDR & SCM_UNPACK (n))


/* From tags.h: Macros to access internal symbol names of isyms.  Deprecated
 * in guile 1.7.0 on 2004-04-22.  */
SCM_API char *scm_isymnames[];
#define SCM_ISYMNUM(n) 	       0
#define SCM_ISYMCHARS(n)       "#@<deprecated>"


/* From tags.h: Macro checking for two tc16 types that are allocated to differ
 * only in the 's'-bit.  Deprecated in guile 1.7.0 on 2003-09-21.  */
#define SCM_TYP16S(x) 		(0xfeff & SCM_CELL_TYPE (x))


/* From numbers.h: Macros checking for types, but avoiding a redundant check
 * for !SCM_IMP.  These were deprecated in guile 1.7.0 on 2003-09-06.  */
#define SCM_SLOPPY_INEXACTP(x) (SCM_TYP16S (x) == scm_tc16_real)
#define SCM_SLOPPY_REALP(x) (SCM_TYP16 (x) == scm_tc16_real)
#define SCM_SLOPPY_COMPLEXP(x) (SCM_TYP16 (x) == scm_tc16_complex)


/* From structs.h:
   Deprecated in Guile 1.9.5 on 2009-11-03. */
#define scm_vtable_index_vtable scm_vtable_index_self
#define scm_vtable_index_printer scm_vtable_index_instance_printer
#define scm_struct_i_free scm_vtable_index_instance_finalize
#define scm_struct_i_flags scm_vtable_index_flags
#define SCM_STRUCTF_MASK ((scm_t_bits)-1)
#define SCM_SET_VTABLE_DESTRUCTOR(X, D) (SCM_STRUCT_DATA(x)[scm_struct_i_free]=(scm_t_bits)(D))

#define scm_substring_move_left_x scm_substring_move_x
#define scm_substring_move_right_x scm_substring_move_x

#define scm_sizet size_t

SCM_DEPRECATED SCM scm_wta (SCM arg, const char *pos, const char *s_subr);

#define SCM_WNA 		8
#define SCM_OUTOFRANGE         10
#define SCM_NALLOC             11

SCM_DEPRECATED void scm_register_module_xxx (char *module_name, void *init_func);
SCM_DEPRECATED SCM scm_registered_modules (void);
SCM_DEPRECATED SCM scm_clear_registered_modules (void);

SCM_DEPRECATED SCM scm_protect_object (SCM obj);
SCM_DEPRECATED SCM scm_unprotect_object (SCM obj);

#define SCM_SETAND_CAR(x, y) \
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) & (y))))
#define SCM_SETOR_CAR(x, y)\
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) | (y))))
#define SCM_SETAND_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) & (y))))
#define SCM_SETOR_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) | (y))))
#define SCM_FREEP(x) (0)
#define SCM_NFREEP(x) (1)
#define SCM_GCTYP16(x) SCM_TYP16 (x)
#define SCM_GCCDR(x) SCM_CDR (x)
SCM_DEPRECATED void scm_remember (SCM * ptr);

SCM_DEPRECATED SCM scm_make_module (SCM name);
SCM_DEPRECATED SCM scm_ensure_user_module (SCM name);
SCM_DEPRECATED SCM scm_load_scheme_module (SCM name);

#define scm_port scm_t_port
#define scm_ptob_descriptor scm_t_ptob_descriptor
#define scm_port_rw_active scm_t_port_rw_active

SCM_DEPRECATED SCM scm_close_all_ports_except (SCM ports);

#define scm_rstate scm_t_rstate
#define scm_rng scm_t_rng

#define SCM_SLOPPY_CONSP(x)  ((1 & SCM_CELL_TYPE (x)) == 0)
#define SCM_SLOPPY_NCONSP(x) (!SCM_SLOPPY_CONSP(x))

#define scm_tc7_ssymbol		scm_tc7_symbol
#define scm_tc7_msymbol		scm_tc7_symbol
#define scm_tcs_symbols         scm_tc7_symbol

SCM_DEPRECATED SCM scm_makstr (size_t len, int);
SCM_DEPRECATED SCM scm_makfromstr (const char *src, size_t len, int);

SCM_DEPRECATED SCM scm_variable_set_name_hint (SCM var, SCM hint);
SCM_DEPRECATED SCM scm_builtin_variable (SCM name);

SCM_DEPRECATED SCM scm_internal_with_fluids (SCM fluids, SCM vals,
					     SCM (*cproc)(void *),
					     void *cdata);

SCM_DEPRECATED SCM scm_make_gsubr (const char *name,
				   int req, int opt, int rst,
				   scm_t_subr fcn);
SCM_DEPRECATED SCM scm_make_gsubr_with_generic (const char *name,
						int req,
						int opt,
						int rst,
						scm_t_subr fcn,
						SCM *gf);

SCM_DEPRECATED SCM scm_create_hook (const char* name, int n_args);


/* Deprecated 13-05-2011 because it's better just to scm_dynwind_begin.
   That also avoids the temptation to stuff pointers in an SCM.  */

typedef SCM (*scm_t_inner) (void *);
SCM_DEPRECATED SCM scm_internal_dynamic_wind (scm_t_guard before,
                                              scm_t_inner inner,
                                              scm_t_guard after,
                                              void *inner_data,
                                              void *guard_data);

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

SCM_DEPRECATED SCM scm_sloppy_memq (SCM x, SCM lst);
SCM_DEPRECATED SCM scm_sloppy_memv (SCM x, SCM lst);
SCM_DEPRECATED SCM scm_sloppy_member (SCM x, SCM lst);

SCM_DEPRECATED SCM scm_read_and_eval_x (SCM port);

#define scm_subr_entry scm_t_subr_entry

#define SCM_SUBR_DOC(x) SCM_BOOL_F

SCM_DEPRECATED SCM scm_call_catching_errors (scm_t_subr thunk,
					     scm_t_subr err_filter,
					     void * closure);

SCM_DEPRECATED long scm_make_smob_type_mfpe (char *name, size_t size,
					     SCM (*mark) (SCM),
					     size_t (*free) (SCM),
					     int (*print) (SCM, SCM,
							   scm_print_state*),
					     SCM (*equalp) (SCM, SCM));

SCM_DEPRECATED void scm_set_smob_mfpe (long tc,
				       SCM (*mark) (SCM),
				       size_t (*free) (SCM),
				       int (*print) (SCM, SCM, scm_print_state*),
				       SCM (*equalp) (SCM, SCM));

SCM_DEPRECATED size_t scm_smob_free (SCM obj);

SCM_DEPRECATED SCM scm_strprint_obj (SCM obj);
SCM_DEPRECATED SCM scm_read_0str (char *expr);
SCM_DEPRECATED SCM scm_eval_0str (const char *expr);

SCM_DEPRECATED char *scm_i_object_chars (SCM);

#define SCM_CHARS(x)   scm_i_object_chars(x)
#define SCM_UCHARS(x)  ((unsigned char *)SCM_CHARS(x))

SCM_DEPRECATED long scm_i_object_length (SCM);

#define SCM_LENGTH(x) scm_i_object_length(x)

#define scm_strhash(str, len, n) (scm_string_hash ((str), (len)) % (n))

SCM_DEPRECATED SCM scm_sym2ovcell_soft (SCM sym, SCM obarray);
SCM_DEPRECATED SCM scm_sym2ovcell (SCM sym, SCM obarray);
SCM_DEPRECATED SCM scm_intern_obarray_soft (const char *name, size_t len,
				     SCM obarray, unsigned int softness);
SCM_DEPRECATED SCM scm_intern_obarray (const char *name, size_t len, SCM obarray);
SCM_DEPRECATED SCM scm_symbol_value0 (const char *name);

SCM_DEPRECATED SCM scm_string_to_obarray_symbol (SCM o, SCM s, SCM softp);
SCM_DEPRECATED SCM scm_intern_symbol (SCM o, SCM s);
SCM_DEPRECATED SCM scm_unintern_symbol (SCM o, SCM s);
SCM_DEPRECATED SCM scm_symbol_binding (SCM o, SCM s);
#if 0
/* This name has been reused for real uninterned symbols. */
SCM_DEPRECATED SCM scm_symbol_interned_p (SCM o, SCM s);
#endif
SCM_DEPRECATED SCM scm_symbol_bound_p (SCM o, SCM s);
SCM_DEPRECATED SCM scm_symbol_set_x (SCM o, SCM s, SCM v);

SCM_DEPRECATED SCM scm_gentemp (SCM prefix, SCM obarray);

#define SCM_OPDIRP(x) (SCM_DIRP (x) && (SCM_DIR_OPEN_P (x)))
#define scm_fport scm_t_fport
#define scm_option scm_t_option
#define scm_srcprops scm_t_srcprops
#define scm_srcprops_chunk scm_t_srcprops_chunk
#define scm_array scm_t_array
#define scm_array_dim scm_t_array_dim
#define SCM_FUNC_NAME (scm_makfrom0str (FUNC_NAME))

#define SCM_WTA(pos, scm) \
  do { scm_wta (scm, (char *) pos, FUNC_NAME); } while (0)

#define RETURN_SCM_WTA(pos, scm) \
  do { return scm_wta (scm, (char *) pos, FUNC_NAME); } while (0)

#define SCM_VALIDATE_NUMBER_COPY(pos, z, cvar)	\
  do {						\
    if (SCM_I_INUMP (z))				\
      cvar = (double) SCM_I_INUM (z);		\
    else if (SCM_REALP (z))			\
      cvar = SCM_REAL_VALUE (z);		\
    else if (SCM_BIGP (z))			\
      cvar = scm_i_big2dbl (z);			\
    else					\
      {						\
	cvar = 0.0;				\
        SCM_WRONG_TYPE_ARG (pos, z);		\
      }						\
  } while (0)

#define SCM_VALIDATE_NUMBER_DEF_COPY(pos, number, def, cvar)	\
  do {								\
    if (SCM_UNBNDP (number))					\
      cvar = def;						\
    else							\
      SCM_VALIDATE_NUMBER_COPY(pos, number, cvar);		\
  } while (0)

#define SCM_VALIDATE_OPDIR(pos, port) SCM_MAKE_VALIDATE (pos, port, OPDIRP)

/* Deprecated because we can not safely cast a SCM* to a scm_t_bits*
 */

#define SCM_CELL_WORD_LOC(x, n)   ((scm_t_bits*)SCM_CELL_OBJECT_LOC((x),(n)))

/* Users shouldn't know about INUMs.
 */

SCM_DEPRECATED SCM scm_i_makinum (scm_t_signed_bits val);
SCM_DEPRECATED int scm_i_inump (SCM obj);
SCM_DEPRECATED scm_t_signed_bits scm_i_inum (SCM obj);

#define SCM_MAKINUM(x)   scm_i_makinum(x)
#define SCM_INUM(x)      scm_i_inum(x)
#define SCM_INUMP(x)     scm_i_inump(x)
#define SCM_NINUMP(x)    (!SCM_INUMP(x))

#define SCM_VALIDATE_INUM(pos, k) SCM_MAKE_VALIDATE_MSG (pos, k, INUMP, "exact integer")

#define SCM_VALIDATE_INUM_COPY(pos, k, cvar) \
  do { \
    SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
    cvar = SCM_I_INUM (k); \
  } while (0)

#define SCM_VALIDATE_BIGINT(pos, k) SCM_MAKE_VALIDATE_MSG (pos, k, BIGP, "bignum")

#define SCM_VALIDATE_INUM_MIN(pos, k, min) \
  do { \
    SCM_ASSERT (SCM_I_INUMP(k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_I_INUM (k) >= min)); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_COPY(pos, k, min, cvar) \
  do { \
    SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_I_INUM (k) >= min)); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_DEF_COPY(pos, k, min, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_I_MAKINUM (default); \
    SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_I_INUM (k) >= min)); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF(pos, k, default) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_I_MAKINUM (default); \
    else SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF_COPY(pos, k, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      { \
        k = SCM_I_MAKINUM (default); \
        cvar = default; \
      } \
    else \
      { \
        SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
        cvar = SCM_INUM (k); \
      } \
  } while (0)

/* [low, high) */
#define SCM_VALIDATE_INUM_RANGE(pos, k, low, high) \
  do { SCM_ASSERT(SCM_I_INUMP(k), k, pos, FUNC_NAME); \
       SCM_ASSERT_RANGE(pos, k, \
                        (SCM_I_INUM (k) >= low && \
                         SCM_I_INUM (k) < high)); \
     } while (0)

#define SCM_VALIDATE_INUM_RANGE_COPY(pos, k, low, high, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, low <= SCM_INUM (k) && SCM_INUM (k) < high); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_STRING_COERCE_0TERMINATION_X(x) (x)

/* XXX - buggy interface, STR might not be large enough.

   Converts the given Scheme string OBJ into a C string, containing a copy
   of OBJ's content with a trailing null byte.  If LENP is non-NULL, set
   *LENP to the string's length.

   When STR is non-NULL it receives the copy and is returned by the function,
   otherwise new memory is allocated and the caller is responsible for 
   freeing it via free().  If out of memory, NULL is returned.

   Note that Scheme strings may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way to 
   determine the length of the returned value.  However, the function always 
   copies the complete contents of OBJ, and sets *LENP to the length of the
   scheme string (if LENP is non-null).  
*/
SCM_DEPRECATED char *scm_c_string2str (SCM obj, char *str, size_t *lenp);

/* XXX - buggy interface, you don't know how many bytes have been copied.

   Copy LEN characters at START from the Scheme string OBJ to memory
   at STR.  START is an index into OBJ; zero means the beginning of
   the string.  STR has already been allocated by the caller.

   If START + LEN is off the end of OBJ, silently truncate the source
   region to fit the string.  If truncation occurs, the corresponding
   area of STR is left unchanged.  
*/
SCM_DEPRECATED char *scm_c_substring2str (SCM obj, char *str, size_t start, size_t len);

SCM_DEPRECATED char *scm_c_symbol2str (SCM obj, char *str, size_t *lenp);

/* Deprecated because the names belong to what is now
   scm_truncate_number and scm_round_number.
*/
SCM_DEPRECATED double scm_truncate (double x);
SCM_DEPRECATED double scm_round (double x);
/* Deprecated, use scm_expt */
SCM_DEPRECATED SCM scm_sys_expt (SCM x, SCM y);

/* if your platform doesn't have asinh et al */
SCM_DEPRECATED double scm_asinh (double x);
SCM_DEPRECATED double scm_acosh (double x);
SCM_DEPRECATED double scm_atanh (double x);
SCM_DEPRECATED SCM scm_sys_atan2 (SCM z1, SCM z2);

/* Deprecated because we don't want people to access the internal
   representation of strings directly.
*/

#define SCM_VALIDATE_STRING_COPY(pos, str, cvar) \
  do { \
    SCM_ASSERT (SCM_STRINGP (str), str, pos, FUNC_NAME); \
    cvar = SCM_STRING_CHARS(str); \
  } while (0)

/* validate a string and optional start/end arguments which default to
   0/string-len.  this is unrelated to the old shared substring
   support, so please do not deprecate it :) */
#define SCM_VALIDATE_SUBSTRING_SPEC_COPY(pos_str, str, c_str, \
                                         pos_start, start, c_start,\
                                         pos_end, end, c_end) \
  do {\
    SCM_VALIDATE_STRING_COPY (pos_str, str, c_str);\
    c_start = SCM_UNBNDP(start)? 0 : scm_to_size_t (start);\
    c_end = SCM_UNBNDP(end)? SCM_STRING_LENGTH(str) : scm_to_size_t (end);\
    SCM_ASSERT_RANGE (pos_start, start,\
                      0 <= c_start \
                      && (size_t) c_start <= SCM_STRING_LENGTH (str));\
    SCM_ASSERT_RANGE (pos_end, end,\
		      c_start <= c_end \
                      && (size_t) c_end <= SCM_STRING_LENGTH (str));\
  } while (0)

/* Deprecated because we don't want people to access the internals of
   symbols directly.
*/

SCM_DEPRECATED char *scm_i_deprecated_symbol_chars (SCM sym);
SCM_DEPRECATED size_t scm_i_deprecated_symbol_length (SCM sym);

#define SCM_SYMBOL_CHARS(x)  scm_i_deprecated_symbol_chars(x)
#define SCM_SYMBOL_LENGTH(x) scm_i_deprecated_symbol_length(x)

/* Deprecated because the macros used to evaluate the arguments more
   than once and because the symbol of a keyword now has no dash.
*/

SCM_DEPRECATED int scm_i_keywordp (SCM obj);
SCM_DEPRECATED SCM scm_i_keywordsym (SCM keyword);

#define SCM_KEYWORDP(x)   scm_i_keywordp(x)
#define SCM_KEYWORDSYM(x) scm_i_keywordsym(x)

/* Deprecated because we don't want to hand out unprotected pointers
   to arrays, vectors, etc. */

#define SCM_VECTOR_MAX_LENGTH ((1L << 24) - 1)

SCM_DEPRECATED int scm_i_vectorp (SCM x);
SCM_DEPRECATED unsigned long scm_i_vector_length (SCM x);
SCM_DEPRECATED const SCM *scm_i_velts (SCM x);
SCM_DEPRECATED SCM *scm_i_writable_velts (SCM x);
SCM_DEPRECATED SCM scm_i_vector_ref (SCM x, size_t idx);
SCM_DEPRECATED void scm_i_vector_set (SCM x, size_t idx, SCM val);
SCM_DEPRECATED SCM scm_vector_equal_p (SCM x, SCM y);

#define SCM_VECTORP(x)         scm_i_vectorp(x)
#define SCM_VECTOR_LENGTH(x)   scm_i_vector_length(x)
#define SCM_VELTS(x)           scm_i_velts(x)
#define SCM_WRITABLE_VELTS(x)  scm_i_writable_velts(x)
#define SCM_VECTOR_REF(x,y)    scm_i_vector_ref(x,y)
#define SCM_VECTOR_SET(x,y,z)  scm_i_vector_set(x,y,z)

typedef scm_i_t_array scm_t_array;

SCM_DEPRECATED int scm_i_arrayp (SCM a);
SCM_DEPRECATED size_t scm_i_array_ndim (SCM a);
SCM_DEPRECATED int scm_i_array_contp (SCM a);
SCM_DEPRECATED scm_t_array *scm_i_array_mem (SCM a);
SCM_DEPRECATED SCM scm_i_array_v (SCM a);
SCM_DEPRECATED size_t scm_i_array_base (SCM a);
SCM_DEPRECATED scm_t_array_dim *scm_i_array_dims (SCM a);

#define SCM_ARRAYP(a)      scm_i_arrayp(a)
#define SCM_ARRAY_NDIM(a)  scm_i_array_ndim(a)
#define SCM_ARRAY_CONTP(a) scm_i_array_contp(a)
#define SCM_ARRAY_MEM(a)   scm_i_array_mem(a)
#define SCM_ARRAY_V(a)     scm_i_array_v(a)
#define SCM_ARRAY_BASE(a)  scm_i_array_base(a)
#define SCM_ARRAY_DIMS(a)  scm_i_array_dims(a)

SCM_DEPRECATED SCM scm_uniform_vector_read_x (SCM v, SCM port_or_fd,
					      SCM start, SCM end);
SCM_DEPRECATED SCM scm_uniform_vector_write (SCM v, SCM port_or_fd,
					     SCM start, SCM end);
SCM_DEPRECATED SCM scm_uniform_array_read_x (SCM ra, SCM port_or_fd,
					     SCM start, SCM end);
SCM_DEPRECATED SCM scm_uniform_array_write (SCM v, SCM port_or_fd,
					    SCM start, SCM end);

/* Deprecated because they should not be lvalues and we want people to
   use the official interfaces.
 */

#define scm_cur_inp           scm_i_cur_inp ()
#define scm_cur_outp          scm_i_cur_outp ()
#define scm_cur_errp          scm_i_cur_errp ()
#define scm_cur_loadp         scm_i_cur_loadp ()
#define scm_progargs          scm_i_progargs ()
#define scm_dynwinds          scm_i_deprecated_dynwinds ()
#define scm_stack_base        scm_i_stack_base ()

SCM_DEPRECATED SCM scm_i_cur_inp (void);
SCM_DEPRECATED SCM scm_i_cur_outp (void);
SCM_DEPRECATED SCM scm_i_cur_errp (void);
SCM_DEPRECATED SCM scm_i_cur_loadp (void);
SCM_DEPRECATED SCM scm_i_progargs (void);
SCM_DEPRECATED SCM scm_i_deprecated_dynwinds (void);
SCM_DEPRECATED SCM_STACKITEM *scm_i_stack_base (void);

/* Deprecated because it evaluates its argument twice.
 */
#define SCM_FLUIDP(x) scm_i_fluidp (x)
SCM_DEPRECATED int scm_i_fluidp (SCM x);

/* Deprecated in Guile 1.9.5 on 2009-11-15 because these are IPv4-only
   functions which are deprecated upstream.  */

SCM_DEPRECATED SCM scm_inet_aton (SCM address);
SCM_DEPRECATED SCM scm_inet_ntoa (SCM inetid);

/* In the old days, SCM_CRITICAL_SECTION_START stopped signal handlers
   from running, since in those days the handler directly ran scheme
   code, and that had to be avoided when the heap was not in a
   consistent state etc.  And since the scheme code could do a stack
   swapping new continuation etc, signals had to be deferred around
   various C library functions which were not safe or not known to be
   safe to swap away, which was a lot of stuff.

   These days signals are implemented with asyncs and don't directly
   run scheme code in the handler, but hold it until an SCM_TICK etc
   where it will be safe.  This means interrupt protection is not
   needed and SCM_CRITICAL_SECTION_START / SCM_CRITICAL_SECTION_END is
   something of an anachronism.

   What past SCM_CRITICAL_SECTION_START usage also did though was
   indicate code that was not reentrant, ie. could not be reentered by
   signal handler code.  The present definitions are a mutex lock,
   affording that reentrancy protection against the new guile 1.8
   free-running posix threads.

   One big problem with the present defintions though is that code which
   throws an error from within a DEFER/ALLOW region will leave the
   defer_mutex locked and hence hang other threads that attempt to enter a
   similar DEFER/ALLOW region.
*/

SCM_DEPRECATED void scm_i_defer_ints_etc (void);
#define SCM_DEFER_INTS scm_i_defer_ints_etc ()
#define SCM_ALLOW_INTS scm_i_defer_ints_etc ()
#define SCM_REDEFER_INTS scm_i_defer_ints_etc ()
#define SCM_REALLOW_INTS scm_i_defer_ints_etc ()

/* In the old days (pre-1.8), this macro was sometimes used as an lvalue as
   in "scm_mask_ints = 1" to block async execution.  It no longer works.  */
#define scm_mask_ints (scm_i_mask_ints ())

SCM_DEPRECATED int scm_i_mask_ints (void);

/* Deprecated since they are unnecessary and had not been documented.
 */
SCM_DEPRECATED SCM scm_guard (SCM guardian, SCM obj, int throw_p);
SCM_DEPRECATED SCM scm_get_one_zombie (SCM guardian);

/* Deprecated since guardians no longer have these special features.
 */
SCM_DEPRECATED SCM scm_destroy_guardian_x (SCM guardian);
SCM_DEPRECATED SCM scm_guardian_greedy_p (SCM guardian);
SCM_DEPRECATED SCM scm_guardian_destroyed_p (SCM guardian);


/* GC-related things deprecated with the move to BDW-GC starting from 1.9.3
   (2009-09-15).  */

SCM_DEPRECATED unsigned long scm_mallocated;
SCM_DEPRECATED unsigned long scm_mtrigger;

SCM_DEPRECATED size_t scm_max_segment_size;

#if defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST)
SCM_DEPRECATED SCM scm_map_free_list (void);
#endif

#if defined (GUILE_DEBUG_FREELIST)
SCM_DEPRECATED SCM scm_gc_set_debug_check_freelist_x (SCM flag);
#endif



/* Deprecated 2009-11-27, scm_call_N is sufficient */
SCM_DEPRECATED scm_t_trampoline_0 scm_trampoline_0 (SCM proc);
SCM_DEPRECATED scm_t_trampoline_1 scm_trampoline_1 (SCM proc);
SCM_DEPRECATED scm_t_trampoline_2 scm_trampoline_2 (SCM proc);



/* Deprecated 2009-12-06, use the procedures instead */
#define SCM_PROCEDURE_WITH_SETTER_P(obj) (scm_is_true (scm_procedure_with_setter_p (obj)))
#define SCM_PROCEDURE(obj) SCM_STRUCT_PROCEDURE (obj, 0)
#define SCM_SETTER(obj) SCM_STRUCT_SETTER (obj, 1)



/* Deprecated 2010-01-05, use SCM_PRIMITIVE_P instead */
SCM_DEPRECATED int scm_i_subr_p (SCM x);
#define scm_subr_p(x) (scm_i_subr_p (x))



/* Deprecated 2010-01-31, use with-throw-handler instead */
SCM_DEPRECATED SCM scm_lazy_catch (SCM tag, SCM thunk, SCM handler);
SCM_DEPRECATED SCM scm_internal_lazy_catch (SCM tag,
                                            scm_t_catch_body body,
                                            void *body_data,
                                            scm_t_catch_handler handler,
                                            void *handler_data);



/* Deprecated 2010-03-31, use array-equal? instead */
SCM_DEPRECATED SCM scm_raequal (SCM ra0, SCM ra1);

/* Deprecated 2010-04-01, use the dynamic FFI instead */
SCM_DEPRECATED SCM scm_dynamic_args_call (SCM symb, SCM dobj, SCM args);

/* Deprecated 2010-05-12, no replacement */
SCM_DEPRECATED int scm_badargsp (SCM formals, SCM args);

/* Deprecated 2010-06-19, use call-with-error-handling instead */
SCM_DEPRECATED SCM scm_internal_stack_catch (SCM tag,
                                             scm_t_catch_body body,
                                             void *body_data,
                                             scm_t_catch_handler handler,
                                             void *handler_data);



/* These functions were "discouraged" in 1.8, and now are deprecated. */

/* scm_to_int, scm_from_int are the official functions to do the job,
   but there is nothing wrong with using scm_num2int, etc.

   These could be trivially defined via macros, but we leave them as
   functions since existing code may take their addresses.
*/

SCM_DEPRECATED SCM scm_short2num (short n);
SCM_DEPRECATED SCM scm_ushort2num (unsigned short n);
SCM_DEPRECATED SCM scm_int2num (int n);
SCM_DEPRECATED SCM scm_uint2num (unsigned int n);
SCM_DEPRECATED SCM scm_long2num (long n);
SCM_DEPRECATED SCM scm_ulong2num (unsigned long n);
SCM_DEPRECATED SCM scm_size2num (size_t n);
SCM_DEPRECATED SCM scm_ptrdiff2num (scm_t_ptrdiff n);
SCM_DEPRECATED short scm_num2short (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_DEPRECATED unsigned short scm_num2ushort (SCM num, unsigned long int pos,
				       const char *s_caller);
SCM_DEPRECATED int scm_num2int (SCM num, unsigned long int pos,
			 const char *s_caller);
SCM_DEPRECATED unsigned int scm_num2uint (SCM num, unsigned long int pos,
				   const char *s_caller);
SCM_DEPRECATED long scm_num2long (SCM num, unsigned long int pos,
			   const char *s_caller);
SCM_DEPRECATED unsigned long scm_num2ulong (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_DEPRECATED scm_t_ptrdiff scm_num2ptrdiff (SCM num, unsigned long int pos,
                                       const char *s_caller);
SCM_DEPRECATED size_t scm_num2size (SCM num, unsigned long int pos,
			     const char *s_caller);
#if SCM_SIZEOF_LONG_LONG != 0
SCM_DEPRECATED SCM scm_long_long2num (long long sl);
SCM_DEPRECATED SCM scm_ulong_long2num (unsigned long long sl);
SCM_DEPRECATED long long scm_num2long_long (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_DEPRECATED unsigned long long scm_num2ulong_long (SCM num, unsigned long int pos,
					       const char *s_caller);
#endif

SCM_DEPRECATED SCM scm_make_real (double x);
SCM_DEPRECATED double scm_num2dbl (SCM a, const char * why);
SCM_DEPRECATED SCM scm_float2num (float n);
SCM_DEPRECATED SCM scm_double2num (double n);

/* The next two are implemented in numbers.c since they use features
   only available there.
*/
SCM_DEPRECATED float scm_num2float (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_DEPRECATED double scm_num2double (SCM num, unsigned long int pos,
			       const char *s_caller);

SCM_DEPRECATED SCM scm_make_complex (double x, double y);

/* Discouraged because they don't make the encoding explicit.
 */

SCM_DEPRECATED SCM scm_mem2symbol (const char *mem, size_t len);
SCM_DEPRECATED SCM scm_mem2uninterned_symbol (const char *mem, size_t len);
SCM_DEPRECATED SCM scm_str2symbol (const char *str);

SCM_DEPRECATED SCM scm_take_str (char *s, size_t len);
SCM_DEPRECATED SCM scm_take0str (char *s);
SCM_DEPRECATED SCM scm_mem2string (const char *src, size_t len);
SCM_DEPRECATED SCM scm_str2string (const char *src);
SCM_DEPRECATED SCM scm_makfrom0str (const char *src);
SCM_DEPRECATED SCM scm_makfrom0str_opt (const char *src);

/* Discouraged because scm_c_make_string has a better name and is more
   consistent with make-string.
 */
SCM_DEPRECATED SCM scm_allocate_string (size_t len);

/* Discouraged because they are just strange.
 */

SCM_DEPRECATED SCM scm_make_keyword_from_dash_symbol (SCM symbol);
SCM_DEPRECATED SCM scm_keyword_dash_symbol (SCM keyword);

/* Discouraged because it does not state what encoding S is in.
 */

SCM_DEPRECATED SCM scm_c_make_keyword (const char *s);

SCM_DEPRECATED unsigned int scm_thread_sleep (unsigned int);
SCM_DEPRECATED unsigned long scm_thread_usleep (unsigned long);
SCM_DEPRECATED int scm_internal_select (int fds,
                                        fd_set *rfds,
                                        fd_set *wfds,
                                        fd_set *efds,
                                        struct timeval *timeout);

/* Deprecated because the cuserid call is deprecated.
 */
SCM_DEPRECATED SCM scm_cuserid (void);



/* Deprecated because it's yet another property interface.
 */
SCM_DEPRECATED SCM scm_primitive_make_property (SCM not_found_proc);
SCM_DEPRECATED SCM scm_primitive_property_ref (SCM prop, SCM obj);
SCM_DEPRECATED SCM scm_primitive_property_set_x (SCM prop, SCM obj, SCM val);
SCM_DEPRECATED SCM scm_primitive_property_del_x (SCM prop, SCM obj);



/* {The old whash table interface}
 * Deprecated, as the hash table interface is sufficient, and accessing
 * handles of weak hash tables is no longer supported.
 */

#define scm_whash_handle SCM

SCM_DEPRECATED SCM scm_whash_get_handle (SCM whash, SCM key);
SCM_DEPRECATED int SCM_WHASHFOUNDP (SCM h);
SCM_DEPRECATED SCM SCM_WHASHREF (SCM whash, SCM handle);
SCM_DEPRECATED void SCM_WHASHSET (SCM whash, SCM handle, SCM obj);
SCM_DEPRECATED SCM scm_whash_create_handle (SCM whash, SCM key);
SCM_DEPRECATED SCM scm_whash_lookup (SCM whash, SCM obj);
SCM_DEPRECATED void scm_whash_insert (SCM whash, SCM key, SCM obj);




/* No need for a table for names, and the struct->class mapping is
   maintained by GOOPS now.  */
#define SCM_STRUCT_TABLE_NAME(X) SCM_CAR (X)
#define SCM_SET_STRUCT_TABLE_NAME(X, NAME) SCM_SETCAR (X, NAME)
#define SCM_STRUCT_TABLE_CLASS(X) SCM_CDR (X)
#define SCM_SET_STRUCT_TABLE_CLASS(X, CLASS) SCM_SETCDR (X, CLASS)

SCM_DEPRECATED SCM scm_struct_table;
SCM_DEPRECATED SCM scm_struct_create_handle (SCM obj);




/* Deprecated 26-05-2011, as the GC_STUBBORN API doesn't do anything any
   more.  */
SCM_DEPRECATED SCM scm_immutable_cell (scm_t_bits car, scm_t_bits cdr);
SCM_DEPRECATED SCM scm_immutable_double_cell (scm_t_bits car, scm_t_bits cbr,
				       scm_t_bits ccr, scm_t_bits cdr);



SCM_DEPRECATED scm_t_bits scm_i_deprecated_asrtgo (scm_t_bits condition);

/* Deprecated 08-01-2012, as it's undocumented and unused.  */
#define SCM_ASRTGO(_cond, _label)		\
  do { if (!scm_i_deprecated_asrtgo(_cond)) goto _label; } while (0)



/* Deprecated 23-05-2012, as as it's undocumented, poorly named, and
   adequately replaced by scm_module_variable /
   scm_ensure_module_variable / scm_define / scm_module_define.  */
SCM_DEPRECATED SCM scm_sym2var (SCM sym, SCM thunk, SCM definep);



/* Eval closure deprecation, 23-05-2012.  */
#define SCM_TOP_LEVEL_LOOKUP_CLOSURE (scm_current_module_lookup_closure())

SCM_DEPRECATED SCM scm_lookup_closure_module (SCM proc);
SCM_DEPRECATED SCM scm_module_lookup_closure (SCM module);
SCM_DEPRECATED SCM scm_current_module_lookup_closure (void);

SCM_DEPRECATED scm_t_bits scm_tc16_eval_closure;

#define SCM_EVAL_CLOSURE_P(x)	SCM_TYP16_PREDICATE (scm_tc16_eval_closure, x)

SCM_DEPRECATED SCM scm_eval_closure_lookup (SCM eclo, SCM sym, SCM definep);
SCM_DEPRECATED SCM scm_standard_eval_closure (SCM module);
SCM_DEPRECATED SCM scm_standard_interface_eval_closure (SCM module);
SCM_DEPRECATED SCM scm_eval_closure_module (SCM eval_closure);



SCM_DEPRECATED SCM scm_struct_vtable_tag (SCM handle);



#ifdef UCHAR_MAX
# define SCM_CHAR_CODE_LIMIT (UCHAR_MAX + 1L)
#else
# define SCM_CHAR_CODE_LIMIT 256L
#endif



SCM_DEPRECATED SCM scm_generalized_vector_p (SCM v);
SCM_DEPRECATED SCM scm_generalized_vector_length (SCM v);
SCM_DEPRECATED SCM scm_generalized_vector_ref (SCM v, SCM idx);
SCM_DEPRECATED SCM scm_generalized_vector_set_x (SCM v, SCM idx, SCM val);
SCM_DEPRECATED SCM scm_generalized_vector_to_list (SCM v);



SCM_DEPRECATED SCM scm_c_program_source (SCM program, size_t ip);



SCM_DEPRECATED SCM scm_gc_live_object_stats (void);



SCM_DEPRECATED SCM scm_htons (SCM in);
SCM_DEPRECATED SCM scm_ntohs (SCM in);
SCM_DEPRECATED SCM scm_htonl (SCM in);
SCM_DEPRECATED SCM scm_ntohl (SCM in);



void scm_i_init_deprecated (void);

#endif

#endif /* SCM_DEPRECATED_H */
