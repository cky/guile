/* classes: h_files */

#ifndef COMPATH
#define COMPATH
/*	Copyright (C) 2001, 2002 Free Software Foundation, Inc.
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


#ifndef SCM_GC8MARKP
#  define SCM_GC8MARKP(X) SCM_GC_MARK_P(X)
#  define SCM_SETGC8MARK(X) SCM_SET_GC_MARK(X)
#endif

#ifndef SCM_GC_MARK_P
#  define SCM_GC_MARK_P(X) SCM_GCMARKP(X)
#  define SCM_SET_GC_MARK(X) SCM_SETGCMARK(X)
#endif

#ifndef SCM_ARRAY_FLAG_CONTIGUOUS
#  define SCM_ARRAY_FLAG_CONTIGUOUS SCM_ARRAY_CONTIGUOUS
#endif

#ifndef HAVE_SCM_T_BITS
typedef scm_bits_t scm_t_bits;
typedef scm_array scm_t_array;
typedef scm_array_dim scm_t_array_dim;
typedef scm_mutex_t scm_t_mutex;
typedef scm_cond_t scm_t_cond;
typedef scm_key_t scm_t_key;
typedef scm_catch_body_t scm_t_catch_body;
typedef scm_catch_handler_t scm_t_catch_handler;
typedef scm_rstate scm_t_rstate;
typedef scm_port scm_t_port;
typedef scm_fport scm_t_fport;
#endif

#ifndef SCM_VALIDATE_DOUBLE_COPY
#define SCM_VALIDATE_DOUBLE_COPY SCM_VALIDATE_NUMBER_COPY
#endif

#ifndef HAVE_SCM_C_DEFINE_MODULE
#define scm_c_define_module(NAME,INIT,DATA) \
  scm_make_module (scm_read_0str ("(" NAME ")"))
#endif

#ifndef SCM_MAKE_CHAR
#define SCM_MAKE_CHAR SCM_MAKICHR
#define SCM_CHAR SCM_ICHR
#define SCM_CHARP SCM_ICHRP
#endif

#ifndef SCM_ROSTRINGP
#define SCM_ROSTRINGP(x) (SCM_STRINGP (x) || SCM_SYMBOLP (x))
#define SCM_RWSTRINGP(x) SCM_STRINGP (x)
#define SCM_ROCHARS(x) \
  (SCM_STRINGP (x) ? SCM_STRING_CHARS (x) : SCM_SYMBOL_CHARS (x))
#define SCM_ROLENGTH(x) \
  (SCM_STRINGP (x) ? SCM_STRING_LENGTH (x) : SCM_SYMBOL_LENGTH (x))
#endif

#ifndef SCM_STRING_COERCE_0TERMINATION_X
#ifdef SCM_COERCE_SUBSTR
#define SCM_STRING_COERCE_0TERMINATION_X SCM_COERCE_SUBSTR
#else
#define SCM_STRING_COERCE_0TERMINATION_X(x) (x)
#endif
#endif

#ifndef HAVE_SCM_C_READ_STRING
#define scm_c_read_string scm_read_0str
#define scm_c_eval_string scm_eval_0str
#define scm_str2symbol(X) SCM_CAR (scm_intern0 (X))
#define scm_mem2string(X, Y) scm_makfromstr ((X), (Y), 0)
#endif

#ifndef HAVE_SCM_MAKE_REAL
#define scm_make_real(X) scm_makdbl ((X), 0.0)
#endif

#ifdef HAVE_SCM_NUM2DOUBLE
#define scm_real2double scm_num2double
#define SCM_REAL2DOUBLE SCM_NUM2DOUBLE
#else
#define scm_real2double(X, POS, WHERE) scm_num2dbl ((X), (WHERE))
#define SCM_REAL2DOUBLE(X, POS) scm_num2dbl ((X), FUNC_NAME)
#endif

#ifndef SCM_VALIDATE_DOUBLE_DEF_COPY
#define SCM_VALIDATE_DOUBLE_DEF_COPY SCM_VALIDATE_NUMBER_DEF_COPY
#endif

#ifndef HAVE_SCM_GC_PROTECT_OBJECT
#define scm_gc_protect_object scm_protect_object
#endif

#ifndef HAVE_SCM_C_DEFINE_GSUBR
#define scm_c_define_gsubr scm_make_gsubr
#endif

#ifndef SCM_STRING_CHARS
#define SCM_STRING_CHARS SCM_CHARS
#define SCM_STRING_UCHARS SCM_UCHARS
#define SCM_STRING_LENGTH SCM_LENGTH
#endif

#ifndef SCM_SUBSTRP
#define SCM_SUBSTRP(X) 0
#endif

#ifndef SCM_VECTOR_LENGTH
#define SCM_VECTOR_LENGTH SCM_LENGTH
#define SCM_UVECTOR_LENGTH SCM_LENGTH
#endif

#ifndef SCM_SET_VECTOR_LENGTH
#define SCM_SET_VECTOR_LENGTH SCM_SETLENGTH
#define SCM_SET_UVECTOR_LENGTH SCM_SETLENGTH
#endif

#ifndef SCM_VECTOR_BASE
#define SCM_VECTOR_BASE SCM_CHARS
#define SCM_UVECTOR_BASE SCM_CHARS
#endif

#ifndef SCM_SET_VECTOR_BASE
#define SCM_SET_VECTOR_BASE SCM_SETCHARS
#define SCM_SET_UVECTOR_BASE SCM_SETCHARS
#endif

#ifndef SCM_UVECTOR_MAX_LENGTH
#define SCM_UVECTOR_MAX_LENGTH SCM_LENGTH_MAX
#endif

#ifndef HAVE_SCM_LIST_1
#define scm_list_1 SCM_LIST1
#define scm_list_2 SCM_LIST2
#define scm_list_3 SCM_LIST3
#define scm_list_4 SCM_LIST4
#define scm_list_5 SCM_LIST5
#define scm_list_n scm_listify
#endif

#ifndef SCM_SYMBOL_CHARS
#define SCM_SYMBOL_CHARS SCM_CHARS
#endif

#endif /* COMPATH */
