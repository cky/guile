/* classes: h_files */

#ifndef SCM_VALIDATE_H
#define SCM_VALIDATE_H

/* Copyright (C) 1999,2000,2001, 2002 Free Software Foundation, Inc.
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

/* Written by Greg J. Badros <gjb@cs.washington.edu>, Dec-1999 */



#define SCM_SYSERROR do { scm_syserror (FUNC_NAME); } while (0)

#define SCM_MEMORY_ERROR do { scm_memory_error (FUNC_NAME); } while (0)

#define SCM_SYSERROR_MSG(str, args, val) \
  do { scm_syserror_msg (FUNC_NAME, (str), (args), (val)); } while (0)

#define SCM_MISC_ERROR(str, args) \
  do { scm_misc_error (FUNC_NAME, str, args); } while (0)

#define SCM_WRONG_NUM_ARGS() \
  do { scm_error_num_args_subr (FUNC_NAME); } while (0)

#define SCM_WRONG_TYPE_ARG(pos, obj) \
  do { scm_wrong_type_arg (FUNC_NAME, pos, obj); } while (0)

#define SCM_NUM2SIZE(pos, arg) (scm_num2size (arg, pos, FUNC_NAME))

#define SCM_NUM2SIZE_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2size (arg, pos, FUNC_NAME))

#define SCM_NUM2PTRDIFF(pos, arg) (scm_num2ptrdiff (arg, pos, FUNC_NAME))

#define SCM_NUM2PTRDIFF_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2ptrdiff (arg, pos, FUNC_NAME))

#define SCM_NUM2SHORT(pos, arg) (scm_num2short (arg, pos, FUNC_NAME))

#define SCM_NUM2SHORT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2short (arg, pos, FUNC_NAME))

#define SCM_NUM2USHORT(pos, arg) (scm_num2ushort (arg, pos, FUNC_NAME))

#define SCM_NUM2USHORT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2ushort (arg, pos, FUNC_NAME))

#define SCM_NUM2INT(pos, arg) (scm_num2int (arg, pos, FUNC_NAME))

#define SCM_NUM2INT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2int (arg, pos, FUNC_NAME))

#define SCM_NUM2UINT(pos, arg) (scm_num2uint (arg, pos, FUNC_NAME))

#define SCM_NUM2UINT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2uint (arg, pos, FUNC_NAME))

#define SCM_NUM2ULONG(pos, arg) (scm_num2ulong (arg, pos, FUNC_NAME))

#define SCM_NUM2ULONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2ulong (arg, pos, FUNC_NAME))

#define SCM_NUM2LONG(pos, arg) (scm_num2long (arg, pos, FUNC_NAME))

#define SCM_NUM2LONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2long (arg, pos, FUNC_NAME))

#define SCM_NUM2LONG_LONG(pos, arg) \
  (scm_num2long_long (arg, pos, FUNC_NAME))

#define SCM_NUM2LONG_LONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2long_long (arg, pos, FUNC_NAME))

#define SCM_NUM2ULONG_LONG(pos, arg) \
  (scm_num2ulong_long (arg, pos, FUNC_NAME))

#define SCM_NUM2ULONG_LONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2ulong_long (arg, pos, FUNC_NAME))

#define SCM_NUM2FLOAT(pos, arg) \
  (scm_num2float (arg, pos, FUNC_NAME))

#define SCM_NUM2DOUBLE(pos, arg) \
  (scm_num2double (arg, pos, FUNC_NAME))

#define SCM_OUT_OF_RANGE(pos, arg) \
  do { scm_out_of_range_pos (FUNC_NAME, arg, SCM_MAKINUM (pos)); } while (0)

#define SCM_ASSERT_RANGE(pos, arg, f) \
  do { if (!(f)) scm_out_of_range_pos (FUNC_NAME, arg, SCM_MAKINUM (pos)); } while (0)

#define SCM_MUST_MALLOC_TYPE(type) \
  ((type *) scm_must_malloc (sizeof (type), FUNC_NAME))

#define SCM_MUST_MALLOC_TYPE_NUM(type, num) \
  ((type *) scm_must_malloc (sizeof (type) * (num), FUNC_NAME))

#define SCM_MUST_MALLOC(size) (scm_must_malloc ((size), FUNC_NAME))

#define SCM_MAKE_VALIDATE(pos, var, pred) \
  do { \
    SCM_ASSERT_TYPE (SCM_ ## pred (var), var, pos, FUNC_NAME, #pred); \
  } while (0)

#define SCM_MAKE_VALIDATE_MSG(pos, var, pred, msg) \
  do { \
    SCM_ASSERT_TYPE (SCM_ ## pred (var), var, pos, FUNC_NAME, msg); \
  } while (0)



#define SCM_VALIDATE_REST_ARGUMENT(x) \
  do { \
    if (SCM_DEBUG_REST_ARGUMENT) { \
      if (scm_ilength (x) < 0) { \
        SCM_MISC_ERROR ("Rest arguments do not form a proper list.", SCM_EOL); \
      } \
    } \
  } while (0)

#define SCM_VALIDATE_NIM(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, NIMP, "non-immediate")

#define SCM_VALIDATE_BOOL(pos, flag) SCM_MAKE_VALIDATE_MSG(pos, flag, BOOLP, "boolean")

#define SCM_VALIDATE_BOOL_COPY(pos, flag, cvar) \
  do { \
    SCM_ASSERT (SCM_BOOLP (flag), flag, pos, FUNC_NAME); \
    cvar = SCM_EQ_P (flag, SCM_BOOL_T) ? 1 : 0; \
  } while (0)

#define SCM_VALIDATE_CHAR(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, CHARP, "character")

#define SCM_VALIDATE_CHAR_COPY(pos, scm, cvar) \
  do { \
    SCM_ASSERT (SCM_CHARP (scm), scm, pos, FUNC_NAME); \
    cvar = SCM_CHAR (scm); \
  } while (0)

#define SCM_VALIDATE_STRING(pos, str) SCM_MAKE_VALIDATE_MSG (pos, str, STRINGP, "string")

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
    SCM_VALIDATE_INUM_DEF_COPY (pos_start, start, 0, c_start);\
    SCM_VALIDATE_INUM_DEF_COPY (pos_end, end, SCM_STRING_LENGTH (str), c_end);\
    SCM_ASSERT_RANGE (pos_start, start,\
                      0 <= c_start \
                      && (size_t) c_start <= SCM_STRING_LENGTH (str));\
    SCM_ASSERT_RANGE (pos_end, end,\
		      c_start <= c_end \
                      && (size_t) c_end <= SCM_STRING_LENGTH (str));\
  } while (0)

#define SCM_VALIDATE_REAL(pos, z) SCM_MAKE_VALIDATE_MSG (pos, z, REALP, "real")

#define SCM_VALIDATE_NUMBER(pos, z) SCM_MAKE_VALIDATE_MSG (pos, z, NUMBERP, "number")

#define SCM_VALIDATE_INUM(pos, k) SCM_MAKE_VALIDATE_MSG (pos, k, INUMP, "exact integer")

#define SCM_VALIDATE_INUM_COPY(pos, k, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_USHORT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2USHORT (pos, k); \
  } while (0)

#define SCM_VALIDATE_SHORT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2SHORT (pos, k); \
  } while (0)

#define SCM_VALIDATE_UINT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2UINT (pos, k); \
  } while (0)

#define SCM_VALIDATE_INT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2INT (pos, k); \
  } while (0)

#define SCM_VALIDATE_ULONG_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2ULONG (pos, k); \
  } while (0)

#define SCM_VALIDATE_LONG_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2LONG (pos, k); \
  } while (0)

#define SCM_VALIDATE_FLOAT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2FLOAT (pos, k); \
  } while (0)

#define SCM_VALIDATE_DOUBLE_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2DOUBLE (pos, k); \
  } while (0)

#define SCM_VALIDATE_BIGINT(pos, k) SCM_MAKE_VALIDATE_MSG (pos, k, BIGP, "bignum")

#define SCM_VALIDATE_INUM_MIN(pos, k, min) \
  do { \
    SCM_ASSERT (SCM_INUMP(k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_INUM (k) >= min)); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_COPY(pos, k, min, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_INUM (k) >= min)); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_DEF_COPY(pos, k, min, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_MAKINUM (default); \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_INUM (k) >= min)); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF(pos, k, default) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_MAKINUM (default); \
    else SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF_COPY(pos, k, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      { \
        k = SCM_MAKINUM (default); \
        cvar = default; \
      } \
    else \
      { \
        SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
        cvar = SCM_INUM (k); \
      } \
  } while (0)

#define SCM_VALIDATE_DOUBLE_DEF_COPY(pos, k, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      { \
        k = scm_make_real (default); \
        cvar = default; \
      } \
    else \
      { \
        cvar = SCM_NUM2DOUBLE (pos, k); \
      } \
  } while (0)

/* [low, high) */
#define SCM_VALIDATE_INUM_RANGE(pos, k, low, high) \
  do { SCM_ASSERT(SCM_INUMP(k), k, pos, FUNC_NAME); \
       SCM_ASSERT_RANGE(pos, k, \
                        (SCM_INUM (k) >= low && \
                         SCM_INUM (k) < high)); \
     } while (0)

#define SCM_VALIDATE_INUM_RANGE_COPY(pos, k, low, high, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, low <= SCM_INUM (k) && SCM_INUM (k) < high); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_NULL(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, NULLP, "null")

#define SCM_VALIDATE_NULL_OR_NIL(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, NULL_OR_NIL_P, "null")

#define SCM_VALIDATE_CONS(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, CONSP, "pair")

#define SCM_VALIDATE_LIST(pos, lst) \
  do { \
    SCM_ASSERT (scm_ilength (lst) >= 0, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_NONEMPTYLIST(pos, lst) \
  do { \
    SCM_ASSERT (scm_ilength (lst) > 0, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_LIST_COPYLEN(pos, lst, cvar) \
  do { \
    cvar = scm_ilength (lst); \
    SCM_ASSERT (cvar >= 0, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_NONEMPTYLIST_COPYLEN(pos, lst, cvar) \
  do { \
    cvar = scm_ilength (lst); \
    SCM_ASSERT (cvar >= 1, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_ALISTCELL(pos, alist) \
  do { \
    SCM_ASSERT (SCM_CONSP (alist) && SCM_CONSP (SCM_CAR (alist)), \
                alist, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_ALISTCELL_COPYSCM(pos, alist, cvar) \
  do { \
    SCM_ASSERT (SCM_CONSP (alist), alist, pos, FUNC_NAME); \
    cvar = SCM_CAR (alist); \
    SCM_ASSERT (SCM_CONSP (cvar), alist, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_OPORT_VALUE(pos, port) \
  do { \
    SCM_ASSERT (scm_valid_oport_value_p (port), port, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_PRINTSTATE(pos, a) SCM_MAKE_VALIDATE_MSG(pos, a, PRINT_STATE_P, "print-state")

#define SCM_VALIDATE_SMOB(pos, obj, type) \
  do { \
    SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_ ## type, obj), \
                obj, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_THREAD(pos, a) SCM_MAKE_VALIDATE_MSG (pos, a, THREADP, "thread")

#define SCM_VALIDATE_THUNK(pos, thunk) \
  do { \
    SCM_ASSERT (!SCM_FALSEP (scm_thunk_p (thunk)), thunk, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_SYMBOL(pos, sym) SCM_MAKE_VALIDATE_MSG (pos, sym, SYMBOLP, "symbol")

#define SCM_VALIDATE_VARIABLE(pos, var) SCM_MAKE_VALIDATE_MSG (pos, var, VARIABLEP, "variable")

#define SCM_VALIDATE_MEMOIZED(pos, obj) SCM_MAKE_VALIDATE_MSG (pos, obj, MEMOIZEDP, "memoized code")

#define SCM_VALIDATE_CLOSURE(pos, obj) SCM_MAKE_VALIDATE_MSG (pos, obj, CLOSUREP, "closure")

#define SCM_VALIDATE_PROC(pos, proc) \
  do { \
    SCM_ASSERT (SCM_EQ_P (scm_procedure_p (proc), SCM_BOOL_T), proc, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_NULLORCONS(pos, env) \
  do { \
    SCM_ASSERT (SCM_NULLP (env) || SCM_CONSP (env), env, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_HOOK(pos, a) SCM_MAKE_VALIDATE_MSG (pos, a, HOOKP, "hook")

#define SCM_VALIDATE_RGXP(pos, a) SCM_MAKE_VALIDATE_MSG (pos, a, RGXP, "regexp")

#define SCM_VALIDATE_DIR(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, DIRP, "directory port")

#define SCM_VALIDATE_PORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, PORTP, "port")

#define SCM_VALIDATE_INPUT_PORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, INPUT_PORT_P, "input port")

#define SCM_VALIDATE_OUTPUT_PORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OUTPUT_PORT_P, "output port")

#define SCM_VALIDATE_FPORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, FPORTP, "file port")

#define SCM_VALIDATE_OPFPORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, OPFPORTP, "open file port")

#define SCM_VALIDATE_OPINPORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OPINPORTP, "open input port")

#define SCM_VALIDATE_OPENPORT(pos, port) \
  do { \
    SCM_ASSERT (SCM_PORTP (port) && SCM_OPENP (port), \
                port, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_OPPORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, OPPORTP, "open port")

#define SCM_VALIDATE_OPOUTPORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OPOUTPORTP, "open output port")

#define SCM_VALIDATE_OPOUTSTRPORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OPOUTSTRPORTP, "open output string port")

#define SCM_VALIDATE_FLUID(pos, fluid) SCM_MAKE_VALIDATE_MSG (pos, fluid, FLUIDP, "fluid")

#define SCM_VALIDATE_KEYWORD(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, KEYWORDP, "keyword")

#define SCM_VALIDATE_STACK(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, STACKP, "stack")

#define SCM_VALIDATE_FRAME(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, FRAMEP, "frame")

#define SCM_VALIDATE_RSTATE(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, RSTATEP, "random-generator-state")

#define SCM_VALIDATE_ARRAY(pos, v) \
  do { \
    SCM_ASSERT (!SCM_IMP (v) \
                && !SCM_FALSEP (scm_array_p (v, SCM_UNDEFINED)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_VECTOR(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, VECTORP, "vector")

#define SCM_VALIDATE_VECTOR_OR_DVECTOR(pos, v) \
  do { \
    SCM_ASSERT ((SCM_VECTORP (v) \
                || (!SCM_IMP (v) && SCM_TYP7 (v) == scm_tc7_dvect)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_STRUCT(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, STRUCTP, "struct")

#define SCM_VALIDATE_VTABLE(pos, v) \
  do { \
    SCM_ASSERT (!SCM_IMP (v) && !SCM_FALSEP (scm_struct_vtable_p (v)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_VECTOR_LEN(pos, v, len) \
  do { \
    SCM_ASSERT (SCM_VECTORP (v) && len == SCM_VECTOR_LENGTH (v), v, pos, FUNC_NAME); \
  } while (0)

#endif  /* SCM_VALIDATE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
