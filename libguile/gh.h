/* Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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


#ifndef __GH_H
#define __GH_H

#ifdef __cplusplus
extern "C" {
#endif

#include <libguile.h>

/* gcc has extern inline functions that are basically as fast as macros */
#ifdef __GNUC__
# define INL inline
# define EXTINL extern inline
#else
# define INL
#define EXTINL
#endif /* __GNUC__ */

void gh_enter(int argc, char *argv[], void (*c_main_prog)(int, char **));
#define gh_init () scm_init_guile ()
void gh_repl(int argc, char *argv[]);
SCM gh_catch(SCM tag, scm_catch_body_t body, void *body_data,
	     scm_catch_handler_t handler, void *handler_data);

SCM gh_standard_handler(void *data, SCM tag, SCM throw_args);

SCM gh_eval_str(const char *scheme_code);
SCM gh_eval_str_with_catch(const char *scheme_code, scm_catch_handler_t handler);
SCM gh_eval_str_with_standard_handler(const char *scheme_code);
SCM gh_eval_str_with_stack_saving_handler(const char *scheme_code);

SCM gh_eval_file(const char *fname);
#define gh_load(fname) gh_eval_file(fname)
SCM gh_eval_file_with_catch(const char *scheme_code, scm_catch_handler_t handler);
SCM gh_eval_file_with_standard_handler(const char *scheme_code);

#define gh_defer_ints() SCM_DEFER_INTS
#define gh_allow_ints() SCM_ALLOW_INTS

SCM gh_new_procedure(const char *proc_name, SCM (*fn)(),
		     int n_required_args, int n_optional_args, int varp);
SCM gh_new_procedure0_0(const char *proc_name, SCM (*fn)(void));
SCM gh_new_procedure0_1(const char *proc_name, SCM (*fn)(SCM));
SCM gh_new_procedure0_2(const char *proc_name, SCM (*fn)(SCM, SCM));
SCM gh_new_procedure1_0(const char *proc_name, SCM (*fn)(SCM));
SCM gh_new_procedure1_1(const char *proc_name, SCM (*fn)(SCM, SCM));
SCM gh_new_procedure1_2(const char *proc_name, SCM (*fn)(SCM, SCM, SCM));
SCM gh_new_procedure2_0(const char *proc_name, SCM (*fn)(SCM, SCM));
SCM gh_new_procedure2_1(const char *proc_name, SCM (*fn)(SCM, SCM, SCM));
SCM gh_new_procedure2_2(const char *proc_name, SCM (*fn)(SCM, SCM, SCM, SCM));
SCM gh_new_procedure3_0(const char *proc_name, SCM (*fn)(SCM, SCM, SCM));
SCM gh_new_procedure4_0(const char *proc_name, SCM (*fn)(SCM, SCM, SCM, SCM));
SCM gh_new_procedure5_0(const char *proc_name, SCM (*fn)(SCM, SCM, SCM, SCM, SCM));

/* C to Scheme conversion */
SCM gh_bool2scm(int x);
SCM gh_int2scm(int x);
SCM gh_ulong2scm(unsigned long x);
SCM gh_long2scm(long x);
SCM gh_double2scm(double x);
SCM gh_char2scm(char c);
SCM gh_str2scm(const char *s, int len);
SCM gh_str02scm(const char *s);
void gh_set_substr(char *src, SCM dst, int start, int len);
SCM gh_symbol2scm(const char *symbol_str);
SCM gh_ints2scm(const int *d, int n);

#ifdef HAVE_ARRAYS
SCM gh_chars2byvect(const char *d, int n);
SCM gh_shorts2svect(const short *d, int n);
SCM gh_longs2ivect(const long *d, int n);
SCM gh_ulongs2uvect(const unsigned long *d, int n);
SCM gh_floats2fvect(const float *d, int n);
SCM gh_doubles2dvect(const double *d, int n);
#endif

SCM gh_doubles2scm(const double *d, int n);

/* Scheme to C conversion */
int gh_scm2bool(SCM obj);
int gh_scm2int(SCM obj);
unsigned long gh_scm2ulong(SCM obj);
long gh_scm2long(SCM obj);
char gh_scm2char(SCM obj);
double gh_scm2double(SCM obj);
char *gh_scm2newstr(SCM str, int *lenp);
void gh_get_substr(SCM src, char *dst, int start, int len);
char *gh_symbol2newstr(SCM sym, int *lenp);
char *gh_scm2chars(SCM vector, char *result);
short *gh_scm2shorts(SCM vector, short *result);
long *gh_scm2longs(SCM vector, long *result);
float *gh_scm2floats(SCM vector, float *result);
double *gh_scm2doubles(SCM vector, double *result);

/* type predicates: tell you if an SCM object has a given type */
int gh_boolean_p(SCM val);
int gh_symbol_p(SCM val);
int gh_char_p(SCM val);
int gh_vector_p(SCM val);
int gh_pair_p(SCM val);
int gh_number_p(SCM val);
int gh_string_p(SCM val);
int gh_procedure_p(SCM val);
int gh_list_p(SCM val);
int gh_inexact_p(SCM val);
int gh_exact_p(SCM val);

/* more predicates */
int gh_eq_p(SCM x, SCM y);
int gh_eqv_p(SCM x, SCM y);
int gh_equal_p(SCM x, SCM y);
int gh_string_equal_p(SCM s1, SCM s2);
int gh_null_p(SCM l);

/* standard Scheme procedures available from C */

#define gh_not(x) scm_not(x)

SCM gh_define(const char *name, SCM val);

/* string manipulation routines */
#define gh_make_string(k, chr)       scm_make_string(k, chr)
#define gh_string_length(str)        scm_string_length(str)
#define gh_string_ref(str, k)        scm_string_ref(str, k)
#define gh_string_set_x(str, k, chr) scm_string_set_x(str, k, chr)
#define gh_substring(str, start,end) scm_substring(str, start, end)
#define gh_string_append(args)       scm_string_append(args)


/* vector manipulation routines */
/* note that gh_vector() does not behave quite like the Scheme (vector
   obj1 obj2 ...), because the interpreter engine does not pass the
   data element by element, but rather as a list.  thus, gh_vector()
   ends up being identical to gh_list_to_vector() */
#define gh_vector(ls) scm_vector(ls)
SCM gh_make_vector(SCM length, SCM val);
SCM gh_vector_set_x(SCM vec, SCM pos, SCM val);
SCM gh_vector_ref(SCM vec, SCM pos);
unsigned long gh_vector_length (SCM v);
unsigned long gh_uniform_vector_length (SCM v);
SCM gh_uniform_vector_ref (SCM v, SCM ilist);
#define gh_list_to_vector(ls) scm_vector(ls)
#define gh_vector_to_list(v) scm_vector_to_list(v)

SCM gh_lookup (const char *sname);
SCM gh_module_lookup (SCM vector, const char *sname);

SCM gh_cons(SCM x, SCM y);
#define gh_list scm_listify
unsigned long gh_length(SCM l);
SCM gh_append(SCM args);
SCM gh_append2(SCM l1, SCM l2);
SCM gh_append3(SCM l1, SCM l2, SCM l3);
SCM gh_append4(SCM l1, SCM l2, SCM l3, SCM l4);
#define gh_reverse(ls) scm_reverse(ls)
#define gh_list_tail(ls, k) scm_list_tail(ls, k)
#define gh_list_ref(ls, k) scm_list_ref(ls, k)
#define gh_memq(x, ls) scm_memq(x, ls)
#define gh_memv(x, ls) scm_memv(x, ls)
#define gh_member(x, ls) scm_member(x, ls)
#define gh_assq(x, alist) scm_assq(x, alist)
#define gh_assv(x, alist) scm_assv(x, alist)
#define gh_assoc(x, alist) scm_assoc(x, alist)

SCM gh_car(SCM x);
SCM gh_cdr(SCM x);

SCM gh_caar(SCM x);
SCM gh_cadr(SCM x);
SCM gh_cdar(SCM x);
SCM gh_cddr(SCM x);

SCM gh_caaar(SCM x);
SCM gh_caadr(SCM x);
SCM gh_cadar(SCM x);
SCM gh_caddr(SCM x);
SCM gh_cdaar(SCM x);
SCM gh_cdadr(SCM x);
SCM gh_cddar(SCM x);
SCM gh_cdddr(SCM x);

SCM gh_set_car_x(SCM pair, SCM value);
SCM gh_set_cdr_x(SCM pair, SCM value);


/* Calling Scheme functions from C.  */
SCM gh_apply (SCM proc, SCM ls);
SCM gh_call0 (SCM proc);
SCM gh_call1 (SCM proc, SCM arg);
SCM gh_call2 (SCM proc, SCM arg1, SCM arg2);
SCM gh_call3 (SCM proc, SCM arg1, SCM arg2, SCM arg3);

/* reading and writing Scheme objects.  */
void gh_display (SCM x);
void gh_write (SCM x);
void gh_newline (void);

/* void  gh_gc_mark(SCM)              : mark an SCM as in use. */
/* void  gh_defer_ints()              : don't interrupt code section. */
/* void  gh_allow_ints()              : see gh_defer_ints(). */
/* void  gh_new_cell(SCM, int tag)    : initialize SCM to be of type 'tag' */
/* int   gh_type_p(SCM, tag)          : test if SCM is of type 'tag' */
/* SCM   gh_intern(char*)             : get symbol corresponding to c-string.*/
/* void  gh_set_ext_data(SCM, void*)  : set extension data on SCM */
/* void *gh_get_ext_data(SCM)         : return extension data from SCM. */

/* void  gh_assert(int cond, char *msg, SCM obj); */

#ifdef __cplusplus
}
#endif

#endif /* __GH_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
