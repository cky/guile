/* classes: h_files */

#ifndef SYMBOLSH
#define SYMBOLSH
/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <libguile/__scm.h>


extern int scm_symhash_dim;

#define SCM_SYMBOLP(x) (SCM_TYP7S(x)==scm_tc7_ssymbol)
#define SCM_LENGTH(x) (((unsigned long)SCM_CAR(x))>>8)
#define SCM_LENGTH_MAX (0xffffffL)
#define SCM_SETLENGTH(x, v, t) SCM_CAR(x) = ((v)<<8)+(t)
#define SCM_SETCHARS SCM_SETCDR
#define SCM_CHARS(x) ((char *)(SCM_CDR(x)))
#define SCM_UCHARS(x) ((unsigned char *)(SCM_CDR(x)))
#define SCM_SLOTS(x) ((SCM *) (* ((SCM *)SCM_CHARS(x) - 1)))
#define SCM_SYMBOL_SLOTS 5
#define SCM_SYMBOL_FUNC(X) (SCM_SLOTS(X)[0])
#define SCM_SYMBOL_PROPS(X) (SCM_SLOTS(X)[1])
#define SCM_SYMBOL_HASH(X) (*(unsigned long*)(&SCM_SLOTS(X)[2]))
#define SCM_SYMBOL_MULTI_BYTE_SCM_STRINGP(X) (*(unsigned long*)(&SCM_SLOTS(X)[3]))

#define SCM_ROSTRINGP(x) ((SCM_TYP7SD(x)==scm_tc7_string) || (SCM_TYP7S(x) == scm_tc7_ssymbol))
#define SCM_ROCHARS(x) ((SCM_TYP7(x) == scm_tc7_substring) \
			? SCM_INUM (SCM_CADR (x)) + SCM_CHARS (SCM_CDDR (x))  \
			: SCM_CHARS (x))
#define SCM_ROUCHARS(x) ((SCM_TYP7(x) == scm_tc7_substring) \
			 ? SCM_INUM (SCM_CADR (x)) + SCM_UCHARS (SCM_CDDR (x))  \
			 : SCM_UCHARS (x))
#define SCM_ROLENGTH(x) SCM_LENGTH (x)
#define SCM_SUBSTRP(x) ((SCM_TYP7S(x) == scm_tc7_substring))
#define SCM_SUBSTR_STR(x) (SCM_CDDR (x))
#define SCM_SUBSTR_OFFSET(x) (SCM_CADR (x))



#ifdef __STDC__
extern unsigned long scm_strhash (unsigned char *str, scm_sizet len, unsigned long n);
extern SCM scm_sym2vcell (SCM sym, SCM thunk, SCM definep);
extern SCM scm_sym2ovcell_soft (SCM sym, SCM obarray);
extern SCM scm_sym2ovcell (SCM sym, SCM obarray);
extern SCM scm_intern_obarray_soft (char *name, scm_sizet len, SCM obarray, int softness);
extern SCM scm_intern_obarray (char *name, scm_sizet len, SCM obarray);
extern SCM scm_intern (char *name, scm_sizet len);
extern SCM scm_intern0 (char * name);
extern SCM scm_sysintern (char *name, SCM val);
extern SCM scm_symbol_p(SCM x);
extern SCM scm_symbol_to_string(SCM s);
extern SCM scm_string_to_symbol(SCM s);
extern SCM scm_string_to_obarray_symbol(SCM o, SCM s, SCM softp);
extern SCM scm_intern_symbol(SCM o, SCM s);
extern SCM scm_unintern_symbol(SCM o, SCM s);
extern SCM scm_symbol_binding (SCM o, SCM s);
extern SCM scm_symbol_interned_p (SCM o, SCM s);
extern SCM scm_symbol_bound_p (SCM o, SCM s);
extern SCM scm_symbol_set_x (SCM o, SCM s, SCM v);
extern SCM scm_symbol_fref (SCM s);
extern SCM scm_symbol_pref (SCM s);
extern SCM scm_symbol_fset_x (SCM s, SCM val);
extern SCM scm_symbol_pset_x (SCM s, SCM val);
extern SCM scm_symbol_hash (SCM s);
extern void scm_init_symbols (void);

#else /* STDC */
extern unsigned long scm_strhash ();
extern SCM scm_sym2vcell ();
extern SCM scm_sym2ovcell_soft ();
extern SCM scm_sym2ovcell ();
extern SCM scm_intern_obarray_soft ();
extern SCM scm_intern_obarray ();
extern SCM scm_intern ();
extern SCM scm_intern0 ();
extern SCM scm_sysintern ();
extern SCM scm_symbol_p();
extern SCM scm_symbol_to_string();
extern SCM scm_string_to_symbol();
extern SCM scm_string_to_obarray_symbol();
extern SCM scm_intern_symbol();
extern SCM scm_unintern_symbol();
extern SCM scm_symbol_binding ();
extern SCM scm_symbol_interned_p ();
extern SCM scm_symbol_bound_p ();
extern SCM scm_symbol_set_x ();
extern SCM scm_symbol_fref ();
extern SCM scm_symbol_pref ();
extern SCM scm_symbol_fset_x ();
extern SCM scm_symbol_pset_x ();
extern SCM scm_symbol_hash ();
extern void scm_init_symbols ();

#endif /* STDC */






#endif  /* SYMBOLSH */
