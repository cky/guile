/* classes: h_files */

#ifndef SYMBOLSH
#define SYMBOLSH
/*	Copyright (C) 1995,1996,1997,1998, 2000 Free Software Foundation, Inc.
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


extern int scm_symhash_dim;

/* SCM_SYMBOL_LENGTH(SYM) is the length of SYM's name in characters, and
 * SCM_SYMBOL_CHARS(SYM) is the address of the first character of SYM's name.
 */

#define SCM_SYMBOLP(x)	(SCM_NIMP (x) && (SCM_TYP7 (x) == scm_tc7_symbol))
#define SCM_SYMBOL_UCHARS(x)  ((unsigned char *) (SCM_CELL_WORD_1 (x)))
#define SCM_SYMBOL_CHARS(x)  ((char *) (SCM_CELL_WORD_1 (x)))
#define SCM_SYMBOL_LENGTH(x)  (((unsigned long) SCM_CELL_WORD_0 (x)) >> 8)

#define SCM_LENGTH_MAX		(0xffffffL)
#define SCM_SETLENGTH(x, v, t)	(SCM_SET_CELL_WORD_0 ((x), ((v) << 8) + (t)))

#define SCM_SETCHARS(x, v)	(SCM_SET_CELL_WORD_1 ((x), (scm_bits_t) (v)))

#define SCM_PROP_SLOTS(X)           (SCM_CELL_WORD_3 (X))
#define SCM_SET_PROP_SLOTS(X, v)    (SCM_SET_CELL_WORD_3 ((X), (v)))
#define SCM_SYMBOL_FUNC(X)	    (SCM_CAR (SCM_CELL_WORD_3 (X)))
#define SCM_SET_SYMBOL_FUNC(X, v)   (SCM_SETCAR (SCM_CELL_WORD_3 (X), (v)))
#define SCM_SYMBOL_PROPS(X)	    (SCM_CDR (SCM_CELL_WORD_3 (X)))
#define SCM_SET_SYMBOL_PROPS(X, v)  (SCM_SETCDR (SCM_CELL_WORD_3 (X), (v)))
#define SCM_SYMBOL_HASH(X)	    (SCM_CELL_WORD_2 (X))
#define SCM_SET_SYMBOL_HASH(X, v)   (SCM_SET_CELL_WORD_2 ((X), (v)))



extern unsigned long scm_string_hash (const unsigned char *str, scm_sizet len);
extern SCM scm_sym2vcell (SCM sym, SCM thunk, SCM definep);
extern SCM scm_sym2ovcell_soft (SCM sym, SCM obarray);
extern SCM scm_sym2ovcell (SCM sym, SCM obarray);
extern SCM scm_intern_obarray_soft (const char *name, scm_sizet len, SCM obarray, unsigned int softness);
extern SCM scm_intern_obarray (const char *name, scm_sizet len, SCM obarray);
extern SCM scm_intern (const char *name, scm_sizet len);
extern SCM scm_intern0 (const char *name);
extern SCM scm_sysintern (const char *name, SCM val);
extern SCM scm_sysintern0 (const char *name);
extern SCM scm_sysintern0_no_module_lookup (const char *name);
extern SCM scm_symbol_value0 (const char *name);
extern SCM scm_symbol_p (SCM x);
extern SCM scm_symbol_to_string (SCM s);
extern SCM scm_string_to_symbol (SCM s);
extern SCM scm_string_to_obarray_symbol (SCM o, SCM s, SCM softp);
extern SCM scm_intern_symbol (SCM o, SCM s);
extern SCM scm_unintern_symbol (SCM o, SCM s);
extern SCM scm_symbol_binding (SCM o, SCM s);
extern SCM scm_symbol_interned_p (SCM o, SCM s);
extern SCM scm_symbol_bound_p (SCM o, SCM s);
extern SCM scm_symbol_set_x (SCM o, SCM s, SCM v);
extern SCM scm_symbol_fref (SCM s);
extern SCM scm_symbol_pref (SCM s);
extern SCM scm_symbol_fset_x (SCM s, SCM val);
extern SCM scm_symbol_pset_x (SCM s, SCM val);
extern SCM scm_symbol_hash (SCM s);
extern SCM scm_builtin_bindings (void);
extern SCM scm_builtin_weak_bindings (void);
extern SCM scm_gensym (SCM prefix);
extern SCM scm_gentemp (SCM prefix, SCM obarray);
extern void scm_init_symbols (void);



#if (SCM_DEBUG_DEPRECATED == 0)

#define SCM_CHARS(x) ((char *) (SCM_CELL_WORD_1 (x)))
#define SCM_UCHARS(x) ((unsigned char *) (SCM_CELL_WORD_1 (x)))
#define SCM_SLOPPY_SUBSTRP(x) (SCM_SUBSTRP (x))
#define SCM_SUBSTR_STR(x) (SCM_CDDR (x))
#define SCM_SUBSTR_OFFSET(x) (SCM_CADR (x))
#define SCM_LENGTH(x) (((unsigned long) SCM_CELL_WORD_0 (x)) >> 8)
#define SCM_ROSTRINGP(x) (SCM_NIMP(x) && ((SCM_TYP7S(x)==scm_tc7_string) \
			  || (SCM_TYP7(x) == scm_tc7_symbol)))
#define SCM_ROLENGTH(x) SCM_LENGTH (x)
#define SCM_ROCHARS(x) ((SCM_TYP7 (x) == scm_tc7_substring) \
			? (SCM_INUM (SCM_CADR (x)) + SCM_STRING_CHARS (SCM_CDDR (x))) \
			: ((SCM_TYP7 (x) == scm_tc7_string) \
			   ? SCM_STRING_CHARS (x) \
			   : SCM_SYMBOL_CHARS (x)))
#define SCM_ROUCHARS(x) ((SCM_TYP7 (x) == scm_tc7_substring) \
			 ? (SCM_INUM (SCM_CADR (x)) + SCM_STRING_UCHARS (SCM_CDDR (x))) \
			 : ((SCM_TYP7 (x) == scm_tc7_string) \
			    ? SCM_STRING_UCHARS (x) \
			    : SCM_SYMBOL_UCHARS (x)))
#define SCM_SUBSTRP(x) (SCM_NIMP (x) && (SCM_TYP7 (x) == scm_tc7_substring))
#define SCM_COERCE_SUBSTR(x) SCM_STRING_COERCE_0TERMINATION_X (x)
#define scm_strhash(str, len, n) (scm_string_hash ((str), (len)) % (n))

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

#endif  /* SYMBOLSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
