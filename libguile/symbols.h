/* classes: h_files */

#ifndef SCM_SYMBOLS_H
#define SCM_SYMBOLS_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2003 Free Software Foundation, Inc.
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


/* SCM_SYMBOL_LENGTH(SYM) is the length of SYM's name in characters, and
 * SCM_SYMBOL_CHARS(SYM) is the address of the first character of SYM's name.
 * 
 * SCM_SYMBOL_HASH is a hash value for the symbol.  It is also used to
 * encode whether the symbol is interned or not.  See
 * SCM_SYMBOL_INTERNED_P.
 */

#define SCM_SYMBOLP(x)              (!SCM_IMP (x) && (SCM_TYP7 (x) == scm_tc7_symbol))
#define SCM_SYMBOL_LENGTH(x)        (((unsigned long) SCM_CELL_WORD_0 (x)) >> 8)
#define SCM_MAKE_SYMBOL_TAG(l)      (((l) << 8) + scm_tc7_symbol)
#define SCM_SET_SYMBOL_LENGTH(s, l) (SCM_SET_CELL_WORD_0 ((s), SCM_MAKE_SYMBOL_TAG(l)))
#define SCM_SYMBOL_CHARS(x)         ((char *) (SCM_CELL_WORD_1 (x)))
#define SCM_SET_SYMBOL_CHARS(s, c)  (SCM_SET_CELL_WORD_1 ((s), (c)))
#define SCM_SYMBOL_HASH(X)          ((unsigned long) SCM_CELL_WORD_2 (X))
#define SCM_SYMBOL_INTERNED_P(X)    (SCM_SYMBOL_HASH(X) <= (SCM_T_BITS_MAX/2))

#define SCM_PROP_SLOTS(X)           (SCM_CELL_OBJECT_3 (X))
#define SCM_SET_PROP_SLOTS(X, v)    (SCM_SET_CELL_OBJECT_3 ((X), (v)))
#define SCM_SYMBOL_FUNC(X)	    (SCM_CAR (SCM_CELL_OBJECT_3 (X)))
#define SCM_SET_SYMBOL_FUNC(X, v)   (SCM_SETCAR (SCM_CELL_OBJECT_3 (X), (v)))
#define SCM_SYMBOL_PROPS(X)	    (SCM_CDR (SCM_CELL_OBJECT_3 (X)))
#define SCM_SET_SYMBOL_PROPS(X, v)  (SCM_SETCDR (SCM_CELL_OBJECT_3 (X), (v)))



#ifdef GUILE_DEBUG
SCM_API SCM scm_sys_symbols (void);
#endif
SCM_API unsigned long scm_i_hash_symbol (SCM obj, unsigned long n, void *closure);
SCM_API SCM scm_mem2symbol (const char*, size_t);
SCM_API SCM scm_mem2uninterned_symbol (const char *name, size_t len);
SCM_API SCM scm_str2symbol (const char*);

SCM_API SCM scm_symbol_p (SCM x);
SCM_API SCM scm_symbol_interned_p (SCM sym);
SCM_API SCM scm_make_symbol (SCM name);
SCM_API SCM scm_symbol_to_string (SCM s);
SCM_API SCM scm_string_to_symbol (SCM s);

SCM_API SCM scm_symbol_fref (SCM s);
SCM_API SCM scm_symbol_pref (SCM s);
SCM_API SCM scm_symbol_fset_x (SCM s, SCM val);
SCM_API SCM scm_symbol_pset_x (SCM s, SCM val);

SCM_API SCM scm_symbol_hash (SCM s);
SCM_API SCM scm_gensym (SCM prefix);

SCM_API char *scm_c_symbol2str (SCM obj, char *str, size_t *lenp);
SCM_API void scm_symbols_prehistory (void);
SCM_API void scm_init_symbols (void);

#endif  /* SCM_SYMBOLS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
