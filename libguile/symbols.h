/* classes: h_files */

#ifndef SCM_SYMBOLS_H
#define SCM_SYMBOLS_H

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



#include "libguile/__scm.h"


/* SCM_SYMBOL_LENGTH(SYM) is the length of SYM's name in characters, and
 * SCM_SYMBOL_CHARS(SYM) is the address of the first character of SYM's name.
 */

#define SCM_SYMBOLP(x)              (!SCM_IMP (x) && (SCM_TYP7 (x) == scm_tc7_symbol))
#define SCM_SYMBOL_LENGTH(x)        (((unsigned long) SCM_CELL_WORD_0 (x)) >> 8)
#define SCM_SET_SYMBOL_LENGTH(s, l) (SCM_SET_CELL_WORD_0 ((s), ((l) << 8) + scm_tc7_symbol))
#define SCM_SYMBOL_CHARS(x)         ((char *) (SCM_CELL_WORD_1 (x)))
#define SCM_SET_SYMBOL_CHARS(s, c)  (SCM_SET_CELL_WORD_1 ((s), (c)))
#define SCM_SYMBOL_HASH(X)          ((unsigned long) SCM_CELL_WORD_2 (X))
#define SCM_SET_SYMBOL_HASH(X, v)   (SCM_SET_CELL_WORD_2 ((X), (v)))

#define SCM_PROP_SLOTS(X)           (SCM_CELL_OBJECT_3 (X))
#define SCM_SET_PROP_SLOTS(X, v)    (SCM_SET_CELL_OBJECT_3 ((X), (v)))
#define SCM_SYMBOL_FUNC(X)	    (SCM_CAR (SCM_CELL_OBJECT_3 (X)))
#define SCM_SET_SYMBOL_FUNC(X, v)   (SCM_SETCAR (SCM_CELL_OBJECT_3 (X), (v)))
#define SCM_SYMBOL_PROPS(X)	    (SCM_CDR (SCM_CELL_OBJECT_3 (X)))
#define SCM_SET_SYMBOL_PROPS(X, v)  (SCM_SETCDR (SCM_CELL_OBJECT_3 (X), (v)))



#ifdef GUILE_DEBUG
extern SCM scm_sys_symbols (void);
#endif
extern SCM scm_mem2symbol (const char*, size_t);
extern SCM scm_str2symbol (const char*);

extern SCM scm_symbol_p (SCM x);
extern SCM scm_symbol_to_string (SCM s);
extern SCM scm_string_to_symbol (SCM s);

extern SCM scm_symbol_fref (SCM s);
extern SCM scm_symbol_pref (SCM s);
extern SCM scm_symbol_fset_x (SCM s, SCM val);
extern SCM scm_symbol_pset_x (SCM s, SCM val);

extern SCM scm_symbol_hash (SCM s);
extern SCM scm_gensym (SCM prefix);

extern void scm_symbols_prehistory (void);
extern void scm_init_symbols (void);

#endif  /* SCM_SYMBOLS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
