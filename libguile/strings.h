/* classes: h_files */

#ifndef SCM_STRINGS_H
#define SCM_STRINGS_H

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



#define SCM_STRINGP(x) (!SCM_IMP (x) && (SCM_TYP7 (x) == scm_tc7_string))
#define SCM_STRING_UCHARS(x) ((unsigned char *) (SCM_CELL_WORD_1 (x)))
#define SCM_STRING_CHARS(x) ((char *) (SCM_CELL_WORD_1 (x)))
#define SCM_SET_STRING_CHARS(s, c) (SCM_SET_CELL_WORD_1 ((s), (c)))
#define SCM_STRING_MAX_LENGTH ((1UL << 24) - 1UL)
#define SCM_STRING_LENGTH(x) ((size_t) (SCM_CELL_WORD_0 (x) >> 8))
#define SCM_SET_STRING_LENGTH(s, l) (SCM_SET_CELL_WORD_0 ((s), (((long) (l)) << 8) + scm_tc7_string))



extern SCM scm_string_p (SCM x);
extern SCM scm_string (SCM chrs);
extern SCM scm_makfromstrs (int argc, char **argv);
extern SCM scm_take_str (char *s, size_t len);
extern SCM scm_take0str (char *s);
extern SCM scm_mem2string (const char *src, size_t len);
extern SCM scm_str2string (const char *src);
extern SCM scm_makfrom0str (const char *src);
extern SCM scm_makfrom0str_opt (const char *src);
extern SCM scm_allocate_string (size_t len);
extern SCM scm_make_string (SCM k, SCM chr);
extern SCM scm_string_length (SCM str);
extern SCM scm_string_ref (SCM str, SCM k);
extern SCM scm_string_set_x (SCM str, SCM k, SCM chr);
extern SCM scm_substring (SCM str, SCM start, SCM end);
extern SCM scm_string_append (SCM args);
extern void scm_init_strings (void);



#if (SCM_ENABLE_DEPRECATED == 1)

#define SCM_STRING_COERCE_0TERMINATION_X(x) (x)

#endif

#endif  /* SCM_STRINGS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
