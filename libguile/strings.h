/* classes: h_files */

#ifndef SCM_STRINGS_H
#define SCM_STRINGS_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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



#define SCM_STRINGP(x) (!SCM_IMP (x) && (SCM_TYP7 (x) == scm_tc7_string))
#define SCM_STRING_UCHARS(x) ((unsigned char *) (SCM_CELL_WORD_1 (x)))
#define SCM_STRING_CHARS(x) ((char *) (SCM_CELL_WORD_1 (x)))
#define SCM_SET_STRING_CHARS(s, c) (SCM_SET_CELL_WORD_1 ((s), (c)))
#define SCM_STRING_MAX_LENGTH ((1UL << 24) - 1UL)
#define SCM_STRING_LENGTH(x) ((size_t) (SCM_CELL_WORD_0 (x) >> 8))
#define SCM_MAKE_STRING_TAG(l) ((((scm_t_bits) (l)) << 8) + scm_tc7_string)
#define SCM_SET_STRING_LENGTH(s, l) (SCM_SET_CELL_WORD_0 ((s), SCM_MAKE_STRING_TAG (l)))



SCM_API SCM scm_string_p (SCM x);
SCM_API SCM scm_string (SCM chrs);
SCM_API SCM scm_makfromstrs (int argc, char **argv);
SCM_API SCM scm_take_str (char *s, size_t len);
SCM_API SCM scm_take0str (char *s);
SCM_API SCM scm_mem2string (const char *src, size_t len);
SCM_API SCM scm_str2string (const char *src);
SCM_API SCM scm_makfrom0str (const char *src);
SCM_API SCM scm_makfrom0str_opt (const char *src);
SCM_API SCM scm_allocate_string (size_t len);
SCM_API SCM scm_make_string (SCM k, SCM chr);
SCM_API SCM scm_string_length (SCM str);
SCM_API SCM scm_string_ref (SCM str, SCM k);
SCM_API SCM scm_string_set_x (SCM str, SCM k, SCM chr);
SCM_API SCM scm_substring (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_append (SCM args);
SCM_API void scm_init_strings (void);
SCM_API char *scm_c_string2str (SCM obj, char *str, size_t *lenp);
SCM_API char *scm_c_substring2str (SCM obj, char *str, size_t start, size_t len);



#if (SCM_ENABLE_DEPRECATED == 1)

#define SCM_STRING_COERCE_0TERMINATION_X(x) (x)

#endif

#endif  /* SCM_STRINGS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
