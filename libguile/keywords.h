/* classes: h_files */

#ifndef SCM_KEYWORDS_H
#define SCM_KEYWORDS_H

/* Copyright (C) 1995,1996,1999,2000,2001, 2006, 2008 Free Software Foundation, Inc.
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



SCM_API scm_t_bits scm_tc16_keyword;



SCM_API SCM scm_keyword_p (SCM obj);
SCM_API SCM scm_symbol_to_keyword (SCM symbol);
SCM_API SCM scm_keyword_to_symbol (SCM keyword);

SCM_API int scm_is_keyword (SCM val);
SCM_API SCM scm_from_locale_keyword (const char *name);
SCM_API SCM scm_from_locale_keywordn (const char *name, size_t len);
SCM_API SCM scm_from_latin1_keyword (const char *name);
SCM_API SCM scm_from_utf8_keyword (const char *name);

enum scm_keyword_arguments_flags
{
  SCM_ALLOW_OTHER_KEYS            = (1U << 0),
  SCM_ALLOW_NON_KEYWORD_ARGUMENTS = (1U << 1)
};

typedef enum scm_keyword_arguments_flags scm_t_keyword_arguments_flags;

SCM_API void
scm_c_bind_keyword_arguments (const char *subr, SCM rest,
                              scm_t_keyword_arguments_flags flags, ...);

SCM_INTERNAL void scm_init_keywords (void);

#endif  /* SCM_KEYWORDS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
