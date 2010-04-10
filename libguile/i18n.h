/* classes: h_files */

#ifndef SCM_I18N_H
#define SCM_I18N_H

/* Copyright (C) 2006, 2008, 2009 Free Software Foundation, Inc.
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

SCM_API SCM scm_global_locale;
SCM_API SCM scm_make_locale (SCM category_mask, SCM locale_name, SCM base_locale);
SCM_API SCM scm_locale_p (SCM obj);
SCM_API SCM scm_string_locale_lt (SCM s1, SCM s2, SCM locale);
SCM_API SCM scm_string_locale_gt (SCM s1, SCM s2, SCM locale);
SCM_API SCM scm_string_locale_ci_lt (SCM s1, SCM s2, SCM locale);
SCM_API SCM scm_string_locale_ci_gt (SCM s1, SCM s2, SCM locale);
SCM_API SCM scm_string_locale_ci_eq (SCM s1, SCM s2, SCM locale);
SCM_API SCM scm_char_locale_lt (SCM c1, SCM c2, SCM locale);
SCM_API SCM scm_char_locale_gt (SCM c1, SCM c2, SCM locale);
SCM_API SCM scm_char_locale_ci_lt (SCM c1, SCM c2, SCM locale);
SCM_API SCM scm_char_locale_ci_gt (SCM c1, SCM c2, SCM locale);
SCM_API SCM scm_char_locale_ci_eq (SCM c1, SCM c2, SCM locale);
SCM_API SCM scm_char_locale_upcase (SCM chr, SCM locale);
SCM_API SCM scm_char_locale_downcase (SCM chr, SCM locale);
SCM_API SCM scm_char_locale_titlecase (SCM chr, SCM locale);
SCM_API SCM scm_string_locale_upcase (SCM chr, SCM locale);
SCM_API SCM scm_string_locale_downcase (SCM chr, SCM locale);
SCM_API SCM scm_string_locale_titlecase (SCM chr, SCM locale);
SCM_API SCM scm_locale_string_to_integer (SCM str, SCM base, SCM locale);
SCM_API SCM scm_locale_string_to_inexact (SCM str, SCM locale);

SCM_INTERNAL SCM scm_nl_langinfo (SCM item, SCM locale);

SCM_INTERNAL void scm_init_i18n (void);
SCM_INTERNAL void scm_bootstrap_i18n (void);


#endif  /* SCM_I18N_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
