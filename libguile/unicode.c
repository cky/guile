/* Copyright (C) 2014 Free Software Foundation, Inc.
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see
 * <http://www.gnu.org/licenses/>.
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <ctype.h>
#include <limits.h>
#include <unicase.h>
#include <unictype.h>
#include <uniname.h>

#include "libguile/_scm.h"
#include "libguile/validate.h"

#include "libguile/unicode.h"



SCM_DEFINE (scm_char_to_formal_name, "char->formal-name", 1, 0, 0, 
            (SCM ch),
	    "Return the formal all-upper-case unicode name of @var{ch},\n"
            "as a string.  If the character has no name, return @code{#f}.")
#define FUNC_NAME s_scm_char_to_formal_name
{
  char buf[UNINAME_MAX + 1];

  SCM_VALIDATE_CHAR (1, ch);

  memset(buf, 0, UNINAME_MAX + 1);

  if (unicode_character_name (SCM_CHAR (ch), buf))
    return scm_from_latin1_string (buf);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_formal_name_to_char, "formal-name->char", 1, 0, 0, 
            (SCM name),
	    "Return the character whose formal all-upper-case unicode name is\n"
            "@var{name}, or @code{#f} if no such character is known.")
#define FUNC_NAME s_scm_formal_name_to_char
{
  char *c_name;
  scm_t_wchar ret;
  
  SCM_VALIDATE_STRING (1, name);

  c_name = scm_to_latin1_string (name);
  ret = unicode_name_character (c_name);
  free (c_name);

  return ret == UNINAME_INVALID ? SCM_BOOL_F : SCM_MAKE_CHAR (ret);
}
#undef FUNC_NAME

static void
scm_load_unicode (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/unicode.x"
#endif
}

void
scm_init_unicode (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_unicode",
                            (scm_t_extension_init_func)scm_load_unicode,
                            NULL);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
