/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* A substitute <strings.h>.

   Copyright (C) 2007-2008 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _GL_STRINGS_H

#if __GNUC__ >= 3
#pragma GCC system_header
#endif

/* The include_next requires a split double-inclusion guard.  */
#include_next <strings.h>

#ifndef _GL_STRINGS_H
#define _GL_STRINGS_H


/* The definition of GL_LINK_WARNING is copied here.  */
/* GL_LINK_WARNING("literal string") arranges to emit the literal string as
   a linker warning on most glibc systems.
   We use a linker warning rather than a preprocessor warning, because
   #warning cannot be used inside macros.  */
#ifndef GL_LINK_WARNING
  /* This works on platforms with GNU ld and ELF object format.
     Testing __GLIBC__ is sufficient for asserting that GNU ld is in use.
     Testing __ELF__ guarantees the ELF object format.
     Testing __GNUC__ is necessary for the compound expression syntax.  */
# if defined __GLIBC__ && defined __ELF__ && defined __GNUC__
#  define GL_LINK_WARNING(message) \
     GL_LINK_WARNING1 (__FILE__, __LINE__, message)
#  define GL_LINK_WARNING1(file, line, message) \
     GL_LINK_WARNING2 (file, line, message)  /* macroexpand file and line */
#  define GL_LINK_WARNING2(file, line, message) \
     GL_LINK_WARNING3 (file ":" #line ": warning: " message)
#  define GL_LINK_WARNING3(message) \
     ({ static const char warning[sizeof (message)]		\
          __attribute__ ((__unused__,				\
                          __section__ (".gnu.warning"),		\
                          __aligned__ (1)))			\
          = message "\n";					\
        (void)0;						\
     })
# else
#  define GL_LINK_WARNING(message) ((void) 0)
# endif
#endif


#ifdef __cplusplus
extern "C" {
#endif


/* Compare strings S1 and S2, ignoring case, returning less than, equal to or
   greater than zero if S1 is lexicographically less than, equal to or greater
   than S2.
   Note: This function does not work in multibyte locales.  */
#if ! 1
extern int strcasecmp (char const *s1, char const *s2);
#endif
#if defined GNULIB_POSIXCHECK
/* strcasecmp() does not work with multibyte strings:
   POSIX says that it operates on "strings", and "string" in POSIX is defined
   as a sequence of bytes, not of characters.   */
# undef strcasecmp
# define strcasecmp(a,b) \
    (GL_LINK_WARNING ("strcasecmp cannot work correctly on character strings " \
                      "in multibyte locales - " \
                      "use mbscasecmp if you care about " \
                      "internationalization, or use c_strcasecmp (from " \
                      "gnulib module c-strcase) if you want a locale " \
                      "independent function"), \
     strcasecmp (a, b))
#endif

/* Compare no more than N bytes of strings S1 and S2, ignoring case,
   returning less than, equal to or greater than zero if S1 is
   lexicographically less than, equal to or greater than S2.
   Note: This function cannot work correctly in multibyte locales.  */
#if ! 1
extern int strncasecmp (char const *s1, char const *s2, size_t n);
#endif
#if defined GNULIB_POSIXCHECK
/* strncasecmp() does not work with multibyte strings:
   POSIX says that it operates on "strings", and "string" in POSIX is defined
   as a sequence of bytes, not of characters.  */
# undef strncasecmp
# define strncasecmp(a,b,n) \
    (GL_LINK_WARNING ("strncasecmp cannot work correctly on character " \
                      "strings in multibyte locales - " \
                      "use mbsncasecmp or mbspcasecmp if you care about " \
                      "internationalization, or use c_strncasecmp (from " \
                      "gnulib module c-strcase) if you want a locale " \
                      "independent function"), \
     strncasecmp (a, b, n))
#endif


#ifdef __cplusplus
}
#endif

#endif /* _GL_STRING_H */
#endif /* _GL_STRING_H */
