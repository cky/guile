/* Copyright (C) 2006 Free Software Foundation, Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#define _GNU_SOURCE /* Ask for glibc's `newlocale' API */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if HAVE_ALLOCA_H
# include <alloca.h>
#elif defined __GNUC__
# define alloca __builtin_alloca
#elif defined _AIX
# define alloca __alloca
#elif defined _MSC_VER
# include <malloc.h>
# define alloca _alloca
#else
# include <stddef.h>
# ifdef  __cplusplus
extern "C"
# endif
void *alloca (size_t);
#endif

#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/i18n.h"
#include "libguile/strings.h"
#include "libguile/chars.h"
#include "libguile/dynwind.h"
#include "libguile/validate.h"
#include "libguile/values.h"

#include <locale.h>
#include <string.h> /* `strcoll ()' */
#include <ctype.h>  /* `toupper ()' et al. */
#include <errno.h>

#if (defined HAVE_NEWLOCALE) && (defined HAVE_STRCOLL_L)
# define USE_GNU_LOCALE_API
#endif

#ifndef USE_GNU_LOCALE_API
# include "libguile/posix.h"  /* for `scm_i_locale_mutex' */
#endif

#ifndef HAVE_SETLOCALE
static inline char *
setlocale (int category, const char *name)
{
  errno = ENOSYS;
  return NULL;
}
#endif



/* Locale objects, string and character collation, and other locale-dependent
   string operations.

   A large part of the code here deals with emulating glibc's reentrant
   locale API on non-GNU systems.  The emulation is a bit "brute-force":
   Whenever a `-locale<?' procedure is passed a locale object, then:

   1. The `scm_t_locale_mutex' is locked.
   2. A series of `setlocale ()' call is performed to store the current
      locale for each category in an `scm_t_locale_settings' object.
   3. A series of `setlocale ()' call is made to install each of the locale
      categories of each of the base locales of each locale object,
      recursively, starting from the last locale object of the chain.
   4. The settings captured in step (2) are restored.
   5. The `scm_t_locale_mutex' is released.

   Hopefully, some smart standard will make that hack useless someday...
   A similar API can be found in MzScheme starting from version 200:
   http://download.plt-scheme.org/chronology/mzmr200alpha14.html .

   Note: We don't wrap glibc's `uselocale ()' call because it sets the locale
   of the current _thread_ (unlike `setlocale ()') and doing so would require
   maintaining per-thread locale information on non-GNU systems and always
   re-installing this locale upon locale-dependent calls.  */


#ifndef USE_GNU_LOCALE_API

/* Provide the locale category masks as found in glibc (copied from
   <locale.h> as found in glibc 2.3.6).  This must be kept in sync with
   `locale-categories.h'.  */

# define LC_CTYPE_MASK		(1 << LC_CTYPE)
# define LC_COLLATE_MASK	(1 << LC_COLLATE)
# define LC_MESSAGES_MASK	(1 << LC_MESSAGES)
# define LC_MONETARY_MASK	(1 << LC_MONETARY)
# define LC_NUMERIC_MASK	(1 << LC_NUMERIC)
# define LC_TIME_MASK		(1 << LC_TIME)

# ifdef LC_PAPER
#   define LC_PAPER_MASK	(1 << LC_PAPER)
# else
#   define LC_PAPER_MASK	0
# endif
# ifdef LC_NAME
#   define LC_NAME_MASK		(1 << LC_NAME)
# else
#   define LC_NAME_MASK		0
# endif
# ifdef LC_ADDRESS
#   define LC_ADDRESS_MASK	(1 << LC_ADDRESS)
# else
#   define LC_ADDRESS_MASK	0
# endif
# ifdef LC_TELEPHONE
#   define LC_TELEPHONE_MASK	(1 << LC_TELEPHONE)
# else
#   define LC_TELEPHONE_MASK	0
# endif
# ifdef LC_MEASUREMENT
#   define LC_MEASUREMENT_MASK	(1 << LC_MEASUREMENT)
# else
#   define LC_MEASUREMENT_MASK	0
# endif
# ifdef LC_IDENTIFICATION
#   define LC_IDENTIFICATION_MASK (1 << LC_IDENTIFICATION)
# else
#   define LC_IDENTIFICATION_MASK 0
# endif

# define LC_ALL_MASK		(LC_CTYPE_MASK \
				 | LC_NUMERIC_MASK \
				 | LC_TIME_MASK \
				 | LC_COLLATE_MASK \
				 | LC_MONETARY_MASK \
				 | LC_MESSAGES_MASK \
				 | LC_PAPER_MASK \
				 | LC_NAME_MASK \
				 | LC_ADDRESS_MASK \
				 | LC_TELEPHONE_MASK \
				 | LC_MEASUREMENT_MASK \
				 | LC_IDENTIFICATION_MASK \
				 )

/* Locale objects as returned by `make-locale' on non-GNU systems.  */
typedef struct scm_locale
{
  SCM   base_locale; /* a `locale' object */
  char *locale_name;
  int   category_mask;
} *scm_t_locale;

#else

/* Alias for glibc's locale type.  */
typedef locale_t scm_t_locale;

#endif

/* Validate parameter ARG as a locale object and set C_LOCALE to the
   corresponding C locale object.  */
#define SCM_VALIDATE_LOCALE_COPY(_pos, _arg, _c_locale)		\
  do								\
    {								\
      SCM_VALIDATE_SMOB ((_pos), (_arg), locale_smob_type);	\
      (_c_locale) = (scm_t_locale)SCM_SMOB_DATA (_arg);		\
    }								\
  while (0)

/* Validate optional parameter ARG as either undefined or bound to a locale
   object.  Set C_LOCALE to the corresponding C locale object or NULL.  */
#define SCM_VALIDATE_OPTIONAL_LOCALE_COPY(_pos, _arg, _c_locale)	\
  do									\
    {									\
      if ((_arg) != SCM_UNDEFINED)					\
	SCM_VALIDATE_LOCALE_COPY (_pos, _arg, _c_locale);		\
      else								\
	(_c_locale) = NULL;						\
    }									\
  while (0)


SCM_SMOB (scm_tc16_locale_smob_type, "locale", 0);

SCM_SMOB_FREE (scm_tc16_locale_smob_type, smob_locale_free, locale)
{
  scm_t_locale c_locale;

  c_locale = (scm_t_locale)SCM_SMOB_DATA (locale);

#ifdef USE_GNU_LOCALE_API
  freelocale ((locale_t)c_locale);
#else
  c_locale->base_locale = SCM_UNDEFINED;
  free (c_locale->locale_name);

  scm_gc_free (c_locale, sizeof (* c_locale), "locale");
#endif

  return 0;
}

#ifndef USE_GNU_LOCALE_API
static SCM
smob_locale_mark (SCM locale)
{
  scm_t_locale c_locale;

  c_locale = (scm_t_locale)SCM_SMOB_DATA (locale);
  return (c_locale->base_locale);
}
#endif


SCM_DEFINE (scm_make_locale, "make-locale", 2, 1, 0,
	    (SCM category_mask, SCM locale_name, SCM base_locale),
	    "Return a reference to a data structure representing a set of "
	    "locale datasets.  Unlike for the @var{category} parameter for "
	    "@code{setlocale}, the @var{category_mask} parameter here uses "
	    "a single bit for each category, made by OR'ing together "
	    "@code{LC_*_MASK} bits.")
#define FUNC_NAME s_scm_make_locale
{
  SCM locale = SCM_BOOL_F;
  int c_category_mask;
  char *c_locale_name;
  scm_t_locale c_base_locale, c_locale;

  SCM_VALIDATE_INT_COPY (1, category_mask, c_category_mask);
  SCM_VALIDATE_STRING (2, locale_name);
  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (3, base_locale, c_base_locale);

  c_locale_name = scm_to_locale_string (locale_name);

#ifdef USE_GNU_LOCALE_API

  c_locale = newlocale (c_category_mask, c_locale_name, c_base_locale);

  if (!c_locale)
    locale = SCM_BOOL_F;
  else
    SCM_NEWSMOB (locale, scm_tc16_locale_smob_type, c_locale);

  free (c_locale_name);

#else

  c_locale = scm_gc_malloc (sizeof (* c_locale), "locale");
  c_locale->base_locale = base_locale;

  c_locale->category_mask = c_category_mask;
  c_locale->locale_name = c_locale_name;

  SCM_NEWSMOB (locale, scm_tc16_locale_smob_type, c_locale);

#endif

  return locale;
}
#undef FUNC_NAME

SCM_DEFINE (scm_locale_p, "locale?", 1, 0, 0,
	    (SCM obj),
	    "Return true if @var{obj} is a locale object.")
#define FUNC_NAME s_scm_locale_p
{
  if (SCM_SMOB_PREDICATE (scm_tc16_locale_smob_type, obj))
    return SCM_BOOL_T;

  return SCM_BOOL_F;
}
#undef FUNC_NAME



#ifndef USE_GNU_LOCALE_API  /* Emulate GNU's reentrant locale API.  */


/* Maximum number of chained locales (via `base_locale').  */
#define LOCALE_STACK_SIZE_MAX  256

typedef struct
{
#define SCM_DEFINE_LOCALE_CATEGORY(_name)  char * _name;
#include "locale-categories.h"
#undef SCM_DEFINE_LOCALE_CATEGORY
} scm_t_locale_settings;

/* Fill out SETTINGS according to the current locale settings.  On success
   zero is returned and SETTINGS is properly initialized.  */
static int
get_current_locale_settings (scm_t_locale_settings *settings)
{
  const char *locale_name;

#define SCM_DEFINE_LOCALE_CATEGORY(_name)			\
  {								\
    SCM_SYSCALL (locale_name = setlocale (LC_ ## _name, NULL));	\
    if (!locale_name)						\
      goto handle_error;					\
								\
    settings-> _name = strdup (locale_name);			\
    if (settings-> _name == NULL)				\
      goto handle_oom;						\
  }

#include "locale-categories.h"
#undef SCM_DEFINE_LOCALE_CATEGORY

  return 0;

 handle_error:
  return errno;

 handle_oom:
  return ENOMEM;
}

/* Restore locale settings SETTINGS.  On success, return zero.  */
static int
restore_locale_settings (const scm_t_locale_settings *settings)
{
  const char *result;

#define SCM_DEFINE_LOCALE_CATEGORY(_name)				\
  SCM_SYSCALL (result = setlocale (LC_ ## _name, settings-> _name));	\
  if (result == NULL)							\
    goto handle_error;

#include "locale-categories.h"
#undef SCM_DEFINE_LOCALE_CATEGORY

  return 0;

 handle_error:
  return errno;
}

/* Free memory associated with SETTINGS.  */
static void
free_locale_settings (scm_t_locale_settings *settings)
{
#define SCM_DEFINE_LOCALE_CATEGORY(_name)	\
  free (settings-> _name);			\
  settings->_name = NULL;
#include  "locale-categories.h"
#undef SCM_DEFINE_LOCALE_CATEGORY
}

/* Install the locale named LOCALE_NAME for all the categories listed in
   CATEGORY_MASK.  */
static int
install_locale_categories (const char *locale_name, int category_mask)
{
  const char *result;

  if (category_mask == LC_ALL_MASK)
    {
      SCM_SYSCALL (result = setlocale (LC_ALL, locale_name));
      if (result == NULL)
	goto handle_error;
    }
  else
    {
#define SCM_DEFINE_LOCALE_CATEGORY(_name)				\
  if (category_mask & LC_ ## _name ## _MASK)				\
    {									\
      SCM_SYSCALL (result = setlocale (LC_ ## _name, locale_name));	\
      if (result == NULL)						\
	goto handle_error;						\
    }
#include "locale-categories.h"
#undef SCM_DEFINE_LOCALE_CATEGORY
    }

  return 0;

 handle_error:
  return errno;
}

/* Install LOCALE, recursively installing its base locales first.  On
   success, zero is returned.  */
static int
install_locale (scm_t_locale locale)
{
  scm_t_locale stack[LOCALE_STACK_SIZE_MAX];
  size_t stack_size = 0;
  int stack_offset = 0;
  const char *result = NULL;

  /* Build up a locale stack by traversing the `base_locale' link.  */
  do
    {
      if (stack_size >= LOCALE_STACK_SIZE_MAX)
	/* We cannot use `scm_error ()' here because otherwise the locale
	   mutex may remain locked.  */
	return EINVAL;

      stack[stack_size++] = locale;

      if (locale->base_locale != SCM_UNDEFINED)
	locale = (scm_t_locale)SCM_SMOB_DATA (locale->base_locale);
      else
	locale = NULL;
    }
  while (locale != NULL);

  /* Install the C locale to start from a pristine state.  */
  SCM_SYSCALL (result = setlocale (LC_ALL, "C"));
  if (result == NULL)
    goto handle_error;

  /* Install the locales in reverse order.  */
  for (stack_offset = stack_size - 1;
       stack_offset >= 0;
       stack_offset--)
    {
      int err;
      scm_t_locale locale;

      locale = stack[stack_offset];
      err = install_locale_categories (locale->locale_name,
				       locale->category_mask);
      if (err)
	goto handle_error;
    }

  return 0;

 handle_error:
  return errno;
}

/* Leave the locked locale section.  */
static inline void
leave_locale_section (const scm_t_locale_settings *settings)
{
  /* Restore the previous locale settings.  */
  (void)restore_locale_settings (settings);

  scm_i_pthread_mutex_unlock (&scm_i_locale_mutex);
}

/* Enter a locked locale section.  */
static inline int
enter_locale_section (scm_t_locale locale,
		      scm_t_locale_settings *prev_locale)
{
  int err;

  scm_i_pthread_mutex_lock (&scm_i_locale_mutex);

  err = get_current_locale_settings (prev_locale);
  if (err)
    {
      scm_i_pthread_mutex_unlock (&scm_i_locale_mutex);
      return err;
    }

  err = install_locale (locale);
  if (err)
    {
      leave_locale_section (prev_locale);
      free_locale_settings (prev_locale);
    }

  return err;
}

/* Throw an exception corresponding to error ERR.  */
static void inline
scm_locale_error (const char *func_name, int err)
{
  SCM s_err;

  s_err = scm_from_int (err);
  scm_error (scm_system_error_key, func_name,
	     "Failed to install locale",
	     scm_cons (scm_strerror (s_err), SCM_EOL),
	     scm_cons (s_err, SCM_EOL));
}

/* Convenient macro to run STATEMENT in the locale context of C_LOCALE.  */
#define RUN_IN_LOCALE_SECTION(_c_locale, _statement)			\
  do									\
   {									\
      int lsec_err;							\
      scm_t_locale_settings lsec_prev_locale;				\
									\
      lsec_err = enter_locale_section ((_c_locale), &lsec_prev_locale);	\
      if (lsec_err)							\
	scm_locale_error (FUNC_NAME, lsec_err);				\
      else								\
	{								\
	  _statement ;							\
									\
	  leave_locale_section (&lsec_prev_locale);			\
	  free_locale_settings (&lsec_prev_locale);			\
	}								\
   }									\
  while (0)

#endif /* !USE_GNU_LOCALE_API */


/* Locale-dependent string comparison.  */

/* Compare null-terminated strings C_S1 and C_S2 according to LOCALE.  Return
   an integer whose sign is the same as the difference between C_S1 and
   C_S2.  */
static inline int
compare_strings (const char *c_s1, const char *c_s2, SCM locale,
		 const char *func_name)
#define FUNC_NAME func_name
{
  int result;
  scm_t_locale c_locale;

  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (3, locale, c_locale);

  if (c_locale)
    {
#ifdef USE_GNU_LOCALE_API
      result = strcoll_l (c_s1, c_s2, c_locale);
#else
#ifdef HAVE_STRCOLL
      RUN_IN_LOCALE_SECTION (c_locale, result = strcoll (c_s1, c_s2));
#else
      result = strcmp (c_s1, c_s2);
#endif
#endif /* !USE_GNU_LOCALE_API */
    }
  else

#ifdef HAVE_STRCOLL
    result = strcoll (c_s1, c_s2);
#else
    result = strcmp (c_s1, c_s2);
#endif

  return result;
}
#undef FUNC_NAME

/* Store into DST an upper-case version of SRC.  */
static inline void
str_upcase (register char *dst, register const char *src)
{
  for (; *src != '\0'; src++, dst++)
    *dst = toupper (*src);
  *dst = '\0';
}

static inline void
str_downcase (register char *dst, register const char *src)
{
  for (; *src != '\0'; src++, dst++)
    *dst = tolower (*src);
  *dst = '\0';
}

#ifdef USE_GNU_LOCALE_API
static inline void
str_upcase_l (register char *dst, register const char *src,
	      scm_t_locale locale)
{
  for (; *src != '\0'; src++, dst++)
    *dst = toupper_l (*src, locale);
  *dst = '\0';
}

static inline void
str_downcase_l (register char *dst, register const char *src,
		scm_t_locale locale)
{
  for (; *src != '\0'; src++, dst++)
    *dst = tolower_l (*src, locale);
  *dst = '\0';
}
#endif


/* Compare null-terminated strings C_S1 and C_S2 in a case-independent way
   according to LOCALE.  Return an integer whose sign is the same as the
   difference between C_S1 and C_S2.  */
static inline int
compare_strings_ci (const char *c_s1, const char *c_s2, SCM locale,
		    const char *func_name)
#define FUNC_NAME func_name
{
  int result;
  scm_t_locale c_locale;
  char *c_us1, *c_us2;

  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (3, locale, c_locale);

  c_us1 = (char *) alloca (strlen (c_s1) + 1);
  c_us2 = (char *) alloca (strlen (c_s2) + 1);

  if (c_locale)
    {
#ifdef USE_GNU_LOCALE_API
      str_upcase_l (c_us1, c_s1, c_locale);
      str_upcase_l (c_us2, c_s2, c_locale);

      result = strcoll_l (c_us1, c_us2, c_locale);
#else
      int err;
      scm_t_locale_settings prev_locale;

      err = enter_locale_section (c_locale, &prev_locale);
      if (err)
	{
	  scm_locale_error (func_name, err);
	  return 0;
	}

      str_upcase (c_us1, c_s1);
      str_upcase (c_us2, c_s2);

#ifdef HAVE_STRCOLL
      result = strcoll (c_us1, c_us2);
#else
      result = strcmp (c_us1, c_us2);
#endif /* !HAVE_STRCOLL */

      leave_locale_section (&prev_locale);
      free_locale_settings (&prev_locale);
#endif /* !USE_GNU_LOCALE_API */
    }
  else
    {
      str_upcase (c_us1, c_s1);
      str_upcase (c_us2, c_s2);

#ifdef HAVE_STRCOLL
      result = strcoll (c_us1, c_us2);
#else
      result = strcmp (c_us1, c_us2);
#endif
    }

  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_locale_lt, "string-locale<?", 2, 1, 0,
	    (SCM s1, SCM s2, SCM locale),
	    "Compare strings @var{s1} and @var{s2} in a locale-dependent way."
	    "If @var{locale} is provided, it should be locale object (as "
	    "returned by @code{make-locale}) and will be used to perform the "
	    "comparison; otherwise, the current system locale is used.")
#define FUNC_NAME s_scm_string_locale_lt
{
  int result;
  const char *c_s1, *c_s2;

  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  c_s1 = scm_i_string_chars (s1);
  c_s2 = scm_i_string_chars (s2);

  result = compare_strings (c_s1, c_s2, locale, FUNC_NAME);

  scm_remember_upto_here_2 (s1, s2);

  return scm_from_bool (result < 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_locale_gt, "string-locale>?", 2, 1, 0,
	    (SCM s1, SCM s2, SCM locale),
	    "Compare strings @var{s1} and @var{s2} in a locale-dependent way."
	    "If @var{locale} is provided, it should be locale object (as "
	    "returned by @code{make-locale}) and will be used to perform the "
	    "comparison; otherwise, the current system locale is used.")
#define FUNC_NAME s_scm_string_locale_gt
{
  int result;
  const char *c_s1, *c_s2;

  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  c_s1 = scm_i_string_chars (s1);
  c_s2 = scm_i_string_chars (s2);

  result = compare_strings (c_s1, c_s2, locale, FUNC_NAME);

  scm_remember_upto_here_2 (s1, s2);

  return scm_from_bool (result > 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_locale_ci_lt, "string-locale-ci<?", 2, 1, 0,
	    (SCM s1, SCM s2, SCM locale),
	    "Compare strings @var{s1} and @var{s2} in a case-insensitive, "
	    "and locale-dependent way.  If @var{locale} is provided, it "
	    "should be locale object (as returned by @code{make-locale}) "
	    "and will be used to perform the comparison; otherwise, the "
	    "current system locale is used.")
#define FUNC_NAME s_scm_string_locale_ci_lt
{
  int result;
  const char *c_s1, *c_s2;

  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  c_s1 = scm_i_string_chars (s1);
  c_s2 = scm_i_string_chars (s2);

  result = compare_strings_ci (c_s1, c_s2, locale, FUNC_NAME);

  scm_remember_upto_here_2 (s1, s2);

  return scm_from_bool (result < 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_locale_ci_gt, "string-locale-ci>?", 2, 1, 0,
	    (SCM s1, SCM s2, SCM locale),
	    "Compare strings @var{s1} and @var{s2} in a case-insensitive, "
	    "and locale-dependent way.  If @var{locale} is provided, it "
	    "should be locale object (as returned by @code{make-locale}) "
	    "and will be used to perform the comparison; otherwise, the "
	    "current system locale is used.")
#define FUNC_NAME s_scm_string_locale_ci_gt
{
  int result;
  const char *c_s1, *c_s2;

  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  c_s1 = scm_i_string_chars (s1);
  c_s2 = scm_i_string_chars (s2);

  result = compare_strings_ci (c_s1, c_s2, locale, FUNC_NAME);

  scm_remember_upto_here_2 (s1, s2);

  return scm_from_bool (result > 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_locale_ci_eq, "string-locale-ci=?", 2, 1, 0,
	    (SCM s1, SCM s2, SCM locale),
	    "Compare strings @var{s1} and @var{s2} in a case-insensitive, "
	    "and locale-dependent way.  If @var{locale} is provided, it "
	    "should be locale object (as returned by @code{make-locale}) "
	    "and will be used to perform the comparison; otherwise, the "
	    "current system locale is used.")
#define FUNC_NAME s_scm_string_locale_ci_eq
{
  int result;
  const char *c_s1, *c_s2;

  SCM_VALIDATE_STRING (1, s1);
  SCM_VALIDATE_STRING (2, s2);

  c_s1 = scm_i_string_chars (s1);
  c_s2 = scm_i_string_chars (s2);

  result = compare_strings_ci (c_s1, c_s2, locale, FUNC_NAME);

  scm_remember_upto_here_2 (s1, s2);

  return scm_from_bool (result == 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_locale_lt, "char-locale<?", 2, 1, 0,
	    (SCM c1, SCM c2, SCM locale),
	    "Return true if character @var{c1} is lower than @var{c2} "
	    "according to @var{locale} or to the current locale.")
#define FUNC_NAME s_scm_char_locale_lt
{
  char c_c1[2], c_c2[2];

  SCM_VALIDATE_CHAR (1, c1);
  SCM_VALIDATE_CHAR (2, c2);

  c_c1[0] = (char)SCM_CHAR (c1); c_c1[1] = '\0';
  c_c2[0] = (char)SCM_CHAR (c2); c_c2[1] = '\0';

  return scm_from_bool (compare_strings (c_c1, c_c2, locale, FUNC_NAME) < 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_locale_gt, "char-locale>?", 2, 1, 0,
	    (SCM c1, SCM c2, SCM locale),
	    "Return true if character @var{c1} is greater than @var{c2} "
	    "according to @var{locale} or to the current locale.")
#define FUNC_NAME s_scm_char_locale_gt
{
  char c_c1[2], c_c2[2];

  SCM_VALIDATE_CHAR (1, c1);
  SCM_VALIDATE_CHAR (2, c2);

  c_c1[0] = (char)SCM_CHAR (c1); c_c1[1] = '\0';
  c_c2[0] = (char)SCM_CHAR (c2); c_c2[1] = '\0';

  return scm_from_bool (compare_strings (c_c1, c_c2, locale, FUNC_NAME) > 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_locale_ci_lt, "char-locale-ci<?", 2, 1, 0,
	    (SCM c1, SCM c2, SCM locale),
	    "Return true if character @var{c1} is lower than @var{c2}, "
	    "in a case insensitive way according to @var{locale} or to "
	    "the current locale.")
#define FUNC_NAME s_scm_char_locale_ci_lt
{
  int result;
  char c_c1[2], c_c2[2];

  SCM_VALIDATE_CHAR (1, c1);
  SCM_VALIDATE_CHAR (2, c2);

  c_c1[0] = (char)SCM_CHAR (c1); c_c1[1] = '\0';
  c_c2[0] = (char)SCM_CHAR (c2); c_c2[1] = '\0';

  result = compare_strings_ci (c_c1, c_c2, locale, FUNC_NAME);

  return scm_from_bool (result < 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_locale_ci_gt, "char-locale-ci>?", 2, 1, 0,
	    (SCM c1, SCM c2, SCM locale),
	    "Return true if character @var{c1} is greater than @var{c2}, "
	    "in a case insensitive way according to @var{locale} or to "
	    "the current locale.")
#define FUNC_NAME s_scm_char_locale_ci_gt
{
  int result;
  char c_c1[2], c_c2[2];

  SCM_VALIDATE_CHAR (1, c1);
  SCM_VALIDATE_CHAR (2, c2);

  c_c1[0] = (char)SCM_CHAR (c1); c_c1[1] = '\0';
  c_c2[0] = (char)SCM_CHAR (c2); c_c2[1] = '\0';

  result = compare_strings_ci (c_c1, c_c2, locale, FUNC_NAME);

  return scm_from_bool (result > 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_locale_ci_eq, "char-locale-ci=?", 2, 1, 0,
	    (SCM c1, SCM c2, SCM locale),
	    "Return true if character @var{c1} is equal to @var{c2}, "
	    "in a case insensitive way according to @var{locale} or to "
	    "the current locale.")
#define FUNC_NAME s_scm_char_locale_ci_eq
{
  int result;
  char c_c1[2], c_c2[2];

  SCM_VALIDATE_CHAR (1, c1);
  SCM_VALIDATE_CHAR (2, c2);

  c_c1[0] = (char)SCM_CHAR (c1); c_c1[1] = '\0';
  c_c2[0] = (char)SCM_CHAR (c2); c_c2[1] = '\0';

  result = compare_strings_ci (c_c1, c_c2, locale, FUNC_NAME);

  return scm_from_bool (result == 0);
}
#undef FUNC_NAME



/* Locale-dependent alphabetic character mapping.  */

SCM_DEFINE (scm_char_locale_downcase, "char-locale-downcase", 1, 1, 0,
	    (SCM chr, SCM locale),
	    "Return the lowercase character that corresponds to @var{chr} "
	    "according to either @var{locale} or the current locale.")
#define FUNC_NAME s_scm_char_locale_downcase
{
  char c_chr;
  int c_result;
  scm_t_locale c_locale;

  SCM_VALIDATE_CHAR (1, chr);
  c_chr = SCM_CHAR (chr);

  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (2, locale, c_locale);

  if (c_locale != NULL)
    {
#ifdef USE_GNU_LOCALE_API
      c_result = tolower_l (c_chr, c_locale);
#else
      RUN_IN_LOCALE_SECTION (c_locale, c_result = tolower (c_chr));
#endif
    }
  else
    c_result = tolower (c_chr);

  return (SCM_MAKE_CHAR (c_result));
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_locale_upcase, "char-locale-upcase", 1, 1, 0,
	    (SCM chr, SCM locale),
	    "Return the uppercase character that corresponds to @var{chr} "
	    "according to either @var{locale} or the current locale.")
#define FUNC_NAME s_scm_char_locale_upcase
{
  char c_chr;
  int c_result;
  scm_t_locale c_locale;

  SCM_VALIDATE_CHAR (1, chr);
  c_chr = SCM_CHAR (chr);

  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (2, locale, c_locale);

  if (c_locale != NULL)
    {
#ifdef USE_GNU_LOCALE_API
      c_result = toupper_l (c_chr, c_locale);
#else
      RUN_IN_LOCALE_SECTION (c_locale, c_result = toupper (c_chr));
#endif
    }
  else
    c_result = toupper (c_chr);

  return (SCM_MAKE_CHAR (c_result));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_locale_upcase, "string-locale-upcase", 1, 1, 0,
	    (SCM str, SCM locale),
	    "Return a new string that is the uppercase version of "
	    "@var{str} according to either @var{locale} or the current "
	    "locale.")
#define FUNC_NAME s_scm_string_locale_upcase
{
  const char *c_str;
  char *c_ustr;
  scm_t_locale c_locale;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (2, locale, c_locale);

  c_str = scm_i_string_chars (str);
  c_ustr = (char *) alloca (strlen (c_str) + 1);

  if (c_locale)
    {
#ifdef USE_GNU_LOCALE_API
      str_upcase_l (c_ustr, c_str, c_locale);
#else
      RUN_IN_LOCALE_SECTION (c_locale, str_upcase (c_ustr, c_str));
#endif
    }
  else
    str_upcase (c_ustr, c_str);

  scm_remember_upto_here (str);

  return (scm_from_locale_string (c_ustr));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_locale_downcase, "string-locale-downcase", 1, 1, 0,
	    (SCM str, SCM locale),
	    "Return a new string that is the down-case version of "
	    "@var{str} according to either @var{locale} or the current "
	    "locale.")
#define FUNC_NAME s_scm_string_locale_downcase
{
  const char *c_str;
  char *c_lstr;
  scm_t_locale c_locale;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (2, locale, c_locale);

  c_str = scm_i_string_chars (str);
  c_lstr = (char *) alloca (strlen (c_str) + 1);

  if (c_locale)
    {
#ifdef USE_GNU_LOCALE_API
      str_downcase_l (c_lstr, c_str, c_locale);
#else
      RUN_IN_LOCALE_SECTION (c_locale, str_downcase (c_lstr, c_str));
#endif
    }
  else
    str_downcase (c_lstr, c_str);

  scm_remember_upto_here (str);

  return (scm_from_locale_string (c_lstr));
}
#undef FUNC_NAME

/* Note: We don't provide mutative versions of `string-locale-(up|down)case'
   because, in some languages, a single downcase character maps to a couple
   of uppercase characters.  Read the SRFI-13 document for a detailed
   discussion about this.  */



/* Locale-dependent number parsing.  */

SCM_DEFINE (scm_locale_string_to_integer, "locale-string->integer",
	    1, 2, 0, (SCM str, SCM base, SCM locale),
	    "Convert string @var{str} into an integer according to either "
	    "@var{locale} (a locale object as returned by @code{make-locale}) "
	    "or the current process locale.  Return two values: an integer "
	    "(on success) or @code{#f}, and the number of characters read "
	    "from @var{str} (@code{0} on failure).")
#define FUNC_NAME s_scm_locale_string_to_integer
{
  SCM result;
  long c_result;
  int c_base;
  const char *c_str;
  char *c_endptr;
  scm_t_locale c_locale;

  SCM_VALIDATE_STRING (1, str);
  c_str = scm_i_string_chars (str);

  if (base != SCM_UNDEFINED)
    SCM_VALIDATE_INT_COPY (2, base, c_base);
  else
    c_base = 10;

  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (3, locale, c_locale);

  if (c_locale != NULL)
    {
#ifdef USE_GNU_LOCALE_API
      c_result = strtol_l (c_str, &c_endptr, c_base, c_locale);
#else
      RUN_IN_LOCALE_SECTION (c_locale,
			     c_result = strtol (c_str, &c_endptr, c_base));
#endif
    }
  else
    c_result = strtol (c_str, &c_endptr, c_base);

  scm_remember_upto_here (str);

  if (c_endptr == c_str)
    result = SCM_BOOL_F;
  else
    result = scm_from_long (c_result);

  return (scm_values (scm_list_2 (result, scm_from_long (c_endptr - c_str))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_locale_string_to_inexact, "locale-string->inexact",
	    1, 1, 0, (SCM str, SCM locale),
	    "Convert string @var{str} into an inexact number according to "
	    "either @var{locale} (a locale object as returned by "
	    "@code{make-locale}) or the current process locale.  Return "
	    "two values: an inexact number (on success) or @code{#f}, and "
	    "the number of characters read from @var{str} (@code{0} on "
	    "failure).")
#define FUNC_NAME s_scm_locale_string_to_inexact
{
  SCM result;
  double c_result;
  const char *c_str;
  char *c_endptr;
  scm_t_locale c_locale;

  SCM_VALIDATE_STRING (1, str);
  c_str = scm_i_string_chars (str);

  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (2, locale, c_locale);

  if (c_locale != NULL)
    {
#ifdef USE_GNU_LOCALE_API
      c_result = strtod_l (c_str, &c_endptr, c_locale);
#else
      RUN_IN_LOCALE_SECTION (c_locale,
			     c_result = strtod (c_str, &c_endptr));
#endif
    }
  else
    c_result = strtod (c_str, &c_endptr);

  scm_remember_upto_here (str);

  if (c_endptr == c_str)
    result = SCM_BOOL_F;
  else
    result = scm_from_double (c_result);

  return (scm_values (scm_list_2 (result, scm_from_long (c_endptr - c_str))));
}
#undef FUNC_NAME



void
scm_init_i18n ()
{
  scm_add_feature ("ice-9-i18n");

#define _SCM_STRINGIFY_LC(_name)  # _name
#define SCM_STRINGIFY_LC(_name)   _SCM_STRINGIFY_LC (_name)

  /* Define all the relevant `_MASK' variables.  */
#define SCM_DEFINE_LOCALE_CATEGORY(_name)		\
  scm_c_define ("LC_" SCM_STRINGIFY_LC (_name) "_MASK",	\
		SCM_I_MAKINUM (LC_ ## _name ## _MASK));
#include "locale-categories.h"

#undef SCM_DEFINE_LOCALE_CATEGORY
#undef SCM_STRINGIFY_LC
#undef _SCM_STRINGIFY_LC

  scm_c_define ("LC_ALL_MASK", SCM_I_MAKINUM (LC_ALL_MASK));

#include "libguile/i18n.x"

#ifndef USE_GNU_LOCALE_API
  scm_set_smob_mark (scm_tc16_locale_smob_type, smob_locale_mark);
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
