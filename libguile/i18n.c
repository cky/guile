/* Copyright (C) 2006, 2007 Free Software Foundation, Inc.
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
#include "libguile/threads.h"

#include <locale.h>
#include <string.h> /* `strcoll ()' */
#include <ctype.h>  /* `toupper ()' et al. */
#include <errno.h>

#if (defined HAVE_NEWLOCALE) && (defined HAVE_STRCOLL_L)
/* The GNU thread-aware locale API is documented in ``Thread-Aware Locale
   Model, a Proposal'', by Ulrich Drepper:

     http://people.redhat.com/drepper/tllocale.ps.gz

   It is now also implemented by Darwin:

     http://developer.apple.com/documentation/Darwin/Reference/ManPages/man3/newlocale.3.html

   The whole API is being standardized by the X/Open Group (as of Jan. 2007)
   following Drepper's proposal.  */
# define USE_GNU_LOCALE_API
#endif

#if (defined USE_GNU_LOCALE_API) && (defined HAVE_XLOCALE_H)
# include <xlocale.h>
#endif

#include "libguile/posix.h"  /* for `scm_i_locale_mutex' */

#if (defined HAVE_LANGINFO_H) && (defined HAVE_NL_TYPES_H)
# include <langinfo.h>
# include <nl_types.h>
#endif

#ifndef HAVE_SETLOCALE
static inline char *
setlocale (int category, const char *name)
{
  errno = ENOSYS;
  return NULL;
}
#endif

/* Helper stringification macro.  */
#define SCM_I18N_STRINGIFY(_name)   # _name



/* Locale objects, string and character collation, and other locale-dependent
   string operations.

   A large part of the code here deals with emulating glibc's reentrant
   locale API on non-GNU systems.  The emulation is a bit "brute-force":
   Whenever a `-locale<?' procedure is passed a locale object, then:

   1. The `scm_i_locale_mutex' is locked.
   2. A series of `setlocale ()' call is performed to store the current
      locale for each category in an `scm_t_locale' object.
   3. A series of `setlocale ()' call is made to install each of the locale
      categories of each of the base locales of each locale object,
      recursively, starting from the last locale object of the chain.
   4. The settings captured in step (2) are restored.
   5. The `scm_i_locale_mutex' is released.

   Hopefully, the X/Open standard will eventually make this hack useless.

   Note: We don't wrap glibc's `uselocale ()' call because it sets the locale
   of the current _thread_ (unlike `setlocale ()') and doing so would require
   maintaining per-thread locale information on non-GNU systems and always
   re-installing this locale upon locale-dependent calls.  */


/* Return the category mask corresponding to CAT.  */
#define SCM_LOCALE_CATEGORY_MASK(_cat)    LC_ ## _cat ## _MASK


#ifndef USE_GNU_LOCALE_API

/* Provide the locale category masks as found in glibc.  This must be kept in
   sync with `locale-categories.h'.  */

# define LC_CTYPE_MASK		1
# define LC_COLLATE_MASK	2
# define LC_MESSAGES_MASK	4
# define LC_MONETARY_MASK	8
# define LC_NUMERIC_MASK	16
# define LC_TIME_MASK		32

# ifdef LC_PAPER
#   define LC_PAPER_MASK	64
# else
#   define LC_PAPER_MASK	0
# endif
# ifdef LC_NAME
#   define LC_NAME_MASK		128
# else
#   define LC_NAME_MASK		0
# endif
# ifdef LC_ADDRESS
#   define LC_ADDRESS_MASK	256
# else
#   define LC_ADDRESS_MASK	0
# endif
# ifdef LC_TELEPHONE
#   define LC_TELEPHONE_MASK	512
# else
#   define LC_TELEPHONE_MASK	0
# endif
# ifdef LC_MEASUREMENT
#   define LC_MEASUREMENT_MASK	1024
# else
#   define LC_MEASUREMENT_MASK	0
# endif
# ifdef LC_IDENTIFICATION
#   define LC_IDENTIFICATION_MASK 2048
# else
#   define LC_IDENTIFICATION_MASK 0
# endif

# define LC_ALL_MASK		(LC_CTYPE_MASK			\
				 | LC_NUMERIC_MASK		\
				 | LC_TIME_MASK			\
				 | LC_COLLATE_MASK		\
				 | LC_MONETARY_MASK		\
				 | LC_MESSAGES_MASK		\
				 | LC_PAPER_MASK		\
				 | LC_NAME_MASK			\
				 | LC_ADDRESS_MASK		\
				 | LC_TELEPHONE_MASK		\
				 | LC_MEASUREMENT_MASK		\
				 | LC_IDENTIFICATION_MASK	\
				 )

/* Locale objects as returned by `make-locale' on non-GNU systems.  */
typedef struct scm_locale
{
  SCM   base_locale; /* a `locale' object */
  char *locale_name;
  int   category_mask;
} *scm_t_locale;


/* Free the resources used by LOCALE.  */
static inline void
scm_i_locale_free (scm_t_locale locale)
{
  free (locale->locale_name);
  locale->locale_name = NULL;
}

#else /* USE_GNU_LOCALE_API */

/* Alias for glibc's locale type.  */
typedef locale_t scm_t_locale;

#define scm_i_locale_free freelocale

#endif /* USE_GNU_LOCALE_API */


/* A locale object denoting the global locale.  */
SCM_GLOBAL_VARIABLE (scm_global_locale, "%global-locale");


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

  c_locale = (scm_t_locale) SCM_SMOB_DATA (locale);
  scm_i_locale_free (c_locale);

  return 0;
}

#ifndef USE_GNU_LOCALE_API
static SCM
smob_locale_mark (SCM locale)
{
  register SCM dependency;

  if (!scm_is_eq (locale, SCM_VARIABLE_REF (scm_global_locale)))
    {
      scm_t_locale c_locale;

      c_locale = (scm_t_locale) SCM_SMOB_DATA (locale);
      dependency = (c_locale->base_locale);
    }
  else
    dependency = SCM_BOOL_F;

  return dependency;
}
#endif


static void inline scm_locale_error (const char *, int) SCM_NORETURN;

/* Throw an exception corresponding to error ERR.  */
static void inline
scm_locale_error (const char *func_name, int err)
{
  scm_syserror_msg (func_name,
		    "Failed to install locale",
		    SCM_EOL, err);
}



/* Emulating GNU's reentrant locale API.  */
#ifndef USE_GNU_LOCALE_API


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
    if (locale_name == NULL)					\
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
  return EINVAL;

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
  return EINVAL;
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
  if (category_mask & SCM_LOCALE_CATEGORY_MASK (_name))			\
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
  return EINVAL;
}

/* Install LOCALE, recursively installing its base locales first.  On
   success, zero is returned.  */
static int
install_locale (scm_t_locale locale)
{
  scm_t_locale stack[LOCALE_STACK_SIZE_MAX];
  int category_mask = 0;
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

      /* Keep track of which categories have already been taken into
	 account.  */
      category_mask |= locale->category_mask;

      if (locale->base_locale != SCM_UNDEFINED)
	locale = (scm_t_locale) SCM_SMOB_DATA (locale->base_locale);
      else
	locale = NULL;
    }
  while ((locale != NULL) && (category_mask != LC_ALL_MASK));

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
  return EINVAL;
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

/* Convert the current locale settings into a locale SMOB.  On success, zero
   is returned and RESULT points to the new SMOB.  Otherwise, an error is
   returned.  */
static int
get_current_locale (SCM *result)
{
  int err = 0;
  scm_t_locale c_locale;
  const char *current_locale;

  c_locale = scm_gc_malloc (sizeof (* c_locale), "locale");


  scm_i_pthread_mutex_lock (&scm_i_locale_mutex);

  c_locale->category_mask = LC_ALL_MASK;
  c_locale->base_locale = SCM_UNDEFINED;

  current_locale = setlocale (LC_ALL, NULL);
  if (current_locale != NULL)
    {
      c_locale->locale_name = strdup (current_locale);
      if (c_locale->locale_name == NULL)
	err = ENOMEM;
    }
  else
    err = EINVAL;

  scm_i_pthread_mutex_unlock (&scm_i_locale_mutex);

  if (err)
    scm_gc_free (c_locale, sizeof (* c_locale), "locale");
  else
    SCM_NEWSMOB (*result, scm_tc16_locale_smob_type, c_locale);

  return err;
}


#endif /* !USE_GNU_LOCALE_API */



/* `make-locale' can take either category lists or single categories (the
   `LC_*' integer constants).  */
#define SCM_LIST_OR_INTEGER_P(arg)				\
  (scm_is_integer (arg) || scm_is_true (scm_list_p (arg)))


/* Return the category mask corresponding to CATEGORY (an `LC_' integer
   constant).  */
static inline int
category_to_category_mask (SCM category,
			   const char *func_name, int pos)
{
  int c_category;
  int c_category_mask;

  c_category = scm_to_int (category);

#define SCM_DEFINE_LOCALE_CATEGORY(_name)			\
      case LC_ ## _name:					\
	c_category_mask = SCM_LOCALE_CATEGORY_MASK (_name);	\
	break;

  switch (c_category)
    {
#include "locale-categories.h"

    case LC_ALL:
      c_category_mask = LC_ALL_MASK;
      break;

    default:
      scm_wrong_type_arg_msg (func_name, pos, category,
			      "locale category");
    }

#undef SCM_DEFINE_LOCALE_CATEGORY

  return c_category_mask;
}

/* Convert CATEGORIES, a list of locale categories or a single category (an
   integer), into a category mask.  */
static int
category_list_to_category_mask (SCM categories,
				const char *func_name, int pos)
{
  int c_category_mask = 0;

  if (scm_is_integer (categories))
    c_category_mask = category_to_category_mask (categories,
						 func_name, pos);
  else
    for (; !scm_is_null (categories); categories = SCM_CDR (categories))
      {
	SCM category = SCM_CAR (categories);

	c_category_mask |=
	  category_to_category_mask (category, func_name, pos);
      }

  return c_category_mask;
}


SCM_DEFINE (scm_make_locale, "make-locale", 2, 1, 0,
	    (SCM category_list, SCM locale_name, SCM base_locale),
	    "Return a reference to a data structure representing a set of "
	    "locale datasets.  @var{category_list} should be either a list "
	    "of locale categories or a single category as used with "
	    "@code{setlocale} (@pxref{Locales, @code{setlocale}}) and "
	    "@var{locale_name} should be the name of the locale considered "
	    "(e.g., @code{\"sl_SI\"}).  Optionally, if @code{base_locale} is "
	    "passed, it should be a locale object denoting settings for "
	    "categories not listed in @var{category_list}.")
#define FUNC_NAME s_scm_make_locale
{
  SCM locale = SCM_BOOL_F;
  int err = 0;
  int c_category_mask;
  char *c_locale_name;
  scm_t_locale c_base_locale, c_locale;

  SCM_MAKE_VALIDATE (1, category_list, LIST_OR_INTEGER_P);
  SCM_VALIDATE_STRING (2, locale_name);
  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (3, base_locale, c_base_locale);

  c_category_mask = category_list_to_category_mask (category_list,
						    FUNC_NAME, 1);
  c_locale_name = scm_to_locale_string (locale_name);

#ifdef USE_GNU_LOCALE_API

  if (scm_is_eq (base_locale, SCM_VARIABLE_REF (scm_global_locale)))
    {
      /* Fetch the current locale and turn in into a `locale_t'.  Don't
	 duplicate the resulting `locale_t' because we want it to be consumed
	 by `newlocale ()'.  */
      char *current_locale;

      scm_i_pthread_mutex_lock (&scm_i_locale_mutex);

      current_locale = setlocale (LC_ALL, NULL);
      c_base_locale = newlocale (LC_ALL_MASK, current_locale, NULL);

      scm_i_pthread_mutex_unlock (&scm_i_locale_mutex);

      if (c_base_locale == (locale_t) 0)
	scm_locale_error (FUNC_NAME, errno);
    }
  else if (c_base_locale != (locale_t) 0)
    {
      /* C_BASE_LOCALE is to be consumed by `newlocale ()' so it needs to be
	 duplicated before.  */
      c_base_locale = duplocale (c_base_locale);
      if (c_base_locale == (locale_t) 0)
	{
	  err = errno;
	  goto fail;
	}
    }

  c_locale = newlocale (c_category_mask, c_locale_name, c_base_locale);

  free (c_locale_name);

  if (c_locale == (locale_t) 0)
    {
      if (scm_is_eq (base_locale, SCM_VARIABLE_REF (scm_global_locale)))
	/* The base locale object was created lazily and must be freed.  */
	freelocale (c_base_locale);

      scm_locale_error (FUNC_NAME, errno);
    }
  else
    SCM_NEWSMOB (locale, scm_tc16_locale_smob_type, c_locale);

#else

  c_locale = scm_gc_malloc (sizeof (* c_locale), "locale");

  c_locale->category_mask = c_category_mask;
  c_locale->locale_name = c_locale_name;

  if (scm_is_eq (base_locale, SCM_VARIABLE_REF (scm_global_locale)))
    {
      /* Get the current locale settings and turn them into a locale
	 object.  */
      err = get_current_locale (&base_locale);
      if (err)
	goto fail;
    }

  c_locale->base_locale = base_locale;

  {
    /* Try out the new locale and raise an exception if it doesn't work.  */
    int err;
    scm_t_locale_settings prev_locale;

    err = enter_locale_section (c_locale, &prev_locale);
    leave_locale_section (&prev_locale);

    if (err)
      goto fail;
    else
      SCM_NEWSMOB (locale, scm_tc16_locale_smob_type, c_locale);
  }

#endif

  return locale;

 fail:
#ifndef USE_GNU_LOCALE_API
  scm_gc_free (c_locale, sizeof (* c_locale), "locale");
#endif
  free (c_locale_name);
  scm_locale_error (FUNC_NAME, err);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_locale_p, "locale?", 1, 0, 0,
	    (SCM obj),
	    "Return true if @var{obj} is a locale object.")
#define FUNC_NAME s_scm_locale_p
{
  return scm_from_bool (SCM_SMOB_PREDICATE (scm_tc16_locale_smob_type, obj));
}
#undef FUNC_NAME



/* Locale-dependent string comparison.

   A similar API can be found in MzScheme starting from version 200:
   http://download.plt-scheme.org/chronology/mzmr200alpha14.html .  */


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


/* Language information, aka. `nl_langinfo ()'.  */

/* FIXME: Issues related to `nl-langinfo'.

   1. The `CODESET' value is not normalized.  This is a secondary issue, but
      still a practical issue.  See
      http://www.cl.cam.ac.uk/~mgk25/ucs/norm_charmap.c for codeset
      normalization.

   2. `nl_langinfo ()' is not available on Windows.

   3. `nl_langinfo ()' may return strings encoded in a locale different from
      the current one, thereby defeating `scm_from_locale_string ()'.
      Example: support the current locale is "Latin-1" and one asks:

        (nl-langinfo DAY_1 (make-locale LC_ALL "eo_EO.UTF-8"))

      The result will be a UTF-8 string.  However, `scm_from_locale_string',
      which expects a Latin-1 string, won't be able to make much sense of the
      returned string.  Thus, we'd need an `scm_from_string ()' variant where
      the locale (or charset) is explicitly passed.  */


SCM_DEFINE (scm_nl_langinfo, "nl-langinfo", 1, 1, 0,
	    (SCM item, SCM locale),
	    "Return a string denoting locale information for @var{item} "
	    "in the current locale or that specified by @var{locale}.  "
	    "The semantics and arguments are the same as those of the "
	    "X/Open @code{nl_langinfo} function (@pxref{The Elegant and "
	    "Fast Way, @code{nl_langinfo},, libc, The GNU C Library "
	    "Reference Manual}).")
#define FUNC_NAME s_scm_nl_langinfo
{
#ifdef HAVE_NL_LANGINFO
  SCM result;
  nl_item c_item;
  char *c_result;
  scm_t_locale c_locale;

  SCM_VALIDATE_INT_COPY (2, item, c_item);
  SCM_VALIDATE_OPTIONAL_LOCALE_COPY (2, locale, c_locale);

  /* Sadly, `nl_langinfo ()' returns a pointer to a static string.  According
     to SuS v2, that static string may be modified by subsequent calls to
     `nl_langinfo ()' as well as by calls to `setlocale ()'.  Thus, we must
     acquire the locale mutex before doing invoking `nl_langinfo ()'.  See
     http://opengroup.org/onlinepubs/007908799/xsh/nl_langinfo.html for
     details.  */

  scm_i_pthread_mutex_lock (&scm_i_locale_mutex);
  if (c_locale != NULL)
    {
#ifdef USE_GNU_LOCALE_API
      c_result = nl_langinfo_l (c_item, c_locale);
#else
      /* We can't use `RUN_IN_LOCALE_SECTION ()' here because the locale
	 mutex is already taken.  */
      int lsec_err;
      scm_t_locale_settings lsec_prev_locale;

      lsec_err = get_current_locale_settings (&lsec_prev_locale);
      if (lsec_err)
	scm_i_pthread_mutex_unlock (&scm_i_locale_mutex);
      else
	{
	  lsec_err = install_locale (c_locale);
	  if (lsec_err)
	    {
	      leave_locale_section (&lsec_prev_locale);
	      free_locale_settings (&lsec_prev_locale);
	    }
	}

      if (lsec_err)
	scm_locale_error (FUNC_NAME, lsec_err);
      else
	{
	  c_result = nl_langinfo (c_item);

	  leave_locale_section (&lsec_prev_locale);
	  free_locale_settings (&lsec_prev_locale);
	}
#endif
    }
  else
    c_result = nl_langinfo (c_item);

  c_result = strdup (c_result);
  scm_i_pthread_mutex_unlock (&scm_i_locale_mutex);

  if (c_result == NULL)
    result = SCM_BOOL_F;
  else
    {
      switch (c_item)
	{
#if (defined GROUPING) && (defined MON_GROUPING)
	case GROUPING:
	case MON_GROUPING:
	  {
	    char *p;

	    /* In this cases, the result is to be interpreted as a list of
	       numbers.  If the last item is `CHARS_MAX', it has the special
	       meaning "no more grouping".  */
	    result = SCM_EOL;
	    for (p = c_result; (*p != '\0') && (*p != CHAR_MAX); p++)
	      result = scm_cons (SCM_I_MAKINUM ((int) *p), result);

	    {
	      SCM last_pair = result;

	      result = scm_reverse_x (result, SCM_EOL);

	      if (*p != CHAR_MAX)
		{
		  /* Cyclic grouping information.  */
		  if (last_pair != SCM_EOL)
		    SCM_SETCDR (last_pair, result);
		}
	    }

	    free (c_result);
	    break;
	  }
#endif

#if (defined FRAC_DIGITS) && (defined INT_FRAC_DIGITS)
	case FRAC_DIGITS:
	case INT_FRAC_DIGITS:
	  /* This is to be interpreted as a single integer.  */
	  if (*c_result == CHAR_MAX)
	    /* Unspecified.  */
	    result = SCM_BOOL_F;
	  else
	    result = SCM_I_MAKINUM (*c_result);

	  free (c_result);
	  break;
#endif

#if (defined P_CS_PRECEDES) && (defined INT_N_CS_PRECEDES)
	case P_CS_PRECEDES:
	case N_CS_PRECEDES:
	case INT_P_CS_PRECEDES:
	case INT_N_CS_PRECEDES:
#if (defined P_SEP_BY_SPACE) && (defined N_SEP_BY_SPACE)
	case P_SEP_BY_SPACE:
	case N_SEP_BY_SPACE:
#endif
	  /* This is to be interpreted as a boolean.  */
	  result = scm_from_bool (*c_result);

	  free (c_result);
	  break;
#endif

#if (defined P_SIGN_POSN) && (defined INT_N_SIGN_POSN)
	case P_SIGN_POSN:
	case N_SIGN_POSN:
	case INT_P_SIGN_POSN:
	case INT_N_SIGN_POSN:
	  /* See `(libc) Sign of Money Amount' for the interpretation of the
	     return value here.  */
	  switch (*c_result)
	    {
	    case 0:
	      result = scm_from_locale_symbol ("parenthesize");
	      break;

	    case 1:
	      result = scm_from_locale_symbol ("sign-before");
	      break;

	    case 2:
	      result = scm_from_locale_symbol ("sign-after");
	      break;

	    case 3:
	      result = scm_from_locale_symbol ("sign-before-currency-symbol");
	      break;

	    case 4:
	      result = scm_from_locale_symbol ("sign-after-currency-symbol");
	      break;

	    default:
	      result = scm_from_locale_symbol ("unspecified");
	    }
	  break;
#endif

	default:
	  /* FIXME: `locale_string ()' is not appropriate here because of
	     encoding issues (see comment above).  */
	  result = scm_take_locale_string (c_result);
	}
    }

  return result;
#else
  scm_syserror_msg (FUNC_NAME, "`nl-langinfo' not supported on your system",
		    SCM_EOL, ENOSYS);

  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME

/* Define the `nl_item' constants.  */
static inline void
define_langinfo_items (void)
{
#if (defined HAVE_NL_TYPES_H) && (defined HAVE_LANGINFO_H)

#define DEFINE_NLITEM_CONSTANT(_item)		\
  scm_c_define (# _item, scm_from_int (_item))

  DEFINE_NLITEM_CONSTANT (CODESET);

  /* Abbreviated days of the week. */
  DEFINE_NLITEM_CONSTANT (ABDAY_1);
  DEFINE_NLITEM_CONSTANT (ABDAY_2);
  DEFINE_NLITEM_CONSTANT (ABDAY_3);
  DEFINE_NLITEM_CONSTANT (ABDAY_4);
  DEFINE_NLITEM_CONSTANT (ABDAY_5);
  DEFINE_NLITEM_CONSTANT (ABDAY_6);
  DEFINE_NLITEM_CONSTANT (ABDAY_7);

  /* Long-named days of the week. */
  DEFINE_NLITEM_CONSTANT (DAY_1);	/* Sunday */
  DEFINE_NLITEM_CONSTANT (DAY_2);	/* Monday */
  DEFINE_NLITEM_CONSTANT (DAY_3);	/* Tuesday */
  DEFINE_NLITEM_CONSTANT (DAY_4);	/* Wednesday */
  DEFINE_NLITEM_CONSTANT (DAY_5);	/* Thursday */
  DEFINE_NLITEM_CONSTANT (DAY_6);	/* Friday */
  DEFINE_NLITEM_CONSTANT (DAY_7);	/* Saturday */

  /* Abbreviated month names.  */
  DEFINE_NLITEM_CONSTANT (ABMON_1);	/* Jan */
  DEFINE_NLITEM_CONSTANT (ABMON_2);
  DEFINE_NLITEM_CONSTANT (ABMON_3);
  DEFINE_NLITEM_CONSTANT (ABMON_4);
  DEFINE_NLITEM_CONSTANT (ABMON_5);
  DEFINE_NLITEM_CONSTANT (ABMON_6);
  DEFINE_NLITEM_CONSTANT (ABMON_7);
  DEFINE_NLITEM_CONSTANT (ABMON_8);
  DEFINE_NLITEM_CONSTANT (ABMON_9);
  DEFINE_NLITEM_CONSTANT (ABMON_10);
  DEFINE_NLITEM_CONSTANT (ABMON_11);
  DEFINE_NLITEM_CONSTANT (ABMON_12);

  /* Long month names.  */
  DEFINE_NLITEM_CONSTANT (MON_1);	/* January */
  DEFINE_NLITEM_CONSTANT (MON_2);
  DEFINE_NLITEM_CONSTANT (MON_3);
  DEFINE_NLITEM_CONSTANT (MON_4);
  DEFINE_NLITEM_CONSTANT (MON_5);
  DEFINE_NLITEM_CONSTANT (MON_6);
  DEFINE_NLITEM_CONSTANT (MON_7);
  DEFINE_NLITEM_CONSTANT (MON_8);
  DEFINE_NLITEM_CONSTANT (MON_9);
  DEFINE_NLITEM_CONSTANT (MON_10);
  DEFINE_NLITEM_CONSTANT (MON_11);
  DEFINE_NLITEM_CONSTANT (MON_12);

  DEFINE_NLITEM_CONSTANT (AM_STR);	/* Ante meridiem string.  */
  DEFINE_NLITEM_CONSTANT (PM_STR);	/* Post meridiem string.  */

  DEFINE_NLITEM_CONSTANT (D_T_FMT); /* Date and time format for strftime.  */
  DEFINE_NLITEM_CONSTANT (D_FMT);   /* Date format for strftime.  */
  DEFINE_NLITEM_CONSTANT (T_FMT);   /* Time format for strftime.  */
  DEFINE_NLITEM_CONSTANT (T_FMT_AMPM);/* 12-hour time format for strftime.  */

  DEFINE_NLITEM_CONSTANT (ERA);	        /* Alternate era.  */
  DEFINE_NLITEM_CONSTANT (ERA_D_FMT);	/* Date in alternate era format.  */
  DEFINE_NLITEM_CONSTANT (ERA_D_T_FMT);	/* Date and time in alternate era
					   format.  */
  DEFINE_NLITEM_CONSTANT (ERA_T_FMT);	/* Time in alternate era format.  */

  DEFINE_NLITEM_CONSTANT (ALT_DIGITS);	/* Alternate symbols for digits.  */
  DEFINE_NLITEM_CONSTANT (RADIXCHAR);
  DEFINE_NLITEM_CONSTANT (THOUSEP);

#ifdef YESEXPR
  DEFINE_NLITEM_CONSTANT (YESEXPR);
#endif
#ifdef NOEXPR
  DEFINE_NLITEM_CONSTANT (NOEXPR);
#endif

#ifdef CRNCYSTR /* currency symbol */
  DEFINE_NLITEM_CONSTANT (CRNCYSTR);
#endif

  /* GNU extensions.  */

#ifdef ERA_YEAR
  DEFINE_NLITEM_CONSTANT (ERA_YEAR);	/* Year in alternate era format.  */
#endif

  /* LC_MONETARY category: formatting of monetary quantities.
     These items each correspond to a member of `struct lconv',
     defined in <locale.h>.  */
#ifdef INT_CURR_SYMBOL
  DEFINE_NLITEM_CONSTANT (INT_CURR_SYMBOL);
#endif
#ifdef MON_DECIMAL_POINT
  DEFINE_NLITEM_CONSTANT (MON_DECIMAL_POINT);
#endif
#ifdef MON_THOUSANDS_SEP
  DEFINE_NLITEM_CONSTANT (MON_THOUSANDS_SEP);
#endif
#ifdef MON_GROUPING
  DEFINE_NLITEM_CONSTANT (MON_GROUPING);
#endif
#ifdef POSITIVE_SIGN
  DEFINE_NLITEM_CONSTANT (POSITIVE_SIGN);
#endif
#ifdef NEGATIVE_SIGN
  DEFINE_NLITEM_CONSTANT (NEGATIVE_SIGN);
#endif
#ifdef GROUPING
  DEFINE_NLITEM_CONSTANT (GROUPING);
#endif
#ifdef INT_FRAC_DIGITS
  DEFINE_NLITEM_CONSTANT (INT_FRAC_DIGITS);
#endif
#ifdef FRAC_DIGITS
  DEFINE_NLITEM_CONSTANT (FRAC_DIGITS);
#endif
#ifdef P_CS_PRECEDES
  DEFINE_NLITEM_CONSTANT (P_CS_PRECEDES);
#endif
#ifdef P_SEP_BY_SPACE
  DEFINE_NLITEM_CONSTANT (P_SEP_BY_SPACE);
#endif
#ifdef N_CS_PRECEDES
  DEFINE_NLITEM_CONSTANT (N_CS_PRECEDES);
#endif
#ifdef N_SEP_BY_SPACE
  DEFINE_NLITEM_CONSTANT (N_SEP_BY_SPACE);
#endif
#ifdef P_SIGN_POSN
  DEFINE_NLITEM_CONSTANT (P_SIGN_POSN);
#endif
#ifdef N_SIGN_POSN
  DEFINE_NLITEM_CONSTANT (N_SIGN_POSN);
#endif
#ifdef INT_P_CS_PRECEDES
  DEFINE_NLITEM_CONSTANT (INT_P_CS_PRECEDES);
#endif
#ifdef INT_P_SEP_BY_SPACE
  DEFINE_NLITEM_CONSTANT (INT_P_SEP_BY_SPACE);
#endif
#ifdef INT_N_CS_PRECEDES
  DEFINE_NLITEM_CONSTANT (INT_N_CS_PRECEDES);
#endif
#ifdef INT_N_SEP_BY_SPACE
  DEFINE_NLITEM_CONSTANT (INT_N_SEP_BY_SPACE);
#endif
#ifdef INT_P_SIGN_POSN
  DEFINE_NLITEM_CONSTANT (INT_P_SIGN_POSN);
#endif
#ifdef INT_N_SIGN_POSN
  DEFINE_NLITEM_CONSTANT (INT_N_SIGN_POSN);
#endif

#undef DEFINE_NLITEM_CONSTANT

#endif /* HAVE_NL_TYPES_H */
}


void
scm_init_i18n ()
{
  SCM global_locale_smob;

#ifdef HAVE_NL_LANGINFO
  scm_add_feature ("nl-langinfo");
  define_langinfo_items ();
#endif

#include "libguile/i18n.x"

#ifndef USE_GNU_LOCALE_API
  scm_set_smob_mark (scm_tc16_locale_smob_type, smob_locale_mark);
#endif

  /* Initialize the global locale object with a special `locale' SMOB.  */
  SCM_NEWSMOB (global_locale_smob, scm_tc16_locale_smob_type, NULL);
  SCM_VARIABLE_SET (scm_global_locale, global_locale_smob);
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
