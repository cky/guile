/*	Copyright (C) 1995,1996,1997, 2000, 2001, 2003, 2004, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif

#include <unistr.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/symbols.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/hash.h"


#ifndef floor
extern double floor();
#endif


unsigned long 
scm_string_hash (const unsigned char *str, size_t len)
{
  /* from suggestion at: */
  /* http://srfi.schemers.org/srfi-13/mail-archive/msg00112.html */

  unsigned long h = 0;
  while (len-- > 0)
    h = *str++ + h*37;
  return h;
}

unsigned long 
scm_i_string_hash (SCM str)
{
  size_t len = scm_i_string_length (str);
  size_t i = 0;

  unsigned long h = 0;
  while (len-- > 0)
    h = (unsigned long) scm_i_string_ref (str, i++) + h * 37;

  scm_remember_upto_here_1 (str);
  return h;
}

unsigned long 
scm_i_locale_string_hash (const char *str, size_t len)
{
#ifdef HAVE_WCHAR_H
  mbstate_t state;
  wchar_t c;
  size_t byte_idx = 0, nbytes;
  unsigned long h = 0;

  if (len == (size_t) -1)
    len = strlen (str);

  while ((nbytes = mbrtowc (&c, str + byte_idx, len - byte_idx, &state)) > 0)
    {
      if (nbytes >= (size_t) -2)
        /* Invalid input string; punt.  */
        return scm_i_string_hash (scm_from_locale_stringn (str, len));

      h = (unsigned long) c + h * 37;
      byte_idx += nbytes;
    }

  return h;
#else
  return scm_i_string_hash (scm_from_locale_stringn (str, len));
#endif
}

unsigned long 
scm_i_latin1_string_hash (const char *str, size_t len)
{
  const scm_t_uint8 *ustr = (const scm_t_uint8 *) str;
  size_t i = 0;
  unsigned long h = 0;
  
  if (len == (size_t) -1)
    len = strlen (str);

  for (; i < len; i++)
    h = (unsigned long) ustr[i] + h * 37;

  return h;
}

unsigned long 
scm_i_utf8_string_hash (const char *str, size_t len)
{
  const scm_t_uint8 *ustr = (const scm_t_uint8 *) str;
  size_t byte_idx = 0;
  unsigned long h = 0;
  
  if (len == (size_t) -1)
    len = strlen (str);

  while (byte_idx < len)
    {
      ucs4_t c;
      int nbytes;

      nbytes = u8_mbtouc (&c, ustr + byte_idx, len - byte_idx);
      if (nbytes == 0)
        break;
      else if (nbytes < 0)
        /* Bad UTF-8; punt.  */
        return scm_i_string_hash (scm_from_utf8_stringn (str, len));

      h = (unsigned long) c + h * 37;
      byte_idx += nbytes;
    }

  return h;
}


/* Dirk:FIXME:: why downcase for characters? (2x: scm_hasher, scm_ihashv) */
/* Dirk:FIXME:: scm_hasher could be made static. */


unsigned long
scm_hasher(SCM obj, unsigned long n, size_t d)
{
  switch (SCM_ITAG3 (obj)) {
  case scm_tc3_int_1: 
  case scm_tc3_int_2:
    return SCM_I_INUM(obj) % n;   /* SCM_INUMP(obj) */
  case scm_tc3_imm24:
    if (SCM_CHARP(obj))
      return (unsigned)(scm_c_downcase(SCM_CHAR(obj))) % n;
    switch (SCM_UNPACK (obj)) {
#ifndef SICP
    case SCM_UNPACK(SCM_EOL):
      d = 256; 
      break;
#endif
    case SCM_UNPACK(SCM_BOOL_T):
      d = 257; 
      break;
    case SCM_UNPACK(SCM_BOOL_F):
      d = 258; 
      break;
    case SCM_UNPACK(SCM_EOF_VAL):
      d = 259; 
      break;
    default: 
      d = 263;		/* perhaps should be error */
    }
    return d % n;
  default: 
    return 263 % n;	/* perhaps should be error */
  case scm_tc3_cons:
    switch SCM_TYP7(obj) {
    default: 
      return 263 % n;
    case scm_tc7_smob:
      return 263 % n;
    case scm_tc7_number:
      switch SCM_TYP16 (obj) {
      case scm_tc16_big:
        return scm_to_ulong (scm_modulo (obj, scm_from_ulong (n)));
      case scm_tc16_real:
	{
	  double r = SCM_REAL_VALUE (obj);
	  if (floor (r) == r) 
	    {
	      obj = scm_inexact_to_exact (obj);
	      return scm_to_ulong (scm_modulo (obj, scm_from_ulong (n)));
	    }
	}
        /* Fall through */
      case scm_tc16_complex:
      case scm_tc16_fraction:
	obj = scm_number_to_string (obj, scm_from_int (10));
        /* Fall through */
      }
      /* Fall through */
    case scm_tc7_string:
      {
	unsigned long hash =
	  scm_i_string_hash (obj) % n;
	return hash;
      }
    case scm_tc7_symbol:
      return scm_i_symbol_hash (obj) % n;
    case scm_tc7_pointer:
      {
	/* Pointer objects are typically used to store addresses of heap
	   objects.  On most platforms, these are at least 3-byte
	   aligned (on x86_64-*-gnu, `malloc' returns 4-byte aligned
	   addresses), so get rid of the least significant bits.  */
	scm_t_uintptr significant_bits;

	significant_bits = (scm_t_uintptr) SCM_POINTER_VALUE (obj) >> 4UL;
	return (size_t) significant_bits  % n;
      }
    case scm_tc7_wvect:
    case scm_tc7_vector:
      {
	size_t len = SCM_SIMPLE_VECTOR_LENGTH (obj);
	if (len > 5)
	  {
	    size_t i = d/2;
	    unsigned long h = 1;
	    while (i--)
	      {
		SCM elt = SCM_SIMPLE_VECTOR_REF (obj, h % len);
		h = ((h << 8) + (scm_hasher (elt, n, 2))) % n;
	      }
	    return h;
	  }
	else
	  {
	    size_t i = len;
	    unsigned long h = (n)-1;
	    while (i--)
	      {
		SCM elt = SCM_SIMPLE_VECTOR_REF (obj, h % len);
		h = ((h << 8) + (scm_hasher (elt, n, d/len))) % n;
	      }
	    return h;
	  }
      }
    case scm_tcs_cons_imcar: 
    case scm_tcs_cons_nimcar:
      if (d) return (scm_hasher (SCM_CAR (obj), n, d/2)
                     + scm_hasher (SCM_CDR (obj), n, d/2)) % n;
      else return 1;
    case scm_tc7_port:
      return ((SCM_RDNG & SCM_CELL_WORD_0 (obj)) ? 260 : 261) % n;
    case scm_tc7_program:
      return 262 % n;
    }
  }
}





unsigned long
scm_ihashq (SCM obj, unsigned long n)
{
  return (SCM_UNPACK (obj) >> 1) % n;
}


SCM_DEFINE (scm_hashq, "hashq", 2, 0, 0,
           (SCM key, SCM size),
	    "Determine a hash value for @var{key} that is suitable for\n"
	    "lookups in a hashtable of size @var{size}, where @code{eq?} is\n"
	    "used as the equality predicate.  The function returns an\n"
	    "integer in the range 0 to @var{size} - 1.  Note that\n"
	    "@code{hashq} may use internal addresses.  Thus two calls to\n"
	    "hashq where the keys are @code{eq?} are not guaranteed to\n"
	    "deliver the same value if the key object gets garbage collected\n"
	    "in between.  This can happen, for example with symbols:\n"
	    "@code{(hashq 'foo n) (gc) (hashq 'foo n)} may produce two\n"
	    "different values, since @code{foo} will be garbage collected.")
#define FUNC_NAME s_scm_hashq
{
  unsigned long sz = scm_to_unsigned_integer (size, 1, ULONG_MAX);
  return scm_from_ulong (scm_ihashq (key, sz));
}
#undef FUNC_NAME





unsigned long
scm_ihashv (SCM obj, unsigned long n)
{
  if (SCM_CHARP(obj))
    return ((unsigned long) (scm_c_downcase (SCM_CHAR (obj)))) % n; /* downcase!?!! */

  if (SCM_NUMP(obj))
    return (unsigned long) scm_hasher(obj, n, 10);
  else
    return SCM_UNPACK (obj) % n;
}


SCM_DEFINE (scm_hashv, "hashv", 2, 0, 0,
           (SCM key, SCM size),
	    "Determine a hash value for @var{key} that is suitable for\n"
	    "lookups in a hashtable of size @var{size}, where @code{eqv?} is\n"
	    "used as the equality predicate.  The function returns an\n"
	    "integer in the range 0 to @var{size} - 1.  Note that\n"
	    "@code{(hashv key)} may use internal addresses.  Thus two calls\n"
	    "to hashv where the keys are @code{eqv?} are not guaranteed to\n"
	    "deliver the same value if the key object gets garbage collected\n"
	    "in between.  This can happen, for example with symbols:\n"
	    "@code{(hashv 'foo n) (gc) (hashv 'foo n)} may produce two\n"
	    "different values, since @code{foo} will be garbage collected.")
#define FUNC_NAME s_scm_hashv
{
  unsigned long sz = scm_to_unsigned_integer (size, 1, ULONG_MAX);
  return scm_from_ulong (scm_ihashv (key, sz));
}
#undef FUNC_NAME





unsigned long
scm_ihash (SCM obj, unsigned long n)
{
  return (unsigned long) scm_hasher (obj, n, 10);
}

SCM_DEFINE (scm_hash, "hash", 2, 0, 0,
           (SCM key, SCM size),
	    "Determine a hash value for @var{key} that is suitable for\n"
	    "lookups in a hashtable of size @var{size}, where @code{equal?}\n"
	    "is used as the equality predicate.  The function returns an\n"
	    "integer in the range 0 to @var{size} - 1.")
#define FUNC_NAME s_scm_hash
{
  unsigned long sz = scm_to_unsigned_integer (size, 1, ULONG_MAX);
  return scm_from_ulong (scm_ihash (key, sz));
}
#undef FUNC_NAME





void
scm_init_hash ()
{
#include "libguile/hash.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
