/*	Copyright (C) 1995,1996,1997, 2000, 2001 Free Software Foundation, Inc.
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
  if (len > 5)
    {
      size_t i = 5;
      unsigned long h = 264;
      while (i--)
	h = (h << 8) + (unsigned) str[h % len];
      return h;
    }
  else
    {
      size_t i = len;
      unsigned long h = 0;
      while (i)
	h = (h << 8) + (unsigned) str[--i];
      return h;
    }
}


/* Dirk:FIXME:: why downcase for characters? (2x: scm_hasher, scm_ihashv) */
/* Dirk:FIXME:: scm_hasher could be made static. */


unsigned long
scm_hasher(SCM obj, unsigned long n, size_t d)
{
  switch (SCM_ITAG3 (obj)) {
  case scm_tc3_int_1: 
  case scm_tc3_int_2:
    return SCM_INUM(obj) % n;   /* SCM_INUMP(obj) */
  case scm_tc3_imm24:
    if (SCM_CHARP(obj))
      return (unsigned)(scm_downcase(SCM_CHAR(obj))) % n;
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
      switch SCM_TYP16 (obj) {
      case scm_tc16_big:
        return SCM_INUM (scm_modulo (obj, SCM_MAKINUM (n)));
      default: 
	return 263 % n;
      case scm_tc16_real:
	{
	  double r = SCM_REAL_VALUE (obj);
	  if (floor (r) == r) {
	    obj = scm_inexact_to_exact (obj);
	    if SCM_IMP (obj) return SCM_INUM (obj) % n;
	    return SCM_INUM (scm_modulo (obj, SCM_MAKINUM (n)));
	  }
	}
      case scm_tc16_complex:
	obj = scm_number_to_string (obj, SCM_MAKINUM (10));
      }
    case scm_tc7_string:
    case scm_tc7_substring:
      return scm_string_hash (SCM_STRING_UCHARS (obj), SCM_STRING_LENGTH (obj)) % n;
    case scm_tc7_symbol:
      return SCM_SYMBOL_HASH (obj) % n;
    case scm_tc7_wvect:
    case scm_tc7_vector:
      {
	size_t len = SCM_VECTOR_LENGTH(obj);
	SCM *data = SCM_VELTS(obj);
	if (len > 5)
	  {
	    size_t i = d/2;
	    unsigned long h = 1;
	    while (i--) h = ((h << 8) + (scm_hasher (data[h % len], n, 2))) % n;
	    return h;
	  }
	else
	  {
	    size_t i = len;
	    unsigned long h = (n)-1;
	    while (i--) h = ((h << 8) + (scm_hasher (data[i], n, d/len))) % n;
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
    case scm_tcs_closures: 
    case scm_tcs_subrs:
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
  SCM_VALIDATE_INUM_MIN (2, size, 0);
  return SCM_MAKINUM (scm_ihashq (key, SCM_INUM (size)));
}
#undef FUNC_NAME





unsigned long
scm_ihashv (SCM obj, unsigned long n)
{
  if (SCM_CHARP(obj))
    return ((unsigned long) (scm_downcase (SCM_CHAR (obj)))) % n; /* downcase!?!! */

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
  SCM_VALIDATE_INUM_MIN (2, size, 0);
  return SCM_MAKINUM (scm_ihashv (key, SCM_INUM (size)));
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
  SCM_VALIDATE_INUM_MIN (2, size, 0);
  return SCM_MAKINUM (scm_ihash (key, SCM_INUM (size)));
}
#undef FUNC_NAME





void
scm_init_hash ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/hash.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
