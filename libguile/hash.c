/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "_scm.h"
#include "chars.h"

#include "scm_validate.h"
#include "hash.h"


#ifndef floor
extern double floor();
#endif


unsigned long
scm_hasher(SCM obj, unsigned long n, scm_sizet d)
{
  switch (7 & (int) obj) {
  case 2: case 6:		/* SCM_INUMP(obj) */
    return SCM_INUM(obj) % n;
  case 4:
    if SCM_ICHRP(obj)
      return (unsigned)(scm_downcase(SCM_ICHR(obj))) % n;
    switch ((int) obj) {
#ifndef SICP
    case (int) SCM_EOL: d = 256; break;
#endif
    case (int) SCM_BOOL_T: d = 257; break;
    case (int) SCM_BOOL_F: d = 258; break;
    case (int) SCM_EOF_VAL: d = 259; break;
    default: d = 263;		/* perhaps should be error */
    }
    return d % n;
  default: return 263 % n;	/* perhaps should be error */
  case 0:
    switch SCM_TYP7(obj) {
    default: return 263 % n;
    case scm_tc7_smob:
      switch SCM_TYP16(obj) {
      case scm_tcs_bignums:
      bighash: return SCM_INUM(scm_modulo(obj, SCM_MAKINUM(n)));
      default: return 263 % n;
#ifdef SCM_FLOATS
      case scm_tc16_flo:
	if SCM_REALP(obj) {
	  double r = SCM_REALPART(obj);
	  if (floor(r)==r) {
	    obj = scm_inexact_to_exact (obj);
	    if SCM_IMP(obj) return SCM_INUM(obj) % n;
	    goto bighash;
	  }
	}
	obj = scm_number_to_string(obj, SCM_MAKINUM(10));
#endif
      }
    case scm_tcs_symbols:
    case scm_tc7_string:
    case scm_tc7_substring:
      return scm_strhash(SCM_ROUCHARS(obj), (scm_sizet) SCM_ROLENGTH(obj), n);
    case scm_tc7_wvect:
    case scm_tc7_vector:
      {
	scm_sizet len = SCM_LENGTH(obj);
	SCM *data = SCM_VELTS(obj);
	if (len>5)
	  {
	    scm_sizet i = d/2;
	    unsigned long h = 1;
	    while (i--) h = ((h<<8) + (scm_hasher(data[h % len], n, 2))) % n;
	    return h;
	  }
	else
	  {
	    scm_sizet i = len;
	    unsigned long h = (n)-1;
	    while (i--) h = ((h<<8) + (scm_hasher(data[i], n, d/len))) % n;
	    return h;
	  }
      }
    case scm_tcs_cons_imcar: case scm_tcs_cons_nimcar:
      if (d) return (scm_hasher(SCM_CAR(obj), n, d/2)+scm_hasher(SCM_CDR(obj), n, d/2)) % n;
      else return 1;
    case scm_tc7_port:
      return ((SCM_RDNG & SCM_CAR(obj)) ? 260 : 261) % n;
    case scm_tcs_closures: case scm_tc7_contin: case scm_tcs_subrs:
      return 262 % n;
    }
  }
}





unsigned int
scm_ihashq (SCM obj, unsigned int n)
{
  return (((unsigned int) obj) >> 1) % n;
}


SCM_DEFINE (scm_hashq, "hashq", 2, 0, 0,
           (SCM obj, SCM n),
	    "@deffnx primitive hashv key size\n"
	    "@deffnx primitive hash key size\n"
	    "Default hash functions for Guile hash tables.  @var{key} is the\n"
	    "object to be hashed, and @var{size} is the size of the target hash\n"
	    "table.  Each function returns an integer in the range 0 to\n"
	    "@var{size}-1.")
#define FUNC_NAME s_scm_hashq
{
  SCM_VALIDATE_INUM_MIN (2,n,0);
  return SCM_MAKINUM(scm_ihashq (obj, SCM_INUM (n)));
}
#undef FUNC_NAME





unsigned int
scm_ihashv (SCM obj, unsigned int n)
{
  if (SCM_ICHRP(obj))
    return ((unsigned int)(scm_downcase(SCM_ICHR(obj)))) % n; /* downcase!?!! */

  if (SCM_NUMP(obj))
    return (unsigned int) scm_hasher(obj, n, 10);
  else
    return ((unsigned int)obj) % n;
}


SCM_DEFINE (scm_hashv, "hashv", 2, 0, 0,
           (SCM obj, SCM n),
	    "")
#define FUNC_NAME s_scm_hashv
{
  SCM_VALIDATE_INUM_MIN (2,n,0);
  return SCM_MAKINUM(scm_ihashv (obj, SCM_INUM (n)));
}
#undef FUNC_NAME





unsigned int
scm_ihash (SCM obj, unsigned int n)
{
  return (unsigned int)scm_hasher (obj, n, 10);
}

SCM_DEFINE (scm_hash, "hash", 2, 0, 0,
           (SCM obj, SCM n),
	    "")
#define FUNC_NAME s_scm_hash
{
  SCM_VALIDATE_INUM_MIN (2,n,0);
  return SCM_MAKINUM(scm_ihash(obj, SCM_INUM(n)));
}
#undef FUNC_NAME





void
scm_init_hash ()
{
#include "hash.x"
}

