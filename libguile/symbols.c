/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "eval.h"
#include "variable.h"
#include "alist.h"
#include "mbstrings.h"

#include "symbols.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif




/* NUM_HASH_BUCKETS is the number of symbol scm_hash table buckets. 
 */
#define NUM_HASH_BUCKETS 137




/* {Symbols}
 */

#ifdef __STDC__
unsigned long 
scm_strhash (unsigned char *str, scm_sizet len, unsigned long n)
#else
unsigned long 
scm_strhash (str, len, n)
     unsigned char *str;
     scm_sizet len;
     unsigned long n;
#endif
{
  if (len > 5)
    {
      scm_sizet i = 5;
      unsigned long h = 264 % n;
      while (i--)
	h = ((h << 8) + ((unsigned) (scm_downcase (str[h % len])))) % n;
      return h;
    }
  else
    {
      scm_sizet i = len;
      unsigned long h = 0;
      while (i)
	h = ((h << 8) + ((unsigned) (scm_downcase (str[--i])))) % n;
      return h;
    }
}

int scm_symhash_dim = NUM_HASH_BUCKETS;


/* scm_sym2vcell
 * looks up the symbol in the symhash table. 
 */
#ifdef __STDC__
SCM 
scm_sym2vcell (SCM sym, SCM thunk, SCM definep)
#else
SCM 
scm_sym2vcell (sym, thunk, definep)
     SCM sym;
     SCM thunk;
     SCM definep;
#endif
{
  if (SCM_NIMP(thunk))
    {
      SCM var = scm_apply (thunk, sym, scm_cons(definep, scm_listofnull));

      if (var == SCM_BOOL_F)
	return SCM_BOOL_F;
      else
	{
	  if (SCM_IMP(var) || !SCM_VARIABLEP (var))
	    scm_wta (sym, "strangely interned symbol? ", "");
	  return SCM_VARVCELL (var);
	}
    }
  else
    {
      SCM lsym;
      SCM * lsymp;
      SCM z;
      scm_sizet scm_hash = scm_strhash (SCM_UCHARS (sym), (scm_sizet) SCM_LENGTH (sym),
				    (unsigned long) scm_symhash_dim);

      SCM_DEFER_INTS;
      for (lsym = SCM_VELTS (scm_symhash)[scm_hash]; SCM_NIMP (lsym); lsym = SCM_CDR (lsym))
	{
	  z = SCM_CAR (lsym);
	  if (SCM_CAR (z) == sym)
	    {
	      SCM_ALLOW_INTS;
	      return z;
	    }
	}

      for (lsym = *(lsymp = &SCM_VELTS (scm_weak_symhash)[scm_hash]);
	   SCM_NIMP (lsym);
	   lsym = *(lsymp = &SCM_CDR (lsym)))
	{
	  z = SCM_CAR (lsym);
	  if (SCM_CAR (z) == sym)
	    {
	      if (definep)
		{
		  *lsymp = SCM_CDR (lsym);
		  SCM_SETCDR (lsym, SCM_VELTS(scm_symhash)[scm_hash]);
		  SCM_VELTS(scm_symhash)[scm_hash] = lsym;
		}
	      SCM_ALLOW_INTS;
	      return z;
	    }
	}
      SCM_ALLOW_INTS;
      return scm_wta (sym, "uninterned symbol? ", "");
    }
}

/* scm_sym2ovcell
 * looks up the symbol in an arbitrary obarray (defaulting to scm_symhash).
 */
#ifdef __STDC__
SCM 
scm_sym2ovcell_soft (SCM sym, SCM obarray)
#else
SCM 
scm_sym2ovcell_soft (sym, obarray)
     SCM sym;
     SCM obarray;
#endif
{
  SCM lsym, z;
  scm_sizet scm_hash;

  scm_hash = scm_strhash (SCM_UCHARS (sym),
			  (scm_sizet) SCM_LENGTH (sym),
			  SCM_LENGTH (obarray));
  SCM_REDEFER_INTS;
  for (lsym = SCM_VELTS (obarray)[scm_hash];
       SCM_NIMP (lsym);
       lsym = SCM_CDR (lsym))
    {
      z = SCM_CAR (lsym);
      if (SCM_CAR (z) == sym)
	{
	  SCM_REALLOW_INTS;
	  return z;
	}
    }
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}

#ifdef __STDC__
SCM 
scm_sym2ovcell (SCM sym, SCM obarray)
#else
SCM 
scm_sym2ovcell (sym, obarray)
     SCM sym;
     SCM obarray;
#endif
{
  SCM answer;
  answer = scm_sym2ovcell_soft (sym, obarray);
  if (answer != SCM_BOOL_F)
    return answer;
  scm_wta (sym, "uninterned symbol? ", "");
  return SCM_UNSPECIFIED;		/* not reached */
}

#ifdef __STDC__
SCM 
scm_intern_obarray_soft (char *name, scm_sizet len, SCM obarray, int softness)
#else
SCM 
scm_intern_obarray_soft (name, len, obarray, softness)
     char *name;
     scm_sizet len;
     SCM obarray;
     int softness;
#endif
{
  SCM lsym;
  SCM z;
  register scm_sizet i;
  register unsigned char *tmp;
  scm_sizet scm_hash;

  SCM_REDEFER_INTS;

  i = len;
  tmp = (unsigned char *) name;

  if (obarray == SCM_BOOL_F)
    {
      scm_hash = scm_strhash (tmp, i, 1019);
      goto uninterned_symbol;
    }

  scm_hash = scm_strhash (tmp, i, SCM_LENGTH(obarray));

  if (softness == -1)
    goto mustintern_symbol;

 retry_new_obarray:
  for (lsym = SCM_VELTS (obarray)[scm_hash]; SCM_NIMP (lsym); lsym = SCM_CDR (lsym))
    {
      z = SCM_CAR (lsym);
      z = SCM_CAR (z);
      tmp = SCM_UCHARS (z);
      if (SCM_LENGTH (z) != len)
	goto trynext;
      for (i = len; i--;)
	if (((unsigned char *) name)[i] != tmp[i])
	  goto trynext;
      {
	SCM a;
	a = SCM_CAR (lsym);
	SCM_REALLOW_INTS;
	return a;
      }
    trynext:;
    }

  if (obarray == scm_symhash)
    {
      obarray = scm_weak_symhash;
      goto retry_new_obarray;
    }
  
 uninterned_symbol:
  if (softness)
    {
      SCM_REALLOW_INTS;
      return SCM_BOOL_F;
    }

 mustintern_symbol:
  lsym = scm_makfromstr (name, len, SCM_SYMBOL_SLOTS);

  SCM_SETLENGTH (lsym, (long) len, scm_tc7_msymbol);
  SCM_SYMBOL_MULTI_BYTE_STRINGP (lsym) = SCM_BOOL_F;
  SCM_SYMBOL_HASH (lsym) = scm_hash;
  SCM_SYMBOL_PROPS (lsym) = SCM_EOL;
  if (obarray == SCM_BOOL_F)
    {
      SCM answer;
      SCM_REALLOW_INTS;
      SCM_NEWCELL (answer);
      SCM_DEFER_INTS;
      SCM_CAR (answer) = lsym;
      SCM_CDR (answer) = SCM_UNDEFINED;
      SCM_REALLOW_INTS;
      return answer;
    }
  else
    {
      SCM a;
      SCM b;

      SCM_NEWCELL (a);
      SCM_NEWCELL (b);
      SCM_SETCAR (a, lsym);
      SCM_SETCDR (a, SCM_UNDEFINED);
      SCM_SETCAR (b, a);
      SCM_SETCDR (b, SCM_VELTS(obarray)[scm_hash]);
      SCM_VELTS(obarray)[scm_hash] = b;
      SCM_REALLOW_INTS;
      return SCM_CAR (b);
    }
}

#ifdef __STDC__
SCM
scm_intern_obarray (char *name, scm_sizet len, SCM obarray)
#else
SCM
scm_intern_obarray (name, len, obarray)
     char *name;
     scm_sizet len;
     SCM obarray;
#endif
{
  return scm_intern_obarray_soft (name, len, obarray, 0);
}


#ifdef __STDC__
SCM 
scm_intern (char *name, scm_sizet len)
#else
SCM 
scm_intern (name, len)
     char *name;
     scm_sizet len;
#endif
{
  return scm_intern_obarray (name, len, scm_symhash);
}

#ifdef __STDC__
SCM
scm_intern0 (char * name)
#else
SCM
scm_intern0 (name)
     char * name;
#endif
{
  return scm_intern (name, strlen (name));
}


#ifdef __STDC__
SCM 
scm_sysintern (char *name, SCM val)
#else
SCM 
scm_sysintern (name, val)
     char *name;
     SCM val;
#endif
{
  SCM easy_answer;
  SCM_DEFER_INTS;
  easy_answer = scm_intern_obarray_soft (name, strlen (name), scm_symhash, 1);
  if (SCM_NIMP (easy_answer))
    {
      SCM_CDR (easy_answer) = val;
      SCM_ALLOW_INTS;
      return easy_answer;
    }
  else
    {
      SCM lsym;
      scm_sizet len = strlen (name);
      register unsigned char *tmp = (unsigned char *) name;
      scm_sizet scm_hash = scm_strhash (tmp, len, (unsigned long) scm_symhash_dim);
      SCM_NEWCELL (lsym);
      SCM_SETLENGTH (lsym, (long) len, scm_tc7_ssymbol);
      SCM_SETCHARS (lsym, name);
      lsym = scm_cons (lsym, val);
      SCM_VELTS (scm_symhash)[scm_hash] = scm_cons (lsym, SCM_VELTS (scm_symhash)[scm_hash]);
      SCM_ALLOW_INTS;
      return lsym;
    }
}


SCM_PROC(s_symbol_p, "symbol?", 1, 0, 0, scm_symbol_p);
#ifdef __STDC__
SCM
scm_symbol_p(SCM x)
#else
SCM
scm_symbol_p(x)
     SCM x;
#endif
{
	if SCM_IMP(x) return SCM_BOOL_F;
	return SCM_SYMBOLP(x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_symbol_to_string, "symbol->string", 1, 0, 0, scm_symbol_to_string);
#ifdef __STDC__
SCM
scm_symbol_to_string(SCM s)
#else
SCM
scm_symbol_to_string(s)
     SCM s;
#endif
{
	SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_to_string);
	return scm_makfromstr(SCM_CHARS(s), (scm_sizet)SCM_LENGTH(s), 0);
}


SCM_PROC(s_string_to_symbol, "string->symbol", 1, 0, 0, scm_string_to_symbol);
#ifdef __STDC__
SCM
scm_string_to_symbol(SCM s)
#else
SCM
scm_string_to_symbol(s)
     SCM s;
#endif
{
  SCM vcell;
  SCM answer;

  SCM_ASSERT(SCM_NIMP(s) && SCM_ROSTRINGP(s), s, SCM_ARG1, s_string_to_symbol);
  vcell = scm_intern(SCM_ROCHARS(s), (scm_sizet)SCM_LENGTH(s));
  answer = SCM_CAR (vcell);
  if (SCM_TYP7 (answer) == scm_tc7_msymbol)
    {
      if (SCM_REGULAR_STRINGP (s))
	SCM_SYMBOL_MULTI_BYTE_STRINGP (answer) = SCM_BOOL_F;
      else
	SCM_SYMBOL_MULTI_BYTE_STRINGP (answer) = SCM_BOOL_T;
    }
  return answer;
}


SCM_PROC(s_string_to_obarray_symbol, "string->obarray-symbol", 2, 1, 0, scm_string_to_obarray_symbol);
#ifdef __STDC__
SCM
scm_string_to_obarray_symbol(SCM o, SCM s, SCM softp)
#else
SCM
scm_string_to_obarray_symbol(o, s, softp)
     SCM o;
     SCM s;
     SCM softp;
#endif
{
  SCM vcell;
  SCM answer;
  int softness;

  SCM_ASSERT(SCM_NIMP(s) && SCM_ROSTRINGP(s), s, SCM_ARG2, s_string_to_obarray_symbol);
  SCM_ASSERT((o == SCM_BOOL_F) || (o == SCM_BOOL_T) || (SCM_NIMP(o) && SCM_VECTORP(o)),
	 o, SCM_ARG1, s_string_to_obarray_symbol);

  softness = ((softp != SCM_UNDEFINED) && (softp != SCM_BOOL_F));
  /* iron out some screwy calling conventions */
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  else if (o == SCM_BOOL_T)
    o = SCM_BOOL_F;
    
  vcell = scm_intern_obarray_soft (SCM_ROCHARS(s), (scm_sizet)SCM_ROLENGTH(s), o, softness);
  if (vcell == SCM_BOOL_F)
    return vcell;
  answer = SCM_CAR (vcell);
  if (SCM_TYP7 (s) == scm_tc7_msymbol)
    {
      if (SCM_REGULAR_STRINGP (s))
	SCM_SYMBOL_MULTI_BYTE_STRINGP (answer) = SCM_BOOL_F;
      else
	SCM_SYMBOL_MULTI_BYTE_STRINGP (answer) = SCM_BOOL_T;
    }
  return answer;
}

SCM_PROC(s_intern_symbol, "intern-symbol", 2, 0, 0, scm_intern_symbol);
#ifdef __STDC__
SCM
scm_intern_symbol(SCM o, SCM s)
#else
SCM
scm_intern_symbol(o, s)
     SCM o;
     SCM s;
#endif
{
        scm_sizet hval;
	SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG2, s_intern_symbol);
	if (o == SCM_BOOL_F)
	  o = scm_symhash;
	SCM_ASSERT(SCM_NIMP(o) && SCM_VECTORP(o), o, SCM_ARG1, s_intern_symbol);
	hval = scm_strhash (SCM_UCHARS (s), SCM_LENGTH (s), SCM_LENGTH(o));
	/* If the symbol is already interned, simply return. */
	SCM_REDEFER_INTS;
	{
	  SCM lsym;
	  SCM sym;
	  for (lsym = SCM_VELTS (o)[hval];
	       SCM_NIMP (lsym);
	       lsym = SCM_CDR (lsym))
	    {
	      sym = SCM_CAR (lsym);
	      if (SCM_CAR (sym) == s)
		{
		  SCM_REALLOW_INTS;
		  return SCM_UNSPECIFIED;
		}
	    }
	  SCM_VELTS (o)[hval] =
	    scm_acons (s, SCM_UNDEFINED, SCM_VELTS (o)[hval]);
	}
	SCM_REALLOW_INTS;
	return SCM_UNSPECIFIED;
}

SCM_PROC(s_unintern_symbol, "unintern-symbol", 2, 0, 0, scm_unintern_symbol);
#ifdef __STDC__
SCM
scm_unintern_symbol(SCM o, SCM s)
#else
SCM
scm_unintern_symbol(o, s)
     SCM o;
     SCM s;
#endif
{
        scm_sizet hval;
	SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG2, s_unintern_symbol);
	if (o == SCM_BOOL_F)
	  o = scm_symhash;
	SCM_ASSERT(SCM_NIMP(o) && SCM_VECTORP(o), o, SCM_ARG1, s_unintern_symbol);
	hval = scm_strhash (SCM_UCHARS (s), SCM_LENGTH (s), SCM_LENGTH(o));
	SCM_DEFER_INTS;
	{
	  SCM lsym_follow;
	  SCM lsym;
	  SCM sym;
	  for (lsym = SCM_VELTS (o)[hval], lsym_follow = SCM_BOOL_F;
	       SCM_NIMP (lsym);
	       lsym_follow = lsym, lsym = SCM_CDR (lsym))
	    {
	      sym = SCM_CAR (lsym);
	      if (SCM_CAR (sym) == s)
		{
		  /* Found the symbol to unintern. */
		  if (lsym_follow == SCM_BOOL_F)
		    SCM_VELTS(o)[hval] = lsym;
		  else
		    SCM_CDR(lsym_follow) = SCM_CDR(lsym);
		  SCM_ALLOW_INTS;
		  return SCM_BOOL_T;
		}
	    }
	}
	SCM_ALLOW_INTS;
	return SCM_BOOL_F;
}

SCM_PROC(s_symbol_binding, "symbol-binding", 2, 0, 0, scm_symbol_binding);
#ifdef __STDC__
SCM
scm_symbol_binding (SCM o, SCM s)
#else
SCM
scm_symbol_binding (o, s)
     SCM o;
     SCM s;
#endif
{
  SCM vcell;
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG2, s_symbol_binding);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(SCM_NIMP(o) && SCM_VECTORP(o), o, SCM_ARG1, s_symbol_binding);
  vcell = scm_sym2ovcell (s, o);
  return SCM_CDR(vcell);
}


SCM_PROC(s_symbol_interned_p, "symbol-interned?", 2, 0, 0, scm_symbol_interned_p);
#ifdef __STDC__
SCM
scm_symbol_interned_p (SCM o, SCM s)
#else
SCM
scm_symbol_interned_p (o, s)
     SCM o;
     SCM s;
#endif
{
  SCM vcell;
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG2, s_symbol_interned_p);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(SCM_NIMP(o) && SCM_VECTORP(o), o, SCM_ARG1, s_symbol_interned_p);
  vcell = scm_sym2ovcell_soft (s, o);
  if (SCM_IMP(vcell) && (o == scm_symhash))
    vcell = scm_sym2ovcell_soft (s, scm_weak_symhash);
  return (SCM_NIMP(vcell)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC(s_symbol_bound_p, "symbol-bound?", 2, 0, 0, scm_symbol_bound_p);
#ifdef __STDC__
SCM 
scm_symbol_bound_p (SCM o, SCM s)
#else
SCM 
scm_symbol_bound_p (o, s)
     SCM o;
     SCM s;
#endif
{
  SCM vcell;
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG2, s_symbol_bound_p);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(SCM_NIMP(o) && SCM_VECTORP(o), o, SCM_ARG1, s_symbol_bound_p);
  vcell = scm_sym2ovcell_soft (s, o);
  return ((  SCM_NIMP(vcell)
	   && (SCM_CDR(vcell) != SCM_UNDEFINED))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC(s_symbol_set_x, "symbol-set!", 3, 0, 0, scm_symbol_set_x);
#ifdef __STDC__
SCM 
scm_symbol_set_x (SCM o, SCM s, SCM v)
#else
SCM 
scm_symbol_set_x (o, s, v)
     SCM o;
     SCM s;
     SCM v;
#endif
{
  SCM vcell;
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG2, s_symbol_set_x);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(SCM_NIMP(o) && SCM_VECTORP(o), o, SCM_ARG1, s_symbol_set_x);
  vcell = scm_sym2ovcell (s, o);
  SCM_CDR(vcell) = v;
  return SCM_UNSPECIFIED;
}

static void
msymbolize (s)
     SCM s;
{
  SCM string;
  string = scm_makfromstr (SCM_CHARS (s), SCM_LENGTH (s), SCM_SYMBOL_SLOTS);
  SCM_SETCHARS (s, SCM_CHARS (string));
  SCM_SETLENGTH (s, SCM_LENGTH (s), scm_tc7_msymbol);
  SCM_SYMBOL_MULTI_BYTE_STRINGP (s) = SCM_BOOL_F;
  SCM_CDR (string) = SCM_EOL;
  SCM_CAR (string) = SCM_EOL;
}


SCM_PROC(s_symbol_fref, "symbol-fref", 1, 0, 0, scm_symbol_fref);
#ifdef __STDC__
SCM
scm_symbol_fref (SCM s)
#else
SCM
scm_symbol_fref (s)
     SCM s;
#endif
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_fref);
  SCM_DEFER_INTS;
  if (SCM_TYP7(s) == scm_tc7_ssymbol)
    msymbolize (s);
  SCM_ALLOW_INTS;
  return SCM_SYMBOL_FUNC (s);
}


SCM_PROC(s_symbol_pref, "symbol-pref", 1, 0, 0, scm_symbol_pref);
#ifdef __STDC__
SCM
scm_symbol_pref (SCM s)
#else
SCM
scm_symbol_pref (s)
     SCM s;
#endif
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_pref);
  SCM_DEFER_INTS;
  if (SCM_TYP7(s) == scm_tc7_ssymbol)
    msymbolize (s);
  SCM_ALLOW_INTS;
  return SCM_SYMBOL_PROPS (s);
}


SCM_PROC(s_symbol_fset_x, "symbol-fset!", 2, 0, 0, scm_symbol_fset_x);
#ifdef __STDC__
SCM
scm_symbol_fset_x (SCM s, SCM val)
#else
SCM
scm_symbol_fset_x (s, val)
     SCM s;
     SCM val;
#endif
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_fset_x);
  SCM_DEFER_INTS;
  if (SCM_TYP7(s) == scm_tc7_ssymbol)
    msymbolize (s);
  SCM_ALLOW_INTS;
  SCM_SYMBOL_FUNC (s) = val;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_symbol_pset_x, "symbol-pset!", 2, 0, 0, scm_symbol_pset_x);
#ifdef __STDC__
SCM
scm_symbol_pset_x (SCM s, SCM val)
#else
SCM
scm_symbol_pset_x (s, val)
     SCM s;
     SCM val;
#endif
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_pset_x);
  SCM_DEFER_INTS;
  if (SCM_TYP7(s) == scm_tc7_ssymbol)
    msymbolize (s);
  SCM_SYMBOL_PROPS (s) = val;
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_symbol_hash, "symbol-hash", 1, 0, 0, scm_symbol_hash);
#ifdef __STDC__
SCM
scm_symbol_hash (SCM s)
#else
SCM
scm_symbol_hash (s)
     SCM s;
#endif
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_hash);
  return SCM_MAKINUM ((unsigned long)s ^ SCM_SYMBOL_HASH (s));
}


#ifdef __STDC__
void
scm_init_symbols (void)
#else
void
scm_init_symbols ()
#endif
{
#include "symbols.x"
}

