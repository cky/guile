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


#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "eval.h"
#include "variable.h"
#include "alist.h"
#include "weaks.h"

#include "symbols.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif




/* NUM_HASH_BUCKETS is the number of symbol scm_hash table buckets. 
 */
#define NUM_HASH_BUCKETS 137




/* {Symbols}
 */


unsigned long 
scm_strhash (str, len, n)
     unsigned char *str;
     scm_sizet len;
     unsigned long n;
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

SCM 
scm_sym2vcell (sym, thunk, definep)
     SCM sym;
     SCM thunk;
     SCM definep;
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
	   lsym = *(lsymp = SCM_CDRLOC (lsym)))
	{
	  z = SCM_CAR (lsym);
	  if (SCM_CAR (z) == sym)
	    {
	      if (SCM_NFALSEP (definep))
		{
		  /* Move handle from scm_weak_symhash to scm_symhash. */
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
 * looks up the symbol in an arbitrary obarray.
 */

SCM 
scm_sym2ovcell_soft (sym, obarray)
     SCM sym;
     SCM obarray;
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


SCM 
scm_sym2ovcell (sym, obarray)
     SCM sym;
     SCM obarray;
{
  SCM answer;
  answer = scm_sym2ovcell_soft (sym, obarray);
  if (answer != SCM_BOOL_F)
    return answer;
  scm_wta (sym, "uninterned symbol? ", "");
  return SCM_UNSPECIFIED;		/* not reached */
}

/* Intern a symbol whose name is the LEN characters at NAME in OBARRAY.

   OBARRAY should be a vector of lists, indexed by the name's hash
   value, modulo OBARRAY's length.  Each list has the form 
   ((SYMBOL . VALUE) ...), where SYMBOL is a symbol, and VALUE is the
   value associated with that symbol (in the current module?  in the
   system module?)

   To "intern" a symbol means: if OBARRAY already contains a symbol by
   that name, return its (SYMBOL . VALUE) pair; otherwise, create a
   new symbol, add the pair (SYMBOL . SCM_UNDEFINED) to the
   appropriate list of the OBARRAY, and return the pair.

   If softness is non-zero, don't create a symbol if it isn't already
   in OBARRAY; instead, just return #f.

   If OBARRAY is SCM_BOOL_F, create a symbol listed in no obarray and
   return (SYMBOL . SCM_UNDEFINED).

   If OBARRAY is scm_symhash, and that doesn't contain the symbol,
   check scm_weak_symhash instead.  */


SCM 
scm_intern_obarray_soft (name, len, obarray, softness)
     char *name;
     scm_sizet len;
     SCM obarray;
     int softness;
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

  /* softness == -1 used to mean that it was known that the symbol
     wasn't already in the obarray.  I don't think there are any
     callers that use that case any more, but just in case...
     -- JimB, Oct 1996  */
  if (softness == -1)
    abort ();

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

  lsym = scm_makfromstr (name, len, SCM_SYMBOL_SLOTS);

  SCM_SETLENGTH (lsym, (long) len, scm_tc7_msymbol);
  SCM_SYMBOL_HASH (lsym) = scm_hash;
  SCM_SYMBOL_PROPS (lsym) = SCM_EOL;
  if (obarray == SCM_BOOL_F)
    {
      SCM answer;
      SCM_REALLOW_INTS;
      SCM_NEWCELL (answer);
      SCM_DEFER_INTS;
      SCM_SETCAR (answer, lsym);
      SCM_SETCDR (answer, SCM_UNDEFINED);
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


SCM
scm_intern_obarray (name, len, obarray)
     char *name;
     scm_sizet len;
     SCM obarray;
{
  return scm_intern_obarray_soft (name, len, obarray, 0);
}


SCM 
scm_intern (name, len)
     char *name;
     scm_sizet len;
{
  return scm_intern_obarray (name, len, scm_symhash);
}


SCM
scm_intern0 (name)
     char * name;
{
  return scm_intern (name, strlen (name));
}


/* Intern the symbol named NAME in scm_symhash, NAME is null-terminated.  */
SCM 
scm_sysintern0_no_module_lookup (name)
     char *name;
{
  SCM easy_answer;
  SCM_DEFER_INTS;
  easy_answer = scm_intern_obarray_soft (name, strlen (name), scm_symhash, 1);
  if (SCM_NIMP (easy_answer))
    {
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
      lsym = scm_cons (lsym, SCM_UNDEFINED);
      SCM_VELTS (scm_symhash)[scm_hash] = scm_cons (lsym, SCM_VELTS (scm_symhash)[scm_hash]);
      SCM_ALLOW_INTS;
      return lsym;
    }
}


/* Is it safe to access SCM_TOP_LEVEL_LOOKUP_CLOSURE_VAR?
 */
int scm_can_use_top_level_lookup_closure_var;

/* Intern the symbol named NAME in scm_symhash, and give it the value
   VAL.  NAME is null-terminated.  Use the current top_level lookup
   closure to give NAME its value.
 */
SCM
scm_sysintern (name, val)
     char *name;
     SCM val;
{
  SCM vcell = scm_sysintern0 (name);
  SCM_SETCDR (vcell, val);
  return vcell;
}

SCM
scm_sysintern0 (name)
     char *name;
{
  SCM lookup_proc;
  if (scm_can_use_top_level_lookup_closure_var && 
      SCM_NIMP (lookup_proc = SCM_CDR (scm_top_level_lookup_closure_var)))
    {
      SCM sym = SCM_CAR (scm_intern0 (name));
      SCM vcell = scm_sym2vcell (sym, lookup_proc, SCM_BOOL_T);
      if (vcell == SCM_BOOL_F)
	  scm_misc_error ("sysintern", "can't define variable", sym);
      return vcell;
    }
  else
    return scm_sysintern0_no_module_lookup (name);
}

/* Lookup the value of the symbol named by the nul-terminated string
   NAME in the current module.  */
SCM
scm_symbol_value0 (name)
     char *name;
{
  /* This looks silly - we look up the symbol twice.  But it is in
     fact necessary given the current module system because the module
     lookup closures are written in scheme which needs real symbols. */
  SCM symbol = scm_intern_obarray_soft (name, strlen (name), scm_symhash, 0);
  SCM vcell = scm_sym2vcell (SCM_CAR (symbol),
			     SCM_CDR (scm_top_level_lookup_closure_var),
			     SCM_BOOL_F);
  if (SCM_FALSEP (vcell))
    return SCM_UNDEFINED;
  return SCM_CDR (vcell);
}

SCM_PROC(s_symbol_p, "symbol?", 1, 0, 0, scm_symbol_p);

SCM
scm_symbol_p(x)
     SCM x;
{
  if SCM_IMP(x) return SCM_BOOL_F;
  return SCM_SYMBOLP(x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_symbol_to_string, "symbol->string", 1, 0, 0, scm_symbol_to_string);

SCM
scm_symbol_to_string(s)
     SCM s;
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_to_string);
  return scm_makfromstr(SCM_CHARS(s), (scm_sizet)SCM_LENGTH(s), 0);
}


SCM_PROC(s_string_to_symbol, "string->symbol", 1, 0, 0, scm_string_to_symbol);

SCM
scm_string_to_symbol(s)
     SCM s;
{
  SCM vcell;
  SCM answer;

  SCM_ASSERT(SCM_NIMP(s) && SCM_ROSTRINGP(s), s, SCM_ARG1, s_string_to_symbol);
  vcell = scm_intern(SCM_ROCHARS(s), (scm_sizet)SCM_LENGTH(s));
  answer = SCM_CAR (vcell);
  return answer;
}


SCM_PROC(s_string_to_obarray_symbol, "string->obarray-symbol", 2, 1, 0, scm_string_to_obarray_symbol);

SCM
scm_string_to_obarray_symbol(o, s, softp)
     SCM o;
     SCM s;
     SCM softp;
{
  SCM vcell;
  SCM answer;
  int softness;

  SCM_ASSERT(SCM_NIMP(s) && SCM_ROSTRINGP(s), s, SCM_ARG2,
	     s_string_to_obarray_symbol);
  SCM_ASSERT((o == SCM_BOOL_F)
	     || (o == SCM_BOOL_T)
	     || (SCM_NIMP(o) && SCM_VECTORP(o)),
	     o,
	     SCM_ARG1,
	     s_string_to_obarray_symbol);

  softness = ((softp != SCM_UNDEFINED) && (softp != SCM_BOOL_F));
  /* iron out some screwy calling conventions */
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  else if (o == SCM_BOOL_T)
    o = SCM_BOOL_F;
    
  vcell = scm_intern_obarray_soft (SCM_ROCHARS(s),
				   (scm_sizet)SCM_ROLENGTH(s),
				   o,
				   softness);
  if (vcell == SCM_BOOL_F)
    return vcell;
  answer = SCM_CAR (vcell);
  return answer;
}

SCM_PROC(s_intern_symbol, "intern-symbol", 2, 0, 0, scm_intern_symbol);

SCM
scm_intern_symbol(o, s)
     SCM o;
     SCM s;
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

SCM
scm_unintern_symbol(o, s)
     SCM o;
     SCM s;
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
	      SCM_SETCDR (lsym_follow, SCM_CDR(lsym));
	    SCM_ALLOW_INTS;
	    return SCM_BOOL_T;
	  }
      }
  }
  SCM_ALLOW_INTS;
  return SCM_BOOL_F;
}

SCM_PROC(s_symbol_binding, "symbol-binding", 2, 0, 0, scm_symbol_binding);

SCM
scm_symbol_binding (o, s)
     SCM o;
     SCM s;
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

SCM
scm_symbol_interned_p (o, s)
     SCM o;
     SCM s;
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

SCM 
scm_symbol_bound_p (o, s)
     SCM o;
     SCM s;
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

SCM 
scm_symbol_set_x (o, s, v)
     SCM o;
     SCM s;
     SCM v;
{
  SCM vcell;
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG2, s_symbol_set_x);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(SCM_NIMP(o) && SCM_VECTORP(o), o, SCM_ARG1, s_symbol_set_x);
  vcell = scm_sym2ovcell (s, o);
  SCM_SETCDR (vcell, v);
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
  SCM_SETCDR (string, SCM_EOL);
  SCM_SETCAR (string, SCM_EOL);
  SCM_SYMBOL_PROPS (s) = SCM_EOL;
  /* If it's a tc7_ssymbol, it comes from scm_symhash */
  SCM_SYMBOL_HASH (s) = scm_strhash (SCM_UCHARS (s),
				     (scm_sizet) SCM_LENGTH (s),
				     SCM_LENGTH (scm_symhash));
}


SCM_PROC(s_symbol_fref, "symbol-fref", 1, 0, 0, scm_symbol_fref);

SCM
scm_symbol_fref (s)
     SCM s;
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_fref);
  SCM_DEFER_INTS;
  if (SCM_TYP7(s) == scm_tc7_ssymbol)
    msymbolize (s);
  SCM_ALLOW_INTS;
  return SCM_SYMBOL_FUNC (s);
}


SCM_PROC(s_symbol_pref, "symbol-pref", 1, 0, 0, scm_symbol_pref);

SCM
scm_symbol_pref (s)
     SCM s;
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_pref);
  SCM_DEFER_INTS;
  if (SCM_TYP7(s) == scm_tc7_ssymbol)
    msymbolize (s);
  SCM_ALLOW_INTS;
  return SCM_SYMBOL_PROPS (s);
}


SCM_PROC(s_symbol_fset_x, "symbol-fset!", 2, 0, 0, scm_symbol_fset_x);

SCM
scm_symbol_fset_x (s, val)
     SCM s;
     SCM val;
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

SCM
scm_symbol_pset_x (s, val)
     SCM s;
     SCM val;
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

SCM
scm_symbol_hash (s)
     SCM s;
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_hash);
  if (SCM_TYP7(s) == scm_tc7_ssymbol)
    msymbolize (s);
  return SCM_MAKINUM ((unsigned long)s ^ SCM_SYMBOL_HASH (s));
}


static void copy_and_prune_obarray SCM_P ((SCM from, SCM to));

static void
copy_and_prune_obarray (from, to)
     SCM from;
     SCM to;
{
  int i;
  int length = SCM_LENGTH (from);
  for (i = 0; i < length; ++i)
    {
      SCM head = SCM_VELTS (from)[i]; /* GC protection */
      SCM ls = head;
      SCM res = SCM_EOL;
      SCM *lloc = &res;
      while (SCM_NIMP (ls))
	{
	  if (!SCM_UNBNDP (SCM_CDAR (ls)))
	    {
	      *lloc = scm_cons (SCM_CAR (ls), SCM_EOL);
	      lloc = SCM_CDRLOC (*lloc);
	    }
	  ls = SCM_CDR (ls);
	}
      SCM_VELTS (to)[i] = res;
    }
}


SCM_PROC(s_builtin_bindings, "builtin-bindings", 0, 0, 0, scm_builtin_bindings);

SCM
scm_builtin_bindings ()
{
  int length = SCM_LENGTH (scm_symhash);
  SCM obarray = scm_make_vector (SCM_MAKINUM (length), SCM_EOL);
  copy_and_prune_obarray (scm_symhash, obarray);
  return obarray;
}


SCM_PROC(s_builtin_weak_bindings, "builtin-weak-bindings", 0, 0, 0, scm_builtin_weak_bindings);

SCM
scm_builtin_weak_bindings ()
{
  int length = SCM_LENGTH (scm_weak_symhash);
  SCM obarray = scm_make_doubly_weak_hash_table (SCM_MAKINUM (length));
  copy_and_prune_obarray (scm_weak_symhash, obarray);
  return obarray;
}

static int gensym_counter;
static SCM gensym_prefix;

/*fixme* Optimize */
SCM_PROC (s_gensym, "gensym", 0, 2, 0, scm_gensym);

SCM
scm_gensym (name, obarray)
     SCM name;
     SCM obarray;
{
  SCM new;
  if (SCM_UNBNDP (name))
    name = gensym_prefix;
  else
    SCM_ASSERT (SCM_ROSTRINGP (name), name, SCM_ARG1, s_gensym);
  new = name;
  if (SCM_UNBNDP (obarray))
    {
      obarray = SCM_BOOL_F;
      goto skip_test;
    }
  else
    SCM_ASSERT (SCM_NIMP (obarray)
		&& (SCM_VECTORP (obarray) || SCM_WVECTP (obarray)),
		obarray,
		SCM_ARG2,
		s_gensym);
  while (scm_string_to_obarray_symbol (obarray, new, SCM_BOOL_T)
	 != SCM_BOOL_F)
    skip_test:
    new = scm_string_append
      (scm_cons2 (name,
		  scm_number_to_string (SCM_MAKINUM (gensym_counter++),
					SCM_UNDEFINED),
		  SCM_EOL));
  return scm_string_to_obarray_symbol (obarray, new, SCM_BOOL_F);
}

void
scm_init_symbols ()
{
  gensym_counter = 0;
  gensym_prefix = scm_permanent_object (scm_makfrom0str ("%%gensym"));
#include "symbols.x"
}
