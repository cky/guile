/*	Copyright (C) 1995,1996,1997,1998, 2000, 2001 Free Software Foundation, Inc.
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



#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/hash.h"
#include "libguile/smob.h"
#include "libguile/variable.h"
#include "libguile/alist.h"
#include "libguile/fluids.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"
#include "libguile/weaks.h"
#include "libguile/modules.h"

#include "libguile/validate.h"
#include "libguile/symbols.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



static SCM symbols;

#ifdef GUILE_DEBUG
SCM_DEFINE (scm_sys_symbols, "%symbols", 0, 0, 0,
	    (),
	    "Return the system symbol obarray.")
#define FUNC_NAME s_scm_sys_symbols
{
  return symbols;
}
#undef FUNC_NAME
#endif



static char *
duplicate_string (const char * src, unsigned long length)
{
  char * dst = scm_must_malloc (length + 1, "duplicate_string");
  memcpy (dst, src, length);
  dst[length] = 0;
  return dst;
}



/* {Symbols}
 */


SCM
scm_mem2symbol (const char *name, scm_sizet len)
{
  scm_sizet raw_hash = scm_string_hash ((const unsigned char *) name, len);
  scm_sizet hash = raw_hash % SCM_VECTOR_LENGTH (symbols);

  {
    /* Try to find the symbol in the symbols table */

    SCM l;

    for (l = SCM_VELTS (symbols) [hash]; !SCM_NULLP (l); l = SCM_CDR (l))
      {
	SCM sym = SCM_CAAR (l);
	if (SCM_SYMBOL_HASH (sym) == raw_hash && SCM_SYMBOL_LENGTH (sym) == len)
	  {
	    char *chrs = SCM_SYMBOL_CHARS (sym);
	    scm_sizet i = len;

	    while (i != 0)
	      {
		--i;
		if (name[i] != chrs[i])
		  goto next_symbol;
	      }

	    return sym;
	  }
      next_symbol:
	;
      }
  }

  {
    /* The symbol was not found - create it. */

    SCM symbol;
    SCM cell;
    SCM slot;

    SCM_NEWCELL2 (symbol);
    SCM_SET_SYMBOL_CHARS (symbol, duplicate_string (name, len));
    SCM_SET_SYMBOL_HASH (symbol, raw_hash);
    SCM_SET_PROP_SLOTS (symbol, scm_cons (SCM_BOOL_F, SCM_EOL));
    SCM_SET_SYMBOL_LENGTH (symbol, (long) len);

    cell = scm_cons (symbol, SCM_UNDEFINED);
    slot = SCM_VELTS (symbols) [hash];
    SCM_VELTS (symbols) [hash] = scm_cons (cell, slot);

    return symbol;
  }
}


SCM
scm_str2symbol (const char *str)
{
  return scm_mem2symbol (str, strlen (str));
}


/* scm_sym2vcell
 * looks up the symbol in the symhash table. 
 */

SCM 
scm_sym2vcell (SCM sym, SCM thunk, SCM definep)
#define FUNC_NAME "scm_sym2vcell"
{
  if (SCM_NIMP (thunk))
    {
      SCM var;

      if (SCM_EVAL_CLOSURE_P (thunk))
	/* Bypass evaluator in the standard case. */
	var = scm_eval_closure_lookup (thunk, sym, definep);
      else
	var = scm_apply (thunk, sym, scm_cons (definep, scm_listofnull));

      if (SCM_FALSEP (var))
	return SCM_BOOL_F;
      else if (SCM_VARIABLEP (var))
	return SCM_VARVCELL (var);
      else
	SCM_MISC_ERROR ("strangely interned symbol: ~S", SCM_LIST1 (sym));
    }
  else
    {
      SCM lsym;
      scm_sizet hash;

      SCM_DEFER_INTS;
      hash = SCM_SYMBOL_HASH (sym) % SCM_VECTOR_LENGTH (scm_symhash);
      for (lsym = SCM_VELTS (scm_symhash)[hash]; SCM_NIMP (lsym); lsym = SCM_CDR (lsym))
	{
	  SCM z = SCM_CAR (lsym);
	  if (SCM_EQ_P (SCM_CAR (z), sym))
	    {
	      SCM_ALLOW_INTS;
	      return z;
	    }
	}

      if (!SCM_FALSEP (definep))
	{
	  SCM cell = scm_cons (sym, SCM_UNDEFINED);
	  SCM slot = SCM_VELTS (scm_symhash) [hash];

	  SCM_VELTS (scm_symhash) [hash] = scm_cons (cell, slot);

	  SCM_ALLOW_INTS;
	  return cell;
	}
      else
	{
	  SCM_ALLOW_INTS;
	  return SCM_BOOL_F;
	}
    }
}
#undef FUNC_NAME


/* scm_sym2ovcell
 * looks up the symbol in an arbitrary obarray.
 */

SCM 
scm_sym2ovcell_soft (SCM sym, SCM obarray)
{
  SCM lsym, z;
  scm_sizet hash = SCM_SYMBOL_HASH (sym) % SCM_VECTOR_LENGTH (obarray);
  SCM_REDEFER_INTS;
  for (lsym = SCM_VELTS (obarray)[hash];
       SCM_NIMP (lsym);
       lsym = SCM_CDR (lsym))
    {
      z = SCM_CAR (lsym);
      if (SCM_EQ_P (SCM_CAR (z), sym))
	{
	  SCM_REALLOW_INTS;
	  return z;
	}
    }
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}


SCM 
scm_sym2ovcell (SCM sym, SCM obarray)
#define FUNC_NAME "scm_sym2ovcell"
{
  SCM answer;
  answer = scm_sym2ovcell_soft (sym, obarray);
  if (!SCM_FALSEP (answer))
    return answer;
  SCM_MISC_ERROR ("uninterned symbol: ~S", SCM_LIST1 (sym));
  return SCM_UNSPECIFIED;		/* not reached */
}
#undef FUNC_NAME


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
   return (SYMBOL . SCM_UNDEFINED).  */


SCM 
scm_intern_obarray_soft (const char *name,scm_sizet len,SCM obarray,unsigned int softness)
{
  SCM symbol = scm_mem2symbol (name, len);
  scm_sizet raw_hash = SCM_SYMBOL_HASH (symbol);
  scm_sizet hash;
  SCM lsym;

  if (SCM_FALSEP (obarray))
    {
      if (softness)
	return SCM_BOOL_F;
      else
	return scm_cons (symbol, SCM_UNDEFINED);
    }

  hash = raw_hash % SCM_VECTOR_LENGTH (obarray);

  for (lsym = SCM_VELTS (obarray)[hash]; SCM_NIMP (lsym); lsym = SCM_CDR (lsym))
    {
      SCM a = SCM_CAR (lsym);
      SCM z = SCM_CAR (a);
      if (SCM_EQ_P (z, symbol))
	return a;
    }
  
  if (softness)
    {
      return SCM_BOOL_F;
    }
  else
    {
      SCM cell = scm_cons (symbol, SCM_UNDEFINED);
      SCM slot = SCM_VELTS (obarray) [hash];

      SCM_VELTS (obarray) [hash] = scm_cons (cell, slot);

      return cell;
    }
}


SCM
scm_intern_obarray (const char *name,scm_sizet len,SCM obarray)
{
  return scm_intern_obarray_soft (name, len, obarray, 0);
}


SCM 
scm_intern (const char *name,scm_sizet len)
{
  return scm_intern_obarray (name, len, scm_symhash);
}


SCM
scm_intern0 (const char * name)
{
  return scm_intern (name, strlen (name));
}


/* Intern the symbol named NAME in scm_symhash, NAME is null-terminated.  */
SCM 
scm_sysintern0_no_module_lookup (const char *name)
{
  scm_sizet len = strlen (name);
  SCM easy_answer;
  SCM_DEFER_INTS;
  easy_answer = scm_intern_obarray_soft (name, len, scm_symhash, 1);
  if (SCM_NIMP (easy_answer))
    {
      SCM_ALLOW_INTS;
      return easy_answer;
    }
  else
    {
      SCM symbol = scm_mem2symbol (name, len);
      scm_sizet raw_hash = SCM_SYMBOL_HASH (symbol);
      scm_sizet hash = raw_hash % SCM_VECTOR_LENGTH (scm_symhash);
      SCM cell = scm_cons (symbol, SCM_UNDEFINED);
      SCM slot = SCM_VELTS (scm_symhash) [hash];

      SCM_VELTS (scm_symhash) [hash] = scm_cons (cell, slot);
      SCM_ALLOW_INTS;
      return cell;
    }
}

/* Intern the symbol named NAME in scm_symhash, and give it the value
   VAL.  NAME is null-terminated.  Use the current top_level lookup
   closure to give NAME its value.
 */
SCM
scm_sysintern (const char *name, SCM val)
{
  SCM vcell = scm_sysintern0 (name);
  SCM_SETCDR (vcell, val);
  return vcell;
}

SCM
scm_sysintern0 (const char *name)
{
  SCM lookup_proc;
  if (scm_module_system_booted_p
      && SCM_NIMP (lookup_proc = SCM_TOP_LEVEL_LOOKUP_CLOSURE))
    {
      SCM sym = scm_str2symbol (name);
      SCM vcell = scm_sym2vcell (sym, lookup_proc, SCM_BOOL_T);
      if (SCM_FALSEP (vcell))
	  scm_misc_error ("sysintern0", "can't define variable", sym);
      return vcell;
    }
  else
    return scm_sysintern0_no_module_lookup (name);
}

/* Lookup the value of the symbol named by the nul-terminated string
   NAME in the current module.  */
SCM
scm_symbol_value0 (const char *name)
{
  /* This looks silly - we look up the symbol twice.  But it is in
     fact necessary given the current module system because the module
     lookup closures are written in scheme which needs real symbols. */
  SCM symbol = scm_str2symbol (name);
  SCM vcell = scm_sym2vcell (symbol, SCM_TOP_LEVEL_LOOKUP_CLOSURE, SCM_BOOL_F);
  if (SCM_FALSEP (vcell))
    return SCM_UNDEFINED;
  return SCM_CDR (vcell);
}


SCM_DEFINE (scm_symbol_p, "symbol?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a symbol, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_symbol_p
{
  return SCM_BOOL (SCM_SYMBOLP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_to_string, "symbol->string", 1, 0, 0, 
           (SCM s),
	    "Return the name of @var{symbol} as a string.  If the symbol was\n"
	    "part of an object returned as the value of a literal expression\n"
	    "(section @pxref{Literal expressions,,,r4rs, The Revised^4\n"
	    "Report on Scheme}) or by a call to the @code{read} procedure,\n"
	    "and its name contains alphabetic characters, then the string\n"
	    "returned will contain characters in the implementation's\n"
	    "preferred standard case---some implementations will prefer\n"
	    "upper case, others lower case.  If the symbol was returned by\n"
	    "@code{string->symbol}, the case of characters in the string\n"
	    "returned will be the same as the case in the string that was\n"
	    "passed to @code{string->symbol}.  It is an error to apply\n"
	    "mutation procedures like @code{string-set!} to strings returned\n"
	    "by this procedure.\n"
	    "\n"
	    "The following examples assume that the implementation's\n"
	    "standard case is lower case:\n"
	    "\n"
	    "@lisp\n"
	    "(symbol->string 'flying-fish)    @result{} \"flying-fish\"\n"
	    "(symbol->string 'Martin)         @result{}  \"martin\"\n"
	    "(symbol->string\n"
	    "   (string->symbol \"Malvina\")) @result{} \"Malvina\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_symbol_to_string
{
  SCM_VALIDATE_SYMBOL (1, s);
  return scm_makfromstr (SCM_SYMBOL_CHARS (s), SCM_SYMBOL_LENGTH (s), 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_symbol, "string->symbol", 1, 0, 0, 
	    (SCM string),
	    "Return the symbol whose name is @var{string}. This procedure\n"
	    "can create symbols with names containing special characters or\n"
	    "letters in the non-standard case, but it is usually a bad idea\n"
	    "to create such symbols because in some implementations of\n"
	    "Scheme they cannot be read as themselves.  See\n"
	    "@code{symbol->string}.\n"
	    "\n"
	    "The following examples assume that the implementation's\n"
	    "standard case is lower case:\n"
	    "\n"
	    "@lisp\n"
	    "(eq? 'mISSISSIppi 'mississippi) @result{} #t\n"
	    "(string->symbol \"mISSISSIppi\") @result{} @r{the symbol with name \"mISSISSIppi\"}\n"
	    "(eq? 'bitBlt (string->symbol \"bitBlt\")) @result{} #f\n"
	    "(eq? 'JollyWog\n"
	    "  (string->symbol (symbol->string 'JollyWog))) @result{} #t\n"
	    "(string=? \"K. Harper, M.D.\"\n"
	    "  (symbol->string\n"
	    "    (string->symbol \"K. Harper, M.D.\"))) @result{}#t\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_to_symbol
{
  SCM_VALIDATE_STRING (1, string);
  return scm_mem2symbol (SCM_STRING_CHARS (string),
			 SCM_STRING_LENGTH (string));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_obarray_symbol, "string->obarray-symbol", 2, 1, 0,
           (SCM o, SCM s, SCM softp),
	    "Intern a new symbol in @var{obarray}, a symbol table, with name\n"
	    "@var{string}.\n\n"
	    "If @var{obarray} is @code{#f}, use the default system symbol table.  If\n"
	    "@var{obarray} is @code{#t}, the symbol should not be interned in any\n"
	    "symbol table; merely return the pair (@var{symbol}\n"
	    ". @var{#<undefined>}).\n\n"
	    "The @var{soft?} argument determines whether new symbol table entries\n"
	    "should be created when the specified symbol is not already present in\n"
	    "@var{obarray}.  If @var{soft?} is specified and is a true value, then\n"
	    "new entries should not be added for symbols not already present in the\n"
	    "table; instead, simply return @code{#f}.")
#define FUNC_NAME s_scm_string_to_obarray_symbol
{
  SCM vcell;
  SCM answer;
  int softness;

  SCM_VALIDATE_STRING (2, s);
  SCM_ASSERT (SCM_BOOLP (o) || SCM_VECTORP (o), o, SCM_ARG1, FUNC_NAME);

  softness = (!SCM_UNBNDP (softp) && !SCM_FALSEP(softp));
  /* iron out some screwy calling conventions */
  if (SCM_FALSEP (o))
    o = scm_symhash;
  else if (SCM_EQ_P (o, SCM_BOOL_T))
    o = SCM_BOOL_F;
    
  vcell = scm_intern_obarray_soft (SCM_STRING_CHARS(s),
				   SCM_STRING_LENGTH (s),
				   o,
				   softness);
  if (SCM_FALSEP (vcell))
    return vcell;
  answer = SCM_CAR (vcell);
  return answer;
}
#undef FUNC_NAME

SCM_DEFINE (scm_intern_symbol, "intern-symbol", 2, 0, 0,
           (SCM o, SCM s),
	    "Add a new symbol to @var{obarray} with name @var{string}, bound to an\n"
	    "unspecified initial value.  The symbol table is not modified if a symbol\n"
	    "with this name is already present.")
#define FUNC_NAME s_scm_intern_symbol
{
  scm_sizet hval;
  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    o = scm_symhash;
  SCM_VALIDATE_VECTOR (1,o);
  hval = SCM_SYMBOL_HASH (s) % SCM_VECTOR_LENGTH (o);
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
	if (SCM_EQ_P (SCM_CAR (sym), s))
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
#undef FUNC_NAME

SCM_DEFINE (scm_unintern_symbol, "unintern-symbol", 2, 0, 0,
           (SCM o, SCM s),
	    "Remove the symbol with name @var{string} from @var{obarray}.  This\n"
	    "function returns @code{#t} if the symbol was present and @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_unintern_symbol
{
  scm_sizet hval;
  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    o = scm_symhash;
  SCM_VALIDATE_VECTOR (1,o);
  hval = SCM_SYMBOL_HASH (s) % SCM_VECTOR_LENGTH (o);
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
	if (SCM_EQ_P (SCM_CAR (sym), s))
	  {
	    /* Found the symbol to unintern. */
	    if (SCM_FALSEP (lsym_follow))
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
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_binding, "symbol-binding", 2, 0, 0,
           (SCM o, SCM s),
	    "Look up in @var{obarray} the symbol whose name is @var{string}, and\n"
	    "return the value to which it is bound.  If @var{obarray} is @code{#f},\n"
	    "use the global symbol table.  If @var{string} is not interned in\n"
	    "@var{obarray}, an error is signalled.")
#define FUNC_NAME s_scm_symbol_binding
{
  SCM vcell;
  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    o = scm_symhash;
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell (s, o);
  return SCM_CDR(vcell);
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_interned_p, "symbol-interned?", 2, 0, 0,
	    (SCM o, SCM s),
	    "Return @code{#t} if @var{obarray} contains a symbol with name\n"
	    "@var{string}, and @code{#f} otherwise.")
#define FUNC_NAME s_scm_symbol_interned_p
{
  SCM vcell;
  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    o = scm_symhash;
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell_soft (s, o);
  return (SCM_NIMP(vcell)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_bound_p, "symbol-bound?", 2, 0, 0,
	    (SCM o, SCM s),
	    "Return @code{#t} if @var{obarray} contains a symbol with name\n"
	    "@var{string} bound to a defined value.  This differs from\n"
	    "@var{symbol-interned?} in that the mere mention of a symbol\n"
	    "usually causes it to be interned; @code{symbol-bound?}\n"
	    "determines whether a symbol has been given any meaningful\n"
	    "value.")
#define FUNC_NAME s_scm_symbol_bound_p
{
  SCM vcell;
  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    o = scm_symhash;
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell_soft (s, o);
  return SCM_BOOL (SCM_NIMP (vcell) && !SCM_UNBNDP (SCM_CDR (vcell)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_set_x, "symbol-set!", 3, 0, 0,
           (SCM o, SCM s, SCM v),
	    "Find the symbol in @var{obarray} whose name is @var{string}, and rebind\n"
	    "it to @var{value}.  An error is signalled if @var{string} is not present\n"
	    "in @var{obarray}.")
#define FUNC_NAME s_scm_symbol_set_x
{
  SCM vcell;
  SCM_VALIDATE_SYMBOL (2,s);
  if (SCM_FALSEP (o))
    o = scm_symhash;
  SCM_VALIDATE_VECTOR (1,o);
  vcell = scm_sym2ovcell (s, o);
  SCM_SETCDR (vcell, v);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_fref, "symbol-fref", 1, 0, 0, 
           (SCM s),
	    "Return the contents of @var{symbol}'s @dfn{function slot}.")
#define FUNC_NAME s_scm_symbol_fref
{
  SCM_VALIDATE_SYMBOL (1,s);
  return SCM_SYMBOL_FUNC (s);
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_pref, "symbol-pref", 1, 0, 0, 
           (SCM s),
	    "Return the @dfn{property list} currently associated with @var{symbol}.")
#define FUNC_NAME s_scm_symbol_pref
{
  SCM_VALIDATE_SYMBOL (1,s);
  return SCM_SYMBOL_PROPS (s);
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_fset_x, "symbol-fset!", 2, 0, 0, 
           (SCM s, SCM val),
	    "Change the binding of @var{symbol}'s function slot.")
#define FUNC_NAME s_scm_symbol_fset_x
{
  SCM_VALIDATE_SYMBOL (1,s);
  SCM_SET_SYMBOL_FUNC (s, val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_pset_x, "symbol-pset!", 2, 0, 0,
           (SCM s, SCM val),
	    "Change the binding of @var{symbol}'s property slot.")
#define FUNC_NAME s_scm_symbol_pset_x
{
  SCM_VALIDATE_SYMBOL (1,s);
  SCM_DEFER_INTS;
  SCM_SET_SYMBOL_PROPS (s, val);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_hash, "symbol-hash", 1, 0, 0, 
	    (SCM symbol),
	    "Return a hash value for @var{symbol}.")
#define FUNC_NAME s_scm_symbol_hash
{
  SCM_VALIDATE_SYMBOL (1, symbol);
  return SCM_MAKINUM (SCM_SYMBOL_HASH (symbol));
}
#undef FUNC_NAME


static void
copy_and_prune_obarray (SCM from, SCM to)
{
  int i;
  int length = SCM_VECTOR_LENGTH (from);
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


SCM_DEFINE (scm_builtin_bindings, "builtin-bindings", 0, 0, 0, 
            (),
	    "Create and return a copy of the global symbol table, removing all\n"
	    "unbound symbols.")
#define FUNC_NAME s_scm_builtin_bindings
{
  int length = SCM_VECTOR_LENGTH (scm_symhash);
  SCM obarray = scm_c_make_hash_table (length);
  copy_and_prune_obarray (scm_symhash, obarray);
  return obarray;
}
#undef FUNC_NAME


#define MAX_PREFIX_LENGTH 30

static int gensym_counter;

SCM_DEFINE (scm_gensym, "gensym", 0, 1, 0,
            (SCM prefix),
	    "Create a new symbol with a name constructed from a prefix and\n"
	    "a counter value. The string @var{prefix} can be specified as\n"
	    "an optional argument. Default prefix is @code{g}.  The counter\n"
	    "is increased by 1 at each call. There is no provision for\n"
	    "resetting the counter.")
#define FUNC_NAME s_scm_gensym
{
  char buf[MAX_PREFIX_LENGTH + SCM_INTBUFLEN];
  char *name = buf;
  int len;
  if (SCM_UNBNDP (prefix))
    {
      name[0] = 'g';
      len = 1;
    }
  else
    {
      SCM_VALIDATE_STRING (1, prefix);
      len = SCM_STRING_LENGTH (prefix);
      if (len > MAX_PREFIX_LENGTH)
	name = SCM_MUST_MALLOC (MAX_PREFIX_LENGTH + SCM_INTBUFLEN);
      strncpy (name, SCM_STRING_CHARS (prefix), len);
    }
  {
    int n_digits = scm_iint2str (gensym_counter++, 10, &name[len]);
    SCM res = scm_mem2symbol (name, len + n_digits);
    if (name != buf)
      scm_must_free (name);
    return res;
  }
}
#undef FUNC_NAME

static int gentemp_counter;

SCM_DEFINE (scm_gentemp, "gentemp", 0, 2, 0,
            (SCM prefix, SCM obarray),
	    "Create a new symbol with a name unique in an obarray.\n"
	    "The name is constructed from an optional string @var{prefix}\n"
	    "and a counter value.  The default prefix is @code{t}.  The\n"
	    "@var{obarray} is specified as a second optional argument.\n"
	    "Default is the system obarray where all normal symbols are\n"
	    "interned.  The counter is increased by 1 at each\n"
	    "call.  There is no provision for resetting the counter.")
#define FUNC_NAME s_scm_gentemp
{
  char buf[MAX_PREFIX_LENGTH + SCM_INTBUFLEN];
  char *name = buf;
  int len, n_digits;
  if (SCM_UNBNDP (prefix))
    {
      name[0] = 't';
      len = 1;
    }
  else
    {
      SCM_VALIDATE_STRING (1, prefix);
      len = SCM_STRING_LENGTH (prefix);
      if (len > MAX_PREFIX_LENGTH)
	name = SCM_MUST_MALLOC (MAX_PREFIX_LENGTH + SCM_INTBUFLEN);
      strncpy (name, SCM_STRING_CHARS (prefix), len);
    }

  if (SCM_UNBNDP (obarray))
    obarray = scm_symhash;
  else
    SCM_ASSERT ((SCM_VECTORP (obarray) || SCM_WVECTP (obarray)),
		obarray,
		SCM_ARG2,
		FUNC_NAME);
  do
    n_digits = scm_iint2str (gentemp_counter++, 10, &name[len]);
  while (!SCM_FALSEP (scm_intern_obarray_soft (name,
					       len + n_digits,
					       obarray,
					       1)));
  {
    SCM vcell = scm_intern_obarray_soft (name,
					 len + n_digits,
					 obarray,
					 0);
    if (name != buf)
      scm_must_free (name);
    return SCM_CAR (vcell);
  }
}
#undef FUNC_NAME


void
scm_symbols_prehistory ()
{
  symbols = scm_make_weak_key_hash_table (SCM_MAKINUM (1009));
  scm_permanent_object (symbols);
}


void
scm_init_symbols ()
{
  gensym_counter = 0;
  gentemp_counter = 0;
#ifndef SCM_MAGIC_SNARFER
#include "libguile/symbols.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
