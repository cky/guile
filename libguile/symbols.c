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



/* {Symbols}
 */


SCM
scm_mem2symbol (const char *name, size_t len)
{
  size_t raw_hash = scm_string_hash ((const unsigned char *) name, len);
  size_t hash = raw_hash % SCM_VECTOR_LENGTH (symbols);

  {
    /* Try to find the symbol in the symbols table */

    SCM l;

    for (l = SCM_VELTS (symbols) [hash]; !SCM_NULLP (l); l = SCM_CDR (l))
      {
	SCM sym = SCM_CAAR (l);
	if (SCM_SYMBOL_HASH (sym) == raw_hash
	    && SCM_SYMBOL_LENGTH (sym) == len)
	  {
	    char *chrs = SCM_SYMBOL_CHARS (sym);
	    size_t i = len;

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
    SCM_SET_SYMBOL_CHARS (symbol, scm_must_strndup (name, len));
    SCM_SET_SYMBOL_HASH (symbol, raw_hash);
    SCM_SET_PROP_SLOTS (symbol, scm_cons (SCM_BOOL_F, SCM_EOL));
    SCM_SET_SYMBOL_LENGTH (symbol, (long) len);

    slot = SCM_VELTS (symbols) [hash];
    cell = scm_cons (symbol, SCM_UNDEFINED);
    SCM_VELTS (symbols) [hash] = scm_cons (cell, slot);

    return symbol;
  }
}


SCM
scm_str2symbol (const char *str)
{
  return scm_mem2symbol (str, strlen (str));
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
	    "(section @pxref{Literal expressions,,,r5rs, The Revised^5\n"
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
  size_t len;
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

SCM_DEFINE (scm_symbol_hash, "symbol-hash", 1, 0, 0, 
	    (SCM symbol),
	    "Return a hash value for @var{symbol}.")
#define FUNC_NAME s_scm_symbol_hash
{
  SCM_VALIDATE_SYMBOL (1, symbol);
  return SCM_MAKINUM (SCM_SYMBOL_HASH (symbol));
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
#ifndef SCM_MAGIC_SNARFER
#include "libguile/symbols.x"
#endif
#if SCM_ENABLE_VCELLS
  scm_init_symbols_deprecated ();
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
