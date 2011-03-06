/* Copyright (C) 1995, 1996, 1997, 1998, 2000, 2001, 2003, 2004,
 *   2006, 2009, 2011 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

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
#include "libguile/read.h"
#include "libguile/srfi-13.h"

#include "libguile/validate.h"
#include "libguile/symbols.h"

#include "libguile/private-options.h"


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

unsigned long
scm_i_hash_symbol (SCM obj, unsigned long n, void *closure)
{
  return scm_i_symbol_hash (obj) % n;
}

struct string_lookup_data
{
  SCM string;
  unsigned long string_hash;
};

static int
string_lookup_predicate_fn (SCM sym, void *closure)
{
  struct string_lookup_data *data = closure;

  if (scm_i_symbol_hash (sym) == data->string_hash
      && scm_i_symbol_length (sym) == scm_i_string_length (data->string))
    {
      size_t n = scm_i_symbol_length (sym);
      while (n--)
        if (scm_i_symbol_ref (sym, n) != scm_i_string_ref (data->string, n))
          return 0;
      return 1;
    }
  else
    return 0;
}

static SCM
lookup_interned_symbol (SCM name, unsigned long raw_hash)
{
  struct string_lookup_data data;
  SCM handle;

  data.string = name;
  data.string_hash = raw_hash;
  
  /* Strictly speaking, we should take a lock here.  But instead we rely
     on the fact that if this fails, we do take the lock on the
     intern_symbol path; and since nothing deletes from the hash table
     except GC, we should be OK.  */
  handle = scm_hash_fn_get_handle_by_hash (symbols, raw_hash,
                                           string_lookup_predicate_fn,
                                           &data);  

  if (scm_is_true (handle))
    return SCM_CAR (handle);
  else
    return SCM_BOOL_F;
}

struct latin1_lookup_data
{
  const char *str;
  size_t len;
  unsigned long string_hash;
};

static int
latin1_lookup_predicate_fn (SCM sym, void *closure)
{
  struct latin1_lookup_data *data = closure;

  return scm_i_symbol_hash (sym) == data->string_hash
    && scm_i_is_narrow_symbol (sym)
    && scm_i_symbol_length (sym) == data->len
    && strncmp (scm_i_symbol_chars (sym), data->str, data->len) == 0;
}

static SCM
lookup_interned_latin1_symbol (const char *str, size_t len,
                               unsigned long raw_hash)
{
  struct latin1_lookup_data data;
  SCM handle;

  data.str = str;
  data.len = len;
  data.string_hash = raw_hash;
  
  /* Strictly speaking, we should take a lock here.  But instead we rely
     on the fact that if this fails, we do take the lock on the
     intern_symbol path; and since nothing deletes from the hash table
     except GC, we should be OK.  */
  handle = scm_hash_fn_get_handle_by_hash (symbols, raw_hash,
                                           latin1_lookup_predicate_fn,
                                           &data);  

  if (scm_is_true (handle))
    return SCM_CAR (handle);
  else
    return SCM_BOOL_F;
}

static unsigned long
symbol_lookup_hash_fn (SCM obj, unsigned long max, void *closure)
{
  return scm_i_symbol_hash (obj) % max;
}

static SCM
symbol_lookup_assoc_fn (SCM obj, SCM alist, void *closure)
{
  for (; !scm_is_null (alist); alist = SCM_CDR (alist))
    {
      SCM sym = SCM_CAAR (alist);

      if (scm_i_symbol_hash (sym) == scm_i_symbol_hash (obj)
          && scm_is_true (scm_string_equal_p (scm_symbol_to_string (sym),
                                              scm_symbol_to_string (obj))))
        return SCM_CAR (alist);
    }

  return SCM_BOOL_F;
}

static scm_i_pthread_mutex_t intern_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Intern SYMBOL, an uninterned symbol.  Might return a different
   symbol, if another one was interned at the same time.  */
static SCM
intern_symbol (SCM symbol)
{
  SCM handle;

  scm_i_pthread_mutex_lock (&intern_lock);
  handle = scm_hash_fn_create_handle_x (symbols, symbol, SCM_UNDEFINED,
                                        symbol_lookup_hash_fn,
                                        symbol_lookup_assoc_fn,
                                        NULL);
  scm_i_pthread_mutex_unlock (&intern_lock);

  return SCM_CAR (handle);
}

static SCM
scm_i_str2symbol (SCM str)
{
  SCM symbol;
  size_t raw_hash = scm_i_string_hash (str);

  symbol = lookup_interned_symbol (str, raw_hash);
  if (scm_is_true (symbol))
    return symbol;
  else
    {
      /* The symbol was not found, create it.  */
      symbol = scm_i_make_symbol (str, 0, raw_hash,
				  scm_cons (SCM_BOOL_F, SCM_EOL));
      return intern_symbol (symbol);
    }
}


static SCM
scm_i_str2uninterned_symbol (SCM str)
{
  size_t raw_hash = scm_i_string_hash (str);

  return scm_i_make_symbol (str, SCM_I_F_SYMBOL_UNINTERNED, 
			    raw_hash, scm_cons (SCM_BOOL_F, SCM_EOL));
}

SCM_DEFINE (scm_symbol_p, "symbol?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a symbol, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_symbol_p
{
  return scm_from_bool (scm_is_symbol (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_interned_p, "symbol-interned?", 1, 0, 0, 
	    (SCM symbol),
	    "Return @code{#t} if @var{symbol} is interned, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_symbol_interned_p
{
  SCM_VALIDATE_SYMBOL (1, symbol);
  return scm_from_bool (scm_i_symbol_is_interned (symbol));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_symbol, "make-symbol", 1, 0, 0,
	    (SCM name),
	    "Return a new uninterned symbol with the name @var{name}.  " 
	    "The returned symbol is guaranteed to be unique and future "
	    "calls to @code{string->symbol} will not return it.")
#define FUNC_NAME s_scm_make_symbol
{
  SCM_VALIDATE_STRING (1, name);
  return scm_i_str2uninterned_symbol (name);
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
  return scm_i_symbol_substring (s, 0, scm_i_symbol_length (s));
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
  return scm_i_str2symbol (string);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ci_to_symbol, "string-ci->symbol", 1, 0, 0,
	    (SCM str),
	    "Return the symbol whose name is @var{str}.  @var{str} is\n"
	    "converted to lowercase before the conversion is done, if Guile\n"
	    "is currently reading symbols case-insensitively.")
#define FUNC_NAME s_scm_string_ci_to_symbol
{
  return scm_string_to_symbol (SCM_CASE_INSENSITIVE_P
			       ? scm_string_downcase(str)
			       : str);
}
#undef FUNC_NAME

/* The default prefix for `gensym'd symbols.  */
static SCM default_gensym_prefix;

#define MAX_PREFIX_LENGTH 30

SCM_DEFINE (scm_gensym, "gensym", 0, 1, 0,
            (SCM prefix),
	    "Create a new symbol with a name constructed from a prefix and\n"
	    "a counter value. The string @var{prefix} can be specified as\n"
	    "an optional argument. Default prefix is @code{ g}.  The counter\n"
	    "is increased by 1 at each call. There is no provision for\n"
	    "resetting the counter.")
#define FUNC_NAME s_scm_gensym
{
  static int gensym_counter = 0;
  
  SCM suffix, name;
  int n, n_digits;
  char buf[SCM_INTBUFLEN];

  if (SCM_UNBNDP (prefix))
    prefix = default_gensym_prefix;

  /* mutex in case another thread looks and incs at the exact same moment */
  scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
  n = gensym_counter++;
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  n_digits = scm_iint2str (n, 10, buf);
  suffix = scm_from_latin1_stringn (buf, n_digits);
  name = scm_string_append (scm_list_2 (prefix, suffix));
  return scm_string_to_symbol (name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_hash, "symbol-hash", 1, 0, 0, 
	    (SCM symbol),
	    "Return a hash value for @var{symbol}.")
#define FUNC_NAME s_scm_symbol_hash
{
  SCM_VALIDATE_SYMBOL (1, symbol);
  return scm_from_ulong (scm_i_symbol_hash (symbol));
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_fref, "symbol-fref", 1, 0, 0, 
           (SCM s),
	    "Return the contents of @var{symbol}'s @dfn{function slot}.")
#define FUNC_NAME s_scm_symbol_fref
{
  SCM_VALIDATE_SYMBOL (1, s);
  return SCM_CAR (SCM_CELL_OBJECT_3 (s));
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_pref, "symbol-pref", 1, 0, 0, 
           (SCM s),
	    "Return the @dfn{property list} currently associated with @var{symbol}.")
#define FUNC_NAME s_scm_symbol_pref
{
  SCM_VALIDATE_SYMBOL (1, s);
  return SCM_CDR (SCM_CELL_OBJECT_3 (s));
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_fset_x, "symbol-fset!", 2, 0, 0, 
           (SCM s, SCM val),
	    "Change the binding of @var{symbol}'s function slot.")
#define FUNC_NAME s_scm_symbol_fset_x
{
  SCM_VALIDATE_SYMBOL (1, s);
  SCM_SETCAR (SCM_CELL_OBJECT_3 (s), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_pset_x, "symbol-pset!", 2, 0, 0,
           (SCM s, SCM val),
	    "Change the binding of @var{symbol}'s property slot.")
#define FUNC_NAME s_scm_symbol_pset_x
{
  SCM_VALIDATE_SYMBOL (1, s);
  SCM_SETCDR (SCM_CELL_OBJECT_3 (s), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_from_locale_symbol (const char *sym)
{
  return scm_from_locale_symboln (sym, -1);
}

SCM
scm_from_locale_symboln (const char *sym, size_t len)
{
  SCM str = scm_from_locale_stringn (sym, len);
  return scm_i_str2symbol (str);
}

SCM
scm_take_locale_symboln (char *sym, size_t len)
{
  SCM str;

  str = scm_take_locale_stringn (sym, len);
  return scm_i_str2symbol (str);
}

SCM
scm_take_locale_symbol (char *sym)
{
  return scm_take_locale_symboln (sym, (size_t)-1);
}

SCM
scm_from_latin1_symbol (const char *sym)
{
  return scm_from_latin1_symboln (sym, -1);
}

SCM
scm_from_latin1_symboln (const char *sym, size_t len)
{
  unsigned long hash;
  SCM ret;

  if (len == (size_t) -1)
    len = strlen (sym);
  hash = scm_i_latin1_string_hash (sym, len);

  ret = lookup_interned_latin1_symbol (sym, len, hash);
  if (scm_is_false (ret))
    {
      SCM str = scm_from_latin1_stringn (sym, len);
      ret = scm_i_str2symbol (str);
    }

  return ret;
}

SCM
scm_from_utf8_symbol (const char *sym)
{
  return scm_from_utf8_symboln (sym, -1);
}

SCM
scm_from_utf8_symboln (const char *sym, size_t len)
{
  SCM str = scm_from_utf8_stringn (sym, len);
  return scm_i_str2symbol (str);
}

void
scm_symbols_prehistory ()
{
  symbols = scm_make_weak_key_hash_table (scm_from_int (2139));
}


void
scm_init_symbols ()
{
#include "libguile/symbols.x"

  default_gensym_prefix = scm_from_latin1_string (" g");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
