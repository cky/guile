/* Copyright (C) 1995,1996,1997,1998, 1999, 2000 Free Software Foundation, Inc.
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
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/keywords.h"


scm_bits_t scm_tc16_keyword;

static int
keyword_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#:", port);
  scm_puts(1 + SCM_SYMBOL_CHARS (SCM_CDR (exp)), port);
  return 1;
}

SCM_DEFINE (scm_make_keyword_from_dash_symbol, "make-keyword-from-dash-symbol", 1, 0, 0, 
            (SCM symbol),
            "Make a keyword object from a @var{symbol} that starts with a dash.")
#define FUNC_NAME s_scm_make_keyword_from_dash_symbol
{
  SCM vcell;

  SCM_ASSERT (SCM_SYMBOLP (symbol)
	      && ('-' == SCM_SYMBOL_CHARS(symbol)[0]),
	      symbol, SCM_ARG1, FUNC_NAME);

  SCM_DEFER_INTS;
  vcell = scm_sym2ovcell_soft (symbol, scm_keyword_obarray);
  if (SCM_FALSEP (vcell))
    {
      SCM keyword;
      SCM_NEWSMOB (keyword, scm_tc16_keyword, SCM_UNPACK (symbol));
      scm_intern_symbol (scm_keyword_obarray, symbol);
      vcell = scm_sym2ovcell_soft (symbol, scm_keyword_obarray);
      SCM_SETCDR (vcell, keyword);
    }
  SCM_ALLOW_INTS;
  return SCM_CDR (vcell);
}
#undef FUNC_NAME

SCM
scm_c_make_keyword (char *s)
{
  char *buf = scm_must_malloc (strlen (s) + 2, "keyword");
  SCM symbol;

  buf[0] = '-';
  strcpy (buf + 1, s);
  symbol = scm_str2symbol (buf);
  scm_must_free (buf);
  scm_done_free (strlen (s) + 2);

  return scm_make_keyword_from_dash_symbol (symbol);
}

SCM_DEFINE (scm_keyword_p, "keyword?", 1, 0, 0, 
            (SCM obj),
	    "Returns @code{#t} if the argument @var{obj} is a keyword, else @code{#f}.")
#define FUNC_NAME s_scm_keyword_p
{
  return SCM_BOOL(SCM_KEYWORDP (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_keyword_dash_symbol, "keyword-dash-symbol", 1, 0, 0, 
            (SCM keyword),
	    "Return the dash symbol for @var{keyword}.\n"
	    "This is the inverse of @code{make-keyword-from-dash-symbol}.")
#define FUNC_NAME s_scm_keyword_dash_symbol
{
  SCM_VALIDATE_KEYWORD (1,keyword);
  return SCM_CDR (keyword);
}
#undef FUNC_NAME



void
scm_init_keywords ()
{
  scm_tc16_keyword = scm_make_smob_type ("keyword", 0);
  scm_set_smob_mark (scm_tc16_keyword, scm_markcdr);
  scm_set_smob_print (scm_tc16_keyword, keyword_print);

  scm_keyword_obarray = scm_c_make_hash_table (256);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/keywords.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
