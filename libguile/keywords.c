/* Copyright (C) 1995,1996,1997,1998, 1999 Free Software Foundation, Inc.
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
#include "genio.h"
#include "smob.h"

#include "keywords.h"



static scm_sizet free_keyword SCM_P ((SCM obj));

static scm_sizet
free_keyword (obj)
     SCM obj;
{
  return 0;
}


static int prin_keyword SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int
prin_keyword (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_puts ("#:", port);
  scm_puts(1 + SCM_CHARS (SCM_CDR (exp)), port);
  return 1;
}

int scm_tc16_keyword;

/* This global is only kept for backward compatibility.
   Will be removed in next release.  */
int scm_tc16_kw;

static scm_smobfuns keyword_smob =
{scm_markcdr, free_keyword, prin_keyword, 0};



SCM_PROC (s_make_keyword_from_dash_symbol, "make-keyword-from-dash-symbol", 1, 0, 0, scm_make_keyword_from_dash_symbol);

SCM
scm_make_keyword_from_dash_symbol (symbol)
     SCM symbol;
{
  SCM vcell;

  SCM_ASSERT (SCM_NIMP (symbol)
	      && SCM_SYMBOLP (symbol)
	      && ('-' == SCM_CHARS(symbol)[0]),
	      symbol, SCM_ARG1, s_make_keyword_from_dash_symbol);

  SCM_DEFER_INTS;
  vcell = scm_sym2ovcell_soft (symbol, scm_keyword_obarray);
  if (vcell == SCM_BOOL_F)
    {
      SCM keyword;
      SCM_NEWCELL(keyword);
      SCM_SETCAR (keyword, (SCM)scm_tc16_keyword);
      SCM_SETCDR (keyword, symbol);
      scm_intern_symbol (scm_keyword_obarray, symbol);
      vcell = scm_sym2ovcell_soft (symbol, scm_keyword_obarray);
      SCM_SETCDR (vcell, keyword);
    }
  SCM_ALLOW_INTS;
  return SCM_CDR (vcell);
}

SCM_PROC(s_keyword_p, "keyword?", 1, 0, 0, scm_keyword_p);

SCM
scm_keyword_p (obj)
     SCM obj;
{
  return ( (SCM_NIMP(obj) && SCM_KEYWORDP (obj))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}



SCM_PROC(s_keyword_dash_symbol, "keyword-dash-symbol", 1, 0, 0, scm_keyword_dash_symbol);

SCM
scm_keyword_dash_symbol (keyword)
     SCM keyword;
{
  SCM_ASSERT (SCM_NIMP (keyword) && SCM_KEYWORDP (keyword),
	      keyword, SCM_ARG1, s_keyword_dash_symbol);
  return SCM_CDR (keyword);
}





void
scm_init_keywords ()
{
  scm_tc16_keyword = scm_newsmob (&keyword_smob);
  scm_tc16_kw = scm_tc16_keyword;
  scm_keyword_obarray = scm_make_vector (SCM_MAKINUM (256), SCM_EOL);
#include "keywords.x"
}

