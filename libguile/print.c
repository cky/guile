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
#include "genio.h"
#include "mbstrings.h"
#include "smob.h"
#include "eval.h"
#include "procprop.h"
#include "read.h"
#include "weaks.h"
#include "unif.h"

#include "print.h"


/* {Names of immediate symbols}
 * 
 * This table must agree with the declarations in scm.h: {Immediate Symbols}.
 */

char *scm_isymnames[] =
{
  /* This table must agree with the declarations */
  "#@and",
  "#@begin",
  "#@case",
  "#@cond",
  "#@do",
  "#@if",
  "#@lambda",
  "#@let",
  "#@let*",
  "#@letrec",
  "#@or",
  "#@quote",
  "#@set!",
  "#@define",
#if 0
  "#@literal-variable-ref",
  "#@literal-variable-set!",
#endif
  "#@apply",
  "#@call-with-current-continuation",

 /* user visible ISYMS */
 /* other keywords */
 /* Flags */

  "#f",
  "#t",
  "#<undefined>",
  "#<eof>",
  "()",
  "#<unspecified>"
};

#ifdef DEBUG_EXTENSIONS
scm_option scm_print_opts[] = {
  { SCM_OPTION_BOOLEAN, "procnames", 0,
    "Print names instead of closures." },
  { SCM_OPTION_SCM, "closure-hook", SCM_BOOL_F,
    "Procedure used to print closures." }
};

SCM_PROC (s_print_options, "print-options-interface", 0, 1, 0, scm_print_options);
#ifdef __STDC__
SCM
scm_print_options (SCM new_values)
#else
SCM
scm_print_options (new_values)
     SCM new_values;
#endif
{
  SCM ans = scm_options (new_values,
			 scm_print_opts,
			 SCM_N_PRINT_OPTIONS,
			 s_print_options);
  return ans;
}
#endif


/* {Printing of Scheme Objects}
 */

/* Print generally.  Handles both write and display according to WRITING.
 */
#ifdef __STDC__
void 
scm_iprin1 (SCM exp, SCM port, int writing)
#else
void 
scm_iprin1 (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  register long i;
taloop:
  switch (7 & (int) exp)
    {
    case 2:
    case 6:
      scm_intprint (SCM_INUM (exp), 10, port);
      break;
    case 4:
      if (SCM_ICHRP (exp))
	{
	  i = SCM_ICHR (exp);
	  scm_put_wchar (i, port, writing);

	}
      else if (   SCM_IFLAGP (exp)
	       && (SCM_ISYMNUM (exp) < (sizeof scm_isymnames / sizeof (char *))))
	  scm_gen_puts (scm_regular_string, SCM_ISYMSCM_CHARS (exp), port);
      else if (SCM_ILOCP (exp))
	{
	  scm_gen_puts (scm_regular_string, "#@", port);
	  scm_intprint ((long) SCM_IFRAME (exp), 10, port);
	  scm_gen_putc (SCM_ICDRP (exp) ? '-' : '+', port);
	  scm_intprint ((long) SCM_IDIST (exp), 10, port);
	}
      else
	goto idef;
      break;
    case 1:
      /* gloc */
      scm_gen_puts (scm_regular_string, "#@", port);
      exp = SCM_CAR (exp - 1);
      goto taloop;
    default:
    idef:
      scm_ipruk ("immediate", exp, port);
      break;
    case 0:
      switch (SCM_TYP7 (exp))
	{
	case scm_tcs_cons_gloc:

	  if (SCM_CDR (SCM_CAR (exp) - 1L) == 0)
	    {
	      scm_gen_write (scm_regular_string, "#<struct ", (scm_sizet) 9, port);
	      scm_intprint(exp, 16, port);
	      scm_gen_putc ('>', port);
	      break;
	    }

	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  scm_iprlist ("(", exp, ')', port, writing);
	  break;
	case scm_tcs_closures:
#ifdef DEBUG_EXTENSIONS
	  if (SCM_PRINT_PROCNAMES_P)
	    {
	      SCM name;
	      name = scm_procedure_property (exp, scm_i_name);
	      scm_gen_puts (scm_regular_string, "#<procedure", port);
	      if (SCM_NFALSEP (name))
		{
		  scm_gen_putc (' ', port);
		  /* FIXME */
		  scm_gen_puts (scm_regular_string, SCM_CHARS (name), port);
		}
	      scm_gen_putc ('>', port);
	    }
	  else
#endif
	    {
	      exp = SCM_CODE (exp);
	      scm_iprlist ("#<CLOSURE ", exp, '>', port, writing);
	    }
	  break;
	case scm_tc7_mb_string:
	case scm_tc7_mb_substring:
	  scm_print_mb_string (exp, port, writing);
	  break;
	case scm_tc7_substring:
	case scm_tc7_string:
	  if (writing)
	    {
	      scm_gen_putc ('\"', port);
	      for (i = 0; i < SCM_ROLENGTH (exp); ++i)
		switch (SCM_ROCHARS (exp)[i])
		  {
		  case '\"':
		  case '\\':
		    scm_gen_putc ('\\', port);
		  default:
		    scm_gen_putc (SCM_ROCHARS (exp)[i], port);
		  }
	      scm_gen_putc ('\"', port);
	      break;
	    }
	  else
	    scm_gen_write (scm_regular_string, SCM_ROCHARS (exp),
			   (scm_sizet) SCM_ROLENGTH (exp),
			   port);
	  break;
	case scm_tcs_symbols:
	  if (SCM_MB_STRINGP (exp))
	    {
	      scm_print_mb_symbol (exp, port);
	      break;
	    }
	  else
	    {
	      int pos;
	      int end;
	      int len;
	      char * str;
	      int weird;
	      int maybe_weird;
	      int mw_pos;

	      len = SCM_LENGTH (exp);
	      str = SCM_CHARS (exp);
	      scm_remember (&exp);
	      pos = 0;
	      weird = 0;
	      maybe_weird = 0;

	      if (len == 0)
		scm_gen_write (scm_regular_string, "#{}#", 4, port);

	      for (end = pos; end < len; ++end)
		switch (str[end])
		  {
#ifdef BRACKETS_AS_PARENS
		  case '[':
		  case ']':
#endif
		  case '(':
		  case ')':
		  case '\"':
		  case ';':
		  case SCM_WHITE_SPACES:
		  case SCM_LINE_INCREMENTORS:
		  weird_handler:
		    if (maybe_weird)
		      {
			end = mw_pos;
			maybe_weird = 0;
		      }
		    if (!weird)
		      {
			scm_gen_write (scm_regular_string, "#{", 2, port);
			weird = 1;
		      }
		    if (pos < end)
		      {
			scm_gen_write (scm_regular_string, str + pos, end - pos, port);
		      }
		    {
		      char buf[2];
		      buf[0] = '\\';
		      buf[1] = str[end];
		      scm_gen_write (scm_regular_string, buf, 2, port);
		    }
		    pos = end + 1;
		    break;
		  case '\\':
		    if (weird)
		      goto weird_handler;
		    if (!maybe_weird)
		      {
			maybe_weird = 1;
			mw_pos = pos;
		      }
		    break;
		  case '}':
		  case '#':
		    if (weird)
		      goto weird_handler;
		    break;
		  default:
		    break;
		  }
	      if (pos < end)
		scm_gen_write (scm_regular_string, str + pos, end - pos, port);
	      if (weird)
		scm_gen_write (scm_regular_string, "}#", 2, port);
	      break;
	    }
	case scm_tc7_wvect:
	  if (SCM_IS_WHVEC (exp))
	    scm_gen_puts (scm_regular_string, "#wh(", port);
	  else
	    scm_gen_puts (scm_regular_string, "#w(", port);
	  goto common_vector_printer;

	case scm_tc7_vector:
	  scm_gen_puts (scm_regular_string, "#(", port);
	common_vector_printer:
	  for (i = 0; i + 1 < SCM_LENGTH (exp); ++i)
	    {
	      /* CHECK_INTS; */
	      scm_iprin1 (SCM_VELTS (exp)[i], port, writing);
	      scm_gen_putc (' ', port);
	    }
	  if (i < SCM_LENGTH (exp))
	    {
	      /* CHECK_INTS; */
	      scm_iprin1 (SCM_VELTS (exp)[i], port, writing);
	    }
	  scm_gen_putc (')', port);
	  break;
	case scm_tc7_bvect:
	case scm_tc7_byvect:
	case scm_tc7_svect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
#ifdef LONGLONGS
	case scm_tc7_llvect:
#endif
	  scm_raprin1 (exp, port, writing);
	  break;
	case scm_tcs_subrs:
	  scm_gen_puts (scm_regular_string, "#<primitive-procedure ", port);
	  scm_gen_puts ((SCM_MB_STRINGP (SCM_SNAME(exp))
			 ? scm_mb_string
			 : scm_regular_string),
			SCM_CHARS (SCM_SNAME (exp)), port);
	  scm_gen_putc ('>', port);
	  break;
#ifdef CCLO
	case scm_tc7_cclo:
	  scm_gen_puts (scm_regular_string, "#<compiled-closure ", port);
	  scm_iprin1 (SCM_CCLO_SUBR (exp), port, writing);
	  scm_gen_putc ('>', port);
	  break;
#endif
	case scm_tc7_contin:
	  scm_gen_puts (scm_regular_string, "#<continuation ", port);
	  scm_intprint (SCM_LENGTH (exp), 10, port);
	  scm_gen_puts (scm_regular_string, " @ ", port);
	  scm_intprint ((long) SCM_CHARS (exp), 16, port);
	  scm_gen_putc ('>', port);
	  break;
	case scm_tc7_port:
	  i = SCM_PTOBNUM (exp);
	  if (i < scm_numptob && scm_ptobs[i].print && (scm_ptobs[i].print) (exp, port, writing))
	    break;
	  goto punk;
	case scm_tc7_smob:
	  i = SCM_SMOBNUM (exp);
	  if (i < scm_numsmob && scm_smobs[i].print
	      && (scm_smobs[i].print) (exp, port, writing))
	    break;
	  goto punk;
	default:
	punk:scm_ipruk ("type", exp, port);
	}
    }
}


/* Print an integer.
 */
#ifdef __STDC__
void 
scm_intprint (long n, int radix, SCM port)
#else
void 
scm_intprint (n, radix, port)
     long n;
     int radix;
     SCM port;
#endif
{
  char num_buf[SCM_INTBUFLEN];
  scm_gen_write (scm_regular_string, num_buf, scm_iint2str (n, radix, num_buf), port);
}

/* Print an object of unrecognized type.
 */
#ifdef __STDC__
void 
scm_ipruk (char *hdr, SCM ptr, SCM port)
#else
void 
scm_ipruk (hdr, ptr, port)
     char *hdr;
     SCM ptr;
     SCM port;
#endif
{
  scm_gen_puts (scm_regular_string, "#<unknown-", port);
  scm_gen_puts (scm_regular_string, hdr, port);
  if (SCM_CELLP (ptr))
    {
      scm_gen_puts (scm_regular_string, " (0x", port);
      scm_intprint (SCM_CAR (ptr), 16, port);
      scm_gen_puts (scm_regular_string, " . 0x", port);
      scm_intprint (SCM_CDR (ptr), 16, port);
      scm_gen_puts (scm_regular_string, ") @", port);
    }
  scm_gen_puts (scm_regular_string, " 0x", port);
  scm_intprint (ptr, 16, port);
  scm_gen_putc ('>', port);
}

/* Print a list.
 */
#ifdef __STDC__
void 
scm_iprlist (char *hdr, SCM exp, char tlr, SCM port, int writing)
#else
void 
scm_iprlist (hdr, exp, tlr, port, writing)
     char *hdr;
     SCM exp;
     char tlr;
     SCM port;
     int writing;
#endif
{
  scm_gen_puts (scm_regular_string, hdr, port);
  /* CHECK_INTS; */
  scm_iprin1 (SCM_CAR (exp), port, writing);
  exp = SCM_CDR (exp);
  for (; SCM_NIMP (exp); exp = SCM_CDR (exp))
    {
      if (SCM_NECONSP (exp))
	break;
      scm_gen_putc (' ', port);
      /* CHECK_INTS; */
      scm_iprin1 (SCM_CAR (exp), port, writing);
    }
  if (SCM_NNULLP (exp))
    {
      scm_gen_puts (scm_regular_string, " . ", port);
      scm_iprin1 (exp, port, writing);
    }
  scm_gen_putc (tlr, port);
}



SCM_PROC(s_write, "write", 1, 1, 0, scm_write);
#ifdef __STDC__
SCM 
scm_write (SCM obj, SCM port)
#else
SCM 
scm_write (obj, port)
     SCM obj;
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG2, s_write);
  scm_iprin1 (obj, port, 1);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_display, "display", 1, 1, 0, scm_display);
#ifdef __STDC__
SCM 
scm_display (SCM obj, SCM port)
#else
SCM 
scm_display (obj, port)
     SCM obj;
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG2, s_display);
  scm_iprin1 (obj, port, 0);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_newline, "newline", 0, 1, 0, scm_newline);
#ifdef __STDC__
SCM
scm_newline(SCM port)
#else
SCM 
scm_newline (port)
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
 port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_newline);
  scm_gen_putc ('\n', port);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
  else
# endif
#endif
  if (port == scm_cur_outp)
    scm_fflush (port);
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_write_char, "write-char", 1, 1, 0, scm_write_char);
#ifdef __STDC__
SCM 
scm_write_char (SCM chr, SCM port)
#else
SCM 
scm_write_char (chr, port)
     SCM chr;
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
 port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG2, s_write_char);
  SCM_ASSERT (SCM_ICHRP (chr), chr, SCM_ARG1, s_write_char);
  scm_gen_putc ((int) SCM_ICHR (chr), port);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}




#ifdef __STDC__
void
scm_init_print (void)
#else
void
scm_init_print ()
#endif
{
#ifdef DEBUG_EXTENSIONS
  scm_init_opts (scm_print_options, scm_print_opts, SCM_N_PRINT_OPTIONS);
#endif
#include "print.x"
}

