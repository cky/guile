/*	Copyright (C) 1995-1999, 2000 Free Software Foundation, Inc.
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
#include "libguile/continuations.h"
#include "libguile/smob.h"
#include "libguile/eval.h"
#include "libguile/macros.h"
#include "libguile/procprop.h"
#include "libguile/read.h"
#include "libguile/weaks.h"
#include "libguile/unif.h"
#include "libguile/alist.h"
#include "libguile/struct.h"
#include "libguile/objects.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/print.h"


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
  "#<unspecified>",
  "#@dispatch",
  "#@slot-ref",
  "#@slot-set!",

  /* Multi-language support */
  
  "#@nil-cond",
  "#@nil-ify",
  "#@t-ify",
  "#@0-cond",
  "#@0-ify",
  "#@1-ify",
  "#@bind",
  
  "#@delay",
  
  "#<unbound>"
};

scm_option scm_print_opts[] = {
  { SCM_OPTION_SCM, "closure-hook", SCM_UNPACK(SCM_BOOL_F),
    "Hook for printing closures." },
  { SCM_OPTION_BOOLEAN, "source", 0,
    "Print closures with source." }
};

SCM_DEFINE (scm_print_options, "print-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the print options. Instead of using\n"
	    "this procedure directly, use the procedures\n"
	    "@code{print-enable}, @code{print-disable}, @code{print-set!}\n"
	    "and @code{print-options}.")
#define FUNC_NAME s_scm_print_options
{
  SCM ans = scm_options (setting,
			 scm_print_opts,
			 SCM_N_PRINT_OPTIONS,
			 FUNC_NAME);
  return ans;
}
#undef FUNC_NAME


/* {Printing of Scheme Objects}
 */

/* Detection of circular references.
 *
 * Due to other constraints in the implementation, this code has bad
 * time complexity (O (depth * N)), The printer code can be
 * rewritten to be O(N).
 */
#define PUSH_REF(pstate, obj) \
do { \
  pstate->ref_stack[pstate->top++] = (obj); \
  if (pstate->top == pstate->ceiling) \
    grow_ref_stack (pstate); \
} while(0)

#define ENTER_NESTED_DATA(pstate, obj, label) \
do { \
  register unsigned long i; \
  for (i = 0; i < pstate->top; ++i) \
    if (SCM_EQ_P (pstate->ref_stack[i], (obj))) \
      goto label; \
  if (pstate->fancyp) \
    { \
      if (pstate->top - pstate->list_offset >= pstate->level) \
	{ \
	  scm_putc ('#', port); \
	  return; \
	} \
    } \
  PUSH_REF(pstate, obj); \
} while(0)

#define EXIT_NESTED_DATA(pstate) { --pstate->top; }

SCM scm_print_state_vtable;

static SCM print_state_pool;

#ifdef GUILE_DEBUG /* Used for debugging purposes */

SCM_DEFINE (scm_current_pstate, "current-pstate", 0, 0, 0, 
           (),
            "Return the current-pstate--the `cadr' of the print_state_pool.\n"
            "`current-pstate' is only included in GUILE_DEBUG builds.")
#define FUNC_NAME s_scm_current_pstate
{
  if (SCM_NNULLP (SCM_CDR (print_state_pool)))
    return SCM_CADR (print_state_pool);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

#endif

#define PSTATE_SIZE 50L

static SCM
make_print_state (void)
{
  SCM print_state = scm_make_struct (SCM_CAR (print_state_pool), /* pstate type */
				     SCM_INUM0,
				     SCM_EOL);
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  pstate->ref_vect = scm_c_make_vector (PSTATE_SIZE, SCM_UNDEFINED);
  pstate->ref_stack = SCM_VELTS (pstate->ref_vect);
  pstate->ceiling = SCM_VECTOR_LENGTH (pstate->ref_vect);
  return print_state;
}

SCM
scm_make_print_state ()
{
  SCM answer = SCM_BOOL_F;

  /* First try to allocate a print state from the pool */
  SCM_DEFER_INTS;
  if (SCM_NNULLP (SCM_CDR (print_state_pool)))
    {
      answer = SCM_CADR (print_state_pool);
      SCM_SETCDR (print_state_pool, SCM_CDDR (print_state_pool));
    }
  SCM_ALLOW_INTS;
  
  return SCM_FALSEP (answer) ? make_print_state () : answer;
}

void
scm_free_print_state (SCM print_state)
{
  SCM handle;
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  /* Cleanup before returning print state to pool.
   * It is better to do it here.  Doing it in scm_prin1
   * would cost more since that function is called much more
   * often.
   */
  pstate->fancyp = 0;
  pstate->revealed = 0;
  SCM_NEWCELL (handle);
  SCM_DEFER_INTS;
  SCM_SETCAR (handle, print_state);
  SCM_SETCDR (handle, SCM_CDR (print_state_pool));
  SCM_SETCDR (print_state_pool, handle);
  SCM_ALLOW_INTS;
}

static void
grow_ref_stack (scm_print_state *pstate)
{
  unsigned long int old_size = SCM_VECTOR_LENGTH (pstate->ref_vect);
  SCM *old_elts = SCM_VELTS (pstate->ref_vect);
  unsigned long int new_size = 2 * pstate->ceiling;
  SCM new_vect = scm_c_make_vector (new_size, SCM_UNDEFINED);
  SCM *new_elts = SCM_VELTS (new_vect);
  unsigned long int i;

  for (i = 0; i != old_size; ++i)
    new_elts [i] = old_elts [i];

  pstate->ref_vect = new_vect;
  pstate->ref_stack = new_elts;
  pstate->ceiling = new_size;
}


static void
print_circref (SCM port,scm_print_state *pstate,SCM ref)
{
  register int i;
  int self = pstate->top - 1;
  i = pstate->top - 1;
  if (SCM_CONSP (pstate->ref_stack[i]))
    {
      while (i > 0)
	{
	  if (SCM_NCONSP (pstate->ref_stack[i - 1])
	      || !SCM_EQ_P (SCM_CDR (pstate->ref_stack[i - 1]), 
			    pstate->ref_stack[i]))
	    break;
	  --i;
	}
      self = i;
    }
  for (i = pstate->top - 1; 1; --i)
    if (SCM_EQ_P (pstate->ref_stack[i], ref))
      break;
  scm_putc ('#', port);
  scm_intprint (i - self, 10, port);
  scm_putc ('#', port);
}

/* Print generally.  Handles both write and display according to PSTATE.
 */
SCM_GPROC(s_write, "write", 1, 1, 0, scm_write, g_write);
SCM_GPROC(s_display, "display", 1, 1, 0, scm_display, g_display);

void 
scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate)
{
taloop:
  switch (SCM_ITAG3 (exp))
    {
    case scm_tc3_closure:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
      /* These tc3 tags should never occur in an immediate value.  They are
       * only used in cell types of non-immediates, i. e. the value returned
       * by SCM_CELL_TYPE (exp) can use these tags.
       */
      scm_ipruk ("immediate", exp, port);
      break;
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      scm_intprint (SCM_INUM (exp), 10, port);
      break;
    case scm_tc3_imm24:
      if (SCM_CHARP (exp))
	{
	  long i = SCM_CHAR (exp);

	  if (SCM_WRITINGP (pstate))
	    {
	      scm_puts ("#\\", port);
	      if ((i >= 0) && (i <= ' ') && scm_charnames[i])
		scm_puts (scm_charnames[i], port);
#ifndef EBCDIC
	      else if (i == '\177')
		scm_puts (scm_charnames[scm_n_charnames - 1], port);
#endif
	      else if (i < 0 || i > '\177')
		scm_intprint (i, 8, port);
	      else
		scm_putc (i, port);
	    }
	  else
	    scm_putc (i, port);
	}
      else if (SCM_IFLAGP (exp)
	       && ((size_t) SCM_ISYMNUM (exp) < (sizeof scm_isymnames / sizeof (char *))))
	  scm_puts (SCM_ISYMCHARS (exp), port);
      else if (SCM_ILOCP (exp))
	{
	  scm_puts ("#@", port);
	  scm_intprint (SCM_IFRAME (exp), 10, port);
	  scm_putc (SCM_ICDRP (exp) ? '-' : '+', port);
	  scm_intprint (SCM_IDIST (exp), 10, port);
	}
      else
	{
	  /* unknown immediate value */
	  scm_ipruk ("immediate", exp, port);
	}
      break;
    case scm_tc3_cons_gloc:
      /* gloc */
      scm_puts ("#@", port);
      exp = SCM_GLOC_SYM (exp);
      goto taloop;
    case scm_tc3_cons:
      switch (SCM_TYP7 (exp))
	{
	case scm_tcs_cons_gloc:

	  if (SCM_STRUCT_VTABLE_DATA (exp) [scm_vtable_index_vcell] == 0)
	    {
	      ENTER_NESTED_DATA (pstate, exp, circref);
	      if (SCM_OBJ_CLASS_FLAGS (exp) & SCM_CLASSF_GOOPS)
		{
		  SCM pwps, print = pstate->writingp ? g_write : g_display;
		  if (!print)
		    goto print_struct;
		  SCM_NEWSMOB (pwps,
			       scm_tc16_port_with_ps,
			       SCM_UNPACK (scm_cons (port, pstate->handle)));
		  scm_call_generic_2 (print, exp, pwps);
		}
	      else
		{
		print_struct:
		  scm_print_struct (exp, port, pstate);
		}
	      EXIT_NESTED_DATA (pstate);
	      break;
	    }

	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_iprlist ("(", exp, ')', port, pstate);
	  EXIT_NESTED_DATA (pstate);
	  break;
	circref:
	  print_circref (port, pstate, exp);
	  break;
	macros:
	  if (!SCM_CLOSUREP (SCM_CDR (exp)))
	    goto prinmacro;
	case scm_tcs_closures:
	  /* The user supplied print closure procedure must handle
	     macro closures as well. */
	  if (SCM_FALSEP (scm_procedure_p (SCM_PRINT_CLOSURE))
	      || SCM_FALSEP (scm_printer_apply (SCM_PRINT_CLOSURE,
						exp, port, pstate)))
	  {
	    SCM name, code, env;
	    if (SCM_TYP16 (exp) == scm_tc16_macro)
	      {
		/* Printing a macro. */
	      prinmacro:
		name = scm_macro_name (exp);
		if (!SCM_CLOSUREP (SCM_CDR (exp)))
		  {
		    code = env = SCM_UNDEFINED;
		    scm_puts ("#<primitive-", port);
		  }
		else
		  {
		    code = SCM_CODE (SCM_CDR (exp));
		    env = SCM_ENV (SCM_CDR (exp));
		    scm_puts ("#<", port);
		  }
		if (SCM_CELL_WORD_0 (exp) & (3L << 16))
		  scm_puts ("macro", port);
		else
		  scm_puts ("syntax", port);
		if (SCM_CELL_WORD_0 (exp) & (2L << 16))
		  scm_putc ('!', port);
	      }
	    else
	      {
		/* Printing a closure. */
		name = scm_procedure_name (exp);
		code = SCM_CODE (exp);
		env = SCM_ENV (exp);
		scm_puts ("#<procedure", port);
	      }
	    if (SCM_SYMBOLP (name))
	      {
		scm_putc (' ', port);
		scm_lfwrite (SCM_SYMBOL_CHARS (name), SCM_SYMBOL_LENGTH (name), port);
	      }
	    else if (SCM_STRINGP (name))
	      {
		scm_putc (' ', port);
		scm_lfwrite (SCM_STRING_CHARS (name), SCM_STRING_LENGTH (name), port);
	      }
	    if (!SCM_UNBNDP (code))
	      {
		if (SCM_PRINT_SOURCE_P)
		  {
		    code = scm_unmemocopy (code,
					   SCM_EXTEND_ENV (SCM_CAR (code),
							   SCM_EOL,
							   env));
		    ENTER_NESTED_DATA (pstate, exp, circref);
		    scm_iprlist (" ", code, '>', port, pstate);
		    EXIT_NESTED_DATA (pstate);
		  }
		else
		  {
		    if (SCM_TYP16 (exp) != scm_tc16_macro)
		      {
			scm_putc (' ', port);
			scm_iprin1 (SCM_CAR (code), port, pstate);
		      }
		    scm_putc ('>', port);
		  }
	      }
	    else
	      scm_putc ('>', port);
	  }
	  break;
	case scm_tc7_substring:
	case scm_tc7_string:
	  if (SCM_WRITINGP (pstate))
	    {
	      scm_sizet i;

	      scm_putc ('"', port);
	      for (i = 0; i < SCM_STRING_LENGTH (exp); ++i)
		switch (SCM_STRING_CHARS (exp)[i])
		  {
		  case '"':
		  case '\\':
		    scm_putc ('\\', port);
		  default:
		    scm_putc (SCM_STRING_CHARS (exp)[i], port);
		  }
	      scm_putc ('"', port);
	      break;
	    }
	  else
	    scm_lfwrite (SCM_STRING_CHARS (exp), SCM_STRING_LENGTH (exp), port);
	  break;
	case scm_tc7_symbol:
	    {
	      int pos;
	      int end;
	      int len;
	      char * str;
	      int weird;
	      int maybe_weird;
	      int mw_pos = 0;

	      len = SCM_SYMBOL_LENGTH (exp);
	      str = SCM_SYMBOL_CHARS (exp);
	      pos = 0;
	      weird = 0;
	      maybe_weird = 0;

	      if (len == 0)
		scm_lfwrite ("#{}#", 4, port);

	      for (end = pos; end < len; ++end)
		switch (str[end])
		  {
#ifdef BRACKETS_AS_PARENS
		  case '[':
		  case ']':
#endif
		  case '(':
		  case ')':
		  case '"':
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
			scm_lfwrite ("#{", 2, port);
			weird = 1;
		      }
		    if (pos < end)
		      {
			scm_lfwrite (str + pos, end - pos, port);
		      }
		    {
		      char buf[2];
		      buf[0] = '\\';
		      buf[1] = str[end];
		      scm_lfwrite (buf, 2, port);
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
		scm_lfwrite (str + pos, end - pos, port);
	      scm_remember_upto_here_1 (exp);
	      if (weird)
		scm_lfwrite ("}#", 2, port);
	      break;
	    }
	case scm_tc7_wvect:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  if (SCM_IS_WHVEC (exp))
	    scm_puts ("#wh(", port);
	  else
	    scm_puts ("#w(", port);
	  goto common_vector_printer;

	case scm_tc7_vector:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_puts ("#(", port);
	common_vector_printer:
	  {
	    register long i;
	    int last = SCM_VECTOR_LENGTH (exp) - 1;
	    int cutp = 0;
	    if (pstate->fancyp && SCM_VECTOR_LENGTH (exp) > pstate->length)
	      {
		last = pstate->length - 1;
		cutp = 1;
	      }
	    for (i = 0; i < last; ++i)
	      {
		/* CHECK_INTS; */
		scm_iprin1 (SCM_VELTS (exp)[i], port, pstate);
		scm_putc (' ', port);
	      }
	    if (i == last)
	      {
		/* CHECK_INTS; */
		scm_iprin1 (SCM_VELTS (exp)[i], port, pstate);
	      }
	    if (cutp)
	      scm_puts (" ...", port);
	    scm_putc (')', port);
	  }
	  EXIT_NESTED_DATA (pstate);
	  break;
#ifdef HAVE_ARRAYS
	case scm_tc7_bvect:
	case scm_tc7_byvect:
	case scm_tc7_svect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
#ifdef HAVE_LONG_LONGS
	case scm_tc7_llvect:
#endif
	  scm_raprin1 (exp, port, pstate);
	  break;
#endif
	case scm_tcs_subrs:
	  scm_puts (SCM_SUBR_GENERIC (exp) && *SCM_SUBR_GENERIC (exp)
		    ? "#<primitive-generic "
		    : "#<primitive-procedure ",
		    port);
	  scm_puts (SCM_SYMBOL_CHARS (SCM_SNAME (exp)), port);
	  scm_putc ('>', port);
	  break;
#ifdef CCLO
	case scm_tc7_cclo:
	  {
	    SCM proc = SCM_CCLO_SUBR (exp);
	    if (SCM_EQ_P (proc, scm_f_gsubr_apply))
	      {
		/* Print gsubrs as primitives */
		SCM name = scm_procedure_name (exp);
		scm_puts ("#<primitive-procedure", port);
		if (SCM_NFALSEP (name))
		  {
		    scm_putc (' ', port);
		    scm_puts (SCM_SYMBOL_CHARS (name), port);
		  }
	      }
	    else
	      {
		scm_puts ("#<compiled-closure ", port);
		scm_iprin1 (proc, port, pstate);
	      }
	    scm_putc ('>', port);
	  }
	  break;
#endif
	case scm_tc7_pws:
	  scm_puts ("#<procedure-with-setter", port);
	  {
	    SCM name = scm_procedure_name (exp);
	    if (SCM_NFALSEP (name))
	      {
		scm_putc (' ', port);
		scm_display (name, port);
	      }
	  }
	  scm_putc ('>', port);
	  break;
	case scm_tc7_port:
	  {
	    register long i = SCM_PTOBNUM (exp);
	    if (i < scm_numptob
		&& scm_ptobs[i].print
		&& (scm_ptobs[i].print) (exp, port, pstate))
	      break;
	    goto punk;
	  }
	case scm_tc7_smob:
	  {
	    register long i;
	    ENTER_NESTED_DATA (pstate, exp, circref);
	    i = SCM_SMOBNUM (exp);
	    if (i < scm_numsmob && scm_smobs[i].print
		&& (scm_smobs[i].print) (exp, port, pstate))
	      {
		EXIT_NESTED_DATA (pstate);
		break;
	      }
	    EXIT_NESTED_DATA (pstate);
	    /* Macros have their print field set to NULL.  They are
	       handled at the same place as closures in order to achieve
	       non-redundancy.  Placing the condition here won't slow
	       down printing of other smobs. */
	    if (SCM_TYP16 (exp) == scm_tc16_macro)
	      goto macros;
	  }
	default:
	punk:
	  scm_ipruk ("type", exp, port);
	}
    }
}

/* Print states are necessary for circular reference safe printing.
 * They are also expensive to allocate.  Therefore print states are
 * kept in a pool so that they can be reused.
 */

/* The PORT argument can also be a print-state/port pair, which will
 * then be used instead of allocating a new print state.  This is
 * useful for continuing a chain of print calls from Scheme.  */

void 
scm_prin1 (SCM exp, SCM port, int writingp)
{
  SCM handle = SCM_BOOL_F; /* Will GC protect the handle whilst unlinked */
  SCM pstate_scm;
  scm_print_state *pstate;

  /* If PORT is a print-state/port pair, use that.  Else create a new
     print-state. */

  if (SCM_PORT_WITH_PS_P (port))
    {
      pstate_scm = SCM_PORT_WITH_PS_PS (port);
      port = SCM_PORT_WITH_PS_PORT (port);
    }
  else
    {
      /* First try to allocate a print state from the pool */
      SCM_DEFER_INTS;
      if (SCM_NNULLP (SCM_CDR (print_state_pool)))
	{
	  handle = SCM_CDR (print_state_pool);
	  SCM_SETCDR (print_state_pool, SCM_CDDR (print_state_pool));
	}
      SCM_ALLOW_INTS;
      if (SCM_FALSEP (handle))
	handle = scm_cons (make_print_state (), SCM_EOL);
      pstate_scm = SCM_CAR (handle);
    }

  pstate = SCM_PRINT_STATE (pstate_scm);
  pstate->writingp = writingp;
  scm_iprin1 (exp, port, pstate);

  /* Return print state to pool if it has been created above and
     hasn't escaped to Scheme. */

  if (!SCM_FALSEP (handle) && !pstate->revealed)
    {
      SCM_DEFER_INTS;
      SCM_SETCDR (handle, SCM_CDR (print_state_pool));
      SCM_SETCDR (print_state_pool, handle);
      SCM_ALLOW_INTS;
    }
}


/* Print an integer.
 */

void 
scm_intprint (long n, int radix, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  scm_lfwrite (num_buf, scm_iint2str (n, radix, num_buf), port);
}

/* Print an object of unrecognized type.
 */

void 
scm_ipruk (char *hdr, SCM ptr, SCM port)
{
  scm_puts ("#<unknown-", port);
  scm_puts (hdr, port);
  if (SCM_CELLP (ptr))
    {
      scm_puts (" (0x", port);
      scm_intprint (SCM_CELL_WORD_0 (ptr), 16, port);
      scm_puts (" . 0x", port);
      scm_intprint (SCM_CELL_WORD_1 (ptr), 16, port);
      scm_puts (") @", port);
    }
  scm_puts (" 0x", port);
  scm_intprint (SCM_UNPACK (ptr), 16, port);
  scm_putc ('>', port);
}

/* Print a list.
 */


void 
scm_iprlist (char *hdr,SCM exp,int tlr,SCM port,scm_print_state *pstate)
{
  register SCM hare, tortoise;
  int floor = pstate->top - 2;
  scm_puts (hdr, port);
  /* CHECK_INTS; */
  if (pstate->fancyp)
    goto fancy_printing;
  
  /* Run a hare and tortoise so that total time complexity will be
     O(depth * N) instead of O(N^2). */
  hare = SCM_CDR (exp);
  tortoise = exp;
  while (SCM_ECONSP (hare))
    {
      if (SCM_EQ_P (hare, tortoise))
	goto fancy_printing;
      hare = SCM_CDR (hare);
      if (SCM_IMP (hare) || SCM_NECONSP (hare))
	break;
      hare = SCM_CDR (hare);
      tortoise = SCM_CDR (tortoise);
    }
  
  /* No cdr cycles intrinsic to this list */
  scm_iprin1 (SCM_CAR (exp), port, pstate);
  exp = SCM_CDR (exp);
  for (; SCM_NIMP (exp); exp = SCM_CDR (exp))
    {
      register int i;

      if (SCM_NECONSP (exp))
	break;
      for (i = floor; i >= 0; --i)
	if (SCM_EQ_P (pstate->ref_stack[i], exp))
	  goto circref;
      PUSH_REF (pstate, exp);
      scm_putc (' ', port);
      /* CHECK_INTS; */
      scm_iprin1 (SCM_CAR (exp), port, pstate);
    }
  if (SCM_NNULLP (exp))
    {
      scm_puts (" . ", port);
      scm_iprin1 (exp, port, pstate);
    }

end:
  scm_putc (tlr, port);
  pstate->top = floor + 2;
  return;
  
fancy_printing:
  {
    int n = pstate->length;
    
    scm_iprin1 (SCM_CAR (exp), port, pstate);
    exp = SCM_CDR (exp); --n;
    for (; SCM_NIMP (exp); exp = SCM_CDR (exp))
      {
	register unsigned long i;

	if (SCM_NECONSP (exp))
	  break;
	for (i = 0; i < pstate->top; ++i)
	  if (SCM_EQ_P (pstate->ref_stack[i], exp))
	    goto fancy_circref;
	if (pstate->fancyp)
	  {
	    if (n == 0)
	      {
		scm_puts (" ...", port);
		goto skip_tail;
	      }
	    else
	      --n;
	  }
	PUSH_REF(pstate, exp);
	++pstate->list_offset;
	scm_putc (' ', port);
	/* CHECK_INTS; */
	scm_iprin1 (SCM_CAR (exp), port, pstate);
      }
  }
  if (SCM_NNULLP (exp))
    {
      scm_puts (" . ", port);
      scm_iprin1 (exp, port, pstate);
    }
skip_tail:
  pstate->list_offset -= pstate->top - floor - 2;
  goto end;

fancy_circref:
  pstate->list_offset -= pstate->top - floor - 2;
  
circref:
  scm_puts (" . ", port);
  print_circref (port, pstate, exp);
  goto end;
}



int
scm_valid_oport_value_p	(SCM val)
{
  return (SCM_OPOUTPORTP (val)
          || (SCM_PORT_WITH_PS_P (val)
              && SCM_OPOUTPORTP (SCM_PORT_WITH_PS_PORT (val))));
}

/* SCM_GPROC(s_write, "write", 1, 1, 0, scm_write, g_write); */

SCM 
scm_write (SCM obj, SCM port)
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;

  SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_write);

  scm_prin1 (obj, port, 1);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}


/* SCM_GPROC(s_display, "display", 1, 1, 0, scm_display, g_display); */

SCM 
scm_display (SCM obj, SCM port)
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;

  SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_display);

  scm_prin1 (obj, port, 0);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}


SCM_DEFINE (scm_simple_format, "simple-format", 2, 0, 1,
            (SCM destination, SCM message, SCM args),
	    "Write @var{message} to @var{destination}, defaulting to\n"
	    "the current output port.\n"
	    "@var{message} can contain @code{~A} (was @code{%s}) and\n"
	    "@code{~S} (was @code{%S}) escapes.  When printed,\n"
	    "the escapes are replaced with corresponding members of\n"
	    "@var{ARGS}:\n"
	    "@code{~A} formats using @code{display} and @code{~S} formats\n"
	    "using @code{write}.\n"
	    "If @var{destination} is @code{#t}, then use the current output\n"
	    "port, if @var{destination} is @code{#f}, then return a string\n"
	    "containing the formatted text. Does not add a trailing newline.")
#define FUNC_NAME s_scm_simple_format
{
  SCM answer = SCM_UNSPECIFIED;
  int fReturnString = 0;
  int writingp;
  char *start;
  char *end;
  char *p;

  if (SCM_EQ_P (destination, SCM_BOOL_T))
    {
      destination = scm_cur_outp;
    }
  else if (SCM_FALSEP (destination))
    {
      fReturnString = 1;
      destination = scm_mkstrport (SCM_INUM0, 
				   scm_make_string (SCM_INUM0, SCM_UNDEFINED),
				   SCM_OPN | SCM_WRTNG,
				   FUNC_NAME);
    }
  else
    {
      SCM_VALIDATE_OPORT_VALUE (1, destination);
      destination = SCM_COERCE_OUTPORT (destination);
    }
  SCM_VALIDATE_STRING (2, message);
  SCM_VALIDATE_REST_ARGUMENT (args);

  start = SCM_STRING_CHARS (message);
  end = start + SCM_STRING_LENGTH (message);
  for (p = start; p != end; ++p)
    if (*p == '~')
      {
	if (!SCM_CONSP (args))
	  continue;
	
	if (++p == end)
	  continue;
	
	if (*p == 'A' || *p == 'a')
	  writingp = 0;
	else if (*p == 'S' || *p == 's')
	  writingp = 1;
	else
	  continue;

	scm_lfwrite (start, p - start - 1, destination);
	scm_prin1 (SCM_CAR (args), destination, writingp);
	args = SCM_CDR (args);
	start = p + 1;
      }
  scm_lfwrite (start, p - start, destination);

  if (fReturnString)
    answer = scm_strport_to_string (destination);

  return scm_return_first (answer, message);
}
#undef FUNC_NAME


SCM_DEFINE (scm_newline, "newline", 0, 1, 0, 
            (SCM port),
	    "Send a newline to @var{port}.")
#define FUNC_NAME s_scm_newline
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;

  SCM_VALIDATE_OPORT_VALUE (1,port);

  scm_putc ('\n', SCM_COERCE_OUTPORT (port));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_write_char, "write-char", 1, 1, 0,
            (SCM chr, SCM port),
	    "Send character @var{chr} to @var{port}.")
#define FUNC_NAME s_scm_write_char
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;

  SCM_VALIDATE_CHAR (1,chr);
  SCM_VALIDATE_OPORT_VALUE (2,port);

  scm_putc ((int) SCM_CHAR (chr), SCM_COERCE_OUTPORT (port));
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Call back to Scheme code to do the printing of special objects
 * (like structs).  SCM_PRINTER_APPLY applies PROC to EXP and a smob
 * containing PORT and PSTATE.  This object can be used as the port for
 * display/write etc to continue the current print chain.  The REVEALED
 * field of PSTATE is set to true to indicate that the print state has
 * escaped to Scheme and thus has to be freed by the GC.
 */

scm_bits_t scm_tc16_port_with_ps;

/* Print exactly as the port itself would */

static int
port_with_ps_print (SCM obj, SCM port, scm_print_state *pstate)
{
  obj = SCM_PORT_WITH_PS_PORT (obj);
  return scm_ptobs[SCM_PTOBNUM (obj)].print (obj, port, pstate);
}

SCM
scm_printer_apply (SCM proc, SCM exp, SCM port, scm_print_state *pstate)
{
  SCM pwps;
  SCM pair = scm_cons (port, pstate->handle);
  SCM_NEWSMOB (pwps, scm_tc16_port_with_ps, SCM_UNPACK (pair));
  pstate->revealed = 1;
  return scm_apply (proc, exp, scm_cons (pwps, scm_listofnull));
}

SCM_DEFINE (scm_port_with_print_state, "port-with-print-state", 2, 0, 0, 
            (SCM port, SCM pstate),
	    "Create a new port which behaves like @var{port}, but with an\n"
	    "included print state @var{pstate}.")
#define FUNC_NAME s_scm_port_with_print_state
{
  SCM pwps;
  SCM_VALIDATE_OPORT_VALUE (1,port);
  SCM_VALIDATE_PRINTSTATE (2,pstate);
  port = SCM_COERCE_OUTPORT (port);
  SCM_NEWSMOB (pwps, scm_tc16_port_with_ps, SCM_UNPACK (scm_cons (port, pstate)));
  return pwps;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_print_state, "get-print-state", 1, 0, 0, 
            (SCM port),
	    "Return the print state of the port @var{port}. If @var{port}\n"
	    "has no associated print state, @code{#f} is returned.")
#define FUNC_NAME s_scm_get_print_state
{
  if (SCM_PORT_WITH_PS_P (port))
    return SCM_PORT_WITH_PS_PS (port);
  if (SCM_OUTPUT_PORT_P (port))
    return SCM_BOOL_F;
  SCM_WRONG_TYPE_ARG (1, port);
}
#undef FUNC_NAME



void
scm_init_print ()
{
  SCM vtable, layout, type;
  
  scm_init_opts (scm_print_options, scm_print_opts, SCM_N_PRINT_OPTIONS);
  vtable = scm_make_vtable_vtable (scm_nullstr, SCM_INUM0, SCM_EOL);
  layout = scm_make_struct_layout (scm_makfrom0str (SCM_PRINT_STATE_LAYOUT));
  type = scm_make_struct (vtable, SCM_INUM0, SCM_LIST1 (layout));
  scm_set_struct_vtable_name_x (type, scm_str2symbol ("print-state"));
  print_state_pool = scm_permanent_object (scm_cons (type, SCM_EOL));

  scm_print_state_vtable = type;

  /* Don't want to bind a wrapper class in GOOPS, so pass 0 as arg1. */
  scm_tc16_port_with_ps = scm_make_smob_type (0, 0);
  scm_set_smob_mark (scm_tc16_port_with_ps, scm_markcdr);
  scm_set_smob_print (scm_tc16_port_with_ps, port_with_ps_print);
  
#ifndef SCM_MAGIC_SNARFER
#include "libguile/print.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
